{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TupleSections             #-}
module RenderAudio
( checkDefined
, jammitPath
, manualLeaf
, computeSimplePart, computeDrumsPart
, computeChannelsPlan
, completePlanAudio
, buildAudioToSpec
, buildPartAudioToSpec
, channelsToSpec
) where

import           Audio
import           AudioSearch
import           Config
import           Control.Arrow                  ((&&&))
import           Control.Monad                  (forM_, join)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace (SendMessage, StackTraceT,
                                                 Staction, fatal, inside, lg)
import           Data.Char                      (toLower)
import           Data.Conduit.Audio
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe                     (fromMaybe, listToMaybe)
import qualified Data.Text                      as T
import           Development.Shake              (need)
import           Development.Shake.FilePath
import qualified RockBand.Codec.Drums           as RBDrums
import           RockBand.Codec.File            (FlexPartName)
import qualified Sound.Jammit.Base              as J

computeChannels :: Audio Duration Int -> Int
computeChannels = \case
  Silence n _ -> n
  Input n -> n
  Mix auds -> foldr max 0 $ map computeChannels auds
  Merge auds -> sum $ map computeChannels auds
  Concatenate auds -> foldr max 0 $ map computeChannels auds
  Gain _ aud -> computeChannels aud
  Take _ _ aud -> computeChannels aud
  Drop _ _ aud -> computeChannels aud
  Fade _ _ aud -> computeChannels aud
  Pad _ _ aud -> computeChannels aud
  Resample aud -> computeChannels aud
  Channels chans _ -> length chans
  StretchSimple _ aud -> computeChannels aud
  StretchFull _ _ aud -> computeChannels aud
  Mask _ _ aud -> computeChannels aud

-- | make sure all audio leaves are defined, catch typos
checkDefined :: (Monad m) => SongYaml -> StackTraceT m ()
checkDefined songYaml = do
  let definedLeaves = HM.keys (_audio songYaml) ++ HM.keys (_jammit songYaml)
  forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
    let leaves = case plan of
          MoggPlan{} -> []
          Plan{..} -> let
            getLeaves = \case
              Named t -> t
              JammitSelect _ t -> t
            in map getLeaves $ concat
              [ maybe [] toList _song
              , maybe [] toList _crowd
              , case _countin of Countin xs -> concatMap (toList . snd) xs
              , toList (getParts _planParts) >>= toList >>= toList
              ]
    case filter (not . (`elem` definedLeaves)) leaves of
      [] -> return ()
      undefinedLeaves -> fatal $
        "Undefined leaves in plan " ++ show planName ++ " audio expression: " ++ show undefinedLeaves

computeChannelsPlan :: SongYaml -> Audio Duration AudioInput -> Int
computeChannelsPlan songYaml = let
  toChannels ai = case ai of
    Named name -> case HM.lookup name $ _audio songYaml of
      Nothing               -> error
        "panic! audio leaf not found, after it should've been checked"
      Just (AudioFile AudioInfo{..}) -> _channels
      Just AudioSnippet{..}          -> computeChannelsPlan songYaml _expr
    JammitSelect _ _ -> 2
  in computeChannels . fmap toChannels

jammitPath :: T.Text -> J.AudioPart -> FilePath
jammitPath name (J.Only part)
  = "gen/jammit" </> T.unpack name </> "only" </> map toLower (drop 4 $ show part) <.> "wav"
jammitPath name (J.Without inst)
  = "gen/jammit" </> T.unpack name </> "without" </> map toLower (show inst) <.> "wav"

-- | Looking up single audio files and Jammit parts in the work directory
manualLeaf :: (SendMessage m, MonadIO m) => FilePath -> AudioLibrary -> SongYaml -> AudioInput -> StackTraceT m (Audio Duration FilePath)
manualLeaf rel alib songYaml (Named name) = case HM.lookup name $ _audio songYaml of
  Just audioQuery -> case audioQuery of
    AudioFile ainfo -> inside ("Looking for the audio file named " ++ show name) $ do
      aud <- searchInfo rel alib ainfo
      lg $ "Found audio file " ++ show name ++ " at: " ++ show (toList aud)
      return aud
    AudioSnippet expr -> join <$> mapM (manualLeaf rel alib songYaml) expr
  Nothing -> fail $ "Couldn't find an audio source named " ++ show name
manualLeaf rel _ songYaml (JammitSelect audpart name) = case HM.lookup name $ _jammit songYaml of
  Just _  -> return $ Input $ rel </> jammitPath name audpart
  Nothing -> fail $ "Couldn't find a Jammit source named " ++ show name

-- | Computing a non-drums instrument's audio for CON/Magma.
-- Always returns 1 or 2 channels, with all volumes 0.
computeSimplePart :: FlexPartName -> Plan -> SongYaml -> [(Double, Double)]
computeSimplePart fpart plan songYaml = case plan of
  MoggPlan{..} -> let
    inds = maybe [] (concat . toList) $ HM.lookup fpart $ getParts _moggParts
    in case inds of
      [] -> [(0, 0)] -- this is only used for Magma
      _  -> map ((_pans !!) &&& (_vols !!)) inds
  Plan{..} -> case HM.lookup fpart $ getParts _planParts of
    Nothing -> [(0, 0)]
    Just (PartSingle pa) -> case computeChannelsPlan songYaml $ _planExpr pa of
      1 -> [(fromMaybe 0 $ listToMaybe $ _planPans pa, 0)]
      _ -> [(-1, 0), (1, 0)]
    Just (PartDrumKit _ _ _) -> [(-1, 0), (1, 0)]

completePlanAudio :: (Monad m) => SongYaml -> PlanAudio Duration AudioInput -> StackTraceT m (Audio Duration AudioInput, [Double], [Double])
completePlanAudio songYaml pa = do
  let chans = computeChannelsPlan songYaml $ _planExpr pa
      vols = map realToFrac $ case _planVols pa of
        []  -> replicate chans 0
        [x] -> replicate chans x
        xs  -> xs
  pans <- case _planPans pa of
    [] -> case chans of
      0 -> return []
      1 -> return [0]
      2 -> return [-1, 1]
      n -> fatal $ "No automatic pan choice for " ++ show n ++ " channels"
    xs -> return $ map realToFrac xs
  return (_planExpr pa, pans, vols)

fitToSpec
  :: (Monad m, MonadResource r)
  => [(Double, Double)]
  -> [(Double, Double)]
  -> AudioSource r Float
  -> StackTraceT m (AudioSource r Float)
fitToSpec pvIn pvOut src = let
  pans = map fst pvIn
  vols = map snd pvIn
  in case pvOut of
    [(pan, 0)] -> if [pan] == pans
      then return $ case fromMaybe 0 $ listToMaybe vols of
        0   -> src
        vol -> gain (10 ** (realToFrac vol / 20)) src -- just apply vol to single channel and we're done
      else fatal $ "Mono output (" ++ show pvOut ++ ") does not match input pans: " ++ show pans
    [(-1, 0), (1, 0)] -> return
      $ applyPansVols (map realToFrac pans) (map realToFrac vols) src -- simple stereo remix
    _ -> if pvIn == pvOut
      then return src -- input == output, we're good
      else fatal $ "Unrecognized audio spec: " ++ show pvOut

channelsToSpec
  :: (MonadResource m)
  => [(Double, Double)]
  -> FilePath
  -> [(Double, Double)]
  -> [Int]
  -> Staction (AudioSource m Float)
channelsToSpec pvOut pathOgg pvIn chans = inside "conforming MOGG channels to output spec" $ do
  let partPVIn = map (pvIn !!) chans
  src <- case chans of
    [] -> do
      lift $ lift $ need [pathOgg]
      rate <- audioRate pathOgg >>= \case
        Just rate -> return $ fromIntegral rate
        Nothing -> do
          lg "Couldn't detect sample rate of input OGG; assuming 44100 Hz."
          return 44100
      return $ silent (Frames 0) rate 1
    _  -> lift $ lift $ buildSource $ Channels (map Just chans) $ Input pathOgg
  fitToSpec partPVIn pvOut src

buildAudioToSpec
  :: (MonadResource m)
  => FilePath
  -> AudioLibrary
  -> SongYaml
  -> [(Double, Double)]
  -> Maybe (PlanAudio Duration AudioInput)
  -> Staction (AudioSource m Float)
buildAudioToSpec rel alib songYaml pvOut mpa = inside "conforming audio file to output spec" $ do
  (expr, pans, vols) <- completePlanAudio songYaml
    $ fromMaybe (PlanAudio (Silence 1 $ Frames 0) [] []) mpa
  src <- mapM (manualLeaf rel alib songYaml) expr >>= lift . lift . buildSource . join
  fitToSpec (zip pans vols) pvOut src

buildPartAudioToSpec
  :: (MonadResource m)
  => FilePath
  -> AudioLibrary
  -> SongYaml
  -> [(Double, Double)]
  -> Maybe (PartAudio (PlanAudio Duration AudioInput))
  -> Staction (AudioSource m Float)
buildPartAudioToSpec rel alib songYaml specPV = \case
  Nothing -> buildAudioToSpec rel alib songYaml specPV Nothing
  Just (PartSingle pa) -> buildAudioToSpec rel alib songYaml specPV $ Just pa
  Just (PartDrumKit kick snare kit) -> do
    kickSrc  <- buildAudioToSpec rel alib songYaml specPV kick
    snareSrc <- buildAudioToSpec rel alib songYaml specPV snare
    kitSrc   <- buildAudioToSpec rel alib songYaml specPV $ Just kit
    return $ mix kickSrc $ mix snareSrc kitSrc

-- | Computing a drums instrument's audio for CON/Magma.
-- Always returns a valid drum mix configuration, with all volumes 0.
computeDrumsPart
  :: (Monad m)
  => FlexPartName
  -> Plan
  -> SongYaml
  -> StackTraceT m (([(Double, Double)], [(Double, Double)], [(Double, Double)]), RBDrums.Audio)
computeDrumsPart fpart plan songYaml = inside "Computing drums audio mix" $ case plan of
  MoggPlan{..} -> case HM.lookup fpart $ getParts _moggParts of
    Nothing -> return stereo
    Just (PartSingle kit) -> case length kit of
      2 -> return stereo
      n -> fatal $ "MOGG plan has single drums audio with " ++ show n ++ " channels (2 required)"
    Just (PartDrumKit kick snare kit) -> do
      mixMode <- lookupSplit (maybe 0 length kick, maybe 0 length snare, length kit)
      return
        ( ( standardIndexes _pans $ fromMaybe [] kick
          , standardIndexes _pans $ fromMaybe [] snare
          , standardIndexes _pans kit
          )
        , mixMode
        )
  Plan{..} -> case HM.lookup fpart $ getParts _planParts of
    Nothing -> return stereo
    Just (PartSingle _) -> return stereo -- any number will be remixed to stereo
    Just (PartDrumKit kick snare kit) -> do
      mixMode <- lookupSplit
        ( maybe 0 (computeChannelsPlan songYaml . _planExpr) kick
        , maybe 0 (computeChannelsPlan songYaml . _planExpr) snare
        , computeChannelsPlan songYaml $ _planExpr kit
        )
      kickPV <- case kick of
        Nothing -> return []
        Just pa -> (\(_, pans, _) -> map (, 0) pans) <$> completePlanAudio songYaml pa
      snarePV <- case snare of
        Nothing -> return []
        Just pa -> (\(_, pans, _) -> map (, 0) pans) <$> completePlanAudio songYaml pa
      kitPV <- (\(_, pans, _) -> map (, 0) pans) <$> completePlanAudio songYaml kit
      return ((kickPV, snarePV, kitPV), mixMode)
  where lookupSplit = \case
          (0, 0, 2) -> return RBDrums.D0
          (1, 1, 2) -> return RBDrums.D1
          (1, 2, 2) -> return RBDrums.D2
          (2, 2, 2) -> return RBDrums.D3
          (1, 0, 2) -> return RBDrums.D4
          trio -> fatal $ "Plan with split drum kit has invalid channel counts: (kick,snare,kit) = " ++ show trio
        stereo = (([], [], [(-1, 0), (1, 0)]), RBDrums.D0)
        standardIndexes pans = \case
          []  -> []
          [i] -> [(pans !! i, 0)]
          _   -> [(-1, 0), (1, 0)]
