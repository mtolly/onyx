{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
module RenderAudio
( AudioSearch(..), JammitSearch(..), MoggSearch(..)
, audioSearch, jammitSearch, moggSearch
, checkDefined
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
import           Config
import           Control.Monad                  (forM_, join)
import           Control.Monad.Extra            (filterM, findM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Resource   (MonadResource)
import           Control.Monad.Trans.StackTrace (StackTraceT, fatal, inside)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.Conduit.Audio
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (partition)
import           Data.Maybe                     (fromMaybe, isNothing,
                                                 listToMaybe)
import qualified Data.Text                      as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified RockBand.Drums                 as RBDrums
import           RockBand.File                  (FlexPartName)
import qualified Sound.Jammit.Base              as J
import qualified Sound.Jammit.Export            as J
import qualified System.Directory               as Dir

-- | Oracle for an audio file search.
-- The String is the 'show' of a value of type 'AudioFile'.
newtype AudioSearch = AudioSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for a Jammit track search.
-- The String is the 'show' of a value of type 'JammitTrack'.
newtype JammitSearch = JammitSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for an existing MOGG file search.
-- The Text is an MD5 hash of the complete MOGG file.
newtype MoggSearch = MoggSearch T.Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

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
  Stretch _ aud -> computeChannels aud
  Mask _ _ aud -> computeChannels aud

audioSearch :: AudioFile -> [FilePath] -> Action (Maybe FilePath)
audioSearch AudioSnippet{} _     = fail "panic! called audioSearch on a snippet. report this bug"
audioSearch AudioFile{..}  files = do
  let sortForMD5 = uncurry (++) . partition (\f -> takeExtension f == ".flac")
  files1 <- case _filePath of
    Nothing   -> return $ sortForMD5 files
    Just path -> do
      need [path]
      liftIO $ fmap (:[]) $ Dir.canonicalizePath path
  files2 <- case _md5 of
    Nothing  -> return files1
    Just md5 -> fmap toList $ findM (fmap (== Just (T.unpack md5)) . audioMD5) files1
  files3 <- case _frames of
    Nothing  -> return files2
    Just len -> filterM (fmap (== Just len) . audioLength) files2
  files4 <- if isNothing _filePath && isNothing _md5
    then fail "audioSearch: you must specify either file-path or md5"
    else return files3
  files5 <- filterM (fmap (== Just _channels) . audioChannels) files4
  files6 <- case _rate of
    Nothing   -> return files5
    Just rate -> filterM (fmap (== Just rate) . audioRate) files5
  need files6
  return $ listToMaybe files6

moggSearch :: T.Text -> [FilePath] -> Action (Maybe FilePath)
moggSearch md5search files = do
  flip findM files $ \f -> case takeExtension f of
    ".mogg" -> do
      md5 <- liftIO $ show . MD5.md5 <$> BL.readFile f
      return $ T.unpack md5search == md5
    _ -> return False

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
      Just AudioFile   {..} -> _channels
      Just AudioSnippet{..} -> computeChannelsPlan songYaml _expr
    JammitSelect _ _ -> 2
  in computeChannels . fmap toChannels

jammitSearch :: SongYaml -> JammitTrack -> J.Library -> Action [(J.AudioPart, FilePath)]
jammitSearch songYaml jmt lib = do
  let title  = fromMaybe (getTitle  $ _metadata songYaml) $ _jammitTitle  jmt
      artist = fromMaybe (getArtist $ _metadata songYaml) $ _jammitArtist jmt
  return $ J.getAudioParts
    $ J.exactSearchBy J.title  (T.unpack title )
    $ J.exactSearchBy J.artist (T.unpack artist) lib

jammitPath :: T.Text -> J.AudioPart -> FilePath
jammitPath name (J.Only part)
  = "gen/jammit" </> T.unpack name </> "only" </> map toLower (drop 4 $ show part) <.> "wav"
jammitPath name (J.Without inst)
  = "gen/jammit" </> T.unpack name </> "without" </> map toLower (show inst) <.> "wav"

-- | Looking up single audio files and Jammit parts in the work directory
manualLeaf :: SongYaml -> AudioInput -> Action (Audio Duration FilePath)
manualLeaf songYaml (Named name) = case HM.lookup name $ _audio songYaml of
  Just audioQuery -> case audioQuery of
    AudioFile{..} -> do
      putNormal $ "Looking for the audio file named " ++ show name
      result <- askOracle $ AudioSearch $ show audioQuery
      case result of
        Nothing -> fail $ "Couldn't find a necessary audio file for query: " ++ show audioQuery
        Just fp -> do
          putNormal $ "Found " ++ show name ++ " located at: " ++ fp
          return $ case _rate of
            Nothing -> Resample $ Input fp
            Just _  -> Input fp -- if rate is specified, don't auto-resample
    AudioSnippet expr -> join <$> mapM (manualLeaf songYaml) expr
  Nothing -> fail $ "Couldn't find an audio source named " ++ show name
manualLeaf songYaml (JammitSelect audpart name) = case HM.lookup name $ _jammit songYaml of
  Just _  -> return $ Input $ jammitPath name audpart
  Nothing -> fail $ "Couldn't find a Jammit source named " ++ show name

-- | Computing a non-drums instrument's audio for CON/Magma.
-- Always returns 1 or 2 channels, with all volumes 0.
computeSimplePart :: FlexPartName -> Plan -> SongYaml -> [(Double, Double)]
computeSimplePart fpart plan songYaml = case plan of
  MoggPlan{..} -> let
    inds = maybe [] (concat . toList) $ HM.lookup fpart $ getParts _moggParts
    in zip (map (_pans !!) inds) (map (_vols !!) inds)
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
        [] -> replicate chans 0
        xs -> xs
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
  vols = map fst pvOut
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
  -> T.Text
  -> [(Double, Double)]
  -> [Int]
  -> StackTraceT Action (AudioSource m Float)
channelsToSpec pvOut planName pvIn chans = inside "conforming MOGG channels to output spec" $ do
  let partPVIn = map (pvIn !!) chans
      mogg = "gen/plan" </> T.unpack planName </> "audio.ogg"
  lift $ need [mogg]
  src <- lift $ buildSource $ case chans of
    [] -> Silence 1 $ Frames 0
    _  -> Channels chans $ Input mogg
  fitToSpec partPVIn pvOut src

buildAudioToSpec
  :: (MonadResource m)
  => SongYaml
  -> [(Double, Double)]
  -> Maybe (PlanAudio Duration AudioInput)
  -> StackTraceT Action (AudioSource m Float)
buildAudioToSpec songYaml pvOut mpa = inside "conforming audio file to output spec" $ do
  (expr, pans, vols) <- completePlanAudio songYaml $ case mpa of
    Nothing -> PlanAudio (Silence 1 $ Frames 0) [] []
    Just pa -> pa
  src <- lift $ do
    aud <- join <$> mapM (manualLeaf songYaml) expr
    need $ toList aud
    buildSource aud
  fitToSpec (zip pans vols) pvOut src

buildPartAudioToSpec
  :: (MonadResource m)
  => SongYaml
  -> [(Double, Double)]
  -> Maybe (PartAudio (PlanAudio Duration AudioInput))
  -> StackTraceT Action (AudioSource m Float)
buildPartAudioToSpec songYaml specPV = \case
  Nothing -> buildAudioToSpec songYaml specPV Nothing
  Just (PartSingle pa) -> buildAudioToSpec songYaml specPV $ Just pa
  Just (PartDrumKit kick snare kit) -> do
    kickSrc  <- buildAudioToSpec songYaml specPV kick
    snareSrc <- buildAudioToSpec songYaml specPV snare
    kitSrc   <- buildAudioToSpec songYaml specPV $ Just kit
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
