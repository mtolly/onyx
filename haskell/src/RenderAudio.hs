{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module RenderAudio
( AudioSearch(..), JammitSearch(..), MoggSearch(..)
, audioSearch, jammitSearch, moggSearch
, checkDefined
, jammitPath
, manualLeaf
, MOGGChannel(..), buildMOGG
, makeAudioFiles
) where

import           Audio
import           Config
import           Control.Monad                  (forM_, guard, join)
import           Control.Monad.Extra            (filterM, findM)
import           Control.Monad.Trans.StackTrace (StackTraceT, fatal)
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (toLower)
import           Data.Conduit.Audio
import           Data.Default.Class             (def)
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (partition)
import           Data.Maybe                     (fromMaybe, isJust, isNothing,
                                                 listToMaybe)
import qualified Data.Text                      as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified RockBand.Drums                 as RBDrums
import           RockBand.File                  (FlexPartName, getPartName)
import qualified Sound.File.Sndfile             as Snd
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

-- make sure all audio leaves are defined, catch typos
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

-- Looking up single audio files and Jammit parts in the work directory
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

data MOGGChannel = ChannelSong | ChannelCrowd | ChannelPart FlexPartName
  deriving (Eq, Ord, Show, Read)

buildMOGG :: Plan -> SongYaml -> ([(MOGGChannel, Double, Double)], RBDrums.Audio)
buildMOGG plan songYaml = case plan of
  MoggPlan{..} -> let
    chans = undefined
    mixMode = undefined
    in (chans, mixMode)
  Plan{..} -> let
    chans = undefined
    mixMode = undefined
    in (chans, mixMode)

    {-
    ogg %> \out -> let
      parts = map Input $ concat
        [ [dir </> "drums-kick.wav"   | _hasDrums     (_instruments songYaml) && mixMode /= RBDrums.D0]
        , [dir </> "drums-snare.wav"  | _hasDrums     (_instruments songYaml) && elem mixMode [RBDrums.D1, RBDrums.D2, RBDrums.D3]]
        , [dir </> "drums-kit.wav"    | _hasDrums    $ _instruments songYaml]
        , [dir </> "bass.wav"         | hasAnyBass   $ _instruments songYaml]
        , [dir </> "guitar.wav"       | hasAnyGuitar $ _instruments songYaml]
        , [dir </> "keys.wav"         | hasAnyKeys   $ _instruments songYaml]
        , [dir </> "vocal.wav"        | hasAnyVocal  $ _instruments songYaml]
        , [dir </> "crowd.wav"        | isJust _crowd                       ]
        , [dir </> "song-countin.wav"]
        ]
      in buildAudio (Merge parts) out
    -}

data PansVols = PansVols
  { bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, crowdPV, songPV :: [(Double, Double)]
  , mixMode                                                                      :: RBDrums.Audio
  } deriving (Eq, Ord, Show, Read)

computePansVols :: TargetRB3 -> Plan -> SongYaml -> PansVols
computePansVols TargetRB3{..} plan songYaml = let
  planPV :: Maybe (PlanAudio Duration AudioInput) -> [(Double, Double)]
  planPV Nothing = [(-1, 0), (1, 0)]
  planPV (Just paud) = let
    chans = computeChannelsPlan songYaml $ _planExpr paud
    pans = case _planPans paud of
      [] -> case chans of
        0 -> []
        1 -> [0]
        2 -> [-1, 1]
        c -> error $ "Error: I don't know what pans to use for " ++ show c ++ "-channel audio"
      ps -> ps
    vols = case _planVols paud of
      [] -> replicate chans 0
      vs -> vs
    in zip pans vols
  makeSingleAudio :: PartAudio (PlanAudio t a) -> PlanAudio t a
  makeSingleAudio = undefined
  makeSingleChans :: PartAudio [Int] -> [Int]
  makeSingleChans = concat . toList
  matchingMix kick snare = case (kick, snare) of
    (0, 0) -> RBDrums.D0
    (1, 1) -> RBDrums.D1
    (1, 2) -> RBDrums.D2
    (2, 2) -> RBDrums.D3
    (1, 0) -> RBDrums.D4
    _      -> error $ "No matching drum mix mode for (kick,snare) == " ++ show (kick, snare)
  bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, crowdPV, songPV :: [(Double, Double)]
  mixMode :: RBDrums.Audio
  bassPV = guard (maybe False (/= def) $ getPart rb3_Bass songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ maybe [] makeSingleChans $ HM.lookup rb3_Bass $ getParts _moggParts
    Plan{..}     -> planPV $ fmap makeSingleAudio $ HM.lookup rb3_Bass $ getParts _planParts
  guitarPV = guard (maybe False (/= def) $ getPart rb3_Guitar songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ maybe [] makeSingleChans $ HM.lookup rb3_Guitar $ getParts _moggParts
    Plan{..}     -> planPV $ fmap makeSingleAudio $ HM.lookup rb3_Guitar $ getParts _planParts
  keysPV = guard (maybe False (/= def) $ getPart rb3_Keys songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ maybe [] makeSingleChans $ HM.lookup rb3_Keys $ getParts _moggParts
    Plan{..}     -> planPV $ fmap makeSingleAudio $ HM.lookup rb3_Keys $ getParts _planParts
  vocalPV = guard (maybe False (/= def) $ getPart rb3_Vocal songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ maybe [] makeSingleChans $ HM.lookup rb3_Vocal $ getParts _moggParts
    Plan{..}     -> planPV $ fmap makeSingleAudio $ HM.lookup rb3_Vocal $ getParts _planParts
  crowdPV = case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggCrowd
    Plan{..}     -> guard (isJust _crowd) >> planPV _crowd
  (kickPV, snarePV, drumsPV, mixMode) = if maybe False (/= def) $ getPart rb3_Drums songYaml
    then case plan of
      MoggPlan{..} -> let
        getChannel i = (_pans !! i, _vols !! i)
        in case HM.lookup rb3_Bass $ getParts _moggParts of
          Nothing -> ([], [], [], RBDrums.D0)
          Just (PartSingle kit) -> ([], [], map getChannel kit, RBDrums.D0)
          Just (PartDrumKit kick snare kit) ->
            ( map getChannel $ fromMaybe [] kick
            , map getChannel $ fromMaybe [] snare
            , map getChannel kit
            , matchingMix (length kick) (length snare)
            )
      Plan{..} -> let
        count = maybe 0 (computeChannelsPlan songYaml . _planExpr)
        (srcKick, srcSnare, srcKit) = case HM.lookup rb3_Drums $ getParts _planParts of
          Nothing                           -> (Nothing, Nothing, Nothing)
          Just (PartSingle x)               -> (Nothing, Nothing, Just x)
          Just (PartDrumKit kick snare kit) -> (kick, snare, Just kit)
        theMix = matchingMix (count srcKick) (count srcSnare)
        in  ( guard (theMix /= RBDrums.D0) >> planPV srcKick
            , guard (theMix `elem` [RBDrums.D1, RBDrums.D2, RBDrums.D3]) >> planPV srcSnare
            , planPV srcKit
            , theMix
            )
    else ([], [], [], RBDrums.D0)
  songPV = case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ let
      notSong = concat $ _moggCrowd : do
        (fpart, chans) <- HM.toList $ getParts _moggParts
        guard $ fpart `elem` [rb3_Guitar, rb3_Bass, rb3_Drums, rb3_Keys, rb3_Vocal]
        return $ makeSingleChans chans
      in filter (`notElem` notSong) [0 .. length _pans - 1]
    Plan{..} -> planPV _song
  in PansVols{..}

makeAudioFiles :: SongYaml -> Plan -> FilePath -> Rules ()
makeAudioFiles songYaml plan dir = case plan of
  Plan{..} -> do
    let locate :: Audio Duration AudioInput -> Action (Audio Duration FilePath)
        locate = fmap join . mapM (manualLeaf songYaml)
        buildPart planPart fout = let
          expr = maybe (Silence 2 $ Frames 0) _planExpr planPart
          in locate expr >>= \aud -> buildAudio aud fout
    dir </> "song.wav"   %> buildPart _song
    dir </> "crowd.wav"  %> buildPart _crowd
    forM_ (HM.keys $ getParts $ _parts songYaml) $ \k -> do
      let name = T.unpack $ getPartName k
      case HM.lookup k $ getParts _planParts of
        Nothing -> do
          dir </> (name ++ "-kick.wav" ) %> buildPart Nothing
          dir </> (name ++ "-snare.wav") %> buildPart Nothing
          dir </> (name ++ "-kit.wav"  ) %> buildPart Nothing
          dir </> (name ++ ".wav"      ) %> buildPart Nothing
        Just (PartSingle pa) -> do
          dir </> (name ++ "-kick.wav" ) %> buildPart Nothing
          dir </> (name ++ "-snare.wav") %> buildPart Nothing
          dir </> (name ++ "-kit.wav"  ) %> buildPart (Just pa)
          dir </> (name ++ ".wav"      ) %> buildPart (Just pa)
        Just (PartDrumKit kick snare kit) -> do
          dir </> (name ++ "-kick.wav" ) %> buildPart kick
          dir </> (name ++ "-snare.wav") %> buildPart snare
          dir </> (name ++ "-kit.wav"  ) %> buildPart (Just kit)
          dir </> (name ++ ".wav"      ) %> buildAudio (Mix
            [ Input $ dir </> (name ++ "-kick.wav" )
            , Input $ dir </> (name ++ "-snare.wav")
            , Input $ dir </> (name ++ "-kit.wav"  )
            ])
  MoggPlan{..} -> do
    let oggChannels []    = buildAudio $ Silence 2 $ Frames 0
        oggChannels chans = buildAudio $ Channels chans $ Input $ dir </> "audio.ogg"
    forM_ (HM.keys $ getParts $ _parts songYaml) $ \k -> do
      let name = T.unpack $ getPartName k
      case HM.lookup k $ getParts _moggParts of
        Nothing -> do
          dir </> (name ++ "-kick.wav" ) %> oggChannels []
          dir </> (name ++ "-snare.wav") %> oggChannels []
          dir </> (name ++ ".wav"      ) %> oggChannels []
        Just (PartSingle pa) -> do
          dir </> (name ++ "-kick.wav" ) %> oggChannels []
          dir </> (name ++ "-snare.wav") %> oggChannels []
          dir </> (name ++ ".wav"      ) %> oggChannels pa
        Just (PartDrumKit kick snare kit) -> do
          dir </> (name ++ "-kick.wav" ) %> oggChannels (join $ toList kick)
          dir </> (name ++ "-snare.wav") %> oggChannels (join $ toList snare)
          dir </> (name ++ ".wav"      ) %> oggChannels kit
    dir </> "crowd.wav" %> oggChannels _moggCrowd
    dir </> "song-countin.wav" %> \out -> do
      need [dir </> "audio.ogg"]
      chanCount <- liftIO $ Snd.channels <$> Snd.getFileInfo (dir </> "audio.ogg")
      let usedChannels :: [Int]
          usedChannels = join (toList (getParts _moggParts) >>= toList) ++ _moggCrowd
          songChannels = do
            i <- [0 .. chanCount - 1]
            guard $ notElem i usedChannels
            return i
      oggChannels songChannels out
