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
, PansVols(..), computePansVols
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
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (nub, partition)
import           Data.Maybe                     (fromMaybe, isJust, isNothing,
                                                 listToMaybe)
import qualified Data.Text                      as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified RockBand.Drums                 as RBDrums
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
            in map getLeaves
              $ concatMap (maybe [] toList) [_song, _guitar, _bass, _keys, _drums, _vocal, _crowd]
              ++ case _countin of Countin xs -> concatMap (toList . snd) xs
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

data PansVols = PansVols
  { bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, crowdPV, songPV :: [(Double, Double)]
  , mixMode                                                                      :: RBDrums.Audio
  } deriving (Eq, Ord, Show, Read)

computePansVols :: SongYaml -> Plan -> PansVols
computePansVols songYaml plan = let
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
  bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, crowdPV, songPV :: [(Double, Double)]
  mixMode :: RBDrums.Audio
  bassPV = guard (hasAnyBass $ _instruments songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggBass
    Plan{..}     -> planPV _bass
  guitarPV = guard (hasAnyGuitar $ _instruments songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggGuitar
    Plan{..}     -> planPV _guitar
  keysPV = guard (hasAnyKeys $ _instruments songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggKeys
    Plan{..}     -> planPV _keys
  vocalPV = guard (hasAnyVocal $ _instruments songYaml) >> case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggVocal
    Plan{..}     -> planPV _vocal
  crowdPV = case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggCrowd
    Plan{..}     -> guard (isJust _crowd) >> planPV _crowd
  (kickPV, snarePV, drumsPV, mixMode) = if _hasDrums $ _instruments songYaml
    then case plan of
      MoggPlan{..} -> let
        getChannel i = (_pans !! i, _vols !! i)
        kickChannels = case _drumMix of
          RBDrums.D0 -> []
          RBDrums.D1 -> take 1 _moggDrums
          RBDrums.D2 -> take 1 _moggDrums
          RBDrums.D3 -> take 2 _moggDrums
          RBDrums.D4 -> take 1 _moggDrums
        snareChannels = case _drumMix of
          RBDrums.D0 -> []
          RBDrums.D1 -> take 1 $ drop 1 _moggDrums
          RBDrums.D2 -> take 2 $ drop 1 _moggDrums
          RBDrums.D3 -> take 2 $ drop 2 _moggDrums
          RBDrums.D4 -> []
        drumsChannels = case _drumMix of
          RBDrums.D0 -> _moggDrums
          RBDrums.D1 -> drop 2 _moggDrums
          RBDrums.D2 -> drop 3 _moggDrums
          RBDrums.D3 -> drop 4 _moggDrums
          RBDrums.D4 -> drop 1 _moggDrums
        in (map getChannel kickChannels, map getChannel snareChannels, map getChannel drumsChannels, _drumMix)
      Plan{..} -> let
        count = maybe 0 (computeChannelsPlan songYaml . _planExpr)
        matchingMix = case (count _kick, count _snare) of
          (0, 0) -> RBDrums.D0
          (1, 1) -> RBDrums.D1
          (1, 2) -> RBDrums.D2
          (2, 2) -> RBDrums.D3
          (1, 0) -> RBDrums.D4
          (k, s) -> error $ "No matching drum mix mode for (kick,snare) == " ++ show (k, s)
        in  ( guard (matchingMix /= RBDrums.D0) >> planPV _kick
            , guard (matchingMix `elem` [RBDrums.D1, RBDrums.D2, RBDrums.D3]) >> planPV _snare
            , planPV _drums
            , matchingMix
            )
    else ([], [], [], RBDrums.D0)
  songPV = case plan of
    MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ let
      notSong = concat [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal, _moggCrowd]
      in filter (`notElem` notSong) [0 .. length _pans - 1]
    Plan{..} -> planPV _song
  in PansVols{..}

makeAudioFiles :: SongYaml -> Plan -> FilePath -> RBDrums.Audio -> Rules ()
makeAudioFiles songYaml plan dir mixMode = case plan of
  Plan{..} -> do
    let locate :: Audio Duration AudioInput -> Action (Audio Duration FilePath)
        locate = fmap join . mapM (manualLeaf songYaml)
        buildPart planPart fout = let
          expr = maybe (Silence 2 $ Frames 0) _planExpr planPart
          in locate expr >>= \aud -> buildAudio aud fout
    dir </> "song.wav"   %> buildPart _song
    dir </> "guitar.wav" %> buildPart _guitar
    dir </> "bass.wav"   %> buildPart _bass
    dir </> "keys.wav"   %> buildPart _keys
    dir </> "kick.wav"   %> buildPart _kick
    dir </> "snare.wav"  %> buildPart _snare
    dir </> "drums.wav"  %> buildPart _drums
    dir </> "vocal.wav"  %> buildPart _vocal
    dir </> "crowd.wav"  %> buildPart _crowd
  MoggPlan{..} -> do
    let oggChannels []    = buildAudio $ Silence 2 $ Frames 0
        oggChannels chans = buildAudio $ Channels chans $ Input $ dir </> "audio.ogg"
    dir </> "guitar.wav" %> oggChannels _moggGuitar
    dir </> "bass.wav" %> oggChannels _moggBass
    dir </> "keys.wav" %> oggChannels _moggKeys
    dir </> "vocal.wav" %> oggChannels _moggVocal
    dir </> "crowd.wav" %> oggChannels _moggCrowd
    dir </> "kick.wav" %> do
      oggChannels $ case mixMode of
        RBDrums.D0 -> []
        RBDrums.D1 -> take 1 _moggDrums
        RBDrums.D2 -> take 1 _moggDrums
        RBDrums.D3 -> take 2 _moggDrums
        RBDrums.D4 -> take 1 _moggDrums
    dir </> "snare.wav" %> do
      oggChannels $ case mixMode of
        RBDrums.D0 -> []
        RBDrums.D1 -> take 1 $ drop 1 _moggDrums
        RBDrums.D2 -> take 2 $ drop 1 _moggDrums
        RBDrums.D3 -> take 2 $ drop 2 _moggDrums
        RBDrums.D4 -> []
    dir </> "drums.wav" %> do
      oggChannels $ case mixMode of
        RBDrums.D0 -> _moggDrums
        RBDrums.D1 -> drop 2 _moggDrums
        RBDrums.D2 -> drop 3 _moggDrums
        RBDrums.D3 -> drop 4 _moggDrums
        RBDrums.D4 -> drop 1 _moggDrums
    dir </> "song-countin.wav" %> \out -> do
      need [dir </> "audio.ogg"]
      chanCount <- liftIO $ Snd.channels <$> Snd.getFileInfo (dir </> "audio.ogg")
      let songChannels = do
            i <- [0 .. chanCount - 1]
            guard $ notElem i $ concat
              [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal, _moggCrowd]
            return i
      oggChannels songChannels out
