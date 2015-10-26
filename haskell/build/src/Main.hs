{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Audio
import           Config
import           Image
import           Magma
import           OneFoot
import           Scripts
import           X360
import           YAMLTree
import           Resources (emptyMilo)
import qualified C3

import           Codec.Picture
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Sndfile
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Magma         as Magma
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (foldl', isPrefixOf, nub)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RockBand.Common                  (Difficulty (..))
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.FiveButton              as RBFive
import qualified RockBand.File                    as RBFile
import qualified RockBand.Vocals                  as RBVox
import qualified RockBand.ProKeys                 as ProKeys
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.Util                  as U
import           Control.Monad.Trans.StackTrace
import           System.Console.GetOpt
import           System.Directory                 (canonicalizePath,
                                                   setCurrentDirectory)
import           System.Environment               (getArgs)
import           System.IO                        (IOMode (ReadMode), hFileSize,
                                                   withFile)

data Argument
  = AudioDir  FilePath
  | JammitDir FilePath
  | SongFile  FilePath
  deriving (Eq, Ord, Show, Read)

optDescrs :: [OptDescr Argument]
optDescrs =
  [ Option [] ["audio" ] (ReqArg AudioDir  "DIR" ) "a directory with audio"
  , Option [] ["jammit"] (ReqArg JammitDir "DIR" ) "a directory with Jammit data"
  , Option [] ["song"  ] (ReqArg SongFile  "FILE") "the song YAML file"
  ]

-- | Oracle for an audio file search.
-- The String is the 'show' of a value of type 'AudioFile'.
newtype AudioSearch = AudioSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for a Jammit track search.
-- The Strings are the title and artist.
newtype JammitSearch = JammitSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for an existing MOGG file search.
-- The String is an MD5 hash of the complete MOGG file.
newtype MoggSearch = MoggSearch T.Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

allFiles :: FilePath -> Action [FilePath]
allFiles absolute = do
  entries <- getDirectoryContents absolute
  flip concatMapM entries $ \entry -> do
    let full = absolute </> entry
    isDir <- doesDirectoryExist full
    if  | entry `elem` [".", ".."] -> return []
        | isDir                    -> allFiles full
        | otherwise                -> return [full]

main :: IO ()
main = do
  argv <- getArgs
  defaultJammitDir <- J.findJammitDir
  let (opts, nonopts, _) = getOpt Permute optDescrs argv
  yamlPath <- canonicalizePath $
    fromMaybe "song.yml" $ listToMaybe [ f | SongFile f <- opts ]
  audioDirs <- mapM canonicalizePath $
    takeDirectory yamlPath : [ d | AudioDir d <- opts ]
  jammitDirs <- mapM canonicalizePath $
    maybe id (:) defaultJammitDir [ d | JammitDir d <- opts ]
  songYaml
    <-  readYAMLTree yamlPath
    >>= runReaderT (printStackTraceIO traceJSON)

  -- make sure all audio leaves are defined, catch typos
  let definedLeaves = HM.keys (_audio songYaml) ++ HM.keys (_jammit songYaml)
  forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
    let leaves = case plan of
          EachPlan{..} -> toList _each
          MoggPlan{} -> []
          Plan{..} -> let
            getLeaves = \case
              Named t -> t
              JammitSelect _ t -> t
            in map getLeaves $ concatMap toList [_song, _guitar, _bass, _keys, _drums, _vocal]
    case filter (not . (`elem` definedLeaves)) leaves of
      [] -> return ()
      undefinedLeaves -> fail $
        "Undefined leaves in plan " ++ show planName ++ " audio expression: " ++ show undefinedLeaves

  let audioSearch :: AudioFile -> Action (Maybe FilePath)
      audioSearch aud = do
        genAbsolute <- liftIO $ canonicalizePath "gen/"
        files <- filter (\f -> not $ genAbsolute `isPrefixOf` f)
          <$> concatMapM allFiles audioDirs
        let md5Result = liftIO $ case _md5 aud of
              Nothing -> return Nothing
              Just md5search -> flip findM files $ \f -> do
                (== Just (T.unpack md5search)) <$> audioMD5 f
            lenResult = liftIO $ case _frames aud of
              Nothing -> return Nothing
              Just len -> flip findM files $ \f -> do
                (== Just len) <$> audioLength f
        firstJustM id [md5Result, lenResult]

      jammitSearch :: JammitTrack -> Action [(J.AudioPart, FilePath)]
      jammitSearch jmt = do
        let title  = fromMaybe (_title  $ _metadata songYaml) $ _jammitTitle  jmt
            artist = fromMaybe (_artist $ _metadata songYaml) $ _jammitArtist jmt
        lib <- liftIO $ concatMapM J.loadLibrary jammitDirs
        return $ J.getAudioParts
          $ J.exactSearchBy J.title  (T.unpack title )
          $ J.exactSearchBy J.artist (T.unpack artist) lib

      moggSearch :: T.Text -> Action (Maybe FilePath)
      moggSearch md5search = do
        genAbsolute <- liftIO $ canonicalizePath "gen/"
        files <- filter (\f -> not $ genAbsolute `isPrefixOf` f)
          <$> concatMapM allFiles audioDirs
        flip findM files $ \f -> case takeExtension f of
          ".mogg" -> do
            md5 <- liftIO $ show . MD5.md5 <$> BL.readFile f
            return $ T.unpack md5search == md5
          _ -> return False

  setCurrentDirectory $ takeDirectory yamlPath
  shakeArgsWith shakeOptions (map (fmap $ const (Right ())) optDescrs) $ \_ _ -> return $ Just $ do

    audioOracle  <- addOracle $ \(AudioSearch  s) -> audioSearch $ read s
    jammitOracle <- addOracle $ \(JammitSearch s) -> fmap show $ jammitSearch $ read s
    moggOracle   <- addOracle $ \(MoggSearch   s) -> moggSearch s

    phony "yaml"   $ liftIO $ print songYaml
    phony "audio"  $ liftIO $ print audioDirs
    phony "jammit" $ liftIO $ print jammitDirs
    phony "clean"  $ cmd "rm -rf gen"

    let audioPath :: T.Text -> FilePath
        audioPath name = "gen/audio" </> T.unpack name <.> "wav"
        jammitPath :: T.Text -> J.AudioPart -> FilePath
        jammitPath name (J.Only part)
          = "gen/jammit" </> T.unpack name </> "only" </> map toLower (drop 4 $ show part) <.> "wav"
        jammitPath name (J.Without inst)
          = "gen/jammit" </> T.unpack name </> "without" </> map toLower (show inst) <.> "wav"

    -- Find and convert all audio files into the work directory
    forM_ (HM.toList $ _audio songYaml) $ \(audioName, audioQuery) -> do
      audioPath audioName %> \out -> do
        putNormal $ "Looking for the audio file named " ++ show audioName
        result <- audioOracle $ AudioSearch $ show audioQuery
        case result of
          Nothing -> fail $ "Couldn't find a necessary audio file for query: " ++ show audioQuery
          Just fp -> buildAudio (Input fp) out

    -- Find and convert all Jammit audio into the work directory
    let jammitAudioParts = map J.Only    [minBound .. maxBound]
                        ++ map J.Without [minBound .. maxBound]
    forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) -> do
      forM_ jammitAudioParts $ \audpart -> do
        jammitPath jammitName audpart %> \out -> do
          putNormal $ "Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
          result <- fmap read $ jammitOracle $ JammitSearch $ show jammitQuery
          case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
            jcfx : _ -> do
              putNormal $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
              liftIO $ J.runAudio [jcfx] [] out
            []       -> fail "Couldn't find a necessary Jammit track"

    -- Looking up single audio files and Jammit parts in the work directory
    let manualLeaf :: AudioInput -> Action (Audio Duration FilePath)
        manualLeaf (Named name) = case HM.lookup name $ _audio songYaml of
          Just audioQuery -> return $ let
            maybeResample = case _rate audioQuery of
              Nothing -> Resample
              Just _  -> id -- if rate is specified, don't auto-resample
            in maybeResample $ Input $ audioPath name
          Nothing -> fail $ "Couldn't find an audio source named " ++ show name
        manualLeaf (JammitSelect audpart name) = case HM.lookup name $ _jammit songYaml of
          Just _  -> return $ Input $ jammitPath name audpart
          Nothing -> fail $ "Couldn't find a Jammit source named " ++ show name

    -- The "auto" mode of Jammit audio assignment, using EachPlan
    let autoLeaf :: Maybe J.Instrument -> T.Text -> Action (Audio Duration FilePath)
        autoLeaf minst name = do
          case HM.lookup name $ _jammit songYaml of
            Nothing -> manualLeaf $ Named name
            Just jmtQuery -> do
              result <- fmap read $ jammitOracle $ JammitSearch $ show jmtQuery
              let _ = result :: [(J.AudioPart, FilePath)]
              let backs = concat
                    [ [J.Drums    | _hasDrums  $ _instruments songYaml]
                    , [J.Bass     | _hasBass   $ _instruments songYaml]
                    , [J.Guitar   | _hasGuitar $ _instruments songYaml]
                    , [J.Keyboard | hasAnyKeys $ _instruments songYaml]
                    , [J.Vocal    | _hasVocal   (_instruments songYaml) /= Vocal0]
                    ]
                  -- audio that is used in the song and bought by the user
                  boughtInstrumentParts :: J.Instrument -> [FilePath]
                  boughtInstrumentParts inst = do
                    guard $ inst `elem` backs
                    J.Only part <- nub $ map fst result
                    guard $ J.partToInstrument part == inst
                    return $ jammitPath name $ J.Only part
                  mixOrStereo []    = Silence 2 $ Frames 0
                  mixOrStereo files = Mix $ map Input files
              case minst of
                Just inst -> return $ mixOrStereo $ boughtInstrumentParts inst
                Nothing -> case filter (\inst -> J.Without inst `elem` map fst result) backs of
                  []       -> fail "No charted instruments with Jammit tracks found"
                  back : _ -> return $ let
                    negative = mixOrStereo $ do
                      otherInstrument <- filter (/= back) backs
                      boughtInstrumentParts otherInstrument
                    in Mix [Input $ jammitPath name $ J.Without back, Gain (-1) negative]

    -- Cover art
    let loadRGB8 = do
          let img = _fileAlbumArt $ _metadata songYaml
          need [img]
          res <- liftIO $ readImage img
          case res of
            Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
            Right dyn -> return $ anyToRGB8 dyn
    "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleBilinear 256 256
    "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleBilinear 256 256
    "gen/cover.dds" %> \out -> loadRGB8 >>= liftIO . writeDDS    out . scaleBilinear 256 256
    "gen/cover.png_xbox" %> \out -> do
      let dds = out -<.> "dds"
      need [dds]
      b <- liftIO $ B.readFile dds
      let header =
            [ 0x01, 0x04, 0x08, 0x00, 0x00, 0x00, 0x04, 0x00
            , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
            , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
            ]
          bytes = B.unpack $ B.drop 0x80 b
          flipPairs (x : y : xs) = y : x : flipPairs xs
          flipPairs _ = []
          b' = B.pack $ header ++ flipPairs bytes
      liftIO $ B.writeFile out b'

    -- The Markdown README file, for GitHub purposes
    "README.md" %> \out -> liftIO $ writeFile out $ execWriter $ do
      let escape = concatMap $ \c -> if c `elem` "\\`*_{}[]()#+-.!"
            then ['\\', c]
            else [c]
          line str = tell $ str ++ "\n"
      line $ "# " ++ escape (T.unpack $ _title $ _metadata songYaml)
      line ""
      line $ "## " ++ escape (T.unpack $ _artist $ _metadata songYaml)
      line ""
      line "Instruments:"
      line ""
      let diffString f dm = case f $ _difficulty $ _metadata songYaml of
            Just (Rank rank) -> g $ rankToTier dm rank
            Just (Tier tier) -> g tier
            Nothing -> ""
            where g = \case
                    1 -> " ⚫️⚫️⚫️⚫️⚫️"
                    2 -> " ⚪️⚫️⚫️⚫️⚫️"
                    3 -> " ⚪️⚪️⚫️⚫️⚫️"
                    4 -> " ⚪️⚪️⚪️⚫️⚫️"
                    5 -> " ⚪️⚪️⚪️⚪️⚫️"
                    6 -> " ⚪️⚪️⚪️⚪️⚪️"
                    7 -> " 😈😈😈😈😈"
                    _ -> ""
      when (_hasDrums $ _instruments songYaml) $ do
        let titleDir  = takeFileName $ takeDirectory yamlPath
            artistDir = takeFileName $ takeDirectory $ takeDirectory yamlPath
            link = "http://pages.cs.wisc.edu/~tolly/customs/?title=" ++ titleDir ++ "&artist=" ++ artistDir
        line $ "  * (Pro) Drums [(preview)](" ++ link ++ ")" ++ diffString _difficultyDrums drumsDiffMap
      when (_hasBass    $ _instruments songYaml) $ line $ "  * Bass"     ++ diffString _difficultyBass bassDiffMap
      when (_hasGuitar  $ _instruments songYaml) $ line $ "  * Guitar"   ++ diffString _difficultyGuitar guitarDiffMap
      when (_hasKeys    $ _instruments songYaml) $ line $ "  * Keys"     ++ diffString _difficultyKeys keysDiffMap
      when (_hasProKeys $ _instruments songYaml) $ line $ "  * Pro Keys" ++ diffString _difficultyProKeys keysDiffMap
      case _hasVocal $ _instruments songYaml of
        Vocal0 -> return ()
        Vocal1 -> line $ "  * Vocals (1)" ++ diffString _difficultyVocal vocalDiffMap
        Vocal2 -> line $ "  * Vocals (2)" ++ diffString _difficultyVocal vocalDiffMap
        Vocal3 -> line $ "  * Vocals (3)" ++ diffString _difficultyVocal vocalDiffMap
      line ""
      line "Supported audio:"
      line ""
      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
        line $ "  * `" ++ T.unpack planName ++ "`" ++ if planName == T.pack "album"
          then " (" ++ escape (T.unpack $ _album $ _metadata songYaml) ++ ")"
          else ""
        line ""
        forM_ (_planComments plan) $ \cmt -> do
          line $ "    * " ++ T.unpack cmt
          line ""
      unless (null $ _comments $ _metadata songYaml) $ do
        line "Notes:"
        line ""
        forM_ (_comments $ _metadata songYaml) $ \cmt -> do
          line $ "  * " ++ T.unpack cmt
          line ""

    forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

      let dir = "gen/plan" </> T.unpack planName

          planAudioPath :: Maybe Instrument -> FilePath
          planAudioPath (Just inst) = dir </> map toLower (show inst) <.> "wav"
          planAudioPath Nothing     = dir </> "song.wav"

      -- Audio files
      case plan of
        Plan{..} -> do
          let locate :: Audio Duration AudioInput -> Action (Audio Duration FilePath)
              locate = fmap join . mapM manualLeaf
              buildPart planPart fout = locate planPart >>= \aud -> buildAudio aud fout
          planAudioPath Nothing       %> buildPart _song
          planAudioPath (Just Guitar) %> buildPart _guitar
          planAudioPath (Just Bass  ) %> buildPart _bass
          planAudioPath (Just Keys  ) %> buildPart _keys
          planAudioPath (Just Drums ) %> buildPart _drums
          planAudioPath (Just Vocal ) %> buildPart _vocal
        EachPlan{..} -> do
          let locate :: Maybe J.Instrument -> Action (Audio Duration FilePath)
              locate inst = fmap join $ mapM (autoLeaf inst) _each
              buildPart maybeInst fout = locate maybeInst >>= \aud -> buildAudio aud fout
          forM_ (Nothing : map Just [minBound .. maxBound]) $ \maybeInst -> do
            planAudioPath maybeInst %> buildPart (fmap jammitInstrument maybeInst)
        MoggPlan{} -> return ()

      -- MIDI files
      let mid2p = dir </> "2p/notes.mid"
          mid1p = dir </> "1p/notes.mid"
          midcountin = dir </> "countin.mid"
      [mid2p, midcountin] &%> \[out, _] -> do
        input <- loadMIDI "notes.mid"
        let extraTracks = "notes-" ++ T.unpack planName ++ ".mid"
            extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
        extra <- doesFileExist extraTracks >>= \b -> if b
          then loadMIDI extraTracks
          else return input{ RBFile.s_tracks = [] }
        tempos <- fmap RBFile.s_tempos $ doesFileExist extraTempo >>= \b -> if b
          then loadMIDI extraTempo
          else return extra
        let trks = RBFile.s_tracks input ++ RBFile.s_tracks extra
            mergeTracks ts = foldr RTB.merge RTB.empty ts
            beatTrack = let
              trk = mergeTracks [ t | RBFile.Beat t <- trks ]
              in RBFile.Beat $ if RTB.null trk
                then U.trackTake (songLength' input) $ makeBeatTrack $ RBFile.s_signatures input
                else trk
            eventsTrack = RBFile.Events $ mergeTracks [ t | RBFile.Events t <- trks ]
            countinTrack = RBFile.Countin $ mergeTracks [ t | RBFile.Countin t <- trks ]
            venueTracks = let
              trk = mergeTracks [ t | RBFile.Venue t <- trks ]
              in guard (not $ RTB.null trk) >> [RBFile.Venue trk]
            drumsTracks = if not $ _hasDrums $ _instruments songYaml
              then []
              else (: []) $ RBFile.PartDrums $ let
                trk = mergeTracks [ t | RBFile.PartDrums t <- trks ]
                mixMode = case plan of
                  MoggPlan{..} -> _drumMix
                  _            -> 0
                in drumMix mixMode $ RBDrums.copyExpert trk
                -- Note: drum mix must be applied *after* copy expert.
                -- Otherwise the automatic EMH mix events prevent copying.
            guitarTracks = if not $ _hasGuitar $ _instruments songYaml
              then []
              else (: []) $ RBFile.copyExpert $ RBFile.PartGuitar
                $ mergeTracks [ t | RBFile.PartGuitar t <- trks ]
            bassTracks = if not $ _hasBass $ _instruments songYaml
              then []
              else (: []) $ RBFile.copyExpert $ RBFile.PartBass
                $ mergeTracks [ t | RBFile.PartBass t <- trks ]
            keysTracks = if not $ hasAnyKeys $ _instruments songYaml
              then []
              else let
                keysDiff diff = mergeTracks [ t | RBFile.PartRealKeys diff' t <- trks, diff == diff' ]
                keysExpert = keysDiff Expert
                keysHard   = keysDiff Hard
                keysMedium = keysDiff Medium
                keysEasy   = keysDiff Easy
                keysExpert' = flip RTB.filter keysExpert $ \case
                  ProKeys.LaneShift _ -> True
                  ProKeys.Glissando _ -> True
                  ProKeys.Trill     _ -> True
                  ProKeys.Note    _ _ -> True
                  _                   -> False
                keysAnim = flip RTB.filter keysExpert $ \case
                  ProKeys.Note _ _ -> True
                  _                -> False
                in  [ RBFile.copyExpert $ RBFile.PartKeys $ if not $ _hasKeys $ _instruments songYaml
                      then expertProKeysToKeys keysExpert -- pro keys to dummy basic keys
                      else mergeTracks [ t | RBFile.PartKeys t <- trks ]
                    , RBFile.PartKeysAnimRH      $ keysAnim
                    , RBFile.PartKeysAnimLH      $ RTB.empty
                    , RBFile.PartRealKeys Expert $ keysExpert
                    , RBFile.PartRealKeys Hard   $ if RTB.null keysHard   then keysExpert' else keysHard
                    , RBFile.PartRealKeys Medium $ if RTB.null keysMedium then keysExpert' else keysMedium
                    , RBFile.PartRealKeys Easy   $ if RTB.null keysEasy   then keysExpert' else keysEasy
                    ]
            vocalTracks = case _hasVocal $ _instruments songYaml of
              Vocal0 -> []
              Vocal1 ->
                [ RBFile.PartVocals partVox'
                ]
              Vocal2 ->
                [ RBFile.PartVocals partVox'
                , RBFile.Harm1 harm1
                , RBFile.Harm2 harm2
                ]
              Vocal3 ->
                [ RBFile.PartVocals partVox'
                , RBFile.Harm1 harm1
                , RBFile.Harm2 harm2
                , RBFile.Harm3 harm3
                ]
              where partVox = mergeTracks [ t | RBFile.PartVocals t <- trks ]
                    partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
                    harm1   = mergeTracks [ t | RBFile.Harm1      t <- trks ]
                    harm2   = mergeTracks [ t | RBFile.Harm2      t <- trks ]
                    harm3   = mergeTracks [ t | RBFile.Harm3      t <- trks ]
        saveMIDI out $ RBFile.Song
          { RBFile.s_tempos = tempos
          , RBFile.s_signatures = RBFile.s_signatures input
          , RBFile.s_tracks = map fixRolls $ concat
            [ [beatTrack]
            , [eventsTrack]
            , venueTracks
            , drumsTracks
            , guitarTracks
            , bassTracks
            , keysTracks
            , vocalTracks
            ]
          }
        saveMIDI midcountin $ RBFile.Song
          { RBFile.s_tempos = tempos
          , RBFile.s_signatures = RBFile.s_signatures input
          , RBFile.s_tracks = [countinTrack]
          }
      mid1p %> \out -> loadMIDI mid2p >>= saveMIDI out . oneFoot 0.18 0.11

      -- -- Guitar rules
      -- dir </> "guitar.mid" %> \out -> do
      --   input <- loadMIDI mid2p
      --   saveMIDI out $ RBFile.playGuitarFile [0, 0, 0, 0, 0, 0] [0, 0, 0, 0] input
      --   -- TODO: support different tunings again

      -- Countin audio, and song+countin files
      dir </> "countin.wav" %> \out -> case _fileCountin $ _metadata songYaml of
        Nothing -> buildAudio (Silence 1 $ Frames 0) out
        Just hit -> makeCountin midcountin hit out
      dir </> "song-countin.wav" %> \out -> do
        let song = Input $ dir </> "song.wav"
            countin = Input $ dir </> "countin.wav"
        buildAudio (Mix [song, countin]) out
      dir </> "song-countin.ogg" %> \out -> do
        buildAudio (Input $ out -<.> "wav") out

      -- Rock Band OGG and MOGG
      let ogg  = dir </> "audio.ogg"
          mogg = dir </> "audio.mogg"
      ogg %> \out -> do
        let parts = concat
              [ [Input $ dir </> "drums.wav"  | _hasDrums  $ _instruments songYaml]
              , [Input $ dir </> "bass.wav"   | _hasBass   $ _instruments songYaml]
              , [Input $ dir </> "guitar.wav" | _hasGuitar $ _instruments songYaml]
              , [Input $ dir </> "keys.wav"   | hasAnyKeys $ _instruments songYaml]
              , [Input $ dir </> "vocal.wav"  | _hasVocal   (_instruments songYaml) /= Vocal0]
              , [Input $ dir </> "song-countin.wav"]
              ]
            audio = Merge $ if length parts == 3
              then parts ++ [Silence 1 $ Seconds 0]
              -- the Silence is to work around oggenc bug:
              -- it assumes 6 channels is 5.1 surround where the last channel
              -- is LFE, so instead we add a silent 7th channel
              else parts
        buildAudio audio out
      mogg %> \out -> do
        case plan of
          MoggPlan{..} -> moggOracle (MoggSearch _moggMD5) >>= \case
            Nothing -> fail "Couldn't find the MOGG file"
            Just f -> copyFile' f out
          _ -> do
            need [ogg]
            oggToMogg ogg out

      -- Low-quality audio files for the online preview app
      let preview ext = dir </> "preview-audio" <.> ext
      preview "wav" %> \out -> do
        let src = dir </> "song-countin.wav"
        need [src]
        cmd "sox" [src, out] "remix 1,2"
      preview "mp3" %> \out -> do
        need [preview "wav"]
        cmd "lame" [preview "wav", out] "-b 16"
      preview "ogg" %> \out -> do
        need [preview "wav"]
        cmd "oggenc -b 16 --resample 16000 -o" [out, preview "wav"]

      let get1xTitle, get2xTitle :: Action String
          get1xTitle = return $ T.unpack $ _title $ _metadata songYaml
          get2xTitle = get2xBass >>= \b -> return $ if b
              then T.unpack (_title $ _metadata songYaml)
              else T.unpack (_title $ _metadata songYaml) ++ " (2x Bass Pedal)"
          get2xBass :: Action Bool
          get2xBass = do
            need [mid1p, mid2p]
            let getFileSize f = liftIO $ withFile f ReadMode hFileSize
            liftM2 (==) (getFileSize mid1p) (getFileSize mid2p)

      let pedalVersions =
            [ (dir </> "1p", get1xTitle, return False)
            , (dir </> "2p", get2xTitle, get2xBass   )
            ]
      forM_ pedalVersions $ \(pedalDir, getTitle, is2xBass) -> do

        let pkg = "onyx" ++ show (hash (pedalDir, _title $ _metadata songYaml, _artist $ _metadata songYaml) `mod` 1000000000)

        -- Rock Band 3 DTA file
        let makeDTA :: Action D.SongPackage
            makeDTA = do
              let mid = pedalDir </> "notes.mid"
              song <- loadMIDI mid
              (pstart, pend) <- previewBounds mid
              len <- songLength mid
              perctype <- getPercType mid
              (numChannels, tracksAssocList, pans, vols) <- case plan of
                MoggPlan{..} -> return $ let
                  tracksAssocList = Map.fromList $ let
                    maybeChannelPair _   []    = []
                    maybeChannelPair str chans = [(str, Right $ D.InParens $ map fromIntegral chans)]
                    in concat
                      [ maybeChannelPair "drum" _moggDrums
                      , maybeChannelPair "guitar" _moggGuitar
                      , maybeChannelPair "bass" _moggBass
                      , maybeChannelPair "keys" _moggKeys
                      , maybeChannelPair "vocals" _moggVocal
                      ]
                  in (length _pans, tracksAssocList, map realToFrac _pans, map realToFrac _vols)
                _ -> do
                  let countChannels file = do
                        need [file]
                        liftIO $ Snd.channels <$> Snd.getFileInfo file
                  numChannels <- countChannels ogg
                  drumsChannels <- if _hasDrums $ _instruments songYaml
                    then countChannels $ dir </> "drums.wav"
                    else return 0
                  bassChannels <- if _hasBass $ _instruments songYaml
                    then countChannels $ dir </> "bass.wav"
                    else return 0
                  guitarChannels <- if _hasGuitar $ _instruments songYaml
                    then countChannels $ dir </> "guitar.wav"
                    else return 0
                  keysChannels <- if _hasGuitar $ _instruments songYaml
                    then countChannels $ dir </> "keys.wav"
                    else return 0
                  vocalChannels <- if _hasVocal (_instruments songYaml) /= Vocal0
                    then countChannels $ dir </> "vocal.wav"
                    else return 0
                  songCountinChannels <- countChannels $ dir </> "song-countin.wav"
                  let channelMapping = concat
                        [ replicate drumsChannels $ Just Drums
                        , replicate bassChannels $ Just Bass
                        , replicate guitarChannels $ Just Guitar
                        , replicate keysChannels $ Just Keys
                        , replicate vocalChannels $ Just Vocal
                        , replicate songCountinChannels Nothing
                        ]
                      tracksAssocList = Map.fromList $ let
                        channelsFor inst = map fst $ filter ((inst ==) . snd) $ zip [0..] channelMapping
                        maybeChannelPair str inst = case channelsFor $ Just inst of
                          []    -> []
                          chans -> [(str, Right $ D.InParens chans)]
                        in concat
                          [ maybeChannelPair "drum" Drums
                          , maybeChannelPair "bass" Bass
                          , maybeChannelPair "guitar" Guitar
                          , maybeChannelPair "keys" Keys
                          , maybeChannelPair "vocals" Vocal
                          ]
                      pans = let
                        allChannels = [drumsChannels, bassChannels, guitarChannels, keysChannels, vocalChannels, songCountinChannels]
                        pansForCount 0 = []
                        pansForCount 1 = [0]
                        pansForCount 2 = [-1, 1]
                        pansForCount n = error $ "FIXME: what pans to assign for " ++ show n ++ "-channel audio?"
                        in concatMap pansForCount allChannels
                      vols = replicate numChannels 0
                  return (numChannels, tracksAssocList, pans, vols)
              title <- getTitle
              return D.SongPackage
                { D.name = title
                , D.artist = T.unpack $ _artist $ _metadata songYaml
                , D.master = True
                , D.songId = Right $ D.Keyword pkg
                , D.song = D.Song
                  { D.songName = "songs/" ++ pkg ++ "/" ++ pkg
                  , D.tracksCount = Nothing
                  , D.tracks = D.InParens $ D.Dict tracksAssocList
                  , D.vocalParts = case _hasVocal $ _instruments songYaml of
                    Vocal0 -> 0
                    Vocal1 -> 1
                    Vocal2 -> 2
                    Vocal3 -> 3
                  , D.pans = D.InParens pans
                  , D.vols = D.InParens vols
                  , D.cores = D.InParens $ take numChannels $ let
                    guitarIndexes = case Map.lookup "guitar" tracksAssocList of
                      Nothing                         -> []
                      Just (Right (D.InParens chans)) -> chans
                      Just (Left  chan              ) -> [chan]
                    in map (\i -> if i `elem` guitarIndexes then 1 else -1) [0..]
                  , D.drumSolo = D.DrumSounds $ D.InParens $ map D.Keyword $ words
                    "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
                  , D.drumFreestyle = D.DrumSounds $ D.InParens $ map D.Keyword $ words
                    "kick.cue snare.cue hat.cue ride.cue crash.cue"
                  , D.crowdChannels = Nothing
                  }
                , D.bank = Just $ Left $ case perctype of
                  Nothing               -> "sfx/tambourine_bank.milo"
                  Just RBVox.Tambourine -> "sfx/tambourine_bank.milo"
                  Just RBVox.Cowbell    -> "sfx/cowbell_bank.milo"
                  Just RBVox.Clap       -> "sfx/handclap_bank.milo"
                , D.drumBank = Nothing
                , D.animTempo = Left D.KTempoMedium
                , D.bandFailCue = Nothing
                , D.songScrollSpeed = 2300
                , D.preview = (fromIntegral pstart, fromIntegral pend)
                , D.songLength = fromIntegral len
                , D.rank = D.Dict $ Map.fromList
                  [ ("drum"     , if _hasDrums  $ _instruments songYaml
                    then case _difficultyDrums $ _difficulty $ _metadata songYaml of
                      Nothing       -> drumsDifficulty song
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank drumsDiffMap t
                    else 0)
                  , ("bass"     , if _hasBass   $ _instruments songYaml
                    then case _difficultyBass $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank bassDiffMap t
                    else 0)
                  , ("guitar"   , if _hasGuitar $ _instruments songYaml
                    then case _difficultyGuitar $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank guitarDiffMap t
                    else 0)
                  , ("vocals"   , if _hasVocal   (_instruments songYaml) /= Vocal0
                    then case _difficultyVocal $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank vocalDiffMap t
                    else 0)
                  , ("keys"     , if hasAnyKeys   $ _instruments songYaml
                    then case _difficultyKeys $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank keysDiffMap t
                    else 0)
                  , ("real_keys", if hasAnyKeys   $ _instruments songYaml
                    then case _difficultyProKeys $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank keysDiffMap t
                    else 0)
                  , ("band"     , case _difficultyBand $ _difficulty $ _metadata songYaml of
                      Nothing       -> 1
                      Just (Rank r) -> r
                      Just (Tier t) -> tierToRank bandDiffMap t
                    )
                  ]
                , D.solo = Just $ D.InParens $ concat
                  [ [D.Keyword "guitar" | hasSolo Guitar song]
                  , [D.Keyword "bass" | hasSolo Bass song]
                  , [D.Keyword "drum" | hasSolo Drums song]
                  , [D.Keyword "keys" | hasSolo Keys song]
                  , [D.Keyword "vocal_percussion" | hasSolo Vocal song]
                  ]
                , D.format = 10
                , D.version = 30
                , D.gameOrigin = D.Keyword "ugc_plus"
                , D.rating = 4
                , D.genre = D.Keyword $ T.unpack $ _genre $ _metadata songYaml
                , D.subGenre = Just $ D.Keyword $ "subgenre_" ++ T.unpack (_subgenre $ _metadata songYaml)
                , D.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
                , D.shortVersion = Nothing
                , D.yearReleased = fromIntegral $ _year $ _metadata songYaml
                , D.albumArt = Just True
                , D.albumName = Just $ T.unpack $ _album $ _metadata songYaml
                , D.albumTrackNumber = Just $ fromIntegral $ _trackNumber $ _metadata songYaml
                , D.vocalTonicNote = toEnum . fromEnum <$> _key (_metadata songYaml)
                , D.songTonality = Nothing
                , D.tuningOffsetCents = Just 0
                , D.realGuitarTuning = Nothing
                , D.realBassTuning = Nothing
                , D.guidePitchVolume = Just (-3)
                , D.encoding = Just $ D.Keyword "utf8"
                }

        -- Rock Band 3 CON package
        let pathDta  = pedalDir </> "rb3/songs/songs.dta"
            pathMid  = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mid")
            pathMogg = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mogg")
            pathPng  = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
            pathMilo = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg <.> ".milo_xbox")
            pathCon  = pedalDir </> "rb3.con"
        pathDta %> \out -> do
          songPkg <- makeDTA
          liftIO $ D.writeFileDTA_utf8 out $ D.serialize $
            D.Dict $ Map.fromList [(pkg, D.toChunks songPkg)]
        pathMid  %> copyFile' (pedalDir </> "notes-magma-export.mid")
        pathMogg %> copyFile' (dir </> "audio.mogg")
        pathPng  %> copyFile' "gen/cover.png_xbox"
        pathMilo %> \out -> liftIO $ B.writeFile out emptyMilo
        pathCon  %> \out -> do
          need [pathDta, pathMid, pathMogg, pathPng, pathMilo]
          rb3pkg
            (T.unpack (_artist $ _metadata songYaml) ++ ": " ++ T.unpack (_title $ _metadata songYaml))
            ("Version: " ++ pedalDir)
            (pedalDir </> "rb3")
            out

        -- Magma RBProj rules
        let makeMagmaProj :: Action Magma.RBProj
            makeMagmaProj = do
              (pstart, _) <- previewBounds $ pedalDir </> "magma/notes.mid"
              perctype    <- getPercType   $ pedalDir </> "magma/notes.mid"
              song <- loadMIDI $ pedalDir </> "notes.mid"
              let silentDryVox = Magma.DryVoxPart
                    { Magma.dryVoxFile = "dryvox.wav"
                    , Magma.dryVoxEnabled = True
                    }
                  emptyDryVox = Magma.DryVoxPart
                    { Magma.dryVoxFile = ""
                    , Magma.dryVoxEnabled = False
                    }
                  disabledFile = Magma.AudioFile
                    { Magma.audioEnabled = False
                    , Magma.channels = 0
                    , Magma.pan = []
                    , Magma.vol = []
                    , Magma.audioFile = ""
                    }
                  monoFile f = Magma.AudioFile
                    { Magma.audioEnabled = True
                    , Magma.channels = 1
                    , Magma.pan = [0]
                    , Magma.vol = [0]
                    , Magma.audioFile = f
                    }
                  stereoFile f = Magma.AudioFile
                    { Magma.audioEnabled = True
                    , Magma.channels = 2
                    , Magma.pan = [-1, 1]
                    , Magma.vol = [0, 0]
                    , Magma.audioFile = f
                    }
                  mixMode = case plan of
                    MoggPlan{..} -> _drumMix
                    _            -> 0
              title <- getTitle
              return Magma.RBProj
                { Magma.project = Magma.Project
                  { Magma.toolVersion = "110411_A"
                  , Magma.projectVersion = 24
                  , Magma.metadata = Magma.Metadata
                    { Magma.songName = title
                    , Magma.artistName = T.unpack $ _artist $ _metadata songYaml
                    , Magma.genre = D.Keyword $ T.unpack $ _genre $ _metadata songYaml
                    , Magma.subGenre = D.Keyword $ "subgenre_" ++ T.unpack (_subgenre $ _metadata songYaml)
                    , Magma.yearReleased = fromIntegral $ _year $ _metadata songYaml
                    , Magma.albumName = T.unpack $ _album $ _metadata songYaml
                    , Magma.author = "Onyxite"
                    , Magma.releaseLabel = "Onyxite Customs"
                    , Magma.country = D.Keyword "ugc_country_us"
                    , Magma.price = 160
                    , Magma.trackNumber = fromIntegral $ _trackNumber $ _metadata songYaml
                    , Magma.hasAlbum = True
                    }
                  , Magma.gamedata = Magma.Gamedata
                    { Magma.previewStartMs = fromIntegral pstart
                    , Magma.rankDrum    = case _difficultyDrums $ _difficulty $ _metadata songYaml of
                      Nothing -> if _hasDrums $ _instruments songYaml
                        then rankToTier drumsDiffMap $ drumsDifficulty song
                        else 1
                      Just (Rank r) -> rankToTier drumsDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankBass    = case _difficultyBass $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier bassDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankGuitar  = case _difficultyGuitar $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier guitarDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankVocals  = case _difficultyVocal $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier vocalDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankKeys    = case _difficultyKeys $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier keysDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankProKeys = case _difficultyProKeys $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier keysDiffMap r
                      Just (Tier t) -> t
                    , Magma.rankBand    = case _difficultyBand $ _difficulty $ _metadata songYaml of
                      Nothing -> 1
                      Just (Rank r) -> rankToTier bandDiffMap r
                      Just (Tier t) -> t
                    , Magma.vocalScrollSpeed = 2300
                    , Magma.animTempo = 32
                    , Magma.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
                    , Magma.vocalPercussion = case perctype of
                      Nothing               -> Magma.Tambourine
                      Just RBVox.Tambourine -> Magma.Tambourine
                      Just RBVox.Cowbell    -> Magma.Cowbell
                      Just RBVox.Clap       -> Magma.Handclap
                    , Magma.vocalParts = case _hasVocal $ _instruments songYaml of
                      Vocal0 -> 0
                      Vocal1 -> 1
                      Vocal2 -> 2
                      Vocal3 -> 3
                    , Magma.guidePitchVolume = -3
                    }
                  , Magma.languages = Magma.Languages
                    { Magma.english  = True
                    , Magma.french   = False
                    , Magma.italian  = False
                    , Magma.spanish  = False
                    , Magma.german   = False
                    , Magma.japanese = False
                    }
                  , Magma.destinationFile = pkg <.> "rba"
                  , Magma.midi = Magma.Midi
                    { Magma.midiFile = "notes.mid"
                    , Magma.autogenTheme = Right ""
                    }
                  , Magma.dryVox = Magma.DryVox
                    { Magma.part0 = if _hasVocal (_instruments songYaml) >= Vocal1
                      then silentDryVox
                      else emptyDryVox
                    , Magma.part1 = if _hasVocal (_instruments songYaml) >= Vocal2
                      then silentDryVox
                      else emptyDryVox
                    , Magma.part2 = if _hasVocal (_instruments songYaml) >= Vocal3
                      then silentDryVox
                      else emptyDryVox
                    , Magma.tuningOffsetCents = 0
                    }
                  , Magma.albumArt = Magma.AlbumArt "cover.bmp"
                  , Magma.tracks = Magma.Tracks
                    { Magma.drumLayout = case mixMode of
                      0 -> Magma.Kit
                      1 -> Magma.KitKickSnare
                      2 -> Magma.KitKickSnare
                      3 -> Magma.KitKickSnare
                      4 -> Magma.KitKick
                      _ -> error $ "Invalid drum mix number: " ++ show mixMode
                    , Magma.drumKit = if _hasDrums $ _instruments songYaml
                      then stereoFile "drums.wav"
                      else disabledFile
                    , Magma.drumKick = case mixMode of
                      _ | not $ _hasDrums $ _instruments songYaml -> disabledFile
                      0 -> disabledFile
                      1 -> monoFile   "kick.wav"
                      2 -> monoFile   "kick.wav"
                      3 -> stereoFile "kick.wav"
                      4 -> monoFile   "kick.wav"
                      _ -> error $ "Invalid drum mix number: " ++ show mixMode
                    , Magma.drumSnare = case mixMode of
                      _ | not $ _hasDrums $ _instruments songYaml -> disabledFile
                      0 -> disabledFile
                      1 -> monoFile   "snare.wav"
                      2 -> stereoFile "snare.wav"
                      3 -> stereoFile "snare.wav"
                      4 -> disabledFile
                      _ -> error $ "Invalid drum mix number: " ++ show mixMode
                    , Magma.bass = if _hasBass $ _instruments songYaml
                      then stereoFile "bass.wav"
                      else disabledFile
                    , Magma.guitar = if _hasGuitar $ _instruments songYaml
                      then stereoFile "guitar.wav"
                      else disabledFile
                    , Magma.vocals = if _hasVocal (_instruments songYaml) /= Vocal0
                      then stereoFile "vocal.wav"
                      else disabledFile
                    , Magma.keys = if hasAnyKeys $ _instruments songYaml
                      then stereoFile "keys.wav"
                      else disabledFile
                    , Magma.backing = stereoFile "song-countin.wav"
                    }
                  }
                }

        -- Magma rules
        do
          let drums  = pedalDir </> "magma/drums.wav"
              bass   = pedalDir </> "magma/bass.wav"
              guitar = pedalDir </> "magma/guitar.wav"
              keys   = pedalDir </> "magma/keys.wav"
              vocal  = pedalDir </> "magma/vocal.wav"
              dryvox = pedalDir </> "magma/dryvox.wav"
              song   = pedalDir </> "magma/song-countin.wav"
              cover  = pedalDir </> "magma/cover.bmp"
              mid    = pedalDir </> "magma/notes.mid"
              proj   = pedalDir </> "magma/magma.rbproj"
              c3     = pedalDir </> "magma/magma.c3"
              setup  = pedalDir </> "magma"
              rba    = pedalDir </> "magma.rba"
              export = pedalDir </> "notes-magma-export.mid"
          drums  %> copyFile' (dir </> "drums.wav" )
          bass   %> copyFile' (dir </> "bass.wav"  )
          guitar %> copyFile' (dir </> "guitar.wav")
          keys   %> copyFile' (dir </> "keys.wav"  )
          vocal  %> copyFile' (dir </> "vocal.wav" )
          dryvox %> \out -> do
            need [mid]
            len <- songLength mid
            let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                audsrc :: (Monad m) => AudioSource m Float
                audsrc = silent (Seconds $ fromIntegral len / 1000) 16000 1
            liftIO $ runResourceT $ sinkSnd out fmt audsrc
          song %> copyFile' (dir </> "song-countin.wav")
          cover %> copyFile' "gen/cover.bmp"
          mid %> copyFile' (pedalDir </> "notes.mid")
          proj %> \out -> do
            p <- makeMagmaProj
            liftIO $ D.writeFileDTA_latin1 out $ D.serialize p
          c3 %> \out -> do
            (pstart, _) <- previewBounds mid
            title <- getTitle
            let isSilence Silence{} = True
                isSilence _         = False
            midi <- loadMIDI mid
            is2x <- is2xBass
            liftIO $ writeFile out $ C3.showC3 $ C3.C3
              { C3.song = T.unpack $ _title $ _metadata songYaml
              , C3.artist = T.unpack $ _artist $ _metadata songYaml
              , C3.album = T.unpack $ _album $ _metadata songYaml
              , C3.customID = pkg
              , C3.version = 1
              , C3.isMaster = True
              , C3.encodingQuality = 5
              , C3.is2xBass = is2x
              , C3.rhythmKeys = False
              , C3.rhythmBass = False
              , C3.karaoke = case plan of
                Plan{..} -> not (isSilence _vocal) && all isSilence
                  [_guitar, _bass, _keys, _drums]
                _ -> False
              , C3.multitrack = case plan of
                Plan{..} -> any (not . isSilence)
                  [_guitar, _bass, _keys, _drums]
                _ -> True
              , C3.convert = False
              , C3.expertOnly = True -- TODO
              , C3.proBassDiff = Nothing
              , C3.proBassTuning = Nothing
              , C3.proGuitarDiff = Nothing
              , C3.proGuitarTuning = Nothing
              , C3.disableProKeys =
                  _hasKeys (_instruments songYaml) && not (_hasProKeys $ _instruments songYaml)
              , C3.tonicNote = _key $ _metadata songYaml
              , C3.tuningCents = 0
              , C3.songRating = 4 -- unrated
              , C3.drumKitSFX = 0 -- default Hard Rock Kit
              , C3.hopoThresholdIndex = 2 -- default 170
              , C3.muteVol = -96
              , C3.vocalMuteVol = -12
              , C3.soloDrums = hasSolo Drums midi
              , C3.soloGuitar = hasSolo Guitar midi
              , C3.soloBass = hasSolo Bass midi
              , C3.soloKeys = hasSolo Keys midi
              , C3.soloVocals = hasSolo Vocal midi
              , C3.songPreview = fromIntegral pstart
              , C3.checkTempoMap = True
              , C3.wiiMode = False
              , C3.doDrumMixEvents = True -- is this a good idea?
              , C3.packageDisplay = T.unpack (_artist $ _metadata songYaml) ++ " - " ++ title
              , C3.packageDescription = "Created with Magma: C3 Roks Edition (forums.customscreators.com) and Onyxite's Build Tool."
              , C3.songAlbumArt = "cover.bmp"
              , C3.packageThumb = ""
              , C3.encodeANSI = True  -- is this right?
              , C3.encodeUTF8 = False -- is this right?
              , C3.useNumericID = False
              , C3.uniqueNumericID = ""
              , C3.uniqueNumericID2x = ""
              , C3.toDoList = C3.defaultToDo
              }
          phony setup $ do
            -- Just make all the Magma prereqs, but don't actually run Magma
            when (_hasDrums  $ _instruments songYaml) $ need [drums ]
            when (_hasBass   $ _instruments songYaml) $ need [bass  ]
            when (_hasGuitar $ _instruments songYaml) $ need [guitar]
            when (hasAnyKeys $ _instruments songYaml) $ need [keys  ]
            when (_hasVocal (_instruments songYaml) /= Vocal0) $ need [vocal, dryvox]
            need [song, cover, mid, proj, c3]
          rba %> \out -> do
            need [setup]
            runMagma proj out
          export %> \out -> do
            need [mid, proj]
            runMagmaMIDI proj out

    want nonopts

getPercType :: FilePath -> Action (Maybe RBVox.PercussionType)
getPercType mid = do
  song <- loadMIDI mid
  let vox = foldr RTB.merge RTB.empty $ mapMaybe getVox $ RBFile.s_tracks song
      getVox (RBFile.PartVocals t) = Just t
      getVox (RBFile.Harm1      t) = Just t
      getVox (RBFile.Harm2      t) = Just t
      getVox (RBFile.Harm3      t) = Just t
      getVox _                     = Nothing
      isPercType (RBVox.PercussionAnimation ptype _) = Just ptype
      isPercType _                                   = Nothing
  return $ listToMaybe $ mapMaybe isPercType $ RTB.getBodies vox

rankToTier :: DiffMap -> Integer -> Integer
rankToTier dm rank = fromIntegral $ length $ takeWhile (<= rank) (1 : dm)

tierToRank :: DiffMap -> Integer -> Integer
tierToRank dm tier = (0 : 1 : dm) !! fromIntegral tier

type DiffMap = [Integer]

drumsDiffMap, vocalDiffMap, bassDiffMap, guitarDiffMap, keysDiffMap,
  proGuitarDiffMap, proBassDiffMap, bandDiffMap :: DiffMap
drumsDiffMap     = [124, 151, 178, 242, 345, 448]
vocalDiffMap     = [132, 175, 218, 279, 353, 427]
bassDiffMap      = [135, 181, 228, 293, 364, 436]
guitarDiffMap    = [139, 176, 221, 267, 333, 409]
keysDiffMap      = [153, 211, 269, 327, 385, 443]
proGuitarDiffMap = [150, 205, 264, 323, 382, 442]
proBassDiffMap   = [150, 208, 267, 325, 384, 442]
bandDiffMap      = [163, 215, 243, 267, 292, 345]

drumsDifficulty :: RBFile.Song U.Beats -> Integer
drumsDifficulty song = let
  drums = foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
  gems = RTB.mapMaybe (\case RBDrums.DiffEvent Expert (RBDrums.Note gem) -> Just gem; _ -> Nothing) drums
  trackTails trk = case RTB.viewL trk of
    Nothing -> []
    Just (_, trk') -> trk : trackTails trk'
  x, spooky :: Double
  x = fromIntegral $ foldl' max 0 $ map (length . RTB.getBodies . U.trackTake (U.Seconds 5))
    $ trackTails $ U.applyTempoTrack (RBFile.s_tempos song) gems
  -- Spooky equation follows. Where did it come from? I...
  -- 1. compiled a list of DT drums tracks and their max notes in a 5 second span
  -- 2. figured out approximately where the tier boundaries should fall, and matched them up with the rank boundaries
  -- 3. fit a curve: np5s(rank) = 2.603e-6*rank**3 - 2.594e-3*rank**2 + 0.91rank - 41.101
  -- 4. had Wolfram Alpha invert that so it's a fn from np5s to rank.
  spooky = (4.73247e14 * sqrt (5.59906e34*x**2 - 7.8792e36*x + 2.77557e38) + 1.11981e32*x - 7.8792e33) ** (1/3)
  rank = round $ 332.181 + 1.19706e-9 * spooky - 5.16941e12 / spooky
  in if rank < 1 then 1 else if rank > 1000 then 1000 else rank

-- | Makes a dummy Basic Keys track, for songs with only Pro Keys charted.
expertProKeysToKeys :: RTB.T U.Beats ProKeys.Event -> RTB.T U.Beats RBFive.Event
expertProKeysToKeys = let
  pkToBasic :: [ProKeys.Event] -> RTB.T U.Beats RBFive.Event
  pkToBasic pk = let
    hasNote    = any (\case ProKeys.Note    _ True  -> True; _ -> False) pk
    hasODTrue  = any (\case ProKeys.Overdrive True  -> True; _ -> False) pk
    hasODFalse = any (\case ProKeys.Overdrive False -> True; _ -> False) pk
    blip diff = RTB.fromPairList
      [ (0     , RBFive.DiffEvent diff $ RBFive.Note True  RBFive.Green)
      , (1 / 32, RBFive.DiffEvent diff $ RBFive.Note False RBFive.Green)
      ]
    in foldr RTB.merge RTB.empty $ concat
      [ [ blip d | d <- [minBound .. maxBound], hasNote ]
      , [ RTB.singleton 0 $ RBFive.Overdrive True  | hasODTrue  ]
      , [ RTB.singleton 0 $ RBFive.Overdrive False | hasODFalse ]
      ]
  in U.trackJoin . fmap pkToBasic . RTB.collectCoincident

hasSolo :: Instrument -> RBFile.Song t -> Bool
hasSolo Guitar song = not $ null $ do
  RBFile.PartGuitar t <- RBFile.s_tracks song
  RBFive.Solo _ <- RTB.getBodies t
  return ()
hasSolo Bass song = not $ null $ do
  RBFile.PartBass t <- RBFile.s_tracks song
  RBFive.Solo _ <- RTB.getBodies t
  return ()
hasSolo Drums song = not $ null $ do
  RBFile.PartDrums t <- RBFile.s_tracks song
  RBDrums.Solo _ <- RTB.getBodies t
  return ()
hasSolo Keys song = not $ null
  $ do
    RBFile.PartKeys t <- RBFile.s_tracks song
    RBFive.Solo _ <- RTB.getBodies t
    return ()
  ++ do
    RBFile.PartRealKeys Expert t <- RBFile.s_tracks song
    ProKeys.Solo _ <- RTB.getBodies t
    return ()
hasSolo Vocal song = not $ null
  $ do
    RBFile.PartVocals t <- RBFile.s_tracks song
    RBVox.Percussion <- RTB.getBodies t
    return ()
  ++ do
    RBFile.Harm1 t <- RBFile.s_tracks song
    RBVox.Percussion <- RTB.getBodies t
    return ()
