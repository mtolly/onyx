{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Audio
import           Config hiding (Difficulty)
import           Image
import           Magma
import           OneFoot
import           Scripts
import           X360
import           YAMLTree
import           Resources (emptyMilo)
import qualified C3
import           Reductions
import qualified OnyxiteDisplay.Process as Proc

import           Reaper.Base (writeRPP)
import qualified Reaper.Build as RPP

import           Codec.Picture
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                       as A
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
import qualified Data.EventList.Absolute.TimeBody as ATB
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (foldl', isPrefixOf, nub, sortOn)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           Development.Shake                hiding ((%>), (&%>), phony)
import qualified Development.Shake                as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RockBand.Common                  (Difficulty (..), Key (..))
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.FiveButton              as RBFive
import qualified RockBand.File                    as RBFile
import qualified RockBand.Vocals                  as RBVox
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Events                  as Events
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.Util                  as U
import           Control.Monad.Trans.StackTrace
import           System.Console.GetOpt
import           System.Directory                 (canonicalizePath,
                                                   setCurrentDirectory)
import           System.Environment               (getArgs)
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Load as Load
import Data.Functor.Identity (runIdentity)

data Argument
  = AudioDir  FilePath
  | SongFile  FilePath
  deriving (Eq, Ord, Show, Read)

optDescrs :: [OptDescr Argument]
optDescrs =
  [ Option [] ["audio"] (ReqArg AudioDir  "DIR" ) "a directory with audio"
  , Option [] ["song" ] (ReqArg SongFile  "FILE") "the song YAML file"
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

newtype GetSongYaml = GetSongYaml ()
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
  case nonopts of
    "build" : buildables -> do

      yamlPath <- canonicalizePath $
        fromMaybe "song.yml" $ listToMaybe [ f | SongFile f <- opts ]
      audioDirs <- mapM canonicalizePath
        $ takeDirectory yamlPath
        : maybe id (:) defaultJammitDir [ d | AudioDir d <- opts ]
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

      let computeChannels :: Audio Duration Int -> Int
          computeChannels = \case
            Silence n _ -> n
            Input n -> n
            Mix auds -> maximum $ map computeChannels auds
            Merge auds -> sum $ map computeChannels auds
            Concatenate auds -> maximum $ map computeChannels auds
            Gain _ aud -> computeChannels aud
            Take _ _ aud -> computeChannels aud
            Drop _ _ aud -> computeChannels aud
            Fade _ _ aud -> computeChannels aud
            Pad _ _ aud -> computeChannels aud
            Resample aud -> computeChannels aud
            Channels chans _ -> length chans

          computeChannelsPlan :: Audio Duration AudioInput -> Int
          computeChannelsPlan = let
            toChannels ai = case ai of
              Named name -> case HM.lookup name $ _audio songYaml of
                Nothing      -> error "panic! audio leaf not found, after it should've been checked"
                Just audfile -> _channels audfile
              JammitSelect _ _ -> 2
            in computeChannels . fmap toChannels

          computeChannelsEachPlan :: Audio Duration T.Text -> Int
          computeChannelsEachPlan = let
            toChannels name = case HM.lookup name $ _jammit songYaml of
              Just _ -> 2
              Nothing -> case HM.lookup name $ _audio songYaml of
                Nothing      -> error "panic! audio leaf not found, after it should've been checked"
                Just audfile -> _channels audfile
            in computeChannels . fmap toChannels

          audioSearch :: AudioFile -> Action (Maybe FilePath)
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
            lib <- liftIO $ concatMapM J.loadLibrary audioDirs
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

        -- Make all rules depend on the parsed song.yml contents
        strSongYaml  <- addOracle $ \(GetSongYaml ()) -> return $ show songYaml
        let (%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
            pat %> f = pat Shake.%> \out -> strSongYaml (GetSongYaml ()) >> f out
            (&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
            pats &%> f = pats Shake.&%> \outs -> strSongYaml (GetSongYaml ()) >> f outs
            phony :: String -> Action () -> Rules ()
            phony s f = Shake.phony s $ strSongYaml (GetSongYaml ())  >> f
            infix 1 %>, &%>

        phony "yaml"  $ liftIO $ print songYaml
        phony "audio" $ liftIO $ print audioDirs
        phony "clean" $ cmd "rm -rf gen"

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

        -- Build a REAPER project
        "notes.RPP" %> \out -> do
          let countin = "gen/plan/album/countin.wav"
              audio = "gen/plan/album/song.wav"
          need [countin, audio, "notes.mid"]
          countinLen <- do
            info <- liftIO $ Snd.getFileInfo countin
            return $ fromIntegral (Snd.frames info) / fromIntegral (Snd.samplerate info)
          audioLen <- do
            info <- liftIO $ Snd.getFileInfo audio
            return $ fromIntegral (Snd.frames info) / fromIntegral (Snd.samplerate info)
          song <- loadMIDI "notes.mid"
          mid <- liftIO $ Load.fromFile "notes.mid"
          let ends = map (U.applyTempoMap $ RBFile.s_tempos song) $ do
                RBFile.Events trk <- RBFile.s_tracks song
                (bts, Events.End) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 trk
                return bts
              midiLen = 5 + case ends of
                [] -> audioLen
                secs : _ -> secs
          liftIO $ writeRPP out $ runIdentity $
            RPP.rpp "REAPER_PROJECT" ["0.1", "5.0/OSX64", "1449358215"] $ do
              RPP.line "VZOOMEX" ["0"]
              RPP.line "SAMPLERATE" ["44100", "0", "0"]
              case mid of
                F.Cons F.Parallel (F.Ticks resn) (tempoTrack : trks) -> do
                  let t_ticks = RPP.processTempoTrack tempoTrack
                      t_beats = RTB.mapTime (\tks -> fromIntegral tks / fromIntegral resn) t_ticks
                      t_secs = U.applyTempoTrack (RBFile.s_tempos song) t_beats
                  RPP.tempoTrack $ RTB.toAbsoluteEventList 0 t_secs
                  let trackOrder :: [(String, Int)]
                      trackOrder = zip
                        [ "PART DRUMS"
                        , "PART BASS"
                        , "PART GUITAR"
                        , "PART VOCALS"
                        , "HARM1"
                        , "HARM2"
                        , "HARM3"
                        , "PART KEYS"
                        , "PART REAL_KEYS_X"
                        , "PART REAL_KEYS_H"
                        , "PART REAL_KEYS_M"
                        , "PART REAL_KEYS_E"
                        , "PART KEYS_ANIM_RH"
                        , "PART KEYS_ANIM_LH"
                        , "EVENTS"
                        , "VENUE"
                        , "BEAT"
                        , "countin"
                        ] [0..]
                  forM_ (sortOn (U.trackName >=> flip lookup trackOrder) trks) $ RPP.track midiLen resn
                  RPP.audio countinLen countin
                  RPP.audio audioLen audio
                _ -> error "Unsupported MIDI format for Reaper project generation"

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
          case T.unpack $ _author $ _metadata songYaml of
            "Onyxite" -> return ()
            auth      -> line $ "Author: " ++ auth
          line ""
          let titleDir  = takeFileName $ takeDirectory yamlPath
              artistDir = takeFileName $ takeDirectory $ takeDirectory yamlPath
              link = "http://pages.cs.wisc.edu/~tolly/customs/?title=" ++ titleDir ++ "&artist=" ++ artistDir
          line $ "[Play in browser](" ++ link ++ ")"
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
          when (_hasDrums   $ _instruments songYaml) $ line $ "  * (Pro) Drums" ++ diffString _difficultyDrums   drumsDiffMap
          when (_hasBass    $ _instruments songYaml) $ line $ "  * Bass"        ++ diffString _difficultyBass    bassDiffMap
          when (_hasGuitar  $ _instruments songYaml) $ line $ "  * Guitar"      ++ diffString _difficultyGuitar  guitarDiffMap
          when (_hasKeys    $ _instruments songYaml) $ line $ "  * Keys"        ++ diffString _difficultyKeys    keysDiffMap
          when (_hasProKeys $ _instruments songYaml) $ line $ "  * Pro Keys"    ++ diffString _difficultyProKeys keysDiffMap
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
          let midPS = dir </> "ps/notes.mid"
              mid2p = dir </> "2p/notes.mid"
              mid1p = dir </> "1p/notes.mid"
              midcountin = dir </> "countin.mid"
              has2p = dir </> "has2p.txt"
              display = dir </> "display.json"
          [midPS, midcountin, mid2p, mid1p, has2p] &%> \_ -> do
            input <- loadMIDI "notes.mid"
            let extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
            tempos <- fmap RBFile.s_tempos $ doesFileExist extraTempo >>= \b -> if b
              then loadMIDI extraTempo
              else return input
            let trks = RBFile.s_tracks input
                mergeTracks ts = foldr RTB.merge RTB.empty ts
                beatTrack = let
                  trk = mergeTracks [ t | RBFile.Beat t <- trks ]
                  in RBFile.Beat $ if RTB.null trk
                    then U.trackTake (songLength' input) $ makeBeatTrack $ RBFile.s_signatures input
                    else trk
                eventsTrack = RBFile.Events eventsRaw
                eventsRaw   = mergeTracks [ t | RBFile.Events t <- trks ]
                countinTrack = RBFile.Countin $ mergeTracks [ t | RBFile.Countin t <- trks ]
                venueTracks = let
                  trk = mergeTracks [ t | RBFile.Venue t <- trks ]
                  in guard (not $ RTB.null trk) >> [RBFile.Venue trk]
                (drumsPS, drums1p, drums2p, has2xNotes) = if not $ _hasDrums $ _instruments songYaml
                  then ([], [], [], False)
                  else let
                    trk = mergeTracks [ t | RBFile.PartDrums t <- trks ]
                    mixMode = case plan of
                      MoggPlan{..} -> _drumMix
                      _            -> 0
                    psKicks = if _auto2xBass $ _metadata songYaml
                      then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
                      else id
                    sections = flip RTB.mapMaybe eventsRaw $ \case
                      Events.PracticeSection s -> Just s
                      _                        -> Nothing
                    ps = psKicks $ drumMix mixMode $ drumsComplete (RBFile.s_signatures input) sections trk
                    -- Note: drumMix must be applied *after* drumsComplete.
                    -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
                    in  ( [RBFile.PartDrums ps]
                        , [RBFile.PartDrums $ rockBand1x ps]
                        , [RBFile.PartDrums $ rockBand2x ps]
                        , any (== RBDrums.Kick2x) ps
                        )
                guitarTracks = if not $ _hasGuitar $ _instruments songYaml
                  then []
                  else (: []) $ RBFile.PartGuitar $ gryboComplete False (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartGuitar t <- trks ]
                bassTracks = if not $ _hasBass $ _instruments songYaml
                  then []
                  else (: []) $ RBFile.PartBass $ gryboComplete False (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartBass t <- trks ]
                keysTracks = if not $ hasAnyKeys $ _instruments songYaml
                  then []
                  else let
                    basicKeys = gryboComplete True (RBFile.s_signatures input) $ if _hasKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartKeys t <- trks ]
                      else expertProKeysToKeys keysExpert
                    keysDiff diff = if _hasProKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartRealKeys diff' t <- trks, diff == diff' ]
                      else keysToProKeys diff $ basicKeys
                    rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
                    keysExpert = keysDiff Expert
                    keysHard   = keysDiff Hard   `orIfNull` pkReduce Hard   (RBFile.s_signatures input) keysOD keysExpert
                    keysMedium = keysDiff Medium `orIfNull` pkReduce Medium (RBFile.s_signatures input) keysOD keysHard
                    keysEasy   = keysDiff Easy   `orIfNull` pkReduce Easy   (RBFile.s_signatures input) keysOD keysMedium
                    keysOD = flip RTB.mapMaybe keysExpert $ \case
                      ProKeys.Overdrive b -> Just b
                      _                   -> Nothing
                    keysAnim = flip RTB.filter keysExpert $ \case
                      ProKeys.Note _ _ -> True
                      _                -> False
                    in  [ RBFile.PartKeys            basicKeys
                        , RBFile.PartKeysAnimRH      keysAnim
                        , RBFile.PartKeysAnimLH      RTB.empty
                        , RBFile.PartRealKeys Expert keysExpert
                        , RBFile.PartRealKeys Hard   keysHard
                        , RBFile.PartRealKeys Medium keysMedium
                        , RBFile.PartRealKeys Easy   keysEasy
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
            forM_ [(midPS, drumsPS), (mid1p, drums1p), (mid2p, drums2p)] $ \(midout, drumsTracks) -> do
              saveMIDI midout $ RBFile.Song
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
            liftIO $ writeFile has2p $ show has2xNotes

          display %> \out -> do
            song <- loadMIDI mid2p
            let gtr = justIf (_hasGuitar $ _instruments songYaml) $ Proc.processFive (Just $ 170 / 480) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartGuitar t <- RBFile.s_tracks song ]
                bass = justIf (_hasBass $ _instruments songYaml) $ Proc.processFive (Just $ 170 / 480) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartBass t <- RBFile.s_tracks song ]
                keys = justIf (_hasKeys $ _instruments songYaml) $ Proc.processFive Nothing (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartKeys t <- RBFile.s_tracks song ]
                drums = justIf (_hasDrums $ _instruments songYaml) $ Proc.processDrums (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                prokeys = justIf (_hasProKeys $ _instruments songYaml) $ Proc.processProKeys (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartRealKeys Expert t <- RBFile.s_tracks song ]
                beat = Proc.processBeat (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.Beat t <- RBFile.s_tracks song ]
                end = U.applyTempoMap (RBFile.s_tempos song) $ songLength' song
                justIf b x = guard b >> Just x
            liftIO $ BL.writeFile out $ A.encode $ Proc.mapTime (realToFrac :: U.Seconds -> Milli)
              $ Proc.Processed gtr bass keys drums prokeys beat end

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
            let parts = map Input $ concat
                  [ [dir </> "drums.wav"  | _hasDrums  $ _instruments songYaml]
                  , [dir </> "bass.wav"   | _hasBass   $ _instruments songYaml]
                  , [dir </> "guitar.wav" | _hasGuitar $ _instruments songYaml]
                  , [dir </> "keys.wav"   | hasAnyKeys $ _instruments songYaml]
                  , [dir </> "vocal.wav"  | _hasVocal   (_instruments songYaml) /= Vocal0]
                  , [dir </> "song-countin.wav"]
                  ]
            buildAudio (Merge parts) out
          mogg %> \out -> do
            case plan of
              MoggPlan{..} -> moggOracle (MoggSearch _moggMD5) >>= \case
                Nothing -> fail "Couldn't find the MOGG file"
                Just f -> copyFile' f out
              _ -> do
                need [ogg]
                oggToMogg ogg out

          -- TODO: song.yml should be able to specify pans/vols for Plan/EachPlan
          let channelCountToPanVol = \case
                0 -> []
                1 -> [(0, 0)]
                2 -> [(-1, 0), (1, 0)]
                c -> error $ "channelCountToPanVol: don't know pans to use for " ++ show c ++ "-channel audio"
          let bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, songPV :: [(Double, Double)]
              bassPV = if _hasBass $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggBass
                  Plan{..} -> channelCountToPanVol $ computeChannelsPlan _bass
                  EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each
                else []
              guitarPV = if _hasGuitar $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggGuitar
                  Plan{..} -> channelCountToPanVol $ computeChannelsPlan _guitar
                  EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each
                else []
              keysPV = if hasAnyKeys $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggKeys
                  Plan{..} -> channelCountToPanVol $ computeChannelsPlan _keys
                  EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each
                else []
              vocalPV = if _hasVocal (_instruments songYaml) /= Vocal0
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggVocal
                  Plan{..} -> channelCountToPanVol $ computeChannelsPlan _vocal
                  EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each
                else []
              drumsPV = if _hasDrums $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ case _drumMix of
                    0 -> _moggDrums
                    1 -> drop 2 _moggDrums
                    2 -> drop 3 _moggDrums
                    3 -> drop 4 _moggDrums
                    4 -> drop 1 _moggDrums
                    n -> error $ "invalid mogg drum mix: " ++ show n
                  Plan{..} -> channelCountToPanVol $ computeChannelsPlan _drums
                  EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each
                else []
              kickPV = if _hasDrums $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ case _drumMix of
                    0 -> []
                    1 -> take 1 _moggDrums
                    2 -> take 1 _moggDrums
                    3 -> take 2 _moggDrums
                    4 -> take 1 _moggDrums
                    n -> error $ "invalid mogg drum mix: " ++ show n
                  Plan{..} -> []
                  EachPlan{..} -> []
                else []
              snarePV = if _hasDrums $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ case _drumMix of
                    0 -> []
                    1 -> take 1 $ drop 1 _moggDrums
                    2 -> take 2 $ drop 1 _moggDrums
                    3 -> take 2 $ drop 2 _moggDrums
                    4 -> []
                    n -> error $ "invalid mogg drum mix: " ++ show n
                  Plan{..} -> []
                  EachPlan{..} -> []
                else []
              songPV = case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ let
                  notSong = concat [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal]
                  in filter (`notElem` notSong) [0 .. length _pans - 1]
                Plan{..} -> channelCountToPanVol $ computeChannelsPlan _song
                EachPlan{..} -> channelCountToPanVol $ computeChannelsEachPlan _each

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

          dir </> "ps/song.ini" %> \out -> do
            (pstart, _) <- previewBounds midPS
            liftIO $ writeFile out $ unlines
              [ "[song]"
              , "artist = " ++ T.unpack (_artist $ _metadata songYaml)
              , "name = " ++ T.unpack (_title $ _metadata songYaml)
              , "album = " ++ T.unpack (_album $ _metadata songYaml)
              , "frets = " ++ T.unpack (_author $ _metadata songYaml)
              , "charter = " ++ T.unpack (_author $ _metadata songYaml)
              , "year = " ++ show (_year $ _metadata songYaml)
              , "genre = " ++ T.unpack (_genre $ _metadata songYaml) -- TODO: capitalize
              , "pro_drums = True"
              , "preview_start_time = " ++ show pstart
              -- TODO: difficulty tiers (from 0 to 6)
              , "diff_guitar = "      ++ if _hasGuitar  $ _instruments songYaml            then "0" else "-1"
              , "diff_bass = "        ++ if _hasBass    $ _instruments songYaml            then "0" else "-1"
              , "diff_drums = "       ++ if _hasDrums   $ _instruments songYaml            then "0" else "-1"
              , "diff_keys = "        ++ if _hasKeys    $ _instruments songYaml            then "0" else "-1"
              , "diff_keys_real = "   ++ if _hasProKeys $ _instruments songYaml            then "0" else "-1"
              , "diff_vocals_harm = " ++ if _hasVocal    (_instruments songYaml) /= Vocal0 then "0" else "-1"
              ]
          dir </> "ps/drums.ogg"  %> buildAudio (Input $ dir </> "drums.wav" )
          dir </> "ps/guitar.ogg" %> buildAudio (Input $ dir </> "guitar.wav")
          dir </> "ps/keys.ogg"   %> buildAudio (Input $ dir </> "keys.wav"  )
          dir </> "ps/rhythm.ogg" %> buildAudio (Input $ dir </> "bass.wav"  )
          dir </> "ps/vocal.ogg"  %> buildAudio (Input $ dir </> "vocals.wav")
          dir </> "ps/song.ogg"   %> buildAudio (Input $ dir </> "song-countin.wav")
          dir </> "ps/album.png"  %> copyFile' "gen/cover.png"
          phony (dir </> "ps") $ do
            need $ map (\f -> dir </> "ps" </> f) $ concat
              [ ["song.ini", "notes.mid", "song.ogg", "album.png"]
              , ["drums.ogg"  | _hasDrums  $ _instruments songYaml]
              , ["guitar.ogg" | _hasGuitar $ _instruments songYaml]
              , ["keys.ogg"   | hasAnyKeys $ _instruments songYaml]
              , ["rhythm.ogg" | _hasBass   $ _instruments songYaml]
              , ["vocal.ogg"  | _hasVocal  (_instruments songYaml) /= Vocal0]
              ]

          let get1xTitle, get2xTitle :: Action String
              get1xTitle = return $ T.unpack $ _title $ _metadata songYaml
              get2xTitle = flip fmap get2xBass $ \b -> if b
                  then T.unpack (_title $ _metadata songYaml) ++ " (2x Bass Pedal)"
                  else T.unpack (_title $ _metadata songYaml)
              get2xBass :: Action Bool
              get2xBass = fmap read $ readFile' has2p

          let pedalVersions =
                [ (dir </> "1p", get1xTitle, return False)
                , (dir </> "2p", get2xTitle, get2xBass   )
                ]
          forM_ pedalVersions $ \(pedalDir, getTitle, is2xBass) -> do

            let pkg = "onyx" ++ show (hash (pedalDir, _title $ _metadata songYaml, _artist $ _metadata songYaml) `mod` 1000000000)

            -- Check for some extra problems that Magma doesn't catch.
            phony (pedalDir </> "problems") $ do
              song <- loadMIDI (pedalDir </> "notes.mid")
              -- Don't have a kick at the start of a drum roll.
              -- It screws up the roll somehow and causes spontaneous misses.
              let drums = foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                  kickSwells = flip RTB.mapMaybe (RTB.collectCoincident drums) $ \evts -> do
                    let kick = RBDrums.DiffEvent Expert $ RBDrums.Note RBDrums.Kick
                        swell1 = RBDrums.SingleRoll True
                        swell2 = RBDrums.DoubleRoll True
                    guard $ elem kick evts && (elem swell1 evts || elem swell2 evts)
                    return ()
              -- Don't have a vocal phrase that ends simultaneous with a lyric event.
              -- In static vocals, this puts the lyric in the wrong phrase.
              let vox = foldr RTB.merge RTB.empty [ t | RBFile.PartVocals t <- RBFile.s_tracks song ]
                  harm1 = foldr RTB.merge RTB.empty [ t | RBFile.Harm1 t <- RBFile.s_tracks song ]
                  harm2 = foldr RTB.merge RTB.empty [ t | RBFile.Harm2 t <- RBFile.s_tracks song ]
                  harm3 = foldr RTB.merge RTB.empty [ t | RBFile.Harm3 t <- RBFile.s_tracks song ]
                  phraseOff = RBVox.Phrase False
                  isLyric = \case RBVox.Lyric _ -> True; _ -> False
                  voxBugs = flip RTB.mapMaybe (RTB.collectCoincident vox) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
                  harm1Bugs = flip RTB.mapMaybe (RTB.collectCoincident harm1) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
                  harm2Bugs = flip RTB.mapMaybe (RTB.collectCoincident $ RTB.merge harm2 harm3) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
              -- Put it all together and show the error positions.
              let showPositions :: RTB.T U.Beats () -> [String]
                  showPositions
                    = map (RBFile.showPosition . U.applyMeasureMap (RBFile.s_signatures song))
                    . ATB.getTimes
                    . RTB.toAbsoluteEventList 0
                  message rtb msg = forM_ (showPositions rtb) $ \pos -> do
                    putNormal $ pos ++ ": " ++ msg
              message kickSwells "kick note can't be simultaneous with start of drum roll"
              message voxBugs "PART VOCALS vocal phrase can't end simultaneous with a lyric"
              message harm1Bugs "HARM1 vocal phrase can't end simultaneous with a lyric"
              message harm2Bugs "HARM2 vocal phrase can't end simultaneous with a (HARM2 or HARM3) lyric"
              unless (all RTB.null [kickSwells, voxBugs, harm1Bugs, harm2Bugs]) $
                fail "At least 1 problem was found in the MIDI."

            -- Rock Band 3 DTA file
            let makeDTA :: Action D.SongPackage
                makeDTA = do
                  let mid = pedalDir </> "notes.mid"
                  song <- loadMIDI mid
                  (pstart, pend) <- previewBounds mid
                  len <- songLength mid
                  perctype <- getPercType mid

                  let channels = concat [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV, songPV]
                      pans = case plan of
                        MoggPlan{..} -> _pans
                        _ -> map fst channels
                      vols = case plan of
                        MoggPlan{..} -> _vols
                        _ -> map snd channels
                      -- I still don't know what cores are...
                      -- All I know is guitar channels are usually (not always) 1 and all others are -1
                      cores = case plan of
                        MoggPlan{..} -> map (\i -> if elem i _moggGuitar then 1 else -1) $ zipWith const [0..] _pans
                        _ -> concat
                          [ map (const (-1)) $ concat [kickPV, snarePV, drumsPV, bassPV]
                          , map (const   1)    guitarPV
                          , map (const (-1)) $ concat [keysPV, vocalPV, songPV]
                          ]
                      tracksAssocList = Map.fromList $ case plan of
                        MoggPlan{..} -> let
                          maybeChannelPair _   []    = []
                          maybeChannelPair str chans = [(str, Right $ D.InParens $ map fromIntegral chans)]
                          in concat
                            [ maybeChannelPair "drum" _moggDrums
                            , maybeChannelPair "guitar" _moggGuitar
                            , maybeChannelPair "bass" _moggBass
                            , maybeChannelPair "keys" _moggKeys
                            , maybeChannelPair "vocals" _moggVocal
                            ]
                        _ -> let
                          counts =
                            [ ("drum", concat [kickPV, snarePV, drumsPV])
                            , ("bass", bassPV)
                            , ("guitar", guitarPV)
                            , ("keys", keysPV)
                            , ("vocals", vocalPV)
                            ]
                          go _ [] = []
                          go n ((inst, chans) : rest) = case length chans of
                            0 -> go n rest
                            c -> (inst, Right $ D.InParens $ map fromIntegral $ take c [n..]) : go (n + c) rest
                          in go 0 counts

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
                      , D.pans = D.InParens $ map realToFrac pans
                      , D.vols = D.InParens $ map realToFrac vols
                      , D.cores = D.InParens cores
                      -- TODO: different drum kit sounds
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
                    , D.rating = fromIntegral $ fromEnum (_rating $ _metadata songYaml) + 1
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
            pathMid  %> copyFile' (pedalDir </> "notes-magma-added.mid")
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
                      pvFile [] _ = disabledFile
                      pvFile pv f = Magma.AudioFile
                        { Magma.audioEnabled = True
                        , Magma.channels = fromIntegral $ length pv
                        , Magma.pan = map (realToFrac . fst) pv
                        , Magma.vol = map (realToFrac . snd) pv
                        , Magma.audioFile = f
                        }
                      mixMode = case plan of
                        MoggPlan{..} -> _drumMix
                        _            -> 0 -- TODO: support other mix modes
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
                        , Magma.author = T.unpack $ _author $ _metadata songYaml
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
                        , Magma.autogenTheme = Right $ case _autogenTheme $ _metadata songYaml of
                          AutogenDefault -> "Default.rbtheme"
                          theme -> show theme ++ ".rbtheme"
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
                        , Magma.drumKit = pvFile drumsPV "drums.wav"
                        , Magma.drumKick = pvFile kickPV "kick.wav"
                        , Magma.drumSnare = pvFile snarePV "snare.wav"
                        , Magma.bass = pvFile bassPV "bass.wav"
                        , Magma.guitar = pvFile guitarPV "guitar.wav"
                        , Magma.vocals = pvFile vocalPV "vocal.wav"
                        , Magma.keys = pvFile keysPV "keys.wav"
                        , Magma.backing = pvFile songPV "song-countin.wav"
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
                  export2 = pedalDir </> "notes-magma-added.mid"
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
                  , C3.songRating = fromEnum (_rating $ _metadata songYaml) + 1
                  , C3.drumKitSFX = fromEnum $ _drumKit $ _metadata songYaml
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
                  , C3.uniqueNumericID2X = ""
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
              export2 %> \out -> do
                -- Using Magma's "export MIDI" option overwrites all animations/venue
                -- with autogenerated ones, even if they were actually authored.
                -- So, we now need to readd them back from the user MIDI (if they exist).
                userMid <- loadMIDI mid
                magmaMid <- loadMIDI export
                let reauthor getTrack eventPredicates magmaTrack = let
                      authoredTrack = foldr RTB.merge RTB.empty $ mapMaybe getTrack $ RBFile.s_tracks userMid
                      applyEventFn isEvent t = let
                        authoredEvents = RTB.filter isEvent authoredTrack
                        magmaNoEvents = RTB.filter (not . isEvent) t
                        in if RTB.null authoredEvents then t else RTB.merge authoredEvents magmaNoEvents
                      in foldr applyEventFn magmaTrack eventPredicates
                    fivePredicates =
                      [ \case RBFive.Mood{} -> True; _ -> False
                      , \case RBFive.HandMap{} -> True; _ -> False
                      , \case RBFive.StrumMap{} -> True; _ -> False
                      , \case RBFive.FretPosition{} -> True; _ -> False
                      ]
                saveMIDI out $ magmaMid
                  { RBFile.s_tracks = flip map (RBFile.s_tracks magmaMid) $ \case
                    RBFile.PartDrums t -> RBFile.PartDrums $ let
                      getTrack = \case RBFile.PartDrums trk -> Just trk; _ -> Nothing
                      isMood = \case RBDrums.Mood{} -> True; _ -> False
                      isAnim = \case RBDrums.Animation{} -> True; _ -> False
                      in reauthor getTrack [isMood, isAnim] t
                    RBFile.PartGuitar t -> RBFile.PartGuitar $ let
                      getTrack = \case RBFile.PartGuitar trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartBass t -> RBFile.PartBass $ let
                      getTrack = \case RBFile.PartBass trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartKeys       t -> RBFile.PartKeys $ let
                      getTrack = \case RBFile.PartKeys trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartVocals t -> RBFile.PartVocals $ let
                      getTrack = \case RBFile.PartVocals trk -> Just trk; _ -> Nothing
                      isMood = \case RBVox.Mood{} -> True; _ -> False
                      in reauthor getTrack [isMood] t
                    RBFile.Venue t -> RBFile.Venue $ let
                      getTrack = \case RBFile.Venue trk -> Just trk; _ -> Nothing
                      -- TODO: split up camera and lighting so you can author just one
                      in reauthor getTrack [const True] t
                    -- Stuff "export midi" doesn't overwrite:
                    -- PART KEYS_ANIM_LH/RH
                    -- Crowd stuff in EVENTS
                    t -> t
                  }

        want buildables

    [] -> return ()
    _ -> error "Invalid command"

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
    hasNote     = any (\case ProKeys.Note      True _ -> True; _ -> False) pk
    hasODTrue   = any (== ProKeys.Overdrive True ) pk
    hasODFalse  = any (== ProKeys.Overdrive False) pk
    hasBRETrue  = any (== ProKeys.BRE       True ) pk
    hasBREFalse = any (== ProKeys.BRE       False) pk
    blip diff = RTB.fromPairList
      [ (0     , RBFive.DiffEvent diff $ RBFive.Note True  RBFive.Green)
      , (1 / 32, RBFive.DiffEvent diff $ RBFive.Note False RBFive.Green)
      ]
    in foldr RTB.merge RTB.empty $ concat
      [ [ blip d | d <- [minBound .. maxBound], hasNote ]
      , [ RTB.singleton 0 $ RBFive.Overdrive True  | hasODTrue   ]
      , [ RTB.singleton 0 $ RBFive.Overdrive False | hasODFalse  ]
      , [ RTB.singleton 0 $ RBFive.BRE       True  | hasBRETrue  ]
      , [ RTB.singleton 0 $ RBFive.BRE       False | hasBREFalse ]
      ]
  in U.trackJoin . fmap pkToBasic . RTB.collectCoincident

-- | Makes a Pro Keys track, for songs with only Basic Keys charted.
keysToProKeys :: Difficulty -> RTB.T U.Beats RBFive.Event -> RTB.T U.Beats ProKeys.Event
keysToProKeys d = let
  basicToPK = \case
    RBFive.DiffEvent d' (RBFive.Note b c) | d == d' ->
      Just $ ProKeys.Note b $ ProKeys.BlueGreen $ case c of
        RBFive.Green  -> C
        RBFive.Red    -> D
        RBFive.Yellow -> E
        RBFive.Blue   -> F
        RBFive.Orange -> G
    RBFive.Overdrive b | d == Expert -> Just $ ProKeys.Overdrive b
    RBFive.Solo      b | d == Expert -> Just $ ProKeys.Solo      b
    RBFive.BRE       b | d == Expert -> Just $ ProKeys.BRE       b
    RBFive.Trill     b               -> Just $ ProKeys.Trill     b
    _                                -> Nothing
  in RTB.cons 0 (ProKeys.LaneShift ProKeys.RangeA) . RTB.mapMaybe basicToPK

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
