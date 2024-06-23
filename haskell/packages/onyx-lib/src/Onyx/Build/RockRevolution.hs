{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Build.RockRevolution (rrRules, intToRRSongID) where

import           Control.Monad.Codec
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Random.Strict       (MonadRandom, evalRand,
                                                    mkStdGen, uniform)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict  (execState)
import           Data.Binary.Put                   (runPut)
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as B8
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (toUpper)
import qualified Data.Conduit.Audio                as CA
import qualified Data.Conduit.Audio.LAME.Binding   as L
import qualified Data.EventList.Relative.TimeBody  as RTB
import           Data.Foldable                     (toList)
import           Data.Functor.Identity             (Identity)
import           Data.Hashable                     (hash)
import           Data.Int                          (Int16)
import           Data.List                         (sort)
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust, listToMaybe)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Data.Version                      (showVersion)
import           Data.Word                         (Word32)
import           Development.Shake                 hiding (phony, (%>))
import           Development.Shake.FilePath
import qualified Numeric.NonNegative.Class         as NNC
import           Onyx.Audio                        (clampIfSilent,
                                                    sinkMP3PadWithHandle)
import           Onyx.Audio.FSB
import           Onyx.Audio.FSB.FEV
import           Onyx.Background
import           Onyx.Build.Common
import           Onyx.Difficulty                   (bassDiffMap,
                                                    computeDrumRank,
                                                    computeFiveRank,
                                                    drumsDiffMap, guitarDiffMap)
import           Onyx.FFMPEG                       (ffSource)
import           Onyx.Guitar                       (applyStatus, gh3LegalHOPOs,
                                                    noExtendedSustains',
                                                    noOpenNotes,
                                                    standardBlipThreshold)
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.MIDI.Common                  (Difficulty (..), Edge (..),
                                                    Key (..), Mood (..),
                                                    RB3Instrument (..),
                                                    StrumHOPOTap (..),
                                                    blipEdgesRBNice,
                                                    blipEdgesRB_, pattern RNil,
                                                    pattern Wait,
                                                    splitEdgesSimple)
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Beat              (BeatTrack (..))
import qualified Onyx.MIDI.Track.Drums             as D
import qualified Onyx.MIDI.Track.Drums.Elite       as E
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File              as F
import qualified Onyx.MIDI.Track.FiveFret          as Five
import           Onyx.MIDI.Track.Venue
import           Onyx.Mode
import           Onyx.PlayStation.NPData           (npdContentID, packNPData,
                                                    rockRevolutionEdatConfig)
import           Onyx.PlayStation.PKG
import           Onyx.Project                      hiding (Difficulty)
import           Onyx.Reductions                   (completeDrumResult,
                                                    completeFiveResult)
import           Onyx.Resources                    (getResourcesPath)
import           Onyx.RockRevolution.MIDI
import           Onyx.StackTrace
import           Onyx.Util.Files                   (shortWindowsPath)
import           Onyx.Util.Handle                  (Folder (..))
import           Onyx.Util.Text.Decode             (encodeLatin1)
import           Onyx.Util.Text.Transform          (replaceCharsRB)
import           Onyx.WebPlayer                    (findTremolos, findTrills,
                                                    laneDifficulty)
import           Onyx.Xbox.STFS                    (rrpkg)
import           Paths_onyx_lib                    (version)
import qualified Sound.MIDI.File                   as File
import qualified Sound.MIDI.File.Event             as Event
import qualified Sound.MIDI.File.Save              as Save
import qualified Sound.MIDI.Util                   as U
import           System.IO                         (IOMode (..), hFileSize,
                                                    withBinaryFile)

intToRRSongID :: Int -> T.Text
intToRRSongID n = T.pack $ reverse $ take 4 $ reverse (show n) <> repeat '0'

rrRules :: BuildInfo -> FilePath -> TargetRR FilePath -> QueueLog Rules ()
rrRules buildInfo dir rr = do

  let songYaml = biSongYaml buildInfo
      _rel = biRelative buildInfo
      gen = biGen buildInfo
      metadata = getTargetMetadata songYaml $ RR rr
      key = intToRRSongID $ fromMaybe 2000 rr.songID
      str = T.unpack key
      bytes = TE.encodeUtf8 key

  -- not sure exactly which chars supported but probably latin-1-ish.
  -- nonsupported chars like japanese don't break anything at least,
  -- they just don't display anything
  title  <- replaceCharsRB False $ targetTitle songYaml $ RR rr
  artist <- replaceCharsRB False $ getArtist metadata

  (planName, plan) <- case getPlan rr.common.plan songYaml of
    Nothing   -> fail $ "Couldn't locate a plan for this target: " ++ show rr
    Just pair -> return pair
  let planDir = gen $ "plan" </> T.unpack planName

  let file f = dir </> "files/data" </> f
      makeStringsBin strs fout = stackIO $ B.writeFile fout
        $ B.concat $ map (\t -> TE.encodeUtf8 t <> B.singleton 0) strs

  let songBinEnglish = file $ "English_s" <> str <> "_Strings.bin"
      songBinFrench  = file $ "French_s"  <> str <> "_Strings.bin"
      songBinSpanish = file $ "Spanish_s" <> str <> "_Strings.bin"
  -- songBinEnglish written with midis below
  songBinFrench  %> shk . copyFile' songBinEnglish
  songBinSpanish %> shk . copyFile' songBinEnglish

  let sFevSeq = file $ "s" <> str <> ".fev.seq"
      sFsbSeq = file $ "s" <> str <> ".fsb.seq"
      sDrumsFsbSeq = file $ "s" <> str <> "_Drums.fsb.seq"
  forM_ [sFevSeq, sFsbSeq, sDrumsFsbSeq] $ \f -> do
    f %> \out -> stackIO $ B.writeFile out (B.pack [1, 0, 0, 0])

  let rrPartDrums  = getPart rr.drums  songYaml >>= anyDrums
      rrPartGuitar = getPart rr.guitar songYaml >>= anyFiveFret
      rrPartBass   = getPart rr.bass   songYaml >>= anyFiveFret
      rrVocals     = getPart rr.vocal  songYaml >>= (.vocal)

      bass01   = file $ "s" <> str <> "_bass_01.mid"
      bass02   = file $ "s" <> str <> "_bass_02.mid"
      bass03   = file $ "s" <> str <> "_bass_03.mid"
      bass04   = file $ "s" <> str <> "_bass_04.mid"
      bass05   = file $ "s" <> str <> "_bass_05.mid"
      drums01  = file $ "s" <> str <> "_drums_01.mid"
      drums02  = file $ "s" <> str <> "_drums_02.mid"
      drums03  = file $ "s" <> str <> "_drums_03.mid"
      drums04  = file $ "s" <> str <> "_drums_04.mid"
      drums05  = file $ "s" <> str <> "_drums_05.mid"
      guitar01 = file $ "s" <> str <> "_guitar_01.mid"
      guitar02 = file $ "s" <> str <> "_guitar_02.mid"
      guitar03 = file $ "s" <> str <> "_guitar_03.mid"
      guitar04 = file $ "s" <> str <> "_guitar_04.mid"
      guitar05 = file $ "s" <> str <> "_guitar_05.mid"
      control  = file $ "s" <> str <> "_control.mid"
      allMidis =
        [   bass01,   bass02,   bass03,   bass04,   bass05
        ,  drums01,  drums02,  drums03,  drums04,  drums05
        , guitar01, guitar02, guitar03, guitar04, guitar05
        , control
        ]

      maxScores = dir </> "max-scores.lua"

  let loadOnyxMidi :: Staction (F.Song (F.OnyxFile U.Beats))
      loadOnyxMidi = F.shakeMIDI $ planDir </> "processed.mid"

  let pathPad = dir </> "pad.txt"
      readPad :: Staction Int
      readPad = shk $ read <$> readFile' pathPad

  (songBinEnglish : maxScores : pathPad : allMidis) %> \_ -> do
    mid <- applyTargetMIDI rr.common <$> loadOnyxMidi
    endTime <- case mid.s_tracks.onyxEvents.eventsEnd of
      Wait dt _ _ -> return dt
      RNil        -> fatal "panic! no [end] in processed midi"
    let input partName = ModeInput
          { tempo  = F.s_tempos mid
          , events = F.onyxEvents $ F.s_tracks mid
          , part   = F.getFlexPart partName $ F.s_tracks mid
          }
        guitar = flip fmap rrPartGuitar $ \builder ->
          completeFiveResult False mid.s_signatures $ builder FiveTypeGuitar $ input rr.guitar
        bass   = flip fmap rrPartBass $ \builder ->
          completeFiveResult False mid.s_signatures $ builder FiveTypeGuitar $ input rr.bass
        drums  = flip fmap rrPartDrums $ \builder ->
          completeDrumResult mid.s_signatures mid.s_tracks.onyxEvents.eventsSections $ builder drumTarget $ input rr.drums
        drumTarget = if rr.is2xBassPedal then DrumTargetRB1x else DrumTargetRB2x

    let startNotes = concat
          [ toList (guitar >>= Map.lookup Expert . (.notes)) >>= take 1 . RTB.getTimes
          , toList (bass   >>= Map.lookup Expert . (.notes)) >>= take 1 . RTB.getTimes
          , toList (drums  >>= Map.lookup Expert . (.notes)) >>= take 1 . RTB.getTimes
          ]
        firstNoteSeconds = case startNotes of
          n : ns -> U.applyTempoMap mid.s_tempos $ foldr min n ns
          []     -> 10 -- shouldn't happen
        padSeconds = max 0 $ ceiling $ 3 - (realToFrac firstNoteSeconds :: Rational)
        adjustMidiTiming
          = (if True -- TODO determine if we should do this on ps3.
            -- decrease tempo at start of midi to account for (~50 ms?) mp3 delay
            then \m -> m { F.s_tempos = applyRR360MP3Hack m.s_tempos }
            else id)
          . F.padRawTrackFile padSeconds
    stackIO $ B.writeFile pathPad $ B8.pack $ show (padSeconds :: Int)

    let toTrackMidi :: (ParseTrack f) => f U.Beats -> F.Song (RTB.T U.Beats (Event.T B.ByteString))
        toTrackMidi trk = adjustMidiTiming mid
          { F.s_tracks = fmap (fmap encodeLatin1) $ execState (codecOut (forcePure parseTrack) trk) RTB.empty
          }

        forcePure
          :: TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
          -> TrackCodec (PureLog Identity) U.Beats (trk U.Beats)
        forcePure = id

        emptyMid = mid { F.s_tracks = RTB.empty }

        makeGB :: Difficulty -> Maybe FiveResult -> (File.T B.ByteString, Int)
        makeGB _    Nothing       = (F.showMixedMIDI emptyMid, 0)
        makeGB diff (Just result) = let
          gems = fromMaybe RTB.empty $ Map.lookup diff result.notes
          rrDiff = makeRRFiveDifficulty result gems
          numNotes
            = length (RTB.collectCoincident rrDiff.rrfStrums)
            + length (RTB.collectCoincident rrDiff.rrfHOPOs )
          in (F.showMixedMIDI $ toTrackMidi rrDiff
            { rrfSolo = result.other.fiveSolo
            }, numNotes)

        makeDrums :: Difficulty -> (File.T B.ByteString, Int)
        makeDrums diff = let
          nonElite =
            (if rr.proTo4 then makeRRDrumDifficulty4Lane else makeRRDrumDifficulty6Lane)
            diff
            (fromMaybe RTB.empty $ drums >>= \dr -> Map.lookup diff dr.notes)
            (maybe mempty (.other) drums)
            mid.s_tracks.onyxEvents
          rrDiff = case drums >>= (.eliteDrums) of
            Just elite | not rr.proTo4 -> case E.splitFlams mid.s_tempos $ E.getDifficulty (Just diff) elite of
              eliteNotes | not $ RTB.null eliteNotes
                -> makeRRDrumDifficultyElite eliteNotes elite mid.s_tracks.onyxEvents
              _ -> nonElite
            _ -> nonElite
          in (F.showMixedMIDI $ toTrackMidi rrDiff
            { rrdSolo = maybe RTB.empty (\res -> res.other.drumSolo) drums
            }, length rrDiff.rrdGems)

        venue = flip evalRand (mkStdGen $ hash key) $ getVenue
          VenueTargetRB3
          (Map.fromList $ concat
            [ [ (Guitar, rr.guitar) | isJust guitar ]
            , [ (Bass  , rr.bass  ) | isJust bass   ]
            , [ (Drums , rr.drums ) | isJust drums  ]
            ])
          mid.s_tempos
          endTime
          mid
        camera = RTB.mapMaybe (listToMaybe . catMaybes)
          $ flip evalRand (mkStdGen $ hash key)
          $ mapM (\cuts -> mapM rrCameraCut $ reverse $ sort cuts)
          $ RTB.collectCoincident venue.venueCameraRB3
        moodFor part = maybe RTB.empty (getMoods mid.s_tempos endTime)
          $ Map.lookup part mid.s_tracks.onyxParts

        (sections, customSections) = makeRRSections mid.s_tracks.onyxEvents.eventsSections

    makeStringsBin
      ( artist
      : title
      : customSections
      ) songBinEnglish

    let midiOut path (m, numNotes) = do
          stackIO $ Save.toFile path m
          return numNotes

    numNotesBass <- sequence
      [ midiOut bass01 $ makeGB Easy   bass
      , midiOut bass02 $ makeGB Easy   bass
      , midiOut bass03 $ makeGB Medium bass
      , midiOut bass04 $ makeGB Hard   bass
      , midiOut bass05 $ makeGB Expert bass
      ]

    numNotesGuitar <- sequence
      [ midiOut guitar01 $ makeGB Easy   guitar
      , midiOut guitar02 $ makeGB Easy   guitar
      , midiOut guitar03 $ makeGB Medium guitar
      , midiOut guitar04 $ makeGB Hard   guitar
      , midiOut guitar05 $ makeGB Expert guitar
      ]

    numNotesDrums <- sequence
      [ midiOut drums01 $ makeDrums Easy
      , midiOut drums02 $ makeDrums Easy
      , midiOut drums03 $ makeDrums Medium
      , midiOut drums04 $ makeDrums Hard
      , midiOut drums05 $ makeDrums Expert
      ]

    stackIO $ Save.toFile control $ F.showMixedMIDI $ adjustMidiTiming mid
      { F.s_tracks = showRRControl RRControl
        { rrcAnimDrummer   = flip fmap (moodFor rr.drums) $ \case
          -- obviously not great, need to instead translate from drum animations
          Mood_idle          -> 109 -- Drummer_NoPlay
          Mood_idle_realtime -> 109
          Mood_idle_intense  -> 109
          Mood_play          -> 52 -- Drummer_44_HH_Cymbal
          Mood_mellow        -> 52
          Mood_intense       -> 52
          Mood_play_solo     -> 52
        , rrcAnimGuitarist = flip fmap (moodFor rr.guitar) $ \case
          Mood_idle          -> 0 -- Guitar_NoPlay
          Mood_idle_realtime -> 0
          Mood_idle_intense  -> 0
          Mood_mellow        -> 1 -- Guitar_TempoB_Funk
          Mood_play          -> 2 -- Guitar_TempoB_Rock
          Mood_intense       -> 3 -- Guitar_TempoB_Metal
          Mood_play_solo     -> 3
        , rrcAnimBassist   = flip fmap (moodFor rr.guitar) $ \case
          Mood_idle          -> 0 -- Bass_NoPlay
          Mood_idle_realtime -> 0
          Mood_idle_intense  -> 0
          Mood_mellow        -> 1 -- Bass_TempoB_Funk
          Mood_play          -> 2 -- Bass_TempoB_Rock
          Mood_intense       -> 3 -- Bass_TempoB_Metal
          Mood_play_solo     -> 3
        , rrcAnimVocalist  = RTB.empty
        , rrcGemsDrums     = fmap fst $ rrdGems $ makeRRDrumDifficulty4Lane
          Expert
          (fromMaybe RTB.empty $ drums >>= \res -> Map.lookup Expert res.notes)
          (maybe mempty (.other) drums)
          mid.s_tracks.onyxEvents
        , rrcGemsGuitar    = fromMaybe RTB.empty $ do
          res <- guitar
          makeRRFiveControl res <$> Map.lookup Expert res.notes
        , rrcGemsBass      = fromMaybe RTB.empty $ do
          res <- bass
          makeRRFiveControl res <$> Map.lookup Expert res.notes
        , rrcVenue         = camera
        , rrcEnd           = eventsEnd $ F.onyxEvents $ F.s_tracks mid
        , rrcSections      = sections
        -- TODO add half-beats (do these even do anything?)
        , rrcBeat          = fmap Just $ beatLines $ F.onyxBeat $ F.s_tracks mid
        }
      }

    -- these are almost correct but not quite, must be some other thing that affects them
    let guessMaxScores = T.intercalate ", " . map (\n -> T.pack $ show $ n * 150 * 8)
    stackIO $ B.writeFile maxScores $ TE.encodeUtf8 $ T.intercalate "\r\n"
      [   "DrumMaxScore = { 0, " <> guessMaxScores numNotesDrums  <> " }"
      , "GuitarMaxScore = { 0, " <> guessMaxScores numNotesGuitar <> " }"
      ,   "BassMaxScore = { 0, " <> guessMaxScores numNotesBass   <> " }"
      ]

  let songLua = file $ "s" <> str <> ".lua"
  songLua %> \out -> do

    mid <- applyTargetMIDI rr.common <$> loadOnyxMidi
    let (pstart, pend) = previewBounds metadata mid 0 False
        showTimeParts :: Int -> T.Text
        showTimeParts ms = let
          (minutes, afterMinutes) = quotRem ms 60000
          (seconds, milliseconds) = quotRem afterMinutes 1000
          in T.pack $ "{ " <> show minutes <> ", " <> show seconds <> ", " <> show milliseconds <> " }"
    endMS <- case eventsEnd $ F.onyxEvents $ F.s_tracks mid of
      Wait dt _ _ -> return $ floor $ U.applyTempoMap (F.s_tempos mid) dt * 1000
      RNil        -> fatal "panic! no [end] in processed midi"
    shk $ need [maxScores]
    maxScoresContent <- stackIO $ TE.decodeUtf8 <$> B.readFile maxScores
    pad <- readPad
    let padMS = pad * 1000

    let difficultyFromRank :: T.Text -> Integer -> T.Text
        difficultyFromRank name rank = let
          -- picked these rank cutoffs so RB 7 tiers convert to 1-1-2-2-3-4-5
          -- on all of guitar/bass/drums
          n = T.pack $ show $ length $ filter (<= rank) [1, 150, 242, 325, 400]
          in name <> " = { 1, " <> T.intercalate ", " (replicate 5 n) <> " }"
        lua = T.intercalate "\r\n"
          [ "Sex = Sex or {}"
          , "Playback = Playback or {}"
          , "QuantiseLevel = QuantiseLevel or {}"
          , ""
          , "-- standard information"
          , "Year = " <> T.pack (show $ getYear songYaml.metadata)
          , ""
          , "KitId = 1002" -- disabling kit anyway with DrumPlayback below
          , "SoloGuitarId = 2200" -- ?
          , "DrumPlayback = Playback.kStem" -- kStem seems to prevent midi drum sounds (most songs have Playback.kMidi)
          , "DisplayArtist = true" -- on disc, this is only true for the 2 masters. in dlc, false even for the bemani pack oddly
          , ""
          -- in official songs this is different from the filename song id
          , "-- leaderboard - this MUST be unique"
          , "LeaderboardId = " <> maybe key (T.pack . show) rr.leaderboardID
          , ""
          -- these can be 1 through 5. never actually different per level
          , "-- star ratings for each difficulty level:"
          , difficultyFromRank "DrumDifficulty"   $ computeDrumRank rr.drums  songYaml drumsDiffMap
          , difficultyFromRank "GuitarDifficulty" $ computeFiveRank rr.guitar songYaml guitarDiffMap
          , difficultyFromRank "BassDifficulty"   $ computeFiveRank rr.bass   songYaml bassDiffMap
          , ""
          , "-- top score for each difficulty level:"
          , T.strip maxScoresContent
          , ""
          -- shuffle is used on the main menu, whole song is fine
          , "-- FE music times = { min, sec, ms }"
          , "ShuffleStartTime = { 0, 0, 0 }"
          , "ShuffleStopTime = "  <> showTimeParts (endMS  + padMS)
          , "PreviewStartTime = " <> showTimeParts (pstart + padMS)
          , "PreviewStopTime = "  <> showTimeParts (pend   + padMS)
          , ""
          , "-- character information"
          , "SingerSex = " <> case rrVocals >>= (.gender) of
            Nothing     -> "Sex.kMale"
            Just Male   -> "Sex.kMale"
            Just Female -> "Sex.kFemale"
          , "LipSyncBan = \"\"" -- setting to something is required to not crash on ps3! (360 is fine oddly)
          , ""
          -- ?
          , "DefaultDrummer = 10"
          , "DefaultBassist = 15"
          , "DefaultGuitarist = 9"
          , "DefaultStandin = 8"
          , ""
          , "-- Audio Mastering information"
          , "s" <> key <> " ="
          , "{"
          , "  ParentMix = \"MusicMix001\","
          , "  MixType = 2,"
          , "  Volume = 0,"
          , "  DSPs = {}"
          , "}"
          , ""
          , "s" <> key <> "_FE ="
          , "{"
          , "  ParentMix = \"FrontMixMusic001\","
          , "  MixType = 2,"
          , "  Volume = -7,"
          , "  DSPs = { \"FEEQ01\", \"FEEQ02\", \"FEMusicVerb\" }"
          , "}"
          , ""
          , "Quantise = QuantiseLevel.kNone"
          , ""
          ]

    stackIO $ B.writeFile out $ TE.encodeUtf8 lua

  let pathGuitar  = dir </> "guitar.mp3"
      pathBass    = dir </> "bass.mp3"
      pathDrums   = dir </> "drums.mp3"
      pathSong    = dir </> "song.mp3"
      pathFE      = dir </> "fe.mp3"
      rrParts = [rr.guitar, rr.bass, rr.drums]
      setup lame = liftIO $ do
        L.check $ L.setBrate lame 128
        L.check $ L.setQuality lame 5
        L.check $ L.setOutSamplerate lame 44100

  pathGuitar %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.guitar, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ padAudio pad $ clampIfSilent s
  pathBass %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.bass, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ padAudio pad $ clampIfSilent s
  pathDrums %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceStereoParts buildInfo rrParts rr.common mid 0 planName plan
      [(rr.drums, 1)]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ padAudio pad $ clampIfSilent s
  pathSong %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceBacking buildInfo rr.common mid 0 planName plan
      [ (rr.guitar, 1)
      , (rr.bass  , 1)
      , (rr.drums , 1)
      ]
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ padAudio pad $ clampIfSilent s
  pathFE %> \out -> do
    mid <- loadOnyxMidi
    s <- sourceBacking buildInfo rr.common mid 0 planName plan []
    pad <- readPad
    stackIO $ runResourceT $ sinkMP3PadWithHandle out setup $ padAudio pad $ clampIfSilent s

  let songFev = file $ "s" <> str <> ".fev" -- makeMainFEV
      songFsb = file $ "s" <> str <> ".fsb"
      songDrumsFsb = file $ "s" <> str <> "_Drums.fsb"
      songFEFev = file $ "s" <> str <> "_FE.fev" -- makeFrontEndFEV
      songFEFsb = file $ "s" <> str <> "_FE.fsb"

  let testSize f = stackIO $ (>= 5000) <$> withBinaryFile f ReadMode hFileSize
      getAudio path = testSize path >>= \case
        False -> return Nothing
        True  -> Just <$> do
          bs <- stackIO $ BL.readFile path
          src <- stackIO $ ffSource $ Right path
          let _ = src :: CA.AudioSource (ResourceT IO) Int16
              size = round $ CA.framesToSeconds (CA.frames src) (CA.rate src) * 1000
          return (bs, size)

  [songFev, songFsb, songDrumsFsb] %> \_ -> do
    shk $ need [pathGuitar, pathBass, pathDrums, pathSong]
    maybeGuitar  <- getAudio pathGuitar
    maybeBass    <- getAudio pathBass
    maybeDrums   <- getAudio pathDrums
    maybeBacking <- getAudio pathSong
    stackIO $ mp3sToFSB3 (catMaybes
      [ (\(bs, _) -> ("onyx_guitar.mp3" , bs)) <$> maybeGuitar
      , (\(bs, _) -> ("onyx_bass.mp3"   , bs)) <$> maybeBass
      , (\(bs, _) -> ("onyx_backing.mp3", bs)) <$> maybeBacking
      ]) >>= BL.writeFile songFsb . emitFSB
    stackIO $ mp3sToFSB3 (catMaybes
      [ (\(bs, _) -> ("onyx_drums.mp3", bs)) <$> maybeDrums
      ]) >>= BL.writeFile songDrumsFsb . emitFSB
    let fev = makeMainFEV bytes (snd <$> maybeGuitar) (snd <$> maybeBass) (snd <$> maybeDrums) (snd <$> maybeBacking)
    stackIO $ BL.writeFile songFev $ runPut $ void $ codecOut binFEV fev

  [songFEFev, songFEFsb] %> \_ -> do
    -- TODO split off vocals like official songs do
    -- TODO if no stems at all, can just reuse backing audio in main fsb as the FE audio
    shk $ need [pathFE]
    (feBytes, feSize) <- getAudio pathFE >>= \case
      Nothing   -> fatal "panic! couldn't produce rock revolution FE audio"
      Just pair -> return pair
    stackIO $ mp3sToFSB3 [("onyx_fe.mp3", feBytes)] >>= BL.writeFile songFEFsb . emitFSB
    let fev = makeFrontEndFEV bytes feSize
    stackIO $ BL.writeFile songFEFev $ runPut $ void $ codecOut binFEV fev

  let rrBuildFiles = concat
        [ [songBinEnglish, songBinFrench, songBinSpanish]
        , [sFevSeq, sFsbSeq, sDrumsFsbSeq]
        , allMidis
        , [songLua]
        , [songFev, songFsb, songDrumsFsb, songFEFev, songFEFsb]
        ]

  dir </> "rrlive" %> \out -> do
    shk $ need rrBuildFiles
    lg "# Producing Rock Revolution LIVE file"
    rrpkg
      (artist <> " - " <> title)
      (T.pack $ "Compiled by Onyx Music Game Toolkit version " <> showVersion version)
      (dir </> "files")
      out

  -- ps3

  let edatConfig = rockRevolutionEdatConfig "CUSTOMSONGS"
      contentID = npdContentID edatConfig

  rrPS3Files <- forM rrBuildFiles $ \f -> do
    let ps3 = dir </> "files-ps3" </> map toUpper (takeFileName f) <.> "EDAT"
    ps3 %> \_ -> do
      -- can't actually get it to work on console yet, so just make unencrypted files for RPCS3
      if False
        then do
          shk $ need [f]
          fin  <- shortWindowsPath False f
          fout <- shortWindowsPath True  ps3
          stackIO $ packNPData edatConfig fin fout $ B8.pack $ takeFileName ps3
        else shk $ copyFile' f ps3
    return ps3

  dir </> "rr.pkg" %> \out -> do
    shk $ need rrPS3Files
    lg "# Producing Rock Revolution .pkg file"
    let container name inner = Folder { folderSubfolders = [(name, inner)], folderFiles = [] }
    main <- container "USRDIR" <$> crawlFolderBytes (dir </> "files-ps3")
    extra <- stackIO (getResourcesPath "pkg-contents/rr") >>= crawlFolderBytes
    stackIO $ makePKG contentID (main <> extra) out

makeRRFiveDifficulty :: FiveResult -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats) -> RRFiveDifficulty U.Beats
makeRRFiveDifficulty result gems = RRFiveDifficulty
  { rrfStrums = blipEdgesRB_ $ flip RTB.mapMaybe processed $ \case
    ((color, Strum), len) -> Just (color, len)
    _                     -> Nothing
  , rrfHOPOs = blipEdgesRB_ $ flip RTB.mapMaybe processed $ \case
    ((_    , Strum), _  ) -> Nothing
    ((color, _    ), len) -> Just (color, len)
  , rrfSolo = RTB.empty -- added later
  } where
    -- just use same hopo rules as GH3: no hopo chords, no repeated hopos
    processed = noOpenNotes result.settings.detectMutedOpens
      $ noExtendedSustains' standardBlipThreshold gap $ gh3LegalHOPOs gems
    gap = fromIntegral result.settings.sustainGap / 480

makeRRFiveControl
  :: FiveResult
  -> RTB.T U.Beats ((Maybe Five.Color, StrumHOPOTap), Maybe U.Beats)
  -> RTB.T U.Beats (Edge () Five.Color)
makeRRFiveControl result gems = let
  processed = noOpenNotes result.settings.detectMutedOpens
    $ noExtendedSustains' standardBlipThreshold gap gems
  gap = fromIntegral result.settings.sustainGap / 480
  in blipEdgesRBNice $ (\((color, _), len) -> ((), color, len)) <$> processed

removeGemsUnderFreestyle :: (NNC.C t) => RRDrumDifficulty t -> RRDrumDifficulty t
removeGemsUnderFreestyle rrd = let
  laneBools = flip fmap rrd.rrdFreestyle $ \case
    EdgeOn _ (_, rrc) -> (rrc, True )
    EdgeOff  (_, rrc) -> (rrc, False)
  in rrd
    { rrdGems = flip RTB.mapMaybe (applyStatus laneBools rrd.rrdGems)
      $ \(currentLanes, pair@(_perc, chan)) -> do
        guard $ notElem chan currentLanes
        return pair
    }

condenseDupes :: (NNC.C t, Ord a) => RTB.T t (Edge () a) -> RTB.T t (Edge () a)
condenseDupes = go Map.empty where
  go activeCounts = \case
    Wait dt e rest -> case e of
      EdgeOn () x -> let
        activeCounts' = Map.alter (Just . (+ 1) . fromMaybe 0) x activeCounts
        in case fromMaybe 0 $ Map.lookup x activeCounts :: Int of
          0 -> Wait dt e $ go activeCounts' rest
          _ -> RTB.delay dt $ go activeCounts' rest
      EdgeOff x -> let
        activeCounts' = Map.alter (Just . subtract 1 . fromMaybe 0) x activeCounts
        in case fromMaybe 0 $ Map.lookup x activeCounts :: Int of
          1 -> Wait dt e $ go activeCounts' rest
          _ -> RTB.delay dt $ go activeCounts' rest
    RNil -> RNil

data RRDrumInput t = RRDrumInput
  { notes      :: RTB.T t (E.EliteGem ())
  , lanes      :: RTB.T t (Edge () (E.EliteGem ()))
  , activation :: RTB.T t Bool
  , events     :: EventsTrack t
  }

makeRRDrumDifficulty :: (NNC.C t) => RRDrumInput t -> RRDrumDifficulty t
makeRRDrumDifficulty input = removeGemsUnderFreestyle RRDrumDifficulty
  { rrdGems = RTB.flatten $ flip fmap (RTB.collectCoincident input.notes) $ \gems -> concat
    [ [ (RR_Kick     , RRC_Kick   ) | elem (E.Kick ()) gems ]
    , [ (RR_Snare    , RRC_Snare  ) | elem E.Snare     gems ]
    , [ (RR_HihatOpen, RRC_Hihat  ) | elem E.Hihat     gems ]
    , [ (RR_Crash1   , RRC_CrashL ) | elem E.CrashL    gems || (elem E.CrashR gems && elem E.Ride gems) ]
    , [ (RR_Tom3     , RRC_HighTom) | elem E.Tom1      gems || (elem E.Tom2 gems && elem E.Tom3 gems) ]
    , [ (RR_Tom5     , RRC_LowTom ) | elem E.Tom2      gems || elem E.Tom3 gems ]
    , [ (RR_Ride     , RRC_CrashR ) | elem E.Ride      gems || elem E.CrashR gems ]
    ]
  , rrdHidden    = RTB.empty
  , rrdFreestyle = let
    lanesNormal = flip RTB.mapMaybe input.lanes $ mapM $ \case
      E.Kick ()   -> Just RRC_Kick
      E.Snare     -> Just RRC_Snare
      E.Hihat     -> Just RRC_Hihat
      E.CrashL    -> Just RRC_CrashL
      E.Tom1      -> Just RRC_HighTom
      E.Tom2      -> Just RRC_LowTom
      E.Tom3      -> Just RRC_LowTom
      E.Ride      -> Just RRC_CrashR
      E.CrashR    -> Just RRC_CrashR
      E.HihatFoot -> Nothing
    lanesBRE = case input.events.eventsCoda of
      RNil -> RTB.empty
      Wait codaTime _ _
        -> RTB.flatten
        $ fmap breEdge
        $ RTB.delay codaTime
        $ U.trackDrop codaTime input.activation
    breEdge b = map (if b then EdgeOn () else EdgeOff)
      [RRC_Kick, RRC_Snare, RRC_Hihat, RRC_CrashL, RRC_HighTom, RRC_LowTom, RRC_CrashR]
    withPerc = fmap $ fmap $ \chan -> let
      perc = case chan of
        RRC_Kick    -> RR_Kick
        RRC_Snare   -> RR_Snare
        RRC_Hihat   -> RR_HihatOpen
        RRC_CrashL  -> RR_Crash1
        RRC_HighTom -> RR_Tom3
        RRC_LowTom  -> RR_Tom5
        RRC_CrashR  -> RR_Ride
        RRC_Hidden  -> RR_Vibraslap -- shouldn't happen
      in (perc, chan)
    in withPerc $ condenseDupes $ RTB.merge lanesNormal lanesBRE
  , rrdSolo      = RTB.empty -- added later
  }

makeRRDrumDifficultyElite
  :: (NNC.C t)
  => RTB.T t (E.EliteDrumNote ())
  -> E.EliteDrumTrack t
  -> EventsTrack t
  -> RRDrumDifficulty t
makeRRDrumDifficultyElite trk elite events = makeRRDrumDifficulty RRDrumInput
  { notes      = (.tdn_gem) <$> trk
  , lanes      = elite.tdLanes
  , activation = elite.tdActivation
  , events     = events
  }

legacyLanes
  :: (Num t, NNC.C t)
  => Difficulty
  -> RTB.T t (D.Gem D.ProType, D.DrumVelocity)
  -> D.DrumTrack t
  -> RTB.T t (Edge () (D.Gem D.ProType))
legacyLanes diff gems trk = let
  hands = RTB.filter (/= D.Kick) $ fmap fst gems
  single = findTremolos hands $ laneDifficulty diff trk.drumSingleRoll
  double = findTrills   hands $ laneDifficulty diff trk.drumDoubleRoll
  in splitEdgesSimple $ fmap (\(gem, len) -> ((), gem, len)) $ RTB.merge single double

makeRRDrumDifficulty6Lane, makeRRDrumDifficulty4Lane
  :: (Num t, NNC.C t)
  => Difficulty
  -> RTB.T t (D.Gem D.ProType, D.DrumVelocity)
  -> D.DrumTrack t
  -> EventsTrack t
  -> RRDrumDifficulty t
makeRRDrumDifficulty6Lane diff gems trk events = let
  gemTranslate = \case
    D.Kick                  -> E.Kick ()
    D.Red                   -> E.Snare
    D.Pro D.Yellow D.Tom    -> E.Tom1
    D.Pro D.Blue   D.Tom    -> E.Tom2
    D.Pro D.Green  D.Tom    -> E.Tom3
    D.Pro D.Yellow D.Cymbal -> E.Hihat
    D.Pro D.Blue   D.Cymbal -> E.CrashL
    D.Pro D.Green  D.Cymbal -> E.CrashR
    D.Orange                -> E.Ride -- shouldn't happen
  in makeRRDrumDifficulty RRDrumInput
    { notes      = gemTranslate . fst <$> gems
    , lanes      = fmap gemTranslate <$> legacyLanes diff gems trk
    , activation = trk.drumActivation
    , events     = events
    }
makeRRDrumDifficulty4Lane diff gems trk events = let
  gemTranslate = \case
    D.Kick           -> E.Kick ()
    D.Red            -> E.Snare
    D.Pro D.Yellow _ -> E.Hihat
    D.Pro D.Blue   _ -> E.Tom2
    D.Pro D.Green  _ -> E.CrashR
    D.Orange         -> E.CrashR -- shouldn't happen
  in makeRRDrumDifficulty RRDrumInput
    { notes      = gemTranslate . fst <$> gems
    , lanes      = fmap gemTranslate <$> legacyLanes diff gems trk
    , activation = trk.drumActivation
    , events     = events
    }

makeMainFEV :: B.ByteString -> Maybe Word32 -> Maybe Word32 -> Maybe Word32 -> Maybe Word32 -> FEV
makeMainFEV songID timeGuitar timeBass timeDrums timeBacking = FEV
  { version = FEVVersion26
  , unkOffset1 = Nothing
  , unkOffset2 = Nothing
  , projectName = "s" <> songID
  , waveBanks =
    [ WaveBank
      { bankType = 512
      , bankMaxStreams = 32
      , bankName = "s" <> songID
      }
    , WaveBank
      { bankType = 512
      , bankMaxStreams = 32
      , bankName = "s" <> songID <> "_Drums"
      }
    ]
  , topLevelEventCategory = EventCategory
    { name = "master"
    , volumeFieldRatio = 1.0
    , pitch = 0.0
    , maxPlaybacks = Nothing
    , maxPlaybackBehavior = Nothing
    , subcategories =
      [ EventCategory
        { name = "music"
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = []
        }
      , EventCategory
        { name = "s" <> songID
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = do
          name <- ["Guitar", "Bass", "Drums", "Backing"]
          return EventCategory
            { name = name
            , volumeFieldRatio = 1.0
            , pitch = 0.0
            , maxPlaybacks = Nothing
            , maxPlaybackBehavior = Nothing
            , subcategories = []
            }
        }
      ]
    }
  , topLevelEventGroups =
    [ EventGroup
      { name = "Stems"
      , userProperties = []
      , subgroups = []
      , events = let
        event eventName volName soundDefName categoryName = Event
          { type_ = Nothing
          , name = eventName
          , unk1 = 1.0
          , unk2 = 0
          , unk3 = 0
          , unk4 = 0
          , unk5 = 0
          , unk6 = 1
          , unk7 = 8
          , unk8 = 24
          , unk9 = 1.0
          , unk10 = 10000.0
          , unk11 = 0
          , unk12 = 8
          , unk13 = [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 360.0, 360.0, 1.0]
          , maxPlaybackBehavior = 1
          , unk14 = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]
          , layers =
            [ EventLayer
              { name = Just eventName
              , magic = 2
              , priority = -1
              , controlParameter = Left volName
              , soundDefInstances =
                [ SoundDefInstance
                  { nameOrIndex = Left soundDefName
                  , soundStart = 0.0
                  , soundLength = 1.0
                  , unk1 = 0
                  , unk2 = 1
                  , unk3 = -1
                  , padding = Just 0
                  , loopCount = Just 0
                  , autopitchEnabled = 0
                  , autopitchReferencePoint = 0
                  , autopitchAtMin = 0
                  , fineTune = 0
                  , volume = 1.0
                  , crossfadeInLength = -1.0
                  , crossfadeOutLength = -1.0
                  , crossfadeInType = 2
                  , crossfadeOutType = 2
                  }
                ]
              , envelopes =
                [ Envelope
                  { envelopeID = Just "env000"
                  , parent = Left ""
                  , name = "Volume"
                  , unk1 = 0
                  , unk2 = 0
                  , points =
                    [ Point
                      { x = 0.0
                      , y = 0.0
                      , curveShapeType = 2
                      }
                    , Point
                      { x = 1.0
                      , y = 1.0
                      , curveShapeType = 2
                      }
                    ]
                  , unk3 = 0
                  , unk4 = 0
                  }
                ]
              }
            ]
          , parameters =
            [ Parameter
              { name = volName
              , velocity = 0.0
              , paramMin = 0.0
              , paramMax = 1.0
              , flagsType = 3
              , seekSpeed = 0.0
              , numEnvelopesControlled = 1
              , unk0 = 0
              }
            ]
          , unk15 = 0
          , parentEventCategoryNames = [ categoryName ]
          }
        in catMaybes
          [ do
            guard $ isJust timeDrums
            Just $ event "Drums"   "Drums_Vol"   ("s" <> songID <> "_drums"   ) ("s" <> songID <> "/Drums"  )
          , do
            guard $ isJust timeGuitar
            Just $ event "Guitar"  "Guitar_Vol"  ("s" <> songID <> "_guitar"  ) ("s" <> songID <> "/Guitar" )
          , do
            guard $ isJust timeBass
            Just $ event "Bass"    "Bass_Vol"    ("s" <> songID <> "_bass"    ) ("s" <> songID <> "/Bass"   )
          , do
            guard $ isJust timeBacking
            Just $ event "Backing" "Backing_Vol" ("s" <> songID <> "_MixMinus") ("s" <> songID <> "/Backing")
          ]
      }
    ]
  , soundDefProperties = Nothing
  , soundDefs = let
    blankDef = SoundDef
      { name = ""
      , property = Left SoundDefProperty
        { play_mode                   = 3
        , min_spawn_time              = 0
        , max_spawn_time              = 0
        , max_spawned_sounds          = 1
        , volume_field_ratio_1        = 1.0
        , volume_rand_method          = 1
        , volume_rand_min_field_ratio = 1.0
        , volume_rand_max_field_ratio = 1.0
        , volume_field_ratio_2        = 1.0
        , pitch                       = 0.0
        , pitch_rand_method           = 1
        , pitch_rand_min_field_ratio  = 0
        , pitch_rand_max_field_ratio  = 0
        , pitch_rand                  = 0
        , recalc_pitch_rand           = Nothing
        , position_3d_randomization   = Nothing
        }
      , waveforms = []
      }
    in catMaybes
      [ flip fmap timeGuitar $ \playtime -> blankDef
        { name = "s" <> songID <> "_guitar"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_guitar.wav"
            , bankName = "s" <> songID
            , indexInBank = 0
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeBass $ \playtime -> blankDef
        { name = "s" <> songID <> "_bass"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_bass.wav"
            , bankName = "s" <> songID
            , indexInBank = fromIntegral $ length $ filter isJust [timeGuitar]
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeDrums $ \playtime -> blankDef
        { name = "s" <> songID <> "_drums"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_drums.wav"
            , bankName = "s" <> songID <> "_Drums"
            , indexInBank = 0
            , playtime = playtime
            }
          ]
        }
      , flip fmap timeBacking $ \playtime -> blankDef
        { name = "s" <> songID <> "_MixMinus"
        , waveforms =
          [ Waveform
            { padding = 0
            , weight = 100
            , name = "../../stems/s" <> songID <> "/s" <> songID <> "_MixMinus.wav"
            , bankName = "s" <> songID
            , indexInBank = fromIntegral $ length $ filter isJust [timeGuitar, timeBass]
            , playtime = playtime
            }
          ]
        }
      ]
  , reverbs = Left []
  }

makeFrontEndFEV :: B.ByteString -> Word32 -> FEV
makeFrontEndFEV songID playtime = FEV
  { version = FEVVersion26
  , unkOffset1 = Nothing
  , unkOffset2 = Nothing
  , projectName = "s" <> songID <> "_FE"
  , waveBanks =
    [ WaveBank
      { bankType = 128
      , bankMaxStreams = 32
      , bankName = "s" <> songID <> "_FE"
      }
    ]
  , topLevelEventCategory = EventCategory
    { name = "master"
    , volumeFieldRatio = 1.0
    , pitch = 0.0
    , maxPlaybacks = Nothing
    , maxPlaybackBehavior = Nothing
    , subcategories = do
      name <- ["music", "s" <> songID <> "_FE"]
      return EventCategory
        { name = name
        , volumeFieldRatio = 1.0
        , pitch = 0.0
        , maxPlaybacks = Nothing
        , maxPlaybackBehavior = Nothing
        , subcategories = []
        }
    }
  , topLevelEventGroups =
    [ EventGroup
      { name = "Song"
      , userProperties = []
      , subgroups = []
      , events =
        [ Event
          { type_ = Nothing
          , name = "Song"
          , unk1 = 1.0
          , unk2 = 0
          , unk3 = 0
          , unk4 = 0
          , unk5 = 128
          , unk6 = 1
          , unk7 = 8
          , unk8 = 24
          , unk9 = 1.0
          , unk10 = 10000.0
          , unk11 = 0
          , unk12 = 8
          , unk13 = [1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 360.0, 360.0, 1.0]
          , maxPlaybackBehavior = 1
          , unk14 = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0]
          , layers =
            [ EventLayer
              { name = Just "layer00"
              , magic = 2
              , priority = -1
              , controlParameter = Left ""
              , soundDefInstances =
                [ SoundDefInstance
                  { nameOrIndex = Left $ "s" <> songID <> "_FE"
                  , soundStart = 0.0
                  , soundLength = 1.0
                  , unk1 = 0
                  , unk2 = 1
                  , unk3 = -1
                  , padding = Just 0
                  , loopCount = Just 0
                  , autopitchEnabled = 0
                  , autopitchReferencePoint = 0
                  , autopitchAtMin = 0
                  , fineTune = 0
                  , volume = 1.0
                  , crossfadeInLength = -1.0
                  , crossfadeOutLength = -1.0
                  , crossfadeInType = 2
                  , crossfadeOutType = 2
                  }
                ]
              , envelopes = []
              }
            ]
          , parameters = []
          , unk15 = 0
          , parentEventCategoryNames = [ "s" <> songID <> "_FE" ]
          }
        ]
      }
    ]
  , soundDefProperties = Nothing
  , soundDefs =
    [ SoundDef
      { name = "s" <> songID <> "_FE"
      , property = Left SoundDefProperty
        { play_mode                   = 3
        , min_spawn_time              = 0
        , max_spawn_time              = 0
        , max_spawned_sounds          = 1
        , volume_field_ratio_1        = 1.0
        , volume_rand_method          = 1
        , volume_rand_min_field_ratio = 1.0
        , volume_rand_max_field_ratio = 1.0
        , volume_field_ratio_2        = 1.0
        , pitch                       = 0.0
        , pitch_rand_method           = 1
        , pitch_rand_min_field_ratio  = 0
        , pitch_rand_max_field_ratio  = 0
        , pitch_rand                  = 0
        , recalc_pitch_rand           = Nothing
        , position_3d_randomization   = Nothing
        }
      , waveforms =
        [ Waveform
          { padding = 0
          , weight = 100
          , name = "../../stems/s" <> songID <> "/s" <> songID <> "_FE.wav"
          , bankName = "s" <> songID <> "_FE"
          , indexInBank = 0
          , playtime = playtime
          }
        ]
      }
    ]
  , reverbs = Left []
  }

rrCameraCut :: (MonadRandom m) => Camera3 -> m (Maybe Int)
rrCameraCut = \case
  -- venue info in v0006_002.lua
  V3_coop_all_behind     -> cut $ trigger E 0 -- RegisterDebugTrigger("Target", "E", 0, "Crowd")
  V3_coop_all_far        -> cut $ trigger C 0 -- RegisterDebugTrigger("Target", "C", 0, "StageSong")
  V3_coop_all_near       -> moneyShot
  V3_coop_front_behind   -> cut $ trigger E 0 -- RegisterDebugTrigger("Target", "E", 0, "Crowd")
  V3_coop_front_near     -> moneyShot
  V3_coop_d_behind       -> cut $ trigger Cs (-2) -- RegisterDebugTrigger("Target", "C#", -2, "Drummer")
  V3_coop_d_near         -> cut $ trigger Cs (-2) -- RegisterDebugTrigger("Target", "C#", -2, "Drummer")
  V3_coop_v_behind       -> cut $ trigger D (-2) -- RegisterDebugTrigger("Target", "D", -2, "Vocalist")
  V3_coop_v_near         -> cut $ trigger D (-2) -- RegisterDebugTrigger("Target", "D", -2, "Vocalist")
  V3_coop_b_behind       -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_b_near         -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_g_behind       -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_g_near         -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_k_behind       -> moneyShot
  V3_coop_k_near         -> moneyShot
  V3_coop_d_closeup_hand -> cut $ trigger Cs (-2) -- RegisterDebugTrigger("Target", "C#", -2, "Drummer")
  V3_coop_d_closeup_head -> cut $ trigger G (-2) -- RegisterDebugTrigger("Target", "G", -2, "Drummer Head")
  V3_coop_v_closeup      -> cut $ trigger Gs (-2) -- RegisterDebugTrigger("Target", "G#", -2, "Vocalist Head")
  V3_coop_b_closeup_hand -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_b_closeup_head -> cut $ trigger A (-2) -- RegisterDebugTrigger("Target", "A", -2, "Bassist Head")
  V3_coop_g_closeup_hand -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_g_closeup_head -> cut $ trigger Fs (-2) -- RegisterDebugTrigger("Target", "F#", -2, "Guitarist Head")
  V3_coop_k_closeup_hand -> moneyShot
  V3_coop_k_closeup_head -> moneyShot
  V3_coop_dv_near        -> cut $ trigger Cs (-2) -- RegisterDebugTrigger("Target", "C#", -2, "Drummer")
  V3_coop_bd_near        -> cutOneOf [trigger Ds (-2), trigger Cs (-2)]
  V3_coop_dg_near        -> cutOneOf [trigger Cs (-2), trigger C (-2)]
  V3_coop_bv_behind      -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_bv_near        -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_gv_behind      -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_gv_near        -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_kv_behind      -> cut $ trigger D (-2) -- RegisterDebugTrigger("Target", "D", -2, "Vocalist")
  V3_coop_kv_near        -> cut $ trigger D (-2) -- RegisterDebugTrigger("Target", "D", -2, "Vocalist")
  V3_coop_bg_behind      -> cutOneOf [trigger Ds (-2), trigger C (-2)]
  V3_coop_bg_near        -> cutOneOf [trigger Ds (-2), trigger C (-2)]
  V3_coop_bk_behind      -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_bk_near        -> cut $ trigger Ds (-2) -- RegisterDebugTrigger("Target", "D#", -2, "Bassist")
  V3_coop_gk_behind      -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  V3_coop_gk_near        -> cut $ trigger C (-2) -- RegisterDebugTrigger("Target", "C", -2, "Guitarist")
  _                      -> return Nothing -- directed cut
  where cut = return . Just
        cutOneOf = fmap Just . uniform
        moneyShot = cutOneOf [trigger Fs 0, trigger G 0, trigger Gs 0]
        -- RegisterDebugTrigger("Target", "F#", 0, "Money Shot 1")
        -- RegisterDebugTrigger("Target", "G", 0, "Money Shot 2")
        -- RegisterDebugTrigger("Target", "G#", 0, "Money Shot 3")
        trigger k oct = fromEnum k + (oct + 2) * 12
