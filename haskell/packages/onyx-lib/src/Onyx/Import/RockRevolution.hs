{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.RockRevolution where

import           Control.Concurrent.Async         (concurrently)
import           Control.Monad                    (forM, guard, replicateM,
                                                   unless, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit)
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     (void)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           Debug.Trace
import           Numeric                          (showHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.FSB
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   each, isNoteEdgeCPV,
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Read                   (ChannelType (..),
                                                   ParseTrack (..),
                                                   channelBlip_, condenseMap,
                                                   eachKey, edges,
                                                   translateEdges)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.Full       as FD
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as RBFile
import qualified Onyx.MIDI.Track.FiveFret         as F
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.StackTrace
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

importRR :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> [Import m]
importRR dir = map (importRRSong dir) $ findRRSongKeys dir

findRRSongKeys :: Folder T.Text Readable -> [T.Text]
findRRSongKeys dir = do
  (name, _) <- folderFiles dir
  key <- toList $ T.stripPrefix "s" (T.toLower name) >>= T.stripSuffix ".lua"
  guard $ T.all isDigit key
  return key

-- notes on channel 0xF in control.mid
data SectionMarker
  = SectionIntro       -- 48
  | SectionVerse       -- 49
  | SectionChorus      -- 50
  | SectionBridge      -- 51
  | SectionMiddleEight -- 52
  | SectionGuitarSolo  -- 53
  | SectionCustom Int  -- pitches 54 through 70, I think (54 = SectionCustom 0)
  | SectionOutro       -- 71
  deriving (Eq)

data RRFiveDifficulty t = RRFiveDifficulty
  { rrfStrums :: RTB.T t (Edge () F.Color)
  , rrfHOPOs  :: RTB.T t (Edge () F.Color)
  , rrfSolo   :: RTB.T t Bool
  } deriving (Show)

instance ParseTrack RRFiveDifficulty where
  parseTrack = do
    rrfStrums <- (rrfStrums =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      F.Green  -> 48
      F.Red    -> 49
      F.Yellow -> 50
      F.Blue   -> 51
      F.Orange -> 52
    rrfHOPOs <- (rrfHOPOs =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      F.Green  -> 60
      F.Red    -> 61
      F.Yellow -> 62
      F.Blue   -> 63
      F.Orange -> 64
    rrfSolo <- rrfSolo =. edges 127
    return RRFiveDifficulty{..}

importRRGuitarBass :: RRFiveDifficulty U.Beats -> F.FiveDifficulty U.Beats
importRRGuitarBass rr = let
  forceEdges
    = RTB.flatten
    . fmap nubOrd
    . RTB.collectCoincident
    . fmap (\case EdgeOn{} -> True; EdgeOff{} -> False)
  in F.FiveDifficulty
    { F.fiveForceStrum = forceEdges $ rrfStrums rr
    , F.fiveForceHOPO = forceEdges $ rrfHOPOs rr
    , F.fiveTap = RTB.empty
    , F.fiveOpen = RTB.empty
    , F.fiveGems = RTB.merge (rrfStrums rr) (rrfHOPOs rr)
    }

importRRSong :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> T.Text -> Import m
importRRSong dir key level = inside ("Song " <> show key) $ do

  when (level == ImportFull) $ lg $ "Importing Rock Revolution song [" <> T.unpack key <> "]"

  let need f = needRead f >>= \r -> stackIO $ useHandle r handleToByteString
      needRead f = case findFileCI (pure f) dir of
        Nothing -> fatal $ "Couldn't locate file: " <> T.unpack f
        Just r  -> return r

  -- these strings appear to be utf-8; see e.g. French_s1002_Strings.bin
  strings <- map TE.decodeUtf8 . B.split 0 . BL.toStrict <$> need ("English_s" <> key <> "_Strings.bin")
  lua <- TE.decodeLatin1 . BL.toStrict <$> need ("s" <> key <> ".lua")

  let year :: Maybe Int
      year = listToMaybe
        $ mapMaybe (\case ["Year", "=", n] -> readMaybe $ T.unpack n; _ -> Nothing)
        $ map T.words $ T.lines lua

      loadAudio r = do
        fsb <- parseFSB r
        let names = map fsbSongName $ either fsb3Songs fsb4Songs $ fsbHeader fsb
        streams <- splitFSBStreams' fsb
        forM (zip names streams) $ \(streamName, stream) -> do
          (streamData, ext) <- getFSBStreamBytes stream
          return (TE.decodeLatin1 streamName <> "." <> ext, streamData)

  (nonDrumStreams, drumStreams) <- case level of
    ImportQuick -> return ([], [])
    ImportFull -> do
      r1 <- need $ "s" <> key <> ".fsb"
      r2 <- need $ "s" <> key <> "_Drums.fsb"
      stackIO $ concurrently (loadAudio r1) (loadAudio r2)

  -- TODO this is a brittle hack. We have to parse the .fev files to figure out channel order correctly
  let findStream insts fsb = case filter (\(name, _) -> any (`T.isInfixOf` T.toLower name) insts) fsb of
        [] -> do
          case level of
            ImportQuick -> return ()
            ImportFull  -> warn $ "Couldn't locate stream in FSB: " <> show insts
          return Nothing
        (name, _) : _ -> return $ Just name
      allStreams = nonDrumStreams <> drumStreams
      -- searching all streams together because s1056 has guitar and drums mixed up
  guitarStream  <- findStream ["guitar"               ] allStreams
  bassStream    <- findStream ["bass"                 ] allStreams
  drumsStream   <- findStream ["drum"                 ] allStreams
  backingStream <- findStream ["mixminus", "mix-minus"] allStreams

  controlMid <- case level of
    ImportQuick -> return emptyChart
    ImportFull  -> needRead ("s" <> key <> "_control.mid")
      >>= \r -> RBFile.loadRawMIDIReadable r >>= RBFile.readMixedMIDI

  let loadGuitarBass inst = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          fiveDiffs <- forM diffs $ \(num, diff) -> do
            mid <- needRead (T.concat ["s", key, "_", inst, "_", num, ".mid"])
              >>= RBFile.loadRawMIDIReadable >>= RBFile.readMixedMIDI
            rr <- RBFile.parseTrackReport $ RBFile.s_tracks mid
            return (diff, (rr, importRRGuitarBass rr))
          return mempty
            { F.fiveDifficulties = fmap snd $ Map.fromList fiveDiffs
            , F.fiveSolo = maybe RTB.empty (rrfSolo . fst) $ lookup Expert fiveDiffs
            }
      loadDrums = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          rrDiffs <- fmap Map.fromList $ forM diffs $ \(num, diff) -> do
            r <- needRead $ T.concat ["s", key, "_drums_", num, ".mid"]
            mid <- RBFile.loadRawMIDIReadable r >>= RBFile.readMixedMIDI
            rrd <- RBFile.parseTrackReport $ RBFile.s_tracks mid
            return (diff, rrd)
          return
            ( importRRDrums rrDiffs
            , mempty { FD.fdDifficulties  = fmap importRRFullDrums rrDiffs }
            , mempty { FD.fdDifficulties = fmap importRRHiddenDrums rrDiffs }
            )
      diffs = [("02", Easy), ("03", Medium), ("04", Hard), ("05", Expert)]
  guitar <- loadGuitarBass "guitar"
  bass <- loadGuitarBass "bass"
  (drums, fullDrums, hiddenDrums) <- loadDrums

  sectionNames <- case level of
    ImportQuick -> return RTB.empty
    ImportFull -> do
      control <- needRead (T.concat ["s", key, "_control.mid"]) >>= RBFile.loadRawMIDIReadable >>= RBFile.readMixedMIDI
      let sections = flip RTB.mapMaybe (RBFile.s_tracks control) $ \evt -> case isNoteEdgeCPV evt of
            Just (15, p, Just _) -> case p of
              48                     -> Just SectionIntro
              49                     -> Just SectionVerse
              50                     -> Just SectionChorus
              51                     -> Just SectionBridge
              52                     -> Just SectionMiddleEight
              53                     -> Just SectionGuitarSolo
              n | 54 <= n && n <= 70 -> Just $ SectionCustom $ n - 54
              71                     -> Just SectionOutro
              _                      -> Nothing
            _ -> Nothing
          getSectionNames prev = \case
            RNil -> RNil
            Wait t sect rest -> let
              name = case sect of
                SectionIntro       -> "Intro"
                SectionVerse       -> "Verse"
                SectionChorus      -> "Chorus"
                SectionBridge      -> "Bridge"
                SectionMiddleEight -> "Middle Eight"
                SectionGuitarSolo  -> "Guitar Solo"
                SectionCustom n    -> case drop (n + 2) strings of
                  []    -> "Custom Section " <> T.singleton (['A'..] !! n)
                  s : _ -> T.strip s
                SectionOutro       -> "Outro"
              numbered = if any (== sect) prev || any (== sect) rest
                then name <> " " <> T.pack (show $ length (filter (== sect) prev) + 1)
                else name
              in Wait t numbered $ getSectionNames (sect : prev) rest
      return $ getSectionNames [] sections

  return SongYaml
    { metadata = def'
      { title        = Just $ T.strip $ strings !! 1
      , artist       = Just $ T.strip $ strings !! 0
      , album        = Nothing
      , year         = year
      , comments     = []
      , fileAlbumArt = Nothing
      }
    , jammit = mempty
    , targets = HM.empty
    , global = def'
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> controlMid
          { RBFile.s_tracks = mempty
            { RBFile.onyxParts = Map.fromList
              [ (RBFile.FlexGuitar, mempty
                { RBFile.onyxPartGuitar = guitar
                })
              , (RBFile.FlexBass, mempty
                { RBFile.onyxPartGuitar = bass
                })
              , (RBFile.FlexDrums, mempty
                { RBFile.onyxPartDrums = drums
                , RBFile.onyxPartFullDrums = fullDrums
                })
              , (RBFile.FlexExtra "hidden-drums", mempty
                { RBFile.onyxPartFullDrums = hiddenDrums
                })
              ]
            , RBFile.onyxEvents = mempty
              { eventsSections = (SectionRB2,) <$> sectionNames
              }
            }
          }
        ImportQuick -> emptyChart
      , _fileSongAnim = Nothing
      }
    , audio = HM.fromList $ do
      (name, bs) <- nonDrumStreams <> drumStreams
      let str = T.unpack name
      return (name, AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , filePath = Just $ SoftFile str $ SoftReadable
          $ makeHandle str $ byteStringSimpleHandle bs
        , commands = []
        , rate = Nothing
        , channels = 2 -- TODO maybe verify
        })
    , plans = HM.singleton "rr" $ StandardPlan StandardPlanInfo
      { song = flip fmap backingStream $ \s -> PlanAudio (Input $ Named s) [] []
      , countin = Countin []
      , parts = Parts $ HM.fromList $ catMaybes
        [ flip fmap guitarStream $ \s -> (RBFile.FlexGuitar, PartSingle $ PlanAudio (Input $ Named s) [] [])
        , flip fmap bassStream   $ \s -> (RBFile.FlexBass  , PartSingle $ PlanAudio (Input $ Named s) [] [])
        , flip fmap drumsStream  $ \s -> (RBFile.FlexDrums , PartSingle $ PlanAudio (Input $ Named s) [] [])
        ]
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , parts = Parts $ HM.fromList
      [ (RBFile.FlexGuitar, def
        { partGRYBO = Just def
        })
      , (RBFile.FlexBass, def
        { partGRYBO = Just def
        })
      , (RBFile.FlexDrums, def
        { partDrums = Just PartDrums
          { drumsDifficulty = Tier 1
          , drumsMode = DrumsFull
          , drumsKicks = Kicks1x
          , drumsFixFreeform = False
          , drumsKit = HardRockKit
          , drumsLayout = StandardLayout
          , drumsFallback = FallbackGreen
          , drumsFileDTXKit = Nothing
          , drumsFullLayout = FDStandard
          }
        })
      {-
      , (RBFile.FlexExtra "hidden-drums", def
        { partDrums = Just PartDrums
          { drumsDifficulty = Tier 1
          , drumsMode = DrumsFull
          , drumsKicks = Kicks1x
          , drumsFixFreeform = False
          , drumsKit = HardRockKit
          , drumsLayout = StandardLayout
          , drumsFallback = FallbackGreen
          , drumsFileDTXKit = Nothing
          , drumsFullLayout = FDStandard
          }
        })
      -}
      ]
    }

-- drum format

-- This corresponds to pitches 36 through 59 of the General MIDI Percussion.
-- Don't put too much stock in the names, notes are occasionally misused!
data RRDrum
  = RR_Kick
  | RR_SideStick
  | RR_Snare
  | RR_HandClap
  | RR_ElectricSnare
  | RR_Tom6
  | RR_HihatClosed
  | RR_Tom5
  | RR_HihatPedal
  | RR_Tom4
  | RR_HihatOpen
  | RR_Tom3
  | RR_Tom2
  | RR_Crash1
  | RR_Tom1
  | RR_Ride
  | RR_China
  | RR_RideBell
  | RR_Tambourine
  | RR_Splash
  | RR_Cowbell
  | RR_Crash2
  | RR_Vibraslap
  | RR_Ride2
  deriving (Eq, Ord, Show, Enum, Bounded)

data RRChannel
  = RRC_Hidden -- putting first so it's default
  | RRC_LowTom
  | RRC_HighTom
  | RRC_Kick
  | RRC_Snare
  | RRC_CrashL
  | RRC_CrashR
  | RRC_Hihat
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ChannelType RRChannel where
  encodeChannel = \case
    RRC_LowTom  -> 0
    RRC_HighTom -> 1
    RRC_Kick    -> 2
    RRC_Snare   -> 3
    RRC_CrashL  -> 4
    RRC_CrashR  -> 5
    RRC_Hihat   -> 6
    -- channel 7 never used
    RRC_Hidden  -> 8
    -- there are a few notes with channel 9. maybe should have been 8?

rrChannel7Lane :: RRChannel -> Maybe FD.FullGem
rrChannel7Lane = \case
  RRC_LowTom  -> Just FD.Tom3
  RRC_HighTom -> Just FD.Tom1
  RRC_Kick    -> Just FD.Kick
  RRC_Snare   -> Just FD.Snare
  RRC_CrashL  -> Just FD.CrashL
  RRC_CrashR  -> Just FD.CrashR
  RRC_Hihat   -> Just FD.Hihat
  RRC_Hidden  -> Nothing

rrChannel4Lane :: RRChannel -> Maybe (D.Gem D.ProType)
rrChannel4Lane = \case
  RRC_LowTom  -> Just $ D.Pro D.Blue D.Tom
  RRC_HighTom -> Just $ D.Pro D.Blue D.Tom
  RRC_Kick    -> Just D.Kick
  RRC_Snare   -> Just D.Red
  RRC_CrashL  -> Just $ D.Pro D.Green D.Cymbal
  RRC_CrashR  -> Just $ D.Pro D.Green D.Cymbal
  RRC_Hihat   -> Just $ D.Pro D.Yellow D.Cymbal
  RRC_Hidden  -> Nothing

-- These are just conventions (channel is what actually determines gem).
-- Comments are channel counts from disc+dlc midis
rrDrumGuessFD :: RRDrum -> (FD.FullGem, FD.FullGemType, D.DrumVelocity)
rrDrumGuessFD = \case
  RR_Kick          -> (FD.Kick     , FD.GemNormal     , D.VelocityNormal) -- [48, [["Kick", 23132], ["HighTom", 4], ["Snare", 4]]]
  RR_HandClap      -> (FD.Hihat    , FD.GemNormal     , D.VelocityNormal) -- [49, [["Hihat", 734], ["Kick", 69], ["Snare", 27], ["LowTom", 1]]]
  RR_Snare         -> (FD.Snare    , FD.GemNormal     , D.VelocityNormal) -- [50, [["Snare", 16995], ["HighTom", 4], ["CrashR", 2], ["LowTom", 1]]]
  RR_SideStick     -> (FD.Snare    , FD.GemRim        , D.VelocityNormal) -- [51, [["Snare", 548], ["Hihat", 409], ["CrashR", 59]]]
  RR_ElectricSnare -> (FD.Snare    , FD.GemNormal     , D.VelocityNormal) -- [52, [["Snare", 31], ["LowTom", 30], ["Hihat", 27], ["HighTom", 24]]]
  RR_Tom6          -> (FD.Tom3     , FD.GemNormal     , D.VelocityNormal) -- [53, [["Hihat", 304], ["LowTom", 298], ["Snare", 60]]]
  RR_HihatClosed   -> (FD.Hihat    , FD.GemHihatClosed, D.VelocityNormal) -- [54, [["Hihat", 7161], ["LowTom", 10], ["HighTom", 9], ["CrashL", 6], ["Kick", 1]]]
  RR_Tom5          -> (FD.Tom3     , FD.GemNormal     , D.VelocityNormal) -- [55, [["LowTom", 1766], ["Hihat", 188], ["HighTom", 82], ["Snare", 3], ["CrashR", 2]]]
  RR_HihatPedal    -> (FD.HihatFoot, FD.GemNormal     , D.VelocityNormal) -- [56, [["Hihat", 4901], ["CrashL", 37]]]
  RR_Tom4          -> (FD.Tom2     , FD.GemNormal     , D.VelocityNormal) -- [57, [["HighTom", 1078], ["LowTom", 291], ["Kick", 9], ["Snare", 7]]]
  RR_HihatOpen     -> (FD.Hihat    , FD.GemHihatOpen  , D.VelocityNormal) -- [58, [["Hihat", 5923], ["CrashR", 205], ["CrashL", 6]]]
  RR_Tom3          -> (FD.Tom1     , FD.GemNormal     , D.VelocityNormal) -- [59, [["HighTom", 657], ["CrashR", 12], ["LowTom", 11], ["Snare", 2]]]
  RR_Tom2          -> (FD.Tom1     , FD.GemNormal     , D.VelocityNormal) -- [60, [["Hihat", 217], ["LowTom", 59], ["HighTom", 43], ["Snare", 5], ["CrashL", 2]]]
  RR_Crash1        -> (FD.CrashR   , FD.GemNormal     , D.VelocityNormal) -- [61, [["CrashR", 2922], ["CrashL", 228], ["Snare", 33], ["Hihat", 1]]]
  RR_Tom1          -> (FD.Tom1     , FD.GemNormal     , D.VelocityNormal) -- [62, [["CrashR", 234], ["LowTom", 69], ["CrashL", 31], ["Snare", 20], ["HighTom", 6]]]
  RR_Ride          -> (FD.Ride     , FD.GemNormal     , D.VelocityNormal) -- [63, [["CrashR", 1440], ["Hihat", 70], ["Snare", 8], ["CrashL", 6]]]
  RR_China         -> (FD.CrashR   , FD.GemNormal     , D.VelocityNormal) -- [64, [["CrashR", 146], ["CrashL", 126], ["Snare", 79], ["LowTom", 56], ["HighTom", 3], ["Hihat", 3]]]
  RR_RideBell      -> (FD.Ride     , FD.GemNormal     , D.VelocityNormal) -- [65, [["CrashR", 196], ["LowTom", 66], ["CrashL", 8], ["Snare", 4], ["Hihat", 1]]]
  RR_Tambourine    -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [66, [["CrashL", 3189], ["CrashR", 87], ["Hihat", 7]]]
  RR_Splash        -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [67, [["CrashL", 187], ["HighTom", 2], ["CrashR", 1]]]
  RR_Cowbell       -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [68, [["CrashL", 2100], ["CrashR", 60], ["Hihat", 2], ["Snare", 1]]]
  RR_Crash2        -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [69, [["CrashL", 162], ["Kick", 10], ["HighTom", 2]]]
  RR_Vibraslap     -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [70, [["CrashL", 352], ["CrashR", 72], ["Hihat", 6]]]
  RR_Ride2         -> (FD.CrashL   , FD.GemNormal     , D.VelocityNormal) -- [71, [["CrashL", 36], ["Kick", 4]]]

data RRDrumDifficulty t = RRDrumDifficulty
  { rrdGems      :: RTB.T t (RRDrum, RRChannel) -- starting from 48. none of these should be RRC_Hidden
  , rrdHidden    :: RTB.T t (RRDrum, RRChannel) -- starting from 72. these should all be RRC_Hidden
  , rrdFreestyle :: RTB.T t (RRDrum, RRChannel) -- starting from 0
  , rrdSolo      :: RTB.T t Bool
  -- TODO pitch 126 (highway star)
  -- TODO pitch 105 (highway star)
  } deriving (Show)

instance ParseTrack RRDrumDifficulty where
  parseTrack = do
    rrdGems      <- (rrdGems      =.) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 48
    rrdHidden    <- (rrdHidden    =.) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 72
    rrdFreestyle <- (rrdFreestyle =.) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum
    rrdSolo      <- rrdSolo =. edges 127
    return RRDrumDifficulty{..}

importRRDrums :: Map.Map Difficulty (RRDrumDifficulty U.Beats) -> D.DrumTrack U.Beats
importRRDrums diffs = mempty
  { D.drumDifficulties = flip fmap diffs $ \rr -> D.DrumDifficulty
    { D.drumMix         = RTB.empty
    , D.drumPSModifiers = RTB.empty
    , D.drumGems
      -- are there ever both toms or both crashes at the same time (which would translate to the same 4-lane pad)?
      = RTB.flatten
      $ fmap nubOrd
      $ RTB.collectCoincident
      $ fmap (\pro -> (void pro, D.VelocityNormal))
      $ RTB.mapMaybe (rrChannel4Lane . snd)
      $ rrdGems rr
    }
  , D.drumToms = let
    expert = maybe mempty (RTB.mapMaybe (rrChannel4Lane . snd) . rrdGems) $ Map.lookup Expert diffs
    in U.trackJoin $ flip fmap expert $ \case
      D.Pro color D.Tom -> RTB.cons 0 (color, D.Tom) $ RTB.cons (1/32) (color, D.Cymbal) RTB.empty
      _                 -> RTB.empty
  , D.drumSolo = maybe RTB.empty rrdSolo $ Map.lookup Expert diffs
  }

importRRFullDrums :: RRDrumDifficulty U.Beats -> FD.FullDrumDifficulty U.Beats
importRRFullDrums rr = FD.FullDrumDifficulty
  { FD.fdFlam = RTB.empty
  , FD.fdGems
    = fmap (\gem -> (gem, FD.GemNormal, D.VelocityNormal))
    $ RTB.mapMaybe (rrChannel7Lane . snd)
    $ rrdGems rr
  }

importRRHiddenDrums :: RRDrumDifficulty U.Beats -> FD.FullDrumDifficulty U.Beats
importRRHiddenDrums rr = FD.FullDrumDifficulty
  { FD.fdFlam = RTB.empty
  , FD.fdGems = fmap (rrDrumGuessFD . fst) $ RTB.merge (rrdGems rr) (rrdHidden rr)
  }

-- .fev files
-- This can currently kind of parse song (s*.fev) files but it's not really right and fails on other .fev files

data FEV = FEV
  { fev_unk1 :: Word16
  , fev_str1 :: B.ByteString
  , fev_fsbs :: [FSBReference]
  , fev_a    :: FEVNodeA
  , fev_b    :: [FEVNodeB]
  , fev_e    :: [FEVNodeE]
  , fev_unk2 :: Word32
  } deriving (Show)

data FSBReference = FSBReference
  { fsb_unk1 :: Word32
  , fsb_unk2 :: Word32
  , fsb_name :: B.ByteString
  } deriving (Show)

data FEVNodeA = FEVNodeA
  { a_name     :: B.ByteString
  , a_unk1     :: Word32
  , a_unk2     :: Word32
  , a_children :: [FEVNodeA]
  } deriving (Show)

data FEVNodeB = FEVNodeB
  { b_name     :: B.ByteString
  , b_unk1     :: Word32
  , b_unk2     :: Word32 -- this is 0 in s1064.fev and s1064_FE.fev, but 8 in g1203.fev. probably array length
  , b_children :: [FEVNodeC]
  } deriving (Show)

data FEVNodeC = FEVNodeC
  { c_name     :: B.ByteString
  , c_unk1     :: [Word32] -- len 30
  , c_str1     :: B.ByteString
  , c_unk2     :: Word32
  , c_str2     :: B.ByteString
  , c_unk3     :: Word16
  -- next is (I think) count of c_children, Word16
  , c_str3     :: B.ByteString
  , c_unk4     :: [Word32] -- len 16
  , c_children :: [FEVNodeD]
  , c_str4     :: Word32
  , c_unk5     :: Word32
  , c_unk6     :: Word32
  , c_str5     :: B.ByteString
  } deriving (Show)

data FEVNodeD = FEVNodeD
  { d_str1 :: B.ByteString
  , d_unk1 :: [Word32] -- len 12
  , d_str2 :: B.ByteString
  , d_unk2 :: [Word32] -- len 8
  } deriving (Show)

data FEVNodeE = FEVNodeE
  { e_name  :: B.ByteString
  , e_unk1  :: [Word32] -- len 17
  , e_wav   :: B.ByteString -- looks like path to original stem wav
  , e_fsb   :: B.ByteString -- the fsb file that this stream is in
  , e_index :: Word32 -- the index of this stream in the fsb
  , e_unk2  :: Word32
  } deriving (Show)

getFEV :: Get FEV
getFEV = do

  let traced lbl f = do
        p <- bytesRead
        x <- f
        trace ("0x" <> showHex p "" <> ": " <> lbl <> " = " <> show x) $ return ()
        return x

  let string = do
        len <- getWord32le
        case len of
          0 -> return ""
          _ -> do
            bs <- getByteString $ fromIntegral len
            unless (B.last bs == 0) $ fail $ "Invalid string: " <> show bs
            return $ B.init bs

  "FEV1" <- getByteString 4
  0 <- getWord16le
  fev_unk1 <- traced "fev_unk1" getWord16le -- always 0x26?
  fev_str1 <- traced "fev_str1" string

  numFSBs <- traced "numFSBs" getWord32le
  fev_fsbs <- replicateM (fromIntegral numFSBs) $ do
    fsb_unk1 <- traced "fsb_unk1" getWord32le
    fsb_unk2 <- traced "fsb_unk2" getWord32le
    fsb_name <- traced "fsb_name" string
    return FSBReference{..}

  let getNodeA = do
        traced "start A" $ return ()
        a_name <- traced "a_name" string
        a_unk1 <- traced "a_unk1" getWord32le
        a_unk2 <- traced "a_unk2" getWord32le
        numChildren <- traced "a numChildren" getWord32le
        a_children <- replicateM (fromIntegral numChildren) getNodeA
        return FEVNodeA{..}
  fev_a <- getNodeA

  let getNodeB = do
        traced "start B" $ return ()
        b_name <- traced "b_name" string
        b_unk1 <- traced "b_unk1" getWord32le
        b_unk2 <- traced "b_unk2" getWord32le
        numChildren <- traced "b numChildren" getWord32le
        b_children <- replicateM (fromIntegral numChildren) getNodeC
        return FEVNodeB{..}
      getNodeC = do
        traced "start C" $ return ()
        c_name <- traced "c_name" string
        c_unk1 <- replicateM 30 $ traced "c_unk1 element" getWord32le
        c_str1 <- traced "c_str1" string
        c_unk2 <- traced "c_unk2" getWord32le
        c_str2 <- traced "c_str2" string
        c_unk3 <- getWord16le
        numChildren <- traced "c numChildren" getWord16le
        c_str3 <- traced "c_str3" string
        c_unk4 <- replicateM 16 $ traced "c_unk4 element" getWord32le
        c_children <- replicateM (fromIntegral numChildren) getNodeD
        c_str4 <- traced "c_str4" getWord32le
        c_unk5 <- traced "c_unk5" getWord32le
        c_unk6 <- traced "c_unk6" getWord32le
        c_str5 <- traced "c_str5" string
        return FEVNodeC{..}
      getNodeD = do
        traced "start D" $ return ()
        d_str1 <- traced "d_str1" string
        d_unk1 <- replicateM 12 $ traced "d_unk1 element" getWord32le
        d_str2 <- traced "d_str2" string
        d_unk2 <- replicateM 8 $ traced "d_unk2 element" getWord32le
        return FEVNodeD{..}
  numB <- traced "numB" getWord32le
  fev_b <- replicateM (fromIntegral numB) getNodeB

  let getNodeE = do
        traced "start E" $ return ()
        e_name <- traced "e_name" string
        e_unk1 <- replicateM 17 $ traced "e_unk1 element" getWord32le
        e_wav <- traced "e_wav" string
        e_fsb <- traced "e_fsb" string
        e_index <- traced "e_index" getWord32le
        e_unk2 <- traced "e_unk2" getWord32le
        return FEVNodeE{..}
  numE <- traced "numE" getWord32le
  fev_e <- replicateM (fromIntegral numE) getNodeE

  fev_unk2 <- getWord32le
  return FEV{..}
