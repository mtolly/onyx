{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.RockRevolution where

import           Control.Concurrent.Async         (forConcurrently)
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit, toLower)
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     (void)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, (!?))
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.FSB
import           Onyx.Audio.FSB.FEV
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   each, isNoteEdgeCPV,
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Read                   (ChannelType (..),
                                                   ParseTrack (..),
                                                   channelBlip_, condenseMap,
                                                   eachKey, edges, fatBlips,
                                                   translateEdges)
import qualified Onyx.MIDI.Track.Drums            as D
import qualified Onyx.MIDI.Track.Drums.True       as TD
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Project                     hiding (Difficulty)
import           Onyx.Sections                    (simpleSection)
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

importRR :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> [Import m]
importRR dir = map (importRRSong dir) $ findRRSongKeys dir

findRRSongKeys :: Folder T.Text Readable -> [T.Text]
findRRSongKeys dir = do
  (name, _) <- folderFiles dir
  key <- toList $ T.stripPrefix "s" (T.toLower name) >>= T.stripSuffix ".lua"
  guard $ T.all isDigit key -- is this necessary?
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
  { rrfStrums :: RTB.T t (Edge () Five.Color)
  , rrfHOPOs  :: RTB.T t (Edge () Five.Color)
  , rrfSolo   :: RTB.T t Bool
  } deriving (Show)

instance ParseTrack RRFiveDifficulty where
  parseTrack = fatBlips (1/8) $ do
    rrfStrums <- (rrfStrums =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      Five.Green  -> 48
      Five.Red    -> 49
      Five.Yellow -> 50
      Five.Blue   -> 51
      Five.Orange -> 52
    rrfHOPOs <- (rrfHOPOs =.) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      Five.Green  -> 60
      Five.Red    -> 61
      Five.Yellow -> 62
      Five.Blue   -> 63
      Five.Orange -> 64
    rrfSolo <- rrfSolo =. edges 127
    return RRFiveDifficulty{..}

importRRGuitarBass :: RRFiveDifficulty U.Beats -> Five.FiveDifficulty U.Beats
importRRGuitarBass rr = let
  forceEdges
    = RTB.flatten
    . fmap nubOrd
    . RTB.collectCoincident
    . fmap (\case EdgeOn{} -> True; EdgeOff{} -> False)
  in Five.FiveDifficulty
    { Five.fiveForceStrum = forceEdges $ rrfStrums rr
    , Five.fiveForceHOPO = forceEdges $ rrfHOPOs rr
    , Five.fiveTap = RTB.empty
    , Five.fiveOpen = RTB.empty
    , Five.fiveGems = RTB.merge (rrfStrums rr) (rrfHOPOs rr)
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

  findDef <- case level of
    ImportQuick -> return $ const Nothing
    ImportFull -> do
      fev <- need ("s" <> key <> ".fev") >>= runGetM (codecIn binFEV)
      return $ \eventName -> listToMaybe $ do
        let bsLower = B8.map toLower
        eventGroup <- fev.topLevelEventGroups
        guard $ bsLower eventGroup.name == "stems"
        event <- eventGroup.events
        guard $ bsLower event.name == eventName
        layer <- event.layers
        inst <- layer.soundDefInstances
        Left sdefName <- return inst.nameOrIndex
        let sdefName' = bsLower sdefName
        soundDef <- fev.soundDefs
        guard $ bsLower soundDef.name == sdefName'
        [waveform] <- return soundDef.waveforms
        return (T.toCaseFold $ TE.decodeLatin1 waveform.bankName, waveform.indexInBank)
  let guitarDef  = findDef "guitar"
      bassDef    = findDef "bass"
      drumsDef   = findDef "drums"
      backingDef = findDef "backing"
      allFSBNames = nubOrd $ map fst $ catMaybes [guitarDef, bassDef, drumsDef, backingDef]
  allFSB <- forM allFSBNames $ \bankName -> do
    r <- need $ bankName <> ".fsb"
    return (bankName, r)
  loadedFSB <- stackIO $ forConcurrently allFSB $ \(bankName, r) -> do
    streams <- loadAudio r
    return (bankName, streams)
  let getStream (bankName, index) = do
        streamsInFile <- lookup bankName loadedFSB
        streamsInFile !? fromIntegral index
      guitarStream  = guitarDef  >>= getStream
      bassStream    = bassDef    >>= getStream
      drumsStream   = drumsDef   >>= getStream
      backingStream = backingDef >>= getStream

  controlMid <- case level of
    ImportQuick -> return emptyChart
    ImportFull  -> needRead ("s" <> key <> "_control.mid")
      >>= \r -> F.loadRawMIDIReadable r >>= F.readMixedMIDI

  let loadGuitarBass inst = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          fiveDiffs <- forM diffs $ \(num, diff) -> do
            mid <- needRead (T.concat ["s", key, "_", inst, "_", num, ".mid"])
              >>= F.loadRawMIDIReadable >>= F.readMixedMIDI
            rr <- F.parseTrackReport $ F.s_tracks mid
            return (diff, (rr, importRRGuitarBass rr))
          return mempty
            { Five.fiveDifficulties = fmap snd $ Map.fromList fiveDiffs
            , Five.fiveSolo = maybe RTB.empty (rrfSolo . fst) $ lookup Expert fiveDiffs
            }
      loadDrums = case level of
        ImportQuick -> return mempty
        ImportFull  -> do
          rrDiffs <- fmap Map.fromList $ forM diffs $ \(num, diff) -> do
            r <- needRead $ T.concat ["s", key, "_drums_", num, ".mid"]
            mid <- F.loadRawMIDIReadable r >>= F.readMixedMIDI
            rrd <- F.parseTrackReport $ F.s_tracks mid
            return (diff, rrd)
          return
            ( importRRDrums rrDiffs
            , mempty { TD.tdDifficulties  = fmap importRRTrueDrums rrDiffs }
            , mempty { TD.tdDifficulties = fmap importRRHiddenDrums rrDiffs }
            )
      diffs = [("02", Easy), ("03", Medium), ("04", Hard), ("05", Expert)]
  guitar <- loadGuitarBass "guitar"
  bass <- loadGuitarBass "bass"
  (drums, trueDrums, hiddenDrums) <- loadDrums

  sectionNames <- case level of
    ImportQuick -> return RTB.empty
    ImportFull -> do
      control <- needRead (T.concat ["s", key, "_control.mid"]) >>= F.loadRawMIDIReadable >>= F.readMixedMIDI
      let sections = flip RTB.mapMaybe (F.s_tracks control) $ \evt -> case isNoteEdgeCPV evt of
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
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> controlMid
          { F.s_tracks = mempty
            { F.onyxParts = Map.fromList
              [ (F.FlexGuitar, mempty
                { F.onyxPartGuitar = guitar
                })
              , (F.FlexBass, mempty
                { F.onyxPartGuitar = bass
                })
              , (F.FlexDrums, mempty
                { F.onyxPartDrums = drums
                , F.onyxPartTrueDrums = trueDrums
                })
              , (F.FlexExtra "hidden-drums", mempty
                { F.onyxPartTrueDrums = hiddenDrums
                })
              ]
            , F.onyxEvents = mempty
              { eventsSections = simpleSection <$> sectionNames
              }
            }
          }
        ImportQuick -> emptyChart
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ do
      (name, bs) <- catMaybes [guitarStream, bassStream, drumsStream, backingStream]
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
      { song = Input . Named . fst <$> backingStream
      , parts = Parts $ HM.fromList $ catMaybes
        [ flip fmap guitarStream $ \(s, _) -> (F.FlexGuitar, PartSingle $ Input $ Named s)
        , flip fmap bassStream   $ \(s, _) -> (F.FlexBass  , PartSingle $ Input $ Named s)
        , flip fmap drumsStream  $ \(s, _) -> (F.FlexDrums , PartSingle $ Input $ Named s)
        ]
      , crowd = Nothing
      , comments = []
      , tuningCents = 0
      , fileTempo = Nothing
      }
    , parts = Parts $ HM.fromList
      [ (F.FlexGuitar, emptyPart
        { grybo = Just def
        })
      , (F.FlexBass, emptyPart
        { grybo = Just def
        })
      , (F.FlexDrums, (emptyPart :: Part SoftFile)
        { drums = Just $ emptyPartDrums DrumsTrue Kicks1x
        })
      {-
      , (F.FlexExtra "hidden-drums", def
        { partDrums = Just emptyPartDrums DrumsTrue Kicks1x
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

rrChannel7Lane :: RRChannel -> Maybe TD.TrueGem
rrChannel7Lane = \case
  RRC_LowTom  -> Just TD.Tom3
  RRC_HighTom -> Just TD.Tom1
  RRC_Kick    -> Just TD.Kick
  RRC_Snare   -> Just TD.Snare
  RRC_CrashL  -> Just TD.CrashL
  RRC_CrashR  -> Just TD.CrashR
  RRC_Hihat   -> Just TD.Hihat
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
rrDrumGuessTD :: RRDrum -> (TD.TrueGem, TD.TrueGemType, D.DrumVelocity)
rrDrumGuessTD = \case
  RR_Kick          -> (TD.Kick     , TD.GemNormal     , D.VelocityNormal) -- [48, [["Kick", 23132], ["HighTom", 4], ["Snare", 4]]]
  RR_HandClap      -> (TD.Hihat    , TD.GemNormal     , D.VelocityNormal) -- [49, [["Hihat", 734], ["Kick", 69], ["Snare", 27], ["LowTom", 1]]]
  RR_Snare         -> (TD.Snare    , TD.GemNormal     , D.VelocityNormal) -- [50, [["Snare", 16995], ["HighTom", 4], ["CrashR", 2], ["LowTom", 1]]]
  RR_SideStick     -> (TD.Snare    , TD.GemRim        , D.VelocityNormal) -- [51, [["Snare", 548], ["Hihat", 409], ["CrashR", 59]]]
  RR_ElectricSnare -> (TD.Snare    , TD.GemNormal     , D.VelocityNormal) -- [52, [["Snare", 31], ["LowTom", 30], ["Hihat", 27], ["HighTom", 24]]]
  RR_Tom6          -> (TD.Tom3     , TD.GemNormal     , D.VelocityNormal) -- [53, [["Hihat", 304], ["LowTom", 298], ["Snare", 60]]]
  RR_HihatClosed   -> (TD.Hihat    , TD.GemHihatClosed, D.VelocityNormal) -- [54, [["Hihat", 7161], ["LowTom", 10], ["HighTom", 9], ["CrashL", 6], ["Kick", 1]]]
  RR_Tom5          -> (TD.Tom3     , TD.GemNormal     , D.VelocityNormal) -- [55, [["LowTom", 1766], ["Hihat", 188], ["HighTom", 82], ["Snare", 3], ["CrashR", 2]]]
  RR_HihatPedal    -> (TD.HihatFoot, TD.GemNormal     , D.VelocityNormal) -- [56, [["Hihat", 4901], ["CrashL", 37]]]
  RR_Tom4          -> (TD.Tom2     , TD.GemNormal     , D.VelocityNormal) -- [57, [["HighTom", 1078], ["LowTom", 291], ["Kick", 9], ["Snare", 7]]]
  RR_HihatOpen     -> (TD.Hihat    , TD.GemHihatOpen  , D.VelocityNormal) -- [58, [["Hihat", 5923], ["CrashR", 205], ["CrashL", 6]]]
  RR_Tom3          -> (TD.Tom1     , TD.GemNormal     , D.VelocityNormal) -- [59, [["HighTom", 657], ["CrashR", 12], ["LowTom", 11], ["Snare", 2]]]
  RR_Tom2          -> (TD.Tom1     , TD.GemNormal     , D.VelocityNormal) -- [60, [["Hihat", 217], ["LowTom", 59], ["HighTom", 43], ["Snare", 5], ["CrashL", 2]]]
  RR_Crash1        -> (TD.CrashR   , TD.GemNormal     , D.VelocityNormal) -- [61, [["CrashR", 2922], ["CrashL", 228], ["Snare", 33], ["Hihat", 1]]]
  RR_Tom1          -> (TD.Tom1     , TD.GemNormal     , D.VelocityNormal) -- [62, [["CrashR", 234], ["LowTom", 69], ["CrashL", 31], ["Snare", 20], ["HighTom", 6]]]
  RR_Ride          -> (TD.Ride     , TD.GemNormal     , D.VelocityNormal) -- [63, [["CrashR", 1440], ["Hihat", 70], ["Snare", 8], ["CrashL", 6]]]
  RR_China         -> (TD.CrashR   , TD.GemNormal     , D.VelocityNormal) -- [64, [["CrashR", 146], ["CrashL", 126], ["Snare", 79], ["LowTom", 56], ["HighTom", 3], ["Hihat", 3]]]
  RR_RideBell      -> (TD.Ride     , TD.GemNormal     , D.VelocityNormal) -- [65, [["CrashR", 196], ["LowTom", 66], ["CrashL", 8], ["Snare", 4], ["Hihat", 1]]]
  RR_Tambourine    -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [66, [["CrashL", 3189], ["CrashR", 87], ["Hihat", 7]]]
  RR_Splash        -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [67, [["CrashL", 187], ["HighTom", 2], ["CrashR", 1]]]
  RR_Cowbell       -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [68, [["CrashL", 2100], ["CrashR", 60], ["Hihat", 2], ["Snare", 1]]]
  RR_Crash2        -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [69, [["CrashL", 162], ["Kick", 10], ["HighTom", 2]]]
  RR_Vibraslap     -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [70, [["CrashL", 352], ["CrashR", 72], ["Hihat", 6]]]
  RR_Ride2         -> (TD.CrashL   , TD.GemNormal     , D.VelocityNormal) -- [71, [["CrashL", 36], ["Kick", 4]]]

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
    rrdGems      <- (rrdGems      =.) $ fatBlips (1/8) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 48
    rrdHidden    <- (rrdHidden    =.) $ fatBlips (1/8) $ condenseMap $ eachKey each $ \drum -> channelBlip_ $ fromEnum drum + 72
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

importRRTrueDrums :: RRDrumDifficulty U.Beats -> TD.TrueDrumDifficulty U.Beats
importRRTrueDrums rr = mempty
  { TD.tdGems
    = fmap (\gem -> (gem, TD.TBDefault, D.VelocityNormal))
    $ RTB.mapMaybe (rrChannel7Lane . snd)
    $ rrdGems rr
  }

importRRHiddenDrums :: RRDrumDifficulty U.Beats -> TD.TrueDrumDifficulty U.Beats
importRRHiddenDrums rr
  = TD.makeTrueDifficulty
  $ fmap (rrDrumGuessTD . fst)
  $ RTB.merge (rrdGems rr) (rrdHidden rr)
