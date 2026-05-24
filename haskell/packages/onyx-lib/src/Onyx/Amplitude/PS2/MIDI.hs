{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Amplitude.PS2.MIDI where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Onyx.Amplitude.Track
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.File             (parseTrackReport, stripTrack)
import           Onyx.StackTrace                  (SendMessage, StackTraceT,
                                                   inside, warn)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data AmpTrack t
  = Catch TrackNumber AmpInstrument T.Text (AmpCatchTrack t)
  | AxeContour TrackNumber (FreestyleTrack t)
  | AxeHarmony TrackNumber (AxeHarmonyTrack t)
  | Scratch TrackNumber ScratchNumber T.Text (FreestyleTrack t)
  | BG T.Text (AmpSampleTrack t)
  | Intro (AmpSampleTrack t)
  | World (WorldTrack t)
  | Bank (BankTrack t)

type TrackNumber = Int

data ScratchNumber = Scratch1 | Scratch2 | Scratch3

data AmpInstrument
  = AmpDrums
  | AmpBass
  | AmpSynth
  | AmpVocal
  | AmpGuitar
  | AmpFX
  deriving (Eq, Ord, Show, Enum, Bounded)

data AmpCatchTrack t = AmpCatchTrack
  { catch   :: CatchTrack t
  , samples :: AmpSampleTrack t
  }

instance ParseTrack AmpCatchTrack where
  parseTrack = do
    catch   <- (.catch  ) =. parseTrack
    samples <- (.samples) =. parseTrack
    return AmpCatchTrack{..}

data FreestyleTrack t = FreestyleTrack
  { samples :: AmpSampleTrack t
  -- TODO notes 96 100 103
  }

instance ParseTrack FreestyleTrack where
  parseTrack = do
    samples <- (.samples) =. parseTrack
    return FreestyleTrack{..}

data WorldTrack t = WorldTrack
  { gems :: RTB.T t Gem
  }

instance ParseTrack WorldTrack where
  parseTrack = do
    gems <- ((.gems) =.) $ condenseMap_ $ eachKey each \gem -> do
      blip $ case gem of
        L -> 96
        M -> 98
        R -> 100
    return WorldTrack{..}

data BankTrack t = BankTrack
  { banks :: RTB.T t T.Text
  }

instance ParseTrack BankTrack where
  parseTrack = do
    banks <- (.banks) =. Codec
      { codecIn = slurpTrack \mt -> (mt.midiLyrics, mt { midiLyrics = RTB.empty })
      , codecOut = makeTrackBuilder $ fmap $ E.MetaEvent . Meta.TextEvent
      }
    return BankTrack{..}

data AmpSampleTrack t = AmpSampleTrack
  { notes   :: RTB.T t (Edge V.Velocity (C.Channel, V.Pitch))
  , bank    :: RTB.T t (C.Channel, Int)
  , program :: RTB.T t (C.Channel, Int)
  }

instance ParseTrack AmpSampleTrack where
  parseTrack = do
    notes <- (.notes) =. Codec
      { codecIn = slurpTrack \mt -> let
        (low, high) = Map.partitionWithKey (\p _ -> V.fromPitch p < 96) mt.midiNotes
        triggers = foldr RTB.merge RTB.empty
          $ map (\(p, trk) -> fmap (, p) <$> trk)
          $ Map.toList low
        mt' = mt { midiNotes = high }
        in (triggers, mt')
      , codecOut = undefined -- TODO
      }
    bank <- (.bank) =. controller 0
    program <- (.program) =. Codec
      { codecIn = slurpTrack \mt -> let
        getProgramChange = \case
          E.MIDIEvent (C.Cons chan (C.Voice (V.ProgramChange val))) -> Just (chan, C.fromProgram val)
          _                                                         -> Nothing
        (pc, rest) = RTB.partitionMaybe getProgramChange mt.midiUnknown
        in (pc, mt { midiUnknown = rest })
      , codecOut = undefined -- TODO
      }
    return AmpSampleTrack{..}

data AxeHarmonyTrack t = AxeHarmonyTrack
  { notes :: RTB.T t (Edge V.Velocity (C.Channel, V.Pitch))
  }

instance ParseTrack AxeHarmonyTrack where
  parseTrack = do
    notes <- (.notes) =. Codec
      { codecIn = slurpTrack \mt -> let
        triggers = foldr RTB.merge RTB.empty
          $ map (\(p, trk) -> fmap (, p) <$> trk)
          $ Map.toList mt.midiNotes
        mt' = mt { midiNotes = Map.empty }
        in (triggers, mt')
      , codecOut = undefined -- TODO
      }
    return AxeHarmonyTrack{..}

parseAmpTrack :: (SendMessage m) => RTB.T U.Beats (E.T T.Text) -> StackTraceT m (Maybe (AmpTrack U.Beats))
parseAmpTrack trk = let
  runParser :: (SendMessage m, ParseTrack trk) => StackTraceT m (trk U.Beats)
  runParser = parseTrackReport $ stripTrack trk
  matchName trackName = case T.toUpper $ T.strip trackName of
    "INTRO" -> Just $ Intro <$> runParser
    "WORLD" -> Just $ World <$> runParser
    "BANK" -> Just $ Bank <$> runParser
    _ -> if "BG" `T.isPrefixOf` trackName
      then Just $ BG trackName <$> runParser
      else do
        (i, rest) <- T.stripPrefix "T" trackName >>= T.uncons
        index <- readMaybe [i]
        case T.splitOn ":" $ T.strip rest of
          ["AXE_CONTOUR"] -> Just $ AxeContour index <$> runParser
          ["AXE_HARMONY"] -> Just $ AxeHarmony index <$> runParser
          [track, instChar, name] -> do
            inst <- case instChar of
              "D" -> Just AmpDrums
              "B" -> Just AmpBass
              "S" -> Just AmpSynth
              "V" -> Just AmpVocal
              "G" -> Just AmpGuitar
              "F" -> Just AmpFX
              _   -> Nothing
            case T.toUpper $ T.strip track of
              "CATCH"       -> Just $ Catch index inst name <$> runParser
              "SCRATCH1"    -> Just $ Scratch index Scratch1 name <$> runParser
              "SCRATCH2"    -> Just $ Scratch index Scratch2 name <$> runParser
              "SCRATCH3"    -> Just $ Scratch index Scratch3 name <$> runParser
              "AXE_CONTOUR" -> Just $ AxeContour index <$> runParser
              "AXE_HARMONY" -> Just $ AxeHarmony index <$> runParser
              _             -> Nothing
          _ -> Nothing
  in case U.trackName trk of
    Nothing -> warn "Unnamed track" >> return Nothing
    Just name -> case matchName name of
      Nothing -> do
        warn $ "Unrecognized track name: " <> show name
        return Nothing
      Just parser -> inside ("Track: " <> show name) $ Just <$> parser

{-

T<num> CATCH:<type>:<name>
  gameplay notes
    easy 96 98 100
    medium 102 104 106
    hard 108 110 112
    expert 114 116 118
  sample playback
  types: D B S V G F
    (drums bass synth vox guitar fx)

T<num> AXE_CONTOUR
  notes: 96 100 103
  sample playback

T<num> AXE_HARMONY
  notes, low pitches

T<num> SCRATCH<1,2,3>:S:SCRATCH
  contents like AXE_CONTOUR

BG_CLICK
  sample playback
  "BG_Click" or "BG_click" seen

INTRO
  more than one track
  sample playback

WORLD
  notes: 96 98 100

BANK
  only text events, each one is a filename
  can have multiple at once (drumnbass_p.bnk and drumnbass_01_s_1.bnk)
  "Bank" seen

-}
