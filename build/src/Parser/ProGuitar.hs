{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.ProGuitar where

import Parser.Base
import Parser.TH
import qualified Data.EventList.Relative.TimeBody as RTB

data Event
  = TrainerGtr Trainer
  | TrainerBass Trainer
  | HandPosition GtrFret
  | ChordRoot Int -- ^ Valid pitches are 4 (E) to 15 (D#).
  | ChordName Difficulty String
  | Trill Bool
  | Tremolo Bool
  | BRE Bool
  | Overdrive Bool
  | Solo Bool
  | NoChordNames Bool
  | SlashChords Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show)

data DiffEvent
  = Note GtrString GtrFret (Maybe NoteType)
  | ForceHOPO Bool
  | Slide SlideType Bool
  | Arpeggio Bool
  | PartialChord StrumArea Bool
  | AllFrets Bool
  deriving (Eq, Ord, Show)

data NoteType
  = NormalNote
  | ArpeggioForm
  | Bent
  | Muted
  | Tapped
  | Harmonic
  | PinchHarmonic
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SlideType = NormalSlide | ReversedSlide
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data StrumArea = High | Mid | Low
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

type GtrFret = Int

data GtrString = S6 | S5 | S4 | S3 | S2 | S1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- TODO
instanceMIDIEvent [t| Event |]
  [([e| \_ -> Nothing |], [e| \case _ -> RTB.empty |])]
