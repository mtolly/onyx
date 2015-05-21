{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.ProGuitar where

import Parser.Base
import Parser.TH
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (guard)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Language.Haskell.TH

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
  = Note GtrString NoteType (Maybe GtrFret)
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

isNoteEdgeCPV :: E.T -> Maybe (Int, Int, Maybe Int)
isNoteEdgeCPV = \case
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOn  p v))) ->
    Just (C.fromChannel c, V.fromPitch p, case V.fromVelocity v of 0 -> Nothing; v' -> Just v')
  E.MIDIEvent (C.Cons c (C.Voice (V.NoteOff p _))) ->
    Just (C.fromChannel c, V.fromPitch p, Nothing)
  _ -> Nothing

makeEdgeCPV :: Int -> Int -> Maybe Int -> E.T
makeEdgeCPV c p v = E.MIDIEvent $ C.Cons (C.toChannel c) $ C.Voice $
  V.NoteOn (V.toPitch p) $ maybe (V.toVelocity 0) V.toVelocity v

noteTypeChannels :: [(Int, NoteType)]
noteTypeChannels = [ (fromEnum t, t) | t <- [minBound .. maxBound] ]

instanceMIDIEvent [t| Event |] $ let
  guitarNote :: Int -> Q Pat -> Q Pat -> (Q Exp, Q Exp)
  guitarNote pitch diff str =
    ( [e| \rtb -> do
      ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
      guard $ p == pitch
      guard $ maybe True (>= 100) v
      ntype <- lookup c noteTypeChannels
      let e = DiffEvent $(fmap patToExp diff) $ Note $(fmap patToExp str) ntype $ fmap (subtract 100) v
      return ((t, e), rtb')
    |], [e| \case
      DiffEvent $diff (Note $str ntype fret) ->
        RTB.singleton 0 $ makeEdgeCPV (fromEnum ntype) pitch $ fmap (+ 100) fret
    |])
  in  [ guitarNote 24  [p| Easy   |] [p| S6 |]
      , guitarNote 25  [p| Easy   |] [p| S5 |]
      , guitarNote 26  [p| Easy   |] [p| S4 |]
      , guitarNote 27  [p| Easy   |] [p| S3 |]
      , guitarNote 28  [p| Easy   |] [p| S2 |]
      , guitarNote 29  [p| Easy   |] [p| S1 |]

      , guitarNote 48  [p| Medium |] [p| S6 |]
      , guitarNote 49  [p| Medium |] [p| S5 |]
      , guitarNote 50  [p| Medium |] [p| S4 |]
      , guitarNote 51  [p| Medium |] [p| S3 |]
      , guitarNote 52  [p| Medium |] [p| S2 |]
      , guitarNote 53  [p| Medium |] [p| S1 |]

      , guitarNote 72  [p| Hard   |] [p| S6 |]
      , guitarNote 73  [p| Hard   |] [p| S5 |]
      , guitarNote 74  [p| Hard   |] [p| S4 |]
      , guitarNote 75  [p| Hard   |] [p| S3 |]
      , guitarNote 76  [p| Hard   |] [p| S2 |]
      , guitarNote 77  [p| Hard   |] [p| S1 |]

      , guitarNote 96  [p| Expert |] [p| S6 |]
      , guitarNote 97  [p| Expert |] [p| S5 |]
      , guitarNote 98  [p| Expert |] [p| S4 |]
      , guitarNote 99  [p| Expert |] [p| S3 |]
      , guitarNote 100 [p| Expert |] [p| S2 |]
      , guitarNote 101 [p| Expert |] [p| S1 |]

      , ([e| \_ -> Nothing |], [e| \case _ -> RTB.empty |]) -- TODO
      ]
