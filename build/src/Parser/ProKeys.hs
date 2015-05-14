-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.ProKeys where

import Parser.Base
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB

data Event
  = LaneShift LaneRange
  -- ^ Change the viewable play range. Should be placed at least a measure
  -- before any notes that require the new range.
  | Trainer Trainer -- ^ The beginning/end of Pro Keys trainer sections.
  | Mood Mood
  | Solo Bool -- ^ A keyboard solo section.
  | Glissando Bool -- ^ Place over a sequence of white notes for a freeform section.
  | Trill Bool -- ^ Fill lanes on two keys.
  | Overdrive Bool -- ^ An energy phrase.
  | BRE Bool -- ^ Fill lanes for a Big Rock Ending.
  | Note Int Bool -- ^ Valid pitches are in MIDI range 48 to 72.
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: E.T -> Maybe [Event]
readEvent (MIDINote p b) = case V.fromPitch p of
  0 -> Just [LaneShift C | b]
  2 -> Just [LaneShift D | b]
  4 -> Just [LaneShift E | b]
  5 -> Just [LaneShift F | b]
  7 -> Just [LaneShift G | b]
  9 -> Just [LaneShift A | b]
  i | 48 <= i && i <= 72 -> one $ Note i b
  115 -> one $ Solo b
  116 -> one $ Overdrive b
  120 -> one $ BRE b
  126 -> one $ Glissando b
  127 -> one $ Trill b
  _ -> Nothing
  where one x = Just [x]
readEvent (readCommand' -> Just mood) = Just [Mood mood]
readEvent (readCommand' -> Just (train, "key")) = Just [Trainer train]
readEvent _ = Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  LaneShift C -> blip' 0
  LaneShift D -> blip' 2
  LaneShift E -> blip' 4
  LaneShift F -> blip' 5
  LaneShift G -> blip' 7
  LaneShift A -> blip' 9
  Trainer   t -> one $ showCommand' (t, "key")
  Mood      m -> one $ showCommand' m
  Note    i b -> one $ edge' i   b
  Solo      b -> one $ edge' 115 b
  Overdrive b -> one $ edge' 116 b
  BRE       b -> one $ edge' 120 b
  Glissando b -> one $ edge' 126 b
  Trill     b -> one $ edge' 127 b
  where one = RTB.singleton 0
