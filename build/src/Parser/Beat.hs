{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.Beat where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U

import Parser.Base

data Event = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: E.T -> Maybe [Event]
readEvent (MIDINote p b) = case V.fromPitch p of
  12 -> Just [Bar  | b]
  13 -> Just [Beat | b]
  _  -> Nothing
readEvent _ = Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = blip' . \case
  Bar  -> 12
  Beat -> 13
