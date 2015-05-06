module Parser.Beat where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U

data Event = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: E.T -> Maybe [Event]
readEvent e = case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))) ->
    (if V.fromVelocity vel == 0 then noteOff else noteOn) p
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> noteOff p
  _ -> Nothing
  where noteOn p = case V.fromPitch p of
          12 -> Just [Bar]
          13 -> Just [Beat]
          _  -> Nothing
        noteOff p = case V.fromPitch p of
          12 -> Just []
          13 -> Just []
          _  -> Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent e = let
  p = case e of
    Bar  -> 12
    Beat -> 13
  note True  = ch0 $ V.NoteOn  (V.toPitch p) (V.toVelocity 96)
  note False = ch0 $ V.NoteOff (V.toPitch p) (V.toVelocity 0 )
  ch0 = E.MIDIEvent . C.Cons (C.toChannel 0) . C.Voice
  in RTB.fromPairList [(0, note True), (1/32, note False)]
