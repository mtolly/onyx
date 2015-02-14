{-# LANGUAGE ViewPatterns #-}
module Main where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import System.Environment (getArgs)

main :: IO ()
main = do
  [f] <- getArgs
  mid <- Load.fromFile f
  Save.toFile f $ case mid of
    F.Cons mtype mres mtrks -> F.Cons mtype mres $ flip map mtrks $ \trk ->
      case U.trackName trk of
        Just "PART DRUMS" -> flip fmap trk $ \e -> case e of
          E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOn (V.fromPitch -> 95) vel)))
            -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOn (V.toPitch 96) vel)))
          E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOff (V.fromPitch -> 95) vel)))
            -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOff (V.toPitch 96) vel)))
          _ -> e
        _ -> trk
