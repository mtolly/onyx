module Main where

import Data.List (partition)
import Data.Maybe (listToMaybe, mapMaybe)
import System.Environment (getArgs, getProgName)

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event.Meta as Meta

import MIDITime

main :: IO ()
main = getArgs >>= \argv -> case argv of
  ["2x-bass-pedal", fin, fout] -> do
    F.Cons typ dvn trks <- Load.fromFile fin
    let (tempo, trks') = case trks of
          []     -> (RTB.empty, [])
          t : ts -> (t        , ts)
    Save.toFile fout $ F.Cons typ dvn $ case partition isDrums trks' of
      (drums, notdrums) -> [tempo] ++ map make2xBass drums ++ notdrums
  _ -> do
    prog <- getProgName
    error $ prog ++ ": invalid arguments"
  where isDrums t = trackName t == Just "PART DRUMS"

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName rtb = case RTB.viewL $ RTB.collectCoincident rtb of
  Just ((t, xs), _) | t == NNC.zero -> let
    isTrackName (E.MetaEvent (Meta.TrackName s)) = Just s
    isTrackName _                                = Nothing
    in listToMaybe $ mapMaybe isTrackName xs
  _ -> Nothing

-- | Move all notes on pitch 95 to pitch 96.
make2xBass :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
make2xBass = fmap $ \x -> case x of
  E.MIDIEvent      (C.Cons ch (C.Voice (V.NoteOn  p v))) | V.fromPitch p == 95
    -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOn  (V.toPitch 96) v)))
  E.MIDIEvent      (C.Cons ch (C.Voice (V.NoteOff p v))) | V.fromPitch p == 95
    -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOff (V.toPitch 96) v)))
  _ -> x
