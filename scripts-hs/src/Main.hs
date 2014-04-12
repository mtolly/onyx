module Main where

import Control.Applicative ((<$>))
import Control.Monad (forM, when)
import Data.Fixed (Milli)
import Data.List (partition, sort)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import System.Environment (getArgs, getProgName)

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event.Meta as Meta
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

import MIDITime

standardMIDI :: F.T -> (RTB.T Beats E.T, [RTB.T Beats E.T])
standardMIDI (F.Cons _ dvn trks) = case dvn of
  F.Ticks n -> case map (ticksToBeats n) trks of
    []     -> (RTB.empty, [])
    t : ts -> (t        , ts)
  _ -> error "standardMIDI: not ticks-based"

fromStandardMIDI :: RTB.T Beats E.T -> [RTB.T Beats E.T] -> F.T
fromStandardMIDI tempo rest = F.Cons F.Parallel (F.Ticks 480) $
  map (beatsToTicks 480) $ tempo : rest

main :: IO ()
main = getArgs >>= \argv -> case argv of

  ["2x-bass-pedal", fin, fout] -> do
    (tempo, trks) <- standardMIDI <$> Load.fromFile fin
    Save.toFile fout $ fromStandardMIDI tempo $ case partition isDrums trks of
      (drums, notdrums) -> map make2xBass drums ++ notdrums

  ["countin", mid, wavin, wavout] -> do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    let tmap = tempoMap tempo
        beats = sort $ concatMap (findText "countin_here") $ tempo : trks
        secs = map (beatsToSeconds tmap) beats
        secstrs = map (\s -> show (realToFrac s :: Milli)) secs
    case secstrs of
      [] -> callProcess "sox" $ [wavin, wavout] ++ words "pad 1 trim 0 1"
      [s] -> callProcess "sox" [wavin, wavout, "pad", s]
      _ -> withSystemTempDirectory "countin" $ \d -> do
        files <- forM secstrs $ \s -> do
          let f = d </> (s ++ ".wav")
          callProcess "sox" [wavin, f, "pad", s]
          return f
        callProcess "sox" $
          words "--combine mix -v 1" ++ files ++ [wavout]

  ["fix-resolution", mid] -> do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    Save.toFile mid $ fromStandardMIDI tempo trks

  "preview-bounds" : mid : rest -> do
    showEnd <- case rest of
      ["start"] -> return False
      ["both"]  -> return True
      []        -> return True
      _         -> error $ "Invalid mode for preview-bounds: " ++ show rest
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    let find s = listToMaybe $ sort $ concatMap (findText s) (tempo : trks)
        tmap = tempoMap tempo
        starts = ["preview_start", "[prc_chorus]", "[prc_chorus_1]"]
        start = case mapMaybe find starts of
          []      -> 0
          bts : _ -> max 0 $ beatsToSeconds tmap bts - 0.6
        end = case find "preview_end" of
          Nothing  -> start + 30
          Just bts -> beatsToSeconds tmap bts
        ms n = show (floor $ n * 1000 :: Int)
    putStr $ ms start
    when showEnd $ do
      putChar ' '
      putStr $ ms end
    putChar '\n'

  ["replace-tempos", fin, ftempo, fout] -> do
    (_    , trks) <- standardMIDI <$> Load.fromFile fin
    (tempo, _   ) <- standardMIDI <$> Load.fromFile ftempo
    Save.toFile fout $ fromStandardMIDI tempo trks

  ["song-length", mid] -> do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    let tmap = tempoMap tempo
    case concatMap (findText "[end]") trks of
      [bts] -> let
        ms = floor $ beatsToSeconds tmap bts * 1000 :: Int
        in print ms
      results -> error $
        "Error: " ++ show (length results) ++ " [end] events found"

  ["magma-clean", mid] -> do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    Save.toFile mid $ fromStandardMIDI
      (fromMaybe RTB.empty $ magmaClean tempo)
      (mapMaybe magmaClean trks)

  _ -> do
    prog <- getProgName
    error $ prog ++ ": invalid arguments: " ++ show argv
  where isDrums t = trackName t == Just "PART DRUMS"

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName rtb = case RTB.viewL $ RTB.collectCoincident rtb of
  Just ((t, xs), _) | t == NNC.zero -> let
    isTrackName (E.MetaEvent (Meta.TrackName s)) = Just s
    isTrackName _                                = Nothing
    in listToMaybe $ mapMaybe isTrackName xs
  _ -> Nothing

-- | Move all notes on pitch 95 to pitch 96.
-- TODO: Ensure overlapping 95/96 pitches work by removing all note-offs and
-- making new ones.
make2xBass :: RTB.T t E.T -> RTB.T t E.T
make2xBass = fmap $ \x -> case x of
  E.MIDIEvent      (C.Cons ch (C.Voice (V.NoteOn  p v))) | V.fromPitch p == 95
    -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOn  (V.toPitch 96) v)))
  E.MIDIEvent      (C.Cons ch (C.Voice (V.NoteOff p v))) | V.fromPitch p == 95
    -> E.MIDIEvent (C.Cons ch (C.Voice (V.NoteOff (V.toPitch 96) v)))
  _ -> x

findText :: String -> RTB.T Beats E.T -> [Beats]
findText s = ATB.getTimes . RTB.toAbsoluteEventList 0 . RTB.filter f where
  f (E.MetaEvent (Meta.TextEvent s')) | s == s' = True
  f _                                           = False

magmaClean :: (NNC.C t) => RTB.T t E.T -> Maybe (RTB.T t E.T)
magmaClean trk = case trackName trk of
  Just "countin"    -> Nothing
  Just "PART_DRUMS" -> Just $ removePitch (V.toPitch 95) $ removeComments trk
  _                 -> Just $ removeComments trk
  where removePitch p = RTB.filter $ \x -> case x of
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn  p' _))) | p == p' -> False
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p' _))) | p == p' -> False
          _ -> True
        removeComments = RTB.filter $ \x -> case x of
          E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
          _ -> True
