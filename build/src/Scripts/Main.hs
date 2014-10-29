module Scripts.Main where

import Control.Applicative ((<$>))
import Data.List (partition, sort)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

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

import Scripts.MIDITime
import Audio

import Development.Shake

standardMIDI :: F.T -> (RTB.T Beats E.T, [RTB.T Beats E.T])
standardMIDI (F.Cons _ dvn trks) = case dvn of
  F.Ticks n -> case map (ticksToBeats n) trks of
    []     -> (RTB.empty, [])
    t : ts -> (t        , ts)
  _ -> error "standardMIDI: not ticks-based"

fromStandardMIDI :: RTB.T Beats E.T -> [RTB.T Beats E.T] -> F.T
fromStandardMIDI tempo rest = F.Cons F.Parallel (F.Ticks 480) $
  map (beatsToTicks 480) $ tempo : rest

make2xBassPedal :: FilePath -> FilePath -> Action ()
make2xBassPedal fin fout = do
  need [fin]
  liftIO $ do
    (tempo, trks) <- standardMIDI <$> Load.fromFile fin
    Save.toFile fout $ fromStandardMIDI tempo $ case partition isDrums trks of
      (drums, notdrums) -> map make2xBass drums ++ notdrums

makeCountin :: FilePath -> FilePath -> FilePath -> Action ()
makeCountin mid wavin wavout = do
  need [mid, wavin]
  (tempo, trks) <- liftIO $ standardMIDI <$> Load.fromFile mid
  let tmap = tempoMap tempo
      beats = sort $ concatMap (findText "countin_here") $ tempo : trks
      secs = map (realToFrac . beatsToSeconds tmap) beats :: [Rational]
      audio = case secs of
        [] -> Silence 2 1
        _ -> Combine Mix $ map (\t -> Unary [Pad Begin t] $ File wavin) secs
  buildAudio audio wavout

fixResolution :: FilePath -> FilePath -> Action ()
fixResolution fin fout = do
  need [fin]
  liftIO $ do
    (tempo, trks) <- standardMIDI <$> Load.fromFile fin
    Save.toFile fout $ fromStandardMIDI tempo trks

previewBounds :: FilePath -> Action (Int, Int)
previewBounds mid = do
  need [mid]
  liftIO $ do
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
        ms n = floor $ n * 1000
    return (ms start, ms end)

replaceTempos :: FilePath -> FilePath -> FilePath -> Action ()
replaceTempos fin ftempo fout = do
  need [fin, ftempo]
  liftIO $ do
    (_    , trks) <- standardMIDI <$> Load.fromFile fin
    (tempo, _   ) <- standardMIDI <$> Load.fromFile ftempo
    Save.toFile fout $ fromStandardMIDI tempo trks

songLength :: FilePath -> Action Int
songLength mid = do
  need [mid]
  liftIO $ do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    let tmap = tempoMap tempo
    case concatMap (findText "[end]") trks of
      [bts] -> return $ floor $ beatsToSeconds tmap bts * 1000
      results -> error $
        "Error: " ++ show (length results) ++ " [end] events found"

magmaClean :: FilePath -> FilePath -> Action ()
magmaClean fin fout = do
  need [fin]
  liftIO $ do
    (tempo, trks) <- standardMIDI <$> Load.fromFile fin
    Save.toFile fout $ fromStandardMIDI
      (fromMaybe RTB.empty $ magmaClean' tempo)
      (mapMaybe magmaClean' trks)

isDrums :: (NNC.C t) => RTB.T t E.T -> Bool
isDrums t = trackName t == Just "PART DRUMS"

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

magmaClean' :: (NNC.C t) => RTB.T t E.T -> Maybe (RTB.T t E.T)
magmaClean' trk = case trackName trk of
  Just "countin"    -> Nothing
  Just "PART DRUMS" -> Just $ removePitch (V.toPitch 95) $ removeComments trk
  _                 -> Just $ removeComments trk
  where removePitch p = RTB.filter $ \x -> case x of
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn  p' _))) | p == p' -> False
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p' _))) | p == p' -> False
          _ -> True
        removeComments = RTB.filter $ \x -> case x of
          E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
          _ -> True
