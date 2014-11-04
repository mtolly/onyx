module Scripts.Main where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.List (partition, sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe, isNothing)
import Text.Read (readMaybe)

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

-- | Ensures that there is a track name event in the tempo track.
tempoTrackName :: F.T -> F.T
tempoTrackName = uncurry fromStandardMIDI . first fixName . standardMIDI where
  fixName = RTB.cons 0 (E.MetaEvent $ Meta.TrackName "tempo")
    . RTB.filter (isNothing . isTrackName)

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
drumMix :: Int -> F.T -> F.T
drumMix n = let
  editTrack trk = if isDrums trk
    then newMix $ fmap editEvent trk
    else trk
  editEvent evt = case fromMix evt of
    Nothing -> evt
    Just (diff, _, disco) -> toMix (diff, n, disco)
  -- Add plain mix events to position 0 for diffs that don't have them
  newMix trk = let
    mixes = do
      evt <- eventsAtZero trk
      Just (diff, _, _) <- return $ fromMix evt
      return diff
    newMixes = [ toMix (diff, n, "") | diff <- [0..3], diff `notElem` mixes ]
    in foldr addZero trk newMixes
  fromMix :: E.T -> Maybe (Int, Int, String)
  fromMix (E.MetaEvent (Meta.TextEvent str)) = do
    ["[mix", d, rest] <- return $ words str
    diff              <- readMaybe d
    c : disco         <- stripPrefix "drums" rest
    disco'            <- stripSuffix "]" disco
    mix               <- readMaybe [c]
    return (diff, mix, disco')
  fromMix _ = Nothing
  stripSuffix sfx xs = fmap reverse $ stripPrefix (reverse sfx) (reverse xs)
  toMix :: (Int, Int, String) -> E.T
  toMix (x, y, z) = E.MetaEvent $ Meta.TextEvent $
    "[mix " ++ show x ++ " drums" ++ show y ++ z ++ "]"
  in uncurry fromStandardMIDI . second (map editTrack) . standardMIDI

-- | Adds an event at position zero *after* all the other events there.
addZero :: (NNC.C t) => a -> RTB.T t a -> RTB.T t a
addZero x rtb = case RTB.viewL rtb of
  Nothing -> RTB.singleton NNC.zero x
  Just ((dt, y), rtb') -> if dt == NNC.zero
    then RTB.cons dt y $ addZero x rtb'
    else RTB.cons NNC.zero x rtb

make2xBassPedal :: F.T -> F.T
make2xBassPedal mid = let
  (tempo, trks) = standardMIDI mid
  in fromStandardMIDI tempo $ case partition isDrums trks of
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

fixResolution :: F.T -> F.T
fixResolution = uncurry fromStandardMIDI . standardMIDI

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

isTrackName :: E.T -> Maybe String
isTrackName (E.MetaEvent (Meta.TrackName s)) = Just s
isTrackName _                                = Nothing

eventsAtZero :: (NNC.C t) => RTB.T t a -> [a]
eventsAtZero rtb = case RTB.viewL $ RTB.collectCoincident rtb of
  Just ((t, xs), _) | t == NNC.zero -> xs
  _ -> []

trackName :: (NNC.C t) => RTB.T t E.T -> Maybe String
trackName = listToMaybe . mapMaybe isTrackName . eventsAtZero

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

-- | Generates a BEAT track (if it doesn't exist already) which ends at the
-- [end] event from the EVENTS track.
autoBeat :: F.T -> F.T
autoBeat f = let
  (tempo, rest) = standardMIDI f
  eventsTrack = case filter (\t -> trackName t == Just "EVENTS") rest of
    t : _ -> t
    []    -> error "autoBeat: no EVENTS track found"
  endEvent = case findText "[end]" eventsTrack of
    t : _ -> t
    []    -> error "autoBeat: no [end] event found"
  hasBeat = not $ null $ filter (\t -> trackName t == Just "BEAT") rest
  beat = rtbTake endEvent $ makeBeatTrack tempo
  in if hasBeat
    then f
    else fromStandardMIDI tempo $ beat : rest

noteOn, noteOff :: Int -> E.T
noteOn p = E.MIDIEvent $ C.Cons (C.toChannel 0) $ C.Voice $
  V.NoteOn (V.toPitch p) $ V.toVelocity 96
noteOff p = E.MIDIEvent $ C.Cons (C.toChannel 0) $ C.Voice $
  V.NoteOff (V.toPitch p) $ V.toVelocity 0

-- | Makes an infinite BEAT track given the time signature track.
makeBeatTrack :: RTB.T Beats E.T -> RTB.T Beats E.T
makeBeatTrack = RTB.cons 0 (E.MetaEvent $ Meta.TrackName "BEAT") . go 4 where
  go :: Beats -> RTB.T Beats E.T -> RTB.T Beats E.T
  go sig sigs = let
    sig' = case mapMaybe isTimeSig $ eventsAtZero sigs of
      []    -> sig
      s : _ -> s
    in rtbGlue sig' infiniteMeasure $ go sig' $ rtbDrop sig' sigs
  isTimeSig :: E.T -> Maybe Beats
  isTimeSig (E.MetaEvent (Meta.TimeSig n d _ _)) = Just $ let
    writtenFraction = fromIntegral n / (2 ^ d)
    in 4 * writtenFraction
  isTimeSig _ = Nothing
  infiniteMeasure, infiniteBeats :: RTB.T Beats E.T
  infiniteMeasure
    = RTB.cons 0 (noteOn 12)
    $ RTB.cons (1/32) (noteOff 12)
    $ RTB.delay (1 - 1/32) infiniteBeats
  infiniteBeats
    = RTB.cons 0 (noteOn 13)
    $ RTB.cons (1/32) (noteOff 13)
    $ RTB.delay (1 - 1/32) infiniteBeats

rtbGlue :: (NNC.C t, Ord a) => t -> RTB.T t a -> RTB.T t a -> RTB.T t a
rtbGlue t xs ys = let
  xs' = rtbTake t xs
  gap = t NNC.-| NNC.sum (RTB.getTimes xs')
  in RTB.append xs' $ RTB.delay gap ys

rtbTake :: (NNC.C t, Ord a) => t -> RTB.T t a -> RTB.T t a
rtbTake t rtb = case RTB.viewL rtb of
  Nothing -> rtb
  Just ((dt, x), rtb') -> case NNC.split t dt of
    (_, (True, _)) {- t <= dt -} -> RTB.empty
    (_, (False, d)) {- t > dt -} -> RTB.cons dt x $ rtbTake d rtb'

rtbDrop :: (NNC.C t, Ord a) => t -> RTB.T t a -> RTB.T t a
rtbDrop t rtb = case RTB.viewL rtb of
  Nothing -> rtb
  Just ((dt, x), rtb') -> case NNC.split t dt of
    (_, (True, d)) {- t <= dt -} -> RTB.cons d x rtb'
    (_, (False, d)) {- t > dt -} -> rtbDrop d rtb'
