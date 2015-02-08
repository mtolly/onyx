module Scripts.Main where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Data.List (partition, sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
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

import Audio

import Development.Shake

import qualified Sound.MIDI.Util as U

standardMIDI :: F.T -> (RTB.T U.Beats E.T, [RTB.T U.Beats E.T])
standardMIDI mid = case U.decodeFile mid of
  Left []       -> (RTB.empty, [])
  Left (t : ts) -> (t        , ts)
  Right _ -> error "standardMIDI: not ticks-based"

fromStandardMIDI :: RTB.T U.Beats E.T -> [RTB.T U.Beats E.T] -> F.T
fromStandardMIDI t ts = U.encodeFileBeats F.Parallel 480 $ t : ts

-- | Ensures that there is a track name event in the tempo track.
tempoTrackName :: F.T -> F.T
tempoTrackName =
  uncurry fromStandardMIDI . first (U.setTrackName "tempo") . standardMIDI

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
      evt <- U.trackTakeZero trk
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
addZero x rtb = case U.trackSplitZero rtb of
  (zero, rest) -> U.trackJoinZero (zero ++ [x]) rest

make2xBassPedal :: F.T -> F.T
make2xBassPedal mid = let
  (tempo, trks) = standardMIDI mid
  in fromStandardMIDI tempo $ case partition isDrums trks of
    (drums, notdrums) -> map make2xBass drums ++ notdrums

makeCountin :: FilePath -> FilePath -> FilePath -> Action ()
makeCountin mid wavin wavout = do
  need [mid, wavin]
  (tempo, trks) <- liftIO $ standardMIDI <$> Load.fromFile mid
  let tmap = U.makeTempoMap tempo
      beats = sort $ concatMap (findText "countin_here") $ tempo : trks
      secs = map (realToFrac . U.applyTempoMap tmap) beats :: [Rational]
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
        tmap = U.makeTempoMap tempo
        starts = ["preview_start", "[prc_chorus]", "[prc_chorus_1]"]
        start = case mapMaybe find starts of
          []      -> 0
          bts : _ -> max 0 $ U.applyTempoMap tmap bts - 0.6
        end = case find "preview_end" of
          Nothing  -> start + 30
          Just bts -> U.applyTempoMap tmap bts
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
    let tmap = U.makeTempoMap tempo
    case concatMap (findText "[end]") trks of
      [bts] -> return $ floor $ U.applyTempoMap tmap bts * 1000
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
isDrums t = U.trackName t == Just "PART DRUMS"

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

findText :: String -> RTB.T U.Beats E.T -> [U.Beats]
findText s = ATB.getTimes . RTB.toAbsoluteEventList 0 . RTB.filter f where
  f (E.MetaEvent (Meta.TextEvent s')) | s == s' = True
  f _                                           = False

magmaClean' :: (NNC.C t) => RTB.T t E.T -> Maybe (RTB.T t E.T)
magmaClean' trk = case U.trackName trk of
  Just "countin"    -> Nothing
  Just "PART DRUMS" -> Just $ removePitch (V.toPitch 95) $ removeText trk
  _                 -> Just $ removeText trk
  where removePitch p = RTB.filter $ \x -> case x of
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn  p' _))) | p == p' -> False
          E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p' _))) | p == p' -> False
          _ -> True
        removeText = RTB.filter $ \x -> case x of
          E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
          E.MetaEvent (Meta.TextEvent ('>' : _)) -> False
          _ -> True

-- | Generates a BEAT track (if it doesn't exist already) which ends at the
-- [end] event from the EVENTS track.
autoBeat :: F.T -> F.T
autoBeat f = let
  (tempo, rest) = standardMIDI f
  eventsTrack = case filter (\t -> U.trackName t == Just "EVENTS") rest of
    t : _ -> t
    []    -> error "autoBeat: no EVENTS track found"
  endEvent = case findText "[end]" eventsTrack of
    t : _ -> t
    []    -> error "autoBeat: no [end] event found"
  hasBeat = not $ null $ filter (\t -> U.trackName t == Just "BEAT") rest
  beat = U.trackTake endEvent $ makeBeatTrack tempo
  in if hasBeat
    then f
    else fromStandardMIDI tempo $ beat : rest

noteOn, noteOff :: Int -> E.T
noteOn p = E.MIDIEvent $ C.Cons (C.toChannel 0) $ C.Voice $
  V.NoteOn (V.toPitch p) $ V.toVelocity 96
noteOff p = E.MIDIEvent $ C.Cons (C.toChannel 0) $ C.Voice $
  V.NoteOff (V.toPitch p) $ V.toVelocity 0

isNoteOn, isNoteOff :: Int -> E.T -> Bool
isNoteOn p (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p' v))))
  = V.toPitch p == p' && V.fromVelocity v /= 0
isNoteOn _ _ = False
isNoteOff p (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p' v))))
  = V.toPitch p == p' && V.fromVelocity v == 0
isNoteOff p (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p' _))))
  = V.toPitch p == p'
isNoteOff _ _ = False

-- | Makes an infinite BEAT track given the time signature track.
makeBeatTrack :: RTB.T U.Beats E.T -> RTB.T U.Beats E.T
makeBeatTrack = RTB.cons 0 (E.MetaEvent $ Meta.TrackName "BEAT") . go 4 where
  go :: U.Beats -> RTB.T U.Beats E.T -> RTB.T U.Beats E.T
  go sig sigs = let
    sig' = case mapMaybe U.readSignature $ U.trackTakeZero sigs of
      []    -> sig
      s : _ -> s
    in trackGlue sig' infiniteMeasure $ go sig' $ U.trackDrop sig' sigs
  infiniteMeasure, infiniteBeats :: RTB.T U.Beats E.T
  infiniteMeasure
    = RTB.cons 0 (noteOn 12)
    $ RTB.cons (1/32) (noteOff 12)
    $ RTB.delay (1 - 1/32) infiniteBeats
  infiniteBeats
    = RTB.cons 0 (noteOn 13)
    $ RTB.cons (1/32) (noteOff 13)
    $ RTB.delay (1 - 1/32) infiniteBeats

trackGlue :: (NNC.C t, Ord a) => t -> RTB.T t a -> RTB.T t a -> RTB.T t a
trackGlue t xs ys = RTB.merge (U.trackTake t xs) (RTB.delay t ys)

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
-- their last gem note-on.
fixRolls :: F.T -> F.T
fixRolls = let
  isRollable t = U.trackName t `elem` map Just ["PART DRUMS", "PART BASS"]
  fixRollsTracks = map $ \t -> if isRollable t then fixRollsTrack t else t
  in uncurry fromStandardMIDI . second fixRollsTracks . standardMIDI

fixRollsTrack :: RTB.T U.Beats E.T -> RTB.T U.Beats E.T
fixRollsTrack = RTB.flatten . go . RTB.collectCoincident where
  go :: RTB.T U.Beats [E.T] -> RTB.T U.Beats [E.T]
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> case findNoteOn [126, 127] evts of
      -- Tremolo (single note or chord) or one-drum roll
      Just 126 -> case findNoteOn [97..100] evts of
        Nothing -> error "fixRollsTrack: found single-roll start without a gem"
        Just p -> let
          rollLength = findFirst [isNoteOff 126] rtb'
          lastGem = findLast [isNoteOn p] $ U.trackTake rollLength rtb'
          newOff = RTB.singleton (lastGem + 1/32) $ noteOff 126
          modified = RTB.collectCoincident
            $ RTB.merge newOff
            $ RTB.flatten
            $ removeNextOff 126 rtb'
          in RTB.cons dt evts $ go modified
      -- Trill or two-drum roll
      Just 127 -> case findNoteOn [97..100] evts of
        Nothing -> error $ "fixRollsTrack: found double-roll start without a gem"
        Just p1 -> case RTB.viewL $ RTB.mapMaybe (findNoteOn [97..100]) rtb' of
          Nothing -> error "fixRollsTrack: found double-roll without a 2nd gem"
          Just ((_, p2), _) -> let
            rollLength = findFirst [isNoteOff 127] rtb'
            lastGem = findLast [isNoteOn p1, isNoteOn p2] $ U.trackTake rollLength rtb'
            newOff = RTB.singleton (lastGem + 1/32) $ noteOff 127
            modified = RTB.collectCoincident
              $ RTB.merge newOff
              $ RTB.flatten
              $ removeNextOff 127 rtb'
            in RTB.cons dt evts $ go modified
      _ -> RTB.cons dt evts $ go rtb'
  findNoteOn :: [Int] -> [E.T] -> Maybe Int
  findNoteOn ps evts = listToMaybe $ filter (\p -> any (isNoteOn p) evts) ps
  removeNextOff :: Int -> RTB.T U.Beats [E.T] -> RTB.T U.Beats [E.T]
  removeNextOff p rtb = case RTB.viewL rtb of
    Nothing -> error $ "fixRollsTrack: unterminated note of pitch " ++ show p
    Just ((dt, evts), rtb') -> case partition (isNoteOff p) evts of
      ([]   , _    ) -> RTB.cons dt evts $ removeNextOff p rtb'
      (_ : _, evts') -> RTB.cons dt evts' rtb'
  findFirst :: [E.T -> Bool] -> RTB.T U.Beats [E.T] -> U.Beats
  findFirst fns rtb = let
    satisfy = any $ \e -> any ($ e) fns
    atb = RTB.toAbsoluteEventList 0 $ RTB.filter satisfy rtb
    in case ATB.toPairList atb of
      []           -> error "findFirst: couldn't find any events satisfying the functions"
      (bts, _) : _ -> bts
  findLast :: [E.T -> Bool] -> RTB.T U.Beats [E.T] -> U.Beats
  findLast fns rtb = let
    satisfy = any $ \e -> any ($ e) fns
    atb = RTB.toAbsoluteEventList 0 $ RTB.filter satisfy rtb
    in case reverse $ ATB.toPairList atb of
      []           -> error "findLast: couldn't find any events satisfying the functions"
      (bts, _) : _ -> bts
