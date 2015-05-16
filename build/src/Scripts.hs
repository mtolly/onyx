{-# LANGUAGE LambdaCase #-}
module Scripts where

import Data.List (sort)
import Data.Maybe (mapMaybe)

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.Util as U

import Audio
import qualified Data.Conduit.Audio as CA

import StackTrace
import Parser.File
import qualified Parser.Drums as Drums
import qualified Parser.Beat as Beat
import qualified Parser.Events as Events
import qualified Parser.FiveButton as Five
import Parser.Base

import Development.Shake

-- | Changes all existing drum mix events to use the given config (not changing
-- stuff like discobeat), and places ones at the beginning if they don't exist
-- already.
drumMix :: Int -> Song U.Beats -> Song U.Beats
drumMix n s = let
  newTracks = flip map (s_tracks s) $ \case
    PartDrums t -> PartDrums $ editTrack t
    trk         -> trk
  editTrack trk = let
    (mixes, notMixes) = flip RTB.partitionMaybe trk $ \case
      Drums.SetMix mix -> Just mix
      _                -> Nothing
    mixes' = fmap (\(Drums.Mix diff _ disco) -> Drums.Mix diff audio disco) mixes
    audio = toEnum n
    alreadyMixed = [ diff | Drums.Mix diff _ _ <- U.trackTakeZero mixes' ]
    addedMixes =
      [ Drums.Mix diff audio Drums.NoDisco
      | diff <- [Easy .. Expert]
      , diff `notElem` alreadyMixed
      ]
    in RTB.merge notMixes $ fmap Drums.SetMix $ foldr addZero mixes' addedMixes
  in s { s_tracks = newTracks }

-- | Adds an event at position zero *after* all the other events there.
addZero :: (NNC.C t) => a -> RTB.T t a -> RTB.T t a
addZero x rtb = case U.trackSplitZero rtb of
  (zero, rest) -> U.trackGlueZero (zero ++ [x]) rest

makeCountin :: FilePath -> FilePath -> FilePath -> Action ()
makeCountin mid wavin wavout = do
  need [wavin]
  song <- loadMIDI mid
  let tmap = s_tempos song
      beats = sort $ flip concatMap (s_tracks song) $ \case
        Countin trk -> ATB.getTimes $ RTB.toAbsoluteEventList 0 trk
        _           -> []
      secs = map (realToFrac . U.applyTempoMap tmap) beats :: [Double]
      audio = case secs of
        [] -> Silence 2 $ CA.Seconds 0
        _ -> Mix $ map (\t -> Pad Start (CA.Seconds t) $ Input $ Sndable wavin Nothing) secs
  buildAudio audio wavout

loadMIDI :: FilePath -> Action (Song U.Beats)
loadMIDI fp = do
  need [fp]
  mid <- liftIO $ Load.fromFile fp
  printStackTraceIO $ readMIDIFile mid

saveMIDI :: FilePath -> Song U.Beats -> Action ()
saveMIDI fp song = liftIO $ Save.toFile fp $ showMIDIFile song

allEvents :: (NNC.C t) => Song t -> RTB.T t Events.Event
allEvents = foldr RTB.merge RTB.empty . mapMaybe getEvents . s_tracks where
  getEvents (Events trk) = Just trk
  getEvents _            = Nothing

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds :: FilePath -> Action (Int, Int)
previewBounds mid = do
  song <- loadMIDI mid
  let starts = map Events.PracticeSection ["chorus", "chorus_1", "verse", "verse_1"]
      events = allEvents song
      find s = fmap (fst . fst) $ RTB.viewL $ RTB.filter (== s) events
      start = case mapMaybe find starts of
        []      -> 0
        bts : _ -> max 0 $ U.applyTempoMap (s_tempos song) bts - 0.6
      end = start + 30
      ms n = floor $ n * 1000
  return (ms start, ms end)

songLength' :: Song U.Beats -> U.Beats
songLength' s = case RTB.getTimes $ RTB.filter (== Events.Simple Events.End) $ allEvents s of
  [bts] -> bts
  results -> error $ "songLength': error, " ++ show (length results) ++ " [end] events found"

-- | Returns the time of the [end] event in milliseconds.
songLength :: FilePath -> Action Int
songLength mid = do
  song <- loadMIDI mid
  return $ floor $ U.applyTempoMap (s_tempos song) (songLength' song) * 1000

-- | Generates a BEAT track (if it doesn't exist already) which ends at the
-- [end] event from the EVENTS track.
autoBeat :: Song U.Beats -> Song U.Beats
autoBeat s = let
  hasBeat = flip any (s_tracks s) $ \case
    Beat _ -> True
    _      -> False
  autoTrack = Beat $ U.trackTake (songLength' s) $ makeBeatTrack $ s_signatures s
  in if hasBeat then s else s { s_tracks = autoTrack : s_tracks s }

-- | Given a measure map, produces an infinite BEAT track.
makeBeatTrack :: U.MeasureMap -> RTB.T U.Beats Beat.Event
makeBeatTrack mmap = go 0 where
  go i = let
    len = U.unapplyMeasureMap mmap (i + 1, 0) - U.unapplyMeasureMap mmap (i, 0)
    -- the rounding below ensures that
    -- e.g. the sig must be at least 3.5 to get bar-beat-beat-beat.
    -- if it's 3.25, then you would get a beat 0.25 before the next bar,
    -- which Magma doesn't like...
    thisMeasure = U.trackTake (fromInteger $ simpleRound len) infiniteMeasure
    -- simpleRound always rounds 0.5 up,
    -- unlike round which rounds to the nearest even number.
    simpleRound frac = case properFraction frac :: (Integer, U.Beats) of
      (_, 0.5) -> ceiling frac
      _        -> round frac
    in trackGlue len thisMeasure $ go $ i + 1
  infiniteMeasure, infiniteBeats :: RTB.T U.Beats Beat.Event
  infiniteMeasure = RTB.cons 0 Beat.Bar  $ RTB.delay 1 infiniteBeats
  infiniteBeats   = RTB.cons 0 Beat.Beat $ RTB.delay 1 infiniteBeats

trackGlue :: (NNC.C t, Ord a) => t -> RTB.T t a -> RTB.T t a -> RTB.T t a
trackGlue t xs ys = let
  xs' = U.trackTake t xs
  gap = t NNC.-| NNC.sum (RTB.getTimes xs')
  in RTB.append xs' $ RTB.delay gap ys

-- | Adjusts instrument tracks so rolls on notes 126/127 end just a tick after
--- their last gem note-on.
fixRolls :: Song U.Beats -> Song U.Beats
fixRolls s = let
  newTracks = flip map (s_tracks s) $ \case
    PartDrums  t -> PartDrums  $ drumsSingle $ drumsDouble t
    PartGuitar t -> PartGuitar $ fiveTremolo $ fiveTrill   t
    PartBass   t -> PartBass   $ fiveTremolo $ fiveTrill   t
    PartKeys   t -> PartKeys   $               fiveTrill   t
    trk          -> trk
  drumsSingle = fixFreeform (== Drums.SingleRoll True) (== Drums.SingleRoll False) isHand
  drumsDouble = fixFreeform (== Drums.DoubleRoll True) (== Drums.DoubleRoll False) isHand
  isHand (Drums.Note Expert gem) = gem /= Drums.Kick
  isHand _                       = False
  fiveTremolo = fixFreeform (== Five.Tremolo True) (== Five.Tremolo False) isGem
  fiveTrill   = fixFreeform (== Five.Trill   True) (== Five.Trill   False) isGem
  isGem (Five.Note Expert _ True) = True
  isGem _                         = False
  in s { s_tracks = newTracks }

fixFreeform
  :: (Ord a)
  => (a -> Bool) -- ^ start of a freeform section
  -> (a -> Bool) -- ^ end of a freeform section
  -> (a -> Bool) -- ^ events which are covered by the freeform section
  -> RTB.T U.Beats a
  -> RTB.T U.Beats a
fixFreeform isStart isEnd isCovered = RTB.flatten . go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evts), rtb') -> RTB.cons dt evts $ if any isStart evts
      then case U.extractFirst (\x -> if isEnd x then Just x else Nothing) $ RTB.flatten rtb' of
        Nothing -> RTB.cons dt evts $ go rtb' -- probably an error
        Just ((oldLength, theEnd), rtb'noEnd) -> let
          coveredEvents = U.trackTake oldLength $ RTB.filter (any isCovered) rtb'
          newLength = case reverse $ ATB.getTimes $ RTB.toAbsoluteEventList 0 coveredEvents of
            pos : _ -> pos + 1/32
            _       -> oldLength
          in RTB.insert newLength [theEnd] $ go $ RTB.collectCoincident rtb'noEnd
      else go rtb'
