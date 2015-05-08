{-# LANGUAGE LambdaCase #-}
module Scripts where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.List (sort)
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Util as U

import Audio
import qualified Data.Conduit.Audio as CA

import Parser
import Parser.File
import qualified Parser.Drums as Drums
import qualified Parser.Beat as Beat
import qualified Parser.Events as Events
import qualified Parser.FiveButton as Five
import Parser.Base

import Development.Shake

loadSong :: FilePath -> Action (Song U.Beats)
loadSong fp = do
  need [fp]
  mid <- liftIO $ Load.fromFile fp
  case runParser $ readMIDIFile mid of
    (Left  msgs, _) -> fail $ show msgs
    (Right song, _) -> return song

liftSong :: (Song U.Beats -> Song U.Beats) -> F.T -> F.T
liftSong f mid = case runParser $ readMIDIFile mid of
  (Left  msgs, _) -> error $ show msgs
  (Right song, _) -> showMIDIFile $ f song

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
drumMix n = liftSong $ \s -> let
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
  song <- loadSong mid
  let tmap = s_tempos song
      beats = sort $ flip concatMap (s_tracks song) $ \case
        Countin trk -> ATB.getTimes $ RTB.toAbsoluteEventList 0 trk
        _           -> []
      secs = map (realToFrac . U.applyTempoMap tmap) beats :: [Double]
      audio = case secs of
        [] -> Silence 2 $ CA.Seconds 0
        _ -> Mix $ map (\t -> Pad Start (CA.Seconds t) $ Input $ Sndable wavin Nothing) secs
  buildAudio audio wavout

fixResolution :: F.T -> F.T
fixResolution = liftSong id

previewBounds :: FilePath -> Action (Int, Int)
previewBounds mid = do
  need [mid]
  liftIO $ do
    (tempo, trks) <- standardMIDI <$> Load.fromFile mid
    let find s = listToMaybe $ sort $ concatMap (findText s) (tempo : trks)
        tmap = U.makeTempoMap tempo
        starts = ["preview_start", "[prc_chorus]", "[prc_chorus_1]", "[prc_verse]", "[prc_verse_1]"]
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
  song   <- loadSong fin
  tempos <- loadSong ftempo
  liftIO $ Save.toFile fout $ showMIDIFile $ song { s_tempos = s_tempos tempos }

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

findText :: String -> RTB.T U.Beats E.T -> [U.Beats]
findText s = ATB.getTimes . RTB.toAbsoluteEventList 0 . RTB.filter f where
  f (E.MetaEvent (Meta.TextEvent s')) | s == s' = True
  f _                                           = False

magmaClean' :: (NNC.C t) => RTB.T t E.T -> Maybe (RTB.T t E.T)
magmaClean' trk = case U.trackName trk of
  Just "countin" -> Nothing
  Just "PART REAL_BASS" -> Nothing
  Just "PART REAL_BASS_22" -> Nothing
  Just "PART REAL_GUITAR" -> Nothing
  Just "PART REAL_GUITAR_22" -> Nothing
  _              -> Just $ removeText trk
  where removeText = RTB.filter $ \x -> case x of
          E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
          E.MetaEvent (Meta.TextEvent ('>' : _)) -> False
          _ -> True

-- | Generates a BEAT track (if it doesn't exist already) which ends at the
-- [end] event from the EVENTS track.
autoBeat :: F.T -> F.T
autoBeat = liftSong $ \s -> let
  hasBeat = flip any (s_tracks s) $ \case
    Beat _ -> True
    _      -> False
  endPosns = flip mapMaybe (s_tracks s) $ \case
    Events t -> do
      ((dt, _), _) <- RTB.viewL (RTB.filter (== Events.Simple Events.End) t)
      return dt
    _        -> Nothing
  endPosn = case endPosns of
    []    -> error "autoBeat: no [end] event found"
    p : _ -> p
  autoTrack = Beat $ U.trackTake endPosn $ makeBeatTrack $ s_signatures s
  in if hasBeat then s else s { s_tracks = autoTrack : s_tracks s }

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
fixRolls :: F.T -> F.T
fixRolls = liftSong $ \s -> let
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
