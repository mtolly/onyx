module OneFoot (oneFoot) where

import Sound.MIDI.File as F
import Sound.MIDI.File.Event as E
import Sound.MIDI.File.Event.Meta as M
import Sound.MIDI.Message.Channel as C
import Sound.MIDI.Message.Channel.Voice as V
import Sound.MIDI.File.Load as Load
import Sound.MIDI.File.Save as Save

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Numeric.NonNegative.Wrapper as NN
import qualified Numeric.NonNegative.Class as NNC

import qualified Data.Map as Map

import Development.Shake

type Beats   = NN.Rational
type Seconds = NN.Rational
type BPS     = NN.Rational

type TempoMap = Map.Map Beats (Seconds, BPS)

getResolution :: F.T -> NN.Int
getResolution (F.Cons _ (F.Ticks res) _) = res
getResolution _ = error "getResolution: not a ticks-based MIDI file"

getBeatTracks :: F.T -> [RTB.T Beats E.T]
getBeatTracks (F.Cons F.Parallel (F.Ticks res) trks)
  = map (RTB.mapTime $ \tks -> fromIntegral tks / fromIntegral res) trks
getBeatTracks _ = error "getBeatTracks: not a parallel ticks-based MIDI file"

fromBeatTracks :: NN.Int -> [RTB.T Beats E.T] -> F.T
fromBeatTracks res = F.Cons F.Parallel (F.Ticks res) . map f where
  f = RTB.discretize . RTB.mapTime (* fromIntegral res)

getTempoMap :: F.T -> TempoMap
getTempoMap f = case getBeatTracks f of
  []        -> Map.empty
  tempo : _ -> makeTempoMap $ RTB.mapMaybe getTempo tempo

getTempo :: E.T -> Maybe BPS
getTempo (E.MetaEvent (M.SetTempo microSecPerBeat)) = Just $ let
  secPerBeat = fromIntegral microSecPerBeat / 1000000
  in recip secPerBeat
getTempo _ = Nothing

applyTempo :: BPS -> Beats -> Seconds
applyTempo bps bts = bts / bps

makeTempoMap :: RTB.T Beats BPS -> TempoMap
makeTempoMap = Map.fromList . go 0 0 2 where
  go :: Beats -> Seconds -> BPS -> RTB.T Beats BPS -> [(Beats, (Seconds, BPS))]
  go bts secs bps rtb = case RTB.viewL rtb of
    Nothing -> [(bts, (secs, bps))]
    Just ((dbts, bps'), rtb') -> let
      dsecs = applyTempo bps dbts
      rest = go (bts + dbts) (secs + dsecs) bps' rtb'
      in case dbts of
        0 -> rest
        _ -> (bts, (secs, bps)) : rest

-- | Can apply either a 'TempoMap' or 'UnTempoMap'.
applyTempoMap :: TempoMap -> Beats -> Seconds
applyTempoMap tmap bts = case Map.lookupLE bts tmap of
  Just (bts', (secs, bps)) -> secs + applyTempo bps (bts - bts')
  Nothing -> error $
    "applyTempoMap: found no tempo before position " ++ show bts

-- | Can apply either a 'TempoMap' or 'UnTempoMap'.
applyTempoMap' :: TempoMap -> RTB.T Beats a -> RTB.T Seconds a
applyTempoMap' tmap
  = RTB.fromAbsoluteEventList
  . ATB.mapTime (applyTempoMap tmap)
  . RTB.toAbsoluteEventList 0

type SPB = NN.Rational
type UnTempoMap = Map.Map Seconds (Beats, SPB)

makeUnTempoMap :: TempoMap -> UnTempoMap
makeUnTempoMap = Map.fromDistinctAscList . map f . Map.toAscList where
  f (bts, (secs, bps)) = (secs, (bts, recip bps))

data DrumColor = Kick | Red | Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getDrumHit :: E.T -> Maybe DrumColor
getDrumHit (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))))
  | V.fromVelocity vel /= 0
  = lookup (V.fromPitch p) $ zip [96 .. 100] [Kick .. Green]
getDrumHit _ = Nothing

trackName :: RTB.T Beats E.T -> Maybe String
trackName rtb = case RTB.viewL rtb of
  Just ((0, x), rtb') -> case x of
    E.MetaEvent (M.TrackName str) -> Just str
    _                             -> trackName rtb'
  _ -> Nothing

data Hand = L | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

assignFeet
  :: Seconds -- ^ The period at which a steady stream of kicks become two feet.
  -> Seconds -- ^ The period at which two isolated kicks become two feet.
  -> RTB.T Seconds DrumColor
  -> RTB.T Seconds Hand
assignFeet timeX timeY = go . RTB.toPairList . fmap (const R) . RTB.filter (== Kick) where
  {-
  1. All kicks start out R.
  2. Any kicks that are bordered on both sides by an R within time X turn L.
  3. Any kicks that follow an R within some smaller time Y time turn L.
  4. Any kicks that follow an R within time X turn L if the R follows
     an L within time X. This is because an isolated KK should become RR,
     but an isolated KKKK (at the same speed) should become RLRL, not RLRR.
  -}
  go = RTB.fromPairList . rule4 . rule3 . rule2
  rule2 pairs = case pairs of
    (t1, R) : (t2, R) : rest@((t3, R) : _)
      | t2 < timeX && t3 < timeX
      -> (t1, R) : (t2, L) : rule2 rest
    p : ps -> p : rule2 ps
    [] -> []
  rule3 pairs = case pairs of
    (t1, R) : (t2, R) : rest
      | t2 < timeY
      -> (t1, R) : (t2, L) : rule3 rest
    p : ps -> p : rule3 ps
    [] -> []
  rule4 pairs = case pairs of
    (t1, L) : (t2, R) : (t3, R) : rest
      | t2 < timeX && t3 < timeX
      -> (t1, L) : (t2, R) : rule4 ((t3, L) : rest)
    p : ps -> p : rule4 ps
    [] -> []

removePitch :: (NNC.C t) => V.Pitch -> RTB.T t E.T -> RTB.T t E.T
removePitch p = RTB.filter $ \e -> case e of
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn  p' _))) | p == p' -> False
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p' _))) | p == p' -> False
  _                                                           -> True

rtbJoin :: (NNC.C t, Ord a) => RTB.T t (RTB.T t a) -> RTB.T t a
rtbJoin rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> RTB.delay dt $ RTB.merge x $ rtbJoin rtb'

thinKicks :: Seconds -> Seconds -> RTB.T Seconds E.T -> RTB.T Seconds E.T
thinKicks tx ty rtb = let
  hits = RTB.mapMaybe getDrumHit rtb
  kicks = assignFeet tx ty hits
  on p = E.MIDIEvent (C.Cons (C.toChannel 0) (C.Voice (V.NoteOn p (V.toVelocity 96))))
  off p = E.MIDIEvent (C.Cons (C.toChannel 0) (C.Voice (V.NoteOff p (V.toVelocity 96))))
  kick = RTB.fromPairList [(0, on $ V.toPitch 96), (1/32, off $ V.toPitch 96)]
  rights = rtbJoin $ fmap (const kick) $ RTB.filter (== R) kicks
  in RTB.merge rights $ removePitch (V.toPitch 96) rtb

oneFoot :: Seconds -> Seconds -> FilePath -> FilePath -> Action ()
oneFoot tx ty fin fout = do
  need [fin]
  liftIO $ do
    mid <- Load.fromFile fin
    let trks = getBeatTracks mid
        tmap = getTempoMap mid
        untmap = makeUnTempoMap tmap
        trks' = flip map trks $ \trk -> case trackName trk of
          Just "PART DRUMS" -> applyTempoMap' untmap $
            thinKicks tx ty $ applyTempoMap' tmap trk
          _                 -> trk
    Save.toFile fout $ fromBeatTracks (getResolution mid) trks'
