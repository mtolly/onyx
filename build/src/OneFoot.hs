module OneFoot (oneFoot) where

import Sound.MIDI.File as F
import Sound.MIDI.File.Event as E
import Sound.MIDI.Message.Channel as C
import Sound.MIDI.Message.Channel.Voice as V

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC

import qualified Sound.MIDI.Util as U

data DrumColor = Kick | Red | Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

getDrumHit :: E.T -> Maybe DrumColor
getDrumHit (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))))
  | V.fromVelocity vel /= 0
  = lookup (V.fromPitch p) $ zip [96 .. 100] [Kick .. Green]
getDrumHit _ = Nothing

data Hand = L | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

assignFeet
  :: U.Seconds -- ^ The period at which a steady stream of kicks become two feet.
  -> U.Seconds -- ^ The period at which two isolated kicks become two feet.
  -> RTB.T U.Seconds DrumColor
  -> RTB.T U.Seconds Hand
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

thinKicks :: U.Seconds -> U.Seconds -> RTB.T U.Seconds E.T -> RTB.T U.Seconds E.T
thinKicks tx ty rtb = let
  hits = RTB.mapMaybe getDrumHit rtb
  kicks = assignFeet tx ty hits
  on p = E.MIDIEvent (C.Cons (C.toChannel 0) (C.Voice (V.NoteOn p (V.toVelocity 96))))
  off p = E.MIDIEvent (C.Cons (C.toChannel 0) (C.Voice (V.NoteOff p (V.toVelocity 96))))
  kick = RTB.fromPairList [(0, on $ V.toPitch 96), (1/32, off $ V.toPitch 96)]
  -- ^ TODO: this should be 1/32 Beats, not Seconds, which means we need to take a TempoMap
  rights = U.trackJoin $ fmap (const kick) $ RTB.filter (== R) kicks
  in RTB.merge rights $ removePitch (V.toPitch 96) rtb

oneFoot :: U.Seconds -> U.Seconds -> F.T -> F.T
oneFoot tx ty mid = case U.decodeFile mid of
  Right _ -> error "oneFoot: SMPTE tracks not supported"
  Left trks -> let
    tmap = U.makeTempoMap $ head trks
    trks' = flip map trks $ \trk -> case U.trackName trk of
      Just "PART DRUMS" -> U.unapplyTempoTrack tmap $
        thinKicks tx ty $ U.applyTempoTrack tmap trk
      _                 -> trk
    in U.encodeFileBeats F.Parallel 480 trks'
