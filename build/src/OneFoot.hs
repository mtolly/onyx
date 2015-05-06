module OneFoot (oneFoot) where

import Sound.MIDI.File as F

import qualified Data.EventList.Relative.TimeBody as RTB

import qualified Sound.MIDI.Util as U
import Parser
import Parser.Base
import Parser.File
import qualified Parser.Drums as Drums

data Hand = L | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

assignFeet
  :: U.Seconds -- ^ The period at which a steady stream of kicks become two feet.
  -> U.Seconds -- ^ The period at which two isolated kicks become two feet.
  -> RTB.T U.Seconds Hand
  -> RTB.T U.Seconds Hand
assignFeet timeX timeY = RTB.fromPairList . rule4 . rule3 . rule2 . RTB.toPairList where
  {-
  1. All kicks start out R.
  2. Any kicks that are bordered on both sides by an R within time X turn L.
  3. Any kicks that follow an R within some smaller time Y time turn L.
  4. Any kicks that follow an R within time X turn L if the R follows
     an L within time X. This is because an isolated KK should become RR,
     but an isolated KKKK (at the same speed) should become RLRL, not RLRR.
  -}
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

thinKicks :: U.Seconds -> U.Seconds -> RTB.T U.Seconds Drums.Event -> RTB.T U.Seconds Drums.Event
thinKicks tx ty rtb = let
  (kicks, notKicks) = RTB.partition (== Drums.Note Expert Drums.Kick) rtb
  assigned = assignFeet tx ty $ fmap (const R) kicks
  rightKicks = flip RTB.mapMaybe assigned $ \foot -> case foot of
    R -> Just $ Drums.Note Expert Drums.Kick
    L -> Nothing
  in RTB.merge rightKicks notKicks

thinKicksFile :: U.Seconds -> U.Seconds -> Song U.Beats -> Song U.Beats
thinKicksFile tx ty song = let
  f (PartDrums t) = PartDrums
    $ U.unapplyTempoTrack (s_tempos song)
    $ thinKicks tx ty
    $ U.applyTempoTrack (s_tempos song) t
  f trk = trk
  in song { s_tracks = map f $ s_tracks song }

oneFoot :: U.Seconds -> U.Seconds -> F.T -> F.T
oneFoot tx ty mid = case runParser $ readMIDIFile mid of
  (Left  msgs, _) -> error $ show msgs
  (Right song, _) -> showMIDIFile $ thinKicksFile tx ty song
