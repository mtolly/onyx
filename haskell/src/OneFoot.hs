-- | Algorithm to generate 1x Bass Pedal versions automatically from the 2x version.
{-# LANGUAGE LambdaCase #-}
module OneFoot (phaseShiftKicks, rockBand1x, rockBand2x) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Drums                   (Hand (..))
import qualified RockBand.Drums                   as Drums
import qualified Sound.MIDI.Util                  as U

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
    (t1, RH) : (t2, RH) : rest@((t3, RH) : _)
      | t2 < timeX && t3 < timeX
      -> (t1, RH) : (t2, LH) : rule2 rest
    p : ps -> p : rule2 ps
    [] -> []
  rule3 pairs = case pairs of
    (t1, RH) : (t2, RH) : rest
      | t2 < timeY
      -> (t1, RH) : (t2, LH) : rule3 rest
    p : ps -> p : rule3 ps
    [] -> []
  rule4 pairs = case pairs of
    (t1, LH) : (t2, RH) : (t3, RH) : rest
      | t2 < timeX && t3 < timeX
      -> (t1, LH) : (t2, RH) : rule4 ((t3, LH) : rest)
    p : ps -> p : rule4 ps
    [] -> []

-- | If there are no PS left kick notes, automatically chooses some if necessary.
phaseShiftKicks :: U.Seconds -> U.Seconds -> RTB.T U.Seconds Drums.Event -> RTB.T U.Seconds Drums.Event
phaseShiftKicks tx ty rtb = let
  (kicks, notKicks) = flip RTB.partitionMaybe rtb $ \case
    Drums.DiffEvent Expert (Drums.Note Drums.Kick) -> Just RH
    Drums.Kick2x                                   -> Just LH
    _                                              -> Nothing
  in if LH `elem` kicks
    then rtb
    else RTB.merge notKicks $ flip fmap (assignFeet tx ty kicks) $ \case
      RH -> Drums.DiffEvent Expert $ Drums.Note Drums.Kick
      LH -> Drums.Kick2x

rockBand1x :: (NNC.C t) => RTB.T t Drums.Event -> RTB.T t Drums.Event
rockBand1x = RTB.filter (/= Drums.Kick2x)

rockBand2x :: RTB.T t Drums.Event -> RTB.T t Drums.Event
rockBand2x = fmap $ \case
  Drums.Kick2x -> Drums.DiffEvent Expert $ Drums.Note Drums.Kick
  evt          -> evt
