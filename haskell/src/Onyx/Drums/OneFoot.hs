-- | Algorithm to generate 1x Bass Pedal versions automatically from the 2x version.
{-# LANGUAGE LambdaCase #-}
module Onyx.Drums.OneFoot (phaseShiftKicks, rockBand1x, rockBand2x) where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.Drums
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
phaseShiftKicks :: U.Seconds -> U.Seconds -> DrumTrack U.Seconds -> DrumTrack U.Seconds
phaseShiftKicks tx ty dt = let
  kicks = RTB.merge
    (RTB.mapMaybe (\case (Kick, _) -> Just RH; _ -> Nothing)
      $ drumGems $ fromMaybe mempty $ Map.lookup Expert $ drumDifficulties dt)
    (fmap (const LH) $ drumKick2x dt)
  in if LH `elem` kicks
    then dt
    else let
      auto = assignFeet tx ty kicks
      lf = RTB.mapMaybe (\case LH -> Just (); RH -> Nothing) auto
      rf = RTB.mapMaybe (\case LH -> Nothing; RH -> Just (Kick, VelocityNormal)) auto
      in dt
        { drumKick2x = lf
        , drumDifficulties = flip Map.mapWithKey (drumDifficulties dt) $ \diff dd ->
          case diff of
            Expert -> dd
              { drumGems = RTB.merge rf $ RTB.filter (\case (Kick, _) -> False; _ -> True) $ drumGems dd
              }
            _ -> dd
        }

rockBand1x :: DrumTrack t -> DrumTrack t
rockBand1x dt = dt { drumKick2x = RTB.empty }

rockBand2x :: (NNC.C t) => DrumTrack t -> DrumTrack t
rockBand2x dt = dt
  { drumKick2x = RTB.empty
  , drumDifficulties = flip Map.mapWithKey (drumDifficulties dt) $ \diff dd ->
    case diff of
      Expert -> dd
        { drumGems
          = RTB.flatten
          $ fmap nubOrd -- this is in case you have 95 + 96 kicks simultaneously
          $ RTB.collectCoincident
          $ RTB.merge (drumGems dd)
          $ fmap (const (Kick, VelocityNormal))
          $ drumKick2x dt
        }
      _ -> dd
  }
