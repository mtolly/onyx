{-# LANGUAGE DeriveFunctor #-}
module ProKeysRanges (completeRanges) where

import RockBand.ProKeys
import RockBand.Common
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import qualified Data.Set as Set
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import qualified Sound.MIDI.Util as U

-- | Adds ranges if there are none.
completeRanges :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
completeRanges rtb = let
  (ranges, notRanges) = flip RTB.partitionMaybe rtb $ \case
    LaneShift r -> Just r
    _           -> Nothing
  held = heldNotes $ flip RTB.mapMaybe notRanges $ \case
    Note b p -> Just (b, p)
    _        -> Nothing
  in if RTB.null ranges
    then RTB.merge rtb $ fmap LaneShift $ pullBackRanges held $ createRanges held
    else rtb

heldNotes :: (NNC.C t) => RTB.T t (Bool, Pitch) -> RTB.T t (Set.Set Pitch)
heldNotes = go Set.empty . RTB.collectCoincident where
  go held rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, ends), rtb') -> let
      ons  = Set.fromList [p | (True , p) <- ends]
      offs = Set.fromList [p | (False, p) <- ends]
      held' = Set.difference held offs `Set.union` ons
      in RTB.cons dt held' $ go held' rtb'

-- | Chooses valid ranges to accommodate the given notes.
createRanges :: (NNC.C t) => RTB.T t (Set.Set Pitch) -> RTB.T t LaneRange
createRanges = go Nothing where
  go currentRange rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, held), rtb') -> if maybe False (\r -> all (keyInRange r) held) currentRange
      then RTB.delay dt $ go currentRange rtb'
      else case bestRange currentRange held rtb' of
        Nothing -> error $ "Couldn't make a Pro Keys range, because all of these notes are held simultaneously: " ++ show held
        Just newRange -> RTB.cons dt newRange $ go (Just newRange) rtb'

pullBackRanges :: (NNC.C t) => RTB.T t (Set.Set Pitch) -> RTB.T t LaneRange -> RTB.T t LaneRange
pullBackRanges rtb rngs = case RTB.viewL rngs of
  Nothing -> RTB.empty
  Just ((trng, rng), rngs') -> case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((theld, held), rtb') -> case NNC.split theld trng of
      (_, (theldLEQtrng, tdiff)) -> if theldLEQtrng
        then if all (all $ keyInRange rng) $ U.trackTake trng rtb
          then RTB.cons theld rng $ pullBackRanges rtb' $ RTB.delay tdiff rngs'
          else RTB.delay theld $ pullBackRanges rtb' $ RTB.cons tdiff rng rngs'
        else RTB.cons trng rng $ pullBackRanges (RTB.cons tdiff held rtb') rngs'

data Lifetime t
  = Mortal t
  | Immortal
  deriving (Eq, Ord, Show, Read, Functor)

bestRange :: (NNC.C t) => Maybe LaneRange -> Set.Set Pitch -> RTB.T t (Set.Set Pitch) -> Maybe LaneRange
bestRange currentRange held rtb = let
  ranges = filter (\rng -> all (keyInRange rng) held) [minBound .. maxBound]
  isDifficult rng = rng `elem` [RangeD, RangeE]
  distance rng = case currentRange of
    Nothing -> 0
    Just cr -> abs $ fromEnum cr - fromEnum rng
  score rng = (rangeLifetime rng rtb, isDifficult rng, distance rng)
  -- First, we want to pick the longest-lasting range.
  -- Second, I personally think D and E ranges are harder to read.
  -- Third, we want to pick a range that is closest to the last one.
  in listToMaybe $ reverse $ sortOn score ranges

rangeLifetime :: (NNC.C t) => LaneRange -> RTB.T t (Set.Set Pitch) -> Lifetime t
rangeLifetime rng rtb = case RTB.viewL rtb of
  Nothing -> Immortal -- range lasts till end of song
  Just ((dt, held), rtb') -> if all (keyInRange rng) held
    then NNC.add dt <$> rangeLifetime rng rtb'
    else Mortal dt

keyInRange :: LaneRange -> Pitch -> Bool
keyInRange RangeC p = RedYellow C <= p && p <= BlueGreen E
keyInRange RangeD p = RedYellow D <= p && p <= BlueGreen F
keyInRange RangeE p = RedYellow E <= p && p <= BlueGreen G
keyInRange RangeF p = RedYellow F <= p && p <= BlueGreen A
keyInRange RangeG p = RedYellow G <= p && p <= BlueGreen B
keyInRange RangeA p = RedYellow A <= p && p <= OrangeC
