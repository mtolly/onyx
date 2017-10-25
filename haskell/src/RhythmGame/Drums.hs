{-# LANGUAGE LambdaCase #-}
module RhythmGame.Drums where

import qualified RockBand.Drums as D
import qualified Data.Map.Strict as Map
import Data.Map.Strict.Internal (Map(..))
import Data.List (partition)
import Data.Maybe (fromMaybe)

data Note a
  = Upcoming a
  | Hit a
  | Missed a
  deriving (Eq, Ord, Show, Read)

data Track t a = Track
  { trackNotes    :: Map t [Note a] -- ^ lists must be non-empty
  , trackOverhits :: Map t [a] -- ^ lists must be non-empty
  , trackTime     :: t -- ^ the latest timestamp we have reached
  , trackWindow   :: t -- ^ the half-window of time on each side of a note
  } deriving (Eq, Ord, Show, Read)

-- | Efficiently updates the range @(k1, k2)@ of values.
updateRange :: (Ord k) => (k -> a -> a) -> k -> k -> Map k a -> Map k a
updateRange f k1 k2 = let
  go = \case
    Tip -> Tip
    Bin size k v mL mR -> if k <= k1
      then Bin size k v mL (go mR)
      else if k2 <= k
        then Bin size k v (go mL) mR
        else let
          v' = f k v
          in v' `seq` Bin size k v' (go mL) (go mR)
  in go

traverseRange_ :: (Applicative f, Ord k) => (k -> a -> f ()) -> Bool -> k -> k -> Map k a -> f ()
traverseRange_ f asc k1 k2 = let
  go = \case
    Tip -> pure ()
    Bin _ k v mL mR -> if k <= k1
      then go mR
      else if k2 <= k
        then go mL
        else if asc
          then go mL *> f k v *> go mR
          else go mR *> f k v *> go mL
  in go

updateTime :: (Num t, Ord t) => t -> Track t a -> Track t a
updateTime t trk = let
  f _ = map $ \case
    Upcoming x -> Missed x
    evt        -> evt
  in trk { trackNotes = updateRange f (t - trackWindow trk) (t + trackWindow trk) $ trackNotes trk }

hitPad :: (Num t, Ord t, Eq a) => t -> a -> Track t a -> Track t a
hitPad t x trk = let
  p1 = Map.lookupLE t $ trackNotes trk
  p2 = Map.lookupGE t $ trackNotes trk
  closestNotes = case p1 of
    Nothing -> case p2 of
      Nothing -> Nothing
      Just (k2, _) -> if k2 - t < trackWindow trk then p2 else Nothing
    Just (k1, _) -> case p2 of
      Nothing -> if t - k1 < trackWindow trk then p1 else Nothing
      Just (k2, _) -> let
        distance1 = t - k1
        distance2 = k2 - t
        in if distance1 < distance2
          then if distance1 < trackWindow trk then p1 else Nothing
          else if distance2 < trackWindow trk then p2 else Nothing
  overhit = trk { trackOverhits = Map.alter newOverhit t $ trackOverhits trk }
  newOverhit = Just . (x :) . fromMaybe []
  in case closestNotes of
    Nothing -> overhit
    Just (k, notes) -> case partition (== Upcoming x) notes of
      ([], _) -> overhit
      (_ : _, notes') ->
        trk { trackNotes = Map.insert k (Hit x : notes') $ trackNotes trk }
