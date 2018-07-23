{-# LANGUAGE RecordWildCards #-}
module Amplitude.Track where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common

newtype CatchTrack t = CatchTrack
  { catchDifficulties :: Map.Map Difficulty (CatchDifficulty t)
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (CatchTrack t) where
  CatchTrack a <> CatchTrack b = CatchTrack $ Map.unionWith (<>) a b

instance (NNC.C t) => Monoid (CatchTrack t) where
  mempty = CatchTrack Map.empty

instance TraverseTrack CatchTrack where
  traverseTrack fn (CatchTrack a) = CatchTrack <$> traverse (traverseTrack fn) a

newtype CatchDifficulty t = CatchDifficulty
  { catchGems :: RTB.T t Gem
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (CatchDifficulty t) where
  CatchDifficulty a <> CatchDifficulty b = CatchDifficulty $ RTB.merge a b

instance (NNC.C t) => Monoid (CatchDifficulty t) where
  mempty = CatchDifficulty RTB.empty

instance TraverseTrack CatchDifficulty where
  traverseTrack fn (CatchDifficulty a) = CatchDifficulty <$> fn a

data Gem = L | M | R
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ParseTrack CatchTrack where
  parseTrack = do
    catchDifficulties <- (catchDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 96  -- beginner
            Medium -> 102 -- intermediate
            Hard   -> 108 -- advanced
            Expert -> 114 -- expert/super
      catchGems <- (catchGems =.) $ condenseMap_ $ eachKey each $ \gem -> do
        blip $ base + case gem of
          L -> 0
          M -> 2
          R -> 4
      return CatchDifficulty{..}
    return CatchTrack{..}
