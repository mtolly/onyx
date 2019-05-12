{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE RecordWildCards    #-}
module Amplitude.Track where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import           RockBand.Common

newtype CatchTrack t = CatchTrack
  { catchDifficulties :: Map.Map Difficulty (CatchDifficulty t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (CatchTrack t)

instance TraverseTrack CatchTrack where
  traverseTrack fn (CatchTrack a) = CatchTrack <$> traverse (traverseTrack fn) a

newtype CatchDifficulty t = CatchDifficulty
  { catchGems :: RTB.T t Gem
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (CatchDifficulty t)

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
