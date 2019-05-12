{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.Beat where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           MergeMonoid
import           RockBand.Codec
import           RockBand.Common

data BeatEvent = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

newtype BeatTrack t = BeatTrack { beatLines :: RTB.T t BeatEvent }
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BeatTrack t)

instance ParseTrack BeatTrack where
  parseTrack = do
    beatLines <- (beatLines =.) $ statusBlips $ condenseMap_ $ eachKey each $ blip . \case
      Bar  -> 12
      Beat -> 13
    return BeatTrack{..}

instance TraverseTrack BeatTrack where
  traverseTrack fn (BeatTrack a) = BeatTrack <$> fn a
