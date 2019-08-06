{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module PhaseShift.Dance where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Profunctor                  (dimap)
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import           RockBand.Common                  (each)

data SMDifficulty
  = SMBeginner
  | SMEasy
  | SMMedium
  | SMHard
  | SMChallenge
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DanceTrack t = DanceTrack
  { danceDifficulties :: Map.Map SMDifficulty (DanceDifficulty t)
  , danceOverdrive    :: RTB.T t Bool -- TODO does OD work in game?
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DanceTrack t)

instance TraverseTrack DanceTrack where
  traverseTrack fn (DanceTrack a b) = DanceTrack
    <$> traverse (traverseTrack fn) a <*> fn b

nullDance :: DanceTrack t -> Bool
nullDance = all (RTB.null . danceNotes) . toList . danceDifficulties

data Arrow = ArrowL | ArrowD | ArrowU | ArrowR
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data NoteType
  = NoteNormal
  | NoteMine
  | NoteLift
  | NoteRoll
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ChannelType NoteType where
  encodeChannel = fromEnum

data DanceDifficulty t = DanceDifficulty
  { danceNotes :: RTB.T t ((Arrow, NoteType), Maybe t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DanceDifficulty t)

instance TraverseTrack DanceDifficulty where
  traverseTrack fn (DanceDifficulty a) = DanceDifficulty
    <$> traverseBlipSustain fn a

instance ParseTrack DanceTrack where
  parseTrack = do
    danceOverdrive <- danceOverdrive =. edges 116
    danceDifficulties <- (danceDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            SMBeginner  -> 48
            SMEasy      -> 60
            SMMedium    -> 72
            SMHard      -> 84
            SMChallenge -> 96
          f1 = fmap $ \((arrow, ntype), len) -> (arrow, (ntype, 100, len))
          f2 = fmap $ \(arrow, (ntype, _, len)) -> ((arrow, ntype), len)
      danceNotes <- (danceNotes =.) $ blipSustainRB $ dimap f1 f2 $ condenseMap $ eachKey each $ matchEdgesCV . channelEdges . \case
        ArrowL -> base + 0
        ArrowD -> base + 1
        ArrowU -> base + 2
        ArrowR -> base + 3
      return DanceDifficulty{..}
    return DanceTrack{..}
