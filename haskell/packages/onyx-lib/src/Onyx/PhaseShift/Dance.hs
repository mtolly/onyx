{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.PhaseShift.Dance where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           Data.Profunctor                  (dimap)
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common                 (Edge (..), each)
import           Onyx.MIDI.Read

data SMDifficulty
  = SMBeginner
  | SMEasy
  | SMMedium
  | SMHard
  | SMChallenge
  deriving (Eq, Ord, Show, Enum, Bounded)

data DanceTrack t = DanceTrack
  { danceDifficulties :: Map.Map SMDifficulty (DanceDifficulty t)
  , danceOverdrive    :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DanceTrack t)

instance TraverseTrack DanceTrack where
  traverseTrack fn (DanceTrack a b) = DanceTrack
    <$> traverse (traverseTrack fn) a <*> fn b

nullDance :: DanceTrack t -> Bool
nullDance = all (RTB.null . danceNotes) . toList . danceDifficulties

data Arrow = ArrowL | ArrowD | ArrowU | ArrowR
  deriving (Eq, Ord, Show, Enum, Bounded)

data NoteType
  = NoteNormal
  | NoteMine
  | NoteLift
  | NoteRoll
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ChannelType NoteType where
  encodeChannel = fromEnum

data DanceDifficulty t = DanceDifficulty
  { danceNotes :: RTB.T t (Edge () (Arrow, NoteType))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (DanceDifficulty t)

instance TraverseTrack DanceDifficulty where
  traverseTrack fn (DanceDifficulty a) = DanceDifficulty <$> fn a

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
          f1 = \case
            EdgeOn () (arrow, nt) -> (arrow, (nt, Just 100))
            EdgeOff (arrow, nt)   -> (arrow, (nt, Nothing))
          f2 = \case
            (arrow, (nt, Just _))  -> EdgeOn () (arrow, nt)
            (arrow, (nt, Nothing)) -> EdgeOff (arrow, nt)
      danceNotes <- (danceNotes =.) $ dimap (fmap f1) (fmap f2) $ condenseMap $ eachKey each $ channelEdges . \case
        ArrowL -> base + 0
        ArrowD -> base + 1
        ArrowU -> base + 2
        ArrowR -> base + 3
      return DanceDifficulty{..}
    return DanceTrack{..}
