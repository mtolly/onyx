{-
Clone Hero designed MIDI track format for 6-fret (Guitar Hero Live) charts.
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.MIDI.Track.SixFret where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Onyx.PhaseShift.Message          as PS

data Fret
  = Black1
  | Black2
  | Black3
  | White1
  | White2
  | White3
  deriving (Eq, Ord, Show, Enum, Bounded)

data SixTrack t = SixTrack
  { difficulties :: Map.Map Difficulty (SixDifficulty t)
  , overdrive    :: RTB.T t Bool
  , solo         :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SixTrack t)

nullSix :: SixTrack t -> Bool
nullSix = all (RTB.null . (.gems)) . toList . (.difficulties)

instance TraverseTrack SixTrack where
  traverseTrack fn (SixTrack a b c) = SixTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c

data SixDifficulty t = SixDifficulty
  { forceStrum :: RTB.T t Bool
  , forceHOPO  :: RTB.T t Bool
  , tap        :: RTB.T t Bool
  , gems       :: RTB.T t (Edge () (Maybe Fret))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SixDifficulty t)

instance TraverseTrack SixDifficulty where
  traverseTrack fn (SixDifficulty a b c d) = SixDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d

instance ParseTrack SixTrack where
  parseTrack = do
    overdrive    <- (.overdrive   ) =. edges 116
    solo         <- (.solo        ) =. edges 103
    difficulties <- (.difficulties) =. do
      eachKey each \diff -> fatBlips (1/8) do
        let base = case diff of
              Easy   -> 60
              Medium -> 72
              Hard   -> 84
              Expert -> 96
        forceStrum <- (.forceStrum) =. edges (base + 6)
        forceHOPO  <- (.forceHOPO ) =. edges (base + 5)
        -- TODO we probably need to support pitch 104 like on 5-fret
        tap        <- (.tap       ) =. sysexPS diff PS.TapNotes
        chordSnap [base - 2 .. base + 4]
        gems       <- (.gems      ) =. do
          translateEdges $ condenseMap $ eachKey (Nothing : map Just each) $ edges . \case
            Nothing     -> base - 2
            Just White1 -> base - 1
            Just White2 -> base
            Just White3 -> base + 1
            Just Black1 -> base + 2
            Just Black2 -> base + 3
            Just Black3 -> base + 4
        return SixDifficulty{..}
    return SixTrack{..}
