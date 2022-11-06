{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.Harmonix.GH2.PartDrum where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Drums

data GH2DrumTrack t = GH2DrumTrack
  { gh2drumDifficulties :: Map.Map Difficulty (GH2DrumDifficulty t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GH2DrumTrack t)

instance TraverseTrack GH2DrumTrack where
  traverseTrack fn (GH2DrumTrack a) = GH2DrumTrack
    <$> traverse (traverseTrack fn) a

data GH2DrumDifficulty t = GH2DrumDifficulty
  { gh2drumStarPower :: RTB.T t Bool
  , gh2drumPlayer1   :: RTB.T t Bool
  , gh2drumPlayer2   :: RTB.T t Bool
  , gh2drumGems      :: RTB.T t (Gem ())
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GH2DrumDifficulty t)

instance TraverseTrack GH2DrumDifficulty where
  traverseTrack fn (GH2DrumDifficulty a b c d) = GH2DrumDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d

instance ParseTrack GH2DrumTrack where
  parseTrack = do
    gh2drumDifficulties <- (gh2drumDifficulties =.) $ eachKey each $ \diff -> do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      gh2drumStarPower <- gh2drumStarPower =. edges (base + 7)
      gh2drumPlayer1   <- gh2drumPlayer1   =. edges (base + 9)
      gh2drumPlayer2   <- gh2drumPlayer2   =. edges (base + 10)
      gh2drumGems      <- (gh2drumGems =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey [Kick, Red, Pro Yellow (), Pro Blue (), Pro Green (), Orange] $ blip . \case
        Kick          -> base + 0
        Red           -> base + 1
        Pro Yellow () -> base + 2
        Pro Blue ()   -> base + 3
        Pro Green ()  -> base + 4
        Orange        -> base + 4
      return GH2DrumDifficulty{..}
    return GH2DrumTrack{..}
