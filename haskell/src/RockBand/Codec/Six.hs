{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.Six where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import qualified PhaseShift.Message               as PS
import           RockBand.Codec
import           RockBand.Common

data Fret
  = Black1
  | Black2
  | Black3
  | White1
  | White2
  | White3
  deriving (Eq, Ord, Show, Enum, Bounded)

data SixTrack t = SixTrack
  { sixDifficulties :: Map.Map Difficulty (SixDifficulty t)
  , sixOverdrive    :: RTB.T t Bool
  , sixSolo         :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SixTrack t)

nullSix :: SixTrack t -> Bool
nullSix = all (RTB.null . sixGems) . toList . sixDifficulties

instance TraverseTrack SixTrack where
  traverseTrack fn (SixTrack a b c) = SixTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c

data SixDifficulty t = SixDifficulty
  { sixForceStrum :: RTB.T t Bool
  , sixForceHOPO  :: RTB.T t Bool
  , sixTap        :: RTB.T t Bool
  , sixGems       :: RTB.T t (Edge () (Maybe Fret))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SixDifficulty t)

instance TraverseTrack SixDifficulty where
  traverseTrack fn (SixDifficulty a b c d) = SixDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d

instance ParseTrack SixTrack where
  parseTrack = do
    sixOverdrive    <- sixOverdrive    =. edges 116
    sixSolo         <- sixSolo         =. edges 103
    sixDifficulties <- (sixDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      sixForceStrum <- sixForceStrum =. edges (base + 6)
      sixForceHOPO  <- sixForceHOPO  =. edges (base + 5)
      sixTap        <- sixTap        =. sysexPS diff PS.TapNotes
      sixGems       <- (sixGems =.) $ translateEdges $ condenseMap
        $ eachKey (Nothing : map Just each) $ edges . \case
          Nothing     -> base - 2
          Just White1 -> base - 1
          Just White2 -> base
          Just White3 -> base + 1
          Just Black1 -> base + 2
          Just Black2 -> base + 3
          Just Black3 -> base + 4
      return SixDifficulty{..}
    return SixTrack{..}
