{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Six where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import qualified RockBand.PhaseShiftMessage       as PS

data Fret
  = Black1
  | Black2
  | Black3
  | White1
  | White2
  | White3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SixTrack t = SixTrack
  { sixDifficulties :: Map.Map Difficulty (SixDifficulty t)
  , sixOverdrive    :: RTB.T t Bool
  , sixSolo         :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

nullSix :: SixTrack t -> Bool
nullSix = all (RTB.null . sixGems) . toList . sixDifficulties

instance (NNC.C t) => Semigroup (SixTrack t) where
  (<>)
    (SixTrack a1 a2 a3)
    (SixTrack b1 b2 b3)
    = SixTrack
      (Map.unionWith (<>) a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)

instance (NNC.C t) => Monoid (SixTrack t) where
  mempty = SixTrack Map.empty RTB.empty RTB.empty

instance TraverseTrack SixTrack where
  traverseTrack fn (SixTrack a b c) = SixTrack
    <$> traverse (traverseTrack fn) a <*> fn b <*> fn c

data SixDifficulty t = SixDifficulty
  { sixForceStrum :: RTB.T t Bool
  , sixForceHOPO  :: RTB.T t Bool
  , sixTap        :: RTB.T t Bool
  , sixGems       :: RTB.T t (Maybe Fret, Maybe t)
  } deriving (Eq, Ord, Show)

instance TraverseTrack SixDifficulty where
  traverseTrack fn (SixDifficulty a b c d) = SixDifficulty
    <$> fn a <*> fn b <*> fn c <*> traverseBlipSustain fn d

instance (NNC.C t) => Semigroup (SixDifficulty t) where
  (<>)
    (SixDifficulty a1 a2 a3 a4)
    (SixDifficulty b1 b2 b3 b4)
    = SixDifficulty
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)

instance (NNC.C t) => Monoid (SixDifficulty t) where
  mempty = SixDifficulty RTB.empty RTB.empty RTB.empty RTB.empty

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
      sixGems       <- (sixGems =.) $ blipSustainRB $ condenseMap
        $ eachKey (Nothing : map Just each) $ matchEdges . edges . \case
          Nothing     -> base - 2
          Just White1 -> base - 1
          Just White2 -> base
          Just White3 -> base + 1
          Just Black1 -> base + 2
          Just Black2 -> base + 3
          Just Black3 -> base + 4
      return SixDifficulty{..}
    return SixTrack{..}
