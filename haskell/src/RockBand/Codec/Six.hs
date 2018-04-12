{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Six where

import           Control.Monad.Codec
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           RockBand.Codec
import           RockBand.Common
import           RockBand.GHL                     (Fret (..))
import qualified RockBand.PhaseShiftMessage       as PS

data SixTrack t = SixTrack
  { sixOverdrive    :: RTB.T t Bool
  , sixSolo         :: RTB.T t Bool
  , sixDifficulties :: Map.Map Difficulty (SixDifficulty t)
  } deriving (Eq, Ord, Show)

data SixDifficulty t = SixDifficulty
  { sixForceStrum :: RTB.T t Bool
  , sixForceHOPO  :: RTB.T t Bool
  , sixTap        :: RTB.T t Bool
  , sixGems       :: RTB.T t (Maybe Fret, Maybe t)
  } deriving (Eq, Ord, Show)

instance Default (SixDifficulty t) where
  def = SixDifficulty RTB.empty RTB.empty RTB.empty RTB.empty

instance ParseTrack SixTrack where
  parseTrack = do
    sixOverdrive    <- sixOverdrive    =. edges 116
    sixSolo         <- sixSolo         =. edges 103
    sixDifficulties <- (sixDifficulties =.) $ eachKey each $ \diff -> do
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
