{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Five where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           RockBand.Codec
import           RockBand.Common
import           RockBand.FiveButton              (Color (..),
                                                   FretPosition (..),
                                                   HandMap (..),
                                                   OnyxCloseEvent (..),
                                                   StrumMap (..))
import qualified RockBand.PhaseShiftMessage       as PS
import qualified Sound.MIDI.Util                  as U

data FiveTrack t = FiveTrack
  { fiveMood         :: RTB.T t Mood
  , fiveHandMap      :: RTB.T t HandMap
  , fiveStrumMap     :: RTB.T t StrumMap
  , fiveFretPosition :: RTB.T t (FretPosition, Bool)
  , fiveTremolo      :: RTB.T t Bool
  , fiveTrill        :: RTB.T t Bool
  , fiveOverdrive    :: RTB.T t Bool
  , fiveBRE          :: RTB.T t Bool
  , fiveSolo         :: RTB.T t Bool
  , fivePlayer1      :: RTB.T t Bool
  , fivePlayer2      :: RTB.T t Bool
  , fiveDifficulties :: Map.Map Difficulty (FiveDifficulty t)
  } deriving (Eq, Ord, Show)

data FiveDifficulty t = FiveDifficulty
  { fiveForceStrum :: RTB.T t Bool
  , fiveForceHOPO  :: RTB.T t Bool
  , fiveTap        :: RTB.T t Bool
  , fiveOpen       :: RTB.T t Bool
  , fiveOnyxClose  :: RTB.T t Int
  , fiveGems       :: RTB.T t (Color, Maybe t)
  } deriving (Eq, Ord, Show)

instance Default (FiveDifficulty t) where
  def = FiveDifficulty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty

parseFive :: (SendMessage m) => TrackCodec m U.Beats (FiveTrack U.Beats)
parseFive = do
  fiveMood         <- fiveMood         =. command
  fiveHandMap      <- fiveHandMap      =. command
  fiveStrumMap     <- fiveStrumMap     =. command
  fiveFretPosition <- (fiveFretPosition =.) $ condenseMap $ eachKey each
    $ \posn -> edges $ fromEnum posn + 40
  fiveTremolo      <- fiveTremolo      =. edges 126
  fiveTrill        <- fiveTrill        =. edges 127
  fiveOverdrive    <- fiveOverdrive    =. edges 116
  fiveBRE          <- fiveBRE          =. edgesBRE [120 .. 124]
  fiveSolo         <- fiveSolo         =. edges 103
  fivePlayer1      <- fivePlayer1      =. edges 105
  fivePlayer2      <- fivePlayer2      =. edges 106
  fiveDifficulties <- (fiveDifficulties =.) $ eachKey each $ \diff -> do
    let base = case diff of
          Easy   -> 60
          Medium -> 72
          Hard   -> 84
          Expert -> 96
    fiveForceStrum <- fiveForceStrum =. edges (base + 6)
    fiveForceHOPO  <- fiveForceHOPO  =. edges (base + 5)
    fiveTap        <- fiveTap        =. sysexPS diff PS.TapNotes
    fiveOpen       <- fiveOpen       =. sysexPS diff PS.OpenStrum
    fiveOnyxClose  <- fiveOnyxClose  =. let
      parse = readCommand' >=> \(OnyxCloseEvent diff' n) -> guard (diff == diff') >> Just n
      unparse n = showCommand' $ OnyxCloseEvent diff n
      in single parse unparse
    fiveGems       <- (fiveGems =.) $ blipSustainRB $ condenseMap $ eachKey each $ matchEdges . edges . \case
      Green  -> base + 0
      Red    -> base + 1
      Yellow -> base + 2
      Blue   -> base + 3
      Orange -> base + 4
    return FiveDifficulty{..}
  return FiveTrack{..}
