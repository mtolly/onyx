{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Drums where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Profunctor                  (dimap)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import           RockBand.Drums                   (Animation (..), Audio (..),
                                                   Disco (..), Gem (..),
                                                   PSGem (..), ProColor (..),
                                                   ProType (..))
import qualified Sound.MIDI.Util                  as U
import qualified RockBand.PhaseShiftMessage as PS

data DrumTrack t = DrumTrack
  { drumMood         :: RTB.T t Mood
  , drumToms         :: RTB.T t (ProColor, ProType)
  , drumSingleRoll   :: RTB.T t Bool
  , drumDoubleRoll   :: RTB.T t Bool
  , drumOverdrive    :: RTB.T t Bool  -- ^ white notes to gain energy
  , drumActivation   :: RTB.T t Bool -- ^ drum fill to activate Overdrive, or BRE
  , drumSolo         :: RTB.T t Bool
  , drumPlayer1      :: RTB.T t Bool
  , drumPlayer2      :: RTB.T t Bool
  , drumDifficulties :: Map.Map Difficulty (DrumDifficulty t)
  , drumKick2x       :: RTB.T t ()
  , drumAnimation    :: RTB.T t Animation
  }

data DrumDifficulty t = DrumDifficulty
  { drumMix         :: RTB.T t (Audio, Disco)
  , drumPSModifiers :: RTB.T t (PSGem, Bool)
  , drumGems        :: RTB.T t (Gem ())
  }

parseProType :: (Monad m, NNC.C t) => Int -> TrackEvent m t ProType
parseProType
  = dimap
    (fmap $ \case Tom -> True; Cymbal -> False)
    (fmap $ \b -> if b then Tom else Cymbal)
  . edges

parseDrums :: (SendMessage m) => TrackCodec m U.Beats (DrumTrack U.Beats)
parseDrums = do
  drumMood <- drumMood =. command
  drumToms <- (drumToms =.) $ condenseMap $ eachKey each $ parseProType . \case
    Yellow -> 110
    Blue   -> 111
    Green  -> 112
  drumSingleRoll <- drumSingleRoll =. edges 126
  drumDoubleRoll <- drumDoubleRoll =. edges 127
  drumOverdrive <- drumOverdrive =. edges 116
  drumActivation <- drumActivation =. edgesBRE [120 .. 124]
  drumSolo <- drumSolo =. edges 103
  drumPlayer1 <- drumPlayer1 =. edges 105
  drumPlayer2 <- drumPlayer2 =. edges 106
  drumDifficulties <- (drumDifficulties =.) $ eachKey each $ \diff -> do
    let base = case diff of
          Easy   -> 60
          Medium -> 72
          Hard   -> 84
          Expert -> 96
        allDrums = [Kick, Red, Pro Yellow (), Pro Blue (), Pro Green (), Orange]
    drumGems <- (drumGems =.) $ condenseMap_ $ eachKey allDrums $ \drum -> do
      blip $ base + case drum of
        Kick          -> 0
        Red           -> 1
        Pro Yellow () -> 2
        Pro Blue   () -> 3
        Pro Green  () -> 4
        Orange        -> 5
    drumMix <- drumMix =. let
      parse = readCommand' >=> \(diff', aud, dsc) -> guard (diff == diff') >> Just (aud, dsc)
      unparse (aud, dsc) = showCommand' (diff :: Difficulty, aud :: Audio, dsc :: Disco)
      in single parse unparse
    drumPSModifiers <- (drumPSModifiers =.) $ condenseMap $ eachKey each $ \psgem -> let
      pid = case psgem of
        Rimshot  -> PS.SnareRimshot
        HHOpen   -> PS.HihatOpen
        HHSizzle -> PS.HihatSizzle
        HHPedal  -> PS.HihatPedal
      in undefined pid
    return DrumDifficulty{..}
  drumKick2x <- drumKick2x =. blip 95 -- TODO this should probably be blip-grouped with expert track
  drumAnimation <- drumAnimation =. undefined
  return DrumTrack{..}
