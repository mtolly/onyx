{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Beat where

import           Control.Monad.Codec
import           Control.Monad.Trans.StackTrace
import qualified Data.EventList.Relative.TimeBody as RTB
import           RockBand.Beat                    (Event (..))
import           RockBand.Codec
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

newtype BeatTrack t = BeatTrack { beatLines :: RTB.T t Event }
  deriving (Eq, Ord, Show)

parseBeat :: (SendMessage m) => TrackCodec m U.Beats (BeatTrack U.Beats)
parseBeat = do
  beatLines <- (beatLines =.) $ condenseMap_ $ eachKey each $ blip . \case
    Bar  -> 12
    Beat -> 13
  return BeatTrack{..}
