{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.Beat where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           RockBand.Beat                    (Event (..))
import           RockBand.Codec
import           RockBand.Common

newtype BeatTrack t = BeatTrack { beatLines :: RTB.T t Event }
  deriving (Eq, Ord, Show)

instance ParseTrack BeatTrack where
  parseTrack = do
    beatLines <- (beatLines =.) $ condenseMap_ $ eachKey each $ blip . \case
      Bar  -> 12
      Beat -> 13
    return BeatTrack{..}
