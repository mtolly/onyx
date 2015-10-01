{-# LANGUAGE LambdaCase #-}
module Background where

import qualified RockBand.Drums as Drums
import RockBand.Common
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U

splitDrumAnimations
  :: RTB.T U.Beats Drums.Event
  -> (RTB.T U.Beats (Either Drums.Animation Mood), RTB.T U.Beats Drums.Event)
splitDrumAnimations = RTB.partitionMaybe $ \case
  Drums.Animation x -> Just $ Left  x
  Drums.Mood      x -> Just $ Right x
  _                 -> Nothing

animateDrums :: RTB.T U.Beats (Drums.Gem ()) -> RTB.T U.Beats (Either Drums.Animation Mood)
animateDrums = const $ RTB.empty
