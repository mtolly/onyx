-- | The \"Clone Hero Live\" MIDI format.
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module RockBand.GHL
( Fret(..)
, Event(..)
, DiffEvent(..)
, sixFromLegacy, sixToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Six
import           RockBand.Common
import           RockBand.FiveButton              (StrumHOPOTap (..))

data Event
  = Overdrive                 Bool
  | Solo                      Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Force StrumHOPOTap Bool
  | Note (LongNote () (Maybe Fret))
  deriving (Eq, Ord, Show, Read)

sixFromLegacy :: (NNC.C t) => RTB.T t Event -> SixTrack t
sixFromLegacy = undefined

sixToLegacy :: (NNC.C t) => SixTrack t -> RTB.T t Event
sixToLegacy = undefined

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing
