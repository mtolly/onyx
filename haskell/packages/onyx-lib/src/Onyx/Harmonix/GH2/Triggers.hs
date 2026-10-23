{- |
TRIGGERS
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Harmonix.GH2.Triggers where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common                 (each)
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Events           (Backing (..))
import           Onyx.MIDI.Track.Venue            (LightingCommand (..))

data TriggersTrack t = TriggersTrack
  { backing          :: RTB.T t Backing
  , lightingCommands :: RTB.T t LightingCommand
  , unknown52        :: RTB.T t Bool -- "venue effect"
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TriggersTrack t)

instance TraverseTrack TriggersTrack where
  traverseTrack fn (TriggersTrack a b c) = TriggersTrack
    <$> fn a <*> fn b <*> fn c

instance ParseTrack TriggersTrack where
  parseTrack = do
    backing <- (.backing) =. do
      fatBlips (1/8) $ condenseMap_ $ eachKey each $ blip . \case
        BackingKick  -> 24
        BackingSnare -> 25
        BackingHihat -> 26
    lightingCommands <- (.lightingCommands) =. do
      condenseMap_ $ fatBlips (1/8) $ eachKey each \case
        LightingFirst -> blip 50
        LightingPrev  -> blip 49
        LightingNext  -> blip 48
    unknown52 <- (.unknown52) =. edges 52
    -- Trogdor has pitches 36 and 38
    -- Tonight I'm Gonna Rock You Tonight has pitch 60
    return TriggersTrack{..}
