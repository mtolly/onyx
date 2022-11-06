{- |
TRIGGERS
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.Harmonix.GH2.Triggers where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common                 (each)
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Events           (Backing (..))

data TriggersTrack t = TriggersTrack
  { triggersBacking   :: RTB.T t Backing
  , triggersUnknown48 :: RTB.T t () -- this is next lighting keyframe like RBN1 venue
  , triggersUnknown49 :: RTB.T t () -- this is prev lighting keyframe like RBN1 venue
  , triggersUnknown50 :: RTB.T t () -- this is first lighting keyframe like RBN1 venue
  , triggersUnknown52 :: RTB.T t Bool -- "venue effect"
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TriggersTrack t)

instance TraverseTrack TriggersTrack where
  traverseTrack fn (TriggersTrack a b c d e) = TriggersTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack TriggersTrack where
  parseTrack = do
    triggersBacking <- (triggersBacking =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey each $ blip . \case
      BackingKick  -> 24
      BackingSnare -> 25
      BackingHihat -> 26
    triggersUnknown48 <- triggersUnknown48 =. blip 48
    triggersUnknown49 <- triggersUnknown49 =. blip 49
    triggersUnknown50 <- triggersUnknown50 =. blip 50
    triggersUnknown52 <- triggersUnknown52 =. edges 52
    -- Trogdor has pitches 36 and 38
    -- Tonight I'm Gonna Rock You Tonight has pitch 60
    return TriggersTrack{..}
