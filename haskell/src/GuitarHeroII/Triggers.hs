{- |
TRIGGERS
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.Triggers where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Events            (Backing (..))
import           RockBand.Common                  (each)

data TriggersTrack t = TriggersTrack
  { triggersBacking   :: RTB.T t Backing
  , triggersUnknown48 :: RTB.T t ()
  , triggersUnknown49 :: RTB.T t ()
  , triggersUnknown50 :: RTB.T t ()
  , triggersUnknown52 :: RTB.T t Bool
  } deriving (Eq, Ord, Show)

instance TraverseTrack TriggersTrack where
  traverseTrack fn (TriggersTrack a b c d e) = TriggersTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance (NNC.C t) => Monoid (TriggersTrack t) where
  mempty = TriggersTrack
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (TriggersTrack a1 a2 a3 a4 a5)
    (TriggersTrack b1 b2 b3 b4 b5)
    = TriggersTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)

instance ParseTrack TriggersTrack where
  parseTrack = do
    triggersBacking <- (triggersBacking =.) $ condenseMap_ $ eachKey each $ blip . \case
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
