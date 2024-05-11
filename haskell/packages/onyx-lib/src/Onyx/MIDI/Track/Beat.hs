{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.MIDI.Track.Beat where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Sound.MIDI.Util                  as U

data BeatEvent = Bar | Beat
  deriving (Eq, Ord, Show, Enum, Bounded)

newtype BeatTrack t = BeatTrack { beatLines :: RTB.T t BeatEvent }
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BeatTrack t)

instance ChopTrack BeatTrack where
  chopTake t = mapTrack $ U.trackTake t
  chopDrop t = mapTrack $ U.trackDrop t

instance ParseTrack BeatTrack where
  parseTrack = do
    beatLines <- (beatLines =.) $ statusBlips $ condenseMap_ $ eachKey each $ blip . \case
      Bar  -> 12
      Beat -> 13
    return BeatTrack{..}

instance TraverseTrack BeatTrack where
  traverseTrack fn (BeatTrack a) = BeatTrack <$> fn a

-- | Given a measure map, produces an infinite BEAT track.
makeBeatTrack :: U.MeasureMap -> RTB.T U.Beats BeatEvent
makeBeatTrack mmap = go 0 where
  go i = let
    len = U.unapplyMeasureMap mmap (i + 1, 0) - U.unapplyMeasureMap mmap (i, 0)
    -- the rounding below ensures that
    -- e.g. the sig must be at least 3.5 to get bar-beat-beat-beat.
    -- if it's 3.25, then you would get a beat 0.25 before the next bar,
    -- which Magma doesn't like...
    -- TODO we may want to tweak this so 3.5 results in bar-beat-beat.
    -- better animations and closer to harmonix songs
    thisMeasure = U.trackTake (fromInteger $ simpleRound len) infiniteMeasure
    -- simpleRound always rounds 0.5 up,
    -- unlike round which rounds to the nearest even number.
    simpleRound frac = case properFraction frac :: (Integer, U.Beats) of
      (_, 0.5) -> ceiling frac
      _        -> round frac
    in trackGlue len thisMeasure $ go $ i + 1
  infiniteMeasure, infiniteBeats :: RTB.T U.Beats BeatEvent
  infiniteMeasure = RTB.cons 0 Bar  $ RTB.delay 1 infiniteBeats
  infiniteBeats   = RTB.cons 0 Beat $ RTB.delay 1 infiniteBeats
