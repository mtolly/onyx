{- |
EVENTS
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Harmonix.GH2.Events where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Venue            (anyLighting)

data CrowdTempo
  = CrowdHalfTempo
  | CrowdNormalTempo
  | CrowdFastTempo
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command CrowdTempo where
  fromCommand = \case
    CrowdHalfTempo   -> ["crowd_half_tempo"]
    CrowdNormalTempo -> ["crowd_normal_tempo"]
    CrowdFastTempo   -> ["crowd_fast_tempo"]
  toCommand = reverseLookup each fromCommand

data CrowdLighters
  = CrowdLightersOff
  | CrowdLightersSlow
  | CrowdLightersFast
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command CrowdLighters where
  fromCommand = \case
    CrowdLightersOff  -> ["crowd_lighters_off"]
    CrowdLightersSlow -> ["crowd_lighters_slow"]
    CrowdLightersFast -> ["crowd_lighters_fast"]
  toCommand = reverseLookup each fromCommand

data Event
  = SyncWag
  | SyncHeadBang
  | BandJump
  | Verse
  | Chorus
  | Solo
  | MusicStart
  | End
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command Event where
  fromCommand = \case
    Verse        -> ["verse"]
    Chorus       -> ["chorus"]
    Solo         -> ["solo"]
    SyncWag      -> ["sync_wag"]
    SyncHeadBang -> ["sync_head_bang"]
    BandJump     -> ["band_jump"] -- yes we can has [sync_band_jump]
    MusicStart   -> ["music_start"]
    End          -> ["end"]
  toCommand = reverseLookup each fromCommand

gh2LightingPresets :: [T.Text]
gh2LightingPresets =
  [ "" -- lighting ()
  , "blackout"
  , "chase"
  , "flare"
  , "strobe"
  , "sweep"
  , "color1"
  , "color2"
  -- laid to rest has [lighting (f)]
  ]

data EventsTrack t = EventsTrack
  { eventsCrowdTempo    :: RTB.T t CrowdTempo
  , eventsCrowdLighters :: RTB.T t CrowdLighters
  , eventsLighting      :: RTB.T t T.Text
  , eventsSections      :: RTB.T t T.Text
  , eventsOther         :: RTB.T t Event
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack EventsTrack where
  parseTrack = do
    eventsCrowdTempo    <- (.eventsCrowdTempo)    =. command
    eventsCrowdLighters <- (.eventsCrowdLighters) =. command
    eventsLighting      <- (.eventsLighting)      =. anyLighting
    eventsOther         <- (.eventsOther)         =. command
    eventsSections      <- (.eventsSections)      =. let
      fp = \case
        ("section" : s) -> Just $ T.unwords s
        _               -> Nothing
      fs t = ["section", t]
      in commandMatch' fp fs
    return EventsTrack{..}
