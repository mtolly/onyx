{- |
EVENTS
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module GuitarHeroII.Events where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import           RockBand.Common

data CrowdTempo
  = CrowdHalfTempo
  | CrowdNormalTempo
  | CrowdFastTempo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

data Lighting
  = Lighting_
  | Lighting_blackout
  | Lighting_chase
  | Lighting_flare
  | Lighting_strobe
  | Lighting_sweep
  | Lighting_color1
  | Lighting_color2
  -- TODO laid to rest has [lighting (f)]
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- TODO handle [lighting(foo)] with no space, like we do from RB
instance Command Lighting where
  fromCommand x = case T.stripPrefix "Lighting_" $ T.pack $ show x of
    Just s  -> ["lighting", "(" <> s <> ")"]
    Nothing -> error "panic! couldn't strip Lighting_ from venue event"
  toCommand = reverseLookup each fromCommand

data EventsTrack t = EventsTrack
  { eventsCrowdTempo    :: RTB.T t CrowdTempo
  , eventsCrowdLighters :: RTB.T t CrowdLighters
  , eventsLighting      :: RTB.T t Lighting
  , eventsSections      :: RTB.T t T.Text
  , eventsOther         :: RTB.T t Event
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack EventsTrack where
  parseTrack = do
    eventsCrowdTempo    <- eventsCrowdTempo    =. command
    eventsCrowdLighters <- eventsCrowdLighters =. command
    eventsLighting      <- eventsLighting      =. command
    eventsOther         <- eventsOther         =. command
    eventsSections      <- eventsSections      =. let
      fp = readCommand' >=> \case
        ("section" : s) -> Just $ T.unwords s
        _               -> Nothing
      fs t = showCommand' ["section", t]
      in single fp fs
    return EventsTrack{..}
