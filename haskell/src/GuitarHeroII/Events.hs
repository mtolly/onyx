{- |
EVENTS
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.Events where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
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
  } deriving (Eq, Ord, Show)

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance (NNC.C t) => Monoid (EventsTrack t) where
  mempty = EventsTrack RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (EventsTrack a1 a2 a3 a4 a5)
    (EventsTrack b1 b2 b3 b4 b5)
    = EventsTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)

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
