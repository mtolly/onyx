{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.Venue where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           RockBand.Codec
import           RockBand.Common
import qualified RockBand.Venue                   as V3
import qualified RockBand.VenueRB2                as V2

data RB2CutEvent
  = CameraCut
  | FocusVocal
  | FocusGuitar
  | FocusBass
  | FocusDrums
  | NoClose
  | OnlyClose
  | OnlyFar
  | NoBehind
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- commands that are always in [lighting (foo)] form
data LightingShared
  -- manual
  = Lighting_ -- empty parens
  | Lighting_intro -- new in rb3
  | Lighting_manual_cool
  | Lighting_manual_warm
  | Lighting_dischord
  | Lighting_stomp
  -- automatic
  | Lighting_loop_cool
  | Lighting_loop_warm
  | Lighting_harmony
  | Lighting_frenzy
  | Lighting_silhouettes
  | Lighting_silhouettes_spot
  | Lighting_searchlights
  | Lighting_sweep
  | Lighting_strobe_slow
  | Lighting_strobe_fast
  | Lighting_blackout_slow
  | Lighting_blackout_fast
  | Lighting_blackout_spot -- new in rb3
  | Lighting_flare_slow
  | Lighting_flare_fast
  | Lighting_bre
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command LightingShared where
  fromCommand x = case T.stripPrefix "Lighting_" $ T.pack $ show x of
    Just s  -> ["lighting", "(" <> s <> ")"]
    Nothing -> error "panic! couldn't strip Lighting_ from venue event"
  toCommand = reverseLookup each fromCommand

-- different formats in rb2 vs rb3
data LightingSplit
  = Lighting_verse
  | Lighting_chorus
  -- controls
  | Lighting_first
  | Lighting_prev
  | Lighting_next
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data VenueTrack t = VenueTrack

  { venueCameraRB3       :: RTB.T t V3.Camera
  , venueCameraRB2       :: RTB.T t RB2CutEvent
  , venueDirectedRB2     :: RTB.T t V2.DoCut

  , venueSingGuitar      :: RTB.T t Bool -- or keys if no guitar
  , venueSingDrums       :: RTB.T t Bool
  , venueSingBass        :: RTB.T t Bool -- or keys if no bass

  , venueSpotKeys        :: RTB.T t Bool -- rb3 only of course
  , venueSpotVocal       :: RTB.T t Bool
  , venueSpotGuitar      :: RTB.T t Bool
  , venueSpotDrums       :: RTB.T t Bool
  , venueSpotBass        :: RTB.T t Bool

  , venuePostProcessRB3  :: RTB.T t V3.PostProcess
  , venuePostProcessRB2  :: RTB.T t V2.PostProcess

  , venueLightingShared  :: RTB.T t LightingShared
  , venueLightingRB3     :: RTB.T t LightingSplit
  , venueLightingRB2     :: RTB.T t LightingSplit

  , venueBonusFX         :: RTB.T t ()
  , venueBonusFXOptional :: RTB.T t ()

  , venueFog             :: RTB.T t Bool

  } deriving (Eq, Ord, Show)

instance ParseTrack VenueTrack where
  parseTrack = do
    venueCameraRB3 <- venueCameraRB3 =. command
    venueCameraRB2 <- (venueCameraRB2 =.) $ condenseMap_ $ eachKey each $ blip . \case
      CameraCut   -> 60
      FocusVocal  -> 64
      FocusGuitar -> 63
      FocusDrums  -> 62
      FocusBass   -> 61
      NoClose     -> 73
      OnlyClose   -> 72
      OnlyFar     -> 71
      NoBehind    -> 70
    venueDirectedRB2     <- venueDirectedRB2     =. command
    venueSingGuitar      <- venueSingGuitar      =. edges 87
    venueSingDrums       <- venueSingDrums       =. edges 86
    venueSingBass        <- venueSingBass        =. edges 85
    venueSpotKeys        <- venueSpotKeys        =. edges 41
    venueSpotVocal       <- venueSpotVocal       =. edges 40
    venueSpotGuitar      <- venueSpotGuitar      =. edges 39
    venueSpotDrums       <- venueSpotDrums       =. edges 38 -- RBN2 docs incorrectly say this is bass
    venueSpotBass        <- venueSpotBass        =. edges 37 -- RBN2 docs incorrectly say this is drums
    venuePostProcessRB3  <- venuePostProcessRB3  =. command
    venuePostProcessRB2  <- (venuePostProcessRB2 =.) $ condenseMap_ $ eachKey each $ blip . \case
      V2.PP_video_trails     -> 110
      V2.PP_video_security   -> 109
      V2.PP_video_bw         -> 108
      V2.PP_video_a          -> 107
      V2.PP_BlueTint         -> 106
      V2.PP_ProFilm_mirror_a -> 105
      V2.PP_ProFilm_b        -> 104
      V2.PP_ProFilm_a        -> 103
      V2.PP_photocopy        -> 102
      V2.PP_photo_negative   -> 101
      V2.PP_film_silvertone  -> 100
      V2.PP_film_sepia_ink   -> 99
      V2.PP_film_16mm        -> 98
      V2.PP_contrast_a       -> 97
      V2.PP_Default          -> 96
    venueLightingShared  <- venueLightingShared  =. command
    venueLightingRB3 <- (venueLightingRB3 =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      Lighting_verse  -> ["lighting", "(verse)"]
      Lighting_chorus -> ["lighting", "(chorus)"]
      Lighting_first  -> ["first"]
      Lighting_prev   -> ["prev"]
      Lighting_next   -> ["next"]
    venueLightingRB2 <- (venueLightingRB2 =.) $ condenseMap_ $ eachKey each $ \case
      Lighting_verse  -> commandMatch ["verse"]
      Lighting_chorus -> commandMatch ["chorus"]
      Lighting_first  -> blip 50
      Lighting_prev   -> blip 49
      Lighting_next   -> blip 48
    venueBonusFX         <- venueBonusFX         =. commandMatch ["bonusfx"]
    venueBonusFXOptional <- venueBonusFXOptional =. commandMatch ["bonusfx_optional"]
    venueFog <- (venueFog =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      True  -> ["FogOn"]
      False -> ["FogOff"]
    return VenueTrack{..}
