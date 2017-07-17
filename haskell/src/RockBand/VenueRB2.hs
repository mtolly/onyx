-- | This format was implemented with the help of:
-- https://web.archive.org/web/20120109105129/http://creators.rockband.com:80/spec/Camera_And_Lights
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.VenueRB2 where

import           Data.Data
import           Data.Monoid     ((<>))
import qualified Data.Text       as T
import           RockBand.Common
import           RockBand.Parse

-- | In RB2 format, only directed cuts use text events.
data Camera
  = Camera_directed_all
  | Camera_directed_all_yeah
  | Camera_directed_bre
  | Camera_directed_brej
  | Camera_directed_drums_np
  | Camera_directed_bass_np
  | Camera_directed_guitar_np
  | Camera_directed_vocals_np
  | Camera_directed_drums
  | Camera_directed_bass
  | Camera_directed_guitar
  | Camera_directed_vocals
  | Camera_directed_bass_cam
  | Camera_directed_guitar_cam
  | Camera_directed_vocals_cam
  | Camera_directed_duo_guitar
  | Camera_directed_duo_bass
  | Camera_directed_duo_drums
  | Camera_directed_drums_pnt
  | Camera_directed_guitar_cls
  | Camera_directed_bass_cls
  | Camera_directed_vocals_cls
  | Camera_directed_drums_kd
  | Camera_directed_stagedive
  | Camera_directed_duo_gb
  | Camera_directed_all_cam
  | Camera_directed_crowd_g
  | Camera_directed_crowd_b
  -- These aren't listed on RBN1 docs but they do exist, I think
  | Camera_directed_all_lt
  | Camera_directed_drums_lt
  | Camera_directed_crowdsurf
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command Camera where
  fromCommand x = case T.stripPrefix "Camera_" $ T.pack $ show x of
    Just s  -> [s]
    Nothing -> error "panic! couldn't strip Camera_ from venue event"
  toCommand = reverseLookup each fromCommand

data DoCut
  = DoDirectedCut Camera
  | DoOptionalCut Camera
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instance Command DoCut where
  fromCommand (DoDirectedCut cam) = "do_directed_cut" : fromCommand cam
  fromCommand (DoOptionalCut cam) = "do_optional_cut" : fromCommand cam
  toCommand ("do_directed_cut" : args) = DoDirectedCut <$> toCommand args
  toCommand ("do_optional_cut" : args) = DoOptionalCut <$> toCommand args
  toCommand _                          = Nothing

data PostProcess
  = PP_video_trails
  | PP_video_security
  | PP_video_bw
  | PP_video_a
  | PP_BlueTint -- dunno what this does in rb3, does not correspond to rbn2 text event
  | PP_ProFilm_mirror_a
  | PP_ProFilm_b
  | PP_ProFilm_a
  | PP_photocopy
  | PP_photo_negative
  | PP_film_silvertone
  | PP_film_sepia_ink
  | PP_film_16mm
  | PP_contrast_a
  | PP_Default -- rbn2 docs say ProFilm_a is "default". apparently in rb2 they were different?
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data Lighting
  = Lighting_ -- ^ @[lighting ()]@
  | Lighting_verse
  | Lighting_chorus
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
  | Lighting_flare_slow
  | Lighting_flare_fast
  | Lighting_bre
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command Lighting where
  fromCommand Lighting_verse = ["verse"]
  fromCommand Lighting_chorus = ["chorus"]
  fromCommand x = case T.stripPrefix "Lighting_" $ T.pack $ show x of
    Just s  -> ["lighting", "(" <> s <> ")"]
    Nothing -> error "panic! couldn't strip Lighting_ from venue event"
  toCommand = reverseLookup each fromCommand

data Event
  = DoCut DoCut
  | PostProcess PostProcess
  | CameraCut
  | FocusVocal
  | FocusGuitar
  | FocusBass
  | FocusDrums
  | NoClose
  | OnlyClose
  | OnlyFar
  | NoBehind
  | SingalongGuitar Bool
  | SingalongDrums Bool
  | SingalongBass Bool
  | SpotlightVocal Bool
  | SpotlightGuitar Bool
  | SpotlightBass Bool
  | SpotlightDrums Bool
  | Lighting Lighting
  | LightingFirst
  | LightingPrev
  | LightingNext
  | BonusFX
  | BonusFXOptional
  deriving (Eq, Ord, Show, Read, Typeable, Data)

instanceMIDIEvent [t| Event |] Nothing

  [ ( [e| mapParseOne DoCut parseCommand |]
    , [e| \case DoCut m -> unparseCommand m |]
    )

  , blip 110 [p| PostProcess PP_video_trails |]
  , blip 109 [p| PostProcess PP_video_security |]
  , blip 108 [p| PostProcess PP_video_bw |]
  , blip 107 [p| PostProcess PP_video_a |]
  , blip 106 [p| PostProcess PP_BlueTint |]
  , blip 105 [p| PostProcess PP_ProFilm_mirror_a |]
  , blip 104 [p| PostProcess PP_ProFilm_b |]
  , blip 103 [p| PostProcess PP_ProFilm_a |]
  , blip 102 [p| PostProcess PP_photocopy |]
  , blip 101 [p| PostProcess PP_photo_negative |]
  , blip 100 [p| PostProcess PP_film_silvertone |]
  , blip 99 [p| PostProcess PP_film_sepia_ink |]
  , blip 98 [p| PostProcess PP_film_16mm |]
  , blip 97 [p| PostProcess PP_contrast_a |]
  , blip 96 [p| PostProcess PP_Default |]

  , blip 60 [p| CameraCut |]
  , blip 64 [p| FocusVocal |]
  , blip 63 [p| FocusGuitar |]
  , blip 62 [p| FocusDrums |]
  , blip 61 [p| FocusBass |]
  , blip 73 [p| NoClose |]
  , blip 72 [p| OnlyClose |]
  , blip 71 [p| OnlyFar |]
  , blip 70 [p| NoBehind |]

  , edge 87 $ applyB [p| SingalongGuitar |]
  , edge 86 $ applyB [p| SingalongDrums |]
  , edge 85 $ applyB [p| SingalongBass |]
  , edge 40 $ applyB [p| SpotlightVocal |]
  , edge 39 $ applyB [p| SpotlightGuitar |]
  -- Did HMX actually swap bass and drums spotlight pitches from RBN1 to RBN2 like docs claim? Should test
  , edge 38 $ applyB [p| SpotlightDrums |]
  , edge 37 $ applyB [p| SpotlightBass |]

  , ( [e| mapParseOne Lighting parseCommand |]
    , [e| \case Lighting m -> unparseCommand m |]
    )
  , blip 50 [p| LightingFirst |]
  , blip 49 [p| LightingPrev |]
  , blip 48 [p| LightingNext |]

  , commandPair ["bonusfx"] [p| BonusFX |]
  , commandPair ["bonusfx_optional"] [p| BonusFXOptional |]

  ]
