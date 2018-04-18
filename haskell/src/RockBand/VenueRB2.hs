-- | This format was implemented with the help of:
-- https://web.archive.org/web/20120109105129/http://creators.rockband.com:80/spec/Camera_And_Lights
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module RockBand.VenueRB2 where

import           RockBand.Codec.Venue (CutEvent2 (..), DoCut2 (..),
                                       LightingShared (..), LightingSplit (..),
                                       PostProcess2 (..))
import           RockBand.Parse

data Event
  = DoCut DoCut2
  | PostProcess PostProcess2
  | Cut CutEvent2
  | SingalongGuitar Bool
  | SingalongDrums Bool
  | SingalongBass Bool
  | SpotlightVocal Bool
  | SpotlightGuitar Bool
  | SpotlightBass Bool
  | SpotlightDrums Bool
  | LightingShared LightingShared
  | LightingSplit LightingSplit
  | BonusFX
  | BonusFXOptional
  | Fog Bool
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing

  [ ( [e| one $ mapParseOne DoCut parseCommand |]
    , [e| \case DoCut m -> unparseCommand m |]
    )

  , blip 110 [p| PostProcess V2_video_trails |]
  , blip 109 [p| PostProcess V2_video_security |]
  , blip 108 [p| PostProcess V2_video_bw |]
  , blip 107 [p| PostProcess V2_video_a |]
  , blip 106 [p| PostProcess V2_BlueTint |]
  , blip 105 [p| PostProcess V2_ProFilm_mirror_a |]
  , blip 104 [p| PostProcess V2_ProFilm_b |]
  , blip 103 [p| PostProcess V2_ProFilm_a |]
  , blip 102 [p| PostProcess V2_photocopy |]
  , blip 101 [p| PostProcess V2_photo_negative |]
  , blip 100 [p| PostProcess V2_film_silvertone |]
  , blip 99 [p| PostProcess V2_film_sepia_ink |]
  , blip 98 [p| PostProcess V2_film_16mm |]
  , blip 97 [p| PostProcess V2_contrast_a |]
  , blip 96 [p| PostProcess V2_Default |]

  , blip 60 [p| Cut CameraCut |]
  , blip 64 [p| Cut FocusVocal |]
  , blip 63 [p| Cut FocusGuitar |]
  , blip 62 [p| Cut FocusDrums |]
  , blip 61 [p| Cut FocusBass |]
  , blip 73 [p| Cut NoClose |]
  , blip 72 [p| Cut OnlyClose |]
  , blip 71 [p| Cut OnlyFar |]
  , blip 70 [p| Cut NoBehind |]

  , edge 87 $ applyB [p| SingalongGuitar |]
  , edge 86 $ applyB [p| SingalongDrums |]
  , edge 85 $ applyB [p| SingalongBass |]
  , edge 40 $ applyB [p| SpotlightVocal |]
  , edge 39 $ applyB [p| SpotlightGuitar |]
  , edge 38 $ applyB [p| SpotlightDrums |]
  , edge 37 $ applyB [p| SpotlightBass |]

  , ( [e| one $ mapParseOne LightingShared parseCommand |]
    , [e| \case LightingShared m -> unparseCommand m |]
    )
  , commandPair ["verse"] [p| LightingSplit Lighting_verse |]
  , commandPair ["chorus"] [p| LightingSplit Lighting_chorus |]
  , blip 50 [p| LightingSplit Lighting_first |]
  , blip 49 [p| LightingSplit Lighting_prev |]
  , blip 48 [p| LightingSplit Lighting_next |]

  , commandPair ["bonusfx"] [p| BonusFX |]
  , commandPair ["bonusfx_optional"] [p| BonusFXOptional |]

  , commandPair ["FogOn"] [p| Fog True |]
  , commandPair ["FogOff"] [p| Fog False |]

  ]
