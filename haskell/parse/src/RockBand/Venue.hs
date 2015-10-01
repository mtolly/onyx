-- | The RB3 (RBN2) VENUE track format.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.Venue where

import           RockBand.Parse
import           RockBand.Common

data Camera
  -- generic 4 camera shots
  = Camera_coop_all_behind
  | Camera_coop_all_far
  | Camera_coop_all_near
  -- 3 char shots (no drum)
  | Camera_coop_front_behind
  | Camera_coop_front_near
  -- 1 char standard shots
  | Camera_coop_d_behind
  | Camera_coop_d_near
  | Camera_coop_v_behind
  | Camera_coop_v_near
  | Camera_coop_b_behind
  | Camera_coop_b_near
  | Camera_coop_g_behind
  | Camera_coop_g_near
  | Camera_coop_k_behind
  | Camera_coop_k_near
  -- 1 char closeups
  | Camera_coop_d_closeup_hand
  | Camera_coop_d_closeup_head
  | Camera_coop_v_closeup
  | Camera_coop_b_closeup_hand
  | Camera_coop_b_closeup_head
  | Camera_coop_g_closeup_hand
  | Camera_coop_g_closeup_head
  | Camera_coop_k_closeup_hand
  | Camera_coop_k_closeup_head
  -- 2 char shots
  | Camera_coop_dv_near
  | Camera_coop_bd_near
  | Camera_coop_dg_near
  | Camera_coop_bv_behind
  | Camera_coop_bv_near
  | Camera_coop_gv_behind
  | Camera_coop_gv_near
  | Camera_coop_kv_behind
  | Camera_coop_kv_near
  | Camera_coop_bg_behind
  | Camera_coop_bg_near
  | Camera_coop_bk_behind
  | Camera_coop_bk_near
  | Camera_coop_gk_behind
  | Camera_coop_gk_near
  -- directed cuts
  | Camera_directed_all
  | Camera_directed_all_cam
  | Camera_directed_all_lt
  | Camera_directed_all_yeah
  | Camera_directed_bre
  | Camera_directed_brej
  | Camera_directed_crowd
  | Camera_directed_drums
  | Camera_directed_drums_pnt
  | Camera_directed_drums_np
  | Camera_directed_drums_lt
  | Camera_directed_drums_kd
  | Camera_directed_vocals
  | Camera_directed_vocals_np
  | Camera_directed_vocals_cls
  | Camera_directed_vocals_cam_pr
  | Camera_directed_vocals_cam_pt
  | Camera_directed_stagedive
  | Camera_directed_crowdsurf
  | Camera_directed_bass
  | Camera_directed_crowd_b
  | Camera_directed_bass_np
  | Camera_directed_bass_cam
  | Camera_directed_bass_cls
  | Camera_directed_guitar
  | Camera_directed_crowd_g
  | Camera_directed_guitar_np
  | Camera_directed_guitar_cls
  | Camera_directed_guitar_cam_pr
  | Camera_directed_guitar_cam_pt
  | Camera_directed_keys
  | Camera_directed_keys_cam
  | Camera_directed_keys_np
  | Camera_directed_duo_drums
  | Camera_directed_duo_bass
  | Camera_directed_duo_guitar
  | Camera_directed_duo_kv
  | Camera_directed_duo_gb
  | Camera_directed_duo_kb
  | Camera_directed_duo_kg
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Camera where
  fromCommand x = [drop (length "Camera_") (show x)]
  toCommand = reverseLookup each fromCommand

data PostProcess
  -- basic
  = PP_ProFilm_a
  | PP_ProFilm_b
  | PP_video_a
  | PP_film_16mm
  | PP_shitty_tv
  | PP_bloom
  | PP_film_sepia_ink
  | PP_film_silvertone
  | PP_film_b_w
  | PP_video_bw
  | PP_contrast_a
  | PP_photocopy
  | PP_film_blue_filter
  | PP_desat_blue
  | PP_video_security
  -- special
  | PP_bright
  | PP_posterize
  | PP_clean_trails
  | PP_video_trails
  | PP_flicker_trails
  | PP_desat_posterize_trails
  | PP_film_contrast
  | PP_film_contrast_blue
  | PP_film_contrast_green
  | PP_film_contrast_red
  | PP_horror_movie_special
  | PP_photo_negative
  | PP_ProFilm_mirror_a
  | PP_ProFilm_psychedelic_blue_red
  | PP_space_woosh
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command PostProcess where
  fromCommand PP_film_b_w = ["film_b+w.pp"]
  fromCommand x = [drop (length "PP_") (show x) ++ ".pp"]
  toCommand = reverseLookup each fromCommand

data Lighting
  = Lighting_intro
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
  | Lighting_blackout_spot
  | Lighting_flare_slow
  | Lighting_flare_fast
  | Lighting_bre
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Lighting where
  fromCommand x = ["lighting", "(" ++ drop (length "Lighting_") (show x) ++ ")"]
  toCommand = reverseLookup each fromCommand

data Event
  = Camera Camera
  | SingalongGuitarKeys Bool
  | SingalongDrums Bool
  | SingalongBassKeys Bool
  | SpotlightKeys Bool
  | SpotlightVocal Bool
  | SpotlightGuitar Bool
  | SpotlightBass Bool
  | SpotlightDrums Bool
  | PostProcess PostProcess
  | Lighting Lighting
  | LightingFirst
  | LightingPrev
  | LightingNext
  | BonusFX
  | BonusFXOptional
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |]
  [ ( [e| mapParseOne Camera parseCommand |]
    , [e| \case Camera m -> unparseCommand m |]
    )
  , edge 87 $ applyB [p| SingalongGuitarKeys |]
  , edge 86 $ applyB [p| SingalongDrums |]
  , edge 85 $ applyB [p| SingalongBassKeys |]
  , edge 41 $ applyB [p| SpotlightKeys |]
  , edge 40 $ applyB [p| SpotlightVocal |]
  , edge 39 $ applyB [p| SpotlightGuitar |]
  , edge 38 $ applyB [p| SpotlightBass |]
  , edge 37 $ applyB [p| SpotlightDrums |]
  , ( [e| mapParseOne PostProcess parseCommand |]
    , [e| \case PostProcess m -> unparseCommand m |]
    )
  , ( [e| mapParseOne Lighting parseCommand |]
    , [e| \case Lighting m -> unparseCommand m |]
    )
  , commandPair ["first"] [p| LightingFirst |]
  , commandPair ["prev"] [p| LightingPrev |]
  , commandPair ["next"] [p| LightingNext |]
  , commandPair ["bonusfx"] [p| BonusFX |]
  , commandPair ["bonusfx_optional"] [p| BonusFXOptional |]
  ]
