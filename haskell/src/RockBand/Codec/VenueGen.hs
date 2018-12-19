{- |
Alternate implementation of kueller's venuegen system:
https://github.com/kueller/ReaperRBTools
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RockBand.Codec.VenueGen where

import           Control.Monad.Codec
import           Control.Monad.Random
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Venue
import           RockBand.Common

data LightingTrack t = LightingTrack
  { lightingPostProcess     :: RTB.T t (PostProcess3, Bool)
  , lightingTypes           :: RTB.T t (Lighting (), Bool)
  , lightingCommands        :: RTB.T t LightingCommand
  , lightingBonusFX         :: RTB.T t ()
  , lightingBonusFXOptional :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (LightingTrack t) where
  (<>)
    (LightingTrack a1 a2 a3 a4 a5)
    (LightingTrack b1 b2 b3 b4 b5)
    = LightingTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)

instance (NNC.C t) => Monoid (LightingTrack t) where
  mempty = LightingTrack
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty

instance TraverseTrack LightingTrack where
  traverseTrack fn (LightingTrack a b c d e) = LightingTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack LightingTrack where
  parseTrack = do
    lightingPostProcess <- (lightingPostProcess =.) $ condenseMap $ eachKey each $ edges . \case
      -- basic
      V3_ProFilm_a                    -> 71
      V3_ProFilm_b                    -> 70
      V3_video_a                      -> 69
      V3_film_16mm                    -> 68
      V3_shitty_tv                    -> 67
      V3_bloom                        -> 66
      V3_film_sepia_ink               -> 65
      V3_film_silvertone              -> 64
      V3_film_b_w                     -> 63
      V3_video_bw                     -> 62
      V3_contrast_a                   -> 61
      V3_photocopy                    -> 60
      V3_film_blue_filter             -> 59
      V3_desat_blue                   -> 58
      V3_video_security               -> 57
      -- special
      V3_bright                       -> 55
      V3_posterize                    -> 54
      V3_clean_trails                 -> 53
      V3_video_trails                 -> 52
      V3_flicker_trails               -> 51
      V3_desat_posterize_trails       -> 50
      V3_film_contrast                -> 49
      V3_film_contrast_blue           -> 48
      V3_film_contrast_green          -> 47
      V3_film_contrast_red            -> 46
      V3_horror_movie_special         -> 45
      V3_photo_negative               -> 44
      V3_ProFilm_mirror_a             -> 43
      V3_ProFilm_psychedelic_blue_red -> 42
      V3_space_woosh                  -> 41
    lightingTypes <- (lightingTypes =.) $ condenseMap $ eachKey allLighting $ edges . \case
      -- manual
      Lighting_                 -> (-1) -- not supported by venuegen
      Lighting_intro            -> (-1) -- not supported by venuegen
      Lighting_verse ()         -> 39
      Lighting_chorus ()        -> 38
      Lighting_manual_cool      -> 37
      Lighting_manual_warm      -> 36
      Lighting_dischord         -> 35
      Lighting_stomp            -> 34
      -- automatic
      Lighting_loop_cool        -> 28
      Lighting_loop_warm        -> 27
      Lighting_harmony          -> 26
      Lighting_frenzy           -> 25
      Lighting_silhouettes      -> 24
      Lighting_silhouettes_spot -> 23
      Lighting_searchlights     -> 22
      Lighting_sweep            -> 21
      Lighting_strobe_slow      -> 20
      Lighting_strobe_fast      -> 19
      Lighting_blackout_slow    -> 18
      Lighting_blackout_fast    -> 17
      Lighting_blackout_spot    -> 16
      Lighting_flare_slow       -> 15
      Lighting_flare_fast       -> 14
      Lighting_bre              -> 13
    lightingCommands <- (lightingCommands =.) $ condenseMap_ $ eachKey each $ blip . \case
      LightingFirst -> 32
      LightingPrev  -> 31
      LightingNext  -> 30
    lightingBonusFXOptional <- lightingBonusFXOptional =. blip 11
    lightingBonusFX         <- lightingBonusFX         =. blip 10
    return LightingTrack{..}

venue_generate :: (NNC.C t) => RTB.T t (a, Bool) -> RTB.T t a
venue_generate = go . RTB.flatten . fmap offsBeforeOns . RTB.collectCoincident where
  offsBeforeOns xs = let (ons, offs) = partition snd xs in offs ++ ons
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, (x, True)), rtb') -> RTB.cons dt x $ go rtb'
    Just ((dt, (x, False)), rtb') -> case RTB.viewL rtb' of
      Just ((dt', _), _) | dt' /= NNC.zero -> RTB.cons dt x $ go rtb'
      _                                    -> RTB.delay dt $ go rtb'

venue_ungenerate :: (NNC.C t, Eq a) => t -> RTB.T t a -> RTB.T t (a, Bool)
venue_ungenerate lastLen = go Nothing where
  go cur rtb = case RTB.viewL rtb of
    Nothing -> case cur of
      Nothing -> RTB.empty
      Just x  -> RTB.singleton lastLen (x, False)
    Just ((dt, x), rtb') -> case cur of
      Nothing -> RTB.cons dt (x, True) $ go (Just x) rtb'
      Just y  -> if x == y
        then RTB.cons dt (x, False) $ go Nothing rtb'
        else RTB.cons dt (x, False) $ RTB.cons NNC.zero (y, True) $ go (Just y) rtb'

buildLighting :: (NNC.C t) => LightingTrack t -> VenueTrack t
buildLighting lt = mempty
  { venuePostProcessRB3   = venue_generate $ lightingPostProcess lt
  , venueLighting         = fmap (fmap $ const RBN2) $ venue_generate $ lightingTypes lt
  , venueLightingCommands = (\cmd -> (cmd, RBN2)) <$> lightingCommands lt
  , venueBonusFX          = lightingBonusFX lt
  , venueBonusFXOptional  = lightingBonusFXOptional lt
  }

unbuildLighting :: (NNC.C t) => t -> VenueTrack t -> LightingTrack t
unbuildLighting lastLen vt = LightingTrack
  { lightingPostProcess     = venue_ungenerate lastLen $ venuePostProcessRB3 vt
  , lightingTypes           = venue_ungenerate lastLen $ fmap (const ()) <$> venueLighting vt
  , lightingCommands        = fst <$> venueLightingCommands vt
  , lightingBonusFX         = venueBonusFX vt
  , lightingBonusFXOptional = venueBonusFXOptional vt
  }

data CameraTrack t = CameraTrack
  { cameraCuts   :: RTB.T t (Camera3, Bool)
  , cameraRandom :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (CameraTrack t) where
  (<>)
    (CameraTrack a1 a2)
    (CameraTrack b1 b2)
    = CameraTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)

instance (NNC.C t) => Monoid (CameraTrack t) where
  mempty = CameraTrack RTB.empty RTB.empty

instance TraverseTrack CameraTrack where
  traverseTrack fn (CameraTrack a b) = CameraTrack <$> fn a <*> fn b

instance ParseTrack CameraTrack where
  parseTrack = do
    cameraRandom <- cameraRandom =. blip 102
    cameraCuts <- (cameraCuts =.) $ condenseMap $ eachKey each $ edges . \case
      -- generic 4 camera shots
      V3_coop_all_behind        -> 100
      V3_coop_all_far           -> 99
      V3_coop_all_near          -> 98
      -- 3 char shots (no drum)
      V3_coop_front_behind      -> 96
      V3_coop_front_near        -> 95
      -- 1 char standard shots
      V3_coop_d_behind          -> 93
      V3_coop_d_near            -> 92
      V3_coop_v_behind          -> 91
      V3_coop_v_near            -> 90
      V3_coop_b_behind          -> 89
      V3_coop_b_near            -> 88
      V3_coop_g_behind          -> 87
      V3_coop_g_near            -> 86
      V3_coop_k_behind          -> 85
      V3_coop_k_near            -> 84
      -- 1 char closeups
      V3_coop_d_closeup_hand    -> 82
      V3_coop_d_closeup_head    -> 81
      V3_coop_v_closeup         -> 80
      V3_coop_b_closeup_hand    -> 79
      V3_coop_b_closeup_head    -> 78
      V3_coop_g_closeup_hand    -> 77
      V3_coop_g_closeup_head    -> 76
      V3_coop_k_closeup_hand    -> 75
      V3_coop_k_closeup_head    -> 74
      -- 2 char shots
      V3_coop_dv_near           -> 72
      V3_coop_bd_near           -> 71
      V3_coop_dg_near           -> 70
      V3_coop_bv_behind         -> 69
      V3_coop_bv_near           -> 68
      V3_coop_gv_behind         -> 67
      V3_coop_gv_near           -> 66
      V3_coop_kv_behind         -> 65
      V3_coop_kv_near           -> 64
      V3_coop_bg_behind         -> 63
      V3_coop_bg_near           -> 62
      V3_coop_bk_behind         -> 61
      V3_coop_bk_near           -> 60
      V3_coop_gk_behind         -> 59
      V3_coop_gk_near           -> 58
      -- directed cuts
      V3_directed_all           -> 56
      V3_directed_all_cam       -> 55
      V3_directed_all_lt        -> 54 -- *
      V3_directed_all_yeah      -> 53
      V3_directed_bre           -> 52
      V3_directed_brej          -> 51
      V3_directed_crowd         -> 10 -- *
      V3_directed_drums         -> 43
      V3_directed_drums_pnt     -> 21
      V3_directed_drums_np      -> 49
      V3_directed_drums_lt      -> 42 -- *
      V3_directed_drums_kd      -> 23 -- *
      V3_directed_vocals        -> 41
      V3_directed_vocals_np     -> 46
      V3_directed_vocals_cls    -> 26
      V3_directed_vocals_cam_pr -> 36
      V3_directed_vocals_cam_pt -> 35
      V3_directed_stagedive     -> 29
      V3_directed_crowdsurf     -> 28
      V3_directed_bass          -> 40
      V3_directed_crowd_b       -> 19
      V3_directed_bass_np       -> 48
      V3_directed_bass_cam      -> 31
      V3_directed_bass_cls      -> 25 -- *
      V3_directed_guitar        -> 39
      V3_directed_crowd_g       -> 20
      V3_directed_guitar_np     -> 47
      V3_directed_guitar_cls    -> 24 -- *
      V3_directed_guitar_cam_pr -> 34
      V3_directed_guitar_cam_pt -> 33
      V3_directed_keys          -> 38
      V3_directed_keys_cam      -> 32
      V3_directed_keys_np       -> 45
      V3_directed_duo_drums     -> 17
      V3_directed_duo_bass      -> 15
      V3_directed_duo_guitar    -> 16
      V3_directed_duo_kv        -> 14
      V3_directed_duo_gb        -> 13
      V3_directed_duo_kb        -> 12
      V3_directed_duo_kg        -> 11
    return CameraTrack{..}

cameraLevels :: [(Camera3, Int)]
cameraLevels =
  [ (V3_coop_gk_near       , 7)
  , (V3_coop_gk_behind     , 7)
  , (V3_coop_bk_near       , 6)
  , (V3_coop_bk_behind     , 6)
  , (V3_coop_bg_near       , 5)
  , (V3_coop_bg_behind     , 5)
  , (V3_coop_kv_near       , 4)
  , (V3_coop_kv_behind     , 4)
  , (V3_coop_gv_near       , 3)
  , (V3_coop_gv_behind     , 3)
  , (V3_coop_bv_near       , 2)
  , (V3_coop_bv_behind     , 2)
  , (V3_coop_dg_near       , 1)
  , (V3_coop_bd_near       , 1)
  , (V3_coop_dv_near       , 0)
  , (V3_coop_k_closeup_head, 8)
  , (V3_coop_k_closeup_hand, 8)
  , (V3_coop_g_closeup_head, 2)
  , (V3_coop_g_closeup_hand, 2)
  , (V3_coop_b_closeup_head, 1)
  , (V3_coop_b_closeup_hand, 1)
  , (V3_coop_v_closeup     , 0)
  , (V3_coop_d_closeup_head, 0)
  , (V3_coop_d_closeup_hand, 0)
  , (V3_coop_k_near        , 8)
  , (V3_coop_k_behind      , 8)
  , (V3_coop_g_near        , 2)
  , (V3_coop_g_behind      , 2)
  , (V3_coop_b_near        , 1)
  , (V3_coop_b_behind      , 1)
  , (V3_coop_v_near        , 0)
  , (V3_coop_v_behind      , 0)
  , (V3_coop_d_near        , 0)
  , (V3_coop_d_behind      , 0)
  , (V3_coop_front_near    , 0)
  , (V3_coop_front_behind  , 0)
  , (V3_coop_all_near      , 0)
  , (V3_coop_all_far       , 0)
  , (V3_coop_all_behind    , 0)
  ]

buildCamera :: (NNC.C t, MonadRandom m) => CameraTrack t -> m (VenueTrack t)
buildCamera = undefined
