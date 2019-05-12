{- |
Alternate implementation of kueller's venuegen system:
https://github.com/kueller/ReaperRBTools
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module RockBand.Codec.VenueGen where

import           Control.Monad                    (guard)
import           Control.Monad.Codec
import           Control.Monad.Random             (MonadRandom, fromListMay)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (partition)
import           Data.Maybe                       (catMaybes, fromMaybe)
import           GHC.Generics                     (Generic)
import           MergeMonoid
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Venue
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data LightingTrack t = LightingTrack
  { lightingPostProcess     :: RTB.T t (PostProcess3, Bool)
  , lightingTypes           :: RTB.T t (Lighting (), Bool)
  , lightingCommands        :: RTB.T t LightingCommand
  , lightingBonusFX         :: RTB.T t ()
  , lightingBonusFXOptional :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (LightingTrack t)

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
      Lighting_                 -> 1 -- not supported by venuegen
      Lighting_intro            -> 2 -- not supported by venuegen
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
    lightingCommands <- (lightingCommands =.) $ fatBlips (1/8) $ condenseMap_ $ eachKey each $ blip . \case
      LightingFirst -> 32
      LightingPrev  -> 31
      LightingNext  -> 30
    lightingBonusFXOptional <- lightingBonusFXOptional =. fatBlips (1/8) (blip 11)
    lightingBonusFX         <- lightingBonusFX         =. fatBlips (1/8) (blip 10)
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
        then RTB.cons dt (y, False) $ go Nothing rtb'
        else RTB.cons dt (y, False) $ RTB.cons NNC.zero (x, True) $ go (Just x) rtb'

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
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (CameraTrack t)

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
      V3_directed_all_lt        -> 54 -- free
      V3_directed_all_yeah      -> 53
      V3_directed_bre           -> 52
      V3_directed_brej          -> 51
      V3_directed_crowd         -> 10 -- free
      V3_directed_drums         -> 43
      V3_directed_drums_pnt     -> 21
      V3_directed_drums_np      -> 49
      V3_directed_drums_lt      -> 42 -- free
      V3_directed_drums_kd      -> 23 -- free
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
      V3_directed_bass_cls      -> 25 -- free
      V3_directed_guitar        -> 39
      V3_directed_crowd_g       -> 20
      V3_directed_guitar_np     -> 47
      V3_directed_guitar_cls    -> 24 -- free
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

cameraLevels :: [(Camera3, (Int, [RB3Instrument]))]
cameraLevels =
  [ (V3_coop_gk_near       , (7, [Guitar, Keys]))
  , (V3_coop_gk_behind     , (7, [Guitar, Keys]))
  , (V3_coop_bk_near       , (6, [Bass, Keys]))
  , (V3_coop_bk_behind     , (6, [Bass, Keys]))
  , (V3_coop_bg_near       , (5, [Bass, Guitar]))
  , (V3_coop_bg_behind     , (5, [Bass, Guitar]))
  , (V3_coop_kv_near       , (4, [Keys, Vocal]))
  , (V3_coop_kv_behind     , (4, [Keys, Vocal]))
  , (V3_coop_gv_near       , (3, [Guitar, Vocal]))
  , (V3_coop_gv_behind     , (3, [Guitar, Vocal]))
  , (V3_coop_bv_near       , (2, [Bass, Vocal]))
  , (V3_coop_bv_behind     , (2, [Bass, Vocal]))
  , (V3_coop_dg_near       , (1, [Drums, Guitar]))
  , (V3_coop_bd_near       , (1, [Bass, Drums]))
  , (V3_coop_dv_near       , (0, [Drums, Vocal]))
  , (V3_coop_k_closeup_head, (8, [Keys]))
  , (V3_coop_k_closeup_hand, (8, [Keys]))
  , (V3_coop_g_closeup_head, (2, [Guitar]))
  , (V3_coop_g_closeup_hand, (2, [Guitar]))
  , (V3_coop_b_closeup_head, (1, [Bass]))
  , (V3_coop_b_closeup_hand, (1, [Bass]))
  , (V3_coop_v_closeup     , (0, [Vocal]))
  , (V3_coop_d_closeup_head, (0, [Drums]))
  , (V3_coop_d_closeup_hand, (0, [Drums]))
  , (V3_coop_k_near        , (8, [Keys]))
  , (V3_coop_k_behind      , (8, [Keys]))
  , (V3_coop_g_near        , (2, [Guitar]))
  , (V3_coop_g_behind      , (2, [Guitar]))
  , (V3_coop_b_near        , (1, [Bass]))
  , (V3_coop_b_behind      , (1, [Bass]))
  , (V3_coop_v_near        , (0, [Vocal]))
  , (V3_coop_v_behind      , (0, [Vocal]))
  , (V3_coop_d_near        , (0, [Drums]))
  , (V3_coop_d_behind      , (0, [Drums]))
  , (V3_coop_front_near    , (0, []))
  , (V3_coop_front_behind  , (0, []))
  , (V3_coop_all_near      , (0, []))
  , (V3_coop_all_far       , (0, []))
  , (V3_coop_all_behind    , (0, []))
  ]

buildCamera :: (NNC.C t, MonadRandom m) => [RB3Instrument] -> CameraTrack t -> m (VenueTrack t)
buildCamera hasInsts ct = do
  let joined = joinEdgesSimple $ (\(cut, b) -> (guard b >> Just (), cut)) <$> cameraCuts ct
      go prev rtb = case RTB.viewL rtb of
        Nothing -> return RTB.empty
        Just ((dt, evts), rtb') -> do
          let written = catMaybes evts
              next = case RTB.viewL rtb' of
                Just ((_, evts'), _) -> catMaybes evts'
                _                    -> []
              avoidCuts = map (\((), cut, _) -> cut) $ prev ++ written ++ next
          random <- if elem Nothing evts
            then do
              let minLevel = case map fst $ catMaybes [ lookup cut cameraLevels | ((), cut, _) <- written ] of
                    []     -> 999
                    x : xs -> foldr min x xs
                  possibleCuts = do
                    (cut, (level, insts)) <- cameraLevels
                    guard $ level < minLevel || (level == 0 && minLevel == 0)
                    guard $ all (`elem` hasInsts) insts
                    guard $ notElem cut avoidCuts
                    return cut
              cut <- fromMaybe V3_coop_all_near <$> fromListMay
                [ (cut, 1) | cut <- possibleCuts ]
              return [((), cut, NNC.zero)]
            else return []
          let this = random ++ written
              starCuts =
                [ V3_directed_all_lt
                , V3_directed_crowd
                , V3_directed_drums_lt
                , V3_directed_drums_kd
                , V3_directed_bass_cls
                , V3_directed_guitar_cls
                ]
              translateNoteOff cut len = if elem cut starCuts
                then case RTB.viewL rtb' of
                  Nothing            -> False      -- last cut, don't include end event
                  Just ((dt', _), _) -> dt' /= len -- include end event if it doesn't go to the next cut
                else False                         -- not a "free" cut
              theseCuts = foldr RTB.merge RTB.empty $ do
                ((), cut, len) <- this
                return $ if translateNoteOff cut len
                  then RTB.fromPairList [(NNC.zero, cut), (len, cut)]
                  else RTB.singleton NNC.zero cut
          RTB.cons dt theseCuts <$> go this rtb'
  cuts <- go [] $ RTB.collectCoincident $ RTB.merge
    (fmap Just joined)
    (fmap (const Nothing) $ cameraRandom ct)
  return mempty { venueCameraRB3 = U.trackJoin cuts }

unbuildCamera :: (NNC.C t) => t -> VenueTrack t -> CameraTrack t
unbuildCamera lastLen vt = CameraTrack
  { cameraCuts = let
    -- each set of cuts, just make it a note that extends to the next set of cuts
    pairs = RTB.toPairList $ RTB.collectCoincident $ venueCameraRB3 vt
    lens = drop 1 (map fst pairs) ++ [lastLen]
    in U.trackJoin $ RTB.fromPairList $ flip map (zip pairs lens) $ \((dt, cuts), len) ->
      (dt, RTB.flatten $ RTB.fromPairList [(NNC.zero, map (, True) cuts), (len, map (, False) cuts)])
    -- TODO this makes repeated directed cuts look weird, should fix
  , cameraRandom = RTB.empty
  }
