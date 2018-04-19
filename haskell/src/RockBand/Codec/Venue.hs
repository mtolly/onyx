{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.Venue where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (sortBy)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common

data Camera3
  -- generic 4 camera shots
  = V3_coop_all_behind
  | V3_coop_all_far
  | V3_coop_all_near
  -- 3 char shots (no drum)
  | V3_coop_front_behind
  | V3_coop_front_near
  -- 1 char standard shots
  | V3_coop_d_behind
  | V3_coop_d_near
  | V3_coop_v_behind
  | V3_coop_v_near
  | V3_coop_b_behind
  | V3_coop_b_near
  | V3_coop_g_behind
  | V3_coop_g_near
  | V3_coop_k_behind
  | V3_coop_k_near
  -- 1 char closeups
  | V3_coop_d_closeup_hand
  | V3_coop_d_closeup_head
  | V3_coop_v_closeup
  | V3_coop_b_closeup_hand
  | V3_coop_b_closeup_head
  | V3_coop_g_closeup_hand
  | V3_coop_g_closeup_head
  | V3_coop_k_closeup_hand
  | V3_coop_k_closeup_head
  -- 2 char shots
  | V3_coop_dv_near
  | V3_coop_bd_near
  | V3_coop_dg_near
  | V3_coop_bv_behind
  | V3_coop_bv_near
  | V3_coop_gv_behind
  | V3_coop_gv_near
  | V3_coop_kv_behind
  | V3_coop_kv_near
  | V3_coop_bg_behind
  | V3_coop_bg_near
  | V3_coop_bk_behind
  | V3_coop_bk_near
  | V3_coop_gk_behind
  | V3_coop_gk_near
  -- directed cuts
  | V3_directed_all
  | V3_directed_all_cam
  | V3_directed_all_lt
  | V3_directed_all_yeah
  | V3_directed_bre
  | V3_directed_brej
  | V3_directed_crowd
  | V3_directed_drums
  | V3_directed_drums_pnt
  | V3_directed_drums_np
  | V3_directed_drums_lt
  | V3_directed_drums_kd
  | V3_directed_vocals
  | V3_directed_vocals_np
  | V3_directed_vocals_cls
  | V3_directed_vocals_cam_pr
  | V3_directed_vocals_cam_pt
  | V3_directed_stagedive
  | V3_directed_crowdsurf
  | V3_directed_bass
  | V3_directed_crowd_b
  | V3_directed_bass_np
  | V3_directed_bass_cam
  | V3_directed_bass_cls
  | V3_directed_guitar
  | V3_directed_crowd_g
  | V3_directed_guitar_np
  | V3_directed_guitar_cls
  | V3_directed_guitar_cam_pr
  | V3_directed_guitar_cam_pt
  | V3_directed_keys
  | V3_directed_keys_cam
  | V3_directed_keys_np
  | V3_directed_duo_drums
  | V3_directed_duo_bass
  | V3_directed_duo_guitar
  | V3_directed_duo_kv
  | V3_directed_duo_gb
  | V3_directed_duo_kb
  | V3_directed_duo_kg
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Camera3 where
  fromCommand x = case T.stripPrefix "V3_" $ T.pack $ show x of
    Just s  -> [s]
    Nothing -> error "panic! couldn't strip V3_ from venue event"
  toCommand = reverseLookup each fromCommand

-- | In RB2 format, only directed cuts use text events.
data Camera2
  = V2_directed_all
  | V2_directed_all_yeah
  | V2_directed_bre
  | V2_directed_brej
  | V2_directed_drums_np
  | V2_directed_bass_np
  | V2_directed_guitar_np
  | V2_directed_vocals_np
  | V2_directed_drums
  | V2_directed_bass
  | V2_directed_guitar
  | V2_directed_vocals
  | V2_directed_bass_cam
  | V2_directed_guitar_cam
  | V2_directed_vocals_cam
  | V2_directed_duo_guitar
  | V2_directed_duo_bass
  | V2_directed_duo_drums
  | V2_directed_drums_pnt
  | V2_directed_guitar_cls
  | V2_directed_bass_cls
  | V2_directed_vocals_cls
  | V2_directed_drums_kd
  | V2_directed_stagedive
  | V2_directed_duo_gb
  | V2_directed_all_cam
  | V2_directed_crowd_g
  | V2_directed_crowd_b
  -- These aren't listed on RBN1 docs but they do exist, I think
  | V2_directed_all_lt
  | V2_directed_drums_lt
  | V2_directed_crowdsurf
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Camera2 where
  fromCommand x = case T.stripPrefix "V2_" $ T.pack $ show x of
    Just s  -> [s]
    Nothing -> error "panic! couldn't strip V2_ from venue event"
  toCommand = reverseLookup each fromCommand

data DoCut2
  = DoDirectedCut Camera2
  | DoOptionalCut Camera2
  deriving (Eq, Ord, Show, Read)

instance Command DoCut2 where
  fromCommand (DoDirectedCut cam) = "do_directed_cut" : fromCommand cam
  fromCommand (DoOptionalCut cam) = "do_optional_cut" : fromCommand cam
  toCommand ("do_directed_cut" : args) = DoDirectedCut <$> toCommand args
  toCommand ("do_optional_cut" : args) = DoOptionalCut <$> toCommand args
  toCommand _                          = Nothing

data PostProcess3
  -- basic
  = V3_ProFilm_a
  | V3_ProFilm_b
  | V3_video_a
  | V3_film_16mm
  | V3_shitty_tv
  | V3_bloom
  | V3_film_sepia_ink
  | V3_film_silvertone
  | V3_film_b_w
  | V3_video_bw
  | V3_contrast_a
  | V3_photocopy
  | V3_film_blue_filter
  | V3_desat_blue
  | V3_video_security
  -- special
  | V3_bright
  | V3_posterize
  | V3_clean_trails
  | V3_video_trails
  | V3_flicker_trails
  | V3_desat_posterize_trails
  | V3_film_contrast
  | V3_film_contrast_blue
  | V3_film_contrast_green
  | V3_film_contrast_red
  | V3_horror_movie_special
  | V3_photo_negative
  | V3_ProFilm_mirror_a
  | V3_ProFilm_psychedelic_blue_red
  | V3_space_woosh
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command PostProcess3 where
  fromCommand V3_film_b_w = ["film_b+w.pp"]
  fromCommand x           = case T.stripPrefix "V3_" $ T.pack $ show x of
    Just s  -> [s <> ".pp"]
    Nothing -> error "panic! couldn't strip V3_ from venue event"
  toCommand = reverseLookup each fromCommand

data PostProcess2
  = V2_video_trails
  | V2_video_security
  | V2_video_bw
  | V2_video_a
  | V2_BlueTint -- dunno what this does in rb3, does not correspond to rbn2 text event
  | V2_ProFilm_mirror_a
  | V2_ProFilm_b
  | V2_ProFilm_a
  | V2_photocopy
  | V2_photo_negative
  | V2_film_silvertone
  | V2_film_sepia_ink
  | V2_film_16mm
  | V2_contrast_a
  | V2_Default -- rbn2 docs say ProFilm_a is "default". apparently in rb2 they were different?
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CutEvent2
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

  { venueCameraRB3       :: RTB.T t Camera3
  , venueCameraRB2       :: RTB.T t CutEvent2
  , venueDirectedRB2     :: RTB.T t DoCut2

  , venueSingGuitar      :: RTB.T t Bool -- or keys if no guitar
  , venueSingDrums       :: RTB.T t Bool
  , venueSingBass        :: RTB.T t Bool -- or keys if no bass

  , venueSpotKeys        :: RTB.T t Bool -- rb3 only of course
  , venueSpotVocal       :: RTB.T t Bool
  , venueSpotGuitar      :: RTB.T t Bool
  , venueSpotDrums       :: RTB.T t Bool
  , venueSpotBass        :: RTB.T t Bool

  , venuePostProcessRB3  :: RTB.T t PostProcess3
  , venuePostProcessRB2  :: RTB.T t PostProcess2

  , venueLightingShared  :: RTB.T t LightingShared
  , venueLightingRB3     :: RTB.T t LightingSplit
  , venueLightingRB2     :: RTB.T t LightingSplit

  , venueBonusFX         :: RTB.T t ()
  , venueBonusFXOptional :: RTB.T t ()

  , venueFog             :: RTB.T t Bool

  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Monoid (VenueTrack t) where
  mempty = VenueTrack RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (VenueTrack a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19)
    (VenueTrack b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 b17 b18 b19)
    = VenueTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)
      (RTB.merge a7 b7)
      (RTB.merge a8 b8)
      (RTB.merge a9 b9)
      (RTB.merge a10 b10)
      (RTB.merge a11 b11)
      (RTB.merge a12 b12)
      (RTB.merge a13 b13)
      (RTB.merge a14 b14)
      (RTB.merge a15 b15)
      (RTB.merge a16 b16)
      (RTB.merge a17 b17)
      (RTB.merge a18 b18)
      (RTB.merge a19 b19)

instance TraverseTrack VenueTrack where
  traverseTrack fn (VenueTrack a b c d e f g h i j k l m n o p q r s) = VenueTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k <*> fn l
    <*> fn m <*> fn n <*> fn o <*> fn p <*> fn q <*> fn r
    <*> fn s

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
      V2_video_trails     -> 110
      V2_video_security   -> 109
      V2_video_bw         -> 108
      V2_video_a          -> 107
      V2_BlueTint         -> 106
      V2_ProFilm_mirror_a -> 105
      V2_ProFilm_b        -> 104
      V2_ProFilm_a        -> 103
      V2_photocopy        -> 102
      V2_photo_negative   -> 101
      V2_film_silvertone  -> 100
      V2_film_sepia_ink   -> 99
      V2_film_16mm        -> 98
      V2_contrast_a       -> 97
      V2_Default          -> 96
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

compileVenueRB3 :: (NNC.C t) => VenueTrack t -> VenueTrack t
compileVenueRB3 vt = vt
  { venueCameraRB3 = let
    directed = flip fmap (venueDirectedRB2 vt) $ \docut -> let
      cut = case docut of
        DoDirectedCut c -> c
        DoOptionalCut c -> c
      in case cut of
        V2_directed_all        -> V3_directed_all
        V2_directed_all_yeah   -> V3_directed_all_yeah
        V2_directed_bre        -> V3_directed_bre
        V2_directed_brej       -> V3_directed_brej
        V2_directed_drums_np   -> V3_directed_drums_np
        V2_directed_bass_np    -> V3_directed_bass_np
        V2_directed_guitar_np  -> V3_directed_guitar_np
        V2_directed_vocals_np  -> V3_directed_vocals_np
        V2_directed_drums      -> V3_directed_drums
        V2_directed_bass       -> V3_directed_bass
        V2_directed_guitar     -> V3_directed_guitar
        V2_directed_vocals     -> V3_directed_vocals
        V2_directed_bass_cam   -> V3_directed_bass_cam
        V2_directed_guitar_cam -> V3_directed_guitar_cam_pt -- tweak
        V2_directed_vocals_cam -> V3_directed_vocals_cam_pt -- tweak
        V2_directed_duo_guitar -> V3_directed_duo_guitar
        V2_directed_duo_bass   -> V3_directed_duo_bass
        V2_directed_duo_drums  -> V3_directed_duo_drums
        V2_directed_drums_pnt  -> V3_directed_drums_pnt
        V2_directed_guitar_cls -> V3_directed_guitar_cls
        V2_directed_bass_cls   -> V3_directed_bass_cls
        V2_directed_vocals_cls -> V3_directed_vocals_cls
        V2_directed_drums_kd   -> V3_directed_drums_kd
        V2_directed_stagedive  -> V3_directed_stagedive
        V2_directed_duo_gb     -> V3_directed_duo_gb
        V2_directed_all_cam    -> V3_directed_all_cam
        V2_directed_crowd_g    -> V3_directed_crowd_g
        V2_directed_crowd_b    -> V3_directed_crowd_b
        V2_directed_all_lt     -> V3_directed_all_lt
        V2_directed_drums_lt   -> V3_directed_drums_lt
        V2_directed_crowdsurf  -> V3_directed_crowdsurf
    generic = RTB.flatten $ fmap genericInstant $ RTB.collectCoincident $ venueCameraRB2 vt
    genericInstant xs = case elem CameraCut xs of
      False -> []
      True -> let
        closeup = all (`notElem` xs) [NoClose, OnlyFar]
        behind = all (`notElem` xs) [OnlyClose, OnlyFar, NoBehind]
        far = notElem OnlyClose xs
        near = all (`notElem` xs) [OnlyClose, OnlyFar]
        cuts = case (elem FocusVocal xs, elem FocusGuitar xs, elem FocusBass xs, elem FocusDrums xs) of
          (True, False, False, False) -> concat
            [ [V3_coop_v_behind | behind]
            , [V3_coop_v_near | near]
            , [V3_coop_v_closeup | closeup]
            ]
          (False, True, False, False) -> concat
            [ [V3_coop_g_behind | behind]
            , [V3_coop_g_near | near]
            , [V3_coop_g_closeup_hand | closeup]
            , [V3_coop_g_closeup_head | closeup]
            ]
          (False, False, True, False) -> concat
            [ [V3_coop_b_behind | behind]
            , [V3_coop_b_near | near]
            , [V3_coop_b_closeup_hand | closeup]
            , [V3_coop_b_closeup_head | closeup]
            ]
          (False, False, False, True) -> concat
            [ [V3_coop_d_behind | behind]
            , [V3_coop_d_near | near]
            , [V3_coop_d_closeup_hand | closeup]
            , [V3_coop_d_closeup_head | closeup]
            ]
          (True, True, True, False) -> concat
            [ [V3_coop_front_behind | behind]
            , [V3_coop_front_near | near]
            ]
          (True, True, False, False) -> concat
            [ [V3_coop_gv_behind | behind]
            , [V3_coop_gv_near | near]
            ]
          (True, False, True, False) -> concat
            [ [V3_coop_bv_behind | behind]
            , [V3_coop_bv_near | near]
            ]
          (False, True, True, False) -> concat
            [ [V3_coop_bg_behind | behind]
            , [V3_coop_bg_near | near]
            ]
          (True, False, False, True) -> concat
            [ [V3_coop_dv_near | near]
            ]
          (False, True, False, True) -> concat
            [ [V3_coop_dg_near | near]
            ]
          (False, False, True, True) -> concat
            [ [V3_coop_bd_near | near]
            ]
          -- all, nothing, or invalid 3-char including drums
          _ -> concat
            [ [V3_coop_all_behind | behind]
            , [V3_coop_all_far | far]
            , [V3_coop_all_near | near]
            ]
        in case cuts of
          [] -> [V3_coop_all_near] -- some default if we cut but excluded all options
          _  -> cuts
    in RTB.merge (venueCameraRB3 vt) $ RTB.merge generic directed
  , venueCameraRB2 = RTB.empty
  , venueDirectedRB2 = RTB.empty
  , venuePostProcessRB3
    = RTB.merge (venuePostProcessRB3 vt)
    $ flip fmap (venuePostProcessRB2 vt)
    $ \case
      V2_video_trails     -> V3_video_trails
      V2_video_security   -> V3_video_security
      V2_video_bw         -> V3_video_bw
      V2_video_a          -> V3_video_a
      V2_BlueTint         -> V3_film_blue_filter -- not in rbn2 list
      V2_ProFilm_mirror_a -> V3_ProFilm_mirror_a
      V2_ProFilm_b        -> V3_ProFilm_b
      V2_ProFilm_a        -> V3_ProFilm_a
      V2_photocopy        -> V3_photocopy
      V2_photo_negative   -> V3_photo_negative
      V2_film_silvertone  -> V3_film_silvertone
      V2_film_sepia_ink   -> V3_film_sepia_ink
      V2_film_16mm        -> V3_film_16mm
      V2_contrast_a       -> V3_contrast_a
      V2_Default          -> V3_ProFilm_a
      -- ^ not in rbn2 list, but rbn2 docs say ProFilm_a is "default"
  , venuePostProcessRB2 = RTB.empty
  , venueLightingRB3 = RTB.merge (venueLightingRB3 vt) (venueLightingRB2 vt)
  , venueLightingRB2 = RTB.empty
  , venueFog = RTB.empty
  }

compileVenueRB2 :: (NNC.C t) => VenueTrack t -> VenueTrack t
compileVenueRB2 vt = let
  cut dist insts = map Left $ CameraCut : (dist ++ insts)
  behind = cut [NoClose]
  close = cut [NoBehind, OnlyClose]
  far = cut [NoBehind, OnlyFar, NoClose]
  near = cut [NoBehind, NoClose]
  directed x = [Right $ DoDirectedCut x]
  camera = RTB.flatten $ flip fmap (RTB.collectCoincident $ venueCameraRB3 vt) $ \cams -> case sortBy (flip compare) cams of
    [] -> [] -- shouldn't happen
    x : _ -> case x of
      -- generic 4 camera shots
      V3_coop_all_behind        -> behind [FocusGuitar, FocusBass, FocusDrums, FocusVocal]
      V3_coop_all_far           -> far [FocusGuitar, FocusBass, FocusDrums, FocusVocal]
      V3_coop_all_near          -> near [FocusGuitar, FocusBass, FocusDrums, FocusVocal]
      -- 3 char shots (no drum)
      V3_coop_front_behind      -> behind [FocusGuitar, FocusBass, FocusVocal]
      V3_coop_front_near        -> near [FocusGuitar, FocusBass, FocusVocal]
      -- 1 char standard shots
      V3_coop_d_behind          -> behind [FocusDrums]
      V3_coop_d_near            -> near [FocusDrums]
      V3_coop_v_behind          -> behind [FocusVocal]
      V3_coop_v_near            -> near [FocusVocal]
      V3_coop_b_behind          -> behind [FocusBass]
      V3_coop_b_near            -> near [FocusBass]
      V3_coop_g_behind          -> behind [FocusGuitar]
      V3_coop_g_near            -> near [FocusGuitar]
      V3_coop_k_behind          -> behind [FocusGuitar]
      V3_coop_k_near            -> near [FocusGuitar]
      -- 1 char closeups
      V3_coop_d_closeup_hand    -> close [FocusDrums]
      V3_coop_d_closeup_head    -> close [FocusDrums]
      V3_coop_v_closeup         -> close [FocusVocal]
      V3_coop_b_closeup_hand    -> close [FocusBass]
      V3_coop_b_closeup_head    -> close [FocusBass]
      V3_coop_g_closeup_hand    -> close [FocusGuitar]
      V3_coop_g_closeup_head    -> close [FocusGuitar]
      V3_coop_k_closeup_hand    -> close [FocusGuitar]
      V3_coop_k_closeup_head    -> close [FocusGuitar]
      -- 2 char shots
      V3_coop_dv_near           -> near [FocusDrums, FocusVocal]
      V3_coop_bd_near           -> near [FocusBass, FocusDrums]
      V3_coop_dg_near           -> near [FocusDrums, FocusGuitar]
      V3_coop_bv_behind         -> behind [FocusBass, FocusVocal]
      V3_coop_bv_near           -> near [FocusBass, FocusVocal]
      V3_coop_gv_behind         -> behind [FocusGuitar, FocusVocal]
      V3_coop_gv_near           -> near [FocusGuitar, FocusVocal]
      V3_coop_kv_behind         -> behind [FocusGuitar, FocusVocal]
      V3_coop_kv_near           -> near [FocusGuitar, FocusVocal]
      V3_coop_bg_behind         -> behind [FocusBass, FocusGuitar]
      V3_coop_bg_near           -> near [FocusBass, FocusGuitar]
      V3_coop_bk_behind         -> behind [FocusBass, FocusGuitar]
      V3_coop_bk_near           -> near [FocusBass, FocusGuitar]
      V3_coop_gk_behind         -> behind [FocusGuitar, FocusBass]
      V3_coop_gk_near           -> near [FocusGuitar, FocusBass]
      -- directed cuts
      V3_directed_all           -> directed V2_directed_all
      V3_directed_all_cam       -> directed V2_directed_all_cam
      V3_directed_all_lt        -> directed V2_directed_all_lt
      V3_directed_all_yeah      -> directed V2_directed_all_yeah
      V3_directed_bre           -> directed V2_directed_bre
      V3_directed_brej          -> directed V2_directed_brej
      V3_directed_crowd         -> directed V2_directed_crowd_g
      V3_directed_drums         -> directed V2_directed_drums
      V3_directed_drums_pnt     -> directed V2_directed_drums_pnt
      V3_directed_drums_np      -> directed V2_directed_drums_np
      V3_directed_drums_lt      -> directed V2_directed_drums_lt
      V3_directed_drums_kd      -> directed V2_directed_drums_kd
      V3_directed_vocals        -> directed V2_directed_vocals
      V3_directed_vocals_np     -> directed V2_directed_vocals_np
      V3_directed_vocals_cls    -> directed V2_directed_vocals_cls
      V3_directed_vocals_cam_pr -> directed V2_directed_vocals_cam
      V3_directed_vocals_cam_pt -> directed V2_directed_vocals_cam
      V3_directed_stagedive     -> directed V2_directed_stagedive
      V3_directed_crowdsurf     -> directed V2_directed_crowdsurf
      V3_directed_bass          -> directed V2_directed_bass
      V3_directed_crowd_b       -> directed V2_directed_crowd_b
      V3_directed_bass_np       -> directed V2_directed_bass_np
      V3_directed_bass_cam      -> directed V2_directed_bass_cam
      V3_directed_bass_cls      -> directed V2_directed_bass_cls
      V3_directed_guitar        -> directed V2_directed_guitar
      V3_directed_crowd_g       -> directed V2_directed_crowd_g
      V3_directed_guitar_np     -> directed V2_directed_guitar_np
      V3_directed_guitar_cls    -> directed V2_directed_guitar_cls
      V3_directed_guitar_cam_pr -> directed V2_directed_guitar_cam
      V3_directed_guitar_cam_pt -> directed V2_directed_guitar_cam
      V3_directed_keys          -> directed V2_directed_crowd_b
      V3_directed_keys_cam      -> directed V2_directed_crowd_b
      V3_directed_keys_np       -> directed V2_directed_crowd_b
      V3_directed_duo_drums     -> directed V2_directed_drums
      V3_directed_duo_bass      -> directed V2_directed_duo_bass
      V3_directed_duo_guitar    -> directed V2_directed_duo_guitar
      V3_directed_duo_kv        -> directed V2_directed_duo_guitar
      V3_directed_duo_gb        -> directed V2_directed_duo_gb
      V3_directed_duo_kb        -> directed V2_directed_duo_gb
      V3_directed_duo_kg        -> directed V2_directed_duo_gb
  in vt
    { venueCameraRB3 = RTB.empty
    , venueCameraRB2 = RTB.merge (venueCameraRB2 vt) $ RTB.mapMaybe
      (\case Left x -> Just x; Right _ -> Nothing)
      camera
    , venueDirectedRB2 = RTB.merge (venueDirectedRB2 vt) $ RTB.mapMaybe
      (\case Left _ -> Nothing; Right x -> Just x)
      camera
    , venueSpotKeys = RTB.empty
    , venuePostProcessRB3 = RTB.empty
    , venuePostProcessRB2
      = RTB.merge (venuePostProcessRB2 vt)
      $ flip fmap (venuePostProcessRB3 vt)
      $ \case
        V3_ProFilm_a                    -> V2_Default
        V3_ProFilm_b                    -> V2_Default
        V3_video_a                      -> V2_video_a
        V3_film_16mm                    -> V2_film_16mm
        V3_shitty_tv                    -> V2_video_security
        V3_bloom                        -> V2_ProFilm_a
        V3_film_sepia_ink               -> V2_film_sepia_ink
        V3_film_silvertone              -> V2_film_silvertone
        V3_film_b_w                     -> V2_video_bw
        V3_video_bw                     -> V2_video_bw
        V3_contrast_a                   -> V2_contrast_a
        V3_photocopy                    -> V2_photocopy
        V3_film_blue_filter             -> V2_BlueTint
        V3_desat_blue                   -> V2_BlueTint
        V3_video_security               -> V2_video_security
        V3_bright                       -> V2_ProFilm_b
        V3_posterize                    -> V2_ProFilm_a
        V3_clean_trails                 -> V2_video_trails
        V3_video_trails                 -> V2_video_trails
        V3_flicker_trails               -> V2_video_trails
        V3_desat_posterize_trails       -> V2_film_16mm
        V3_film_contrast                -> V2_contrast_a
        V3_film_contrast_blue           -> V2_BlueTint
        V3_film_contrast_green          -> V2_contrast_a
        V3_film_contrast_red            -> V2_contrast_a
        V3_horror_movie_special         -> V2_photo_negative
        V3_photo_negative               -> V2_photo_negative
        V3_ProFilm_mirror_a             -> V2_ProFilm_mirror_a
        V3_ProFilm_psychedelic_blue_red -> V2_photo_negative
        V3_space_woosh                  -> V2_video_trails
    , venueLightingShared = flip fmap (venueLightingShared vt) $ \case
      Lighting_intro         -> Lighting_
      Lighting_blackout_spot -> Lighting_silhouettes_spot
      x                      -> x
    , venueLightingRB3 = RTB.empty
    , venueLightingRB2 = RTB.merge (venueLightingRB3 vt) (venueLightingRB2 vt)
    }
