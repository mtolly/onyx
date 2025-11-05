{- |
VENUE track format that supports both pre-RB3 format and RBN2 format.
Pre-RB3 venue format information from:
<https://web.archive.org/web/20120109105129/http://creators.rockband.com:80/spec/Camera_And_Lights>
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.MIDI.Track.Venue where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     ((<&>))
import           Data.List.Extra                  (nubOrd, sortBy)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus1)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Sound.MIDI.Util                  as U

#if MIN_VERSION_base(4,18,0)
-- liftA2 from Prelude
#else
import           Control.Applicative              (liftA2)
#endif

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
  deriving (Eq, Ord, Show, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command Camera2 where
  fromCommand x = case T.stripPrefix "V2_" $ T.pack $ show x of
    Just s  -> [s]
    Nothing -> error "panic! couldn't strip V2_ from venue event"
  toCommand = reverseLookup each fromCommand

data DoCut2
  = DoDirectedCut Camera2
  | DoOptionalCut Camera2
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command PostProcess3 where
  fromCommand V3_film_b_w = ["film_b+w.pp"]
  fromCommand x           = case T.stripPrefix "V3_" $ T.pack $ show x of
    Just s  -> [s <> ".pp"]
    Nothing -> error "panic! couldn't strip V3_ from venue event"
  toCommand = reverseLookupCI each fromCommand
  -- case insensitive since RB4 .rbsong just has them all lowercase

data PostProcess2
  = V2_video_trails
  | V2_video_security
  | V2_video_bw
  | V2_video_a
  | V2_BlueTint -- ^ dunno what this does in rb3, does not correspond to rbn2 text event
  | V2_ProFilm_mirror_a
  | V2_ProFilm_b
  | V2_ProFilm_a
  | V2_photocopy
  | V2_photo_negative
  | V2_film_silvertone
  | V2_film_sepia_ink
  | V2_film_16mm
  | V2_contrast_a
  | V2_Default -- ^ rbn2 docs say ProFilm_a is @"default"@. apparently in rb2 they were different?
  deriving (Eq, Ord, Show, Enum, Bounded)

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
  deriving (Eq, Ord, Show, Enum, Bounded)

data Lighting
  -- manual
  = Lighting_ -- ^ empty parens, only pre-rb3
  | Lighting_verse -- ^ only in rb3 (rb2 uses different format)
  | Lighting_chorus -- ^ only in rb3 (rb2 uses different format)
  | Lighting_intro -- ^ new in rb3
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

data LightingMode2 = ModeVerse | ModeChorus
  deriving (Eq, Ord, Show, Enum, Bounded)

data VenueFormat = RBN1 | RBN2
  deriving (Eq, Ord, Show, Enum, Bounded)

data LightingCommand
  = LightingFirst
  | LightingPrev
  | LightingNext
  deriving (Eq, Ord, Show, Enum, Bounded)

isLighting :: [T.Text] -> Maybe T.Text
isLighting = let
  stripParens = T.stripPrefix "(" >=> T.stripSuffix ")"
  in \case
    ["lighting", s] -> stripParens s
    [s]             -> T.stripPrefix "lighting" s >>= stripParens
    _               -> Nothing

specificLighting :: (Monad m, NNC.C t) => T.Text -> TrackEvent m t ()
specificLighting s = commandMatch'
  (isLighting >=> guard . (== s))
  (\() -> ["lighting", "(" <> s <> ")"])

data VenueTrack t = VenueTrack

  { cameraRB3        :: RTB.T t Camera3
  , cameraRB2        :: RTB.T t CutEvent2
  , directedRB2      :: RTB.T t DoCut2

  , singGuitar       :: RTB.T t Bool -- or keys if no guitar
  , singDrums        :: RTB.T t Bool
  , singBass         :: RTB.T t Bool -- or keys if no bass
  -- these are actually "part 2" "part 4" "part 3" respectively,
  -- as they can be redirected by BandSongPref

  , spotlightKeys    :: RTB.T t Bool -- rb3 only of course
  , spotlightVocal   :: RTB.T t Bool
  , spotlightGuitar  :: RTB.T t Bool
  , spotlightDrums   :: RTB.T t Bool
  , spotlightBass    :: RTB.T t Bool

  , postProcessRB3   :: RTB.T t PostProcess3
  , postProcessRB2   :: RTB.T t PostProcess2

  , lighting         :: RTB.T t Lighting
  , lightingCommands :: RTB.T t (LightingCommand, VenueFormat)
  , lightingMode     :: RTB.T t LightingMode2

  , bonusFX          :: RTB.T t ()
  , bonusFXOptional  :: RTB.T t ()

  , fog              :: RTB.T t Bool

  -- TODO [crowd_lighters_on] / [crowd_lighters_off]
  -- and [crowd_horns_on] / [crowd_horns_off]
  -- seen in Lane Breakers

  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (VenueTrack t)

instance ChopTrack VenueTrack where
  chopTake t o = VenueTrack
    { cameraRB3        = U.trackTake  t o.cameraRB3
    , cameraRB2        = U.trackTake  t o.cameraRB2
    , directedRB2      = U.trackTake  t o.directedRB2
    , singGuitar       = chopTakeBool t o.singGuitar
    , singDrums        = chopTakeBool t o.singDrums
    , singBass         = chopTakeBool t o.singBass
    , spotlightKeys    = chopTakeBool t o.spotlightKeys
    , spotlightVocal   = chopTakeBool t o.spotlightVocal
    , spotlightGuitar  = chopTakeBool t o.spotlightGuitar
    , spotlightDrums   = chopTakeBool t o.spotlightDrums
    , spotlightBass    = chopTakeBool t o.spotlightBass
    , postProcessRB3   = U.trackTake  t o.postProcessRB3
    , postProcessRB2   = U.trackTake  t o.postProcessRB2
    , lighting         = U.trackTake  t o.lighting
    , lightingCommands = U.trackTake  t o.lightingCommands
    , lightingMode     = U.trackTake  t o.lightingMode
    , bonusFX          = U.trackTake  t o.bonusFX
    , bonusFXOptional  = U.trackTake  t o.bonusFXOptional
    , fog              = U.trackTake  t o.fog
    }
  chopDrop t o = VenueTrack
    { cameraRB3        = U.trackDrop  t o.cameraRB3
    , cameraRB2        = U.trackDrop  t o.cameraRB2
    , directedRB2      = U.trackDrop  t o.directedRB2
    , singGuitar       = chopDropBool t o.singGuitar
    , singDrums        = chopDropBool t o.singDrums
    , singBass         = chopDropBool t o.singBass
    , spotlightKeys    = chopDropBool t o.spotlightKeys
    , spotlightVocal   = chopDropBool t o.spotlightVocal
    , spotlightGuitar  = chopDropBool t o.spotlightGuitar
    , spotlightDrums   = chopDropBool t o.spotlightDrums
    , spotlightBass    = chopDropBool t o.spotlightBass
    , postProcessRB3   = U.trackDrop  t o.postProcessRB3
    , postProcessRB2   = U.trackDrop  t o.postProcessRB2
    , lighting         = U.trackDrop  t o.lighting
    , lightingCommands = U.trackDrop  t o.lightingCommands
    , lightingMode     = U.trackDrop  t o.lightingMode
    , bonusFX          = U.trackDrop  t o.bonusFX
    , bonusFXOptional  = U.trackDrop  t o.bonusFXOptional
    , fog              = U.trackDrop  t o.fog
    }

instance TraverseTrack VenueTrack where
  traverseTrack fn (VenueTrack a b c d e f g h i j k l m n o p q r s) = VenueTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k <*> fn l
    <*> fn m <*> fn n <*> fn o <*> fn p <*> fn q <*> fn r
    <*> fn s

instance ParseTrack VenueTrack where
  parseTrack = do
    cameraRB3 <- (.cameraRB3) =. command
    cameraRB2 <- (.cameraRB2) =. do
      condenseMap_ $ fatBlips (1/8) $ eachKey each $ blip . \case
        CameraCut   -> 60
        FocusVocal  -> 64
        FocusGuitar -> 63
        FocusDrums  -> 62
        FocusBass   -> 61
        NoClose     -> 73
        OnlyClose   -> 72
        OnlyFar     -> 71
        NoBehind    -> 70
    directedRB2     <- (.directedRB2    ) =. command
    singGuitar      <- (.singGuitar     ) =. edges 87
    singDrums       <- (.singDrums      ) =. edges 86
    singBass        <- (.singBass       ) =. edges 85
    spotlightKeys   <- (.spotlightKeys  ) =. edges 41
    spotlightVocal  <- (.spotlightVocal ) =. edges 40
    spotlightGuitar <- (.spotlightGuitar) =. edges 39
    spotlightDrums  <- (.spotlightDrums ) =. edges 38 -- RBN2 docs incorrectly say this is bass
    spotlightBass   <- (.spotlightBass  ) =. edges 37 -- RBN2 docs incorrectly say this is drums
    postProcessRB3  <- (.postProcessRB3 ) =. command
    postProcessRB2  <- (.postProcessRB2 ) =. do
      condenseMap_ $ fatBlips (1/8) $ eachKey each $ blip . \case
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
    lighting <- (.lighting) =. do
      condenseMap_ $ eachKey each \light ->
        specificLighting case T.stripPrefix "Lighting_" $ T.pack $ show light of
          Just s  -> s
          Nothing -> error $ "panic! couldn't strip Lighting_ from: " ++ show light
    lightingCommands <- (.lightingCommands) =. do
      condenseMap_ $ fatBlips (1/8) $ eachKey (liftA2 (,) each each) \case
        (LightingFirst, RBN2) -> commandMatch ["first"]
        (LightingPrev , RBN2) -> commandMatch ["prev"]
        (LightingNext , RBN2) -> commandMatch ["next"]
        (LightingFirst, RBN1) -> blip 50
        (LightingPrev , RBN1) -> blip 49
        (LightingNext , RBN1) -> blip 48
    lightingMode <- (.lightingMode) =. do
      condenseMap_ $ eachKey each \case
        ModeVerse  -> commandMatch ["verse"]
        ModeChorus -> commandMatch ["chorus"]
    bonusFX         <- (.bonusFX        ) =. commandMatch ["bonusfx"]
    bonusFXOptional <- (.bonusFXOptional) =. commandMatch ["bonusfx_optional"]
    fog <- (.fog) =. do
      condenseMap_ $ eachKey each $ commandMatch . \case
        True  -> ["FogOn" ]
        False -> ["FogOff"]
    return VenueTrack{..}

compileVenueRB3 :: (NNC.C t) => VenueTrack t -> VenueTrack t
compileVenueRB3 vt = vt
  { cameraRB3 = let
    directed = vt.directedRB2 <&> \docut -> let
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
    generic = RTB.flatten $ fmap genericInstant $ RTB.collectCoincident vt.cameraRB2
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
          (True, False, False, True) ->
            [V3_coop_dv_near | near]
          (False, True, False, True) ->
            [V3_coop_dg_near | near]
          (False, False, True, True) ->
            [V3_coop_bd_near | near]
          -- all, nothing, or invalid 3-char including drums
          _ -> concat
            [ [V3_coop_all_behind | behind]
            , [V3_coop_all_far | far]
            , [V3_coop_all_near | near]
            ]
        in case cuts of
          [] -> [V3_coop_all_near] -- some default if we cut but excluded all options
          _  -> cuts
    in RTB.merge vt.cameraRB3 $ RTB.merge generic directed
  , cameraRB2 = RTB.empty
  , directedRB2 = RTB.empty
  , postProcessRB3
    = RTB.merge vt.postProcessRB3
    $ vt.postProcessRB2 <&> \case
      -- TODO do we need to remove redundant events to prevent blending from an RB2 source
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
      -- not in rbn2 list, but rbn2 docs say ProFilm_a is "default"
  , postProcessRB2 = RTB.empty
  , lighting = let
    -- in RB2, [verse]/[chorus] are a separate mode which affect any lighting setting.
    -- the docs say they most noticeably affect [lighting ()].
    -- RB3 just replaces it with [lighting (verse)]/[lighting (chorus)].
    withMode = applyStatus1 ModeVerse vt.lightingMode vt.lighting
    noBlend = if RTB.null vt.lightingMode then id else noRedundantStatus
    in noBlend $ withMode <&> \case
      (ModeVerse , Lighting_) -> Lighting_verse
      (ModeChorus, Lighting_) -> Lighting_chorus
      (_         , light    ) -> light
  , lightingCommands = (\(cmd, _) -> (cmd, RBN2)) <$> vt.lightingCommands
  , lightingMode = RTB.empty
  , fog = RTB.empty
  }

compileVenueRB2 :: (NNC.C t) => VenueTrack t -> VenueTrack t
compileVenueRB2 vt | vt == mempty = vt -- don't add anything (like [verse]) to an empty input
compileVenueRB2 vt = let
  cut dist insts = map Left $ CameraCut : (dist ++ insts)
  behind = cut [NoClose]
  close = cut [NoBehind, OnlyClose]
  far = cut [NoBehind, OnlyFar, NoClose]
  near = cut [NoBehind, NoClose]
  directed x = [Right $ DoDirectedCut x]
  camera = RTB.flatten $ RTB.collectCoincident vt.cameraRB3 <&> \cams -> case sortBy (flip compare) cams of
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
    { cameraRB3 = RTB.empty
    , cameraRB2 = RTB.merge vt.cameraRB2 $ RTB.mapMaybe
      (\case Left x -> Just x; Right _ -> Nothing)
      camera
    , directedRB2 = RTB.merge vt.directedRB2 $ RTB.mapMaybe
      (\case Left _ -> Nothing; Right x -> Just x)
      camera
    , spotlightKeys = RTB.empty
    , postProcessRB3 = RTB.empty
    , postProcessRB2
      = RTB.merge vt.postProcessRB2
      $ vt.postProcessRB3 <&> \case
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
    , lighting = noRedundantStatus $ fmap snd lightingWithMode'
    , lightingCommands
      = fmap (\cmd -> (cmd, RBN1))
      -- avoid problems with two simultaneous [next] causing overlapping midi notes
      $ RTB.flatten . fmap nubOrd . RTB.collectCoincident
      $ fst <$> vt.lightingCommands
    , lightingMode = noRedundantStatus $
      RTB.cons NNC.zero ModeVerse $ fmap fst lightingWithMode'
      -- have to have [verse] at the start
    } where
      lightingWithMode = applyStatus1 ModeVerse vt.lightingMode vt.lighting
      lightingWithMode' = lightingWithMode <&> \case
        (_   , Lighting_verse        ) -> (ModeVerse , Lighting_                )
        (_   , Lighting_chorus       ) -> (ModeChorus, Lighting_                )
        (mode, Lighting_intro        ) -> (mode      , Lighting_                )
        (mode, Lighting_blackout_spot) -> (mode      , Lighting_silhouettes_spot)
        x                              -> x
