{- |
VENUE track format that supports both pre-RB3 format and RBN2 format.
Pre-RB3 venue format information from:
<https://web.archive.org/web/20120109105129/http://creators.rockband.com:80/spec/Camera_And_Lights>
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
module Onyx.MIDI.Track.Venue where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (unless, (>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, sortBy, unsnoc)
import           Data.Maybe                       (isNothing, listToMaybe,
                                                   mapMaybe)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus1)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.StackTrace                  (SendMessage, StackTraceT,
                                                   warn)
import qualified Sound.MIDI.Util                  as U

#if MIN_VERSION_base(4,18,0)
-- liftA2 from Prelude
#else
import           Control.Applicative              (liftA2)
#endif

identifierToString :: (Show a) => T.Text -> (a -> Maybe T.Text) -> a -> T.Text
identifierToString prefix override x = case override x of
  Just y  -> y
  Nothing -> case T.stripPrefix prefix $ T.pack $ show x of
    Just y  -> y
    Nothing -> error $ unwords
      [ "panic! couldn't strip prefix"
      , show prefix
      , "from event"
      , show x
      ]

identifierTable :: (Enum a, Bounded a) => (a -> T.Text) -> HM.HashMap T.Text a
identifierTable f = HM.fromList [ (T.toCaseFold $ f x, x) | x <- each ]

class VenueIdentifier a where
  venueToString :: a -> T.Text
  venueTable :: HM.HashMap T.Text a

type Camera3 = Either CutNormal3 CutDirected3

data CutNormal3
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
  deriving (Eq, Ord, Show, Enum, Bounded)

instance VenueIdentifier CutNormal3 where
  venueToString = identifierToString "V3_" $ const Nothing
  venueTable = identifierTable venueToString

data CutDirected3
  = V3_directed_all
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

instance VenueIdentifier CutDirected3 where
  venueToString = identifierToString "V3_" $ const Nothing
  venueTable = identifierTable venueToString

data CutDirected2
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

instance VenueIdentifier CutDirected2 where
  venueToString = identifierToString "V2_" $ const Nothing
  venueTable = identifierTable venueToString

data DirectedCutStyle
  = Directed      -- rb3
  | DoDirectedCut -- pre-rb3, always runs
  | DoOptionalCut -- pre-rb3, runs if doing well
  deriving (Eq, Ord, Show)

instance Command (DirectedCutStyle, T.Text) where
  fromCommand (style, cut) = case style of
    Directed      -> [cut]
    DoDirectedCut -> ["do_directed_cut", cut]
    DoOptionalCut -> ["do_optional_cut", cut]
  toCommand = \case
    [cut] | "directed_" `T.isPrefixOf` cut -> Just (Directed     , cut)
    ["do_directed_cut", cut]               -> Just (DoDirectedCut, cut)
    ["do_optional_cut", cut]               -> Just (DoOptionalCut, cut)
    _                                      -> Nothing

anyNormalCut :: (Monad m, NNC.C t) => TrackEvent m t T.Text
anyNormalCut = commandMatch'
  isNormalCut
  (\cut -> [cut])

isNormalCut :: [T.Text] -> Maybe T.Text
isNormalCut = \case
  -- band_ is from rb4
  [cut] | any (`T.isPrefixOf` cut) ["coop_", "band_"] -> Just cut
  _                                                   -> Nothing

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

-- does not include .pp
instance VenueIdentifier PostProcess3 where
  venueToString = identifierToString "V3_" \case
    V3_film_b_w -> Just "film_b+w"
    _           -> Nothing
  venueTable = identifierTable venueToString

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

-- not normally text in rb2 midi but we use the docs names when converting
instance VenueIdentifier PostProcess2 where
  venueToString = identifierToString "V2_" $ const Nothing
  venueTable = identifierTable venueToString

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

data LightingRB
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

instance VenueIdentifier LightingRB where
  venueToString = identifierToString "Lighting_" $ const Nothing
  venueTable = identifierTable venueToString

data LightingMode2 = ModeVerse | ModeChorus
  deriving (Eq, Ord, Show, Enum, Bounded)

data VenueFormat = RBN1 | RBN2
  deriving (Eq, Ord, Show, Enum, Bounded)

data LightingCommand
  = LightingFirst
  | LightingPrev
  | LightingNext
  deriving (Eq, Ord, Show, Enum, Bounded)

data VisualResult a = VisualResult
  { match     :: Maybe a -- perfect match
  , fuzzy     :: [a] -- sort of matches
  , fallback1 :: a -- generic replacements
  , fallback2 :: a
  , fallback3 :: a
  , original  :: T.Text
  } deriving (Functor)

matchLightingRB :: T.Text -> VisualResult LightingRB
matchLightingRB t = let
  folded = T.toCaseFold t
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just lrb -> (Just lrb, [])
    Nothing  -> (Nothing,) case folded of
      -- GH2 other than () and (sweep)
      "blackout" -> [Lighting_blackout_fast, Lighting_blackout_slow]
      "chase"    -> [Lighting_searchlights] -- ?
      "flare"    -> [Lighting_flare_fast, Lighting_flare_slow]
      "strobe"   -> [Lighting_strobe_fast, Lighting_strobe_slow]
      "color1"   -> [Lighting_manual_cool]
      "color2"   -> [Lighting_manual_warm]
      _          -> []
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = Lighting_manual_cool
    , fallback2 = Lighting_manual_warm
    , fallback3 = Lighting_dischord
    , original  = t
    }

selectSimpleVisual :: (NNC.C t, SendMessage m, Eq a) => RTB.T t (VisualResult a) -> StackTraceT m (RTB.T t a)
selectSimpleVisual input = do

  let unknown = nubOrd $ map (.original) $ filter (isNothing . (.match)) $ RTB.getBodies input
  unless (null unknown) $ warn $ T.unpack $ "Substituting replacement events for: " <> T.intercalate ", " unknown

  let perTime = RTB.collectCoincident input
  unless (all (null . drop 1) perTime) $ warn "Simultaneous events found, dropping all but one per timestamp"

  return $ RTB.flatten $ perTime <&> \case
    result : _ -> return $ case toList result.match <> result.fuzzy of
      x : _ -> x
      []    -> result.fallback1
    [] -> []

selectFadingVisual :: (NNC.C t, SendMessage m, Eq a) => RTB.T t (VisualResult a) -> StackTraceT m (RTB.T t a)
selectFadingVisual input = do

  let unknown = nubOrd $ map (.original) $ filter (isNothing . (.match)) $ RTB.getBodies input
  unless (null unknown) $ warn $ T.unpack $ "Substituting replacement events for: " <> T.intercalate ", " unknown

  let perTime = RTB.collectCoincident input
  unless (all (null . drop 1) perTime) $ warn "Simultaneous events found, dropping all but one per timestamp"

  return $ selectFadingVisualPure input

selectFadingVisualPure :: (NNC.C t, Eq a) => RTB.T t (VisualResult a) -> RTB.T t a
selectFadingVisualPure input = let

  perTime = RTB.collectCoincident input
  noStacks = RTB.flatten $ take 1 <$> perTime

  go :: (Eq a) => Maybe (T.Text, a) -> RTB.T t (VisualResult a) -> RTB.T t a
  go prev = \case
    RNil            -> RNil
    Wait dt vr rest -> let
      nextResult = listToMaybe $ dropWhile (\r -> vr.original == r.original) $ RTB.getBodies rest
      this = case vr.match of
        -- is it an exact match
        Just match -> match
        Nothing    -> case prev of
          -- does this continue the previous setting
          Just (prevText, x) | prevText == vr.original -> x
          _ -> let
            -- fuzzy options need to be different from previous selection + next exact match
            isGoodFuzzy x = and
              [ maybe True (\(_, y) -> x /= y) prev
              , case nextResult of
                Nothing -> True
                Just nr -> maybe True (x /=) nr.match
              ]
            in case filter isGoodFuzzy vr.fuzzy of
              x : _ -> x
              []    -> let
                -- fallback options need to be different from previous selection + next exact/fuzzy match (only first one)
                isGoodFallback x = and
                  [ maybe True (\(_, y) -> x /= y) prev
                  , case nextResult of
                    Nothing -> True
                    Just nr -> maybe True (x /=) $ nr.match <|> listToMaybe nr.fuzzy
                  ]
                in if isGoodFallback vr.fallback1
                  then vr.fallback1
                  else if isGoodFallback vr.fallback2
                    then vr.fallback2
                    else vr.fallback3
      in Wait dt this $ go (Just (vr.original, this)) rest

  in go Nothing noStacks

data BlendableEvent a
  = BlendableCut    a
  | BlendableFade a a

computeBlendable :: (NNC.C t, Eq a) => RTB.T t a -> RTB.T t (BlendableEvent a)
computeBlendable = \case
  Wait t1 x1 (Wait t2 x2 (Wait t3 x3 rest))
    | x1 == x2 && x1 == x3
    -> computeBlendable $ Wait t1 x1 $ Wait (t2 <> t3) x2 rest
    | x1 == x2
    -> Wait t1 (BlendableCut x1) $ Wait t2 (BlendableFade x2 x3) $ computeBlendable $ Wait t3 x3 rest
  Wait t1 x1 rest
    -> Wait t1 (BlendableCut x1) $ computeBlendable rest
  RNil -> RNil

-- Converts from pre-RB3 [verse]/[chorus] to instead replace [lighting ()] presets
lightingModeToPresets :: (NNC.C t) => RTB.T t LightingMode2 -> RTB.T t T.Text -> RTB.T t T.Text
lightingModeToPresets modes lights = if RTB.null modes
  then lights <&> \case
    "" -> "verse"
    x  -> x
  else applyStatus1 ModeVerse modes lights <&> \case
    -- this could remove blending if adjacent () become different, not sure of best solution
    (ModeVerse , "") -> "verse"
    (ModeChorus, "") -> "chorus"
    (_         , x ) -> x

bakeLightingModeRB2 :: (NNC.C t) => RTB.T t LightingMode2 -> RTB.T t LightingRB -> (RTB.T t LightingMode2, RTB.T t LightingRB)
bakeLightingModeRB2 modes lights = if RTB.null modes && RTB.null lights
  then (modes, lights) -- don't touch empty input
  else let
    -- this could add unintended blending! should be smarter
    toParens = \case
      Lighting_verse  -> Lighting_
      Lighting_chorus -> Lighting_
      x               -> x
    newLights = toParens <$> lights
    newModes = if RTB.null modes
      then flip RTB.mapMaybe lights \case
        Lighting_verse  -> Just ModeVerse
        Lighting_chorus -> Just ModeChorus
        _               -> Nothing
      -- assume there should never be (verse)/(chorus) and [verse]/[chorus] in same file
      else modes
    -- magma v1 needs to have a mode at time 0, so add [verse] if needed
    withStartMode = case newModes of
      Wait t _ _ | t == NNC.zero -> newModes
      _                          -> Wait NNC.zero ModeVerse newModes
    -- replace (intro) and (blackout_spot); pick replacements to not add blending
    noRB3Lights = selectFadingVisualPure $ newLights <&> \x -> case x of
      Lighting_intro -> VisualResult
        { match     = Nothing
        , fuzzy     = []
        , fallback1 = Lighting_manual_cool
        , fallback2 = Lighting_manual_warm
        , fallback3 = Lighting_dischord
        , original  = venueToString x
        }
      Lighting_blackout_spot -> VisualResult
        { match     = Nothing
        , fuzzy     = []
        , fallback1 = Lighting_blackout_fast
        -- these aren't great but hopefully never happen
        , fallback2 = Lighting_blackout_slow
        , fallback3 = Lighting_sweep
        , original  = venueToString x
        }
      _ -> VisualResult
        { match     = Just x
        , fuzzy     = []
        , fallback1 = Lighting_manual_cool
        , fallback2 = Lighting_manual_warm
        , fallback3 = Lighting_dischord
        , original  = venueToString x
        }
    in (withStartMode, noRB3Lights)

compileLightingRB3 :: (NNC.C t, SendMessage m) => RTB.T t LightingMode2 -> RTB.T t T.Text -> StackTraceT m (RTB.T t T.Text)
compileLightingRB3 modes lights = do
  results <- selectFadingVisual $ matchLightingRB <$> lights
  return $ lightingModeToPresets modes $ venueToString <$> results

compileLightingRB2 :: (NNC.C t, SendMessage m) => RTB.T t LightingMode2 -> RTB.T t T.Text -> StackTraceT m (RTB.T t LightingMode2, RTB.T t T.Text)
compileLightingRB2 modes lights = do
  results <- selectFadingVisual $ matchLightingRB <$> lights
  let (modes', lights') = bakeLightingModeRB2 modes results
  return (modes', venueToString <$> lights')

anyLighting :: (Monad m, NNC.C t) => TrackEvent m t T.Text
anyLighting = commandMatch'
  isLighting
  (\s -> ["lighting", "(" <> s <> ")"])

isLighting :: [T.Text] -> Maybe T.Text
isLighting = let
  stripParens = T.stripPrefix "(" >=> T.stripSuffix ")"
  in \case
    ["lighting", s] -> stripParens s
    [s]             -> T.stripPrefix "lighting" s >>= stripParens
    _               -> Nothing

anyPostProc :: (Monad m, NNC.C t) => TrackEvent m t T.Text
anyPostProc = commandMatch'
  isPostProc
  (\s -> [s <> ".pp"])

isPostProc :: [T.Text] -> Maybe T.Text
isPostProc xs = case unsnoc xs of
  -- support multiple words as long as last one ends in .pp
  Just (ys, T.stripSuffix ".pp" -> Just y) -> Just $ T.replace " " "_" $ T.unwords $ ys <> [y]
  _                                        -> Nothing

matchPostProcRB3 :: T.Text -> VisualResult PostProcess3
matchPostProcRB3 t = let
  folded = T.toCaseFold t
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just pp -> (Just pp, [])
    Nothing -> (Nothing,) case folded of
      -- RB2, all others match an RB3 name
      "default"             -> [V3_ProFilm_a] -- rbn2 docs say ProFilm_a is "default"?
      "bluetint"            -> [V3_film_blue_filter]
      -- RB4
      "b&w"                 -> [V3_film_b_w]
      "blue_bright"         -> [V3_film_blue_filter]
      "cool"                -> [V3_ProFilm_b]
      "high_contrast_cool"  -> [V3_posterize]
      "high_contrast_dark"  -> [V3_photocopy]
      "high_contrast_warm"  -> [V3_film_contrast]
      "low_contrast_green"  -> [V3_film_contrast_green]
      "low_contrast_purple" -> [V3_desat_blue]
      "low_contrast_red"    -> [V3_film_contrast_red]
      "orange_bright"       -> [V3_film_contrast_red]
      "pink_bright"         -> [V3_film_contrast_red]
      "purple_bright"       -> [V3_film_contrast_blue]
      "sepia_bright"        -> [V3_film_sepia_ink]
      "sepia_dark"          -> [V3_film_sepia_ink]
      "video"               -> [V3_video_a]
      "warm"                -> [V3_posterize]
      "yellow_bright"       -> [V3_film_contrast_green]
      "<null>"              -> [V3_ProFilm_a] -- ??? seen in neverenough.rbsong
      _                     -> []
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = V3_ProFilm_a
    , fallback2 = V3_ProFilm_b
    , fallback3 = V3_video_a
    , original  = t
    }

compilePostProcRB3 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (RTB.T t PostProcess3)
compilePostProcRB3 vt = selectFadingVisual $ fmap matchPostProcRB3 $ RTB.merge
  vt.postProcessRB3
  -- should we remove redundant events to prevent blending from RB2 postprocs?
  (venueToString <$> vt.postProcessRB2)

matchPostProcRB2 :: T.Text -> VisualResult PostProcess2
matchPostProcRB2 t = let
  folded = T.toCaseFold t
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just pp -> (Just pp, [])
    Nothing -> (Nothing,) let
      -- just match to RB3 first, to handle RB4 also
      rb3Result = matchPostProcRB3 folded
      in case toList rb3Result.match <> rb3Result.fuzzy of
        x : _ -> case x of
          -- only ones that didn't already match an RB2 name
          V3_shitty_tv                    -> [V2_video_security]
          V3_bloom                        -> [V2_ProFilm_a]
          V3_film_b_w                     -> [V2_video_bw]
          V3_film_blue_filter             -> [V2_BlueTint]
          V3_desat_blue                   -> [V2_BlueTint]
          V3_bright                       -> [V2_ProFilm_b]
          V3_posterize                    -> [V2_ProFilm_a]
          V3_clean_trails                 -> [V2_video_trails]
          V3_flicker_trails               -> [V2_video_trails]
          V3_desat_posterize_trails       -> [V2_film_16mm]
          V3_film_contrast                -> [V2_contrast_a]
          V3_film_contrast_blue           -> [V2_BlueTint]
          V3_film_contrast_green          -> [V2_contrast_a]
          V3_film_contrast_red            -> [V2_contrast_a]
          V3_horror_movie_special         -> [V2_photo_negative]
          V3_ProFilm_psychedelic_blue_red -> [V2_photo_negative]
          V3_space_woosh                  -> [V2_video_trails]
          _                               -> []
        _ -> []
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = V2_ProFilm_a
    , fallback2 = V2_ProFilm_b
    , fallback3 = V2_video_a
    , original  = t
    }

compilePostProcRB2 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (RTB.T t PostProcess2)
compilePostProcRB2 vt = if RTB.null vt.postProcessRB3
  then return vt.postProcessRB2
  else selectFadingVisual $ fmap matchPostProcRB2 $ RTB.merge
    vt.postProcessRB3
    (venueToString <$> vt.postProcessRB2)

data VenueTrack t = VenueTrack

  { cameraRB3        :: RTB.T t T.Text -- undirected cuts
  , cameraRB2        :: RTB.T t CutEvent2
  , directed         :: RTB.T t (DirectedCutStyle, T.Text)

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

  , postProcessRB3   :: RTB.T t T.Text
  , postProcessRB2   :: RTB.T t PostProcess2

  , lighting         :: RTB.T t T.Text
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
    , directed         = U.trackTake  t o.directed
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
    , directed         = U.trackDrop  t o.directed
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
    cameraRB3 <- (.cameraRB3) =. anyNormalCut
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
    directed        <- (.directed       ) =. command
    singGuitar      <- (.singGuitar     ) =. edges 87
    singDrums       <- (.singDrums      ) =. edges 86
    singBass        <- (.singBass       ) =. edges 85
    spotlightKeys   <- (.spotlightKeys  ) =. edges 41
    spotlightVocal  <- (.spotlightVocal ) =. edges 40
    spotlightGuitar <- (.spotlightGuitar) =. edges 39
    spotlightDrums  <- (.spotlightDrums ) =. edges 38 -- RBN2 docs incorrectly say this is bass
    spotlightBass   <- (.spotlightBass  ) =. edges 37 -- RBN2 docs incorrectly say this is drums
    postProcessRB3  <- (.postProcessRB3 ) =. anyPostProc
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
    lighting <- (.lighting) =. anyLighting
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

matchNormalCutRB3 :: T.Text -> VisualResult CutNormal3
matchNormalCutRB3 t = let
  foldedOrig = T.toCaseFold t
  -- band_ prefix used in RB4 rbsong
  folded = case T.stripPrefix "band_" foldedOrig of
    Nothing        -> foldedOrig
    Just minusBand -> "coop_" <> minusBand
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just cut -> (Just cut, [])
    Nothing  -> (Nothing,) case folded of
      -- rest is all RB4 matching
      "coop_dv_behind"   -> [V3_coop_dv_near]
      "coop_bd_behind"   -> [V3_coop_bd_near]
      "coop_dg_behind"   -> [V3_coop_dg_near]
      "coop_d_near_head" -> [V3_coop_d_closeup_head]
      "coop_g_near_head" -> [V3_coop_g_closeup_head]
      "coop_v_near_head" -> [V3_coop_v_closeup]
      "coop_b_near_head" -> [V3_coop_b_closeup_head]
      _                  -> case T.stripSuffix "_crowd" folded of
        Nothing         -> []
        Just minusCrowd -> let
          withBehind = matchNormalCutRB3 $ minusCrowd <> "_behind"
          in toList withBehind.match <> withBehind.fuzzy
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = V3_coop_all_near
    , fallback2 = V3_coop_all_near
    , fallback3 = V3_coop_all_near
    , original  = t
    }

compileNormalRB3 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (RTB.T t T.Text)
compileNormalRB3 vt
  = fmap (fmap venueToString)
  $ selectSimpleVisual
  $ matchNormalCutRB3 <$> vt.cameraRB3

matchDirectedRB3 :: T.Text -> VisualResult CutDirected3
matchDirectedRB3 t = let
  folded = T.toCaseFold t
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just cut -> (Just cut, [])
    Nothing  -> (Nothing,) case folded of
      -- RB2 tweaks
      "directed_guitar_cam" -> [V3_directed_guitar_cam_pt]
      "directed_vocals_cam" -> [V3_directed_vocals_cam_pt]
      _                     -> []
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = V3_directed_all
    , fallback2 = V3_directed_all
    , fallback3 = V3_directed_all
    , original  = t
    }

compileDirectedRB3 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (RTB.T t (DirectedCutStyle, T.Text))
compileDirectedRB3 vt
  = fmap (fmap \cut -> (Directed, venueToString cut))
  $ selectSimpleVisual
  $ vt.directed <&> \(_, x) -> matchDirectedRB3 x

compileVenueRB3 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (VenueTrack t)
compileVenueRB3 vt = do
  lighting <- compileLightingRB3 vt.lightingMode vt.lighting
  postProc <- fmap venueToString <$> compilePostProcRB3 vt
  normal <- compileNormalRB3 vt
  directed <- compileDirectedRB3 vt
  return vt
    { cameraRB3 = let
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
      in RTB.merge normal $ venueToString <$> generic
    , cameraRB2 = RTB.empty
    , directed = directed
    , postProcessRB3 = postProc
    , postProcessRB2 = RTB.empty
    , lighting = lighting
    , lightingCommands = (\(cmd, _) -> (cmd, RBN2)) <$> vt.lightingCommands
    , lightingMode = RTB.empty
    , fog = RTB.empty
    }

matchDirectedRB2 :: T.Text -> VisualResult CutDirected2
matchDirectedRB2 t = let
  folded = T.toCaseFold t
  (match, fuzzy) = case HM.lookup folded venueTable of
    Just cut -> (Just cut, [])
    Nothing  -> (Nothing,) case folded of
      -- RB3 missing
      "directed_vocals_cam_pr" -> [V2_directed_vocals_cam]
      "directed_vocals_cam_pt" -> [V2_directed_vocals_cam]
      "directed_guitar_cam_pr" -> [V2_directed_guitar_cam]
      "directed_guitar_cam_pt" -> [V2_directed_guitar_cam]
      "directed_keys"          -> [V2_directed_crowd_b]
      "directed_keys_cam"      -> [V2_directed_crowd_b]
      "directed_keys_np"       -> [V2_directed_crowd_b]
      "directed_duo_drums"     -> [V2_directed_drums]
      "directed_duo_kv"        -> [V2_directed_duo_guitar]
      "directed_duo_kb"        -> [V2_directed_duo_gb]
      "directed_duo_kg"        -> [V2_directed_duo_gb]
      _                        -> []
  in VisualResult
    { match     = match
    , fuzzy     = fuzzy
    , fallback1 = V2_directed_all
    , fallback2 = V2_directed_all
    , fallback3 = V2_directed_all
    , original  = t
    }

compileDirectedRB2 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (RTB.T t (DirectedCutStyle, T.Text))
compileDirectedRB2 vt
  = selectSimpleVisual
  $ vt.directed <&> \(style, x) -> let
    newStyle = case style of
      Directed -> DoDirectedCut
      _        -> style -- already one of the rb2 styles
    in matchDirectedRB2 x <&> \cut -> (newStyle, venueToString cut)

compileVenueRB2 :: (NNC.C t, SendMessage m) => VenueTrack t -> StackTraceT m (VenueTrack t)
compileVenueRB2 vt | vt == mempty = return vt -- don't add anything (like [verse]) to an empty input
compileVenueRB2 vt = let
  cut dist insts = map Left $ CameraCut : (dist ++ insts)
  behind = cut [NoClose]
  close = cut [NoBehind, OnlyClose]
  far = cut [NoBehind, OnlyFar, NoClose]
  near = cut [NoBehind, NoClose]
  camera = RTB.flatten $ RTB.collectCoincident vt.cameraRB3 <&> \cams -> let
    rb3 = mapMaybe (`HM.lookup` venueTable) cams
    in case sortBy (flip compare) rb3 of
      []    -> cut [] []
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
  in do
    (modes, lighting) <- compileLightingRB2 vt.lightingMode vt.lighting
    postProc <- compilePostProcRB2 vt
    directed <- compileDirectedRB2 vt
    return vt
      { cameraRB3 = RTB.empty
      , cameraRB2 = RTB.merge vt.cameraRB2 $ RTB.mapMaybe
        (\case Left x -> Just x; Right _ -> Nothing)
        camera
      , directed = directed
      , spotlightKeys = RTB.empty
      , postProcessRB3 = RTB.empty
      , postProcessRB2 = postProc
      , lighting = lighting
      , lightingCommands
        = fmap (\cmd -> (cmd, RBN1))
        -- avoid problems with two simultaneous [next] causing overlapping midi notes
        $ RTB.flatten . fmap nubOrd . RTB.collectCoincident
        $ fst <$> vt.lightingCommands
      , lightingMode = modes
      }
