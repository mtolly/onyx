{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module RockBand2 (convertMIDI, dryVoxAudio) where

import           Config                           (Instrument (..))
import           Control.Monad                    (guard)
import           Data.Conduit.Audio               (AudioSource)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (inits, nub, sort, sortBy,
                                                   tails)
import           Data.Maybe                       (isNothing, listToMaybe,
                                                   mapMaybe)
import qualified Data.Set                         as Set
import           DryVox                           (sineDryVox)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), joinEdges,
                                                   splitEdges)
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as F
import qualified RockBand.FiveButton              as Five
import           RockBand.PhaseShiftMessage       (discardPS)
import qualified RockBand.Venue                   as V
import qualified RockBand.VenueRB2                as V2
import qualified RockBand.Vocals                  as Vox
import           Scripts                          (trackGlue)
import qualified Sound.MIDI.Util                  as U

dryVoxAudio :: (Monad m) => F.Song (F.OnyxFile U.Beats) -> AudioSource m Float
dryVoxAudio f = sineDryVox $ U.applyTempoTrack (F.s_tempos f)
  $ discardPS $ F.flexPartVocals $ F.getFlexPart F.FlexVocal $ F.s_tracks f

-- | Removes OD phrases to ensures that no phrases overlap on different tracks,
-- except for precisely matching unison phrases on all tracks.
fixOverdrive :: (NNC.C t) => [RTB.T t Bool] -> [RTB.T t Bool]
fixOverdrive [] = []
fixOverdrive tracks = let
  go trks = case sort $ mapMaybe (fmap fst . RTB.viewL) trks of
    [] -> trks -- all tracks are empty
    (_, ((), (), Nothing)) : _ -> panic "blip in joined phrase stream"
    firstPhrase@(dt, ((), (), Just len)) : _ -> let
      hasThisPhrase trk = case RTB.viewL trk of
        Nothing             -> Nothing
        Just (phrase, trk') -> guard (phrase == firstPhrase) >> Just trk'
      in case mapM hasThisPhrase trks of
        Just trks' -> map (uncurry RTB.cons firstPhrase) $ go trks' -- full unison
        Nothing    -> let
          ix = length $ takeWhile (isNothing . hasThisPhrase) trks
          trksNext = map (RTB.delay len . U.trackDrop (NNC.add dt len)) trks
          repackage i = if i == ix
            then uncurry RTB.cons firstPhrase
            else RTB.delay dt
          in zipWith repackage [0..] $ go trksNext
  boolToLong b = if b then NoteOn () () else NoteOff ()
  longToBool (NoteOn  () ()) = True
  longToBool (Blip    () ()) = panic "blip in LongNote stream"
  longToBool (NoteOff    ()) = False
  panic s = error $ "RockBand2.fixOverdrive: panic! this shouldn't happen: " ++ s
  in map (fmap longToBool . splitEdges) $ go $ map (joinEdges . fmap boolToLong) tracks

convertMIDI :: F.Song (F.RB3File U.Beats) -> F.Song (F.RB2File U.Beats)
convertMIDI mid = mid
  { F.s_tracks = fixUnisons $ F.RB2File
    { F.rb2PartDrums = fixDrumColors $ fixDoubleEvents $
      flip RTB.mapMaybe (F.rb3PartDrums $ F.s_tracks mid) $ \case
        -- Drums.ProType{} -> Nothing -- Magma is fine with pro markers
        Drums.SingleRoll{} -> Nothing
        Drums.DoubleRoll{} -> Nothing
        Drums.Kick2x -> Nothing
        Drums.DiffEvent diff (Drums.Mix aud Drums.DiscoNoFlip) ->
          Just $ Drums.DiffEvent diff $ Drums.Mix aud Drums.NoDisco
        Drums.Animation a -> Just $ Drums.Animation $ case a of
          -- these were added in RB3
          Drums.Snare Drums.SoftHit hand -> Drums.Snare Drums.HardHit hand
          Drums.Ride Drums.LH            -> Drums.Hihat Drums.LH
          Drums.Crash2 hit Drums.LH      -> Drums.Crash1 hit Drums.LH
          _                              -> a
        x -> Just x
    , F.rb2PartGuitar = fixFiveColors $ fixGB True $ F.rb3PartGuitar $ F.s_tracks mid
    , F.rb2PartBass = fixFiveColors $ fixGB False $ F.rb3PartBass $ F.s_tracks mid
    , F.rb2PartVocals = flip RTB.filter (F.rb3PartVocals $ F.s_tracks mid) $ \case
      Vox.LyricShift -> False
      Vox.RangeShift{} -> False
      _ -> True
    , F.rb2Events = flip RTB.filter (F.rb3Events $ F.s_tracks mid) $ \case
      Events.SectionRB3 _ -> False
      Events.SectionRB2 _ -> False
      _                   -> True
    , F.rb2Beat = F.rb3Beat $ F.s_tracks mid
    , F.rb2Venue = convertVenue endPosn $ F.rb3Venue $ F.s_tracks mid
    }
  } where
    endPosn :: Maybe U.Beats
    endPosn = listToMaybe $ toList $ fmap (fst . fst) $ RTB.viewL
      $ RTB.filter (== Events.End) $ F.rb3Events $ F.s_tracks mid
    fixGB hasSolos t = flip RTB.mapMaybe t $ \case
      Five.Tremolo{} -> Nothing
      Five.Trill{} -> Nothing
      Five.Solo{} | not hasSolos -> Nothing
      e -> Just e
    -- this fixes when a song, inexplicably, has simultaneous "soft snare LH" and "hard snare LH"
    fixDoubleEvents = RTB.flatten . fmap nub . RTB.collectCoincident
    -- the complicated dance to extract OD phrases, fix partial unisons,
    -- and put the phrases back
    fixUnisons trks = let
      gtr  = F.rb2PartGuitar trks
      bass = F.rb2PartBass   trks
      drum = F.rb2PartDrums  trks
      gtrOD  = RTB.mapMaybe getFiveOD gtr
      bassOD = RTB.mapMaybe getFiveOD bass
      drumOD = RTB.mapMaybe getDrumOD drum
      getFiveOD = \case Five.Overdrive  b -> Just b; _ -> Nothing
      getDrumOD = \case Drums.Overdrive b -> Just b; _ -> Nothing
      replaceFiveOD od trk = RTB.merge (fmap Five.Overdrive od)
        $ RTB.filter (\case Five.Overdrive _ -> False; _ -> True) trk
      replaceDrumsOD od trk = RTB.merge (fmap Drums.Overdrive od)
        $ RTB.filter (\case Drums.Overdrive _ -> False; _ -> True) trk
      in case (not $ RTB.null gtr, not $ RTB.null bass, not $ RTB.null drum) of
        (False, False, False) -> trks
        ( True, False, False) -> trks
        (False,  True, False) -> trks
        (False, False,  True) -> trks
        ( True,  True, False) -> let
          [gtrOD', bassOD'] = fixOverdrive [gtrOD, bassOD]
          in trks
            { F.rb2PartGuitar = replaceFiveOD gtrOD'  $ F.rb2PartGuitar trks
            , F.rb2PartBass   = replaceFiveOD bassOD' $ F.rb2PartBass   trks
            }
        ( True, False,  True) -> let
          [drumOD', gtrOD'] = fixOverdrive [drumOD, gtrOD]
          in trks
            { F.rb2PartGuitar = replaceFiveOD  gtrOD'  $ F.rb2PartGuitar trks
            , F.rb2PartDrums  = replaceDrumsOD drumOD' $ F.rb2PartDrums  trks
            }
        (False,  True,  True) -> let
          [drumOD', bassOD'] = fixOverdrive [drumOD, bassOD]
          in trks
            { F.rb2PartBass   = replaceFiveOD  bassOD' $ F.rb2PartBass   trks
            , F.rb2PartDrums  = replaceDrumsOD drumOD' $ F.rb2PartDrums  trks
            }
        ( True,  True,  True) -> let
          [drumOD', gtrOD', bassOD'] = fixOverdrive [drumOD, gtrOD, bassOD]
          in trks
            { F.rb2PartGuitar = replaceFiveOD  gtrOD'  $ F.rb2PartGuitar trks
            , F.rb2PartBass   = replaceFiveOD  bassOD' $ F.rb2PartBass   trks
            , F.rb2PartDrums  = replaceDrumsOD drumOD' $ F.rb2PartDrums  trks
            }

convertVenue :: Maybe U.Beats -> RTB.T U.Beats V.Event -> RTB.T U.Beats V2.Event
convertVenue endPosn rtb = let
  (z, nz) = U.trackSplitZero rtb
  z' = V.Lighting V.Lighting_verse : filter (\case V.Lighting _ -> False; _ -> True) z
  annotateEnding trk = case endPosn of
    Nothing  -> annotate True trk
    Just end -> case U.trackSplit (end - 1) trk of
      (a, b) -> trackGlue (end - 1) (annotate True a) (annotate False b)
  annotate b = fmap $ \x -> (b, x)
  in RTB.flatten
    $ fmap (nub . uncurry convertVenueInstant)
    $ annotateEnding
    $ RTB.collectCoincident
    $ U.trackGlueZero z' nz

convertVenueInstant :: Bool -> [V.Event] -> [V2.Event]
convertVenueInstant canMakeBlips evts = nub $ let
  makeBlip x = [x | canMakeBlips]
  changed = flip map evts $ \e -> case e of
    V.Camera c              -> Left c
    V.SingalongGuitarKeys b -> Right [V2.SingalongGuitar b]
    V.SingalongDrums      b -> Right [V2.SingalongDrums  b]
    V.SingalongBassKeys   b -> Right [V2.SingalongBass   b]
    V.SpotlightKeys   _     -> Right []
    V.SpotlightVocal  b     -> Right [V2.SpotlightVocal  b]
    V.SpotlightGuitar b     -> Right [V2.SpotlightGuitar b]
    V.SpotlightBass   b     -> Right [V2.SpotlightBass   b]
    V.SpotlightDrums  b     -> Right [V2.SpotlightDrums  b]
    V.PostProcess pp        -> Right $ makeBlip $ V2.PostProcess $ postproc pp
    V.Lighting l            -> Right [V2.Lighting $ lighting l]
    V.LightingFirst         -> Right $ makeBlip V2.LightingFirst
    V.LightingPrev          -> Right $ makeBlip V2.LightingPrev
    V.LightingNext          -> Right $ makeBlip V2.LightingNext
    V.BonusFX               -> Right [V2.BonusFX]
    V.BonusFXOptional       -> Right [V2.BonusFXOptional]
  postproc = \case
    V.PP_ProFilm_a -> V2.PP_Default
    V.PP_ProFilm_b -> V2.PP_Default
    V.PP_video_a -> V2.PP_video_a
    V.PP_film_16mm -> V2.PP_film_16mm
    V.PP_shitty_tv -> V2.PP_video_security
    V.PP_bloom -> V2.PP_ProFilm_a
    V.PP_film_sepia_ink -> V2.PP_film_sepia_ink
    V.PP_film_silvertone -> V2.PP_film_silvertone
    V.PP_film_b_w -> V2.PP_video_bw
    V.PP_video_bw -> V2.PP_video_bw
    V.PP_contrast_a -> V2.PP_contrast_a
    V.PP_photocopy -> V2.PP_photocopy
    V.PP_film_blue_filter -> V2.PP_BlueTint
    V.PP_desat_blue -> V2.PP_BlueTint
    V.PP_video_security -> V2.PP_video_security
    V.PP_bright -> V2.PP_ProFilm_b
    V.PP_posterize -> V2.PP_ProFilm_a
    V.PP_clean_trails -> V2.PP_video_trails
    V.PP_video_trails -> V2.PP_video_trails
    V.PP_flicker_trails -> V2.PP_video_trails
    V.PP_desat_posterize_trails -> V2.PP_film_16mm
    V.PP_film_contrast -> V2.PP_contrast_a
    V.PP_film_contrast_blue -> V2.PP_BlueTint
    V.PP_film_contrast_green -> V2.PP_contrast_a
    V.PP_film_contrast_red -> V2.PP_contrast_a
    V.PP_horror_movie_special -> V2.PP_photo_negative
    V.PP_photo_negative -> V2.PP_photo_negative
    V.PP_ProFilm_mirror_a -> V2.PP_ProFilm_mirror_a
    V.PP_ProFilm_psychedelic_blue_red -> V2.PP_photo_negative
    V.PP_space_woosh -> V2.PP_video_trails
  lighting l = case l of
    V.Lighting_                 -> V2.Lighting_
    V.Lighting_intro            -> V2.Lighting_ -- different
    V.Lighting_verse            -> V2.Lighting_verse
    V.Lighting_chorus           -> V2.Lighting_chorus
    V.Lighting_manual_cool      -> V2.Lighting_manual_cool
    V.Lighting_manual_warm      -> V2.Lighting_manual_warm
    V.Lighting_dischord         -> V2.Lighting_dischord
    V.Lighting_stomp            -> V2.Lighting_stomp
    V.Lighting_loop_cool        -> V2.Lighting_loop_cool
    V.Lighting_loop_warm        -> V2.Lighting_loop_warm
    V.Lighting_harmony          -> V2.Lighting_harmony
    V.Lighting_frenzy           -> V2.Lighting_frenzy
    V.Lighting_silhouettes      -> V2.Lighting_silhouettes
    V.Lighting_silhouettes_spot -> V2.Lighting_silhouettes_spot
    V.Lighting_searchlights     -> V2.Lighting_searchlights
    V.Lighting_sweep            -> V2.Lighting_sweep
    V.Lighting_strobe_slow      -> V2.Lighting_strobe_slow
    V.Lighting_strobe_fast      -> V2.Lighting_strobe_fast
    V.Lighting_blackout_slow    -> V2.Lighting_blackout_slow
    V.Lighting_blackout_fast    -> V2.Lighting_blackout_fast
    V.Lighting_blackout_spot    -> V2.Lighting_silhouettes_spot -- different
    V.Lighting_flare_slow       -> V2.Lighting_flare_slow
    V.Lighting_flare_fast       -> V2.Lighting_flare_fast
    V.Lighting_bre              -> V2.Lighting_bre
  directed c = [V2.DoCut $ V2.DoDirectedCut c]
  behind = cut [V2.NoClose]
  close = cut [V2.NoBehind, V2.OnlyClose]
  far = cut [V2.NoBehind, V2.OnlyFar, V2.NoClose]
  cut distance insts = V2.CameraCut : (distance ++ map cutInst insts)
  cutInst = \case
    Bass -> V2.FocusBass
    Drums -> V2.FocusDrums
    Guitar -> V2.FocusGuitar
    Vocal -> V2.FocusVocal
    Keys -> V2.FocusGuitar
  cameraEvents = case sortBy (flip compare) $ lefts changed of
    [] -> []
    cam : _ -> case cam of
      -- generic 4 camera shots
      V.Camera_coop_all_behind        -> behind [Guitar, Bass, Drums, Vocal]
      V.Camera_coop_all_far           -> far [Guitar, Bass, Drums, Vocal]
      V.Camera_coop_all_near          -> close [Guitar, Bass, Drums, Vocal]
      -- 3 char shots (no drum)
      V.Camera_coop_front_behind      -> behind [Guitar, Bass, Vocal]
      V.Camera_coop_front_near        -> close [Guitar, Bass, Vocal]
      -- 1 char standard shots
      V.Camera_coop_d_behind          -> behind [Drums]
      V.Camera_coop_d_near            -> close [Drums]
      V.Camera_coop_v_behind          -> behind [Vocal]
      V.Camera_coop_v_near            -> close [Vocal]
      V.Camera_coop_b_behind          -> behind [Bass]
      V.Camera_coop_b_near            -> close [Bass]
      V.Camera_coop_g_behind          -> behind [Guitar]
      V.Camera_coop_g_near            -> close [Guitar]
      V.Camera_coop_k_behind          -> behind [Guitar]
      V.Camera_coop_k_near            -> close [Guitar]
      -- 1 char closeups
      V.Camera_coop_d_closeup_hand    -> close [Drums]
      V.Camera_coop_d_closeup_head    -> close [Drums]
      V.Camera_coop_v_closeup         -> close [Vocal]
      V.Camera_coop_b_closeup_hand    -> close [Bass]
      V.Camera_coop_b_closeup_head    -> close [Bass]
      V.Camera_coop_g_closeup_hand    -> close [Guitar]
      V.Camera_coop_g_closeup_head    -> close [Guitar]
      V.Camera_coop_k_closeup_hand    -> close [Guitar]
      V.Camera_coop_k_closeup_head    -> close [Guitar]
      -- 2 char shots
      V.Camera_coop_dv_near           -> close [Drums, Vocal]
      V.Camera_coop_bd_near           -> close [Bass, Drums]
      V.Camera_coop_dg_near           -> close [Drums, Guitar]
      V.Camera_coop_bv_behind         -> behind [Bass, Vocal]
      V.Camera_coop_bv_near           -> close [Bass, Vocal]
      V.Camera_coop_gv_behind         -> behind [Guitar, Vocal]
      V.Camera_coop_gv_near           -> close [Guitar, Vocal]
      V.Camera_coop_kv_behind         -> behind [Guitar, Vocal]
      V.Camera_coop_kv_near           -> close [Guitar, Vocal]
      V.Camera_coop_bg_behind         -> behind [Bass, Guitar]
      V.Camera_coop_bg_near           -> close [Bass, Guitar]
      V.Camera_coop_bk_behind         -> behind [Bass, Guitar]
      V.Camera_coop_bk_near           -> close [Bass, Guitar]
      V.Camera_coop_gk_behind         -> behind [Guitar, Bass]
      V.Camera_coop_gk_near           -> close [Guitar, Bass]
      -- directed cuts
      V.Camera_directed_all           -> directed V2.Camera_directed_all
      V.Camera_directed_all_cam       -> directed V2.Camera_directed_all_cam
      V.Camera_directed_all_lt        -> directed V2.Camera_directed_all_lt
      V.Camera_directed_all_yeah      -> directed V2.Camera_directed_all_yeah
      V.Camera_directed_bre           -> directed V2.Camera_directed_bre
      V.Camera_directed_brej          -> directed V2.Camera_directed_brej
      V.Camera_directed_crowd         -> directed V2.Camera_directed_crowd_g
      V.Camera_directed_drums         -> directed V2.Camera_directed_drums
      V.Camera_directed_drums_pnt     -> directed V2.Camera_directed_drums_pnt
      V.Camera_directed_drums_np      -> directed V2.Camera_directed_drums_np
      V.Camera_directed_drums_lt      -> directed V2.Camera_directed_drums_lt
      V.Camera_directed_drums_kd      -> directed V2.Camera_directed_drums_kd
      V.Camera_directed_vocals        -> directed V2.Camera_directed_vocals
      V.Camera_directed_vocals_np     -> directed V2.Camera_directed_vocals_np
      V.Camera_directed_vocals_cls    -> directed V2.Camera_directed_vocals_cls
      V.Camera_directed_vocals_cam_pr -> directed V2.Camera_directed_vocals_cam
      V.Camera_directed_vocals_cam_pt -> directed V2.Camera_directed_vocals_cam
      V.Camera_directed_stagedive     -> directed V2.Camera_directed_stagedive
      V.Camera_directed_crowdsurf     -> directed V2.Camera_directed_crowdsurf
      V.Camera_directed_bass          -> directed V2.Camera_directed_bass
      V.Camera_directed_crowd_b       -> directed V2.Camera_directed_crowd_b
      V.Camera_directed_bass_np       -> directed V2.Camera_directed_bass_np
      V.Camera_directed_bass_cam      -> directed V2.Camera_directed_bass_cam
      V.Camera_directed_bass_cls      -> directed V2.Camera_directed_bass_cls
      V.Camera_directed_guitar        -> directed V2.Camera_directed_guitar
      V.Camera_directed_crowd_g       -> directed V2.Camera_directed_crowd_g
      V.Camera_directed_guitar_np     -> directed V2.Camera_directed_guitar_np
      V.Camera_directed_guitar_cls    -> directed V2.Camera_directed_guitar_cls
      V.Camera_directed_guitar_cam_pr -> directed V2.Camera_directed_guitar_cam
      V.Camera_directed_guitar_cam_pt -> directed V2.Camera_directed_guitar_cam
      V.Camera_directed_keys          -> directed V2.Camera_directed_crowd_b
      V.Camera_directed_keys_cam      -> directed V2.Camera_directed_crowd_b
      V.Camera_directed_keys_np       -> directed V2.Camera_directed_crowd_b
      V.Camera_directed_duo_drums     -> directed V2.Camera_directed_drums
      V.Camera_directed_duo_bass      -> directed V2.Camera_directed_duo_bass
      V.Camera_directed_duo_guitar    -> directed V2.Camera_directed_duo_guitar
      V.Camera_directed_duo_kv        -> directed V2.Camera_directed_duo_guitar
      V.Camera_directed_duo_gb        -> directed V2.Camera_directed_duo_gb
      V.Camera_directed_duo_kb        -> directed V2.Camera_directed_duo_gb
      V.Camera_directed_duo_kg        -> directed V2.Camera_directed_duo_gb
  in concat $ cameraEvents : rights changed

fixFiveColors :: RTB.T U.Beats Five.Event -> RTB.T U.Beats Five.Event
fixFiveColors rtb = let
  getDiff d = RTB.partitionMaybe $ \case
    Five.DiffEvent d' (Five.Note ln) | d == d' -> Just ln
    _                                          -> Nothing
  (easy  , notEasy  ) = getDiff Easy rtb
  (medium, notMedium) = getDiff Medium notEasy
  (hard  , notHard  ) = getDiff Hard notMedium
  (expert, _        ) = getDiff Expert notHard
  usedColors = Set.fromList $ concatMap toList $ RTB.getBodies expert
  easy'   = makeDiff Easy   $ useColorsFive usedColors easy
  medium' = makeDiff Medium $ useColorsFive usedColors medium
  hard'   = makeDiff Hard   $ useColorsFive usedColors hard
  makeDiff d = fmap $ Five.DiffEvent d . Five.Note
  in foldr RTB.merge notHard [easy', medium', hard']

useColorsFive :: Set.Set Five.Color -> RTB.T U.Beats (LongNote () Five.Color) -> RTB.T U.Beats (LongNote () Five.Color)
useColorsFive cols rtb = let
  gtr = joinEdges $ Five.guitarify rtb
  present = Set.fromList $ concatMap toList $ RTB.getBodies rtb
  missing = Set.difference cols present
  good = foldl (>>=) [gtr] $ map useColorFive $ Set.toDescList missing
  in if Set.null missing then rtb else case good of
    []    -> rtb
    g : _ -> RTB.flatten $ fmap (traverse toList) $ splitEdges g

focuses :: [a] -> [([a], a, [a])]
focuses [] = []
focuses xs = zip3 (inits xs) xs (tail $ tails xs)

useColorFive
  ::                      Five.Color
  ->  RTB.T U.Beats ((), [Five.Color], Maybe U.Beats)
  -> [RTB.T U.Beats ((), [Five.Color], Maybe U.Beats)]
useColorFive newColor rtb = do
  -- TODO sort this better (move closer colors first)
  (before, (t, ((), oldColors, len)), after) <- focuses $ reverse $ RTB.toPairList rtb
  oldColor <- oldColors
  let newColors = map (\c -> if c == oldColor then newColor else c) oldColors
  guard $ elem oldColor $ concatMap (\(_, (_, cols, _)) -> cols) $ before ++ after
  return $ RTB.fromPairList $ reverse $ before ++ [(t, ((), newColors, len))] ++ after

fixDrumColors :: RTB.T U.Beats Drums.Event -> RTB.T U.Beats Drums.Event
fixDrumColors rtb = let
  getDiff d = RTB.partitionMaybe $ \case
    Drums.DiffEvent d' (Drums.Note gem) | d == d' -> Just gem
    _                                             -> Nothing
  (easy  , notEasy  ) = getDiff Easy rtb
  (medium, notMedium) = getDiff Medium notEasy
  (hard  , notHard  ) = getDiff Hard notMedium
  (expert, _        ) = getDiff Expert notHard
  usedColors = Set.fromList $ RTB.getBodies expert
  easy'   = makeDiff Easy   $ useColorsDrums usedColors expert easy
  medium' = makeDiff Medium $ useColorsDrums usedColors expert medium
  hard'   = makeDiff Hard   $ useColorsDrums usedColors expert hard
  makeDiff d = fmap $ Drums.DiffEvent d . Drums.Note
  in foldr RTB.merge notHard [easy', medium', hard']

useColorsDrums :: Set.Set (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ()) -> RTB.T U.Beats (Drums.Gem ())
useColorsDrums cols expert rtb = let
  drums = RTB.collectCoincident rtb
  present = Set.fromList $ RTB.getBodies rtb
  missing = Set.difference cols present
  expert' = RTB.collectCoincident expert
  good = foldl (>>=) [drums] $ map (useColorDrums expert') $ Set.toDescList missing
  in if Set.null missing then rtb else case good of
    []    -> rtb
    g : _ -> RTB.flatten g

useColorDrums
  ::  RTB.T U.Beats [Drums.Gem ()]
  ->                 Drums.Gem ()
  ->  RTB.T U.Beats [Drums.Gem ()]
  -> [RTB.T U.Beats [Drums.Gem ()]]
useColorDrums expert gem rtb = let
  annotated = RTB.mapMaybe annotate $ RTB.collectCoincident $ RTB.merge (fmap Left expert) (fmap Right rtb)
  annotate = \case
    [Left x, Right y] -> Just ( x, y)
    [Right y]         -> Just ([], y)
    [Left x]          -> Just (x, [])
    _                 -> error "RockBand2.useColorDrums: panic! impossible case while fixing drums reductions"
  removeX (t, (_, gems)) = (t, gems)
  in do
    (before, (t, (xgems, gems)), after) <- focuses $ reverse $ RTB.toPairList annotated
    let otherGems = concatMap (snd . snd) $ before ++ after
    guard $ elem gem xgems
    guard $ all (`elem` otherGems) gems
    return $ RTB.fromPairList $ reverse $ map removeX before ++ [(t, [gem])] ++ map removeX after
