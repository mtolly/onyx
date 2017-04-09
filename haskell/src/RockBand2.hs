{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module RockBand2 (convertMIDI, dryVoxAudio) where

import           Config                           (Instrument (..))
import           Control.Monad                    (guard)
import           Data.Conduit.Audio               (AudioSource)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (inits, nub, sort, tails)
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
import           RockBand.Parse                   (unparseBlip, unparseCommand,
                                                   unparseList, unparseOne)
import           RockBand.PhaseShiftMessage       (discardPS)
import qualified RockBand.Venue                   as V
import qualified RockBand.Vocals                  as Vox
import           Scripts                          (trackGlue)
import qualified Sound.MIDI.File.Event            as E
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

convertMIDI :: KeysRB2 -> U.Beats -> F.Song (F.RB3File U.Beats) -> F.Song (F.RB2File U.Beats)
convertMIDI keysrb2 hopoThresh mid = mid
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
    , F.rb2PartGuitar = fixFiveColors $ fixGB True $ case keysrb2 of
      KeysGuitar -> Five.keysToGuitar hopoThresh $ F.rb3PartKeys $ F.s_tracks mid
      _ -> F.rb3PartGuitar $ F.s_tracks mid
    , F.rb2PartBass = fixFiveColors $ fixGB False $ case keysrb2 of
      KeysBass -> Five.keysToGuitar hopoThresh $ F.rb3PartKeys $ F.s_tracks mid
      _ -> F.rb3PartBass $ F.s_tracks mid
    , F.rb2PartVocals = flip RTB.filter (F.rb3PartVocals $ F.s_tracks mid) $ \case
      Vox.LyricShift -> False
      Vox.RangeShift{} -> False
      _ -> True
    , F.rb2Events = flip RTB.filter (F.rb3Events $ F.s_tracks mid) $ \case
      Events.PracticeSection _ -> False
      _                        -> True
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

convertVenue :: Maybe U.Beats -> RTB.T U.Beats V.Event -> RTB.T U.Beats E.T
convertVenue endPosn rtb = let
  (z, nz) = U.trackSplitZero rtb
  z' = V.Lighting V.Lighting_verse : filter (\case V.Lighting _ -> False; _ -> True) z
  annotateEnding trk = case endPosn of
    Nothing  -> annotate True trk
    Just end -> case U.trackSplit (end - 1) trk of
      (a, b) -> trackGlue (end - 1) (annotate True a) (annotate False b)
  annotate b = fmap $ \x -> (b, x)
  in U.setTrackName "VENUE"
    $ RTB.flatten $ fmap nub $ RTB.collectCoincident
    $ U.trackJoin
    $ fmap (uncurry convertVenueInstant)
    $ annotateEnding
    $ RTB.collectCoincident
    $ U.trackGlueZero z' nz

convertVenueInstant :: Bool -> [V.Event] -> RTB.T U.Beats E.T
convertVenueInstant canMakeBlips evts = let
  unparseBlip' p = if canMakeBlips then unparseBlip p else RTB.empty
  changed = flip mapMaybe evts $ \e -> case e of
    V.Camera c              -> Just $ Left c
    V.SingalongGuitarKeys{} -> unchanged e
    V.SingalongDrums{}      -> unchanged e
    V.SingalongBassKeys{}   -> unchanged e
    V.SpotlightKeys{}       -> Nothing
    V.SpotlightVocal{}      -> unchanged e
    V.SpotlightGuitar{}     -> unchanged e
    V.SpotlightBass{}       -> unchanged e
    V.SpotlightDrums{}      -> unchanged e
    V.PostProcess pp        -> Just $ Right $ postproc pp
    V.Lighting l            -> Just $ Right $ lighting l
    V.LightingFirst         -> Just $ Right $ unparseBlip' 50
    V.LightingPrev          -> Just $ Right $ unparseBlip' 49
    V.LightingNext          -> Just $ Right $ unparseBlip' 48
    V.BonusFX{}             -> unchanged e
    V.BonusFXOptional{}     -> unchanged e
  unchanged = Just . Right . unparseOne
  postproc = \case
    V.PP_ProFilm_a -> unparseBlip' 96
    V.PP_ProFilm_b -> unparseBlip' 96
    V.PP_video_a -> unparseBlip' 107
    V.PP_film_16mm -> unparseBlip' 98
    V.PP_shitty_tv -> unparseBlip' 109
    V.PP_bloom -> unparseBlip' 103
    V.PP_film_sepia_ink -> unparseBlip' 99
    V.PP_film_silvertone -> unparseBlip' 100
    V.PP_film_b_w -> unparseBlip' 108
    V.PP_video_bw -> unparseBlip' 108
    V.PP_contrast_a -> unparseBlip' 97
    V.PP_photocopy -> unparseBlip' 102
    V.PP_film_blue_filter -> unparseBlip' 106
    V.PP_desat_blue -> unparseBlip' 106
    V.PP_video_security -> unparseBlip' 109
    V.PP_bright -> unparseBlip' 104
    V.PP_posterize -> unparseBlip' 103
    V.PP_clean_trails -> unparseBlip' 110
    V.PP_video_trails -> unparseBlip' 110
    V.PP_flicker_trails -> unparseBlip' 110
    V.PP_desat_posterize_trails -> unparseBlip' 98
    V.PP_film_contrast -> unparseBlip' 97
    V.PP_film_contrast_blue -> unparseBlip' 106
    V.PP_film_contrast_green -> unparseBlip' 97
    V.PP_film_contrast_red -> unparseBlip' 97
    V.PP_horror_movie_special -> unparseBlip' 101
    V.PP_photo_negative -> unparseBlip' 101
    V.PP_ProFilm_mirror_a -> unparseBlip' 105
    V.PP_ProFilm_psychedelic_blue_red -> unparseBlip' 101
    V.PP_space_woosh -> unparseBlip' 110
  lighting l = case l of
    V.Lighting_                 -> unparseCommand l
    V.Lighting_intro            -> unparseCommand V.Lighting_
    V.Lighting_verse            -> unparseList ["verse"]
    V.Lighting_chorus           -> unparseList ["chorus"]
    V.Lighting_manual_cool      -> unparseCommand l
    V.Lighting_manual_warm      -> unparseCommand l
    V.Lighting_dischord         -> unparseCommand l
    V.Lighting_stomp            -> unparseCommand l
    V.Lighting_loop_cool        -> unparseCommand l
    V.Lighting_loop_warm        -> unparseCommand l
    V.Lighting_harmony          -> unparseCommand l
    V.Lighting_frenzy           -> unparseCommand l
    V.Lighting_silhouettes      -> unparseCommand l
    V.Lighting_silhouettes_spot -> unparseCommand l
    V.Lighting_searchlights     -> unparseCommand l
    V.Lighting_sweep            -> unparseCommand l
    V.Lighting_strobe_slow      -> unparseCommand l
    V.Lighting_strobe_fast      -> unparseCommand l
    V.Lighting_blackout_slow    -> unparseCommand l
    V.Lighting_blackout_fast    -> unparseCommand l
    V.Lighting_blackout_spot    -> unparseCommand V.Lighting_silhouettes_spot
    V.Lighting_flare_slow       -> unparseCommand l
    V.Lighting_flare_fast       -> unparseCommand l
    V.Lighting_bre              -> unparseCommand l
  directed d = unparseList ["do_directed_cut", d]
  behind = cut [73]
  far = cut [70, 72]
  close = cut [70, 71, 73]
  cut :: [Int] -> [Instrument] -> RTB.T U.Beats E.T
  cut distance insts = foldr RTB.merge RTB.empty $ map unparseBlip' $
    60 : (distance ++ map cutInst insts)
  cutInst = \case
    Bass -> 61
    Drums -> 62
    Guitar -> 63
    Vocal -> 64
    Keys -> 63
  cameraEvents = case reverse $ sort $ lefts changed of
    [] -> RTB.empty
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
      V.Camera_directed_all           -> directed "directed_all"
      V.Camera_directed_all_cam       -> directed "directed_all_cam"
      V.Camera_directed_all_lt        -> directed "directed_all_lt"
      V.Camera_directed_all_yeah      -> directed "directed_all_yeah"
      V.Camera_directed_bre           -> directed "directed_bre"
      V.Camera_directed_brej          -> directed "directed_brej"
      V.Camera_directed_crowd         -> directed "directed_crowd_g"
      V.Camera_directed_drums         -> directed "directed_drums"
      V.Camera_directed_drums_pnt     -> directed "directed_drums_pnt"
      V.Camera_directed_drums_np      -> directed "directed_drums_np"
      V.Camera_directed_drums_lt      -> directed "directed_drums_lt"
      V.Camera_directed_drums_kd      -> directed "directed_drums_kd"
      V.Camera_directed_vocals        -> directed "directed_vocals"
      V.Camera_directed_vocals_np     -> directed "directed_vocals_np"
      V.Camera_directed_vocals_cls    -> directed "directed_vocals_cls"
      V.Camera_directed_vocals_cam_pr -> directed "directed_vocals_cam"
      V.Camera_directed_vocals_cam_pt -> directed "directed_vocals_cam"
      V.Camera_directed_stagedive     -> directed "directed_stagedive"
      V.Camera_directed_crowdsurf     -> directed "directed_crowdsurf"
      V.Camera_directed_bass          -> directed "directed_bass"
      V.Camera_directed_crowd_b       -> directed "directed_crowd_b"
      V.Camera_directed_bass_np       -> directed "directed_bass_np"
      V.Camera_directed_bass_cam      -> directed "directed_bass_cam"
      V.Camera_directed_bass_cls      -> directed "directed_bass_cls"
      V.Camera_directed_guitar        -> directed "directed_guitar"
      V.Camera_directed_crowd_g       -> directed "directed_crowd_g"
      V.Camera_directed_guitar_np     -> directed "directed_guitar_np"
      V.Camera_directed_guitar_cls    -> directed "directed_guitar_cls"
      V.Camera_directed_guitar_cam_pr -> directed "directed_guitar_cam"
      V.Camera_directed_guitar_cam_pt -> directed "directed_guitar_cam"
      V.Camera_directed_keys          -> directed "directed_crowd_b"
      V.Camera_directed_keys_cam      -> directed "directed_crowd_b"
      V.Camera_directed_keys_np       -> directed "directed_crowd_b"
      V.Camera_directed_duo_drums     -> directed "directed_drums"
      V.Camera_directed_duo_bass      -> directed "directed_duo_bass"
      V.Camera_directed_duo_guitar    -> directed "directed_duo_guitar"
      V.Camera_directed_duo_kv        -> directed "directed_duo_guitar"
      V.Camera_directed_duo_gb        -> directed "directed_duo_gb"
      V.Camera_directed_duo_kb        -> directed "directed_duo_gb"
      V.Camera_directed_duo_kg        -> directed "directed_duo_gb"
  in foldr RTB.merge cameraEvents $ rights changed

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

data KeysRB2
  = NoKeys
  | KeysGuitar
  | KeysBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
