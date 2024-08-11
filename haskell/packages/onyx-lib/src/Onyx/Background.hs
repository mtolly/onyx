{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PatternSynonyms       #-}
module Onyx.Background where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard)
import qualified Control.Monad.Random.Class       as R
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common                 (Difficulty (..), Edge (..),
                                                   LongNote (..), Mood (..),
                                                   RB3Instrument (..),
                                                   pattern RNil, pattern Wait)
import           Onyx.MIDI.Read                   (mapTrack)
import qualified Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.Drums.Elite
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret
import qualified Onyx.MIDI.Track.Mania
import qualified Onyx.MIDI.Track.ProGuitar
import qualified Onyx.MIDI.Track.ProKeys
import qualified Onyx.MIDI.Track.Rocksmith
import qualified Onyx.MIDI.Track.SixFret
import           Onyx.MIDI.Track.Venue
import           Onyx.MIDI.Track.VenueGen
import qualified Onyx.MIDI.Track.Vocal
import qualified Sound.MIDI.Util                  as U

-- character moods

getMoods :: U.TempoMap -> U.Beats -> F.OnyxPart U.Beats -> RTB.T U.Beats Mood
getMoods tmap endTime opart = let
  authoredMoods = filter (not . RTB.null)
    [ opart.onyxPartDrums.drumMood
    , opart.onyxPartDrums2x.drumMood
    , opart.onyxPartRealDrumsPS.drumMood
    , opart.onyxPartGuitar.fiveMood
    , opart.onyxPartKeys.fiveMood
    , opart.onyxPartGuitarExt.fiveMood
    , opart.onyxPartVocals.vocalMood
    , opart.onyxHarm1.vocalMood
    ]
  autoMoods = mapMaybe (\t -> guard (not $ RTB.null t) >> Just (getAutoMoods t)) $
    [ longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartDrums
    , longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartDrums2x
    , longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartRealDrumsPS
    , longBlips (\t -> Map.lookup Expert t.tdDifficulties  ) (.tdGems  ) opart.onyxPartEliteDrums
    , longNotes (\t -> Map.lookup Expert t.fiveDifficulties) (.fiveGems) opart.onyxPartGuitar
    , longNotes (\t -> Map.lookup Expert t.fiveDifficulties) (.fiveGems) opart.onyxPartKeys
    , longNotes (\t -> Map.lookup Expert t.fiveDifficulties) (.fiveGems) opart.onyxPartGuitarExt
    , longNotes (\t -> Map.lookup Expert t.sixDifficulties) (.sixGems) opart.onyxPartSix
    , longNotes (\t -> Map.lookup Expert t.pgDifficulties) (.pgNotes) opart.onyxPartRealGuitar
    , longNotes (\t -> Map.lookup Expert t.pgDifficulties) (.pgNotes) opart.onyxPartRealGuitar22
    , longNotes Just (.rsNotes) opart.onyxPartRSGuitar
    , longNotes Just (.rsNotes) opart.onyxPartRSBass
    , longNotes Just (.pkNotes) opart.onyxPartRealKeysX
    , longVox opart.onyxPartVocals
    , longVox opart.onyxHarm1
    ] <> map (longNotes Just (.maniaNotes)) (Map.elems opart.onyxPartMania)
  longBlips getDiff getGems trk = maybe RTB.empty ((Blip () () <$) . getGems) $ getDiff trk
  longNotes getDiff getGems trk = maybe RTB.empty (fmap edgeToLong . getGems) $ getDiff trk
  longVox = fmap (\(_, b) -> if b then NoteOn () () else NoteOff ()) . (.vocalNotes)
  edgeToLong = \case
    EdgeOn _ _ -> NoteOn () ()
    EdgeOff  _ -> NoteOff   ()
  getAutoMoods
    = U.trackTake endTime
    . U.unapplyTempoTrack tmap
    . makeMoodsFromNotes (0.4 :: U.Seconds) 2
    . U.applyTempoTrack tmap
  in fromMaybe RTB.empty $ listToMaybe authoredMoods <|> listToMaybe autoMoods

makeMoodsFromNotes :: (NNC.C t) => t -> t -> RTB.T t (LongNote s a) -> RTB.T t Mood
makeMoodsFromNotes buffer gap rtb = let
  edges = RTB.collectCoincident $ U.trackJoin $ flip fmap rtb $ \case
    Blip{}    -> Wait NNC.zero True  $ Wait buffer False RNil
    NoteOn{}  -> Wait NNC.zero True  RNil
    NoteOff{} -> Wait buffer   False RNil
  go held = \case
    RNil -> RNil
    Wait dt changes rest -> let
      held' = held + sum (map (\b -> if b then 1 else -1) changes)
      in if held == 0 && held' /= 0
        then Wait dt Mood_play $ go held' rest
        else if held /= 0 && held' == 0
          then case rest of
            RNil -> Wait dt Mood_idle_realtime RNil
            Wait dt' _ _ -> if dt' > gap
              then Wait dt Mood_idle $ go held' rest
              else RTB.delay dt $ go held' rest
          else RTB.delay dt $ go held' rest
  removeDupes = \case
    Wait tx x (Wait ty y rest) | x == y -> removeDupes $ Wait tx x $ RTB.delay ty rest
    Wait tx x rest                      -> Wait tx x $ removeDupes rest
    RNil                                -> RNil
  in removeDupes $ go (0 :: Int) edges

-- venue

data VenueTarget
  = VenueTargetRB2
  | VenueTargetRB3

getVenue
  :: (R.MonadRandom m)
  => VenueTarget
  -> Map.Map RB3Instrument F.FlexPartName -- map of target parts to project parts
  -> U.TempoMap
  -> U.Beats
  -> F.Song (F.OnyxFile U.Beats)
  -> m (VenueTrack U.Beats)
getVenue target partMap tmap endTime ofile = do
  camera <- buildCamera (Map.keys partMap) ofile.s_tracks.onyxCamera
  -- cut off at end mostly since new blips introduced in rb3->rb2 can go past the end event
  let trimEnd = mapTrack $ U.trackTake endTime
      gen = mconcat
        [ ofile.s_tracks.onyxVenue
        , buildLighting ofile.s_tracks.onyxLighting
        , camera
        ]
  -- TODO lighting autogen
  withCamera <- if RTB.null gen.venueCameraRB3 && RTB.null gen.venueCameraRB2 && RTB.null gen.venueDirectedRB2
    then (\cam -> gen { venueCameraRB3 = cam }) <$> autoCamera partMap tmap endTime ofile
    else return gen
  return $ trimEnd
    $ (case target of VenueTargetRB2 -> compileVenueRB2; VenueTargetRB3 -> compileVenueRB3)
    $ withCamera

autoCamera
  :: (R.MonadRandom m)
  => Map.Map RB3Instrument F.FlexPartName
  -> U.TempoMap
  -> U.Beats
  -> F.Song (F.OnyxFile U.Beats)
  -> m (RTB.T U.Beats Camera3)
autoCamera partMap tmap endTime ofile = do
  let moodMap = flip fmap partMap $ \fpart -> let
        moods = getMoods tmap endTime $ fromMaybe mempty $ Map.lookup fpart $ ofile.s_tracks.onyxParts
        in Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 moods
      endSeconds = U.applyTempoMap tmap endTime
      cutTimes = map (U.unapplyTempoMap tmap) $ takeWhile (<= endSeconds) $ map fromRational [0, 2.5 ..]
  fmap (RTB.fromAbsoluteEventList . ATB.fromPairList) $ forM cutTimes $ \t -> do
    let insts = flip mapMaybe (Map.toList moodMap) $ \(rb3, moods) -> case Map.lookupLE t moods of
          Nothing                      -> Nothing
          Just (_, Mood_idle_realtime) -> Nothing
          Just (_, Mood_idle         ) -> Nothing
          Just (_, Mood_idle_intense ) -> Nothing
          _                            -> Just rb3
    cut <- R.fromList $ concat
      [ [ (V3_coop_all_behind    , 1) ]
      , [ (V3_coop_all_far       , 1) ]
      , [ (V3_coop_all_near      , 1) ]
      , [ (V3_coop_front_behind  , 1) ]
      , [ (V3_coop_front_near    , 1) ]
      , [ (V3_coop_d_behind      , 3) | elem Drums insts ]
      , [ (V3_coop_d_near        , 3) | elem Drums insts ]
      , [ (V3_coop_v_behind      , 3) | elem Vocal insts ]
      , [ (V3_coop_v_near        , 3) | elem Vocal insts ]
      , [ (V3_coop_b_behind      , 3) | elem Bass insts ]
      , [ (V3_coop_b_near        , 3) | elem Bass insts ]
      , [ (V3_coop_g_behind      , 3) | elem Guitar insts ]
      , [ (V3_coop_g_near        , 3) | elem Guitar insts ]
      , [ (V3_coop_k_behind      , 3) | elem Keys insts ]
      , [ (V3_coop_k_near        , 3) | elem Keys insts ]
      , [ (V3_coop_d_closeup_hand, 1) | elem Drums insts ]
      , [ (V3_coop_d_closeup_head, 1) | elem Drums insts ]
      , [ (V3_coop_v_closeup     , 1) | elem Vocal insts ]
      , [ (V3_coop_b_closeup_hand, 1) | elem Bass insts ]
      , [ (V3_coop_b_closeup_head, 1) | elem Bass insts ]
      , [ (V3_coop_g_closeup_hand, 1) | elem Guitar insts ]
      , [ (V3_coop_g_closeup_head, 1) | elem Guitar insts ]
      , [ (V3_coop_k_closeup_hand, 1) | elem Keys insts ]
      , [ (V3_coop_k_closeup_head, 1) | elem Keys insts ]
      , [ (V3_coop_dv_near       , 0.5) | all (`elem` insts) [Drums, Vocal] ]
      , [ (V3_coop_bd_near       , 0.5) | all (`elem` insts) [Bass, Drums] ]
      , [ (V3_coop_dg_near       , 0.5) | all (`elem` insts) [Drums, Guitar] ]
      , [ (V3_coop_bv_behind     , 0.5) | all (`elem` insts) [Bass, Vocal] ]
      , [ (V3_coop_bv_near       , 0.5) | all (`elem` insts) [Bass, Vocal] ]
      , [ (V3_coop_gv_behind     , 0.5) | all (`elem` insts) [Guitar, Vocal] ]
      , [ (V3_coop_gv_near       , 0.5) | all (`elem` insts) [Guitar, Vocal] ]
      , [ (V3_coop_kv_behind     , 0.5) | all (`elem` insts) [Keys, Vocal] ]
      , [ (V3_coop_kv_near       , 0.5) | all (`elem` insts) [Keys, Vocal] ]
      , [ (V3_coop_bg_behind     , 0.5) | all (`elem` insts) [Bass, Guitar] ]
      , [ (V3_coop_bg_near       , 0.5) | all (`elem` insts) [Bass, Guitar] ]
      , [ (V3_coop_bk_behind     , 0.5) | all (`elem` insts) [Bass, Keys] ]
      , [ (V3_coop_bk_near       , 0.5) | all (`elem` insts) [Bass, Keys] ]
      , [ (V3_coop_gk_behind     , 0.5) | all (`elem` insts) [Guitar, Keys] ]
      , [ (V3_coop_gk_near       , 0.5) | all (`elem` insts) [Guitar, Keys] ]
      ]
    return (t, cut)

{-
TODO better camera ideas:
- first identify possible key points: instrument entrances, solo starts, vocal phrase starts
- filter if too close, then pick more points in between those (both of these could use configurable cut pacing)
- for each point, decide which instruments should/can get focus
- apply some randomness to pick a cut for each one, try to balance instruments
- separately, maybe add some directed cuts during solos?
-}
