module RockBand2 (convertMIDI, dryVoxAudio) where

import qualified RockBand.File as F
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified RockBand.Drums as Drums
import qualified RockBand.Vocals as Vox
import qualified RockBand.FiveButton as Five
import qualified RockBand.Events as Events
import qualified Sound.MIDI.Util as U
import Data.Conduit.Audio (AudioSource, Duration(Seconds), silent, concatenate, sine)
import Scripts (trackGlue)
import Data.List (partition)
import Control.Monad (guard)

dryVoxAudio :: (Monad m) => F.Song U.Beats -> AudioSource m Float
dryVoxAudio f = let
  vox = foldr RTB.merge RTB.empty [ t | F.PartVocals t <- F.s_tracks f ]
  notes = RTB.normalize $ flip RTB.mapMaybe vox $ \case
    Vox.Note True  p -> Just (Just p)
    Vox.Note False _ -> Just Nothing
    _                -> Nothing
  go p rtb = case RTB.viewL rtb of
    Nothing -> silent (Seconds 1) 16000 1
    Just ((dt, p'), rtb') -> let
      chunk = case p of
        Nothing -> silent (Seconds $ realToFrac dt) 16000 1
        Just pitch -> sine (midiPitchToFreq $ fromEnum pitch + 36) (Seconds $ realToFrac dt) 16000
      in concatenate chunk $ go p' rtb'
  midiPitchToFreq p = (2 ** ((fromIntegral p - 69) / 12)) * 440
  in go Nothing $ U.applyTempoTrack (F.s_tempos f) notes

convertMIDI :: F.Song U.Beats -> F.Song U.Beats
convertMIDI mid = mid
  { F.s_tracks = fixUnisons $ flip mapMaybe (F.s_tracks mid) $ \case
    F.PartDrums  t -> Just $ F.PartDrums $ flip RTB.mapMaybe t $ \case
      -- Drums.ProType{} -> Nothing -- Magma is fine with pro markers
      Drums.SingleRoll{} -> Nothing
      Drums.DoubleRoll{} -> Nothing
      Drums.Kick2x -> Nothing
      Drums.DiffEvent diff (Drums.Mix aud Drums.DiscoNoFlip) ->
        Just $ Drums.DiffEvent diff $ Drums.Mix aud Drums.NoDisco
      Drums.Animation a -> Just $ Drums.Animation $ case a of
        -- these were added in RB3
        Drums.Snare Drums.SoftHit hand -> Drums.Snare Drums.HardHit hand
        Drums.Ride Drums.LH -> Drums.Hihat Drums.LH
        Drums.Crash2 hit Drums.LH -> Drums.Crash1 hit Drums.LH
        _ -> a
      x -> Just x
    F.PartGuitar t -> Just $ F.PartGuitar $ fixGB True  t
    F.PartBass   t -> Just $ F.PartBass $ fixGB False t
    F.PartVocals t -> Just $ F.PartVocals $ flip RTB.mapMaybe t $ \case
      Vox.LyricShift -> Nothing
      Vox.RangeShift{} -> Nothing
      x -> Just x
    F.Events     t -> Just $ F.Events $ flip RTB.mapMaybe t $ \case
      Events.PracticeSection _ -> Nothing
      e -> Just e
    F.Beat       t -> Just $ F.Beat t
    F.Venue      _ -> Nothing -- TODO
    _ -> Nothing
  } where
    fixGB hasSolos t = flip RTB.mapMaybe t $ \case
      Five.Tremolo{} -> Nothing
      Five.Trill{} -> Nothing
      Five.Solo{} | not hasSolos -> Nothing
      e -> Just e
    -- for any unison missing an instrument (only has 2 of gtr/bass/drums)
    -- just remove one of the 2 instruments to make it not a unison
    fixUnisons trks = let
      getODTimes f = Set.fromList . ATB.getTimes . RTB.toAbsoluteEventList 0 . RTB.filter f
      gtr = foldr RTB.merge RTB.empty [ t | F.PartGuitar t <- trks ]
      bass = foldr RTB.merge RTB.empty [ t | F.PartBass t <- trks ]
      drum = foldr RTB.merge RTB.empty [ t | F.PartDrums t <- trks ]
      gtrOD = getODTimes (== Five.Overdrive True) gtr
      bassOD = getODTimes (== Five.Overdrive True) bass
      drumOD = getODTimes (== Drums.Overdrive True) drum
      gb = Set.difference (Set.intersection gtrOD bassOD) drumOD
      bd = Set.difference (Set.intersection bassOD drumOD) gtrOD
      gd = Set.difference (Set.intersection gtrOD drumOD) bassOD
      removeOD isStart isEnd time trk = case U.trackSplit time trk of
        (before, atafter) -> case U.trackSplitZero atafter of
          (at, after) -> case partition isStart at of
            ([_], at') -> case U.extractFirst (\x -> guard (isEnd x) >> Just ()) after of
              Just (_, after') -> trackGlue time before $ U.trackGlueZero at' after'
              Nothing -> trk -- probably an error though
            _ -> trk
      fixTrack = \case
        F.PartGuitar t -> F.PartGuitar $ foldr
          (removeOD (== Five.Overdrive True) (== Five.Overdrive False))
          t
          (Set.toList gd)
        F.PartBass t -> F.PartBass $ foldr
          (removeOD (== Five.Overdrive True) (== Five.Overdrive False))
          t
          (Set.toList $ Set.union gb bd)
        trk -> trk
      in if RTB.null gtr || RTB.null bass || RTB.null drum
        then trks
        else map fixTrack trks
