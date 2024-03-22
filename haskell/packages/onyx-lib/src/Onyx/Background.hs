{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PatternSynonyms       #-}
module Onyx.Background where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Control.Monad.Random.Class
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
import qualified Onyx.MIDI.Track.Drums.True
import qualified Onyx.MIDI.Track.Events
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
import           Onyx.PhaseShift.Dance            (SMDifficulty (SMChallenge))
import qualified Onyx.PhaseShift.Dance
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
  autoMoods = mapMaybe (\t -> guard (not $ RTB.null t) >> Just (getAutoMoods t))
    [ longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartDrums
    , longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartDrums2x
    , longBlips (\t -> Map.lookup Expert t.drumDifficulties) (.drumGems) opart.onyxPartRealDrumsPS
    , longBlips (\t -> Map.lookup Expert t.tdDifficulties) (.tdGems)  opart.onyxPartTrueDrums
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
    , longNotes (\t -> Map.lookup SMChallenge t.danceDifficulties) (.danceNotes) opart.onyxPartDance
    , longNotes Just (.maniaNotes) opart.onyxPartMania
    ]
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
  :: (MonadRandom m)
  => VenueTarget
  -> Map.Map RB3Instrument F.FlexPartName -- map of target parts to project parts
  -> F.Song (F.OnyxFile U.Beats)
  -> m (VenueTrack U.Beats)
getVenue target partMap ofile = do
  camera <- buildCamera (Map.keys partMap) ofile.s_tracks.onyxCamera
  -- cut off at end mostly since new blips introduced in rb3->rb2 can go past the end event
  let trimEnd = case ofile.s_tracks.onyxEvents.eventsEnd of
        Wait endTime _ _ -> mapTrack $ U.trackTake endTime
        _                -> id
  -- TODO autogen of lighting, camera, etc.
  return
    $ trimEnd
    $ (case target of VenueTargetRB2 -> compileVenueRB2; VenueTargetRB3 -> compileVenueRB3)
    $ mconcat
      [ ofile.s_tracks.onyxVenue
      , buildLighting ofile.s_tracks.onyxLighting
      , camera
      ]
