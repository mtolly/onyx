{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Convert where

import           Config
import           Control.Monad                    (guard)
import qualified Data.DTA.Serialize.GH2           as D
import           Data.DTA.Serialize.RB3           (AnimTempo (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           GuitarHeroII.BandBass
import           GuitarHeroII.BandDrums
import           GuitarHeroII.BandKeys
import           GuitarHeroII.BandSinger
import           GuitarHeroII.Events
import           GuitarHeroII.File
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           Guitars
import qualified RockBand.Codec.Drums             as RB
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as F
import qualified RockBand.Codec.Five              as RB
import qualified RockBand.Codec.Vocal             as RB
import           RockBand.Common                  (Difficulty (..), Mood (..))
import           RockBand.Sections                (makeGH2Section)
import qualified Sound.MIDI.Util                  as U

midiRB3toGH2
  :: SongYaml f
  -> TargetGH2
  -> F.Song (F.OnyxFile U.Beats)
  -> F.Song (GH2File U.Beats)
midiRB3toGH2 song target (F.Song tmap mmap onyx) = let
  makeMoods moods = let
    bools = flip fmap moods $ \case
      Mood_idle_realtime -> False
      Mood_idle          -> False
      Mood_idle_intense  -> False
      Mood_play          -> True
      Mood_mellow        -> True
      Mood_intense       -> True
      Mood_play_solo     -> True
    in (const () <$> RTB.filter not bools, const () <$> RTB.filter id bools) -- (idle, play)
  makePartGuitar rbg = mempty
    { partDifficulties = flip fmap (RB.fiveDifficulties rbg) $ \fdiff -> PartDifficulty
      { partStarPower = RB.fiveOverdrive rbg
      , partPlayer1   = RB.fivePlayer1 rbg
      , partPlayer2   = RB.fivePlayer2 rbg
      , partGems      = RB.fiveGems fdiff
      }
    , partFretPosition = RB.fiveFretPosition rbg
    , partIdle         = idle
    , partPlay         = play
    , partHandMap      = flip fmap (RB.fiveHandMap rbg) $ \case
      RB.HandMap_Default   -> HandMap_Default
      RB.HandMap_NoChords  -> HandMap_NoChords
      RB.HandMap_AllChords -> HandMap_Default
      RB.HandMap_Solo      -> HandMap_Solo
      RB.HandMap_DropD     -> HandMap_DropD2
      RB.HandMap_DropD2    -> HandMap_DropD2
      RB.HandMap_AllBend   -> HandMap_Solo
      RB.HandMap_Chord_C   -> HandMap_Default
      RB.HandMap_Chord_D   -> HandMap_Default
      RB.HandMap_Chord_A   -> HandMap_Default
    } where (idle, play) = makeMoods $ RB.fiveMood rbg
  makeGRYBO fpart = case getPart fpart song >>= partGRYBO of
    Nothing -> mempty
    Just grybo -> let
      src = F.getFlexPart fpart onyx
      (trackOrig, algo) = getFive src
      gap = fromIntegral (gryboSustainGap grybo) / 480
      ht = gryboHopoThreshold grybo
      fiveEachDiff f ft = ft { RB.fiveDifficulties = fmap f $ RB.fiveDifficulties ft }
      toGtr = fiveEachDiff $ \fd ->
          emit5'
        . fromClosed'
        . no5NoteChords'
        . noOpenNotes'
        . noTaps'
        . noExtendedSustains' standardBlipThreshold gap
        . applyForces (getForces5 fd)
        . strumHOPOTap' algo (fromIntegral ht / 480)
        . fixSloppyNotes (10 / 480)
        . closeNotes'
        $ fd
      in makePartGuitar $ toGtr trackOrig
  makeBandBass trk = mempty
    { bassIdle  = idle
    , bassPlay  = play
    , bassStrum
      = fmap (const ())
      . RTB.collectCoincident
      . RB.fiveGems
      . fromMaybe mempty
      . Map.lookup Expert
      $ RB.fiveDifficulties trk
    } where (idle, play) = makeMoods $ RB.fiveMood trk
  makeBandDrums trk = mempty
    { drumsIdle = idle
    , drumsPlay = play
    , drumsKick  = fmap (const ()) $ flip RTB.filter (RB.drumAnimation trk) $ \case
      RB.KickRF -> True
      _         -> False
    , drumsCrash = fmap (const ()) $ flip RTB.filter (RB.drumAnimation trk) $ \case
      RB.Crash1{}        -> True
      RB.Crash2{}        -> True
      RB.Crash1RHChokeLH -> True
      RB.Crash2RHChokeLH -> True
      _                  -> False
    } where (idle, play) = makeMoods $ RB.drumMood trk
  makeBandKeys trk = let
    (idle, play) = makeMoods $ RB.fiveMood trk
    in mempty { keysIdle = idle, keysPlay = play }
  makeBandSinger trk = let
    (idle, play) = makeMoods $ RB.vocalMood trk
    in mempty { singerIdle = idle, singerPlay = play }
  events = mempty
    { eventsSections      = fmap (makeGH2Section . snd)
      $ RB.eventsSections $ F.onyxEvents onyx
    , eventsOther         = foldr RTB.merge RTB.empty
      [ fmap (const MusicStart) $ RB.eventsMusicStart $ F.onyxEvents onyx
      , fmap (const End) $ RB.eventsEnd $ F.onyxEvents onyx
      ]
    }
  triggers = mempty
    { triggersBacking = RB.eventsBacking $ F.onyxEvents onyx
    }
  gh2 = GH2File
    { gh2PartGuitar     = makeGRYBO $ gh2_Guitar target
    , gh2PartBass       = case gh2_Coop target of
      GH2Rhythm -> mempty
      GH2Bass   -> makeGRYBO $ gh2_Bass target
    , gh2PartRhythm     = case gh2_Coop target of
      GH2Rhythm -> makeGRYBO $ gh2_Rhythm target
      GH2Bass   -> mempty
    , gh2PartGuitarCoop = mempty
    , gh2BandBass       = makeBandBass $ fst $ getFive $ F.getFlexPart (gh2_Bass target) onyx
    , gh2BandDrums      = makeBandDrums $ F.onyxPartDrums $ F.getFlexPart (gh2_Drums target) onyx
    , gh2BandKeys       = makeBandKeys $ fst $ getFive $ F.getFlexPart (gh2_Keys target) onyx
    , gh2BandSinger     = makeBandSinger $ F.onyxPartVocals $ F.getFlexPart (gh2_Vocal target) onyx
    , gh2Events         = events
    , gh2Triggers       = triggers
    }
  getFive = F.selectGuitarTrack F.FiveTypeGuitar
  in F.Song tmap mmap gh2

makeGH2DTA :: SongYaml f -> (Int, Int) -> TargetGH2 -> D.SongPackage
makeGH2DTA song preview target = D.SongPackage
  { D.name = getTitle $ _metadata song
  , D.artist = getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName      = "songs/$SONGKEY/$SONGKEY"
    , D.tracks        = HM.fromList [("guitar", [2, 3]), (coop, [4, 5])]
    , D.pans          = [-1, 1, -1, 1, -1, 1]
    , D.vols          = [0, 0, 0, 0, 0, 0]
    , D.cores         = [-1, -1, 1, 1, -1, -1]
    , D.midiFile      = "songs/$SONGKEY/$SONGKEY.mid"
    , D.hopoThreshold = Nothing
    }
  , D.animTempo = KTempoMedium
  , D.preview = (fromIntegral $ fst preview, fromIntegral $ snd preview)
  , D.quickplay = gh2_Quickplay target
  , D.practiceSpeeds = Just [100, 90, 75, 60]
  , D.songCoop = Nothing
  , D.songPractice1 = Just $ prac 90
  , D.songPractice2 = Just $ prac 75
  , D.songPractice3 = Just $ prac 60
  , D.band = Nothing -- TODO
  } where
    coop = case gh2_Coop target of GH2Bass -> "bass"; GH2Rhythm -> "rhythm"
    prac :: Int -> D.Song
    prac speed = D.Song
      { D.songName = "songs/$SONGKEY/$SONGKEY_p" <> T.pack (show speed)
      , D.tracks = HM.fromList [(coop, [0]), ("guitar", [1])]
      , D.pans = [0, 0]
      , D.vols = [0, 0]
      , D.cores = [-1, -1]
      , D.midiFile = "songs/$SONGKEY/$SONGKEY.mid"
      , D.hopoThreshold = Nothing
      }
