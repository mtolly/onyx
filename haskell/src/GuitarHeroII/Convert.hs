{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Convert where

import           Config
import           Control.Monad                    (guard)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Guitars
import qualified RockBand.File                    as F
import qualified Sound.MIDI.Util                  as U

import           RockBand.Common                  (Difficulty (..), Mood (..),
                                                   eachDifficulty)
import qualified RockBand.Drums                   as RBD
import qualified RockBand.Events                  as RBEv
import qualified RockBand.FiveButton              as RB5
import qualified RockBand.Vocals                  as RBV

import qualified GuitarHeroII.BandBass            as BB
import qualified GuitarHeroII.BandDrums           as BD
import qualified GuitarHeroII.BandKeys            as BK
import qualified GuitarHeroII.BandSinger          as BS
import qualified GuitarHeroII.Events              as Ev
import qualified GuitarHeroII.PartGuitar          as PG
import qualified GuitarHeroII.Triggers            as Tr

import qualified Data.DTA.Serialize.GH2           as D
import           Data.DTA.Serialize.RB3           (AnimTempo (..))

midiRB3toGH2
  :: SongYaml
  -> TargetGH2
  -> F.Song (F.OnyxFile U.Beats)
  -> F.Song (F.GH2File U.Beats)
midiRB3toGH2 song target (F.Song tmap mmap onyx) = let
  makeMood mood idle play = case mood of
    Mood_idle_realtime -> idle
    Mood_idle          -> idle
    Mood_idle_intense  -> idle
    Mood_play          -> play
    Mood_mellow        -> play
    Mood_intense       -> play
    Mood_play_solo     -> play
  makePartGuitar trk = RTB.flatten $ flip fmap trk $ \case
    RB5.DiffEvent diff (RB5.Note lnote) -> [PG.DiffEvent diff $ PG.Note lnote]
    RB5.Player1 b -> [ PG.DiffEvent diff $ PG.Player1 b | diff <- [Easy .. Expert] ]
    RB5.Player2 b -> [ PG.DiffEvent diff $ PG.Player2 b | diff <- [Easy .. Expert] ]
    RB5.Overdrive b -> [ PG.DiffEvent diff $ PG.StarPower b | diff <- [Easy .. Expert] ]
    RB5.Mood mood -> [makeMood mood PG.Idle PG.Play]
    RB5.HandMap hm -> case hm of
      RB5.HandMap_Default   -> [PG.HandMapDefault]
      RB5.HandMap_NoChords  -> [PG.HandMapNoChords]
      RB5.HandMap_AllChords -> [PG.HandMapDefault]
      RB5.HandMap_Solo      -> [PG.HandMapSolo]
      RB5.HandMap_DropD     -> [PG.HandMapDropD2]
      RB5.HandMap_DropD2    -> [PG.HandMapDropD2]
      RB5.HandMap_AllBend   -> [PG.HandMapSolo]
      RB5.HandMap_Chord_C   -> [PG.HandMapDefault]
      RB5.HandMap_Chord_D   -> [PG.HandMapDefault]
      RB5.HandMap_Chord_A   -> [PG.HandMapDefault]
    _ -> []
  makeBandBass trk = flip RTB.mapMaybe trk $ \case
    RB5.Mood mood -> Just $ makeMood mood BB.Idle BB.Play
    RB5.DiffEvent Expert RB5.Note{} -> Just BB.Strum
    _ -> Nothing
  makeBandDrums trk = flip RTB.mapMaybe trk $ \case
    RBD.Animation anim -> case anim of
      RBD.KickRF          -> Just BD.Kick
      RBD.Crash1{}        -> Just BD.Crash
      RBD.Crash2{}        -> Just BD.Crash
      RBD.Crash1RHChokeLH -> Just BD.Crash
      RBD.Crash2RHChokeLH -> Just BD.Crash
      _                   -> Nothing
    RBD.Mood mood -> Just $ makeMood mood BD.Idle BD.Play
    _ -> Nothing
  makeBandKeys trk = flip RTB.mapMaybe trk $ \case
    RB5.Mood mood -> Just $ makeMood mood BK.Idle BK.Play
    _ -> Nothing
  makeBandSinger trk = flip RTB.mapMaybe trk $ \case
    RBV.Mood mood -> Just $ makeMood mood BS.Idle BS.Play
    _ -> Nothing
  events = flip RTB.mapMaybe (F.onyxEvents onyx) $ \case
    RBEv.MusicStart -> Just Ev.MusicStart
    RBEv.End -> Just Ev.End
    RBEv.SectionRB3 t -> Just $ Ev.PracticeSection t
    RBEv.SectionRB2 t -> Just $ Ev.PracticeSection t
    _ -> Nothing
  triggers = flip RTB.mapMaybe (F.onyxEvents onyx) $ \case
    RBEv.PracticeKick -> Just Tr.PracticeKick
    RBEv.PracticeSnare -> Just Tr.PracticeSnare
    RBEv.PracticeHihat -> Just Tr.PracticeHihat
    _ -> Nothing
  makeGRYBO fpart = case getPart fpart song >>= partGRYBO of
    Nothing -> RTB.empty
    Just grybo -> let
      flex = F.getFlexPart fpart onyx
      toGtr = eachDifficulty
        $ emit5
        . fromClosed
        . noOpenNotes False
        . noExtendedSustains standardBlipThreshold standardSustainGap
        . strumHOPOTap
          (if F.flexFiveIsKeys flex then HOPOsRBKeys else HOPOsRBGuitar)
          (fromIntegral (gryboHopoThreshold grybo) / 480)
        . closeNotes
      in makePartGuitar $ toGtr $ F.flexFiveButton flex
  gh2 = F.GH2File
    { F.gh2PartGuitar     = makeGRYBO $ gh2_Guitar target
    , F.gh2PartBass       = case gh2_Coop target of
      GH2Rhythm -> RTB.empty
      GH2Bass   -> makeGRYBO $ gh2_Bass target
    , F.gh2PartRhythm     = case gh2_Coop target of
      GH2Rhythm -> makeGRYBO $ gh2_Rhythm target
      GH2Bass   -> RTB.empty
    , F.gh2PartGuitarCoop = RTB.empty
    , F.gh2BandBass       = makeBandBass $ F.flexFiveButton $ F.getFlexPart (gh2_Bass target) onyx
    , F.gh2BandDrums      = makeBandDrums $ F.flexPartDrums $ F.getFlexPart (gh2_Drums target) onyx
    , F.gh2BandKeys       = makeBandKeys $ F.flexFiveButton $ F.getFlexPart (gh2_Keys target) onyx
    , F.gh2BandSinger     = makeBandSinger $ F.flexPartVocals $ F.getFlexPart (gh2_Vocal target) onyx
    , F.gh2Events         = events
    , F.gh2Triggers       = triggers
    }
  in F.Song tmap mmap gh2

makeGH2DTA :: SongYaml -> (Int, Int) -> TargetGH2 -> D.SongPackage
makeGH2DTA song preview target = D.SongPackage
  { D.name = getTitle $ _metadata song
  , D.artist = getArtist $ _metadata song
  , D.caption = guard (not $ _cover $ _metadata song) >> Just "performed_by"
  , D.song = D.Song
    { D.songName = "songs/$SONGKEY/$SONGKEY"
    , D.tracks   = HM.fromList [("guitar", [2, 3]), (coop, [4, 5])]
    , D.pans     = [-1, 1, -1, 1, -1, 1]
    , D.vols     = [0, 0, 0, 0, 0, 0]
    , D.cores    = [-1, -1, 1, 1, -1, -1]
    , D.midiFile = "songs/$SONGKEY/$SONGKEY.mid"
    }
  , D.animTempo = KTempoMedium
  , D.preview = (fromIntegral $ fst preview, fromIntegral $ snd preview)
  , D.quickplay = gh2_Quickplay target
  , D.practiceSpeeds = [100, 90, 75, 60]
  , D.songCoop = Nothing
  , D.songPractice1 = prac 90
  , D.songPractice2 = prac 75
  , D.songPractice3 = prac 60
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
      }
