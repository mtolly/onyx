{-# LANGUAGE LambdaCase #-}
module GuitarHeroII.Convert where

import           Config
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified RockBand.File                    as F
import           RockBand.PhaseShiftMessage       (discardPS)
import qualified Sound.MIDI.Util                  as U

import           RockBand.Common                  (Difficulty (..), Mood (..))
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
  events = flip RTB.mapMaybe (discardPS $ F.onyxEvents onyx) $ \case
    RBEv.MusicStart -> Just Ev.MusicStart
    RBEv.End -> Just Ev.End
    RBEv.PracticeSection t -> Just $ Ev.PracticeSection t
    _ -> Nothing
  triggers = flip RTB.mapMaybe (discardPS $ F.onyxEvents onyx) $ \case
    RBEv.PracticeKick -> Just Tr.PracticeKick
    RBEv.PracticeSnare -> Just Tr.PracticeSnare
    RBEv.PracticeHihat -> Just Tr.PracticeHihat
    _ -> Nothing
  makeGRYBO fpart = case getPart fpart song >>= partGRYBO of
    Nothing -> RTB.empty
    Just grybo -> let
      flex = F.getFlexPart fpart onyx
      toGtr = if F.flexFiveIsKeys flex then RB5.keysToGuitar (fromIntegral (gryboHopoThreshold grybo) / 480) else id
      in makePartGuitar $ toGtr $ discardPS $ F.flexFiveButton flex
  gh2 = F.GH2File
    { F.gh2PartGuitar     = makeGRYBO $ gh2_Guitar target
    , F.gh2PartBass       = case gh2_Coop target of
      GH2Rhythm -> RTB.empty
      GH2Bass   -> makeGRYBO $ gh2_Bass target
    , F.gh2PartRhythm     = case gh2_Coop target of
      GH2Rhythm -> makeGRYBO $ gh2_Rhythm target
      GH2Bass   -> RTB.empty
    , F.gh2PartGuitarCoop = RTB.empty
    , F.gh2BandBass       = makeBandBass $ discardPS $ F.flexFiveButton $ F.getFlexPart (gh2_Bass target) onyx
    , F.gh2BandDrums      = makeBandDrums $ discardPS $ F.flexPartDrums $ F.getFlexPart (gh2_Drums target) onyx
    , F.gh2BandKeys       = makeBandKeys $ discardPS $ F.flexFiveButton $ F.getFlexPart (gh2_Keys target) onyx
    , F.gh2BandSinger     = makeBandSinger $ discardPS $ F.flexPartVocals $ F.getFlexPart (gh2_Vocal target) onyx
    , F.gh2Events         = events
    , F.gh2Triggers       = triggers
    }
  in F.Song tmap mmap gh2

makeGH2DTA :: SongYaml -> TargetGH2 -> D.SongPackage
makeGH2DTA song target = D.SongPackage
  { D.name = getTitle $ _metadata song
  , D.artist = getArtist $ _metadata song
  , D.song = undefined
  , D.animTempo = KTempoMedium
  , D.preview = undefined
  , D.quickplay = undefined
  , D.practiceSpeeds = undefined
  , D.songCoop = undefined
  , D.songPractice1 = undefined
  , D.songPractice2 = undefined
  , D.songPractice3 = undefined
  , D.band = undefined
  }
