{-# LANGUAGE MultiWayIf #-}
module RockBand2 (convertCON, convertMIDI) where

import qualified Data.DTA.Serialize.RB3 as RB3
import qualified Data.DTA as D
import qualified Data.DTA.Serialize as D
import qualified Data.Map as Map
import qualified RockBand.File as F
import Data.Maybe (mapMaybe)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified RockBand.Drums as Drums
import qualified RockBand.Vocals as Vox
import qualified RockBand.FiveButton as Five
import qualified RockBand.Events as Events
import qualified Sound.MIDI.Util as U
import System.IO.Temp (withSystemTempDirectory)
import STFS.Extract (extractSTFS)
import System.FilePath ((</>), (<.>))
import System.Directory (getDirectoryContents)
import X360 (rb2pkg)
import Development.Shake (shake, shakeOptions, action)
import Import (readRB3DTA)
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import Control.Monad (forM_)
import Control.Monad.Trans.StackTrace (printStackTraceIO)

convertCON :: FilePath -> FilePath -> IO ()
convertCON rb3 rb2 = withSystemTempDirectory "onyx_rb2" $ \temp -> do
  extractSTFS rb3 temp
  let pathDTA = temp </> "songs/songs.dta"
  (sname, dta, _) <- readRB3DTA pathDTA
  D.writeFileDTA_latin1 pathDTA $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Key sname : D.toChunks (convertDTA dta)))]
  dirs <- fmap (filter (`notElem` [".", "..", "songs.dta"])) $ getDirectoryContents $ temp </> "songs"
  forM_ dirs $ \song -> do
    let mpath = temp </> "songs" </> song </> song <.> "mid"
    mid <- Load.fromFile mpath >>= printStackTraceIO . F.readMIDIFile
    Save.toFile mpath $ F.showMIDIFile $ convertMIDI mid
  shake shakeOptions $ action $ rb2pkg ("RB3to2: " ++ show dirs) "Converted with onyx" temp rb2

convertMIDI :: F.Song U.Beats -> F.Song U.Beats
convertMIDI mid = mid
  { F.s_tracks = flip mapMaybe (F.s_tracks mid) $ \case
    F.PartDrums  t -> Just $ F.PartDrums $ flip RTB.mapMaybe t $ \case
      Drums.ProType{} -> Nothing
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

convertDTA :: RB3.SongPackage -> RB3.SongPackage
convertDTA rb3 = rb3
  { RB3.drumBank = Nothing
  , RB3.bandFailCue = Nothing -- ?
  , RB3.format = 4
  , RB3.version = 0 -- ?
  , RB3.gameOrigin = D.Keyword "rb2"
  , RB3.shortVersion = Nothing -- ?
  , RB3.decade = Just $ D.Keyword $ if
    | 1960 <= year && year < 1970 -> "the60s"
    | 1970 <= year && year < 1980 -> "the70s"
    | 1980 <= year && year < 1990 -> "the80s"
    | 1990 <= year && year < 2000 -> "the90s"
    | 2000 <= year && year < 2010 -> "the00s"
    | 2010 <= year && year < 2020 -> "the10s"
    | otherwise -> "the10s" -- meh
  , RB3.context = Just 2000 -- TODO what is this?
  , RB3.downloaded = Just True
  , RB3.basePoints = Just 0
  , RB3.encoding = Nothing -- ?
  , RB3.vocalTonicNote = Nothing
  , RB3.songTonality = Nothing
  , RB3.realGuitarTuning = Nothing
  , RB3.realBassTuning = Nothing
  , RB3.guidePitchVolume = Nothing -- ?
  , RB3.song = song
    { RB3.tracksCount = Nothing -- TODO see if this is necessary
    , RB3.tracks = undefined -- TODO
    , RB3.vocalParts = Nothing
    , RB3.midiFile = Just $ RB3.songName song ++ ".mid"
    }
  , RB3.solo = Nothing -- dunno if this is even used pre-rb3
  , RB3.rank = D.Dict $ Map.filterWithKey (\k _ -> k `elem` words "guitar bass drum vocals band") $ D.fromDict $ RB3.rank rb3
  } where
    year = RB3.yearReleased rb3
    song = RB3.song rb3
