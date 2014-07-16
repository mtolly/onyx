{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Main where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Classes
import YAMLTree
import Config
import Audio
import qualified Data.Aeson as A
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import Scripts.Main

import qualified Data.DTA.Serialize as D
import qualified Data.DTA.Serialize.RB3 as D
import qualified Data.DTA.Serialize.Magma as Magma
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map

jammitTitle :: Song -> String
jammitTitle s = fromMaybe (_title s) (_jammitTitle s)

jammitArtist :: Song -> String
jammitArtist s = fromMaybe (_artist s) (_jammitArtist s)

jammitSearch :: String -> String -> Action String
jammitSearch title artist = do
  Stdout out <- cmd "jammittools -d -T" [title] "-R" [artist]
  return $ case reverse $ words out of
    []        -> ""
    parts : _ -> parts

jammitRules :: Song -> Rules ()
jammitRules s = do
  let jCmd = ["jammittools", "-T", jTitle, "-R", jArtist]
      jTitle  = jammitTitle s
      jArtist = jammitArtist s
      jSearch = askOracle $ JammitResults (jTitle, jArtist)
  forM_ ["1p", "2p"] $ \feet -> do
    let dir = "gen/jammit" </> feet
    dir </> "drums_untimed.wav" *> \out -> do
      hasDrums <- ('d' `elem`) <$> jSearch
      if hasDrums
        then cmd jCmd "-y d -a" out
        else buildAudio (Silence 2 0) out
    dir </> "bass_untimed.wav" *> \out -> do
      hasBass <- ('b' `elem`) <$> jSearch
      if hasBass
        then cmd jCmd "-y b -a" out
        else buildAudio (Silence 2 0) out
    dir </> "song_untimed.wav" *> \out -> do
      has <- jSearch
      let hasDrums = 'd' `elem` has
          hasBass  = 'b' `elem` has
      case _config s of
        Drums -> cmd jCmd "-y D -a" out
        DrumsBass -> case (hasDrums, hasBass) of
          (True , True ) -> cmd jCmd "-y D -n b -a" out
          (True , False) -> cmd jCmd "-y D -a" out
          (False, True ) -> cmd jCmd "-y B -a" out
          (False, False) -> fail "Couldn't find Jammit drums or bass"
    forM_ ["drums", "bass", "song"] $ \part -> do
      dir </> (part ++ ".wav") *> \out -> do
        let untimed = dropExtension out ++ "_untimed.wav"
        case _jammitAudio s of
          Nothing  -> fail "No jammit-audio configuration"
          Just aud -> buildAudio (bimap realToFrac (const untimed) aud) out

albumRules :: Song -> Rules ()
albumRules s = do
  forM_ ["1p", "2p"] $ \feet -> do
    let dir = "gen/album" </> feet
    forM_ ["drums.wav", "bass.wav"] $ \inst -> do
      dir </> inst *> buildAudio (Silence 2 0)
      dir </> "song.wav" *> \out -> do
        userAudio <- getDirectoryFiles "" ["audio-album.*"]
        case userAudio of
          [] -> fail "no audio-album.xxx found"
          ua : _ -> do
            need [ua]
            case _albumAudio s of
              Nothing  -> fail "No album-audio configuration"
              Just aud -> buildAudio (bimap realToFrac (const ua) aud) out

countinRules :: Rules ()
countinRules = do
  forM_ ["jammit", "album"] $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      dir </> "countin.wav" *> \out -> do
        let mid = dir </> "notes.mid"
            hit = "../../sound/hihat-foot.wav"
        makeCountin mid hit out
      dir </> "song-countin.wav" *> \out -> do
        let song = File $ dir </> "song.wav"
            countin = File $ dir </> "countin.wav"
        buildAudio (Combine Mix [song, countin]) out

oggRules :: Song -> Rules ()
oggRules s =
  forM_ ["jammit", "album"] $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      dir </> "audio.ogg" *> \out -> do
        let drums = File $ dir </> "drums.wav"
            bass  = File $ dir </> "bass.wav"
            song  = File $ dir </> "song-countin.wav"
            audio = Combine Merge $ case _config s of
              Drums     -> [drums, song]
              DrumsBass -> [drums, bass, song, Silence 1 0]
              -- the Silence is to work around oggenc bug
              -- (it assumes 6 channels is 5.1 surround with lfe channel)
        buildAudio audio out
      dir </> "audio.mogg" *> \mogg -> do
        let ogg = mogg -<.> "ogg"
        need [ogg]
        cmd "ogg2mogg" [ogg, mogg]

midRules :: Rules ()
midRules = forM_ ["jammit", "album"] $ \src -> do
  let mid1p = "gen" </> src </> "1p/notes.mid"
      mid2p = "gen" </> src </> "2p/notes.mid"
  mid1p *> \out -> do
    need ["notes.mid"]
    let tempos = "tempo-" ++ src ++ ".mid"
    b <- doesFileExist tempos
    if b
      then replaceTempos "notes.mid" tempos out
      else fixResolution "notes.mid" out
  mid2p *> \out -> make2xBassPedal mid1p out

newtype JammitResults = JammitResults (String, String)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

main :: IO ()
main = do
  yaml <- readYAMLTree "erotomania.yml"
  case A.fromJSON yaml of
    A.Error s -> fail s
    A.Success song -> shakeArgs shakeOptions $ do
      _ <- addOracle $ \(JammitResults (title, artist)) ->
        jammitSearch title artist
      phony "clean" $ cmd "rm -rf gen"
      midRules
      jammitRules song
      albumRules song
      countinRules
      oggRules song

makeDTA :: FilePath -> Song -> Action D.SongPackage
makeDTA mid s = do
  (pstart, pend) <- previewBounds mid
  len <- songLength mid
  return D.SongPackage
    { D.name = B8.pack $ _title s
    , D.artist = B8.pack $ _artist s
    , D.master = True
    , D.songId = Right $ D.Keyword $ B8.pack $ _package s
    , D.song = D.Song
      { D.songName = B8.pack $ "songs/" ++ _package s ++ "/" ++ _package s
      , D.tracksCount = Just $ D.InParens [2, if _config s == DrumsBass then 2 else 0, 0, 0, 0, 2]
      , D.tracks = D.InParens $ D.Dict $ Map.fromList $ let
        trackDrum = (B8.pack "drum", Right $ D.InParens [0, 1])
        trackBass = (B8.pack "bass", Right $ D.InParens [2, 3])
        in case _config s of
          Drums -> [trackDrum]
          DrumsBass -> [trackDrum, trackBass]
      , D.vocalParts = 0
      , D.pans = D.InParens $ case _config s of
        Drums -> [-1, 1, -1, 1]
        DrumsBass -> [-1, 1, -1, 1, -1, 1]
      , D.vols = D.InParens $ case _config s of
        Drums -> replicate 4 0
        DrumsBass -> replicate 6 0
      , D.cores = D.InParens $ case _config s of
        Drums -> replicate 4 (-1)
        DrumsBass -> replicate 6 (-1)
      , D.drumSolo = D.DrumSounds $ D.InParens $ map (D.Keyword . B8.pack) $ words
        "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
      , D.drumFreestyle = D.DrumSounds $ D.InParens $ map (D.Keyword . B8.pack) $ words
        "kick.cue snare.cue hat.cue ride.cue crash.cue"
      }
    , D.bank = Just $ Left $ B8.pack "sfx/tambourine_bank.milo"
    , D.drumBank = Nothing
    , D.animTempo = Left D.KTempoMedium
    , D.bandFailCue = Nothing
    , D.songScrollSpeed = 2300
    , D.preview = (fromIntegral pstart, fromIntegral pend)
    , D.songLength = fromIntegral len
    , D.rank = D.Dict $ Map.fromList
      [ (B8.pack "drum", 1)
      , (B8.pack "guitar", 0)
      , (B8.pack "bass", if _config s == DrumsBass then 1 else 0)
      , (B8.pack "vocals", 0)
      , (B8.pack "keys", 0)
      , (B8.pack "real_keys", 0)
      , (B8.pack "band", 1)
      ]
    , D.solo = Nothing
    , D.format = 10
    , D.version = 30
    , D.gameOrigin = D.Keyword $ B8.pack "ugc_plus"
    , D.rating = 4
    , D.genre = D.Keyword $ B8.pack $ _genre s
    , D.subGenre = Just $ D.Keyword $ B8.pack $ "subgenre_" ++ _subgenre s
    , D.vocalGender = case _vocalGender s of
      Male -> Magma.Male
      Female -> Magma.Female
    , D.shortVersion = Nothing
    , D.yearReleased = fromIntegral $ _year s
    , D.albumArt = Just True
    , D.albumName = Just $ B8.pack $ _album s
    , D.albumTrackNumber = Just $ fromIntegral $ _trackNumber s
    , D.vocalTonicNote = Nothing
    , D.songTonality = Nothing
    , D.tuningOffsetCents = Just 0
    , D.realGuitarTuning = Nothing
    , D.realBassTuning = Nothing
    , D.guidePitchVolume = Just (-3)
    , D.encoding = Just $ D.Keyword $ B8.pack "latin1"
    }
