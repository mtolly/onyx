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

countinRules :: Rules ()
countinRules = do
  forM_ ["jammit", "album"] $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      dir </> "countin.wav" *> \out -> do
        let mid = dir </> "notes.mid"
            hit = "../../sound/hihat-foot.wav"
        need [mid, hit]
        cmd "../../scripts/countin" [mid, hit, out]
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
      then do
        need [tempos]
        cmd "../../scripts/replace-tempos" ["notes.mid", tempos, out]
      else do
        copyFile' "notes.mid" out
        cmd ["../../scripts/fix-resolution", out]
  mid2p *> \out -> do
    need [mid1p]
    cmd "../../scripts/2x-bass-pedal" [mid1p, out]

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
      countinRules
      oggRules song
