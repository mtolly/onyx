module Main where

import Development.Shake
import Development.Shake.FilePath
import YAMLTree
import Config
import Audio
import qualified Data.Aeson as A
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)

jammitSearch :: String -> String -> Action String
jammitSearch title artist = do
  Stdout out <- cmd "jammittools -d -T" [title] "-R" [artist]
  return $ case reverse $ words out of
    []        -> ""
    parts : _ -> parts

jammitRules :: Song -> Rules ()
jammitRules s = do
  let jCmd = ["jammittools", "-T", jTitle, "-R", jArtist]
      jTitle  = fromMaybe (_title  s) (_jammitTitle  s)
      jArtist = fromMaybe (_artist s) (_jammitArtist s)
      jSearch = jammitSearch jTitle jArtist
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
      hasDrums <- ('d' `elem`) <$> jSearch
      hasBass <- ('b' `elem`) <$> jSearch
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
        need [untimed]
        case _jammitAudio s of
          Nothing  -> fail "No jammit-audio configuration"
          Just aud -> buildAudio (bimap realToFrac (const untimed) aud) out

oggRules :: Song -> Rules ()
oggRules s =
  forM_ ["jammit", "album"] $ \src -> do
    forM_ ["1p", "2p"] $ \feet -> do
      let dir = "gen" </> src </> feet
      dir </> "audio.ogg" *> \out -> do
        let drums = File $ dir </> "drums.wav"
            bass = File $ dir </> "bass.wav"
            song = File $ dir </> "song.wav"
            audio = Combine Merge $ case _config s of
              Drums -> [drums, song]
              DrumsBass -> [drums, bass, song, Silence 1 0]
        buildAudio audio out
      dir </> "audio.mogg" *> \mogg -> do
        let ogg = mogg -<.> "ogg"
        need [ogg]
        cmd "ogg2mogg" [ogg, mogg]

main :: IO ()
main = do
  yaml <- readYAMLTree "erotomania.yml"
  case A.fromJSON yaml of
    A.Error s -> fail s
    A.Success song -> shakeArgs shakeOptions $ do
      phony "clean" $ cmd "rm -rf gen"
      jammitRules song
      oggRules song
