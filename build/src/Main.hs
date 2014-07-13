module Main where

import Development.Shake
import Development.Shake.FilePath
import YAMLTree
import Config
import Audio
import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Applicative ((<$>))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

eachPedal :: (Monad m) => (String -> m ()) -> m ()
eachPedal = forM_ ["1p", "2p"]

eachAudio :: (Monad m) => (String -> m ()) -> m ()
eachAudio = forM_ ["1p", "2p"]

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
    "gen/jammit" </> feet </> "drums_untimed.wav" *> \out -> do
      hasDrums <- ('d' `elem`) <$> jSearch
      if hasDrums
        then cmd jCmd "-y d -a" out
        else buildAudio (Silence 2 0) out
    "gen/jammit" </> feet </> "bass_untimed.wav" *> \out -> do
      hasBass <- ('b' `elem`) <$> jSearch
      if hasBass
        then cmd jCmd "-y b -a" out
        else buildAudio (Silence 2 0) out

getPart :: String -> String -> Char -> FilePath -> Action ()
getPart title artist p fout =
  cmd ["jammittools", "-a", fout, "-T", title, "-R", artist, "-y", [p]]

main :: IO ()
main = shakeArgs shakeOptions $ do
  phony "clean" $ cmd "rm -rf gen"
  "drums-untimed.wav" *> \out -> do
    getPart "A Mind Beside Itself I. Erotomania" "Dream Theater" 'd' out
  "drums.wav" *> \out -> do
    let untimed = "drums-untimed.wav"
    buildAudio (Unary [Pad Begin 1.193] $ File untimed) out
