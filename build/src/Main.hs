module Main where

import Development.Shake
import YAMLTree
import Config
import Audio

jammitSearch :: String -> String -> Action String
jammitSearch title artist = do
  Stdout out <- cmd "jammittools -d -T" [title] "-R" [artist]
  return $ case reverse $ words out of
    []        -> ""
    parts : _ -> parts

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
