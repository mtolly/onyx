{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Main where

import Development.Shake
--import Development.Shake.FilePath
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import Numeric (showFFloat)
import qualified Data.Foldable as F

data Edge = Begin | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio t a
  = Silence t
  | File a
  | Concat (Audio t a) (Audio t a)
  | Mix (Audio t a) (Audio t a)
  | Trim Edge t (Audio t a)
  | Fade Edge t (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable)

-- | Assumes 16-bit 44100 Hz stereo audio files.
buildAudio :: Audio Rational FilePath -> FilePath -> Action ()
buildAudio aud out = let
  dir = "gen/temp"
  newWav :: Action FilePath
  newWav = liftIO $ do
    (f, h) <- openTempFile dir "audio.wav"
    hClose h
    return f
  evalAudio :: Audio Rational FilePath -> Action FilePath
  evalAudio expr = case expr of
    Silence t -> do
      f <- newWav
      () <- cmd "sox -n -b 16" [f] "rate 44100 channels 2 trim 0" [showSeconds t]
      return f
    File x -> return x
    Concat (Silence t) x -> do
      fx <- evalAudio x
      f <- newWav
      () <- cmd "sox" [fx, f] "pad" [showSeconds t]
      return f
    Concat x (Silence t) -> do
      fx <- evalAudio x
      f <- newWav
      () <- cmd "sox" [fx, f] "pad 0" [showSeconds t]
      return f
    Concat x y -> do
      fx <- evalAudio x
      fy <- evalAudio y
      f <- newWav
      () <- cmd "sox --combine concatenate" [fx, fy, f]
      return f
    Mix x y -> do
      fx <- evalAudio x
      fy <- evalAudio y
      f <- newWav
      () <- cmd "sox --combine mix" [fx, fy, f]
      return f
    Trim edge t x -> do
      fx <- evalAudio x
      f <- newWav
      () <- case edge of
        Begin -> cmd "sox" [fx, f] "trim" [showSeconds t]
        End   -> cmd "sox" [fx, f] "trim 0" ['-' : showSeconds t]
      return f
    Fade edge t x -> do
      fx <- evalAudio x
      f <- newWav
      () <- case edge of
        Begin -> cmd "sox" [fx, f] "fade t" [showSeconds t]
        End   -> cmd "sox" [fx, f] "fade t 0 0" [showSeconds t]
      return f
  showSeconds r = showFFloat (Just 4) (realToFrac r :: Double) ""
  in do
    need $ F.toList aud
    liftIO $ createDirectoryIfMissing True dir
    f <- evalAudio aud
    liftIO $ copyFile f out

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
    buildAudio (Concat (Silence 1.193) (File untimed)) out
