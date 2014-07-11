{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
module Main where

import Development.Shake
--import Development.Shake.FilePath
import System.Directory (copyFile, createDirectoryIfMissing)
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import Numeric (showFFloat)
import qualified Data.Foldable as F
import Data.Char (toLower)
import JSONLink

data Edge = Begin | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio t a
  = Silence Int t
  | File a
  | Combine Combine [Audio t a]
  | Unary [Unary t] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable)

data Combine = Concatenate | Mix | Merge
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Unary t
  = Trim Edge t
  | Fade Edge t
  | Pad Edge t
  deriving (Eq, Ord, Show, Read)

-- | Assumes 16-bit 44100 Hz audio files.
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
    Silence chans t -> do
      f <- newWav
      () <- cmd "sox -n -b 16" [f] "rate 44100 channels" [show chans]
        "trim 0" [showSeconds t]
      return f
    File x -> return x
    Combine _ [] -> error "buildAudio: can't combine 0 files"
    Combine _ [x] -> evalAudio x
    Combine comb xs -> do
      fxs <- mapM evalAudio xs
      let fxs' = fxs >>= \fx -> ["-v", "1", fx]
      f <- newWav
      () <- cmd "sox --combine" [map toLower $ show comb] fxs' f
      return f
    Unary uns x -> do
      fx <- evalAudio x
      f <- newWav
      () <- cmd "sox" [fx, f] $ uns >>= \un -> case un of
        Trim Begin t -> ["trim", showSeconds t]
        Trim End   t -> ["trim", "0", '-' : showSeconds t]
        Fade Begin t -> ["fade", "t", showSeconds t]
        Fade End   t -> ["fade", "t", "0", "0", showSeconds t]
        Pad  Begin t -> ["pad", showSeconds t]
        Pad  End   t -> ["pad", "0", showSeconds t]
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
    buildAudio (Unary [Pad Begin 1.193] $ File untimed) out
