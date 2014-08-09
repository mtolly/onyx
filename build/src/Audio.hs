{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Audio where

import Development.Shake (cmd, Action, need, liftIO)
import Development.Shake.FilePath (takeExtension)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import Numeric (showFFloat)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Char (toLower)
import qualified Data.Aeson as A
import Text.Read (readEither)
import Data.Bifunctor
import Control.Monad (forM)

data Edge = Begin | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio t a
  = Silence Int t
  | File a
  | Combine Combine [Audio t a]
  | Combine' Combine [(Audio t a, Double)]
  | Unary [Unary t] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, T.Traversable)

data Combine = Concatenate | Mix | Merge
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Unary t
  = Trim Edge t
  | Fade Edge t
  | Pad Edge t
  | Take Edge t
  deriving (Eq, Ord, Show, Read, Functor)

mapTime :: (t -> u) -> Audio t a -> Audio u a
mapTime f aud = case aud of
  Silence n t     -> Silence n $ f t
  File x          -> File x
  Combine c auds  -> Combine c $ map (first f) auds
  Combine' c auds -> Combine' c $ map (first $ first f) auds
  Unary uns aud'  -> Unary (map (fmap f) uns) $ first f aud'

instance Bifunctor Audio where
  first = mapTime
  second = fmap

instance (Read t, Read a) => A.FromJSON (Audio t a) where
  parseJSON v = A.parseJSON v >>= either fail return . readEither

instance (Show t, Show a) => A.ToJSON (Audio t a) where
  toJSON = A.toJSON . show

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
    File x -> case takeExtension x of
      ".mp3" -> do
        f <- newWav
        () <- cmd "lame --decode" [x, f]
        evalAudio $ File f
      _ -> do
        f <- newWav
        () <- cmd "sox" [x, f] "rate 44100 channels 2"
        return f
    Combine comb xs -> evalAudio $ Combine' comb $ map (\x -> (x, 1)) xs
    Combine' _ [] -> fail "buildAudio: can't combine 0 files"
    Combine' comb xs -> do
      fxs <- fmap concat $ forM xs $ \(x, vol) -> do
        a <- evalAudio x
        return ["-v", showFFloat (Just 4) vol "", a]
      f <- newWav
      let comb' = case xs of
            _ : _ : _ -> "--combine " ++ map toLower (show comb)
            _         -> ""
      () <- cmd "sox" comb' fxs f
      return f
    Unary uns x -> do
      a <- evalAudio x
      f <- newWav
      () <- cmd "sox" [a, f] $ uns >>= \un -> case un of
        Trim Begin t -> ["trim", showSeconds t]
        Trim End   t -> ["trim", "0", '-' : showSeconds t]
        Fade Begin t -> ["fade", "t", showSeconds t]
        Fade End   t -> ["fade", "t", "0", "0", showSeconds t]
        Pad  Begin t -> ["pad", showSeconds t]
        Pad  End   t -> ["pad", "0", showSeconds t]
        Take Begin t -> ["trim", "0", showSeconds t]
        Take End   t -> ["trim", '-' : showSeconds t]
      return f
  showSeconds r = showFFloat (Just 4) (realToFrac r :: Double) ""
  in do
    need $ F.toList aud
    liftIO $ createDirectoryIfMissing True dir
    f <- evalAudio aud
    cmd "sox" [f] [out]
