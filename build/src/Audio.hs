{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Audio where

import Development.Shake (cmd, Action, need, liftIO)
import Development.Shake.FilePath (takeExtension)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Aeson as A
import Text.Read (readEither)
import Data.Bifunctor
import Control.Monad (forM)
import qualified Sound.Jammit.Export as J
import Control.Applicative ((<$>))
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import qualified Sound.File.Sndfile as Snd
import Data.Conduit.Audio.SampleRate

data Edge = Begin | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio t a
  = Silence Int t
  | File a
  | Combine Combine [Audio t a]
  | Combine' Combine [(Audio t a, Double)]
  | Unary [Unary t] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, T.Traversable)

data InputFile
  = Sndable FilePath
  | JammitAIFC FilePath
  deriving (Eq, Ord, Show, Read)

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

buildSource :: (MonadResource m) =>
  Audio Double InputFile -> Action (AudioSource m Float)
buildSource aud = case aud of
  Silence chans t -> return $ silent (Seconds t) 44100 chans
  File fin -> case fin of
    Sndable x -> case takeExtension x of
      ".mp3" -> do
        liftIO $ createDirectoryIfMissing True "gen/temp"
        (f, h) <- liftIO $ openTempFile "gen/temp" "from-mp3.wav"
        liftIO $ hClose h
        () <- cmd "lame --decode" [x, f]
        buildSource $ File $ Sndable f
      _ -> do
        src <- liftIO $ sourceSnd x
        return $ if rate src == 44100
          then src
          else resampleTo 44100 SincBestQuality src
    JammitAIFC x -> liftIO $ mapSamples fractionalSample <$> J.audioSource x
  Combine meth xs -> buildSource $ Combine' meth [ (x, 1) | x <- xs ]
  Combine' meth xs -> do
    srcs <- forM xs $ \(x, vol) -> gain (realToFrac vol) <$> buildSource x
    case srcs of
      [] -> error "buildSource: can't combine 0 files"
      s : ss -> case meth of
        Concatenate -> return $ foldl concatenate s ss
        Mix         -> return $ foldl mix         s ss
        Merge       -> return $ foldl merge       s ss
  Unary fs x -> let
    gs = flip map fs $ \f -> case f of
      Trim Begin t -> dropStart $ Seconds t
      Trim End t -> dropEnd $ Seconds t
      Fade Begin t -> \src -> concatenate
        (fadeIn $ takeStart (Seconds t) src)
        (dropStart (Seconds t) src)
      Fade End t -> \src -> concatenate
        (dropEnd (Seconds t) src)
        (fadeOut $ takeEnd (Seconds t) src)
      Pad Begin t -> padStart $ Seconds t
      Pad End t -> padEnd $ Seconds t
      Take Begin t -> takeStart $ Seconds t
      Take End t -> takeEnd $ Seconds t
    in buildSource x >>= \src -> return $ foldl (flip ($)) src gs

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Rational InputFile -> FilePath -> Action ()
buildAudio aud out = do
  need $ do
    fin <- F.toList aud
    return $ case fin of
      Sndable x -> x
      JammitAIFC x -> x
  src <- buildSource $ mapTime realToFrac aud
  liftIO $ runResourceT $ sinkSnd out
    (Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile)
    src
