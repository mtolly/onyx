{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Audio where

import Development.Shake (Action, need, liftIO)
import Development.Shake.FilePath (takeExtension)
import Data.Foldable (toList)
import qualified Sound.Jammit.Export as J
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Vector.Storable as V

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import qualified Sound.File.Sndfile as Snd
import Data.Conduit.Audio.SampleRate

import Config

data InputFile
  = InputSndfile FilePath
  | InputJammit  FilePath
  deriving (Eq, Ord, Show, Read)

buildSource :: (MonadResource m) =>
  Audio Duration InputFile -> Action (AudioSource m Float)
buildSource aud = case aud of
  Silence c t -> return $ silent t 44100 c
  Input fin -> case fin of
    InputSndfile fp -> liftIO $ sourceSnd fp
    InputJammit  fp -> liftIO $ mapSamples fractionalSample <$> J.audioSource fp
  Mix         xs -> combine mix         xs
  Merge       xs -> combine merge       xs
  Concatenate xs -> combine concatenate xs
  Gain d x -> gain (realToFrac d) <$> buildSource x
  Take Start t x -> takeStart t <$> buildSource x
  Take End t x -> takeEnd t <$> buildSource x
  Drop Start t x -> dropStart t <$> buildSource x
  Drop End t x -> dropEnd t <$> buildSource x
  Pad Start t x -> padStart t <$> buildSource x
  Pad End t x -> padEnd t <$> buildSource x
  Fade Start t x -> buildSource x >>= \src -> return $ concatenate
    (fadeIn $ takeStart t src)
    (dropStart t src)
  Fade End t x -> buildSource x >>= \src -> return $ concatenate
    (dropEnd t src)
    (fadeOut $ takeEnd t src)
  Resample x -> buildSource x >>= \src -> return $ if rate src == 44100
    then src
    else resampleTo 44100 SincMediumQuality src
  Channels cs x -> do
    src <- buildSource x
    let chans = splitChannels src
    case map (chans !!) cs of
      [] -> error "buildSource: can't select 0 channels"
      s : ss -> return $ foldl merge s ss
  where combine meth xs = mapM buildSource xs >>= \srcs -> case srcs of
          [] -> error "buildSource: can't combine 0 files"
          s : ss -> return $ foldl meth s ss

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Duration InputFile -> FilePath -> Action ()
buildAudio aud out = do
  need $ do
    fin <- toList aud
    return $ case fin of
      InputSndfile x -> x
      InputJammit  x -> x
  src <- buildSource aud
  let fmt = case takeExtension out of
        ".ogg" -> Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
        ".wav" -> Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
        ext -> error $ "buildAudio: unknown audio output file extension " ++ ext
  liftIO $ runResourceT $ sinkSnd out fmt $ clampFloat src

-- | Forces floating point samples to be in @[-1, 1]@.
-- libsndfile should do this, after https://github.com/kaoskorobase/hsndfile/pull/12
clampFloat :: (Monad m) => AudioSource m Float -> AudioSource m Float
clampFloat src = src { source = source src =$= CL.map clampVector } where
  clampVector = V.map $ \s -> if
    | s < (-1)  -> -1
    | s > 1     -> 1
    | otherwise -> s
