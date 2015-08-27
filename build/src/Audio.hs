{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Audio where

import Development.Shake (Action, need, liftIO)
import Development.Shake.FilePath (takeExtension)
import Data.Foldable (toList)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Vector.Storable as V

import Data.Conduit ((=$=))
import qualified Data.Conduit.List as CL
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import qualified Sound.File.Sndfile as Snd
import Data.Conduit.Audio.SampleRate
import Control.Monad (ap)

data Audio t a
  = Silence Int t
  | Input a
  | Mix            [Audio t a]
  | Merge          [Audio t a]
  | Concatenate    [Audio t a]
  | Gain Double    (Audio t a)
  | Take Edge t    (Audio t a)
  | Drop Edge t    (Audio t a)
  | Fade Edge t    (Audio t a)
  | Pad  Edge t    (Audio t a)
  | Resample       (Audio t a)
  | Channels [Int] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative (Audio t) where
  pure = Input
  (<*>) = ap

instance Monad (Audio t) where
  return = Input
  x >>= f = let
    join_ = \case
      Silence c t      -> Silence c t
      Input       sub  -> sub
      Mix         auds -> Mix $ map join_ auds
      Merge       auds -> Merge $ map join_ auds
      Concatenate auds -> Concatenate $ map join_ auds
      Gain      d aud  -> Gain d $ join_ aud
      Take    e t aud  -> Take e t $ join_ aud
      Drop    e t aud  -> Drop e t $ join_ aud
      Fade    e t aud  -> Fade e t $ join_ aud
      Pad     e t aud  -> Pad e t $ join_ aud
      Resample    aud  -> Resample $ join_ aud
      Channels cs aud  -> Channels cs $ join_ aud
    in join_ $ fmap f x

data Edge = Start | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

mapTime :: (t -> u) -> Audio t a -> Audio u a
mapTime f aud = case aud of
  Silence c t     -> Silence c $ f t
  Input   x       -> Input x
  Mix         xs  -> Mix         $ map (mapTime f) xs
  Merge       xs  -> Merge       $ map (mapTime f) xs
  Concatenate xs  -> Concatenate $ map (mapTime f) xs
  Gain g x        -> Gain g $ mapTime f x
  Take e t x      -> Take e (f t) $ mapTime f x
  Drop e t x      -> Drop e (f t) $ mapTime f x
  Fade e t x      -> Fade e (f t) $ mapTime f x
  Pad  e t x      -> Pad  e (f t) $ mapTime f x
  Resample x      -> Resample     $ mapTime f x
  Channels cs x   -> Channels cs  $ mapTime f x

buildSource :: (MonadResource m) =>
  Audio Duration FilePath -> Action (AudioSource m Float)
buildSource aud = case aud of
  Silence c t -> return $ silent t 44100 c
  Input fin -> liftIO $ sourceSnd fin
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
buildAudio :: Audio Duration FilePath -> FilePath -> Action ()
buildAudio aud out = do
  need $ toList aud
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
