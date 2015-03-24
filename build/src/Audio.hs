{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Audio where

import Development.Shake (cmd, Action, need, liftIO)
import Development.Shake.FilePath (takeExtension)
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (openTempFile)
import System.IO (hClose)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Aeson as A
import Data.Bifunctor
import qualified Sound.Jammit.Export as J
import Control.Applicative ((<$>), (<|>))
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Data.Monoid (mempty)

import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import qualified Sound.File.Sndfile as Snd
import Data.Conduit.Audio.SampleRate

data Edge = Start | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Audio t a
  = Silence Int t
  | Input       a
  | Mix         [Audio t a]
  | Merge       [Audio t a]
  | Concatenate [Audio t a]
  | Gain Double (Audio t a)
  | Take Edge t (Audio t a)
  | Drop Edge t (Audio t a)
  | Fade Edge t (Audio t a)
  | Pad  Edge t (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, T.Traversable)

pattern OneKey k v <- A.Object (HM.toList -> [(T.unpack -> k, v)])

instance A.FromJSON Edge where
  parseJSON (A.String s) = case T.unpack s of
    "start" -> return Start
    "begin" -> return Start
    "end"   -> return End
    _       -> mempty
  parseJSON _ = mempty

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

instance (A.FromJSON t, A.FromJSON a) => A.FromJSON (Audio t a) where
  parseJSON v = case v of
    OneKey "silence" x -> fmap (uncurry Silence) $ A.parseJSON x
    OneKey "mix" x -> fmap Mix $ A.parseJSON x
    OneKey "merge" x -> fmap Merge $ A.parseJSON x
    OneKey "concatenate" x -> fmap Concatenate $ A.parseJSON x
    OneKey "gain" x -> fmap (uncurry Gain) $ A.parseJSON x
    OneKey "take" x -> withEdge Take x
    OneKey "drop" x -> withEdge Drop x
    OneKey "trim" x -> withEdge Drop x
    OneKey "fade" x -> withEdge Fade x
    OneKey "pad"  x -> withEdge Pad  x
    _ -> fmap Input $ A.parseJSON v
    where withEdge f x
            =   fmap (uncurry3  f      ) (A.parseJSON x)
            <|> fmap (uncurry $ f Start) (A.parseJSON x)

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

instance Bifunctor Audio where
  first = mapTime
  second = fmap

data InputFile
  = Sndable    FilePath
  | JammitAIFC FilePath
  deriving (Eq, Ord, Show, Read)

buildSource :: (MonadResource m) =>
  Audio Double InputFile -> Action (AudioSource m Float)
buildSource aud = case aud of
  Silence c t -> return $ silent (Seconds t) 44100 c
  Input fin -> case fin of
    Sndable x -> case takeExtension x of
      ".mp3" -> do
        liftIO $ createDirectoryIfMissing True "gen/temp"
        (f, h) <- liftIO $ openTempFile "gen/temp" "from-mp3.wav"
        liftIO $ hClose h
        () <- cmd "lame --decode" [x, f]
        buildSource $ Input $ Sndable f
      _ -> do
        src <- liftIO $ sourceSnd x
        let srcRate = if rate src == 44100 then src else resampleTo 44100 SincBestQuality src
            srcChan = case channels srcRate of
              2 -> srcRate
              1 -> merge srcRate srcRate
              c -> error $ "buildSource: only audio with 1 or 2 channels supported (" ++ show c ++ " given)"
        return srcChan
    JammitAIFC x -> liftIO $ mapSamples fractionalSample <$> J.audioSource x
  Mix         xs -> combine mix         xs
  Merge       xs -> combine merge       xs
  Concatenate xs -> combine concatenate xs
  Gain d x -> gain (realToFrac d) <$> buildSource x
  Take Start t x -> takeStart (Seconds t) <$> buildSource x
  Take End t x -> takeEnd (Seconds t) <$> buildSource x
  Drop Start t x -> dropStart (Seconds t) <$> buildSource x
  Drop End t x -> dropEnd (Seconds t) <$> buildSource x
  Pad Start t x -> padStart (Seconds t) <$> buildSource x
  Pad End t x -> padEnd (Seconds t) <$> buildSource x
  Fade Start t x -> buildSource x >>= \src -> return $ concatenate
    (fadeIn $ takeStart (Seconds t) src)
    (dropStart (Seconds t) src)
  Fade End t x -> buildSource x >>= \src -> return $ concatenate
    (dropEnd (Seconds t) src)
    (fadeOut $ takeEnd (Seconds t) src)
  where combine meth xs = mapM buildSource xs >>= \srcs -> case srcs of
          [] -> error "buildSource: can't combine 0 files"
          s : ss -> return $ foldl meth s ss

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Rational InputFile -> FilePath -> Action ()
buildAudio aud out = do
  need $ do
    fin <- F.toList aud
    return $ case fin of
      Sndable    x -> x
      JammitAIFC x -> x
  src <- buildSource $ mapTime realToFrac aud
  liftIO $ runResourceT $ sinkSnd out
    (Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile)
    src
