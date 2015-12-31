{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Audio where

import           Control.Exception             (evaluate)
import           Control.Monad                 (ap)
import           Control.Monad.Trans.Resource  (MonadResource, runResourceT)
import           Data.Binary.Get               (getWord32le, runGetOrFail)
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Char                     (toLower)
import           Data.Conduit                  ((=$=))
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import qualified Data.Conduit.List             as CL
import qualified Data.Digest.Pure.MD5          as MD5
import           Data.Foldable                 (toList)
import qualified Data.Vector.Storable          as V
import           Data.Word                     (Word8)
import           Development.Shake             (Action, liftIO, need, putNormal)
import           Development.Shake.FilePath    (takeExtension)
import           Numeric                       (showHex)
import qualified Sound.File.Sndfile            as Snd
import           System.IO

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

-- | Duplicates mono into stereo, or otherwise just tacks on silent channels to one source.
sameChannels :: (Monad m, Num a, V.Storable a) => (AudioSource m a, AudioSource m a) -> (AudioSource m a, AudioSource m a)
sameChannels (a1, a2) = case (channels a1, channels a2) of
  (1, c2) | c2 /= 1 -> let
    a1' = foldr merge a1 $ replicate (c2 - 1) a1
    in (a1', a2)
  (c1, 1) | c1 /= 1 -> let
    a2' = foldr merge a2 $ replicate (c1 - 1) a2
    in (a1, a2')
  (c1, c2) -> let
    a1' = case max c1 c2 - c1 of
      0 -> a1
      n -> merge a1 $ silent (Frames 0) (rate a1) n
    a2' = case max c1 c2 - c2 of
      0 -> a2
      n -> merge a2 $ silent (Frames 0) (rate a2) n
    in (a1', a2')

buildSource :: (MonadResource m) =>
  Audio Duration FilePath -> Action (AudioSource m Float)
buildSource aud = case aud of
  Silence c t -> return $ silent t 44100 c
  Input fin -> liftIO $ sourceSnd fin
  Mix         xs -> combine (\a b -> uncurry mix $ sameChannels (a, b)) xs
  Merge       xs -> combine merge xs
  Concatenate xs -> combine (\a b -> uncurry concatenate $ sameChannels (a, b)) xs
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
      src' = if takeExtension out == ".ogg" && channels src == 6
        then merge src $ silent (Frames 0) (rate src) 1
        -- this works around an issue with oggenc:
        -- it assumes 6 channels is 5.1 surround where the last channel
        -- is LFE, so instead we add a silent 7th channel
        else src
  putNormal $ "Writing an audio expression to " ++ out
  liftIO $ runResourceT $ sinkSnd out fmt $ clampFloat src'
  putNormal $ "Finished writing an audio expression to " ++ out

-- | Forces floating point samples to be in @[-1, 1]@.
-- libsndfile should do this, after https://github.com/kaoskorobase/hsndfile/pull/12
clampFloat :: (Monad m) => AudioSource m Float -> AudioSource m Float
clampFloat src = src { source = source src =$= CL.map clampVector } where
  clampVector = V.map $ \s -> if
    | s < (-1)  -> -1
    | s > 1     -> 1
    | otherwise -> s

audioMD5 :: FilePath -> IO (Maybe String)
audioMD5 f = case takeExtension f of
  ".flac" -> withBinaryFile f ReadMode $ \h -> do
    hSeek h AbsoluteSeek 26
    md5bytes <- BL.hGet h 16
    let showByte :: Word8 -> String
        showByte w8 = case map toLower $ showHex w8 "" of
          [c] -> ['0', c]
          s   -> s
    return $ Just $ concatMap showByte $ BL.unpack md5bytes
  ".wav" -> let
    findChunk :: BL.ByteString -> BL.ByteString -> Maybe BL.ByteString
    findChunk tag bytes = if BL.null bytes
      then Nothing
      else let
        thisTag = BL.take 4 bytes
        len = case runGetOrFail getWord32le $ BL.drop 4 bytes of
          Left  _         -> Nothing
          Right (_, _, l) -> Just $ fromIntegral l
        in if tag == thisTag
          then len >>= \l -> Just $ BL.take l $ BL.drop 8 bytes
          else len >>= \l -> findChunk tag $ BL.drop (8 + l) bytes
    in do
      wav <- BL.readFile f
      evaluate $ do
        riff <- findChunk (BL8.pack "RIFF") wav
        data_ <- findChunk (BL8.pack "data") $ BL.drop 4 riff
        return $ show $ MD5.md5 data_
  _ -> return Nothing

audioLength :: FilePath -> IO (Maybe Integer)
audioLength f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then Just . fromIntegral . Snd.frames <$> Snd.getFileInfo f
  else return Nothing
