{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Audio
( Audio(..)
, Edge(..)
, Seam(..)
, mapTime
, sameChannels
, buildSource, buildSource'
, buildAudio
, runAudio
, audioIO
, clampFloat
, audioMD5
, audioLength
, audioChannels
, audioRate
, audioSeconds
, applyPansVols
, applyVolsMono
, decentMP3
, decentVorbis
, stretchFull
, stretchFullSmart
, stretchRealtime
, fadeStart, fadeEnd
, mixMany, mixMany'
, clampIfSilent
, stereoPanRatios
, emptyChannels
, remapChannels
, makeFSB4, makeFSB4', makeGH3FSB, makeXMAPieces
) where

import           Control.Concurrent               (threadDelay)
import           Control.DeepSeq                  (($!!))
import           Control.Exception                (evaluate)
import           Control.Monad                    (ap, forM, replicateM_,
                                                   unless, void, when)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Resource     (MonadResource, ResourceT,
                                                   runResourceT)
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT,
                                                   Staction, inside, lg,
                                                   stackIO, stackProcess,
                                                   tempDir)
import           Data.Binary.Get                  (getWord32le)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as BL8
import           Data.Char                        (isSpace, toLower)
import           Data.Conduit
import           Data.Conduit.Audio
import           Data.Conduit.Audio.LAME
import           Data.Conduit.Audio.LAME.Binding  as L
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import qualified Data.Conduit.List                as CL
import qualified Data.Digest.Pure.MD5             as MD5
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Int                         (Int16)
import           Data.List                        (elemIndex, sortOn)
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.SimpleHandle                (Readable, fileReadable)
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as V
import           Data.Word                        (Word8)
import           Development.Shake                (Action, need)
import           Development.Shake.FilePath       (takeExtension, (-<.>))
import           FFMPEG                           (FFSourceSample, ffSource,
                                                   ffSourceFrom)
import           GuitarHeroII.Audio               (readVGS)
import           Magma                            (withWin32Exe)
import           MoggDecrypt                      (sourceVorbisFile)
import           Numeric                          (showHex)
import qualified Numeric.NonNegative.Wrapper      as NN
import           Preferences
import           Resources                        (makeFSB4exe, xma2encodeExe)
import           RockBand.Common                  (pattern RNil, pattern Wait)
import           SndfileExtra
import qualified Sound.File.Sndfile               as Snd
import           Sound.FSB
import qualified Sound.MIDI.Util                  as U
import qualified Sound.RubberBand                 as RB
import           STFS.Package                     (runGetM)
import           System.FilePath                  ((</>))
import           System.Info                      (os)
import qualified System.IO                        as IO
import           System.Process                   (proc)

data Audio t a
  = Silence Int t
  | Input a
  | Mix                       (NonEmpty (Audio t a))
  | Merge                     (NonEmpty (Audio t a))
  | Concatenate               (NonEmpty (Audio t a))
  | Gain Double               (Audio t a)
  | Take Edge t               (Audio t a)
  | Drop Edge t               (Audio t a)
  | Fade Edge t               (Audio t a)
  | Pad  Edge t               (Audio t a)
  | Resample                  (Audio t a)
  | Channels [Maybe Int]      (Audio t a)
  | StretchSimple Double      (Audio t a)
  | StretchFull Double Double (Audio t a)
  | Mask [T.Text] [Seam t]    (Audio t a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Seam t = Seam
  { seamCenter :: t
  , seamFade   :: t
  , seamTag    :: T.Text
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (Audio t) where
  pure = Input
  (<*>) = ap

instance Monad (Audio t) where
  return = Input
  x >>= f = let
    join_ = \case
      Silence c t          -> Silence c t
      Input           sub  -> sub
      Mix             auds -> Mix $ fmap join_ auds
      Merge           auds -> Merge $ fmap join_ auds
      Concatenate     auds -> Concatenate $ fmap join_ auds
      Gain      d     aud  -> Gain d $ join_ aud
      Take    e t     aud  -> Take e t $ join_ aud
      Drop    e t     aud  -> Drop e t $ join_ aud
      Fade    e t     aud  -> Fade e t $ join_ aud
      Pad     e t     aud  -> Pad e t $ join_ aud
      Resample        aud  -> Resample $ join_ aud
      Channels cs     aud  -> Channels cs $ join_ aud
      StretchSimple d aud  -> StretchSimple d $ join_ aud
      StretchFull t p aud  -> StretchFull t p $ join_ aud
      Mask tags seams aud  -> Mask tags seams $ join_ aud
    in join_ $ fmap f x

data Edge = Start | End
  deriving (Eq, Ord, Show, Enum, Bounded)

mapTime :: (t -> u) -> Audio t a -> Audio u a
mapTime f aud = case aud of
  Silence c t       -> Silence c $ f t
  Input   x         -> Input x
  Mix         xs    -> Mix         $ fmap (mapTime f) xs
  Merge       xs    -> Merge       $ fmap (mapTime f) xs
  Concatenate xs    -> Concatenate $ fmap (mapTime f) xs
  Gain g x          -> Gain g $ mapTime f x
  Take e t x        -> Take e (f t) $ mapTime f x
  Drop e t x        -> Drop e (f t) $ mapTime f x
  Fade e t x        -> Fade e (f t) $ mapTime f x
  Pad  e t x        -> Pad  e (f t) $ mapTime f x
  Resample x        -> Resample     $ mapTime f x
  Channels cs x     -> Channels cs  $ mapTime f x
  StretchSimple d x -> StretchSimple d $ mapTime f x
  StretchFull t p x -> StretchFull t p $ mapTime f x
  Mask tags seams x -> Mask tags (map (fmap f) seams) $ mapTime f x

{- |
Simple linear interpolation of an audio stream.
This is intended to make very small duration adjustments.
-}
stretchSimple :: (MonadResource m) => Double -> AudioSource m Float -> AudioSource m Float
stretchSimple ratio src = AudioSource
  { rate     = rate src
  , frames   = ceiling $ fromIntegral (frames src) * ratio
  , channels = channels src
  , source   = source src .| pipe 0 (repeat 0)
  } where
    stride = recip ratio
    pipe phase prev = await >>= \case
      Nothing -> return ()
      Just v -> let
        chans = deinterleave (channels src) v
        len :: Double
        len = fromIntegral $ V.length $ head chans
        allIndexes = iterate (+ stride) phase
        (usedIndexes, nextIndex : _) = span (<= len - 1) allIndexes
        stretchChannel prevSample channel = let
          doubleIndex d
            | d >= 0 = case properFraction d of
              (i, d') -> intIndex i * realToFrac (1 - d') + intIndex (i + 1) * realToFrac d'
            | otherwise = case properFraction d of
              (i, d') -> intIndex (i - 1) * realToFrac (negate d') + intIndex i * realToFrac (1 + d')
          intIndex (-1) = prevSample
          intIndex i    = channel V.! i
          in V.fromList $ map doubleIndex usedIndexes
        in do
          yield $ interleave $ zipWith stretchChannel prev chans
          pipe (nextIndex - len) $ map V.last chans

-- | Avoids giving silent channels to the audio stretcher.
stretchFullSmart :: (MonadResource m) => Double -> Double -> AudioSource m Float -> AudioSource m Float
stretchFullSmart tr pr src = let
  stretchAll = stretchFull tr pr src
  in stretchAll
    { source = emptyChannels src >>= source . \case
      []    -> stretchAll
      chans -> let
        soundChannels = filter (`notElem` chans) [0 .. channels src - 1]
        transformIn = remapChannels $ map Just soundChannels
        transformOut = remapChannels $ map (`elemIndex` soundChannels) [0 .. channels src - 1]
        in transformOut $ stretchFull tr pr $ transformIn src
    }

-- | Proper audio stretching of time and/or pitch separately.
stretchFull :: (MonadResource m) => Double -> Double -> AudioSource m Float -> AudioSource m Float
stretchFull timeRatio pitchRatio src = AudioSource
  { rate     = rate src
  , frames   = ceiling $ fromIntegral (frames src) * timeRatio
  , channels = channels src
  , source   = pipe $ source $ reorganize chunkSize src
  } where
    pipe upstream = do
      rb <- liftIO $ RB.new
        (round $ rate src)
        (channels src)
        RB.defaultOptions{ RB.oStretch = RB.Precise }
        timeRatio
        pitchRatio
      liftIO $ RB.setMaxProcessSize rb chunkSize
      upstream .| studyAll rb
      upstream .| processAll rb
    studyAll rb = await >>= \case
      Nothing -> return ()
      Just v -> await >>= \case
        Nothing -> liftIO $ RB.study rb (deinterleave (channels src) v) True
        Just v' -> do
          leftover v'
          liftIO $ RB.study rb (deinterleave (channels src) v) False
          studyAll rb
    processAll rb = liftIO (RB.available rb) >>= \case
      Nothing -> return ()
      Just 0 -> liftIO (RB.getSamplesRequired rb) >>= \case
        0 -> liftIO (threadDelay 1000) >> processAll rb
        _ -> await >>= \case
          Nothing -> return ()
          Just v -> await >>= \case
            Nothing -> liftIO $ RB.process rb (deinterleave (channels src) v) True
            Just v' -> do
              leftover v'
              liftIO $ RB.process rb (deinterleave (channels src) v) False
              processAll rb
      Just n -> do
        liftIO (interleave <$> RB.retrieve rb (min n chunkSize)) >>= yield
        processAll rb

stretchRealtime :: (MonadResource m) => Double -> Double -> AudioSource m Float -> AudioSource m Float
stretchRealtime timeRatio pitchRatio src = AudioSource
  { rate     = rate src
  , frames   = ceiling $ fromIntegral (frames src) * timeRatio
  , channels = channels src
  , source   = pipe $ source $ reorganize chunkSize src
  } where
    pipe upstream = do
      rb <- liftIO $ RB.new
        (round $ rate src)
        (channels src)
        RB.defaultOptions{ RB.oProcess = RB.RealTime, RB.oStretch = RB.Precise }
        timeRatio
        pitchRatio
      liftIO $ RB.setMaxProcessSize rb chunkSize
      upstream .| processAll rb
    processAll rb = liftIO (RB.available rb) >>= \case
      Nothing -> return ()
      Just 0 -> liftIO (RB.getSamplesRequired rb) >>= \case
        0 -> liftIO (threadDelay 1000) >> processAll rb
        _ -> await >>= \case
          Nothing -> return ()
          Just v -> await >>= \case
            Nothing -> liftIO $ RB.process rb (deinterleave (channels src) v) True
            Just v' -> do
              leftover v'
              liftIO $ RB.process rb (deinterleave (channels src) v) False
              processAll rb
      Just n -> do
        liftIO (interleave <$> RB.retrieve rb (min n chunkSize)) >>= yield
        processAll rb

-- | Duplicates mono into stereo, or otherwise just tacks on silent channels to one source.
-- TODO: change this to only do mono->stereo, and use proper volume adjustment (see applyPansVols)
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

fadeStart :: (Monad m, Ord a, Fractional a, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
fadeStart dur (AudioSource s r c l) = let
  fadeFrames = case dur of
    Frames  fms  -> fms
    Seconds secs -> secondsToFrames secs r
  go i
    | i > fadeFrames = awaitForever yield
    | otherwise      = await >>= \mx -> case mx of
      Nothing -> return ()
      Just v  -> let
        fader = V.generate (V.length v) $ \j ->
          min 1 $ fromIntegral (i + quot j c) / fromIntegral fadeFrames
        in yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
  in AudioSource (s .| go 0) r c l

fadeEnd :: (Monad m, Ord a, Fractional a, V.Storable a) => Duration -> AudioSource m a -> AudioSource m a
fadeEnd dur (AudioSource s r c l) = let
  fadeFrames = case dur of
    Frames  fms  -> fms
    Seconds secs -> secondsToFrames secs r
  go i = await >>= \mx -> case mx of
    Nothing -> return ()
    Just v  -> if i + vectorFrames v c > l - fadeFrames
      then let
        fader = V.generate (V.length v) $ \j ->
          min 1 $ fromIntegral (l - (i + quot j c)) / fromIntegral fadeFrames
        in yield (V.zipWith (*) v fader) >> go (i + vectorFrames v c)
      else yield v >> go (i + vectorFrames v c)
  in AudioSource (s .| go 0) r c l

data MaskSections
  = MaskFade Bool Frames Frames MaskSections
  | MaskStay Bool Frames MaskSections
  | MaskEnd Bool
  deriving (Eq, Ord, Show)

seamsToSections :: [T.Text] -> [Seam Frames] -> MaskSections
seamsToSections tags seams = let
  seams1 :: [(Frames, Frames, Bool)]
  seams1 = flip map (sortOn seamCenter seams) $ \seam ->
    ( seamCenter seam - (seamFade seam `quot` 2) -- seam start
    , seamFade seam -- seam length
    , seamTag seam `elem` tags -- is audio active after seam
    )
  go _   st [] = MaskEnd st
  go now st ((start, len, st') : rest) = MaskStay st (start - now) $ if st == st'
    then go start st rest
    else if len == 0
      then go start st' rest
      else MaskFade st' 0 len $ go (start + len) st' rest
  in go 0 False seams1

renderMask :: (Monad m) => [T.Text] -> [Seam Duration] -> AudioSource m Float -> AudioSource m Float
renderMask tags seams (AudioSource s r c l) = let
  sections = seamsToSections tags $ flip map seams $ fmap $ \case
    Seconds secs -> secondsToFrames secs r
    Frames  fms  -> fms
  masker   (MaskEnd  b) = when b $ CL.map id
  masker   (MaskStay _ 0   rest) = masker rest
  masker m@(MaskStay b fms rest) = await >>= \case
    Nothing -> return ()
    Just chunk -> let
      len = vectorFrames chunk c
      in if len <= fms
        then do
          if b
            then yield chunk
            else yield $ V.replicate (V.length chunk) 0
          masker $ MaskStay b (fms - len) rest
        else do
          let (chunkA, chunkB) = V.splitAt (fms * c) chunk
          leftover chunkB
          leftover chunkA
          masker m
  masker m@(MaskFade b done total rest) = if done == total
    then masker rest
    else await >>= \case
      Nothing -> return ()
      Just chunk -> let
        len = vectorFrames chunk c
        in if len <= total - done
          then do
            yield $ V.generate (V.length chunk) $ \i -> let
              mult = fromIntegral (done + quot i c + 1) / fromIntegral total
              mult' = if b then mult else 1 - mult
              in (chunk V.! i) * mult'
            masker $ MaskFade b (done + len) total rest
          else do
            let (chunkA, chunkB) = V.splitAt ((total - done) * c) chunk
            leftover chunkB
            leftover chunkA
            masker m
  in AudioSource (s .| masker sections) r c l

remapChannels :: (Monad m, Num a, V.Storable a) => [Maybe Int] -> AudioSource m a -> AudioSource m a
remapChannels cs (AudioSource s r c f) = let
  adjustBlock v = let
    chans = deinterleave c v
    zero = V.replicate (V.length $ head chans) 0
    in interleave $ map (maybe zero (chans !!)) cs
  in AudioSource (s .| CL.map adjustBlock) r (length cs) f

buildSource :: (MonadResource m) =>
  Audio Duration FilePath -> Action (AudioSource m Float)
buildSource aud = need (toList aud) >> buildSource' aud

standardRate :: (MonadResource m) => AudioSource m Float -> AudioSource m Float
standardRate src = if rate src == 44100
  then src
  else if rate src < 1000
    -- this is a quick hack because libsamplerate breaks if you try to resample
    -- by a factor less than 1/256 or greater than 256.
    -- shouldn't happen ever but found out while messing with VGS low sample rates
    then silent (Seconds 0) 44100 (channels src)
    else resampleTo 44100 SincMediumQuality $ reorganize chunkSize src

buildSource' :: (MonadResource m, MonadIO f) =>
  Audio Duration FilePath -> f (AudioSource m Float)
buildSource' aud = case aud of
  -- optimizations
  Drop Start (Seconds t1) (Pad Start (Seconds t2) x) -> dropPad Start Seconds t1 t2 x
  Drop End   (Seconds t1) (Pad End   (Seconds t2) x) -> dropPad End   Seconds t1 t2 x
  Drop Start (Frames  t1) (Pad Start (Frames  t2) x) -> dropPad Start Frames  t1 t2 x
  Drop End   (Frames  t1) (Pad End   (Frames  t2) x) -> dropPad End   Frames  t1 t2 x
  Drop Start t (Input fin) -> liftIO $ case takeExtension fin of
    ".ogg" -> sourceVorbisFile t fin
    -- TODO implement smart VGS seeking
    ".vgs" -> dropStart t <$> buildSource' (Input fin)
    -- FFMPEG appears to not seek XMA correctly (or we're generating seek table wrong maybe?)
    ".xma" -> dropStart t <$> buildSource' (Input fin)
    _      -> ffSourceFixPath t fin
  Drop Start (Seconds s) (Resample (Input fin)) -> buildSource' $ Resample $ Drop Start (Seconds s) (Input fin)
  Drop Start t (Merge xs) -> buildSource' $ Merge $ fmap (Drop Start t) xs
  Drop Start t (Mix   xs) -> buildSource' $ Mix   $ fmap (Drop Start t) xs
  Drop edge t (Gain d x) -> buildSource' $ Gain d $ Drop edge t x
  Drop edge t (Channels cs x) -> buildSource' $ Channels cs $ Drop edge t x
  Channels (sequence -> Just cs) (Input fin) | takeExtension fin == ".vgs" -> do
    chans <- liftIO $ readVGS fin
    case map (standardRate . mapSamples fractionalSample . (chans !!)) cs of
      src : srcs -> return $ foldl merge src srcs
      []         -> error "buildSource: 0 channels selected"
  -- normal cases
  Silence c t -> return $ silent t 44100 c
  Input fin -> liftIO $ case takeExtension fin of
    ".ogg" -> sourceVorbisFile (Frames 0) fin
    ".vgs" -> do
      chans <- readVGS fin
      case map (standardRate . mapSamples fractionalSample) chans of
        src : srcs -> return $ foldl merge src srcs
        []         -> error "buildSource: VGS has 0 channels"
    _      -> ffSourceFixPath (Frames 0) fin
  Mix         xs -> combine (\a b -> uncurry mix $ sameChannels (a, b)) xs
  Merge       xs -> combine merge xs
  Concatenate xs -> combine (\a b -> uncurry concatenate $ sameChannels (a, b)) xs
  Gain d x -> gain (realToFrac d) <$> buildSource' x
  Take Start t x -> takeStart t <$> buildSource' x
  Take End t x -> takeEnd t <$> buildSource' x
  Drop Start t x -> dropStart t <$> buildSource' x
  Drop End t x -> dropEnd t <$> buildSource' x
  Pad Start t x -> padStart t <$> buildSource' x
  Pad End t x -> padEnd t <$> buildSource' x
  Fade Start t x -> fadeStart t <$> buildSource' x
  Fade End t x -> fadeEnd t <$> buildSource' x
  Resample x -> standardRate <$> buildSource' x
  Channels cs x -> remapChannels cs <$> buildSource' x
  StretchSimple d x -> stretchSimple d <$> buildSource' x
  StretchFull t p x -> stretchFull t p <$> buildSource' x
  Mask tags seams x -> renderMask tags seams <$> buildSource' x
  where combine meth xs = do
          s :| ss <- mapM buildSource' xs
          return $ foldl meth s ss
        dropPad edge dur t1 t2 x = buildSource' $ case compare t1 t2 of
          EQ -> x
          GT -> Drop edge (dur $ t1 - t2) x
          LT -> Pad edge (dur $ t2 - t1) x

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Duration FilePath -> FilePath -> Staction ()
buildAudio aud out = do
  src <- lift $ lift $ buildSource aud
  runAudio src out

audioIO :: Maybe Double -> AudioSource (ResourceT IO) Float -> FilePath -> IO ()
audioIO oggQuality src out = let
  src' = clampFloat $ if takeExtension out == ".ogg" && channels src == 6
    then merge src $ silent (Frames 0) (rate src) 1
    -- this works around an issue with oggenc:
    -- it assumes 6 channels is 5.1 surround where the last channel
    -- is LFE, so instead we add a silent 7th channel
    else src
  withSndFormat fmt = runResourceT $ case (takeExtension out, oggQuality) of
    (".ogg", Just q) -> do
      let setup hsnd = void $ liftIO $ setVBREncodingQuality hsnd q
      sinkSndWithHandle out fmt setup src'
    _ -> sinkSnd out fmt src'
  in case takeExtension out of
    ".ogg" -> withSndFormat $ Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
    ".wav" -> withSndFormat $ Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
    ".mp3" -> runResourceT $ sinkMP3 out src'
    ext -> error $ "audioIO: unknown audio output file extension " ++ ext

runAudio :: (SendMessage m, MonadIO m) => AudioSource (ResourceT IO) Float -> FilePath -> StackTraceT m ()
runAudio src out = do
  lg $ "Writing audio to " ++ out
  oggQuality <- prefOGGQuality <$> readPreferences
  inside ("Writing audio to " ++ out) $ stackIO $ audioIO (Just oggQuality) src out
  lg $ "Finished writing audio to " ++ out

-- | Forces floating point samples to be in @[-1, 1]@.
-- libsndfile should do this, after <https://github.com/kaoskorobase/hsndfile/pull/12>
clampFloat :: (Monad m) => AudioSource m Float -> AudioSource m Float
clampFloat src = src { source = source src .| CL.map clampVector } where
  clampVector = V.map $ \s -> if
    | s < (-1)  -> -1
    | s > 1     -> 1
    | otherwise -> s

audioMD5 :: (MonadIO m) => FilePath -> m (Maybe String)
audioMD5 f = liftIO $ case takeExtension f of
  ".flac" -> do
    let dropUntilSubstr sub bs
          | sub `BL.isPrefixOf` bs = bs
          | otherwise              = case BL.uncons bs of
            Nothing       -> bs
            Just (_, bs') -> dropUntilSubstr sub bs'
    flacFile <- dropUntilSubstr (BL8.pack "fLaC") <$> BL.readFile f
    let md5bytes = BL.take 16 $ BL.drop 26 flacFile
        showByte :: Word8 -> String
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
        len = fmap fromIntegral $ runGetM getWord32le $ BL.drop 4 bytes
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

-- previously we used shortWindowsPath, because ffmpeg cannot take UTF-8 char* on windows.
-- but short paths are not guaranteed to exist, and they usually don't on non-system drive letters.
-- so now we use a Readable on the Haskell side so ffmpeg doesn't have to see the path.

ffSourceFixPath :: (MonadResource m, FFSourceSample a) => Duration -> FilePath -> IO (AudioSource m a)
ffSourceFixPath dur = ffSourceFrom dur . Left . fileReadable

ffSourceSimple :: FilePath -> IO (AudioSource (ResourceT IO) Int16)
ffSourceSimple = ffSourceFixPath $ Frames 0

supportedFFExt :: FilePath -> Bool
supportedFFExt f = map toLower (takeExtension f) `elem`
  [".flac", ".wav", ".ogg", ".opus", ".mp3", ".xma"]

audioLength :: (MonadIO m) => FilePath -> m (Maybe Integer)
audioLength f = case map toLower $ takeExtension f of
  -- ffmpeg fails to give 0 frames for empty ogg files, saying
  -- "Estimating duration from bitrate, this may be inaccurate"
  ".ogg" -> liftIO $ Just . fromIntegral . frames
    <$> (sourceVorbisFile (Frames 0) f :: IO (AudioSource (ResourceT IO) Float))
  _      -> if supportedFFExt f
    then liftIO $ Just . fromIntegral . frames <$> ffSourceSimple f
    else return Nothing

audioChannels :: (MonadIO m) => FilePath -> m (Maybe Int)
audioChannels f = if supportedFFExt f
  then liftIO $ Just . channels <$> ffSourceSimple f
  else case takeExtension f of
    ".vgs" -> do
      chans <- liftIO (readVGS f :: IO [AudioSource (ResourceT IO) Int16])
      return $ Just $ length chans
    _ -> return Nothing

audioRate :: (MonadIO m) => FilePath -> m (Maybe Int)
audioRate f = if supportedFFExt f
  then liftIO $ Just . round . rate <$> ffSourceSimple f
  else return Nothing

audioSeconds :: (MonadIO m, MonadFail m) => FilePath -> m U.Seconds
audioSeconds f = do
  maybeFms <- audioLength f
  maybeRate <- audioRate f
  case (maybeFms, maybeRate) of
    (Nothing , _      ) -> fail $ "Couldn't obtain audio frame count: " <> f
    (_       , Nothing) -> fail $ "Couldn't obtain audio sample rate: " <> f
    (Just fms, Just r ) -> return $ fromIntegral fms / fromIntegral r

-- | Applies Rock Band's pan and volume lists
-- to turn a multichannel OGG input into a stereo output.
applyPansVols :: (Monad m) => [Float] -> [Float] -> AudioSource m Float -> AudioSource m Float
applyPansVols [-1, 1] [0, 0] src = src
applyPansVols pans    vols   src = AudioSource
  { rate     = rate src
  , frames   = frames src
  , channels = 2
  , source   = source src .| CL.map applyChunk
  } where
    applyChunk :: V.Vector Float -> V.Vector Float
    applyChunk v = V.generate (vectorFrames v (channels src) * 2) $ \i -> do
      case quotRem i 2 of
        (frame, chan) -> let
          pvx = zip3 pans vols $ V.toList $ V.drop (frame * channels src) v
          wire (pan, volDB, sample) = let
            volRatio = 10 ** (volDB / 20)
            panRatio = (if chan == 0 then fst else snd) $ stereoPanRatios pan
            in panRatio * volRatio * sample
          in sum $ map wire pvx

-- | Constant power panning: http://dsp.stackexchange.com/a/21736
stereoPanRatios :: Float -> (Float, Float)
stereoPanRatios pan = let
  theta = pan * (pi / 4)
  ratioL = (sqrt 2 / 2) * (cos theta - sin theta)
  ratioR = (sqrt 2 / 2) * (cos theta + sin theta)
  in (ratioL, ratioR)

-- | Like 'applyPansVols', but mixes into mono instead of stereo.
applyVolsMono :: (Monad m) => [Float] -> AudioSource m Float -> AudioSource m Float
applyVolsMono vols src = AudioSource
  { rate     = rate src
  , frames   = frames src
  , channels = 1
  , source   = source src .| CL.map applyChunk
  } where
    vols' = take (channels src) $ vols ++ repeat 0
    applyChunk :: V.Vector Float -> V.Vector Float
    applyChunk v = V.generate (vectorFrames v $ channels src) $ \frame -> let
      vx = zip vols' $ V.toList $ V.drop (frame * channels src) v
      wire (volDB, sample) = let
        volRatio = 10 ** (volDB / 20)
        in 0.5 * volRatio * sample
      in sum $ map wire vx

decentMP3 :: (MonadResource m) => FilePath -> AudioSource m Float -> m ()
decentMP3 out = sinkMP3WithHandle out $ \lame -> liftIO $ do
  L.check $ L.setVBR lame L.VbrDefault
  L.check $ L.setVBRQ lame 6 -- 0 (hq) to 9 (lq)

decentVorbis :: (MonadResource m) => FilePath -> AudioSource m Float -> m ()
decentVorbis out = let
  setup hsnd = liftIO (setVBREncodingQuality hsnd 0.2) >>= \case
    True  -> return ()
    False -> error "decentVorbis: couldn't set encoding quality"
  fmt = Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
  in sinkSndWithHandle out fmt setup

-- | Returns channel indexes (starting from 0) which are silent.
emptyChannels :: (Monad m, V.Storable a, Eq a, Num a) => AudioSource m a -> ConduitT () o m [Int]
emptyChannels src = let
  loop []    = return []
  loop chans = await >>= \case
    Nothing  -> return chans
    Just blk -> let
      chans' = flip filter chans $ \chan -> let
        indexes = [chan, chan + channels src .. V.length blk - 1]
        in flip all indexes $ \i -> (blk V.! i) == 0
      in loop $!! chans'
  in source src .| loop [0 .. channels src - 1]

-- | Modifies the source to return 0 audio frames if all samples are silent.
-- TODO this is weird that the frames value is untouched...
clampIfSilent :: (Monad m, V.Storable a, Eq a, Num a)
  => AudioSource m a -> AudioSource m a
clampIfSilent src = src
  { source = source src .| let
    loop !samples = await >>= \case
      Nothing -> return () -- whole thing was silent. yield no audio
      Just blk -> if V.all (== 0) blk
        then loop $ samples + V.length blk
        else do
          -- got a non-silent block. yield all the silence, then passthrough upstream
          let maxSize = chunkSize * channels src
              maxChunk = V.replicate maxSize 0
          case quotRem samples maxSize of
            (q, r) -> do
              replicateM_ q $ yield maxChunk
              unless (r == 0) $ yield $ V.replicate r 0
          yield blk
          awaitForever yield
    in loop 0
  }

unvoid :: (Monad m) => ConduitT i Void m r -> ConduitT i o m r
unvoid = mapOutput $ \case {}

getChunk
  :: (Monad m, Num a, V.Storable a)
  => Int
  -> SealedConduitT () (V.Vector a) m ()
  -> ConduitT () o m (Maybe (SealedConduitT () (V.Vector a) m ()), V.Vector a)
getChunk n sc = do
  (sc', mv) <- unvoid $ sc =$$++ await
  case mv of
    Nothing -> return (Nothing, V.replicate n 0)
    Just v -> case compare (V.length v) n of
      EQ -> return (Just sc', v)
      LT -> do
        (msc, v') <- getChunk (n - V.length v) sc'
        return (msc, v <> v')
      GT -> do
        let (this, after) = V.splitAt n v
        (sc'', ()) <- unvoid $ sc' =$$++ leftover after
        return (Just sc'', this)

mixMany
  :: (Monad m, Num a, Ord a, Fractional a, V.Storable a)
  => Rate
  -> Channels
  -> Maybe (Int, U.Seconds) -- ^ max polyphony and cutoff fade time
  -> RTB.T U.Seconds (AudioSource m a)
  -> AudioSource m a
mixMany r c polyphony srcs = mixMany' r c
  (const polyphony)
  ((, ()) <$> srcs)

mixMany'
  :: (Monad m, Num a, Ord a, Fractional a, V.Storable a, Ord g)
  => Rate
  -> Channels
  -> (g -> Maybe (Int, U.Seconds)) -- ^ level of polyphony and cutoff fade time for each group (Nothing = unlimited)
  -> RTB.T U.Seconds (AudioSource m a, g) -- ^ each audio source is annotated with a group
  -> AudioSource m a
mixMany' r c polyphony srcs = let
  srcs' = RTB.discretize $ RTB.mapTime (* realToFrac r) srcs
  in AudioSource
    { rate = r
    , channels = c
    , frames = foldr max 0
      $ map (\(t, (src, _)) -> NN.toNumber t + frames src)
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0 srcs'
    , source = let
      getFrames n sources = do
        results <- forM sources $ \(src, group) -> (, group) <$> getChunk (n * c) src
        let newSources = flip mapMaybe results $ \case
              ((Just src, _), group) -> Just (src, group)
              _                      -> Nothing
            result = case map (snd . fst) results of
              []     -> V.replicate (n * c) 0
              v : vs -> foldr (V.zipWith (+)) v vs
        return (newSources, result)
      cutoff cutoffLength src = sealConduitT $ source $ fadeOut $ takeStart (Seconds cutoffLength') AudioSource
        { rate = r
        , channels = c
        , frames = secondsToFrames cutoffLength' r
        , source = unsealConduitT src
        } where cutoffLength' = (realToFrac :: U.Seconds -> Seconds) cutoffLength
      go currentSources future = case future of
        RNil -> case currentSources of
          [] -> return ()
          _ -> do
            (nextSources, v) <- getFrames chunkSize currentSources
            yield v
            go nextSources RNil
        Wait 0 next@(_src, _group) rest -> do
          let (now, later) = U.trackSplitZero rest
              possibleSources = map Left (next : now) ++ map Right currentSources
              processSources _ [] = []
              processSources groups (Left (src, group) : remaining) = case polyphony group of
                Just (i, _) | fromMaybe 0 (Map.lookup group groups) >= i -> processSources groups remaining
                _ -> Left (src, group) : processSources (addGroup group groups) remaining
              processSources groups (Right (osrc, group) : remaining) = case polyphony group of
                Just (i, fadeTime) | fromMaybe 0 (Map.lookup group groups) >= i
                  -> Right (cutoff fadeTime osrc, group) : processSources groups remaining
                _ -> Right (osrc, group) : processSources (addGroup group groups) remaining
              processed = processSources Map.empty possibleSources
              addGroup = Map.alter $ maybe (Just 1) (Just . (+ 1))
          opened <- unvoid $ forM (lefts processed) $ \(src, group) -> do
            fmap (\(osrc, _) -> (osrc, group)) $ src =$$+ return ()
          go (opened ++ rights processed) later
        Wait dt next rest -> do
          let sizeToGet = min chunkSize $ fromIntegral dt
          (nextSources, v) <- getFrames sizeToGet currentSources
          yield v
          go nextSources $ Wait (dt - fromIntegral sizeToGet) next rest
      in go [] $ (\(asrc, group) -> (source asrc, group)) <$> srcs'
    }

-- Use official FMOD generator
makeFSB4 :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> StackTraceT m ()
makeFSB4 wav fsb = do
  exe <- stackIO makeFSB4exe
  let createProc = withWin32Exe proc exe [wav, fsb]
  inside "converting WAV to FSB4" $ do
    str <- stackProcess createProc
    when (any (not . isSpace) str) $ lg str
    stackIO $ IO.withBinaryFile fsb IO.ReadWriteMode $ \h -> do
      IO.hSeek h IO.AbsoluteSeek 0x32
      B.hPut h $ "multichannel sound" <> B.replicate 12 0
    lg $ "Created XMA (Xbox 360) FSB4 at: " <> fsb

-- Use Microsoft generator, then repackage xma as fsb
makeFSB4' :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> StackTraceT m ()
makeFSB4' wav fsb = do
  exe <- stackIO xma2encodeExe
  let xma = fsb -<.> "xma"
  -- this is required for wine, otherwise it messes up the unix paths somehow.
  let windowsPath s = if os == "mingw32"
        then return s
        else fmap (takeWhile (/= '\n')) $ stackProcess $ proc "winepath" ["-w", s]
  wav' <- windowsPath wav
  xma' <- windowsPath xma
  let createProc = withWin32Exe proc exe [wav', "/TargetFile", xma', "/BlockSize", "32"]
  inside ("converting WAV to FSB4 (XMA)") $ do
    str <- stackProcess createProc
    when (any (not . isSpace) str) $ lg str
    madeFSB <- stackIO (BL.readFile xma) >>= parseXMA >>= ghBandFSB
    stackIO $ BL.writeFile fsb $ emitFSB madeFSB
    lg $ "Created XMA (Xbox 360) FSB4 at: " <> fsb

-- Ensures the XMA ends on a full 16-packet (32 Kb) block
trimXMA :: (MonadFail m) => XMAContents -> m XMAContents
trimXMA xma = do
  let blockSize = 32 * 1024
  newSize <- case quotRem (BL.length $ xmaData xma) blockSize of
    (q, 0) -> if q >= 2
      then return $ (q - 1) * blockSize
      else fail "trimXMA: not enough XMA data to trim off a block safely"
    (0, _) -> fail "trimXMA: not a full block in XMA data"
    (q, _) -> return $ q * blockSize
  let newData = BL.take newSize $ xmaData xma
  packets <- markXMAPacketStreams <$> splitXMA2Packets newData
  return xma
    { xmaSamples = fromIntegral $ sum [ if stream == 0 then xma2FrameCount pkt else 0 | (stream, pkt) <- packets ] * 512
    , xmaData    = newData
    }

-- All are assumed to be same rate and channels.
-- All except last one should end in a full block
concatenateXMA :: [XMAContents] -> XMAContents
concatenateXMA xmas = (head xmas)
  { xmaSamples = sum $ map xmaSamples xmas
  , xmaData    = BL.concat $ map xmaData xmas
  }

-- Encode an XMA file, possibly splitting the input up into pieces to avoid xma2encode's memory limit.
-- The seams between pieces will have very small audio gaps.
makeXMAPieces :: (MonadResource m, SendMessage m) => Either Readable FilePath -> StackTraceT m XMAContents
makeXMAPieces input = do
  -- TODO fix the ffSource/ffSourceFrom here to use the windows path fix versions
  -- (maybe not crucial because this is only called on temp folder inputs)
  exe <- stackIO xma2encodeExe
  c <- stackIO $ channels <$> (ffSource input :: IO (AudioSource (ResourceT IO) Int16))
  let maxSamples = 15 * 60 * 44100 * 4
      -- probably could go a bit higher; xma2encode limit is somewhere between 20 and 60 minutes of 4-channel 44100 Hz
      maxFrames  = quot maxSamples c
  tempDir "onyx-xma" $ \temp -> do
    let windowsPath s = if os == "mingw32"
          then return s
          else fmap (takeWhile (/= '\n')) $ stackProcess $ proc "winepath" ["-w", s]
        wav = temp </> "audio.wav"
        xma = temp </> "audio.xma"
    wav' <- windowsPath wav
    xma' <- windowsPath xma
    let runXMA = do
          str <- stackProcess $ withWin32Exe proc exe [wav', "/TargetFile", xma', "/BlockSize", "32"]
          when (any (not . isSpace) str) $ lg str
        go contents startFrame = do
          src <- stackIO $ ffSourceFrom (Frames startFrame) input
          case frames src of
            0 -> return $ concatenateXMA contents
            n -> if n < maxFrames
              then do
                runAudio src wav
                runXMA
                newData <- stackIO (BL.readFile xma) >>= parseXMA
                return $ concatenateXMA $ contents <> [newData]
              else do
                runAudio (takeStart (Frames maxFrames) src) wav
                runXMA
                origData <- stackIO (BL.readFile xma) >>= parseXMA
                newData <- trimXMA origData
                go (contents <> [newData]) $ startFrame + xmaSamples newData
    go [] 0

makeGH3FSB :: (MonadIO m, SendMessage m) => FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> StackTraceT m ()
makeGH3FSB gtr preview rhythm song fsb = do
  exe <- stackIO xma2encodeExe
  let inputs =
        -- don't think the names matter, just the positions
        [ ("onyx_guitar.xma", gtr)
        , ("onyx_preview.xma", preview)
        , ("onyx_rhythm.xma", rhythm)
        , ("onyx_song.xma", song)
        ]
  inputs' <- forM inputs $ \(name, wav) -> do
    let xma = wav -<.> "xma"
    -- this is required for wine, otherwise it messes up the unix paths somehow.
    let windowsPath s = if os == "mingw32"
          then return s
          else fmap (takeWhile (/= '\n')) $ stackProcess $ proc "winepath" ["-w", s]
    wav' <- windowsPath wav
    xma' <- windowsPath xma
    let createProc = withWin32Exe proc exe [wav', "/TargetFile", xma', "/BlockSize", "32"]
    madeXMA <- inside ("converting WAV to XMA: " <> wav) $ do
      str <- stackProcess createProc
      when (any (not . isSpace) str) $ lg str
      stackIO (BL.readFile xma) >>= parseXMA
    return (name, madeXMA)
  madeFSB <- toGH3FSB <$> xmasToFSB inputs'
  stackIO $ BL.writeFile fsb $ emitFSB madeFSB
  lg $ "Created XMA (Xbox 360) FSB3 at: " <> fsb
