{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE PatternSynonyms   #-}
module Audio
( Audio(..)
, Edge(..)
, Seam(..)
, mapTime
, sameChannels
, buildSource
, buildAudio
, runAudio
, clampFloat
, audioMD5
, audioLength
, audioChannels
, audioRate
, audioSeconds
, applyPansVols
, applyVolsMono
, crapMP3
, crapVorbis
) where

import           Control.Exception               (evaluate)
import           Control.Monad                   (ap)
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Resource    (MonadResource, ResourceT,
                                                  runResourceT)
import           Data.Binary.Get                 (getWord32le, runGetOrFail)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import           Data.Char                       (toLower)
import           Data.Conduit                    (await, awaitForever, leftover,
                                                  yield, (=$=))
import           Data.Conduit.Audio
import           Data.Conduit.Audio.LAME
import           Data.Conduit.Audio.LAME.Binding as L
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import qualified Data.Conduit.List               as CL
import qualified Data.Digest.Pure.MD5            as MD5
import           Data.Foldable                   (toList)
import           Data.List                       (sortOn)
import qualified Data.Text                       as T
import qualified Data.Vector.Storable            as V
import           Data.Word                       (Word8)
import           Development.Shake               (Action, need, putNormal)
import           Development.Shake.FilePath      (takeExtension)
import           Numeric                         (showHex)
import           SndfileExtra
import qualified Sound.File.Sndfile              as Snd
import qualified Sound.MIDI.Util                 as U

data Audio t a
  = Silence Int t
  | Input a
  | Mix                    [Audio t a]
  | Merge                  [Audio t a]
  | Concatenate            [Audio t a]
  | Gain Double            (Audio t a)
  | Take Edge t            (Audio t a)
  | Drop Edge t            (Audio t a)
  | Fade Edge t            (Audio t a)
  | Pad  Edge t            (Audio t a)
  | Resample               (Audio t a)
  | Channels [Int]         (Audio t a)
  | Stretch Double         (Audio t a)
  | Mask [T.Text] [Seam t] (Audio t a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data Seam t = Seam
  { seamCenter :: t
  , seamFade   :: t
  , seamTag    :: T.Text
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance Applicative (Audio t) where
  pure = Input
  (<*>) = ap

instance Monad (Audio t) where
  return = Input
  x >>= f = let
    join_ = \case
      Silence c t          -> Silence c t
      Input           sub  -> sub
      Mix             auds -> Mix $ map join_ auds
      Merge           auds -> Merge $ map join_ auds
      Concatenate     auds -> Concatenate $ map join_ auds
      Gain      d     aud  -> Gain d $ join_ aud
      Take    e t     aud  -> Take e t $ join_ aud
      Drop    e t     aud  -> Drop e t $ join_ aud
      Fade    e t     aud  -> Fade e t $ join_ aud
      Pad     e t     aud  -> Pad e t $ join_ aud
      Resample        aud  -> Resample $ join_ aud
      Channels cs     aud  -> Channels cs $ join_ aud
      Stretch   d     aud  -> Stretch d $ join_ aud
      Mask tags seams aud  -> Mask tags seams $ join_ aud
    in join_ $ fmap f x

data Edge = Start | End
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

mapTime :: (t -> u) -> Audio t a -> Audio u a
mapTime f aud = case aud of
  Silence c t       -> Silence c $ f t
  Input   x         -> Input x
  Mix         xs    -> Mix         $ map (mapTime f) xs
  Merge       xs    -> Merge       $ map (mapTime f) xs
  Concatenate xs    -> Concatenate $ map (mapTime f) xs
  Gain g x          -> Gain g $ mapTime f x
  Take e t x        -> Take e (f t) $ mapTime f x
  Drop e t x        -> Drop e (f t) $ mapTime f x
  Fade e t x        -> Fade e (f t) $ mapTime f x
  Pad  e t x        -> Pad  e (f t) $ mapTime f x
  Resample x        -> Resample     $ mapTime f x
  Channels cs x     -> Channels cs  $ mapTime f x
  Stretch d x       -> Stretch d    $ mapTime f x
  Mask tags seams x -> Mask tags (map (fmap f) seams) $ mapTime f x

-- | Simple linear interpolation of an audio stream.
stretch :: (MonadResource m) => Double -> AudioSource m Float -> AudioSource m Float
stretch ratio src = AudioSource
  { rate     = rate src
  , frames   = ceiling $ fromIntegral (frames src) * ratio
  , channels = channels src
  , source   = source src =$= pipe 0 (repeat 0)
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

{-
-- To be used in the future. Stretches time and/or pitch separately.
stretch2 :: (MonadResource m) => Double -> AudioSource m Float -> AudioSource m Float
stretch2 ratio src = AudioSource
  { rate     = rate src
  , frames   = ceiling $ fromIntegral (frames src) * ratio
  , channels = channels src
  , source   = pipe $ source $ reorganize chunkSize src
  } where
    pipe upstream = do
      rb <- liftIO $ RB.new (round $ rate src) (channels src) RB.defaultOptions ratio 1
      liftIO $ RB.setMaxProcessSize rb chunkSize
      upstream =$= studyAll rb
      upstream =$= processAll rb
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
-}

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
  in AudioSource (s =$= go 0) r c l

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
  in AudioSource (s =$= go 0) r c l

data MaskSections
  = MaskFade Bool Frames Frames MaskSections
  | MaskStay Bool Frames MaskSections
  | MaskEnd Bool
  deriving (Eq, Ord, Show, Read)

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
  masker   (MaskEnd  b) = if b then CL.map id else return ()
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
            let (chunkA, chunkB) = V.splitAt ((done - total) * c) chunk
            leftover chunkB
            leftover chunkA
            masker m
  in AudioSource (s =$= masker sections) r c l

buildSource :: (MonadResource m) =>
  Audio Duration FilePath -> Action (AudioSource m Float)
buildSource aud = need (toList aud) >> case aud of
  -- optimizations
  Drop Start t (Input fin) -> liftIO $ sourceSndFrom t fin
  Drop Start (Seconds s) (Resample (Input fin)) -> buildSource $ Resample $ Drop Start (Seconds s) (Input fin)
  -- normal cases
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
  Fade Start t x -> fadeStart t <$> buildSource x
  Fade End t x -> fadeEnd t <$> buildSource x
  Resample x -> buildSource x >>= \src -> return $ if rate src == 44100
    then src
    else resampleTo 44100 SincMediumQuality src
  Channels cs x -> do
    src <- buildSource x
    let chans = splitChannels src
    case map (chans !!) cs of
      []     -> error "buildSource: can't select 0 channels"
      s : ss -> return $ foldl merge s ss
  Stretch d x -> stretch d <$> buildSource x
  Mask tags seams x -> renderMask tags seams <$> buildSource x
  where combine meth xs = mapM buildSource xs >>= \srcs -> case srcs of
          []     -> error "buildSource: can't combine 0 files"
          s : ss -> return $ foldl meth s ss

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Duration FilePath -> FilePath -> Action ()
buildAudio aud out = do
  src <- buildSource aud
  runAudio src out

runAudio :: AudioSource (ResourceT IO) Float -> FilePath -> Action ()
runAudio src out = let
  src' = clampFloat $ if takeExtension out == ".ogg" && channels src == 6
    then merge src $ silent (Frames 0) (rate src) 1
    -- this works around an issue with oggenc:
    -- it assumes 6 channels is 5.1 surround where the last channel
    -- is LFE, so instead we add a silent 7th channel
    else src
  withSndFormat fmt = do
    putNormal $ "Writing an audio expression to " ++ out
    liftIO $ runResourceT $ sinkSnd out fmt src'
    putNormal $ "Finished writing an audio expression to " ++ out
  in case takeExtension out of
    ".ogg" -> withSndFormat $ Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
    ".wav" -> withSndFormat $ Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
    ".mp3" -> do
      putNormal $ "Writing an audio expression to " ++ out
      liftIO $ runResourceT $ sinkMP3 out src'
      putNormal $ "Finished writing an audio expression to " ++ out
    ext -> error $ "runAudio: unknown audio output file extension " ++ ext

-- | Forces floating point samples to be in @[-1, 1]@.
-- libsndfile should do this, after https://github.com/kaoskorobase/hsndfile/pull/12
clampFloat :: (Monad m) => AudioSource m Float -> AudioSource m Float
clampFloat src = src { source = source src =$= CL.map clampVector } where
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

audioLength :: (MonadIO m) => FilePath -> m (Maybe Integer)
audioLength f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then liftIO $ Just . fromIntegral . Snd.frames <$> Snd.getFileInfo f
  else return Nothing

audioChannels :: (MonadIO m) => FilePath -> m (Maybe Int)
audioChannels f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then liftIO $ Just . Snd.channels <$> Snd.getFileInfo f
  else return Nothing

audioRate :: (MonadIO m) => FilePath -> m (Maybe Int)
audioRate f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then liftIO $ Just . Snd.samplerate <$> Snd.getFileInfo f
  else return Nothing

audioSeconds :: (MonadIO m) => FilePath -> m U.Seconds
audioSeconds f = do
  info <- liftIO $ Snd.getFileInfo f
  return $ realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info)

-- | Applies Rock Band's pan and volume lists
-- to turn a multichannel OGG input into a stereo output.
applyPansVols :: (Monad m) => [Float] -> [Float] -> AudioSource m Float -> AudioSource m Float
applyPansVols [-1, 1] [0, 0] src = src
applyPansVols pans    vols   src = AudioSource
  { rate     = rate src
  , frames   = frames src
  , channels = 2
  , source   = source src =$= CL.map applyChunk
  } where
    applyChunk :: V.Vector Float -> V.Vector Float
    applyChunk v = V.generate (vectorFrames v (channels src) * 2) $ \i -> do
      case quotRem i 2 of
        (frame, chan) -> let
          pvx = zip3 pans vols $ V.toList $ V.drop (frame * channels src) v
          wire (pan, volDB, sample) = let
            volRatio = 10 ** (volDB / 20)
            -- constant power panning: http://dsp.stackexchange.com/a/21736
            theta = pan * (pi / 4)
            panRatio = if chan == 0
              then (sqrt 2 / 2) * (cos theta - sin theta)
              else (sqrt 2 / 2) * (cos theta + sin theta)
            in panRatio * volRatio * sample
          in sum $ map wire pvx

-- | Like 'applyPansVols', but mixes into mono instead of stereo.
applyVolsMono :: (Monad m) => [Float] -> AudioSource m Float -> AudioSource m Float
applyVolsMono vols src = AudioSource
  { rate     = rate src
  , frames   = frames src
  , channels = 1
  , source   = source src =$= CL.map applyChunk
  } where
    vols' = take (channels src) $ vols ++ repeat 0
    applyChunk :: V.Vector Float -> V.Vector Float
    applyChunk v = V.generate (vectorFrames v $ channels src) $ \frame -> let
      vx = zip vols' $ V.toList $ V.drop (frame * channels src) v
      wire (volDB, sample) = let
        volRatio = 10 ** (volDB / 20)
        in 0.5 * volRatio * sample
      in sum $ map wire vx

crapMP3 :: (MonadResource m) => FilePath -> AudioSource m Float -> m ()
crapMP3 out src = let
  setup lame = liftIO $ do
    L.check $ L.setVBR lame L.VbrDefault
    L.check $ L.setVBRQ lame 9 -- lowest
  in sinkMP3WithHandle out setup $ resampleTo 16000 SincMediumQuality src

crapVorbis :: (MonadResource m) => FilePath -> AudioSource m Float -> m ()
crapVorbis out src = let
  setup hsnd = liftIO (setVBREncodingQuality hsnd 0) >>= \case
    True  -> return ()
    False -> error "crapVorbis: couldn't set crappy encoding quality"
  fmt = Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile
  in sinkSndWithHandle out fmt setup $ resampleTo 16000 SincMediumQuality src
