{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE PatternSynonyms #-}
module Audio
( module Audio.Types
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

import           Audio.Types

import           Control.Exception               (evaluate)
import           Control.Monad.Trans.Resource    (MonadResource, ResourceT,
                                                  runResourceT)
import           Data.Binary.Get                 (getWord32le, runGetOrFail)
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import           Data.Char                       (toLower)
import           Data.Conduit                    (await, yield, (=$=))
import           Data.Conduit.Audio
import           Data.Conduit.Audio.LAME
import           Data.Conduit.Audio.LAME.Binding as L
import           Data.Conduit.Audio.SampleRate
import           Data.Conduit.Audio.Sndfile
import qualified Data.Conduit.List               as CL
import qualified Data.Digest.Pure.MD5            as MD5
import           Data.Foldable                   (toList)
import qualified Data.Vector.Storable            as V
import           Data.Word                       (Word8)
import           Development.Shake               (Action, liftIO, need,
                                                  putNormal)
import           Development.Shake.FilePath      (takeExtension)
import           Numeric                         (showHex)
import           SndfileExtra
import qualified Sound.File.Sndfile              as Snd
import qualified Sound.MIDI.Util                 as U

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
  Stretch d x -> stretch d <$> buildSource x
  where combine meth xs = mapM buildSource xs >>= \srcs -> case srcs of
          [] -> error "buildSource: can't combine 0 files"
          s : ss -> return $ foldl meth s ss

-- | Assumes 16-bit 44100 Hz audio files.
buildAudio :: Audio Duration FilePath -> FilePath -> Action ()
buildAudio aud out = do
  need $ toList aud
  src <- buildSource aud
  runAudio src out

runAudio :: AudioSource (ResourceT IO) Float -> FilePath -> Action ()
runAudio src out = do
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

audioLength :: FilePath -> IO (Maybe Integer)
audioLength f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then Just . fromIntegral . Snd.frames <$> Snd.getFileInfo f
  else return Nothing

audioChannels :: FilePath -> IO (Maybe Int)
audioChannels f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then Just . Snd.channels <$> Snd.getFileInfo f
  else return Nothing

audioRate :: FilePath -> IO (Maybe Int)
audioRate f = if takeExtension f `elem` [".flac", ".wav", ".ogg"]
  then Just . Snd.samplerate <$> Snd.getFileInfo f
  else return Nothing

audioSeconds :: FilePath -> IO U.Seconds
audioSeconds f = do
  info <- Snd.getFileInfo f
  return $ realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info)

-- | Applies Rock Band's pan and volume lists
-- to turn a multichannel OGG input into a stereo output.
applyPansVols :: (Monad m) => [Float] -> [Float] -> AudioSource m Float -> AudioSource m Float
applyPansVols pans vols src = AudioSource
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
            panRatio = if chan == 0
              then (negate pan + 1) * 0.5
              else (       pan + 1) * 0.5
              -- TODO: this should be improved. panning should be in dB (logarithmic)
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
