{-# LANGUAGE LambdaCase #-}
module Onyx.Harmonix.MOGG
( moggToOggFiles, moggToOgg, oggToMogg, oggToMoggFiles, sourceVorbis
, encryptMOGG, encryptMOGGFiles, encryptMOGGToByteString
, decryptMOGG
, fixOldC3Mogg
, decryptBink
) where

import           Control.Applicative          (liftA2)
import           Control.Monad                (forM, forM_, guard, void)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource
import           Data.Binary.Get              (getWord32le)
import           Data.Binary.Put              (putLazyByteString, putWord32le,
                                               runPut)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as CA
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as MV
import           Foreign                      hiding (void)
import           Foreign.C
import           Onyx.Harmonix.MOGG.Crypt
import           Onyx.StackTrace
import           Onyx.Util.Binary             (runGetM)
import           Onyx.Util.Handle
import           Onyx.VorbisFile
import           System.IO                    (SeekMode (..), hSeek)

moggToOggFiles :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
moggToOggFiles mogg ogg = stackIO $ saveReadable (moggToOgg $ fileReadable mogg) ogg

-- Output is a 0x0A type (unencrypted) mogg, with header
decryptMOGG :: Readable -> Readable
decryptMOGG enc = makeHandle "decrypted mogg" $ do
  header <- useHandle enc $ \h -> do
    hSeek h AbsoluteSeek 16
    numPairs <- BL.hGet h 4 >>= runGetM getWord32le
    hSeek h AbsoluteSeek 12
    bufSizeAndTable <- BL.hGet h $ 4 + 4 + 8 * fromIntegral numPairs
    return $ runPut $ do
      putWord32le 0xA -- unencrypted
      putWord32le $ 20 + 8 * numPairs -- size of header before the ogg
      putWord32le 0x10 -- "ogg map version"
      putLazyByteString bufSizeAndTable
  shHeader <- byteStringSimpleHandle header
  shOgg <- rOpen enc >>= moggToOggHandles
  appendSimpleHandle shHeader shOgg

runVorbisReadable :: Readable -> (Maybe OggVorbis_File -> IO a) -> IO a
runVorbisReadable r fn = useHandle r $ \h -> do
  runResourceT $ loadVorbisHandle h >>= liftIO . fn . fmap snd

makeMoggTable :: Word32 -> Word32 -> [(Word32, Word32)] -> [(Word32, Word32)]
makeMoggTable bufSize audioLen = go 0 (0, 0) where
  go curSample prevPair pairs = if curSample >= audioLen
    then []
    else case pairs of
      []                       -> prevPair : go (curSample + bufSize) prevPair pairs
      p@(_bytes, samples) : ps -> if samples <= curSample
        then go curSample p ps
        else prevPair : go (curSample + bufSize) prevPair pairs

oggToMogg :: Readable -> Readable
oggToMogg ogg = makeHandle "ogg -> mogg" $ do
  (audioLen, table) <- runVorbisReadable ogg $ \case
    Nothing -> fail "Couldn't open Ogg file to generate MOGG"
    Just ov -> let
      go bytes = ov_raw_seek ov bytes >>= \case
        0 -> do
          samples <- ov_pcm_tell ov
          ((fromIntegral bytes, fromIntegral samples) :) <$> go (bytes + 0x8000)
        _ -> return []
      in do
        audioLen <- ov_pcm_total ov (-1)
        table <- go 0
        return (audioLen, table)
  let bufSize = 20000
      table' = makeMoggTable bufSize (fromIntegral audioLen) table
  shHeader <- byteStringSimpleHandle $ runPut $ do
    let len = fromIntegral $ length table' :: Word32
    putWord32le 0xA -- unencrypted
    putWord32le $ 20 + 8 * len -- size of header before the ogg
    putWord32le 0x10 -- "ogg map version"
    putWord32le bufSize -- buffer size
    putWord32le len -- number of pairs
    forM_ table' $ \(bytes, samples) -> do
      putWord32le bytes
      putWord32le samples
  shOgg <- rOpen ogg >>= simplifyHandle
  appendSimpleHandle shHeader shOgg

oggToMoggFiles :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
oggToMoggFiles ogg mogg = stackIO $ saveReadable (oggToMogg $ fileReadable ogg) mogg

sourceVorbis :: (MonadResource m, MonadIO f) => CA.Duration -> Readable -> f (CA.AudioSource m Float)
sourceVorbis pos ogg = liftIO $ runVorbisReadable ogg $ \case
  Nothing -> fail "Couldn't load OGG file"
  Just ovInit -> do
    (chans, rate) <- getChannelsRate ovInit
    total <- ov_pcm_total ovInit (-1)
    let seekTo = fromIntegral $ case pos of
          CA.Seconds secs -> CA.secondsToFrames secs (fromIntegral rate)
          CA.Frames fms   -> fms
    return $ CA.reorganize CA.chunkSize $ CA.AudioSource
      { CA.source = if total <= seekTo
        then return () -- seeked past end of file
        else resourceHandle ogg >>= \(h, releaseHandle) -> loadVorbisHandle h >>= \case
          Nothing -> return () -- failed to load, maybe log somewhere?
          Just (releaseOV, ov) -> let
            loop = do
              (numRead, ppfloat) <- liftIO $ alloca $ \pppfloat -> do
                with (-1) $ \psection -> do
                  numRead <- ov_read_float ov pppfloat (fromIntegral CA.chunkSize) psection
                  ppfloat <- peek pppfloat
                  return (numRead, ppfloat)
              if numRead <= 0
                then return ()
                else do
                  pfloats <- liftIO $ peekArray (fromIntegral chans) ppfloat
                  vchans <- liftIO $ forM pfloats $ \pfloat -> do
                    fptr <- newForeignPtr_ ((castPtr :: Ptr CFloat -> Ptr Float) pfloat)
                    let mv = MV.unsafeFromForeignPtr0 fptr $ fromIntegral numRead
                    V.freeze mv
                  C.yield $ CA.interleave vchans
                  loop
            in do
              void $ liftIO $ ov_pcm_seek ov seekTo
              loop
              releaseOV
              liftIO $ releaseHandle
      , CA.rate = fromIntegral rate
      , CA.channels = fromIntegral chans
      , CA.frames = max 0 $ fromIntegral total - fromIntegral seekTo
      }

-- logic to fix .mogg files encrypted with older versions of C3 CON Tools.
-- such moggs would work on 360 but not PS3, requiring reencryption,
-- but it was just because their keymask value was not set correctly.

c3PS3MaskBad, c3PS3MaskGood :: B.ByteString
c3PS3MaskBad  = B.pack [0xC3, 0xC3, 0xC3, 0xC3, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B]
c3PS3MaskGood = B.pack [0xA5, 0xCE, 0xFD, 0x06, 0x11, 0x93, 0x23, 0x21, 0xF8, 0x87, 0x85, 0xEA, 0x95, 0xE4, 0x94, 0xD4]

patchPosition :: Int -> B.ByteString -> B.ByteString -> B.ByteString
patchPosition pos patch origData = if pos >= B.length origData || pos + B.length patch <= 0
  then origData -- no change, patch is either before or after this chunk
  else B.take (B.length origData) $ B.concat
    [ B.take pos origData
    , B.drop (negate pos) patch
    , B.drop (pos + B.length patch) origData
    ]

fixOldC3Mogg :: Readable -> Readable
fixOldC3Mogg r = Readable
  { rFilePath = Nothing
  , rOpen = do
    -- first figure out if we need to patch
    patchLocation <- useHandle r $ \h -> do
      hSeek h AbsoluteSeek 16
      numEntries <- BL.hGet h 4 >>= runGetM getWord32le
      let patchLocation = 20 + fromIntegral numEntries * 8 + 16 + 16
      hSeek h AbsoluteSeek patchLocation
      mask <- B.hGet h 16
      return $ guard (mask == c3PS3MaskBad) >> Just patchLocation
    case patchLocation of
      -- need to patch, wrap the original handle and modify output
      Just loc -> do
        h <- rOpen r
        sh <- simplifyHandle h
        openSimpleHandle (handleLabel h <> " | fixOldC3Mogg") sh
          { shRead = \n -> do
            pos <- shTell sh
            origData <- shRead sh n
            return $ patchPosition (fromIntegral $ loc - pos) c3PS3MaskGood origData
          }
      -- don't need to patch, just pass through
      Nothing -> rOpen r
  }
