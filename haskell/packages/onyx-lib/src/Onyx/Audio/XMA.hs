{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-|
Encoding audio to XMA (the Xbox 360's hardware audio codec),
using the encoder in cbits/xma_encode.c.
-}
module Onyx.Audio.XMA
( defaultXMAQuality
, encodeXMA1
, encodeXMA2
) where

import           Control.Monad          (forM, forM_, when)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Conduit           (runConduit, (.|))
import qualified Data.Conduit.Audio     as CA
import qualified Data.Conduit.List      as CL
import           Data.Int               (Int64)
import qualified Data.Vector.Storable   as V
import           Data.Word              (Word32, Word8)
import           Foreign
import           Foreign.C.Types
import           Onyx.Audio.FSB         (FSB3XMA (..), XMA1Contents (..),
                                         XMA2Contents (..))

data XmaEncoder

foreign import ccall unsafe "xma_encoder_new"
  c_xma_encoder_new :: CInt -> CInt -> CInt -> IO (Ptr XmaEncoder)

foreign import ccall unsafe "&xma_encoder_free"
  p_xma_encoder_free :: FunPtr (Ptr XmaEncoder -> IO ())

foreign import ccall safe "xma_encoder_push"
  c_xma_encoder_push :: Ptr XmaEncoder -> Ptr CFloat -> CInt -> IO CInt

foreign import ccall safe "xma_encoder_finish"
  c_xma_encoder_finish :: Ptr XmaEncoder -> IO CInt

foreign import ccall unsafe "xma_encoder_samples"
  c_xma_encoder_samples :: Ptr XmaEncoder -> IO Int64

foreign import ccall unsafe "xma_encoder_frames"
  c_xma_encoder_frames :: Ptr XmaEncoder -> IO Int64

foreign import ccall unsafe "xma_encoder_trim_end"
  c_xma_encoder_trim_end :: Ptr XmaEncoder -> IO CInt

foreign import ccall safe "xma2_pack"
  c_xma2_pack
    :: Ptr (Ptr XmaEncoder) -> CInt -> CInt
    -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word32) -> Ptr CSize
    -> IO CInt

foreign import ccall safe "xma1_pack"
  c_xma1_pack
    :: Ptr XmaEncoder
    -> Ptr (Ptr Word8) -> Ptr CSize -> Ptr (Ptr Word32) -> Ptr CSize -> Ptr Word32
    -> IO CInt

foreign import ccall unsafe "xma_free"
  c_xma_free :: Ptr a -> IO ()

-- | Same as xma2encode's default. Goes from 1 to 100.
defaultXMAQuality :: Int
defaultXMAQuality = 60

newEncoder :: Int -> Int -> Int -> IO (ForeignPtr XmaEncoder)
newEncoder chans rate quality = do
  p <- c_xma_encoder_new (fromIntegral chans) (fromIntegral rate) (fromIntegral quality)
  when (p == nullPtr) $ fail $
    "Couldn't create XMA encoder for " <> show chans <> " channels at " <> show rate <> " Hz"
  newForeignPtr p_xma_encoder_free p

-- | Encodes audio as one stream per stereo pair of channels,
-- plus a mono stream at the end if there is an odd channel.
encodeStreams :: (MonadIO m) => Int -> CA.AudioSource m Float -> m [ForeignPtr XmaEncoder]
encodeStreams quality src = do
  let chans = CA.channels src
      (pairs, odd') = quotRem chans 2
      layout = [ (2 * i, 2) | i <- [0 .. pairs - 1] ] <> [ (2 * pairs, 1) | odd' == 1 ]
  when (chans < 1) $ liftIO $ fail "Can't encode XMA with no audio channels"
  encs <- liftIO $ forM layout \(_, n) -> newEncoder n (round $ CA.rate src) quality
  let pushChunk chunk = liftIO $ forM_ (zip layout encs) \((start, n), enc) -> do
        let frames = V.length chunk `quot` chans
            part = V.generate (frames * n) \i -> case quotRem i n of
              (f, c) -> V.unsafeIndex chunk $ f * chans + start + c
        withForeignPtr enc \penc -> V.unsafeWith part \pdata -> do
          res <- c_xma_encoder_push penc (castPtr pdata) (fromIntegral frames)
          when (res /= 0) $ fail "XMA encoding failed"
  runConduit $ CA.source src .| CL.mapM_ pushChunk
  liftIO $ forM_ encs \enc -> withForeignPtr enc \penc -> do
    res <- c_xma_encoder_finish penc
    when (res /= 0) $ fail "XMA encoding failed"
  return encs

-- | Takes ownership of a buffer allocated by the C code.
takeBuffer :: (Storable a) => Ptr (Ptr a) -> Ptr CSize -> IO [a]
takeBuffer pp plen = do
  p <- peek pp
  len <- peek plen
  xs <- peekArray (fromIntegral len) p
  c_xma_free p
  return xs

takeBytes :: Ptr (Ptr Word8) -> Ptr CSize -> IO B.ByteString
takeBytes pp plen = do
  p <- peek pp
  len <- peek plen
  bs <- B.packCStringLen (castPtr p, fromIntegral len)
  c_xma_free p
  return bs

-- | Encodes to XMA2, with one stream for each pair of channels.
-- FSB4 (GHWT and later) uses 16 packets (32 KB) per block.
encodeXMA2 :: (MonadIO m) => Int -> Int -> CA.AudioSource m Float -> m XMA2Contents
encodeXMA2 quality packetsPerBlock src = do
  encs <- encodeStreams quality src
  liftIO $ withMany withForeignPtr encs \ptrs -> do
    samples <- c_xma_encoder_samples $ head ptrs
    (bytes, seekTable) <- withArrayLen ptrs \n parr ->
      alloca \pout -> alloca \plen -> alloca \pseek -> alloca \pseeklen -> do
        res <- c_xma2_pack parr (fromIntegral n) (fromIntegral packetsPerBlock) pout plen pseek pseeklen
        when (res /= 0) $ fail "Failed to pack XMA2 data"
        (,) <$> takeBytes pout plen <*> takeBuffer pseek pseeklen
    return XMA2Contents
      { xma2Channels        = CA.channels src
      , xma2Rate            = round $ CA.rate src
      , xma2Samples         = fromIntegral samples
      , xma2PacketsPerBlock = packetsPerBlock
      , xma2SeekTable       = Just seekTable
      , xma2Data            = BL.fromStrict bytes
      }

-- | Encodes mono or stereo audio to XMA1, for an FSB3 (Guitar Hero 3).
encodeXMA1 :: (MonadIO m) => Int -> CA.AudioSource m Float -> m FSB3XMA
encodeXMA1 quality src = do
  when (CA.channels src > 2) $ liftIO $ fail $
    "XMA1 encoding only supports mono or stereo, but got " <> show (CA.channels src) <> " channels"
  encs <- encodeStreams quality src
  liftIO $ withMany withForeignPtr encs \ptrs -> do
    let p = head ptrs
    samples <- c_xma_encoder_samples p
    frames <- c_xma_encoder_frames p
    trimEnd <- c_xma_encoder_trim_end p
    (bytes, seekTable, lastFrame) <-
      alloca \pout -> alloca \plen -> alloca \pseek -> alloca \pseeklen -> alloca \plast -> do
        res <- c_xma1_pack p pout plen pseek pseeklen plast
        when (res /= 0) $ fail "Failed to pack XMA1 data"
        (,,) <$> takeBytes pout plen <*> takeBuffer pseek pseeklen <*> peek plast
    when (frames == 0) $ fail "No XMA frames encoded"
    return FSB3XMA
      { fsb3XMA = XMA1Contents
        { xma1Channels  = CA.channels src
        , xma1Rate      = round $ CA.rate src
        , xma1SeekTable = seekTable
        , xma1Data      = BL.fromStrict bytes
        }
      , fsb3XMASamples   = fromIntegral samples
      , fsb3XMALastFrame = lastFrame
      , fsb3XMASubframes = fromIntegral $ (512 - trimEnd + 127) `quot` 128
      }
