{-# LANGUAGE FlexibleContexts #-}
module Image (toDDS, toPNG_XBOX, readPNGXbox) where

import           Codec.Picture
import qualified Codec.Picture.STBIR        as STBIR
import           Control.Monad              (forM_, replicateM)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Binary.Get
import           Data.Bits                  (shiftL, shiftR, testBit, (.&.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Word                  (Word16, Word8)
import           Foreign
import           Foreign.C
import           System.IO.Unsafe           (unsafePerformIO)

#include "stb_dxt.h"

{#fun stb_compress_dxt_block
  { id `Ptr CUChar'
  , id `Ptr CUChar'
  , `CInt'
  , `CInt'
  } -> `()'
#}

compressDXTBlock :: Image PixelRGB8 -> B.ByteString
compressDXTBlock img = unsafePerformIO $ do
  let bs = B.pack $ do
        y <- [0..3]
        x <- [0..3]
        let PixelRGB8 r g b = pixelAt img x y
        [r, g, b, 0]
  allocaBytes 8 $ \dst -> do
    B.useAsCString bs $ \src -> do
      let cast1 = castPtr :: Ptr Word8 -> Ptr CUChar
          cast2 = castPtr :: Ptr CChar -> Ptr CUChar
          cast3 = castPtr :: Ptr Word8 -> Ptr CChar
      stb_compress_dxt_block
        (cast1 dst)
        (cast2 src)
        0
        2 -- high quality
      B.packCStringLen (cast3 dst, 8)

bits5to8 :: Word16 -> Word8
bits5to8 w = fromIntegral $ shiftL w' 3 + shiftR w' 2
  where w' = w .&. 0x1F

bits6to8 :: Word16 -> Word8
bits6to8 w = fromIntegral $ shiftL w' 2 + shiftR w' 4
  where w' = w .&. 0x3F

pixelFrom565 :: Word16 -> PixelRGB8
pixelFrom565 w = PixelRGB8
  (bits5to8 $ w `shiftR` 11)
  (bits6to8 $ w `shiftR` 5 )
  (bits5to8   w            )

mixProportions :: (Pixel a, Integral (PixelBaseComponent a)) =>
  Float -> Float -> a -> a -> a
mixProportions f0 f1 = mixWith $ \_ p0 p1 ->
  floor $ fromIntegral p0 * f0 + fromIntegral p1 * f1

contentDXT1 :: Image PixelRGB8 -> BL.ByteString
contentDXT1 img_ = execWriter $ do
  forM_ [256, 128, 64, 32, 16, 8, 4] $ \size -> do
    let img = STBIR.resize STBIR.defaultOptions size size img_
    forM_ [0, 4 .. size - 4] $ \y ->
      forM_ [0, 4 .. size - 4] $ \x -> do
        let chunk = generateImage (\cx cy -> pixelAt img (x + cx) (y + cy)) 4 4
        tell $ BL.fromStrict $ compressDXTBlock chunk

toDDS :: Image PixelRGB8 -> BL.ByteString
toDDS = let
  -- header stolen from an imagemagick-output file
  header = BL.pack [0x44,0x44,0x53,0x20,0x7C,0x00,0x00,0x00,0x07,0x10,0x0A,0x00,0x00,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x09,0x00,0x00,0x00,0x49,0x4D,0x41,0x47,0x45,0x4D,0x41,0x47,0x49,0x43,0x4B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x20,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x44,0x58,0x54,0x31,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x10,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
  in BL.append header . contentDXT1

toPNG_XBOX :: Image PixelRGB8 -> BL.ByteString
toPNG_XBOX = BL.append (BL.fromStrict pngXboxDXT1Signature) . flipWord16s . contentDXT1

readDXTChunk :: Bool -> Get (Image PixelRGB8)
readDXTChunk isDXT1 = do
  c0w <- getWord16le
  c1w <- getWord16le
  let c0 = pixelFrom565 c0w
      c1 = pixelFrom565 c1w
      c2 = if not isDXT1 || c0w > c1w
        then mixProportions (2/3) (1/3) c0 c1
        else mixProportions (1/2) (1/2) c0 c1
      c3 = if not isDXT1 || c0w > c1w
        then mixProportions (1/3) (2/3) c0 c1
        else PixelRGB8 0 0 0 -- should be transparent black
  rows <- replicateM 4 getWord8
  let gen x y = case ((rows !! y) `testBit` (x * 2 + 1), (rows !! y) `testBit` (x * 2)) of
        (False, False) -> c0
        (False,  True) -> c1
        ( True, False) -> c2
        ( True,  True) -> c3
  return $ generateImage gen 4 4

pngXboxDXT1Signature :: B.ByteString
pngXboxDXT1Signature = B.pack
  [ 0x01, 0x04, 0x08, 0x00, 0x00, 0x00, 0x04, 0x00
  , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

pngXboxDXT3Signature :: B.ByteString
pngXboxDXT3Signature = B.pack
  [ 0x01, 0x08, 0x18, 0x00, 0x00, 0x00, 0x04, 0x00
  , 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

flipWord16s :: BL.ByteString -> BL.ByteString
flipWord16s = let
  flipPairs (x : y : xs) = y : x : flipPairs xs
  flipPairs _            = []
  in BL.pack . flipPairs . BL.unpack

-- | Supports both official DXT1 and C3 DXT2/3.
readPNGXbox :: BL.ByteString -> Image PixelRGB8
readPNGXbox bs = let
  sig = BL.toStrict $ BL.take 32 bs
  in if sig == pngXboxDXT1Signature
    then let
      chunks = runGet (replicateM 4096 $ readDXTChunk True) $ flipWord16s $ BL.take 32768 $ BL.drop 32 bs
      gen x y = let
        (xa, xb) = quotRem x 4
        (ya, yb) = quotRem y 4
        in pixelAt (chunks !! (ya * 64 + xa)) xb yb
      in generateImage gen 256 256
    else if sig == pngXboxDXT3Signature
      then let
        chunks = runGet (replicateM 4096 $ skip 8 >> readDXTChunk False) $ flipWord16s $ BL.take 65536 $ BL.drop 32 bs
        gen x y = let
          (xa, xb) = quotRem x 4
          (ya, yb) = quotRem y 4
          in pixelAt (chunks !! (ya * 64 + xa)) xb yb
        in generateImage gen 256 256
      else generateImage (\_ _ -> PixelRGB8 255 0 255) 256 256
