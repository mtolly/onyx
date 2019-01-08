-- | Functions dealing with the DXT texture formats used for album art by RB games.
{-# LANGUAGE BinaryLiterals   #-}
{-# LANGUAGE FlexibleContexts #-}
module Image (toDXT1File, DXTFormat(..), readRBImage) where

import           Codec.Picture
import qualified Codec.Picture.STBIR        as STBIR
import           Control.Monad              (forM_, guard, replicateM)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Array                 (listArray, (!))
import           Data.Binary.Get
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
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

data DXTFormat
  = DDS
  | PNGXbox
  | PNGWii
  deriving (Eq)

readDXTChunk :: DXTFormat -> Bool -> Get (Image PixelRGB8)
readDXTChunk fmt isDXT1 = do
  let getColors = case fmt of DDS -> getWord16le; _ -> getWord16be
  c0w <- getColors
  c1w <- getColors
  let c0 = pixelFrom565 c0w
      c1 = pixelFrom565 c1w
      c2 = if not isDXT1 || c0w > c1w
        then mixProportions (2/3) (1/3) c0 c1
        else mixProportions (1/2) (1/2) c0 c1
      c3 = if not isDXT1 || c0w > c1w
        then mixProportions (1/3) (2/3) c0 c1
        else PixelRGB8 0 0 0 -- should be transparent black
  rows <- replicateM 4 getWord8
  let gen x y = let
        x' = case fmt of
          PNGWii  -> 3 - x -- wii blocks flipped horizontally
          _       -> x
        y' = case fmt of
          PNGXbox -> y `complementBit` 0 -- xbox rows swapped in pairs
          _       -> y
        in case ((rows !! y') `testBit` (x' * 2 + 1), (rows !! y') `testBit` (x' * 2)) of
          (False, False) -> c0
          (False,  True) -> c1
          ( True, False) -> c2
          ( True,  True) -> c3
  return $ generateImage gen 4 4

writeDXT1Chunk :: DXTFormat -> Image PixelRGB8 -> BB.Builder
writeDXT1Chunk fmt chunk = let
  dds = compressDXTBlock chunk
  in case fmt of
    DDS     -> BB.byteString dds
    PNGXbox -> mconcat $ map (BB.word8 . B.index dds) [1, 0, 3, 2, 5, 4, 7, 6]
    PNGWii  -> let
      cols = map (BB.word8 . B.index dds) [1, 0, 3, 2]
      pxs  = map (BB.word8 . flipHoriz . B.index dds) [4, 5, 6, 7]
      flipHoriz b
        =   ((b .&.       0b11) `shiftL` 6)
        .|. ((b .&.     0b1100) `shiftL` 2)
        .|. ((b .&.   0b110000) `shiftR` 2)
        .|. ((b .&. 0b11000000) `shiftR` 6)
      in mconcat $ cols ++ pxs

toDXT1File :: DXTFormat -> Image PixelRGB8 -> BL.ByteString
toDXT1File fmt img_ = BB.toLazyByteString $ execWriter $ do
  tell $ BB.byteString $ case fmt of
    DDS     -> ddsDXT1Signature
    PNGXbox -> pngXboxDXT1Signature
    PNGWii  -> pngWii256DXT1Signature
  let mipmaps = forM_ [256, 128, 64, 32, 16, 8, 4] go
      go size = do
        let img = STBIR.resize STBIR.defaultOptions size size img_
            chunkAt x y = generateImage (\cx cy -> pixelAt img (x + cx) (y + cy)) 4 4
        case fmt of
          PNGWii -> do
            -- first 4x4 blocks are 4-grouped into 8x8 in reading-order,
            -- then those 8x8 blocks are in reading-order
            forM_ [0, 8 .. size - 8] $ \bigY ->
              forM_ [0, 8 .. size - 8] $ \bigX ->
                forM_ [bigY, bigY + 4] $ \y ->
                  forM_ [bigX, bigX + 4] $ \x ->
                    tell $ writeDXT1Chunk fmt $ chunkAt x y
          _ -> do
            -- simple reading-order
            forM_ [0, 4 .. size - 4] $ \y ->
              forM_ [0, 4 .. size - 4] $ \x ->
                tell $ writeDXT1Chunk fmt $ chunkAt x y
  case fmt of
    DDS     -> mipmaps
    PNGXbox -> mipmaps
    PNGWii  -> go 256

ddsDXT1Signature :: B.ByteString
ddsDXT1Signature = B.pack -- header stolen from an imagemagick-output file
  [0x44,0x44,0x53,0x20,0x7C,0x00,0x00,0x00,0x07,0x10,0x0A,0x00,0x00,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x09,0x00,0x00,0x00,0x49,0x4D,0x41,0x47,0x45,0x4D,0x41,0x47,0x49,0x43,0x4B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x20,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x44,0x58,0x54,0x31,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x10,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]

pngXboxDXT1Signature :: B.ByteString
pngXboxDXT1Signature = B.pack
  [ 0x01, 0x04, 0x08, 0x00, 0x00, 0x00, 0x04, 0x00
  , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

pngWii256DXT1Signature :: B.ByteString
pngWii256DXT1Signature = B.pack
  [ 0x01, 0x04, 0x48, 0x00, 0x00, 0x00, 0x04, 0x00
  , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  ]

-- | Supports .png_xbox in both official DXT1 and C3 DXT2/3, and also .png_wii.
readRBImage :: BL.ByteString -> Image PixelRGB8
readRBImage bs = let
  readWiiChunk = fmap (arrangeRows 2 2) $ replicateM 4 $ readDXTChunk PNGWii True
  arrangeRows cols rows xs = let
    w = imageWidth  $ head xs
    h = imageHeight $ head xs
    xs' = listArray (0, cols * rows - 1) xs
    gen x y = let
      (xa, xb) = quotRem x w
      (ya, yb) = quotRem y h
      in pixelAt (xs' ! (ya * cols + xa)) xb yb
    in generateImage gen (w * cols) (h * rows)
  parseImage = do
    1             <- getWord8
    bitsPerPixel  <- getWord8
    format        <- getWord32le
    _mipmaps      <- getWord8
    width         <- fromIntegral <$> getWord16le
    height        <- fromIntegral <$> getWord16le
    _bytesPerLine <- getWord16le
    zeroes        <- getByteString 19
    guard $ B.all (== 0) zeroes
    case (bitsPerPixel, format) of
      -- Xbox DXT1
      (0x04, 0x08) -> fmap (arrangeRows (quot width 4) (quot height 4))
        $ replicateM (quot (width * height) 16) $ readDXTChunk PNGXbox True
      -- Xbox DXT3
      (0x08, 0x18) -> fmap (arrangeRows (quot width 4) (quot height 4))
        $ replicateM (quot (width * height) 16) $ skip 8 >> readDXTChunk PNGXbox False
      -- Wii DXT1
      (0x04, 0x48) -> fmap (arrangeRows (quot width 8) (quot height 8))
        $ replicateM (quot (width * height) 64) readWiiChunk
      _ -> fail "Unrecognized HMX image format"
  in case runGetOrFail parseImage bs of
    Left  _           -> generateImage (\_ _ -> PixelRGB8 255 0 255) 256 256
    Right (_, _, img) -> img
