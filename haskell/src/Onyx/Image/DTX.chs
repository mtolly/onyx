-- | Functions dealing with the DXT texture formats used for album art by RB games.
{-# LANGUAGE BinaryLiterals    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Image.DTX (toDXT1File, toHMXPS2, DXTFormat(..), readRBImage, readRBImageMaybe, readDDS, arrangeRows, readDXTChunk, swapPNGXboxPS3) where

import           Codec.Picture
import qualified Codec.Picture.STBIR        as STBIR
import           Codec.Picture.Types        (promotePixel)
import           Control.Monad              (forM_, guard, replicateM, unless)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Array                 (listArray, (!))
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString            as B
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe                 (fromMaybe)
import           Foreign
import           Foreign.C
import           System.IO.Unsafe           (unsafePerformIO)
import qualified Data.Vector.Storable as V
import Onyx.Xbox.STFS (runGetM)

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
  | PNGPS3
  | PNGWii
  deriving (Eq)

readDXTChunk :: DXTFormat -> Bool -> Get (Image PixelRGBA8)
readDXTChunk fmt isDXT1 = do
  let getColors = case fmt of
        DDS     -> getWord16le
        PNGPS3  -> getWord16le
        PNGXbox -> getWord16be
        PNGWii  -> getWord16be
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
  return $ pixelMap promotePixel $ generateImage gen 4 4

writeDXT1Chunk :: DXTFormat -> Image PixelRGB8 -> BB.Builder
writeDXT1Chunk fmt chunk = let
  dds = compressDXTBlock chunk
  in case fmt of
    DDS     -> BB.byteString dds
    PNGPS3  -> BB.byteString dds
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
    PNGPS3  -> pngXboxDXT1Signature -- identical, apparently
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
    PNGPS3  -> mipmaps
    PNGWii  -> go 256

-- TODO support alpha pixels (needed for "distressed photo" bonus song art)
toHMXPS2 :: Image PixelRGB8 -> BL.ByteString
toHMXPS2 img_ = BB.toLazyByteString $ execWriter $ do
  tell $ BB.byteString $ B.pack
    [ 0x01, 0x08, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00
    , 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    ]
  let img = STBIR.resize STBIR.defaultOptions 256 256 img_
      (indexed, palette) = palettize PaletteOptions
        { paletteCreationMethod = MedianMeanCut
        , enableImageDithering = True
        , paletteColorCount = 256
        } img
      paletteColors = do
        x <- [0 .. imageWidth  palette - 1]
        y <- [0 .. imageHeight palette - 1]
        return $ pixelAt palette x y
  forM_ (take 256 $ paletteColors <> repeat (PixelRGB8 0 0 0)) $ \(PixelRGB8 r g b) -> do
    tell $ BB.byteString $ B.pack [r, g, b, 0x80]
  forM_ (V.toList $ imageData indexed) $ \i -> do
    tell $ BB.word8 $ flip34 i

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

arrangeRows :: (Pixel a) => Int -> Int -> [Image a] -> Image a
arrangeRows cols rows xs = let
  w = imageWidth  $ head xs
  h = imageHeight $ head xs
  xs' = listArray (0, cols * rows - 1) xs
  gen x y = let
    (xa, xb) = quotRem x w
    (ya, yb) = quotRem y h
    in pixelAt (xs' ! (ya * cols + xa)) xb yb
  in generateImage gen (w * cols) (h * rows)

-- | Currently just hacked together for Rocksmith album art, otherwise untested
readDDS :: BL.ByteString -> Maybe (Image PixelRGBA8)
readDDS bs = let
  parseImage = do
    magic <- getByteString 4 -- "DDS "
    unless (magic == "DDS ") $ fail "Invalid magic number at start of DDS"
    -- header
    _dwSize <- getWord32le -- always 0x7C
    _dwFlags <- getWord32le
    dwHeight <- getWord32le
    dwWidth <- getWord32le
    _dwPitchOrLinearSize <- getWord32le
    _dwDepth <- getWord32le
    _dwMipMapCount <- getWord32le
    skip $ 11 * 4 -- DWORD dwReserved1[11]
    -- pixel format
    _fmt_dwSize <- getWord32le
    _fmt_dwFlags <- getWord32le
    fmt_dwFourCC <- getByteString 4
    _fmt_dwRGBBitCount <- getWord32le
    _fmt_dwRBitMask <- getWord32le
    _fmt_dwGBitMask <- getWord32le
    _fmt_dwBBitMask <- getWord32le
    _fmt_dwABitMask <- getWord32le
    -- end pixel format
    _dwCaps <- getWord32le
    _dwCaps2 <- getWord32le
    _dwCaps3 <- getWord32le
    _dwCaps4 <- getWord32le
    skip 4 -- DWORD dwReserved2
    -- end header
    let width = fromIntegral dwWidth
        height = fromIntegral dwHeight
    case fmt_dwFourCC of
      "DXT1" -> fmap (arrangeRows (quot width 4) (quot height 4))
        $ replicateM (quot (width * height) 16) $ readDXTChunk DDS True
      -- these are untested
      f | elem f ["DXT2", "DXT3", "DXT4", "DXT5"] -> fmap (arrangeRows (quot width 4) (quot height 4))
        $ replicateM (quot (width * height) 16) $ readDXTChunk DDS False
      _ -> fail "Unrecognized DDS compression format"
  in runGetM parseImage bs

-- | Supports .png_xbox in both official DXT1 and C3 DXT2/3, and also .png_wii.
readRBImageMaybe :: Bool -> BL.ByteString -> Maybe (Image PixelRGBA8)
readRBImageMaybe isPS3 bs = let
  readWiiChunk = fmap (arrangeRows 2 2) $ replicateM 4 $ readDXTChunk PNGWii True
  readRGBA = do
    -- used in all the bitmap (non-dxt) types
    r <- getWord8
    g <- getWord8
    b <- getWord8
    a <- getWord8 -- goes from 0 to 0x80
    return [r, g, b, if a >= 0x80 then 0xFF else a * 2]
  palette8bit, palette4bit :: Int -> Int -> Get (Image PixelRGBA8)
  palette8bit width height = do
    palette <- replicateM 256 readRGBA
    fmap (Image width height . V.fromList . concat) $ replicateM (width * height) $ do
      i <- getWord8
      return $ palette !! fromIntegral (flip34 i)
  palette4bit width height = do
    palette <- replicateM 16 readRGBA
    fmap (Image width height . V.fromList . concat . concat) $ replicateM (quot (width * height) 2) $ do
      i <- getWord8
      let high = (i `shiftR` 4) .&. 0xF
          low  = i              .&. 0xF
      return [palette !! fromIntegral low, palette !! fromIntegral high]
  parseImage = getWord8 >>= \case
    0 -> do
      -- Amplitude (PS2)
      bitsPerPixel  <- getWord8
      format        <- getWord8
      _mipmaps      <- getWord8 -- usually 0 but not in arenas/constructo/gen/tall_building01_05.bmp.gz
      width         <- fromIntegral <$> getWord16le
      height        <- fromIntegral <$> getWord16le
      _bytesPerLine <- getWord16le -- assuming this is like later format
      zeroes        <- getByteString 6
      guard $ B.all (== 0) zeroes
      case (bitsPerPixel, format) of
        -- 8-bit palette
        (0x08, 0x03) -> palette8bit width height
        -- 4-bit palette
        (0x04, 0x03) -> palette4bit width height
        -- non-paletted simple bitmap, see metagame/image/gen/turret_arm.bmp.gz
        (0x20, 0x03) -> fmap (Image width height . V.fromList . concat) $ replicateM (width * height) readRGBA
        _ -> fail "Unrecognized HMX image format"
    1 -> do
      -- GH1 and onward
      bitsPerPixel  <- getWord8
      format        <- getWord32le
      _mipmaps      <- getWord8
      width         <- fromIntegral <$> getWord16le
      height        <- fromIntegral <$> getWord16le
      _bytesPerLine <- getWord16le
      zeroes        <- getByteString 19
      guard $ B.all (== 0) zeroes
      let xboxOrPS3 = if isPS3 then PNGPS3 else PNGXbox
      case (bitsPerPixel, format) of
        -- Xbox DXT1
        (0x04, 0x08) -> fmap (arrangeRows (quot width 4) (quot height 4))
          $ replicateM (quot (width * height) 16) $ readDXTChunk xboxOrPS3 True
        -- Xbox DXT3
        (0x08, 0x18) -> fmap (arrangeRows (quot width 4) (quot height 4))
          $ replicateM (quot (width * height) 16) $ skip 8 >> readDXTChunk xboxOrPS3 False
        -- Wii DXT1
        (0x04, 0x48) -> fmap (arrangeRows (quot width 8) (quot height 8))
          $ replicateM (quot (width * height) 64) readWiiChunk
        -- PS2 BMP, 8-bit indices; typical color images
        (0x08, 0x03) -> palette8bit width height
        -- PS2 BMP, 4-bit indices; seen in GH1 for fonts and other B/W images, e.g. ghui/image/gen/hand_gw.bmp_ps2
        (0x04, 0x03) -> palette4bit width height
        _ -> fail "Unrecognized HMX image format"
    _ -> fail "Unrecognized HMX image format"
  in runGetM parseImage bs

-- PS2 palette indices swap bits 3 and 4 for some reason
flip34 :: Word8 -> Word8
flip34 i
  = (if i `testBit` 3 then (`setBit` 4) else (`clearBit` 4))
  . (if i `testBit` 4 then (`setBit` 3) else (`clearBit` 3))
  $ i

readRBImage :: Bool -> BL.ByteString -> Image PixelRGBA8
readRBImage isPS3 = let
  magenta = generateImage (\_ _ -> PixelRGBA8 255 0 255 255) 256 256
  in fromMaybe magenta . readRBImageMaybe isPS3

-- Quick converts between .png_xbox and .png_ps3
swapPNGXboxPS3 :: BL.ByteString -> BL.ByteString
swapPNGXboxPS3 b = let
  swapBytes (x : y : xs) = y : x : swapBytes xs
  swapBytes xs           = xs
  in BL.take 0x20 b <> BL.pack (swapBytes $ BL.unpack $ BL.drop 0x20 b)
