{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Onyx.Image.DXT.RB4 where

import           Codec.Picture
import           Control.Monad
import           Data.Binary.Get
import           Onyx.Image.DXT

data PNGPS4Magic
  = PNGPS4Magic_6
  | PNGPS4Magic_4
  | PNGPS4Magic_3

readPNGPS4 :: Get (Image PixelRGBA8)
readPNGPS4 = do
  magic <- getWord32le >>= \case
    6 -> return PNGPS4Magic_6
    4 -> return PNGPS4Magic_4
    3 -> return PNGPS4Magic_3 -- accordingtoyou
    n -> fail $ "Unknown texture magic: " <> show n
  versionC <- (== 0xC) <$> getWord32le
  _hdrData <- getByteString $ case magic of
    PNGPS4Magic_6 -> if versionC then 0x7C else 0xAC
    PNGPS4Magic_4 -> 0xA4 -- LibForge says 0xA8 but this appears to be correct
    PNGPS4Magic_3 -> 0x8C -- just guessing
  mipmapLevels <- getWord32le
  mipmaps <- replicateM (fromIntegral mipmapLevels) $ do
    width <- fromIntegral <$> getWord32le
    height <- fromIntegral <$> getWord32le
    flags <- getInt32le
    when versionC $ skip 8
    return (width, height, flags)
  skip 4
  -- just gonna read first mipmap
  (width, height, _) <- case mipmaps of
    mm : _ -> return mm
    []     -> fail "No mipmaps found in image"
  let numPixels = width * height
      arrange = arrangeRows (quot width 4) (quot height 4)
  dataLen <- case magic of
    PNGPS4Magic_3 -> return $ quot numPixels 2 -- not sure how this works but this appears to be right
    _             -> fromIntegral <$> getWord32le
  img <- if
    | dataLen == numPixels * 4 -> fail "size indicates raw bitmap in .png_ps4?"
    | dataLen == numPixels -> fmap arrange
      $ replicateM (quot numPixels 16) $ skip 8 >> readDXTChunk PNGPS3 False
    | dataLen * 2 == numPixels -> fmap arrange
      $ replicateM (quot numPixels 16) $ readDXTChunk PNGPS3 False
    | otherwise -> fail $ unwords
      [ "Unexpected size of data for image's first mipmap:"
      , "width", show width
      , "height", show height
      , "data", show dataLen
      ]
  -- after this is the rest of the mipmaps, then a 0x1C byte footer
  return img
