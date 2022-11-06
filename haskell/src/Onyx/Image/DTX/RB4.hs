{-
Ported from LibForge
https://github.com/maxton/LibForge
-}
{-# LANGUAGE MultiWayIf #-}
module Onyx.Image.DTX.RB4 where

import           Codec.Picture
import           Control.Monad
import           Data.Binary.Get
import           Onyx.Image.DTX

readPNGPS4 :: Get (Image PixelRGBA8)
readPNGPS4 = do
  magic <- getWord32le
  magic6 <- case magic of
    6 -> return True
    4 -> return False
    _ -> fail "Unknown texture magic"
  versionC <- (== 0xC) <$> getWord32le
  _hdrData <- getByteString $ if magic6
    then if versionC then 0x7C else 0xAC
    else 0xA4 -- LibForge says 0xA8 but this appears to be correct
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
  dataLen <- fromIntegral <$> getWord32le
  let numPixels = width * height
      arrange = arrangeRows (quot width 4) (quot height 4)
  img <- if
    | dataLen == numPixels * 4 -> undefined -- raw bitmap?
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
