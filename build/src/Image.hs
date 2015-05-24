{-# LANGUAGE FlexibleContexts #-}
module Image where

import Codec.Picture
import Codec.Picture.Types
import Data.Bits (shiftR, shiftL)
import Data.Word (Word16)
import Control.Monad (forM_, guard)
import qualified Data.ByteString as B
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.IO (withBinaryFile, IOMode(..))

anyToRGB8 :: DynamicImage -> Image PixelRGB8
anyToRGB8 dyn = case dyn of
  ImageY8 i -> promoteImage i
  ImageY16 i -> anyToRGB8 $ ImageRGB16 $ promoteImage i
  ImageYF i -> anyToRGB8 $ ImageRGBF $ promoteImage i
  ImageYA8 i -> promoteImage i
  ImageYA16 i -> anyToRGB8 $ ImageRGBA16 $ promoteImage i
  ImageRGB8 i -> i
  ImageRGB16 i -> pixelMap (\(PixelRGB16 r g b) -> PixelRGB8 (f r) (f g) (f b)) i
    where f w16 = fromIntegral $ w16 `shiftR` 8
  ImageRGBF i -> pixelMap (\(PixelRGBF r g b) -> PixelRGB8 (f r) (f g) (f b)) i
    where f w16 = floor $ min 0xFF $ w16 * 0x100
  ImageRGBA8 i -> dropAlphaLayer i
  ImageRGBA16 i -> anyToRGB8 $ ImageRGB16 $ dropAlphaLayer i
  ImageYCbCr8 i -> convertImage i
  ImageCMYK8 i -> convertImage i
  ImageCMYK16 i -> anyToRGB8 $ ImageRGB16 $ convertImage i

-- | Scales an image using the bilinear interpolation algorithm.
scaleBilinear
  :: (Pixel a, Integral (PixelBaseComponent a))
  => Int -> Int -> Image a -> Image a
scaleBilinear w' h' img = generateImage f w' h' where
  f x' y' = let
    x, y :: Rational
    x = fromIntegral x' / fromIntegral (w' - 1) * fromIntegral (imageWidth  img - 1)
    y = fromIntegral y' / fromIntegral (h' - 1) * fromIntegral (imageHeight img - 1)
    (xi, xf) = properFraction x
    (yi, yf) = properFraction y
    gx _ c1 c2 = round $ fromIntegral c1 * (1 - xf) + fromIntegral c2 * xf
    gy _ c1 c2 = round $ fromIntegral c1 * (1 - yf) + fromIntegral c2 * yf
    in case (xf, yf) of
      (0, 0) -> pixelAt img xi yi
      (0, _) -> mixWith gy (pixelAt img xi yi) (pixelAt img xi (yi + 1))
      (_, 0) -> mixWith gx (pixelAt img xi yi) (pixelAt img (xi + 1) yi)
      (_, _) -> mixWith gy
        (mixWith gx (pixelAt img xi yi      ) (pixelAt img (xi + 1) yi      ))
        (mixWith gx (pixelAt img xi (yi + 1)) (pixelAt img (xi + 1) (yi + 1)))

pixel565 :: PixelRGB8 -> Word16
pixel565 (PixelRGB8 r g b) = sum
  [ (fromIntegral r `shiftR` 3) `shiftL` 11 -- 5 bits
  , (fromIntegral g `shiftR` 2) `shiftL` 5  -- 6 bits
  ,  fromIntegral b `shiftR` 3              -- 5 bits
  ]

pixelFrom565 :: Word16 -> PixelRGB8
pixelFrom565 w = PixelRGB8
  (fromIntegral $ (w `shiftR` 11) `shiftL` 3)
  (fromIntegral $ (w `shiftR` 5 ) `shiftL` 2)
  (fromIntegral $  w              `shiftL` 3)

colorDiff :: PixelRGB8 -> PixelRGB8 -> Int
colorDiff (PixelRGB8 r0 g0 b0) (PixelRGB8 r1 g1 b1) = sum
  [ fromIntegral $ max r0 r1 - min r0 r1
  , fromIntegral $ max g0 g1 - min g0 g1
  , fromIntegral $ max b0 b1 - min b0 b1
  ]

-- | Computes a valid palette for DXT1 encoding a 4x4 chunk.
-- In (c0, c1, c2, c3), c0 and c1 are the two stated colors for the chunk,
-- and c2 and c3 are at third-intervals between c0 and c1
-- (c2 closer to c0, c3 closer to c1).
findPalette :: Image PixelRGB8 -> (PixelRGB8, PixelRGB8, PixelRGB8, PixelRGB8)
findPalette img = let
  pixels = [ pixelAt img x y | x <- [0 .. imageWidth img - 1], y <- [0 .. imageHeight img - 1] ]
  choices = do
    c0 <- pixels
    c1 <- pixels
    guard $ pixel565 c0 >= pixel565 c1
    let c2 = mixWith (\_ p0 p1 -> floor $ flt p0 * (2/3) + flt p1 * (1/3)) c0 c1
        c3 = mixWith (\_ p0 p1 -> floor $ flt p0 * (1/3) + flt p1 * (2/3)) c0 c1
        flt i = fromIntegral i :: Float
        score = sum $ map (\p -> minimum $ map (colorDiff p) [c0,c1,c2,c3]) pixels
    return (score, (c0, c1, c2, c3))
  in snd $ minimumBy (comparing fst) choices

-- | Use this instead of findPalette for a sweet mosaic effect.
-- This was an accidental find before I wrote the current findPalette!
findPalette' :: Image PixelRGB8 -> (PixelRGB8, PixelRGB8, PixelRGB8, PixelRGB8)
findPalette' img = let
  (_, pal) = palettize (PaletteOptions MedianMeanCut True 2) img
  colors = [ pixelAt pal x y | x <- [0 .. imageWidth pal - 1], y <- [0 .. imageHeight pal - 1] ]
  (cA, cB) = (head colors, last colors)
  (c0, c1) = if pixel565 cA < pixel565 cB then (cB, cA) else (cA, cB)
  c2 = mixWith (\_ p0 p1 -> floor $ flt p0 * (2/3) + flt p1 * (1/3)) c0 c1
  c3 = mixWith (\_ p0 p1 -> floor $ flt p0 * (1/3) + flt p1 * (2/3)) c0 c1
  flt i = fromIntegral i :: Float
  in (c0, c1, c2, c3)

-- | Writes 256x256 image to DXT1
writeDDS :: FilePath -> Image PixelRGB8 -> IO ()
writeDDS fout img = let
  -- header stolen from an imagemagick-output file
  header = B.pack [0x44,0x44,0x53,0x20,0x7C,0x00,0x00,0x00,0x07,0x10,0x0A,0x00,0x00,0x01,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x02,0x00,0x00,0x00,0x00,0x00,0x00,0x09,0x00,0x00,0x00,0x49,0x4D,0x41,0x47,0x45,0x4D,0x41,0x47,0x49,0x43,0x4B,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x20,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x44,0x58,0x54,0x31,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x08,0x10,0x40,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]
  in withBinaryFile fout WriteMode $ \h -> do
    B.hPut h header
    forM_ [0,4..252] $ \y ->
      forM_ [0,4..252] $ \x -> do
        let chunk = generateImage (\cx cy -> pixelAt img (x + cx) (y + cy)) 4 4
            (c0, c1, c2, c3) = findPalette chunk
        B.hPut h $ B.pack
          [ fromIntegral $ pixel565 c0
          , fromIntegral $ pixel565 c0 `shiftR` 8
          , fromIntegral $ pixel565 c1
          , fromIntegral $ pixel565 c1 `shiftR` 8
          ]
        forM_ [y .. y + 3] $ \py -> do
          let byte = if pixel565 c0 == pixel565 c1
                then 0
                else sum $ flip map
                  [ (x    , 0x01)
                  , (x + 1, 0x04)
                  , (x + 2, 0x10)
                  , (x + 3, 0x40)
                  ] $ \(px, v) -> let
                  score c = colorDiff c $ pixelAt img px py
                  scoreTable =
                    [ (score c0, v * 0)
                    , (score c1, v * 1)
                    , (score c2, v * 2)
                    , (score c3, v * 3)
                    ]
                  in snd $ minimum scoreTable
          B.hPut h $ B.singleton byte
