{-# LANGUAGE FlexibleContexts #-}
module Image (scaleBilinear, toDDS, toPNG_XBOX, readPNGXbox) where

import           Codec.Picture
import           Control.Monad              (forM_, guard, replicateM)
import           Control.Monad.Trans.Writer (execWriter, tell)
import           Data.Binary.Get
import           Data.Bits                  (shiftL, shiftR, testBit, (.&.))
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.List                  (minimumBy)
import           Data.Ord                   (comparing)
import           Data.Word                  (Word16, Word8)

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

colorDiff :: PixelRGB8 -> PixelRGB8 -> Int
colorDiff (PixelRGB8 r0 g0 b0) (PixelRGB8 r1 g1 b1) = sum
  [ fromIntegral $ max r0 r1 - min r0 r1
  , fromIntegral $ max g0 g1 - min g0 g1
  , fromIntegral $ max b0 b1 - min b0 b1
  ]

mixProportions :: (Pixel a, Integral (PixelBaseComponent a)) =>
  Float -> Float -> a -> a -> a
mixProportions f0 f1 = mixWith $ \_ p0 p1 ->
  floor $ fromIntegral p0 * f0 + fromIntegral p1 * f1

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
    let c2 = mixProportions (2/3) (1/3) c0 c1
        c3 = mixProportions (1/3) (2/3) c0 c1
        score = sum $ map (\p -> minimum $ map (colorDiff p) [c0,c1,c2,c3]) pixels
    return (score, (c0, c1, c2, c3))
  in snd $ minimumBy (comparing fst) choices

contentDXT1 :: Image PixelRGB8 -> BL.ByteString
contentDXT1 img_ = execWriter $ do
  forM_ [256, 128, 64, 32, 16, 8, 4] $ \size -> do
    let img = scaleBilinear size size img_
    forM_ [0, 4 .. size - 4] $ \y ->
      forM_ [0, 4 .. size - 4] $ \x -> do
        let chunk = generateImage (\cx cy -> pixelAt img (x + cx) (y + cy)) 4 4
            (c0, c1, c2, c3) = findPalette chunk
        tell $ BL.pack
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
          tell $ BL.singleton byte

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
  flipPairs _ = []
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
