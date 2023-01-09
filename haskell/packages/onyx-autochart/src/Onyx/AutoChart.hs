module Onyx.AutoChart (autoChart) where

import           Data.Bifunctor   (first)
import           Data.List        (sort)
import           Data.Ratio       (denominator)
import           Foreign
import           Foreign.C
import           System.IO.Unsafe (unsafePerformIO)

foreign import ccall "midich_compute"
  c_midiCH :: CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{-# NOINLINE autoChartInt #-}
autoChartInt :: Int -> [(Int, Int)] -> [(Int, Int)]
autoChartInt frets input = unsafePerformIO $ do
  let sorted = sort input -- sort by ticks, then pitch
  withArrayLen (map (fromIntegral . fst) sorted) $ \len p1 -> do
    withArray (map (fromIntegral . snd) sorted) $ \p2 -> do
      with (fromIntegral len) $ \plen -> do
        c_midiCH (fromIntegral frets) plen p1 p2
        newLen <- fromIntegral <$> peek plen
        newTicks <- map fromIntegral <$> peekArray newLen p1
        newFrets <- map fromIntegral <$> peekArray newLen p2
        return $ zip newTicks newFrets

autoChart :: Int -> [(Rational, Int)] -> [(Rational, Int)]
autoChart frets notes = let
  resolution :: Rational
  resolution = fromIntegral $ foldr lcm 1 $ map (denominator . fst) notes
  in map (first $ \ticks -> fromIntegral ticks / resolution)
    $ autoChartInt frets
    $ map (first $ \posn -> round $ posn * resolution) notes
