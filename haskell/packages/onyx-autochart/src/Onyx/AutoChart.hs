module Onyx.AutoChart (autoChart) where

import           Control.Monad      (guard)
import           Data.Bifunctor     (first)
import           Data.Foldable      (toList)
import           Data.List          (sort)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe, listToMaybe)
import           Data.Ratio         (denominator)
import qualified Data.Set           as Set
import           Foreign
import           Foreign.C
import           System.IO.Unsafe   (unsafePerformIO)

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

-- Try to fix cases where different subsequent pitches turned into the same frets.
-- (The opposite case, same pitches switched to different frets, is usually fine)
distinguishFrets :: (Ord t) => Int -> [(t, Int)] -> [(t, Int)] -> [(t, Int)]
distinguishFrets frets orig auto = let

  -- group chords together, join input/output, and sort everything (with sets) so we can compare easily
  combined = Map.toList $ foldr (Map.unionWith (<>)) Map.empty
    $  [ Map.singleton k (Set.singleton v, Set.empty) | (k, v) <- orig ]
    <> [ Map.singleton k (Set.empty, Set.singleton v) | (k, v) <- auto ]

  -- group successive notes where both the input and output repeat
  toGroups (firstNote@(_t, (xs, ys)) : rest) = let
    (rest1, rest2) = span (\(_, (xs', ys')) -> xs == xs' && ys == ys') rest
    in (firstNote NE.:| rest1) : toGroups rest2
  toGroups [] = []

  average :: Set.Set Int -> Double
  average ints = fromIntegral (sum ints) / fromIntegral (Set.size ints)

  -- is a group of frets at the bottom or top of the set (can't be moved down or up respectively)?
  isBottom, isTop :: Set.Set Int -> Bool
  isBottom x = x == Set.fromList (take (Set.size x) [0 ..]                   )
  isTop    x = x == Set.fromList (take (Set.size x) [frets - 1, frets - 2 ..])

  fix prev (group1 : after1@(group2 : rest)) = let
    (_, (orig1, auto1)) = NE.head group1
    (_, (orig2, auto2)) = NE.head group2
    -- if we need to distinguish these, which direction should the new movement go
    movementUp = average orig1 <= average orig2 -- we'll move up in the rare case of no clear movement between chords
    -- if we need to change group1 or group2, which is it?
    editGroup2 :: Bool
    editGroup2 = if movementUp
      then not $ isTop    auto2 -- plan to move group2 up  , unless it can't, then we'll move group1 down
      else not $ isBottom auto2 -- plan to move group2 down, unless it can't, then we'll move group1 up
    in if auto1 == auto2 -- if these are equal, original pitches can't be (otherwise they would've been grouped)
      then if editGroup2
        then let
          -- edit group2, can't be the same as group1 or the following group (if there is one)
          neighbors = auto1 : case rest of
            group3 : _ -> [snd $ snd $ NE.head group3]
            []         -> []
          fixedAuto = fixOne neighbors movementUp auto2
          group2' = fmap (\(t, (thisOrig, _)) -> (t, (thisOrig, fixedAuto))) group2
          in group1 : fix (Just auto1) (group2' : rest)
        else let
          -- edit group1, can't be the same as prev (if any) or group2
          neighbors = auto2 : toList prev
          fixedAuto = fixOne neighbors (not movementUp) auto1
          group1' = fmap (\(t, (thisOrig, _)) -> (t, (thisOrig, fixedAuto))) group1
          in group1' : fix (Just fixedAuto) after1
      else group1 : fix (Just auto1) after1
  fix _ oneOrEmpty = oneOrEmpty

  allFretCombos :: [Set.Set Int]
  allFretCombos = map (Set.fromList . concat)
    $ sequence [ [[], [i]] | i <- [0 .. frets - 1] ]

  -- fix one group of frets by moving it (ideally in a target direction) to not be the same as its neighbors
  fixOne :: [Set.Set Int] -> Bool -> Set.Set Int -> Set.Set Int
  fixOne neighbors aimUp firstTry = let
    firstTryAverage = average firstTry
    allChoices = do
      fc <- allFretCombos
      guard $ Set.size fc == Set.size firstTry && notElem fc neighbors
      let distance = firstTryAverage - average fc
      -- first we want to move in the right direction, then we want to travel the shortest distance
      return (if aimUp then distance > 0 else distance < 0, abs distance, fc)
    in fromMaybe firstTry $ listToMaybe [ choice | (_, _, choice) <- sort allChoices ]

  toOutput fixedGroups = do
    group <- fixedGroups
    (time, (_, ys)) <- NE.toList group
    fret <- Set.toList ys
    return (time, fret)

  in toOutput $ fix Nothing $ toGroups combined

autoChart :: Int -> [(Rational, Int)] -> [(Rational, Int)]
autoChart frets notes = let
  resolution :: Rational
  resolution = fromIntegral $ foldr lcm 1 $ map (denominator . fst) notes
  cppInput = map (first $ \posn -> round $ posn * resolution) notes
  cppOutput = autoChartInt frets cppInput
  in map (first $ \ticks -> fromIntegral ticks / resolution)
    $ distinguishFrets frets cppInput cppOutput
