{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Reductions (gryboComplete) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified RockBand.FiveButton as Five
import RockBand.Common (Difficulty(..))
import qualified Sound.MIDI.Util as U
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (guard)
import Numeric.NonNegative.Class ((-|))
import Data.List (sort)

-- | Fills out a GRYBO chart by generating difficulties if needed.
gryboComplete :: Bool -> U.MeasureMap -> RTB.T U.Beats Five.Event -> RTB.T U.Beats Five.Event
gryboComplete isKeys mmap trk = let
  od        = flip RTB.mapMaybe trk $ \case
    Five.Overdrive b -> Just b
    _                -> Nothing
  getDiff d = flip RTB.mapMaybe trk $ \case
    Five.DiffEvent d' e | d == d' -> Just e
    _                             -> Nothing
  rtb1 `orIfNull` rtb2 = if RTB.null rtb1 then rtb2 else rtb1
  expert    = getDiff Expert
  hard      = getDiff Hard   `orIfNull` gryboReduce Hard   isKeys mmap od expert
  medium    = getDiff Medium `orIfNull` gryboReduce Medium isKeys mmap od hard
  easy      = getDiff Easy   `orIfNull` gryboReduce Easy   isKeys mmap od medium
  noDiffs = flip RTB.filter trk $ \case
    Five.DiffEvent _ _ -> False
    _                  -> True
  in foldr RTB.merge RTB.empty
    [ noDiffs
    , Five.DiffEvent Expert <$> expert
    , Five.DiffEvent Hard   <$> hard
    , Five.DiffEvent Medium <$> medium
    , Five.DiffEvent Easy   <$> easy
    ]

{-

for keys, start by just getting rid of overlapping sustains
(just like how the game does it for playing keys on guitar)

expert to hard:
* start by keeping any sustain notes
* if there's a note within 1/16 of a sustain start, and there's nothing
  for an 8th before that, remove that note and extend the sustain onto it
* then keep all the 8th-note aligned notes that aren't within an 8th of a sustain
* then keep any more notes which have an 8th gap on both sides
* take middle note out of 3 note chords, make GO into GB or RO
* make sure any hopos which can't be hopos anymore are made into strums

hard to medium:
* keep sustains
* pull sustains back so there's a quarter note gap, un-sustain if necessary
* make GB into GY or RB, and RO into RB or YO
* keep notes on 4tr note boundaries (as long as not within a 4tr of a sustain)
* keep any remaining notes with 4tr note gap on both sides
* remove all force hopo/sustain

medium to easy:
* again, make sure quarter note gap after sustain
* all chords to single notes
* keep notes on 1/2 note boundaries (not within 1/2 of a sustain)
* keep any remaining notes with 1/2 note gap on both sides

At all steps, must take care to always keep at least one note in each overdrive phrase.

-}

gryboReduce
  :: Difficulty
  -> Bool                         -- ^ is this a basic keys track?
  -> U.MeasureMap
  -> RTB.T U.Beats Bool           -- ^ Overdrive phrases
  -> RTB.T U.Beats Five.DiffEvent -- ^ The source difficulty, one level up
  -> RTB.T U.Beats Five.DiffEvent -- ^ The target difficulty

gryboReduce Expert _      _    _  diffEvents = diffEvents
gryboReduce diff   isKeys mmap od diffEvents = let
  odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  gnotes1 = readGuitarNotes isKeys diffEvents
  isOD bts = fromMaybe False $ fmap snd $ Map.lookupLE bts odMap
  -- TODO: replace the next 2 with BEAT track
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  -- Step: simplify grace notes
  gnotes2 = RTB.fromAbsoluteEventList $ ATB.fromPairList $ disgrace $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes1
  disgrace = \case
    [] -> []
    (t1, GuitarNote _ Strum Nothing) : (t2, GuitarNote cols HOPO (Just len)) : rest
      | t2 - t1 <= 0.25 && isAligned 0.5 t1 && not (not (isOD t1) && isOD t2)
      -> (t1, GuitarNote cols Strum $ Just $ len + t2 - t1) : disgrace rest
    x : xs -> x : disgrace xs
  -- Step: simplify 3-note chords and GO chords (or GB and RO chords on medium)
  gnotes3 = flip fmap gnotes2 $ \(GuitarNote cols ntype len) -> let
    cols' = case diff of
      Expert -> cols
      Hard   -> case cols of
        [_, _, _] -> case (minimum cols, maximum cols) of
          (Five.Green, Five.Orange) -> [Five.Green, Five.Blue]
          (mincol, maxcol) -> [mincol, maxcol]
        [Five.Green, Five.Orange] -> [Five.Green, Five.Blue]
        [Five.Orange, Five.Green] -> [Five.Green, Five.Blue]
        _ -> cols
      Medium -> case cols of
        [x] -> [x]
        _ -> case (minimum cols, maximum cols) of
          (Five.Green , c2) -> [Five.Green, min c2 Five.Yellow]
          (Five.Red   , c2) -> [Five.Red, min c2 Five.Blue]
          (c1         , c2) -> [c1, c2]
      Easy -> [minimum cols]
    in GuitarNote cols' ntype len
  -- Step: start marking notes as kept according to these (in order of importance):
  -- 1. Look at OD phrases first
  -- 2. Keep sustains first
  -- 3. Keep notes on a measure line first
  -- 4. Keep notes aligned to an 8th-note within a measure first
  -- 5. Finally, look at notes in chronological order
  -- TODO: maybe also prioritize strums over hopos, and chords over single notes?
  -- TODO: this order causes some oddness when OD phrases make adjacent notes disappear
  sortedPositions = map snd $ sort $ do
    (bts, gnote) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
    let isSustain = case gnote of
          GuitarNote _ _ (Just _) -> True
          _                       -> False
        priority :: Int
        priority = sum
          [ if isOD           bts then 0 else 8
          , if isSustain          then 0 else 4
          , if isMeasure      bts then 0 else 2
          , if isAligned divn bts then 0 else 1
          ]
        divn = case diff of
          Expert -> 0.25 -- doesn't matter
          Hard   -> 0.5
          Medium -> 1
          Easy   -> 2
    return (priority, bts)
  handlePosns kept []             = kept
  handlePosns kept (posn : posns) = let
    padding = case diff of
      Expert -> 0
      Hard   -> 0.5
      Medium -> 1
      Easy   -> 2
    slice = fst $ Set.split (posn + padding) $ snd $ Set.split (posn -| padding) kept
    in if Set.null slice
      then handlePosns (Set.insert posn kept) posns
      else handlePosns kept posns
  keptPosns = handlePosns Set.empty sortedPositions
  gnotes4
    = RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ filter (\(t, _) -> Set.member t keptPosns)
    $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
  -- Step: for hard, only hopo chords and sustained notes
  -- for easy/medium, all strum
  gnotes5 = case diff of
    Expert -> gnotes4
    Hard -> flip fmap gnotes4 $ \case
      GuitarNote [col] HOPO Nothing -> GuitarNote [col] Strum Nothing
      gnote                         -> gnote
    _ -> (\(GuitarNote cols _ len) -> GuitarNote cols Strum len) <$> gnotes4
  -- Step: chord->hopochord becomes note->hopochord
  gnotes6 = RTB.fromPairList $ fixDoubleHopoChord $ RTB.toPairList gnotes5
  fixDoubleHopoChord = \case
    [] -> []
    (t1, GuitarNote cols@(_ : _ : _) ntype len) : rest@((_, GuitarNote (_ : _ : _) HOPO _) : _)
      -> (t1, GuitarNote [minimum cols] ntype len) : fixDoubleHopoChord rest
    x : xs -> x : fixDoubleHopoChord xs
  -- Step: fix quick jumps (GO for hard, GB/RO for medium/easy)
  gnotes7 = if isKeys
    then gnotes6
    else RTB.fromPairList $ fixJumps $ RTB.toPairList gnotes6
  jumpDiff = case diff of
    Expert -> 4
    Hard   -> 3
    Medium -> 2
    Easy   -> 2
  jumpTime = case diff of
    Expert -> 0
    Hard   -> 0.5
    Medium -> 1
    Easy   -> 2
  fixJumps = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ len1) : (t2, GuitarNote cols2 ntype2 len2) : rest
      | fromEnum (maximum cols2) - fromEnum (minimum cols1) > jumpDiff
      && (t2 <= jumpTime || case len1 of Nothing -> False; Just l1 -> t2 -| l1 <= jumpTime)
      -> tnote1 : fixJumps ((t2, GuitarNote (map pred cols2) ntype2 len2) : rest)
      | fromEnum (maximum cols1) - fromEnum (minimum cols2) > jumpDiff
      && (t2 <= jumpTime || case len1 of Nothing -> False; Just l1 -> t2 -| l1 <= jumpTime)
      -> tnote1 : fixJumps ((t2, GuitarNote (map succ cols2) ntype2 len2) : rest)
    x : xs -> x : fixJumps xs
  -- Step: fix HOPO notes with the same high fret as the note before them
  gnotes8 = RTB.fromPairList $ fixHOPOs $ RTB.toPairList gnotes7
  fixHOPOs = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ _) : (t2, GuitarNote cols2 HOPO len2) : rest
      | maximum cols1 == maximum cols2
      -> tnote1 : fixHOPOs ((t2, GuitarNote cols2 Strum len2) : rest)
    x : xs -> x : fixHOPOs xs
  -- Step: bring back sustains for quarter note gap on medium/easy
  gnotes9 = case diff of
    Expert -> gnotes8
    Hard -> gnotes8
    _ -> RTB.fromPairList $ pullBackSustains $ RTB.toPairList gnotes8
  pullBackSustains = \case
    [] -> []
    (t1, GuitarNote cols1 ntype1 (Just l1)) : rest@((t2, _) : _) -> let
      l1' = min l1 $ t2 -| 1
      len1' = guard (l1' >= 1) >> Just l1'
      in (t1, GuitarNote cols1 ntype1 len1') : pullBackSustains rest
    x : xs -> x : pullBackSustains xs
  in showGuitarNotes (isKeys || elem diff [Easy, Medium]) gnotes9

data NoteType = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GuitarNote t = GuitarNote [Five.Color] NoteType (Maybe t)
  deriving (Eq, Ord, Show, Read)

readGuitarNotes :: Bool -> RTB.T U.Beats Five.DiffEvent -> RTB.T U.Beats (GuitarNote U.Beats)
readGuitarNotes isKeys = go . RTB.collectCoincident . assign where
  assign = if not isKeys
    then Five.assignHOPO $ 170 / 480
    else RTB.mapMaybe $ \case
      Five.Note True  color -> Just $ Five.Strum   color
      Five.Note False color -> Just $ Five.NoteOff color
      _                     -> Nothing
  go :: RTB.T U.Beats [Five.AssignedNote] -> RTB.T U.Beats (GuitarNote U.Beats)
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> let
      strums = flip mapMaybe xs $ \case Five.Strum   c -> Just c; _ -> Nothing
      hopos  = flip mapMaybe xs $ \case Five.HOPO    c -> Just c; _ -> Nothing
      len = case RTB.viewL rtb' of
        Nothing              -> error "GRYBO reduction: panic! note with no note-off"
        Just ((dt', xs'), _) -> if any (\case Five.NoteOff _ -> False; _ -> True) xs'
          then dt' -| 0.125
          else dt'
      len' = guard (len > 0.25) >> Just len
      in if null strums
        then if null hopos
          then RTB.delay dt $ go rtb'
          else RTB.cons dt (GuitarNote hopos HOPO len') $ go rtb'
        else RTB.cons dt (GuitarNote strums Strum len') $ go rtb'

showGuitarNotes :: Bool -> RTB.T U.Beats (GuitarNote U.Beats) -> RTB.T U.Beats Five.DiffEvent
showGuitarNotes isKeys = U.trackJoin . fmap f where
  f (GuitarNote cols ntype len) = RTB.flatten $ RTB.fromPairList
    [ (0, force True ++ map (Five.Note True) cols)
    , (fromMaybe (1 / 32) len, force False ++ map (Five.Note False) cols)
    ] where force b = case ntype of
              Strum -> [Five.ForceStrum b | not isKeys]
              HOPO  -> [Five.ForceHOPO  b | not isKeys]
