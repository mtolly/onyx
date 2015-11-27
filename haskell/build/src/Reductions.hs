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
  hard      = getDiff Hard   `orIfNull` gryboHarden  isKeys mmap od expert
  medium    = getDiff Medium `orIfNull` gryboMediate isKeys mmap od hard
  easy      = getDiff Easy   `orIfNull` gryboEase    isKeys mmap od medium
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

gryboHarden, gryboMediate, gryboEase
  :: Bool                         -- ^ is this a basic keys track?
  -> U.MeasureMap
  -> RTB.T U.Beats Bool           -- ^ Overdrive phrases
  -> RTB.T U.Beats Five.DiffEvent -- ^ The source difficulty, one level up
  -> RTB.T U.Beats Five.DiffEvent -- ^ The target difficulty

gryboHarden isKeys mmap od diffEvents = let
  odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  gnotes1 = readGuitarNotes isKeys diffEvents
  isOD bts = fromMaybe False $ fmap snd $ Map.lookupLE bts odMap
  -- TODO: replace the next 2 with BEAT track
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  is8th bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure * 2 :: (Int, U.Beats)
    in frac == 0
  -- Step: simplify grace notes
  gnotes2 = RTB.fromAbsoluteEventList $ ATB.fromPairList $ disgrace $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes1
  disgrace = \case
    [] -> []
    (t1, GuitarNote _ Strum Nothing) : (t2, GuitarNote cols HOPO (Just len)) : rest
      | t2 - t1 <= 0.25 && is8th t1 && not (not (isOD t2) && isOD t1)
      -> (t1, GuitarNote cols Strum $ Just $ len + t2 - t1) : disgrace rest
    x : xs -> x : disgrace xs
  -- Step: simplify 3-note chords and GO chords
  gnotes3 = flip fmap gnotes2 $ \(GuitarNote cols ntype len) -> let
    cols' = case cols of
      [_, _, _] -> case (minimum cols, maximum cols) of
        (Five.Green, Five.Orange) -> [Five.Green, Five.Blue]
        (mincol, maxcol) -> [mincol, maxcol]
      [Five.Green, Five.Orange] -> [Five.Green, Five.Blue]
      [Five.Orange, Five.Green] -> [Five.Green, Five.Blue]
      _ -> cols
    in GuitarNote cols' ntype len
  -- Step: start marking notes as kept according to these (in order of importance):
  -- 1. Look at OD phrases first
  -- 2. Keep sustains first
  -- 3. Keep notes on a measure line first
  -- 4. Keep notes aligned to an 8th-note within a measure first
  -- 5. Finally, look at notes in chronological order
  sortedPositions = map snd $ sort $ do
    (bts, gnote) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
    let isSustain = case gnote of
          GuitarNote _ _ (Just _) -> True
          _                       -> False
        priority :: Int
        priority = sum
          [ if isOD      bts then 0 else 8
          , if isSustain     then 0 else 4
          , if isMeasure bts then 0 else 2
          , if is8th     bts then 0 else 1
          ]
    return (priority, bts)
  handlePosns kept []             = kept
  handlePosns kept (posn : posns) = let
    slice = fst $ Set.split (posn + 0.5) $ snd $ Set.split (posn -| 0.5) kept
    in if Set.null slice
      then handlePosns (Set.insert posn kept) posns
      else handlePosns kept posns
  keptPosns = handlePosns Set.empty sortedPositions
  gnotes4
    = RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ filter (\(t, _) -> Set.member t keptPosns)
    $ ATB.toPairList $ RTB.toAbsoluteEventList 0 gnotes3
  -- Step: only hopo chords and sustained notes
  gnotes5 = flip fmap gnotes4 $ \case
    GuitarNote [col] HOPO Nothing -> GuitarNote [col] Strum Nothing
    gnote                         -> gnote
  -- Step: chord->hopochord becomes note->hopochord
  gnotes6 = RTB.fromPairList $ fixDoubleHopoChord $ RTB.toPairList gnotes5
  fixDoubleHopoChord = \case
    [] -> []
    (t1, GuitarNote cols@(_ : _ : _) ntype len) : rest@((_, GuitarNote (_ : _ : _) HOPO _) : _)
      -> (t1, GuitarNote [minimum cols] ntype len) : fixDoubleHopoChord rest
    x : xs -> x : fixDoubleHopoChord xs
  -- Step: fix quick green-orange jumps
  gnotes7 = RTB.fromPairList $ fixJumps $ RTB.toPairList gnotes6
  fixJumps = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ len1) : rest@((t2, GuitarNote cols2 ntype2 len2) : rest')
      | (elem Five.Green cols1 && elem Five.Orange cols2) || (elem Five.Orange cols1 && elem Five.Green cols2)
      -> if t2 <= 0.5 || case len1 of
          Nothing -> False
          Just l1 -> t2 -| l1 <= 0.5
        then let
          cols2' = if elem Five.Green cols1
            then if sort cols2 == [Five.Blue, Five.Orange]
              then [Five.Yellow, Five.Blue]
              else map (\case Five.Orange -> Five.Blue; c -> c) cols2
            else if sort cols2 == [Five.Green, Five.Red]
              then [Five.Red, Five.Yellow]
              else map (\case Five.Green -> Five.Red; c -> c) cols2
          in tnote1 : fixJumps ((t2, GuitarNote cols2' ntype2 len2) : rest')
        else tnote1 : fixJumps rest
    x : xs -> x : fixJumps xs
  -- Step: fix HOPO notes with the same high fret as the note before them
  gnotes8 = RTB.fromPairList $ fixHOPOs $ RTB.toPairList gnotes7
  fixHOPOs = \case
    [] -> []
    tnote1@(_, GuitarNote cols1 _ _) : (t2, GuitarNote cols2 HOPO len2) : rest
      | maximum cols1 == maximum cols2
      -> tnote1 : fixHOPOs ((t2, GuitarNote cols2 Strum len2) : rest)
    x : xs -> x : fixHOPOs xs
  in showGuitarNotes isKeys gnotes8

gryboMediate isKeys mmap od = id -- TODO

gryboEase isKeys mmap od = id -- TODO

data NoteType = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GuitarNote t = GuitarNote [Five.Color] NoteType (Maybe t)
  deriving (Eq, Ord, Show, Read)

readGuitarNotes :: Bool -> RTB.T U.Beats Five.DiffEvent -> RTB.T U.Beats (GuitarNote U.Beats)
readGuitarNotes isKeys = go . RTB.collectCoincident . assign where
  assign = if isKeys
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
