{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Reductions (gryboComplete, pkReduce, drumsComplete) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified RockBand.FiveButton as Five
import qualified RockBand.Drums as Drums
import qualified RockBand.ProKeys as PK
import RockBand.Common (Difficulty(..), Key(..))
import qualified Sound.MIDI.Util as U
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (guard)
import Numeric.NonNegative.Class ((-|))
import Data.List (sort, nub, sortOn)
import Data.Functor (($>))

-- | Fills out a GRYBO chart by generating difficulties if needed.
gryboComplete :: Bool -> U.MeasureMap -> RTB.T U.Beats Five.Event -> RTB.T U.Beats Five.Event
gryboComplete isKeys mmap trk = let
  od        = flip RTB.mapMaybe trk $ \case
    Five.Overdrive b -> Just b
    _                -> Nothing
  getDiff d = flip RTB.mapMaybe trk $ \case
    Five.DiffEvent d' e | d == d' -> Just e
    _                             -> Nothing
  rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
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

data PKNote t = PKNote [PK.Pitch] (Maybe t)
  deriving (Eq, Ord, Show, Read)

readPKNotes :: RTB.T U.Beats PK.Event -> RTB.T U.Beats (PKNote U.Beats)
readPKNotes = go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> let
      ons = flip mapMaybe xs $ \case PK.Note True p -> Just p; _ -> Nothing
      len = case RTB.viewL rtb' of
        Nothing              -> error "Pro keys reduction: panic! note with no note-off"
        Just ((dt', xs'), _) -> if any (\case PK.Note True _ -> True; _ -> False) xs'
          then dt' -| 0.125
          else dt'
      len' = guard (len > 0.25) >> Just len
      in if null ons
        then RTB.delay dt $ go rtb'
        else RTB.cons dt (PKNote ons len') $ go rtb'

showPKNotes :: RTB.T U.Beats (PKNote U.Beats) -> RTB.T U.Beats PK.Event
showPKNotes = U.trackJoin . fmap f where
  f (PKNote ps len) = RTB.flatten $ RTB.fromPairList
    [ (0, map (PK.Note True) ps)
    , (fromMaybe (1 / 32) len, map (PK.Note False) ps)
    ]

pkReduce
  :: Difficulty
  -> U.MeasureMap
  -> RTB.T U.Beats Bool     -- ^ Overdrive phrases
  -> RTB.T U.Beats PK.Event -- ^ The source difficulty, one level up
  -> RTB.T U.Beats PK.Event -- ^ The target difficulty

pkReduce Expert _    _  diffEvents = diffEvents
pkReduce diff   mmap od diffEvents = let
  odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  pknotes1 = readPKNotes diffEvents
  isOD bts = fromMaybe False $ fmap snd $ Map.lookupLE bts odMap
  -- TODO: replace the next 2 with BEAT track
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  -- Step: simplify grace notes
  pknotes2 = RTB.fromAbsoluteEventList $ ATB.fromPairList $ disgrace $ ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes1
  disgrace = \case
    [] -> []
    (t1, PKNote _ Nothing) : (t2, PKNote ps (Just len)) : rest
      | t2 - t1 <= 0.25 && isAligned 0.5 t1 && not (not (isOD t1) && isOD t2)
      -> (t1, PKNote ps $ Just $ len + t2 - t1) : disgrace rest
    x : xs -> x : disgrace xs
  -- Step: simplify chords
  pknotes3 = flip fmap pknotes2 $ \(PKNote ps len) -> let
    ps' = case diff of
      Expert -> ps
      Hard   -> case ps of
        [x] -> [x]
        [x, y] -> [x, y]
        _    -> let
          (p1, p3) = (minimum ps, maximum ps)
          p2 = minimum $ filter (`notElem` [p1, p3]) ps
          in [p1, p2, p3]
      Medium -> case ps of
        [x] -> [x]
        _   -> [minimum ps, maximum ps]
      Easy   -> [maximum ps]
    in PKNote ps' len
  -- Step: start marking notes as kept according to these (in order of importance):
  -- 1. Look at OD phrases first
  -- 2. Keep sustains first
  -- 3. Keep notes on a measure line first
  -- 4. Keep notes aligned to an 8th-note within a measure first
  -- 5. Finally, look at notes in chronological order
  -- TODO: maybe also prioritize chords over single notes?
  -- TODO: this order causes some oddness when OD phrases make adjacent notes disappear
  sortedPositions = map snd $ sort $ do
    (bts, pknote) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes3
    let isSustain = case pknote of
          PKNote _ (Just _) -> True
          _                 -> False
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
  pknotes4
    = RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ filter (\(t, _) -> Set.member t keptPosns)
    $ ATB.toPairList $ RTB.toAbsoluteEventList 0 pknotes3
  -- Step: limit range on medium/easy
  ranges = if elem diff [Easy, Medium]
    then RTB.singleton 0 PK.RangeF
    else flip RTB.mapMaybe diffEvents $ \case PK.LaneShift r -> Just r; _ -> Nothing
  -- Step: fix quick jumps (TODO) and out-of-range notes on easy/medium
  pknotes5 = if elem diff [Expert, Hard]
    then pknotes4
    else flip fmap pknotes4 $ \(PKNote ps len) -> let
      ps' = nub $ flip map ps $ \case
        PK.RedYellow k -> if k < F
          then PK.BlueGreen k
          else PK.RedYellow k
        PK.BlueGreen k -> if A < k
          then PK.RedYellow k
          else PK.BlueGreen k
        PK.OrangeC -> PK.BlueGreen C
      in PKNote ps' len
  -- Step: bring back sustains for quarter note gap on medium/easy
  pknotes6 = case diff of
    Expert -> pknotes5
    Hard -> pknotes5
    _ -> RTB.fromPairList $ pullBackSustains $ RTB.toPairList pknotes5
  pullBackSustains = \case
    [] -> []
    (t1, PKNote ps1 (Just l1)) : rest@((t2, _) : _) -> let
      l1' = min l1 $ t2 -| 1
      len1' = guard (l1' >= 1) >> Just l1'
      in (t1, PKNote ps1 len1') : pullBackSustains rest
    x : xs -> x : pullBackSustains xs
  in RTB.merge (fmap PK.LaneShift ranges) (showPKNotes pknotes6)

drumsComplete
  :: U.MeasureMap
  -> RTB.T U.Beats String -- ^ Practice sections
  -> RTB.T U.Beats Drums.Event
  -> RTB.T U.Beats Drums.Event
drumsComplete mmap sections trk = let
  od        = flip RTB.mapMaybe trk $ \case
    Drums.Overdrive b -> Just b
    _                -> Nothing
  assigned = Drums.assignToms trk
  getAssigned d = fmap snd $ RTB.filter (\(d', _) -> d == d') assigned
  getRaw d = flip RTB.mapMaybe trk $ \case
    Drums.DiffEvent d' e | d == d' -> Just e
    _                              -> Nothing
  reduceStep diff source = let
    authored = getRaw diff
    in if length authored < 5
      then let
        auto = drumsReduce diff mmap od sections source
        in (stripToms diff auto, auto)
      else (Drums.DiffEvent diff <$> authored, getAssigned diff)
  expert              = getAssigned Expert
  (hardRaw  , hard  ) = reduceStep Hard   expert
  (mediumRaw, medium) = reduceStep Medium hard
  (easyRaw  , _     ) = reduceStep Easy   medium
  untouched = flip RTB.filter trk $ \case
    Drums.DiffEvent Expert _ -> True
    Drums.DiffEvent _      _ -> False
    _                        -> True
  stripToms d = fmap $ \progem -> Drums.DiffEvent d $ Drums.Note $ progem $> ()
  in foldr RTB.merge RTB.empty [untouched, hardRaw, mediumRaw, easyRaw]

drumsReduce
  :: Difficulty
  -> U.MeasureMap
  -> RTB.T U.Beats Bool                      -- ^ Overdrive phrases
  -> RTB.T U.Beats String                    -- ^ Practice sections
  -> RTB.T U.Beats (Drums.Gem Drums.ProType) -- ^ The source difficulty, one level up
  -> RTB.T U.Beats (Drums.Gem Drums.ProType) -- ^ The target difficulty
drumsReduce Expert _    _  _        trk = trk
drumsReduce diff   mmap od sections trk = let
  -- odMap = Map.fromList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.normalize od
  -- isOD bts = fromMaybe False $ fmap snd $ Map.lookupLE bts odMap
  isMeasure bts = snd (U.applyMeasureMap mmap bts) == 0
  isAligned divn bts = let
    beatsWithinMeasure = snd $ U.applyMeasureMap mmap bts
    (_, frac) = properFraction $ beatsWithinMeasure / divn :: (Int, U.Beats)
    in frac == 0
  priority :: U.Beats -> Int
  priority bts = sum
    [ if isMeasure     bts then 0 else 1
    , if isAligned 2   bts then 0 else 1
    , if isAligned 1   bts then 0 else 1
    , if isAligned 0.5 bts then 0 else 1
    ]
  snares = ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.filter (== Drums.Red) trk
  keepSnares kept [] = kept
  keepSnares kept (posn : rest) = let
    padding = if diff == Hard then 0.5 else 1
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    in if Map.null slice
      then keepSnares (Map.insert posn [Drums.Red] kept) rest
      else keepSnares kept rest
  keptSnares = keepSnares Map.empty $ sortOn (\bts -> (priority bts, bts)) snares
  kit = ATB.toPairList $ RTB.toAbsoluteEventList 0 $ RTB.collectCoincident $ RTB.filter (`notElem` [Drums.Red, Drums.Kick]) trk
  keepKit kept [] = kept
  keepKit kept ((posn, gems) : rest) = let
    gems' = case sort gems of
      [Drums.Pro _ Drums.Cymbal, green@(Drums.Pro Drums.Green Drums.Cymbal)] -> [green]
      [Drums.Pro Drums.Yellow Drums.Cymbal, blue@(Drums.Pro Drums.Blue Drums.Cymbal)] -> [blue]
      [tom1@(Drums.Pro _ Drums.Tom), Drums.Pro _ Drums.Tom] | diff <= Medium -> [tom1]
      _ -> gems
    padding = if diff == Hard then 0.5 else 1
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    in if case Map.toList slice of [(p, _)] | p == posn -> True; [] -> True; _ -> False
      then keepKit (Map.alter (Just . (gems' ++) . fromMaybe []) posn kept) rest
      else keepKit kept rest
  keptHands = keepKit keptSnares $ sortOn (\(bts, _) -> (priority bts, bts)) kit
  kicks = ATB.getTimes $ RTB.toAbsoluteEventList 0 $ RTB.filter (== Drums.Kick) trk
  keepKicks kept [] = kept
  keepKicks kept (posn : rest) = let
    padding = if diff == Hard then 1 else 2
    slice = fst $ Map.split (posn + padding) $ snd $ Map.split (posn -| padding) kept
    hasKick = any (Drums.Kick `elem`) $ Map.elems slice
    hasOneHandGem = case Map.lookup posn slice of
      Just [_] -> True
      _        -> False
    in if not hasKick && (diff == Hard || Map.null slice || hasOneHandGem)
      then keepKicks (Map.alter (Just . (Drums.Kick :) . fromMaybe []) posn kept) rest
      else keepKicks kept rest
  keptAll = keepKicks keptHands $ sortOn (\bts -> (priority bts, bts)) kicks
  nullNothing [] = Nothing
  nullNothing xs = Just xs
  makeEasy, makeSnareKick, makeNoKick
    :: U.Beats -> Maybe U.Beats
    -> Map.Map U.Beats [Drums.Gem Drums.ProType]
    -> Map.Map U.Beats [Drums.Gem Drums.ProType]
  makeEasy start maybeEnd progress = let
    (_, startNote, sliceStart) = Map.splitLookup start progress
    slice = concat $ maybe id (:) startNote $ Map.elems $ case maybeEnd of
      Nothing  -> sliceStart
      Just end -> fst $ Map.split end sliceStart
    sliceKicks = length $ filter (== Drums.Kick) slice
    sliceHihats = length $ filter (== Drums.Pro Drums.Yellow Drums.Cymbal) slice
    sliceOtherKit = length $ filter (`notElem` [Drums.Kick, Drums.Red, Drums.Pro Drums.Yellow Drums.Cymbal]) slice
    fn  | sliceKicks == 0                          = makeNoKick
        | sliceKicks > sliceHihats + sliceOtherKit = makeSnareKick
        | sliceHihats > sliceOtherKit              = makeSnareKick
        | otherwise                                = makeNoKick
    in fn start maybeEnd progress
  makeSnareKick start maybeEnd = Map.mapMaybeWithKey $ \bts gems ->
    if start <= bts && case maybeEnd of Just end -> bts < end; Nothing -> True
      then nullNothing $ if start == bts && elem (Drums.Pro Drums.Green Drums.Cymbal) gems
        then filter (/= Drums.Kick) gems
        else filter (`elem` [Drums.Kick, Drums.Red]) gems
      else Just gems
  makeNoKick start maybeEnd = Map.mapMaybeWithKey $ \bts gems ->
    if start <= bts && case maybeEnd of Just end -> bts < end; Nothing -> True
      then nullNothing $ filter (/= Drums.Kick) gems
      else Just gems
  sectionStarts = ATB.toPairList $ RTB.toAbsoluteEventList 0 sections
  sectionBounds = zip sectionStarts (map (Just . fst) (drop 1 sectionStarts) ++ [Nothing])
  sectioned = if diff /= Easy
    then keptAll
    else let
      f ((start, _), maybeEnd) = makeEasy start maybeEnd
      in foldr f keptAll sectionBounds
  -- TODO: use "od" to make sure every OD phrase at least one note, add one back in if necessary
  in RTB.flatten $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ Map.toAscList sectioned
