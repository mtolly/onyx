{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Guitar where

import           Control.Monad                    (guard)
import           Data.Bifunctor                   (first, second)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (sort)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Set                         as Set
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.FiveFret         as G5
import           Onyx.MIDI.Track.SixFret
import qualified Sound.MIDI.Util                  as U

data GuitarEvent a
  = Force StrumHOPOTap Bool
  | Note a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

computeFiveFretNotes :: FiveDifficulty U.Beats -> RTB.T U.Beats (Maybe G5.Color, Maybe U.Beats)
computeFiveFretNotes fd = fmap (\(isOpen, (col, len)) -> (guard (not isOpen) >> Just col, len))
  $ applyStatus1 False (fiveOpen fd)
  $ edgeBlips_ minSustainLengthRB
  $ fiveGems fd

data HOPOsAlgorithm
  = HOPOsRBGuitar
  | HOPOsRBKeys
  | HOPOsGH3
  deriving (Eq, Ord, Show, Enum, Bounded)

trackState :: (NNC.C t) => s -> (s -> t -> a -> (s, Maybe b)) -> RTB.T t a -> RTB.T t b
trackState curState step rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case step curState dt x of
    (nextState, Nothing) -> RTB.delay dt   $ trackState nextState step rtb'
    (nextState, Just y ) -> RTB.cons  dt y $ trackState nextState step rtb'

compareStatus :: Either a b -> Either a b -> Bool
compareStatus (Right _) (Left _) = False
compareStatus _         _        = True

applyStatus1 :: (NNC.C t) => s -> RTB.T t s -> RTB.T t a -> RTB.T t (s, a)
applyStatus1 start status events = let
  fn current _ = \case
    Left  s -> (s      , Nothing          )
    Right x -> (current, Just (current, x))
  in trackState start fn $ RTB.mergeBy compareStatus (fmap Left status) (fmap Right events)

applyStatus :: (NNC.C t, Ord s) => RTB.T t (s, Bool) -> RTB.T t a -> RTB.T t ([s], a)
applyStatus status events = let
  fn current _ = \case
    Left  (s, True ) -> (Set.insert s current, Nothing                     )
    Left  (s, False) -> (Set.delete s current, Nothing                     )
    Right x          -> (             current, Just (Set.toList current, x))
  in trackState Set.empty fn $ RTB.mergeBy compareStatus (fmap Left status) (fmap Right events)

applyBlipStatus :: (NNC.C t, Ord a, Ord s) => RTB.T t s -> RTB.T t a -> RTB.T t ([s], a)
applyBlipStatus status events
  = RTB.flatten
  $ fmap (\xs -> let thisStatus = lefts xs in map (thisStatus,) $ rights xs)
  $ RTB.collectCoincident
  $ RTB.merge (fmap Left status) (fmap Right events)

-- | Computes the default strum or HOPO value for each note.
strumHOPOTap :: (NNC.C t, Ord color) => HOPOsAlgorithm -> t -> RTB.T t (color, len) -> RTB.T t ((color, StrumHOPOTap), len)
strumHOPOTap algo threshold rtb = let
  instantList = RTB.toPairList $ RTB.collectCoincident rtb
  paired = zip instantList $ Nothing : map Just instantList
  f ((dt, gems), prev) = let
    gems' = [ ((color, sht), len) | (color, len) <- gems ]
    thisColors = map fst gems
    sht = case prev of
      Nothing -> Strum
      Just (_, prevNotes) -> let
        prevColors = map fst prevNotes
        in if dt > threshold
          -- the > above is tested on both RB3 and Moonscraper/CH:
          -- notes that are exactly the threshold apart will produce HOPOs
          then Strum
          else case thisColors of
            [] -> Strum -- shouldn't happen
            [c] -> case algo of
              -- TODO verify this behavior for all 3 algorithms
              HOPOsGH3 -> if thisColors == prevColors then Strum else HOPO
              _        -> if c `elem` prevColors then Strum else HOPO
            _ -> case algo of
              HOPOsRBKeys -> if sort thisColors /= sort prevColors
                then HOPO
                else Strum
              _ -> Strum
    in (dt, gems')
  in RTB.flatten $ RTB.fromPairList $ map f paired

getForces5 :: (NNC.C t) => FiveDifficulty t -> RTB.T t (StrumHOPOTap, Bool)
getForces5 fd = RTB.merge
  ((Strum ,) <$> fiveForceStrum fd)
  $ RTB.merge
    ((HOPO ,) <$> fiveForceHOPO fd)
    ((Tap ,) <$> fiveTap fd)

getForces6 :: (NNC.C t) => SixDifficulty t -> RTB.T t (StrumHOPOTap, Bool)
getForces6 fd = RTB.merge
  ((Strum ,) <$> sixForceStrum fd)
  $ RTB.merge
    ((HOPO ,) <$> sixForceHOPO fd)
    ((Tap ,) <$> sixTap fd)

-- | Modifies the default strum or HOPO value depending on force events.
applyForces :: (NNC.C t, Ord color, Ord len) => RTB.T t (StrumHOPOTap, Bool) -> RTB.T t ((color, StrumHOPOTap), len) -> RTB.T t ((color, StrumHOPOTap), len)
applyForces forceTrack notes = let
  f (forces, ((color, sht), len)) = let
    sht'  | elem Tap   forces = Tap
          | elem HOPO  forces = HOPO
          | elem Strum forces = Strum
          | otherwise         = sht
    in ((color, sht'), len)
  in fmap f $ applyStatus forceTrack notes

no5NoteChords :: (NNC.C t, Num t) => RTB.T t ((G5.Color, sht), Maybe t) -> RTB.T t ((G5.Color, sht), Maybe t)
no5NoteChords = let
  f trips = let
    colors = [ c | ((c, _), _) <- trips ]
    in if length colors == 5
      then filter (\((c, _), _) -> c /= G5.Yellow) trips -- turn GRYBO into GRBO
      else trips
  -- note: this doesn't check for ext sustains that create 5 note chords,
  -- because Magma allows that on keys.
  in RTB.flatten . fmap f . RTB.collectCoincident

noExtendedSustains' :: (NNC.C t, Num t) => t -> t -> RTB.T t (a, Maybe t) -> RTB.T t (a, Maybe t)
noExtendedSustains' blipThreshold sustainGap = let
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, gems), rtb') -> let
      len1 = maximum [ len | (_, len) <- gems ]
      len2 = case RTB.viewL rtb' of
        Nothing            -> len1
        Just ((dt', _), _) -> min (dt' NNC.-| sustainGap) <$> len1
      len3 = len2 >>= \l2 -> guard (l2 >= blipThreshold) >> len2
      in RTB.cons dt [ (x, len3) | (x, _) <- gems ] $ go rtb'
  in RTB.flatten . go . RTB.collectCoincident

noExtendedSustains :: (NNC.C t, Num t, Ord s, Ord a) => t -> t -> RTB.T t (LongNote s a) -> RTB.T t (LongNote s a)
noExtendedSustains blipThreshold sustainGap = let
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, gems), rtb') -> let
      len1 = maximum [ len | (_, _, len) <- gems ]
      len2 = case RTB.viewL rtb' of
        Nothing            -> len1
        Just ((dt', _), _) -> min (dt' NNC.-| sustainGap) <$> len1
      len3 = len2 >>= \l2 -> guard (l2 >= blipThreshold) >> len2
      in RTB.cons dt [ (s, x, len3) | (s, x, _) <- gems ] $ go rtb'
  in splitEdges . RTB.flatten . go . RTB.collectCoincident . joinEdges

-- Used for GH:WoR where extended sustains only work when the held note is lower.
-- So we trim sustains when a lower note than the sustain appears
noLowerExtSustains :: (NNC.C t, Ord color) => t -> t -> RTB.T t ((color, sht), Maybe t) -> RTB.T t ((color, sht), Maybe t)
noLowerExtSustains blipThreshold sustainGap = go where
  go = \case
    Wait t x rest -> Wait t (pullBack rest x) $ go rest
    RNil          -> RNil
  pullBack _    note@((_    , _  ), Nothing ) = note
  pullBack rest note@((color, sht), Just len) = let
    possibleConflicts = U.trackTake len $ U.trackDropZero rest
    conflict ((color', _), _) = color' < color
    in case RTB.filter conflict possibleConflicts of
      Wait t _ _ -> let
        len1 = t NNC.-| sustainGap
        len2 = guard (len1 >= blipThreshold) >> Just len1
        in ((color, sht), len2)
      RNil -> note

-- Used for .chart import where you can "hold" open then play other notes on top.
-- (PS) MIDI doesn't support this, so we have to trim the opens back before emitting
noOpenExtSustains :: (NNC.C t, Num t) => t -> t -> RTB.T t ((Maybe color, sht), Maybe t) -> RTB.T t ((Maybe color, sht), Maybe t)
noOpenExtSustains blipThreshold sustainGap = let
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, [((Nothing, sht), len1)]), rtb') -> let
      len2 = case RTB.viewL rtb' of
        Nothing            -> len1
        Just ((dt', _), _) -> min (dt' NNC.-| sustainGap) <$> len1
      len3 = len2 >>= \l2 -> guard (l2 >= blipThreshold) >> len2
      in RTB.cons dt [((Nothing, sht), len3)] $ go rtb'
    Just ((dt, xs), rtb') -> RTB.cons dt xs $ go rtb'
  in RTB.flatten . go . RTB.collectCoincident

standardBlipThreshold :: U.Beats
standardBlipThreshold = 3/8 -- is this correct? I think 1/3 sustains might be possible in games

standardSustainGap :: U.Beats
standardSustainGap = 1/8

guitarify' :: (Ord a) => RTB.T U.Beats (a, Maybe U.Beats) -> RTB.T U.Beats ([a], Maybe U.Beats)
guitarify'
  = fmap chordify
  . RTB.collectCoincident
  . noExtendedSustains' standardBlipThreshold standardSustainGap
  where chordify xs = (map fst xs, snd $ head xs)

guitarify :: (Ord s, Ord a) => RTB.T U.Beats (LongNote s a) -> RTB.T U.Beats (LongNote s [a])
guitarify
  = fmap chordify
  . RTB.collectCoincident
  . noExtendedSustains standardBlipThreshold standardSustainGap
  where chordify xs = fmap (const $ concatMap toList xs) $ head xs

fromClosed' :: RTB.T t ((color, sht), len) -> RTB.T t ((Maybe color, sht), len)
fromClosed' = fmap $ first $ first Just

-- Note, should probably remove extended sustains before running this?
noOpenNotes
  :: (NNC.C t)
  => Bool -- Should opens between chords be treated as muted strums and converted to HMX style (low fret of chord)?
  -> RTB.T t ((Maybe G5.Color, StrumHOPOTap), len)
  -> RTB.T t ((G5.Color, StrumHOPOTap), len)
noOpenNotes detectMuted notes = let
  -- if there are no open notes, no need to do any steps
  checkNotOpen ((fret, sht), len) = do
    notOpen <- fret
    Just ((notOpen, sht), len)
  in case mapM checkNotOpen notes of
    Just allNotOpen -> allNotOpen
    Nothing         -> RTB.flatten
      $ noOpenNotesNewAlgorithm
      $ (if detectMuted then mutedOpensToRBStyle else id)
      $ RTB.collectCoincident notes

-- Identifies "(chord or song start) (strummed open notes) (strummed chord)"
-- and turns the open notes into the low note of the following chord,
-- or the preceding chord if it's closer to the open notes.
mutedOpensToRBStyle
  :: (NNC.C t)
  => RTB.T t [((Maybe G5.Color, StrumHOPOTap), len)] -- should be from collectCoincident
  -> RTB.T t [((Maybe G5.Color, StrumHOPOTap), len)]
mutedOpensToRBStyle = go True where
  go _       RNil                  = RNil
  go isStart view@(Wait dt x rest) = if isChord x
    then findOpens (Just (dt, x)) rest
    else if isStart
      then findOpens Nothing view
      else Wait dt x $ go False rest
  findOpens previousChord rest = case RTB.span isOpenStrum rest of
    (opens@(Wait prevChordTime _ _), afterOpens) -> case afterOpens of
      Wait nextChordTime nextChord _ | isChordStrum nextChord -> let
        -- we could take sustain lengths into account but probably doesn't matter
        nextChordLow = minimum $ map (\((fret, _), _) -> fret) nextChord
        prevChordLow = case previousChord of
          Nothing         -> Just G5.Green -- doesn't matter
          Just (_, chord) -> minimum $ map (\((fret, _), _) -> fret) chord
        chordLow = case previousChord of
          Nothing -> nextChordLow
          Just _  -> if prevChordTime < nextChordTime
            then prevChordLow
            else nextChordLow
        in addPrevious $ RTB.append (moveToFret chordLow <$> opens)
          $ go False afterOpens
      _ -> addPrevious $ go False rest
    _ -> addPrevious $ go False rest
    where addPrevious = case previousChord of
            Just (dt, x) -> Wait dt x
            Nothing      -> id
  isChordStrum = \case
    ((_, Strum), _) : _ : _ -> True
    _                       -> False
  isChord = \case
    ((_, _), _) : _ : _ -> True
    _                   -> False
  isOpenStrum = \case
    [((Nothing, Strum), _)] -> True
    _                       -> False
  moveToFret fret = map $ \((_, sht), len) -> ((fret, sht), len)

data FretGroup
  = FretGroupLow -- notes that will be moved up
  | FretGroupHigh -- notes that will stay in place
  deriving (Eq)

noOpenNotesNewAlgorithm
  :: (NNC.C t)
  => RTB.T t [((Maybe G5.Color, StrumHOPOTap), len)] -- should be from collectCoincident
  -> RTB.T t [((G5.Color, StrumHOPOTap), len)]
noOpenNotesNewAlgorithm input = let

  inputList = RTB.toPairList input
  times     = map fst inputList
  events    = map snd inputList

  -- First fix the full 6-note sequences which will be a problem for wrapping.
  -- We change "open G R Y B O" to "open G Y R B O" which will become "G R Y R B O" later.
  isFret f [((fret, _), _)] = fret == f
  isFret _ _                = False
  fixWrapping :: [[((Maybe G5.Color, StrumHOPOTap), len)]] -> [[((Maybe G5.Color, StrumHOPOTap), len)]]
  fixWrapping [] = []
  fixWrapping fix1@(head1 : tail1) = let
    noWrap = head1 : fixWrapping tail1
    in case span (isFret Nothing) fix1 of
      (opens@(_ : _), fix2) -> case span (isFret $ Just G5.Green) fix2 of
        (greens@(_ : _), fix3) -> case span (isFret $ Just G5.Red) fix3 of
          (reds@(_ : _), fix4) -> case span (isFret $ Just G5.Yellow) fix4 of
            (yellows@(_ : _), fix5) -> case span (isFret $ Just G5.Blue) fix5 of
              (blues@(_ : _), fix6) -> if any (isFret $ Just G5.Orange) $ take 1 fix6
                then concat
                  [ opens
                  , greens
                  , map (map $ \((_, sht), len) -> ((Just G5.Yellow, sht), len)) reds
                  , map (map $ \((_, sht), len) -> ((Just G5.Red, sht), len)) yellows
                  , blues
                  , fixWrapping fix6
                  ]
                else noWrap
              _ -> noWrap
            _ -> noWrap
          _ -> noWrap
        _ -> noWrap
      _ -> noWrap

  -- initial markings: mark open as Low, single orange as High, chords as High
  initState = map $ \xs -> case xs of
    [((Nothing       , _), _)] -> (xs, Just FretGroupLow )
    [((Just G5.Orange, _), _)] -> (xs, Just FretGroupHigh)
    (_ : _ : _               ) -> (xs, Just FretGroupHigh)
    _                          -> (xs, Nothing           )

  fretMovement :: Maybe G5.Color -> Maybe G5.Color -> Int
  fretMovement fret1 fret2 = maybe (-1) fromEnum fret2 - maybe (-1) fromEnum fret1

  -- pass 1 (apply both ways): High notes spread to adjacent notes on same fret or 1 down
  pass1 = \case
    pair1@([((fret1, _), _)], Just FretGroupHigh) : (gems2@[((fret2, _), _)], Nothing) : rest
      | elem (fretMovement fret1 fret2) [0, -1]
      -> pair1 : pass1 ((gems2, Just FretGroupHigh) : rest)
    x : xs -> x : pass1 xs
    [] -> []

  -- pass 2 (apply both ways): Low notes spread to adjacent notes on same fret or 1 up
  pass2 = \case
    pair1@([((fret1, _), _)], Just FretGroupLow) : (gems2@[((fret2, _), _)], Nothing) : rest
      | elem (fretMovement fret1 fret2) [0, 1]
      -> pair1 : pass2 ((gems2, Just FretGroupLow) : rest)
    x : xs -> x : pass2 xs
    [] -> []

  -- pass 3: remove Low marking from open notes
  pass3 = map $ \case
    (open@[((Nothing, _), _)], Just FretGroupLow) -> (open, Nothing)
    x                                             -> x

  -- pass 4: for any sequence with Low or song boundary on either side and no High in between, also mark Low
  marked m (_, mark) = mark == m
  -- first handle the case of unmarked notes at start
  pass4 [] = []
  pass4 in1 = case span (marked Nothing) in1 of
    (unmarked@(_ : _), in2) -> if all (marked $ Just FretGroupLow) $ take 1 in2 -- note, 'all' handles when in2 is empty
      then concat
        [ map (\(x, _) -> (x, Just FretGroupLow)) unmarked
        , pass4' in2
        ]
      else unmarked <> pass4' in2
    _ -> pass4' in1
  -- then just handle "low, unmarked, low/end"
  pass4' [] = []
  pass4' in1@(head1 : tail1) = case span (marked $ Just FretGroupLow) in1 of
    (lows@(_ : _), in2) -> case span (marked Nothing) in2 of
      (unmarked@(_ : _), in3) -> if all (marked $ Just FretGroupLow) $ take 1 in3 -- note, 'all' handles when in3 is empty
        then concat
          [ lows
          , map (\(x, _) -> (x, Just FretGroupLow)) unmarked
          , pass4' in3
          ]
        else head1 : pass4' tail1
      _ -> head1 : pass4' tail1
    _ -> head1 : pass4' tail1

  -- pass 5: remaining notes all High
  pass5 = map $ \(gems, mark) -> (gems, fromMaybe FretGroupHigh mark)

  applyGroups = map $ \(gems, mark) -> let
    delta = case mark of
      FretGroupLow  -> 1
      FretGroupHigh -> 0
    in do
      ((fret, sht), len) <- gems
      let fret' = case maybe (-1) fromEnum fret + delta of
            1 -> G5.Red
            2 -> G5.Yellow
            3 -> G5.Blue
            n -> if n < 1 then G5.Green else G5.Orange
      return ((fret', sht), len)

  events'
    = applyGroups
    $ pass5
    $ pass4
    $ pass3
    $ reverse $ pass2 $ reverse $ pass2
    $ reverse $ pass1 $ reverse $ pass1
    $ initState
    $ reverse $ fixWrapping $ reverse $ fixWrapping events

  in RTB.fromPairList $ zip times events'

-- Previously used steps to move notes around for non-open-note game targets.
-- This process ensures that any two adjacent notes
-- * will be the same frets if they were the same to begin with
-- * will be different frets if they were different to begin with
-- which means HOPOs/strum settings don't need to be edited.
noOpenNotesOldAlgorithm
  :: (NNC.C t)
  => RTB.T t [((Maybe G5.Color, StrumHOPOTap), len)] -- should be from collectCoincident
  -> RTB.T t [((G5.Color, StrumHOPOTap), len)]
noOpenNotesOldAlgorithm = let
  rev = RTB.fromPairList . reverse . RTB.toPairList

  fixForward RNil = RNil
  fixForward (Wait dt xs@[((Nothing, _), _)] rest) = case rest of
    Wait _ [((Just Green, _), _)] _ -> case tryPush Nothing rest of
      Nothing    -> Wait dt xs $ fixForward $ fallbackFlip rest
      Just rest' -> Wait dt xs $ fixForward rest'
    _ -> Wait dt xs $ fixForward rest
  fixForward (Wait dt xs rest) = Wait dt xs $ fixForward rest

  isPred Nothing (Just Green) = True
  isPred (Just x) (Just y)    = x /= maxBound && succ x == y
  isPred _ _                  = False

  -- Attempt to push a sequence of ascending notes up one fret.
  -- Fails if there's a full sequence of open-G-R-Y-B-O.
  tryPush prev rtb = case rtb of
    Wait dt open@[((Nothing, _), _)] rest | prev == Nothing -> Wait dt open <$> tryPush Nothing rest
    Wait dt [((this@(Just c), sht), len)] rest | prev == this || isPred prev this -> do
      guard $ c /= Orange
      Wait dt [((Just $ succ c, sht), len)] <$> tryPush this rest
    _ -> Just rtb -- a chord, or we moved down, or moved >1 fret up, or no more notes

  -- For that full ascending sequence, move the G to Y instead of R.
  -- So it becomes G-Y-R-Y-B-O.
  fallbackFlip (Wait dt [((Just Green, sht), len)] rest)
    = Wait dt [((Just Yellow, sht), len)] $ fallbackFlip rest
  fallbackFlip rest = rest

  openGreen = fmap $ map $ \((mc, sht), len) -> ((fromMaybe Green mc, sht), len)
  in openGreen . rev . fixForward . rev . fixForward

-- | For GH3, turns taps into HOPOs, and then turns repeated HOPOs into strums
-- since they don't work played as HOPOs.
gh3LegalHOPOs :: (NNC.C t, Ord color) => RTB.T t ((color, StrumHOPOTap), len) -> RTB.T t ((color, StrumHOPOTap), len)
gh3LegalHOPOs = RTB.flatten . go . RTB.collectCoincident . noTaps where
  getColors = sort . map (fst . fst)
  go = \case
    Wait t1 group1 (Wait t2 group2 rest) -> Wait t1 group1 $
      if getColors group1 == getColors group2
        then let
          group2Strum = [ ((color, Strum), len) | ((color, _), len) <- group2 ]
          in go $ Wait t2 group2Strum rest
        else go $ Wait t2 group2      rest
    lessThan2 -> lessThan2

-- | Turns all tap notes into HOPO notes.
noTaps :: RTB.T t ((color, StrumHOPOTap), len) -> RTB.T t ((color, StrumHOPOTap), len)
noTaps = fmap $ first $ second $ \case Tap -> HOPO; sh -> sh

cleanEdges :: (NNC.C t, Ord a) => RTB.T t (Bool, a) -> RTB.T t (Bool, a)
cleanEdges = go . RTB.normalize where
  go RNil = RNil
  go (Wait tx (False, x) (Wait ty (True, y) rest)) = if x == y
    then RTB.delay (tx <> ty) $ go rest
    else Wait (tx <> ty) (False, x) $ Wait NNC.zero (True, y) $ go rest
  go (Wait t pair rest) = Wait t pair $ go rest

{-

Clone Hero (v0.22.5) does not apply tap-off until the next tick.
So we move each tap-off earlier by one tick.
(Moonscraper doesn't have this issue, so CH must be using an old version?)

2020-10-26: CH v0.24.0.2068-master also now has this bug with SP phrases...

2022-01-27: CH v1.0.0.4080 appears to not have it with SP. But still on taps.
Also, Moonscraper v1.5.0 is the same.

-}
fixTapOffCH :: RTB.T U.Beats (Bool, StrumHOPOTap) -> RTB.T U.Beats (Bool, StrumHOPOTap)
fixTapOffCH = \case
  -- if this is the last force change, don't need to adjust
  -- (would cause problems if we did)
  Wait t x RNil -> Wait t x RNil
  -- pull back tap-off by one tick if it's not the last force change
  -- (note: make sure tap-off comes before strum-on/hopo-on in list;
  -- cleanEdges does this but otherwise we'd need RTB.normalize)
  Wait t pair@(False, Tap) rest -> let
    t' = t NNC.-| (1/480)
    in Wait t' pair $ fixTapOffCH $ RTB.delay (t - t') rest
  Wait t x rest -> Wait t x $ fixTapOffCH rest
  RNil -> RNil

-- | Writes every note with an explicit HOPO/strum force.
emit5' :: RTB.T U.Beats ((Maybe G5.Color, StrumHOPOTap), Maybe U.Beats) -> FiveDifficulty U.Beats
emit5' notes = FiveDifficulty
  { fiveForceStrum = makeForce Strum
  , fiveForceHOPO = makeForce HOPO
  , fiveTap = makeForce Tap
  , fiveOpen = U.trackJoin $ flip RTB.mapMaybe notes $ \case
    ((Nothing, _), _) -> Just boolBlip
    _                 -> Nothing
  , fiveGems
    = blipEdgesRB_
    $ fmap (\((mc, _), len) -> (fromMaybe G5.Green mc, len)) notes
  } where
    shts = fmap (snd . fst) $ RTB.flatten $ fmap (take 1) $ RTB.collectCoincident notes
    shtEdges = fixTapOffCH $ cleanEdges $ U.trackJoin $ fmap (\sht -> fmap (, sht) boolBlip) shts
    makeForce sht = fmap fst $ RTB.filter ((== sht) . snd) shtEdges
    boolBlip = RTB.fromPairList [(0, True), (1/480, False)]

-- | Writes every note with an explicit HOPO/strum force.
emit6' :: RTB.T U.Beats ((Maybe Fret, StrumHOPOTap), Maybe U.Beats) -> SixDifficulty U.Beats
emit6' notes = SixDifficulty
  { sixForceStrum = makeForce Strum
  , sixForceHOPO = makeForce HOPO
  , sixTap = makeForce Tap
  , sixGems = blipEdgesRB_ $ fmap (\((mc, _), len) -> (mc, len)) notes
  } where
    shts = fmap (snd . fst) $ RTB.flatten $ fmap (take 1) $ RTB.collectCoincident notes
    shtEdges = fixTapOffCH $ cleanEdges $ U.trackJoin $ fmap (\sht -> fmap (, sht) boolBlip) shts
    makeForce sht = fmap fst $ RTB.filter ((== sht) . snd) shtEdges
    boolBlip = RTB.fromPairList [(0, True), (1/480, False)]
