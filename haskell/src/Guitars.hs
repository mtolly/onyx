{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Guitars where

import           Control.Monad                    (guard)
import           Data.Bifunctor                   (first, second)
import           Data.Either                      (lefts, rights)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (sort)
import           Data.Maybe                       (fromMaybe)
import qualified Data.Set                         as Set
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Five              as G5
import           RockBand.Codec.Six
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data GuitarEvent a
  = Force StrumHOPOTap Bool
  | Note a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

openNotes' :: FiveDifficulty U.Beats -> RTB.T U.Beats (Maybe G5.Color, Maybe U.Beats)
openNotes' fd = fmap (\(isOpen, (col, len)) -> (guard (not isOpen) >> Just col, len))
  $ applyStatus1 False (fiveOpen fd)
  $ edgeBlipsRB_
  $ fiveGems fd

closeNotes' :: FiveDifficulty U.Beats -> RTB.T U.Beats (Maybe G5.Color, Maybe U.Beats)
closeNotes' fd = fmap (\(offset, (col, len)) -> let
  col' = case maybe (-1) fromEnum col + offset of
    0 -> Just G5.Green
    1 -> Just G5.Red
    2 -> Just G5.Yellow
    3 -> Just G5.Blue
    n -> if n < 0 then Nothing else Just G5.Orange
  in (col', len)
  ) $ applyStatus1 0 (fiveOnyxClose fd) (openNotes' fd)

{-

The closest 2 notes on the same fret can be is 15 ticks (a 128th note) because
this is the shortest possible MIDI note. Any closer and you get a double
note-on/note-off error.

The closest 2 notes on different frets can be is 11 ticks. Any closer and you
get `Overlapping or too-close gems`.

-}

fixSloppyNotes :: (NNC.C t) => t -> RTB.T t a -> RTB.T t a
fixSloppyNotes threshold = RTB.flatten . go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> case RTB.viewL rtb' of
      Nothing -> rtb
      Just ((dt', ys), rtb'') -> if dt' <= threshold
        then RTB.cons dt (xs ++ ys) $ go $ RTB.delay dt' rtb''
        else RTB.cons dt xs $ go rtb'

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
strumHOPOTap' :: (NNC.C t, Ord color) => HOPOsAlgorithm -> t -> RTB.T t (color, len) -> RTB.T t ((color, StrumHOPOTap), len)
strumHOPOTap' algo threshold rtb = let
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

no5NoteChords' :: (NNC.C t, Num t) => RTB.T t ((G5.Color, sht), Maybe t) -> RTB.T t ((G5.Color, sht), Maybe t)
no5NoteChords' = let
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
    RNil -> RNil
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

noOpenNotes'
  :: (NNC.C t)
  => RTB.T t ((Maybe G5.Color, StrumHOPOTap), len)
  -> RTB.T t ((G5.Color, StrumHOPOTap), len)
noOpenNotes' = let
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

  openGreen = fmap $ \((mc, sht), len) -> ((fromMaybe Green mc, sht), len)
  in openGreen . RTB.flatten . rev . fixForward . rev . fixForward . RTB.collectCoincident

-- | Turns all tap notes into HOPO notes.
noTaps' :: RTB.T t ((color, StrumHOPOTap), len) -> RTB.T t ((color, StrumHOPOTap), len)
noTaps' = fmap $ first $ second $ \case Tap -> HOPO; sh -> sh

cleanEdges :: (NNC.C t, Ord a) => RTB.T t (Bool, a) -> RTB.T t (Bool, a)
cleanEdges = go . RTB.normalize where
  go RNil = RNil
  go (Wait tx (False, x) (Wait ty (True, y) rest)) = if x == y
    then RTB.delay (tx <> ty) $ go rest
    else Wait (tx <> ty) (False, x) $ Wait NNC.zero (True, y) $ go rest
  go (Wait t pair rest) = Wait t pair $ go rest

-- | Clone Hero (v0.22.5) does not apply tap-off until the next tick.
-- So we move each tap-off earlier by one tick.
-- (Moonscraper doesn't have this issue, so CH must be using an old version?)
-- 2020-10-26: CH v0.24.0.2068-master also now has this bug with SP phrases...
fixTapOff :: RTB.T U.Beats Bool -> RTB.T U.Beats Bool
fixTapOff = \case
  Wait t False rest -> let
    t' = t NNC.-| (1/480)
    in Wait t' False $ fixTapOff $ RTB.delay (t - t') rest
  Wait t True rest -> Wait t True $ fixTapOff rest
  RNil -> RNil

-- | Writes every note with an explicit HOPO/strum force.
emit5' :: RTB.T U.Beats ((Maybe G5.Color, StrumHOPOTap), Maybe U.Beats) -> FiveDifficulty U.Beats
emit5' notes = FiveDifficulty
  { fiveForceStrum = makeForce Strum
  , fiveForceHOPO = makeForce HOPO
  , fiveTap = fixTapOff $ makeForce Tap
  , fiveOpen = U.trackJoin $ flip RTB.mapMaybe notes $ \case
    ((Nothing, _), _) -> Just boolBlip
    _                 -> Nothing
  , fiveOnyxClose = RTB.empty
  , fiveGems
    = blipEdgesRB_
    $ fmap (\((mc, _), len) -> (fromMaybe G5.Green mc, len)) notes
  } where
    shts = fmap (snd . fst) $ RTB.flatten $ fmap (take 1) $ RTB.collectCoincident notes
    shtEdges = cleanEdges $ U.trackJoin $ fmap (\sht -> fmap (, sht) boolBlip) shts
    makeForce sht = fmap fst $ RTB.filter ((== sht) . snd) shtEdges
    boolBlip = RTB.fromPairList [(0, True), (1/32, False)]

-- | Writes every note with an explicit HOPO/strum force.
emit6' :: RTB.T U.Beats ((Maybe Fret, StrumHOPOTap), Maybe U.Beats) -> SixDifficulty U.Beats
emit6' notes = SixDifficulty
  { sixForceStrum = makeForce Strum
  , sixForceHOPO = makeForce HOPO
  , sixTap = makeForce Tap
  , sixGems = blipEdgesRB_ $ fmap (\((mc, _), len) -> (mc, len)) notes
  } where
    shts = fmap (snd . fst) $ RTB.flatten $ fmap (take 1) $ RTB.collectCoincident notes
    shtEdges = cleanEdges $ U.trackJoin $ fmap (\sht -> fmap (, sht) boolBlip) shts
    makeForce sht = fmap fst $ RTB.filter ((== sht) . snd) shtEdges
    boolBlip = RTB.fromPairList [(0, True), (1/32, False)]
