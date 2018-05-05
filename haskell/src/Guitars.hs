{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
module Guitars where

import           Control.Monad                    (guard)
import           Control.Monad.Trans.State
import           Data.Bifunctor                   (first, second)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (nub, sort)
import           Data.Maybe                       (fromMaybe, isNothing)
import qualified Data.Set                         as Set
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Five
import           RockBand.Codec.Six
import           RockBand.Common
import qualified RockBand.Legacy.Five             as G5
import qualified RockBand.Legacy.Six              as G6
import qualified Sound.MIDI.Util                  as U

data GuitarEvent a
  = Force StrumHOPOTap Bool
  | Note a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

splitGuitarEvents :: (NNC.C t, Ord s, Ord a) => RTB.T t (GuitarEvent (s, a, Maybe t)) -> RTB.T t (GuitarEvent (LongNote s a))
splitGuitarEvents rtb = let
  (notes, notNotes) = RTB.partitionMaybe (\case Note x -> Just x; _ -> Nothing) rtb
  notNotes' = flip RTB.mapMaybe notNotes $ \case
    Force sht b -> Just $ Force sht b
    Note{} -> Nothing -- never happens
  in RTB.merge (fmap Note $ splitEdges notes) notNotes'

openNotes' :: (NNC.C t) => FiveDifficulty t -> RTB.T t (Maybe G5.Color, Maybe t)
openNotes' fd = fmap (\(isOpen, (col, len)) -> (guard (not isOpen) >> Just col, len))
  $ applyStatus1 False (fiveOpen fd) (fiveGems fd)

openNotes :: (NNC.C t) => RTB.T t G5.DiffEvent -> RTB.T t (GuitarEvent (LongNote () (Maybe G5.Color)))
openNotes rtb = let
  (notes, notNotes) = RTB.partitionMaybe (\case G5.Note ln -> Just ln; _ -> Nothing) rtb
  eithers = RTB.merge (fmap Left notNotes) (fmap Right $ joinEdges notes)
  eachEvent = \case
    Left (G5.Force sht b) -> return [Force sht b]
    Left (G5.OpenNotes b) -> put b >> return []
    Left G5.OnyxClose{} -> return []
    Left G5.Note{} -> return [] -- never happens
    Right (s, a, t) -> get >>= \open -> return
      [Note (s, if open then Nothing else Just a, t)]
  in splitGuitarEvents $ RTB.flatten $ evalState (traverse eachEvent $ RTB.normalize eithers) False

closeNotes' :: (NNC.C t) => FiveDifficulty t -> RTB.T t (Maybe G5.Color, Maybe t)
closeNotes' fd = fmap (\(offset, (col, len)) -> let
  col' = case maybe (-1) fromEnum col + offset of
    0 -> Just G5.Green
    1 -> Just G5.Red
    2 -> Just G5.Yellow
    3 -> Just G5.Blue
    n -> if n < 0 then Nothing else Just G5.Orange
  in (col', len)
  ) $ applyStatus1 0 (fiveOnyxClose fd) (openNotes' fd)

closeNotes :: (NNC.C t) => RTB.T t G5.DiffEvent -> RTB.T t (GuitarEvent (LongNote () (Maybe G5.Color)))
closeNotes rtb = let
  (notes, notNotes) = RTB.partitionMaybe (\case G5.Note ln -> Just ln; _ -> Nothing) rtb
  eithers = RTB.merge (fmap Left notNotes) (fmap Right $ joinEdges notes)
  eachEvent = \case
    Left (G5.Force sht b) -> return [Force sht b]
    Left (G5.OpenNotes open) -> do
      (_, offset) <- get
      put (open, offset)
      return []
    Left (G5.OnyxClose offset) -> do
      (open, _) <- get
      put (open, offset)
      return []
    Left G5.Note{} -> return [] -- never happens
    Right (s, a, t) -> do
      (open, offset) <- get
      let modifyFret color = case offset + if open then -1 else fromEnum color of
            0 -> Just G5.Green
            1 -> Just G5.Red
            2 -> Just G5.Yellow
            3 -> Just G5.Blue
            n -> if n < 0 then Nothing else Just G5.Orange
      return [Note (s, modifyFret a, t)]
  in splitGuitarEvents $ RTB.flatten $ evalState (traverse eachEvent $ RTB.normalize eithers) (False, 0)

ghlNotes :: RTB.T t G6.DiffEvent -> RTB.T t (GuitarEvent (LongNote () (Maybe G6.Fret)))
ghlNotes = fmap $ \case
  G6.Force sht b -> Force sht b
  G6.Note ln     -> Note ln

data HOPOsAlgorithm
  = HOPOsRBGuitar
  | HOPOsRBKeys
  | HOPOsGH3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

trackState :: (NNC.C t) => s -> (s -> t -> a -> (s, Maybe b)) -> RTB.T t a -> RTB.T t b
trackState curState step rtb = case RTB.viewL rtb of
  Nothing -> RTB.empty
  Just ((dt, x), rtb') -> case step curState dt x of
    (nextState, Nothing) -> RTB.delay dt   $ trackState nextState step rtb'
    (nextState, Just y ) -> RTB.cons  dt y $ trackState nextState step rtb'

applyStatus1 :: (NNC.C t, Ord s, Ord a) => s -> RTB.T t s -> RTB.T t a -> RTB.T t (s, a)
applyStatus1 start status events = let
  fn current _ = \case
    Left  s -> (s      , Nothing          )
    Right x -> (current, Just (current, x))
  in trackState start fn $ RTB.merge (fmap Left status) (fmap Right events)

applyStatus :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, Bool) -> RTB.T t a -> RTB.T t ([s], a)
applyStatus status events = let
  fn current _ = \case
    Left  (s, True ) -> (Set.insert s current, Nothing                     )
    Left  (s, False) -> (Set.delete s current, Nothing                     )
    Right x          -> (             current, Just (Set.toList current, x))
  in trackState Set.empty fn $ RTB.merge (fmap Left status) (fmap Right events)

allStrums :: (NNC.C t) => RTB.T t (GuitarEvent (LongNote () a)) -> RTB.T t (LongNote StrumHOPOTap a)
allStrums = RTB.mapMaybe $ \case
  Note ln -> Just $ first (const Strum) ln
  _       -> Nothing

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
          | elem Strum forces = Strum
          | elem HOPO  forces = HOPO
          | otherwise         = sht
    in ((color, sht'), len)
  in fmap f $ applyStatus forceTrack notes

strumHOPOTap :: (NNC.C t, Ord a) => HOPOsAlgorithm -> t -> RTB.T t (GuitarEvent (LongNote () a)) -> RTB.T t (LongNote StrumHOPOTap a)
strumHOPOTap algo threshold rtb = let
  notes = RTB.mapMaybe (\case Note ln -> Just ln; _ -> Nothing) rtb
  mods = flip RTB.mapMaybe rtb $ \case
    Force sht b -> Just (sht, b)
    _           -> Nothing
  withMods = applyStatus mods $ RTB.collectCoincident notes
  fn prev dt (thisMods, longs) = let
    blips = [ x | Blip   () x <- longs ]
    ons   = [ x | NoteOn () x <- longs ]
    offs  = [ x | NoteOff   x <- longs ]
    newPrev = case blips ++ ons of
      []   -> fmap (\(ago, gems) -> (NNC.add ago dt, gems)) prev
      gems -> Just (NNC.zero, gems)
    autoSH = case prev of
      Nothing -> Strum
      Just (ago, prevColors) -> let
        thisColors = blips ++ ons
        distance = NNC.add ago dt
        in if distance > threshold
          -- the > above is tested on both RB3 and Moonscraper/CH:
          -- notes that are exactly the threshold apart will produce HOPOs
          then Strum
          else case thisColors of
            [] -> Strum -- doesn't matter
            [c] -> case algo of
              -- TODO verify this behavior for all 3 algorithms
              HOPOsGH3 -> if thisColors == prevColors then Strum else HOPO
              _        -> if c `elem` prevColors then Strum else HOPO
            _ -> case algo of
              HOPOsRBKeys -> if sort thisColors /= sort prevColors
                then HOPO
                else Strum
              _ -> Strum
    sht | elem Tap   thisMods = Tap
        | elem Strum thisMods = Strum
        | elem HOPO  thisMods = HOPO
        | otherwise              = autoSH
    newEvents = concat
      [ map (Blip   sht) blips
      , map (NoteOn sht) ons
      , map NoteOff              offs
      ]
    in (newPrev, Just newEvents)
  in RTB.flatten $ trackState Nothing fn withMods

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

noExtendedSustains' :: (NNC.C t, Num t, Ord a) => t -> t -> RTB.T t (a, Maybe t) -> RTB.T t (a, Maybe t)
noExtendedSustains' blipThreshold sustainGap = let
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, gems), rtb') -> let
      len1 = maximum [ len | (_, len) <- gems ]
      len2 = case RTB.viewL rtb' of
        Nothing            -> len1
        Just ((dt', _), _) -> min (dt' NNC.-| sustainGap) <$> len1
      len3 = len2 >>= \l2 -> guard (l2 > blipThreshold) >> len2
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
      len3 = len2 >>= \l2 -> guard (l2 > blipThreshold) >> len2
      in RTB.cons dt [ (s, x, len3) | (s, x, _) <- gems ] $ go rtb'
  in splitEdges . RTB.flatten . go . RTB.collectCoincident . joinEdges

standardBlipThreshold :: U.Beats
standardBlipThreshold = 3/8

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

fromClosed :: RTB.T t (LongNote s a) -> RTB.T t (LongNote s (Maybe a))
fromClosed = fmap $ fmap Just

noOpenNotes'
  :: (NNC.C t)
  => Bool -- ^ whether open HOPOs\/taps should be removed
  -> RTB.T t ((Maybe G5.Color, StrumHOPOTap), len)
  -> RTB.T t ((G5.Color, StrumHOPOTap), len)
noOpenNotes' removeOpenHOPO = RTB.mapMaybe $ \case
  ((Nothing, sht), _) | removeOpenHOPO && sht /= Strum -> Nothing
  ((Nothing, sht), len) -> Just ((G5.Green, sht), len)
  ((Just x, sht), len) -> Just ((x, sht), len)

noOpenNotes
  :: (NNC.C t)
  => Bool -- ^ whether open HOPOs\/taps should be removed
  -> RTB.T t (LongNote StrumHOPOTap (Maybe G5.Color))
  -> RTB.T t (LongNote StrumHOPOTap G5.Color)
noOpenNotes removeOpenHOPO = let
  f = \case
    (sht, Nothing, _) | removeOpenHOPO && sht /= Strum -> Nothing
    (ntype, Nothing, len) -> Just (ntype, G5.Green, len)
    (ntype, Just x, len) -> Just (ntype, x, len)
  in splitEdges . RTB.mapMaybe f . joinEdges

-- | Turns all tap notes into HOPO notes.
noTaps' :: RTB.T t ((color, StrumHOPOTap), len) -> RTB.T t ((color, StrumHOPOTap), len)
noTaps' = fmap $ first $ second $ \case Tap -> HOPO; sh -> sh

-- | Turns all tap notes into HOPO notes.
noTaps :: RTB.T t (LongNote StrumHOPOTap a) -> RTB.T t (LongNote StrumHOPOTap a)
noTaps = fmap $ first $ \case Tap -> HOPO; sh -> sh

-- | Writes every note with an explicit HOPO/strum force.
emit5' :: RTB.T U.Beats ((Maybe G5.Color, StrumHOPOTap), Maybe U.Beats) -> FiveDifficulty U.Beats
emit5' notes = FiveDifficulty
  { fiveForceStrum = makeForce Strum
  , fiveForceHOPO = makeForce HOPO
  , fiveTap = makeForce Tap
  , fiveOpen = U.trackJoin $ flip RTB.mapMaybe notes $ \case
    ((Nothing, _), _) -> Just boolBlip
    _                 -> Nothing
  , fiveOnyxClose = RTB.empty
  , fiveGems = fmap (\((mc, _), len) -> (fromMaybe G5.Green mc, len)) notes
  } where
    shts = fmap (snd . fst) $ RTB.flatten $ fmap (take 1) $ RTB.collectCoincident notes
    makeForce sht = U.trackJoin $ fmap (const boolBlip) $ RTB.filter (== sht) shts
    boolBlip = RTB.fromPairList [(0, True), (1/32, False)]

-- | Writes every note with an explicit HOPO/strum force.
emit5 :: RTB.T U.Beats (LongNote StrumHOPOTap (Maybe G5.Color)) -> RTB.T U.Beats G5.DiffEvent
emit5 = let
  eachEvent = \case
    NoteOff mc -> note NoteOff mc
    NoteOn sht mc -> foldr RTB.merge RTB.empty
      [ force sht
      , open $ isNothing mc
      , note (NoteOn ()) mc
      ]
    Blip sht mc -> foldr RTB.merge RTB.empty
      [ force sht
      , open $ isNothing mc
      , note (Blip ()) mc
      ]
  note f mc = RTB.singleton 0 $ G5.Note $ f $ fromMaybe G5.Green mc
  force sht = RTB.fromPairList [(0, G5.Force sht True), (1/32, G5.Force sht False)]
  open True = RTB.fromPairList [(0, G5.OpenNotes True), (1/32, G5.OpenNotes False)]
  open False = RTB.empty
  nubByTime = RTB.flatten . fmap nub . RTB.collectCoincident
  in nubByTime . U.trackJoin . fmap eachEvent

-- | Writes every note with an explicit HOPO/strum force.
emit6' :: RTB.T U.Beats ((Maybe G6.Fret, StrumHOPOTap), Maybe U.Beats) -> SixDifficulty U.Beats
emit6' notes = SixDifficulty
  { sixForceStrum = makeForce Strum
  , sixForceHOPO = makeForce HOPO
  , sixTap = makeForce Tap
  , sixGems = fmap (\((mc, _), len) -> (mc, len)) notes
  } where
    makeForce sht = U.trackJoin $ flip RTB.mapMaybe notes $ \case
      ((_, sht'), _) | sht == sht' -> Just boolBlip
      _                            -> Nothing
    boolBlip = RTB.fromPairList [(0, True), (1/32, False)]

emit6 :: RTB.T U.Beats (LongNote StrumHOPOTap (Maybe G6.Fret)) -> RTB.T U.Beats G6.DiffEvent
emit6 = let
  eachEvent = \case
    NoteOff mc -> note NoteOff mc
    NoteOn sht mc -> foldr RTB.merge RTB.empty
      [ force sht
      , note (NoteOn ()) mc
      ]
    Blip sht mc -> foldr RTB.merge RTB.empty
      [ force sht
      , note (Blip ()) mc
      ]
  note f mc = RTB.singleton 0 $ G6.Note $ f mc
  force sht = RTB.fromPairList [(0, G6.Force sht True), (1/32, G6.Force sht False)]
  nubByTime = RTB.flatten . fmap nub . RTB.collectCoincident
  in nubByTime . U.trackJoin . fmap eachEvent
