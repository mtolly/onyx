{-# LANGUAGE LambdaCase #-}
module Guitars where

import           Control.Monad                    (guard)
import           Control.Monad.Trans.State
import           Data.Bifunctor                   (first)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (nub, sort)
import           Data.Maybe                       (fromMaybe)
import           Data.Maybe                       (isNothing)
import qualified Data.Set                         as Set
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import qualified RockBand.FiveButton              as G5
import qualified RockBand.GHL                     as G6
import qualified Sound.MIDI.Util                  as U

data GuitarEvent s a
  = Force G5.StrumHOPO Bool
  | TapNotes Bool
  | Note (LongNote s a)
  deriving (Eq, Ord, Show, Read)

openNotes :: (NNC.C t) => RTB.T t G5.DiffEvent -> RTB.T t (GuitarEvent () (Maybe G5.Color))
openNotes rtb = let
  eachEvent = \case
    G5.Force sh b -> return [Force sh b]
    G5.TapNotes b -> return [TapNotes b]
    G5.OpenNotes b -> put b >> return []
    G5.OnyxClose{} -> return []
    G5.Note ln -> get >>= \open -> return
      [Note $ fmap (if open then const Nothing else Just) ln]
  in RTB.flatten $ evalState (traverse eachEvent $ RTB.normalize rtb) False

closeNotes :: (NNC.C t) => RTB.T t G5.DiffEvent -> RTB.T t (GuitarEvent () G5.Color)
closeNotes rtb = let
  eachEvent = \case
    G5.Force sh b -> return [Force sh b]
    G5.TapNotes b -> return [TapNotes b]
    G5.OpenNotes open -> do
      (_, offset) <- get
      put (open, offset)
      return []
    G5.OnyxClose offset -> do
      (open, _) <- get
      put (open, offset)
      return []
    G5.Note ln -> do
      (open, offset) <- get
      let modifyFret color = case offset + if open then -1 else fromEnum color of
            1 -> G5.Red
            2 -> G5.Yellow
            3 -> G5.Blue
            n -> if n <= 0 then G5.Green else G5.Orange
      return [Note $ fmap modifyFret ln]
  in RTB.flatten $ evalState (traverse eachEvent $ RTB.normalize rtb) (False, 0)

ghlNotes :: RTB.T t G6.DiffEvent -> RTB.T t (GuitarEvent () (Maybe G6.Fret))
ghlNotes = fmap $ \case
  G6.Force sh b -> Force sh b
  G6.TapNotes b -> TapNotes b
  G6.Note ln    -> Note ln

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

applyStatus :: (NNC.C t, Ord s, Ord a) => RTB.T t (s, Bool) -> RTB.T t a -> RTB.T t ([s], a)
applyStatus status events = let
  fn current _ = \case
    Left  (s, True ) -> (Set.insert s current, Nothing                     )
    Left  (s, False) -> (Set.delete s current, Nothing                     )
    Right x          -> (             current, Just (Set.toList current, x))
  in trackState Set.empty fn $ RTB.merge (fmap Left status) (fmap Right events)

allStrums :: (NNC.C t) => RTB.T t (GuitarEvent () a) -> RTB.T t (LongNote (G5.StrumHOPO, Bool) a)
allStrums = RTB.mapMaybe $ \case
  Note ln -> Just $ first (const (G5.Strum, False)) ln
  _       -> Nothing

strumHOPOTap :: (NNC.C t, Ord a) => HOPOsAlgorithm -> t -> RTB.T t (GuitarEvent () a) -> RTB.T t (LongNote (G5.StrumHOPO, Bool) a)
strumHOPOTap algo threshold rtb = let
  notes = RTB.mapMaybe (\case Note ln -> Just ln; _ -> Nothing) rtb
  mods = flip RTB.mapMaybe rtb $ \case
    Force sh b -> Just (Just sh, b)
    TapNotes b -> Just (Nothing, b)
    _          -> Nothing
  withMods = applyStatus mods $ RTB.collectCoincident notes
  fn prev dt (thisMods, longs) = let
    blips = [ x | Blip   () x <- longs ]
    ons   = [ x | NoteOn () x <- longs ]
    offs  = [ x | NoteOff   x <- longs ]
    newPrev = case blips ++ ons of
      []   -> fmap (\(ago, gems) -> (NNC.add ago dt, gems)) prev
      gems -> Just (NNC.zero, gems)
    isTap = elem Nothing thisMods
    autoSH = case prev of
      Nothing -> G5.Strum
      Just (ago, prevColors) -> let
        thisColors = blips ++ ons
        distance = NNC.add ago dt
        in if distance > threshold
          -- TODO: should this be > or >= ?
          -- Moonscraper says default .chart threshold is >1/3;
          -- that is, 1/3 gap and smaller are hopos, so it's > for now.
          -- But need to test what RB does
          then G5.Strum
          else case thisColors of
            [] -> G5.Strum -- doesn't matter
            [c] -> case algo of
              -- TODO verify this behavior for all 3 algorithms
              HOPOsGH3 -> if thisColors == prevColors then G5.Strum else G5.HOPO
              _ -> if c `elem` prevColors then G5.Strum else G5.HOPO
            _ -> case algo of
              HOPOsRBKeys -> if sort thisColors /= sort prevColors
                then G5.HOPO
                else G5.Strum
              _ -> G5.Strum
    sh  | elem (Just G5.Strum) thisMods = G5.Strum
        | elem (Just G5.HOPO ) thisMods = G5.HOPO
        | otherwise                  = autoSH
    newEvents = concat
      [ map (Blip   (sh, isTap)) blips
      , map (NoteOn (sh, isTap)) ons
      , map NoteOff              offs
      ]
    in (newPrev, Just newEvents)
  in RTB.flatten $ trackState Nothing fn withMods

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

guitarify :: (Ord s, Ord a) => RTB.T U.Beats (LongNote s a) -> RTB.T U.Beats (LongNote s [a])
guitarify
  = fmap chordify
  . RTB.collectCoincident
  . noExtendedSustains standardBlipThreshold standardSustainGap
  where chordify xs = fmap (const $ concatMap toList xs) $ head xs

fromClosed :: RTB.T t (LongNote s a) -> RTB.T t (LongNote s (Maybe a))
fromClosed = fmap $ fmap Just

noTaps :: RTB.T t (LongNote (G5.StrumHOPO, Bool) a) -> RTB.T t (LongNote (G5.StrumHOPO, Bool) a)
noTaps = fmap $ first $ \(sh, _) -> (sh, False)

-- | Writes every note with an explicit HOPO/strum force.
emit5 :: RTB.T U.Beats (LongNote (G5.StrumHOPO, Bool) (Maybe G5.Color)) -> RTB.T U.Beats G5.DiffEvent
emit5 = let
  eachEvent = \case
    NoteOff mc -> note NoteOff mc
    NoteOn (sh, isTap) mc -> foldr RTB.merge RTB.empty
      [ force sh
      , tap isTap
      , open $ isNothing mc
      , note (NoteOn ()) mc
      ]
    Blip (sh, isTap) mc -> foldr RTB.merge RTB.empty
      [ force sh
      , tap isTap
      , open $ isNothing mc
      , note (Blip ()) mc
      ]
  note f mc = RTB.singleton 0 $ G5.Note $ f $ fromMaybe G5.Green mc
  force sh  = RTB.fromPairList [(0, G5.Force sh True), (1/32, G5.Force sh False)]
  tap True  = RTB.fromPairList [(0, G5.TapNotes True), (1/32, G5.TapNotes False)]
  tap False = RTB.empty
  open True = RTB.fromPairList [(0, G5.OpenNotes True), (1/32, G5.OpenNotes False)]
  open False = RTB.empty
  nubByTime = RTB.flatten . fmap nub . RTB.collectCoincident
  in nubByTime . U.trackJoin . fmap eachEvent

emit6 :: RTB.T U.Beats (LongNote (G5.StrumHOPO, Bool) (Maybe G6.Fret)) -> RTB.T U.Beats G6.DiffEvent
emit6 = let
  eachEvent = \case
    NoteOff mc -> note NoteOff mc
    NoteOn (sh, isTap) mc -> foldr RTB.merge RTB.empty
      [ force sh
      , tap isTap
      , note (NoteOn ()) mc
      ]
    Blip (sh, isTap) mc -> foldr RTB.merge RTB.empty
      [ force sh
      , tap isTap
      , note (Blip ()) mc
      ]
  note f mc = RTB.singleton 0 $ G6.Note $ f mc
  force sh  = RTB.fromPairList [(0, G6.Force sh True), (1/32, G6.Force sh False)]
  tap True  = RTB.fromPairList [(0, G6.TapNotes True), (1/32, G6.TapNotes False)]
  tap False = RTB.empty
  nubByTime = RTB.flatten . fmap nub . RTB.collectCoincident
  in nubByTime . U.trackJoin . fmap eachEvent
