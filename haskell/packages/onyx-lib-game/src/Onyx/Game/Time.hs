{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Onyx.Game.Time where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (guard)
import           Control.Monad.Trans.State        (evalState, get, put)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (find)
import qualified Data.Map.Strict.Internal         as Map
import           Data.Maybe                       (fromMaybe, isJust,
                                                   listToMaybe, mapMaybe)
import qualified Data.Set.Internal                as Set
import qualified Data.Text                        as T
import           GHC.Generics
import           GHC.TypeLits
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.MIDI.Common                 (StrumHOPOTap, pattern RNil,
                                                   pattern Wait)
import           Onyx.MIDI.Track.Beat
import           Onyx.MIDI.Track.Drums.True
import qualified Onyx.MIDI.Track.ProGuitar        as PG
import qualified Sound.MIDI.Util                  as U

data PNF sust now
  = Empty
  | P sust
  | N now
  -- just F is invalid
  | PN sust now
  | PF sust -- past sustain continues to future
  | NF now sust
  | PNF sust now sust
  deriving (Eq, Show)

getPast :: PNF sust now -> Maybe sust
getPast = \case
  P   p     -> Just p
  PN  p _   -> Just p
  PF  p     -> Just p
  PNF p _ _ -> Just p
  _         -> Nothing

getFuture :: PNF sust now -> Maybe sust
getFuture = \case
  NF  _ f   -> Just f
  PF  f     -> Just f
  PNF _ _ f -> Just f
  _         -> Nothing

getNow :: PNF sust now -> Maybe now
getNow = \case
  N n       -> Just n
  PN _ n    -> Just n
  NF n _    -> Just n
  PNF _ n _ -> Just n
  _         -> Nothing

-- | Events in the input list must not overlap.
-- The output will only have one PNF event per timestamp.
buildPNF :: (NNC.C t, Ord now, Ord sust) => RTB.T t (now, Maybe (sust, t)) -> RTB.T t (PNF sust now)
buildPNF
  = fmap (\evts -> let
    endSustain = listToMaybe [ sust | Right sust <- evts ]
    blip = listToMaybe [ now | Left (now, Nothing) <- evts ]
    startSustain = listToMaybe [ (now, sust) | Left (now, Just sust) <- evts ]
    in case (endSustain, blip, startSustain) of
      (Nothing  , Nothing , Nothing           ) -> Empty -- shouldn't happen
      (Nothing  , Just now, _                 ) -> N now
      (Nothing  , _       , Just (now, future)) -> NF now future
      (Just past, Nothing , Nothing           ) -> P past
      (Just past, Just now, _                 ) -> PN past now
      (Just past, _       , Just (now, future)) -> PNF past now future
      -- we don't need to make PF, those get inserted later
    )
  . RTB.collectCoincident
  . U.trackJoin
  . fmap (\(now, msust) -> case msust of
    Nothing          -> Wait NNC.zero (Left (now, Nothing)) RNil
    Just (sust, len) -> Wait NNC.zero (Left (now, Just sust)) $ Wait len (Right sust) RNil
    )

data Toggle
  = ToggleEmpty -- Empty
  | ToggleStart -- NF
  | ToggleEnd -- P
  | ToggleRestart -- PNF
  | ToggleOn -- PF
  deriving (Eq, Show)

makeToggle :: Map.Map t Bool -> Map.Map t Toggle
makeToggle m = let
  go b = do
    cur <- get
    put b
    return $ case (cur, b) of
      (False, False) -> ToggleEmpty
      (False, True)  -> ToggleStart
      (True, False)  -> ToggleEnd
      (True, True)   -> ToggleRestart
  in evalState (traverse go m) False

type IsOverdrive = Bool

data CommonState a = CommonState
  { commonState     :: a
  , commonOverdrive :: Toggle
  , commonBRE       :: Toggle
  , commonSolo      :: Toggle
  , commonBeats     :: Maybe (Maybe BeatEvent)
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (CommonState a)

data DrumState pad lane = DrumState
  { drumNotes      :: Set.Set pad
  , drumLanes      :: Map.Map lane Toggle
  , drumActivation :: Toggle
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (DrumState pad lane)

data GuitarState fret = GuitarState
  { guitarNotes   :: Map.Map fret (PNF IsOverdrive StrumHOPOTap)
  , guitarTremolo :: Map.Map fret Toggle
  , guitarTrill   :: Map.Map fret Toggle
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (GuitarState fret)

data PGSustain t = PGSustain
  { pgSlide     :: Maybe (PG.Slide, t, t) -- start and end times of the slide
  , pgSustainOD :: IsOverdrive
  -- TODO probably also add fret and notetype
  } deriving (Eq, Ord, Show)

data PGNote = PGNote
  { pgFret :: PG.GtrFret
  , pgType :: PG.NoteType
  , pgSHT  :: StrumHOPOTap
  } deriving (Eq, Ord, Show)

data PGState t = PGState
  { pgNotes    :: Map.Map PG.GtrString (PNF (PGSustain t) PGNote)
  , pgArea     :: Maybe PG.StrumArea
  , pgChords   :: PNF T.Text T.Text
  , pgArpeggio :: PNF (Map.Map PG.GtrString PG.GtrFret) ()
  -- TODO tremolo, trill
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (PGState t)

newtype ManiaState = ManiaState
  { maniaNotes :: Map.Map Int (PNF () ())
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState ManiaState

class TimeState a where
  before :: a -> a
  before _ = empty
  after :: a -> a
  after _ = empty
  empty :: a

instance TimeState (PNF sust now) where
  before = \case
    Empty                 -> Empty
    P past                -> PF past
    N _now                -> Empty
    PN past _now          -> PF past
    PF sust               -> PF sust
    NF _now _future       -> Empty
    PNF past _now _future -> PF past
  after = \case
    Empty                 -> Empty
    P _past               -> Empty
    N _now                -> Empty
    PN _past _now         -> Empty
    PF sust               -> PF sust
    NF _now future        -> PF future
    PNF _past _now future -> PF future
  empty = Empty

instance TimeState Toggle where
  before = \case
    ToggleStart -> ToggleEmpty
    ToggleEmpty -> ToggleEmpty
    _           -> ToggleOn
  after = \case
    ToggleEnd   -> ToggleEmpty
    ToggleEmpty -> ToggleEmpty
    _           -> ToggleOn
  empty = ToggleEmpty

instance TimeState (Maybe a) where
  empty = Nothing

instance TimeState Bool where
  empty = False

instance (TimeState a, TimeState b) => TimeState (a, b) where
  before (x, y) = (before x, before y)
  after (x, y) = (after x, after y)
  empty = (empty, empty)

instance (TimeState a, Eq a) => TimeState (Map.Map k a) where
  before = Map.filter (/= empty) . fmap before
  after = Map.filter (/= empty) . fmap after
  empty = Map.empty

instance TimeState (Set.Set k) where
  empty = Set.empty

zipStateMaps :: (TimeState a, TimeState b, Ord t) => Map.Map t a -> Map.Map t b -> Map.Map t (a, b)
zipStateMaps xs ys = let
  xs' = fmap (\x -> (Just x, Nothing)) xs
  ys' = fmap (\y -> (Nothing, Just y)) ys
  zs = Map.unionWith (\(a, b) (c, d) -> (a <|> c, b <|> d)) xs' ys'
  go (mx, my) = do
    (px, py) <- get
    let xy = (fromMaybe (after px) mx, fromMaybe (after py) my)
    put xy
    return xy
  initX = maybe empty (before . snd) $ Map.lookupMin xs
  initY = maybe empty (before . snd) $ Map.lookupMin ys
  in evalState (traverse go zs) (initX, initY)

infixl 1 `zipStateMaps`

zipStateLists :: (TimeState a, TimeState b, NNC.C t) => RTB.T t a -> RTB.T t b -> RTB.T t (a, b)
zipStateLists rtbX rtbY = let
  initX = case rtbX of
    RNil       -> empty
    Wait _ x _ -> before x
  initY = case rtbY of
    RNil       -> empty
    Wait _ y _ -> before y
  go px _ RNil ys = fmap (px,) ys
  go _ py xs RNil = fmap (,py) xs
  go px py (Wait tx x xs) (Wait ty y ys) = case NNC.split tx ty of
    (_, (b, d)) -> if d == NNC.zero
      then {- tx == ty -} Wait tx (x, y) $ go (after x) (after y) xs ys
      else if b
        then {- tx < ty -} Wait tx (x, py) $ go (after x) py xs (Wait d y ys)
        else {- tx > ty -} Wait ty (px, y) $ go px (after y) (Wait d x xs) ys
  in go initX initY rtbX rtbY

-- generic stuff

newtype GenericTimeState a = GenericTimeState a

instance (Generic a, TimeStateProduct (Rep a)) => TimeState (GenericTimeState a) where
  before (GenericTimeState x) = GenericTimeState $ to $ genericBefore $ from x
  after (GenericTimeState x) = GenericTimeState $ to $ genericAfter $ from x
  empty = GenericTimeState $ to genericEmpty

class TimeStateProduct f where
  genericBefore :: f k -> f k
  genericAfter :: f k -> f k
  genericEmpty :: f k

instance (TypeError ('Text "You can't use `GenericTimeState` for sum types"))
  => TimeStateProduct (a :+: b) where
  genericBefore = undefined
  genericAfter = undefined
  genericEmpty = undefined

instance TimeStateProduct U1 where
  genericBefore U1 = U1
  genericAfter U1 = U1
  genericEmpty = U1

instance TimeStateProduct c => TimeStateProduct (D1 md c) where
  genericBefore (M1 x) = M1 (genericBefore x)
  genericAfter (M1 x) = M1 (genericAfter x)
  genericEmpty = M1 genericEmpty

instance TimeStateProduct s => TimeStateProduct (C1 mc s) where
  genericBefore (M1 x) = M1 (genericBefore x)
  genericAfter (M1 x) = M1 (genericAfter x)
  genericEmpty = M1 genericEmpty

instance (TimeStateProduct a, TimeStateProduct b) => TimeStateProduct (a :*: b) where
  genericBefore (a :*: b) = genericBefore a :*: genericBefore b
  genericAfter (a :*: b) = genericAfter a :*: genericAfter b
  genericEmpty = genericEmpty :*: genericEmpty

instance TimeState t => TimeStateProduct (S1 m (Rec0 t)) where
  genericBefore (M1 (K1 x)) = M1 $ K1 $ before x
  genericAfter (M1 (K1 x)) = M1 $ K1 $ after x
  genericEmpty = M1 $ K1 empty

-- drum gameplay

data EventResult t pad = EventResult
  { eventHit    :: Maybe (pad, Maybe t) -- Just if a note was correctly hit
  , eventMissed :: [(t, pad)]
  } deriving (Show)

data GamePlayState = GamePlayState
  { gameScore :: Int
  , gameCombo :: Int
  } deriving (Show)

initialState :: GamePlayState
initialState = GamePlayState
  { gameScore = 0
  , gameCombo = 0
  }

data DrumPlayState t pad lane = DrumPlayState
  { drumEvents    :: [(t, (EventResult t pad, GamePlayState))]
  , drumTrack     :: Map.Map t (CommonState (DrumState pad lane))
  , drumNoteTimes :: Set.Set t
  } deriving (Show)

data NoteStatus t
  = NoteFuture
  | NoteHitAt t
  | NoteMissed
  deriving (Eq, Show)

processedNotes :: [(t, (EventResult t pad, GamePlayState))] -> [(t, pad, NoteStatus t)]
processedNotes = concatMap $ \(eventTime, (res, _)) -> let
  hit = case eventHit res of
    Just (pad, Just t) -> [(t, pad, NoteHitAt eventTime)]
    _                  -> []
  misses = [ (t, pad, NoteMissed) | (t, pad) <- eventMissed res ]
  in hit ++ misses

lookupNote :: (Ord t, Eq pad) => t -> pad -> [(t, pad, NoteStatus t)] -> NoteStatus t
lookupNote t pad
  = fromMaybe NoteFuture
  . listToMaybe
  . mapMaybe (\(pt, ppad, status) -> guard (pt == t && ppad == pad) >> Just status)
  . takeWhile (\(pt, _, _) -> pt >= t)

noteStatus :: (Ord t, Eq pad) => t -> pad -> [(t, (EventResult t pad, GamePlayState))] -> NoteStatus t
noteStatus t pad = lookupNote t pad . processedNotes

zoomMapList :: (Ord k) => k -> k -> Map.Map k a -> [(k, a)]
zoomMapList _ _ Map.Tip = []
zoomMapList kmin kmax (Map.Bin _ k v ml mr)
  =  (if k <= kmin then [] else zoomMapList kmin kmax ml)
  ++ [(k, v) | kmin <= k && k <= kmax]
  ++ (if kmax <= k then [] else zoomMapList kmin kmax mr)

data Bound k
  = Unbounded
  | Inclusive k
  | Exclusive k

zoomSetList :: (Ord k) => Bound k -> Bound k -> Set.Set k -> [k]
zoomSetList boundMin boundMax = let
  testMin x = case boundMin of
    Unbounded      -> True
    Inclusive kmin -> kmin <= x
    Exclusive kmin -> kmin <  x
  testMax x = case boundMax of
    Unbounded      -> True
    Inclusive kmax -> x <= kmax
    Exclusive kmax -> x <  kmax
  go Set.Tip = []
  go (Set.Bin _ k ml mr) = let
    passMin = testMin k
    passMax = testMax k
    in   (if passMin then go ml else [])
      <> [k | passMin && passMax]
      <> (if passMax then go mr else [])
  in go

applyDrumEvent :: (Show t, Ord t, Num t, Ord pad) => t -> Maybe pad -> t -> DrumPlayState t pad lane -> DrumPlayState t pad lane
applyDrumEvent tNew mpadNew halfWindow dps = let
  applyNoRewind (t, mpad) evts = let
    mClosestTime = case (Set.lookupLE t $ drumNoteTimes dps, Set.lookupGE t $ drumNoteTimes dps) of
      (x, Nothing)     -> x
      (Nothing, y)     -> y
      (Just x, Just y) -> Just $ if t - x < y - t then x else y
    mClosestTime' = mClosestTime >>= \ct -> do
      guard $ abs (ct - t) < halfWindow
      return ct
    eventHit = flip fmap mpad $ \pad -> let
      hitNote = mClosestTime' >>= \closestTime -> do
        guard $ case Map.lookup closestTime $ drumTrack dps of
          Nothing -> False
          Just cs -> Set.member pad $ drumNotes $ commonState cs
        guard $ noteStatus closestTime pad evts == NoteFuture
        Just closestTime
      in (pad, hitNote)
    eventMissed = let
      -- earliest missable time is the last processed note time
      -- latest missable time is t
      -- for each time, miss a note if we didn't process it already and:
      -- * it's before the note we are hitting now
      -- * or, it's not hittable because it's <= t - halfWindow
      firstPossibleMiss = case processedNotes evts of
        []                   -> 0
        (lastTime, _, _) : _ -> lastTime
      lastPossibleMiss = t
      in do
        (cst, cs) <- zoomMapList firstPossibleMiss lastPossibleMiss $ drumTrack dps
        notePad <- Set.toList $ drumNotes $ commonState cs
        let isFuture = noteStatus cst notePad evts == NoteFuture
            beforeCurrentHit = case eventHit of
              Just (_, Just hitTime) -> cst < hitTime
              _                      -> False
            beforeWindow = cst <= t - halfWindow
        guard $ isFuture && (beforeCurrentHit || beforeWindow)
        return (cst, notePad)
    newState = let
      prevState = case evts of
        []              -> initialState
        (_, (_, s)) : _ -> s
      comboPlus = case eventHit of
        Just (_, Just _) -> 1 -- hit a note
        _                -> 0
      comboBase = case eventHit of
        Just (_, Nothing) -> 0 -- overhit
        _                 -> case eventMissed of
          _ : _ -> 0 -- missed a note
          []    -> gameCombo prevState
      scorePlus = comboPlus * 25 * if
        | comboBase < 9  -> 1
        | comboBase < 19 -> 2
        | comboBase < 29 -> 3
        | otherwise      -> 4
      in GamePlayState
        { gameScore = gameScore prevState + scorePlus
        , gameCombo = comboBase + comboPlus
        }
    in case (eventHit, eventMissed) of
      (Nothing, []) -> evts -- time passed but nothing happened
      _             -> (t, (EventResult{..}, newState)) : evts
  in case span ((> tNew) . fst) $ drumEvents dps of
    (eventsToRewind, rest) -> let
      eventsToRewind' = flip map eventsToRewind $ \(t, (res, _)) ->
        (t, fmap fst $ eventHit res)
      newEvents = foldr applyNoRewind rest $ eventsToRewind' ++ [(tNew, mpadNew)]
      in dps { drumEvents = newEvents }

----------------------------

-- Inputs from the player (minus opening the hihat pedal)
data TrueDrumHit = TrueDrumHit
  { tdh_gem      :: TrueGem
  , tdh_rim      :: Bool
  , tdh_velocity :: Double -- 0 to 1
  } deriving (Show)

data TrueDrumInput
  = TDInputHit TrueDrumHit
  | TDInputHihatOpen
  deriving (Show)

data TrueDrumPlayState t = TrueDrumPlayState
  { tdEvents    :: [(t, (Maybe TrueDrumInput, TrueDrumGameState t))]
  , tdTrack     :: Map.Map t (CommonState (DrumState (TrueDrumNote FlamStatus) TrueGem))
  , tdNoteTimes :: Set.Set t
  } deriving (Show)

data TrueDrumGameState t = TrueDrumGameState
  { tdScore       :: Int
  , tdCombo       :: Int
  , tdIsHihatOpen :: Bool
  , tdNoteResults :: [(t, Map.Map (TrueDrumNote FlamStatus) (NoteResult t))] -- times are of notes
  , tdOverhits    :: [(t, TrueDrumHit)] -- times are of inputs
  } deriving (Show)

data NoteResult t
  = TDMissed
  | TDHit t
  | TDHitPendingHihat Bool -- once hihat open = bool, changes to hit
  | TDTwoHits t -- flam that has been hit twice
  | TDTwoHitsPendingHihat Bool
  deriving (Eq, Show)

initialTDState :: TrueDrumGameState t
initialTDState = TrueDrumGameState
  { tdScore       = 0
  , tdCombo       = 0
  , tdIsHihatOpen   = False
  , tdNoteResults = []
  , tdOverhits    = []
  }

trueNoteStatus :: (Ord t) => t -> TrueDrumNote FlamStatus -> [(t, (Maybe TrueDrumInput, TrueDrumGameState t))] -> NoteStatus t
trueNoteStatus noteTime note events = let
  slices = case events of
    []                      -> []
    (_, (_, gameState)) : _ -> tdNoteResults gameState
  in case dropWhile ((> noteTime) . fst) slices of
    (sliceTime, slice) : _ -> if noteTime == sliceTime
      then case Map.lookup note slice of
        Nothing                        -> NoteFuture
        Just TDMissed                  -> NoteMissed
        Just (TDHit t)                 -> NoteHitAt t
        Just (TDHitPendingHihat _)     -> NoteFuture
        Just (TDTwoHits t)             -> NoteHitAt t
        Just (TDTwoHitsPendingHihat _) -> NoteFuture
      else NoteFuture
    [] -> NoteFuture

applyTrueDrumEvent
  :: (Show t, Ord t, Num t)
  => t
  -> Maybe TrueDrumInput
  -> t
  -> TrueDrumPlayState t
  -> TrueDrumPlayState t
applyTrueDrumEvent tNew mpadNew halfWindow dps = let
  applyNoRewind (t, maybeInput) evts = let
    mClosestTime = do
      ct <- case (Set.lookupLE t $ tdNoteTimes dps, Set.lookupGE t $ tdNoteTimes dps) of
        (x, Nothing)     -> x
        (Nothing, y)     -> y
        (Just x, Just y) -> Just $ if t - x < y - t then x else y
      guard $ abs (ct - t) < halfWindow
      return ct
    originalSlices = case evts of
      []                      -> []
      (_, (_, gameState)) : _ -> tdNoteResults gameState

    -- determine misses
    missRemainingNotesInSlice sliceTime slice = let
      sliceNotes = maybe [] (Set.toList . drumNotes . commonState) $ Map.lookup sliceTime $ tdTrack dps
      allMisses = Map.fromList $ map (, TDMissed) sliceNotes
      combineStatus (TDHitPendingHihat     _) miss = miss
      combineStatus (TDTwoHitsPendingHihat _) miss = miss
      combineStatus current                   _    = current
      in (sliceTime, Map.unionWith combineStatus slice allMisses)
    -- first, if the top status is out of scope, all non-hit notes should be missed
    missRestOfTopStatus = case originalSlices of
      slices@((lastSliceTime, slice) : restSlices) -> if Just lastSliceTime == mClosestTime
        then slices
        else missRemainingNotesInSlice lastSliceTime slice : restSlices
      [] -> []
    -- then, any times greater than it but less than `fromMaybe t mClosestTime` should miss all notes
    missMaxBound = Exclusive $ fromMaybe t mClosestTime
    missMinBound = case missRestOfTopStatus of
      (lastSliceTime, _) : _ -> Exclusive lastSliceTime
      []                     -> Unbounded
    missableTimes = zoomSetList missMinBound missMaxBound $ tdNoteTimes dps
    missSkippedSlices = foldr (\time slices -> missRemainingNotesInSlice time Map.empty : slices) missRestOfTopStatus missableTimes
    didMiss = take 1 missSkippedSlices /= take 1 originalSlices

    -- The closest notes, which could be hit at this moment
    targetNotes = mClosestTime >>= \t' -> Map.lookup t' $ tdTrack dps
    previousState = case evts of
      []              -> initialTDState
      (_, (_, s)) : _ -> s
    -- The existing status of the target notes
    currentSlice = case mClosestTime of
      Nothing -> Map.empty
      Just sliceTime -> case originalSlices of
        (lastSliceTime, slice) : _ | sliceTime == lastSliceTime -> slice
        _                                                       -> Map.empty
    -- determine if normal note hit
    matchingHit = do
      state <- targetNotes
      input <- maybeInput
      case input of
        TDInputHihatOpen -> Nothing
        TDInputHit hit -> let
          match note = tdn_gem note == tdh_gem hit
          in find match $ drumNotes $ commonState state
    normalHitData = case matchingHit of
      Nothing -> Nothing
      Just note -> case Map.lookup note currentSlice of
        Nothing -> let
          newStatus = case tdn_type note of
            GemHihatOpen -> if tdIsHihatOpen previousState then TDHit t else TDHitPendingHihat True
            GemHihatClosed -> if tdIsHihatOpen previousState then TDHitPendingHihat False else TDHit t
            _ -> TDHit t
          in Just (note, newStatus)
        Just TDMissed -> Nothing -- probably shouldn't happen?
        Just (TDHit _) -> case tdn_extra note of
          Flam    -> Just (note, TDTwoHits t)
          NotFlam -> Nothing
        Just (TDHitPendingHihat b) -> case tdn_extra note of
          Flam    -> Just (note, TDTwoHitsPendingHihat b)
          NotFlam -> Nothing
        Just (TDTwoHits _) -> Nothing
        Just (TDTwoHitsPendingHihat _) -> Nothing
    isNormalHit = case normalHitData of
      Nothing                           -> False
      Just (_, TDHitPendingHihat     _) -> False
      Just (_, TDTwoHitsPendingHihat _) -> False
      Just _                            -> True
    -- determine if hihat pedal to make existing hit not pending
    hihatStatusChange = case maybeInput of
      Just TDInputHihatOpen                            -> Just True
      Just (TDInputHit hit) | tdh_gem hit == HihatFoot -> Just False
      _                                                -> Nothing
    hihatFixData = case hihatStatusChange of
      Nothing -> []
      Just newHihatOpen -> let
        canFinalizeNote (note, status) = case status of
          TDHitPendingHihat b -> do
            guard $ b == newHihatOpen
            Just (note, TDHit t)
          TDTwoHitsPendingHihat b -> do
            guard $ b == newHihatOpen
            Just (note, TDTwoHits t)
          _ -> Nothing
        in mapMaybe canFinalizeNote $ Map.toList currentSlice
    countNewHits = (if isNormalHit then 1 else 0) + length hihatFixData
    -- determine overhit
    overhitData = case normalHitData of
      Just _ -> Nothing
      Nothing -> case maybeInput of
        Just TDInputHihatOpen -> Nothing
        Just (TDInputHit hit) -> do
          guard $ tdh_gem hit /= HihatFoot
          Just (t, hit)
        Nothing -> Nothing

    -- finally, apply hit data to the current slice
    applyHitStatus
      = (\m -> foldr (uncurry Map.insert) m hihatFixData)
      $ maybe id (uncurry Map.insert) normalHitData
      $ currentSlice
    newSlices = case mClosestTime of
      Nothing   -> missSkippedSlices
      Just time -> case missSkippedSlices of
        []                    -> [(time, applyHitStatus)]
        (sliceTime, _) : rest -> if time == sliceTime
          then (time, applyHitStatus) : rest
          else (time, applyHitStatus) : missSkippedSlices

    newState = let
      comboBase = if didMiss || isJust overhitData
        then 0
        else tdCombo previousState
      in TrueDrumGameState
        { tdScore = tdScore previousState + 25 * countNewHits -- TODO apply multiplier
        , tdCombo = comboBase + countNewHits
        , tdIsHihatOpen = fromMaybe (tdIsHihatOpen previousState) hihatStatusChange
        , tdNoteResults = newSlices
        , tdOverhits = maybe id (:) overhitData $ tdOverhits previousState
        }
    in (t, (maybeInput, newState)) : evts
  in case span ((> tNew) . fst) $ tdEvents dps of
    (eventsToRewind, rest) -> let
      eventsToRewind' = flip map eventsToRewind $ \(t, (hit, _)) -> (t, hit)
      newEvents = foldr applyNoRewind rest $ eventsToRewind' ++ [(tNew, mpadNew)]
      in dps { tdEvents = newEvents }
