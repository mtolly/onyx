{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module RhythmGame.PNF where

import           Control.Applicative              ((<|>))
import           Control.Monad.Trans.State        (evalState, get, put)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Set                         as Set
import           GHC.Generics
import           GHC.TypeLits
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Beat
import           RockBand.Common                  (pattern RNil, StrumHOPOTap,
                                                   pattern Wait)

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

data DrumState pad = DrumState
  { drumNotes      :: Set.Set pad
  , drumLanes      :: Map.Map pad Toggle
  , drumActivation :: Toggle
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (DrumState pad)

data GuitarState fret = GuitarState
  { guitarNotes   :: Map.Map fret (PNF IsOverdrive StrumHOPOTap)
  , guitarTremolo :: Map.Map fret Toggle
  , guitarTrill   :: Map.Map fret Toggle
  } deriving (Show, Generic)
    deriving (TimeState) via GenericTimeState (GuitarState fret)

class TimeState a where
  before :: a -> a
  before _ = empty
  after :: a -> a
  after _ = empty
  empty :: a

instance TimeState (PNF sust now) where
  before = \case
    Empty -> Empty
    P past -> PF past
    N _now -> Empty
    PN past _now -> PF past
    PF sust -> PF sust
    NF _now _future -> Empty
    PNF past _now _future -> PF past
  after = \case
    Empty -> Empty
    P _past -> Empty
    N _now -> Empty
    PN _past _now -> Empty
    PF sust -> PF sust
    NF _now future -> PF future
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
