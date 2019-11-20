{-# LANGUAGE DataKinds            #-}
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

type PNFBool = PNF Bool ()

newtype TotalMap k a = TotalMap (Map.Map k a)
  deriving (Eq, Ord, Show)

type IsOverdrive = Bool

data CommonState a = CommonState
  { commonState     :: a
  , commonOverdrive :: PNFBool
  , commonBRE       :: PNFBool
  , commonSolo      :: PNFBool
  , commonBeats     :: Maybe (Maybe BeatEvent)
  } deriving (Generic)
    deriving (TimeState) via GenericTimeState (CommonState a)

data DrumState pad = DrumState
  { drumNotes      :: TotalMap pad Bool
  , drumLanes      :: TotalMap pad PNFBool
  , drumActivation :: PNFBool
  } deriving (Generic)
    deriving (TimeState) via GenericTimeState (DrumState pad)

data GuitarState fret = GuitarState
  { guitarNotes   :: TotalMap fret (PNF IsOverdrive (IsOverdrive, StrumHOPOTap))
  , guitarTremolo :: TotalMap fret PNFBool
  , guitarTrill   :: TotalMap fret PNFBool
  } deriving (Generic)
    deriving (TimeState) via GenericTimeState (GuitarState fret)

class TimeState a where
  before :: a -> a
  after :: a -> a
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

instance TimeState (Maybe a) where
  before _ = Nothing
  after _ = Nothing
  empty = Nothing

instance TimeState Bool where
  before _ = False
  after _ = False
  empty = False

instance (TimeState a, Ord k, Enum k, Bounded k) => TimeState (TotalMap k a) where
  before (TotalMap m) = TotalMap $ fmap before m
  after (TotalMap m) = TotalMap $ fmap after m
  empty = TotalMap $ Map.fromList [ (k, empty) | k <- [minBound .. maxBound] ]

combineStateMaps :: (TimeState a, TimeState b, Ord t) => Map.Map t a -> Map.Map t b -> Map.Map t (a, b)
combineStateMaps xs ys = let
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

combineStateLists :: (TimeState a, TimeState b, NNC.C t) => RTB.T t a -> RTB.T t b -> RTB.T t (a, b)
combineStateLists rtbX rtbY = let
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
