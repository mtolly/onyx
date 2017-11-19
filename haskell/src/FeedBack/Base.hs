{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module FeedBack.Base where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Micro, Milli)
import           Data.Foldable                    (Foldable)
import qualified Data.HashMap.Strict              as Map
import qualified Data.Text                        as T
import           Data.Traversable                 (Traversable)
import qualified Numeric.NonNegative.Wrapper      as NN

data Atom
  = Int Integer
  | Real Rational
  | Str T.Text
  deriving (Eq, Ord, Show, Read)

type RawSection = (T.Text, RawLines)
type RawLines = [(Atom, [Atom])]

type Ticks = NN.Integer

data Event t
  = TimeSig Integer Integer
  | BPM Milli
  | Anchor Micro
  | Event T.Text
  | Note Integer t
  | Stream Integer t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Chart t = Chart
  { chartSong   :: Map.HashMap T.Text Atom
  , chartTracks :: Map.HashMap T.Text (RTB.T t (Event t))
  } deriving (Eq, Show)

instance Functor Chart where
  fmap f (Chart song trks) =
    Chart song $ flip fmap trks $ RTB.mapTime f . RTB.mapBody (fmap f)
