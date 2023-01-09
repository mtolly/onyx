{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
module Onyx.FeedBack.Base where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Micro, Milli)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Wrapper      as NN

data Atom
  = Int Integer
  | Real Rational
  | Str T.Text
  deriving (Eq, Ord, Show)

type RawSection = (T.Text, RawLines)
type RawLines = [(Atom, [Atom])]

type Ticks = NN.Integer

data Event t
  = TimeSig Integer Integer
  | BPM Milli
  | Anchor Micro
  | Event T.Text
  | Note Integer t
  | Special Integer t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Chart t = Chart
  { chartSong   :: HM.HashMap T.Text Atom
  , chartTracks :: HM.HashMap T.Text (RTB.T t (Event t))
  } deriving (Eq, Show)

instance Functor Chart where
  fmap f (Chart song trks) =
    Chart song $ flip fmap trks $ RTB.mapTime f . RTB.mapBody (fmap f)
