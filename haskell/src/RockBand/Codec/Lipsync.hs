{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.Lipsync where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           RockBand.Codec
import           RockBand.Codec.ProGuitar         (GtrChannel (..),
                                                   channelEdges)
import           RockBand.Common

newtype LipsyncTrack t = LipsyncTrack
  { lipEvents :: RTB.T t (MagmaViseme, (Slide, Int, t))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (LipsyncTrack t)

data Slide = NoSlide | SlideToNext | SlideFromPrev
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance GtrChannel Slide where
  encodeChannel = fromEnum

instance TraverseTrack LipsyncTrack where
  traverseTrack fn (LipsyncTrack a) = LipsyncTrack <$> do
    fmap (fmap (\(fret, (str, nt), len) -> (str, (nt, fret, len))) . joinEdgesSimple)
      $ fn
      $ splitEdgesSimple
      $ fmap (\(str, (nt, fret, len)) -> (fret, (str, nt), len))
      $ a

instance ParseTrack LipsyncTrack where
  parseTrack = do
    lipEvents <- (lipEvents =.) $ condenseMap $ eachKey each $ \viseme ->
      matchEdgesCV $ channelEdges (100 - fromEnum viseme)
    return LipsyncTrack{..}

data MagmaViseme
  = Viseme_Blink
  | Viseme_Brow_aggressive
  | Viseme_Brow_down
  | Viseme_Brow_pouty
  | Viseme_Brow_up
  | Viseme_Bump_hi
  | Viseme_Bump_lo
  | Viseme_Cage_hi
  | Viseme_Cage_lo
  | Viseme_Church_hi
  | Viseme_Church_lo
  | Viseme_Earth_hi
  | Viseme_Earth_lo
  | Viseme_Eat_hi
  | Viseme_Eat_lo
  | Viseme_Fave_hi
  | Viseme_Fave_lo
  | Viseme_If_hi
  | Viseme_If_lo
  | Viseme_New_hi
  | Viseme_New_lo
  | Viseme_Oat_hi
  | Viseme_Oat_lo
  | Viseme_Ox_hi
  | Viseme_Ox_lo
  | Viseme_Roar_hi
  | Viseme_Roar_lo
  | Viseme_Size_hi
  | Viseme_Size_lo
  | Viseme_Squint
  | Viseme_Though_hi
  | Viseme_Though_lo
  | Viseme_Told_hi
  | Viseme_Told_lo
  | Viseme_Wet_hi
  | Viseme_Wet_lo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
