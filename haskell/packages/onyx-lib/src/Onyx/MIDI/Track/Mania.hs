{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.Mania where

import           Control.Monad.Codec
import           Data.Bifunctor
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Sound.MIDI.Message.Channel.Voice as V

newtype ManiaTrack t = ManiaTrack
  { maniaNotes :: RTB.T t (Edge () Int)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ManiaTrack t)

instance TraverseTrack ManiaTrack where
  traverseTrack fn (ManiaTrack a) = ManiaTrack <$> fn a

instance ParseTrack ManiaTrack where
  parseTrack = do
    maniaNotes <- maniaNotes =. fatBlips (1/8) Codec
      { codecIn = slurpTrack $ \mt -> let
        applyPitchEdges (pitch, thisEdges) = flip fmap thisEdges $ bimap
          (const ())
          (const $ V.fromPitch pitch)
        notes
          = foldr RTB.merge RTB.empty
          $ map applyPitchEdges
          $ Map.toList $ midiNotes mt
        mt' = mt { midiNotes = Map.empty }
        in (notes, mt')
      , codecOut = makeTrackBuilder $ fmap $ makeEdge' . bimap (const 96) (0,)
      }
    return ManiaTrack{..}
