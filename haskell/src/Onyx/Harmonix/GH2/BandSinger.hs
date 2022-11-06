{- |
BAND SINGER
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.Harmonix.GH2.BandSinger where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.Harmonix.GH2.PartGuitar     (Tempo (..))
import           Onyx.MIDI.Read

data BandSingerTrack t = BandSingerTrack
  { singerTempo :: RTB.T t Tempo
  , singerIdle  :: RTB.T t ()
  , singerPlay  :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BandSingerTrack t)

instance TraverseTrack BandSingerTrack where
  traverseTrack fn (BandSingerTrack a b c) = BandSingerTrack
    <$> fn a <*> fn b <*> fn c

instance ParseTrack BandSingerTrack where
  parseTrack = do
    singerTempo    <- singerTempo    =. command
    singerIdle     <- singerIdle     =. commandMatch ["idle"]
    singerPlay     <- singerPlay     =. commandMatch ["play"]
    return BandSingerTrack{..}
