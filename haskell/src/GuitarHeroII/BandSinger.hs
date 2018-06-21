{- |
BAND SINGER
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.BandSinger where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GuitarHeroII.PartGuitar          (Tempo (..))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec

data BandSingerTrack t = BandSingerTrack
  { singerTempo :: RTB.T t Tempo
  , singerIdle  :: RTB.T t ()
  , singerPlay  :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance TraverseTrack BandSingerTrack where
  traverseTrack fn (BandSingerTrack a b c) = BandSingerTrack
    <$> fn a <*> fn b <*> fn c

instance (NNC.C t) => Semigroup (BandSingerTrack t) where
  (<>)
    (BandSingerTrack a1 a2 a3)
    (BandSingerTrack b1 b2 b3)
    = BandSingerTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)

instance (NNC.C t) => Monoid (BandSingerTrack t) where
  mempty = BandSingerTrack RTB.empty RTB.empty RTB.empty

instance ParseTrack BandSingerTrack where
  parseTrack = do
    singerTempo    <- singerTempo    =. command
    singerIdle     <- singerIdle     =. commandMatch ["idle"]
    singerPlay     <- singerPlay     =. commandMatch ["play"]
    return BandSingerTrack{..}


