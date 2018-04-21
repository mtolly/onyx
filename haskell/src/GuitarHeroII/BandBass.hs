{- |
BAND BASS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.BandBass where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GuitarHeroII.PartGuitar          (Tempo (..))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec

data BandBassTrack t = BandBassTrack
  { bassTempo     :: RTB.T t Tempo
  , bassIdle      :: RTB.T t ()
  , bassPlay      :: RTB.T t ()
  , bassStrum     :: RTB.T t ()
  , bassMystery61 :: RTB.T t ()
  -- [wail_on] [wail_off]
  } deriving (Eq, Ord, Show)

instance TraverseTrack BandBassTrack where
  traverseTrack fn (BandBassTrack a b c d e) = BandBassTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance (NNC.C t) => Monoid (BandBassTrack t) where
  mempty = BandBassTrack RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (BandBassTrack a1 a2 a3 a4 a5)
    (BandBassTrack b1 b2 b3 b4 b5)
    = BandBassTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)

instance ParseTrack BandBassTrack where
  parseTrack = do
    bassTempo    <- bassTempo    =. command
    bassIdle     <- bassIdle     =. commandMatch ["idle"]
    bassPlay     <- bassPlay     =. commandMatch ["play"]
    bassStrum <- bassStrum =. blip 36
    bassMystery61 <- bassMystery61 =. blip 61
    return BandBassTrack{..}
