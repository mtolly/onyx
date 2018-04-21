{- |
BAND DRUMS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.BandDrums where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GuitarHeroII.PartGuitar          (Tempo (..))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec

data BandDrumsTrack t = BandDrumsTrack
  { drumsTempo      :: RTB.T t Tempo
  , drumsIdle       :: RTB.T t ()
  , drumsPlay       :: RTB.T t ()
  , drumsNoBeat     :: RTB.T t ()
  , drumsAllBeat    :: RTB.T t ()
  , drumsKick       :: RTB.T t ()
  , drumsCrash      :: RTB.T t ()
  , drumsMystery65  :: RTB.T t ()
   -- not sure if actually a difference between tempo and time
  , drumsHalfTime   :: RTB.T t ()
  , drumsDoubleTime :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance TraverseTrack BandDrumsTrack where
  traverseTrack fn (BandDrumsTrack a b c d e f g h i j) = BandDrumsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j

instance (NNC.C t) => Monoid (BandDrumsTrack t) where
  mempty = BandDrumsTrack
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (BandDrumsTrack a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    (BandDrumsTrack b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
    = BandDrumsTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)
      (RTB.merge a7 b7)
      (RTB.merge a8 b8)
      (RTB.merge a9 b9)
      (RTB.merge a10 b10)

instance ParseTrack BandDrumsTrack where
  parseTrack = do
    drumsKick <- drumsKick =. blip 36
    drumsCrash <- drumsCrash =. blip 37
    -- Laughtrack has 48 and 49, I think they are Kick and Crash (mistakenly up an octave)
    drumsMystery65 <- drumsMystery65 =. blip 65
    drumsTempo   <- drumsTempo   =. command
    drumsIdle    <- drumsIdle    =. commandMatch ["idle"]
    drumsPlay    <- drumsPlay    =. commandMatch ["play"]
    drumsNoBeat  <- drumsNoBeat  =. commandMatch ["nobeat"]
    drumsAllBeat <- drumsAllBeat =. commandMatch ["allbeat"] -- Jessica has [all_beat], soy bomb has [allplay]
    drumsHalfTime  <- drumsHalfTime  =. commandMatch ["half_time"] -- new black has [halftime]
    drumsDoubleTime  <- drumsDoubleTime  =. commandMatch ["double_time"] -- Who Was In My Room has [doubletime]
    -- War Pigs has [double]
    return BandDrumsTrack{..}
