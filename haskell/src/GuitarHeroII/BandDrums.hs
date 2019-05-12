{- |
BAND DRUMS
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module GuitarHeroII.BandDrums where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           GuitarHeroII.PartGuitar          (Tempo (..))
import           MergeMonoid
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
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BandDrumsTrack t)

instance TraverseTrack BandDrumsTrack where
  traverseTrack fn (BandDrumsTrack a b c d e f g h i j) = BandDrumsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j

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
