{- |
BAND KEYS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.BandKeys where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GuitarHeroII.PartGuitar          (Tempo (..))
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec

data BandKeysTrack t = BandKeysTrack
  { keysTempo :: RTB.T t Tempo
  , keysIdle  :: RTB.T t ()
  , keysPlay  :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance TraverseTrack BandKeysTrack where
  traverseTrack fn (BandKeysTrack a b c) = BandKeysTrack
    <$> fn a <*> fn b <*> fn c

instance (NNC.C t) => Monoid (BandKeysTrack t) where
  mempty = BandKeysTrack RTB.empty RTB.empty RTB.empty
  mappend
    (BandKeysTrack a1 a2 a3)
    (BandKeysTrack b1 b2 b3)
    = BandKeysTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)

instance ParseTrack BandKeysTrack where
  parseTrack = do
    keysTempo    <- keysTempo    =. command
    -- didn't actually see double tempo on keys
    keysIdle     <- keysIdle     =. commandMatch ["idle"]
    keysPlay     <- keysPlay     =. commandMatch ["play"]
    return BandKeysTrack{..}


