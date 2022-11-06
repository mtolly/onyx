{- |
BAND BASS
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.Harmonix.GH2.BandBass where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.Harmonix.GH2.PartGuitar     (Tempo (..))
import           Onyx.MIDI.Read

data BandBassTrack t = BandBassTrack
  { bassTempo     :: RTB.T t Tempo
  , bassIdle      :: RTB.T t ()
  , bassPlay      :: RTB.T t ()
  , bassStrum     :: RTB.T t ()
  , bassMystery61 :: RTB.T t ()
  -- [wail_on] [wail_off]
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (BandBassTrack t)

instance TraverseTrack BandBassTrack where
  traverseTrack fn (BandBassTrack a b c d e) = BandBassTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e

instance ParseTrack BandBassTrack where
  parseTrack = do
    bassTempo    <- bassTempo    =. command
    bassIdle     <- bassIdle     =. commandMatch ["idle"]
    bassPlay     <- bassPlay     =. commandMatch ["play"]
    bassStrum <- bassStrum =. fatBlips (1/8) (blip 36)
    bassMystery61 <- bassMystery61 =. blip 61
    return BandBassTrack{..}
