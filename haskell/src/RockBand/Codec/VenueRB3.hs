{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Codec.VenueRB3 where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           RockBand.Codec
import           RockBand.Venue                   (Camera, Lighting,
                                                   PostProcess)

data VenueRB3 t = VenueRB3
  { vrb3Camera          :: RTB.T t Camera
  , vrb3SingGuitar      :: RTB.T t Bool
  , vrb3SingDrums       :: RTB.T t Bool
  , vrb3SingBass        :: RTB.T t Bool
  , vrb3SpotKeys        :: RTB.T t Bool
  , vrb3SpotVocal       :: RTB.T t Bool
  , vrb3SpotGuitar      :: RTB.T t Bool
  , vrb3SpotDrums       :: RTB.T t Bool
  , vrb3SpotBass        :: RTB.T t Bool
  , vrb3PostProcess     :: RTB.T t PostProcess
  , vrb3Lighting        :: RTB.T t Lighting
  , vrb3LightingFirst   :: RTB.T t ()
  , vrb3LightingPrev    :: RTB.T t ()
  , vrb3LightingNext    :: RTB.T t ()
  , vrb3BonusFX         :: RTB.T t ()
  , vrb3BonusFXOptional :: RTB.T t ()
  } deriving (Eq, Ord, Show)

instance ParseTrack VenueRB3 where
  parseTrack = do
    vrb3Camera          <- vrb3Camera          =. command
    vrb3SingGuitar      <- vrb3SingGuitar      =. edges 87
    vrb3SingDrums       <- vrb3SingDrums       =. edges 86
    vrb3SingBass        <- vrb3SingBass        =. edges 85
    vrb3SpotKeys        <- vrb3SpotKeys        =. edges 41
    vrb3SpotVocal       <- vrb3SpotVocal       =. edges 40
    vrb3SpotGuitar      <- vrb3SpotGuitar      =. edges 39
    vrb3SpotDrums       <- vrb3SpotDrums       =. edges 38 -- RBN2 docs incorrectly say this is bass
    vrb3SpotBass        <- vrb3SpotBass        =. edges 37 -- RBN2 docs incorrectly say this is drums
    vrb3PostProcess     <- vrb3PostProcess     =. command
    vrb3Lighting        <- vrb3Lighting        =. command
    vrb3LightingFirst   <- vrb3LightingFirst   =. commandMatch ["first"]
    vrb3LightingPrev    <- vrb3LightingPrev    =. commandMatch ["prev"]
    vrb3LightingNext    <- vrb3LightingNext    =. commandMatch ["next"]
    vrb3BonusFX         <- vrb3BonusFX         =. commandMatch ["bonusfx"]
    vrb3BonusFXOptional <- vrb3BonusFXOptional =. commandMatch ["bonusfx_optional"]
    return VenueRB3{..}
