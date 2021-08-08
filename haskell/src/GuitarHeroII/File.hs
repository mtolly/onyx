{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module GuitarHeroII.File where

import           Control.Monad.Codec
import           DeriveHelpers
import           GHC.Generics            (Generic)
import           GuitarHeroII.BandBass
import           GuitarHeroII.BandDrums
import           GuitarHeroII.BandKeys
import           GuitarHeroII.BandSinger
import           GuitarHeroII.Events
import           GuitarHeroII.PartDrum
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import           RockBand.Codec
import           RockBand.Codec.File

data GH2File t = GH2File
  { gh2PartGuitar     :: PartTrack t
  , gh2PartBass       :: PartTrack t
  , gh2PartRhythm     :: PartTrack t
  , gh2PartDrum       :: GH2DrumTrack t
  , gh2PartGuitarCoop :: PartTrack t
  , gh2BandBass       :: BandBassTrack t
  , gh2BandDrums      :: BandDrumsTrack t
  , gh2BandKeys       :: BandKeysTrack t
  , gh2BandSinger     :: BandSingerTrack t
  , gh2Events         :: EventsTrack t
  , gh2Triggers       :: TriggersTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GH2File t)

instance TraverseTrack GH2File where
  traverseTrack fn
    (GH2File a b c d e f g h i j k)
    = GH2File <$> traverseTrack fn a
      <*> traverseTrack fn b <*> traverseTrack fn c <*> traverseTrack fn d
      <*> traverseTrack fn e <*> traverseTrack fn f <*> traverseTrack fn g
      <*> traverseTrack fn h <*> traverseTrack fn i <*> traverseTrack fn j
      <*> traverseTrack fn k

instance ParseFile GH2File where
  parseFile = do
    gh2PartGuitar     <- gh2PartGuitar     =. fileTrack (pure "PART GUITAR"     )
    gh2PartBass       <- gh2PartBass       =. fileTrack (pure "PART BASS"       )
    gh2PartRhythm     <- gh2PartRhythm     =. fileTrack (pure "PART RHYTHM"     )
    gh2PartDrum       <- gh2PartDrum       =. fileTrack (pure "PART DRUM"       )
    gh2PartGuitarCoop <- gh2PartGuitarCoop =. fileTrack (pure "PART GUITAR COOP")
    gh2BandBass       <- gh2BandBass       =. fileTrack (pure "BAND BASS"       )
    gh2BandDrums      <- gh2BandDrums      =. fileTrack (pure "BAND DRUMS"      )
    gh2BandKeys       <- gh2BandKeys       =. fileTrack (pure "BAND KEYS"       )
    gh2BandSinger     <- gh2BandSinger     =. fileTrack (pure "BAND SINGER"     )
    gh2Events         <- gh2Events         =. fileTrack (pure "EVENTS"          )
    gh2Triggers       <- gh2Triggers       =. fileTrack (pure "TRIGGERS"        )
    return GH2File{..}
