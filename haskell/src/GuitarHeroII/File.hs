{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.File where

import           Control.Monad.Codec
import           GuitarHeroII.BandBass
import           GuitarHeroII.BandDrums
import           GuitarHeroII.BandKeys
import           GuitarHeroII.BandSinger
import           GuitarHeroII.Events
import           GuitarHeroII.PartGuitar
import           GuitarHeroII.Triggers
import qualified Numeric.NonNegative.Class as NNC
import           RockBand.Codec
import           RockBand.Codec.File

data GH2File t = GH2File
  { gh2PartGuitar     :: PartTrack t
  , gh2PartBass       :: PartTrack t
  , gh2PartRhythm     :: PartTrack t
  , gh2PartGuitarCoop :: PartTrack t
  , gh2BandBass       :: BandBassTrack t
  , gh2BandDrums      :: BandDrumsTrack t
  , gh2BandKeys       :: BandKeysTrack t
  , gh2BandSinger     :: BandSingerTrack t
  , gh2Events         :: EventsTrack t
  , gh2Triggers       :: TriggersTrack t
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (GH2File t) where
  (<>)
    (GH2File a1 a2 a3 a4 a5 a6 a7 a8 a9 a10)
    (GH2File b1 b2 b3 b4 b5 b6 b7 b8 b9 b10)
    = GH2File
      (a1  <> b1 )
      (a2  <> b2 )
      (a3  <> b3 )
      (a4  <> b4 )
      (a5  <> b5 )
      (a6  <> b6 )
      (a7  <> b7 )
      (a8  <> b8 )
      (a9  <> b9 )
      (a10 <> b10)

instance (NNC.C t) => Monoid (GH2File t) where
  mempty = GH2File
    mempty mempty mempty mempty mempty
    mempty mempty mempty mempty mempty

instance TraverseTrack GH2File where
  traverseTrack fn
    (GH2File a b c d e f g h i j)
    = GH2File <$> traverseTrack fn a
      <*> traverseTrack fn b <*> traverseTrack fn c <*> traverseTrack fn d
      <*> traverseTrack fn e <*> traverseTrack fn f <*> traverseTrack fn g
      <*> traverseTrack fn h <*> traverseTrack fn i <*> traverseTrack fn j

instance ParseFile GH2File where
  parseFile = do
    gh2PartGuitar     <- gh2PartGuitar     =. fileTrack "PART GUITAR"      []
    gh2PartBass       <- gh2PartBass       =. fileTrack "PART BASS"        []
    gh2PartRhythm     <- gh2PartRhythm     =. fileTrack "PART RHYTHM"      []
    gh2PartGuitarCoop <- gh2PartGuitarCoop =. fileTrack "PART GUITAR COOP" []
    gh2BandBass       <- gh2BandBass       =. fileTrack "BAND BASS"        []
    gh2BandDrums      <- gh2BandDrums      =. fileTrack "BAND DRUMS"       []
    gh2BandKeys       <- gh2BandKeys       =. fileTrack "BAND KEYS"        []
    gh2BandSinger     <- gh2BandSinger     =. fileTrack "BAND SINGER"      []
    gh2Events         <- gh2Events         =. fileTrack "EVENTS"           []
    gh2Triggers       <- gh2Triggers       =. fileTrack "TRIGGERS"         []
    return GH2File{..}
