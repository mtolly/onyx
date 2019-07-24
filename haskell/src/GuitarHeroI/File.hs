{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module GuitarHeroI.File where

import           Control.Monad.Codec
import qualified Data.Map                as Map
import           DeriveHelpers
import           GHC.Generics            (Generic)
import           GuitarHeroII.PartGuitar (PartDifficulty (..), parseDifficulty)
import           RockBand.Codec
import           RockBand.Codec.File
import           RockBand.Common         (Difficulty (..), each)

data GH1File t = GH1File
  { gh1T1Gems   :: GemsTrack t
  , gh1Anim     :: AnimTrack t
  , gh1Triggers :: TriggersTrack t
  , gh1Events   :: EventsTrack t
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GH1File t)

instance TraverseTrack GH1File where
  traverseTrack fn
    (GH1File a b c d)
    = GH1File
      <$> traverseTrack fn a <*> traverseTrack fn b
      <*> traverseTrack fn c <*> traverseTrack fn d

instance ParseFile GH1File where
  parseFile = do
    gh1T1Gems   <- gh1T1Gems   =. fileTrack "T1 GEMS"  []
    gh1Anim     <- gh1Anim     =. fileTrack "ANIM"     []
    gh1Triggers <- gh1Triggers =. fileTrack "TRIGGERS" []
    gh1Events   <- gh1Events   =. fileTrack "EVENTS"   []
    return GH1File{..}

data GemsTrack t = GemsTrack
  { gemsDifficulties :: Map.Map Difficulty (PartDifficulty t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GemsTrack t)

instance TraverseTrack GemsTrack where
  traverseTrack fn (GemsTrack a) = GemsTrack
    <$> traverse (traverseTrack fn) a

instance ParseTrack GemsTrack where
  parseTrack = do
    gemsDifficulties <- (gemsDifficulties =.) $ eachKey each parseDifficulty
    return GemsTrack{..}

data AnimTrack t = AnimTrack -- TODO
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid, Mergeable) via GenericMerge (AnimTrack t)

data TriggersTrack t = TriggersTrack -- TODO
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TriggersTrack t)

data EventsTrack t = EventsTrack -- TODO
  deriving (Eq, Ord, Show, Generic)
  deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance TraverseTrack AnimTrack where
  traverseTrack _ AnimTrack = pure AnimTrack

instance ParseTrack AnimTrack where
  parseTrack = return AnimTrack

instance TraverseTrack TriggersTrack where
  traverseTrack _ TriggersTrack = pure TriggersTrack

instance ParseTrack TriggersTrack where
  parseTrack = return TriggersTrack

instance TraverseTrack EventsTrack where
  traverseTrack _ EventsTrack = pure EventsTrack

instance ParseTrack EventsTrack where
  parseTrack = return EventsTrack
