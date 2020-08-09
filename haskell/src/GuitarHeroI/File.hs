{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module GuitarHeroI.File where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (stripPrefix)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           GuitarHeroII.PartGuitar          (HandMap (..),
                                                   PartDifficulty (..),
                                                   parseDifficulty)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.File
import           RockBand.Codec.Five              (FretPosition (..))
import           RockBand.Common                  (Difficulty (..), each)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import           Text.Read                        (readMaybe)

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
  , gemsMouthOpen    :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (GemsTrack t)

instance TraverseTrack GemsTrack where
  traverseTrack fn (GemsTrack a b) = GemsTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b

instance ParseTrack GemsTrack where
  parseTrack = do
    gemsDifficulties <- gemsDifficulties =. eachKey each parseDifficulty
    gemsMouthOpen    <- gemsMouthOpen    =. edges 108
    return GemsTrack{..}

data FretPosition60
  = FretPosition FretPosition
  | Fret60
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via GenericFullEnum FretPosition60

data AnimTrack t = AnimTrack
  { animFretPosition :: RTB.T t (FretPosition60, Bool)
  , animHandMap      :: RTB.T t HandMap
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (AnimTrack t)

data TriggersTrack t = TriggersTrack
  { triggers60 :: RTB.T t ()
  , triggers61 :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (TriggersTrack t)

data Event
  = Event_gtr_on
  | Event_gtr_off
  | Event_wail_on
  | Event_wail_off
  | Event_solo
  | Event_solo_on
  | Event_solo_off
  | Event_sing_on
  | Event_sing_off
  | Event_bass_on
  | Event_bass_off
  | Event_keys_on
  | Event_keys_off
  | Event_drum_on
  | Event_drum_normal
  | Event_drum_double
  | Event_drum_half
  | Event_drum_allbeat
  | Event_drum_off
  | Event_crowd_half_tempo
  | Event_sing_half_tempo
  | Event_bass_half_tempo
  | Event_crowd_normal_tempo
  | Event_sing_normal_tempo
  | Event_bass_normal_tempo
  | Event_verse
  | Event_chorus
  | Event_end
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EventsTrack t = EventsTrack
  { eventsList :: RTB.T t Event
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance TraverseTrack AnimTrack where
  traverseTrack fn (AnimTrack a b) = AnimTrack <$> fn a <*> fn b

instance ParseTrack AnimTrack where
  parseTrack = do
    animFretPosition <- (animFretPosition =.) $ condenseMap $ eachKey each
      $ \posn -> edges $ fromEnum posn + 40
    animHandMap <- animHandMap =. simpleText ""
    return AnimTrack{..}

instance TraverseTrack TriggersTrack where
  traverseTrack fn (TriggersTrack a b) = TriggersTrack <$> fn a <*> fn b

instance ParseTrack TriggersTrack where
  parseTrack = do
    triggers60 <- triggers60 =. blip 60
    triggers61 <- triggers61 =. blip 61
    return TriggersTrack{..}

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a) = EventsTrack <$> fn a

instance ParseTrack EventsTrack where
  parseTrack = do
    eventsList <- eventsList =. simpleText "Event_"
    return EventsTrack{..}

simpleText :: (Show a, Read a, Monad m, NNC.C t) => String -> TrackEvent m t a
simpleText prefix = Codec
  { codecIn = slurpTrack $ \mt -> let
    matcher s = readMaybe $ prefix ++ T.unpack s
    in case RTB.partitionMaybe matcher $ midiLyrics mt of
      (matches, rest) -> (matches, mt { midiLyrics = rest })
  , codecOut = makeTrackBuilder $ fmap $
    E.MetaEvent . Meta.TextEvent . (\s -> fromMaybe s $ stripPrefix prefix s) . show
  }
