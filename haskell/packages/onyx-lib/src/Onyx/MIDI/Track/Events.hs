{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module Onyx.MIDI.Track.Events where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.Sections
import qualified Sound.MIDI.Util                  as U

data CrowdMood
  = CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  deriving (Eq, Ord, Show, Enum, Bounded)

data Backing
  = BackingKick
  | BackingSnare
  | BackingHihat
  deriving (Eq, Ord, Show, Enum, Bounded)

data EventsTrack t = EventsTrack
  { eventsMusicStart :: RTB.T t ()
  , eventsMusicEnd   :: RTB.T t ()
  , eventsEnd        :: RTB.T t ()
  , eventsCoda       :: RTB.T t ()
  , eventsCodaResume :: RTB.T t () -- onyx event: if present, notes between [coda] and [coda_resume] are removed for RB but not CH
  , eventsCrowd      :: RTB.T t CrowdMood
  , eventsCrowdClap  :: RTB.T t Bool
  , eventsSections   :: RTB.T t Section
  , eventsBacking    :: RTB.T t Backing
  , eventsPreview    :: RTB.T t () -- seen in DLC, probably internal event that is used to calculate for songs.dta
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance ChopTrack EventsTrack where
  chopTake t = mapTrack $ U.trackTake t
  chopDrop t trk = EventsTrack
    { eventsMusicStart = U.trackDrop    t $ eventsMusicStart trk
    , eventsMusicEnd   = U.trackDrop    t $ eventsMusicEnd   trk
    , eventsEnd        = U.trackDrop    t $ eventsEnd        trk
    , eventsCoda       = U.trackDrop    t $ eventsCoda       trk
    , eventsCodaResume = U.trackDrop    t $ eventsCodaResume trk
    , eventsCrowd      = chopDropStatus t $ eventsCrowd      trk
    , eventsCrowdClap  = chopDropStatus t $ eventsCrowdClap  trk
    , eventsSections   = chopDropStatus t $ eventsSections   trk
    , eventsBacking    = U.trackDrop    t $ eventsBacking    trk
    , eventsPreview    = U.trackDrop    t $ eventsPreview    trk
    }

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e f g h i j) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j

instance ParseTrack EventsTrack where
  parseTrack = do
    eventsMusicStart <- eventsMusicStart =. commandMatch ["music_start"]
    eventsMusicEnd   <- eventsMusicEnd   =. commandMatch ["music_end"]
    eventsEnd        <- eventsEnd        =. commandMatch ["end"]
    eventsCoda       <- eventsCoda       =. commandMatch ["coda"]
    eventsCodaResume <- eventsCodaResume =. commandMatch ["coda_resume"]
    eventsCrowd <- (eventsCrowd =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      CrowdRealtime -> ["crowd_realtime"]
      CrowdIntense  -> ["crowd_intense"]
      CrowdNormal   -> ["crowd_normal"]
      CrowdMellow   -> ["crowd_mellow"]
    eventsCrowdClap <- (eventsCrowdClap =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      False -> ["crowd_noclap"]
      True  -> ["crowd_clap"]
    eventsSections <- eventsSections =. commandMatch' parseSection (pure . emitSection)
    eventsBacking <- (eventsBacking =.) $ condenseMap_ $ eachKey each $ blip . \case
      BackingKick  -> 24
      BackingSnare -> 25
      BackingHihat -> 26
    eventsPreview <- eventsPreview =. commandMatch ["preview"]
    return EventsTrack{..}
