{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
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

instance Command CrowdMood where
  fromCommand = \case
    CrowdRealtime -> ["crowd_realtime"]
    CrowdIntense  -> ["crowd_intense" ]
    CrowdNormal   -> ["crowd_normal"  ]
    CrowdMellow   -> ["crowd_mellow"  ]
  toCommand = reverseLookup each fromCommand

data Backing
  = BackingKick
  | BackingSnare
  | BackingHihat
  deriving (Eq, Ord, Show, Enum, Bounded)

data EventsTrack t = EventsTrack
  { musicStart :: RTB.T t ()
  , musicEnd   :: RTB.T t ()
  , end        :: RTB.T t ()
  , coda       :: RTB.T t ()
  , codaResume :: RTB.T t () -- onyx event: if present, notes between [coda] and [coda_resume] are removed for RB but not CH
  , crowd      :: RTB.T t CrowdMood
  , crowdClap  :: RTB.T t Bool
  , sections   :: RTB.T t Section
  , backing    :: RTB.T t Backing
  , preview    :: RTB.T t () -- seen in DLC, probably internal event that is used to calculate for songs.dta
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EventsTrack t)

instance ChopTrack EventsTrack where
  chopTake t = mapTrack $ U.trackTake t
  chopDrop t trk = EventsTrack
    { musicStart = U.trackDrop    t trk.musicStart
    , musicEnd   = U.trackDrop    t trk.musicEnd
    , end        = U.trackDrop    t trk.end
    , coda       = U.trackDrop    t trk.coda
    , codaResume = U.trackDrop    t trk.codaResume
    , crowd      = chopDropStatus t trk.crowd
    , crowdClap  = chopDropStatus t trk.crowdClap
    , sections   = chopDropStatus t trk.sections
    , backing    = U.trackDrop    t trk.backing
    , preview    = U.trackDrop    t trk.preview
    }

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e f g h i j) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h <*> fn i <*> fn j

instance ParseTrack EventsTrack where
  parseTrack = do
    musicStart <- (.musicStart) =. commandMatch ["music_start"]
    musicEnd   <- (.musicEnd  ) =. commandMatch ["music_end"]
    end        <- (.end       ) =. commandMatch ["end"]
    coda       <- (.coda      ) =. commandMatch ["coda"]
    codaResume <- (.codaResume) =. commandMatch ["coda_resume"]
    crowd      <- (.crowd     ) =. command
    crowdClap  <- (.crowdClap ) =. do
      condenseMap_ $ eachKey each $ commandMatch . \case
        False -> ["crowd_noclap"]
        True  -> ["crowd_clap"]
    sections   <- (.sections  ) =. commandMatch' parseSection (pure . emitSection)
    backing    <- (.backing   ) =. do
      condenseMap_ $ eachKey each $ blip . \case
        BackingKick  -> 24
        BackingSnare -> 25
        BackingHihat -> 26
    preview    <- (.preview   ) =. commandMatch ["preview"]
    return EventsTrack{..}
