{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Codec.Events where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common

data CrowdMood
  = CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Backing
  = BackingKick
  | BackingSnare
  | BackingHihat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data SectionType
  = SectionRB2 -- @[section foo]@
  | SectionRB3 -- @[prc_foo]@
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EventsTrack t = EventsTrack
  { eventsMusicStart :: RTB.T t ()
  , eventsMusicEnd   :: RTB.T t ()
  , eventsEnd        :: RTB.T t ()
  , eventsCoda       :: RTB.T t ()
  , eventsCrowd      :: RTB.T t CrowdMood
  , eventsCrowdClap  :: RTB.T t Bool
  , eventsSections   :: RTB.T t (SectionType, T.Text)
  , eventsBacking    :: RTB.T t Backing
  } deriving (Eq, Ord, Show)

instance TraverseTrack EventsTrack where
  traverseTrack fn (EventsTrack a b c d e f g h) = EventsTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g <*> fn h

instance ParseTrack EventsTrack where
  parseTrack = do
    eventsMusicStart <- eventsMusicStart =. commandMatch ["music_start"]
    eventsMusicEnd   <- eventsMusicEnd   =. commandMatch ["music_end"]
    eventsEnd        <- eventsEnd        =. commandMatch ["end"]
    eventsCoda       <- eventsCoda       =. commandMatch ["coda"]
    eventsCrowd <- (eventsCrowd =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      CrowdRealtime -> ["crowd_realtime"]
      CrowdIntense  -> ["crowd_intense"]
      CrowdNormal   -> ["crowd_normal"]
      CrowdMellow   -> ["crowd_mellow"]
    eventsCrowdClap <- (eventsCrowdClap =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      False -> ["crowd_noclap"]
      True  -> ["crowd_clap"]
    eventsSections <- eventsSections =. let
      fp = readCommand' >=> \case
        ["section", t] -> Just (SectionRB2, t)
        [s]            -> (SectionRB3 ,) <$> T.stripPrefix "prc_" s
        _              -> Nothing
      fs (SectionRB2, t) = showCommand' ["section", t]
      fs (SectionRB3, t) = showCommand' ["prc_" <> t]
      in single fp fs
    eventsBacking <- (eventsBacking =.) $ condenseMap_ $ eachKey each $ blip . \case
      BackingKick  -> 24
      BackingSnare -> 25
      BackingHihat -> 26
    return EventsTrack{..}

instance (NNC.C t) => Monoid (EventsTrack t) where
  mempty = EventsTrack
    RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (EventsTrack a1 a2 a3 a4 a5 a6 a7 a8)
    (EventsTrack b1 b2 b3 b4 b5 b6 b7 b8)
    = EventsTrack
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)
      (RTB.merge a7 b7)
      (RTB.merge a8 b8)
