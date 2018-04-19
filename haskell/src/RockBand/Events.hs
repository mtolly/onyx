{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RockBand.Events where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import qualified RockBand.Codec.Events            as Ev

data Event
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | CrowdNoclap
  | CrowdClap
  | SectionRB2 T.Text
  | SectionRB3 T.Text
  | PracticeKick
  | PracticeSnare
  | PracticeHihat
  deriving (Eq, Ord, Show, Read)

eventsFromLegacy :: (NNC.C t) => RTB.T t Event -> Ev.EventsTrack t
eventsFromLegacy = undefined

eventsToLegacy :: (NNC.C t) => Ev.EventsTrack t -> RTB.T t Event
eventsToLegacy = undefined
