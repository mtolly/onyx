{-# LANGUAGE LambdaCase #-}
module Midi
( Preview(..), buildPreview
, ExtendedBeat(..)
) where

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Drums                   as Drums
import           RockBand.Events
import qualified RockBand.File                    as File
import qualified Sound.MIDI.Util                  as U

data ExtendedBeat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

insertHalfBeats :: RTB.T U.Beats ExtendedBeat -> RTB.T U.Beats ExtendedBeat
insertHalfBeats = let
  f rtb = case RTB.viewL rtb of
    Nothing              -> RTB.empty
    Just ((dt, x), rtb') -> RTB.cons dt x $ g rtb'
  g rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') ->
      RTB.cons (dt * 0.5) HalfBeat $ RTB.cons (dt * 0.5) x $ g rtb'
  in f

data Preview = Preview
  { gems          :: !(Map.Map U.Seconds [Drums.Gem Drums.ProType])
  , beatLines     :: !(Map.Map U.Seconds ExtendedBeat)
  , timeToMeasure :: !(U.Seconds -> U.MeasureBeats)
  , theEnd        :: !(Maybe U.Seconds)
  }

buildPreview :: File.Song U.Beats -> Preview
buildPreview file = let
  tmap = File.s_tempos file
  mmap = File.s_signatures file
  trks = File.s_tracks file
  theDrums  = foldr RTB.merge RTB.empty [ trk | File.PartDrums trk <- trks ]
  theBeat   = foldr RTB.merge RTB.empty [ trk | File.Beat      trk <- trks ]
  theEvents = foldr RTB.merge RTB.empty [ trk | File.Events    trk <- trks ]
  gemTrack :: RTB.T U.Seconds (Drums.Gem Drums.ProType)
  gemTrack = U.applyTempoTrack tmap $ pickExpert $ Drums.assignToms theDrums
  pickExpert = RTB.mapMaybe $ \(d, x) -> case d of
    Expert -> Just x
    _      -> Nothing
  gemMap :: Map.Map U.Seconds [Drums.Gem Drums.ProType]
  gemMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    RTB.collectCoincident gemTrack
  beatTrack :: RTB.T U.Seconds ExtendedBeat
  beatTrack = U.applyTempoTrack tmap $ insertHalfBeats $ extendBeats theBeat
  beatMap :: Map.Map U.Seconds ExtendedBeat
  beatMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 beatTrack
  endEvent :: Maybe U.Seconds
  endEvent = case RTB.viewL $ RTB.filter (== End) theEvents of
    Just ((bts, _), _) -> Just $ U.applyTempoMap tmap bts
    Nothing            -> Nothing
  extendBeats = fmap $ \case
    Beat.Bar  -> Bar
    Beat.Beat -> Beat
  in Preview
    { gems = gemMap
    , beatLines = beatMap
    , timeToMeasure = U.applyMeasureMap mmap . U.unapplyTempoMap tmap
    , theEnd = endEvent
    }
