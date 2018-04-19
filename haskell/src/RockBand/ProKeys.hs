-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RockBand.ProKeys
( LaneRange(..), Pitch(..)
, Event(..)
, fixPSRange
, pkFromLegacy, pkToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.ProKeys
import           RockBand.Common

data Event
  = LaneShift LaneRange
  -- ^ Change the viewable play range. Should be placed at least a measure
  -- before any notes that require the new range.
  | Trainer Trainer -- ^ The beginning/end of Pro Keys trainer sections.
  | Mood       Mood
  | Solo       Bool -- ^ A keyboard solo section.
  | Glissando  Bool -- ^ Place over a sequence of white notes for a freeform section.
  | Trill      Bool -- ^ Fill lanes on two keys.
  | Overdrive  Bool -- ^ An energy phrase.
  | BRE        Bool -- ^ Fill lanes for a Big Rock Ending.
  | Note (LongNote () Pitch)
  deriving (Eq, Ord, Show, Read)

-- | Moves the first range event to time zero, to work around a Phase Shift bug.
fixPSRange :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
fixPSRange trk = let
  (ranges, notRanges) = flip RTB.partitionMaybe trk $ \case
    LaneShift r -> Just r
    _           -> Nothing
  rangesFixed = case RTB.viewL ranges of
    Just ((t1, range1), ranges') -> case RTB.viewL ranges' of
      Just ((t2, range2), ranges'') ->
        RTB.cons NNC.zero range1 $ RTB.cons (NNC.add t1 t2) range2 ranges''
      Nothing -> RTB.singleton NNC.zero range1
    Nothing -> RTB.empty
  in RTB.merge (fmap LaneShift rangesFixed) notRanges

pkFromLegacy :: (NNC.C t) => RTB.T t Event -> ProKeysTrack t
pkFromLegacy leg = ProKeysTrack
  { pkTrainer   = RTB.mapMaybe (\case Trainer x -> Just x; _ -> Nothing) leg
  , pkMood      = RTB.mapMaybe (\case Mood x -> Just x; _ -> Nothing) leg
  , pkSolo      = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  , pkGlissando = RTB.mapMaybe (\case Glissando x -> Just x; _ -> Nothing) leg
  , pkTrill     = RTB.mapMaybe (\case Trill x -> Just x; _ -> Nothing) leg
  , pkOverdrive = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , pkBRE       = RTB.mapMaybe (\case BRE x -> Just x; _ -> Nothing) leg
  , pkLanes     = RTB.mapMaybe (\case LaneShift x -> Just x; _ -> Nothing) leg
  , pkNotes     = fmap (\((), c, len) -> (c, len)) $ joinEdges $ RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg
  }

pkToLegacy :: (NNC.C t) => ProKeysTrack t -> RTB.T t Event
pkToLegacy o = foldr RTB.merge RTB.empty
  [ Trainer   <$> pkTrainer   o
  , Mood      <$> pkMood      o
  , Solo      <$> pkSolo      o
  , Glissando <$> pkGlissando o
  , Trill     <$> pkTrill     o
  , Overdrive <$> pkOverdrive o
  , BRE       <$> pkBRE       o
  , LaneShift <$> pkLanes     o
  , fmap Note $ splitEdges $ fmap (\(c, len) -> ((), c, len)) $ pkNotes o
  ]
