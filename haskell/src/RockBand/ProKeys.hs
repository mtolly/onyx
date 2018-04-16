-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
module RockBand.ProKeys
( LaneRange(..), Pitch(..)
, Event(..)
, fixPSRange
) where

import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.ProKeys           (LaneRange (..), Pitch (..))
import           RockBand.Common
import           RockBand.Parse
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U

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

-- | Stretches out each range shift event until the next one.
-- Thanks to mazegeek999 for showing me that this is allowed!
unparseNice :: U.Beats -> RTB.T U.Beats Event -> RTB.T U.Beats E.T
unparseNice defLength trk = let
  (ranges, notRanges) = flip RTB.partitionMaybe trk $ \case
    LaneShift r -> Just r
    _           -> Nothing
  (notes, notNotes) = flip RTB.partitionMaybe notRanges $ \case
    Note ln -> Just ln
    _       -> Nothing
  notRanges' = RTB.merge (Note <$> showEdgesNice' defLength notes) notNotes
  rangeEvents = go Nothing ranges
  lastTime rtb = case reverse $ ATB.toPairList $ RTB.toAbsoluteEventList 0 rtb of
    []         -> 0
    (t, _) : _ -> t
  -- The last range shift will be stretched until the very last PK event.
  lastLength = max (1 / 32) $ lastTime trk - lastTime ranges
  go curRange rngs = case RTB.viewL rngs of
    Nothing -> case curRange of
      Nothing  -> RTB.empty
      Just cur -> RTB.singleton lastLength $ endRange cur
    Just ((dt, rng), rngs') -> case curRange of
      Nothing  -> RTB.cons dt (startRange rng) $ go (Just rng) rngs'
      Just cur -> RTB.cons dt (endRange cur) $ RTB.cons 0 (startRange rng) $ go (Just rng) rngs'
  startRange r = makeEdge (rangePitch r) True
  endRange r = makeEdge (rangePitch r) False
  rangePitch = \case
    RangeC -> 0
    RangeD -> 2
    RangeE -> 4
    RangeF -> 5
    RangeG -> 7
    RangeA -> 9
  notRangeEvents = U.trackJoin $ fmap unparseOne notRanges'
  in RTB.merge rangeEvents notRangeEvents

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |]) $

  [ blip 0 [p| LaneShift RangeC |]
  , blip 2 [p| LaneShift RangeD |]
  , blip 4 [p| LaneShift RangeE |]
  , blip 5 [p| LaneShift RangeF |]
  , blip 7 [p| LaneShift RangeG |]
  , blip 9 [p| LaneShift RangeA |]

  ] ++ noteParser 48 [p| RedYellow C  |] (\p -> [p| Note $p |])
    ++ noteParser 49 [p| RedYellow Cs |] (\p -> [p| Note $p |])
    ++ noteParser 50 [p| RedYellow D  |] (\p -> [p| Note $p |])
    ++ noteParser 51 [p| RedYellow Ds |] (\p -> [p| Note $p |])
    ++ noteParser 52 [p| RedYellow E  |] (\p -> [p| Note $p |])
    ++ noteParser 53 [p| RedYellow F  |] (\p -> [p| Note $p |])
    ++ noteParser 54 [p| RedYellow Fs |] (\p -> [p| Note $p |])
    ++ noteParser 55 [p| RedYellow G  |] (\p -> [p| Note $p |])
    ++ noteParser 56 [p| RedYellow Gs |] (\p -> [p| Note $p |])
    ++ noteParser 57 [p| RedYellow A  |] (\p -> [p| Note $p |])
    ++ noteParser 58 [p| RedYellow As |] (\p -> [p| Note $p |])
    ++ noteParser 59 [p| RedYellow B  |] (\p -> [p| Note $p |])
    ++ noteParser 60 [p| BlueGreen C  |] (\p -> [p| Note $p |])
    ++ noteParser 61 [p| BlueGreen Cs |] (\p -> [p| Note $p |])
    ++ noteParser 62 [p| BlueGreen D  |] (\p -> [p| Note $p |])
    ++ noteParser 63 [p| BlueGreen Ds |] (\p -> [p| Note $p |])
    ++ noteParser 64 [p| BlueGreen E  |] (\p -> [p| Note $p |])
    ++ noteParser 65 [p| BlueGreen F  |] (\p -> [p| Note $p |])
    ++ noteParser 66 [p| BlueGreen Fs |] (\p -> [p| Note $p |])
    ++ noteParser 67 [p| BlueGreen G  |] (\p -> [p| Note $p |])
    ++ noteParser 68 [p| BlueGreen Gs |] (\p -> [p| Note $p |])
    ++ noteParser 69 [p| BlueGreen A  |] (\p -> [p| Note $p |])
    ++ noteParser 70 [p| BlueGreen As |] (\p -> [p| Note $p |])
    ++ noteParser 71 [p| BlueGreen B  |] (\p -> [p| Note $p |])
    ++ noteParser 72 [p| OrangeC      |] (\p -> [p| Note $p |]) ++

  [ edge 115 $ applyB [p| Solo |]
  , edge 116 $ applyB [p| Overdrive |]
  , edge 120 $ applyB [p| BRE |]
  , edge 126 $ applyB [p| Glissando |]
  , edge 127 $ applyB [p| Trill |]
  , ( [e| one $ mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| one $ firstEventWhich $ \e -> readCommand' e >>= \case
        (t, s) | s == T.pack "key" -> Just $ Trainer t
        _                          -> Nothing
      |]
    , [e| \case Trainer t -> RTB.singleton NNC.zero $ showCommand' (t, T.pack "key") |]
    )
  ]
