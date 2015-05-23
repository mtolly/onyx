-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.ProKeys where

import Parser.Base
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Parser.TH

data Event
  = LaneShift LaneRange
  -- ^ Change the viewable play range. Should be placed at least a measure
  -- before any notes that require the new range.
  | Trainer Trainer -- ^ The beginning/end of Pro Keys trainer sections.
  | Mood Mood
  | Solo Bool -- ^ A keyboard solo section.
  | Glissando Bool -- ^ Place over a sequence of white notes for a freeform section.
  | Trill Bool -- ^ Fill lanes on two keys.
  | Overdrive Bool -- ^ An energy phrase.
  | BRE Bool -- ^ Fill lanes for a Big Rock Ending.
  | Note ProKey Bool
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = RangeC | RangeD | RangeE | RangeF | RangeG | RangeA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ProKey = RedYellow Key | BlueGreen Key | OrangeC
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |]

  [ blip 0 [p| LaneShift RangeC |]
  , blip 2 [p| LaneShift RangeD |]
  , blip 4 [p| LaneShift RangeE |]
  , blip 5 [p| LaneShift RangeF |]
  , blip 7 [p| LaneShift RangeG |]
  , blip 9 [p| LaneShift RangeA |]

  , edge 48 $ applyB [p| Note (RedYellow C ) |]
  , edge 49 $ applyB [p| Note (RedYellow Cs) |]
  , edge 50 $ applyB [p| Note (RedYellow D ) |]
  , edge 51 $ applyB [p| Note (RedYellow Ds) |]
  , edge 52 $ applyB [p| Note (RedYellow E ) |]
  , edge 53 $ applyB [p| Note (RedYellow F ) |]
  , edge 54 $ applyB [p| Note (RedYellow Fs) |]
  , edge 55 $ applyB [p| Note (RedYellow G ) |]
  , edge 56 $ applyB [p| Note (RedYellow Gs) |]
  , edge 57 $ applyB [p| Note (RedYellow A ) |]
  , edge 58 $ applyB [p| Note (RedYellow As) |]
  , edge 59 $ applyB [p| Note (RedYellow B ) |]
  , edge 60 $ applyB [p| Note (BlueGreen C ) |]
  , edge 61 $ applyB [p| Note (BlueGreen Cs) |]
  , edge 62 $ applyB [p| Note (BlueGreen D ) |]
  , edge 63 $ applyB [p| Note (BlueGreen Ds) |]
  , edge 64 $ applyB [p| Note (BlueGreen E ) |]
  , edge 65 $ applyB [p| Note (BlueGreen F ) |]
  , edge 66 $ applyB [p| Note (BlueGreen Fs) |]
  , edge 67 $ applyB [p| Note (BlueGreen G ) |]
  , edge 68 $ applyB [p| Note (BlueGreen Gs) |]
  , edge 69 $ applyB [p| Note (BlueGreen A ) |]
  , edge 70 $ applyB [p| Note (BlueGreen As) |]
  , edge 71 $ applyB [p| Note (BlueGreen B ) |]
  , edge 72 $ applyB [p| Note OrangeC        |]

  , edge 115 $ applyB [p| Solo |]
  , edge 116 $ applyB [p| Overdrive |]
  , edge 120 $ applyB [p| BRE |]
  , edge 126 $ applyB [p| Glissando |]
  , edge 127 $ applyB [p| Trill |]
  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| firstEventWhich $ \e -> readCommand' e >>= \case
        (t, "key") -> Just $ Trainer t
        _          -> Nothing
      |]
    , [e| \case Trainer t -> RTB.singleton NNC.zero $ showCommand' (t, "key") |]
    )
  ]
