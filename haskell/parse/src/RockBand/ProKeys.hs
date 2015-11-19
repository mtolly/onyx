-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module RockBand.ProKeys where

import RockBand.Common
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import RockBand.Parse

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
  | Note       Bool Pitch
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = RangeC | RangeD | RangeE | RangeF | RangeG | RangeA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Pitch = RedYellow Key | BlueGreen Key | OrangeC
  deriving (Eq, Ord, Show, Read)

instance Enum Pitch where
  fromEnum (RedYellow k) = fromEnum k
  fromEnum (BlueGreen k) = fromEnum k + 12
  fromEnum OrangeC       = 24

  toEnum i = case divMod i 12 of
    (0, j) -> RedYellow $ toEnum j
    (1, j) -> BlueGreen $ toEnum j
    (2, 0) -> OrangeC
    _      -> error $ "No pro keys Pitch for: fromEnum " ++ show i

  succ (RedYellow B) = BlueGreen C
  succ (RedYellow k) = RedYellow $ succ k
  succ (BlueGreen B) = OrangeC
  succ (BlueGreen k) = BlueGreen $ succ k
  succ OrangeC       = error "no succ for pro keys Pitch OrangeC"

  pred (RedYellow C) = error "no pred for pro keys Pitch: RedYellow C"
  pred (RedYellow k) = RedYellow $ pred k
  pred (BlueGreen C) = RedYellow B
  pred (BlueGreen k) = BlueGreen $ pred k
  pred OrangeC       = BlueGreen B

instance Bounded Pitch where
  minBound = RedYellow minBound
  maxBound = OrangeC

instanceMIDIEvent [t| Event |]

  [ blip 0 [p| LaneShift RangeC |]
  , blip 2 [p| LaneShift RangeD |]
  , blip 4 [p| LaneShift RangeE |]
  , blip 5 [p| LaneShift RangeF |]
  , blip 7 [p| LaneShift RangeG |]
  , blip 9 [p| LaneShift RangeA |]

  , edgeRange [48..59] $ \_i _b -> [p| Note $(boolP _b) (RedYellow $(keyP $ _i - 48)) |]
  , edgeRange [60..71] $ \_i _b -> [p| Note $(boolP _b) (BlueGreen $(keyP $ _i - 60)) |]
  , edge      72       $ \   _b -> [p| Note $(boolP _b) OrangeC                       |]

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
