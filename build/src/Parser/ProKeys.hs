-- | The contents of the \"PART REAL_KEYS_?\" and \"PART KEYS_ANIM_?H\" tracks.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.ProKeys where

import Parser.Base
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Parser.TH
import Language.Haskell.TH

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
  | Note Int Bool -- ^ Valid pitches are in MIDI range 48 to 72.
  deriving (Eq, Ord, Show)

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = C | D | E | F | G | A
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

rosetta :: (Q Exp, Q Exp)
rosetta = translation
  [ blip 0 [p| LaneShift C |]
  , blip 2 [p| LaneShift D |]
  , blip 4 [p| LaneShift E |]
  , blip 5 [p| LaneShift F |]
  , blip 7 [p| LaneShift G |]
  , blip 9 [p| LaneShift A |]
  , ( [e| U.extractFirst $ \e -> case isNoteEdge e of
        Just (i, b) | 48 <= i && i <= 72 -> Just $ Note i b
        _ -> Nothing
      |]
    , [e| \case Note i b -> RTB.singleton NNC.zero $ makeEdge i b |]
    )
  , edge 115 $ applyB [p| Solo |]
  , edge 116 $ applyB [p| Overdrive |]
  , edge 120 $ applyB [p| BRE |]
  , edge 126 $ applyB [p| Glissando |]
  , edge 127 $ applyB [p| Trill |]
  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| U.extractFirst $ \e -> readCommand' e >>= \case
        (t, "key") -> Just $ Trainer t
        _          -> Nothing
      |]
    , [e| \case Trainer t -> RTB.singleton NNC.zero $ showCommand' (t, "key") |]
    )
  ]
