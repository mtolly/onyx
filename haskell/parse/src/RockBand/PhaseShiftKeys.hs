-- | Phase Shift's Real Keys (full keyboard) mode.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.PhaseShiftKeys
( Event(..)
, Hand(..)
) where

import RockBand.Drums (Hand(..))
import RockBand.Parse
import qualified Numeric.NonNegative.Class as NNC
import Control.Monad (guard)
import RockBand.Common
import qualified Sound.MIDI.File.Event as E
import qualified Data.EventList.Relative.TimeBody as RTB
import Data.Maybe (isJust)

data Event
  = LaneShift  Hand Int
  | Trainer    Trainer
  | Mood       Mood
  | Solo            Bool -- ^ A keyboard solo section.
  | Glissando  Hand Bool -- ^ Place over a sequence of white notes for a freeform section.
  | Trill      Hand Bool -- ^ Fill lanes on two keys.
  | Overdrive       Bool -- ^ An energy phrase.
  | BRE             Bool -- ^ Fill lanes for a Big Rock Ending.
  | Note       Hand Bool Int
  deriving (Eq, Ord, Show)

parseRange :: (NNC.C t) => Int -> Int -> Hand -> ParseOne t E.T Event
parseRange pitch chan hand rtb = do
  ((t, (c, p, v)), rtb') <- parseBlipCPV rtb
  guard $ p == pitch
  guard $ c == chan
  return ((t, LaneShift hand v), rtb')

parseNote :: (NNC.C t) => ParseOne t E.T Event
parseNote rtb = do
  ((t, (c, p, v)), rtb') <- firstEventWhich isNoteEdgeCPV rtb
  guard $ 21 <= p && p <= 108
  hand <- case c of
    0 -> Just RH
    1 -> Just LH
    _ -> Nothing
  return ((t, Note hand (isJust v) p), rtb')

instanceMIDIEvent [t| Event |]

  [ ( [e| parseRange 12 1 LH |]
    , [e| \case LaneShift LH p -> unparseBlipCPV (1, 12, p) |]
    )
  , ( [e| parseRange 13 0 RH |]
    , [e| \case LaneShift RH p -> unparseBlipCPV (0, 13, p) |]
    )

  , ( [e| parseNote |]
    , [e| \case
      Note RH b p -> RTB.singleton 0 $ makeEdgeCPV 0 p $ guard b >> Just 96
      Note LH b p -> RTB.singleton 0 $ makeEdgeCPV 1 p $ guard b >> Just 96
      |]
    )

  , edge 115 $ applyB [p| Solo |]
  , edge 116 $ applyB [p| Overdrive |]
  , edge 120 $ applyB [p| BRE |]
  , edge 124 $ applyB [p| Glissando LH |] -- proposed
  , edge 125 $ applyB [p| Trill     LH |] -- proposed
  , edge 126 $ applyB [p| Glissando RH |]
  , edge 127 $ applyB [p| Trill     RH |]

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
