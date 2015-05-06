{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.FiveButton where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import Parser.Base

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Event
  = ForceHOPO Difficulty Bool
  | ForceStrum Difficulty Bool
  | Overdrive Bool
  | Solo Bool
  | Note Difficulty Color Bool
  deriving (Eq, Ord, Show, Read)

readEvent :: E.T -> Maybe [Event]
readEvent (MIDINote p b) = case V.fromPitch p of
  60 -> one $ Note Easy Green b
  61 -> one $ Note Easy Red b
  62 -> one $ Note Easy Yellow b
  63 -> one $ Note Easy Blue b
  64 -> one $ Note Easy Orange b
  65 -> one $ ForceHOPO Easy b
  66 -> one $ ForceStrum Easy b

  72 -> one $ Note Medium Green b
  73 -> one $ Note Medium Red b
  74 -> one $ Note Medium Yellow b
  75 -> one $ Note Medium Blue b
  76 -> one $ Note Medium Orange b
  77 -> one $ ForceHOPO Medium b
  78 -> one $ ForceStrum Medium b

  84 -> one $ Note Hard Green b
  85 -> one $ Note Hard Red b
  86 -> one $ Note Hard Yellow b
  87 -> one $ Note Hard Blue b
  88 -> one $ Note Hard Orange b
  89 -> one $ ForceHOPO Hard b
  90 -> one $ ForceStrum Hard b

  96 -> one $ Note Expert Green b
  97 -> one $ Note Expert Red b
  98 -> one $ Note Expert Yellow b
  99 -> one $ Note Expert Blue b
  100 -> one $ Note Expert Orange b
  101 -> one $ ForceHOPO Expert b
  102 -> one $ ForceStrum Expert b

  103 -> one $ Solo b
  116 -> one $ Overdrive b
  _ -> Nothing
  where one x = Just [x]
readEvent _ = Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  ForceHOPO  d b -> one $ edge' (60 + 12 * fromEnum d + 5) b
  ForceStrum d b -> one $ edge' (60 + 12 * fromEnum d + 6) b
  Note     d c b -> one $ edge' (60 + 12 * fromEnum d + fromEnum c) b
  Overdrive    b -> one $ edge' 116 b
  Solo         b -> one $ edge' 103 b
  where one = RTB.singleton 0
