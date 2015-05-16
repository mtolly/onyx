-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.FiveButton where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Parser.Base
import Language.Haskell.TH
import Parser.TH

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Event
  = Mood Mood
  | ForceHOPO Difficulty Bool
  | ForceStrum Difficulty Bool
  | Tremolo Bool
  | Trill Bool
  | Overdrive Bool
  | BRE Bool
  | Solo Bool
  | Note Difficulty Color Bool
  | HandMap HandMap
  | StrumMap StrumMap
  | FretPosition Int Bool -- ^ Int is a MIDI pitch; can be 40 to 59
  | Player1 Bool
  | Player2 Bool
  deriving (Eq, Ord, Show, Read)

-- | Controls the fretting hand animation of a guitarist/bassist.
data HandMap
  -- | Normal fingering. Single gems = single fingers, gems with duration =
  -- vibrato, chord gems = chords.
  = HandDefault
  | NoChords -- ^ All single fingers/vibrato.
  | AllChords -- ^ All chords.
  | HandSolo -- ^ D major shape for all chords, vibrato for all chord sustains.
  | DropD -- ^ Open hand for all green gems, all other gems are chords.
  | DropD2 -- ^ Open hand for all green gems.
  | AllBend -- ^ All ring finger high vibrato.
  | ChordC -- ^ All C chord shape.
  | ChordD -- ^ All D chord shape.
  | ChordA -- ^ All A minor chord shape.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

handMapName :: HandMap -> String
handMapName = \case
  HandDefault -> "HandMap_Default"
  NoChords -> "HandMap_NoChords"
  AllChords -> "HandMap_AllChords"
  HandSolo -> "HandMap_Solo"
  DropD -> "HandMap_DropD"
  DropD2 -> "HandMap_DropD2"
  AllBend -> "HandMap_AllBend"
  ChordC -> "HandMap_Chord_C"
  ChordD -> "HandMap_Chord_D"
  ChordA -> "HandMap_Chord_A"

instance Command HandMap where
  fromCommand hm = ["map", handMapName hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumDefault
  | Pick
  | SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

strumMapName :: StrumMap -> String
strumMapName = \case
  StrumDefault -> "StrumMap_Default"
  Pick -> "StrumMap_Pick"
  SlapBass -> "StrumMap_SlapBass"

instance Command StrumMap where
  fromCommand sm = ["map", strumMapName sm]
  toCommand = reverseLookup each fromCommand

rosetta :: (Q Exp, Q Exp)
rosetta = translation

  [ ( [e| firstEventWhich $ \e -> isNoteEdge e >>= \case
        (i, b) | 40 <= i && i <= 59 -> Just $ FretPosition i b
        _                           -> Nothing
      |]
    , [e| \case FretPosition i b -> RTB.singleton NNC.zero $ makeEdge i b |]
    )

  , edge 60 $ applyB [p| Note Easy Green |]
  , edge 61 $ applyB [p| Note Easy Red |]
  , edge 62 $ applyB [p| Note Easy Yellow |]
  , edge 63 $ applyB [p| Note Easy Blue |]
  , edge 64 $ applyB [p| Note Easy Orange |]
  , edge 65 $ applyB [p| ForceHOPO Easy |]
  , edge 66 $ applyB [p| ForceStrum Easy |]

  , edge 72 $ applyB [p| Note Medium Green |]
  , edge 73 $ applyB [p| Note Medium Red |]
  , edge 74 $ applyB [p| Note Medium Yellow |]
  , edge 75 $ applyB [p| Note Medium Blue |]
  , edge 76 $ applyB [p| Note Medium Orange |]
  , edge 77 $ applyB [p| ForceHOPO Medium |]
  , edge 78 $ applyB [p| ForceStrum Medium |]

  , edge 84 $ applyB [p| Note Hard Green |]
  , edge 85 $ applyB [p| Note Hard Red |]
  , edge 86 $ applyB [p| Note Hard Yellow |]
  , edge 87 $ applyB [p| Note Hard Blue |]
  , edge 88 $ applyB [p| Note Hard Orange |]
  , edge 89 $ applyB [p| ForceHOPO Hard |]
  , edge 90 $ applyB [p| ForceStrum Hard |]

  , edge 96  $ applyB [p| Note Expert Green |]
  , edge 97  $ applyB [p| Note Expert Red |]
  , edge 98  $ applyB [p| Note Expert Yellow |]
  , edge 99  $ applyB [p| Note Expert Blue |]
  , edge 100 $ applyB [p| Note Expert Orange |]
  , edge 101 $ applyB [p| ForceHOPO Expert |]
  , edge 102 $ applyB [p| ForceStrum Expert |]

  , edge 103 $ applyB [p| Solo |]
  , edge 105 $ applyB [p| Player1 |]
  , edge 106 $ applyB [p| Player2 |]
  , edge 116 $ applyB [p| Overdrive |]
  , edges [120 .. 124] $ applyB [p| BRE |]
  , edge 126 $ applyB [p| Tremolo |]
  , edge 127 $ applyB [p| Trill |]

  , ( [e| mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| mapParseOne HandMap parseCommand |]
    , [e| \case HandMap m -> unparseCommand m |]
    )
  , ( [e| mapParseOne StrumMap parseCommand |]
    , [e| \case StrumMap m -> unparseCommand m |]
    )
  ]
