-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.FiveButton where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import RockBand.Common
import RockBand.Parse

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Event
  = Mood             Mood
  | HandMap       HandMap
  | StrumMap     StrumMap
  | FretPosition Int Bool -- ^ Int is a MIDI pitch; can be 40 to 59
  | Tremolo          Bool
  | Trill            Bool
  | Overdrive        Bool
  | BRE              Bool
  | Solo             Bool
  | Player1          Bool
  | Player2          Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = ForceHOPO  Bool
  | ForceStrum Bool
  | Note       Bool Color
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
  NoChords    -> "HandMap_NoChords"
  AllChords   -> "HandMap_AllChords"
  HandSolo    -> "HandMap_Solo"
  DropD       -> "HandMap_DropD"
  DropD2      -> "HandMap_DropD2"
  AllBend     -> "HandMap_AllBend"
  ChordC      -> "HandMap_Chord_C"
  ChordD      -> "HandMap_Chord_D"
  ChordA      -> "HandMap_Chord_A"

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
  Pick         -> "StrumMap_Pick"
  SlapBass     -> "StrumMap_SlapBass"

instance Command StrumMap where
  fromCommand sm = ["map", strumMapName sm]
  toCommand = reverseLookup each fromCommand

instanceMIDIEvent [t| Event |]

  [ ( [e| firstEventWhich $ \e -> isNoteEdge e >>= \case
        (i, b) | 40 <= i && i <= 59 -> Just $ FretPosition i b
        _                           -> Nothing
      |]
    , [e| \case FretPosition i b -> RTB.singleton NNC.zero $ makeEdge i b |]
    )

  , edge 60 $ \_b -> [p| DiffEvent Easy (Note $(boolP _b) Green ) |]
  , edge 61 $ \_b -> [p| DiffEvent Easy (Note $(boolP _b) Red   ) |]
  , edge 62 $ \_b -> [p| DiffEvent Easy (Note $(boolP _b) Yellow) |]
  , edge 63 $ \_b -> [p| DiffEvent Easy (Note $(boolP _b) Blue  ) |]
  , edge 64 $ \_b -> [p| DiffEvent Easy (Note $(boolP _b) Orange) |]
  , edge 65 $ \_b -> [p| DiffEvent Easy (ForceHOPO  $(boolP _b)) |]
  , edge 66 $ \_b -> [p| DiffEvent Easy (ForceStrum $(boolP _b)) |]

  , edge 72 $ \_b -> [p| DiffEvent Medium (Note $(boolP _b) Green ) |]
  , edge 73 $ \_b -> [p| DiffEvent Medium (Note $(boolP _b) Red   ) |]
  , edge 74 $ \_b -> [p| DiffEvent Medium (Note $(boolP _b) Yellow) |]
  , edge 75 $ \_b -> [p| DiffEvent Medium (Note $(boolP _b) Blue  ) |]
  , edge 76 $ \_b -> [p| DiffEvent Medium (Note $(boolP _b) Orange) |]
  , edge 77 $ \_b -> [p| DiffEvent Medium (ForceHOPO  $(boolP _b)) |]
  , edge 78 $ \_b -> [p| DiffEvent Medium (ForceStrum $(boolP _b)) |]

  , edge 84 $ \_b -> [p| DiffEvent Hard (Note $(boolP _b) Green ) |]
  , edge 85 $ \_b -> [p| DiffEvent Hard (Note $(boolP _b) Red   ) |]
  , edge 86 $ \_b -> [p| DiffEvent Hard (Note $(boolP _b) Yellow) |]
  , edge 87 $ \_b -> [p| DiffEvent Hard (Note $(boolP _b) Blue  ) |]
  , edge 88 $ \_b -> [p| DiffEvent Hard (Note $(boolP _b) Orange) |]
  , edge 89 $ \_b -> [p| DiffEvent Hard (ForceHOPO  $(boolP _b)) |]
  , edge 90 $ \_b -> [p| DiffEvent Hard (ForceStrum $(boolP _b)) |]

  , edge 96  $ \_b -> [p| DiffEvent Expert (Note $(boolP _b) Green ) |]
  , edge 97  $ \_b -> [p| DiffEvent Expert (Note $(boolP _b) Red   ) |]
  , edge 98  $ \_b -> [p| DiffEvent Expert (Note $(boolP _b) Yellow) |]
  , edge 99  $ \_b -> [p| DiffEvent Expert (Note $(boolP _b) Blue  ) |]
  , edge 100 $ \_b -> [p| DiffEvent Expert (Note $(boolP _b) Orange) |]
  , edge 101 $ \_b -> [p| DiffEvent Expert (ForceHOPO  $(boolP _b)) |]
  , edge 102 $ \_b -> [p| DiffEvent Expert (ForceStrum $(boolP _b)) |]

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
  -- TODO:
  -- "[map HandMap NoChords]"
  -- "[map HandMap_Drop_D2]"
  -- "[map handMap_DropD2]"
  -- "map HandMap DropD2]"
  , ( [e| mapParseOne StrumMap parseCommand |]
    , [e| \case StrumMap m -> unparseCommand m |]
    )
  -- TODO: "[map HandMap_Pick]"
  ]

copyExpert :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
copyExpert = baseCopyExpert DiffEvent $ \case
  DiffEvent d e -> Just (d, e)
  _             -> Nothing
