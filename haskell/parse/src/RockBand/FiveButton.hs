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
  = Mood                      Mood
  | HandMap                HandMap
  | StrumMap              StrumMap
  | FretPosition FretPosition Bool
  | Tremolo                   Bool
  | Trill                     Bool
  | Overdrive                 Bool
  | BRE                       Bool
  | Solo                      Bool
  | Player1                   Bool
  | Player2                   Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

-- | These don't actually correspond to 20 different frets;
-- see http://i.imgur.com/fRg6Vo9.png by Orange Harrison
data FretPosition
  = Fret40 -- ^ the nut
  | Fret41
  | Fret42
  | Fret43
  | Fret44
  | Fret45
  | Fret46
  | Fret47
  | Fret48
  | Fret49
  | Fret50
  | Fret51
  | Fret52
  | Fret53
  | Fret54
  | Fret55
  | Fret56
  | Fret57
  | Fret58
  | Fret59 -- ^ roughly fret 13
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data DiffEvent
  = ForceHOPO  Bool
  | ForceStrum Bool
  | Note       Bool Color
  deriving (Eq, Ord, Show, Read)

-- | Controls the fretting hand animation of a guitarist/bassist.
data HandMap
  = HandMap_Default
  -- ^ Normal fingering. Single gems = single fingers, gems with duration =
  -- vibrato, chord gems = chords.
  | HandMap_NoChords  -- ^ All single fingers/vibrato.
  | HandMap_AllChords -- ^ All chords.
  | HandMap_Solo      -- ^ D major shape for all chords, vibrato for all chord sustains.
  | HandMap_DropD     -- ^ Open hand for all green gems, all other gems are chords.
  | HandMap_DropD2    -- ^ Open hand for all green gems.
  | HandMap_AllBend   -- ^ All ring finger high vibrato.
  | HandMap_Chord_C   -- ^ All C chord shape.
  | HandMap_Chord_D   -- ^ All D chord shape.
  | HandMap_Chord_A   -- ^ All A minor chord shape.
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command HandMap where
  fromCommand hm = ["map", show hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumMap_Default
  | StrumMap_Pick
  | StrumMap_SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command StrumMap where
  fromCommand sm = ["map", show sm]
  toCommand = reverseLookup each fromCommand

instanceMIDIEvent [t| Event |]

  [ edge 40 $ applyB [p| FretPosition Fret40 |]
  , edge 41 $ applyB [p| FretPosition Fret41 |]
  , edge 42 $ applyB [p| FretPosition Fret42 |]
  , edge 43 $ applyB [p| FretPosition Fret43 |]
  , edge 44 $ applyB [p| FretPosition Fret44 |]
  , edge 45 $ applyB [p| FretPosition Fret45 |]
  , edge 46 $ applyB [p| FretPosition Fret46 |]
  , edge 47 $ applyB [p| FretPosition Fret47 |]
  , edge 48 $ applyB [p| FretPosition Fret48 |]
  , edge 49 $ applyB [p| FretPosition Fret49 |]
  , edge 50 $ applyB [p| FretPosition Fret50 |]
  , edge 51 $ applyB [p| FretPosition Fret51 |]
  , edge 52 $ applyB [p| FretPosition Fret52 |]
  , edge 53 $ applyB [p| FretPosition Fret53 |]
  , edge 54 $ applyB [p| FretPosition Fret54 |]
  , edge 55 $ applyB [p| FretPosition Fret55 |]
  , edge 56 $ applyB [p| FretPosition Fret56 |]
  , edge 57 $ applyB [p| FretPosition Fret57 |]
  , edge 58 $ applyB [p| FretPosition Fret58 |]
  , edge 59 $ applyB [p| FretPosition Fret59 |]

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

data AssignedNote
  = NoteOff Color
  | Strum Color
  | HOPO Color
  deriving (Eq, Ord, Show, Read)

assignHOPO :: (NNC.C t) => t -> RTB.T t DiffEvent -> RTB.T t AssignedNote
assignHOPO threshold = RTB.flatten . start . RTB.collectCoincident where
  start = go Nothing False False
  go lastNoteOn forceStrum forceHOPO rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evs), rtb') -> let
      forceStrum' = or
        [ forceStrum && notElem (ForceStrum False) evs
        ,               elem    (ForceStrum True ) evs
        ]
      forceHOPO' = or
        [ forceHOPO && notElem (ForceHOPO False) evs
        ,              elem    (ForceHOPO True ) evs
        ]
      fretsOn  = [ color | Note True  color <- evs ]
      fretsOff = [ color | Note False color <- evs ]
      autoHOPO = case (fretsOn, lastNoteOn) of
        ([color], Just (ago, colors)) -> and
          [ color `notElem` colors
          , NNC.add ago dt < threshold -- TODO: should be < or <= ?
          ]
        _ -> False -- chord, or first note of the song
      lastNoteOn' = if null fretsOn
        then flip fmap lastNoteOn $ \(ago, colors) -> (NNC.add ago dt, colors)
        else Just (NNC.zero, fretsOn)
      makeNoteOn = if forceStrum' then Strum
        else if forceHOPO' || autoHOPO then HOPO
        else Strum
      theseEvents = map NoteOff fretsOff ++ map makeNoteOn fretsOn
      in RTB.cons dt theseEvents $ go lastNoteOn' forceStrum' forceHOPO' rtb'
