-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module RockBand.FiveButton where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import RockBand.Common
import RockBand.Parse
import Control.Monad (guard)
import qualified Sound.MIDI.Util as U

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
  = Force StrumHOPO Bool
  | Note (LongNote () Color)
  deriving (Eq, Ord, Show, Read)

data StrumHOPO = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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

instanceMIDIEvent [t| Event |] $

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

  ] ++ noteParser 60 [p| Green |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 61 [p| Red |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 62 [p| Yellow |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 63 [p| Blue |] (\p -> [p| DiffEvent Easy (Note $p) |])
    ++ noteParser 64 [p| Orange |] (\p -> [p| DiffEvent Easy (Note $p) |]) ++
  [ edge 65 $ \_b -> [p| DiffEvent Easy (Force HOPO  $(boolP _b)) |]
  , edge 66 $ \_b -> [p| DiffEvent Easy (Force Strum $(boolP _b)) |]

  ] ++ noteParser 72 [p| Green |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 73 [p| Red |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 74 [p| Yellow |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 75 [p| Blue |] (\p -> [p| DiffEvent Medium (Note $p) |])
    ++ noteParser 76 [p| Orange |] (\p -> [p| DiffEvent Medium (Note $p) |]) ++
  [ edge 77 $ \_b -> [p| DiffEvent Medium (Force HOPO  $(boolP _b)) |]
  , edge 78 $ \_b -> [p| DiffEvent Medium (Force Strum $(boolP _b)) |]

  ] ++ noteParser 84 [p| Green |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 85 [p| Red |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 86 [p| Yellow |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 87 [p| Blue |] (\p -> [p| DiffEvent Hard (Note $p) |])
    ++ noteParser 88 [p| Orange |] (\p -> [p| DiffEvent Hard (Note $p) |]) ++
  [ edge 89 $ \_b -> [p| DiffEvent Hard (Force HOPO  $(boolP _b)) |]
  , edge 90 $ \_b -> [p| DiffEvent Hard (Force Strum $(boolP _b)) |]

  ] ++ noteParser 96 [p| Green |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 97 [p| Red |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 98 [p| Yellow |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 99 [p| Blue |] (\p -> [p| DiffEvent Expert (Note $p) |])
    ++ noteParser 100 [p| Orange |] (\p -> [p| DiffEvent Expert (Note $p) |]) ++
  [ edge 101 $ \_b -> [p| DiffEvent Expert (Force HOPO  $(boolP _b)) |]
  , edge 102 $ \_b -> [p| DiffEvent Expert (Force Strum $(boolP _b)) |]

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

assignKeys :: (NNC.C t) => RTB.T t DiffEvent -> RTB.T t (LongNote StrumHOPO Color)
assignKeys = RTB.mapMaybe $ \case
  Force _ _          -> Nothing
  Note (NoteOff   c) -> Just $ NoteOff      c
  Note (Blip   () c) -> Just $ Blip   Strum c
  Note (NoteOn () c) -> Just $ NoteOn Strum c

assignHOPO :: (NNC.C t) => t -> RTB.T t DiffEvent -> RTB.T t (LongNote StrumHOPO Color)
assignHOPO threshold = RTB.flatten . start . RTB.collectCoincident where
  start = go Nothing False False
  go lastNoteOn forceStrum forceHOPO rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, evs), rtb') -> let
      forceStrum' = or
        [ forceStrum && notElem (Force Strum False) evs
        ,               elem    (Force Strum True ) evs
        ]
      forceHOPO' = or
        [ forceHOPO && notElem (Force HOPO False) evs
        ,              elem    (Force HOPO True ) evs
        ]
      blips    = [ c | Note (Blip   () c) <- evs ]
      fretsOn  = [ c | Note (NoteOn () c) <- evs ]
      fretsOff = [ c | Note (NoteOff   c) <- evs ]
      autoHOPO = case (blips ++ fretsOn, lastNoteOn) of
        ([color], Just (ago, colors)) -> and
          [ color `notElem` colors
          , NNC.add ago dt < threshold -- TODO: should be < or <= ?
          ]
        _ -> False -- chord, or first note of the song
      lastNoteOn' = if null $ blips ++ fretsOn
        then flip fmap lastNoteOn $ \(ago, colors) -> (NNC.add ago dt, colors)
        else Just (NNC.zero, blips ++ fretsOn)
      ntype = if forceStrum' then Strum
        else if forceHOPO' || autoHOPO then HOPO
        else Strum
      theseEvents = map NoteOff fretsOff ++ map (Blip ntype) blips ++ map (NoteOn ntype) fretsOn
      in RTB.cons dt theseEvents $ go lastNoteOn' forceStrum' forceHOPO' rtb'

guitarify :: (Ord s, Ord a) => RTB.T U.Beats (LongNote s a) -> RTB.T U.Beats (LongNote s [a])
guitarify = splitEdges . go . RTB.collectCoincident where
  go rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, xs), rtb') -> let
      notes = xs >>= \case
        NoteOn s c -> [(s, c)]
        Blip   s c -> [(s, c)]
        NoteOff  _ -> []
      len = case RTB.viewL rtb' of
        Nothing              -> error "guitar note interpretation: panic! note with no note-off"
        Just ((dt', xs'), _) -> if any (\case NoteOff _ -> False; _ -> True) xs'
          then dt' NNC.-| (1/8) -- 32nd note gap between notes
          else dt'
      len' = guard (len > 1/3) >> Just len
      -- anything 1/3 beat or less, make into blip.
      -- RB does not do this step, so it produces 16th note sustains on keytar...
      in case notes of
        [] -> RTB.delay dt $ go rtb'
        (ntype, _) : _ -> RTB.cons dt (ntype, map snd notes, len') $ go rtb'
