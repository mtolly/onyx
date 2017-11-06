-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.FiveButton where

import           Data.Bifunctor                   (first)
import           Data.Data
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common
import           RockBand.Parse
import qualified RockBand.PhaseShiftMessage       as PS
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data Color = Green | Red | Yellow | Blue | Orange
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

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
  deriving (Eq, Ord, Show, Read, Typeable, Data)

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
  | Fret59 -- ^ roughly fret 12
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

data DiffEvent
  = Force StrumHOPO Bool
  | TapNotes Bool
  | OpenNotes Bool
  | OnyxClose Int
  | Note (LongNote () Color)
  deriving (Eq, Ord, Show, Read, Typeable, Data)

data StrumHOPO = Strum | HOPO
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

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
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

-- | Controls the strumming animation for a bassist.
data StrumMap
  = StrumMap_Default
  | StrumMap_Pick
  | StrumMap_SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

data OnyxCloseEvent = OnyxCloseEvent Difficulty Int

instance Command OnyxCloseEvent where
  fromCommand (OnyxCloseEvent diff offset) =
    ["onyx", "close", T.toLower $ T.pack $ show diff, T.pack $ show offset]
  toCommand cmd = do
    ["onyx", "close", d, n] <- Just cmd
    diff <- reverseLookup each (T.toLower . T.pack . show) d
    offset <- readMaybe $ T.unpack n
    return $ OnyxCloseEvent diff offset

instanceMIDIEvent [t| Event |] (Just [e| unparseNice (1/8) |]) $

  -- TODO: unknown notes on pitch 12, 13, 15

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

  -- pitch 95 is a shortcut for green + open modifier
  , ( [e| fmap (\((t, ()), rtb) -> (RTB.fromPairList
        [ (t, DiffEvent Expert $ OpenNotes True)
        , (0, DiffEvent Expert $ Note $ Blip () Green)
        , (1/32, DiffEvent Expert $ OpenNotes False)
        ], rtb)) . parseBlipMax (1/3) 95 |]
    , [e| \case {} |]
    )
  , ( [e| many $ parseEdge 95 $ \b ->
        [ DiffEvent Expert $ Note $ (if b then NoteOn () else NoteOff) Green
        , DiffEvent Expert $ OpenNotes b
        ]
      |]
    , [e| \case {} |]
    )
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

  , ( [e| one $ mapParseOne Mood parseCommand |]
    , [e| \case Mood m -> unparseCommand m |]
    )
  , ( [e| one $ mapParseOne HandMap parseCommand |]
    , [e| \case HandMap m -> unparseCommand m |]
    )
  -- TODO:
  -- "[map HandMap NoChords]"
  -- "[map HandMap_Drop_D2]"
  -- "[map handMap_DropD2]"
  -- "map HandMap DropD2]"
  , ( [e| one $ mapParseOne StrumMap parseCommand |]
    , [e| \case StrumMap m -> unparseCommand m |]
    )
  -- TODO: "[map HandMap_Pick]"

  , ( [e| one $ mapParseOne (\(OnyxCloseEvent d o) -> DiffEvent d $ OnyxClose o) parseCommand |]
    , [e| \case DiffEvent d (OnyxClose o) -> unparseCommand $ OnyxCloseEvent d o |]
    )

  , ( [e| many $ flip filterParseOne PS.parsePSMessage $ \case
        PS.PSMessage mdiff PS.OpenStrum b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ OpenNotes b) diffs
        PS.PSMessage mdiff PS.TapNotes b -> Just $ let
          diffs = maybe [minBound .. maxBound] (: []) mdiff
          in map (\diff -> DiffEvent diff $ TapNotes b) diffs
        _ -> Nothing
      |]
    , [e| \case
        DiffEvent d (OpenNotes b) -> unparseOne $ PS.PSMessage (Just d) PS.OpenStrum b
        DiffEvent d (TapNotes  b) -> unparseOne $ PS.PSMessage (Just d) PS.TapNotes b
      |]
    )

  ]

copyExpert :: (NNC.C t) => RTB.T t Event -> RTB.T t Event
copyExpert = baseCopyExpert DiffEvent $ \case
  DiffEvent d e -> Just (d, e)
  _             -> Nothing

assignKeys :: (NNC.C t) => RTB.T t DiffEvent -> RTB.T t (LongNote StrumHOPO Color)
assignKeys = RTB.mapMaybe $ \case
  Note note -> Just $ first (const Strum) note
  _ -> Nothing

eachDifficulty :: (NNC.C t) => (RTB.T t DiffEvent -> RTB.T t DiffEvent) -> RTB.T t Event -> RTB.T t Event
eachDifficulty f evts = let
  getDiffEvent diff = \case
    DiffEvent d e | d == diff -> Just e
    _                         -> Nothing
  (expert, notExpert) = RTB.partitionMaybe (getDiffEvent Expert) evts
  (hard  , notHard  ) = RTB.partitionMaybe (getDiffEvent Hard  ) notExpert
  (medium, notMedium) = RTB.partitionMaybe (getDiffEvent Medium) notHard
  (easy  , notEasy  ) = RTB.partitionMaybe (getDiffEvent Easy  ) notMedium
  in foldr RTB.merge RTB.empty
    [ DiffEvent Expert <$> f expert
    , DiffEvent Hard   <$> f hard
    , DiffEvent Medium <$> f medium
    , DiffEvent Easy   <$> f easy
    , notEasy
    ]

unparseNice :: U.Beats -> RTB.T U.Beats Event -> RTB.T U.Beats E.T
unparseNice defLength = U.trackJoin . fmap unparseOne . showBlipsNice defLength

showBlipsNice :: (NNC.C t) => t -> RTB.T t Event -> RTB.T t Event
showBlipsNice defLength evts = let
  getDiffNotes diff = \case
    DiffEvent d (Note ln) | d == diff -> Just ln
    _                                 -> Nothing
  (expert, notExpert) = RTB.partitionMaybe (getDiffNotes Expert) evts
  (hard  , notHard  ) = RTB.partitionMaybe (getDiffNotes Hard  ) notExpert
  (medium, notMedium) = RTB.partitionMaybe (getDiffNotes Medium) notHard
  (easy  , notEasy  ) = RTB.partitionMaybe (getDiffNotes Easy  ) notMedium
  in foldr RTB.merge RTB.empty
    [ DiffEvent Expert . Note <$> showEdgesNice' defLength expert
    , DiffEvent Hard   . Note <$> showEdgesNice' defLength hard
    , DiffEvent Medium . Note <$> showEdgesNice' defLength medium
    , DiffEvent Easy   . Note <$> showEdgesNice' defLength easy
    , notEasy
    ]
