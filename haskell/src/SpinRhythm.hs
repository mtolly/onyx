{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
module SpinRhythm where

import           Control.Monad.Codec
import qualified Data.Aeson                       as A
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import           JSONData
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Drums             (Hand (..))
import           RockBand.Common

{-

TYPES:
  0 = match note
    uses colorIndex and column
  1 = beat note
    colorIndex and column can be anything, they don't matter
  2 = spin to the right (magenta)
    colorIndex and column can be anything, they don't matter
  3 = spin to the left (cyan)
    colorIndex and column can be anything, they don't matter
  4 = start slide
    this also produces the tap note
    uses colorIndex and column
  5 = continue slide
    colorIndex should probably match the slide
    uses column
    m_size produces horizontal movements in the slide:
      it's equal to the number of columns to move to the left
      (so movement to the right is a negative number?)
      the movement happens at the next event?
  6 unused?
  7 unused?
  8 = tap note
    uses colorIndex and column

COLUMNS:
  3 is far left
  2
  1
  0 is center
  -1
  -2
  -3 is far right

COLORS:
  0 blue
  1 red

-}

-- | The data stored in string val under SO_TrackData_asset key
data TrackData = TrackData
  { td_customName                :: T.Text
  , td_previewLoopBars           :: MinMax Int
  , td_goBeatOffsetFromFirstNote :: Double
  , td_scoreMedalThresholds      :: [Int]
  , td_difficultyType            :: Int
  , td_meshTrails                :: [()] -- TODO
  , td_initialWheelAngle         :: Double
  , td_isTutorial                :: Bool
  , td_tutorialTitleTranslation  :: Translation
  , td_clipInfoAssetReferences   :: [AssetReference]
  , td_background                :: AssetReference
  , td_tutorialObjects           :: [()] -- TODO
  , td_tutorialTexts             :: [()] -- TODO
  , td_clipData                  :: [ClipData]
  , td_notes                     :: [Note Double]
  , td_events                    :: [()] -- TODO
  , td_rewindSections            :: [()] -- TODO
  , td_lastEditedOnDate          :: T.Text
  , td_noteStats                 :: NoteStats
  } deriving (Eq, Show)

instance StackJSON TrackData where
  stackJSON = asObject "TrackData" $ do
    let unitList = listCodec $ Codec (return ()) (makeOut $ \() -> "unknown")
    td_customName                <- td_customName                =. req "customName"                stackJSON
    td_previewLoopBars           <- td_previewLoopBars           =. req "previewLoopBars"           stackJSON
    td_goBeatOffsetFromFirstNote <- td_goBeatOffsetFromFirstNote =. req "goBeatOffsetFromFirstNote" stackJSON
    td_scoreMedalThresholds      <- td_scoreMedalThresholds      =. req "scoreMedalThresholds"      stackJSON
    td_difficultyType            <- td_difficultyType            =. req "difficultyType"            stackJSON
    td_meshTrails                <- td_meshTrails                =. req "meshTrails"                unitList
    td_initialWheelAngle         <- td_initialWheelAngle         =. req "initialWheelAngle"         stackJSON
    td_isTutorial                <- td_isTutorial                =. req "isTutorial"                stackJSON
    td_tutorialTitleTranslation  <- td_tutorialTitleTranslation  =. req "tutorialTitleTranslation"  stackJSON
    td_clipInfoAssetReferences   <- td_clipInfoAssetReferences   =. req "clipInfoAssetReferences"   stackJSON
    td_background                <- td_background                =. req "background"                stackJSON
    td_tutorialObjects           <- td_tutorialObjects           =. req "tutorialObjects"           unitList
    td_tutorialTexts             <- td_tutorialTexts             =. req "tutorialTexts"             unitList
    td_clipData                  <- td_clipData                  =. req "clipData"                  stackJSON
    td_notes                     <- td_notes                     =. req "notes"                     stackJSON
    td_events                    <- td_events                    =. req "events"                    unitList
    td_rewindSections            <- td_rewindSections            =. req "rewindSections"            unitList
    td_lastEditedOnDate          <- td_lastEditedOnDate          =. req "lastEditedOnDate"          stackJSON
    td_noteStats                 <- td_noteStats                 =. req "noteStats"                 stackJSON
    return TrackData{..}

data MinMax a = MinMax
  { mm_min :: a
  , mm_max :: a
  } deriving (Eq, Show)

instance (StackJSON a) => StackJSON (MinMax a) where
  stackJSON = asObject "MinMax" $ do
    mm_min <- mm_min =. req "min" stackJSON
    mm_max <- mm_max =. req "max" stackJSON
    return MinMax{..}

data Translation = Translation
  { trans_key :: T.Text
  } deriving (Eq, Show)

instance StackJSON Translation where
  stackJSON = asObject "Translation" $ do
    trans_key <- trans_key =. req "key" stackJSON
    return Translation{..}

data AssetReference = AssetReference
  { ar_bundle    :: T.Text
  , ar_assetName :: T.Text
  } deriving (Eq, Show)

instance StackJSON AssetReference where
  stackJSON = asObject "AssetReference" $ do
    ar_bundle    <- ar_bundle    =. req "bundle"    stackJSON
    ar_assetName <- ar_assetName =. req "assetName" stackJSON
    return AssetReference{..}

data ClipData = ClipData
  { cd_clipIndex           :: Int
  , cd_startBar            :: Int
  , cd_endBar              :: Int
  , cd_transitionIn        :: Int
  , cd_transitionInValue   :: Double
  , cd_transitionInOffset  :: Double
  , cd_transitionOut       :: Int
  , cd_transitionOutValue  :: Double
  , cd_transitionOutOffset :: Double
  } deriving (Eq, Show)

instance StackJSON ClipData where
  stackJSON = asObject "ClipData" $ do
    cd_clipIndex           <- cd_clipIndex           =. req "clipIndex"           stackJSON
    cd_startBar            <- cd_startBar            =. req "startBar"            stackJSON
    cd_endBar              <- cd_endBar              =. req "endBar"              stackJSON
    cd_transitionIn        <- cd_transitionIn        =. req "transitionIn"        stackJSON
    cd_transitionInValue   <- cd_transitionInValue   =. req "transitionInValue"   stackJSON
    cd_transitionInOffset  <- cd_transitionInOffset  =. req "transitionInOffset"  stackJSON
    cd_transitionOut       <- cd_transitionOut       =. req "transitionOut"       stackJSON
    cd_transitionOutValue  <- cd_transitionOutValue  =. req "transitionOutValue"  stackJSON
    cd_transitionOutOffset <- cd_transitionOutOffset =. req "transitionOutOffset" stackJSON
    return ClipData{..}

data Note t = Note
  { note_time       :: t
  , note_type       :: Int
  , note_colorIndex :: Int
  , note_column     :: Int
  , note_m_size     :: Int
  } deriving (Eq, Ord, Show, Functor)

instance (StackJSON t) => StackJSON (Note t) where
  stackJSON = asObject "Note" $ do
    note_time       <- note_time       =. req "time"       stackJSON
    note_type       <- note_type       =. req "type"       stackJSON
    note_colorIndex <- note_colorIndex =. req "colorIndex" stackJSON
    note_column     <- note_column     =. req "column"     stackJSON
    note_m_size     <- note_m_size     =. req "m_size"     stackJSON
    return Note{..}

data NoteStats = NoteStats
  { ns_matchNotes  :: Int
  , ns_tapNotes    :: Int
  , ns_beatNotes   :: Int
  , ns_spinNotes   :: Int
  , ns_holdNotes   :: Int
  , ns_matchPerMin :: Double
  , ns_tapPerMin   :: Double
  , ns_beatsPerMin :: Double
  , ns_spinPerMin  :: Double
  , ns_holdPerMin  :: Double
  } deriving (Eq, Show)

instance StackJSON NoteStats where
  stackJSON = asObject "NoteStats" $ do
    ns_matchNotes  <- ns_matchNotes  =. req "matchNotes"  stackJSON
    ns_tapNotes    <- ns_tapNotes    =. req "tapNotes"    stackJSON
    ns_beatNotes   <- ns_beatNotes   =. req "beatNotes"   stackJSON
    ns_spinNotes   <- ns_spinNotes   =. req "spinNotes"   stackJSON
    ns_holdNotes   <- ns_holdNotes   =. req "holdNotes"   stackJSON
    ns_matchPerMin <- ns_matchPerMin =. req "matchPerMin" stackJSON
    ns_tapPerMin   <- ns_tapPerMin   =. req "tapPerMin"   stackJSON
    ns_beatsPerMin <- ns_beatsPerMin =. req "beatsPerMin" stackJSON
    ns_spinPerMin  <- ns_spinPerMin  =. req "spinPerMin"  stackJSON
    ns_holdPerMin  <- ns_holdPerMin  =. req "holdPerMin"  stackJSON
    return NoteStats{..}

-- | The data stored in string val under SO_TrackDataAssetReference_assetRef key
data TrackDataAssetReference = TrackDataAssetReference
  { tdar_bundle     :: T.Text
  , tdar_assetName  :: T.Text
  , tdar_difficulty :: Int
  } deriving (Eq, Show)

instance StackJSON TrackDataAssetReference where
  stackJSON = asObject "TrackDataAssetReference" $ do
    tdar_bundle     <- tdar_bundle     =. req "bundle"      stackJSON
    tdar_assetName  <- tdar_assetName  =. req "assetName"   stackJSON
    tdar_difficulty <- tdar_difficulty =. req "_difficulty" stackJSON
    return TrackDataAssetReference{..}

makeTrackFile :: TrackData -> TrackDataAssetReference -> A.Value
makeTrackFile tdata tdar = A.object
  [ (,) "unityObjectValuesContainer" $ A.object
    [ (,) "values" $ A.Array $ V.fromList
      [ A.object
        [ ("key", "asset")
        , ("jsonKey", "SO_TrackData_asset")
        , ("fullType", "TrackData")
        ]
      ]
    ]
  , (,) "objectValuesContainer" $ A.object
    [ (,) "values" $ A.Array $ V.fromList
      [ A.object
        [ ("key", "assetRef")
        , ("jsonKey", "SO_TrackDataAssetReference_assetRef")
        , ("fullType", "TrackDataAssetReference")
        ]
      ]
    ]
  , (,) "largeStringValuesContainer" $ A.object
    [ (,) "values" $ A.Array $ V.fromList
      [ A.object
        [ ("key", "SO_TrackData_asset")
        , ("val", A.String $ T.pack $ show $ toJSON tdata)
        , ("loadedGenerationId", A.Number 1)
        ]
      , A.object
        [ ("key", "SO_TrackDataAssetReference_assetRef")
        , ("val", A.String $ T.pack $ show $ toJSON tdar)
        , ("loadedGenerationId", A.Number 1)
        ]
      ]
    ]
  ]

{-

UnityFS file (song_2minutes after decompression):
(numbers big endian)

55 6E 69 74 79 46 53 -- "UnityFS"
00 00 00 00
06 35 2E 78 2E 78 00 -- length? then "5.x.x"
32 30 31 39 2E 31 2E 31 31 66 31 00 -- "2019.1.11f1"
00 00 00 00
00 6A 3B 73 -- song_2minutes file size
00 00 00 9D
00 00 00 9D
00 00 00 40
00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
00 00 00 01
00 6A 3A A4 -- sum of the two file sizes
00 6A 3A A4 -- sum of the two file sizes (the 2 are probably compressed vs uncompressed sizes)
00 40
00 00 00 02 -- probably file count

00 00 00 00
00 00 00 00 -- probably offset within data chunk (0 for first file)
00 00 00 00
00 00 22 64 -- size of the MonoResource file
00 00 00 04
43 41 42 2D 35 61 32 31 39 66 63 30 66 63 63 65 30 33 30 37 65 36 30 62 36 33 62 30 33 36 31 65 35 38 36 64 00
  -- "CAB-5a219fc0fcce0307e60b63b0361e586d" name of MonoResource file

00 00 00 00
00 00 22 64 -- size of the MonoResource file, probably offset of audio start within data
00 00 00 00
00 6A 18 40 -- size of the audio file
00 00 00 00
43 41 42 2D 35 61 32 31 39 66 63 30 66 63 63 65 30 33 30 37 65 36 30 62 36 33 62 30 33 36 31 65 35 38 36 64 2E 72 65 73 6F 75 72 63 65 00
  -- "CAB-5a219fc0fcce0307e60b63b0361e586d.resource" name of audio file

(contents of CAB-5a219fc0fcce0307e60b63b0361e586d)
(contents of CAB-5a219fc0fcce0307e60b63b0361e586d.resource)

-}

data Column
  = ColumnL4
  | ColumnL3
  | ColumnL2
  | ColumnL1
  | ColumnC
  | ColumnR1
  | ColumnR2
  | ColumnR3
  | ColumnR4
  deriving (Eq, Ord, Show, Enum, Bounded)

data Color = Blue | Red
  deriving (Eq, Ord, Show, Enum, Bounded)

type MSize = Int

data SRTrack t = SRTrack
  { srTap           :: RTB.T t (Column, Color)
  , srMatch         :: RTB.T t (Column, Color)
  , srBeat          :: RTB.T t ()
  , srSpin          :: RTB.T t Hand
  , srSlideStart    :: RTB.T t (Column, (Color, MSize)) -- is MSize needed here?
  , srSlideContinue :: RTB.T t (Column, (Color, MSize))
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SRTrack t)

instance ParseTrack SRTrack where
  parseTrack = fatBlips (1/8) $ do
    let dimapColor = dimap (fmap (, 100)) (fmap fst) . dimapColorMSize
        dimapColorMSize = dimap
          (fmap $ \(color, size) -> (case color of Blue -> 0; Red -> 1, 100 + size))
          (fmap $ \(c, v) -> (case c of 0 -> Blue; _ -> Red, v - 100))
    srTap <- (srTap =.) $ condenseMap $ eachKey each $ \col ->
      dimapColor $ blipCV $ 100 - 5 * fromEnum col
    srMatch <- (srMatch =.) $ condenseMap $ eachKey each $ \col ->
      dimapColor $ blipCV $ 100 - 5 * fromEnum col - 1
    srSlideStart <- (srSlideStart =.) $ condenseMap $ eachKey each $ \col ->
      dimapColorMSize $ blipCV $ 100 - 5 * fromEnum col - 2
    srSlideContinue <- (srSlideContinue =.) $ condenseMap $ eachKey each $ \col ->
      dimapColorMSize $ blipCV $ 100 - 5 * fromEnum col - 3
    srSpin <- (srSpin =.) $ condenseMap_ $ eachKey each $ blip . \case
      LH -> 55
      RH -> 54
    srBeat <- srBeat =. blip 52
    return SRTrack{..}

srTrackFromJSON :: [Note t] -> SRTrack t
srTrackFromJSON = undefined

srTrackToJSON :: (NNC.C t, Num t) => SRTrack t -> [Note t]
srTrackToJSON sr = let
  columnNumber col = 4 - fromEnum col
  rtb = foldr RTB.merge RTB.empty
    [ flip fmap (srTap sr) $ \(col, color) -> Note
      { note_time       = ()
      , note_type       = 8
      , note_colorIndex = fromEnum color
      , note_column     = columnNumber col
      , note_m_size     = 0
      }
    , flip fmap (srMatch sr) $ \(col, color) -> Note
      { note_time       = ()
      , note_type       = 0
      , note_colorIndex = fromEnum color
      , note_column     = columnNumber col
      , note_m_size     = 0
      }
    , flip fmap (srBeat sr) $ \() -> Note
      { note_time       = ()
      , note_type       = 1
      , note_colorIndex = 0
      , note_column     = 0
      , note_m_size     = 0
      }
    , flip fmap (srSpin sr) $ \dir -> Note
      { note_time       = ()
      , note_type       = case dir of RH -> 2; LH -> 3
      , note_colorIndex = 0
      , note_column     = 0
      , note_m_size     = 0
      }
    , flip fmap (srSlideStart sr) $ \(col, (color, size)) -> Note
      { note_time       = ()
      , note_type       = 4
      , note_colorIndex = fromEnum color
      , note_column     = columnNumber col
      , note_m_size     = size
      }
    , flip fmap (srSlideContinue sr) $ \(col, (color, size)) -> Note
      { note_time       = ()
      , note_type       = 5
      , note_colorIndex = fromEnum color
      , note_column     = columnNumber col
      , note_m_size     = size
      }
    ]
  in fmap (uncurry (<$)) $ ATB.toPairList $ RTB.toAbsoluteEventList NNC.zero rtb
