{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.Ragnarock (importRagnarock) where

import           Control.Monad                    (forM, guard)
import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                       as A
import qualified Data.ByteString                  as B
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort, sortOn)
import qualified Data.Map                         as Map
import           Data.Scientific                  (Scientific)
import qualified Data.Text                        as T
import           Onyx.Audio
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..))
import           Onyx.MIDI.Track.Drums
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))

data InfoDat = InfoDat
  { version                   :: Maybe T.Text
  , songName                  :: T.Text
  , songSubName               :: Maybe T.Text
  , songAuthorName            :: Maybe T.Text
  , levelAuthorName           :: Maybe T.Text
  , explicit                  :: Maybe T.Text
  , beatsPerMinute            :: Scientific
  , shuffle                   :: Maybe Int
  , shufflePeriod             :: Maybe Scientific
  , previewStartTime          :: Maybe Scientific
  , previewDuration           :: Maybe Scientific
  , songApproximativeDuration :: Maybe Scientific
  , songFilename              :: T.Text
  , coverImageFilename        :: Maybe T.Text
  , environmentName           :: Maybe T.Text
  , songTimeOffset            :: Maybe Scientific
  -- TODO _customData
  , difficultyBeatmapSets     :: [BeatmapSet]
  } deriving (Show)

instance StackJSON InfoDat where
  stackJSON = asObject "info.dat" $ do
    version                   <- (.version                  ) =. opt  Nothing "_version"                   stackJSON
    songName                  <- (.songName                 ) =. req          "_songName"                  stackJSON
    songSubName               <- (.songSubName              ) =. opt  Nothing "_songSubName"               stackJSON
    songAuthorName            <- (.songAuthorName           ) =. opt  Nothing "_songAuthorName"            stackJSON
    levelAuthorName           <- (.levelAuthorName          ) =. opt  Nothing "_levelAuthorName"           stackJSON
    explicit                  <- (.explicit                 ) =. opt  Nothing "_explicit"                  stackJSON
    beatsPerMinute            <- (.beatsPerMinute           ) =. req          "_beatsPerMinute"            stackJSON
    shuffle                   <- (.shuffle                  ) =. opt  Nothing "_shuffle"                   stackJSON
    shufflePeriod             <- (.shufflePeriod            ) =. opt  Nothing "_shufflePeriod"             stackJSON
    previewStartTime          <- (.previewStartTime         ) =. opt  Nothing "_previewStartTime"          stackJSON
    previewDuration           <- (.previewDuration          ) =. opt  Nothing "_previewDuration"           stackJSON
    songApproximativeDuration <- (.songApproximativeDuration) =. opt  Nothing "_songApproximativeDuration" stackJSON
    songFilename              <- (.songFilename             ) =. req          "_songFilename"              stackJSON
    coverImageFilename        <- (.coverImageFilename       ) =. opt  Nothing "_coverImageFilename"        stackJSON
    environmentName           <- (.environmentName          ) =. opt  Nothing "_environmentName"           stackJSON
    songTimeOffset            <- (.songTimeOffset           ) =. opt  Nothing "_songTimeOffset"            stackJSON
    difficultyBeatmapSets     <- (.difficultyBeatmapSets    ) =. fill []      "_difficultyBeatmapSets"     stackJSON
    return InfoDat{..}

data BeatmapSet = BeatmapSet
  { beatmapCharacteristicName :: T.Text
  , difficultyBeatmaps        :: [BeatmapInfo]
  } deriving (Eq, Show)

instance StackJSON BeatmapSet where
  stackJSON = asObject "info.dat beatmap set" $ do
    beatmapCharacteristicName <- (.beatmapCharacteristicName) =. req     "_beatmapCharacteristicName" stackJSON
    difficultyBeatmaps        <- (.difficultyBeatmaps       ) =. fill [] "_difficultyBeatmaps"        stackJSON
    return BeatmapSet{..}

data BeatmapInfo = BeatmapInfo
  { difficulty              :: T.Text
  , difficultyRank          :: Int
  , beatmapFilename         :: T.Text
  , noteJumpMovementSpeed   :: Scientific
  , noteJumpStartBeatOffset :: Scientific
  -- TODO _customData
  } deriving (Eq, Show)

instance StackJSON BeatmapInfo where
  stackJSON = asObject "info.dat beatmap" $ do
    difficulty              <- (.difficulty             ) =. req "_difficulty"              stackJSON
    difficultyRank          <- (.difficultyRank         ) =. req "_difficultyRank"          stackJSON
    beatmapFilename         <- (.beatmapFilename        ) =. req "_beatmapFilename"         stackJSON
    noteJumpMovementSpeed   <- (.noteJumpMovementSpeed  ) =. req "_noteJumpMovementSpeed"   stackJSON
    noteJumpStartBeatOffset <- (.noteJumpStartBeatOffset) =. req "_noteJumpStartBeatOffset" stackJSON
    return BeatmapInfo{..}

data Beatmap = Beatmap
  { version    :: T.Text
  , customData :: BeatmapCustomData
  -- TODO _events
  , notes      :: [Note]
  -- TODO _obstacles
  } deriving (Show)

instance StackJSON Beatmap where
  stackJSON = asObject "Beatmap" $ do
    version    <- (.version   ) =. req     "_version"    stackJSON
    customData <- (.customData) =. req     "_customData" stackJSON
    notes      <- (.notes     ) =. fill [] "_notes"      stackJSON
    return Beatmap{..}

data BeatmapCustomData = BeatmapCustomData
  { time       :: Scientific
  , bpmChanges :: [BPMChange]
  } deriving (Show)

instance StackJSON BeatmapCustomData where
  stackJSON = asObject "BeatmapCustomData" $ do
    time       <- (.time      ) =. req     "_time"       stackJSON
    bpmChanges <- (.bpmChanges) =. fill [] "_bpmChanges" stackJSON
    return BeatmapCustomData{..}

data BPMChange = BPMChange
  { bpm             :: Scientific
  , time            :: Scientific
  , beatsPerBar     :: Int
  , metronomeOffset :: Int
  } deriving (Eq, Show)

instance StackJSON BPMChange where
  stackJSON = asObject "BPMChange" $ do
    bpm             <- (.bpm            ) =. req "_bpm"             stackJSON
    time            <- (.time           ) =. req "_time"            stackJSON
    beatsPerBar     <- (.beatsPerBar    ) =. req "_beatsPerBar"     stackJSON
    metronomeOffset <- (.metronomeOffset) =. req "_metronomeOffset" stackJSON
    return BPMChange{..}

data Note = Note
  { time         :: Scientific
  , lineIndex    :: Int
  , lineLayer    :: Int
  , type_        :: Int
  , cutDirection :: Int
  } deriving (Eq, Show)

instance StackJSON Note where
  stackJSON = asObject "Note" $ do
    time         <- (.time        ) =. req "_time"         stackJSON
    lineIndex    <- (.lineIndex   ) =. req "_lineIndex"    stackJSON
    lineLayer    <- (.lineLayer   ) =. req "_lineLayer"    stackJSON
    type_        <- (.type_       ) =. req "_type"         stackJSON
    cutDirection <- (.cutDirection) =. req "_cutDirection" stackJSON
    return Note{..}

importRagnarock :: (SendMessage m, MonadIO m) => FilePath -> Import m
importRagnarock pathInfo level = do
  let dir = takeDirectory pathInfo
      loadJSON f = do
        json <- stackIO (B.readFile f) >>= either fatal return . A.eitherDecodeStrict
        mapStackTraceT (`runReaderT` json) fromJSON
  info <- loadJSON pathInfo
  let _ = info :: InfoDat
  art <- case info.coverImageFilename of
    Nothing -> return Nothing
    Just f  -> let
      f' = dir </> T.unpack f
      in return $ Just $ SoftFile ("cover" <.> takeExtension f') $ SoftReadable $ fileReadable f'
  maps <- case level of
    ImportFull  -> let
      infos = do
        set <- info.difficultyBeatmapSets
        guard $ set.beatmapCharacteristicName == "Standard"
        set.difficultyBeatmaps
      in forM infos $ \mapInfo -> do
        beatmap <- loadJSON $ dir </> T.unpack mapInfo.beatmapFilename
        return (mapInfo, beatmap)
    ImportQuick -> return []
  return SongYaml
    { metadata = def'
      { title        = Just info.songName
      , artist       = info.songAuthorName
      , author       = info.levelAuthorName
      , fileAlbumArt = art
      }
    , jammit = mempty
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> ragnarockToDrums info maps
        ImportQuick -> emptyChart
      , fileSongAnim = Nothing
      }
    , audio = HM.singleton "song" $ let
      f = dir </> T.unpack info.songFilename
      in AudioFile AudioInfo
        { md5      = Nothing
        , frames   = Nothing
        , commands = []
        , filePath = Just $ SoftFile ("audio" <.> takeExtension f) $ SoftReadable $ fileReadable f
        , rate     = Nothing
        , channels = 2 -- just assuming
        }
    , plans = HM.singleton "plan" $ StandardPlan StandardPlanInfo
      { song        = Just $ Input $ Named "song"
      , parts       = Parts HM.empty
      , crowd       = Nothing
      , comments    = []
      , tuningCents = 0
      , fileTempo   = Nothing
      }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.FlexDrums (emptyPart :: Part SoftFile)
      { drums = Just $ emptyPartDrums Drums4 Kicks1x
      }
    }

ragnarockToDrums :: InfoDat -> [(BeatmapInfo, Beatmap)] -> F.Song (F.OnyxFile U.Beats)
ragnarockToDrums info maps = let
  maps' = zip [Expert, Hard, Medium, Easy]
    $ sortOn (\(mapInfo, _) -> negate $ mapInfo.difficultyRank) maps
  toBPS :: Scientific -> U.BPS
  toBPS sci = realToFrac sci / 60
  -- TODO probably need to apply songTimeOffset (and/or do that in audio plan)
  -- TODO need to compute tempo map for lower difficulties independently (and then translate)
  tmap = U.tempoMapFromBPS $ RTB.fromAbsoluteEventList $ ATB.fromPairList
    $ sort $ (0, toBPS info.beatsPerMinute) : do
      change <- (snd $ snd $ head maps').customData.bpmChanges
      return (realToFrac change.time, toBPS change.bpm)
  in F.Song
    { F.s_tempos = tmap
    , F.s_signatures = U.measureMapFromTimeSigs U.Error RTB.empty -- TODO maybe
    , F.s_tracks = mempty
      { F.onyxParts = Map.singleton F.FlexDrums mempty
        { F.onyxPartDrums = mempty
          { drumDifficulties = Map.fromList $ flip map maps' $ fmap $ \(_, beatmap) -> mempty
            { drumGems = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
              note <- beatmap.notes
              let t = realToFrac note.time :: U.Beats
              gem <- case note.lineIndex of
                0 -> [Red          ]
                1 -> [Pro Yellow ()]
                2 -> [Pro Blue   ()]
                3 -> [Pro Green  ()]
                _ -> []
              return (t, (gem, VelocityNormal))
            }
          }
        }
      }
    }
