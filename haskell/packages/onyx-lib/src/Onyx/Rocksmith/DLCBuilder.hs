{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Rocksmith.DLCBuilder where

import           Control.Monad.Codec
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader (ask)
import           Data.Aeson                 ((.=))
import qualified Data.Aeson                 as A
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.Rocksmith.CST         (Tone2014 (..))
import           Onyx.StackTrace

data RS2DLC = RS2DLC
  { version            :: T.Text
  , author             :: T.Text
  , dlcKey             :: T.Text
  , artistName         :: Sortable
  , japaneseArtistName :: Maybe T.Text
  , japaneseTitle      :: Maybe T.Text
  , title              :: Sortable
  , albumName          :: Sortable
  , year               :: Int
  , albumArtFile       :: FilePath
  , audioFile          :: AudioFile
  , audioPreviewFile   :: AudioFile
  , ignoredIssues      :: [T.Text]
  , arrangements       :: [Arrangement]
  , tones              :: [Tone2014]
  } deriving (Show)

instance StackJSON RS2DLC where
  stackJSON = asObject ".rs2dlc" $ do
    version            <- (.version           ) =. req "Version"            stackJSON
    author             <- (.author            ) =. req "Author"             stackJSON
    dlcKey             <- (.dlcKey            ) =. req "DLCKey"             stackJSON
    artistName         <- (.artistName        ) =. req "ArtistName"         stackJSON
    japaneseArtistName <- (.japaneseArtistName) =. req "JapaneseArtistName" stackJSON
    japaneseTitle      <- (.japaneseTitle     ) =. req "JapaneseTitle"      stackJSON
    title              <- (.title             ) =. req "Title"              stackJSON
    albumName          <- (.albumName         ) =. req "AlbumName"          stackJSON
    year               <- (.year              ) =. req "Year"               stackJSON
    albumArtFile       <- (.albumArtFile      ) =. req "AlbumArtFile"       stackJSON
    audioFile          <- (.audioFile         ) =. req "AudioFile"          stackJSON
    audioPreviewFile   <- (.audioPreviewFile  ) =. req "AudioPreviewFile"   stackJSON
    ignoredIssues      <- (.ignoredIssues     ) =. req "IgnoredIssues"      stackJSON
    arrangements       <- (.arrangements      ) =. req "Arrangements"       stackJSON
    tones              <- (.tones             ) =. req "Tones"              stackJSON
    return RS2DLC{..}

data Sortable = Sortable
  { value     :: T.Text
  , sortValue :: T.Text
  } deriving (Show)

instance StackJSON Sortable where
  stackJSON = asObject "display and sort values" $ do
    value     <- (.value    ) =. req "Value"     stackJSON
    sortValue <- (.sortValue) =. req "SortValue" stackJSON
    return Sortable{..}

data AudioFile = AudioFile
  { path   :: FilePath
  , volume :: Double
  } deriving (Show)

instance StackJSON AudioFile where
  stackJSON = asObject "audio path and volume" $ do
    path   <- (.path  ) =. req "Path"   stackJSON
    volume <- (.volume) =. req "Volume" stackJSON
    return AudioFile{..}

data Arrangement
  = ArrInstrumental Instrumental
  | ArrVocals       Vocals
  | ArrShowlights   Showlights
  deriving (Show)

instance StackJSON Arrangement where
  stackJSON = Codec
    { codecIn = inside "Parsing arrangement" $ lift ask >>= \case
      A.Object o -> do
        arrCase   <- maybe (fatal "Arrangement has no Case"  ) return $ KM.lookup "Case"   o
        arrFields <- maybe (fatal "Arrangement has no Fields") return $ KM.lookup "Fields" o
        arrFields1 <- case arrFields of
          A.Array a -> case V.toList a of
            [x] -> return x
            _   -> fatal "Fields has more than 1 element"
          _         -> fatal "Fields not an array"
        case arrCase of
          "Instrumental" -> ArrInstrumental <$> parseFrom arrFields1 (codecIn stackJSON)
          "Vocals"       -> ArrVocals       <$> parseFrom arrFields1 (codecIn stackJSON)
          "Showlights"   -> ArrShowlights   <$> parseFrom arrFields1 (codecIn stackJSON)
          _              -> fatal $ "Unrecognized arrangement Case: " <> show arrCase
      _ -> fatal "Not an object"
    , codecOut = makeOut $ \case
      ArrInstrumental x -> A.object
        [ "Case"   .= ("Instrumental" :: T.Text)
        , "Fields" .= toJSON [x]
        ]
      ArrVocals x -> A.object
        [ "Case"   .= ("Vocals" :: T.Text)
        , "Fields" .= toJSON [x]
        ]
      ArrShowlights x -> A.object
        [ "Case"   .= ("Showlights" :: T.Text)
        , "Fields" .= toJSON [x]
        ]
    }

data Instrumental = Instrumental
  { xml          :: FilePath
  , name         :: Int
  , routeMask    :: Int
  , priority     :: Int
  , scrollSpeed  :: Double
  , bassPicked   :: Bool
  , tuning       :: [Int]
  , tuningPitch  :: Double
  , baseTone     :: T.Text
  , tones        :: [T.Text]
  , masterID     :: Int
  , persistentID :: T.Text
  } deriving (Show)

instance StackJSON Instrumental where
  stackJSON = asObject "Instrumental" $ do
    xml          <- (.xml         ) =. req "XML"          stackJSON
    name         <- (.name        ) =. req "Name"         stackJSON
    routeMask    <- (.routeMask   ) =. req "RouteMask"    stackJSON
    priority     <- (.priority    ) =. req "Priority"     stackJSON
    scrollSpeed  <- (.scrollSpeed ) =. req "ScrollSpeed"  stackJSON
    bassPicked   <- (.bassPicked  ) =. req "BassPicked"   stackJSON
    tuning       <- (.tuning      ) =. req "Tuning"       stackJSON
    tuningPitch  <- (.tuningPitch ) =. req "TuningPitch"  stackJSON
    baseTone     <- (.baseTone    ) =. req "BaseTone"     stackJSON
    tones        <- (.tones       ) =. req "Tones"        stackJSON
    masterID     <- (.masterID    ) =. req "MasterID"     stackJSON
    persistentID <- (.persistentID) =. req "PersistentID" stackJSON
    return Instrumental{..}

data Vocals = Vocals
  { xml          :: FilePath
  , japanese     :: Bool
  , customFont   :: Maybe T.Text -- is this a path
  , masterID     :: Int
  , persistentID :: T.Text
  } deriving (Show)

instance StackJSON Vocals where
  stackJSON = asObject "Vocals" $ do
    xml          <- (.xml         ) =. req "XML"          stackJSON
    japanese     <- (.japanese    ) =. req "Japanese"     stackJSON
    customFont   <- (.customFont  ) =. req "CustomFont"   stackJSON
    masterID     <- (.masterID    ) =. req "MasterID"     stackJSON
    persistentID <- (.persistentID) =. req "PersistentID" stackJSON
    return Vocals{..}

data Showlights = Showlights
  { xml :: FilePath
  } deriving (Show)

instance StackJSON Showlights where
  stackJSON = asObject "Showlights" $ do
    xml <- (.xml) =. req "XML" stackJSON
    return Showlights{..}
