{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.DLCBuilder where

import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx
import           Control.Monad.Codec.Onyx.JSON
import           Control.Monad.Trans            (lift)
import           Control.Monad.Trans.Reader     (ask)
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.=))
import qualified Data.Aeson                     as A
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Rocksmith.CST                  (Tone2014 (..))

data RS2DLC = RS2DLC
  { rs2_Version            :: T.Text
  , rs2_Author             :: T.Text
  , rs2_DLCKey             :: T.Text
  , rs2_ArtistName         :: Sortable
  , rs2_JapaneseArtistName :: Maybe T.Text
  , rs2_JapaneseTitle      :: Maybe T.Text
  , rs2_Title              :: Sortable
  , rs2_AlbumName          :: Sortable
  , rs2_Year               :: Int
  , rs2_AlbumArtFile       :: FilePath
  , rs2_AudioFile          :: AudioFile
  , rs2_AudioPreviewFile   :: AudioFile
  , rs2_IgnoredIssues      :: [T.Text]
  , rs2_Arrangements       :: [Arrangement]
  , rs2_Tones              :: [Tone2014]
  } deriving (Show)

instance StackJSON RS2DLC where
  stackJSON = asObject ".rs2dlc" $ do
    rs2_Version            <- rs2_Version            =. req "Version"            stackJSON
    rs2_Author             <- rs2_Author             =. req "Author"             stackJSON
    rs2_DLCKey             <- rs2_DLCKey             =. req "DLCKey"             stackJSON
    rs2_ArtistName         <- rs2_ArtistName         =. req "ArtistName"         stackJSON
    rs2_JapaneseArtistName <- rs2_JapaneseArtistName =. req "JapaneseArtistName" stackJSON
    rs2_JapaneseTitle      <- rs2_JapaneseTitle      =. req "JapaneseTitle"      stackJSON
    rs2_Title              <- rs2_Title              =. req "Title"              stackJSON
    rs2_AlbumName          <- rs2_AlbumName          =. req "AlbumName"          stackJSON
    rs2_Year               <- rs2_Year               =. req "Year"               stackJSON
    rs2_AlbumArtFile       <- rs2_AlbumArtFile       =. req "AlbumArtFile"       stackJSON
    rs2_AudioFile          <- rs2_AudioFile          =. req "AudioFile"          stackJSON
    rs2_AudioPreviewFile   <- rs2_AudioPreviewFile   =. req "AudioPreviewFile"   stackJSON
    rs2_IgnoredIssues      <- rs2_IgnoredIssues      =. req "IgnoredIssues"      stackJSON
    rs2_Arrangements       <- rs2_Arrangements       =. req "Arrangements"       stackJSON
    rs2_Tones              <- rs2_Tones              =. req "Tones"              stackJSON
    return RS2DLC{..}

data Sortable = Sortable
  { rs2_Value     :: T.Text
  , rs2_SortValue :: T.Text
  } deriving (Show)

instance StackJSON Sortable where
  stackJSON = asObject "display and sort values" $ do
    rs2_Value     <- rs2_Value     =. req "Value"     stackJSON
    rs2_SortValue <- rs2_SortValue =. req "SortValue" stackJSON
    return Sortable{..}

data AudioFile = AudioFile
  { audio_Path   :: FilePath
  , audio_Volume :: Double
  } deriving (Show)

instance StackJSON AudioFile where
  stackJSON = asObject "audio path and volume" $ do
    audio_Path   <- audio_Path   =. req "Path"   stackJSON
    audio_Volume <- audio_Volume =. req "Volume" stackJSON
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
        arrCase   <- maybe (fatal "Arrangement has no Case"  ) return $ HM.lookup "Case"   o
        arrFields <- maybe (fatal "Arrangement has no Fields") return $ HM.lookup "Fields" o
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
  { inst_XML          :: FilePath
  , inst_Name         :: Int
  , inst_RouteMask    :: Int
  , inst_Priority     :: Int
  , inst_ScrollSpeed  :: Double
  , inst_BassPicked   :: Bool
  , inst_Tuning       :: [Int]
  , inst_TuningPitch  :: Double
  , inst_BaseTone     :: T.Text
  , inst_Tones        :: [T.Text]
  , inst_MasterID     :: Int
  , inst_PersistentID :: T.Text
  } deriving (Show)

instance StackJSON Instrumental where
  stackJSON = asObject "Instrumental" $ do
    inst_XML          <- inst_XML          =. req "XML"          stackJSON
    inst_Name         <- inst_Name         =. req "Name"         stackJSON
    inst_RouteMask    <- inst_RouteMask    =. req "RouteMask"    stackJSON
    inst_Priority     <- inst_Priority     =. req "Priority"     stackJSON
    inst_ScrollSpeed  <- inst_ScrollSpeed  =. req "ScrollSpeed"  stackJSON
    inst_BassPicked   <- inst_BassPicked   =. req "BassPicked"   stackJSON
    inst_Tuning       <- inst_Tuning       =. req "Tuning"       stackJSON
    inst_TuningPitch  <- inst_TuningPitch  =. req "TuningPitch"  stackJSON
    inst_BaseTone     <- inst_BaseTone     =. req "BaseTone"     stackJSON
    inst_Tones        <- inst_Tones        =. req "Tones"        stackJSON
    inst_MasterID     <- inst_MasterID     =. req "MasterID"     stackJSON
    inst_PersistentID <- inst_PersistentID =. req "PersistentID" stackJSON
    return Instrumental{..}

data Vocals = Vocals
  { vocals_XML          :: FilePath
  , vocals_Japanese     :: Bool
  , vocals_CustomFont   :: Maybe T.Text -- is this a path
  , vocals_MasterID     :: Int
  , vocals_PersistentID :: T.Text
  } deriving (Show)

instance StackJSON Vocals where
  stackJSON = asObject "Vocals" $ do
    vocals_XML          <- vocals_XML          =. req "XML"          stackJSON
    vocals_Japanese     <- vocals_Japanese     =. req "Japanese"     stackJSON
    vocals_CustomFont   <- vocals_CustomFont   =. req "CustomFont"   stackJSON
    vocals_MasterID     <- vocals_MasterID     =. req "MasterID"     stackJSON
    vocals_PersistentID <- vocals_PersistentID =. req "PersistentID" stackJSON
    return Vocals{..}

data Showlights = Showlights
  { showlights_XML :: FilePath
  } deriving (Show)

instance StackJSON Showlights where
  stackJSON = asObject "Showlights" $ do
    showlights_XML <- showlights_XML =. req "XML" stackJSON
    return Showlights{..}
