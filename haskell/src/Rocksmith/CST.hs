{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.CST where

import           Control.Monad                  (join, void)
import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx       (makeOut, opt, req)
import           Control.Monad.Codec.Onyx.JSON
import           Control.Monad.Codec.Onyx.XML
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                     as A
import qualified Data.ByteString                as B
import           Data.Fixed                     (Milli)
import           Data.Functor.Identity          (Identity)
import qualified Data.HashMap.Strict            as HM
import           Data.Int                       (Int32)
import           Data.Maybe                     (isNothing)
import           Data.Profunctor                (dimap)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.IO                   as T
import qualified Data.Vector                    as V
import           Text.Read                      (readMaybe)
import           Text.XML.Light

cstSpaceW3, cstSpaceMain, cstSpaceArrProps, cstSpaceSng2014, cstSpaceAggGraph, cstSpaceTone, cstSpaceTone2014, cstSpaceArrays :: String
cstSpaceW3       = "http://www.w3.org/2001/XMLSchema-instance"
cstSpaceMain     = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage"
cstSpaceArrProps = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.XML"
cstSpaceSng2014  = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.Sng2014HSL"
cstSpaceAggGraph = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.AggregateGraph"
cstSpaceTone     = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest.Tone"
cstSpaceTone2014 = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest2014.Tone"
cstSpaceArrays   = "http://schemas.microsoft.com/2003/10/Serialization/Arrays"

data DLCPackageData = DLCPackageData
  { dlc_AlbumArtPath      :: T.Text
  , dlc_AppId             :: Int
  , dlc_Arrangements      :: V.Vector Arrangement
  , dlc_ArtFiles          :: Maybe () -- TODO
  , dlc_DefaultShowlights :: Bool
  , dlc_GameVersion       :: T.Text
  , dlc_Inlay             :: Maybe () -- TODO
  , dlc_Mac               :: Bool
  , dlc_Name              :: T.Text
  , dlc_OggPath           :: T.Text
  , dlc_OggPreviewPath    :: T.Text
  , dlc_OggQuality        :: Milli
  , dlc_PS3               :: Bool
  , dlc_Pc                :: Bool
  , dlc_PreviewVolume     :: Milli
  , dlc_SignatureType     :: T.Text
  , dlc_SongInfo          :: SongInfo
  , dlc_Tones             :: V.Vector () -- TODO
  , dlc_TonesRS2014       :: V.Vector Tone2014
  , dlc_ToolkitInfo       :: ToolkitInfo
  , dlc_Version           :: T.Text
  , dlc_Volume            :: Milli
  , dlc_XBox360           :: Bool
  , dlc_XBox360Licenses   :: V.Vector () -- TODO
  } deriving (Eq, Show)

instance IsInside DLCPackageData where
  insideCodec = do
    useNamespace (Just "i") cstSpaceW3
    useNamespace (Just "") cstSpaceMain
    dlc_AlbumArtPath      <- dlc_AlbumArtPath      =. childTag (inSpace cstSpaceMain "AlbumArtPath"     ) (parseInside' childText)
    dlc_AppId             <- dlc_AppId             =. childTag (inSpace cstSpaceMain "AppId"            ) (parseInside' $ intText childText)
    dlc_Arrangements      <- dlc_Arrangements      =. pluralBare (inSpace cstSpaceMain "Arrangement")
    dlc_ArtFiles          <- dlc_ArtFiles          =. childTag (inSpace cstSpaceMain "ArtFiles"         ) (parseInside' $ nillable $ return ())
    dlc_DefaultShowlights <- dlc_DefaultShowlights =. childTag (inSpace cstSpaceMain "DefaultShowlights") (parseInside' $ boolWordText childText)
    dlc_GameVersion       <- dlc_GameVersion       =. childTag (inSpace cstSpaceMain "GameVersion"      ) (parseInside' childText)
    dlc_Inlay             <- dlc_Inlay             =. childTag (inSpace cstSpaceMain "Inlay"            ) (parseInside' $ nillable $ return ())
    dlc_Mac               <- dlc_Mac               =. childTag (inSpace cstSpaceMain "Mac"              ) (parseInside' $ boolWordText childText)
    dlc_Name              <- dlc_Name              =. childTag (inSpace cstSpaceMain "Name"             ) (parseInside' childText)
    dlc_OggPath           <- dlc_OggPath           =. childTag (inSpace cstSpaceMain "OggPath"          ) (parseInside' childText)
    dlc_OggPreviewPath    <- dlc_OggPreviewPath    =. childTag (inSpace cstSpaceMain "OggPreviewPath"   ) (parseInside' childText)
    dlc_OggQuality        <- dlc_OggQuality        =. childTag (inSpace cstSpaceMain "OggQuality"       ) (parseInside' $ milliText childText)
    dlc_PS3               <- dlc_PS3               =. childTag (inSpace cstSpaceMain "PS3"              ) (parseInside' $ boolWordText childText)
    dlc_Pc                <- dlc_Pc                =. childTag (inSpace cstSpaceMain "Pc"               ) (parseInside' $ boolWordText childText)
    dlc_PreviewVolume     <- dlc_PreviewVolume     =. childTag (inSpace cstSpaceMain "PreviewVolume"    ) (parseInside' $ milliText childText)
    dlc_SignatureType     <- dlc_SignatureType     =. childTag (inSpace cstSpaceMain "SignatureType"    ) (parseInside' childText)
    dlc_SongInfo          <- dlc_SongInfo          =. childTag (inSpace cstSpaceMain "SongInfo"         ) (parseInside' insideCodec)
    dlc_Tones             <- dlc_Tones             =. childTag (inSpace cstSpaceMain "Tones"            ) (parseInside' $ return V.empty)
    dlc_TonesRS2014       <- dlc_TonesRS2014       =. pluralBare' (inSpace cstSpaceMain "TonesRS2014") (inSpace cstSpaceTone2014 "Tone2014")
    dlc_ToolkitInfo       <- dlc_ToolkitInfo       =. childTag (inSpace cstSpaceMain "ToolkitInfo"      ) (parseInside' insideCodec)
    dlc_Version           <- dlc_Version           =. childTag (inSpace cstSpaceMain "Version"          ) (parseInside' childText)
    dlc_Volume            <- dlc_Volume            =. childTag (inSpace cstSpaceMain "Volume"           ) (parseInside' $ milliText childText)
    dlc_XBox360           <- dlc_XBox360           =. childTag (inSpace cstSpaceMain "XBox360"          ) (parseInside' $ boolWordText childText)
    dlc_XBox360Licenses   <- dlc_XBox360Licenses   =. childTag (inSpace cstSpaceMain "XBox360Licenses"  ) (parseInside' $ return V.empty)
    return DLCPackageData{..}

data Tone2014 = Tone2014
  { t14_GearList        :: Gear2014
  , t14_IsCustom        :: Bool
  , t14_Key             :: T.Text
  , t14_Name            :: T.Text
  , t14_NameSeparator   :: T.Text
  , t14_SortOrder       :: Milli
  , t14_ToneDescriptors :: V.Vector T.Text
  , t14_Volume          :: Milli
  } deriving (Eq, Show)

instance IsInside Tone2014 where
  insideCodec = do
    useNamespace (Just "i") cstSpaceW3
    useNamespace (Just "") cstSpaceTone2014
    t14_GearList        <- t14_GearList        =. childTag (inSpace cstSpaceTone2014 "GearList"       ) (parseInside' insideCodec)
    t14_IsCustom        <- t14_IsCustom        =. childTag (inSpace cstSpaceTone2014 "IsCustom"       ) (parseInside' $ boolWordText childText)
    t14_Key             <- t14_Key             =. childTag (inSpace cstSpaceTone2014 "Key"            ) (parseInside' childText)
    t14_Name            <- t14_Name            =. childTag (inSpace cstSpaceTone2014 "Name"           ) (parseInside' childText)
    t14_NameSeparator   <- t14_NameSeparator   =. childTag (inSpace cstSpaceTone2014 "NameSeparator"  ) (parseInside' childTextRaw)
    t14_SortOrder       <- t14_SortOrder       =. childTag (inSpace cstSpaceTone2014 "SortOrder"      ) (parseInside' $ milliText childText)
    t14_ToneDescriptors <- t14_ToneDescriptors =. childTag (inSpace cstSpaceTone2014 "ToneDescriptors") (parseInside' toneDescriptors)
    t14_Volume          <- t14_Volume          =. childTag (inSpace cstSpaceTone2014 "Volume"         ) (parseInside' $ milliText childText)
    return Tone2014{..}

toneDescriptors :: (SendMessage m) => InsideCodec m (V.Vector T.Text)
toneDescriptors = do
  useNamespace Nothing cstSpaceArrays
  bareList $ isTag (inSpace cstSpaceArrays "string") $ parseInside' childText

-- | Format used in manifest .json
instance StackJSON Tone2014 where
  stackJSON = asStrictObject "Tone2014" $ do
    t14_GearList        <- t14_GearList        =. req "GearList" stackJSON
    t14_IsCustom        <- t14_IsCustom        =. req "IsCustom" stackJSON
    t14_Key             <- t14_Key             =. req "Key" stackJSON
    t14_Name            <- t14_Name            =. req "Name" stackJSON
    t14_NameSeparator   <- t14_NameSeparator   =. req "NameSeparator" stackJSON
    t14_SortOrder       <- t14_SortOrder       =. req "SortOrder" stackJSON
    t14_ToneDescriptors <- t14_ToneDescriptors =. req "ToneDescriptors" stackJSON
    t14_Volume          <- t14_Volume          =. req "Volume" Codec
      { codecIn = codecIn stackJSON >>= \s -> case readMaybe s of
        Nothing -> fatal "Couldn't read volume number from string"
        Just v  -> return v
      , codecOut = makeOut $ A.String . T.pack . show
      }
    return Tone2014{..}

data Gear2014 = Gear2014
  { g14_Amp        :: Maybe Pedal2014
  , g14_Cabinet    :: Maybe Pedal2014
  , g14_PostPedal1 :: Maybe Pedal2014
  , g14_PostPedal2 :: Maybe Pedal2014
  , g14_PostPedal3 :: Maybe Pedal2014
  , g14_PostPedal4 :: Maybe Pedal2014
  , g14_PrePedal1  :: Maybe Pedal2014
  , g14_PrePedal2  :: Maybe Pedal2014
  , g14_PrePedal3  :: Maybe Pedal2014
  , g14_PrePedal4  :: Maybe Pedal2014
  , g14_Rack1      :: Maybe Pedal2014
  , g14_Rack2      :: Maybe Pedal2014
  , g14_Rack3      :: Maybe Pedal2014
  , g14_Rack4      :: Maybe Pedal2014
  } deriving (Eq, Show)

instance IsInside Gear2014 where
  insideCodec = do
    g14_Amp        <- g14_Amp        =. childTag (inSpace cstSpaceTone2014 "Amp"       ) (parseInside' $ nillable insideCodec)
    g14_Cabinet    <- g14_Cabinet    =. childTag (inSpace cstSpaceTone2014 "Cabinet"   ) (parseInside' $ nillable insideCodec)
    g14_PostPedal1 <- g14_PostPedal1 =. childTag (inSpace cstSpaceTone2014 "PostPedal1") (parseInside' $ nillable insideCodec)
    g14_PostPedal2 <- g14_PostPedal2 =. childTag (inSpace cstSpaceTone2014 "PostPedal2") (parseInside' $ nillable insideCodec)
    g14_PostPedal3 <- g14_PostPedal3 =. childTag (inSpace cstSpaceTone2014 "PostPedal3") (parseInside' $ nillable insideCodec)
    g14_PostPedal4 <- g14_PostPedal4 =. childTag (inSpace cstSpaceTone2014 "PostPedal4") (parseInside' $ nillable insideCodec)
    g14_PrePedal1  <- g14_PrePedal1  =. childTag (inSpace cstSpaceTone2014 "PrePedal1" ) (parseInside' $ nillable insideCodec)
    g14_PrePedal2  <- g14_PrePedal2  =. childTag (inSpace cstSpaceTone2014 "PrePedal2" ) (parseInside' $ nillable insideCodec)
    g14_PrePedal3  <- g14_PrePedal3  =. childTag (inSpace cstSpaceTone2014 "PrePedal3" ) (parseInside' $ nillable insideCodec)
    g14_PrePedal4  <- g14_PrePedal4  =. childTag (inSpace cstSpaceTone2014 "PrePedal4" ) (parseInside' $ nillable insideCodec)
    g14_Rack1      <- g14_Rack1      =. childTag (inSpace cstSpaceTone2014 "Rack1"     ) (parseInside' $ nillable insideCodec)
    g14_Rack2      <- g14_Rack2      =. childTag (inSpace cstSpaceTone2014 "Rack2"     ) (parseInside' $ nillable insideCodec)
    g14_Rack3      <- g14_Rack3      =. childTag (inSpace cstSpaceTone2014 "Rack3"     ) (parseInside' $ nillable insideCodec)
    g14_Rack4      <- g14_Rack4      =. childTag (inSpace cstSpaceTone2014 "Rack4"     ) (parseInside' $ nillable insideCodec)
    return Gear2014{..}

-- | Format used in manifest .json
instance StackJSON Gear2014 where
  stackJSON = asStrictObject "Gear2014" $ do
    g14_Amp        <- g14_Amp        =. opt Nothing "Amp"        stackJSON
    g14_Cabinet    <- g14_Cabinet    =. opt Nothing "Cabinet"    stackJSON
    g14_PostPedal1 <- g14_PostPedal1 =. opt Nothing "PostPedal1" stackJSON
    g14_PostPedal2 <- g14_PostPedal2 =. opt Nothing "PostPedal2" stackJSON
    g14_PostPedal3 <- g14_PostPedal3 =. opt Nothing "PostPedal3" stackJSON
    g14_PostPedal4 <- g14_PostPedal4 =. opt Nothing "PostPedal4" stackJSON
    g14_PrePedal1  <- g14_PrePedal1  =. opt Nothing "PrePedal1"  stackJSON
    g14_PrePedal2  <- g14_PrePedal2  =. opt Nothing "PrePedal2"  stackJSON
    g14_PrePedal3  <- g14_PrePedal3  =. opt Nothing "PrePedal3"  stackJSON
    g14_PrePedal4  <- g14_PrePedal4  =. opt Nothing "PrePedal4"  stackJSON
    g14_Rack1      <- g14_Rack1      =. opt Nothing "Rack1"      stackJSON
    g14_Rack2      <- g14_Rack2      =. opt Nothing "Rack2"      stackJSON
    g14_Rack3      <- g14_Rack3      =. opt Nothing "Rack3"      stackJSON
    g14_Rack4      <- g14_Rack4      =. opt Nothing "Rack4"      stackJSON
    return Gear2014{..}

data Pedal2014 = Pedal2014
  { p14_Category   :: Maybe T.Text
  , p14_KnobValues :: V.Vector (T.Text, Milli)
  , p14_PedalKey   :: T.Text
  , p14_Skin       :: Maybe T.Text
  , p14_SkinIndex  :: Maybe Int
  , p14_Type       :: T.Text
  } deriving (Eq, Show)

optNillable :: (SendMessage m) => ParseName -> InsideCodec m a -> InsideCodec m (Maybe a)
optNillable pn vc = dimap (fmap Just) join $ childTagOpt pn $ parseInside' $ nillable vc

instance IsInside Pedal2014 where
  insideCodec = do
    p14_Category   <- p14_Category   =. optNillable (inSpace cstSpaceTone2014 "Category"  ) childText
    p14_KnobValues <- p14_KnobValues =. childTag    (inSpace cstSpaceTone2014 "KnobValues") (parseInside' knobValues)
    p14_PedalKey   <- p14_PedalKey   =. childTag    (inSpace cstSpaceTone2014 "PedalKey"  ) (parseInside' childText)
    p14_Skin       <- p14_Skin       =. optNillable (inSpace cstSpaceTone2014 "Skin"      ) childText
    p14_SkinIndex  <- p14_SkinIndex  =. optNillable (inSpace cstSpaceTone2014 "SkinIndex" ) (intText childText)
    p14_Type       <- p14_Type       =. childTag    (inSpace cstSpaceTone2014 "Type"      ) (parseInside' childText)
    return Pedal2014{..}

knobValues :: (SendMessage m) => InsideCodec m (V.Vector (T.Text, Milli))
knobValues = do
  useNamespace Nothing cstSpaceArrays
  bareList $ isTag (inSpace cstSpaceArrays "KeyValueOfstringfloat") $ parseInside' $ do
    x <- fst =. childTag (inSpace cstSpaceArrays "Key") (parseInside' childText)
    y <- snd =. childTag (inSpace cstSpaceArrays "Value") (parseInside' $ milliText childText)
    return (x, y)

nillable :: (Monad m) => InsideCodec m a -> InsideCodec m (Maybe a)
nillable c = do
  isNil <- (\x -> if isNothing x then Just "true" else Nothing) =. optAttr (inSpace cstSpaceW3 "nil")
  if isNil == Just "true"
    then return Nothing
    else Codec
      { codecIn = Just <$> codecIn c
      , codecOut = fmapArg $ mapM_ $ codecOut c
      }

-- | Format used in manifest .json
instance StackJSON Pedal2014 where
  stackJSON = asStrictObject "Pedal2014" $ do
    p14_Category   <- p14_Category   =. opt Nothing "Category" stackJSON
    p14_KnobValues <- p14_KnobValues =. req "KnobValues"
      (dimap (HM.fromList . V.toList) (V.fromList . HM.toList) $ dict stackJSON)
    p14_PedalKey   <- p14_PedalKey   =. req "Key" stackJSON
    p14_Skin       <- p14_Skin       =. opt Nothing "Skin" stackJSON
    p14_SkinIndex  <- p14_SkinIndex  =. opt Nothing "SkinIndex" stackJSON
    p14_Type       <- p14_Type       =. req "Type" stackJSON
    return Pedal2014{..}

data Arrangement = Arrangement
  { arr_ArrangementName      :: T.Text
  , arr_ArrangementPropeties :: Maybe ArrangementPropeties
  , arr_ArrangementSort      :: Int
  , arr_ArrangementType      :: T.Text
  , arr_BonusArr             :: Bool
  , arr_CapoFret             :: Int
  , arr_GlyphsXmlPath        :: Maybe T.Text
  , arr_Id                   :: T.Text -- UUID
  , arr_LyricsArtPath        :: Maybe T.Text
  , arr_MasterId             :: Int32 -- CST generates a random non-negative Int32 for this
  , arr_Metronome            :: T.Text
  , arr_PluckedType          :: T.Text
  , arr_Represent            :: Bool
  , arr_RouteMask            :: T.Text
  , arr_ScrollSpeed          :: Int -- can be fractional?
  , arr_Sng2014              :: Maybe () -- TODO
  , arr_SongFile             :: AggregateGraph
  , arr_SongXml              :: AggregateGraph
  , arr_ToneA                :: T.Text
  , arr_ToneB                :: T.Text
  , arr_ToneBase             :: T.Text
  , arr_ToneC                :: T.Text
  , arr_ToneD                :: T.Text
  , arr_ToneMultiplayer      :: Maybe T.Text
  , arr_Tuning               :: T.Text
  , arr_TuningPitch          :: Milli
  , arr_TuningStrings        :: TuningStrings
  } deriving (Eq, Show)

instance IsInside Arrangement where
  insideCodec = do
    arr_ArrangementName      <- arr_ArrangementName      =. childTag (inSpace cstSpaceMain "ArrangementName") (parseInside' childText)
    arr_ArrangementPropeties <- arr_ArrangementPropeties =. childTag (inSpace cstSpaceMain "ArrangementPropeties") (parseInside' $ nillable insideCodec)
    arr_ArrangementSort      <- arr_ArrangementSort      =. childTag (inSpace cstSpaceMain "ArrangementSort") (parseInside' $ intText childText)
    arr_ArrangementType      <- arr_ArrangementType      =. childTag (inSpace cstSpaceMain "ArrangementType") (parseInside' childText)
    arr_BonusArr             <- arr_BonusArr             =. childTag (inSpace cstSpaceMain "BonusArr") (parseInside' $ boolWordText childText)
    arr_CapoFret             <- arr_CapoFret             =. childTag (inSpace cstSpaceMain "CapoFret") (parseInside' $ intText childText)
    arr_GlyphsXmlPath        <- arr_GlyphsXmlPath        =. childTag (inSpace cstSpaceMain "GlyphsXmlPath") (parseInside' $ nillable childText)
    arr_Id                   <- arr_Id                   =. childTag (inSpace cstSpaceMain "Id") (parseInside' childText)
    arr_LyricsArtPath        <- arr_LyricsArtPath        =. childTag (inSpace cstSpaceMain "LyricsArtPath") (parseInside' $ nillable childText)
    arr_MasterId             <- arr_MasterId             =. childTag (inSpace cstSpaceMain "MasterId") (parseInside' $ intText childText)
    arr_Metronome            <- arr_Metronome            =. childTag (inSpace cstSpaceMain "Metronome") (parseInside' childText)
    arr_PluckedType          <- arr_PluckedType          =. childTag (inSpace cstSpaceMain "PluckedType") (parseInside' childText)
    arr_Represent            <- arr_Represent            =. childTag (inSpace cstSpaceMain "Represent") (parseInside' $ boolWordText childText)
    arr_RouteMask            <- arr_RouteMask            =. childTag (inSpace cstSpaceMain "RouteMask") (parseInside' childText)
    arr_ScrollSpeed          <- arr_ScrollSpeed          =. childTag (inSpace cstSpaceMain "ScrollSpeed") (parseInside' $ intText childText)
    arr_Sng2014              <- arr_Sng2014              =. childTag (inSpace cstSpaceMain "Sng2014") (parseInside' $ nillable $ return ())
    arr_SongFile             <- arr_SongFile             =. childTag (inSpace cstSpaceMain "SongFile") (parseInside' insideCodec)
    arr_SongXml              <- arr_SongXml              =. childTag (inSpace cstSpaceMain "SongXml") (parseInside' insideCodec)
    arr_ToneA                <- arr_ToneA                =. childTag (inSpace cstSpaceMain "ToneA") (parseInside' childText)
    arr_ToneB                <- arr_ToneB                =. childTag (inSpace cstSpaceMain "ToneB") (parseInside' childText)
    arr_ToneBase             <- arr_ToneBase             =. childTag (inSpace cstSpaceMain "ToneBase") (parseInside' childText)
    arr_ToneC                <- arr_ToneC                =. childTag (inSpace cstSpaceMain "ToneC") (parseInside' childText)
    arr_ToneD                <- arr_ToneD                =. childTag (inSpace cstSpaceMain "ToneD") (parseInside' childText)
    arr_ToneMultiplayer      <- arr_ToneMultiplayer      =. childTag (inSpace cstSpaceMain "ToneMultiplayer") (parseInside' $ nillable childText)
    arr_Tuning               <- arr_Tuning               =. childTag (inSpace cstSpaceMain "Tuning") (parseInside' childText)
    arr_TuningPitch          <- arr_TuningPitch          =. childTag (inSpace cstSpaceMain "TuningPitch") (parseInside' $ milliText childText)
    arr_TuningStrings        <- arr_TuningStrings        =. childTag (inSpace cstSpaceMain "TuningStrings") (parseInside' insideCodec)
    return Arrangement{..}

data TuningStrings = TuningStrings
  { ts_String0 :: Int
  , ts_String1 :: Int
  , ts_String2 :: Int
  , ts_String3 :: Int
  , ts_String4 :: Int
  , ts_String5 :: Int
  } deriving (Eq, Show)

instance IsInside TuningStrings where
  insideCodec = do
    useNamespace Nothing cstSpaceArrProps
    ts_String0 <- ts_String0 =. childTag (inSpace cstSpaceArrProps "String0") (parseInside' $ intText childText)
    ts_String1 <- ts_String1 =. childTag (inSpace cstSpaceArrProps "String1") (parseInside' $ intText childText)
    ts_String2 <- ts_String2 =. childTag (inSpace cstSpaceArrProps "String2") (parseInside' $ intText childText)
    ts_String3 <- ts_String3 =. childTag (inSpace cstSpaceArrProps "String3") (parseInside' $ intText childText)
    ts_String4 <- ts_String4 =. childTag (inSpace cstSpaceArrProps "String4") (parseInside' $ intText childText)
    ts_String5 <- ts_String5 =. childTag (inSpace cstSpaceArrProps "String5") (parseInside' $ intText childText)
    return TuningStrings{..}

data ArrangementPropeties = ArrangementPropeties
  { ap_BarreChords       :: Bool
  , ap_BassPick          :: Bool
  , ap_Bends             :: Bool
  , ap_DoubleStops       :: Bool
  , ap_DropDPower        :: Bool
  , ap_FifthsAndOctaves  :: Bool
  , ap_FingerPicking     :: Bool
  , ap_FretHandMutes     :: Bool
  , ap_Harmonics         :: Bool
  , ap_Hopo              :: Bool
  , ap_NonStandardChords :: Bool
  , ap_OpenChords        :: Bool
  , ap_PalmMutes         :: Bool
  , ap_PickDirection     :: Bool
  , ap_PinchHarmonics    :: Bool
  , ap_PowerChords       :: Bool
  , ap_Represent         :: Bool
  , ap_SlapPop           :: Bool
  , ap_Slides            :: Bool
  , ap_StandardTuning    :: Bool
  , ap_Sustain           :: Bool
  , ap_Syncopation       :: Bool
  , ap_Tapping           :: Bool
  , ap_Tremolo           :: Bool
  , ap_TwoFingerPicking  :: Bool
  , ap_UnpitchedSlides   :: Bool
  , ap_Vibrato           :: Bool
  , ap_BonusArr          :: Bool
  , ap_Metronome         :: Bool
  , ap_PathBass          :: Bool
  , ap_PathLead          :: Bool
  , ap_PathRhythm        :: Bool
  , ap_RouteMask         :: Int
  } deriving (Eq, Show)

instance IsInside ArrangementPropeties where
  insideCodec = do
    useNamespace Nothing cstSpaceArrProps
    ap_BarreChords       <- ap_BarreChords       =. childTag (inSpace cstSpaceArrProps "BarreChords"      ) (parseInside' $ boolText childText)
    ap_BassPick          <- ap_BassPick          =. childTag (inSpace cstSpaceArrProps "BassPick"         ) (parseInside' $ boolText childText)
    ap_Bends             <- ap_Bends             =. childTag (inSpace cstSpaceArrProps "Bends"            ) (parseInside' $ boolText childText)
    ap_DoubleStops       <- ap_DoubleStops       =. childTag (inSpace cstSpaceArrProps "DoubleStops"      ) (parseInside' $ boolText childText)
    ap_DropDPower        <- ap_DropDPower        =. childTag (inSpace cstSpaceArrProps "DropDPower"       ) (parseInside' $ boolText childText)
    ap_FifthsAndOctaves  <- ap_FifthsAndOctaves  =. childTag (inSpace cstSpaceArrProps "FifthsAndOctaves" ) (parseInside' $ boolText childText)
    ap_FingerPicking     <- ap_FingerPicking     =. childTag (inSpace cstSpaceArrProps "FingerPicking"    ) (parseInside' $ boolText childText)
    ap_FretHandMutes     <- ap_FretHandMutes     =. childTag (inSpace cstSpaceArrProps "FretHandMutes"    ) (parseInside' $ boolText childText)
    ap_Harmonics         <- ap_Harmonics         =. childTag (inSpace cstSpaceArrProps "Harmonics"        ) (parseInside' $ boolText childText)
    ap_Hopo              <- ap_Hopo              =. childTag (inSpace cstSpaceArrProps "Hopo"             ) (parseInside' $ boolText childText)
    ap_NonStandardChords <- ap_NonStandardChords =. childTag (inSpace cstSpaceArrProps "NonStandardChords") (parseInside' $ boolText childText)
    ap_OpenChords        <- ap_OpenChords        =. childTag (inSpace cstSpaceArrProps "OpenChords"       ) (parseInside' $ boolText childText)
    ap_PalmMutes         <- ap_PalmMutes         =. childTag (inSpace cstSpaceArrProps "PalmMutes"        ) (parseInside' $ boolText childText)
    ap_PickDirection     <- ap_PickDirection     =. childTag (inSpace cstSpaceArrProps "PickDirection"    ) (parseInside' $ boolText childText)
    ap_PinchHarmonics    <- ap_PinchHarmonics    =. childTag (inSpace cstSpaceArrProps "PinchHarmonics"   ) (parseInside' $ boolText childText)
    ap_PowerChords       <- ap_PowerChords       =. childTag (inSpace cstSpaceArrProps "PowerChords"      ) (parseInside' $ boolText childText)
    ap_Represent         <- ap_Represent         =. childTag (inSpace cstSpaceArrProps "Represent"        ) (parseInside' $ boolText childText)
    ap_SlapPop           <- ap_SlapPop           =. childTag (inSpace cstSpaceArrProps "SlapPop"          ) (parseInside' $ boolText childText)
    ap_Slides            <- ap_Slides            =. childTag (inSpace cstSpaceArrProps "Slides"           ) (parseInside' $ boolText childText)
    ap_StandardTuning    <- ap_StandardTuning    =. childTag (inSpace cstSpaceArrProps "StandardTuning"   ) (parseInside' $ boolText childText)
    ap_Sustain           <- ap_Sustain           =. childTag (inSpace cstSpaceArrProps "Sustain"          ) (parseInside' $ boolText childText)
    ap_Syncopation       <- ap_Syncopation       =. childTag (inSpace cstSpaceArrProps "Syncopation"      ) (parseInside' $ boolText childText)
    ap_Tapping           <- ap_Tapping           =. childTag (inSpace cstSpaceArrProps "Tapping"          ) (parseInside' $ boolText childText)
    ap_Tremolo           <- ap_Tremolo           =. childTag (inSpace cstSpaceArrProps "Tremolo"          ) (parseInside' $ boolText childText)
    ap_TwoFingerPicking  <- ap_TwoFingerPicking  =. childTag (inSpace cstSpaceArrProps "TwoFingerPicking" ) (parseInside' $ boolText childText)
    ap_UnpitchedSlides   <- ap_UnpitchedSlides   =. childTag (inSpace cstSpaceArrProps "UnpitchedSlides"  ) (parseInside' $ boolText childText)
    ap_Vibrato           <- ap_Vibrato           =. childTag (inSpace cstSpaceArrProps "Vibrato"          ) (parseInside' $ boolText childText)
    ap_BonusArr          <- ap_BonusArr          =. childTag (inSpace cstSpaceArrProps "BonusArr"         ) (parseInside' $ boolText childText)
    ap_Metronome         <- ap_Metronome         =. childTag (inSpace cstSpaceArrProps "Metronome"        ) (parseInside' $ boolText childText)
    ap_PathBass          <- ap_PathBass          =. childTag (inSpace cstSpaceArrProps "PathBass"         ) (parseInside' $ boolText childText)
    ap_PathLead          <- ap_PathLead          =. childTag (inSpace cstSpaceArrProps "PathLead"         ) (parseInside' $ boolText childText)
    ap_PathRhythm        <- ap_PathRhythm        =. childTag (inSpace cstSpaceArrProps "PathRhythm"       ) (parseInside' $ boolText childText)
    ap_RouteMask         <- ap_RouteMask         =. childTag (inSpace cstSpaceArrProps "RouteMask"        ) (parseInside' $ intText childText)
    return ArrangementPropeties{..}

data SongInfo = SongInfo
  { si_Album               :: T.Text
  , si_AlbumSort           :: T.Text
  , si_Artist              :: T.Text
  , si_ArtistSort          :: T.Text
  , si_AverageTempo        :: Int -- can be fractional?
  , si_JapaneseArtistName  :: T.Text
  , si_JapaneseSongName    :: T.Text
  , si_SongDisplayName     :: T.Text
  , si_SongDisplayNameSort :: T.Text
  , si_SongYear            :: Int -- can be empty?
  } deriving (Eq, Show)

instance IsInside SongInfo where
  insideCodec = do
    si_Album               <- si_Album               =. childTag (inSpace cstSpaceMain "Album"              ) (parseInside' childText)
    si_AlbumSort           <- si_AlbumSort           =. childTag (inSpace cstSpaceMain "AlbumSort"          ) (parseInside' childText)
    si_Artist              <- si_Artist              =. childTag (inSpace cstSpaceMain "Artist"             ) (parseInside' childText)
    si_ArtistSort          <- si_ArtistSort          =. childTag (inSpace cstSpaceMain "ArtistSort"         ) (parseInside' childText)
    si_AverageTempo        <- si_AverageTempo        =. childTag (inSpace cstSpaceMain "AverageTempo"       ) (parseInside' $ intText childText)
    si_JapaneseArtistName  <- si_JapaneseArtistName  =. childTag (inSpace cstSpaceMain "JapaneseArtistName" ) (parseInside' childText)
    si_JapaneseSongName    <- si_JapaneseSongName    =. childTag (inSpace cstSpaceMain "JapaneseSongName"   ) (parseInside' childText)
    si_SongDisplayName     <- si_SongDisplayName     =. childTag (inSpace cstSpaceMain "SongDisplayName"    ) (parseInside' childText)
    si_SongDisplayNameSort <- si_SongDisplayNameSort =. childTag (inSpace cstSpaceMain "SongDisplayNameSort") (parseInside' childText)
    si_SongYear            <- si_SongYear            =. childTag (inSpace cstSpaceMain "SongYear"           ) (parseInside' $ intText childText)
    return SongInfo{..}

data AggregateGraph = AggregateGraph
  { ag_UUID    :: T.Text
  , ag_File    :: T.Text
  , ag_Version :: Maybe T.Text
  } deriving (Eq, Show)

instance IsInside AggregateGraph where
  insideCodec = do
    useNamespace Nothing cstSpaceAggGraph
    ag_UUID    <- ag_UUID    =. childTag (inSpace cstSpaceAggGraph "UUID"   ) (parseInside' childText)
    ag_File    <- ag_File    =. childTag (inSpace cstSpaceAggGraph "File"   ) (parseInside' childText)
    ag_Version <- ag_Version =. childTag (inSpace cstSpaceAggGraph "Version") (parseInside' $ nillable childText)
    return AggregateGraph{..}

data ToolkitInfo = ToolkitInfo
  { tk_PackageAuthor  :: Maybe T.Text
  , tk_PackageComment :: Maybe T.Text
  , tk_PackageRating  :: Maybe T.Text
  , tk_PackageVersion :: Maybe T.Text
  , tk_ToolkitVersion :: Maybe T.Text
  } deriving (Eq, Show)

instance IsInside ToolkitInfo where
  insideCodec = do
    tk_PackageAuthor  <- tk_PackageAuthor  =. childTag (inSpace cstSpaceMain "PackageAuthor" ) (parseInside' $ nillable childText)
    tk_PackageComment <- tk_PackageComment =. childTag (inSpace cstSpaceMain "PackageComment") (parseInside' $ nillable childText)
    tk_PackageRating  <- tk_PackageRating  =. childTag (inSpace cstSpaceMain "PackageRating" ) (parseInside' $ nillable childText)
    tk_PackageVersion <- tk_PackageVersion =. childTag (inSpace cstSpaceMain "PackageVersion") (parseInside' $ nillable childText)
    tk_ToolkitVersion <- tk_ToolkitVersion =. childTag (inSpace cstSpaceMain "ToolkitVersion") (parseInside' $ nillable childText)
    return ToolkitInfo{..}

parseProject :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m DLCPackageData
parseProject f = inside ("Loading: " <> f) $ do
  stackIO (T.readFile f) >>= \t -> case parseXMLDoc t of
    Nothing  -> fatal "Couldn't parse XML"
    Just elt -> mapStackTraceT (`runReaderT` elt)
      $ codecIn $ isTag (inSpace cstSpaceMain "DLCPackageData") $ parseInside' insideCodec

parseTone :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m Tone2014
parseTone f = inside ("Loading: " <> f) $ do
  stackIO (T.readFile f) >>= \t -> case parseXMLDoc t of
    Nothing  -> fatal "Couldn't parse XML"
    Just elt -> mapStackTraceT (`runReaderT` elt)
      $ codecIn $ isTag (inSpace cstSpaceTone2014 "Tone2014") $ parseInside' insideCodec

writeProject :: (MonadIO m) => FilePath -> DLCPackageData -> m ()
writeProject f proj = liftIO $ do
  let xml = makeDoc (inSpace cstSpaceMain "DLCPackageData")
        $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) DLCPackageData) proj
  B.writeFile f $ TE.encodeUtf8 $ T.pack $ ppTopElement xml

writeTone :: (MonadIO m) => FilePath -> Tone2014 -> m ()
writeTone f tone = liftIO $ do
  let xml = makeDoc (inSpace cstSpaceTone2014 "Tone2014")
        $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) Tone2014) tone
  B.writeFile f $ TE.encodeUtf8 $ T.pack $ ppTopElement xml

toneBytes :: Tone2014 -> B.ByteString
toneBytes tone = let
  xml = makeDoc (inSpace cstSpaceTone2014 "Tone2014")
    $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) Tone2014) tone
  in TE.encodeUtf8 $ T.pack $ ppTopElement xml
