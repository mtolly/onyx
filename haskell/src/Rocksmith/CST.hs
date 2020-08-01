{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.CST where

import           Control.Monad                  (void)
import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx.XML
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import           Data.Fixed                     (Milli)
import           Data.Functor.Identity          (Identity)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.IO                   as T
import           Text.XML.Light

cstSpaceW3, cstSpaceMain, cstSpaceArrProps, cstSpaceSng2014, cstSpaceAggGraph, cstSpaceTone, cstSpaceTone2014, cstSpaceArrays :: String
cstSpaceW3 = "http://www.w3.org/2001/XMLSchema-instance"
cstSpaceMain = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage"
cstSpaceArrProps = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.XML"
cstSpaceSng2014 = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.Sng2014HSL"
cstSpaceAggGraph = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.AggregateGraph"
cstSpaceTone = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest.Tone"
cstSpaceTone2014 = "http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest2014.Tone"
cstSpaceArrays = "http://schemas.microsoft.com/2003/10/Serialization/Arrays"

data DLCPackageData = DLCPackageData
  { dlc_AlbumArtPath      :: T.Text
  , dlc_AppId             :: Int
  , dlc_Arrangements      :: () -- TODO
  , dlc_ArtFiles          :: () -- TODO
  , dlc_DefaultShowlights :: Bool
  , dlc_GameVersion       :: T.Text
  , dlc_Inlay             :: () -- TODO  i:nil="true" />
  , dlc_Mac               :: Bool
  , dlc_Name              :: T.Text
  , dlc_OggPath           :: T.Text
  , dlc_OggPreviewPath    :: T.Text
  , dlc_OggQuality        :: Milli
  , dlc_PS3               :: Bool
  , dlc_Pc                :: Bool
  , dlc_PreviewVolume     :: Milli
  , dlc_SignatureType     :: T.Text
  , dlc_SongInfo          :: () -- TODO >
  , dlc_Tones             :: () -- TODO  xmlns:d2p1="http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest.Tone" />
  , dlc_TonesRS2014       :: () -- TODO  xmlns:d2p1="http://schemas.datacontract.org/2004/07/RocksmithToolkitLib.DLCPackage.Manifest2014.Tone">
  , dlc_ToolkitInfo       :: () -- TODO >
  , dlc_Version           :: T.Text
  , dlc_Volume            :: Milli
  , dlc_XBox360           :: Bool
  , dlc_XBox360Licenses   :: () -- TODO  />
  } deriving (Eq, Show)

instance IsInside DLCPackageData where
  insideCodec = do
    useNamespace (Just "i") cstSpaceW3
    useNamespace (Just "") cstSpaceMain
    dlc_AlbumArtPath      <- dlc_AlbumArtPath      =. childTag (inSpace cstSpaceMain "AlbumArtPath"     ) (parseInside' childText)
    dlc_AppId             <- dlc_AppId             =. childTag (inSpace cstSpaceMain "AppId"            ) (parseInside' $ intText childText)
    dlc_DefaultShowlights <- dlc_DefaultShowlights =. childTag (inSpace cstSpaceMain "DefaultShowlights") (parseInside' $ boolWordText childText)
    dlc_GameVersion       <- dlc_GameVersion       =. childTag (inSpace cstSpaceMain "GameVersion"      ) (parseInside' childText)
    dlc_Mac               <- dlc_Mac               =. childTag (inSpace cstSpaceMain "Mac"              ) (parseInside' $ boolWordText childText)
    dlc_Name              <- dlc_Name              =. childTag (inSpace cstSpaceMain "Name"             ) (parseInside' childText)
    dlc_OggPath           <- dlc_OggPath           =. childTag (inSpace cstSpaceMain "OggPath"          ) (parseInside' childText)
    dlc_OggPreviewPath    <- dlc_OggPreviewPath    =. childTag (inSpace cstSpaceMain "OggPreviewPath"   ) (parseInside' childText)
    dlc_PS3               <- dlc_PS3               =. childTag (inSpace cstSpaceMain "PS3"              ) (parseInside' $ boolWordText childText)
    dlc_Pc                <- dlc_Pc                =. childTag (inSpace cstSpaceMain "Pc"               ) (parseInside' $ boolWordText childText)
    dlc_Version           <- dlc_Version           =. childTag (inSpace cstSpaceMain "Version"          ) (parseInside' childText)
    dlc_XBox360           <- dlc_XBox360           =. childTag (inSpace cstSpaceMain "XBox360"          ) (parseInside' $ boolWordText childText)
    let dlc_Arrangements = ()
        dlc_ArtFiles = ()
        dlc_Inlay = ()
        dlc_OggQuality = 0
        dlc_PreviewVolume = 0
        dlc_SignatureType = ""
        dlc_SongInfo = ()
        dlc_Tones = ()
        dlc_TonesRS2014 = ()
        dlc_ToolkitInfo = ()
        dlc_Volume = 0
        dlc_XBox360Licenses = ()
    return DLCPackageData{..}

parseProject :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m DLCPackageData
parseProject f = inside ("Loading: " <> f) $ do
  stackIO (T.readFile f) >>= \t -> case parseXMLDoc t of
    Nothing  -> fatal "Couldn't parse XML"
    Just elt -> mapStackTraceT (`runReaderT` elt)
      $ codecIn $ isTag (inSpace cstSpaceMain "DLCPackageData") $ parseInside' insideCodec

writeProject :: (MonadIO m) => FilePath -> DLCPackageData -> m ()
writeProject f proj = liftIO $ do
  let xml = makeDoc (inSpace cstSpaceMain "DLCPackageData")
        $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) DLCPackageData) proj
  B.writeFile f $ TE.encodeUtf8 $ T.pack $ ppTopElement xml
