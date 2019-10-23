{-# LANGUAGE TemplateHaskell #-}
module Resources where

import qualified Codec.Picture      as P
import           Control.Arrow      (first)
import qualified Data.ByteString    as B
import qualified Data.DTA           as D
import           Data.FileEmbed     (embedDir, embedFile, makeRelativeToProject)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import           System.Directory   (getHomeDirectory)
import           System.Environment (getExecutablePath)
import           System.FilePath    (takeDirectory, takeFileName, (</>))

getResourcesPath :: IO FilePath
getResourcesPath = do
  exe <- getExecutablePath
  if takeFileName exe == "ghc"
    then (</> ".local/bin") <$> getHomeDirectory -- we're in ghci, use installed resources
    else return $ takeDirectory exe

magmaV1Dir, magmaV2Dir, magmaCommonDir, rb3Updates, kanwadict, itaijidict :: IO FilePath
magmaV1Dir       = (</> "magma-v1")       <$> getResourcesPath
magmaV2Dir       = (</> "magma-v2")       <$> getResourcesPath
magmaCommonDir   = (</> "magma-common")   <$> getResourcesPath
rb3Updates       = (</> "rb3-updates")    <$> getResourcesPath
kanwadict        = (</> "kanwadict")      <$> getResourcesPath
itaijidict       = (</> "itaijidict")     <$> getResourcesPath

xboxKV :: B.ByteString
xboxKV = $(makeRelativeToProject "vendors/KV.bin" >>= embedFile)

rb3Thumbnail :: B.ByteString
rb3Thumbnail = $(makeRelativeToProject "vendors/rb3.png" >>= embedFile)

rb2Thumbnail :: B.ByteString
rb2Thumbnail = $(makeRelativeToProject "vendors/rb2.png" >>= embedFile)

emptyMilo :: B.ByteString
emptyMilo = $(makeRelativeToProject "vendors/empty.milo_xbox" >>= embedFile)

emptyMiloRB2 :: B.ByteString
emptyMiloRB2 = $(makeRelativeToProject "vendors/empty-rb2.milo_xbox" >>= embedFile)

emptyWeightsRB2 :: B.ByteString
emptyWeightsRB2 = $(makeRelativeToProject "vendors/empty-rb2_weights.bin" >>= embedFile)

webDisplay :: [(FilePath, B.ByteString)]
webDisplay = concat
  [ [("index.html", $(makeRelativeToProject "../player/www/index.html" >>= embedFile))]
  , [("style.css", $(makeRelativeToProject "../player/www/style.css" >>= embedFile))]
  , [("app.min.js", $(makeRelativeToProject "../player/www/app.min.js" >>= embedFile))]
  , [("customize.js", $(makeRelativeToProject "../player/www/customize.js" >>= embedFile))]
  , [("VarelaRound-Regular.ttf", $(makeRelativeToProject "../player/www/VarelaRound-Regular.ttf" >>= embedFile))]
  , map (first ("images" </>)) $(makeRelativeToProject "../player/www/images" >>= embedDir)
  ]

onyxAlbum :: P.Image P.PixelRGB8
onyxAlbum = case P.decodeImage $(makeRelativeToProject "vendors/album.png" >>= embedFile) of
  Left  err -> error $ "panic! couldn't decode default album art into image: " ++ err
  Right dyn -> P.convertRGB8 dyn

missingSongData :: D.DTA T.Text
missingSongData = D.readDTA $ decodeUtf8 $(makeRelativeToProject "vendors/missing_song_data.dta" >>= embedFile)

colorMapDrums, colorMapGRYBO, colorMapGHL :: B.ByteString
colorMapDrums = $(makeRelativeToProject "vendors/rockband_drums.png"      >>= embedFile)
colorMapGRYBO = $(makeRelativeToProject "vendors/rockband_guitarbass.png" >>= embedFile)
colorMapGHL   = $(makeRelativeToProject "vendors/rockband_ghl.png"        >>= embedFile)
