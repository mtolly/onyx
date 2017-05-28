{-# LANGUAGE TemplateHaskell #-}
module Resources where

import qualified Codec.Picture      as P
import           Control.Arrow      (first)
import qualified Data.ByteString    as B
import           Data.FileEmbed     (embedDir, embedFile, makeRelativeToProject)
import           System.Environment (getExecutablePath)
import           System.FilePath    (takeDirectory, (</>))

magmaV1Dir, magmaV2Dir, magmaCommonDir, magmaOgg2MoggDir :: IO FilePath
magmaV1Dir       = (</> "magma-v1")       . takeDirectory <$> getExecutablePath
magmaV2Dir       = (</> "magma-v2")       . takeDirectory <$> getExecutablePath
magmaCommonDir   = (</> "magma-common")   . takeDirectory <$> getExecutablePath
magmaOgg2MoggDir = (</> "magma-ogg2mogg") . takeDirectory <$> getExecutablePath

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(makeRelativeToProject "vendors/xbox/rb3pkg/bin/Release/" >>= embedDir)

emptyMilo :: B.ByteString
emptyMilo = $(makeRelativeToProject "vendors/empty.milo_xbox" >>= embedFile)

emptyMiloRB2 :: B.ByteString
emptyMiloRB2 = $(makeRelativeToProject "vendors/empty-rb2.milo_xbox" >>= embedFile)

emptyWeightsRB2 :: B.ByteString
emptyWeightsRB2 = $(makeRelativeToProject "vendors/empty-rb2_weights.bin" >>= embedFile)

webDisplay :: [(FilePath, B.ByteString)]
webDisplay = concat
  [ [("index.html", $(makeRelativeToProject "../player/www/index.html" >>= embedFile))]
  , [("app.min.js", $(makeRelativeToProject "../player/www/app.min.js" >>= embedFile))]
  , map (first ("images" </>)) $(makeRelativeToProject "../player/www/images" >>= embedDir)
  , map (first ("lib" </>)) $(makeRelativeToProject "../player/www/lib" >>= embedDir)
  ]

onyxAlbum :: P.Image P.PixelRGB8
onyxAlbum = case P.decodeImage $(makeRelativeToProject "vendors/album.png" >>= embedFile) of
  Left  err -> error $ "panic! couldn't decode default album art into image: " ++ err
  Right dyn -> P.convertRGB8 dyn

pentatonicTTF :: B.ByteString
pentatonicTTF = $(makeRelativeToProject "vendors/Pentatonic.ttf" >>= embedFile)
