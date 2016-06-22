{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Resources where

import           Control.Arrow                (first)
import           Data.FileEmbed               (embedDir, embedFile)
import qualified Data.ByteString as B
import           System.FilePath              ((</>))

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(embedDir "vendors/magma/")

magmaV1Files :: [(FilePath, B.ByteString)]
magmaV1Files = $(embedDir "vendors/magma-v1/")

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(embedDir "vendors/xbox/rb3pkg/bin/Release/")

emptyMilo :: B.ByteString
emptyMilo = $(embedFile "vendors/empty.milo_xbox")

emptyMiloRB2 :: B.ByteString
emptyMiloRB2 = $(embedFile "vendors/empty-rb2.milo_xbox")

emptyWeightsRB2 :: B.ByteString
emptyWeightsRB2 = $(embedFile "vendors/empty-rb2_weights.bin")

webDisplay :: [(FilePath, B.ByteString)]
webDisplay = concat
  [ [("index.html", $(embedFile "../../../player/www/index.html"))]
  , [("app.min.js", $(embedFile "../../../player/www/app.min.js"))]
  , map (first ("images" </>)) $(embedDir "../../../player/www/images")
  , map (first ("lib" </>)) $(embedDir "../../../player/www/lib")
  ]
