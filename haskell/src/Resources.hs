{-# LANGUAGE TemplateHaskell #-}
module Resources where

import           Control.Arrow   (first)
import qualified Data.ByteString as B
import           Data.FileEmbed  (embedDir, embedFile, makeRelativeToProject)
import           System.FilePath ((</>))

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(makeRelativeToProject "vendors/magma/" >>= embedDir)

magmaV1Files :: [(FilePath, B.ByteString)]
magmaV1Files = $(makeRelativeToProject "vendors/magma-v1/" >>= embedDir)

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
