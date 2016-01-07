{-# LANGUAGE TemplateHaskell #-}
module Resources where

import           Control.Arrow                (first)
import           Data.FileEmbed               (embedDir, embedFile)
import qualified Data.ByteString as B
import           System.FilePath              ((</>))

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(embedDir "vendors/magma/")

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(embedDir "vendors/xbox/rb3pkg/bin/Release/")

emptyMilo :: B.ByteString
emptyMilo = $(embedFile "vendors/empty.milo_xbox")

mogg2ogg :: B.ByteString
mogg2ogg = B.empty
-- mogg2ogg = $(embedFile "vendors/mogg2ogg.exe")

webDisplay :: [(FilePath, B.ByteString)]
webDisplay = concat
  [ [("index.html", $(embedFile "../../web-display/www/index.html"))]
  , [("app.min.js", $(embedFile "../../web-display/www/app.min.js"))]
  , [("runmain.js", $(embedFile "../../web-display/www/runmain.js"))]
  , map (first ("images" </>)) $(embedDir "../../web-display/www/images")
  , map (first ("lib"    </>)) $(embedDir "../../web-display/www/lib")
  ]
