{-# LANGUAGE TemplateHaskell #-}
module Resources where

import           Data.FileEmbed               (embedDir)
import qualified Data.ByteString as B

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(embedDir "vendors/magma/")

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(embedDir "vendors/xbox/rb3pkg/bin/Release/")

