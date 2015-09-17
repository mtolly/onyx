{-# LANGUAGE TemplateHaskell #-}
module Resources where

import           Data.FileEmbed               (embedDir, embedFile)
import qualified Data.ByteString as B

magmaFiles :: [(FilePath, B.ByteString)]
magmaFiles = $(embedDir "vendors/magma/")

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(embedDir "vendors/xbox/rb3pkg/bin/Release/")

emptyMilo :: B.ByteString
emptyMilo = $(embedFile "vendors/empty.milo_xbox")
