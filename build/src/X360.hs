{-# LANGUAGE TemplateHaskell #-}
module X360 (rb3pkg) where

import Data.FileEmbed (embedDir)
import qualified Data.ByteString as B
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process (callProcess)
import Control.Monad (forM_)
import System.FilePath ((</>))

rb3pkgFiles :: [(FilePath, B.ByteString)]
rb3pkgFiles = $(embedDir "xbox/rb3pkg/bin/Release/")

withExe :: (FilePath -> [String] -> IO a) -> FilePath -> [String] -> IO a
withExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args

rb3pkg :: String -> String -> FilePath -> FilePath -> IO ()
rb3pkg title desc dir fout = withSystemTempDirectory "rb3pkg" $ \tmp -> do
  forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  withExe callProcess (tmp </> "rb3pkg.exe")
    [ "-p", title
    , "-d", desc
    , "-f", dir
    , fout
    ]
