module X360 (rb3pkg) where

import           Control.Monad     (forM_)
import qualified Data.ByteString   as B
import           Development.Shake
import           Magma             (withSystemTempDirectory)
import           System.FilePath   ((</>))
import           System.Info       (os)
import Resources (rb3pkgFiles)

withExe :: (FilePath -> [String] -> a) -> FilePath -> [String] -> a
withExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args

callProcess :: FilePath -> [String] -> Action ()
callProcess = command_
  [ Stdin ""
  , WithStdout True
  , WithStderr True
  , EchoStdout False
  , EchoStderr False
  ]

rb3pkg :: String -> String -> FilePath -> FilePath -> Action ()
rb3pkg title desc dir fout = withSystemTempDirectory "rb3pkg" $ \tmp -> do
  liftIO $ forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  withExe callProcess (tmp </> "rb3pkg.exe")
    [ "-p", title
    , "-d", desc
    , "-f", dir
    , fout
    ]
