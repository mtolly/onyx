module X360 (rb3pkg, rb2pkg) where

import           Control.Monad     (forM_)
import qualified Data.ByteString   as B
import qualified Data.Text         as T
import           Development.Shake
import           Magma             (withSystemTempDirectory)
import           Resources         (rb3pkgFiles)
import           System.FilePath   ((</>))
import           System.Info       (os)

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

rb3pkg :: T.Text -> T.Text -> FilePath -> FilePath -> Action ()
rb3pkg title desc dir fout = withSystemTempDirectory "rb3pkg" $ \tmp -> do
  liftIO $ forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  withExe callProcess (tmp </> "rb3pkg.exe")
    [ "-p", T.unpack title
    , "-d", T.unpack desc
    , "-f", dir
    , fout
    ]

rb2pkg :: T.Text -> T.Text -> FilePath -> FilePath -> Action ()
rb2pkg title desc dir fout = withSystemTempDirectory "rb2pkg" $ \tmp -> do
  liftIO $ forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  withExe callProcess (tmp </> "rb3pkg.exe")
    [ "-p", T.unpack title
    , "-d", T.unpack desc
    , "-f", dir
    , "-i", show (0x45410869 :: Integer)
    , fout
    ]
