module X360 (rb3pkg, rb2pkg) where

import           Control.Monad   (forM_)
import qualified Data.ByteString as B
import qualified Data.Text       as T
import           Magma           (presentExitCode)
import           Resources       (rb3pkgFiles)
import           System.FilePath ((</>))
import           System.Info     (os)
import           System.IO.Temp  (withSystemTempDirectory)
import           System.Process

withDotNetExe :: (FilePath -> [String] -> a) -> FilePath -> [String] -> a
withDotNetExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args

rb3pkg :: T.Text -> T.Text -> FilePath -> FilePath -> IO String
rb3pkg title desc dir fout = withSystemTempDirectory "rb3pkg" $ \tmp -> do
  forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , fout
        ]
  readCreateProcessWithExitCode createProc "" >>= presentExitCode "rb3pkgIO"

rb2pkg :: T.Text -> T.Text -> FilePath -> FilePath -> IO String
rb2pkg title desc dir fout = withSystemTempDirectory "rb2pkg" $ \tmp -> do
  forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , "-i", show (0x45410869 :: Integer)
        , fout
        ]
  readCreateProcessWithExitCode createProc "" >>= presentExitCode "rb2pkg"
