{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module X360DotNet (rb3pkg, rb2pkg, stfsFolder) where

import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.IO.Unlift        (MonadUnliftIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import           Data.Word                      (Word32)
import           Resources                      (x360RB3pkgDir)
import           System.Directory               (copyFile)
import           System.FilePath                ((</>))
import           System.Info                    (os)
import           System.IO                      (IOMode (ReadMode),
                                                 SeekMode (AbsoluteSeek), hSeek,
                                                 withBinaryFile)
import           System.Process

withDotNetExe :: (FilePath -> [String] -> a) -> FilePath -> [String] -> a
withDotNetExe f exe args = if os == "mingw32"
  then f exe args
  else f "mono" $ exe : args

rb3pkg :: (SendMessage m, MonadUnliftIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb3pkg title desc dir fout = tempDir "onyx_x360" $ \tmp -> do
  x360dir <- stackIO $ x360RB3pkgDir
  forM_ ["KV.bin", "rb3.png", "rb3pkg.exe", "X360.dll"] $ \f ->
    stackIO $ copyFile (x360dir </> f) (tmp </> f)
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , fout
        ]
  str <- inside "making RB3 CON package with X360" $ stackProcess createProc
  lg str

rb2pkg :: (SendMessage m, MonadUnliftIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb2pkg title desc dir fout = tempDir "onyx_x360" $ \tmp -> do
  x360dir <- stackIO $ x360RB3pkgDir
  forM_ ["KV.bin", "rb3.png", "rb3pkg.exe", "X360.dll"] $ \f ->
    stackIO $ copyFile (x360dir </> f) (tmp </> f)
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , "-i", show (0x45410869 :: Integer)
        , "-g", "Rock Band 2"
        , fout
        ]
  str <- inside "making RB2 CON package with X360" $ stackProcess createProc
  lg str

stfsFolder :: (MonadIO m) => FilePath -> m (Word32, Word32)
stfsFolder f = liftIO $ withBinaryFile f ReadMode $ \h -> do
  sign <- B.hGet h 4 >>= \case
    "CON " -> return 1
    "LIVE" -> return 2
    magic  -> error $ "stfsFolder: unknown magic number " ++ show magic
  hSeek h AbsoluteSeek 0x360
  [a, b, c, d] <- map fromIntegral . B.unpack <$> B.hGet h 4
  let titleID = a * 0x1000000 + b * 0x10000 + c * 0x100 + d
  return (titleID, sign)
