{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module X360 (rb3pkg, rb2pkg, stfsFolder) where

import           Control.Monad                  (forM_)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import qualified Data.Text                      as T
import           Data.Word                      (Word32)
import           Resources                      (rb3pkgFiles)
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

rb3pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m String
rb3pkg title desc dir fout = tempDir "rb3pkg" $ \tmp -> do
  liftIO $ forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , fout
        ]
  inside "making RB3 CON package with X360" $ stackProcess createProc ""

rb2pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m String
rb2pkg title desc dir fout = tempDir "rb2pkg" $ \tmp -> do
  liftIO $ forM_ rb3pkgFiles $ \(fp, bs) -> B.writeFile (tmp </> fp) bs
  let createProc = withDotNetExe proc (tmp </> "rb3pkg.exe")
        [ "-p", T.unpack title
        , "-d", T.unpack desc
        , "-f", dir
        , "-i", show (0x45410869 :: Integer)
        , fout
        ]
  inside "making RB2 CON package with X360" $ stackProcess createProc ""

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
