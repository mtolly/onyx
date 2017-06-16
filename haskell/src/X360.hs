{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module X360 (rb3pkg, rb2pkg, stfsFolder) where

import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import           Data.List                      (inits, intercalate, nub, sort)
import qualified Data.Text                      as T
import           Data.Word                      (Word32)
import           Resources                      (rb3Thumbnail, xboxKV)
import           System.Directory.Extra         (listFilesRecursive)
import           System.FilePath                (makeRelative, splitDirectories)
import           System.IO                      (IOMode (ReadMode),
                                                 SeekMode (AbsoluteSeek), hSeek,
                                                 withBinaryFile)
import           XboxInternals                  (buildSTFSPackage)

directoryChain :: FilePath -> [FilePath]
directoryChain f = let
  chain = splitDirectories f
  good = filter (\c -> not $ null c || length c == length chain) $ inits chain
  in map (intercalate "/") good

rbpkg :: (MonadIO m) => String -> Word32 -> T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rbpkg game tid title desc dir fout = do
  files <- liftIO $ listFilesRecursive dir
  let dirs = sort $ nub $ concatMap (directoryChain . makeRelative dir) files
  merr <- liftIO $ buildSTFSPackage
    (T.unpack title)
    (T.unpack desc)
    "Harmonix"
    game
    tid
    dirs
    [ (f, makeRelative dir f) | f <- files ]
    rb3Thumbnail
    rb3Thumbnail
    xboxKV
    fout
  inside "XboxInternals STFS package creation" $ mapM_ fatal merr

rb3pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb3pkg = rbpkg "Rock Band 3" 0x45410914

rb2pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb2pkg = rbpkg "Rock Band 2" 0x45410869

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
