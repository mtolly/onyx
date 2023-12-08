{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Zip.Load where

import           Control.Exception             (bracket)
import           Control.Monad                 (forM, forM_, when)
import qualified Data.ByteString               as B
import qualified Data.ByteString.Builder       as BB
import qualified Data.ByteString.Lazy          as BL
import           Data.ByteString.Lazy.Internal (smallChunkSize)
import           Data.Foldable                 (toList)
import           Data.Functor                  (void)
import           Data.Maybe                    (catMaybes)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import           Data.Text.Encoding.Error      (lenientDecode)
import           Foreign                       (allocaBytes, castPtr)
import           Onyx.Util.Handle
import           Onyx.Zip

withZip :: B.ByteString -> (Zip -> IO a) -> IO a
withZip path f = do
  bracket (zipEitherInt $ zip_open path zip_RDONLY) (mapM_ zip_close) $ \case
    Left err -> fail $ "Failed to open zip file at " <> show path <> ": " <> show err
    Right z  -> f z

loadZipFolder
  :: Maybe B.ByteString
  -> FilePath
  -> (FilePath -> FilePath -> IO BL.ByteString -> a)
  -> IO (Folder T.Text a)
loadZipFolder password path eachFile = do
  let path' = TE.encodeUtf8 $ T.pack path
  files <- withZip path' $ \z -> do
    n <- zip_get_num_entries z mempty
    zipAlloca $ \stat -> do
      fmap catMaybes $ forM [0 .. fromIntegral n - 1] $ \i -> do
        zip_stat_index z i mempty stat >>= \code -> case code of
          0 -> do
            info <- getZipStat stat
            return $ do
              filePath <- TE.decodeUtf8With lenientDecode <$> zipStatName info
              pathList <- splitPath filePath
              Just (T.unpack filePath, pathList, i)
          _ -> return Nothing
  return $ fromFiles $ do
    (filePath, pathList, i) <- files
    -- I'm making an assumption that the indexes won't change between zip loads
    let output = eachFile path filePath $ withZip path' $ \z -> do
          void $ zip_set_default_password z password
          bracket (zip_fopen_index z i mempty) (mapM_ zip_fclose) $ \mfile -> case mfile of
            Nothing   -> do
              err <- zip_get_error z >>= getZipError
              fail $ "Failed to open file " <> show filePath <> " in zip file " <> show path <> ": " <> show err
            Just file -> readZipFile file
    return (pathList, output)

loadZipReadables :: Maybe B.ByteString -> FilePath -> IO (Folder T.Text Readable)
loadZipReadables password path = loadZipFolder password path zipFileReadable

loadZipLazyBytes :: Maybe B.ByteString -> FilePath -> IO (Folder T.Text (IO BL.ByteString))
loadZipLazyBytes password path = loadZipFolder password path $ \_ _ -> id

loadZipStrictBytes :: Maybe B.ByteString -> FilePath -> IO (Folder T.Text (IO B.ByteString))
loadZipStrictBytes password path = loadZipFolder password path $ \_ _ -> fmap BL.toStrict

zipFileReadable :: FilePath -> FilePath -> IO BL.ByteString -> Readable
zipFileReadable path filePath reader = makeHandle (path <> " | " <> filePath) $ reader >>= byteStringSimpleHandle

readZipFile :: ZipFile -> IO BL.ByteString
readZipFile zf = allocaBytes smallChunkSize $ \buf -> let
  loop !builder = zip_fread zf buf (fromIntegral smallChunkSize) >>= \n -> case n of
    0         -> return $ BB.toLazyByteString builder
    bytesRead -> do
      chunk <- B.packCStringLen (castPtr buf, fromIntegral bytesRead)
      loop $ builder <> BB.byteString chunk
  in loop mempty

-- Only loads content from simple files for now.
-- May want Readable interface in the future but need to either use callbacks or be smart about buffers
makeZipFile :: FilePath -> Folder T.Text FilePath -> IO ()
makeZipFile path folder = do
  let flags = zip_CREATE <> zip_TRUNCATE
  bracket (zipEitherInt $ zip_open (TE.encodeUtf8 $ T.pack path) flags) (mapM_ zip_close) $ \case
    Left err -> fail $ "Failed to open zip file at " <> show path <> ": " <> show err
    Right z  -> do
      forM_ (allFiles folder) $ \(pathList, srcFile) -> do
        let filePath = TE.encodeUtf8 $ T.intercalate "/" $ toList pathList
            srcPath = TE.encodeUtf8 $ T.pack srcFile
        zip_source_file z srcPath 0 zip_LENGTH_TO_END >>= \case
          Nothing -> do
            err <- zip_get_error z >>= getZipError
            fail $ "Failed to " <> show srcPath <> " into source for zip " <> show path <> ": " <> show err
          Just src -> do
            n <- zip_file_add z filePath src zip_FL_ENC_UTF_8
            when (n < 0) $ do
              err <- zip_get_error z >>= getZipError
              fail $ "Failed to add file " <> show filePath <> " to zip " <> show path <> ": " <> show err
