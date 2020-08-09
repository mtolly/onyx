{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
module Amplitude.PS2.Ark where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import Data.Binary.Get (runGet, getInt32le)
import Data.Binary.Put (runPut, putInt32le)
import Data.Int (Int32)
import System.IO (withBinaryFile, IOMode(..), hSeek, SeekMode(..), hTell)
import Control.Monad.Extra (replicateM, forM_, concatForM, forM)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Data.List.Extra (nubOrd)
import Codec.Compression.GZip (decompress)
import Data.List (isSuffixOf)
import Data.Foldable (toList)

data FileEntry = FileEntry
  { fe_offset  :: Int32 -- offset in bytes into the full .ark file
  , fe_name    :: B.ByteString
  , fe_folder  :: Maybe B.ByteString
  , fe_size    :: Int32
  , fe_inflate :: Int32 -- inflated size, or 0 if not gzipped.
  -- Only ".bmp.gz" files in "gen" folders are gzipped (and they all are).
  -- (Not actually .bmp -- likely the usual HMX image format like .png_xbox)
  } deriving (Eq, Show)

readFileEntries :: FilePath -> IO [FileEntry]
readFileEntries ark = withBinaryFile ark ReadMode $ \h -> do
  let read32 = runGet getInt32le . BL.fromStrict <$> B.hGet h 4
  2 <- read32
  entryCount <- read32
  entryBytes <- B.hGet h $ fromIntegral entryCount * 20
  stringSize <- read32
  stringBytes <- B.hGet h $ fromIntegral stringSize
  offsetCount <- read32
  offsets <- replicateM (fromIntegral offsetCount) read32
  let findString i = B.takeWhile (/= 0) $ B.drop (fromIntegral $ offsets !! fromIntegral i) stringBytes
  return $ flip runGet (BL.fromStrict entryBytes) $ replicateM (fromIntegral entryCount) $ do
    fe_offset <- getInt32le
    fe_name <- findString <$> getInt32le
    fe_folder <- (\i -> if i == -1 then Nothing else Just $ findString i) <$> getInt32le
    fe_size <- getInt32le
    fe_inflate <- getInt32le
    return FileEntry{..}

extractArk :: FilePath -> FilePath -> IO ()
extractArk ark dout = do
  entries <- readFileEntries ark
  withBinaryFile ark ReadMode $ \h -> forM_ entries $ \entry -> do
    let dir = maybe dout ((dout </>) . B8.unpack) $ fe_folder entry
        fout = dir </> B8.unpack (fe_name entry)
    createDirectoryIfMissing True dir
    hSeek h AbsoluteSeek $ fromIntegral $ fe_offset entry
    bytes <- BL.hGet h $ fromIntegral $ fe_size entry
    BL.writeFile fout bytes

data FoundFile = FoundFile
  { ff_onDisk    :: FilePath
  , ff_arkName   :: B.ByteString
  , ff_arkParent :: Maybe B.ByteString
  , ff_gzip      :: Bool
  } deriving (Eq, Show)

traverseFolder :: FilePath -> IO [FoundFile]
traverseFolder = go Nothing where
  go parent dir = do
    files <- listDirectory dir
    concatForM files $ \f -> do
      let fullPath = dir </> f
      doesDirectoryExist fullPath >>= \case
        True -> go (Just $ maybe f (<> "/" <> f) parent) fullPath
        False -> do
          return [FoundFile
            { ff_onDisk = fullPath
            , ff_arkName = B8.pack f
            , ff_arkParent = B8.pack <$> parent
            , ff_gzip = ".gz" `isSuffixOf` f
            }]

makeStringBank :: [B.ByteString] -> (BL.ByteString, [Int32])
makeStringBank = go (BL.singleton 0) [] where
  go !bank offs []       = (bank, reverse offs)
  go !bank offs (s : ss) = go
    (bank <> BL.fromStrict s <> BL.singleton 0)
    (fromIntegral (BL.length bank) : offs)
    ss

createArk :: FilePath -> FilePath -> IO ()
createArk dout ark = do
  files <- traverseFolder dout
  let strings = nubOrd $ files >>= \ff -> ff_arkName ff : toList (ff_arkParent ff)
      (stringBank, offsets) = makeStringBank strings
      stringToIndex = HM.fromList $ zip strings [0..]
      getStringIndex str = case HM.lookup str stringToIndex of
        Nothing -> fail "panic! couldn't find string offset in bank"
        Just i  -> return i
      fileCount = length files
      stringCount = HM.size stringToIndex
      sizeBeforeFiles = sum
        [ 4
        , 4, fromIntegral $ 20 * fileCount -- file entries
        , 4, fromIntegral $ BL.length stringBank -- string bank
        , 4, fromIntegral $ 4 * stringCount -- string offset list
        ]
  withBinaryFile ark WriteMode $ \h -> do
    hSeek h AbsoluteSeek sizeBeforeFiles
    entries <- forM files $ \ff -> do
      posn <- hTell h
      contents <- BL.fromStrict <$> B.readFile (ff_onDisk ff)
      BL.hPut h contents
      return FileEntry
        { fe_offset = fromIntegral posn
        , fe_name = ff_arkName ff
        , fe_folder = ff_arkParent ff
        , fe_size = fromIntegral $ BL.length contents
        , fe_inflate = if ff_gzip ff
          then fromIntegral $ BL.length $ decompress contents
          else 0
        }
    hSeek h AbsoluteSeek 0
    let write32 = BL.hPut h . runPut . putInt32le
    write32 2
    write32 $ fromIntegral fileCount
    forM_ entries $ \entry -> do
      write32 $ fe_offset entry
      getStringIndex (fe_name entry) >>= write32
      maybe (return (-1)) getStringIndex (fe_folder entry) >>= write32
      write32 $ fe_size entry
      write32 $ fe_inflate entry
    write32 $ fromIntegral $ BL.length stringBank
    BL.hPut h stringBank
    write32 $ fromIntegral stringCount
    mapM_ write32 offsets
