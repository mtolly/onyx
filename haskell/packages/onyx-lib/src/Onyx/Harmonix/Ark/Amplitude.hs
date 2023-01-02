{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Harmonix.Ark.Amplitude where

import           Codec.Compression.GZip (decompress)
import           Control.Monad.Extra    (concatForM, forM, forM_)
import           Data.Binary.Put        (putInt32le, putWord32le, runPut)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as B8
import qualified Data.ByteString.Lazy   as BL
import           Data.Foldable          (toList)
import qualified Data.HashMap.Strict    as HM
import           Data.List              (isSuffixOf)
import           Data.List.Extra        (nubOrd)
import           Data.Word              (Word32)
import           Onyx.Harmonix.Ark
import           System.Directory       (doesDirectoryExist, listDirectory)
import           System.FilePath        ((</>))
import           System.IO              (IOMode (..), SeekMode (..), hSeek,
                                         hTell, withBinaryFile)

data FoundFile = FoundFile
  { onDisk    :: FilePath
  , arkName   :: B.ByteString
  , arkParent :: Maybe B.ByteString
  , gzip      :: Bool
  } deriving (Eq, Show)

traverseFolder :: FilePath -> IO [FoundFile]
traverseFolder = go Nothing where
  go parent dir = do
    files <- listDirectory dir
    concatForM files $ \f -> do
      let fullPath = dir </> f
      doesDirectoryExist fullPath >>= \case
        True -> let
          -- match the way we extracted a guitar hero ark with "../../" paths
          f' = case f of
            "dotdot" -> ".."
            _        -> f
          in go (Just $ maybe f' (<> "/" <> f') parent) fullPath
        False -> do
          return [FoundFile
            { onDisk = fullPath
            , arkName = B8.pack f
            , arkParent = B8.pack <$> parent
            , gzip = ".gz" `isSuffixOf` f
            }]

makeStringBank :: [B.ByteString] -> (BL.ByteString, [Word32])
makeStringBank = go (BL.singleton 0) [] where
  go !bank offs []       = (bank, reverse offs)
  go !bank offs (s : ss) = go
    (bank <> BL.fromStrict s <> BL.singleton 0)
    (fromIntegral (BL.length bank) : offs)
    ss

createArk :: FilePath -> FilePath -> IO ()
createArk dout ark = do
  files <- traverseFolder dout
  let strings = nubOrd $ files >>= \ff -> ff.arkName : toList ff.arkParent
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
      contents <- BL.fromStrict <$> B.readFile ff.onDisk
      BL.hPut h contents
      return FileEntry
        { offset = fromIntegral posn
        , name = ff.arkName
        , folder = ff.arkParent
        , size = fromIntegral $ BL.length contents
        , inflate = if ff.gzip
          then fromIntegral $ BL.length $ decompress contents
          else 0
        }
    hSeek h AbsoluteSeek 0
    let w32 = BL.hPut h . runPut . putWord32le
        i32 = BL.hPut h . runPut . putInt32le
    w32 2
    w32 $ fromIntegral fileCount
    forM_ entries $ \entry -> do
      w32 $ fromIntegral entry.offset
      getStringIndex entry.name >>= i32
      maybe (return (-1)) getStringIndex entry.folder >>= i32
      w32 entry.size
      w32 entry.inflate
    w32 $ fromIntegral $ BL.length stringBank
    BL.hPut h stringBank
    w32 $ fromIntegral stringCount
    mapM_ w32 offsets
