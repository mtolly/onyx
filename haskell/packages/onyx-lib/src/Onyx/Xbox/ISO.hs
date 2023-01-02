{-
Ported from maxton's GameArchives
https://github.com/maxton/GameArchives/blob/97875280485af067da6adebed5299beaac302fda/Library/XISO/XISOPackage.cs
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Xbox.ISO where

import           Control.Monad.Extra  (firstJustM)
import           Data.Binary.Get
import           Data.Bits            (testBit)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS       (runGetM)
import           System.IO

sectorLength :: Integer
sectorLength = 0x800

data XboxHeader = XboxHeader
  { partitionOffset :: Integer
  , rootSector      :: Word32
  , rootSize        :: Word32
  } deriving (Show)

rootOffset :: XboxHeader -> Integer
rootOffset hdr = hdr.partitionOffset + fromIntegral hdr.rootSector * sectorLength

findXboxHeader :: Handle -> IO (Maybe XboxHeader)
findXboxHeader h = do
  len <- hFileSize h
  let possibleOffsets = [0x00000000, 0x0000FB20, 0x00020600, 0x02080000, 0x0FD90000]
  flip firstJustM possibleOffsets $ \off -> do
    let seekTo = off + 32 * sectorLength
    if seekTo + 20 >= len
      then return Nothing
      else do
        hSeek h AbsoluteSeek seekTo
        magic <- B.hGet h 20
        nums <- BL.hGet h 8
        if magic == "MICROSOFT*XBOX*MEDIA"
          then fmap Just $ flip runGetM nums $ do
            rootSector <- getWord32le
            rootSize <- getWord32le
            return XboxHeader{ partitionOffset = off, .. }
          else return Nothing

parseRoot :: Handle -> XboxHeader -> IO (Folder B.ByteString (XDVDFSEntry B.ByteString))
parseRoot h hdr = parseTree h hdr (rootOffset hdr) 0

parseTree :: Handle -> XboxHeader -> Integer -> Integer -> IO (Folder B.ByteString (XDVDFSEntry B.ByteString))
parseTree h hdr baseOffset entryOffset = do
  entry <- readEntry h baseOffset entryOffset
  thisTree <- if testBit entry.attribs 4
    then do
      subDir <- parseTree h hdr (hdr.partitionOffset + fromIntegral entry.sector * sectorLength) 0
      return Folder { folderFiles = [], folderSubfolders = [(entry.name, subDir)] }
    else return Folder { folderFiles = [(entry.name, entry)], folderSubfolders = [] }
  left <- if entry.left /= 0
    then parseTree h hdr baseOffset $ fromIntegral entry.left * 4
    else return mempty
  right <- if entry.right /= 0
    then parseTree h hdr baseOffset $ fromIntegral entry.right * 4
    else return mempty
  return $ thisTree <> left <> right

data XDVDFSEntry a = XDVDFSEntry
  { left    :: Word16
  , right   :: Word16
  , sector  :: Word32
  , length_ :: Word32
  , attribs :: Word8
  , name    :: a
  } deriving (Show)

readEntry :: Handle -> Integer -> Integer -> IO (XDVDFSEntry B.ByteString)
readEntry h baseOffset entryOffset = do
  hSeek h AbsoluteSeek $ baseOffset + entryOffset
  bs <- BL.hGet h 14
  entry <- flip runGetM bs $ do
    left    <- getWord16le
    right   <- getWord16le
    sector  <- getWord32le
    length_ <- getWord32le
    attribs <- getWord8
    name    <- getWord8
    return XDVDFSEntry{..}
  name <- B.hGet h $ fromIntegral entry.name
  return entry { name = name }

getXboxFile :: XboxHeader -> XDVDFSEntry B.ByteString -> Readable -> Readable
getXboxFile hdr entry = subHandle
  id -- TODO
  (hdr.partitionOffset + fromIntegral entry.sector * sectorLength)
  (Just $ fromIntegral entry.length_)

loadXboxISO :: Readable -> IO (Maybe (Folder B.ByteString Readable))
loadXboxISO r = useHandle r $ \h -> findXboxHeader h >>= \case
  Nothing -> return Nothing
  Just hdr -> Just <$> do
    folder <- parseRoot h hdr
    return $ fmap (\entry -> getXboxFile hdr entry r) folder
