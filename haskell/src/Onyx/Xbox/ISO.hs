{-
Ported from maxton's GameArchives
https://github.com/maxton/GameArchives/blob/97875280485af067da6adebed5299beaac302fda/Library/XISO/XISOPackage.cs
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
  { x_PartitionOffset :: Integer
  , x_RootSector      :: Word32
  , x_RootSize        :: Word32
  } deriving (Show)

rootOffset :: XboxHeader -> Integer
rootOffset hdr = x_PartitionOffset hdr + fromIntegral (x_RootSector hdr) * sectorLength

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
            x_RootSector <- getWord32le
            x_RootSize <- getWord32le
            return XboxHeader{ x_PartitionOffset = off, .. }
          else return Nothing

parseRoot :: Handle -> XboxHeader -> IO (Folder B.ByteString (XDVDFSEntry B.ByteString))
parseRoot h hdr = parseTree h hdr (rootOffset hdr) 0

parseTree :: Handle -> XboxHeader -> Integer -> Integer -> IO (Folder B.ByteString (XDVDFSEntry B.ByteString))
parseTree h hdr baseOffset entryOffset = do
  entry <- readEntry h baseOffset entryOffset
  thisTree <- if testBit (xdvd_Attribs entry) 4
    then do
      subDir <- parseTree h hdr (x_PartitionOffset hdr + fromIntegral (xdvd_Sector entry) * sectorLength) 0
      return Folder { folderFiles = [], folderSubfolders = [(xdvd_Name entry, subDir)] }
    else return Folder { folderFiles = [(xdvd_Name entry, entry)], folderSubfolders = [] }
  left <- if xdvd_Left entry /= 0
    then parseTree h hdr baseOffset $ fromIntegral (xdvd_Left entry) * 4
    else return mempty
  right <- if xdvd_Right entry /= 0
    then parseTree h hdr baseOffset $ fromIntegral (xdvd_Right entry) * 4
    else return mempty
  return $ thisTree <> left <> right

data XDVDFSEntry a = XDVDFSEntry
  { xdvd_Left    :: Word16
  , xdvd_Right   :: Word16
  , xdvd_Sector  :: Word32
  , xdvd_Length  :: Word32
  , xdvd_Attribs :: Word8
  , xdvd_Name    :: a
  } deriving (Show)

readEntry :: Handle -> Integer -> Integer -> IO (XDVDFSEntry B.ByteString)
readEntry h baseOffset entryOffset = do
  hSeek h AbsoluteSeek $ baseOffset + entryOffset
  bs <- BL.hGet h 14
  entry <- flip runGetM bs $ do
    xdvd_Left    <- getWord16le
    xdvd_Right   <- getWord16le
    xdvd_Sector  <- getWord32le
    xdvd_Length  <- getWord32le
    xdvd_Attribs <- getWord8
    xdvd_Name    <- getWord8
    return XDVDFSEntry{..}
  name <- B.hGet h $ fromIntegral $ xdvd_Name entry
  return entry { xdvd_Name = name }

getXboxFile :: XboxHeader -> XDVDFSEntry B.ByteString -> Readable -> Readable
getXboxFile hdr entry = subHandle
  id -- TODO
  (x_PartitionOffset hdr + fromIntegral (xdvd_Sector entry) * sectorLength)
  (Just $ fromIntegral $ xdvd_Length entry)

loadXboxISO :: Readable -> IO (Maybe (Folder B.ByteString Readable))
loadXboxISO r = useHandle r $ \h -> findXboxHeader h >>= \case
  Nothing -> return Nothing
  Just hdr -> Just <$> do
    folder <- parseRoot h hdr
    return $ fmap (\entry -> getXboxFile hdr entry r) folder
