{-
PS2 ISOs are standard ISO 9660 format, no extensions required.
(There is also UDF info in DVD images but we don't need to parse it.)
But, also supports dual layer ISOs (RB1/RB2, probably later GHs).

Mostly implemented using the ECMA-119 version of the spec:
https://www.ecma-international.org/wp-content/uploads/ECMA-119_4th_edition_june_2019.pdf
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.ISO where

import           Control.Monad        (forM, unless, when)
import           Data.Bifunctor       (bimap)
import           Data.Binary.Get
import           Data.Bits            (testBit)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.List.NonEmpty   (NonEmpty (..))
import qualified Data.List.NonEmpty   as NE
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Word
import           Onyx.Util.Handle     (Folder (..), Readable, fromFiles,
                                       subHandle, useHandle)
import           Onyx.Xbox.STFS       (runGetM)
import           System.IO            (Handle, SeekMode (..), hFileSize, hSeek)

data PrimaryVolumeDescriptor = PrimaryVolumeDescriptor
  -- byte 1, string "CD001"
  { pvd_Version                        :: Word8 -- 1
  -- byte 0
  , pvd_SystemIdentifier               :: B.ByteString -- "PLAYSTATION"
  , pvd_VolumeIdentifier               :: B.ByteString -- "BAND_S" for RB2
  -- 8 bytes of 0
  , pvd_VolumeSpaceSize                :: Word32
  -- 32 bytes of 0
  , pvd_VolumeSetSize                  :: Word16
  , pvd_VolumeSequenceNumber           :: Word16
  , pvd_LogicalBlockSize               :: Word16 -- 0x800; logical sectors and logical blocks appear to be the same for us
  , pvd_PathTableSize                  :: Word32
  , pvd_LocationTypeLPathTable         :: Word32
  , pvd_LocationOptionalTypeLPathTable :: Word32
  , pvd_LocationTypeMPathTable         :: Word32
  , pvd_LocationOptionalTypeMPathTable :: Word32
  , pvd_RootDirectoryRecord            :: DirectoryRecord
  , pvd_VolumeSetIdentifier            :: B.ByteString
  , pvd_PublisherIdentifier            :: B.ByteString
  , pvd_DataPreparerIdentifier         :: B.ByteString
  , pvd_ApplicationIdentifier          :: B.ByteString
  , pvd_CopyrightFileIdentifier        :: B.ByteString
  , pvd_AbstractFileIdentifier         :: B.ByteString
  , pvd_BibliographicFileIdentifier    :: B.ByteString
  , pvd_VolumeCreationDateTime         :: B.ByteString
  , pvd_VolumeModificationDateTime     :: B.ByteString
  , pvd_VolumeExpirationDateTime       :: B.ByteString
  , pvd_VolumeEffectiveDateTime        :: B.ByteString
  , pvd_FileStructureVersion           :: Word8
  -- rest is reserved or application use
  } deriving (Show)

data DirectoryRecord = DirectoryRecord
  { dr_ExtendedAttributeRecordLength :: Word8
  , dr_LocationOfExtent              :: Word32
  , dr_DataLength                    :: Word32
  , dr_RecordingDateTime             :: B.ByteString -- 7 bytes
  , dr_FileFlags                     :: Word8
  , dr_FileUnitSize                  :: Word8
  , dr_InterleaveGapSize             :: Word8
  , dr_VolumeSequenceNumber          :: Word16
  , dr_FileIdentifier                :: B.ByteString
  } deriving (Show)

data PathTableRecord = PathTableRecord
  { ptr_ExtendedAttributeRecordLength :: Word8
  , ptr_LocationOfExtent              :: Word32
  , ptr_ParentDirectoryNumber         :: Word16
  , ptr_DirectoryIdentifier           :: B.ByteString
  } deriving (Show)

mirrored :: (Eq a, Show a) => Get a -> Get a -> Get a
mirrored getLE getBE = do
  x <- getLE
  y <- getBE
  unless (x == y) $ fail $ "Both-byte order values didn't match: " <> show x <> " != " <> show y
  return x

spacePadded :: B.ByteString -> B.ByteString
spacePadded = B.dropWhileEnd (== 0x20)

folderISO :: Readable -> IO (Folder B.ByteString Readable)
folderISO r = isISO r >>= \case
  False -> return mempty
  True  -> do
    (layer, nextLayer) <- folderISOLayer r
    rest <- folderISO $ subHandle id nextLayer Nothing r
    return $ layer <> rest

isISO :: Readable -> IO Bool
isISO r = useHandle r $ \h -> do
  len <- hFileSize h
  if len < 0x8006
    then return False
    else do
      hSeek h AbsoluteSeek 0x8001
      bs <- B.hGet h 5
      return $ bs == "CD001"

folderISOLayer :: Readable -> IO (Folder B.ByteString Readable, Integer)
folderISOLayer r = do
  (pvd, recs) <- useHandle r $ \h -> do
    hSeek h AbsoluteSeek 0x8000
    pvd <- B.hGet h 0x800 >>= runGetM getPrimaryVolumeDescriptor . BL.fromStrict
    hSeek h AbsoluteSeek $ fromIntegral (pvd_LocationTypeMPathTable pvd) * 0x800
    ptrs <- B.hGet h 0x800 >>= runGetM (getMany getPathTableRecordBE) . BL.fromStrict
    recs <- fromFiles <$> getFileTree h ptrs
    return (pvd, recs)
  return (bimap
    (\name -> fromMaybe name $ B.stripSuffix ";1" name)
    (\dr -> subHandle
      id -- TODO add label
      (fromIntegral (dr_LocationOfExtent dr) * 0x800)
      (Just $ fromIntegral $ dr_DataLength dr)
      r
    )
    recs, fromIntegral (pvd_VolumeSpaceSize pvd) * 0x800 - 0x8000)

getFileTree :: Handle -> [PathTableRecord] -> IO [(NE.NonEmpty B.ByteString, DirectoryRecord)]
getFileTree h ptrs = do
  -- RB2 PS2 2nd layer appears to have root and GEN folder with same extent number.
  -- Map.fromList keeps the last entry for each extent number
  let realPTRs = Map.elems $ Map.fromList [ (ptr_LocationOfExtent ptr, ptr) | ptr <- ptrs ]
      ptrToParents ptr = case ptr_DirectoryIdentifier ptr of
        "\0"  -> []
        dirID -> dirID : ptrToParents (ptrs !! fromIntegral (ptr_ParentDirectoryNumber ptr - 1))
  fmap concat $ forM realPTRs $ \ptr -> do
    let parents = ptrToParents ptr
    recs <- getFolderFiles h ptr
    return $ flip map recs $ \rec -> let
      path = NE.reverse $ dr_FileIdentifier rec :| parents
      in (path, rec)

getFolderFiles :: Handle -> PathTableRecord -> IO [DirectoryRecord]
getFolderFiles h ptr = do
  hSeek h AbsoluteSeek $ fromIntegral (ptr_LocationOfExtent ptr) * 0x800
  bs <- B.hGet h 0x800
  recs <- runGetM (getMany getDirectoryRecord) $ BL.fromStrict bs
  -- bit 1 is true if directory (including special . and .. entries)
  return $ filter (\rec -> not $ testBit (dr_FileFlags rec) 1) recs

getPrimaryVolumeDescriptor :: Get PrimaryVolumeDescriptor
getPrimaryVolumeDescriptor = do
  1                                  <- getWord8
  "CD001"                            <- getByteString 5
  pvd_Version                        <- getWord8
  skip 1
  pvd_SystemIdentifier               <- spacePadded <$> getByteString 0x20
  pvd_VolumeIdentifier               <- spacePadded <$> getByteString 0x20
  skip 8
  pvd_VolumeSpaceSize                <- mirrored getWord32le getWord32be
  skip 32
  pvd_VolumeSetSize                  <- mirrored getWord16le getWord16be
  pvd_VolumeSequenceNumber           <- mirrored getWord16le getWord16be
  pvd_LogicalBlockSize               <- mirrored getWord16le getWord16be
  pvd_PathTableSize                  <- mirrored getWord32le getWord32be
  pvd_LocationTypeLPathTable         <- getWord32le
  pvd_LocationOptionalTypeLPathTable <- getWord32le
  pvd_LocationTypeMPathTable         <- getWord32be
  pvd_LocationOptionalTypeMPathTable <- getWord32be
  Just pvd_RootDirectoryRecord       <- getDirectoryRecord
  pvd_VolumeSetIdentifier            <- spacePadded <$> getByteString 0x80
  pvd_PublisherIdentifier            <- spacePadded <$> getByteString 0x80
  pvd_DataPreparerIdentifier         <- spacePadded <$> getByteString 0x80
  pvd_ApplicationIdentifier          <- spacePadded <$> getByteString 0x80
  pvd_CopyrightFileIdentifier        <- spacePadded <$> getByteString 37
  pvd_AbstractFileIdentifier         <- spacePadded <$> getByteString 37
  pvd_BibliographicFileIdentifier    <- spacePadded <$> getByteString 37
  pvd_VolumeCreationDateTime         <- getByteString 17
  pvd_VolumeModificationDateTime     <- getByteString 17
  pvd_VolumeExpirationDateTime       <- getByteString 17
  pvd_VolumeEffectiveDateTime        <- getByteString 17
  pvd_FileStructureVersion           <- getWord8
  return PrimaryVolumeDescriptor{..}

getMany :: Get (Maybe a) -> Get [a]
getMany g = g >>= \case
  Just x  -> (x :) <$> getMany g
  Nothing -> return []

getDirectoryRecord :: Get (Maybe DirectoryRecord)
getDirectoryRecord = lookAhead getWord8 >>= \case
  0   -> return Nothing
  len -> Just <$> do
    posn1                            <- bytesRead
    skip 1 -- len
    dr_ExtendedAttributeRecordLength <- getWord8
    dr_LocationOfExtent              <- mirrored getWord32le getWord32be
    dr_DataLength                    <- mirrored getWord32le getWord32be
    dr_RecordingDateTime             <- getByteString 7
    dr_FileFlags                     <- getWord8
    dr_FileUnitSize                  <- getWord8
    dr_InterleaveGapSize             <- getWord8
    dr_VolumeSequenceNumber          <- mirrored getWord16le getWord16be
    lenFileIdentifier                <- getWord8
    dr_FileIdentifier                <- getByteString $ fromIntegral lenFileIdentifier
    when (even lenFileIdentifier) $ skip 1
    posn2                            <- bytesRead
    skip $ fromIntegral len - fromIntegral (posn2 - posn1)
    return DirectoryRecord{..}

getPathTableRecordBE :: Get (Maybe PathTableRecord)
getPathTableRecordBE = lookAhead getWord8 >>= \case
  0 -> return Nothing
  lenDI -> Just <$> do
    skip 1 -- lenDI
    ptr_ExtendedAttributeRecordLength <- getWord8
    ptr_LocationOfExtent              <- getWord32be
    ptr_ParentDirectoryNumber         <- getWord16be
    ptr_DirectoryIdentifier           <- getByteString $ fromIntegral lenDI
    when (odd lenDI) $ skip 1
    return PathTableRecord{..}
