{-
PS2 ISOs are standard ISO 9660 format, no extensions required.
(There is also UDF info in DVD images but we don't need to parse it.)
But, also supports dual layer ISOs (RB1/RB2, probably later GHs).

Mostly implemented using the ECMA-119 version of the spec:
https://www.ecma-international.org/wp-content/uploads/ECMA-119_4th_edition_june_2019.pdf
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
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
import           Onyx.Util.Binary     (runGetM)
import           Onyx.Util.Handle     (Folder (..), Readable, fromFiles,
                                       subHandle, useHandle)
import           System.IO            (Handle, SeekMode (..), hFileSize, hSeek)

data PrimaryVolumeDescriptor = PrimaryVolumeDescriptor
  -- byte 1, string "CD001"
  { version                        :: Word8 -- 1
  -- byte 0
  , systemIdentifier               :: B.ByteString -- "PLAYSTATION"
  , volumeIdentifier               :: B.ByteString -- "BAND_S" for RB2
  -- 8 bytes of 0
  , volumeSpaceSize                :: Word32
  -- 32 bytes of 0
  , volumeSetSize                  :: Word16
  , volumeSequenceNumber           :: Word16
  , logicalBlockSize               :: Word16 -- 0x800; logical sectors and logical blocks appear to be the same for us
  , pathTableSize                  :: Word32
  , locationTypeLPathTable         :: Word32
  , locationOptionalTypeLPathTable :: Word32
  , locationTypeMPathTable         :: Word32
  , locationOptionalTypeMPathTable :: Word32
  , rootDirectoryRecord            :: DirectoryRecord
  , volumeSetIdentifier            :: B.ByteString
  , publisherIdentifier            :: B.ByteString
  , dataPreparerIdentifier         :: B.ByteString
  , applicationIdentifier          :: B.ByteString
  , copyrightFileIdentifier        :: B.ByteString
  , abstractFileIdentifier         :: B.ByteString
  , bibliographicFileIdentifier    :: B.ByteString
  , volumeCreationDateTime         :: B.ByteString
  , volumeModificationDateTime     :: B.ByteString
  , volumeExpirationDateTime       :: B.ByteString
  , volumeEffectiveDateTime        :: B.ByteString
  , fileStructureVersion           :: Word8
  -- rest is reserved or application use
  } deriving (Show)

data DirectoryRecord = DirectoryRecord
  { extendedAttributeRecordLength :: Word8
  , locationOfExtent              :: Word32
  , dataLength                    :: Word32
  , recordingDateTime             :: B.ByteString -- 7 bytes
  , fileFlags                     :: Word8
  , fileUnitSize                  :: Word8
  , interleaveGapSize             :: Word8
  , volumeSequenceNumber          :: Word16
  , fileIdentifier                :: B.ByteString
  } deriving (Show)

data PathTableRecord = PathTableRecord
  { extendedAttributeRecordLength :: Word8
  , locationOfExtent              :: Word32
  , parentDirectoryNumber         :: Word16
  , directoryIdentifier           :: B.ByteString
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
    hSeek h AbsoluteSeek $ fromIntegral pvd.locationTypeMPathTable * 0x800
    ptrs <- B.hGet h 0x800 >>= runGetM (getMany getPathTableRecordBE) . BL.fromStrict
    recs <- fromFiles <$> getFileTree h ptrs
    return (pvd, recs)
  return (bimap
    (\name -> fromMaybe name $ B.stripSuffix ";1" name)
    (\dr -> subHandle
      id -- TODO add label
      (fromIntegral dr.locationOfExtent * 0x800)
      (Just $ fromIntegral dr.dataLength)
      r
    )
    recs, fromIntegral pvd.volumeSpaceSize * 0x800 - 0x8000)

getFileTree :: Handle -> [PathTableRecord] -> IO [(NE.NonEmpty B.ByteString, DirectoryRecord)]
getFileTree h ptrs = do
  -- RB2 PS2 2nd layer appears to have root and GEN folder with same extent number.
  -- Map.fromList keeps the last entry for each extent number
  let realPTRs = Map.elems $ Map.fromList [ (ptr.locationOfExtent, ptr) | ptr <- ptrs ]
      ptrToParents ptr = case ptr.directoryIdentifier of
        "\0"  -> []
        dirID -> dirID : ptrToParents (ptrs !! fromIntegral (ptr.parentDirectoryNumber - 1))
  fmap concat $ forM realPTRs $ \ptr -> do
    let parents = ptrToParents ptr
    recs <- getFolderFiles h ptr
    return $ flip map recs $ \rec -> let
      path = NE.reverse $ rec.fileIdentifier :| parents
      in (path, rec)

getFolderFiles :: Handle -> PathTableRecord -> IO [DirectoryRecord]
getFolderFiles h ptr = do
  hSeek h AbsoluteSeek $ fromIntegral ptr.locationOfExtent * 0x800
  bs <- B.hGet h 0x800
  recs <- runGetM (getMany getDirectoryRecord) $ BL.fromStrict bs
  -- bit 1 is true if directory (including special . and .. entries)
  return $ filter (\rec -> not $ testBit rec.fileFlags 1) recs

getPrimaryVolumeDescriptor :: Get PrimaryVolumeDescriptor
getPrimaryVolumeDescriptor = do
  1                              <- getWord8
  "CD001"                        <- getByteString 5
  version                        <- getWord8
  skip 1
  systemIdentifier               <- spacePadded <$> getByteString 0x20
  volumeIdentifier               <- spacePadded <$> getByteString 0x20
  skip 8
  volumeSpaceSize                <- mirrored getWord32le getWord32be
  skip 32
  volumeSetSize                  <- mirrored getWord16le getWord16be
  volumeSequenceNumber           <- mirrored getWord16le getWord16be
  logicalBlockSize               <- mirrored getWord16le getWord16be
  pathTableSize                  <- mirrored getWord32le getWord32be
  locationTypeLPathTable         <- getWord32le
  locationOptionalTypeLPathTable <- getWord32le
  locationTypeMPathTable         <- getWord32be
  locationOptionalTypeMPathTable <- getWord32be
  Just rootDirectoryRecord       <- getDirectoryRecord
  volumeSetIdentifier            <- spacePadded <$> getByteString 0x80
  publisherIdentifier            <- spacePadded <$> getByteString 0x80
  dataPreparerIdentifier         <- spacePadded <$> getByteString 0x80
  applicationIdentifier          <- spacePadded <$> getByteString 0x80
  copyrightFileIdentifier        <- spacePadded <$> getByteString 37
  abstractFileIdentifier         <- spacePadded <$> getByteString 37
  bibliographicFileIdentifier    <- spacePadded <$> getByteString 37
  volumeCreationDateTime         <- getByteString 17
  volumeModificationDateTime     <- getByteString 17
  volumeExpirationDateTime       <- getByteString 17
  volumeEffectiveDateTime        <- getByteString 17
  fileStructureVersion           <- getWord8
  return PrimaryVolumeDescriptor{..}

getMany :: Get (Maybe a) -> Get [a]
getMany g = g >>= \case
  Just x  -> (x :) <$> getMany g
  Nothing -> return []

getDirectoryRecord :: Get (Maybe DirectoryRecord)
getDirectoryRecord = lookAhead getWord8 >>= \case
  0   -> return Nothing
  len -> Just <$> do
    posn1                         <- bytesRead
    skip 1 -- len
    extendedAttributeRecordLength <- getWord8
    locationOfExtent              <- mirrored getWord32le getWord32be
    dataLength                    <- mirrored getWord32le getWord32be
    recordingDateTime             <- getByteString 7
    fileFlags                     <- getWord8
    fileUnitSize                  <- getWord8
    interleaveGapSize             <- getWord8
    volumeSequenceNumber          <- mirrored getWord16le getWord16be
    lenFileIdentifier             <- getWord8
    fileIdentifier                <- getByteString $ fromIntegral lenFileIdentifier
    when (even lenFileIdentifier) $ skip 1
    posn2                         <- bytesRead
    skip $ fromIntegral len - fromIntegral (posn2 - posn1)
    return DirectoryRecord{..}

getPathTableRecordBE :: Get (Maybe PathTableRecord)
getPathTableRecordBE = lookAhead getWord8 >>= \case
  0 -> return Nothing
  lenDI -> Just <$> do
    skip 1 -- lenDI
    extendedAttributeRecordLength <- getWord8
    locationOfExtent              <- getWord32be
    parentDirectoryNumber         <- getWord16be
    directoryIdentifier           <- getByteString $ fromIntegral lenDI
    when (odd lenDI) $ skip 1
    return PathTableRecord{..}
