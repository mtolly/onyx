{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module STFS.Package (extractSTFS, withSTFS, STFSContents(..)) where

import           Control.Monad        (forM_, guard, replicateM)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Maybe           (mapMaybe)
import qualified Data.Text            as T
import           Data.Text.Encoding   (decodeLatin1, decodeUtf16BE)
import           Data.Time
import           Data.Word
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath      ((</>))
import           System.IO

data Header
  = CON CONHeader
  | LIVE LIVEHeader
  | PIRS LIVEHeader
  deriving (Eq, Show)

data CONHeader = CONHeader
  { ch_PublicKeyCertSize       :: B.ByteString -- 2 bytes
  , ch_CertOwnerConsoleID      :: B.ByteString -- 5 bytes
  , ch_CertOwnerConsolePartNum :: T.Text -- 0x14 bytes (ascii string)
  , ch_CertOwnerConsoleType    :: ConsoleType -- 1 byte: 1 for devkit, 2 for retail
  , ch_CertDateGeneration      :: T.Text -- 8 bytes (ascii string): MM-DD-YY
  , ch_PublicExponent          :: B.ByteString -- 4 bytes
  , ch_PublicModulus           :: B.ByteString -- 0x80 bytes
  , ch_CertSignature           :: B.ByteString -- 0x100 bytes
  , ch_Signature               :: B.ByteString -- 0x80 bytes
  } deriving (Eq, Show)

data ConsoleType = Devkit | Retail
  deriving (Eq, Show)

data LIVEHeader = LIVEHeader
  { lh_PackageSignature :: B.ByteString
  } deriving (Eq, Show)

getHeader :: Get Header
getHeader = getByteString 4 >>= \case
  "CON " -> CON <$> getCONHeader
  "LIVE" -> LIVE <$> getLIVEHeader
  "PIRS" -> PIRS <$> getLIVEHeader
  s -> fail $ "Unrecognized STFS magic: " <> show s
  where getCONHeader = do
          ch_PublicKeyCertSize <- getByteString 2
          ch_CertOwnerConsoleID <- getByteString 5
          ch_CertOwnerConsolePartNum <- T.takeWhile (/= '\0') . decodeLatin1 <$> getByteString 0x14
          ch_CertOwnerConsoleType <- getWord8 >>= \case
            1 -> return Devkit
            2 -> return Retail
            n -> fail $ "Unrecognized CON console type: " <> show n
          ch_CertDateGeneration <- decodeLatin1 <$> getByteString 8
          ch_PublicExponent <- getByteString 4
          ch_PublicModulus <- getByteString 0x80
          ch_CertSignature <- getByteString 0x100
          ch_Signature <- getByteString 0x80
          return CONHeader{..}
        getLIVEHeader = do
          lh_PackageSignature <- getByteString 0x100
          _pad <- getByteString 0x128
          return LIVEHeader{..}

data Metadata = Metadata
  { md_LicenseEntries       :: [LicenseEntry] -- 0x10 licenses, each 0x10 bytes
  , md_HeaderSHA1           :: B.ByteString -- 0x14 bytes: hash from 0x344 to first hash table
  , md_HeaderSize           :: Word32
  , md_ContentType          :: ContentType -- 4 bytes
  , md_MetadataVersion      :: Int32 -- 4 bytes: only supporting 1 for now but 2 also exists
  , md_ContentSize          :: Int64
  , md_MediaID              :: Word32
  , md_Version              :: Int32 -- 4 bytes: used for system/title updates
  , md_BaseVersion          :: Int32 -- 4 bytes: used for system/title updates
  , md_TitleID              :: Word32
  , md_Platform             :: Platform -- 1 byte: 2 for 360, 4 for PC. cons appear to be 0
  , md_ExecutableType       :: Word8
  , md_DiscNumber           :: Word8
  , md_DiscInSet            :: Word8
  , md_SaveGameID           :: Word32
  , md_ConsoleID            :: B.ByteString -- 5 bytes
  , md_ProfileID            :: B.ByteString -- 8 bytes
  , md_VolumeDescriptor     :: STFSDescriptor -- can be STFS or SVOD but we only support STFS
  , md_DataFileCount        :: Int32
  , md_DataFileCombinedSize :: Int64
  , md_DescriptorType       :: DescriptorType -- 4 bytes: 0 for STFS, 1 for SVOD
  , md_Reserved             :: Int32
  -- in metadata v2 this padding contains series/season/episode stuff, we don't care
  , md_Padding              :: B.ByteString -- 0x4C bytes
  , md_DeviceID             :: B.ByteString -- 0x14 bytes
  , md_DisplayName          :: [T.Text] -- 0x900 bytes, split up into 0x80 for each locale
  , md_DisplayDescription   :: [T.Text] -- 0x900 bytes, split up into 0x80 for each locale
  , md_PublisherName        :: T.Text -- 0x80 bytes
  , md_TitleName            :: T.Text -- 0x80 bytes
  , md_TransferFlags        :: Word8
  -- stored as 4 bytes (thumb size), 4 bytes (title thumb size),
  -- 0x4000 bytes (thumb image), 0x4000 bytes (title thumb image).
  -- in metadata v2 the max size of these is smaller and then additional names
  -- and description locales are at the end.
  , md_ThumbnailImage       :: B.ByteString
  , md_TitleThumbnailImage  :: B.ByteString
  } deriving (Eq, Show)

data Platform = P_Unknown | P_Xbox360 | P_PC
  deriving (Eq, Show)

data DescriptorType = D_STFS | D_SVOD
  deriving (Eq, Show)

getMetadata :: Get Metadata
getMetadata = do
  md_LicenseEntries <- replicateM 0x10 getLicenseEntry
  md_HeaderSHA1 <- getByteString 0x14
  md_HeaderSize <- getWord32be
  md_ContentType <- getWord32be >>= \w ->
    case filter (\ct -> contentTypeID ct == w) [minBound .. maxBound] of
      ct : _ -> return ct
      []     -> fail $ "Unrecognized STFS content type: " <> show w
  md_MetadataVersion <- getInt32be
  md_ContentSize <- getInt64be
  md_MediaID <- getWord32be
  md_Version <- getInt32be
  md_BaseVersion <- getInt32be
  md_TitleID <- getWord32be
  md_Platform <- getWord8 >>= \case
    0 -> return P_Unknown
    2 -> return P_Xbox360
    4 -> return P_PC
    b -> fail $ "Unrecognized STFS platform: " <> show b
  md_ExecutableType <- getWord8
  md_DiscNumber <- getWord8
  md_DiscInSet <- getWord8
  md_SaveGameID <- getWord32be
  md_ConsoleID <- getByteString 5
  md_ProfileID <- getByteString 8
  md_VolumeDescriptor <- getSTFSDescriptor
  md_DataFileCount <- getInt32be
  md_DataFileCombinedSize <- getInt64be
  md_DescriptorType <- getInt32be >>= \case
    0 -> return D_STFS
    1 -> return D_SVOD
    b -> fail $ "Unrecognized STFS descriptor type: " <> show b
  md_Reserved <- getInt32be
  md_Padding <- getByteString 0x4C
  md_DeviceID <- getByteString 0x14
  let str0x80 = T.takeWhile (/= '\0') . decodeUtf16BE <$> getByteString 0x80
  md_DisplayName <- replicateM 18 str0x80
  md_DisplayDescription <- replicateM 18 str0x80
  md_PublisherName <- str0x80
  md_TitleName <- str0x80
  md_TransferFlags <- getWord8
  thumbSize <- getInt32be
  titleThumbSize <- getInt32be
  md_ThumbnailImage <- B.take (fromIntegral thumbSize) <$> getByteString 0x4000
  md_TitleThumbnailImage <- B.take (fromIntegral titleThumbSize) <$> getByteString 0x4000
  return Metadata{..}

data LicenseEntry = LicenseEntry
  { le_LicenseID    :: Int64 -- 8 bytes: XUID / PUID / console id
  , le_LicenseBits  :: Int32 -- 4 bytes
  , le_LicenseFlags :: Int32 -- 4 bytes
  } deriving (Eq, Show)

getLicenseEntry :: Get LicenseEntry
getLicenseEntry = do
  le_LicenseID <- getInt64be
  le_LicenseBits <- getInt32be
  le_LicenseFlags <- getInt32be
  return LicenseEntry{..}

data STFSDescriptor = STFSDescriptor
  { sd_VolDescSize                :: Word8
  , sd_Reserved                   :: Word8
  , sd_BlockSeparation            :: Word8
  , sd_FileTableBlockCount        :: Int16
  , sd_FileTableBlockNumber       :: Int32 -- 3 bytes, should be Int24
  , sd_TopHashTableHash           :: B.ByteString -- 0x14 bytes
  , sd_TotalAllocatedBlockCount   :: Int32
  , sd_TotalUnallocatedBlockCount :: Int32
  } deriving (Eq, Show)

getInt24be :: Get Int32
getInt24be = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  return $ fromIntegral a * 0x10000 + fromIntegral b * 0x100 + fromIntegral c

getInt24le :: Get Int32
getInt24le = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  return $ fromIntegral a + fromIntegral b * 0x100 + fromIntegral c * 0x10000

getSTFSDescriptor :: Get STFSDescriptor
getSTFSDescriptor = do
  sd_VolDescSize <- getWord8
  sd_Reserved <- getWord8
  sd_BlockSeparation <- getWord8
  sd_FileTableBlockCount <- getInt16le
  sd_FileTableBlockNumber <- getInt24le
  sd_TopHashTableHash <- getByteString 0x14
  sd_TotalAllocatedBlockCount <- getInt32be
  sd_TotalUnallocatedBlockCount <- getInt32be
  return STFSDescriptor{..}

data ContentType
  = CT_ArcadeTitle
  | CT_AvatarItem
  | CT_CacheFile
  | CT_CommunityGame
  | CT_GameDemo
  | CT_GamerPicture
  | CT_GameTitle
  | CT_GameTrailer
  | CT_GameVideo
  | CT_InstalledGame
  | CT_Installer
  | CT_IPTVPauseBuffer
  | CT_LicenseStore
  | CT_MarketplaceContent
  | CT_Movie
  | CT_MusicVideo
  | CT_PodcastVideo
  | CT_Profile
  | CT_Publisher
  | CT_SavedGame
  | CT_StorageDownload
  | CT_Theme
  | CT_TV
  | CT_Video
  | CT_ViralVideo
  | CT_XboxDownload
  | CT_XboxOriginalGame
  | CT_XboxSavedGame
  | CT_Xbox360Title
  | CT_XboxTitle
  | CT_XNA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

contentTypeID :: ContentType -> Word32
contentTypeID = \case
  CT_ArcadeTitle -> 0xD0000
  CT_AvatarItem -> 0x9000
  CT_CacheFile -> 0x40000
  CT_CommunityGame -> 0x2000000
  CT_GameDemo -> 0x80000
  CT_GamerPicture -> 0x20000
  CT_GameTitle -> 0xA0000
  CT_GameTrailer -> 0xC0000
  CT_GameVideo -> 0x400000
  CT_InstalledGame -> 0x4000
  CT_Installer -> 0xB0000
  CT_IPTVPauseBuffer -> 0x2000
  CT_LicenseStore -> 0xF0000
  CT_MarketplaceContent -> 0x2
  CT_Movie -> 0x100000
  CT_MusicVideo -> 0x300000
  CT_PodcastVideo -> 0x500000
  CT_Profile -> 0x10000
  CT_Publisher -> 0x3
  CT_SavedGame -> 0x1
  CT_StorageDownload -> 0x50000
  CT_Theme -> 0x30000
  CT_TV -> 0x200000
  CT_Video -> 0x90000
  CT_ViralVideo -> 0x600000
  CT_XboxDownload -> 0x70000
  CT_XboxOriginalGame -> 0x5000
  CT_XboxSavedGame -> 0x60000
  CT_Xbox360Title -> 0x1000
  CT_XboxTitle -> 0x5000
  CT_XNA -> 0xE0000

data BlockHashRecord = BlockHashRecord
  { bhr_SHA1      :: B.ByteString -- 0x14 bytes
  , bhr_Status    :: BlockStatus
  , bhr_NextBlock :: Int32
  } deriving (Eq, Show)

data BlockStatus
  = BlockUnused
  | BlockFree
  | BlockUsed
  | BlockNewlyAllocated
  deriving (Eq, Ord, Show)

getBlockHashRecord :: Get BlockHashRecord
getBlockHashRecord = do
  bhr_SHA1 <- getByteString 0x14
  bhr_Status <- getWord8 >>= \case
    0x00 -> return BlockUnused
    0x40 -> return BlockFree
    0x80 -> return BlockUsed
    0xC0 -> return BlockNewlyAllocated
    b    -> fail $ "Unrecognized block hash record status: " <> show b
  bhr_NextBlock <- getInt24be
  return BlockHashRecord{..}

data STFSContents = STFSContents
  { stfsDirectories :: [FilePath]
  , stfsFiles       :: [(FilePath, IO BL.ByteString)]
  }

data TableSizeShift = Shift0 | Shift1
  deriving (Eq, Show, Enum)

tableSpacing :: TableSizeShift -> (Int32, Int32, Int32)
tableSpacing = \case
  Shift0 -> (0xAB, 0x718F, 0xFE7DA)
  Shift1 -> (0xAC, 0x723A, 0xFD00B)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

data FileEntry = FileEntry
  { fe_FileName        :: T.Text
  , fe_Consecutive     :: Bool
  , fe_Directory       :: Bool
  , fe_Blocks1         :: Int32
  , fe_Blocks2         :: Int32 -- copy of Blocks1 apparently
  , fe_FirstBlock      :: Int32
  , fe_PathIndex       :: Int16
  , fe_Size            :: Word32
  , fe_UpdateTimestamp :: LocalTime
  , fe_AccessTimestamp :: LocalTime
  } deriving (Eq, Show)

getTimestamp :: Get LocalTime -- dunno if utc or local
getTimestamp = do
  ts <- getWord32le
  return $ LocalTime
    { localDay = fromGregorian
      (1980 + fromIntegral (ts `shiftR` 25)) -- year
      (fromIntegral $ (ts `shiftR` 21) .&. 0xF) -- month
      (fromIntegral $ (ts `shiftR` 16) .&. 0x1F) -- day
    , localTimeOfDay = TimeOfDay
      { todHour = fromIntegral $ (ts `shiftR` 11) .&. 0x1F
      , todMin = fromIntegral $ (ts `shiftR` 6) .&. 0x3F
      , todSec = fromIntegral $ ts .&. 0x1F
      }
    }

getFileEntry :: Get FileEntry
getFileEntry = do
  name <- getByteString 0x28
  len <- getWord8
  let fe_FileName = decodeLatin1 $ B.take (fromIntegral $ len .&. 0x3F) name
      fe_Consecutive = len `testBit` 6
      fe_Directory = len `testBit` 7
  fe_Blocks1 <- getInt24le
  fe_Blocks2 <- getInt24le
  fe_FirstBlock <- getInt24le
  fe_PathIndex <- getInt16be
  fe_Size <- getWord32be
  fe_UpdateTimestamp <- getTimestamp
  fe_AccessTimestamp <- getTimestamp
  return FileEntry{..}

withSTFS :: FilePath -> (STFSContents -> IO a) -> IO a
withSTFS stfs fn = withBinaryFile stfs ReadMode $ \fd -> do
  headerMetaBytes <- BL.hGet fd 0x971A
  let (_header, meta) = flip runGet headerMetaBytes $ (,) <$> getHeader <*> getMetadata
      stfsDesc = md_VolumeDescriptor meta

  let tableSizeShift = if ((md_HeaderSize meta + 0xFFF) .&. 0xF000) `shiftR` 0xC == 0xB
        then Shift0
        else Shift1
      tss = fromEnum tableSizeShift

      fixBlockNumber :: Int32 -> Int32
      fixBlockNumber blk = blk + sum
        [ if blk >= 0xAA then ((blk `div` 0xAA) + 1) `shiftL` tss else 0
        , if blk > 0x70E4 then ((blk `div` 0x70E4) + 1) `shiftL` tss else 0
        ]

      readBlock :: Int32 -> Word32 -> IO BL.ByteString
      readBlock blk len = do
        -- Read a block given its block number
        -- If reading data blocks call fixBlockNumber first
        hSeek fd AbsoluteSeek $ 0xc000 + fromIntegral blk * 0x1000
        BL.hGet fd $ fromIntegral len

      getBlockHash :: Int32 -> Int32 -> IO BlockHashRecord
      getBlockHash blk tableOffset = do
        -- Given a block number return the hash object that goes with it
        let record = blk `mod` 0xAA
        -- Num tables * space blocks between each (0xAB or 0xAC for [0])
        let tablenum = sum
              [ (blk `div` 0xAA) * fst3 (tableSpacing tableSizeShift)
              , if blk >= 0xAA
                then (blk `div` 0x70E4 + 1) `shiftL` tss -- skip level 1 tables
                else 0
              , if blk >= 0x70E4
                then 1 `shiftL` tss -- If we're into level 2 add the level 2 table
                else 0
              -- Read the table block, get the correct record and pass it to BlockHashRecord
              -- Fix to point at the first table (these numbers are offset from data block numbers)
              , tableOffset - (1 `shiftL` tss)
              ]
        hashdata <- readBlock tablenum 0x1000
        return $ runGet getBlockHashRecord
          $ BL.take 0x18 $ BL.drop (fromIntegral record * 0x18) hashdata

      readFileBlocks :: Word32 -> Int32 -> IO BL.ByteString
      readFileBlocks 0    _   = return BL.empty
      readFileBlocks size blk = do
        let len = min 0x1000 size
        blockData <- readBlock (fixBlockNumber blk) len
        blkHash <- getBlockHash blk 0
        blkHash' <- if tableSizeShift == Shift1 && bhr_Status blkHash < BlockUsed
          then getBlockHash blk 1
          else return blkHash
        BL.append (blockData) <$> readFileBlocks (size - len) (bhr_NextBlock blkHash')

      readBlocks :: Word32 -> Int32 -> BlockStatus -> IO BL.ByteString
      readBlocks size blk info
        | size <= 0
          || blk <= 0
          || blk >= sd_TotalAllocatedBlockCount stfsDesc
          || info < BlockUsed
          = return BL.empty
        | otherwise = do
          let len = min 0x1000 size
          blockData <- readBlock (fixBlockNumber blk) len
          blkHash <- getBlockHash blk 0
          blkHash' <- if tableSizeShift == Shift1 && bhr_Status blkHash < BlockUsed
            then getBlockHash blk 1
            else return blkHash
          BL.append (blockData) <$> readBlocks (size - len) (bhr_NextBlock blkHash') (bhr_Status blkHash')

  filesBytes <- readFileBlocks
    (fromIntegral (sd_FileTableBlockCount stfsDesc) * 0x1000)
    (sd_FileTableBlockNumber stfsDesc)
  let files
        = takeWhile (not . T.null . fe_FileName)
        $ runGet (replicateM (fromIntegral (BL.length filesBytes) `div` 0x40) getFileEntry) filesBytes
      locateFile fe = if fe_PathIndex fe == -1
        then T.unpack $ fe_FileName fe
        else locateFile (files !! fromIntegral (fe_PathIndex fe)) </> T.unpack (fe_FileName fe)
      stfsFiles = flip mapMaybe files $ \fe -> do
        guard $ not $ fe_Directory fe
        return (locateFile fe, readBlocks (fe_Size fe) (fe_FirstBlock fe) BlockUsed)
      stfsDirectories = flip mapMaybe files $ \fe -> do
        guard $ fe_Directory fe
        return $ locateFile fe

  fn STFSContents{..}

extractSTFS :: FilePath -> FilePath -> IO ()
extractSTFS stfs dir = withSTFS stfs $ \contents -> do
  forM_ (stfsDirectories contents) $ createDirectoryIfMissing True . (dir </>)
  forM_ (stfsFiles contents) $ \(path, getFile) -> do
    getFile >>= BL.writeFile (dir </> path)
