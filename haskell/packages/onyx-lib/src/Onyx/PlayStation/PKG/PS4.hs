{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.PlayStation.PKG.PS4 where

import           Control.Monad
import           Control.Monad.Codec
import           Data.Binary.Get
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Int
import           Data.Word
import           Onyx.Rocksmith.Sng2014 (nullTerm)
import           Onyx.Util.Binary       (runGetM)
import           System.IO

data Header = Header
  { magic              :: B.ByteString
  , flags              :: PKGFlags
  , unk_0x08           :: Word32
  , unk_0x0C           :: Word32 -- 0xF
  , entry_count        :: Word32
  , sc_entry_count     :: Word16
  , entry_count_2      :: Word16
  , entry_table_offset :: Word32
  , main_ent_data_size :: Word32
  , body_offset        :: Word64
  , body_size          :: Word64
  -- 0x10 bytes gap, zero?
  , content_id         :: B.ByteString -- 0x30 bytes null terminated
  , drm_type           :: DrmType
  , content_type       :: ContentType
  , content_flags      :: ContentFlags
  , promote_size       :: Word32
  , version_date       :: Word32
  , version_hash       :: Word32
  , unk_0x88           :: Word32 -- for delta patches only?
  , unk_0x8C           :: Word32 -- for delta patches only?
  , unk_0x90           :: Word32 -- for delta patches only?
  , unk_0x94           :: Word32 -- for delta patches only?
  , iro_tag            :: IROTag
  , ekc_version        :: Word32 -- drm type version
  -- 0x60 bytes gap, zero?
  , sc_entries1_hash   :: B.ByteString
  , sc_entries2_hash   :: B.ByteString
  , digest_table_hash  :: B.ByteString
  , body_digest        :: B.ByteString
  } deriving (Show)

type PKGFlags = Word32
type DrmType = Int32
type ContentType = Int32
type ContentFlags = Word32
type IROTag = Int32

readHeader :: Get Header
readHeader = do
  magic              <- getByteString 4
  when (magic /= "\x7F\x43\x4E\x54") $ fail $ "Unrecognized PS4 .pkg magic string: " <> show magic
  flags              <- getWord32be
  unk_0x08           <- getWord32be
  unk_0x0C           <- getWord32be
  entry_count        <- getWord32be
  sc_entry_count     <- getWord16be
  entry_count_2      <- getWord16be
  entry_table_offset <- getWord32be
  main_ent_data_size <- getWord32be
  body_offset        <- getWord64be
  body_size          <- getWord64be
  skip 0x10
  content_id         <- codecIn $ nullTerm 0x30
  drm_type           <- getInt32be
  content_type       <- getInt32be
  content_flags      <- getWord32be
  promote_size       <- getWord32be
  version_date       <- getWord32be
  version_hash       <- getWord32be
  unk_0x88           <- getWord32be
  unk_0x8C           <- getWord32be
  unk_0x90           <- getWord32be
  unk_0x94           <- getWord32be
  iro_tag            <- getInt32be
  ekc_version        <- getWord32be
  skip 0x60
  sc_entries1_hash   <- getByteString 0x20
  sc_entries2_hash   <- getByteString 0x20
  digest_table_hash  <- getByteString 0x20
  body_digest        <- getByteString 0x20
  return Header{..}

-- Maxton had these in Header, but wrote:
-- "TODO i think these fields are actually members of element of container array"
data Header0x400 = Header0x400
  { unk_0x400            :: Word32
  , pfs_image_count      :: Word32
  , pfs_flags            :: Word64
  , pfs_image_offset     :: Word64
  , pfs_image_size       :: Word64
  , mount_image_offset   :: Word64
  , mount_image_size     :: Word64
  , package_size         :: Word64
  , pfs_signed_size      :: Word32
  , pfs_cache_size       :: Word32
  , pfs_image_digest     :: B.ByteString
  , pfs_signed_digest    :: B.ByteString
  , pfs_split_size_nth_0 :: Word64
  , pfs_split_size_nth_1 :: Word64
  } deriving (Show)

readHeader0x400 :: Get Header0x400
readHeader0x400 = do
  unk_0x400            <- getWord32be
  pfs_image_count      <- getWord32be
  pfs_flags            <- getWord64be
  pfs_image_offset     <- getWord64be
  pfs_image_size       <- getWord64be
  mount_image_offset   <- getWord64be
  mount_image_size     <- getWord64be
  package_size         <- getWord64be
  pfs_signed_size      <- getWord32be
  pfs_cache_size       <- getWord32be
  pfs_image_digest     <- getByteString 0x20
  pfs_signed_digest    <- getByteString 0x20
  pfs_split_size_nth_0 <- getWord64be
  pfs_split_size_nth_1 <- getWord64be
  return Header0x400{..}

data PKG = PKG
  { header          :: Header
  , header0x400     :: Header0x400
  , headerDigest    :: B.ByteString
  , headerSignature :: B.ByteString
  , metas           :: [MetaEntry (Maybe EntryContents)]
  } deriving (Show)

readPKG :: Handle -> IO PKG
readPKG h = do
  hSeek h AbsoluteSeek 0
  headerBytes <- BL.hGet h 0x5A0
  header <- runGetM readHeader headerBytes
  header0x400 <- runGetM readHeader0x400 $ BL.drop 0x400 headerBytes
  hSeek h AbsoluteSeek 0xFE0
  headerDigest <- B.hGet h 32
  headerSignature <- B.hGet h 256
  hSeek h AbsoluteSeek $ fromIntegral header.entry_table_offset
  metasBytes <- BL.hGet h $ 0x20 * fromIntegral header.entry_count
  metas <- flip runGetM metasBytes $ replicateM (fromIntegral header.entry_count) $ do
    id_ <- getWord32be
    nameTableOffset <- getWord32be
    flags1 <- getWord32be
    flags2 <- getWord32be
    dataOffset <- getWord32be
    dataSize <- getWord32be
    skip 8
    return MetaEntry{entryContents = Nothing, ..}
  -- metas <- forM metas' $ \meta -> do
  --   hSeek h AbsoluteSeek $ fromIntegral meta.dataOffset
  --   bs <- BL.hGet h $ fromIntegral meta.dataSize
  --   return meta { entryContents = Just $ GenericEntry bs }
  return PKG{..}

data MetaEntry a = MetaEntry
  { id_             :: EntryId
  , nameTableOffset :: Word32
  , flags1          :: Word32
  , flags2          :: Word32
  , dataOffset      :: Word32
  , dataSize        :: Word32
  , entryContents   :: a
  } deriving (Show)

type EntryId = Word32

-- TODO rest of parsed entry types
data EntryContents
  = GenericEntry BL.ByteString

instance Show EntryContents where
  show = \case
    GenericEntry{} -> "GenericEntry{...}"

--------------------------------------------------------------------------------

data PfsHeader = PfsHeader
  { version          :: Int64
  , magic            :: Int64
  , id_              :: Int64
  , fmode            :: Word8
  , clean            :: Word8
  , readOnly         :: Word8
  , rsv              :: Word8
  , mode             :: PfsMode
  , unk1             :: Word16
  , blockSize        :: Word32
  , nBackup          :: Word32
  , nBlock           :: Int64
  , dinodeCount      :: Int64
  , ndblock          :: Int64
  , dinodeBlockCount :: Int64
  , inodeBlockSig    :: DinodeS64
  , seed             :: B.ByteString
  } deriving (Show)

type PfsMode = Word16

data Inode a = Inode
  { mode           :: InodeMode
  , nlink          :: Word16
  , flags          :: InodeFlags
  , size           :: Int64
  , sizeCompressed :: Int64
  , time1_sec      :: Int64
  , time2_sec      :: Int64
  , time3_sec      :: Int64
  , time4_sec      :: Int64
  , time1_nsec     :: Word32
  , time2_nsec     :: Word32
  , time3_nsec     :: Word32
  , time4_nsec     :: Word32
  , uid            :: Word32
  , gid            :: Word32
  , unk1           :: Word64
  , unk2           :: Word64
  , blocks         :: a
  , db             :: [BlockSig a] -- count 12
  , ib             :: [BlockSig a] -- count 5
  } deriving (Show)

type InodeMode = Word16
type InodeFlags = Word32

type DinodeS64 = Inode Int64

readInode :: Get a -> Get (Inode a)
readInode getBlockType = do
  mode           <- getWord16le
  nlink          <- getWord16le
  flags          <- getWord32le
  size           <- getInt64le
  sizeCompressed <- getInt64le
  time1_sec      <- getInt64le
  time2_sec      <- getInt64le
  time3_sec      <- getInt64le
  time4_sec      <- getInt64le
  time1_nsec     <- getWord32le
  time2_nsec     <- getWord32le
  time3_nsec     <- getWord32le
  time4_nsec     <- getWord32le
  uid            <- getWord32le
  gid            <- getWord32le
  unk1           <- getWord64le
  unk2           <- getWord64le
  blocks         <- getBlockType
  db             <- replicateM 12 $ readBlockSig getBlockType
  ib             <- replicateM 5 $ readBlockSig getBlockType
  return Inode{..}

data BlockSig a = BlockSig
  { sig   :: B.ByteString
  , block :: a
  } deriving (Show)

readBlockSig :: Get a -> Get (BlockSig a)
readBlockSig getBlockType = do
  sig <- getByteString 32
  block <- getBlockType
  return BlockSig{..}

readPfsHeader :: Get PfsHeader
readPfsHeader = do
  start            <- bytesRead
  version          <- getInt64le
  magic            <- getInt64le
  id_              <- getInt64le
  fmode            <- getWord8
  clean            <- getWord8
  readOnly         <- getWord8
  rsv              <- getWord8
  mode             <- getWord16le
  unk1             <- getWord16le
  blockSize        <- getWord32le
  nBackup          <- getWord32le
  nBlock           <- getInt64le
  dinodeCount      <- getInt64le
  ndblock          <- getInt64le
  dinodeBlockCount <- getInt64le
  skip 8 -- skip a 64-bit zero
  inodeBlockSig    <- readInode getInt64le
  when (version /= 1 || magic /= 20130315) $ fail "Invalid PFS superblock version or magic"
  current          <- bytesRead
  skip $ fromIntegral $ (start + 0x370) - current
  seed             <- getByteString 16
  return PfsHeader{..}

data Pfs = Pfs
  { header :: PfsHeader
  -- TODO
  } deriving (Show)

readPfs :: Handle -> PKG -> IO Pfs
readPfs h pkg = do
  hSeek h AbsoluteSeek $ fromIntegral pkg.header0x400.pfs_image_offset
  header <- BL.hGet h 0x400 >>= runGetM readPfsHeader
  -- TODO
  return Pfs{..}
