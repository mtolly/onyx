{- |

This code was written with the help of

  * X360 and Le Fluffie by DJ Shepherd - <https://github.com/mtolly/X360>
  * py360 by arkem - <https://github.com/arkem/py360>
  * Free60 - <https://free60project.github.io/wiki/STFS.html>

-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module STFS.Package
( extractSTFS
, getSTFSFolder
, rb3pkg
, rb2pkg
, gh2pkg
, ghworpkg
, makeCON
, makeCONMemory
, makeCONReadable
, CreateOptions(..)
, stfsFolder
, withSTFSPackage
, STFSPackage(..)
, LicenseEntry(..)
, Header(..)
, Metadata(..)
, runGetM, runGetMOffset
, makePack
, repackOptions
, saveCreateOptions
, loadCreateOptions
, packCombineFolders
, rb3STFSOptions, rb2STFSOptions
) where

import           Control.Monad.Codec
import           Control.Monad.Extra            (allM, forM, forM_, guard,
                                                 replicateM, unless, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Crypto.Hash                    (Digest, hash)
import           Crypto.Hash.Algorithms         (MD5, SHA1 (..))
import           Crypto.PubKey.RSA.PKCS15       (sign)
import           Crypto.PubKey.RSA.Types
import           Data.Binary.Codec
import           Data.Binary.Codec.Class
import           Data.Bits
import           Data.ByteArray                 (convert)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Either                    (lefts, rights)
import           Data.Foldable                  (toList)
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import           Data.List.Extra                (nubOrd, partition)
import qualified Data.Map                       as Map
import           Data.Maybe                     (catMaybes, fromMaybe,
                                                 isNothing, mapMaybe)
import           Data.Profunctor                (dimap)
import           Data.Sequence                  ((|>))
import qualified Data.Sequence                  as Seq
import           Data.SimpleHandle
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Time
import qualified Data.Vector.Unboxed            as VU
import           Data.Yaml                      ((.:))
import qualified Data.Yaml                      as Y
import           Resources                      (gh2Thumbnail, ghWoRthumbnail,
                                                 rb2Thumbnail, rb3Thumbnail,
                                                 xboxKV)
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
  , ch_CertSignature           :: B.ByteString -- 0x100 bytes. this is 0xA60 to 0xB60 in KV.bin
  , ch_Signature               :: B.ByteString -- 0x80 bytes.
  -- use kv.bin to sign "0x118 bytes starting from 0x22C"
  -- which is the license entries, header hash, and header size
  -- and then reverse the signature.
  } deriving (Eq, Show)

utf8String :: Int -> BinaryCodec T.Text
utf8String n = dimap
  (\s -> B.take n $ encodeUtf8 s <> B.replicate n 0) -- this might result in broken utf8
  (T.takeWhile (/= '\0') . decodeUtf8)
  $ byteString n

utf16BEString :: Int -> BinaryCodec T.Text
utf16BEString n = dimap
  (\s -> B.take n $ encodeUtf16BE s <> B.replicate n 0)
  (T.takeWhile (/= '\0') . decodeUtf16BE)
  $ byteString n

instance Bin CONHeader where
  bin = do
    ch_PublicKeyCertSize       <- ch_PublicKeyCertSize       =. byteString 2
    ch_CertOwnerConsoleID      <- ch_CertOwnerConsoleID      =. byteString 5
    ch_CertOwnerConsolePartNum <- ch_CertOwnerConsolePartNum =. utf8String 0x14
    ch_CertOwnerConsoleType    <- ch_CertOwnerConsoleType    =. bin
    ch_CertDateGeneration      <- ch_CertDateGeneration      =. utf8String 8
    ch_PublicExponent          <- ch_PublicExponent          =. byteString 4
    ch_PublicModulus           <- ch_PublicModulus           =. byteString 0x80
    ch_CertSignature           <- ch_CertSignature           =. byteString 0x100
    ch_Signature               <- ch_Signature               =. byteString 0x80
    return CONHeader{..}

data ConsoleType = Devkit | Retail
  deriving (Eq, Show)

instance Bin ConsoleType where
  bin = Codec
    { codecIn = getWord8 >>= \case
      1 -> return Devkit
      2 -> return Retail
      b -> fail $ "Unrecognized CON console type: " <> show b
    , codecOut = fmapArg $ putWord8 . \case
      Devkit -> 1
      Retail -> 2
    }

data LIVEHeader = LIVEHeader
  { lh_PackageSignature :: B.ByteString
  } deriving (Eq, Show)

instance Bin LIVEHeader where
  bin = do
    lh_PackageSignature <- lh_PackageSignature =. byteString 0x100
    _ <- const (B.replicate 0x128 0) =. byteString 0x128
    return LIVEHeader{..}

instance Bin Header where
  bin = Codec
    { codecIn = getByteString 4 >>= \case
      "CON " -> CON  <$> codecIn bin
      "LIVE" -> LIVE <$> codecIn bin
      "PIRS" -> PIRS <$> codecIn bin
      s      -> fail $ "Unrecognized STFS magic: " <> show s
    , codecOut = fmapArg $ \case
      CON  x -> void $ putByteString "CON " >> codecOut bin x
      LIVE x -> void $ putByteString "LIVE" >> codecOut bin x
      PIRS x -> void $ putByteString "PIRS" >> codecOut bin x
    }

data Metadata = Metadata
  { md_LicenseEntries       :: [LicenseEntry] -- 0x10 licenses, each 0x10 bytes
  , md_HeaderSHA1           :: B.ByteString -- 0x14 bytes: hash from 0x344 to first hash table
  , md_HeaderSize           :: Word32
  , md_ContentType          :: ContentType -- 4 bytes
  , md_MetadataVersion      :: Int32 -- 4 bytes: 1 or 2 (doesn't really affect us)
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
  , md_DisplayName          :: [T.Text] -- 0x900 bytes, split up into 0x100 (0x80 chars) for each locale
  , md_DisplayDescription   :: [T.Text] -- 0x900 bytes, split up into 0x100 (0x80 chars) for each locale
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

instance Bin Platform where
  bin = Codec
    { codecIn = getWord8 >>= \case
      0 -> return P_Unknown
      2 -> return P_Xbox360
      4 -> return P_PC
      b -> fail $ "Unrecognized STFS platform: " <> show b
    , codecOut = fmapArg $ putWord8 . \case
      P_Unknown -> 0
      P_Xbox360 -> 2
      P_PC      -> 4
    }

data DescriptorType = D_STFS | D_SVOD
  deriving (Eq, Show)

instance Bin DescriptorType where
  bin = Codec
    { codecIn = getWord32be >>= \case
      0 -> return D_STFS
      1 -> return D_SVOD
      b -> fail $ "Unrecognized STFS descriptor type: " <> show b
    , codecOut = fmapArg $ putWord32be . \case
      D_STFS -> 0
      D_SVOD -> 1
    }

fixedList :: Int -> BinaryCodec a -> BinaryCodec [a]
fixedList n cdc = Codec
  { codecIn = replicateM n $ codecIn cdc
  , codecOut = \xs -> if length xs == n
    then mapM (codecOut cdc) xs
    else error $ "fixedList: expected a list of size " <> show n <> " but got " <> show (length xs)
  }

instance Bin Metadata where
  bin = do
    md_LicenseEntries       <- md_LicenseEntries       =. fixedList 0x10 bin
    md_HeaderSHA1           <- md_HeaderSHA1           =. byteString 0x14
    md_HeaderSize           <- md_HeaderSize           =. word32be
    md_ContentType          <- md_ContentType          =. bin
    md_MetadataVersion      <- md_MetadataVersion      =. int32be
    md_ContentSize          <- md_ContentSize          =. int64be
    md_MediaID              <- md_MediaID              =. word32be
    md_Version              <- md_Version              =. int32be
    md_BaseVersion          <- md_BaseVersion          =. int32be
    md_TitleID              <- md_TitleID              =. word32be
    md_Platform             <- md_Platform             =. bin
    md_ExecutableType       <- md_ExecutableType       =. word8
    md_DiscNumber           <- md_DiscNumber           =. word8
    md_DiscInSet            <- md_DiscInSet            =. word8
    md_SaveGameID           <- md_SaveGameID           =. word32be
    md_ConsoleID            <- md_ConsoleID            =. byteString 5
    md_ProfileID            <- md_ProfileID            =. byteString 8
    md_VolumeDescriptor     <- md_VolumeDescriptor     =. bin
    md_DataFileCount        <- md_DataFileCount        =. int32be
    md_DataFileCombinedSize <- md_DataFileCombinedSize =. int64be
    md_DescriptorType       <- md_DescriptorType       =. bin
    md_Reserved             <- md_Reserved             =. int32be
    md_Padding              <- md_Padding              =. byteString 0x4C
    md_DeviceID             <- md_DeviceID             =. byteString 0x14
    md_DisplayName          <- md_DisplayName          =. fixedList 9 (utf16BEString 0x100)
    md_DisplayDescription   <- md_DisplayDescription   =. fixedList 9 (utf16BEString 0x100)
    md_PublisherName        <- md_PublisherName        =. utf16BEString 0x80
    md_TitleName            <- md_TitleName            =. utf16BEString 0x80
    md_TransferFlags        <- md_TransferFlags        =. word8
    (md_ThumbnailImage, md_TitleThumbnailImage) <- Codec
      { codecIn = do
        thumbSize <- getInt32be
        titleSize <- getInt32be
        thumb <- B.take (fromIntegral thumbSize) <$> getByteString 0x4000
        title <- B.take (fromIntegral titleSize) <$> getByteString 0x4000
        return (thumb, title)
      , codecOut = \md -> do
        let thumbSize = B.length $ md_ThumbnailImage md
            titleSize = B.length $ md_TitleThumbnailImage md
        putInt32be $ fromIntegral thumbSize
        putInt32be $ fromIntegral titleSize
        putByteString $ md_ThumbnailImage      md <> B.replicate (0x4000 - thumbSize) 0
        putByteString $ md_TitleThumbnailImage md <> B.replicate (0x4000 - titleSize) 0
        return (md_ThumbnailImage md, md_TitleThumbnailImage md)
      }
    return Metadata{..}

data LicenseEntry = LicenseEntry
  { le_LicenseID    :: Int64 -- 8 bytes: XUID / PUID / console id
  , le_LicenseBits  :: Int32 -- 4 bytes
  , le_LicenseFlags :: Int32 -- 4 bytes
  } deriving (Eq, Show)

instance Bin LicenseEntry where
  bin = do
    le_LicenseID    <- le_LicenseID    =. int64be
    le_LicenseBits  <- le_LicenseBits  =. int32be
    le_LicenseFlags <- le_LicenseFlags =. int32be
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

int24le :: BinaryCodec Int32
int24le = Codec
  { codecIn = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    return $ fromIntegral a + fromIntegral b * 0x100 + fromIntegral c * 0x10000
  , codecOut = fmapArg $ \w -> do
    putWord16le $ fromIntegral w
    putWord8 $ fromIntegral $ w `shiftR` 16
  }

int24be :: BinaryCodec Int32
int24be = Codec
  { codecIn = do
    a <- getWord8
    b <- getWord8
    c <- getWord8
    return $ fromIntegral a * 0x10000 + fromIntegral b * 0x100 + fromIntegral c
  , codecOut = fmapArg $ \w -> do
    putWord8 $ fromIntegral $ w `shiftR` 16
    putWord16be $ fromIntegral w
  }

instance Bin STFSDescriptor where
  bin = do
    sd_VolDescSize                <- sd_VolDescSize                =. word8
    sd_Reserved                   <- sd_Reserved                   =. word8
    sd_BlockSeparation            <- sd_BlockSeparation            =. word8
    sd_FileTableBlockCount        <- sd_FileTableBlockCount        =. int16le
    sd_FileTableBlockNumber       <- sd_FileTableBlockNumber       =. int24le
    sd_TopHashTableHash           <- sd_TopHashTableHash           =. byteString 0x14
    sd_TotalAllocatedBlockCount   <- sd_TotalAllocatedBlockCount   =. int32be
    sd_TotalUnallocatedBlockCount <- sd_TotalUnallocatedBlockCount =. int32be
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
  deriving (Eq, Ord, Show, Enum, Bounded)

contentTypeID :: ContentType -> Word32
contentTypeID = \case
  CT_ArcadeTitle        -> 0xD0000
  CT_AvatarItem         -> 0x9000
  CT_CacheFile          -> 0x40000
  CT_CommunityGame      -> 0x2000000
  CT_GameDemo           -> 0x80000
  CT_GamerPicture       -> 0x20000
  CT_GameTitle          -> 0xA0000
  CT_GameTrailer        -> 0xC0000
  CT_GameVideo          -> 0x400000
  CT_InstalledGame      -> 0x4000
  CT_Installer          -> 0xB0000
  CT_IPTVPauseBuffer    -> 0x2000
  CT_LicenseStore       -> 0xF0000
  CT_MarketplaceContent -> 0x2
  CT_Movie              -> 0x100000
  CT_MusicVideo         -> 0x300000
  CT_PodcastVideo       -> 0x500000
  CT_Profile            -> 0x10000
  CT_Publisher          -> 0x3
  CT_SavedGame          -> 0x1
  CT_StorageDownload    -> 0x50000
  CT_Theme              -> 0x30000
  CT_TV                 -> 0x200000
  CT_Video              -> 0x90000
  CT_ViralVideo         -> 0x600000
  CT_XboxDownload       -> 0x70000
  CT_XboxOriginalGame   -> 0x5000
  CT_XboxSavedGame      -> 0x60000
  CT_Xbox360Title       -> 0x1000
  CT_XboxTitle          -> 0x5000
  CT_XNA                -> 0xE0000

instance Bin ContentType where
  bin = Codec
    { codecIn = do
      w <- getWord32be
      case filter (\ct -> contentTypeID ct == w) [minBound .. maxBound] of
        ct : _ -> return ct
        []     -> fail $ "Unrecognized STFS content type: " <> show w
    , codecOut = fmapArg $ putWord32be . contentTypeID
    }

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

instance Bin BlockStatus where
  bin = Codec
    { codecIn = getWord8 >>= \case
      0x00 -> return BlockUnused
      0x40 -> return BlockFree
      0x80 -> return BlockUsed
      0xC0 -> return BlockNewlyAllocated
      b    -> fail $ "Unrecognized block hash record status: " <> show b
    , codecOut = fmapArg $ putWord8 . \case
      BlockUnused         -> 0x00
      BlockFree           -> 0x40
      BlockUsed           -> 0x80
      BlockNewlyAllocated -> 0xC0
    }

instance Bin BlockHashRecord where
  bin = do
    bhr_SHA1      <- bhr_SHA1      =. byteString 0x14
    bhr_Status    <- bhr_Status    =. bin
    bhr_NextBlock <- bhr_NextBlock =. int24be
    return BlockHashRecord{..}

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
  , fe_UpdateTimestamp :: Word32
  , fe_AccessTimestamp :: Word32
  } deriving (Eq, Show)

readFatTime :: Word32 -> LocalTime
readFatTime ts = LocalTime
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

showFatTime :: LocalTime -> Word32
showFatTime lt = let
  (y, m, d) = toGregorian $ localDay lt
  in foldr (.|.) 0
    [ fromIntegral (y - 1980) `shiftL` 25
    , fromIntegral m `shiftL` 21
    , fromIntegral d `shiftL` 16
    , fromIntegral (todHour $ localTimeOfDay lt) `shiftL` 11
    , fromIntegral (todMin $ localTimeOfDay lt) `shiftL` 6
    , floor $ todSec $ localTimeOfDay lt
    ]

instance Bin FileEntry where
  bin = do
    (fe_FileName, fe_Consecutive, fe_Directory) <- Codec
      { codecIn = do
        bytes <- getByteString 0x28
        len <- getWord8
        let name = decodeLatin1 $ B.take (fromIntegral $ len .&. 0x3F) bytes
            cons = len `testBit` 6
            dir  = len `testBit` 7
        return (name, cons, dir)
      , codecOut = \fe -> do
        let str = fe_FileName fe
        putByteString $ encodeUtf8 $ str <> T.replicate (0x28 - T.length str) "\0"
        putWord8 $ foldr (.|.) (fromIntegral $ T.length str)
          [ if fe_Consecutive fe then bit 6 else 0
          , if fe_Directory   fe then bit 7 else 0
          ]
        return (fe_FileName fe, fe_Consecutive fe, fe_Directory fe)
      }
    fe_Blocks1         <- fe_Blocks1         =. int24le
    fe_Blocks2         <- fe_Blocks2         =. int24le
    fe_FirstBlock      <- fe_FirstBlock      =. int24le
    fe_PathIndex       <- fe_PathIndex       =. int16be
    fe_Size            <- fe_Size            =. word32be
    fe_UpdateTimestamp <- fe_UpdateTimestamp =. word32le
    fe_AccessTimestamp <- fe_AccessTimestamp =. word32le
    return FileEntry{..}

data STFSPackage = STFSPackage
  { stfsHandle   :: Handle
  , stfsHeader   :: Header
  , stfsMetadata :: Metadata
  }

runGetM :: (MonadFail m) => Get a -> BL.ByteString -> m a
runGetM = runGetMOffset 0

runGetMOffset :: (MonadFail m) => Int64 -> Get a -> BL.ByteString -> m a
runGetMOffset offset g bs = case runGetOrFail g bs of
  Left (_, pos, err) -> fail $ "Binary parse error at position " <> show (pos + offset) <> ": " <> err
  Right (_, _, x) -> return x

withSTFSPackage :: FilePath -> (STFSPackage -> IO a) -> IO a
withSTFSPackage stfs fn = withBinaryFile stfs ReadMode $ \fd -> do
  headerMetaBytes <- BL.hGet fd 0x971A
  (header, meta) <- flip runGetM headerMetaBytes $ (,)
    <$> (codecIn bin :: Get Header)
    <*> (codecIn bin :: Get Metadata)
  fn $ STFSPackage fd header meta

newtype RealBlock = RealBlock Int32
  deriving (Eq, Show)
newtype FileBlock = FileBlock Int32
  deriving (Eq, Show)

seekToRealBlock :: RealBlock -> STFSPackage -> IO ()
seekToRealBlock (RealBlock blk) stfs = do
  hSeek (stfsHandle stfs) AbsoluteSeek $ 0xc000 + fromIntegral blk * 0x1000

tableSizeShift :: Metadata -> TableSizeShift
tableSizeShift meta
  | ((md_HeaderSize meta + 0xFFF) .&. 0xF000) `shiftR` 0xC == 0xB = Shift0
  | otherwise                                                     = Shift1

seekToFileBlock :: FileBlock -> STFSPackage -> IO ()
seekToFileBlock fblk stfs = seekToRealBlock (fixBlockNumber fblk stfs) stfs

seekToBlockHash :: FileBlock -> Int32 -> STFSPackage -> IO ()
seekToBlockHash (FileBlock blk) tableOffset stfs = do
  let meta = stfsMetadata stfs
      tss = tableSizeShift meta
      tssNum = fromEnum tss
  -- Given a block number return the hash object that goes with it
  let record = blk `mod` 0xAA
  -- Num tables * space blocks between each (0xAB or 0xAC for [0])
  let tablenum = sum
        [ (blk `div` 0xAA) * fst3 (tableSpacing tss)
        , if blk >= 0xAA
          then (blk `div` 0x70E4 + 1) `shiftL` tssNum -- skip level 1 tables
          else 0
        , if blk >= 0x70E4
          then 1 `shiftL` tssNum -- If we're into level 2 add the level 2 table
          else 0
        -- Read the table block, get the correct record and pass it to BlockHashRecord
        -- Fix to point at the first table (these numbers are offset from data block numbers)
        , tableOffset - (1 `shiftL` tssNum)
        ]
  seekToRealBlock (RealBlock tablenum) stfs
  hSeek (stfsHandle stfs) RelativeSeek $ fromIntegral record * 0x18

fixBlockNumber :: FileBlock -> STFSPackage -> RealBlock
fixBlockNumber (FileBlock blk) stfs = let
  tss = fromEnum $ tableSizeShift $ stfsMetadata stfs
  in RealBlock $ blk + sum
    [ if blk >= 0xAA then ((blk `div` 0xAA) + 1) `shiftL` tss else 0
    , if blk >= 0x70E4 then ((blk `div` 0x70E4) + 1) `shiftL` tss else 0
    -- py360 had "blk > 0x70E4" but that appears to not be correct
    ]

readBlock :: Word32 -> STFSPackage -> IO B.ByteString
readBlock len stfs = B.hGet (stfsHandle stfs) $ fromIntegral len

readBlockHash :: STFSPackage -> IO BlockHashRecord
readBlockHash stfs = do
  bs <- BL.hGet (stfsHandle stfs) 0x18
  runGetM (codecIn bin) $ if BL.null bs then BL.replicate 0x18 0 else bs

getCorrectBlockHash :: FileBlock -> STFSPackage -> IO BlockHashRecord
getCorrectBlockHash fblk stfs = do
  let getBlockHash tableOffset = do
        seekToBlockHash fblk tableOffset stfs
        readBlockHash stfs
  hsh <- getBlockHash 0
  if tableSizeShift (stfsMetadata stfs) == Shift1 && bhr_Status hsh < BlockUsed
    then getBlockHash 1
    else return hsh

-- writeBlockHash :: FileBlock -> BlockHashRecord -> STFSPackage -> IO ()
-- writeBlockHash fblk bhr stfs = do
--   let tableOffset = if tableSizeShift (stfsMetadata stfs) == Shift1 then 1 else 0
--   seekToBlockHash fblk tableOffset stfs
--   BL.hPut (stfsHandle stfs) $ runPut $ void $ codecOut bin bhr

getFileEntries :: STFSPackage -> IO [FileEntry]
getFileEntries stfs = let
  stfsDesc = md_VolumeDescriptor $ stfsMetadata stfs
  readFileBlocks :: Word32 -> Int32 -> IO BL.ByteString
  readFileBlocks 0     _   = return BL.empty
  readFileBlocks count blk = do
    seekToFileBlock (FileBlock blk) stfs
    blockData <- BL.fromStrict <$> readBlock 0x1000 stfs
    blkHash <- getCorrectBlockHash (FileBlock blk) stfs
    BL.append blockData <$> readFileBlocks (count - 1) (bhr_NextBlock blkHash)
  in do
    filesBytes <- readFileBlocks
      (fromIntegral $ sd_FileTableBlockCount stfsDesc)
      (sd_FileTableBlockNumber stfsDesc)
    fmap (takeWhile $ not . T.null . fe_FileName)
      $ flip runGetM filesBytes
      $ replicateM (fromIntegral (BL.length filesBytes) `div` 0x40)
      $ codecIn bin

getFileHandle :: FileEntry -> STFSPackage -> Readable -> Readable
getFileHandle fe stfs readable = Readable
  { rFilePath = Nothing
  , rOpen = do
    h <- rOpen readable
    let stfs' = stfs { stfsHandle = h }
        stfsDesc = md_VolumeDescriptor $ stfsMetadata stfs'
        getBlockSequence :: Word32 -> FileBlock -> BlockStatus -> IO [Int32]
        getBlockSequence size fb@(FileBlock blk) info
          | size <= 0
            || blk <= 0
            || blk >= sd_TotalAllocatedBlockCount stfsDesc
            || info < BlockUsed
            = return []
          | otherwise = do
            let len = min 0x1000 size
            blkHash <- getCorrectBlockHash fb stfs'
            (blk :) <$> getBlockSequence (size - len) (FileBlock $ bhr_NextBlock blkHash) (bhr_Status blkHash)
    blks <- VU.fromList <$> if fe_Consecutive fe
      -- important optimization, this saves a lot of time when opening e.g. mogg files.
      -- unfortunately I didn't mark Onyx-created cons as consecutive until 20210702 (2ced8ab2)
      -- and I don't think X360 (C3 CON Tools, Le Fluffie, RB3Maker) marks as consecutive.
      -- but new Onyx files and real DLC files can benefit at least.
      then return $ take (fromIntegral $ fe_Blocks1 fe) [fe_FirstBlock fe ..]
      else getBlockSequence (fe_Size fe) (FileBlock $ fe_FirstBlock fe) BlockUsed
    posn <- newIORef 0
    -- TODO include the whole directory in the new label, not just fe_FileName
    openSimpleHandle (handleLabel h <> " | " <> T.unpack (fe_FileName fe)) SimpleHandle
      { shSize  = fromIntegral $ fe_Size fe
      , shSeek  = writeIORef posn
      , shTell  = readIORef posn
      , shClose = hClose h
      , shRead  = \n -> do
        p <- readIORef posn
        writeIORef posn $ p + n
        let readBlocks [] _ _ = return []
            readBlocks (fblk : fblks) offset bytesLeft = if bytesLeft <= 0
              then return []
              else do
                seekToFileBlock (FileBlock fblk) stfs'
                blockData <- B.take bytesLeft . B.drop offset <$> readBlock 0x1000 stfs'
                (blockData :) <$> readBlocks fblks 0 (bytesLeft - fromIntegral (B.length blockData))
        case quotRem p 0x1000 of
          (q, r) -> B.concat <$> readBlocks
            (VU.toList $ VU.drop (fromIntegral q) blks)
            (fromIntegral r)
            (fromIntegral n)
      }
  }

getSTFSFolder :: FilePath -> IO (Folder T.Text Readable)
getSTFSFolder stfsPath = withSTFSPackage stfsPath $ \stfs -> do
  files <- getFileEntries stfs
  let getFolder pathIndex = let
        contents = [ (i, fe) | (i, fe) <- zip [0..] files, fe_PathIndex fe == pathIndex ]
        in Folder
          { folderFiles = do
            (_, fe) <- contents
            guard $ not $ fe_Directory fe
            return (fe_FileName fe, getFileHandle fe stfs $ fileReadable stfsPath)
          , folderSubfolders = do
            (i, fe) <- contents
            guard $ fe_Directory fe
            return (fe_FileName fe, getFolder i)
          }
  return $ getFolder (-1)

extractSTFS :: FilePath -> FilePath -> IO ()
extractSTFS stfs dir = getSTFSFolder stfs >>= \folder -> saveHandleFolder folder dir

data BlockContents a
  = L0Hashes a -- ^ hashes of 0xAA data blocks
  | DataBlock a
  | L1Hashes a -- ^ hashes of 0xAA L0 hash blocks
  | L2Hashes a -- ^ hashes of 0xAA L1 hash blocks
  | Dummy
  deriving (Eq, Ord, Show)

blockContentsPattern :: TableSizeShift -> [BlockContents Int32]
blockContentsPattern tss = let
  makeHash fn = case tss of
    Shift0 -> [fn ()]
    Shift1 -> [Dummy, fn ()] -- TODO verify this is actually correct
  -- this pattern is so weird
  l0 = makeHash L0Hashes ++ replicate 0xAA (DataBlock ())
  l1first = l0 ++ makeHash L1Hashes ++ concat (replicate 0xA9 l0)
  l1later = makeHash L1Hashes ++ concat (replicate 0xAA l0)
  l2 = l1first ++ makeHash L2Hashes ++ concat (replicate 0xA9 l1later)
  go a b c d = \case
    [] -> []
    Dummy : rest -> Dummy : go a b c d rest
    L0Hashes ()  : rest -> L0Hashes  a : go (a + 1) b c d rest
    DataBlock () : rest -> DataBlock b : go a (b + 1) c d rest
    L1Hashes ()  : rest -> L1Hashes  c : go a b (c + 1) d rest
    L2Hashes ()  : rest -> L2Hashes  d : go a b c (d + 1) rest
  in go 0 0 0 0 l2

verifyHashes :: Bool -> STFSPackage -> IO ()
verifyHashes fixHashes stfs = let
  meta = stfsMetadata stfs
  sd = md_VolumeDescriptor meta
  pattern = takeWhile
    (/= DataBlock (sd_TotalAllocatedBlockCount sd + 1))
    -- TODO does this need to use sd_TotalUnallocatedBlockCount as well
    (blockContentsPattern $ tableSizeShift meta)
  patternWithIndexes = zip pattern $ map RealBlock [(-1) ..]
  patternTable = Map.fromList patternWithIndexes
  verify hashed hblk i = case Map.lookup hashed patternTable of
    Nothing -> return ()
    Just rblk -> do
      seekToRealBlock rblk stfs
      contents <- readBlock 0x1000 stfs
      unless (B.null contents) $ do
        seekToRealBlock hblk stfs
        hSeek (stfsHandle stfs) RelativeSeek $ i * 0x18
        hsh <- readBlockHash stfs
        let written = bhr_SHA1 hsh
            expected = convert (hash contents :: Digest SHA1)
        if written == expected
          then return ()
          else if fixHashes
            then do
              let hsh' = hsh { bhr_SHA1 = expected }
              seekToRealBlock hblk stfs
              hSeek (stfsHandle stfs) RelativeSeek $ i * 0x18
              BL.hPut (stfsHandle stfs) $ runPut $ void $ codecOut bin hsh'
            else putStrLn $ unwords
              [ show hashed
              , "has written hash"
              , show $ B.unpack written
              , "but calculated"
              , show $ B.unpack expected
              ]
  in do
    forM_ patternWithIndexes $ \(contents, rblk) -> case contents of
      L0Hashes i -> do
        forM_ [0 .. 0xA9] $ \j -> do
          let hashedIndex = i * 0xAA + fromIntegral j
          verify (DataBlock hashedIndex) rblk j
      _ -> return ()
    forM_ patternWithIndexes $ \(contents, rblk) -> case contents of
      L1Hashes i -> do
        forM_ [0 .. 0xA9] $ \j -> do
          let hashedIndex = i * 0xAA + fromIntegral j
          verify (L0Hashes hashedIndex) rblk j
        -- add a count of data blocks covered by this hash, seen in (some?) DLC files
        when fixHashes $ do
          seekToRealBlock rblk stfs
          hSeek (stfsHandle stfs) RelativeSeek 0xFF0
          BL.hPut (stfsHandle stfs) $ runPut $ putInt32be $ min (0xAA * 0xAA) $ sd_TotalAllocatedBlockCount sd - 0xAA * 0xAA * i
      _ -> return ()
    forM_ patternWithIndexes $ \(contents, rblk) -> case contents of
      L2Hashes i -> do
        forM_ [0 .. 0xA9] $ \j -> do
          let hashedIndex = i * 0xAA + fromIntegral j
          verify (L1Hashes hashedIndex) rblk j
        -- add a count of data blocks covered by this hash (which is all of them), seen in (some?) DLC files
        when fixHashes $ do
          seekToRealBlock rblk stfs
          hSeek (stfsHandle stfs) RelativeSeek 0xFF0
          BL.hPut (stfsHandle stfs) $ runPut $ putInt32be $ sd_TotalAllocatedBlockCount sd
      _ -> return ()

stockScramble :: B.ByteString -> B.ByteString
stockScramble bs = case quotRem (B.length bs) 8 of
  (q, 0) -> B.concat $ do
    i <- [0 .. q - 1]
    return $ B.take 8 $ B.drop ((q - 1 - i) * 8) bs
  _ -> error "stockScramble: input length not divisible by 8"

data KV = KV
  { kv_Certificate  :: B.ByteString
  , kv_PrivateKey   :: PrivateKey
  , kv_PackageMagic :: B.ByteString
  } deriving (Eq, Show)

bytesToInteger :: B.ByteString -> Integer
bytesToInteger bs = sum $ do
  (i, b) <- zip [0..] $ B.unpack $ B.reverse bs
  return $ (0x100 ^ (i :: Int)) * fromIntegral b

bytesFromInteger :: Int -> Integer -> B.ByteString
bytesFromInteger len n = if len <= 0
  then B.empty
  else bytesFromInteger (len - 1) (n `shiftR` 8)
    <> B.singleton (fromIntegral $ n .&. 0xFF)

xk0, xk3, xk4 :: B.ByteString
xk0 = B.pack [81,236,31,157,86,38,194,252,16,166,103,100,203,58,109,77,161,231,78,168,66,240,244,253,250,102,239,199,142,16,47,228,28,163,29,208,206,57,46,195,25,45,208,88,116,121,172,8,231,144,193,172,45,198,235,71,232,61,207,76,109,255,81,101,212,110,189,15,21,121,55,149,196,175,144,158,43,80,138,10,34,74,179,65,229,137,128,115,205,250,33,2,245,221,48,221,7,42,111,52,7,129,151,126,178,251,114,233,234,193,136,57,172,72,43,168,77,252,215,237,155,249,222,194,69,147,76,76]
xk3 = B.pack [31,134,143,212,255,128,118,98,205,60,200,172,170,166,104,67,176,19,125,175,0,67,246,14,197,155,196,208,213,163,80,44,129,207,141,172,234,16,7,18,26,14,85,27,200,167,221,17,17,64,100,183,69,50,218,177,176,178,243,210,221,183,24,158,24,215,105,3,48,243,119,138,224,101,191,168,244,24,201,51,136,133,117,249,194,159,105,166,22,56,128,140,28,96,45,238,68,90,142,119,104,112,161,82,145,90,161,158,212,76,199,151,229,125,20,168,91,26,97,126,93,52,68,95,146,114,108,53,15,77,71,204,91,8,98,245,64,230,221,173,141,60,158,239,68,45,51,86,58,188,22,127,131,147,53,41,1,79,174,25,235,177,15,18,222,22,146,104,233,54,164,77,85,107,144,139,34,12,129,114,156,134,235,209,50,73,34,113,52,235,176,208,51,31,121,31,230,13,94,38,66,196,202,83,49,145,41,53,172,6,128,125,158,93,167,9,235,68,22,192,68,26,85,62,156,66,34,233,142,151,79,107,12,205,197,138,38,158,47,102,17,62,152,71,68,155,112,56,86,34,199,60,61,102,78,153]
xk4 = B.pack [181,255,98,235,236,123,195,61,221,160,246,230,6,142,28,132,31,63,53,213,48,161,74,192,119,88,214,132,78,88,136,198,150,2,247,130,204,73,188,247,120,232,152,168,239,168,99,110,110,60,122,255,24,179,170,254,142,252,118,42,151,197,232,0,99,210,97,91,49,186,221,180,92,167,238,111,128,218,120,51,125,214,157,144,189,220,209,86,221,161,63,135,33,118,176,177,167,77,250,123,145,17,245,169,130,239,145,116,104,176,85,93,125,5,25,79,190,57,153,119,119,130,226,92,48,144,36,206,168,247,4,199,2,249,243,149,184,26,94,238,35,146,125,180,78,68,169,86,163,210,253,64,7,202,113,163,151,62,211,141,37,158,152,66,67,60,178,99,251,138,49,84,50,204,82,136,109,197,134,174,72,76,249,25,202,174,129,4,25,41,240,4,98,221,146,89,66,109,233,62,196,38,200,57,97,60,236,229,120,7,56,178,249,235,100,24,193,10,70,22,102,84,67,172,47,91,47,127,99,137,169,148,101,48,98,178,209,180,45,126,179,186,209,221,47,89,168,63,146,18,205,135,39,241,41,165,234,183,23,109,53,32,215,214,169,156,187,121,78,56,161,156,74,252,84,95,124,108,4,23,118,154,27,211,13,224,15,9,158,145,129,210,51,6,58,61,14,255,168,26,7,34,159,39,119,102,255,204,234,30,157,131,79,47,103,155,185,1,133,176,38,83,14,69,132,92,168,108,205,12,246,30,43,202,244,45,166,81,17,106,43,191,132,202,71,223,30,167,125,57,47,59,188,236,221,24,35,84,131,73,3,172,110,32,219,142,79,231,174,126,4,155,90,11,186,226,91,14,63,223,25,27,237,123,198,128,116,251,181,197,75,142,243,80,225,123,142,254,4,76,22,154,236,253,203,174,35,210,251,137,38,205,176,210,94,231,200,254,94,81,172,13,238,71,56,217,6,182,178,3,76,241,57,236,200,142,230,203,190,75,248,199,215,233,56,135,204,223,253,23,230,206,41,170,45,192,74,13,101,176,166,155,13,155,180,253,171,242,71,4,224,37,250,207,58,24,141,47,241,169,188,253,63,25,174,242,168,197,158,166,234,240,183,109,0,173,80,5,199,165,182,79,247,207,139,235,141,237,97,179,75,95,156,122,15,158,35,107,58,143,27,189,210,80,222,208,107,189,135,82,226,234,82,242,173,100,249,188,18,140,179,234,180,177,20,97,1,54,204,174,209,126,9,255,197,102,175,108,106,26,79,153,255,221,241,105,190,87,138,31,154,103,208,171,174,117,110,226,9,131,173,147,26,243,51,93,249,105,114,135,77,115,196,54,11,156,29,42,88,134,218,148,191,26,83,123,116,210,125,243,62,16,23,141,172,219,87,200,73,107,61,9,138,154,116,84,3,18,60,7,209,236,60,180,42,148,187,103,243,167,132,85,163,82,121,46,50,95,76,224,150,82,95,84,2,221,100,103,72,169,50,116,23,225,253,6,25,222,117,225,148,154,133,254,233,139,200,9,73,132,208,144,175,36,118,172,221,246,38,157,219,9,239,50,126,221,80,133,58,155,123,5,51,63,254,15,239,52,27,198,201,42,220,8,238,117,196,103,94,103,205,254,114,161,132,173,234,195,252,138,38,187,8,202,161,27,211,83,127,102,116,161,197,217,20,111,71,75,36,243,85,200,224,3,218,110,121,138,165,53,7,242,94,158,65,34,50,63,91,254,121,244,25,41,169,127,10,150,26,196,120,92,131,10,43,18,221,181,5,12,127,73,114,163,99,202,76,188,84,61,61,213,43,187,180,204,232,21,234,203,19,201,26,120,199,146,224,167,225,134,4,217,213,239,59,40,27,248,245,51,169,168,97,194,218,130,11,165,251,248,122,142,102,28,33,16,168,102,88,97,184,220,51,211,78,21,94,131,173,71,181,255,9,53,163,237,228,196,65,210,51,142,110,49,44,254,76,216,253,158,43,208,60,78,39,143,71,36,165,165,199,55,146,208,216,141]

loadKVbinCON :: B.ByteString -> KV
loadKVbinCON bs = let
  base = case B.length bs of
    0x3FF0 -> bs
    0x4000 -> B.drop 0x10 bs
    _      -> error "loadKVbinCON: incorrect size"
  xc = B.take 0x1A8 $ B.drop 0x9B8 base
  d = bytesToInteger xk0 -- constant
  e = bytesToInteger $ B.take 4 $ B.drop 0x28C base
  modulus = bytesToInteger $ stockScramble $ B.take 0x80 $ B.drop 0x298 base
  p = bytesToInteger $ stockScramble $ B.take 0x40 $ B.drop (0x298 + 0x80) base
  q = bytesToInteger $ stockScramble $ B.take 0x40 $ B.drop (0x298 + 0x80 + 0x40) base
  dp = bytesToInteger $ stockScramble $ B.take 0x40 $ B.drop (0x298 + 0x80 + 0x40 * 2) base
  dq = bytesToInteger $ stockScramble $ B.take 0x40 $ B.drop (0x298 + 0x80 + 0x40 * 3) base
  inverseq = bytesToInteger $ stockScramble $ B.take 0x40 $ B.drop (0x298 + 0x80 + 0x40 * 4) base
  in KV
    { kv_Certificate = xc
    , kv_PrivateKey = PrivateKey
      { private_pub = PublicKey
        { public_size = 0x80 -- I think?
        , public_n = modulus
        , public_e = e
        }
      , private_d = d
      , private_p = p
      , private_q = q
      , private_dP = dp
      , private_dQ = dq
      , private_qinv = inverseq
      }
    , kv_PackageMagic = "CON "
    }

loadKVbinLIVE :: KV
loadKVbinLIVE = let
  base = xk4
  e = 3
  d = bytesToInteger xk3
  modulus = bytesToInteger $ B.take 0x100 base
  p = bytesToInteger $ B.take 0x80 $ B.drop 0x100 base
  q = bytesToInteger $ B.take 0x80 $ B.drop (0x100 + 0x80) base
  dp = bytesToInteger $ B.take 0x80 $ B.drop (0x100 + 0x80 * 2) base
  dq = bytesToInteger $ B.take 0x80 $ B.drop (0x100 + 0x80 * 3) base
  inverseq = bytesToInteger $ B.take 0x80 $ B.drop (0x100 + 0x80 * 4) base
  xc = B.replicate 0x1A8 0
  in KV
    { kv_Certificate = xc
    , kv_PrivateKey = PrivateKey
      { private_pub = PublicKey
        { public_size = 0x100 -- I think?
        , public_n = modulus
        , public_e = e
        }
      , private_d = d
      , private_p = p
      , private_q = q
      , private_dP = dp
      , private_dQ = dq
      , private_qinv = inverseq
      }
    , kv_PackageMagic = "LIVE"
    }

-- testSign :: FilePath -> FilePath -> IO ()
-- testSign kv msg = do
--   kv' <- loadKVbin <$> B.readFile kv
--   print kv'
--   msg' <- B.readFile msg
--   print $ sign Nothing (Just SHA1) (kv_PrivateKey kv') msg'

-- readAsBlocks :: FilePath -> IO (Integer, [B.ByteString])
-- readAsBlocks f = do
--   size <- Dir.getFileSize f
--   blocks <- withBinaryFile f ReadMode $ \h -> fix $ \go -> do
--     blk <- B.hGet h 0x1000
--     if B.null blk
--       then return []
--       else let
--         blk' = if B.length blk == 0x1000
--           then blk
--           else blk <> B.replicate (0x1000 - B.length blk) 0
--         in (blk' :) <$> go
--   return (size, blocks)

-- parent index, name, maybe (length, data blocks)
type BlocksPlan = [(Int, T.Text, Maybe (Integer, IO [B.ByteString]))]

traverseReadable :: Folder T.Text Readable -> IO BlocksPlan
traverseReadable top = toList <$> execStateT (go (-1) top) Seq.empty where
  go parentIndex folder = do
    forM_ (folderSubfolders folder) $ \(f, sub) -> do
      thisIndex <- gets Seq.length
      modify (|> (parentIndex, f, Nothing))
      go thisIndex sub
    forM_ (folderFiles folder) $ \(f, r) -> do
      lenBlocks <- splitIntoBlocks r
      modify (|> (parentIndex, f, Just lenBlocks))
  splitIntoBlocks r = do
    len <- liftIO $ useHandle r hFileSize
    let continueSplit bs = if BL.null bs
          then []
          else let
            (h, t) = BL.splitAt 0x1000 bs
            in BL.toStrict h : continueSplit t
        getBlocks = continueSplit <$> useHandle r handleToByteString
    return (len, getBlocks)

traverseMemory :: Folder T.Text BL.ByteString -> BlocksPlan
traverseMemory top = toList $ execState (go (-1) top) Seq.empty where
  go parentIndex folder = do
    forM_ (folderSubfolders folder) $ \(f, sub) -> do
      thisIndex <- gets Seq.length
      modify (|> (parentIndex, f, Nothing))
      go thisIndex sub
    forM_ (folderFiles folder) $ \(f, bs) -> do
      modify (|> (parentIndex, f, Just (fromIntegral $ BL.length bs, return $ splitIntoBlocks bs)))
  splitIntoBlocks bs = if BL.null bs
    then []
    else let
      (h, t) = BL.splitAt 0x1000 bs
      in BL.toStrict h : splitIntoBlocks t

data CreateOptions = CreateOptions
  { createNames         :: [T.Text]
  , createDescriptions  :: [T.Text]
  , createTitleID       :: Word32
  , createTitleName     :: T.Text
  , createThumb         :: B.ByteString
  , createTitleThumb    :: B.ByteString
  , createLicenses      :: [LicenseEntry]
  , createMediaID       :: Word32
  , createVersion       :: Int32
  , createBaseVersion   :: Int32
  , createTransferFlags :: Word8
  , createLIVE          :: Bool
  }

gh2pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
gh2pkg title desc dir fout = inside "making GH2 LIVE package" $ stackIO $ do
  thumb <- gh2Thumbnail >>= B.readFile
  makeCON CreateOptions
    { createNames = [title]
    , createDescriptions = [desc]
    , createTitleID = 0x415607E7
    , createTitleName = "Guitar Hero II"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicenses = [LicenseEntry (-1) 1 0] -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE = True
    } dir fout

ghworpkg :: (MonadIO m) => [T.Text] -> [T.Text] -> FilePath -> FilePath -> StackTraceT m ()
ghworpkg titles descs dir fout = inside "making GH:WoR LIVE package" $ stackIO $ do
  thumb <- ghWoRthumbnail >>= B.readFile
  makeCON CreateOptions
    { createNames = titles
    , createDescriptions = descs
    , createTitleID = 0x41560883
    , createTitleName = "Guitar Hero : Warriors of Rock" -- save data might be under "GH™: Warriors of Rock"?
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicenses = [LicenseEntry (-1) 1 1, LicenseEntry (-1) 1 0]
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE = True
    } dir fout

rb3STFSOptions :: T.Text -> T.Text -> Bool -> IO CreateOptions
rb3STFSOptions title desc live = do
  thumb <- rb3Thumbnail >>= B.readFile
  return CreateOptions
    { createNames = [title]
    , createDescriptions = [desc]
    , createTitleID = 0x45410914
    , createTitleName = "Rock Band 3"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicenses = [LicenseEntry (-1) 1 0] -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE = live
    }

rb3pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb3pkg title desc dir fout = inside "making RB3 CON package" $ stackIO $ do
  opts <- rb3STFSOptions title desc False
  makeCON opts dir fout

rb2STFSOptions :: T.Text -> T.Text -> Bool -> IO CreateOptions
rb2STFSOptions title desc live = do
  thumb <- rb2Thumbnail >>= B.readFile
  return CreateOptions
    { createNames = [title]
    , createDescriptions = [desc]
    , createTitleID = 0x45410869
    , createTitleName = "Rock Band 2"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicenses = [LicenseEntry (-1) 1 0] -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE = live
    }

rb2pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb2pkg title desc dir fout = inside "making RB2 CON package" $ stackIO $ do
  opts <- rb2STFSOptions title desc False
  makeCON opts dir fout

makeCON :: CreateOptions -> FilePath -> FilePath -> IO ()
makeCON opts dir con = do
  folder <- crawlFolder dir
  (opts', folder') <- case partition ((== "onyx-repack") . fst) $ folderSubfolders folder of
    ([], _) -> return (opts, folder)
    ((_, sub) : _, otherSubs) -> do
      let folder' = folder { folderSubfolders = otherSubs }
      loadCreateOptions sub >>= \case
        Nothing -> return (opts, folder')
        Just opts' -> return (opts', folder')
  fileList <- traverseReadable folder'
  makeCONGeneral opts' fileList con

makeCONMemory :: CreateOptions -> Folder T.Text BL.ByteString -> FilePath -> IO ()
makeCONMemory opts mem con = makeCONGeneral opts (traverseMemory mem) con

makeCONReadable :: CreateOptions -> Folder T.Text Readable -> FilePath -> IO ()
makeCONReadable opts mem con = traverseReadable mem >>= \plan -> makeCONGeneral opts plan con

makeCONGeneral :: CreateOptions -> BlocksPlan -> FilePath -> IO ()
makeCONGeneral opts fileList con = withBinaryFile con ReadWriteMode $ \fd -> do
  hSetFileSize fd 0

  -- each 0x1000 block can store 64 file entries (each of which is 64 bytes)
  -- and then we need one at the end for the final (null) entry
  let listBlocks = ((length fileList + 1) `quot` 64) + 1
      totalBlocks = sum $ listBlocks : do
        (_, _, Just (size, _)) <- fileList
        return $ fileSizeToBlocks size
      fileSizeToBlocks fileSize = fromIntegral $ div (fileSize - 1) 0x1000 + 1

  let metadata = Metadata
        { md_LicenseEntries = take 0x10
          $ createLicenses opts
          <> repeat (LicenseEntry 0 0 0)
        , md_HeaderSHA1 = B.replicate 0x14 0 -- filled in later
        , md_HeaderSize = 0xAD0E
        , md_ContentType = if createLIVE opts then CT_MarketplaceContent else CT_SavedGame
        , md_MetadataVersion = 2
        , md_ContentSize = 0 -- filled in later
        , md_MediaID = createMediaID opts
        , md_Version = createVersion opts
        , md_BaseVersion = createBaseVersion opts
        , md_TitleID = createTitleID opts
        , md_Platform = P_Unknown
        , md_ExecutableType = 0
        , md_DiscNumber = 0
        , md_DiscInSet = 0
        , md_SaveGameID = 0
        , md_ConsoleID = B.replicate 5 0
        , md_ProfileID = B.replicate 8 0
        , md_VolumeDescriptor = STFSDescriptor
          { sd_VolDescSize                = 0x24
          , sd_Reserved                   = 0
          , sd_BlockSeparation            = 1
          , sd_FileTableBlockCount        = fromIntegral listBlocks
          , sd_FileTableBlockNumber       = 0
          , sd_TopHashTableHash           = B.replicate 0x14 0 -- filled in later
          , sd_TotalAllocatedBlockCount   = fromIntegral totalBlocks
          , sd_TotalUnallocatedBlockCount = 0
          }
        , md_DataFileCount = 0
        , md_DataFileCombinedSize = 0
        , md_DescriptorType = D_STFS
        , md_Reserved = 0
        , md_Padding = B.replicate 0x4C 0
        , md_DeviceID = B.replicate 0x14 0
        , md_DisplayName = take 9 $ createNames opts <> repeat ""
        , md_DisplayDescription = take 9 $ createDescriptions opts <> repeat ""
        , md_PublisherName = ""
        , md_TitleName = createTitleName opts
        , md_TransferFlags = createTransferFlags opts
        , md_ThumbnailImage = createThumb opts
        , md_TitleThumbnailImage = createTitleThumb opts
        }
  hSeek fd AbsoluteSeek 0x22C
  BL.hPut fd $ runPut $ void $ codecOut bin metadata

  kv <- xboxKV >>= B.readFile
  let key = if createLIVE opts then loadKVbinLIVE else loadKVbinCON kv
      header maybeSig = if createLIVE opts
        then LIVE LIVEHeader
          { lh_PackageSignature = fromMaybe (B.replicate 0x100 0) maybeSig
          }
        else CON CONHeader
          { ch_PublicKeyCertSize       = B.pack [0x01, 0xA8]
          , ch_CertOwnerConsoleID      = B.pack [0x09, 0x12, 0xBA, 0x26, 0xE3]
          , ch_CertOwnerConsolePartNum = "X803395-001"
          , ch_CertOwnerConsoleType    = Retail
          , ch_CertDateGeneration      = "09-18-06"
          , ch_PublicExponent          = bytesFromInteger 4 $ public_e $ private_pub $ kv_PrivateKey key
          , ch_PublicModulus           = stockScramble $ bytesFromInteger 0x80 $ public_n $ private_pub $ kv_PrivateKey key
          , ch_CertSignature           = B.take 0x100 $ B.drop 0xA60 kv
          , ch_Signature               = fromMaybe (B.replicate 0x80 0) maybeSig
          }
      initHeader = header Nothing
      initPackage = STFSPackage
        { stfsHandle   = fd
        , stfsHeader   = initHeader
        , stfsMetadata = metadata
        }

  hSeek fd AbsoluteSeek 0
  BL.hPut fd $ runPut $ void $ codecOut bin initHeader

  let makeFileEntries _ [] = (:[]) $ FileEntry
        { fe_FileName        = ""
        , fe_Consecutive     = False
        , fe_Directory       = False
        , fe_Blocks1         = 0
        , fe_Blocks2         = 0
        , fe_FirstBlock      = 0
        , fe_PathIndex       = 0
        , fe_Size            = 0
        , fe_UpdateTimestamp = 0
        , fe_AccessTimestamp = 0
        }
      makeFileEntries used ((parent, name, blocks) : rest) = let
        thisBlocks = maybe 0 (fileSizeToBlocks . fst) blocks
        in FileEntry
          { fe_FileName        = name
          , fe_Consecutive     = True
          , fe_Directory       = isNothing blocks
          , fe_Blocks1         = thisBlocks
          , fe_Blocks2         = thisBlocks
          , fe_FirstBlock      = if isNothing blocks then 0 else used
          , fe_PathIndex       = fromIntegral parent
          , fe_Size            = maybe 0 (fromIntegral . fst) blocks
          -- these are from GHWoR DLC, probably not necessary but was trying a lot of things to make it work
          , fe_UpdateTimestamp = 0x20006F2D
          , fe_AccessTimestamp = 0x20006F2D
          } : makeFileEntries (used + thisBlocks) rest
      fileEntries = makeFileEntries (fromIntegral listBlocks) fileList

      writeFileBlock n hasNext bs = do
        seekToFileBlock (FileBlock n) initPackage
        let len = BL.length bs
            blockData = if len > 0x1000
              then BL.take 0x1000 bs
              else bs <> BL.replicate (0x1000 - len) 0
        BL.hPut fd blockData
        seekToBlockHash (FileBlock n) (if tableSizeShift metadata == Shift1 then 1 else 0) initPackage
        let hsh = BlockHashRecord
              { bhr_SHA1      = B.replicate 0x14 0 -- fixed later
              , bhr_Status    = BlockUsed
              , bhr_NextBlock = if hasNext then n + 1 else (-1)
              }
        BL.hPut fd $ runPut $ void $ codecOut bin hsh

      writeFileList _ [] = return ()
      writeFileList n ents = do
        let (thisBlock, laterBlocks) = splitAt 64 ents
        writeFileBlock n (not $ null laterBlocks) $ BL.concat
          $ map (runPut . void . codecOut bin) thisBlock
        writeFileList (n + 1) laterBlocks

      addHasNext :: [a] -> [(a, Bool)]
      addHasNext []       = []
      addHasNext [x]      = [(x, False)]
      addHasNext (x : xs) = (x, True) : addHasNext xs

      writeFiles _ [] = return ()
      writeFiles n ((_, _, Nothing) : rest) = writeFiles n rest
      writeFiles n ((_, _, Just (_size, getBlocks)) : rest) = do
        blocks <- getBlocks
        forM_ (addHasNext $ zip [n..] blocks) $ \((n', block), hasNext) -> do
          writeFileBlock n' hasNext $ BL.fromStrict block
        writeFiles (n + fromIntegral (length blocks)) rest

  writeFileList 0 fileEntries
  writeFiles (fromIntegral listBlocks) fileList

  verifyHashes True initPackage
  sizeAfterHash <- hFileSize fd
  case sizeAfterHash `rem` 0x1000 of
    0 -> return () -- usual case, last block is a data block and already 0x1000 size
    n -> do
      -- last block is a hash and we didn't pad the end with 0.
      -- not sure if it's necessary but we do that now to be safe
      hSeek fd SeekFromEnd 0
      BL.hPut fd $ BL.replicate (fromIntegral $ 0x1000 - n) 0

  nextMetadata <- do
    size <- hFileSize fd
    let pat = zip (blockContentsPattern $ tableSizeShift metadata) [-1 ..]
        topTable
          | totalBlocks < 0xAA        = L0Hashes 0
          | totalBlocks < 0xAA * 0xAA = L1Hashes 0
          | otherwise                 = L2Hashes 0
    seekToRealBlock (RealBlock $ fromMaybe 0 $ lookup topTable pat) initPackage
    table <- readBlock 0x1000 initPackage
    return metadata
      { md_ContentSize = fromIntegral size - 0xB000
      , md_VolumeDescriptor = (md_VolumeDescriptor metadata)
        { sd_TopHashTableHash = convert (hash table :: Digest SHA1)
        }
      }
  hSeek fd AbsoluteSeek 0x22C
  BL.hPut fd $ runPut $ void $ codecOut bin nextMetadata

  finalMetadata <- do
    hSeek fd AbsoluteSeek 0x344
    headerBytes <- B.hGet fd $ 0xB000 - 0x344
    return nextMetadata
      { md_HeaderSHA1 = convert (hash headerBytes :: Digest SHA1)
      }
  hSeek fd AbsoluteSeek 0x22C
  BL.hPut fd $ runPut $ void $ codecOut bin finalMetadata

  finalHeader <- do
    hSeek fd AbsoluteSeek 0x22C
    signedStuff <- B.hGet fd 0x118
    case sign Nothing (Just SHA1) (kv_PrivateKey key) signedStuff of
      Left err        -> error $ show err
      Right signature -> return $ header $ Just $ B.reverse signature
  hSeek fd AbsoluteSeek 0
  BL.hPut fd $ runPut $ void $ codecOut bin finalHeader

stfsFolder :: (MonadIO m) => FilePath -> m (Word32, Word32)
stfsFolder f = liftIO $ withBinaryFile f ReadMode $ \h -> do
  ftype <- B.hGet h 4 >>= \case
    "CON " -> return 1
    "LIVE" -> return 2
    magic  -> error $ "stfsFolder: unknown magic number " ++ show magic
  hSeek h AbsoluteSeek 0x360
  [a, b, c, d] <- map fromIntegral . B.unpack <$> B.hGet h 4
  let titleID = a * 0x1000000 + b * 0x10000 + c * 0x100 + d
  return (titleID, ftype)

packCombineFolders :: (MonadIO m) => [Folder T.Text Readable] -> StackTraceT m (Folder T.Text Readable)
packCombineFolders roots = let
  combineFolders parents folders = do
    let allNames = nubOrd $ folders >>= \f -> map fst (folderSubfolders f) <> map fst (folderFiles f)
    newContents <- fmap catMaybes $ forM allNames $ \name -> let
      matchFolders = mapMaybe (lookup name . folderSubfolders) folders
      matchFiles   = mapMaybe (lookup name . folderFiles     ) folders
      in case (matchFolders, matchFiles) of
        (_    , []   ) -> Just . Left . (name,)  <$> combineFolders (name : parents) matchFolders
        ([]   , _    ) -> fmap (Right . (name,)) <$> combineFiles parents name matchFiles
        (_ : _, _ : _) -> fatal $ "Name refers to a folder in some input CON/LIVE, but a file in others: " <> showPath parents name
    return Folder
      { folderSubfolders = lefts  newContents
      , folderFiles      = rights newContents
      }
  combineFiles parents name contents
    | ".dta" `T.isSuffixOf` name = do
      let stripBOM bs = fromMaybe bs $ BL.stripPrefix "\xEF\xBB\xBF" bs
      bytes <- stackIO $ forM contents $ \r -> stripBOM <$> useHandle r handleToByteString
      return $ Just $ makeHandle ("Pack-merged contents for " <> showPath parents name)
        $ byteStringSimpleHandle $ BL.intercalate "\n" bytes
    | ".pak.xen" `T.isSuffixOf` name = fatal "Pack creator does not yet support Neversoft GH files."
    | otherwise = case contents of
      []     -> return Nothing -- shouldn't happen
      [r]    -> return $ Just r
      r : rs -> if name == "spa.bin" -- file in official GH2 DLC that can be dropped
        then return Nothing
        else if elem name ["ICON0.PNG", "PARAM.SFO", "PS3LOGO.DAT"]
          then return $ Just r -- ps3 package root files, assume they are constant
          else do
            let hashContents rdbl = hash . BL.toStrict <$> useHandle rdbl handleToByteString :: IO (Digest MD5)
            firstHash <- stackIO $ hashContents r
            stackIO (allM (fmap (== firstHash) . hashContents) rs) >>= \case
              True  -> return $ Just r
              False -> fatal $ "File contains different contents across input CON/LIVE files: " <> showPath parents name
  showPath parents name = T.unpack $ T.intercalate "/" $ reverse $ name : parents
  in combineFolders [] roots

makePack :: (MonadIO m) => [FilePath] -> (CreateOptions -> CreateOptions) -> FilePath -> StackTraceT m ()
makePack []                 _         _    = fatal "Need at least 1 file for STFS pack"
makePack inputs@(input : _) applyOpts fout = do
  opts <- stackIO $ withSTFSPackage input $ \stfs -> return $ applyOpts CreateOptions
    { createNames         = ["Package"]
    , createDescriptions  = []
    , createTitleID       = md_TitleID             $ stfsMetadata stfs
    , createTitleName     = md_TitleName           $ stfsMetadata stfs
    , createThumb         = md_TitleThumbnailImage $ stfsMetadata stfs -- use title thumb for package as well
    , createTitleThumb    = md_TitleThumbnailImage $ stfsMetadata stfs
    , createLicenses      = [LicenseEntry (-1) 1 0] -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    , createLIVE          = case stfsHeader stfs of
      CON{} -> False
      _     -> True
    }
  roots <- stackIO $ mapM getSTFSFolder inputs
  merged <- packCombineFolders roots
  stackIO $ makeCONReadable opts merged fout

repackOptions :: STFSPackage -> CreateOptions
repackOptions stfs = CreateOptions
  { createNames         = md_DisplayName         $ stfsMetadata stfs
  , createDescriptions  = md_DisplayDescription  $ stfsMetadata stfs
  , createTitleID       = md_TitleID             $ stfsMetadata stfs
  , createTitleName     = md_TitleName           $ stfsMetadata stfs
  , createThumb         = md_ThumbnailImage      $ stfsMetadata stfs
  , createTitleThumb    = md_TitleThumbnailImage $ stfsMetadata stfs
  , createLicenses      = md_LicenseEntries      $ stfsMetadata stfs
  , createMediaID       = md_MediaID             $ stfsMetadata stfs
  , createVersion       = md_Version             $ stfsMetadata stfs
  , createBaseVersion   = md_BaseVersion         $ stfsMetadata stfs
  , createTransferFlags = md_TransferFlags       $ stfsMetadata stfs
  , createLIVE          = case stfsHeader stfs of
    CON{} -> False
    _     -> True
  }

saveCreateOptions :: CreateOptions -> Folder T.Text Readable
saveCreateOptions copts = Folder
  { folderSubfolders = []
  , folderFiles =
    [ ("thumbnail.png", makeHandle "thumbnail.png" $ byteStringSimpleHandle $ BL.fromStrict $ createThumb copts)
    , ("title-thumbnail.png", makeHandle "thumbnail.png" $ byteStringSimpleHandle $ BL.fromStrict $ createTitleThumb copts)
    , ("repack-stfs.yaml", makeHandle "repack-stfs.yaml" $ byteStringSimpleHandle $ BL.fromStrict $ Y.encode $ Y.object
      [ ("package-name", Y.toJSON $ createNames copts)
      , ("package-description", Y.toJSON $ createDescriptions copts)
      , ("title-id", Y.toJSON $ createTitleID copts) -- TODO maybe replace with hex
      , ("title-name", Y.toJSON $ createTitleName copts)
      , ("licenses", Y.toJSON $ flip map (createLicenses copts) $ \license -> Y.object
        [ ("id", Y.toJSON $ le_LicenseID license)
        , ("bits", Y.toJSON $ le_LicenseBits license)
        , ("flags", Y.toJSON $ le_LicenseFlags license)
        ])
      , ("media-id", Y.toJSON $ createMediaID copts)
      , ("version", Y.toJSON $ createVersion copts)
      , ("base-version", Y.toJSON $ createBaseVersion copts)
      , ("transfer-flags", Y.toJSON $ createTransferFlags copts)
      , ("sign-live", Y.toJSON $ createLIVE copts)
      ])
    ]
  }

loadCreateOptions :: Folder T.Text Readable -> IO (Maybe CreateOptions)
loadCreateOptions folder = do
  findByteString (return "repack-stfs.yaml") folder >>= \case
    Nothing    -> return Nothing
    Just byaml -> do
      createThumb <- findByteString (return "thumbnail.png") folder >>= \case
        Nothing -> rb3Thumbnail >>= B.readFile
        Just b  -> return $ BL.toStrict b
      createTitleThumb <- findByteString (return "title-thumbnail.png") folder >>= \case
        Nothing -> rb3Thumbnail >>= B.readFile
        Just b  -> return $ BL.toStrict b
      obj <- case Y.decodeEither' $ BL.toStrict byaml of
        Left  e -> fail $ "Failed parsing repack-stfs.yaml: " <> show e
        Right o -> return o
      let parseCreate o = do
            createNames <- o .: "package-name"
            createDescriptions <- o .: "package-description"
            createTitleID <- o .: "title-id"
            createTitleName <- o .: "title-name"
            createMediaID <- o .: "media-id"
            createVersion <- o .: "version"
            createBaseVersion <- o .: "base-version"
            createTransferFlags <- o .: "transfer-flags"
            createLIVE <- o .: "sign-live"
            licenseObjects <- o .: "licenses"
            createLicenses <- forM (licenseObjects :: [Y.Object]) $ \lobj -> do
              le_LicenseID <- lobj .: "id"
              le_LicenseBits <- lobj .: "bits"
              le_LicenseFlags <- lobj .: "flags"
              return LicenseEntry{..}
            return CreateOptions{..}
      case Y.parseEither parseCreate obj of
        Left e -> fail $ "Failed getting data from repack-stfs.yaml: " <> e
        Right opts -> return $ Just opts
