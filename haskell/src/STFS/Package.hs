{- |

This code was written with the help of

  * X360 and Le Fluffie by DJ Shepherd - <https://github.com/mtolly/X360>
  * py360 by arkem - <https://github.com/arkem/py360>
  * Free60 - <https://free60project.github.io/wiki/STFS.html>

-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module STFS.Package
( extractSTFS
, getSTFSFolder
, rb3pkg
, rb2pkg
, makeCON
, makeCONMemory
, CreateOptions(..)
, stfsFolder
, withSTFSPackage
, STFSPackage(..)
, LicenseEntry(..)
, Header(..)
, Metadata(..)
, runGetM
) where

import           Control.Monad                  (forM_, guard, replicateM,
                                                 unless, void)
import           Control.Monad.Codec
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Crypto.Hash                    (Digest, hash)
import           Crypto.Hash.Algorithms         (SHA1 (..))
import           Crypto.PubKey.RSA.PKCS15       (sign)
import           Crypto.PubKey.RSA.Types
import           Data.Binary.Codec
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.ByteArray                 (convert)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.Foldable                  (toList)
import           Data.Int
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Profunctor                (dimap)
import           Data.Sequence                  ((|>))
import qualified Data.Sequence                  as Seq
import           Data.SimpleHandle
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Time
import qualified Data.Vector.Unboxed            as VU
import           Data.Word
import           Resources                      (rb2Thumbnail, rb3Thumbnail,
                                                 xboxKV)
import           System.IO

class Bin a where
  bin :: BinaryCodec a

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
  , fe_UpdateTimestamp :: LocalTime
  , fe_AccessTimestamp :: LocalTime
  } deriving (Eq, Show)

instance Bin LocalTime where -- dunno if utc or local
  bin = dimap
    (\lt -> let
      (y, m, d) = toGregorian $ localDay lt
      in foldr (.|.) 0
        [ fromIntegral (y - 1980) `shiftL` 25
        , fromIntegral m `shiftL` 21
        , fromIntegral d `shiftL` 16
        , fromIntegral (todHour $ localTimeOfDay lt) `shiftL` 11
        , fromIntegral (todMin $ localTimeOfDay lt) `shiftL` 6
        , floor $ todSec $ localTimeOfDay lt
        ]
    )
    (\ts -> LocalTime
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
    )
    word32le

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
    fe_UpdateTimestamp <- fe_UpdateTimestamp =. bin
    fe_AccessTimestamp <- fe_AccessTimestamp =. bin
    return FileEntry{..}

data STFSPackage = STFSPackage
  { stfsHandle   :: Handle
  , stfsHeader   :: Header
  , stfsMetadata :: Metadata
  }

runGetM :: (MonadFail m) => Get a -> BL.ByteString -> m a
runGetM g bs = case runGetOrFail g bs of
  Left (_, pos, err) -> fail $ "Binary Get at position " <> show pos <> ": " <> err
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
    blks <- VU.fromList <$> getBlockSequence (fe_Size fe) (FileBlock $ fe_FirstBlock fe) BlockUsed
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
      L0Hashes i -> forM_ [0 .. 0xA9] $ \j -> do
        let hashedIndex = i * 0xAA + fromIntegral j
        verify (DataBlock hashedIndex) rblk j
      _ -> return ()
    forM_ patternWithIndexes $ \(contents, rblk) -> case contents of
      L1Hashes i -> forM_ [0 .. 0xA9] $ \j -> do
        let hashedIndex = i * 0xAA + fromIntegral j
        verify (L0Hashes hashedIndex) rblk j
      _ -> return ()
    forM_ patternWithIndexes $ \(contents, rblk) -> case contents of
      L2Hashes i -> forM_ [0 .. 0xA9] $ \j -> do
        let hashedIndex = i * 0xAA + fromIntegral j
        verify (L1Hashes hashedIndex) rblk j
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

xk0 :: B.ByteString
xk0 = B.pack [81,236,31,157,86,38,194,252,16,166,103,100,203,58,109,77,161,231,78,168,66,240,244,253,250,102,239,199,142,16,47,228,28,163,29,208,206,57,46,195,25,45,208,88,116,121,172,8,231,144,193,172,45,198,235,71,232,61,207,76,109,255,81,101,212,110,189,15,21,121,55,149,196,175,144,158,43,80,138,10,34,74,179,65,229,137,128,115,205,250,33,2,245,221,48,221,7,42,111,52,7,129,151,126,178,251,114,233,234,193,136,57,172,72,43,168,77,252,215,237,155,249,222,194,69,147,76,76]

loadKVbin :: B.ByteString -> KV
loadKVbin bs = let
  base = case B.length bs of
    0x3FF0 -> bs
    0x4000 -> B.drop 0x10 bs
    _      -> error "loadKVbin: incorrect size"
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
type BlocksPlan = [(Int, T.Text, Maybe (Integer, [B.ByteString]))]

traverseFolder :: FilePath -> IO BlocksPlan
traverseFolder top = traverseMemory <$> do
  crawlFolder top >>= traverse (\h -> useHandle h handleToByteString)

traverseMemory :: Folder T.Text BL.ByteString -> BlocksPlan
traverseMemory top = toList $ execState (go (-1) top) Seq.empty where
  go parentIndex folder = do
    forM_ (folderSubfolders folder) $ \(f, sub) -> do
      thisIndex <- gets Seq.length
      modify (|> (parentIndex, f, Nothing))
      go thisIndex sub
    forM_ (folderFiles folder) $ \(f, bs) -> do
      modify (|> (parentIndex, f, Just (fromIntegral $ BL.length bs, splitIntoBlocks bs)))
  splitIntoBlocks bs = if BL.null bs
    then []
    else let
      (h, t) = BL.splitAt 0x1000 bs
      in BL.toStrict h : splitIntoBlocks t

data CreateOptions = CreateOptions
  { createName          :: T.Text
  , createDescription   :: T.Text
  , createTitleID       :: Word32
  , createTitleName     :: T.Text
  , createThumb         :: B.ByteString
  , createTitleThumb    :: B.ByteString
  , createLicense       :: LicenseEntry
  , createMediaID       :: Word32
  , createVersion       :: Int32
  , createBaseVersion   :: Int32
  , createTransferFlags :: Word8
  }

rb3pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb3pkg title desc dir fout = inside "making RB3 CON package" $ stackIO $ do
  thumb <- rb3Thumbnail >>= B.readFile
  makeCON CreateOptions
    { createName = title
    , createDescription = desc
    , createTitleID = 0x45410914
    , createTitleName = "Rock Band 3"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicense = LicenseEntry (-1) 1 0 -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    } dir fout

rb2pkg :: (MonadIO m) => T.Text -> T.Text -> FilePath -> FilePath -> StackTraceT m ()
rb2pkg title desc dir fout = inside "making RB2 CON package" $ stackIO $ do
  thumb <- rb2Thumbnail >>= B.readFile
  makeCON CreateOptions
    { createName = title
    , createDescription = desc
    , createTitleID = 0x45410869
    , createTitleName = "Rock Band 2"
    , createThumb = thumb
    , createTitleThumb = thumb
    , createLicense = LicenseEntry (-1) 1 0 -- unlocked
    , createMediaID       = 0
    , createVersion       = 0
    , createBaseVersion   = 0
    , createTransferFlags = 0xC0
    } dir fout

makeCON :: CreateOptions -> FilePath -> FilePath -> IO ()
makeCON opts dir con = do
  fileList <- traverseFolder dir
  makeCONGeneral opts fileList con

makeCONMemory :: CreateOptions -> Folder T.Text BL.ByteString -> FilePath -> IO ()
makeCONMemory opts mem con = makeCONGeneral opts (traverseMemory mem) con

makeCONGeneral :: CreateOptions -> BlocksPlan -> FilePath -> IO ()
makeCONGeneral opts fileList con = withBinaryFile con ReadWriteMode $ \fd -> do
  hSetFileSize fd 0

  -- each 0x1000 block can store 64 file entries (each of which is 64 bytes)
  -- and then we need one at the end for the final (null) entry
  let listBlocks = ((length fileList + 1) `quot` 64) + 1
      totalBlocks = sum $ listBlocks : do
        (_, _, Just (_, blocks)) <- fileList
        return $ length blocks

  let metadata = Metadata
        { md_LicenseEntries = take 0x10
          $ createLicense opts
          : repeat (LicenseEntry 0 0 0)
        , md_HeaderSHA1 = B.replicate 0x14 0 -- filled in later
        , md_HeaderSize = 0xAD0E
        , md_ContentType = CT_SavedGame
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
        , md_DisplayName = take 9 $ createName opts : repeat ""
        , md_DisplayDescription = take 9 $ createDescription opts : repeat ""
        , md_PublisherName = ""
        , md_TitleName = createTitleName opts
        , md_TransferFlags = createTransferFlags opts
        , md_ThumbnailImage = createThumb opts
        , md_TitleThumbnailImage = createTitleThumb opts
        }
  hSeek fd AbsoluteSeek 0x22C
  BL.hPut fd $ runPut $ void $ codecOut bin metadata

  kv <- xboxKV >>= B.readFile
  let key = loadKVbin kv
      initHeader = CONHeader
        { ch_PublicKeyCertSize       = B.pack [0x01, 0xA8]
        , ch_CertOwnerConsoleID      = B.pack [0x09, 0x12, 0xBA, 0x26, 0xE3]
        , ch_CertOwnerConsolePartNum = "X803395-001"
        , ch_CertOwnerConsoleType    = Retail
        , ch_CertDateGeneration      = "09-18-06"
        , ch_PublicExponent          = bytesFromInteger 4 $ public_e $ private_pub $ kv_PrivateKey key
        , ch_PublicModulus           = stockScramble $ bytesFromInteger 0x80 $ public_n $ private_pub $ kv_PrivateKey key
        , ch_CertSignature           = B.take 0x100 $ B.drop 0xA60 kv
        , ch_Signature               = B.replicate 0x80 0 -- gets filled in when package is done
        }
      initPackage = STFSPackage
        { stfsHandle   = fd
        , stfsHeader   = CON initHeader
        , stfsMetadata = metadata
        }

  hSeek fd AbsoluteSeek 0
  BL.hPut fd $ runPut $ void $ codecOut bin $ CON initHeader

  let makeFileEntries _ [] = (:[]) $ FileEntry
        { fe_FileName        = ""
        , fe_Consecutive     = False
        , fe_Directory       = False
        , fe_Blocks1         = 0
        , fe_Blocks2         = 0
        , fe_FirstBlock      = 0
        , fe_PathIndex       = 0
        , fe_Size            = 0
        , fe_UpdateTimestamp = timestamp
        , fe_AccessTimestamp = timestamp
        }
      makeFileEntries used ((parent, name, blocks) : rest) = let
        thisBlocks = maybe 0 (fromIntegral . length . snd) blocks
        in FileEntry
          { fe_FileName        = name
          , fe_Consecutive     = False
          , fe_Directory       = isNothing blocks
          , fe_Blocks1         = thisBlocks
          , fe_Blocks2         = thisBlocks
          , fe_FirstBlock      = if isNothing blocks then 0 else used
          , fe_PathIndex       = fromIntegral parent
          , fe_Size            = maybe 0 (fromIntegral . fst) blocks
          , fe_UpdateTimestamp = timestamp
          , fe_AccessTimestamp = timestamp
          } : makeFileEntries (used + thisBlocks) rest
      timestamp = LocalTime
        { localDay = fromGregorian 2019 8 8
        , localTimeOfDay = TimeOfDay 0 0 0
        }
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
      writeFiles n ((_, _, Just (_size, blocks)) : rest) = do
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
      Left err -> error $ show err
      Right signature -> return initHeader
        { ch_Signature = B.reverse signature
        }
  hSeek fd AbsoluteSeek 0
  BL.hPut fd $ runPut $ void $ codecOut bin $ CON finalHeader

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
