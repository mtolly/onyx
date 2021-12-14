{-
PS3 .pkg format

References:
- https://www.psdevwiki.com/ps3/PKG_files
-}
{-# LANGUAGE RecordWildCards #-}
module PlayStation.PKG where

import           Control.Monad           (forM, replicateM)
import           Control.Monad.Codec
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Binary.Codec
import           Data.Binary.Codec.Class
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as BL
import qualified Data.List.NonEmpty      as NE
import           Data.Profunctor         (dimap)
import           Data.SimpleHandle
import           STFS.Package            (runGetM)
import           System.IO               (IOMode (..), withBinaryFile)

nullPadded :: Int -> BinaryCodec B.ByteString
nullPadded n = dimap
  (\s -> B.take n s <> B.replicate n 0)
  (B.takeWhile (/= 0))
  $ byteString n

data PKGHeader = PKGHeader
  { pkgMagic          :: Word32 -- ".PKG"
  , pkgRevision       :: Word16 -- 80 00 for finalized (retail), 00 00 for non finalized (debug)
  , pkgType           :: Word16 -- 00 01 for PS3, 00 02 for PSP and PSVita
  , pkgMetadataOffset :: Word32 -- usually 0xC0 for PS3, usually 0x280 for PSP and PSVita
  , pkgMetadataCount  :: Word32
  , pkgMetadataSize   :: Word32 -- usually 0xC0 for PSP and PS3, usually 0x160 for PSVita
  , pkgItemCount      :: Word32 -- files and folders into the encrypted data
  , pkgTotalSize      :: Word64 -- total pkg file size
  , pkgDataOffset     :: Word64 -- encrypted data offset
  , pkgDataSize       :: Word64 -- encrypted data size
  , pkgContentID      :: B.ByteString -- 0x30 bytes, ID followed by zeroes
  , pkgDigest         :: B.ByteString -- 0x10 bytes, sha1 from debug files and attributes together merged in one block
  , pkgDataRIV        :: B.ByteString -- 0x10 bytes, aes-128-ctr IV, used with gpkg_aes_key to decrypt data
  , pkgHeaderDigests  :: PKGDigests -- 0x40 bytes:
    -- 0x10 bytes, CMAC OMAC hash from 0x00-0x7F, PS3 gpkg_key used as key
    -- 0x28 bytes, PKG header NPDRM ECDSA (R_sig, S_sig)
    -- 0x8 bytes, last 8 bytes of sha1 hash of 0x00-0x7F
  } deriving (Show)

instance Bin PKGHeader where
  bin = do
    pkgMagic          <- pkgMagic          =. word32be
    pkgRevision       <- pkgRevision       =. word16be
    pkgType           <- pkgType           =. word16be
    pkgMetadataOffset <- pkgMetadataOffset =. word32be
    pkgMetadataCount  <- pkgMetadataCount  =. word32be
    pkgMetadataSize   <- pkgMetadataSize   =. word32be
    pkgItemCount      <- pkgItemCount      =. word32be
    pkgTotalSize      <- pkgTotalSize      =. word64be
    pkgDataOffset     <- pkgDataOffset     =. word64be
    pkgDataSize       <- pkgDataSize       =. word64be
    pkgContentID      <- pkgContentID      =. nullPadded 0x30
    pkgDigest         <- pkgDigest         =. byteString 0x10
    pkgDataRIV        <- pkgDataRIV        =. byteString 0x10
    pkgHeaderDigests  <- pkgHeaderDigests  =. bin
    return PKGHeader{..}

data PKGMetadata = PKGMetadata
  { metaID    :: Word32
  , metaBytes :: B.ByteString
  } deriving (Show)

instance Bin PKGMetadata where
  bin = Codec
    { codecIn = do
      metaID <- getWord32be
      len <- getWord32be
      metaBytes <- getByteString $ fromIntegral len
      return PKGMetadata{..}
    , codecOut = fmapArg $ \PKGMetadata{..} -> do
      putWord32be metaID
      putWord32be $ fromIntegral $ B.length metaBytes
      putByteString metaBytes
    }

data PKGDigests = PKGDigests
  { digestCMACHash       :: B.ByteString -- 0x10 bytes
  , digestNPDRMSignature :: B.ByteString -- 0x28 bytes
  , digestSHA1Hash       :: B.ByteString -- 0x8 bytes
  } deriving (Show)

instance Bin PKGDigests where
  bin = do
    digestCMACHash       <- digestCMACHash       =. byteString 0x10
    digestNPDRMSignature <- digestNPDRMSignature =. byteString 0x28
    digestSHA1Hash       <- digestSHA1Hash       =. byteString 0x8
    return PKGDigests{..}

data PKGItemRecord = PKGItemRecord
  { recFilenameOffset :: Word32
  , recFilenameSize   :: Word32
  , recDataOffset     :: Word64
  , recDataSize       :: Word64
  , recFlags          :: Word32
  , recPadding        :: Word32
  } deriving (Show)

instance Bin PKGItemRecord where
  bin = do
    recFilenameOffset <- recFilenameOffset =. word32be
    recFilenameSize   <- recFilenameSize   =. word32be
    recDataOffset     <- recDataOffset     =. word64be
    recDataSize       <- recDataSize       =. word64be
    recFlags          <- recFlags          =. word32be
    -- flags seen:
    -- 0x80000002 .edat file
    -- 0x80000003 unencrypted file
    -- 0x80000004 folder (data size is 0)
    recPadding        <- recPadding        =. word32be
    return PKGItemRecord{..}

data PKG = PKG
  { pkgHeader   :: PKGHeader
  , pkgMetadata :: [PKGMetadata]
  , pkgDigests  :: PKGDigests
  , pkgFolder   :: Folder B.ByteString (Word32, B.ByteString)
  } deriving (Show)

withPKG :: FilePath -> (PKG -> IO a) -> IO a
withPKG stfs fn = withBinaryFile stfs ReadMode $ \fd -> do
  headerBytes  <- BL.hGet fd 0xC0
  pkgHeader    <- runGetM (codecIn bin) headerBytes
  metaBytes    <- BL.hGet fd $ fromIntegral (pkgMetadataSize pkgHeader) - 0x40
  pkgMetadata  <- runGetM (replicateM (fromIntegral $ pkgMetadataCount pkgHeader) $ codecIn bin) metaBytes
  digestsBytes <- BL.hGet fd 0x40
  pkgDigests   <- runGetM (codecIn bin) digestsBytes

  enc <- B.hGet fd $ fromIntegral $ pkgDataSize pkgHeader
  let mdec = do
        let makeKey k = case cipherInit k of
              CryptoPassed cipher -> Just (cipher :: AES128)
              _                   -> Nothing
        iv <- makeIV $ pkgDataRIV pkgHeader
        -- ps3_gpkg_aes_key from https://www.psdevwiki.com/ps3/Keys
        key <- makeKey $ B.pack [0x2E,0x7B,0x71,0xD7,0xC9,0xC9,0xA1,0x4E,0xA3,0x22,0x1F,0x18,0x88,0x28,0xB8,0xF8]
        return $ ctrCombine key iv enc
  dec <- case mdec of
    Nothing  -> fail "Couldn't decode PKG contents"
    Just dec -> return dec

  let recordBytes = BL.fromStrict $ B.take (0x20 * fromIntegral (pkgItemCount pkgHeader)) dec
  recs <- runGetM (replicateM (fromIntegral $ pkgItemCount pkgHeader) $ codecIn bin) recordBytes
  let notFolder = \(_, (flags, _)) -> flags .&. 0x4 == 0
  pkgFolder <- fmap (fromFiles . filter notFolder) $ forM recs $ \PKGItemRecord{..} -> let
    name = B.take (fromIntegral recFilenameSize) $ B.drop (fromIntegral recFilenameOffset) dec
    dat = B.take (fromIntegral recDataSize) $ B.drop (fromIntegral recDataOffset) dec
    in case NE.nonEmpty $ B8.split '/' name of
      Nothing        -> fail "Couldn't split filename in PKG contents"
      Just nameParts -> return (nameParts, (recFlags, dat))

  fn PKG{..}
