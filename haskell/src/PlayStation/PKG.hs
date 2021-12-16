{-
PS3 .pkg format

References:
- https://www.psdevwiki.com/ps3/PKG_files
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PlayStation.PKG where

import           Control.Monad                  (forM, guard, replicateM, void)
import           Control.Monad.Codec
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace (SendMessage, StackTraceT, lg,
                                                 stackIO, warn)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Binary.Codec
import           Data.Binary.Codec.Class
import           Data.Bits
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Digest.Pure.MD5           as MD5
import qualified Data.List.NonEmpty             as NE
import           Data.Profunctor                (dimap)
import           Data.SimpleHandle
import           NPData                         (NPDecryptConfig (..),
                                                 decryptNPData)
import           OSFiles                        (fixFileCase)
import           Resources                      (getResourcesPath)
import           STFS.Package                   (runGetM)
import           System.Directory               (doesFileExist)
import           System.FilePath                ((<.>), (</>))
import           System.IO                      (IOMode (..), withBinaryFile)
import           System.IO.Temp                 (withSystemTempDirectory)

nullPadded :: Int -> BinaryCodec B.ByteString
nullPadded n = dimap
  (\s -> B.take n $ s <> B.replicate n 0)
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

decryptHarmonixEDAT
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> B.ByteString
  -> B.ByteString
  -> StackTraceT m B.ByteString
decryptHarmonixEDAT dir name edat = do
  let klic = MD5.md5DigestBytes $ MD5.md5 $ "Ih38rtW1ng3r" <> BL.fromStrict dir <> "10025250"
      contentID = B.takeWhile (/= 0) $ B.take 0x30 $ B.drop 0x10 edat
      licenseType = B.take 4 $ B.drop 8 edat
  rapPath <- stackIO $ getResourcesPath ("raps" </> B8.unpack contentID <.> "rap") >>= fixFileCase
  mrap <- stackIO $ doesFileExist rapPath >>= \case
    False -> return Nothing
    True  -> Just <$> B.readFile rapPath
  case (mrap, B.unpack licenseType) of
    (Nothing, [0,0,0,2]) -> warn $ unlines
      [ "This .mid.edat requires a missing RAP to decrypt, so it will probably not work."
      , "Please provide: " <> rapPath
      ]
    (Nothing, _        ) -> lg "Free .mid.edat, no RAP required"
    (Just _ , _        ) -> lg $ "Found matching RAP for: " <> B8.unpack contentID
  let cfg = NPDecryptConfig { decKLIC = klic, decRAP = mrap }
  stackIO $ withSystemTempDirectory "onyx-edat-decrypt" $ \tmp -> do
    B.writeFile (tmp </> "in.edat") edat
    decryptNPData cfg (tmp </> "in.edat") (tmp </> "out.bin") name
    B.readFile $ tmp </> "out.bin"

decryptHarmonixEDATs
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> Folder B.ByteString B.ByteString
  -> StackTraceT m (Folder B.ByteString B.ByteString)
decryptHarmonixEDATs dir folder = do
  subs <- forM (folderSubfolders folder) $ \(name, sub) -> do
    sub' <- decryptHarmonixEDATs dir sub
    return (name, sub')
  files <- forM (folderFiles folder) $ \pair@(name, file) -> case B.stripSuffix ".edat" name of
    Just decName -> do
      dec <- decryptHarmonixEDAT dir name file
      return (decName, dec)
    Nothing -> return pair
  return Folder
    { folderSubfolders = subs
    , folderFiles = files
    }

-- Each output is (full name, data, flags)
makePKGRecords :: Folder B.ByteString B.ByteString -> [(B.ByteString, B.ByteString, Word32)]
makePKGRecords = go "" where
  go path folder = let
    addPath = if B.null path
      then id
      else ((path <> "/") <>)
    this = do
      guard $ not $ B.null path
      return (path, "", 0x80000004)
    files = do
      (name, file) <- folderFiles folder
      let isEDAT = ".edat" `B.isSuffixOf` name
          flags = if isEDAT then 0x80000002 else 0x80000003
      return (addPath name, file, flags)
    subs = do
      (name, sub) <- folderSubfolders folder
      go (addPath name) sub
    in this <> files <> subs

-- Turns each input into (input a, offset, length)
makePKGBank :: [(B.ByteString, a)] -> (BL.ByteString, [(a, Int64, Int)])
makePKGBank = go "" [] where
  go bank entries [] = (bank, reverse entries)
  go bank entries ((new, x) : rest) = let
    padding = case B.length new `rem` 0x10 of
      0 -> BL.empty
      n -> BL.replicate (0x10 - fromIntegral n) 0
    bank' = bank <> BL.fromStrict new <> padding
    entry = (x, BL.length bank, B.length new)
    in go bank' (entry : entries) rest

-- Returns (number of file/folder entries, data ready for encrypt)
makePKGContents :: Folder B.ByteString B.ByteString -> (Int, BL.ByteString)
makePKGContents folder = let
  records = makePKGRecords folder
  (stringBank, afterStringBank) = makePKGBank
    [ (path, (dat, flags)) | (path, dat, flags) <- records ]
  (dataBank, afterDataBank) = makePKGBank
    [ (dat, (flags, pathPos, pathLen)) | ((dat, flags), pathPos, pathLen) <- afterStringBank ]
  recordsSize = 0x20 * length records
  finalRecords = do
    ((flags, pathPos, pathLen), dataPos, dataLen) <- afterDataBank
    return PKGItemRecord
      { recFilenameOffset = fromIntegral recordsSize + fromIntegral pathPos
      , recFilenameSize   = fromIntegral pathLen
      , recDataOffset     = fromIntegral recordsSize + fromIntegral (BL.length stringBank) + fromIntegral dataPos
      , recDataSize       = fromIntegral dataLen
      , recFlags          = flags
      , recPadding        = 0
      }
  recordBytes = runPut $ mapM_ (codecOut bin) finalRecords
  in (length records, recordBytes <> stringBank <> dataBank)

makePKG :: B.ByteString -> Folder B.ByteString B.ByteString -> Maybe BL.ByteString
makePKG contentID folder = let
  (numEntries, pkgContents) = makePKGContents folder
  makeKey k = case cipherInit k of
    CryptoPassed cipher -> Just (cipher :: AES128)
    _                   -> Nothing
  ivBytes = B.pack $ take 16 $ cycle [0x09, 0x16]
  maybeCrypt = do
    key <- makeKey $ B.pack [0x2E,0x7B,0x71,0xD7,0xC9,0xC9,0xA1,0x4E,0xA3,0x22,0x1F,0x18,0x88,0x28,0xB8,0xF8]
    iv <- makeIV ivBytes
    return (key, iv)
  in flip fmap maybeCrypt $ \(key, iv) -> let
    encrypted = ctrCombine key iv $ BL.toStrict pkgContents
    fakeHeader = PKGHeader 0 0 0 0 0 0 0 0 0 0 (B.replicate 0x30 0) (B.replicate 0x10 0) (B.replicate 0x10 0) fakeDigests
    fakeDigests = PKGDigests
      { digestCMACHash       = B.replicate 0x10 0
      , digestNPDRMSignature = B.replicate 0x28 0
      , digestSHA1Hash       = B.replicate 0x8  0
      }
    metadataOffset = BL.length $ runPut $ do
      void $ codecOut bin fakeHeader
    metadataAndDigestsSize = BL.length $ runPut $ do
      mapM_ (codecOut bin) metadata
      void $ codecOut bin digests
    dataOffset = metadataOffset + metadataAndDigestsSize
    header = PKGHeader
      { pkgMagic          = 0x7F504B47
      , pkgRevision       = 0x8000
      , pkgType           = 0x0001
      , pkgMetadataOffset = fromIntegral metadataOffset
      , pkgMetadataCount  = fromIntegral $ length metadata
      , pkgMetadataSize   = fromIntegral metadataAndDigestsSize
      , pkgItemCount      = fromIntegral numEntries
      , pkgTotalSize      = fromIntegral dataOffset + fromIntegral (BL.length pkgContents)
      , pkgDataOffset     = fromIntegral dataOffset
      , pkgDataSize       = fromIntegral $ BL.length pkgContents
      , pkgContentID      = contentID -- auto padded with 0
      , pkgDigest         = B.replicate 0x10 0 -- TODO
      , pkgDataRIV        = ivBytes
      , pkgHeaderDigests  = fakeDigests -- TODO
      }
    metadata =
      [ PKGMetadata 1 $ B.pack [0,0,0,3] -- drm type: free, no license
      , PKGMetadata 2 $ B.pack [0,0,0,4] -- content type: GameData
      , PKGMetadata 3 $ B.pack [0,0,0,8] -- package type, unknown meaning
      , PKGMetadata 4 $ BL.toStrict $ runPut $ putWord64be $ fromIntegral $ BL.length pkgContents
      , PKGMetadata 5 $ B.pack [0x10,0x61,0x00,0x00] -- make_package_npdrm Revision (2 bytes) + Package Version (2 bytes)
      ]
    digests = fakeDigests -- TODO
    in runPut $ do
      void $ codecOut bin header
      mapM_ (codecOut bin) metadata
      void $ codecOut bin digests
      putByteString encrypted
