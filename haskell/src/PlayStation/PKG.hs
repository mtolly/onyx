{-
PS3 .pkg format

References:
- https://www.psdevwiki.com/ps3/PKG_files
- https://github.com/HACKERCHANNEL/PS3Py
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PlayStation.PKG where

import           Control.Monad                  (forM, guard, replicateM,
                                                 unless, void)
import           Control.Monad.Codec
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace (SendMessage, StackTraceT, lg,
                                                 stackIO, warn)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash                    (Digest, SHA1, hash)
import           Crypto.MAC.CMAC
import           Data.Binary.Codec
import           Data.Binary.Codec.Class
import           Data.Bits
import           Data.ByteArray                 (convert)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Digest.Pure.MD5           as MD5
import qualified Data.List.NonEmpty             as NE
import           Data.Profunctor                (dimap)
import           Data.SimpleHandle
import           NPData                         (NPDecryptConfig (..),
                                                 decryptNPData)
import           Numeric                        (showHex)
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
  , pkgQADigest       :: B.ByteString -- 0x10 bytes, sha1 from debug files and attributes together merged in one block
  , pkgDataRIV        :: B.ByteString -- 0x10 bytes, (in finalized pkgs): aes-128-ctr IV, used with gpkg_aes_key to decrypt data
  } deriving (Show)

instance Bin PKGHeader where
  bin = do
    pkgMagic          <- pkgMagic          =. word32be
    Codec
      { codecIn  = unless (pkgMagic == 0x7F504B47) $ fail "Incorrect PKG magic"
      , codecOut = \_ -> return ()
      }
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
    pkgQADigest       <- pkgQADigest       =. byteString 0x10
    pkgDataRIV        <- pkgDataRIV        =. byteString 0x10
    return PKGHeader{..}

-- Not finished
finalizedDigest :: B.ByteString -> Maybe B.ByteString
finalizedDigest bytes =
  -- gpkg-key from https://github.com/jmesmon/ps3keys/blob/master/gpkg-key
  case cipherInit $ B.pack [0x2e,0x7b,0x71,0xd7,0xc9,0xc9,0xa1,0x4e,0xa3,0x22,0x1f,0x18,0x88,0x28,0xb8,0xf8] of
    CryptoPassed cipher -> Just $ B.concat
      -- digestCMACHash       -- 0x10 bytes, CMAC OMAC hash, PS3 gpkg_key used as key
      [ convert (cmac cipher bytes :: CMAC AES128)
      -- digestNPDRMSignature -- 0x28 bytes, PKG header NPDRM ECDSA (R_sig, S_sig)
      , B.replicate 0x28 0 -- TODO guessing this requires sony private key
      -- digestSHA1Hash       -- 0x8 bytes, last 8 bytes of sha1 hash
      , B.drop 12 $ convert (hash bytes :: Digest SHA1) -- replace with "B.takeEnd 8" when available
      ]
    _ -> Nothing

nonFinalizedDigests :: PKGHeader -> [PKGMetadata] -> (B.ByteString, B.ByteString)
nonFinalizedDigests header meta = let
  trimmedSHA1 b = B.take 16 $ B.drop 3 $ convert (hash b :: Digest SHA1)
  headerSHA = trimmedSHA1 $ BL.toStrict $ runPut $ void $ codecOut bin header
  metaBlockSHA = trimmedSHA1 $ BL.toStrict $ runPut $ mapM_ (codecOut bin) meta
  metaBlockSHAPad = B.replicate 0x30 0
  metaBlockSHAPadEnc = nonFinalizedCrypt False 0 metaBlockSHA metaBlockSHAPad
  metaBlockSHAPadEnc2 = nonFinalizedCrypt False 0 headerSHA metaBlockSHAPadEnc
  digest1 = headerSHA <> metaBlockSHAPadEnc2
  digest2 = metaBlockSHA <> metaBlockSHAPadEnc
  in (digest1, digest2)

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
  , pkgDigests1 :: B.ByteString
  , pkgMetadata :: [PKGMetadata]
  , pkgDigests2 :: B.ByteString
  , pkgInside   :: B.ByteString -- decrypted file table, strings, file contents
  , pkgFolder   :: Folder B.ByteString (Word32, Readable)
  } deriving (Show)

cryptFinalizedPKG :: PKGHeader -> B.ByteString -> Maybe B.ByteString
cryptFinalizedPKG header enc = do
  let makeKey k = case cipherInit k of
        CryptoPassed cipher -> Just (cipher :: AES128)
        _                   -> Nothing
  iv <- makeIV $ pkgDataRIV header
  -- ps3_gpkg_aes_key from https://www.psdevwiki.com/ps3/Keys
  key <- makeKey $ B.pack [0x2E,0x7B,0x71,0xD7,0xC9,0xC9,0xA1,0x4E,0xA3,0x22,0x1F,0x18,0x88,0x28,0xB8,0xF8]
  return $ ctrCombine key iv enc

splitByteChunks :: Int -> B.ByteString -> [B.ByteString]
splitByteChunks len b = if B.null b
  then []
  else case B.splitAt len b of
    (x, y) -> x : splitByteChunks len y

nonFinalizedCrypt :: Bool -> Word64 -> B.ByteString -> B.ByteString -> B.ByteString
nonFinalizedCrypt isArcade startIndex keySource bs = let
  keyPrefix = B.concat
    [ B.take 8 keySource
    , B.take 8 keySource
    , (if isArcade then B.take else B.drop) 8 $ keySource
    , B.drop 8 keySource
    , B.replicate 0x18 $ if isArcade then 0xA0 else 0
    ]
  keyForIndex i = BL.toStrict $ runPut $ putByteString keyPrefix >> putWord64be i
  f chunk key = let
    sha1 = convert (hash key :: Digest SHA1)
    in B.pack $ B.zipWith xor chunk sha1 -- replace with B.packZipWith after snapshot upgrade
  in B.concat $ zipWith f (splitByteChunks 16 bs) $ map keyForIndex $ [startIndex ..] <> [0 ..]

cryptNonFinalizedPKG :: Bool -> PKGHeader -> B.ByteString -> B.ByteString
cryptNonFinalizedPKG isArcade = nonFinalizedCrypt isArcade 0 . pkgQADigest

loadPKG :: FilePath -> IO PKG
loadPKG stfs = withBinaryFile stfs ReadMode $ \fd -> do
  pkgHeader   <- BL.hGet fd 0x80 >>= runGetM (codecIn bin)
  pkgDigests1 <- B.hGet fd 0x40
  let metadataSize = fromIntegral (pkgMetadataSize pkgHeader) - 0x40 -- size minus digests
  pkgMetadata <- BL.hGet fd metadataSize >>= runGetM (replicateM (fromIntegral $ pkgMetadataCount pkgHeader) $ codecIn bin)
  pkgDigests2 <- B.hGet fd 0x40

  enc <- B.hGet fd $ fromIntegral $ pkgDataSize pkgHeader
  isFinalized <- case pkgRevision pkgHeader of
    0x8000 -> return True
    0x0000 -> return False
    n      -> fail $ "Unrecognized finalization value in .pkg: " <> showHex n ""
  let mdec = if isFinalized
        then cryptFinalizedPKG pkgHeader enc
        else Just $ cryptNonFinalizedPKG False pkgHeader enc
  pkgInside <- case mdec of
    Nothing  -> fail "Couldn't decode PKG contents"
    Just dec -> return dec

  let recordBytes = BL.fromStrict $ B.take (0x20 * fromIntegral (pkgItemCount pkgHeader)) pkgInside
  recs <- runGetM (replicateM (fromIntegral $ pkgItemCount pkgHeader) $ codecIn bin) recordBytes
  let notFolder = \(_, (flags, _)) -> flags .&. 0x4 == 0
  pkgFolder <- fmap (fromFiles . filter notFolder) $ forM recs $ \PKGItemRecord{..} -> let
    name = B.take (fromIntegral recFilenameSize) $ B.drop (fromIntegral recFilenameOffset) pkgInside
    name' = if B.take 1 name == "/"
      then B.drop 1 name -- not seen in official dlc but PS3Py starts its paths with slash
      else name
    dat = B.take (fromIntegral recDataSize) $ B.drop (fromIntegral recDataOffset) pkgInside
    readable = makeHandle (B8.unpack name) $ byteStringSimpleHandle $ BL.fromStrict dat
    in case NE.nonEmpty $ B8.split '/' name' of
      Nothing        -> fail "Couldn't split filename in PKG contents"
      Just nameParts -> return (nameParts, (recFlags, readable))

  return PKG{..}

decryptHarmonixEDAT
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> B.ByteString
  -> Readable
  -> StackTraceT m Readable
decryptHarmonixEDAT dir name readable = do
  edat <- stackIO $ useHandle readable handleToByteString
  if BL.take 4 edat == "NPD\0"
    then do
      let klic = MD5.md5DigestBytes $ MD5.md5 $ "Ih38rtW1ng3r" <> BL.fromStrict dir <> "10025250"
          contentID = BL.toStrict $ BL.takeWhile (/= 0) $ BL.take 0x30 $ BL.drop 0x10 edat
          licenseType = BL.take 4 $ BL.drop 8 edat
      rapPath <- stackIO $ getResourcesPath ("raps" </> B8.unpack contentID <.> "rap") >>= fixFileCase
      mrap <- stackIO $ doesFileExist rapPath >>= \case
        False -> return Nothing
        True  -> Just <$> B.readFile rapPath
      case (mrap, BL.unpack licenseType) of
        (Nothing, [0,0,0,2]) -> warn $ unlines
          [ show name <> " requires a missing RAP to decrypt, so it will probably not work."
          , "Please provide: " <> rapPath
          ]
        (Nothing, _        ) -> lg "Free .mid.edat, no RAP required"
        (Just _ , _        ) -> lg $ "Found matching RAP for: " <> B8.unpack contentID
      let cfg = NPDecryptConfig { decKLIC = klic, decRAP = mrap }
      stackIO $ withSystemTempDirectory "onyx-edat-decrypt" $ \tmp -> do
        BL.writeFile (tmp </> "in.edat") edat
        decryptNPData cfg (tmp </> "in.edat") (tmp </> "out.bin") name
        bs <- B.readFile $ tmp </> "out.bin"
        return $ makeHandle (B8.unpack name) $ byteStringSimpleHandle $ BL.fromStrict bs
    else do
      warn $ show name <> " does not appear to be an EDAT; assuming it is unencrypted for RPCS3."
      return readable

decryptHarmonixEDATs
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> Folder B.ByteString Readable
  -> StackTraceT m (Folder B.ByteString Readable)
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
makePKGRecords :: Folder B.ByteString Readable -> IO [(B.ByteString, B.ByteString, Word32)]
makePKGRecords = fmap (go "") . mapM readReadable where
  readReadable r = fmap BL.toStrict $ useHandle r $ handleToByteString
  go path folder = let
    addPath = if B.null path
      then id
      else ((path <> "/") <>)
    this = do
      guard $ not $ B.null path
      return (path, "", 0x80000004)
    files = do
      (name, file) <- folderFiles folder
      -- Check both the name and magic number, in case we are generating unencrypted midis for RPCS3
      let isEDAT = (".edat" `B.isSuffixOf` name) && B.take 4 file == "NPD\0"
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

-- Returns (number of file/folder entries, records and filenames section, data section)
makePKGContents :: Folder B.ByteString Readable -> IO (Int, BL.ByteString, BL.ByteString)
makePKGContents folder = do
  records <- makePKGRecords folder
  let (stringBank, afterStringBank) = makePKGBank
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
  return (length records, recordBytes <> stringBank, dataBank)

makePKG :: B.ByteString -> Folder B.ByteString Readable -> IO BL.ByteString
makePKG contentID folder = do
  (numEntries, pkgEntriesNames, pkgFileContents) <- makePKGContents folder
  fileContents <- map (\(_, fileContents, _) -> fileContents) <$> makePKGRecords folder
  return $ let
    pkgContents = pkgEntriesNames <> pkgFileContents
    fakeHeader = PKGHeader 0 0 0 0 0 0 0 0 0 0 (B.replicate 0x30 0) (B.replicate 0x10 0) (B.replicate 0x10 0)
    metadataOffset = BL.length (runPut $ void $ codecOut bin fakeHeader) + 0x40
    metadataAndDigestsSize = BL.length (runPut $ mapM_ (codecOut bin) metadata) + 0x40
    dataOffset = metadataOffset + metadataAndDigestsSize
    metadata =
      [ PKGMetadata 1 $ B.pack [0,0,0,3] -- drm type: free, no license
      , PKGMetadata 2 $ B.pack [0,0,0,5] -- content type: was 4 for GameData
      , PKGMetadata 3 $ B.pack [0,0,0,0xE] -- package type, was 8, unknown meaning
      , PKGMetadata 4 $ BL.toStrict $ runPut $ putWord64be $ fromIntegral $ BL.length pkgContents
      , PKGMetadata 5 $ B.pack [0x10,0x61,0x00,0x00] -- make_package_npdrm Revision (2 bytes) + Package Version (2 bytes)
      ]
    headerForQADigest = PKGHeader
      { pkgMagic          = 0x7F504B47
      , pkgRevision       = 0x0000 -- making a non-finalized pkg
      , pkgType           = 0x0001
      , pkgMetadataOffset = fromIntegral metadataOffset
      , pkgMetadataCount  = fromIntegral $ length metadata
      , pkgMetadataSize   = fromIntegral metadataAndDigestsSize
      , pkgItemCount      = fromIntegral numEntries
      , pkgTotalSize      = fromIntegral dataOffset + fromIntegral (BL.length pkgContents) + 0x60
      , pkgDataOffset     = fromIntegral dataOffset
      , pkgDataSize       = fromIntegral $ BL.length pkgContents
      , pkgContentID      = "" -- filled in later
      , pkgQADigest       = B.replicate 0x10 0 -- filled in later
      , pkgDataRIV        = B.replicate 0x10 0 -- filled in later
      }
    qaDigest = B.take 0x10 $ convert (hash $ BL.toStrict $ BL.concat
      [ BL.fromChunks fileContents
      , runPut $ void $ codecOut bin headerForQADigest
      , pkgEntriesNames
      ] :: Digest SHA1)
    klicensee = nonFinalizedCrypt False 0xFFFFFFFFFFFFFFFF qaDigest $ B.replicate 0x10 0
    header = headerForQADigest
      { pkgContentID      = contentID -- auto padded with 0
      , pkgQADigest       = qaDigest
      , pkgDataRIV        = B.take 0x10 $ B.take (B.length contentID) klicensee <> B.replicate 0x10 0
      }
    (digest1, digest2) = nonFinalizedDigests header metadata
    encrypted = cryptNonFinalizedPKG False header $ BL.toStrict pkgContents
    in runPut $ do
      void $ codecOut bin header
      putByteString digest1
      mapM_ (codecOut bin) metadata
      putByteString digest2
      putByteString encrypted
      putByteString $ B.replicate 0x60 0 -- this is needed to install ok, but why?
