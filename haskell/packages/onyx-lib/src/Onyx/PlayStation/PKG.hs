{-
PS3 .pkg format

References:
- https://www.psdevwiki.com/ps3/PKG_files
- https://github.com/HACKERCHANNEL/PS3Py
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.PlayStation.PKG (PKGHeader(..), PKGMetadata(..), PKG(..), loadPKG, makePKG, tryDecryptEDAT, tryDecryptEDATs, tryDecryptEDATsInFolder, getDecryptedUSRDIR) where

import           Control.Applicative     ((<|>))
import           Control.Monad           (forM, forM_, guard, replicateM,
                                          unless, void, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class  (MonadIO)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash             (Context, Digest, SHA1, hash, hashInit,
                                          hashUpdates)
import qualified Crypto.Hash.SHA1        as SHA1
import           Crypto.MAC.CMAC
import           Data.Binary.Codec
import           Data.Bits
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8   as B8
import qualified Data.ByteString.Lazy    as BL
import           Data.List               (inits)
import qualified Data.List.NonEmpty      as NE
import           Data.Maybe              (isJust)
import           Data.Profunctor         (dimap)
import           Numeric                 (showHex)
import           Onyx.Codec.Binary
import           Onyx.PlayStation.NPData (NPDecryptConfig (..), decryptNPData,
                                          ghworKLIC, rockBandKLIC, rs2014KLIC)
import           Onyx.Resources          (getResourcesPath)
import           Onyx.StackTrace         (SendMessage, StackTraceT,
                                          errorToWarning, lg, stackIO, warn)
import           Onyx.Util.Files         (fixFileCase)
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS          (runGetM)
import           System.Directory        (doesFileExist)
import           System.FilePath         ((<.>), (</>))
import           System.IO               (Handle, IOMode (..), SeekMode (..),
                                          hFileSize, hSeek, withBinaryFile)
import           System.IO.Temp          (withSystemTempDirectory)

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
_finalizedDigest :: B.ByteString -> Maybe B.ByteString
_finalizedDigest bytes =
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
  metaBlockSHAPad = BL.replicate 0x30 0
  metaBlockSHAPadEnc = nonFinalizedCrypt False 0 metaBlockSHA metaBlockSHAPad
  metaBlockSHAPadEnc2 = nonFinalizedCrypt False 0 headerSHA metaBlockSHAPadEnc
  digest1 = headerSHA <> BL.toStrict metaBlockSHAPadEnc2
  digest2 = metaBlockSHA <> BL.toStrict metaBlockSHAPadEnc
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
  , pkgFolder   :: Folder B.ByteString Readable
  }

cryptFinalizedPKG :: PKGHeader -> Maybe (Word64 -> B.ByteString -> B.ByteString)
cryptFinalizedPKG header = do
  let makeKey k = case cipherInit k of
        CryptoPassed cipher -> Just (cipher :: AES128)
        _                   -> Nothing
  iv <- makeIV $ pkgDataRIV header
  -- ps3_gpkg_aes_key from https://www.psdevwiki.com/ps3/Keys
  key <- makeKey $ B.pack [0x2E,0x7B,0x71,0xD7,0xC9,0xC9,0xA1,0x4E,0xA3,0x22,0x1F,0x18,0x88,0x28,0xB8,0xF8]
  return $ \startIndex enc -> ctrCombine key (ivAdd iv $ fromIntegral startIndex) enc

nonFinalizedCrypt :: Bool -> Word64 -> B.ByteString -> BL.ByteString -> BL.ByteString
nonFinalizedCrypt isArcade startIndex keySource bs = let
  shaInit = SHA1.start $ B.concat
    [ B.take 8 keySource
    , B.take 8 keySource
    , (if isArcade then B.take else B.drop) 8 $ keySource
    , B.drop 8 keySource
    , B.replicate 0x18 $ if isArcade then 0xA0 else 0
    ]
  indexStream = [startIndex ..] <> [0 ..]
  keyStream = BB.toLazyByteString $ mconcat $ flip map indexStream $ \i -> let
    key = runPut $ putWord64be i
    sha1 = SHA1.finalize $ SHA1.update shaInit $ BL.toStrict key
    in BB.byteString $ B.take 16 sha1
  in BL.packZipWith xor bs keyStream

cryptNonFinalizedPKG :: Bool -> Word64 -> PKGHeader -> BL.ByteString -> BL.ByteString
cryptNonFinalizedPKG isArcade startIndex = nonFinalizedCrypt isArcade startIndex . pkgQADigest

cryptNonFinalizedPKG' :: Bool -> Word64 -> PKGHeader -> [(Int, Readable)] -> [Readable]
cryptNonFinalizedPKG' isArcade startIndex header pieces = do
  (before, (_, this)) <- zip (inits pieces) pieces
  return $ case sum (map fst before) `quotRem` 0x10 of
    (hexes, 0) -> let
      newIndex = startIndex + fromIntegral hexes
      withBytes = nonFinalizedCrypt isArcade newIndex $ pkgQADigest header
      in transformBytes withBytes this
    _ -> error "Contents start at a position not divisble by 0x10"

readPKGName :: B.ByteString -> IO (NE.NonEmpty B.ByteString)
readPKGName name = let
  name' = if B.take 1 name == "/"
    then B.drop 1 name -- not seen in official dlc but PS3Py starts its paths with slash
    else name
  in case NE.nonEmpty $ B8.split '/' name' of
    Nothing        -> fail "Couldn't split filename in PKG contents"
    Just nameParts -> return nameParts

loadFinalizedPKGFolder :: FilePath -> Handle -> PKGHeader -> IO (Folder B.ByteString Readable)
loadFinalizedPKGFolder f h header = do
  decoder <- maybe (fail "Couldn't decode PKG contents") return $ cryptFinalizedPKG header
  let decryptPart aHandle posn len = case quotRem posn 0x10 of
        (hexes, 0) -> do
          hSeek aHandle AbsoluteSeek $ fromIntegral $ posn + pkgDataOffset header
          enc <- B.hGet aHandle len
          return $ BL.fromStrict $ decoder hexes enc
        _ -> fail "Expected a read position divisible by 0x10"
  recs <- decryptPart h 0 (0x20 * fromIntegral (pkgItemCount header))
    >>= runGetM (replicateM (fromIntegral $ pkgItemCount header) $ codecIn bin)
  let notFolder = \(_, (flags, _)) -> flags .&. 0x4 == 0
  fmap (fmap snd . fromFiles . filter notFolder) $ forM recs $ \PKGItemRecord{..} -> do
    name <- BL.toStrict <$> decryptPart h (fromIntegral recFilenameOffset) (fromIntegral recFilenameSize)
    nameParts <- readPKGName name
    let readable = makeHandle (B8.unpack name) $ do
          bs <- withBinaryFile f ReadMode $ \h' -> do
            decryptPart h' (fromIntegral recDataOffset) (fromIntegral recDataSize)
          byteStringSimpleHandle bs
    return (nameParts, (recFlags, readable))

loadNonFinalizedPKGFolder :: FilePath -> Handle -> PKGHeader -> IO (Folder B.ByteString Readable)
loadNonFinalizedPKGFolder f h header = do
  let decryptPart aHandle posn len = case quotRem posn 0x10 of
        (hexes, 0) -> do
          hSeek aHandle AbsoluteSeek $ fromIntegral $ posn + pkgDataOffset header
          enc <- BL.hGet aHandle len
          return $ cryptNonFinalizedPKG False hexes header enc
        _ -> fail "Expected a read position divisible by 0x10"
  recs <- decryptPart h 0 (0x20 * fromIntegral (pkgItemCount header))
    >>= runGetM (replicateM (fromIntegral $ pkgItemCount header) $ codecIn bin)
  let notFolder = \(_, (flags, _)) -> flags .&. 0x4 == 0
  fmap (fmap snd . fromFiles . filter notFolder) $ forM recs $ \PKGItemRecord{..} -> do
    name <- BL.toStrict <$> decryptPart h (fromIntegral recFilenameOffset) (fromIntegral recFilenameSize)
    nameParts <- readPKGName name
    let readable = makeHandle (B8.unpack name) $ do
          bs <- withBinaryFile f ReadMode $ \h' -> do
            decryptPart h' (fromIntegral recDataOffset) (fromIntegral recDataSize)
          byteStringSimpleHandle bs
    return (nameParts, (recFlags, readable))

loadPKG :: FilePath -> IO PKG
loadPKG f = withBinaryFile f ReadMode $ \h -> do
  pkgHeader   <- BL.hGet h 0x80 >>= runGetM (codecIn bin)
  pkgDigests1 <- B.hGet h 0x40
  let metadataSize = fromIntegral (pkgMetadataSize pkgHeader) - 0x40 -- size minus digests
  pkgMetadata <- BL.hGet h metadataSize >>= runGetM (replicateM (fromIntegral $ pkgMetadataCount pkgHeader) $ codecIn bin)
  pkgDigests2 <- B.hGet h 0x40

  isFinalized <- case pkgRevision pkgHeader of
    0x8000 -> return True
    0x0000 -> return False
    n      -> fail $ "Unrecognized finalization value in .pkg: " <> showHex n ""
  pkgFolder <- if isFinalized
    then loadFinalizedPKGFolder    f h pkgHeader
    else loadNonFinalizedPKGFolder f h pkgHeader

  return PKG{..}

-- Games that use the folder-MD5 KLIC generation (NPData.rockBandKLIC)
rockBandIDs :: [B.ByteString]
rockBandIDs =
  [ "BLUS30050", "BLES00228" -- RB1
  , "BLUS30463", "BLES00986" -- RB3
  , "BLUS30282", "BLES00532" -- TBRB
  -- Haven't seen any .pkg actually using the rest of these
  , "BLUS30147", "BLES00385" -- RB2
  , "BLUS30350", "BLES00787" -- GDRB
  , "BLUS30351" -- Country Track Pack
  , "BLUS30623" -- Country Track Pack 2
  , "BLUS30352" -- Metal Track Pack
  , "BLUS30327" -- Classic Rock Track Pack
  ]

-- Games that use the common Neversoft KLIC
neversoftIDs :: [B.ByteString]
neversoftIDs =
  [ "BLUS30487", "BLES00801" -- GHWoR
  , "BLUS30292", "BLES00576" -- GH5
  , "BLUS30164", "BLES00299" -- GHWT
  , "BLUS30074", "BLES00134" -- GH3
  ]

tryDecryptEDAT
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> B.ByteString
  -> Readable
  -> StackTraceT m (Maybe Readable)
tryDecryptEDAT dir name readable = do
  edat <- stackIO $ useHandle readable handleToByteString
  if BL.take 4 edat /= "NPD\0"
    then do
      warn $ show name <> " does not appear to be an EDAT; assuming it is unencrypted for RPCS3."
      return $ Just readable
    else do
      let contentID = BL.toStrict $ BL.takeWhile (/= 0) $ BL.take 0x30 $ BL.drop 0x10 edat
          licenseType = BL.take 4 $ BL.drop 8 edat
          gameID = B.take 9 $ B.drop 7 contentID
      mklic <- case gameID of
        "BLUS30670" -> do
          lg "Decrypting Rocksmith 2014 EDAT file"
          return $ Just rs2014KLIC
        _ | elem gameID neversoftIDs -> do
            lg "Decrypting Neversoft GH EDAT file"
            return $ Just ghworKLIC
          | elem gameID rockBandIDs -> do
            lg "Decrypting Rock Band EDAT file"
            return $ Just $ rockBandKLIC dir
          | otherwise -> do
            warn $ "Unknown KLIC for game ID " <> show gameID
            return Nothing
      case mklic of
        Nothing -> return Nothing
        Just klic -> do
          rapPath <- stackIO $ getResourcesPath ("raps" </> B8.unpack contentID <.> "rap") >>= fixFileCase
          mrap <- stackIO $ doesFileExist rapPath >>= \case
            False -> return Nothing
            True  -> Just <$> B.readFile rapPath
          case (mrap, BL.unpack licenseType) of
            (Nothing, [0,0,0,2]) -> do
              warn $ unlines
                [ show name <> " requires a missing RAP to decrypt, so it will probably not work."
                , "Please provide: " <> rapPath
                ]
              return Nothing
            _ -> do
              when (isJust mrap) $ lg $ "Found matching RAP for: " <> B8.unpack contentID
              let cfg = NPDecryptConfig { decKLIC = klic, decRAP = mrap }
              errorToWarning $ stackIO $ withSystemTempDirectory "onyx-edat-decrypt" $ \tmp -> do
                BL.writeFile (tmp </> "in.edat") edat
                decryptNPData cfg (tmp </> "in.edat") (tmp </> "out.bin") name
                bs <- B.readFile $ tmp </> "out.bin"
                return $ makeHandle (B8.unpack name) $ byteStringSimpleHandle $ BL.fromStrict bs

tryDecryptEDATsInFolder
  :: (MonadIO m, SendMessage m)
  => B.ByteString
  -> Folder B.ByteString Readable
  -> StackTraceT m (Folder B.ByteString Readable)
tryDecryptEDATsInFolder dir folder = do
  subs <- forM (folderSubfolders folder) $ \(name, sub) -> do
    sub' <- tryDecryptEDATsInFolder dir sub
    return (name, sub')
  files <- forM (folderFiles folder) $ \pair@(name, file) -> case B.stripSuffix ".edat" name <|> B.stripSuffix ".EDAT" name of
    Just decName -> tryDecryptEDAT dir name file >>= \case
      Nothing  -> do
        warn $ "Not able to decrypt: " <> B8.unpack name
        return pair
      Just dec -> return (decName, dec)
    Nothing -> return pair
  return Folder
    { folderSubfolders = subs
    , folderFiles = files
    }

tryDecryptEDATs
  :: (SendMessage m, MonadIO m)
  => Folder B.ByteString Readable
  -> StackTraceT m (Folder B.ByteString Readable)
tryDecryptEDATs folder = case findFolder ["USRDIR"] folder of
  Nothing -> do
    warn "Couldn't find USRDIR in .pkg"
    return folder
  Just usr -> do
    subs <- forM (folderSubfolders usr) $ \(subName, sub) -> do
      sub' <- tryDecryptEDATsInFolder subName sub
      return (subName, sub')
    return folder
      { folderSubfolders = ("USRDIR", usr { folderSubfolders = subs })
        : filter (\(name, _) -> name /= "USRDIR") (folderSubfolders folder)
      }

getDecryptedUSRDIR
  :: (SendMessage m, MonadIO m)
  => Folder B.ByteString Readable
  -> StackTraceT m [(B.ByteString, Folder B.ByteString Readable)]
getDecryptedUSRDIR folder = case findFolder ["USRDIR"] folder of
  Nothing -> do
    warn "Couldn't find USRDIR"
    return []
  Just usr -> forM (folderSubfolders usr) $ \(name, sub) -> do
    sub' <- tryDecryptEDATsInFolder name sub
    return (name, sub')

-- Each output is (full name, (size, data), flags)
makePKGRecords :: Folder B.ByteString Readable -> IO [(B.ByteString, (Int, Readable), Word32)]
makePKGRecords = fmap (go "") . mapM annotate where
  annotate r = useHandle r $ \h -> do
    size <- fromIntegral <$> hFileSize h
    magic <- B.hGet h 4
    return (size, magic, r)
  go path folder = let
    addPath = if B.null path
      then id
      else ((path <> "/") <>)
    this = do
      guard $ not $ B.null path
      return (path, (0, makeHandle "" $ byteStringSimpleHandle ""), 0x80000004)
    files = do
      (name, (size, magic, r)) <- folderFiles folder
      -- Check both the name and magic number, in case we are generating unencrypted midis for RPCS3
      let isEDAT = ((".edat" `B.isSuffixOf` name) || (".EDAT" `B.isSuffixOf` name))
            && magic == "NPD\0"
          flags = if isEDAT then 0x80000002 else 0x80000003
      return (addPath name, (size, r), flags)
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

makePKGBank' :: [((Int, Readable), a)] -> ([(Int, Readable)], [(a, Int, Int)])
makePKGBank' = go [] [] where
  go bank entries [] = (reverse bank, reverse entries)
  go bank entries (((newSize, newData), x) : rest) = let
    newPair = case newSize `rem` 0x10 of
      0 -> (newSize, newData)
      n -> let
        paddingSize = 0x10 - fromIntegral n
        padding     = BL.replicate (fromIntegral paddingSize) 0
        in (newSize + paddingSize, transformBytes (<> padding) newData)
    bank' = newPair : bank
    entry = (x, sum $ map fst bank, newSize)
    in go bank' (entry : entries) rest

-- Returns (number of file/folder entries, records and filenames section, data section)
makePKGContents :: [(B.ByteString, (Int, Readable), Word32)] -> (Int, BL.ByteString, [(Int, Readable)])
makePKGContents records = let
  (stringBank, afterStringBank) = makePKGBank
    [ (path, (dat, flags)) | (path, dat, flags) <- records ]
  (dataBank, afterDataBank) = makePKGBank'
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
  in (length records, recordBytes <> stringBank, dataBank)

makePKG :: B.ByteString -> Folder B.ByteString Readable -> FilePath -> IO ()
makePKG contentID folder fout = do
  records <- makePKGRecords folder
  let goHash :: Context SHA1 -> [(Int, Readable)] -> IO (Context SHA1)
      goHash !ctx = \case
        [] -> return ctx
        (_, r) : rest -> do
          bs <- useHandle r handleToByteString
          goHash (hashUpdates ctx $ BL.toChunks bs) rest
  fileContentsHash <- goHash hashInit $ map (\(_, fileContents, _) -> fileContents) records
  let (numEntries, pkgEntriesNames, pkgFileContents) = makePKGContents records
      pkgContentsLength = fromIntegral (BL.length pkgEntriesNames) + fromIntegral (sum $ map fst pkgFileContents) :: Word64
      pkgContents
        = (fromIntegral $ BL.length pkgEntriesNames, makeHandle "" $ byteStringSimpleHandle pkgEntriesNames)
        : pkgFileContents
      fakeHeader = PKGHeader 0 0 0 0 0 0 0 0 0 0 (B.replicate 0x30 0) (B.replicate 0x10 0) (B.replicate 0x10 0)
      metadataOffset = BL.length (runPut $ void $ codecOut bin fakeHeader) + 0x40
      metadataAndDigestsSize = BL.length (runPut $ mapM_ (codecOut bin) metadata) + 0x40
      dataOffset = metadataOffset + metadataAndDigestsSize
      metadata =
        [ PKGMetadata 1 $ B.pack [0,0,0,3] -- drm type: free, no license
        , PKGMetadata 2 $ B.pack [0,0,0,5] -- content type: was 4 for GameData
        , PKGMetadata 3 $ B.pack [0,0,0,0xE] -- package type, was 8, unknown meaning
        , PKGMetadata 4 $ BL.toStrict $ runPut $ putWord64be pkgContentsLength
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
        , pkgTotalSize      = fromIntegral dataOffset + pkgContentsLength + 0x60
        , pkgDataOffset     = fromIntegral dataOffset
        , pkgDataSize       = pkgContentsLength
        , pkgContentID      = "" -- filled in later
        , pkgQADigest       = B.replicate 0x10 0 -- filled in later
        , pkgDataRIV        = B.replicate 0x10 0 -- filled in later
        }
      qaDigest = B.take 0x10 $ convert $ hashUpdates fileContentsHash
        [ BL.toStrict $ runPut $ void $ codecOut bin headerForQADigest
        , BL.toStrict pkgEntriesNames
        ]
      klicensee = BL.toStrict $ nonFinalizedCrypt False 0xFFFFFFFFFFFFFFFF qaDigest $ BL.replicate 0x10 0
      header = headerForQADigest
        { pkgContentID      = contentID -- auto padded with 0
        , pkgQADigest       = qaDigest
        , pkgDataRIV        = B.take 0x10 $ B.take (B.length contentID) klicensee <> B.replicate 0x10 0
        }
      (digest1, digest2) = nonFinalizedDigests header metadata
      encrypted = cryptNonFinalizedPKG' False 0 header pkgContents
  withBinaryFile fout WriteMode $ \h -> do
    BL.hPut h $ runPut $ void $ codecOut bin header
    B.hPut h digest1
    forM_ metadata $ \meta -> do
      BL.hPut h $ runPut $ void $ codecOut bin meta
    B.hPut h digest2
    forM_ encrypted $ \piece -> copyReadableToHandle piece h
    B.hPut h $ B.replicate 0x60 0 -- this is needed to install ok, but why?
