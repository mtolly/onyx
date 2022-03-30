{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PowerGig.Crypt where

import           Control.Monad
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash
import           Data.Bifunctor          (bimap, second)
import           Data.Binary.Codec.Class
import           Data.ByteArray          (convert)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable           (toList)
import           Data.List.Extra         (nubOrd)
import qualified Data.List.NonEmpty      as NE
import qualified Data.Map                as Map
import           Data.Maybe              (fromMaybe)
import           Data.SimpleHandle
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as TE
import qualified Data.Vector             as V
import           Foreign.Marshal.Utils   (withMany)
import           System.IO

powerGigBlockSize :: Int
powerGigBlockSize = 512

powerGigKey :: B.ByteString
powerGigKey = convert (hash ("The corresponding .model file does not exist for '%s'" :: B.ByteString) :: Digest SHA256)

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n bs = if B.null bs
  then []
  else let (x, y) = B.splitAt n bs in x : splitEvery n y

-- I'm not sure why you have to do this, but each block end doesn't decrypt properly otherwise.
-- Maxton says: "Apparently we have to overshoot by one block because the last block is never decrypted?"
decryptOvershoot :: AES256 -> IV AES256 -> B.ByteString -> B.ByteString
decryptOvershoot cipher iv bs
  = B.take (B.length bs) $ cbcDecrypt cipher iv $ bs <> B.replicate 16 0

getE2Header :: (MonadFail m) => B.ByteString -> m B.ByteString
getE2Header input = do
  Just iv <- return $ makeIV $ B.take 16 $ B.drop (powerGigBlockSize - 16) input
  CryptoPassed cipher <- return $ cipherInit powerGigKey
  return $ decryptOvershoot cipher iv $ B.take powerGigBlockSize input

decryptE2 :: (MonadFail m) => B.ByteString -> m BL.ByteString
decryptE2 input = do
  header <- getE2Header input
  Just iv <- return $ makeIV $ B.take 16 header
  CryptoPassed cipher <- return $ cipherInit $ B.take 32 $ B.drop 16 header
  let encBlocks = drop 1 $ splitEvery powerGigBlockSize input
      -- this length is needed so you don't get extra garbage when decrypting .lua files
      decryptLength = runGet getWord32le $ BL.fromStrict $ B.take 4 $ B.drop 0x30 header
  return $ BL.take (fromIntegral decryptLength)
    $ BL.fromChunks $ map (decryptOvershoot cipher iv) encBlocks

data PGHeader = PGHeader
  { h_Magic             :: Word32 -- 0x745
  , h_Version           :: Word32 -- 1
  , h_BlockSize         :: Word32
  , h_NumFiles          :: Word32
  , h_NumUnk            :: Word32
  , h_NumDirs           :: Word32
  , h_Unk1              :: Word32
  , h_Unk2              :: Word32
  , h_StringTableOffset :: Word32
  , h_StringTableSize   :: Word32
  , h_NumOffsets        :: Word32
  } deriving (Eq, Show)

instance Bin PGHeader where
  bin = do
    h_Magic             <- h_Magic             =. word32le
    h_Version           <- h_Version           =. word32le
    h_BlockSize         <- h_BlockSize         =. word32le
    h_NumFiles          <- h_NumFiles          =. word32le
    h_NumUnk            <- h_NumUnk            =. word32le
    h_NumDirs           <- h_NumDirs           =. word32le
    h_Unk1              <- h_Unk1              =. word32le
    h_Unk2              <- h_Unk2              =. word32le
    h_StringTableOffset <- h_StringTableOffset =. word32le
    h_StringTableSize   <- h_StringTableSize   =. word32le
    h_NumOffsets        <- h_NumOffsets        =. word32le
    return PGHeader{..}

data PGFileEntry = PGFileEntry
  { fe_Unk1      :: B.ByteString -- 6 bytes
  , fe_DirNum    :: Word16
  , fe_StringNum :: Word32
  , fe_OffsetNum :: Word32
  , fe_Size      :: Int64
  , fe_Unk2      :: B.ByteString -- 16 bytes
  , fe_Time      :: Int64
  } deriving (Eq, Show)

instance Bin PGFileEntry where
  bin = do
    fe_Unk1      <- fe_Unk1      =. byteString 6
    fe_DirNum    <- fe_DirNum    =. word16le
    fe_StringNum <- fe_StringNum =. word32le
    fe_OffsetNum <- fe_OffsetNum =. word32le
    fe_Size      <- fe_Size      =. int64le
    fe_Unk2      <- fe_Unk2      =. byteString 16
    fe_Time      <- fe_Time      =. int64le
    return PGFileEntry{..}

data PGDirEntry = PGDirEntry
  { de_PathHash  :: Word32
  , de_Parent    :: Int32
  , de_StringNum :: Word32
  } deriving (Eq, Show)

instance Bin PGDirEntry where
  bin = do
    de_PathHash  <- de_PathHash  =. word32le
    de_Parent    <- de_Parent    =. int32le
    de_StringNum <- de_StringNum =. word32le
    return PGDirEntry{..}

data PGOffsetEntry = PGOffsetEntry
  { oe_PKOffset :: Word32
  , oe_PKNum    :: Word16
  , oe_Unk      :: Word16
  } deriving (Eq, Show)

instance Bin PGOffsetEntry where
  bin = do
    oe_PKOffset <- oe_PKOffset =. word32le
    oe_PKNum    <- oe_PKNum    =. word16le
    oe_Unk      <- oe_Unk      =. word16le
    return PGOffsetEntry{..}

data FullHeader = FullHeader
  { fh_Header  :: PGHeader
  , fh_Files   :: V.Vector PGFileEntry
  , fh_Dirs    :: V.Vector PGDirEntry
  , fh_Strings :: V.Vector B.ByteString
  , fh_Offsets :: V.Vector PGOffsetEntry
  } deriving (Eq, Show)

readHeader :: (MonadFail m) => BL.ByteString -> m FullHeader
readHeader bs = let
  getter = do
    fh_Header  <- codecIn bin
    fh_Files   <- V.fromList <$> replicateM (fromIntegral $ h_NumFiles fh_Header) (codecIn bin)
    fh_Dirs    <- V.fromList <$> replicateM (fromIntegral $ h_NumDirs fh_Header) (codecIn bin)
    fh_Strings <- V.fromList . B.split 0 <$> getByteString (fromIntegral $ h_StringTableSize fh_Header)
    fh_Offsets <- V.fromList <$> replicateM (fromIntegral $ h_NumOffsets fh_Header) (codecIn bin)
    return FullHeader{..}
  in case runGetOrFail getter bs of
    Right (_, _, x) -> return x
    Left  err       -> fail $ show err

data PKReference = PKReference
  { pkArchive :: Int
  , pkOffset  :: Integer
  , pkLength  :: Integer
  } deriving (Eq, Show)

-- Each file is (pk num, offset, length)
getFolder :: FullHeader -> Folder B.ByteString PKReference
getFolder fh = fromFiles $ do
  f <- V.toList $ fh_Files fh
  let offset = fh_Offsets fh V.! fromIntegral (fe_OffsetNum f)
      getString i = fh_Strings fh V.! fromIntegral i
      path = getParents (fromIntegral $ fe_DirNum f) (return $ getString $ fe_StringNum f)
      getParents (-1) cur = cur
      getParents dirNum cur = let
        dir = fh_Dirs fh V.! dirNum
        newPath = case getString $ de_StringNum dir of
          ""  -> cur -- should be at top
          str -> NE.cons str cur
        in getParents (fromIntegral $ de_Parent dir) newPath
      ref = PKReference
        { pkArchive = fromIntegral $ oe_PKNum offset
        , pkOffset  = fromIntegral $ oe_PKOffset offset
        , pkLength  = fromIntegral $ fe_Size f
        }
  return (path, ref)

modifyFiles :: ((p, a) -> (p, a)) -> Folder p a -> Folder p a
modifyFiles f folder = Folder
  { folderSubfolders = map (second $ modifyFiles f) $ folderSubfolders folder
  , folderFiles = map f $ folderFiles folder
  }

-- Opens each .pk* file only once. So, don't access two subfiles at once, or handles will get messed up!
-- Decrypts all .e.2 and .lua files.
withPKFiles
  :: FilePath
  -> Folder B.ByteString PKReference
  -> (Folder T.Text Readable -> IO a)
  -> IO a
withPKFiles base dir inner = do
  let pkNums = nubOrd $ map pkArchive $ toList dir
      pkPaths = [ base <> ".pk" <> show i | i <- pkNums ]
  withMany (`withBinaryFile` ReadMode) pkPaths $ \pks -> do
    let pkMapping = Map.fromList $ zip pkNums pks
        getPK i = fromMaybe (error "panic! .pk not loaded") $ Map.lookup i pkMapping
        getReadable ref = subHandle' id (pkOffset ref) (Just $ pkLength ref)
          (return $ getPK $ pkArchive ref) (\_ -> return ())
        decrypt (name, r) = case T.stripSuffix ".e.2" name of
          Just name' -> (name', transformBytes (fromMaybe (error "Couldn't decrypt .e.2 file") . decryptE2 . BL.toStrict) r)
          Nothing    -> if ".lua" `T.isSuffixOf` name
            then (name, transformBytes (fromMaybe (error "Couldn't decrypt .lua file") . decryptE2 . BL.toStrict) r)
            else (name, r)
        readableFolder = modifyFiles decrypt $ bimap TE.decodeLatin1 getReadable dir
    inner readableFolder

extractFolder :: FilePath -> Folder B.ByteString PKReference -> FilePath -> IO ()
extractFolder base dir dest = withPKFiles base dir (`saveHandleFolder` dest)
