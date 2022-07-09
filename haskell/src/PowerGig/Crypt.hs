{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PowerGig.Crypt where

import           Control.Monad
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State (execState, get, gets, modify, put,
                                            runStateT)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Crypto.Hash
import           Data.Bifunctor            (bimap, first, second)
import           Data.Binary.Codec.Class
import           Data.Bits                 (xor)
import           Data.ByteArray            (convert)
import qualified Data.ByteArray            as BA
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as B8
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (toLower)
import           Data.Foldable             (toList)
import qualified Data.HashMap.Strict       as HM
import           Data.List.Extra           (nubOrd)
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.List.NonEmpty        as NE
import qualified Data.Map                  as Map
import           Data.Maybe                (fromMaybe)
import           Data.Sequence             (Seq, (><), (|>))
import qualified Data.Sequence             as Seq
import           Data.SimpleHandle
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import qualified Data.Vector               as V
import           Foreign.Marshal.Utils     (withMany)
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

encryptE2 :: (MonadFail m) => B.ByteString -> m BL.ByteString
encryptE2 input = do
  let ivBytes       = B.replicate 16 0x15
      cipherBytes   = B.replicate 32 0x69
      headerIVBytes = B.replicate 16 0x42
      line4         = BL.toStrict $ runPut $ do
        putWord32le $ fromIntegral $ B.length input
        putWord32le 0
        putWord32le 2
        putWord32le 0
      headerBlock = ivBytes <> cipherBytes <> line4 <> B.replicate (powerGigBlockSize - 0x40) 0
  Just headerIV <- return $ makeIV headerIVBytes
  CryptoPassed headerCipher <- return $ cipherInit powerGigKey
  let headerBlockEnc
        = B.take (powerGigBlockSize - 0x10) (cbcEncrypt headerCipher headerIV headerBlock)
        <> headerIVBytes
      blocks = map
        (\b -> if B.length b == powerGigBlockSize then b else b <> B.replicate (powerGigBlockSize - B.length b) 0)
        (splitEvery powerGigBlockSize input)
  Just iv <- return $ makeIV ivBytes
  CryptoPassed cipher <- return $ cipherInit cipherBytes
  let _ = [iv, headerIV] :: [IV AES256]
  return $ BL.fromChunks $ headerBlockEnc : map (cbcEncrypt cipher iv) blocks

data PGHeader = PGHeader
  { h_Magic             :: Word32 -- 0x745
  , h_Version           :: Word32 -- 1
  , h_BlockSize         :: Word32
  , h_NumFiles          :: Word32
  , h_Unk1              :: Word32
  , h_NumDirs           :: Word32
  , h_Unk2              :: Word32
  , h_NumStrings        :: Word32
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
    h_Unk1              <- h_Unk1              =. word32le
    h_NumDirs           <- h_NumDirs           =. word32le
    h_Unk2              <- h_Unk2              =. word32le
    h_NumStrings        <- h_NumStrings        =. word32le
    h_StringTableOffset <- h_StringTableOffset =. word32le
    h_StringTableSize   <- h_StringTableSize   =. word32le
    h_NumOffsets        <- h_NumOffsets        =. word32le
    return PGHeader{..}

data PGFileEntry = PGFileEntry
  { fe_Lua       :: Word16 -- 1 if .lua file
  , fe_PathHash  :: Word32 -- FNV132 hash of lowercase filename using backslash as separator
  , fe_DirNum    :: Word16
  , fe_StringNum :: Word32
  , fe_OffsetNum :: Word32
  , fe_Size      :: Int64
  , fe_FileHash  :: B.ByteString -- MD5 of file contents
  , fe_Time      :: Int64 -- maxton labeled this time, but what format?
  } deriving (Eq, Show)

instance Bin PGFileEntry where
  bin = do
    fe_Lua       <- fe_Lua       =. word16le
    fe_PathHash  <- fe_PathHash  =. word32le
    fe_DirNum    <- fe_DirNum    =. word16le
    fe_StringNum <- fe_StringNum =. word32le
    fe_OffsetNum <- fe_OffsetNum =. word32le
    fe_Size      <- fe_Size      =. int64le
    fe_FileHash  <- fe_FileHash  =. byteString 16
    fe_Time      <- fe_Time      =. int64le
    return PGFileEntry{..}

data PGDirEntry = PGDirEntry
  { de_PathHash  :: Word32 -- FNV132 hash of lowercase filename using backslash as separator, e.g. "animtool\\textures" (one backslash not two)
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
  , oe_Copies   :: Word16 -- number of subsequent offset entries which contain extra copies of this data
  } deriving (Eq, Show)

instance Bin PGOffsetEntry where
  bin = do
    oe_PKOffset <- oe_PKOffset =. word32le
    oe_PKNum    <- oe_PKNum    =. word16le
    oe_Copies   <- oe_Copies   =. word16le
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
    fh_Strings <- V.fromList . take (fromIntegral $ h_NumStrings fh_Header) . B.split 0
      <$> getByteString (fromIntegral $ h_StringTableSize fh_Header)
    fh_Offsets <- V.fromList <$> replicateM (fromIntegral $ h_NumOffsets fh_Header) (codecIn bin)
    return FullHeader{..}
  in case runGetOrFail getter bs of
    Right (_, _, x) -> return x
    Left  err       -> fail $ show err

buildHeader :: FullHeader -> BL.ByteString
buildHeader fh = let
  files = runPut $ mapM_ (codecOut bin) $ V.toList $ fh_Files fh
  dirs  = runPut $ mapM_ (codecOut bin) $ V.toList $ fh_Dirs fh
  strings = BL.fromChunks $ concatMap (\b -> [b, "\0"]) $ V.toList $ fh_Strings fh
  offsets = runPut $ mapM_ (codecOut bin) $ V.toList $ fh_Offsets fh
  newHeader = (fh_Header fh)
    { h_NumFiles          = fromIntegral $ V.length $ fh_Files fh
    , h_NumDirs           = fromIntegral $ V.length $ fh_Dirs fh
    , h_NumStrings        = fromIntegral $ V.length $ fh_Strings fh
    , h_StringTableOffset = fromIntegral $ 44 + BL.length files + BL.length dirs
    , h_StringTableSize   = fromIntegral $ BL.length strings
    , h_NumOffsets        = fromIntegral $ V.length $ fh_Offsets fh
    }
  in runPut (void $ codecOut bin newHeader) <> files <> dirs <> strings <> offsets

data PKReference = PKReference
  { pkFileEntry     :: PGFileEntry
  , pkOffsetEntries :: NonEmpty PGOffsetEntry
  } deriving (Eq, Show)

pkArchive :: PKReference -> Int
pkArchive = fromIntegral . oe_PKNum . NE.head . pkOffsetEntries

pkOffset :: PKReference -> Integer
pkOffset = fromIntegral . oe_PKOffset . NE.head . pkOffsetEntries

pkLength :: PKReference -> Integer
pkLength = fromIntegral . fe_Size . pkFileEntry

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
        { pkFileEntry = f
        , pkOffsetEntries = offset :| do
          i <- take (fromIntegral $ oe_Copies offset) [fromIntegral (fe_OffsetNum f) + 1 ..]
          return $ fh_Offsets fh V.! i
        }
  return (path, ref)

data RebuildState = RebuildState
  { rbsFiles   :: Seq PGFileEntry
  , rbsDirs    :: Seq PGDirEntry
  , rbsOffsets :: Seq PGOffsetEntry
  }

fnv132lowercase :: B.ByteString -> Word32
fnv132lowercase = go 2166136261 . B8.unpack where
  go cur []       = cur
  go cur (c : cs) = go ((cur * 16777619) `xor` byte c) cs
  byte :: Char -> Word32
  byte = fromIntegral . fromEnum . toLower

rebuildFullHeader :: PGHeader -> Folder B.ByteString PKReference -> FullHeader
rebuildFullHeader hdr dir = let
  getAllStrings folder = map fst (folderSubfolders folder) <> map fst (folderFiles folder)
    <> concatMap (getAllStrings . snd) (folderSubfolders folder)
  allStrings = nubOrd $ "" : getAllStrings dir
  stringToIndexMap = HM.fromList $ zip allStrings [0..]
  stringToIndex s = case HM.lookup s stringToIndexMap of
    Nothing -> error $ "rebuildFullHeader: panic! string not in table: " <> show s
    Just i  -> i
  hashPath :: [B.ByteString] -> Word32
  hashPath = fnv132lowercase . B.intercalate "\\" . reverse
  go curDir curParentIndex curName = do
    -- first add new dir entry
    let newDirEntry = PGDirEntry
          { de_PathHash  = hashPath curName
          , de_Parent    = fromIntegral curParentIndex
          , de_StringNum = stringToIndex $ case curName of
            []    -> ""
            x : _ -> x
          }
    dirIndex <- gets $ Seq.length . rbsDirs
    modify $ \rbs -> rbs { rbsDirs = rbsDirs rbs |> newDirEntry }
    -- then add new file entries
    forM_ (folderFiles curDir) $ \(fileName, pkref) -> do
      offsetIndex <- gets $ Seq.length . rbsOffsets
      let fileEntry = (pkFileEntry pkref)
            { fe_PathHash  = hashPath $ fileName : curName
            , fe_DirNum    = fromIntegral dirIndex
            , fe_StringNum = stringToIndex fileName
            , fe_OffsetNum = fromIntegral offsetIndex
            }
      modify $ \rbs -> rbs
        { rbsOffsets = rbsOffsets rbs >< Seq.fromList (NE.toList $ pkOffsetEntries pkref)
        , rbsFiles   = rbsFiles rbs |> fileEntry
        }
    -- then recurse into subfolders
    forM_ (folderSubfolders curDir) $ \(folderName, subDir) -> do
      go subDir dirIndex (folderName : curName)
  finalState = execState (go dir (-1) []) $ RebuildState Seq.empty Seq.empty Seq.empty
  in FullHeader
    { fh_Header = hdr
    , fh_Files = V.fromList $ toList $ rbsFiles finalState
    , fh_Dirs = V.fromList $ toList $ rbsDirs finalState
    , fh_Strings = V.fromList allStrings
    , fh_Offsets = V.fromList $ toList $ rbsOffsets finalState
    }

makeNewPK :: Int -> Folder T.Text Readable -> IO (Folder B.ByteString PKReference, BL.ByteString)
makeNewPK pkNumber root = runStateT (mapM eachFile $ first (B8.pack . T.unpack) root) BL.empty where
  eachFile r = do
    pk <- get
    bs <- liftIO $ useHandle r handleToByteString
    let md5 = BA.convert (hash $ BL.toStrict bs :: Digest MD5)
    put $ pk <> bs -- do we need to pad this?
    return PKReference
      { pkFileEntry = PGFileEntry
        { fe_Lua = 0 -- TODO
        , fe_PathHash = 0 -- filled in later
        , fe_DirNum = 0 -- filled in later
        , fe_StringNum = 0 -- filled in later
        , fe_OffsetNum = 0 -- filled in later
        , fe_Size = fromIntegral $ BL.length bs
        , fe_FileHash = md5
        , fe_Time = 0 -- unknown
        }
      , pkOffsetEntries = return PGOffsetEntry
        { oe_PKOffset = fromIntegral $ BL.length pk
        , oe_PKNum = fromIntegral pkNumber
        , oe_Copies = 0
        }
      }

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

-- Each Readable opens the correct .pk* file itself.
connectPKFiles
  :: Folder T.Text Readable
  -> FilePath
  -> Folder B.ByteString PKReference
  -> Folder T.Text Readable
connectPKFiles extTree base pkTree = let
  pkNums = nubOrd $ map pkArchive $ toList pkTree
  pkReadables = do
    i <- pkNums
    toList $ findFile (return $ T.pack $ base <> ".pk" <> show i) extTree
  pkMapping = Map.fromList $ zip pkNums pkReadables
  getPK i = fromMaybe (error "panic! .pk not found") $ Map.lookup i pkMapping
  getReadable ref = subHandle id (pkOffset ref) (Just $ pkLength ref) (getPK $ pkArchive ref)
  decrypt (name, r) = case T.stripSuffix ".e.2" name of
    Just name' -> (name', transformBytes (fromMaybe (error "Couldn't decrypt .e.2 file") . decryptE2 . BL.toStrict) r)
    Nothing    -> if ".lua" `T.isSuffixOf` name
      then (name, transformBytes (fromMaybe (error "Couldn't decrypt .lua file") . decryptE2 . BL.toStrict) r)
      else (name, r)
  in modifyFiles decrypt $ bimap TE.decodeLatin1 getReadable pkTree
