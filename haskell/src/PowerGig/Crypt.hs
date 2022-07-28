{-# LANGUAGE MultiWayIf        #-}
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
  -- taken from tornado of souls, other values work fine for disc but not dlc?
  let ivBytes       = B.pack [0xf1,0xad,0x72,0x5b,0xc7,0xee,0x83,0x20,0xbf,0xab,0xbb,0xf7,0xf2,0x91,0x5a,0xc0]
      cipherBytes   = B.pack [0x17,0x96,0x1,0xfa,0x33,0xb1,0xee,0x2a,0xd,0x20,0xea,0xed,0xc8,0x37,0xc8,0x12,0xf1,0x1,0x4d,0xb5,0x20,0x57,0x54,0x1e,0xac,0xc,0x22,0xe8,0xd6,0x94,0xd8,0x7c]
      headerIVBytes = B.pack [0x67,0xe9,0x85,0xe1,0x24,0x5e,0xb7,0x98,0x6e,0xec,0xee,0x0c,0x55,0xf9,0xb2,0x2c]
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
        (splitEvery powerGigBlockSize $ input <> B.replicate 16 0xD)
  Just iv <- return $ makeIV ivBytes
  CryptoPassed cipher <- return $ cipherInit cipherBytes
  let _ = [iv, headerIV] :: [IV AES256]
      dataSizeRoundedUp = (quot (B.length input) 0x10 + 1) * 0x10
  return $ BL.take (fromIntegral $ powerGigBlockSize + dataSizeRoundedUp)
    $ BL.fromChunks $ headerBlockEnc : map (cbcEncrypt cipher iv) blocks

data PGHeader = PGHeader
  { h_Magic             :: Word32 -- 0x745
  , h_Version           :: Word32 -- 1
  , h_BlockSize         :: Word32 -- 2048 in 360 disc, 1 in PS3 disc and 360 tornado of souls
  , h_NumFiles          :: Word32
  , h_FilesOffset       :: Word32 -- always 44
  , h_NumDirs           :: Word32
  , h_DirsOffset        :: Word32
  , h_NumStrings        :: Word32
  , h_StringTableOffset :: Word32
  , h_StringTableSize   :: Word32
  , h_NumOffsets        :: Word32
  } deriving (Eq, Show, Read)

instance Bin PGHeader where
  bin = do
    h_Magic             <- h_Magic             =. word32le
    h_Version           <- h_Version           =. word32le
    h_BlockSize         <- h_BlockSize         =. word32le
    h_NumFiles          <- h_NumFiles          =. word32le
    h_FilesOffset       <- h_FilesOffset       =. word32le
    h_NumDirs           <- h_NumDirs           =. word32le
    h_DirsOffset        <- h_DirsOffset        =. word32le
    h_NumStrings        <- h_NumStrings        =. word32le
    h_StringTableOffset <- h_StringTableOffset =. word32le
    h_StringTableSize   <- h_StringTableSize   =. word32le
    h_NumOffsets        <- h_NumOffsets        =. word32le
    return PGHeader{..}

data PGFileEntry = PGFileEntry
  { fe_Lua       :: Word16 -- 1 if .lua file, 2 if compressed .xml file, otherwise 0
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
  { fh_Header   :: PGHeader
  , fh_Contents :: HeaderContents
  } deriving (Eq, Show)

data HeaderContents = HeaderContents
  { hc_Files   :: V.Vector PGFileEntry
  , hc_Dirs    :: V.Vector PGDirEntry
  , hc_Strings :: V.Vector B.ByteString
  , hc_Offsets :: V.Vector PGOffsetEntry
  } deriving (Eq, Show)

readHeader :: (MonadFail m) => BL.ByteString -> m FullHeader
readHeader bs = let
  getter = do
    hdr <- codecIn bin
    hc_Files   <- V.fromList <$> replicateM (fromIntegral $ h_NumFiles hdr) (codecIn bin)
    hc_Dirs    <- V.fromList <$> replicateM (fromIntegral $ h_NumDirs hdr) (codecIn bin)
    hc_Strings <- V.fromList . take (fromIntegral $ h_NumStrings hdr) . B.split 0
      <$> getByteString (fromIntegral $ h_StringTableSize hdr)
    hc_Offsets <- V.fromList <$> replicateM (fromIntegral $ h_NumOffsets hdr) (codecIn bin)
    return FullHeader { fh_Header = hdr, fh_Contents = HeaderContents{..} }
  in case runGetOrFail getter bs of
    Right (_, _, x) -> return x
    Left  err       -> fail $ show err

buildHeader :: HeaderContents -> BL.ByteString
buildHeader hc = let
  files = runPut $ mapM_ (codecOut bin) $ V.toList $ hc_Files hc
  dirs  = runPut $ mapM_ (codecOut bin) $ V.toList $ hc_Dirs hc
  strings = BL.fromChunks $ concatMap (\b -> [b, "\0"]) $ V.toList $ hc_Strings hc
  offsets = runPut $ mapM_ (codecOut bin) $ V.toList $ hc_Offsets hc
  newHeader = PGHeader
    { h_Magic             = 0x745
    , h_Version           = 1
    , h_BlockSize         = 1
    , h_NumFiles          = fromIntegral $ V.length $ hc_Files hc
    , h_FilesOffset       = 44
    , h_NumDirs           = fromIntegral $ V.length $ hc_Dirs hc
    , h_DirsOffset        = 44 + 0x30 * fromIntegral (V.length $ hc_Files hc)
    , h_NumStrings        = fromIntegral $ V.length $ hc_Strings hc
    , h_StringTableOffset = fromIntegral $ 44 + BL.length files + BL.length dirs
    , h_StringTableSize   = fromIntegral $ BL.length strings
    , h_NumOffsets        = fromIntegral $ V.length $ hc_Offsets hc
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

getFolder :: HeaderContents -> Folder B.ByteString PKReference
getFolder hc = fromFiles $ do
  f <- V.toList $ hc_Files hc
  let offset = hc_Offsets hc V.! fromIntegral (fe_OffsetNum f)
      getString i = hc_Strings hc V.! fromIntegral i
      path = getParents (fromIntegral $ fe_DirNum f) (return $ getString $ fe_StringNum f)
      getParents (-1) cur = cur
      getParents dirNum cur = let
        dir = hc_Dirs hc V.! dirNum
        newPath = case getString $ de_StringNum dir of
          ""  -> cur -- should be at top
          str -> NE.cons str cur
        in getParents (fromIntegral $ de_Parent dir) newPath
      ref = PKReference
        { pkFileEntry = f
        , pkOffsetEntries = offset :| do
          i <- take (fromIntegral $ oe_Copies offset) [fromIntegral (fe_OffsetNum f) + 1 ..]
          return $ hc_Offsets hc V.! i
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

rebuildFullHeader :: Folder B.ByteString PKReference -> HeaderContents
rebuildFullHeader dir = let
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
  in HeaderContents
    { hc_Files = V.fromList $ toList $ rbsFiles finalState
    , hc_Dirs = V.fromList $ toList $ rbsDirs finalState
    , hc_Strings = V.fromList allStrings
    , hc_Offsets = V.fromList $ toList $ rbsOffsets finalState
    }

makeNewPK :: Int -> Folder T.Text Readable -> IO (Folder B.ByteString PKReference, BL.ByteString)
makeNewPK pkNumber root = runStateT (traverseFiles eachFile $ first (B8.pack . T.unpack) root) BL.empty where
  eachFile name r = do
    pk <- get
    bs <- liftIO $ useHandle r handleToByteString
    let md5 = BA.convert (hash $ BL.toStrict bs :: Digest MD5)
    put $ pk <> bs -- do we need to pad this?
    return PKReference
      { pkFileEntry = PGFileEntry
        { fe_Lua = if
          | ".lua" `B.isSuffixOf` name -> 1
          | "CMP1" `BL.isPrefixOf` bs  -> 2
          | otherwise                  -> 0
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
  in bimap TE.decodeLatin1 getReadable pkTree

decryptPKContents
  :: Folder T.Text Readable
  -> Folder T.Text Readable
decryptPKContents = modifyFiles $ \(name, r) -> case T.stripSuffix ".e.2" name of
  Just name' -> (name', transformBytes (fromMaybe (error "Couldn't decrypt .e.2 file") . decryptE2 . BL.toStrict) r)
  Nothing    -> if ".lua" `T.isSuffixOf` name
    then (name, transformBytes (fromMaybe (error "Couldn't decrypt .lua file") . decryptE2 . BL.toStrict) r)
    else (name, r)

encryptPKContents
  :: Folder T.Text Readable
  -> Folder T.Text Readable
encryptPKContents = modifyFiles $ \(name, r) -> if ".lua" `T.isSuffixOf` name
  then (name, transformBytes (fromMaybe (error "Couldn't encrypt .lua file") . encryptE2 . BL.toStrict) r)
  else if "_all.xma" `T.isSuffixOf` name
    then (name <> ".e.2", transformBytes (fromMaybe (error "Couldn't encrypt .xma file") . encryptE2 . BL.toStrict) r)
    else (name, r)
