{- |
WIP port of (parts of) X360, a GPL C# library by DJ Shepherd
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module STFS.Create where

import           Control.Applicative            (liftA2)
import           Control.Monad.Extra            (andM, anyM, forM, forM_, guard,
                                                 join, orM, unless, unlessM,
                                                 void, when, whenM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.StackTrace (SendMessage, StackTraceT,
                                                 catchError, fatal, lg, stackIO,
                                                 throwError)
import           Data.Bits                      (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString                as B
import           Data.Char                      (toLower)
import           Data.Function                  ((&))
import           Data.Int                       (Int16, Int32, Int64)
import           Data.IORef                     (IORef, newIORef)
import           Data.List.Split                (splitOn)
import           Data.Maybe                     (isJust)
import           Data.StateVar                  (GettableStateVar, StateVar,
                                                 get, ($=))
import qualified Data.Time                      as Time
import           Data.Word                      (Word16, Word32, Word8)
import qualified System.Directory               as Dir
import           System.IO                      (IOMode (..), hFileSize,
                                                 withBinaryFile)

var :: (MonadIO m) => a -> m (IORef a)
var = liftIO . newIORef

type Nullable a = IORef (Maybe a)

type List a = IORef [a]

type Meth m o a = o -> StackTraceT m a

data DJsIO

listAdd :: (MonadIO m) => a -> Maybe (List a) -> StackTraceT m ()
listAdd x mlst = do
  lst <- noNull mlst
  xs <- get lst
  lst $= (xs ++ [x])

listCount :: (MonadIO m) => Maybe (List a) -> StackTraceT m Int32
listCount mlst = noNull mlst >>= fmap (fromIntegral . length) . get

hsList :: (MonadIO m) => Maybe (List a) -> StackTraceT m [a]
hsList mlst = noNull mlst >>= get

fun :: (Monad m) => ((a -> ContT a (StackTraceT m) b) -> ContT a (StackTraceT m) a) -> StackTraceT m a
fun = evalContT . callCC

lastIndexOf :: (Eq a) => a -> [a] -> Maybe Int
lastIndexOf x xs = lookup x $ reverse $ zip xs [0..]

noNull :: (Monad m) => Maybe a -> StackTraceT m a
noNull Nothing  = fatal "Null reference"
noNull (Just x) = return x

setIndex :: (MonadIO m) => Int -> a -> Maybe (List a) -> StackTraceT m ()
setIndex i x mlst = do
  lst <- noNull mlst
  xs <- get lst
  lst $= case splitAt i xs of
    (a, b) -> a ++ [x] ++ drop 1 b

newDJsIO_temp :: (MonadIO m) => Bool -> StackTraceT m DJsIO
newDJsIO_temp = undefined
{-
public DJsIO(bool BigEndian)
{
    IsBigEndian = BigEndian;
    xFile = string.Copy(VariousFunctions.GetTempFileLocale());
    XSetStream(DJFileMode.Create);
}
-}

dj_Dispose :: (MonadIO m) => Bool -> Meth m DJsIO Bool
dj_Dispose = undefined
{-
public virtual bool Dispose()
{
    return Dispose(false);
}

internal bool Dispose(bool DeleteFile)
{
    if (!Close())
        return false;
    if (xThisData != DataType.Real)
    {
        try { xStream.Dispose(); }
        catch { return false; }
        if (xThisData == DataType.File && DeleteFile)
            VariousFunctions.DeleteFile(FileNameLong);
    }
    xFile = null;
    txtidx = null;
    return true;
}
-}

dj_Close :: Meth m DJsIO Bool
dj_Close = undefined
{-
public virtual bool Close()
{
    if (Accessed)
    {
        try { xStream.Close(); }
        catch { return false; }
    }
    return true;
}
-}

glue4Bytes :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
glue4Bytes a b c d
  =   (fromIntegral a `shiftL` 24)
  .|. (fromIntegral b `shiftL` 16)
  .|. (fromIntegral c `shiftL` 8)
  .|.  fromIntegral d

glue2Bytes :: Word8 -> Word8 -> Word16
glue2Bytes a b = (fromIntegral a `shiftL` 8) .|. fromIntegral b

(%) :: (a -> b) -> a -> b
(%) = id
infixl 0 %

listIndex :: (MonadIO m) => Int -> Maybe (List a) -> StackTraceT m a
listIndex i mlst = noNull mlst >>= get >>= return . (!! i)

---------
-- STFSDescriptor.cs
---------

data BlockRecord = BlockRecord
  { br_xFlags     :: Nullable (List Word8)
  , br_xThisBlock :: Nullable (List Word8)
  , br_xLevel     :: IORef Word8
  }

initBlockRecord :: (MonadIO m) => StackTraceT m BlockRecord
initBlockRecord = do
  br_xFlags <- var Nothing
  br_xThisBlock <- var (replicate 3 0) >>= var . Just
  br_xLevel <- var 0
  return BlockRecord{..}

br_ThisBlock :: BlockRecord -> StateVar Word32
br_ThisBlock = undefined

br_ThisLevel :: BlockRecord -> StateVar TreeLevel
br_ThisLevel = undefined

br_Indicator :: BlockRecord -> StateVar Word8
br_Indicator = undefined

br_Flags :: BlockRecord -> StateVar Word32
br_Flags = undefined

-- public BlockRecord() { xFlags = new byte[] { 0, 0, 0, 0 }; }
newBlockRecord_empty :: (MonadIO m) => StackTraceT m BlockRecord
newBlockRecord_empty = do
  this <- initBlockRecord
  (br_xFlags this $=) . Just =<< var [0, 0, 0, 0]
  return this

-- public BlockRecord(uint xFlagIn) { Flags = xFlagIn; }
newBlockRecord_uint :: (MonadIO m) => Word32 -> StackTraceT m BlockRecord
newBlockRecord_uint xFlagIn = do
  this <- initBlockRecord
  (br_Flags this) $= xFlagIn
  return this

-- public BlockRecord(HashStatus xStatus, uint xNext) { Flags = (uint)((uint)xStatus << 30 | (xNext & 0xFFFFFF)); }
newBlockRecord_hash_uint :: (MonadIO m) => HashStatus -> Word32 -> StackTraceT m BlockRecord
newBlockRecord_hash_uint = undefined

br_Status :: BlockRecord -> StateVar HashStatus
br_Status = undefined

br_NextBlock :: BlockRecord -> StateVar Word32
br_NextBlock = undefined

br_MarkOld :: (MonadIO m) => Meth m BlockRecord ()
br_MarkOld = undefined

br_Index :: BlockRecord -> GettableStateVar Word8
br_Index = undefined

br_AllocationFlag :: BlockRecord -> StateVar HashFlag
br_AllocationFlag = undefined

br_BlocksFree :: BlockRecord -> StateVar Int32
br_BlocksFree = undefined

br_Switch :: (MonadIO m) => BlockRecord -> StackTraceT m Bool
br_Switch = undefined

data STFSDescriptor = STFSDescriptor
  { sd_xStruct       :: Nullable (List Word8)
  , sd_xSpaceBetween :: Nullable (List Word32) -- also SpaceBetween
  , sd_xBaseByte     :: IORef Word8
  , sd_xBlockCount   :: IORef Word32 -- also BlockCount
  , sd_TopRecord     :: Nullable BlockRecord
  , sd_Shift         :: IORef Word8
  }

sd_DirectoryBlockCount :: STFSDescriptor -> StateVar Word16
sd_DirectoryBlockCount = undefined

sd_DirectoryBlock :: STFSDescriptor -> StateVar Word32
sd_DirectoryBlock = undefined

sd_BaseBlock :: STFSDescriptor -> GettableStateVar Word16
sd_BaseBlock = undefined

sd_ThisType :: STFSDescriptor -> GettableStateVar STFSType
sd_ThisType = undefined

sd_OldBlockCount :: STFSDescriptor -> GettableStateVar Word32
sd_OldBlockCount = undefined

initSTFSDescriptor :: (MonadIO m) => StackTraceT m STFSDescriptor
initSTFSDescriptor = do
  sd_xStruct <- var Nothing
  sd_xSpaceBetween <- var (replicate 3 0) >>= var . Just
  sd_xBaseByte <- var 0
  sd_xBlockCount <- var 0
  sd_TopRecord <- newBlockRecord_empty >>= var . Just
  sd_Shift <- var 0
  return STFSDescriptor{..}

-- internal STFSDescriptor(STFSType xType, uint xTotalBlocks)
newSTFSDescriptor_type_uint :: (MonadIO m) => STFSType -> Word32 -> StackTraceT m STFSDescriptor
newSTFSDescriptor_type_uint xType xTotalBlocks = do
  this <- initSTFSDescriptor
  this & sd_XSetStructure xType
  (sd_xStruct this $=) . Just =<< var [0, 0, 0, 0, 0]
  sb <- get (sd_xSpaceBetween this) >>= hsList
  when (xTotalBlocks > (sb !! 2)) $ do
    sd_xStruct this $= Nothing
    fatal "STFSExcepts.MaxOver"
  sd_xBlockCount this $= xTotalBlocks
  (sd_xBaseByte this $=) =<< do
    get (sd_ThisType this) >>= return . \case
      STFSType_Type0 -> 0xB
      _              -> 0xA
  return this

-- void XSetStructure(STFSType xType)
sd_XSetStructure :: (MonadIO m) => STFSType -> Meth m STFSDescriptor ()
sd_XSetStructure xType this = case xType of
  STFSType_Type0 -> do
    xsb <- get $ sd_xSpaceBetween this
    xsb & setIndex 0 0xA0
    xsb & setIndex 1 0x718F
    xsb & setIndex 2 0xFE7DA -- Max Block
    sd_Shift this $= 0
  STFSType_Type1 -> do
    xsb <- get $ sd_xSpaceBetween this
    xsb & setIndex 0 0xAC
    xsb & setIndex 1 0x723A
    xsb & setIndex 2 0xFD00B -- Max Block before size of package over does FATX limit
    sd_Shift this $= 1

-- internal STFSDescriptor(byte[] xDescriptor, uint xTotalBlocks, uint xOldBlocks, byte xType)
newSTFSDescriptor_bytes_uint_uint_byte :: (MonadIO m) => Maybe (List Word8) -> Word32 -> Word32 -> Word8 -> StackTraceT m STFSDescriptor
newSTFSDescriptor_bytes_uint_uint_byte = undefined

-- internal STFSDescriptor(STFSPackage xPackage)
newSTFSDescriptor_pkg :: (MonadIO m) => Maybe STFSPackage -> StackTraceT m STFSDescriptor
newSTFSDescriptor_pkg = undefined

-- internal uint GenerateDataBlock(uint xBlock)
sd_GenerateDataBlock :: (MonadIO m) => Word32 -> Meth m STFSDescriptor Word32
sd_GenerateDataBlock xBlock this
  | xBlock >= 0x4AF768 = return $ fromIntegral constants_STFSEnd
  | otherwise = catchError
    % do
      shft <- fromIntegral <$> get (sd_Shift this)
      return $ sum $ concat
        -- Gets 0xAA section, shifts it for 1 or 2 tables per section, and adds original block
        [ return $ (((xBlock `quot` constants_BlockLevel0) + 1) `shiftL` shft) + xBlock
        -- Gets current 0x70e4 section, adjusts to table count
        , do
          guard $ xBlock >= constants_BlockLevel0
          return $ ((xBlock `quot` constants_BlockLevel1) + 1) `shiftL` shft
        -- There is only going to be 1 0x4AF768 section, add to base
        , do
          guard $ xBlock >= constants_BlockLevel1
          return $ 1 `shiftL` shft
        ]
    % \_ -> fatal "STFSExcepts.General"

-- internal uint GenerateHashBlock(uint xBlock, TreeLevel xTree)
sd_GenerateHashBlock :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSDescriptor Word32
sd_GenerateHashBlock xBlock xTree this
  | xBlock >= 0x4AF768 = return $ fromIntegral constants_STFSEnd
  | otherwise = catchError
    % case xTree of
      L0 -> do
        sp0 <- get (sd_xSpaceBetween this) >>= listIndex 0
        shft <- fromIntegral <$> get (sd_Shift this)
        return $ sum $ concat
          -- Get Base Level 0 Table
          [ return $ (xBlock `quot` constants_BlockLevel0) * sp0
          -- Adjusts the result for Level 1 table count
          , do
            guard $ xBlock >= constants_BlockLevel0
            return $ ((xBlock `quot` constants_BlockLevel1) + 1) `shiftL` shft
          -- Adjusts for the Level 2 table
          , do
            guard $ xBlock >= constants_BlockLevel1
            return $ 1 `shiftL` shft
          ]
      L1 -> do
        -- Grab the number of Table 1 blocks
        if xBlock < constants_BlockLevel1
          then get (sd_xSpaceBetween this) >>= listIndex 0
          else do
            sb1 <- get (sd_xSpaceBetween this) >>= listIndex 1
            shft <- fromIntegral <$> get (sd_Shift this)
            return $ sb1 * (xBlock `quot` constants_BlockLevel1) + (1 `shiftL` shft)
      L2 -> do
        -- Only one Level 2 table
        get (sd_xSpaceBetween this) >>= listIndex 1
      _ -> return $ fromIntegral constants_STFSEnd
    % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
sd_GenerateHashOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSDescriptor Int64
sd_GenerateHashOffset xBlock xTree this
  | xBlock >= 0x4AF768 = return $ fromIntegral constants_STFSEnd
  | otherwise = catchError
    % do
      result <- this & sd_GenerateHashBlock xBlock xTree
      xReturn <- this & sd_BlockToOffset result
      return $ xReturn + case xTree of
        L0 -> 0x18 * fromIntegral (xBlock `rem` constants_BlockLevel0)
        L1 -> 0x18 * fromIntegral ((xBlock `quot` constants_BlockLevel0) `rem` constants_BlockLevel0)
        L2 -> 0x18 * fromIntegral ((xBlock `quot` constants_BlockLevel1) `rem` constants_BlockLevel0)
        _  -> 0
    % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateDataOffset(uint xBlock)
sd_GenerateDataOffset :: (MonadIO m) => Word32 -> Meth m STFSDescriptor Int64
sd_GenerateDataOffset xBlock this = catchError
  % do
    if xBlock >= 0x4AF768
      then return $ fromIntegral constants_STFSEnd
      else (`sd_BlockToOffset` this) =<< (this & sd_GenerateDataBlock xBlock)
  % \_ -> fatal "STFSExcepts.General"

-- internal long BlockToOffset(uint xBlock)
sd_BlockToOffset :: (MonadIO m) => Word32 -> Meth m STFSDescriptor Int64
sd_BlockToOffset xBlock this = catchError
  % do
    ((fromIntegral xBlock * 0x1000) +) . fromIntegral <$> get (sd_BaseBlock this)
  % \_ -> fatal "STFSExcepts.General"

-- internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
sd_GenerateBaseOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSDescriptor Int64
sd_GenerateBaseOffset xBlock xTree this = catchError
  % do
    (`sd_BlockToOffset` this) =<< (this & sd_GenerateHashBlock xBlock xTree)
  % \_ -> fatal "STFSExcepts.General"

-- internal byte[] GetData()
sd_GetData :: (MonadIO m) => Meth m STFSDescriptor (List Word8)
sd_GetData = undefined
-- byte idx = 1;
-- if (ThisType == STFSType.Type1)
--     idx = (byte)(TopRecord.Index << 1);
-- // Returns the Descriptor in a data fashion
-- List<byte> xReturn = new List<byte>();
-- xReturn.AddRange(new byte[] { 0x24, 0 });
-- xReturn.Add(idx);
-- xReturn.AddRange(xStruct);
-- xReturn.AddRange(new byte[20]);
-- xReturn.AddRange(BitConv.GetBytes(xBlockCount, true));
-- xReturn.AddRange(BitConv.GetBytes(TopRecord.BlocksFree, true));
-- return xReturn.ToArray();

---------
-- Create.cs
---------

data CItemEntry = CItemEntry
  { ci_create    :: Nullable CreateSTFS
  , ci_xthispath :: Nullable String
  }

newCItemEntry :: (MonadIO m) => Maybe String -> Maybe CreateSTFS -> m CItemEntry
newCItemEntry path xCreate = do
  ci_xthispath <- var path
  ci_create <- var xCreate
  return CItemEntry{..}

-- internal string getparentpath()
ci_getparentpath :: (MonadIO m) => Meth m CItemEntry String
ci_getparentpath this = do
  str <- get (ci_xthispath this) >>= noNull
  return $ case lastIndexOf '/' str of
    Nothing -> ""
    Just i  -> map toLower $ take i str

ci_Name_get :: (MonadIO m) => Meth m CItemEntry (Maybe String)
ci_Name_get this = fmap (Just . xExtractName) $ get (ci_xthispath this) >>= noNull

ci_Name_set :: (MonadIO m) => Maybe String -> Meth m CItemEntry ()
ci_Name_set value this = do
  v <- noNull value
  _ <- isValidXboxName v
  let v' = take 0x28 v
  xtp <- get (ci_xthispath this) >>= noNull
  ci_xthispath this $= case lastIndexOf '/' xtp of
    Nothing -> Just v'
    Just i  -> Just $ take i xtp ++ "/" ++ v'

-- public static uint BlockCount(string file)
createTools_BlockCount :: (MonadIO m) => Maybe FilePath -> StackTraceT m Word32
createTools_BlockCount mfile = do
  file <- noNull mfile
  stackIO $ Dir.doesFileExist file >>= \case
    False -> return constants_STFSEnd
    True -> do
      len <- (fromIntegral :: Integer -> Int64) <$> withBinaryFile file ReadMode hFileSize
      return $ fromIntegral $ quot (len - 1) 0x1000 + 1

data CFileEntry = CFileEntry
  { cfi_base       :: CItemEntry
  , cfi_filelocale :: Nullable String
  }

cfi_BlockCount :: (MonadIO m) => CFileEntry -> StackTraceT m Word32
cfi_BlockCount this = get (cfi_filelocale this) >>= createTools_BlockCount

-- public int GetLength() { return (int)new FileInfo(filelocale).Length; }
cfi_GetLength :: (MonadIO m) => Meth m CFileEntry Int32
cfi_GetLength = undefined

newCFileEntry :: (MonadIO m) => Maybe String -> Maybe String -> Maybe CreateSTFS -> m CFileEntry
newCFileEntry xFile path xCreate = do
  cfi_base <- newCItemEntry path xCreate
  cfi_filelocale <- var xFile
  return CFileEntry{..}

data CFolderEntry = CFolderEntry
  { cfo_base :: CItemEntry
  }

newCFolderEntry :: (MonadIO m) => Maybe String -> Maybe CreateSTFS -> m CFolderEntry
newCFolderEntry path xCreate = CFolderEntry <$> newCItemEntry path xCreate

-- public CFileEntry[] GetFiles()
cfo_GetFiles :: Meth m CFolderEntry (List CFileEntry)
cfo_GetFiles = undefined
-- List<CFileEntry> xReturn = new List<CFileEntry>();
-- foreach (CFileEntry x in create.xFileDirectory)
-- {
--     if (x.getparentpath() == xthispath.ToLower())
--         xReturn.Add(x);
-- }
-- return xReturn.ToArray();

data SphereColor

data DashStyle

data ThemeParams

data CreateSTFS = CreateSTFS
  { cs_xFileDirectory   :: Nullable (List CFileEntry)
  , cs_xFolderDirectory :: Nullable (List CFolderEntry)
  , cs_STFSType         :: IORef STFSType -- also xStruct
  , cs_HeaderData       :: Nullable HeaderData
  , cs_xtheme           :: Nullable ThemeParams -- also ThemeSettings
  , cs_root             :: Nullable CFolderEntry -- also RootPath
  }

-- internal uint[] BlockStep
cs_BlockStep :: (MonadIO m) => Meth m CreateSTFS [Word32]
cs_BlockStep this = do
  xStruct <- get $ cs_STFSType this
  return
    [ 0xAA, 0x70E4
    , case xStruct of
      STFSType_Type0 -> 0xFE7DA
      STFSType_Type1 -> 0xFD00B
    ]

-- internal byte GetDirectoryCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }
cs_GetDirectoryCount :: (MonadIO m) => Meth m CreateSTFS Word8
-- Subtract 1 for prevention of Modular error
cs_GetDirectoryCount this = do
  files <- get (cs_xFileDirectory this) >>= listCount
  folders <- get (cs_xFolderDirectory this) >>= listCount
  return $ fromIntegral $ quot (files + folders - 1) 0x40 + 1

-- short UppedDirectCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }
cs_UppedDirectCount :: (MonadIO m) => Meth m CreateSTFS Int16
cs_UppedDirectCount this = do
  files <- get (cs_xFileDirectory this) >>= listCount
  folders <- get (cs_xFolderDirectory this) >>= listCount
  let n = quot (files + folders - 1) 0x40 + 1
  return $ fromIntegral (fromIntegral n :: Word8)

-- internal uint TotalBlocks
cs_TotalBlocks :: (MonadIO m) => Meth m CreateSTFS Word32
cs_TotalBlocks this = do
  xReturn <- fromIntegral <$> cs_GetDirectoryCount this
  files <- get (cs_xFileDirectory this) >>= hsList
  sum . (xReturn :) <$> mapM cfi_BlockCount files

-- uint UppedTotalBlocks(uint xFileAdd) { return (uint)(UppedDirectCount + xFileAdd); }
cs_UppedTotalBlocks :: (MonadIO m) => Word32 -> Meth m CreateSTFS Word32
cs_UppedTotalBlocks xFileAdd this = (xFileAdd +) . fromIntegral <$> cs_UppedDirectCount this

initCreateSTFS :: (MonadIO m) => StackTraceT m CreateSTFS
initCreateSTFS = do
  cs_xFileDirectory   <- var [] >>= var . Just
  cs_xFolderDirectory <- var [] >>= var . Just
  cs_STFSType         <- var STFSType_Type0
  cs_HeaderData       <- undefined -- public HeaderData HeaderData = new HeaderData();
  cs_xtheme           <- undefined -- ThemeParams xtheme = new ThemeParams();
  cs_root             <- var Nothing
  return CreateSTFS{..}

-- public CreateSTFS() { root = new CFolderEntry("", this); }
newCreateSTFS :: (MonadIO m) => StackTraceT m CreateSTFS
newCreateSTFS = do
  this <- initCreateSTFS
  (cs_root this $=) . Just =<< newCFolderEntry (Just "") (Just this)
  return this

-- public bool AddFile(string FileLocation, string FilePath)
cs_AddFile :: (MonadIO m) => Maybe FilePath -> Maybe FilePath -> Meth m CreateSTFS Bool
cs_AddFile fileLocation filePath this = let
  c1 = (>= 0x3FF) <$> cs_UppedDirectCount this
  utb = createTools_BlockCount fileLocation >>= \bc -> this & cs_UppedTotalBlocks bc
  bs2 = (!! 2) <$> cs_BlockStep this
  c2 = liftA2 (>) utb bs2
  c3 = return $ null filePath
  in orM [c1, c2, c3] >>= \case
    True -> return False
    False -> do
      filePath' <- xExtractLegitPath filePath
      (this & cs_containsfile filePath') >>= \case
        True -> return False
        False -> do
          cfe <- newCFileEntry fileLocation filePath' (Just this)
          get (cs_xFileDirectory this) >>= listAdd cfe
          return True

-- public bool AddFolder(string FolderPath)
cs_AddFolder :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_AddFolder folderPath this = fun $ \ret -> do
  folderPath' <- lift $ xExtractLegitPath folderPath
  when (folderPath' == Just "") $ ret False
  idx <- lift $ lastIndexOf '/' <$> noNull folderPath'
  name <- var $ Just ""
  case idx of
    Nothing -> name $= folderPath'
    Just i -> do
      fp' <- lift $ noNull folderPath'
      name $= Just (drop (i + 1) fp')
      let parentpath = take i fp'
      b <- lift $ this & cs_containspath (Just parentpath)
      unless b $ ret False
  b <- lift $ this & cs_containspath folderPath'
  when b $ ret False
  _ <- lift $ get name >>= noNull >>= isValidXboxName
  cfe <- lift $ newCFolderEntry folderPath' (Just this)
  lift $ get (cs_xFolderDirectory this) >>= listAdd cfe
  return True

-- bool containspath(string path)
cs_containspath :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_containspath path this = do
  cfes <- get (cs_xFolderDirectory this) >>= hsList
  flip anyM cfes $ \x -> do
    xtp <- get $ ci_xthispath $ cfo_base x
    (==) <$> fmap (map toLower) (noNull xtp) <*> fmap (map toLower) (noNull path)

cs_containsfile :: (MonadIO m) => Maybe FilePath -> Meth m CreateSTFS Bool
cs_containsfile = cs_containspath

-- public bool DeleteFolder(string FolderPath)
cs_DeleteFolder :: (MonadIO m) => Maybe String -> Meth m CreateSTFS Bool
cs_DeleteFolder = undefined
{-
FolderPath = FolderPath.xExtractLegitPath();
if (!containspath(FolderPath))
    return false;
for (int i = 0; i < xFolderDirectory.Count; i++)
{
    if (xFolderDirectory[i].getparentpath() == FolderPath.ToLower())
        DeleteFolder(xFolderDirectory[i].xthispath);
}
for (int i = 0; i < xFileDirectory.Count; i++)
{
    if (xFileDirectory[i].getparentpath() == FolderPath.ToLower())
        xFileDirectory.RemoveAt(i--);
}
return true;
-}

-- public bool DeleteFile(string FilePath)
cs_DeleteFile :: (MonadIO m) => Maybe String -> Meth m CreateSTFS Bool
cs_DeleteFile = undefined
{-
FilePath = FilePath.xExtractLegitPath();
for (int i = 0; i < xFileDirectory.Count; i++)
{
    if (xFileDirectory[i].xthispath == FilePath.ToLower())
        xFileDirectory.RemoveAt(i--);
}
return true;
-}

-- public CFileEntry GetFile(string FilePath)
cs_GetFile :: (MonadIO m) => Maybe String -> Meth m CreateSTFS (Maybe CFileEntry)
cs_GetFile filePath this = fun $ \ret -> do
  fp <- lift $ xExtractLegitPath filePath >>= noNull
  files <- lift $ get (cs_xFileDirectory this) >>= hsList
  forM_ files $ \x -> do
    xptl <- lift $ get (ci_xthispath $ cfi_base x) >>= noNull
    when (map toLower xptl == map toLower fp) $ ret $ Just x
  return Nothing

-- public CFolderEntry GetFolder(string FolderPath)
cs_GetFolder :: (MonadIO m) => Maybe String -> Meth m CreateSTFS (Maybe CFolderEntry)
cs_GetFolder folderPath this = fun $ \ret -> do
  fp <- lift $ xExtractLegitPath folderPath >>= noNull
  when (fp == "") $ get (cs_root this) >>= ret
  folders <- lift $ get (cs_xFolderDirectory this) >>= hsList
  forM_ folders $ \x -> do
    xptl <- lift $ get (ci_xthispath $ cfo_base x) >>= noNull
    when (map toLower xptl == map toLower fp) $ ret $ Just x
  return Nothing

---------
-- STFSPackage.cs
---------

data ItemEntry = ItemEntry
  { ie_xPackage         :: Nullable STFSPackage
  , ie_xCreated         :: IORef Int32
  , ie_xAccessed        :: IORef Int32
  , ie_xSize            :: IORef Int32
  , ie_xBlockCount      :: IORef Word32
  , ie_xStartBlock      :: IORef Word32
  , ie_xName            :: Nullable String
  , ie_xEntryID         :: IORef Word16
  , ie_xFolderPointer   :: IORef Word16
  , ie_xFlag            :: IORef Word8
  , ie_xDirectoryOffset :: IORef Int64
  }

-- TODO rest of ItemEntry

data FileEntry = FileEntry
  { fi_base       :: ItemEntry
  , fi_xBlocks    :: Nullable (List BlockRecord)
  , fi_RealStream :: Nullable DJsIO
  }

-- TODO rest of FileEntry

data FolderEntry = FolderEntry
  { fo_base :: ItemEntry
  }

newFolderEntry_1 :: (MonadIO m) => Maybe ItemEntry -> StackTraceT m FolderEntry
newFolderEntry_1 xEntry = FolderEntry <$> undefined xEntry
-- internal FolderEntry(ItemEntry xEntry) : base(xEntry) { }

newFolderEntry_5 :: (MonadIO m) =>
  Maybe String -> Int32 -> Word16 -> Word16 -> Maybe STFSPackage -> StackTraceT m FolderEntry
newFolderEntry_5 nameIn sizeIn xID xFolder xPackageIn =
  FolderEntry <$> undefined nameIn sizeIn True xID xFolder xPackageIn
-- internal FolderEntry(string NameIn, int SizeIn, ushort xID, ushort xFolder, STFSPackage xPackageIn)
--     : base(NameIn, SizeIn, true, xID, xFolder, xPackageIn) { }

-- TODO rest of FolderEntry

data STFSLicense = STFSLicense
  { sl_xID    :: IORef Int64
  , sl_xInt1  :: IORef Int32
  , sl_xInt2  :: IORef Int32
  , sl_xfirst :: IORef Bool
  }

-- TODO rest of STFSLicense

data HeaderData = HeaderData
  -- TODO rest of the fields
  { hd_TitleID            :: IORef Word32
  , hd_Publisher          :: IORef String
  , hd_Title_Package      :: IORef String
  , hd_ThisType           :: IORef PackageType
  , hd_Title_Display      :: IORef String
  , hd_Description        :: IORef String
  , hd_PackageImageBinary :: Nullable B.ByteString
  , hd_ContentImageBinary :: Nullable B.ByteString
  }

-- TODO rest of HeaderData

data STFSPackage = STFSPackage
  { sp_xHeader          :: Nullable HeaderData -- also Header
  , sp_xFileDirectory   :: Nullable (List FileEntry)
  , sp_xFolderDirectory :: Nullable (List FolderEntry)
  , sp_xIO              :: Nullable DJsIO
  , sp_xSTFSStruct      :: Nullable STFSDescriptor -- also STFSStruct
  , sp_xFileBlocks      :: Nullable (List BlockRecord)
  , sp_xActive          :: IORef Bool
  , sp_xroot            :: Nullable FolderEntry -- also RootDirectory
  }

-- public bool ParseSuccess { get { return xIO != null; } }
sp_ParseSuccess :: (MonadIO m) => Meth m STFSPackage Bool
sp_ParseSuccess = fmap isJust . get . sp_xIO

-- uint xNewEntBlckCnt(uint xCount)
sp_xNewEntBlckCnt :: (MonadIO m) => Word32 -> Meth m STFSPackage Word32
sp_xNewEntBlckCnt = undefined
-- uint x = (uint)(xFileDirectory.Count + xFolderDirectory.Count + xCount);
-- if (x != 0)
--     return (uint)(((x - 1) / 0x40) + 1);
-- return 0;

-- internal uint xCurEntBlckCnt
sp_xCurEntBlckCnt :: (MonadIO m) => Meth m STFSPackage Word32
sp_xCurEntBlckCnt = undefined
-- int x = (xFileDirectory.Count + xFolderDirectory.Count);
-- if (x != 0)
--     return (uint)(((x - 1) / 0x40) + 1);
-- return 0;

-- bool xExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)

-- protected internal void AddToLog(string xInput)

-- protected internal bool ParseCheck()
sp_ParseCheck :: (MonadIO m) => Meth m STFSPackage Bool
sp_ParseCheck = undefined
-- if (xIO == null || !xIO.Accessed || !ParseSuccess)
--     throw STFSExcepts.Unsuccessful;
-- return true;

-- internal bool ActiveCheck()
sp_ActiveCheck :: (MonadIO m) => Meth m STFSPackage Bool
sp_ActiveCheck this = sp_ParseCheck this >>= \case
  False -> return False
  True -> get (sp_xActive this) >>= \case
    True -> return False
    False -> do
      sp_xActive this $= True
      return True

-- internal bool GetBlocks(uint xCount, uint xStartBlock, out BlockRecord[] xOutBlocks)

-- internal bool XTakeHash(long xRead, long xWrite, int xSize)
sp_XTakeHash_3 :: (MonadIO m) => Int64 -> Int64 -> Int32 -> Meth m STFSPackage Bool
sp_XTakeHash_3 xRead xWrite xSize this = do
  xIO <- get $ sp_xIO this
  this & sp_XTakeHash_5 xIO xRead xWrite xSize xIO

-- bool XTakeHash(long xRead, long xWrite, int xSize, ref DJsIO io)
sp_XTakeHash_4 :: (MonadIO m) => Int64 -> Int64 -> Int32 -> Maybe DJsIO -> Meth m STFSPackage Bool
sp_XTakeHash_4 xRead xWrite xSize io = sp_XTakeHash_5 io xRead xWrite xSize io

-- bool XTakeHash(ref DJsIO ioin, long xRead, long xWrite, int xSize, ref DJsIO ioout)
sp_XTakeHash_5 :: (MonadIO m) =>
  Maybe DJsIO -> Int64 -> Int64 -> Int32 -> Maybe DJsIO -> Meth m STFSPackage Bool
sp_XTakeHash_5 _ioin _xRead _xWrite _xSize _ioout _this = catchError
  % do
    -- ioin.Position = xRead;
    -- byte[] xData = ioin.ReadBytes(xSize);
    -- ioout.Position = xWrite;
    -- ioout.Write(SHA1Quick.ComputeHash(xData));
    -- return true;
    undefined
  % \_ -> return False

-- bool XVerifyHash(long xRead, int xSize, ref byte[] xHash)

-- bool xEntriesToFile(out DJsIO xFile)

-- internal bool xWriteTo(ref DJsIO xIOIn, BlockRecord[] xBlocks)

-- internal bool xDoAdd(ref DJsIO xIOIn, ref BlockRecord[] xEntAlloc, ref BlockRecord[] xFileAlloc)

-- bool xWriteDescriptor(ref DJsIO io)
sp_xWriteDescriptor :: (SendMessage m, MonadIO m) => Maybe DJsIO -> Meth m STFSPackage Bool
sp_xWriteDescriptor _io _this = do
  lg "Writing new Descriptor"
  -- io.Position = 0x379;
  -- xSTFSStruct.xDirectoryBlockCount = (ushort)xCurEntBlckCnt;
  -- io.Write(xSTFSStruct.GetData());
  -- io.Flush();
  _ <- undefined
  return True

-- internal long GenerateDataOffset(uint xBlock)
sp_GenerateDataOffset :: (MonadIO m) => Word32 -> Meth m STFSPackage Int64
sp_GenerateDataOffset xBlock this =
  get (sp_xSTFSStruct this) >>= noNull >>= sd_GenerateDataOffset xBlock

-- internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
sp_GenerateHashOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSPackage Int64
sp_GenerateHashOffset = undefined
-- long xReturn = xSTFSStruct.GenerateHashOffset(xBlock, xTree);
-- if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
--     xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
-- return xReturn;

-- internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
sp_GenerateBaseOffset :: (MonadIO m) => Word32 -> TreeLevel -> Meth m STFSPackage Int64
sp_GenerateBaseOffset = undefined
-- long xReturn = xSTFSStruct.GenerateBaseOffset(xBlock, xTree);
-- if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
--     xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
-- return xReturn;

-- Verified VerifySignature(bool xDev)

-- void SetSamePackage(ref STFSPackage xIn)

-- bool xWriteTables()
sp_xWriteTables :: (SendMessage m, MonadIO m) => Meth m STFSPackage Bool
sp_xWriteTables this = do
  lg "Fixing Level 0"
  ssbc <- get (sp_xSTFSStruct this) >>= noNull >>= get . sd_xBlockCount
  forM_ [0 .. ssbc - 1] $ \i -> do
    join $ sp_XTakeHash_4
      <$> sp_GenerateDataOffset i this
      <*> sp_GenerateHashOffset i L0 this
      <*> return 0x1000
      <*> get (sp_xIO this)
      <*> return this
  {-
  if (STFSStruct.BlockCount > Constants.BlockLevel[0])
  {
      AddToLog("Fixing Level 1");
      // Get level 1 count
      uint ct = (((xSTFSStruct.BlockCount - 1) / Constants.BlockLevel[0]) + 1);
      for (uint i = 0; i < ct; i++)
      {
          XTakeHash(GenerateBaseOffset(i * Constants.BlockLevel[0], TreeLevel.L0),
          GenerateHashOffset(i * Constants.BlockLevel[0], TreeLevel.L1),
              0x1000, ref xIO);
      }
      if (STFSStruct.BlockCount > Constants.BlockLevel[1])
      {
          AddToLog("Fixing Level 2");
          ct = (((xSTFSStruct.BlockCount - 1) / Constants.BlockLevel[1]) + 1);
          for (uint i = 0; i < ct; i++)
          {
              XTakeHash(GenerateHashOffset((i * Constants.BlockLevel[1]), TreeLevel.L1),
                  GenerateHashOffset((i * Constants.BlockLevel[1]), TreeLevel.L2),
                  0x1000, ref xIO);
          }
      }
  }
  xIO.Flush();
  -}
  return True

sp_xWriteHeader :: (SendMessage m, MonadIO m) => Maybe RSAParams -> Meth m STFSPackage Bool
sp_xWriteHeader = undefined
{-
internal bool xWriteHeader(RSAParams xParams)
{
    if (!xParams.Valid)
        throw CryptoExcepts.ParamError;
    // Writes, hashes, and signs data to a temp file
    AddToLog("Writing Header values");
    DJsIO x = new DJsIO(true);
    if (!x.Accessed)
        return false;
    if (!xHeader.Write(ref x))
    {
        x.Close();
        return false;
    }
    xHeader.SetSize(xIO.Length - xSTFSStruct.BaseBlock);
    x.Position = 0x340;
    if (xSTFSStruct.ThisType == STFSType.Type0)
        x.Write((int)0xAD0E);
    else x.Write((int)0x971A);
    // Fills to bottom of header
    x.Position = x.Length;
    x.Write(new byte[(0x8E6 + (xSTFSStruct.BaseBlock - 0xA000))]);
    x.Position = 0x379;
    xWriteDescriptor(ref x);
    AddToLog("Writing Master hash");
    long xLocale = 0;
    if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[0])
        xLocale = GenerateBaseOffset(0, TreeLevel.L0);
    else if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[1])
        xLocale = GenerateBaseOffset(0, TreeLevel.L1);
    else xLocale = GenerateBaseOffset(0, TreeLevel.L2);
    XTakeHash(ref xIO, xLocale, 0x381, 0x1000, ref x);
    AddToLog("Writing Header hash");
    int xSize = 0;
    if (xSTFSStruct.BaseBlock == 0xA000)
        xSize = 0x9CBC;
    else xSize = 0xACBC; // b000
    XTakeHash(0x344, 0x32C, xSize, ref x);
    AddToLog("Signing Header");
    x.Position = 0x22C;
    byte[] xHash = SHA1Quick.ComputeHash(x.ReadBytes(0x118));
    x.Position = 4;
    if (xParams.Type == PackageMagic.CON)
    {
        x.Write(xParams.Certificate);
        x.Write(ScrambleMethods.StockScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash), true));
    }
    else
    {
        x.Write(ScrambleMethods.DevScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash)));
        x.Write(new byte[0x128]);
    }
    x.IsBigEndian = true;
    x.Position = 0;
    x.Write(((uint)xParams.Type));
    x.Flush();
    xHeader.xMagic = xParams.Type;
    // Writes header to Package just incase of a emergency close, the Package still attains original strucure
    AddToLog("Writing Header to Package");
    xIO.Position = 0;
    xIO.Write(x.ReadStream());
    xIO.Flush();
    // Flush all the unused blocks to say they are written and now perm wif the new STFS Descriptor
    /*switched0.Clear();
    switched1.Clear();
    switched2 = false;*/
    x.Close();
    VariousFunctions.DeleteFile(x.FileNameLong);
    return true;
}
-}

-- internal string GetFolderNameByID(ushort ID)

-- internal FolderEntry xGetFolder(ushort ID)

-- internal FolderEntry xGetParentFolder(string Path)

-- internal FileEntry xGetFile(string Name, ushort FolderPointer)

-- bool xAddFile(DJsIO xIOIn, string xFileName, ushort Folder)

-- internal int xDeleteEntry(ItemEntry x)

-- int sortpathct(CFolderEntry x1, CFolderEntry x2)

-- string dlcname()

data SwitchType
  = SwitchType_None
  | SwitchType_Allocate
  | SwitchType_Delete
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- void SwitchNWrite(BlockRecord RecIn, SwitchType Change)
sp_SwitchNWrite :: (MonadIO m) => Maybe BlockRecord -> SwitchType -> Meth m STFSPackage ()
sp_SwitchNWrite = undefined

-- BlockRecord GetRecord(uint xBlock, TreeLevel xLevel)

sp_xWriteChain :: (MonadIO m) => Maybe (List BlockRecord) -> Meth m STFSPackage Bool
sp_xWriteChain xRecs this = do
  recs <- noNull xRecs >>= get
  forM_ (zip [0..] recs) $ \(i, rec) -> do
    if (i + 1 < length recs)
      then (br_NextBlock rec $=) =<< get (br_ThisBlock $ recs !! (i + 1))
      else br_NextBlock rec $= constants_STFSEnd
    this & sp_SwitchNWrite (Just rec) SwitchType_Allocate
  return True

-- internal bool xDeleteChain(BlockRecord[] xBlocks)

-- internal BlockRecord[] xAllocateBlocks(uint count, uint xStart)

initSTFSPackage :: (MonadIO m) => StackTraceT m STFSPackage
initSTFSPackage = do
  sp_xHeader          <- var Nothing
  sp_xFileDirectory   <- var [] >>= var . Just
  sp_xFolderDirectory <- var [] >>= var . Just
  sp_xIO              <- var Nothing
  sp_xSTFSStruct      <- var Nothing
  sp_xFileBlocks      <- var Nothing
  sp_xActive          <- var False
  sp_xroot            <- var Nothing
  return STFSPackage{..}

newSTFSPackage_load :: (SendMessage m, MonadIO m) =>
  FilePath -> StackTraceT m STFSPackage
newSTFSPackage_load = undefined

-- public STFSPackage(CreateSTFS xSession, RSAParams xSigning, string xOutPath, LogRecord LogIn)
newSTFSPackage_create :: (SendMessage m, MonadIO m) =>
  CreateSTFS -> RSAParams -> FilePath -> StackTraceT m STFSPackage
newSTFSPackage_create xSession xSigning _xOutPath = do
  this <- initSTFSPackage
  sp_xActive this $= True
  unlessM (get $ rp_xV xSigning) $
    fatal "CryptoExcepts.ParamError"
  whenM ((== 0) <$> (get (cs_xFileDirectory xSession) >>= listCount)) $
    fatal "new Exception()"
  catchError
    % do
      lg "Setting Package variables"
      -- ignoring threading stuff
      (sp_xroot this $=) . Just =<< newFolderEntry_5 (Just "") 0 0xFFFF 0xFFFF (Just this)
      -- ignoring ThematicSkin/Game stuff
      (sp_xHeader this $=) =<< get (cs_HeaderData xSession)
      get (cs_STFSType xSession) >>= \stype -> newSTFSDescriptor_type_uint stype 0 >>= (sp_xSTFSStruct this $=) . Just
      (sp_xIO this $=) . Just =<< newDJsIO_temp True
      (sp_xFileBlocks this $=) . Just =<< var =<< do
        gdc <- cs_GetDirectoryCount xSession
        forM [0 .. fromIntegral gdc - 1] $ \i -> do
          br <- newBlockRecord_empty
          br_ThisBlock br $= i
          return br
      void $ get (sp_xFileBlocks this) >>= \xfb -> this & sp_xWriteChain xfb
      {-
      xSTFSStruct.xDirectoryBlockCount = (ushort)xFileBlocks.Length;
      ushort xCurID = 0;
      xSession.xFolderDirectory.Sort(new Comparison<CFolderEntry>(sortpathct));
      foreach (CFolderEntry x in xSession.xFolderDirectory)
      {
          ushort pointer = 0xFFFF;
          if (x.xthispath.xPathCount() > 1)
              pointer = xGetParentFolder(x.Path).EntryID;
          xFolderDirectory.Add(new FolderEntry(x.Name, 0, xCurID++, pointer, this));
          xFolderDirectory[xFolderDirectory.Count - 1].xFixOffset();
      }
      foreach (CFileEntry x in xSession.xFileDirectory)
      {
          ushort pointer = 0xFFFF;
          if (x.xthispath.xPathCount() > 1)
              pointer = xGetParentFolder(x.Path).EntryID;
          xFileDirectory.Add(new FileEntry(x.Name, (int)x.GetLength(), false,xCurID++, pointer, this));
          List<BlockRecord> xAlloc = new List<BlockRecord>();
          for (uint i = 0; i < x.BlockCount(); i++)
          {
              xAlloc.Add(new BlockRecord());
              xAlloc[xAlloc.Count - 1].ThisBlock = xcurblock++;
              /*if (!switched0.Contains((int)(xcurblock / Constants.BlockLevel[0])))
                  switched0.Add((int)(xcurblock / Constants.BlockLevel[0]));
              if (!switched1.Contains((int)(xcurblock / Constants.BlockLevel[1])))
                  switched1.Add((int)(xcurblock / Constants.BlockLevel[1]));*/
          }
          xFileDirectory[xFileDirectory.Count - 1].xBlockCount = (uint)xAlloc.Count;
          xFileDirectory[xFileDirectory.Count - 1].xStartBlock = xAlloc[0].ThisBlock;
          xFileDirectory[xFileDirectory.Count - 1].xPackage = this;
          xFileDirectory[xFileDirectory.Count - 1].xFixOffset();
          xWriteChain(xAlloc.ToArray());
      }
      AddToLog("Writing Entry Table");
      DJsIO xent;
      if (!xEntriesToFile(out xent))
          throw new Exception();
      xWriteTo(ref xent, xFileBlocks);
      xent.Close();
      VariousFunctions.DeleteFile(xent.FileNameLong);
      AddToLog("Writing Files");
      uint curblck = xSession.GetDirectoryCount;
      foreach (CFileEntry z in xSession.xFileDirectory)
      {
          List<BlockRecord> w = new List<BlockRecord>();
          uint ct = z.BlockCount();
          for (uint y = 0; y < ct; y++)
          {
              w.Add(new BlockRecord());
              w[w.Count - 1].ThisBlock = curblck++;
          }
          DJsIO x = null;
          try
          {
              x = new DJsIO(z.FileLocale, DJFileMode.Open, true);
              xWriteTo(ref x, w.ToArray());
          }
          catch { }
          if (x != null)
              x.Dispose();
      }
      xWriteTables();
      xWriteHeader(xSigning);
      xIO.Close();
      VariousFunctions.MoveFile(xIO.FileNameLong, xOutPath);
      xIO = new DJsIO(xOutPath, DJFileMode.Open, true);
      xActive = false;
      -}
      void undefined
    % \err -> do
      sp_xFileDirectory this $= Nothing
      sp_xFolderDirectory this $= Nothing
      _ <- get (sp_xIO this) >>= noNull >>= dj_Dispose False
      throwError err
  return this

-- public bool UpdateHeader(RSAParams xParams)

-- public bool FlushPackage(RSAParams xParams)
sp_FlushPackage :: (SendMessage m, MonadIO m) => Maybe RSAParams -> Meth m STFSPackage Bool
sp_FlushPackage xParams this = sp_ActiveCheck this >>= \case
  False -> return False
  True -> catchError
    % do
      xsucceeded <- andM [this & sp_xWriteTables, this & sp_xWriteHeader xParams]
      sp_xActive this $= False
      return xsucceeded
    % \err -> do
      sp_xActive this $= False
      throwError err

-- public Verified[] VerifyHashTables()

-- public Verified[] VerifyHeader()

-- public bool AddFolder(string FolderPath)

-- public bool ExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)

-- public bool ExtractPayload(bool xIncludeSubItems, string xDescription, bool xIncludeHeader)

-- public FileEntry GetFile(string Path)
-- public FileEntry GetFile(string Name, ushort FolderPointer)

-- public FolderEntry GetFolder(ushort FolderID)
-- public FolderEntry GetFolder(string Path)

-- public FileEntry[] GetFiles(ushort FolderPointer)
-- public FileEntry[] GetFiles(string FolderPath)

-- public bool MakeFile(string Name, DJsIO xIOIn, ushort FolderID, AddType xType)
-- public bool MakeFile(string Path, DJsIO xIOIn, AddType xType)

-- public bool MakeBackup(string xOutLocation)

-- public bool RebuildPackage(RSAParams xParams)

-- public string GetCurrentDLCFileName()

-- public string FileNameLong { get { return xIO.FileNameLong; }}
-- public string FileNameShort { get { return xIO.FileNameShort; }}
-- public string FilePath { get { return xIO.FilePath; }}
-- public string FileExtension { get { return xIO.FileExtension; }}

-- public bool CloseIO()
sp_closeIO :: (MonadIO m) => Meth m STFSPackage Bool
sp_closeIO this = get (sp_xActive this) >>= \case
  True -> return False
  False -> do
    sp_xActive this $= True
    get (sp_xIO this) >>= \case
      Nothing -> return ()
      Just xIO -> void $ dj_Close xIO
    return True

---------
-- STFSStuff.cs
---------

data AddType
  = AddType_NoOverWrite
  | AddType_Inject
  | AddType_Replace
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Languages
  = English
  | Japanese
  | German
  | French
  | Spanish
  | Italian
  | Korean
  | Chinese
  | Portuguese
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data HashStatus
  = HashStatus_Unused
  | HashStatus_Old
  | HashStatus_New
  | HashStatus_Reused
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data HashFlag
  = Unallocated
  | AllocatedFree
  | AllocatedInUseOld
  | AllocatedInUseCurrent
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data STFSType = STFSType_Type0 | STFSType_Type1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data PackageType
  = PT_None -- ^ No package type
  | PT_SavedGame -- ^ Game save
  | PT_MarketPlace -- ^ Market place item
  | PT_Publisher -- ^ Unknown
  | PT_IPTV_DVR -- ^ IPTV DVR
  | PT_Xbox360Title -- ^ Unknown
  | PT_IPTV_PauseBuffer -- ^ IPTV Buffer
  | PT_XNACommunity -- ^ XNA Game?
  | PT_HDDInstalledGame -- ^ Hard drive installed game
  | PT_OriginalXboxGame -- ^ Original game
  | PT_SocialTitle -- ^ Unknown
  | PT_GamesOnDemand -- ^ Games on demand
  | PT_SystemPacks -- ^ Unknown
  | PT_AvatarItem -- ^ Avatar item
  | PT_Profile -- ^ Xbox 360 title
  | PT_GamerPicture -- ^ Xbox profile gamerpictures
  | PT_ThematicSkin -- ^ Xbox theme skin
  | PT_Cache -- ^ System cache?
  | PT_StorageDownload -- ^ Unknown
  | PT_XboxSavedGame -- ^ Unknown
  | PT_XboxDownload -- ^ Unknown
  | PT_GameDemo -- ^ Game Demo
  | PT_Video -- ^ Video
  | PT_GameTitle -- ^ Unknown
  | PT_Installer -- ^ Unknown
  | PT_GameTrailer -- ^ Game trailer
  | PT_Arcade
  | PT_XNA -- ^ XNA Launcher?
  | PT_LicenseStore -- ^ Xbox Licenses
  | PT_Movie -- ^ Marketplace movie
  | PT_TV -- ^ Marketplace TV show
  | PT_MusicVideo -- ^ Marketplace Music Video
  | PT_GameVideo -- ^ Unknown
  | PT_PodcastVideo -- ^ Podcast video
  | PT_ViralVideo -- ^ Unknown
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

packageTypeValue :: PackageType -> Word32
packageTypeValue = \case
  PT_None -> 0
  PT_SavedGame -> 1
  PT_MarketPlace -> 2
  PT_Publisher -> 3
  PT_IPTV_DVR -> 0xFFD
  PT_Xbox360Title -> 0x1000
  PT_IPTV_PauseBuffer -> 0x2000
  PT_XNACommunity -> 0x3000
  PT_HDDInstalledGame -> 0x4000
  PT_OriginalXboxGame -> 0x5000
  PT_SocialTitle -> 0x6000
  PT_GamesOnDemand -> 0x7000
  PT_SystemPacks -> 0x8000
  PT_AvatarItem -> 0x9000
  PT_Profile -> 0x10000
  PT_GamerPicture -> 0x20000
  PT_ThematicSkin -> 0x30000
  PT_Cache -> 0x40000
  PT_StorageDownload -> 0x50000
  PT_XboxSavedGame -> 0x60000
  PT_XboxDownload -> 0x70000
  PT_GameDemo -> 0x80000
  PT_Video -> 0x90000
  PT_GameTitle -> 0xA0000
  PT_Installer -> 0xB0000
  PT_GameTrailer -> 0xC0000
  PT_Arcade -> 0xD0000
  PT_XNA -> 0xE0000
  PT_LicenseStore -> 0xF0000
  PT_Movie -> 0x100000
  PT_TV -> 0x200000
  PT_MusicVideo -> 0x300000
  PT_GameVideo -> 0x400000
  PT_PodcastVideo -> 0x500000
  PT_ViralVideo -> 0x600000

data PackageMagic
  = PackageMagic_CON
  | PackageMagic_LIVE
  | PackageMagic_PIRS
  | PackageMagic_Unknown
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

packageMagicValue :: PackageMagic -> Word32
packageMagicValue = \case
  PackageMagic_CON -> 0x434F4E20
  PackageMagic_LIVE -> 0x4C495645
  PackageMagic_PIRS -> 0x50495253
  PackageMagic_Unknown -> 0xFFFFFFF

data TreeLevel = L0 | L1 | L2 | LT
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- not translating STFSExcepts (just using strings)

data StrongSigned
  = StrongSigned_LIVE
  | StrongSigned_PIRS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data RSAParams = RSAParams
  { rp_xC :: Nullable (List Word8)
  , rp_xK :: () -- RSAParameters xK = new RSAParameters();
  , rp_xM :: IORef PackageMagic
  , rp_xV :: IORef Bool -- also Valid
  }

-- TODO rest of RSAParams

-- TODO Verified

data ItemType
  = ItemType_Data -- ^ Data block hash
  | ItemType_TableTree0 -- ^ Hash table level 0 hash
  | ItemType_TableTree1 -- ^ Hash table level 1 hash
  | ItemType_Master -- ^ Master hash
  | ItemType_Header -- ^ Header hash
  | ItemType_Signature -- ^ Data Digest RSA Signature
  | ItemType_Certificate -- ^ Certificate Digest RSA Signature
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data TransferLock
  = TransferLock_NoTransfer
  | TransferLock_ProfileAllowOnly
  | TransferLock_DeviceAllowOnly
  | TransferLock_AllowTransfer
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

---------
-- Other.cs (only needed parts)
---------

constants_STFSEnd :: Word32
constants_STFSEnd = 0xFFFFFF

constants_BlockLevel0, constants_BlockLevel1 :: Word32
constants_BlockLevel0 = 0xAA
constants_BlockLevel1 = 0x70E4

-- internal static string xExtractLegitPath(this string xin)
xExtractLegitPath_pure :: FilePath -> FilePath
xExtractLegitPath_pure "" = ""
xExtractLegitPath_pure xin = let
  xin1 = map (\case '\\' -> '/'; c -> c) xin
  xin2 = case xin1 of
    '/' : t -> t
    _       -> xin1
  xin3 = case reverse xin2 of
    '/' : t -> reverse t
    _       -> xin2
  in xin3

xExtractLegitPath :: (Monad m) => Maybe FilePath -> StackTraceT m (Maybe FilePath)
xExtractLegitPath ms = Just . xExtractLegitPath_pure <$> noNull ms

-- internal static string xExtractName(this string xin)
xExtractName :: String -> String
xExtractName = reverse . takeWhile (/= '/') . reverse

-- internal static int xPathCount(this string xin) { return xin.Split(new char[] { '/' }).Length; }
xPathCount :: String -> Int
xPathCount = length . splitOn "/"

-- public static bool IsValidXboxName(this string x)
isValidXboxName :: (Monad m) => String -> StackTraceT m Bool
isValidXboxName x = do
  let no = map toEnum $ concat
        [ 0 ... 0x20
        -- char 0x20 - 0x2D usable symbols except 0x22 and 0x2A
        , [0x22] -- '"'
        , [0x2A] -- '*'
        , [0x2F] -- '/'
        -- char 0x30 - 0x39 are '0' - '9'
        , [0x3A] -- ':'
        -- char 0x3B and 0x3D are usable
        , [0x3C] -- '<'
        , 0x3E ... 0x40 -- unusable
        -- 0x41 - 0x5A are A - Z, usable symbols up thru 0x60 except 0x5C
        , [0x5C] -- '\'
        -- 0x61 - 0x7A are a - z, 0x7B, 0x7D, and 0x7E are usable
        , [0x7C] -- '|'
        , 0x7F ... 0xFF
        , [0xFF]
        ]
      a ... b = takeWhile (< b) [a..]
  when (x == "" || any (`elem` no) x) $ fatal "STFSExcepts.InvalChars"
  return True

-- public static DateTime FatTimeDT(int xDateTime)
fatTimeDT :: (MonadIO m) => Int32 -> m Time.LocalTime
fatTimeDT xDateTime = let
  -- MT: these are Int16 (short) in C# but that doesn't make sense
  xDate = fromIntegral $ xDateTime `shiftR` 0x10 :: Word16
  xTime = fromIntegral $ xDateTime .&. 0xFFFF :: Word16
  in if xDate == 0 && xTime == 0
    then liftIO $ fmap Time.zonedTimeToLocalTime $ Time.getZonedTime
    else return $ let
      year   = fromIntegral $ ((xDate .&. 0xFE00) `shiftR` 9) + 0x7BC
      month  = fromIntegral $ (xDate .&. 0x1E0) `shiftR` 5
      day    = fromIntegral $ xDate .&. 0x1F
      hour   = fromIntegral $ (xTime .&. 0xF800) `shiftR` 0xB
      minute = fromIntegral $ (xTime .&. 0x7E0) `shiftR` 5
      sec    = fromIntegral $ (xTime .&. 0x1F) * 2
      in Time.LocalTime (Time.fromGregorian year month day) (Time.TimeOfDay hour minute sec)
