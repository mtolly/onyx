{- |
WIP port of (parts of) X360, a GPL C# library by DJ Shepherd
-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module STFS.Create where

import           Control.Applicative            (liftA2)
import           Control.Monad.Extra            (anyM, orM, unless, unlessM,
                                                 when, whenM)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.StackTrace (SendMessage, StackTraceT,
                                                 fatal, stackIO)
import qualified Data.ByteString                as B
import           Data.Char                      (toLower)
import           Data.Function                  ((&))
import           Data.Int                       (Int16, Int32, Int64)
import           Data.IORef                     (IORef, newIORef)
import           Data.Maybe                     (isJust)
import           Data.StateVar                  (get, ($=))
import           Data.Word                      (Word32, Word8)
import qualified System.Directory               as Dir
import           System.IO                      (IOMode (..), hFileSize,
                                                 withBinaryFile)

var :: (MonadIO m) => a -> m (IORef a)
var = liftIO . newIORef

type Nullable a = IORef (Maybe a)

type List a = IORef [a]

data CreateSTFS = CreateSTFS
  { cs_HeaderData       :: IORef HeaderData
  , cs_STFSType         :: IORef STFSType
  , cs_xFileDirectory   :: IORef (List CFileEntry)
  , cs_xFolderDirectory :: IORef (List CFolderEntry)
  }

data CItemEntry = CItemEntry
  { ci_create    :: IORef CreateSTFS
  , ci_xthispath :: IORef String
  }

newCItemEntry :: (MonadIO m) => String -> CreateSTFS -> m CItemEntry
newCItemEntry path xCreate = do
  ci_xthispath <- var path
  ci_create <- var xCreate
  return CItemEntry{..}

data CFileEntry = CFileEntry
  { cfi_base       :: CItemEntry
  , cfi_filelocale :: IORef String
  }

newCFileEntry :: (MonadIO m) => String -> String -> CreateSTFS -> m CFileEntry
newCFileEntry xFile path xCreate = do
  cfi_base <- newCItemEntry path xCreate
  cfi_filelocale <- var xFile
  return CFileEntry{..}

cfi_BlockCount :: (MonadIO m) => CFileEntry -> StackTraceT m Word32
cfi_BlockCount this = get (cfi_filelocale this) >>= createTools_BlockCount

data CFolderEntry = CFolderEntry
  { cfo_base :: CItemEntry
  }

newCFolderEntry :: (MonadIO m) => String -> CreateSTFS -> m CFolderEntry
newCFolderEntry path xCreate = CFolderEntry <$> newCItemEntry path xCreate

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

data HeaderData = HeaderData
  { hd_TitleID            :: IORef Word32
  , hd_Publisher          :: IORef String
  , hd_Title_Package      :: IORef String
  , hd_ThisType           :: IORef PackageType
  , hd_Title_Display      :: IORef String
  , hd_Description        :: IORef String
  , hd_PackageImageBinary :: Nullable B.ByteString
  , hd_ContentImageBinary :: Nullable B.ByteString
  }

data STFSPackage = STFSPackage
  { sp_xActive     :: IORef Bool
  , sp_xIO         :: Nullable DJsIO
  , sp_xSTFSStruct :: Nullable STFSDescriptor
  }

data STFSDescriptor

data RSAParams = RSAParams
  { rp_Valid :: IORef Bool
  }

data DJsIO

listAdd :: (MonadIO m) => a -> List a -> m ()
listAdd x lst = do
  xs <- get lst
  lst $= (xs ++ [x])

addFile :: (MonadIO m) => FilePath -> FilePath -> CreateSTFS -> StackTraceT m Bool
addFile fileLocation filePath this = let
  c1 = (>= 0x3FF) <$> uppedDirectCount this
  utb = createTools_BlockCount fileLocation >>= \bc -> this & uppedTotalBlocks bc
  bs2 = (!! 2) <$> blockStep this
  c2 = liftA2 (>) utb bs2
  c3 = return $ null filePath
  in orM [c1, c2, c3] >>= \case
    True -> return False
    False -> do
      let filePath' = xExtractLegitPath filePath
      (this & containsfile filePath') >>= \case
        True -> return False
        False -> do
          cfe <- newCFileEntry fileLocation filePath' this
          get (cs_xFileDirectory this) >>= listAdd cfe
          return True
{-
public bool AddFile(string FileLocation, string FilePath)
{
    if (UppedDirectCount >= 0x3FF ||
        UppedTotalBlocks(CreateTools.BlockCount(FileLocation)) > BlockStep[2] ||
        FilePath == null || FilePath == "")
        return false;
    FilePath = FilePath.xExtractLegitPath();
    if (containsfile(FilePath))
        return false;
    xFileDirectory.Add(new CFileEntry(FileLocation, FilePath, this));
    return true;
}
-}

containsfile :: (MonadIO m) => FilePath -> CreateSTFS -> StackTraceT m Bool
containsfile = containspath

xExtractLegitPath :: FilePath -> FilePath
xExtractLegitPath "" = ""
xExtractLegitPath xin = let
  xin1 = map (\case '\\' -> '/'; c -> c) xin
  xin2 = case xin1 of
    '/' : t -> t
    _       -> xin1
  xin3 = case reverse xin2 of
    '/' : t -> reverse t
    _       -> xin2
  in xin3
{-
internal static string xExtractLegitPath(this string xin)
{
    if (xin == null || xin == "")
        return "";
    xin = xin.Replace('\\', '/');
    if (xin[0] == '/')
        xin = xin.Substring(1, xin.Length - 1);
    if (xin[xin.Length - 1] == '/')
        xin = xin.Substring(0, xin.Length - 1);
    return xin;
}
-}

constants_STFSEnd :: Word32
constants_STFSEnd = 0xFFFFFF

blockStep :: (MonadIO m) => CreateSTFS -> StackTraceT m [Word32]
blockStep this = do
  xStruct <- get $ cs_STFSType this
  return
    [ 0xAA, 0x70E4
    , case xStruct of
      STFSType_Type0 -> 0xFE7DA
      STFSType_Type1 -> 0xFD00B
    ]
{-
uint[] xBckStp = { 0xAA, 0x70E4, 0 };
internal uint[] BlockStep
{
    get
    {
        xBckStp[2] = (uint)((xStruct == STFSType.Type0) ? 0xFE7DA : 0xFD00B);
        return xBckStp;
    }
}
-}

createTools_BlockCount :: (MonadIO m) => FilePath -> StackTraceT m Word32
createTools_BlockCount file = stackIO $ do
  Dir.doesFileExist file >>= \case
    False -> return constants_STFSEnd
    True -> do
      len <- (fromIntegral :: Integer -> Int64) <$> withBinaryFile file ReadMode hFileSize
      return $ fromIntegral $ quot (len - 1) 0x1000 + 1
{-
public static uint BlockCount(string file)
{
    if (!File.Exists(file))
        return Constants.STFSEnd;
    long len = new FileInfo(file).Length;
    return (uint)(((len - 1) / 0x1000) + 1);
}
-}

uppedTotalBlocks :: (MonadIO m) => Word32 -> CreateSTFS -> StackTraceT m Word32
uppedTotalBlocks xFileAdd this = (xFileAdd +) . fromIntegral <$> uppedDirectCount this
-- uint UppedTotalBlocks(uint xFileAdd) { return (uint)(UppedDirectCount + xFileAdd); }

listCount :: (MonadIO m) => List a -> m Int32
listCount = fmap (fromIntegral . length) . get

uppedDirectCount :: (MonadIO m) => CreateSTFS -> StackTraceT m Int16
uppedDirectCount this = do
  files <- get (cs_xFileDirectory this) >>= listCount
  folders <- get (cs_xFolderDirectory this) >>= listCount
  let n = quot (files + folders - 1) 0x40 + 1
  return $ fromIntegral (fromIntegral n :: Word8)
-- short UppedDirectCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }

fun :: (Monad m) => ((a -> ContT a (StackTraceT m) b) -> ContT a (StackTraceT m) a) -> StackTraceT m a
fun = evalContT . callCC

lastIndexOf :: (Eq a) => a -> [a] -> Maybe Int
lastIndexOf x xs = lookup x $ reverse $ zip xs [0..]

addFolder :: (MonadIO m) => FilePath -> CreateSTFS -> StackTraceT m Bool
addFolder folderPath this = fun $ \ret -> do
  let folderPath' = xExtractLegitPath folderPath
  when (folderPath' == "") $ ret False
  let idx = folderPath' & lastIndexOf '/'
  name <- var ""
  case idx of
    Nothing -> name $= folderPath'
    Just i -> do
      name $= drop (i + 1) folderPath'
      let parentpath = take i folderPath'
      b <- lift $ this & containspath parentpath
      unless b $ ret False
  b <- lift $ this & containspath folderPath'
  when b $ ret False
  _ <- lift $ get name >>= isValidXboxName
  cfe <- lift $ newCFolderEntry folderPath' this
  lift $ get (cs_xFolderDirectory this) >>= listAdd cfe
  return True
{-
public bool AddFolder(string FolderPath)
{
    if (FolderPath == null)
        return false;
    FolderPath = FolderPath.xExtractLegitPath();
    if (FolderPath == "")
        return false;
    int idx = FolderPath.LastIndexOf('/');
    string name = "";
    if (idx == -1)
        name = FolderPath;
    else
    {
        name = FolderPath.Substring(idx + 1, FolderPath.Length - 1 - idx);
        string parentpath = FolderPath.Substring(0, idx);
        if (!containspath(parentpath))
            return false;
    }
    if (containspath(FolderPath))
        return false;
    name.IsValidXboxName();
    xFolderDirectory.Add(new CFolderEntry(FolderPath, this));
    return true;
}
-}

containspath :: (MonadIO m) => String -> CreateSTFS -> StackTraceT m Bool
containspath path this = do
  cfes <- get (cs_xFolderDirectory this) >>= get
  flip anyM cfes $ \x -> do
    xtp <- get $ ci_xthispath $ cfo_base x
    return $ map toLower xtp == map toLower path
{-
bool containspath(string path)
{
    foreach (CFolderEntry x in xFolderDirectory)
    {
        if (x.xthispath.ToLower() == path.ToLower())
            return true;
    }
    return false;
}
-}

newSTFSPackage :: (SendMessage m, MonadIO m) =>
  CreateSTFS -> RSAParams -> FilePath -> StackTraceT m STFSPackage
newSTFSPackage xSession xSigning _xOutPath = do
  this <- undefined
  sp_xActive this $= True
  unlessM (get $ rp_Valid xSigning) $
    fatal "CryptoExcepts.ParamError"
  whenM ((== 0) <$> (get (cs_xFileDirectory xSession) >>= listCount)) $
    fatal "new Exception()"
  _ <- undefined
  return this
{-
public STFSPackage(CreateSTFS xSession, RSAParams xSigning, string xOutPath, LogRecord LogIn)
{
    xActive = true;
    if (!xSigning.Valid)
        throw CryptoExcepts.ParamError;
    if (xSession.xFileDirectory.Count == 0)
        throw new Exception();
    try
    {
        AddToLog("Setting Package variables");
        new System.Threading.Thread(new System.Threading.ParameterizedThreadStart(System.DLLIdentify.PrivilegeCheck)).Start(System.Threading.Thread.CurrentThread);
        xroot = new FolderEntry("", 0, 0xFFFF, 0xFFFF, this);
        if (xSession.HeaderData.ThisType == PackageType.ThematicSkin)
        {
            DJsIO x1 = new DJsIO(true);
            DJsIO x2 = new DJsIO(true);
            x1.Write((int)xSession.ThemeSettings.StyleType);
            x1.Flush();
            x1.Close();
            if (!xSession.AddFile(x1.FileNameLong, "DashStyle"))
                throw STFSExcepts.ThemeError;
            x2.Write("SphereColor=" + ((byte)xSession.ThemeSettings.Sphere).ToString().PadRight(2, '\0'));
            x2.Write(new byte[] { 0xD, 0xA });
            x2.Write("AvatarLightingDirectional=" +
                xSession.ThemeSettings.AvatarLightingDirectional0.ToString("#0.0") + "," +
                xSession.ThemeSettings.AvatarLightingDirectional1.ToString("#0.0000") + "," +
                xSession.ThemeSettings.AvatarLightingDirectional2.ToString("#0.0") + ",0x" +
                xSession.ThemeSettings.AvatarLightingDirectional3.ToString("X"));
            x2.Write(new byte[] { 0xD, 0xA });
            x2.Write("AvatarLightingAmbient=0x" + xSession.ThemeSettings.AvatarLightingAmbient.ToString("X"));
            x2.Write(new byte[] { 0xD, 0xA });
            x2.Flush();
            x2.Close();
            if (!xSession.AddFile(x2.FileNameLong, "parameters.ini"))
                throw STFSExcepts.ThemeError;
        }
        else if (xSession.HeaderData.ThisType == PackageType.GamesOnDemand ||
            xSession.HeaderData.ThisType == PackageType.HDDInstalledGame ||
            xSession.HeaderData.ThisType == PackageType.OriginalXboxGame ||
            xSession.HeaderData.ThisType == PackageType.SocialTitle)
            throw STFSExcepts.Game;
        xLog = LogIn;
        xHeader = xSession.HeaderData;
        xSTFSStruct = new STFSDescriptor(xSession.STFSType, 0);
        xIO = new DJsIO(true);
        List<BlockRecord> DirectoryBlockz = new List<BlockRecord>();
        // switched2 = true;
        uint xcurblock = 0;
        for (ushort i = 0; i < xSession.GetDirectoryCount; i++)
        {
            DirectoryBlockz.Add(new BlockRecord());
            DirectoryBlockz[DirectoryBlockz.Count - 1].ThisBlock = xcurblock++;
            /*if (!switched0.Contains((int)(xcurblock / Constants.BlockLevel[0])))
                switched0.Add((int)(xcurblock / Constants.BlockLevel[0]));
            if (!switched1.Contains((int)(xcurblock / Constants.BlockLevel[1])))
                switched1.Add((int)(xcurblock / Constants.BlockLevel[1]));*/
        }
        xFileBlocks = DirectoryBlockz.ToArray();
        xWriteChain(xFileBlocks);
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
    }
    catch (Exception x) { xFileDirectory = null; xFolderDirectory = null; xIO.Dispose(); throw x; }
}
-}

flushPackage :: (MonadIO m) => RSAParams -> STFSPackage -> StackTraceT m ()
flushPackage = undefined
{-
public bool FlushPackage(RSAParams xParams)
{
    if (!ActiveCheck())
        return false;
    try
    {
        bool xsucceeded = (xWriteTables() && xWriteHeader(xParams));
        xActive = false;
        return xsucceeded;
    }
    catch (Exception x) { xActive = false; throw x; }
}
-}

xWriteTables :: (SendMessage m, MonadIO m) => STFSPackage -> StackTraceT m Bool
xWriteTables = undefined
{-
bool xWriteTables()
{
    AddToLog("Fixing Level 0");
    for (uint i = 0; i < STFSStruct.BlockCount; i++)
    {
        XTakeHash(GenerateDataOffset(i),
            GenerateHashOffset(i, TreeLevel.L0),
            0x1000, ref xIO);
    }
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
    return true;
}
-}

xWriteHeader :: (SendMessage m, MonadIO m) => RSAParams -> STFSPackage -> StackTraceT m Bool
xWriteHeader = undefined
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

activeCheck :: (MonadIO m) => STFSPackage -> StackTraceT m Bool
activeCheck this = parseCheck this >>= \case
  False -> return False
  True -> get (sp_xActive this) >>= \case
    True -> return False
    False -> do
      sp_xActive this $= True
      return True
{-
internal bool ActiveCheck()
{
    if (!ParseCheck())
        return false;
    if (xActive)
        return false;
    return (xActive = true);
}
-}

parseCheck :: (MonadIO m) => STFSPackage -> StackTraceT m Bool
parseCheck = undefined
{-
protected internal bool ParseCheck()
{
    if (xIO == null || !xIO.Accessed || !ParseSuccess)
        throw STFSExcepts.Unsuccessful;
    return true;
}
-}

parseSuccess :: (MonadIO m) => STFSPackage -> StackTraceT m Bool
parseSuccess = fmap isJust . get . sp_xIO
-- public bool ParseSuccess { get { return xIO != null; } }

closeIO :: (MonadIO m) => STFSPackage -> StackTraceT m ()
closeIO = undefined
{-
public bool CloseIO()
{
    if (xActive)
        return false;
    xActive = true;
    if (xIO != null)
        xIO.Close();
    return true;
}
-}

loadSTFSPackage :: (SendMessage m, MonadIO m) =>
  FilePath -> StackTraceT m STFSPackage
loadSTFSPackage = undefined

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
{-
public static bool IsValidXboxName(this string x)
{
    if (x != null && x != "")
    {
        List<char> no = new List<char>();
        for (byte i = 0; i < 0x20; i++)
            no.Add((char)i);
        // char 0x20 - 0x2D usable symbols except 0x22 and 0x2A
        no.Add((char)0x22); // '"'
        no.Add((char)0x2A); // '*'
        no.Add((char)0x2F); // '/'
        // char 0x30 - 0x39 are '0' - '9'
        no.Add((char)0x3A); // ':'
        // char 0x3B and 0x3D are usable
        no.Add((char)0x3C); // '<'
        for (byte i = 0x3E; i < 0x40; i++)
            no.Add((char)i); // unusuable
        // 0x41 - 0x5A are A - Z, usable symbols up thru 0x60 except 0x5C
        no.Add((char)0x5C); // '\'
        // 0x61 - 0x7A are a - z, 0x7B, 0x7D, and 0x7E are usable
        no.Add((char)0x7C); // '|'
        for (byte i = 0x7F; i < 0xFF; i++)
            no.Add((char)i);
        no.Add((char)0xFF);
        if (x.IndexOfAny(no.ToArray()) == -1)
            return true;
    }
    throw STFSExcepts.InvalChars;
}
-}

xExtractName :: String -> String
xExtractName = reverse . takeWhile (/= '/') . reverse
{-
internal static string xExtractName(this string xin)
{
    int idx = xin.LastIndexOf('/');
    if (idx == -1)
        return xin;
    return xin.Substring(idx + 1, xin.Length - idx - 1);
}
-}
