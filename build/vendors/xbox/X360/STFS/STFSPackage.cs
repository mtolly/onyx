// NOTE This class is protected under GPL License as well as terms and conditions.
/* */ // Most notably, you must not obfuscate/protect this code, you must include an open source
/* */ // to your project that uses this code, and you must also not make profit on it.
/* */ // For more details, access:
// *http://www.gnu.org/
// *License included in the library source
// *License located at X360.PublicResources.GPL30
// *X360.XAbout.GNUProtected for GNU and TaC (Terms and Conditions)
/* */ // You agree to these terms when you use this code.
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using System.Security.Cryptography;
using System.Runtime.CompilerServices;
using System.Drawing;
using System.Threading;
using System.ComponentModel;
using X360.IO;
using X360.Profile;
using X360.Other;
using X360.Security.Cryptography;
using X360.IO.STFSExtensions;

namespace X360.STFS
{
    /// <summary>
    /// Base class for STFS items
    /// </summary>
    public class ItemEntry
    {
        #region Variables
        [CompilerGenerated]
        internal STFSPackage xPackage;
        [CompilerGenerated]
        internal int xCreated;
        [CompilerGenerated]
        internal int xAccessed;
        [CompilerGenerated]
        internal int xSize;
        [CompilerGenerated]
        internal uint xBlockCount;
        [CompilerGenerated]
        internal uint xStartBlock;
        [CompilerGenerated]
        string xName;
        [CompilerGenerated]
        internal ushort xEntryID = 0;
        [CompilerGenerated]
        internal ushort xFolderPointer = 0xFFFF;
        [CompilerGenerated]
        byte xFlag = 0;
        [CompilerGenerated]
        long xDirectoryOffset;
        #endregion

        /// <summary>
        /// Time Created
        /// </summary>
        public DateTime Created { get { return TimeStamps.FatTimeDT(xCreated); } }
        /// <summary>
        /// Entry size
        /// </summary>
        public int Size { get { return xSize; } }
        /// <summary>
        /// Time last accessed
        /// </summary>
        public DateTime Accessed { get { return TimeStamps.FatTimeDT(xAccessed); } }
        /// <summary>
        /// Unknown flag
        /// </summary>
        public bool UnknownFlag
        {
            get { return ((xFlag >> 6) & 1) == 1; }
            set
            {
                if (value)
                    xFlag = (byte)(((FolderFlag ? 1 : 0) << 7) | (1 << 6) | (int)xNameLength);
                else xFlag = (byte)(((FolderFlag ? 1 : 0) << 7) | (0 << 6) | (int)xNameLength);
            }
        }
        byte xNameLength
        {
            get { return (byte)(xFlag & 0x3F); }
            set
            {
                if (value < 0)
                    value = 0;
                else if (value > 0x28)
                    value = 0x28;
                xFlag = (byte)(((FolderFlag ? 1 : 0) << 7) | ((UnknownFlag ? 1 : 0) << 6) | (int)value);
            }
        }
        /// <summary>
        /// Parent directory pointer
        /// </summary>
        public ushort FolderPointer { get { return xFolderPointer; } }
        /// <summary>
        /// Deleted flag
        /// </summary>
        public bool IsDeleted { get { return xNameLength == 0; } }
        /// <summary>
        /// Offset in the package
        /// </summary>
        public long DirectoryOffset { get { return xDirectoryOffset; }}
        /// <summary>
        /// Is a folder
        /// </summary>
        public bool FolderFlag { get { return ((xFlag >> 7) & 1) == 1; }}
        /// <summary>
        /// Entry name
        /// </summary>
        public string Name
        {
            get { return xName; }
            set
            {
                value.IsValidXboxName();
                if (value.Length >= 0x28)
                    value = value.Substring(0, 0x28);
                xName = value;
                xNameLength = (byte)value.Length;
            }
        }
        /// <summary>
        /// Entry ID
        /// </summary>
        public ushort EntryID { get { return xEntryID; } }
        /// <summary>
        /// Start Blocm
        /// </summary>
        public uint StartBlock { get { return xStartBlock; } }
        /// <summary>
        /// Block Count
        /// </summary>
        public uint BlockCount { get { return xBlockCount; } }

        internal ItemEntry(byte[] xDataIn, long DirectOffset, ushort xID, STFSPackage xPackageIn)
        {
            try
            {
                xPackage = xPackageIn;
                DJsIO xFileIO = new DJsIO(xDataIn, true);
                xFileIO.Position = 0;
                xEntryID = xID;
                xFileIO.Position = 0x28;
                xFlag = xFileIO.ReadByte();
                if (xNameLength > 0x28)
                    xNameLength = 0x28;
                xFileIO.Position = 0;
                if (xNameLength == 0)
                    return;
                xName = xFileIO.ReadString(StringForm.ASCII, xNameLength);
                xName.IsValidXboxName();
                xFileIO.Position = 0x2F;
                xStartBlock = xFileIO.ReadUInt24(false);
                xFolderPointer = xFileIO.ReadUInt16();
                xSize = xFileIO.ReadInt32();
                xBlockCount = (uint)(((xSize - 1) / 0x1000) + 1);
                xCreated = xFileIO.ReadInt32();
                xAccessed = xFileIO.ReadInt32();
                xDirectoryOffset = DirectOffset;
            }
            catch { xNameLength = 0; }
        }

        internal ItemEntry(string NameIn, int SizeIn, bool xIsFolder, ushort xID, ushort xFolder, STFSPackage xPackageIn)
        {
            xPackage = xPackageIn;
            xEntryID = xID;
            xFolderPointer = xFolder;
            if (NameIn.Length >= 0x28)
                xName = NameIn.Substring(0, 0x28);
            else xName = NameIn;
            xFlag = (byte)(((xIsFolder ? 1 : 0) << 7) | xName.Length);
            DateTime x = DateTime.Now;
            xCreated = TimeStamps.FatTimeInt(x);
            xAccessed = xCreated;
            if (xIsFolder)
            {
                xSize = 0;
                xStartBlock = 0;
                xBlockCount = 0;
            }
            else
            {
                xSize = SizeIn;
                if (xSize != 0)
                    xBlockCount = (uint)(((xSize - 1) / 0x1000) + 1);
            }
        }

        internal ItemEntry(ItemEntry x)
        {
            xName = x.xName;
            xAccessed = x.xAccessed;
            xCreated = x.xCreated;
            xBlockCount = x.xBlockCount;
            xDirectoryOffset = x.xDirectoryOffset;
            xFlag = x.xFlag;
            xEntryID = x.xEntryID;
            xFolderPointer = x.xFolderPointer;
            xSize = x.xSize;
            xStartBlock = x.xStartBlock;
            xFlag = (byte)((x.FolderFlag ? 1 : 0) << 7 | (x.UnknownFlag ? 1 : 0) << 6 | xName.Length);
            xPackage = x.xPackage;
        }

        internal void xFixOffset() { xDirectoryOffset = xPackage.STFSStruct.GenerateDataOffset(xPackage.xFileBlocks[xEntryID / 0x40].ThisBlock) + ((0x40 * xEntryID) % 0x40); }

        int DelFold(ushort foldID)
        {
            for (int i = 0; i < xPackage.xFileDirectory.Count; i++)
            {
                if (xPackage.xFileDirectory[i].FolderPointer == foldID)
                {
                    xPackage.xFileDirectory[i].xNameLength = 0;
                    xPackage.xFileDirectory.RemoveAt(i--);
                }
            }
            for (int i = 0; i < xPackage.xFolderDirectory.Count; i++)
            {
                FolderEntry x = xPackage.xFolderDirectory[i];
                if (x.FolderPointer == foldID)
                    i = DelFold(x.EntryID);
            }
            return xPackage.xDeleteEntry(this);
        }

        /// <summary>
        /// Deletes entry
        /// </summary>
        /// <returns></returns>
        public bool Delete()
        {
            if (!xPackage.ActiveCheck())
                return false;
            try
            {
                if (FolderFlag)
                    DelFold(EntryID);
                else
                {
                    xNameLength = 0;
                    xPackage.xDeleteEntry(this);
                }
                return !(xPackage.xActive = false);
            }
            catch { return (xPackage.xActive = false); }
        }

        /// <summary>
        /// Grabs the binary data representation
        /// </summary>
        /// <returns></returns>
        public byte[] GetEntryData()
        {
            try
            {
                List<byte> xReturn = new List<byte>();
                xReturn.AddRange(Encoding.ASCII.GetBytes(xName.ToCharArray()));
                xReturn.AddRange(new byte[0x28 - xName.Length]);
                xReturn.Add(xFlag);
                List<byte> xbuff = new List<byte>();
                xbuff.AddRange(BitConv.GetBytes(xBlockCount, false));
                xbuff.RemoveAt(3);
                xReturn.AddRange(xbuff);
                xReturn.AddRange(xbuff);
                xbuff.Clear();
                xbuff.AddRange(BitConv.GetBytes(xStartBlock, false));
                xbuff.RemoveAt(3);
                xReturn.AddRange(xbuff);
                xbuff.Clear();
                xbuff = null;
                xReturn.AddRange(BitConv.GetBytes(xFolderPointer, true));
                xReturn.AddRange(BitConv.GetBytes(xSize, true));
                xReturn.AddRange(BitConv.GetBytes(xCreated, false));
                xReturn.AddRange(BitConv.GetBytes(xAccessed, false));
                return xReturn.ToArray();
            }
            catch { return new byte[0]; }
        }

        /// <summary>
        /// Writes the binary data
        /// </summary>
        /// <returns></returns>
        public bool WriteEntry()
        {
            if (!xPackage.ActiveCheck())
                return false;
            try
            {
                xPackage.xIO.Position = xDirectoryOffset;
                xPackage.xIO.Write(GetEntryData());
                xPackage.xIO.Flush();
                return !(xPackage.xActive = false);
            }
            catch { return (xPackage.xActive = false); }
        }

        /// <summary>
        /// Grabsthe path
        /// </summary>
        /// <returns></returns>
        public string GetPath()
        {
            string xReturn = "";
            try
            {
                xReturn = Name;
                ushort currfold = xFolderPointer;
                while (currfold != 0xFFFF)
                {
                    ItemEntry xAbove = xPackage.xGetFolder(currfold);
                    if (xAbove != null)
                    {
                        xReturn = xAbove.Name + "/" + xReturn;
                        currfold = xAbove.xFolderPointer;
                    }
                    else return null;
                }
                return xReturn;
            }
            catch (Exception x) { throw x; }
        }
    }

    /// <summary>
    /// Object for STFS File Entry
    /// </summary>
    public sealed class FileEntry : ItemEntry
    {
        internal FileEntry(ItemEntry xEntry) : base(xEntry) { }
        
        internal FileEntry(string NameIn, int SizeIn, bool xIsFolder, ushort xID, ushort xFolder, STFSPackage xPackageIn)
            : base(NameIn, SizeIn, xIsFolder, xID, xFolder, xPackageIn) { }

        [CompilerGenerated]
        internal BlockRecord[] xBlocks;
        [CompilerGenerated]
        internal DJsIO RealStream = null;

        internal bool Opened { get { return xBlocks != null && xBlocks.Length > 0; }}

        internal bool ReadBlocks()
        {
            try
            {
                if (RealStream != null)
                    return false;
                xPackage.GetBlocks(xBlockCount, xStartBlock, out xBlocks);
                if (xBlocks.Length < xBlockCount)
                    ClearBlocks();
                return Opened;
            }
            catch { return false; }
        }

        internal bool ClearBlocks()
        {
            try
            {
                if (RealStream != null)
                    return false;
                xBlocks = null;
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Fixes the hashes of the file
        /// </summary>
        /// <param name="SignTypeOrNull"></param>
        /// <returns></returns>
        public bool FixHashes(RSAParams SignTypeOrNull)
        {
            if (SignTypeOrNull != null && !SignTypeOrNull.Valid)
                throw CryptoExcepts.ParamError;
            if (!xPackage.ActiveCheck())
                return false;
            if (!Opened && !ReadBlocks())
                return (xPackage.xActive = false);
            try
            {
                // Fixes each hash associated with each block for each Level
                List<uint> x1 = new List<uint>();
                foreach (BlockRecord x in xBlocks)
                {
                    xPackage.AddToLog("Fixing Level 0");
                    xPackage.XTakeHash(xPackage.GenerateDataOffset(x.ThisBlock), xPackage.GenerateHashOffset(x.ThisBlock, TreeLevel.L0), 0x1000);
                    if (!x1.Contains(x.ThisBlock / Constants.BlockLevel[0]))
                        x1.Add(x.ThisBlock / Constants.BlockLevel[0]);
                }
                if (xPackage.xSTFSStruct.BlockCount >= Constants.BlockLevel[0])
                {
                    xPackage.AddToLog("Fixing Level 1");
                    List<uint> x2 = new List<uint>();
                    foreach (uint x in x1)
                    {
                        xPackage.XTakeHash(xPackage.GenerateBaseOffset((x * Constants.BlockLevel[0]), TreeLevel.L0),
                            xPackage.GenerateHashOffset((x * Constants.BlockLevel[0]), TreeLevel.L1), 0x1000);
                        if (!x2.Contains(x / Constants.BlockLevel[0]))
                            x2.Add(x / Constants.BlockLevel[0]);
                    }
                    if (xPackage.xSTFSStruct.BlockCount > 0x70E4)
                    {
                        xPackage.AddToLog("Fixing Level 2");
                        foreach (uint x in x2)
                            xPackage.XTakeHash(xPackage.GenerateBaseOffset((x * Constants.BlockLevel[1]), TreeLevel.L1),
                                xPackage.GenerateHashOffset((x * Constants.BlockLevel[1]), TreeLevel.L2), 0x1000);
                    }    
                }
                if (SignTypeOrNull != null)
                    xPackage.xWriteHeader(SignTypeOrNull);
                ClearBlocks();
                return !(xPackage.xActive = false);
            }
            catch 
            {
                ClearBlocks();
                return (xPackage.xActive = false);
            }
        }

        /// <summary>
        /// Replace the file via IO
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <returns></returns>
        public bool Replace(DJsIO xIOIn)
        {
            if (!xPackage.ActiveCheck())
                return false;
            return (xReplace(xIOIn) & !(xPackage.xActive = false));
        }

        /// <summary>
        /// Replace the file via File Location
        /// </summary>
        /// <param name="FileIn"></param>
        /// <returns></returns>
        public bool Replace(string FileIn)
        {
            DJsIO xIO = null;
            try
            {
                xIO = new DJsIO(FileIn, DJFileMode.Open, true);
                bool Success = Replace(xIO);
                xIO.Dispose();
                return Success;
            }
            catch
            {
                if (xIO != null)
                    xIO.Dispose();
                return xPackage.xActive = false;
            }
        }

        internal bool xReplace(DJsIO xIOin)
        {
            if (!Opened)
                ReadBlocks();
            if (!xIOin.Accessed || xIOin.Length > 0xFFFFFFFF)
                return false;
            try
            {
                // Allocates new blocks for new data
                xPackage.AddToLog("Allocating blocks");
                BlockRecord[] xEntAlloc = xPackage.xAllocateBlocks(xPackage.xCurEntBlckCnt, 0);
                BlockRecord[] xFileAlloc = xPackage.xAllocateBlocks(xIOin.BlockCountSTFS(), xEntAlloc[xEntAlloc.Length - 1].ThisBlock + 1);
                // Updates entry
                xPackage.AddToLog("Updating entry");
                xStartBlock = xFileAlloc[0].ThisBlock;
                xSize = (int)xIOin.Length;
                xBlockCount = xIOin.BlockCountSTFS();
                if (xPackage.xDoAdd(ref xIOin, ref xEntAlloc, ref xFileAlloc))
                {
                    xPackage.xDeleteChain(xBlocks);
                    ClearBlocks();
                    return true;
                }
                return false;
            }
            catch { ClearBlocks(); return false; }
        }

        internal bool xExtract(DJsIO xIOOut)
        {
            if (!Opened && !ReadBlocks())
                return false;
            try
            {
                // Gets data and writes it
                xIOOut.Position = 0;
                for (uint i = 0; i < xBlockCount; i++)
                {
                    xPackage.xIO.Position = xPackage.GenerateDataOffset(xBlocks[i].ThisBlock);
                    if (i < (xBlockCount - 1))
                        xIOOut.Write(xPackage.xIO.ReadBytes(0x1000));
                    else xIOOut.Write(xPackage.xIO.ReadBytes((((Size - 1) % 0x1000) + 1)));
                }
                xIOOut.Flush();
                ClearBlocks();
                return true;
            }
            catch
            {
                ClearBlocks();
                return false;
            }
        }

        /// <summary>
        /// Extracts the entry via user selection
        /// </summary>
        /// <param name="DialogTitle"></param>
        /// <param name="DialogFilter"></param>
        /// <returns></returns>
        public bool Extract(string DialogTitle, string DialogFilter)
        {
            string FileOut = VariousFunctions.GetUserFileLocale(DialogTitle, DialogFilter, false);
            if (FileOut == null)
                return false;
            return Extract(FileOut);
        }

        /// <summary>
        /// Extracts entry to a location
        /// </summary>
        /// <param name="FileOut"></param>
        /// <returns></returns>
        public bool Extract(string FileOut)
        {
            if (!xPackage.ActiveCheck())
                return false;
            bool xReturn = false;
            DJsIO xIO = new DJsIO(true);
            try
            {
                xReturn = xExtract(xIO);
                xIO.Close();
                if (xReturn)
                {
                    if (!VariousFunctions.MoveFile(xIO.FileNameLong, FileOut))
                        throw new Exception();
                }
            }
            catch
            {
                xReturn = false;
                xIO.Close();
            }
            VariousFunctions.DeleteFile(xIO.FileNameLong);
            xPackage.xActive = false;
            return xReturn;
        }

        internal bool xInject(DJsIO xIOin)
        {
            if (!Opened && !ReadBlocks())
                return false;
            try
            {
                if (xIOin.Length > 0xFFFFFFFF)
                    return false;
                uint y = xIOin.BlockCountSTFS();
                List<BlockRecord> x = xBlocks.ToList();
                List<BlockRecord> xdel = null;
                if (y > xBlocks.Length)// Allocates data for blocks needed
                    x.AddRange(xPackage.xAllocateBlocks((uint)(y - xBlocks.Length), 0)); 
                else
                {
                    xdel = new List<BlockRecord>();
                    for (int i = (int)y; i < x.Count; i++)
                    {
                        xdel.Add(x[i]);
                        x.RemoveAt(i--);
                    }
                }
                bool success = xPackage.xWriteTo(ref xIOin, x.ToArray());
                if (success)
                {
                    xBlocks = x.ToArray();
                    if (xdel != null && xdel.Count != 0)
                        xPackage.xDeleteChain(xdel.ToArray());
                }
                return (success & ClearBlocks());
            }
            catch { ClearBlocks(); return false; }
        }

        /// <summary>
        /// Overwrites the file data from an IO
        /// </summary>
        /// <param name="xIOin"></param>
        /// <returns></returns>
        public bool Inject(DJsIO xIOin)
        {
            if (!xPackage.ActiveCheck())
                return false;
            return (xInject(xIOin) & !(xPackage.xActive = false));
        }

        /// <summary>
        /// Overwrites the data from a file location
        /// </summary>
        /// <param name="FileIn"></param>
        /// <returns></returns>
        public bool Inject(string FileIn)
        {
            DJsIO xIO = null;
            try
            {
                xIO = new DJsIO(FileIn, DJFileMode.Open, true);
                bool Success = Inject(xIO);
                xIO.Dispose();
                return Success;
            }
            catch
            {
                if (xIO != null)
                    xIO.Dispose();
                return xPackage.xActive = false;
            }
        }

        /// <summary>
        /// Returns a real time STFS file stream
        /// </summary>
        /// <param name="MakeCopy"></param>
        /// <param name="BigEndian"></param>
        /// <returns></returns>
        public DJsIO GrabSTFSStream(bool MakeCopy, bool BigEndian)
        {
            try
            {
                if (RealStream != null)
                    return RealStream;
                if (!xPackage.ActiveCheck())
                    return null;
                if (MakeCopy)
                {
                    DJsIO xtemp = new DJsIO(true);
                    if (!xExtract(xtemp))
                    {
                        xtemp.Close();
                        VariousFunctions.DeleteFile(xtemp.FileNameLong);
                        return null;
                    }
                    bool success = xReplace(xtemp);
                    xtemp.Close();
                    VariousFunctions.DeleteFile(xtemp.FileNameLong);
                    if (!success)
                        return null;
                }
                if (!Opened && !ReadBlocks())
                    return null;
                return (RealStream = new STFSStreamIO(this, BigEndian));
            }
            catch { RealStream = null; xPackage.xActive = false; return null; }
        }

        internal DJsIO xGetTempIO(bool BigEndian)
        {
            if (!Opened && !ReadBlocks())
                return null;
            DJsIO xIO = new DJsIO(BigEndian);
            if (!xExtract(xIO))
            {
                xIO.Close();
                VariousFunctions.DeleteFile(xIO.FileNameLong);
                xIO = null;
            }
            ClearBlocks();
            return xIO;
        }

        /// <summary>
        /// Extracts the file to a temporary location
        /// </summary>
        /// <param name="BigEndian"></param>
        /// <returns></returns>
        public DJsIO GetTempIO(bool BigEndian)
        {
            if (!xPackage.ActiveCheck())
                return null;
            DJsIO xReturn = xGetTempIO(BigEndian);
            xPackage.xActive = false;
            return xReturn;
        }

        internal FileEntry Copy()
        {
            FileEntry x = new FileEntry(this);
            x.xBlocks = xBlocks;
            return x;
        }
    }

    /// <summary>
    /// Class for STFS Folder items
    /// </summary>
    public sealed class FolderEntry : ItemEntry
    {
        internal FolderEntry(ItemEntry xEntry) : base(xEntry) { }

        internal FolderEntry(string NameIn, int SizeIn, ushort xID, ushort xFolder, STFSPackage xPackageIn)
            : base(NameIn, SizeIn, true, xID, xFolder, xPackageIn) { }

        internal bool folderextract(bool xInclude, string xOut)
        {
            try
            {
                if (!VariousFunctions.xCheckDirectory(xOut))
                    return false;
                foreach (FileEntry x in xPackage.xFileDirectory)
                {
                    if (x.FolderPointer != xEntryID)
                        continue;
                    DJsIO xIO = new DJsIO(VariousFunctions.xGetUnusedFile(xOut + "/" + x.Name), DJFileMode.Create, true);
                    if (xIO.Accessed)
                    {
                        x.xExtract(xIO);
                        xIO.Dispose();
                    }
                }
                foreach (FolderEntry z in xPackage.xFolderDirectory)
                {
                    if (z.FolderPointer == EntryID)
                        z.folderextract(xInclude, xOut + "/" + z.Name);
                }
                xPackage.AddToLog(Name + " Extracted");
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Extract the files
        /// </summary>
        /// <param name="xOutLocale"></param>
        /// <param name="xIncludeSubItems"></param>
        /// <returns></returns>
        public bool Extract(string xOutLocale, bool xIncludeSubItems)
        {
            if (!xPackage.ActiveCheck())
                return false;
            if (xOutLocale[xOutLocale.Length - 1] == '/' ||
                xOutLocale[xOutLocale.Length - 1] == '\\')
                xOutLocale = xOutLocale.Substring(0, xOutLocale.Length - 1);
            xPackage.AddToLog("Checking Directory");
            if (!VariousFunctions.xCheckDirectory(xOutLocale))
            {
                xPackage.AddToLog("Directory error");
                return false;
            }
            xPackage.AddToLog("Extracting payload");
            folderextract(xIncludeSubItems, xOutLocale);
            return true;
        }

        /// <summary>
        /// Extract the files
        /// </summary>
        /// <param name="xIncludeSubItems"></param>
        /// <param name="xDescription"></param>
        /// <returns></returns>
        public bool Extract(bool xIncludeSubItems, string xDescription)
        {
            if (!xPackage.ActiveCheck())
                return false;
            string y = VariousFunctions.GetUserFolderLocale("Select a save location");
            if (y != null)
                return folderextract(xIncludeSubItems, y);
            return false;
        }

        /// <summary>
        /// Grabs the subfolders
        /// </summary>
        /// <returns></returns>
        public FolderEntry[] GetSubFolders()
        {
            if (!xPackage.ActiveCheck())
                return null;
            FolderEntry[] xReturn = xGetFolders();
            xPackage.xActive = false;
            return xReturn;
        }

        internal FolderEntry[] xGetFolders()
        {
            List<FolderEntry> xReturn = new List<FolderEntry>();
            foreach (FolderEntry x in xPackage.xFolderDirectory)
            {
                if (x.FolderPointer == EntryID)
                    xReturn.Add(x);
            }
            return xReturn.ToArray();
        }

        /// <summary>
        /// Grabs the files
        /// </summary>
        /// <returns></returns>
        public FileEntry[] GetSubFiles()
        {
            if (!xPackage.ActiveCheck())
                return null;
            FileEntry[] xReturn = xGetFiles();
            xPackage.xActive = false;
            return xReturn;
        }

        internal FileEntry[] xGetFiles()
        {
            List<FileEntry> xReturn = new List<FileEntry>();
            foreach (FileEntry x in xPackage.xFileDirectory)
            {
                if (x.FolderPointer == EntryID)
                    xReturn.Add(x);
            }
            return xReturn.ToArray();
        }
    }
    
    /// <summary>
    /// Class for STFS Licenses
    /// </summary>
    public sealed class STFSLicense
    {
        [CompilerGenerated]
        internal long xID;
        [CompilerGenerated]
        internal int xInt1;
        [CompilerGenerated]
        internal int xInt2;
        [CompilerGenerated]
        bool xfirst = false;

        /// <summary>
        /// ID
        /// </summary>
        public long ID { get { return xID; } }
        /// <summary>
        /// Bits
        /// </summary>
        public int Var1 { get { return xInt1; } }
        /// <summary>
        /// Flags
        /// </summary>
        public int Flags { get { return xInt2; } }

        internal STFSLicense(long xid, int x1, int x2, bool first)
        {
            xInt1 = x1;
            xInt2 = x2;
            xID = xid;
            xfirst = first;
        }

        /// <summary>
        /// Clear all licenses
        /// </summary>
        /// <returns></returns>
        public bool Clear()
        {
            try
            {
                if (xfirst)
                    xID = -1;
                else xID = 0;
                xInt1 = 0;
                xInt2 = 0;
                return true;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Class for Header info
    /// </summary>
    public sealed class HeaderData
    {
        #region Non-Property Variables
        [CompilerGenerated]
        internal PackageMagic xMagic = PackageMagic.Unknown;
        [CompilerGenerated]
        List<STFSLicense> xLisc = new List<STFSLicense>();
        [CompilerGenerated]
        internal PackageType xThisType = PackageType.SavedGame;
        [CompilerGenerated]
        byte[] xPackageImage;
        [CompilerGenerated]
        byte[] xContentImage;
        /// <summary>
        /// Meta Data Version
        /// </summary>
        [CompilerGenerated]
        public uint MetaDataVersion = 2;
        [CompilerGenerated]
        long xContentSize;
        /// <summary>
        /// Media ID
        /// </summary>
        [CompilerGenerated]
        public uint MediaID;
        /// <summary>
        /// Version
        /// </summary>
        [CompilerGenerated]
        public uint Version_;
        /// <summary>
        /// Base Version
        /// </summary>
        [CompilerGenerated]
        public uint Version_Base;
        /// <summary>
        /// Title ID
        /// </summary>
        [CompilerGenerated]
        public uint TitleID = 0xFFFE07D1;
        /// <summary>
        /// Platform
        /// </summary>
        [CompilerGenerated]
        public byte Platform;
        /// <summary>
        /// Executable Type
        /// </summary>
        [CompilerGenerated]
        public byte ExecutableType;
        /// <summary>
        /// Disc Number
        /// </summary>
        [CompilerGenerated]
        public byte DiscNumber;
        /// <summary>
        /// Disc In Set
        /// </summary>
        [CompilerGenerated]
        public byte DiscInSet;
        /// <summary>
        /// Save Game ID
        /// </summary>
        [CompilerGenerated]
        public uint SaveGameID;
        /// <summary>
        /// Data File Count
        /// </summary>
        [CompilerGenerated]
        public uint DataFileCount;
        /// <summary>
        /// Data File Size
        /// </summary>
        [CompilerGenerated]
        public long DataFileSize;
        /// <summary>
        /// Reserved
        /// </summary>
        [CompilerGenerated]
        public long Reserved;
        [CompilerGenerated]
        byte[] xSeriesID = new byte[0x10];
        [CompilerGenerated]
        byte[] xSeasonID = new byte[0x10];
        /// <summary>
        /// Season Number
        /// </summary>
        [CompilerGenerated]
        public ushort SeasonNumber;
        /// <summary>
        /// Episode Number
        /// </summary>
        [CompilerGenerated]
        public ushort EpidsodeNumber;
        [CompilerGenerated]
        long xSaveConsoleID;
        /// <summary>
        /// Profile ID
        /// </summary>
        [CompilerGenerated]
        public long ProfileID;
        [CompilerGenerated]
        byte[] xDeviceID = new byte[20];
        [CompilerGenerated]
        string[] xTitles = new string[9];
        [CompilerGenerated]
        string[] xDescriptions = new string[9];
        [CompilerGenerated]
        string xPublisher = "";
        [CompilerGenerated]
        string xTitle = "";
        [CompilerGenerated]
        byte IDTransferByte;
        [CompilerGenerated]
        bool xLoaded = false;
        [CompilerGenerated]
        Languages xCurrent = Languages.English;
        #endregion

        #region Property Variables
        /// <summary>
        /// Signature type
        /// </summary>
        public PackageMagic Magic { get { return xMagic; } }
        /// <summary>
        /// Transfer flags
        /// </summary>
        public TransferLock IDTransfer { get { return (TransferLock)(IDTransferByte >> 6); } set { IDTransferByte = (byte)((((byte)value) & 3) << 6); } }
        /// <summary>
        /// Package type
        /// </summary>
        public PackageType ThisType { get { return xThisType; } set { xThisType = value; }}
        /// <summary>
        /// STFS Licenses
        /// </summary>
        public STFSLicense[] Liscenses { get { return xLisc.ToArray(); } }
        /// <summary>
        /// STFS Licenses
        /// </summary>
        public long RecordedContentSize { get { return xContentSize; }}
        /// <summary>
        /// Series ID
        /// </summary>
        public byte[] SeriesID { get { return xSeriesID; } set { VariousFunctions.xSetByteArray(ref xSeriesID, value); } }
        /// <summary>
        /// Season ID
        /// </summary>
        public byte[] SeasonID { get { return xSeasonID; } set { VariousFunctions.xSetByteArray(ref xSeasonID, value); } }
        /// <summary>
        /// Console ID (creator)
        /// </summary>
        public long SaveConsoleID 
        {
            get { return xSaveConsoleID; }
            set
            {
                if (value > 0xFFFFFFFFFF)
                    value = 0xFFFFFFFFFF;
                xSaveConsoleID = value;
            }
        }
        /// <summary>
        /// Device ID
        /// </summary>
        public byte[] DeviceID { get { return xDeviceID; } set { VariousFunctions.xSetByteArray(ref xDeviceID, value); } }
        /// <summary>
        /// Description
        /// </summary>
        public string Description
        {
            get { return xDescriptions[(byte)xCurrent]; }
            set
            {
                if (value.Length > 0x80)
                    value = value.Substring(0, 0x80);
                xDescriptions[(byte)xCurrent] = value;
            }
        }
        /// <summary>
        /// Display Title
        /// </summary>
        public string Title_Display
        {
            get { return xTitles[(byte)xCurrent]; }
            set
            {
                if (value.Length > 0x80)
                    value = value.Substring(0, 0x80);
                xTitles[(byte)xCurrent] = value;
            }
        }
        /// <summary>
        /// Package Title
        /// </summary>
        public string Title_Package
        {
            get { return xTitle; }
            set
            {
                if (value.Length > 0x40)
                    value = value.Substring(0, 0x40);
                xTitle = value;
            }
        }
        /// <summary>
        /// Publisher
        /// </summary>
        public string Publisher
        {
            get { return xPublisher; }
            set
            {
                if (value.Length > 0x40)
                    value = value.Substring(0, 0x40);
                xPublisher = value;
            }
        }
        /// <summary>
        /// Content image
        /// </summary>
        public Image ContentImage
        {
            get { return ContentImageBinary.BytesToImage(); }
            set
            {
                byte[] x = value.ImageToBytes(System.Drawing.Imaging.ImageFormat.Png);
                if (x.Length > 0x4000)
                    return;
                xContentImage = x;
            }
        }
        /// <summary>
        /// Package image
        /// </summary>
        public Image PackageImage
        {
            get { return PackageImageBinary.BytesToImage(); }
            set
            {
                byte[] x = value.ImageToBytes(System.Drawing.Imaging.ImageFormat.Png);
                if (x.Length > 0x4000)
                    return;
                xPackageImage = x;
            }
        }
        /// <summary>
        /// Bytes of the Package image
        /// </summary>
        public byte[] PackageImageBinary
        {
            get { return xPackageImage; }
            set
            {
                if (value.Length > 0x4000)
                    return;
                xPackageImage = value;
            }
        }
        /// <summary>
        /// Bytes of the Content image
        /// </summary>
        public byte[] ContentImageBinary
        {
            get { return xContentImage; }
            set
            {
                if (value.Length > 0x4000)
                    return;
                xContentImage = value;
            }
        }
        #endregion

        void read(DJsIO xIO, STFSPackage xPackage, PackageMagic MagicType)
        {
            xMagic = MagicType;
            xIO.Position = 0x22C;
            if (xPackage != null)
                xPackage.AddToLog("Reading Liscenses");
            xLisc = new List<STFSLicense>();
            for (int i = 0; i < 0x10; i++)
                xLisc.Add(new STFSLicense(xIO.ReadInt64(), xIO.ReadInt32(), xIO.ReadInt32(), i == 0));
            if (xPackage != null)
                xPackage.AddToLog("Reading Package locks");
            xIO.Position = 0x344;
            if (xPackage != null)
                xPackage.AddToLog("Reading Header Values");
            xThisType = (PackageType)xIO.ReadUInt32(); ;
            MetaDataVersion = xIO.ReadUInt32();
            xContentSize = xIO.ReadInt64();
            MediaID = xIO.ReadUInt32();
            Version_ = xIO.ReadUInt32();
            Version_Base = xIO.ReadUInt32();
            TitleID = xIO.ReadUInt32();
            Platform = xIO.ReadByte();
            ExecutableType = xIO.ReadByte();
            DiscNumber = xIO.ReadByte();
            DiscInSet = xIO.ReadByte();
            SaveGameID = xIO.ReadUInt32();
            SaveConsoleID = (long)xIO.ReadUInt40();
            ProfileID = xIO.ReadInt64();
            xIO.Position = 0x39D;
            DataFileCount = xIO.ReadUInt32();
            DataFileSize = xIO.ReadInt64();
            Reserved = xIO.ReadInt64();
            xSeriesID = xIO.ReadBytes(0x10);
            xSeasonID = xIO.ReadBytes(0x10);
            SeasonNumber = xIO.ReadUInt16();
            EpidsodeNumber = xIO.ReadUInt16();
            xIO.Position += 0x28;
            xDeviceID = xIO.ReadBytes(0x14);
            for (int i = 0; i < 9; i++)
                xTitles[i] = xIO.ReadString(StringForm.Unicode, 0x80).Replace("\0", "");
            for (int i = 0; i < 9; i++)
                xDescriptions[i] = xIO.ReadString(StringForm.Unicode, 0x80).Replace("\0", "");
            xPublisher = xIO.ReadString(StringForm.Unicode, 0x40).Replace("\0", "");
            xTitle = xIO.ReadString(StringForm.Unicode, 0x40).Replace("\0", "");
            IDTransferByte = xIO.ReadByte();
            // Package Image
            int xSize = xIO.ReadInt32();
            xIO.Position = 0x171A;
            if (xSize < 0x4000)
                xPackageImage = xIO.ReadBytes(xSize);
            else xPackageImage = xIO.ReadBytes(0x4000);
            // Content Image
            xIO.Position = 0x1716;
            xSize = xIO.ReadInt32();
            xIO.Position = 0x571A;
            if (xSize < 0x4000)
                xContentImage = xIO.ReadBytes(xSize);
            else xContentImage = xIO.ReadBytes(0x4000);
            xLoaded = true;
        }

        internal HeaderData(STFSPackage xPackage, PackageMagic MagicType) { read(xPackage.xIO, xPackage, MagicType); }

        /// <summary>
        /// Initializes a default object
        /// </summary>
        public HeaderData()
        {
            xLoaded = true;
            for (int i = 0; i < 9; i++)
            {
                xTitles[i] = "";
                xDescriptions[i] = "";
            }
            xLisc.Add(new STFSLicense(-1, 0, 0, true));
            for (int i = 0; i < 0xF; i++)
                xLisc.Add(new STFSLicense(0, 0, 0, false));
            IDTransfer = TransferLock.AllowTransfer;
            xPackageImage = PublicResources.NoImage.ImageToBytes(System.Drawing.Imaging.ImageFormat.Png);
            xContentImage = xPackageImage;
        }

        internal HeaderData(DJsIO xIOIn, PackageMagic MagicType) { read(xIOIn, null, MagicType); }

        internal bool Write(ref DJsIO x)
        {
            if (!xLoaded)
                return false;
            try
            {
                if (x == null || !x.Accessed)
                    return false;
                x.Position = 0x22C;
                foreach (STFSLicense b in xLisc)
                {
                    x.Write(b.ID);
                    x.Write(b.Var1);
                    x.Write(b.Flags);
                }
                x.Position = 0x344;
                x.Write((uint)xThisType);
                x.Write(MetaDataVersion);
                x.Write(xContentSize);
                x.Write(MediaID);
                x.Write(Version_);
                x.Write(Version_Base);
                x.Write(TitleID);
                x.Write(Platform);
                x.Write(ExecutableType);
                x.Write(DiscNumber);
                x.Write(DiscInSet);
                x.Write(SaveGameID);
                x.WriteUInt40((ulong)SaveConsoleID);
                x.Write(ProfileID);
                x.Position = 0x39D;
                x.Write(DataFileCount);
                x.Write(DataFileSize);
                x.Write(Reserved);
                x.Write(SeriesID);
                x.Write(SeasonID);
                x.Write(SeasonNumber);
                x.Write(EpidsodeNumber);
                x.Position += 0x28;
                x.Write(xDeviceID);
                for (int i = 0; i < 9; i++)
                    x.Write(xTitles[i], StringForm.Unicode, 0x80, PadLocale.Right, PadType.Null);
                for (int i = 0; i < 9; i++)
                    x.Write(xDescriptions[i], StringForm.Unicode, 0x80, PadLocale.Right, PadType.Null);
                x.Write(xPublisher, StringForm.Unicode, 0x40, PadLocale.Right, PadType.Null);
                x.Write(xTitle, StringForm.Unicode, 0x40, PadLocale.Right, PadType.Null);
                x.Write(IDTransferByte);
                x.Write(xPackageImage.Length);
                x.Write(xContentImage.Length);
                x.Write(xPackageImage);
                x.Write(new byte[0x4000 - xPackageImage.Length]);
                x.Write(xContentImage);
                x.Write(new byte[(0x4000 - xContentImage.Length)]);
                return true;
            }
            catch { return false; }
        }

        internal bool WriteText(ref DJsIO x)
        {
            if (!xLoaded || x == null || !x.Accessed)
                return false;
            try
            {
                x.Position = 0;
                x.Write("Signature Type - " + xMagic.ToString() + Environment.NewLine);
                x.Write("Package Licences:" + Environment.NewLine);
                for (int i = 0; i < xLisc.Count; i++)
                {
                    x.Write("License " + i.ToString() + " ID: " + xLisc[i].ID.ToString("X") + Environment.NewLine);
                    x.Write("License " + i.ToString() + " Bits: " + xLisc[i].Var1.ToString("X") + Environment.NewLine);
                    x.Write("License " + i.ToString() + " Flags: " + xLisc[i].Flags.ToString("X") + Environment.NewLine);
                }
                x.Write("Package Type: " + xThisType.ToString() + Environment.NewLine);
                x.Write("Meta Data Version: " + MetaDataVersion.ToString() + Environment.NewLine);
                x.Write("Recorded ContentSize: " + xContentSize.ToString() + " bytes" + Environment.NewLine);
                x.Write("Media ID: " + MediaID.ToString() + Environment.NewLine);
                x.Write("Version: " + Version_.ToString() + Environment.NewLine);
                x.Write("Version Base: " + Version_Base.ToString() + Environment.NewLine);
                x.Write("Title ID: " + TitleID.ToString("X2") + Environment.NewLine);
                x.Write("Platform: " + Platform.ToString() + Environment.NewLine);
                x.Write("Executable Type: " + ExecutableType.ToString() + Environment.NewLine);
                x.Write("Disc Number: " + DiscNumber.ToString() + Environment.NewLine);
                x.Write("Disc In Set: " + DiscInSet.ToString() + Environment.NewLine);
                x.Write("Save Game ID: " + SaveGameID.ToString() + Environment.NewLine);
                x.Write("Creator's Console ID: " + xSaveConsoleID.ToString("X2") + Environment.NewLine);
                x.Write("Creator's Profile ID: " + ProfileID.ToString("X2") + Environment.NewLine);
                x.Write("Data File Count: " + DataFileCount.ToString() + Environment.NewLine);
                x.Write("Data File Size: " + DataFileSize.ToString() + Environment.NewLine);
                x.Write("Series ID: " + SeriesID.HexString() + Environment.NewLine);
                x.Write("Season ID: " + SeasonID.HexString() + Environment.NewLine);
                x.Write("Season Number: " + SeasonNumber.ToString() + Environment.NewLine);
                x.Write("Epidsode Number: " + EpidsodeNumber.ToString() + Environment.NewLine);
                x.Write("Device ID: " + xDeviceID.HexString() + Environment.NewLine);
                x.Write("Languages:" + Environment.NewLine);
                for (int i = 0; i < 9; i++)
                {
                    x.Write(((Languages)i).ToString() + " Display Title and Description:" + Environment.NewLine);
                    x.Write(xTitles[i] + Environment.NewLine);
                    x.Write(xDescriptions[i] + Environment.NewLine);
                }
                x.Write("Publisher Name: " + xPublisher + Environment.NewLine);
                x.Write("Package Title: " + xTitle + Environment.NewLine);
                x.Write("Package Transfer Type: " + IDTransfer.ToString());
                x.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Set the language to grab the description/title
        /// </summary>
        /// <param name="xDesired"></param>
        public void SetLanguage(Languages xDesired)
        {
            xCurrent = xDesired;   
        }

        /// <summary>
        /// Clear all the licenses
        /// </summary>
        /// <returns></returns>
        public bool ClearLicenses()
        {
            if (!xLoaded)
                return false;
            foreach (STFSLicense x in xLisc)
                x.Clear();
            return true;
        }

        /// <summary>
        /// Clears licenses, ID's, and makes the ID's worthless
        /// </summary>
        /// <returns></returns>
        public bool MakeAnonymous()
        {
            if (!xLoaded)
                return false;
            try
            {
                xDeviceID = new byte[20];
                xSaveConsoleID = 0;
                ProfileID = 0;
                IDTransfer = TransferLock.AllowTransfer;
                ClearLicenses();
                return true;
            }
            catch { return false; }
        }

        internal void SetSize(long x)
        {
            xContentSize = x;
        }

        /// <summary>
        /// Attempts to add a license
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="Bits"></param>
        /// <param name="Flags"></param>
        /// <returns></returns>
        public bool AddLicense(long ID, int Bits, int Flags)
        {
            for (int i = 0; i < 0x10; i++)
            {
                if (xLisc[i].xInt1 == 0 && xLisc[1].xInt2 == 0)
                {
                    xLisc[i].xID = ID;
                    xLisc[i].xInt1 = Bits;
                    xLisc[i].xInt2 = Flags;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Allows a DLC to be unlocked
        /// </summary>
        /// <returns></returns>
        public bool UnlockDLC()
        {
            if (!xLoaded)
                return false;
            return (ClearLicenses() && AddLicense(-1, 0, 1));
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class STFSPackage
    {
        #region Variables
        [CompilerGenerated]
        HeaderData xHeader;
        [CompilerGenerated]
        internal List<FileEntry> xFileDirectory = new List<FileEntry>();
        [CompilerGenerated]
        internal List<FolderEntry> xFolderDirectory = new List<FolderEntry>();
        [CompilerGenerated]
        internal DJsIO xIO;
        [CompilerGenerated]
        internal STFSDescriptor xSTFSStruct;
        [CompilerGenerated]
        LogRecord xLog = null;
        [CompilerGenerated]
        internal BlockRecord[] xFileBlocks;
        [CompilerGenerated]
        internal bool xActive = false; // To protect against errors from multithreading
        [CompilerGenerated]
        FolderEntry xroot;
        // used to preserve the old table
        /*Locations of commented out table control (self future ref)
         * xwritedescriptor
         * xwriteheader
         * create package
         */
        /*[CompilerGenerated]
        List<int> switched0 = new List<int>();
        [CompilerGenerated]
        List<int> switched1 = new List<int>();
        [CompilerGenerated]
        bool switched2 = false;*/
        
        /// <summary>
        /// Package read correctly
        /// </summary>
        public bool ParseSuccess { get { return xIO != null; } }
        /// <summary>
        /// Log of library production
        /// </summary>
        public LogRecord Log { get { return xLog; }}
        bool IsLogged { get { return (xLog != null); }}
        /// <summary>
        /// The STFS struct of the package
        /// </summary>
        public STFSDescriptor STFSStruct { get { return xSTFSStruct; } }
        /// <summary>
        /// Header metadata
        /// </summary>
        public HeaderData Header { get { return xHeader; }}
        /// <summary>
        /// Root Directory of package
        /// </summary>
        public FolderEntry RootDirectory { get { return xroot; } }

        uint xNewEntBlckCnt(uint xCount)
        { 
            uint x = (uint)(xFileDirectory.Count + xFolderDirectory.Count + xCount);
            if (x != 0)
                return (uint)(((x - 1) / 0x40) + 1);
            return 0;
        }

        internal uint xCurEntBlckCnt
        {
            get
            {
                int x = (xFileDirectory.Count + xFolderDirectory.Count);
                if (x != 0)
                    return (uint)(((x - 1) / 0x40) + 1);
                return 0;
            }
        }
        #endregion

        #region Local Methods
        /// <summary>
        /// Extracts via out locale
        /// </summary>
        /// <param name="xOutLocale"></param>
        /// <param name="xIncludeSubItems"></param>
        /// <param name="xIncludeHeader"></param>
        /// <returns></returns>
        bool xExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)
        {
            try
            {
                AddToLog("Checking Directory");
                xOutLocale.Replace("\\", "/");
                if (xOutLocale[xOutLocale.Length - 1] == '/')
                    xOutLocale = xOutLocale.Substring(0, xOutLocale.Length - 1);
                if (!VariousFunctions.xCheckDirectory(xOutLocale))
                {
                    AddToLog("Directory error");
                    return false;
                }
                if (xIncludeHeader)
                {
                    // Records the meta data
                    AddToLog("Noting Header Information");
                    DJsIO xhead = new DJsIO(VariousFunctions.xGetUnusedFile(xOutLocale + "/" + dlcname() + ".txt")
                        , DJFileMode.Create, true);
                    xHeader.WriteText(ref xhead);
                    xhead.Dispose();
                    xhead = new DJsIO(VariousFunctions.xGetUnusedFile(xOutLocale + "/Content Image.png")
                        , DJFileMode.Create, true);
                    xhead.Position = 0;
                    xhead.Write(xHeader.ContentImageBinary);
                    xhead.Dispose();
                    xhead = new DJsIO(VariousFunctions.xGetUnusedFile(xOutLocale + "/Package Image.png")
                        , DJFileMode.Create, true);
                    xhead.Position = 0;
                    xhead.Write(xHeader.PackageImageBinary);
                    xhead.Dispose();
                }
                xOutLocale += "/Root";
                if (!VariousFunctions.xCheckDirectory(xOutLocale))
                    return (xActive = false);
                // Runs a regular folder extract
                AddToLog("Extracting payload");
                RootDirectory.folderextract(xIncludeSubItems, xOutLocale);
                AddToLog("Package Extracted");
                return !(xActive = false);
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Adds to log
        /// </summary>
        /// <param name="xInput"></param>
        protected internal void AddToLog(string xInput)
        {
            if (xInput == null || xInput == "")
                return;
            if (IsLogged)
            {
                Thread x = new Thread(new ParameterizedThreadStart(xLog.DoLog));
                x.Start(xInput);
            }
        }

        /// <summary>
        /// Checks to see if the package was parsed
        /// </summary>
        /// <returns></returns>
        protected internal bool ParseCheck()
        {
            if (xIO == null || !xIO.Accessed || !ParseSuccess)
                throw STFSExcepts.Unsuccessful;
            return true;
        }

        /// <summary>
        /// Checks if the package is fine
        /// </summary>
        /// <returns></returns>
        internal bool ActiveCheck()
        {
            if (!ParseCheck())
                return false;
            if (xActive)
                return false;
            return (xActive = true);
        }

        /// <summary>
        /// Returns the blocks of a file
        /// </summary>
        /// <param name="xCount"></param>
        /// <param name="xStartBlock"></param>
        /// <param name="xOutBlocks"></param>
        /// <returns></returns>
        internal bool GetBlocks(uint xCount, uint xStartBlock, out BlockRecord[] xOutBlocks)
        {
            // Follows the blocks for the specified max count
            List<BlockRecord> xBlockList = new List<BlockRecord>();
            BlockRecord xBlock = GetRecord(xStartBlock, TreeLevel.L0);
            if (xBlock.ThisBlock >= xSTFSStruct.xBlockCount)
                throw STFSExcepts.InvalBlock;
            for (uint i = 0; i < xCount; i++)
            {
                if (!xBlockList.ContainsBlock(xBlock))
                    xBlockList.Add(xBlock);
                else break; // If it contains, it's just going to loop
                if (xBlock.NextBlock == Constants.STFSEnd)
                    break; // Stop means stop
                if (xBlock.NextBlock >= xSTFSStruct.xBlockCount)
                    throw STFSExcepts.InvalBlock;
                // Gets the next block record
                xBlock = GetRecord(xBlock.NextBlock, TreeLevel.L0);
            }
            xOutBlocks = xBlockList.ToArray();
            // Success if 1 - end block is reached and 2 - count is the count of the blocks found
            return (xBlockList.Count == xCount);
        }

        /// <summary>
        /// Writes a SHA1 hash from base IO
        /// </summary>
        /// <param name="xRead"></param>
        /// <param name="xWrite"></param>
        /// <param name="xSize"></param>
        /// <returns></returns>
        internal bool XTakeHash(long xRead, long xWrite, int xSize)
        {
            try { return XTakeHash(ref xIO, xRead, xWrite, xSize, ref xIO); }
            catch { return false; }
        }

        /// <summary>
        /// Writes a SHA1 hash reading from base IO to another IO
        /// </summary>
        /// <param name="xRead"></param>
        /// <param name="xWrite"></param>
        /// <param name="xSize"></param>
        /// <param name="io"></param>
        /// <returns></returns>
        bool XTakeHash(long xRead, long xWrite, int xSize, ref DJsIO io)
        {
            try { return XTakeHash(ref io, xRead, xWrite, xSize, ref io); }
            catch { return false; }
        }

        /// <summary>
        /// Reads from one IO, hashes, stores it in another IO
        /// </summary>
        /// <param name="ioin"></param>
        /// <param name="xRead"></param>
        /// <param name="xWrite"></param>
        /// <param name="xSize"></param>
        /// <param name="ioout"></param>
        /// <returns></returns>
        bool XTakeHash(ref DJsIO ioin, long xRead, long xWrite, int xSize, ref DJsIO ioout)
        {
            try
            {
                ioin.Position = xRead;
                byte[] xData = ioin.ReadBytes(xSize);
                ioout.Position = xWrite;
                ioout.Write(SHA1Quick.ComputeHash(xData));
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Returns a bool if the corresponding offset/hash is the same
        /// </summary>
        /// <param name="xRead"></param>
        /// <param name="xSize"></param>
        /// <param name="xHash"></param>
        /// <returns></returns>
        bool XVerifyHash(long xRead, int xSize, ref byte[] xHash)
        {
            try
            {
                // Compares strings of the hashes
                xIO.Position = xRead;
                byte[] xData = xIO.ReadBytes(xSize);
                return (BitConverter.ToString(xHash) == BitConverter.ToString(SHA1Quick.ComputeHash(xData)));
            }
            catch { return false; }
        }

        /// <summary>
        /// Produces a file via entries
        /// </summary>
        /// <param name="xFile"></param>
        /// <returns></returns>
        bool xEntriesToFile(out DJsIO xFile)
        {
            xFile = null;
            try
            {
                // Not much explaination is needed, just writes entries into a file
                xFile = new DJsIO(true);
                ushort xCurEnt = 0;
                foreach (FolderEntry v in xFolderDirectory)
                {
                    if (v.IsDeleted)
                        continue;
                    xFile.Position = 0x40 * xCurEnt;
                    // Reorders the folders to current entry
                    // Note: Don't have to do this, but I think it's sexy to handle folders at top of directory
                    foreach (FolderEntry y in xFolderDirectory)
                    {
                        if (y.xFolderPointer == v.EntryID)
                            y.xFolderPointer = xCurEnt;
                    }
                    foreach (FileEntry y in xFileDirectory)
                    {
                        if (y.xFolderPointer == v.EntryID)
                            y.xFolderPointer = xCurEnt;
                    }
                    // Sets current entry
                    v.xEntryID = xCurEnt;
                    // Writes
                    xFile.Write(v.GetEntryData());
                    xCurEnt++;
                }
                for (int i = 0; i < xFolderDirectory.Count; i++)
                {
                    // Write new folder pointer
                    xFile.Position = (0x40 * i) + 0x32;
                    xFile.Write(xFolderDirectory[i].xFolderPointer);
                }
                foreach (FileEntry y in xFileDirectory)
                {
                    if (y.IsDeleted)
                        continue;
                    // Sets
                    y.xEntryID = xCurEnt;
                    xFile.Position = 0x40 * xCurEnt;
                    // Writes
                    xFile.Write(y.GetEntryData());
                    xCurEnt++;
                }
                xFile.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Writes a file via blocks
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <param name="xBlocks"></param>
        /// <returns></returns>
        internal bool xWriteTo(ref DJsIO xIOIn, BlockRecord[] xBlocks)
        {
            if (!xIOIn.Accessed || (xIOIn.BlockCountSTFS() != xBlocks.Length))
                return false;
            try
            {
                xIOIn.Position = 0;
                for (int i = 0; i < xBlocks.Length - 1; i++)
                {
                    // Finds spot and writes block of data
                    xIO.Position = GenerateDataOffset(xBlocks[i].ThisBlock);
                    xIO.Write(xIOIn.ReadBytes(0x1000));
                }
                xIO.Position = GenerateDataOffset(xBlocks[xBlocks.Length - 1].ThisBlock);
                xIO.Write(xIOIn.ReadBytes(xIOIn.BlockRemainderSTFS()));
                xIO.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Add a file to the package
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <param name="xEntAlloc"></param>
        /// <param name="xFileAlloc"></param>
        /// <returns></returns>
        internal bool xDoAdd(ref DJsIO xIOIn, ref BlockRecord[] xEntAlloc, ref BlockRecord[] xFileAlloc)
        {
            // Gets Entry Table file
            DJsIO xEntFile;
            if (!xEntriesToFile(out xEntFile))
                return (xActive = false);
            // Writes it
            AddToLog("Adding new entry table to package");
            if (!xWriteTo(ref xEntFile, xEntAlloc))
            {
                xEntFile.Close();
                VariousFunctions.DeleteFile(xEntFile.FileNameLong);
                return (xActive = false);
            }
            xEntFile.Close();
            VariousFunctions.DeleteFile(xEntFile.FileNameLong);
            // Writes the new file
            AddToLog("Writing file to package");
            if (!xWriteTo(ref xIOIn, xFileAlloc))
                return (xActive = false);
            if (!xWriteChain(xEntAlloc))
                return (xActive = false);
            if (!xWriteChain(xFileAlloc))
                return (xActive = false);
            // Fixes internal variables and then writes hashes
            AddToLog("Fixing Package variables");
            xDeleteChain(xFileBlocks);
            xFileBlocks = xEntAlloc;
            xSTFSStruct.xDirectoryBlock = xEntAlloc[0].ThisBlock;
            foreach (FileEntry x in xFileDirectory)
                x.xFixOffset();
            foreach (FolderEntry x in xFolderDirectory)
                x.xFixOffset();
            xWriteDescriptor(ref xIO);
            return !(xActive = false);
        }

        /// <summary>
        /// Writes the STFS Descriptor to make perm. changes
        /// </summary>
        /// <returns></returns>
        bool xWriteDescriptor(ref DJsIO io)
        {
                AddToLog("Writing new Descriptor");
                io.Position = 0x379;
                xSTFSStruct.xDirectoryBlockCount = (ushort)xCurEntBlckCnt;
                io.Write(xSTFSStruct.GetData());
                io.Flush();
                /*switched0.Clear();
                switched1.Clear();
                switched2 = false;*/
                return true;
        }

        /// <summary>
        /// Generates the data location of the block
        /// </summary>
        /// <param name="xBlock"></param>
        /// <returns></returns>
        internal long GenerateDataOffset(uint xBlock)
        {
            return xSTFSStruct.GenerateDataOffset(xBlock);
        }

        /// <summary>
        /// Generates a hash offset via block
        /// </summary>
        /// <param name="xBlock"></param>
        /// <param name="xTree"></param>
        /// <returns></returns>
        internal long GenerateHashOffset(uint xBlock, TreeLevel xTree)
        {
            long xReturn = xSTFSStruct.GenerateHashOffset(xBlock, xTree);
            if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
                xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
            return xReturn;
        }

        /// <summary>
        /// Generates the Hash Base
        /// </summary>
        /// <param name="xBlock"></param>
        /// <param name="xTree"></param>
        /// <returns></returns>
        internal long GenerateBaseOffset(uint xBlock, TreeLevel xTree)
        {
            long xReturn = xSTFSStruct.GenerateBaseOffset(xBlock, xTree);
            if (xSTFSStruct.ThisType == STFSType.Type1) // Grabs the one up level block record for shifting
                xReturn += (GetRecord(xBlock, (TreeLevel)((byte)xTree + 1)).Index << 0xC);
            return xReturn;
        }

        /// <summary>
        /// Verifies the Header signature
        /// </summary>
        /// <param name="xDev"></param>
        /// <returns></returns>
        Verified VerifySignature(bool xDev)
        {
            try
            {
                AddToLog("Verifying Signature");
                RSAParameters xRSAKeyz = new RSAParameters();
                short xSigSpot = 0;
                switch (xHeader.Magic)
                {
                    case PackageMagic.CON: // signature is the same way for both Dev and Stock
                        {
                            xSigSpot = 0x1AC;
                            xIO.Position = 0x28;
                            xRSAKeyz.Exponent = xIO.ReadBytes(4);
                            xRSAKeyz.Modulus = ScrambleMethods.StockScramble(xIO.ReadBytes(0x80), false);
                        }
                        break;
                    case PackageMagic.LIVE:
                        {
                            xSigSpot = 4;
                            if (!xDev)
                            {
                                xRSAKeyz.Exponent = new byte[] { 0, 1, 0, 1 };
                                xRSAKeyz.Modulus = global::X360.Properties.Resources.XK1;
                            }
                            else
                            {
                                xRSAKeyz.Exponent = new byte[] { 0, 0, 0, 3 };
                                DJsIO xLK = new DJsIO(global::X360.Properties.Resources.XK4, true);
                                xRSAKeyz.Modulus = xLK.ReadBytes(0x100);
                                xLK.Close();
                            }
                        }
                        break;
                    case PackageMagic.PIRS:
                        {
                            xSigSpot = 4;
                            if (!xDev)
                            {
                                xRSAKeyz.Exponent = new byte[] { 0, 0, 0, 3 };
                                xRSAKeyz.Modulus = global::X360.Properties.Resources.XK2;
                            }
                            else
                            {
                                xRSAKeyz.Exponent = new byte[] { 0, 0, 0, 3 };
                                DJsIO xPK = new DJsIO(global::X360.Properties.Resources.XK5, true);
                                xRSAKeyz.Modulus = xPK.ReadBytes(0x100);
                                xPK.Close();
                            }
                        }
                        break;
                }
                xIO.Position = xSigSpot;
                byte[] xSiggy = ScrambleMethods.StockScramble(xIO.ReadBytes(xRSAKeyz.Modulus.Length), true);
                xIO.Position = 0x22C;
                byte[] xHeadr = xIO.ReadBytes(0x118);
                return new Verified(ItemType.Signature, RSAQuick.SignatureVerify(xRSAKeyz, SHA1Quick.ComputeHash(xHeadr), xSiggy), 0x22C, xSigSpot);
            }
            catch { throw CryptoExcepts.CryptoVeri; }
        }

        /// <summary>
        /// Sets a package comming in to this package
        /// </summary>
        /// <param name="xIn"></param>
        void SetSamePackage(ref STFSPackage xIn)
        {
            xLog = xIn.xLog;
            AddToLog("Setting Package");
            xIO = xIn.xIO;
            xSTFSStruct = xIn.STFSStruct;
            xFolderDirectory = xIn.xFolderDirectory;
            xFileDirectory = xIn.xFileDirectory;
            xHeader = xIn.xHeader;
            xFileBlocks = xIn.xFileBlocks;
            xActive = xIn.xActive;
            xroot = xIn.xroot;
            xIn = null;
            foreach (FileEntry x in xFileDirectory)
                x.xPackage = this;
            foreach (FolderEntry x in xFolderDirectory)
                x.xPackage = this;
        }
        
        /// <summary>
        /// Writes hash tables
        /// </summary>
        /// <returns></returns>
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

        /// <summary>
        /// Writes header to the file
        /// </summary>
        /// <param name="xParams"></param>
        /// <returns></returns>
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

        /// <summary>
        /// Gets a folder name via it's Entry ID
        /// </summary>
        /// <param name="ID"></param>
        /// <returns></returns>
        internal string GetFolderNameByID(ushort ID)
        {
            foreach (ItemEntry x in xFolderDirectory)
            {
                if (x.EntryID == ID)
                    return x.Name;
            }
            return null;
        }

        /// <summary>
        /// Gets a folder entry by ID
        /// </summary>
        /// <param name="ID"></param>
        /// <returns></returns>
        internal FolderEntry xGetFolder(ushort ID)
        {
            foreach (FolderEntry x in xFolderDirectory)
            {
                if (x.EntryID == ID)
                    return x;
            }
            return null;
        }

        /// <summary>
        /// Gets the last folder before the target ItemEntry
        /// </summary>
        /// <param name="Path"></param>
        /// <returns></returns>
        internal FolderEntry xGetParentFolder(string Path)
        {
            Path = Path.xExtractLegitPath();
            List<string> folds = Path.Split(new char[] { '/' }).ToList();
            foreach (string x in folds)
                x.IsValidXboxName();
            folds.RemoveAt(folds.Count - 1); // Last entry
            ushort curid = 0xFFFF;
            FolderEntry xcurrent = xroot;
            foreach (string x in folds)
            {
                bool found = false;
                // Grab folders pointing to current instance
                FolderEntry[] folderz = xcurrent.xGetFolders();
                foreach (FolderEntry y in folderz)
                {
                    if (y.Name.ToLower() != x.ToLower())
                        continue; // If not it, then continue searching
                    // Set new found variables
                    found = true;
                    curid = y.FolderPointer;
                    xcurrent = y;
                    break;
                }
                if (!found) // Whoopsie, not found
                    return null;
            }
            return xcurrent; // Must've been found, return it
        }

        /// <summary>
        /// Searches the files for the name and folder pointer
        /// </summary>
        /// <param name="Name"></param>
        /// <param name="FolderPointer"></param>
        /// <returns></returns>
        internal FileEntry xGetFile(string Name, ushort FolderPointer)
        {
            foreach (FileEntry x in xFileDirectory)
            {
                if (x.FolderPointer != FolderPointer || x.Name.ToLower() != Name.ToLower())
                    continue;
                return x;
            }
            return null;
        }

        /// <summary>
        /// Adds a file to the package
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <param name="xFileName"></param>
        /// <param name="Folder"></param>
        /// <returns></returns>
        bool xAddFile(DJsIO xIOIn, string xFileName, ushort Folder)
        {
            try
            {
                if (xIOIn == null || !xIOIn.Accessed || xIOIn.Length > ((xSTFSStruct.SpaceBetween[2] - 1) << 0xC))
                    return (xActive = false);
                foreach (FileEntry m in xFileDirectory)
                {
                    if (m.FolderPointer == Folder && m.Name == xFileName)
                        return (xActive = false);
                }
                // Allocates blocks
                AddToLog("Allocating blocks");
                BlockRecord[] xEntAlloc = xAllocateBlocks(xNewEntBlckCnt(1), 0);
                BlockRecord[] xFileAlloc = xAllocateBlocks(xIOIn.BlockCountSTFS(), xEntAlloc[xEntAlloc.Length - 1].ThisBlock + 1);
                // Adds new file info
                AddToLog("Adding file information");
                ItemEntry x = new ItemEntry(xFileName, (int)xIOIn.Length, false, (ushort)(xFileDirectory.Count + xFolderDirectory.Count), Folder, this);
                FileEntry y = new FileEntry(x);
                y.xStartBlock = xFileAlloc[0].ThisBlock;
                xFileDirectory.Add(y);
                return xDoAdd(ref xIOIn, ref xEntAlloc, ref xFileAlloc);
            }
            catch (Exception x) { return (xActive = false); /*throw x;*/ }
        }

        /// <summary>
        /// Deletes an item via its entry
        /// </summary>
        /// <param name="x"></param>
        internal int xDeleteEntry(ItemEntry x)
        {
            if (x.FolderFlag)
            {
                for (int i = 0; i < xFolderDirectory.Count; i++)
                {
                    if (xFolderDirectory[i].EntryID == x.EntryID)
                    {
                        xFolderDirectory.RemoveAt(i);
                        return i;
                    }
                }
            }
            else
            {
                for (int i = 0; i < xFileDirectory.Count; i++)
                {
                    if (xFileDirectory[i].EntryID == x.EntryID)
                    {
                        if (xFileDirectory[i].ReadBlocks())
                            xDeleteChain(xFileDirectory[i].xBlocks);
                        xFileDirectory.RemoveAt(i);
                        return i;
                    }
                }
            }
            return 0;
        }

        /// <summary>
        /// For sorting folders by path
        /// </summary>
        /// <param name="x1"></param>
        /// <param name="x2"></param>
        /// <returns></returns>
        int sortpathct(CFolderEntry x1, CFolderEntry x2)
        {
            return x1.xthispath.xPathCount().CompareTo(x2.xthispath.xPathCount());
        }

        /// <summary>
        /// Gets a name via hash string and Title ID
        /// </summary>
        /// <returns></returns>
        string dlcname()
        {
            try
            {
                xIO.Position = 0x32C;
                return xIO.ReadBytes(0x14).HexString() + ((byte)(xHeader.TitleID >> 16)).ToString("X2");
            }
            catch { return "00000000000000000000000000000000000000000000"; }
        }

        enum SwitchType { None, Allocate, Delete }

        void SwitchNWrite(BlockRecord RecIn, SwitchType Change)
        {
            // As a temp, this will just grab the correct offset and write to it
            // I disabled the switch/backup effect in order to preserve functionality
            //
            // RECONSTRUCT
            //

            bool canswitch = (xSTFSStruct.ThisType == STFSType.Type1);
            //const uint basetable = 0xFFFFF000;
            BlockRecord current = xSTFSStruct.TopRecord;
            long[] pos = new long[] { 0, 0, 0 };
            // Grab base starting points
            if (RecIn.ThisBlock >= Constants.BlockLevel[0] ||
                xSTFSStruct.xBlockCount > Constants.BlockLevel[0])
            {
                if (RecIn.ThisBlock >= Constants.BlockLevel[1] ||
                    xSTFSStruct.xBlockCount > Constants.BlockLevel[1])
                    pos[0] = xSTFSStruct.GenerateHashOffset(RecIn.ThisBlock, TreeLevel.L2) + 0x14;
                pos[1] = xSTFSStruct.GenerateHashOffset(RecIn.ThisBlock, TreeLevel.L1) + 0x14;
            }
            pos[2] = xSTFSStruct.GenerateHashOffset(RecIn.ThisBlock, TreeLevel.L0) + 0x14;
            //bool wipe = current.BlocksFree >= (Constants.BlockLevel[1] * Constants.BlockLevel[0]);
            long len = GenerateDataOffset(RecIn.ThisBlock) + 0x1000;
            if (xIO.Length < len)
                xIO.SetLength(len);
            if (Change == SwitchType.Allocate)
                xSTFSStruct.TopRecord.BlocksFree--;
            else if (Change == SwitchType.Delete)
                xSTFSStruct.TopRecord.BlocksFree++;
            if (pos[0] != 0)
            {
                //if (wipe)
                //{
                //    xIO.Position = (pos[0] & basetable) + (current.Index << 0xC);
                //    xIO.Write(new byte[0x1000]);
                //    xIO.Flush();
                //}
                /*if (canswitch) // If this table hasn't been switched yet
                {
                    if (!switched2)
                    {
                        xIO.Position = (pos[0] & basetable) + (current.Index << 0xC); // Get starting of table
                        byte[] data = xIO.ReadBytes(0x1000); // Read it
                        xSTFSStruct.TopRecord.Switch(); // Switch the index
                        pos[0] += (xSTFSStruct.TopRecord.Index << 0xC); // Add to the base STFS function result
                        xIO.Position = (pos[0] & basetable); // Go to new position and write it
                        xIO.Write(data);
                        xIO.Flush();
                        data = null;
                        switched2 = true;
                    }
                    else pos[0] += (current.Index << 0xC); // Already switched, add the index
                }*/

                //KILL ON RECONSTRUCTION
                if (canswitch)
                    pos[0] += (current.Index << 0xC);
                // ---------------------

                xIO.Position = pos[0];
                current = new BlockRecord(xIO.ReadUInt32());
                //wipe = current.BlocksFree >= Constants.BlockLevel[1];
                if (Change != SwitchType.None)
                {
                    if (Change == SwitchType.Allocate)
                        current.BlocksFree--; // Takes away a free block
                    else current.BlocksFree++; // Adds a free block
                    xIO.Position = pos[0];
                    xIO.Write(current.Flags);
                    xIO.Flush();
                }
            }
            // Follows same pattern
            if (pos[1] != 0)
            {
                //if (wipe)
                //{
                //    xIO.Position = (pos[1] & basetable) + (current.Index << 0xC);
                //    xIO.Write(new byte[0x1000]);
                //    xIO.Flush();
                //}
                /*if (canswitch)
                {
                    if (!switched1.Contains((int)(RecIn.ThisBlock / Constants.BlockLevel[1])))
                    {
                        xIO.Position = (pos[1] & basetable) + (current.Index << 0xC);
                        byte[] data = xIO.ReadBytes(0x1000);
                        current.Switch();
                        pos[1] += (current.Index << 0xC);
                        xIO.Position = (pos[1] & basetable);
                        xIO.Write(data);
                        xIO.Flush();
                        data = null;
                        if (pos[0] != 0)
                        {
                            xIO.Position = pos[0];
                            xIO.Write(current.Flags);
                            xIO.Flush();
                        }
                        switched1.Add((int)(RecIn.ThisBlock / Constants.BlockLevel[1]));
                        if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[1])
                            xSTFSStruct.TopRecord.Switch();
                    }
                    else pos[1] += (current.Index << 0xC);
                }*/

                //KILL ON RECONSTRUCTION
                if (canswitch)
                    pos[1] += (current.Index << 0xC);
                // ---------------------

                xIO.Position = pos[1];
                current = new BlockRecord(xIO.ReadUInt32());
                //wipe = current.BlocksFree >= Constants.BlockLevel[0];
                if (Change != SwitchType.None)
                {
                    if (Change == SwitchType.Allocate)
                        current.BlocksFree--; // Takes away a free block
                    else current.BlocksFree++; // Adds a free block
                    xIO.Position = pos[1];
                    xIO.Write(current.Flags);
                    xIO.Flush();
                }
            }
            //if (wipe)
            //{
            //    xIO.Position = (pos[0] & basetable) + (current.Index << 0xC);
            //    xIO.Write(new byte[0x1000]);
            //    xIO.Flush();
            //}
            /*if (canswitch)
            {
                if (!switched0.Contains((int)(RecIn.ThisBlock / Constants.BlockLevel[0])))
                {
                    xIO.Position = (pos[2] & basetable) + (current.Index << 0xC);
                    byte[] data = xIO.ReadBytes(0x1000);
                    current.Switch();
                    pos[2] += (current.Index << 0xC);
                    xIO.Position = (pos[2] & basetable);
                    xIO.Write(data);
                    xIO.Flush();
                    data = null;
                    if (pos[1] != 0)
                    {
                        xIO.Position = pos[1];
                        xIO.Write(current.Flags);
                        xIO.Flush();
                    }
                    switched1.Add((int)(RecIn.ThisBlock / Constants.BlockLevel[0]));
                    if (xSTFSStruct.xBlockCount <= Constants.BlockLevel[0])
                        xSTFSStruct.TopRecord.Switch();
                }
                else pos[2] += (current.Index << 0xC);
            }*/

            //KILL ON RECONSTRUCTION
            if (canswitch)
                pos[2] += (current.Index << 0xC);
            // ---------------------

            if (Change == SwitchType.Allocate)
            {
                if (RecIn.Status == HashStatus.Old)
                    RecIn.Status = HashStatus.Reused;
                else RecIn.Status = HashStatus.New;
            }
            else if (Change == SwitchType.Delete)
                RecIn.MarkOld();
            xIO.Position = pos[2];
            xIO.Write(RecIn.Flags);
            xIO.Flush();
            if (RecIn.ThisBlock >= xSTFSStruct.xBlockCount)
                xSTFSStruct.xBlockCount = RecIn.ThisBlock + 1;
        }

        BlockRecord GetRecord(uint xBlock, TreeLevel xLevel)
        {
            if (xLevel == TreeLevel.LT)
                return xSTFSStruct.TopRecord;
            BlockRecord current = xSTFSStruct.TopRecord;
            bool canswitch = (xSTFSStruct.ThisType == STFSType.Type1);
            if (xSTFSStruct.xBlockCount > Constants.BlockLevel[1])
            {
                // Grab base position
                xIO.Position = (xSTFSStruct.GenerateHashOffset(xBlock, TreeLevel.L2) + 0x14);
                if (canswitch)
                    xIO.Position += (current.Index << 0xC);
                current = new BlockRecord(xIO.ReadUInt32()); // Read new flag
                if (xLevel == TreeLevel.L2)
                {
                    // return if needed
                    current.ThisBlock = xBlock;
                    current.ThisLevel = TreeLevel.L2;
                    return current;
                }
            }
            else if (xLevel == TreeLevel.L2)
                return xSTFSStruct.TopRecord;
            // Follows same procedure
            if (xSTFSStruct.xBlockCount > Constants.BlockLevel[0])
            {
                xIO.Position = (xSTFSStruct.GenerateHashOffset(xBlock, TreeLevel.L1)) + 0x14;
                if (canswitch)
                    xIO.Position += (current.Index << 0xC);
                current = new BlockRecord(xIO.ReadUInt32());
                if (xLevel == TreeLevel.L1)
                {
                    current.ThisBlock = xBlock;
                    current.ThisLevel = TreeLevel.L1;
                    return current;
                }
            }
            else if (xLevel == TreeLevel.L1)
                return xSTFSStruct.TopRecord;
            xIO.Position = (xSTFSStruct.GenerateHashOffset(xBlock, TreeLevel.L0)) + 0x14;
            if (canswitch)
                xIO.Position += (current.Index << 0xC);
            current = new BlockRecord(xIO.ReadUInt32());
            current.ThisBlock = xBlock;
            current.ThisLevel = TreeLevel.L0;
            return current;
        }

        internal bool xWriteChain(BlockRecord[] xRecs)
        {
            for (int i = 0; i < xRecs.Length; i++)
            {
                if ((i + 1) < xRecs.Length)
                    xRecs[i].NextBlock = xRecs[i + 1].ThisBlock;
                else xRecs[i].NextBlock = Constants.STFSEnd;
                SwitchNWrite(xRecs[i], SwitchType.Allocate);
            }
            return true;
        }

        internal bool xDeleteChain(BlockRecord[] xBlocks)
        {
            if (xBlocks == null)
                return true;
            foreach (BlockRecord x in xBlocks)
            {
                SwitchNWrite(x, SwitchType.Delete);
            }
            return true;
        }

        internal BlockRecord[] xAllocateBlocks(uint count, uint xStart)
        {
            if ((xSTFSStruct.BlockCount + count) > xSTFSStruct.SpaceBetween[2])
                return new BlockRecord[0];
            List<BlockRecord> xReturn = new List<BlockRecord>();
            for (uint i = 0; i < count; i++)
            {
                BlockRecord x = null;
                while (x == null)
                {
                    if (xStart > xSTFSStruct.SpaceBetween[2])
                        break;
                    // Grab record or make new one
                    if (xStart < xSTFSStruct.xBlockCount)
                    {
                        BlockRecord y = GetRecord(xStart, TreeLevel.L0);
                        if (y.Status == HashStatus.Old || y.Status == HashStatus.Unused)
                            x = y;
                    }
                    else
                    {
                        if (xStart == Constants.BlockLevel[0])
                        {
                            xIO.Position = GenerateHashOffset(0, TreeLevel.L1) + (xSTFSStruct.TopRecord.Index << 0xC) + 0x14;
                            xIO.Write(xSTFSStruct.TopRecord.Flags);
                            xIO.Flush();
                        }
                        else if (xStart == Constants.BlockLevel[1])
                        {
                            xIO.Position = GenerateHashOffset(0, TreeLevel.L2) + (xSTFSStruct.TopRecord.Index << 0xC) + 0x14;
                            xIO.Write(xSTFSStruct.TopRecord.Flags);
                            xIO.Flush();
                        }
                        x = new BlockRecord(HashStatus.New, Constants.STFSEnd);
                        x.ThisBlock = xStart;
                        x.ThisLevel = TreeLevel.L0;
                    }
                    xStart++;
                }
                xReturn.Add(x);
            }
            return xReturn.ToArray();
        }
        #endregion

        #region Package initialization
        /// <summary>
        /// Lets user auto select package
        /// </summary>
        public STFSPackage(LogRecord LogIn)
            : this(new DJsIO(DJFileMode.Open, "Open an Xbox Package", "", true),
            LogIn)
        {
            if (!ParseSuccess && xIO != null)
                xIO.Dispose();
        }

        /// <summary>
        /// Initializes a package parse from an already accessed file
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <param name="LogIn"></param>
        public STFSPackage(DJsIO xIOIn, LogRecord LogIn)
        {
            if (!xIOIn.Accessed)
                return;
            xIO = xIOIn;
            xLog = LogIn;
            xActive = true;
            try
            {
                xIO.Position = 0;
                xIO.IsBigEndian = true;
                uint xBuff = xIOIn.ReadUInt32();
                PackageMagic xMagic = PackageMagic.Unknown;
                if (Enum.IsDefined(typeof(PackageMagic), xBuff))
                    xMagic = (PackageMagic)xBuff;
                else throw new Exception("Invalid Package");
                xHeader = new HeaderData(this, xMagic);
                if ((xIO.Length % 0x1000) != 0)
                {
                    xIO.Position = xIO.Length;
                    xIO.Write(new byte[(int)(0x1000 - (xIO.Length % 0x1000))]);
                    xIO.Flush();
                }
                if (xHeader.ThisType == PackageType.HDDInstalledGame ||
                    xHeader.ThisType == PackageType.OriginalXboxGame ||
                    xHeader.ThisType == PackageType.GamesOnDemand ||
                    xHeader.ThisType == PackageType.SocialTitle)
                    throw STFSExcepts.Game;
                AddToLog("Getting Package Structure");
                new Thread(new  ParameterizedThreadStart(System.DLLIdentify.PrivilegeCheck)).Start(Thread.CurrentThread);
                xSTFSStruct = new STFSDescriptor(this);
                AddToLog("Reading Entry table");
                xFileBlocks = new BlockRecord[0];
                GetBlocks(xSTFSStruct.DirectoryBlockCount, xSTFSStruct.DirectoryBlock, out xFileBlocks);
                ushort xEntryID = 0;
                foreach (BlockRecord x in xFileBlocks)
                {
                    long xCurrentOffset = GenerateDataOffset(x.ThisBlock);
                    for (int i = 0; i < 0x40; i++)
                    {
                        xIO.Position = (xCurrentOffset + (0x40 * i));
                        if (xIO.ReadByte() == 0)
                            continue;
                        xIO.Position--;
                        ItemEntry xItem = new ItemEntry(xIO.ReadBytes(0x40), (xIO.Position - 0x40), xEntryID, this);
                        if (xItem.IsDeleted)
                            continue;
                        if (!xItem.FolderFlag)
                            xFileDirectory.Add(new FileEntry(xItem));
                        else xFolderDirectory.Add(new FolderEntry(xItem));
                        xEntryID++;
                    }
                }
                xroot = new FolderEntry("", 0, 0xFFFF, 0xFFFF, this);
                xActive = false;
            }
            catch (Exception x) { xIO = null; throw x; }
            
        }

        /// <summary>
        /// Attempts to parse a file from a specific location
        /// </summary>
        /// <param name="xLocation"></param>
        /// <param name="LogIn"></param>
        public STFSPackage(string xLocation, LogRecord LogIn) : this(
            new DJsIO(xLocation, DJFileMode.Open, true),
            LogIn) { }

        /// <summary>
        /// Create an STFS Package
        /// </summary>
        /// <param name="xSession"></param>
        /// <param name="xSigning"></param>
        /// <param name="xOutPath"></param>
        /// <param name="LogIn"></param>
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

        /// <summary>
        /// Function for partial classes, importing packages
        /// </summary>
        /// <param name="xIn"></param>
        protected STFSPackage(ref STFSPackage xIn) { xActive = true; SetSamePackage(ref xIn); xActive = false; }
        #endregion
        
        #region Public Methods
        /* Structure Fixing */
        /// <summary>
        /// Updates the information in the header
        /// </summary>
        /// <returns></returns>
        public bool UpdateHeader(RSAParams xParams)
        {
            if (!ActiveCheck())
                return false;
            try
            {
                bool xSuccess = xWriteHeader(xParams);
                xActive = false;
                return xSuccess;
            }
            catch (Exception x) { xActive = false; throw x; }
        }

        /// <summary>
        /// Writes Tables and updates Header
        /// </summary>
        /// <param name="xParams"></param>
        /// <returns></returns>
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

        /* Structure Verification */
        /// <summary>
        /// Returns a List of details containing the package
        /// </summary>
        /// <returns></returns>
        public Verified[] VerifyHashTables()
        {
            if (!ActiveCheck())
                return null;
            List<Verified> xReturn = new List<Verified>();
            try
            {
                // Verifies each level needed
                AddToLog("Verifying Level 0");
                for (uint i = 0; i < xSTFSStruct.BlockCount; i++)
                {
                    BlockRecord lvl = GetRecord(i, TreeLevel.L1);
                    if (lvl.BlocksFree < Constants.BlockLevel[0])
                    {
                        long xDataBlock = GenerateDataOffset(i);
                        if (xDataBlock < xIO.Length)
                        {
                            long xHashLocale = xSTFSStruct.GenerateHashOffset(i, 0) + (lvl.Index << 0xC);
                            xIO.Position = xHashLocale;
                            byte[] xHash = xIO.ReadBytes(20);
                            xReturn.Add(new Verified(ItemType.Data, XVerifyHash(xDataBlock, 0x1000, ref xHash), xDataBlock, xHashLocale));
                        }
                    }
                }
                if (STFSStruct.BlockCount > Constants.BlockLevel[0])
                {
                    AddToLog("Verifying Level 1");
                    uint ct = (((xSTFSStruct.xBlockCount - 1) / Constants.BlockLevel[0]) + 1);
                    for (uint i = 0; i < ct; i++)
                    {
                        BlockRecord lvl = GetRecord(i * Constants.BlockLevel[0], TreeLevel.L2);
                        BlockRecord current = GetRecord(i * Constants.BlockLevel[0], TreeLevel.L1);
                        if (lvl.BlocksFree < Constants.BlockLevel[1] ||
                            current.BlocksFree < Constants.BlockLevel[0])
                        {
                            long xInputLocale = xSTFSStruct.GenerateBaseOffset(i * Constants.BlockLevel[0], TreeLevel.L0) + (current.Index << 0xC);
                            if (xInputLocale < xIO.Length)
                            {
                                long xHashLocale = xSTFSStruct.GenerateHashOffset((i * Constants.BlockLevel[0]), TreeLevel.L1) + (lvl.Index << 0xC);
                                xIO.Position = xHashLocale;
                                byte[] xHash = xIO.ReadBytes(20);
                                xReturn.Add(new Verified(ItemType.TableTree0, XVerifyHash(xInputLocale, 0x1000, ref xHash), xInputLocale, xHashLocale));
                            }
                        }
                    }
                    if (STFSStruct.BlockCount > Constants.BlockLevel[1])
                    {
                        AddToLog("Verifying Level 2");
                        ct = (((xSTFSStruct.xBlockCount - 1) / Constants.BlockLevel[1]) + 1);
                        for (uint i = 0; i < ct; i++)
                        {
                            BlockRecord current = GetRecord(i * Constants.BlockLevel[1], TreeLevel.L2);
                            if (current.BlocksFree < Constants.BlockLevel[1])
                            {
                                long xInputLocale = xSTFSStruct.GenerateBaseOffset((i * Constants.BlockLevel[1]), TreeLevel.L1) + (current.Index << 0xC);
                                long xHashLocale = GenerateHashOffset((i * Constants.BlockLevel[1]), TreeLevel.L2);
                                xIO.Position = xHashLocale;
                                byte[] xHash = xIO.ReadBytes(20);
                                xReturn.Add(new Verified(ItemType.TableTree1, XVerifyHash(xInputLocale, 0x1000, ref xHash), xInputLocale, xHashLocale));
                            }
                        }
                    }
                }
                xActive = false;
                return xReturn.ToArray();
            }
            catch (Exception xerror) { xActive = false; throw xerror; }
        }

        /// <summary>
        /// Verify the header
        /// </summary>
        /// <returns></returns>
        public Verified[] VerifyHeader()
        {
            if (!ActiveCheck())
                return null;
            try
            {
                List<Verified> xReturn = new List<Verified>();
                // Verifies master hash with currently written header
                AddToLog("Verifying Master hash");
                xIO.Position = 0x395;
                xIO.IsBigEndian = true;
                int xBlockCount = xIO.ReadInt32();
                long xLocale = 0;
                if (xBlockCount <= Constants.BlockLevel[0])
                    xLocale = GenerateBaseOffset(0, 0);
                else if (xBlockCount <= Constants.BlockLevel[1])
                    xLocale = GenerateBaseOffset(0, TreeLevel.L1);
                else xLocale = GenerateBaseOffset(0, TreeLevel.L2);
                xIO.Position = 0x381;
                byte[] xHash = xIO.ReadBytes(20);
                xReturn.Add(new Verified(ItemType.Master, XVerifyHash(xLocale, 0x1000, ref xHash), xLocale, (0x381)));
                // Verifies currently written header
                AddToLog("Verifying Header hash");
                int xSize = 0;
                if (xSTFSStruct.BaseBlock == 0xA000)
                    xSize = 0x9CBC;
                else xSize = 0xACBC; // b000
                xIO.Position = 0x32C;
                xHash = xIO.ReadBytes(20);
                xReturn.Add(new Verified(ItemType.Header, XVerifyHash(0x344, xSize, ref xHash), 0x344, 0x32C));
                switch (xHeader.Magic)
                {
                    case PackageMagic.CON:
                        {
                            // Verifies Certificate
                            AddToLog("Verifying Certificate");
                            RSAParameters xRSAKeyz = new RSAParameters();
                            xRSAKeyz.Exponent = new byte[] { 0, 0, 0, 3 };
                            xRSAKeyz.Modulus = global::X360.Properties.Resources.XK6;
                            xIO.Position = 4;
                            byte[] xCert = xIO.ReadBytes(0xA8);
                            byte[] xSig = xIO.ReadBytes(0x100);
                            xReturn.Add(new Verified(ItemType.Certificate, RSAQuick.SignatureVerify(xRSAKeyz, SHA1Quick.ComputeHash(xCert), ScrambleMethods.StockScramble(xSig, true)), 4, 0xAC));
                            xReturn.Add(VerifySignature(false)); // Doesn't matter, same thing for CON
                            xActive = false;
                            return xReturn.ToArray();
                        }
                    default:
                        {
                            xReturn.Add(VerifySignature(false));
                            xReturn.Add(VerifySignature(true));
                            xActive = false;
                            return xReturn.ToArray();
                        }
                }
            }
            catch { xActive = false; throw STFSExcepts.General; }
        }

        /* Entry Functions */
        /// <summary>
        /// Adds a folder to the package via the root
        /// </summary>
        /// <param name="FolderPath"></param>
        /// <returns></returns>
        public bool AddFolder(string FolderPath)
        {
            if (!ActiveCheck())
                return false;
            try
            {
                if (xFileDirectory.Count + xFolderDirectory.Count + 1 >= 0x65535)
                    return (xActive = false);
                FolderPath = FolderPath.Replace("\\", "/");
                if (FolderPath[0] == '/')
                    FolderPath = FolderPath.Substring(1, FolderPath.Length - 1);
                if (FolderPath[FolderPath.Length - 1] == '/')
                    FolderPath = FolderPath.Substring(0, FolderPath.Length - 1);
                List<string> folds = FolderPath.Split(new char[] { '/' }).ToList();
                foreach (string x in folds)
                    x.IsValidXboxName();
                string foldtoadd = folds[folds.Count - 1];
                folds.RemoveAt(folds.Count - 1);
                ushort curid = 0xFFFF;
                foreach (string x in folds)
                {
                    bool found = false;
                    foreach (FolderEntry y in xFolderDirectory)
                    {
                        if (y.FolderPointer != curid || y.Name.ToLower() != x.ToLower())
                            continue;
                        found = true;
                        curid = y.FolderPointer;
                        if (y.Name == x)
                            break;
                    }
                    if (!found)
                        return (xActive = false);
                }
                if (xFileDirectory.Count + xFolderDirectory.Count + 1 >= 0x65535)
                    return (xActive = false);
                return !(xActive = false);
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Extracts the package via out location
        /// </summary>
        /// <param name="xOutLocale"></param>
        /// <param name="xIncludeSubItems"></param>
        /// <param name="xIncludeHeader"></param>
        /// <returns></returns>
        public bool ExtractPayload(string xOutLocale, bool xIncludeSubItems, bool xIncludeHeader)
        {
            if (!ActiveCheck())
                return false;
            return xExtractPayload(xOutLocale, xIncludeSubItems, xIncludeHeader);
        }

        /// <summary>
        /// GUI based extraction
        /// </summary>
        /// <param name="xIncludeSubItems"></param>
        /// <param name="xDescription"></param>
        /// <param name="xIncludeHeader"></param>
        /// <returns></returns>
        public bool ExtractPayload(bool xIncludeSubItems, string xDescription, bool xIncludeHeader)
        {
            if (!ActiveCheck())
                return false;
            string y = VariousFunctions.GetUserFolderLocale(xDescription);
            if (y != null)
                return xExtractPayload(y, xIncludeSubItems, xIncludeHeader);
            return (xActive = false);
        }

        /// <summary>
        /// Gets a file by path
        /// </summary>
        /// <param name="Path"></param>
        /// <returns></returns>
        public FileEntry GetFile(string Path)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                if (Path == null || Path == "")
                    throw new Exception();
                Path = Path.Replace("\\", "/");
                if (Path[0] == '/')
                    Path = Path.Substring(1, Path.Length - 1);
                if (Path[Path.Length - 1] == '/')
                    Path = Path.Substring(0, Path.Length - 1);
                FolderEntry parent = xGetParentFolder(Path);
                if (parent == null)
                    throw new Exception();
                string file = Path.Split(new char[] { '/' }).LastValue();
                if (file == null || file == "")
                    throw new Exception();
                FileEntry z = xGetFile(file, parent.EntryID);
                xActive = false;
                return z;
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Gets a file by the name and pointer
        /// </summary>
        /// <param name="Name"></param>
        /// <param name="FolderPointer"></param>
        /// <returns></returns>
        public FileEntry GetFile(string Name, ushort FolderPointer)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                FileEntry xReturn = xGetFile(Name, FolderPointer);
                xActive = false;
                return xReturn;
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Gets a folder by it's ID
        /// </summary>
        /// <param name="FolderID"></param>
        /// <returns></returns>
        public FolderEntry GetFolder(ushort FolderID)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                FolderEntry xReturn = xGetFolder(FolderID);
                xActive = false;
                return xReturn;
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Gets a folder by it's path
        /// </summary>
        /// <param name="Path"></param>
        /// <returns></returns>
        public FolderEntry GetFolder(string Path)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                Path = Path.Replace("\\", "/");
                if (Path[0] == '/')
                    Path = Path.Substring(1, Path.Length - 1);
                if (Path[Path.Length - 1] == '/')
                    Path = Path.Substring(0, Path.Length - 1);
                FolderEntry parent = xGetParentFolder(Path);
                if (parent == null)
                    throw new Exception();
                string folder = Path.Split(new char[] { '/' }).LastValue();
                if (folder == null || folder == "")
                    throw new Exception();
                FolderEntry[] foldz = parent.xGetFolders();
                foreach (FolderEntry x in foldz)
                {
                    if (x.Name.ToLower() != folder.ToLower())
                        continue;
                    xActive = false;
                    return x;
                }
                throw new Exception();
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Grabs the files via the pointer
        /// </summary>
        /// <param name="FolderPointer"></param>
        /// <returns></returns>
        public FileEntry[] GetFiles(ushort FolderPointer)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                List<FileEntry> xReturn = new List<FileEntry>();
                foreach (FileEntry x in xFileDirectory)
                {
                    if (x.FolderPointer == FolderPointer)
                        xReturn.Add(x);
                }
                xActive = false;
                return xReturn.ToArray();
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Grabs the files via the path
        /// </summary>
        /// <param name="FolderPath"></param>
        /// <returns></returns>
        public FileEntry[] GetFiles(string FolderPath)
        {
            if (!ActiveCheck())
                return null;
            try
            {
                if (FolderPath == null || FolderPath == "")
                    throw new Exception();
                FolderPath = FolderPath.Replace("\\", "/");
                if (FolderPath[0] == '/')
                    FolderPath = FolderPath.Substring(1, FolderPath.Length - 1);
                if (FolderPath[FolderPath.Length - 1] == '/')
                    FolderPath = FolderPath.Substring(0, FolderPath.Length - 1);
                FolderPath += "/a"; // Fake a random name so i can just use the parent folder function
                FolderEntry parent = xGetParentFolder(FolderPath);
                if (parent == null)
                    throw new Exception();
                List<FileEntry> xReturn = new List<FileEntry>();
                foreach (FileEntry x in xFileDirectory)
                {
                    if (x.FolderPointer == parent.FolderPointer)
                        xReturn.Add(x);
                }
                xActive = false;
                return xReturn.ToArray();
            }
            catch { xActive = false; return null; }
        }

        /// <summary>
        /// Adds a file under a Folder ID
        /// </summary>
        /// <param name="Name"></param>
        /// <param name="xIOIn"></param>
        /// <param name="FolderID"></param>
        /// <param name="xType"></param>
        /// <returns></returns>
        public bool MakeFile(string Name, DJsIO xIOIn, ushort FolderID, AddType xType)
        {
            if (!ActiveCheck())
                return false;
            foreach (FileEntry x in xFileDirectory)
            {
                if (x.FolderPointer == FolderID && x.Name.ToLower() == Name.ToLower())
                {
                    if (xType == AddType.NoOverWrite)
                        return (xActive = false);
                    return x.Replace(xIOIn);
                }
            }
            try { return xAddFile(xIOIn, Name, FolderID); }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Adds a file
        /// </summary>
        /// <param name="Path"></param>
        /// <param name="xIOIn"></param>
        /// <param name="xType"></param>
        /// <returns></returns>
        public bool MakeFile(string Path, DJsIO xIOIn, AddType xType)
        {
            if (!ActiveCheck())
                return false;
            if (Path == null || Path == "")
                return (xActive = false);
            Path = Path.Replace("\\", "/");
            if (Path[0] == '/')
                Path = Path.Substring(1, Path.Length - 1);
            if (Path[Path.Length - 1] == '/')
                Path = Path.Substring(0, Path.Length - 1);
            FolderEntry parent = xGetParentFolder(Path);
            if (parent == null)
                return (xActive = false);
            string file = Path.Split(new char[] { '/' }).LastValue();
            if (file == null || file == "")
                return (xActive = false);
            FileEntry z = xGetFile(file, parent.FolderPointer);
            if (z != null && xType == AddType.NoOverWrite)
                return (xActive = false);
            if (z == null)
            {
                if (xFileDirectory.Count + xFolderDirectory.Count + 1 >= 65535)
                    return (xActive = false);
                return xAddFile(xIOIn, file, parent.FolderPointer);
            }
            else if (xType == AddType.Inject)
                return z.xInject(xIOIn);
            else return z.xReplace(xIOIn);
        }
        
        /* Misc Functions */
        /// <summary>
        /// Backs up this package to a specific location
        /// </summary>
        /// <param name="xOutLocation"></param>
        /// <returns>ya boi</returns>
        public bool MakeBackup(string xOutLocation)
        {
            if (!ActiveCheck())
                return false;
            try
            {
                AddToLog("Attempting to copy to given location");
                File.Copy(xIO.FileNameLong, xOutLocation);
                return !(xActive = false);
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Rebuilds the package using package creation
        /// </summary>
        /// <param name="xParams"></param>
        /// <returns></returns>
        public bool RebuildPackage(RSAParams xParams)
        {
            if (!ActiveCheck())
                return false;
            if (!xParams.Valid)
                return (xActive = false);
            CreateSTFS x = new CreateSTFS();
            x.HeaderData = xHeader;
            x.STFSType = xSTFSStruct.ThisType;
            // Populate
            foreach (FolderEntry y in xFolderDirectory)
                x.AddFolder(y.GetPath());
            foreach (FileEntry y in xFileDirectory)
            {
                DJsIO io = y.xGetTempIO(true);
                if (io != null && io.Accessed)
                {
                    io.Close();
                    x.AddFile(io.FileNameLong, y.GetPath());
                }
            }
            STFSPackage xreturn = new STFSPackage(x, xParams, VariousFunctions.GetTempFileLocale(), xLog);
            if (xreturn.ParseSuccess)
            {
                xIO.Close();
                xreturn.xIO.Close();
                if (!VariousFunctions.MoveFile(xreturn.xIO.FileNameLong, xIO.FileNameLong))
                    return (xActive = false);
                xreturn.xIO = xIO;
                SetSamePackage(ref xreturn);
                xIO.OpenAgain();
                return !(xActive = false);
            }
            xreturn.xIO.Close();
            VariousFunctions.DeleteFile(xreturn.xIO.FileNameLong);
            return (xActive = false);
        }
        
        /// <summary>
        /// Returns the name used for DLC names
        /// </summary>
        /// <returns></returns>
        public string GetCurrentDLCFileName()
        {
            if (!ActiveCheck())
                return null;
            string xReturn = dlcname();
            xActive = false;
            return xReturn;
        }
        #endregion

        #region Package IO stuff
        /// <summary>
        /// File location
        /// </summary>
        public string FileNameLong { get { return xIO.FileNameLong; }}
        /// <summary>
        /// File Name
        /// </summary>
        public string FileNameShort { get { return xIO.FileNameShort; }}
        /// <summary>
        /// File Path
        /// </summary>
        public string FilePath { get { return xIO.FilePath; }}
        /// <summary>
        /// File Extension
        /// </summary>
        public string FileExtension { get { return xIO.FileExtension; }}
        
        /// <summary>
        /// Close the IO
        /// </summary>
        /// <returns></returns>
        public bool CloseIO()
        {
            if (xActive)
                return false;
            xActive = true;
            if (xIO != null)
                xIO.Close();
            return true;
        }
        #endregion
    }

    static class extenz
    {
        public static uint[] Reverse(this uint[] xIn)
         {
             List<uint> xreturn = new List<uint>(xIn);
             xreturn.Reverse();
             xIn = xreturn.ToArray();
             return xIn;
         }

        public static string LastValue(this string[] xIn)
        {
            if (xIn.Length == 0)
                return null;
            return xIn[xIn.Length - 1];
        }

        public static bool ContainsBlock(this List<BlockRecord> x, BlockRecord y)
        {
            foreach (BlockRecord z in x)
            {
                if (z.ThisBlock == y.ThisBlock)
                    return true;
            }
            return false;
        }
    }
}
