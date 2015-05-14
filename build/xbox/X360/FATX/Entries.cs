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
using System.Runtime.CompilerServices;
using System.IO;
using System.Diagnostics;
using X360.FATX;
using X360.IO;
using X360.IO.FATXExtensions;
using X360.Other;

namespace X360.FATX
{
    /// <summary>
    /// Generic entry for FATX
    /// </summary>
    public class FATXEntry
    {
        #region Variables
        [CompilerGenerated]
        internal byte xNLen;
        [CompilerGenerated]
        internal string xName;
        [CompilerGenerated]
        internal int xSize;
        [CompilerGenerated]
        internal uint xStartBlock;
        [CompilerGenerated]
        internal int xT1;
        [CompilerGenerated]
        internal int xT2;
        [CompilerGenerated]
        internal int xT3;
        [CompilerGenerated]
        internal bool xIsValid = false;
        [CompilerGenerated]
        internal bool xIsFolder = false;
        [CompilerGenerated]
        FATXPartition xPart;
        [CompilerGenerated]
        internal long xOffset;
        [CompilerGenerated]
        internal FATXDrive xDrive;

        /// <summary>
        /// Entry Size
        /// </summary>
        public int Size
        {
            get
            {
                if (!xIsValid)
                    throw FATXExcepts.ValidExcept;
                return xSize;
            }
        }
        /// <summary>
        /// Entry Start Block
        /// </summary>
        public uint StartBlock
        {
            get
            {
                if (!xIsValid)
                    throw FATXExcepts.ValidExcept;
                return xStartBlock;
            }
        }
        /// <summary>
        /// Entry folder flag
        /// </summary>
        public bool IsFolder
        {
            get
            {
                if (!xIsValid)
                    throw FATXExcepts.ValidExcept;
                return xIsFolder;
            }
        }
        /// <summary>
        /// Entry name
        /// </summary>
        public string Name
        {
            get
            {
                if (!xIsValid)
                    throw FATXExcepts.ValidExcept;
                return xName;
            }
            set
            {
                if (value.Length > 0x2A)
                    value = value.Substring(0, 0x2A);
                xName = value;
                if (xNLen != 0xE5)
                    xNLen = (byte)value.Length;
            }
        }
        /// <summary>
        /// is a FATX partition
        /// </summary>
        public FATXPartition Partition { get { return xPart; } }
        #endregion

        internal FATXEntry(ref FATXEntry xEntry, ref FATXDrive xdrive)
        {
            xOffset = xEntry.xOffset;
            xNLen = xEntry.xNLen;
            xName = xEntry.xName;
            xStartBlock = xEntry.xStartBlock;
            xSize = xEntry.xSize;
            xT1 = xEntry.xT1;
            xT2 = xEntry.xT2;
            xT3 = xEntry.xT3;
            xIsValid = xEntry.xIsValid;
            xIsFolder = xEntry.IsFolder;
            xPart = xEntry.xPart;
            xDrive = xEntry.xDrive;
        }

        internal FATXEntry(long Pos, byte[] xData, ref FATXDrive xdrive)
        {
            xDrive = xdrive;
            xOffset = Pos;
            try
            {
                DJsIO xIO = new DJsIO(xData, true);
                xNLen = xIO.ReadByte();
                if (xNLen == 0xE5 || xNLen == 0xFF || xNLen == 0 || xNLen > 0x2A)
                    return;
                byte xatt = (byte)((xIO.ReadByte() >> 4) & 1);
                byte xLen = (byte)(xNLen & 0x3F);
                xName = xIO.ReadString(StringForm.ASCII, xLen);
                xName.IsValidXboxName();
                xIO.Position = 0x2C;
                xStartBlock = xIO.ReadUInt32();
                if (xStartBlock == Constants.FATX32End)
                    return;
                xSize = xIO.ReadInt32();
                xT1 = xIO.ReadInt32();
                xT2 = xIO.ReadInt32();
                xT3 = xIO.ReadInt32();
                if (xatt == 1)
                    xIsFolder = true;
                else if (xSize == 0)
                    return;
                xIsValid = true;
            }
            catch { xIsValid = false; }
        }

        internal FATXEntry(string xNameIn, uint xStart, int xSizeIn, long xPosition, bool xFolder, ref FATXDrive xdrive)
        {
            int DT = TimeStamps.FatTimeInt(DateTime.Now);
            xT1 = DT;
            xT2 = DT;
            xT3 = DT;
            Name = xNameIn;
            xStartBlock = xStart;
            xSize = (xIsFolder = xFolder) ? 0 : xSizeIn;
            xOffset = xPosition;
            xIsValid = true;
            xDrive = xdrive;
        }

        internal void SetAtts(FATXPartition Part)
        {
            xPart = Part;
        }

        internal byte[] GetData()
        {
            List<byte> xArray = new List<byte>();
            xArray.Add(xNLen);
            xArray.Add((byte)((IsFolder ? 1 : 0) << 4));
            xArray.AddRange(Encoding.ASCII.GetBytes(xName));
            xArray.AddRange(new byte[0x2A - xName.Length]);
            xArray.AddRange(BitConv.GetBytes(xStartBlock, true));
            xArray.AddRange(BitConv.GetBytes(xSize, true));
            xArray.AddRange(BitConv.GetBytes(xT1, true));
            xArray.AddRange(BitConv.GetBytes(xT2, true));
            xArray.AddRange(BitConv.GetBytes(xT3, true));
            return xArray.ToArray();
        }

        internal bool xWriteEntry()
        {
            try
            {
                byte[] xdata = GetData();
                xDrive.GetIO();
                xDrive.xIO.Position = xOffset;
                xDrive.xIO.Write(xdata);
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Writes the entry data
        /// </summary>
        /// <returns></returns>
        public bool WriteEntry()
        {
            if (xDrive.ActiveCheck())
                return false;
            return (xWriteEntry() & !(xDrive.xActive = false));
        }
    }

    /// <summary>
    /// Object to hold FATX File Entry
    /// </summary>
    public sealed class FATXFileEntry : FATXEntry
    {
        internal FATXFileEntry(FATXEntry x, ref FATXDrive xdrive)
            : base(ref x, ref xdrive) { }

        /// <summary>
        /// Overwrite the file
        /// </summary>
        /// <param name="FileIn"></param>
        /// <returns></returns>
        public bool Inject(string FileIn)
        {
            if (xDrive.ActiveCheck())
                return false;
            DJsIO xIOIn = null;
            try { xIOIn = new DJsIO(FileIn, DJFileMode.Open, true); }
            catch { return (xDrive.xActive = false); }
            if (xIOIn == null || !xIOIn.Accessed)
                return (xDrive.xActive = false);
            try { return xInject(xIOIn) & !(xDrive.xActive = false); }
            catch { xIOIn.Close(); return (xDrive.xActive = false); }
        }

        internal bool xInject(DJsIO xIOIn)
        {
            List<uint> blocks = new List<uint>(Partition.xTable.GetBlocks(xStartBlock));
            if (blocks.Count == 0)
                throw new Exception();
            uint xct = xIOIn.BlockCountFATX(Partition);
            if (blocks.Count < xct)
            {
                uint[] blocks2 = Partition.xTable.GetNewBlockChain((uint)(xct - blocks.Count), 1);
                if (blocks2.Length == 0)
                    throw new Exception();
                blocks.AddRange(blocks2);
                uint[] x = blocks.ToArray();
                if (!Partition.xTable.WriteChain(ref x))
                    throw new Exception();
            }
            else if (blocks.Count > xct)
            {
                uint[] xUnneeded = new uint[blocks.Count - xct];
                for (uint i = xct; i < blocks.Count; i++)
                {
                    xUnneeded[(int)i] = i;
                    blocks.RemoveAt((int)i--);
                }
                if (!Partition.xTable.DC(ref xUnneeded))
                    throw new Exception();
            }
            xIOIn.Position = 0;
            xDrive.GetIO();
            foreach (uint i in blocks)
            {
                xDrive.xIO.Position = Partition.BlockToOffset(i);
                xDrive.xIO.Write(xIOIn.ReadBytes(Partition.xBlockSize));
            }
            if ((xSize == 0 || (uint)(((xSize - 1) / Partition.xBlockSize) + 1) != xct) &&
                !Partition.WriteAllocTable())
                throw new Exception();
            xSize = (int)xIOIn.Length;
            xIOIn.Close();
            return xWriteEntry();
        }

        /// <summary>
        /// Replace the file
        /// </summary>
        /// <param name="FileIn"></param>
        /// <returns></returns>
        public bool Replace(string FileIn)
        {
            if (xDrive.ActiveCheck())
                return false;
            DJsIO xIOIn = null;
            try { xIOIn = new DJsIO(FileIn, DJFileMode.Open, true); }
            catch { return (xDrive.xActive = false); }
            if (xIOIn == null || !xIOIn.Accessed)
                return (xDrive.xActive = false);
            return xReplace(xIOIn) & !(xDrive.xActive = false);
        }

        internal bool xReplace(DJsIO xIOIn)
        {
            uint bu = xStartBlock;
            int size = xSize;
            try
            {
                uint[] curblocks = Partition.xTable.GetBlocks(xStartBlock);
                uint[] blocks = Partition.xTable.GetNewBlockChain(xIOIn.BlockCountFATX(Partition), 1);
                if (blocks.Length == 0)
                    throw new Exception();
                if (!Partition.xTable.WriteChain(ref blocks))
                    throw new Exception();
                if (!Partition.xTable.DC(ref curblocks))
                    throw new Exception();
                xIOIn.Position = 0;
                xDrive.GetIO();
                if (!Partition.WriteFile(blocks, ref xIOIn))
                    throw new Exception();
                if (!Partition.WriteAllocTable())
                    throw new Exception();
                base.xStartBlock = blocks[0];
                base.xSize = (int)xIOIn.Length;
                xIOIn.Close();
                return xWriteEntry();
            }
            catch { xIOIn.Close(); base.xStartBlock = bu; base.xSize = size; return false; }
        }

        /// <summary>
        /// Delete the file
        /// </summary>
        /// <returns></returns>
        public bool Delete()
        {
            if (xDrive.ActiveCheck())
                return false;
            try
            {
                uint[] blocks = Partition.xTable.GetBlocks(xStartBlock);
                if (blocks.Length == 0 || !Partition.xTable.DC(ref blocks) || !Partition.WriteAllocTable())
                    return (xDrive.xActive = false);
                xNLen = 0xE5;
                if (!xWriteEntry())
                    return (xDrive.xActive = false);
                return !(xDrive.xActive = false);
            }
            catch { return (xDrive.xActive = false); }
        }

        internal bool xExtract(ref DJsIO xIOOut)
        {
            try
            {
                xIOOut.Position = 0;
                uint[] xChain = Partition.xTable.GetBlocks(xStartBlock);
                uint xct = (uint)(((xSize - 1) / Partition.xBlockSize) + 1);
                if (xChain.Length < xct)
                    return false;
                xDrive.GetIO();
                for (uint i = 0; i < xct - 1; i++)
                {
                    xDrive.xIO.Position = Partition.BlockToOffset(xChain[(int)i]);
                    xIOOut.Write(xDrive.xIO.ReadBytes(Partition.xBlockSize));
                }
                int xleft = (int)(((xSize - 1) % Partition.xBlockSize) + 1);
                xDrive.xIO.Position = Partition.BlockToOffset(xChain[(int)xct - 1]);
                xIOOut.Write(xDrive.xIO.ReadBytes(xleft));
                xIOOut.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Extract the file
        /// </summary>
        /// <param name="OutLocation"></param>
        /// <returns></returns>
        public bool Extract(string OutLocation)
        {
            if (xDrive.ActiveCheck())
                return false;
            bool xReturn = false;
            DJsIO xIO = new DJsIO(true);
            try
            {
                xReturn = xExtract(ref xIO);
                xIO.Close();
                if (xReturn)
                    xReturn = VariousFunctions.MoveFile(xIO.FileNameLong, OutLocation);
            }
            catch
            {
                xIO.Close();
                xReturn = false;
            }
            VariousFunctions.DeleteFile(xIO.FileNameLong);
            xDrive.xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Grabs the STFS name of the package
        /// </summary>
        /// <returns></returns>
        public string GetSTFSName()
        {
            if (xDrive.ActiveCheck())
                return null;
            string xReturn = null;
            try
            {
                if (xSize < 0x500)
                    throw new Exception();
                xDrive.GetIO();
                uint[] blocks = Partition.xTable.GetBlocks(xStartBlock);
                if (blocks.Length == 0)
                    throw new Exception();
                xDrive.xActive = false;
                FATXStreamIO io = new FATXStreamIO(this, ref blocks, true);
                uint xBuff = io.ReadUInt32();
                if (xBuff != (uint)STFS.PackageMagic.CON &&
                    xBuff != (uint)STFS.PackageMagic.LIVE &&
                    xBuff != (uint)STFS.PackageMagic.PIRS)
                    throw new Exception();
                io.Position = 0x411;
                xReturn = io.ReadString(StringForm.Unicode, 0x80);
                io.Position = 0x340;
                byte xbase = (byte)(((io.ReadUInt32() + 0xFFF) & 0xF000) >> 0xC);
                if (io.ReadUInt32() != (uint)STFS.PackageType.Profile)
                    throw new Exception();
                io.Position = 0x379;
                if (io.ReadByte() != 0x24 || io.ReadByte() != 0)
                    throw new Exception();
                byte idx = (byte)(io.ReadByte() & 3);
                byte[] Desc = io.ReadBytes(5);
                if (idx == 0 || idx == 2)
                {
                    if (xbase != 0xA)
                        throw new Exception();
                }
                else if (idx == 1)
                {
                    if (xbase != 0xB)
                        throw new Exception();
                }
                else throw new Exception();
                io.Position = 0x395;
                STFS.STFSDescriptor xDesc = new X360.STFS.STFSDescriptor(Desc, io.ReadUInt32(), io.ReadUInt32(), idx);
                int pos = (int)xDesc.GenerateDataOffset(xDesc.DirectoryBlock);
                uint block = xDesc.DirectoryBlock;
                while (pos != -1)
                {
                    for (int i = 0; i < 0x40; i++)
                    {
                        if (pos == -1)
                            break;
                        io.Position = pos + 0x28 + (0x40 * i);
                        byte nlen = (byte)(io.ReadByte() & 0x3F);
                        if (nlen > 0x28)
                            nlen = 0x28;
                        io.Position = pos + (0x40 * i);
                        if (io.ReadString(StringForm.ASCII, nlen) == "Account")
                        {
                            io.Position = pos + (0x40 * i) + 0x2F;
                            List<byte> buff = new List<byte>(io.ReadBytes(3));
                            buff.Add(0);
                            block = BitConv.ToUInt32(buff.ToArray(), false);
                            pos = -1;
                        }
                    }
                    if (pos != -1)
                    {
                        byte shift = xDesc.TopRecord.Index;
                        if (xDesc.BlockCount >= Constants.BlockLevel[1])
                        {
                            io.Position = (int)xDesc.GenerateHashOffset(block, X360.STFS.TreeLevel.L2) + 0x14 +
                                (shift << 0xC);
                            shift = (byte)((io.ReadByte() >> 6) & 1);
                        }
                        if (xDesc.BlockCount >= Constants.BlockLevel[0])
                        {
                            io.Position = (int)xDesc.GenerateHashOffset(block, X360.STFS.TreeLevel.L1) + 0x14 +
                                (xDesc.ThisType == STFS.STFSType.Type0 ? 0 : (shift << 0xC));
                            shift = (byte)((io.ReadByte() >> 6) & 1);
                        }
                        io.Position = (int)xDesc.GenerateHashOffset(block, X360.STFS.TreeLevel.L0) + 0x15 +
                                (xDesc.ThisType == STFS.STFSType.Type0 ? 0 : (shift << 0xC));
                        List<byte> xbuff = new List<byte>(io.ReadBytes(3));
                        xbuff.Reverse();
                        xbuff.Insert(0, 3);
                        block = BitConv.ToUInt32(xbuff.ToArray(), true);
                        if (block == Constants.STFSEnd)
                            pos = -1;
                    }
                }
                if (block == 0xFFFFFF)
                    throw new Exception();
                io.Position = (int)xDesc.GenerateDataOffset(block);
                byte[] databuff = io.ReadBytes(404);
                Profile.UserAccount ua = new X360.Profile.UserAccount(new DJsIO(databuff, true), X360.Profile.AccountType.Stock, false);
                if (!ua.Success)
                {
                    ua = new X360.Profile.UserAccount(new DJsIO(databuff, true), X360.Profile.AccountType.Kits, false);
                    if (!ua.Success)
                        throw new Exception();
                }
                xReturn = ua.GetGamertag();
                io.Close();
                xDrive.xActive = false;
                return xReturn;
            }
            catch { xDrive.xActive = false; return xReturn; }
        }
    }

    /// <summary>
    /// Object to hold contents of a read folder
    /// </summary>
    public sealed class FATXReadContents
    {
        [CompilerGenerated]
        internal List<FATXFolderEntry> xfolds;
        [CompilerGenerated]
        internal List<FATXFileEntry> xfiles;
        [CompilerGenerated]
        internal List<FATXPartition> xsubparts = new List<FATXPartition>();
        
        /// <summary>
        /// Files
        /// </summary>
        public FATXFileEntry[] Files { get { return xfiles.ToArray(); } }
        /// <summary>
        /// Folders
        /// </summary>
        public FATXFolderEntry[] Folders { get { return xfolds.ToArray(); } }
        /// <summary>
        /// Subpartitions
        /// </summary>
        public FATXPartition[] SubPartitions { get { return xsubparts.ToArray(); } }

        internal FATXReadContents() {}
    }

    /// <summary>
    /// Object to hold FATX Folder
    /// </summary>
    public sealed class FATXFolderEntry : FATXEntry
    {
        internal FATXFolderEntry(FATXEntry xEntry, ref FATXDrive xdrive) : base(ref xEntry, ref xdrive) { }

        /// <summary>
        /// Reads the contents
        /// </summary>
        /// <returns></returns>
        public FATXReadContents Read()
        {
            if (xDrive.ActiveCheck())
                return null;
            FATXReadContents xReturn = xRead();
            xDrive.xActive = false;
            return xReturn;
        }

        internal FATXReadContents xRead()
        {
            FATXReadContents xreturn = new FATXReadContents();
            try
            {
                xDrive.GetIO();
                List<FATXEntry> xEntries = new List<FATXEntry>();
                uint[] xBlocks = Partition.xTable.GetBlocks(xStartBlock);
                for (int i = 0; i < xBlocks.Length; i++)
                {
                    long xCurrent = Partition.BlockToOffset(xBlocks[i]);
                    if (xCurrent == -1)
                        break;
                    for (int x = 0; x < Partition.xEntryCount; x++)
                    {
                        xDrive.xIO.Position = xCurrent + (0x40 * x);
                        FATXEntry z = new FATXEntry((xCurrent + (0x40 * x)), xDrive.xIO.ReadBytes(0x40), ref xDrive);
                        z.SetAtts(Partition);
                        if (z.xIsValid)
                            xEntries.Add(z);
                        else if (z.xNLen != 0xE5)
                                break;
                    }
                }
                xreturn.xfolds = new List<FATXFolderEntry>();
                xreturn.xfiles = new List<FATXFileEntry>();
                for (int i = 0; i < xEntries.Count; i++)
                {
                    if (xEntries[i].IsFolder)
                        xreturn.xfolds.Add(new FATXFolderEntry(xEntries[i], ref xDrive));
                    else xreturn.xfiles.Add(new FATXFileEntry(xEntries[i], ref xDrive));
                }
                return xreturn;
            }
            catch { return (xreturn = null); }
        }

        /// <summary>
        /// Gets a location for a new entry
        /// </summary>
        /// <param name="block"></param>
        /// <returns></returns>
        long GetNewEntryPos(out uint block)
        {
            block = 0;
            List<uint> xFileBlocks = new List<uint>(Partition.xTable.GetBlocks(xStartBlock));
            xDrive.GetIO();
            // Searches current allocated blocks
            for (int x = 0; x < xFileBlocks.Count; x++)
            {
                long xCurOffset = Partition.BlockToOffset(xFileBlocks[x]);
                for (int i = 0; i < Partition.xEntryCount; i++)
                {
                    xDrive.xIO.Position = xCurOffset + (0x40 * i);
                    byte xCheck = xDrive.xIO.ReadByte();
                    if (xCheck == 0 || xCheck > 0x2A || xCheck == 0xFF)
                        return --xDrive.xIO.Position;
                }
            }
            uint[] xBlock = Partition.xTable.GetNewBlockChain(1, 1);
            if (xBlock.Length > 0)
            {
                // Nulls out a new block and returns the start of the new block
                xDrive.xIO.Position = Partition.BlockToOffset(xBlock[0]);
                //List<byte> xbuff = new List<byte>();
                byte[] xnull = new byte[Partition.xBlockSize];
                xDrive.xIO.Write(xnull);
                xFileBlocks.Add(xBlock[0]);
                block = xBlock[0];
                return Partition.BlockToOffset(xBlock[0]); // Returns the beginning of the allocated block
            }
            return -1;
        }

        /* Note: Have plans for safer and better manipulation to prevent
         * minimal block loss to human error */

        /// <summary>
        /// Adds a folder
        /// </summary>
        /// <param name="FolderName"></param>
        /// <returns></returns>
        public bool AddFolder(string FolderName)
        {
            FolderName.IsValidXboxName();
            if (xDrive.ActiveCheck())
                return false;
            try
            {
                FATXReadContents xconts = xRead();
                foreach (FATXFolderEntry x in xconts.xfolds)
                {
                    if (x.Name == FolderName)
                        return (xDrive.xActive = false);
                }
                DJsIO xIOIn = new DJsIO(new byte[Partition.xBlockSize], true);
                uint xnew = 0;
                long xpos = GetNewEntryPos(out xnew);
                if (xpos == -1)
                    return (xDrive.xActive = false);
                uint[] blocks = Partition.xTable.GetNewBlockChain(xIOIn.BlockCountFATX(Partition), xnew + 1);
                if (blocks.Length == 0)
                    return (xDrive.xActive = false);
                if (!Partition.WriteFile(blocks, ref xIOIn))
                    return (xDrive.xActive = false);
                FATXEntry y = new FATXEntry(FolderName, blocks[0], (int)xIOIn.Length, xpos, true, ref xDrive);
                if (!y.xWriteEntry())
                    return (xDrive.xActive = false);
                if (xnew > 0)
                {
                    List<uint> fileblocks = new List<uint>(Partition.xTable.GetBlocks(xStartBlock));
                    fileblocks.Add(xnew);
                    uint[] xtemp = fileblocks.ToArray();
                    if (!Partition.xTable.WriteChain(ref xtemp))
                        return (xDrive.xActive = false);
                }
                if (!Partition.xTable.WriteChain(ref blocks))
                    return (xDrive.xActive = false);
                if (Partition.WriteAllocTable())
                    return !(xDrive.xActive = false);
                return (xDrive.xActive = false);
            }
            catch { return xDrive.xActive = false; }
        }

        /// <summary>
        /// Adds a file
        /// </summary>
        /// <param name="FileName"></param>
        /// <param name="FileLocation"></param>
        /// <param name="xType"></param>
        /// <returns></returns>
        public bool AddFile(string FileName, string FileLocation, AddType xType)
        {
            FileName.IsValidXboxName();
            if (xDrive.ActiveCheck())
                return false;
            DJsIO xIOIn = null;
            try { xIOIn = new DJsIO(FileLocation, DJFileMode.Open, true); }
            catch { return (xDrive.xActive = false); }
            try
            {
                FATXReadContents xconts = xRead();
                foreach (FATXFileEntry x in xconts.xfiles)
                {
                    if (x.Name == FileName)
                    {
                        bool xreturn = false;
                        if (xType == AddType.NoOverWrite)
                             return (xDrive.xActive = false);
                        else if (xType == AddType.Inject)
                            xreturn = x.xInject(xIOIn);
                        else xreturn = x.xReplace(xIOIn);
                        return (xreturn & !(xDrive.xActive = false));
                    }
                }
                uint xnew = 0;
                long xpos = GetNewEntryPos(out xnew);
                if (xpos == -1)
                    return (xDrive.xActive = false);
                uint[] blocks = Partition.xTable.GetNewBlockChain(xIOIn.BlockCountFATX(Partition), xnew + 1);
                if (blocks.Length == 0)
                    return (xDrive.xActive = false);
                if (!Partition.WriteFile(blocks, ref xIOIn))
                    return (xDrive.xActive = false);
                FATXEntry y = new FATXEntry(FileName, blocks[0], (int)xIOIn.Length, xpos, false, ref xDrive);
                if (!y.xWriteEntry())
                    return (xDrive.xActive = false);
                if (xnew > 0)
                {
                    List<uint> fileblocks = new List<uint>(Partition.xTable.GetBlocks(xStartBlock));
                    fileblocks.Add(xnew);
                    uint[] xtemp = fileblocks.ToArray();
                    if (!Partition.xTable.WriteChain(ref xtemp))
                        return (xDrive.xActive = false);
                }
                if (!Partition.xTable.WriteChain(ref blocks))
                    return (xDrive.xActive = false);
                if (Partition.WriteAllocTable())
                    return !(xDrive.xActive = false);
                return (xDrive.xActive = false);
            }
            catch { xIOIn.Close(); return (xDrive.xActive = false); }
        }

        bool xExtract(string xOut, bool Sub)
        {
            if (!VariousFunctions.xCheckDirectory(xOut))
                return false;
            FATXReadContents xread = xRead();
            if (xread == null)
                return false;
            foreach (FATXFileEntry x in xread.Files)
            {
                DJsIO xIOOut = new DJsIO(xOut + "/" + x.Name, DJFileMode.Create, true);
                if (!xIOOut.Accessed)
                    continue;
                x.xExtract(ref xIOOut);
                xIOOut.Dispose();
            }
            if (!Sub)
                return true;
            foreach (FATXFolderEntry x in xread.Folders)
                x.xExtract(xOut + "/" + x.Name, Sub);
            return true;
        }

        /// <summary>
        /// Extracts a file
        /// </summary>
        /// <param name="xOutPath"></param>
        /// <param name="IncludeSubFolders"></param>
        /// <returns></returns>
        public bool Extract(string xOutPath, bool IncludeSubFolders)
        {
            if (xDrive.ActiveCheck())
                return false;
            xOutPath = xOutPath.Replace('\\', '/');
            if (xOutPath[xOutPath.Length - 1] == '/')
                xOutPath += xName;
            else xOutPath += "/" + xName;
            return (xExtract(xOutPath, IncludeSubFolders) &
                !(xDrive.xActive = false));
        }
    }

    /// <summary>
    /// Object to hold a FATX partition
    /// </summary>
    public sealed class FATXPartition
    {
        #region Variables
        [CompilerGenerated]
        long xBase;
        [CompilerGenerated]
        internal int xFATSize;
        [CompilerGenerated]
        uint SectorsPerBlock;
        [CompilerGenerated]
        internal List<FATXFolderEntry> xFolders;
        [CompilerGenerated]
        internal List<FATXFileEntry> xFiles;
        [CompilerGenerated]
        internal FATXType FatType = FATXType.None;
        [CompilerGenerated]
        internal FATXDrive xdrive;
        [CompilerGenerated]
        internal AllocationTable xTable;
        [CompilerGenerated]
        internal List<FATXPartition> xExtParts;
        [CompilerGenerated]
        string xName;

        /// <summary>
        /// Folders
        /// </summary>
        public FATXFolderEntry[] Folders { get { return xFolders.ToArray(); } }
        /// <summary>
        /// Files in the partition
        /// </summary>
        public FATXFileEntry[] Files { get { return xFiles.ToArray(); } }
        /// <summary>
        /// Subpartitions
        /// </summary>
        public FATXPartition[] SubPartitions { get { return xExtParts.ToArray(); } }
        internal long xFATLocale { get { return xBase + 0x1000; } }
        long xDataStart { get { return xBase + 0x1000 + xFATSize; } }
        internal int xBlockSize { get { return (int)(SectorsPerBlock * xdrive.SectorSize); } }
        internal short xEntryCount { get { return (short)(xBlockSize / 0x40); } }
        /// <summary>
        /// Valid instance
        /// </summary>
        public bool IsValid { get { return (xFolders != null && xFiles != null && (xFolders.Count + xFiles.Count != 0)); } }
        /// <summary>
        /// Partition name
        /// </summary>
        public string PartitionName { get { return xName; } }
        #endregion

        internal FATXPartition(long xOffset, long xPartitionSize, FATXDrive xDrive, string xLocaleName)
        {
            xdrive = xDrive;
            xName = xLocaleName;
            xBase = xOffset;
            xDrive.GetIO();
            xDrive.xIO.IsBigEndian = true;
            xDrive.xIO.Position = xOffset;
            if (xDrive.xIO.ReadUInt32() != 0x58544146)
                return;
            xDrive.xIO.ReadBytes(4); // Partition ID
            SectorsPerBlock = xDrive.xIO.ReadUInt32();
            uint blockct = (uint)(xPartitionSize / xBlockSize);
            if (blockct < 0xFFF5)
                FatType = FATXType.FATX16;
            else FatType = FATXType.FATX32;
            uint dirblock = xdrive.xIO.ReadUInt32();
            xFATSize = (int)(blockct * (byte)FatType);
            xFATSize += (0x1000 - (xFATSize % 0x1000));
            xTable = new AllocationTable(new DJsIO(true), (uint)((xPartitionSize - 0x1000 - xFATSize) / xBlockSize), FatType);
            xTable.xAllocTable.Position = 0;
            xDrive.xIO.Position = xFATLocale;
            for (int i = 0; i < xFATSize; i += 0x1000)
                xTable.xAllocTable.Write(xdrive.xIO.ReadBytes(0x1000));
            xTable.xAllocTable.Flush();
            long DirOffset = BlockToOffset(dirblock);
            xFolders = new List<FATXFolderEntry>();
            xFiles = new List<FATXFileEntry>();
            List<FATXEntry> xEntries = new List<FATXEntry>();
            for (byte x = 0; x < xEntryCount; x++)
            {
                xDrive.xIO.Position = DirOffset + (0x40 * x);
                FATXEntry z = new FATXEntry((DirOffset + (0x40 * x)), xdrive.xIO.ReadBytes(0x40), ref xdrive);
                z.SetAtts(this);
                if (z.xIsValid)
                    xEntries.Add(z);
                else if (z.xNLen != 0xE5)
                    break;
            }
            foreach (FATXEntry x in xEntries)
            {
                if (x.IsFolder)
                    xFolders.Add(new FATXFolderEntry(x, ref xdrive));
                else xFiles.Add(new FATXFileEntry(x, ref xdrive));
            }
            xExtParts = new List<FATXPartition>();
            for (int i = 0; i < xFiles.Count; i++)
            {
                if (xFiles[i].Name.ToLower() != "extendedsystem.partition")
                    continue;
                FATXPartition x = new FATXPartition(BlockToOffset(xFiles[i].StartBlock), xFiles[i].Size, xdrive, xFiles[i].Name);
                if (!x.IsValid)
                    continue;
                xExtParts.Add(x);
                xFiles.RemoveAt(i--);
            }
        }

        internal long BlockToOffset(uint xBlock)
        {
            switch (FatType)
            {
                case FATXType.FATX16:
                    return ((xBlock == Constants.FATX16End || xBlock == 0 || xBlock >= xTable.BlockCount) ? -1 : ((long)(xBlock - 1) * (long)xBlockSize) + xDataStart);
                case FATXType.FATX32:
                    return ((xBlock == Constants.FATX32End || xBlock == 0 || xBlock >= xTable.BlockCount) ? -1 : ((long)(xBlock - 1) * (long)xBlockSize) + xDataStart);
                default: return -1;
            }

        }

        internal bool WriteFile(uint[] xChain, ref DJsIO xIOIn)
        {
            try
            {
                xdrive.GetIO();
                for (int i = 0; i < xChain.Length; i++)
                {
                    xdrive.xIO.Position = BlockToOffset(xChain[i]);
                    xIOIn.Position = (i * xBlockSize);
                    xdrive.xIO.Write(xIOIn.ReadBytes(xBlockSize));
                    xdrive.xIO.Flush();
                }
                return true;
            }
            catch { return false; }
        }

        internal void RestoreAllocTable()
        {
            string xfn = xTable.xAllocTable.FileNameLong;
            xTable.xAllocTable.Close();
            xTable.xAllocTable = new DJsIO(xfn, DJFileMode.Create, true);
            xdrive.GetIO();
            xdrive.xIO.Position = xFATLocale;
            xTable.xAllocTable.Write(xdrive.xIO.ReadBytes(xFATSize));
            xTable.xAllocTable.Flush();
        }

        internal bool WriteAllocTable()
        {
            try
            {
                xdrive.GetIO();
                xdrive.xIO.Position = xFATLocale;
                for (int i = 0; i < (((xFATSize - 1) / xdrive.SectorSize) + 1); i++)
                {
                    xTable.xAllocTable.Position = i * xdrive.SectorSize;
                    xdrive.xIO.Write(xTable.xAllocTable.ReadBytes((int)xdrive.SectorSize));
                    xdrive.xIO.Flush();
                }
                return true;
            }
            catch { return false; }
        }
    }

    class AllocationTable
    {
        [CompilerGenerated]
        public DJsIO xAllocTable;
        [CompilerGenerated]
        public uint BlockCount;
        [CompilerGenerated]
        FATXType PartitionType;

        public AllocationTable(DJsIO xIOIn, uint xCount, FATXType xType)
        {
            xAllocTable = xIOIn;
            BlockCount = xCount;
            PartitionType = xType;
        }

        internal bool DC(ref uint[] xChain)
        {
            if (PartitionType == FATXType.None)
                return false;
            try
            {
                for (int i = 0; i < xChain.Length; i++)
                {
                    if (xChain[i] >= BlockCount || xChain[i] == 0)
                        continue;
                    for (int x = 0; x < (byte)PartitionType; x++)
                        xAllocTable.Write(0);
                }
                return true;
            }
            catch { return false; }
        }

        uint GetNextBlock(uint xBlock)
        {
            if (PartitionType == FATXType.None)
                return Constants.FATX32End;
            xAllocTable.Position = (xBlock * (byte)PartitionType);
            List<byte> xList = xAllocTable.ReadBytes((byte)PartitionType).ToList();
            for (int i = (int)PartitionType; i < 4; i++)
                xList.Insert(0, 0);
            return BitConv.ToUInt32(xList.ToArray(), true);
        }

        internal uint[] GetBlocks(uint xBlock)
        {
            List<uint> xReturn = new List<uint>();
            while (xBlock < BlockCount && xBlock != 0)
            {
                switch (PartitionType)
                {
                    case FATXType.FATX16:
                        if (xBlock == Constants.FATX16End)
                            return xReturn.ToArray();
                        break;
                    case FATXType.FATX32:
                        if (xBlock == Constants.FATX32End)
                            return xReturn.ToArray();
                        break;
                    default: return xReturn.ToArray();
                }
                if (!xReturn.Contains(xBlock))
                    xReturn.Add(xBlock);
                else break;
                xBlock = GetNextBlock(xBlock);
            }
            return xReturn.ToArray();
        }
        
        internal bool WriteChain(ref uint[] xChain)
        {
            if (PartitionType == FATXType.None)
                return false;
            try
            {
                for (int i = 0; i < xChain.Length; i++)
                {
                    xAllocTable.Position = (xChain[i] * (byte)PartitionType);
                    uint xblock = (i < (xChain.Length - 1)) ?
                        xChain[i + 1] : Constants.FATX32End;
                    switch (PartitionType)
                    {
                        case FATXType.FATX16: xAllocTable.Write((ushort)xblock); break;

                        case FATXType.FATX32: xAllocTable.Write(xblock); break;

                        default: break;
                    }
                }
                return true;
            }
            catch { return false; }
        }
        
        internal uint[] GetNewBlockChain(uint xCount, uint xBlockStart)
        {
            List<uint> xReturn = new List<uint>();
            for (uint i = xBlockStart; i < BlockCount; i++)
            {
                if (xReturn.Count == xCount)
                    return xReturn.ToArray();
                xAllocTable.Position = ((byte)PartitionType * i);
                uint xCheck = Constants.FATX32End;
                switch (PartitionType)
                {
                    case FATXType.FATX16: xCheck = xAllocTable.ReadUInt16(); break;

                    case FATXType.FATX32: xCheck = xAllocTable.ReadUInt32(); break;

                    default: break;
                }
                if (xCheck == 0)
                    xReturn.Add(i);
            }
            return new uint[0];
        }
    }
}
