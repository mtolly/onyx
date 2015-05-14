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
using System.Diagnostics;
using X360.IO;
using X360.Other;
using X360.Security.Cryptography;

namespace X360.GDFX
{
    /// <summary>
    /// GDF Exceptions
    /// </summary>
    [DebuggerStepThrough]
    public static class GDFExceptions
    {
        [CompilerGenerated]
        static readonly Exception xNotGDF = new Exception("Not a GDF Image");

        /// <summary>
        /// Not a GDFX image
        /// </summary>
        public static Exception NotGDF { get { return xNotGDF; }}
    }

    /// <summary>
    /// GDFX signatures
    /// </summary>
    [DebuggerStepThrough]
    public static class GDFMagic
    {
        /// <summary>
        /// MICROSOFT*XBOX*MEDIA
        /// </summary>
        public static readonly byte[] XMedia = new byte[] { 0x4D, 0x49, 0x43, 0x52, 0x4F, 0x53, 0x4F,
            0x46, 0x54, 0x2A, 0x58, 0x42, 0x4F, 0x58, 0x2A, 0x4D, 0x45, 0x44, 0x49, 0x41 };
        /// <summary>
        /// XBOX_DVD_LAYOUT_TOOL_SIG
        /// </summary>
        public static readonly byte[] XDVDLayout = new byte[] { 0x58, 0x42, 0x4F, 0x58, 0x5F, 0x44, 0x56, 0x44, 0x5F, 0x4C, 0x41,
            0x59, 0x4F, 0x55, 0x54, 0x5F, 0x54, 0x4F, 0x4F, 0x4C, 0x5F, 0x53, 0x49, 0x47 };
    }

    /// <summary>
    /// GDFX File attributes
    /// </summary>
    [Flags]
    public enum GDFAttributes
    {
        /// <summary>
        /// No attributes
        /// </summary>
        None,
        /// <summary>
        /// Read only
        /// </summary>
        ReadOnly,
        /// <summary>
        /// Hidden
        /// </summary>
        Hidden,
        /// <summary>
        /// System
        /// </summary>
        System = 4,
        // 8 Unused?
        /// <summary>
        /// Directory
        /// </summary>
        Directory = 0x10,
        /// <summary>
        /// Archived
        /// </summary>
        Archived = 0x20,
        // 0x40 FileAttributes for computers is device so prolly not used
        /// <summary>
        /// Normal
        /// </summary>
        Normal = 0x80
    }
    
    /// <summary>
    /// Object to hold a GDF entry
    /// </summary>
    public class GDFEntry
    {
        [CompilerGenerated]
        GDFAttributes Atts = GDFAttributes.None;
        [CompilerGenerated]
        string xName = "";
        [CompilerGenerated]
        internal int xSize;
        [CompilerGenerated]
        internal uint xStartBlock;
        [CompilerGenerated]
        internal GDFImage xref;
        [CompilerGenerated]
        internal long entryoffset;
        /// <summary>
        /// Attributes
        /// </summary>
        public GDFAttributes EntryAttributes { get { return Atts; } }
        /// <summary>
        /// Name
        /// </summary>
        public string Name { get { return xName; } }
        /// <summary>
        /// Size
        /// </summary>
        public int Size { get { return xSize; } }

        internal GDFEntry(GDFImage xIn)
        {
            DJsIO xIO = xIn.xIO;
            entryoffset = xIO.Position;
            xStartBlock = xIO.ReadUInt32(false);
            xSize = xIO.ReadInt32(false);
            Atts = (GDFAttributes)xIO.ReadByte();
            byte nlen = xIO.ReadByte();
            if (nlen != 0xFF)
                (xName = xIO.ReadString(StringForm.ASCII, nlen)).IsValidXboxName();
            if (xName == "")
                throw new Exception("No Name");
            xName.IsValidXboxName();
            xref = xIn;
        }

        internal GDFEntry(GDFEntry xIn)
        {
            xref = xIn.xref;
            xName = xIn.xName;
            xSize = xIn.xSize;
            Atts = xIn.Atts;
            xStartBlock = xIn.xStartBlock;
        }

        internal GDFEntry() { }
    }

    /// <summary>
    /// Object to hold a GDF File
    /// </summary>
    public class GDFFile : GDFEntry
    {
        internal GDFFile(GDFEntry xIn) : base(xIn) { }

        internal bool xExtract(DJsIO xIO)
        {
            if (xSize == 0)
                return true;
            try
            {
                xIO.Position = 0;
                xref.xIO.Position = xref.GenerateDataOffset(xStartBlock);
                int ct = (((xSize - 1) / GDFImage.blocksize) + 1);
                for (int i = 0; i < (ct - 1); i++)
                    xIO.Write(xref.xIO.ReadBytes(GDFImage.blocksize));
                xIO.Write(xref.xIO.ReadBytes(((xSize - 1) % GDFImage.blocksize) + 1));
                xIO.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Extract a file
        /// </summary>
        /// <param name="OutLocale"></param>
        /// <returns></returns>
        public bool Extract(string OutLocale)
        {
            if (!xref.ActiveCheck())
                return false;
            DJsIO xIO = new DJsIO(true);
            if (!xExtract(xIO))
            {
                xIO.Close();
                VariousFunctions.DeleteFile(xIO.FileNameLong);
                return (xref.xActive = false);
            }
            xIO.Close();
            bool xSuccess = VariousFunctions.MoveFile(xIO.FileNameLong, OutLocale);
            VariousFunctions.DeleteFile(xIO.FileNameLong);
            return (xSuccess & !(xref.xActive = false));
        }

        /// <summary>
        /// Overwrite the file
        /// </summary>
        /// <param name="xIO"></param>
        /// <returns></returns>
        public bool Inject(DJsIO xIO)
        {
            if (!xref.ActiveCheck())
                return false;
            return (xInject(xIO) & !(xref.xActive = false));
        }

        /// <summary>
        /// Overwrite the file
        /// </summary>
        /// <param name="FileLocale"></param>
        /// <returns></returns>
        public bool Inject(string FileLocale)
        {
            if (!xref.ActiveCheck())
                return false;
            DJsIO xIO = null;
            bool success = false;
            try
            {
                xIO = new DJsIO(FileLocale, DJFileMode.Open, true);
                if (!xIO.Accessed)
                    throw new Exception();
                success = xInject(xIO);
            }
            catch { success = false; }
            if (xIO != null)
                xIO.Dispose();
            return (success & !(xref.xActive = false));
        }

        internal bool xInject(DJsIO xIO)
        {
            try
            {
                if (xIO.Length == 0)
                    return true;
                xIO.Position = 0;
                xref.xIO.Position = xref.GenerateDataOffset(xStartBlock);
                int ct = (int)(((xIO.Length - 1) / GDFImage.blocksize) + 1);
                for (int i = 0; i < ct; i++)
                {
                    if (i < (ct - 1))
                        xref.xIO.Write(xIO.ReadBytes(GDFImage.blocksize));
                    else xref.xIO.Write(xIO.ReadBytes((int)(((xIO.Length - 1) % GDFImage.blocksize) + 1)));
                }
                xref.xIO.Position = (entryoffset + 4);
                xref.xIO.Write((int)xIO.Length, false);
                xref.xIO.Flush();
                return true;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// GDF folder entry
    /// </summary>
    public class GDFFolder : GDFEntry
    {
        internal GDFFolder(GDFEntry xIn) : base(xIn) { }

        internal GDFFolder(int size, uint xBlock, GDFImage Ref) { xSize = size; xStartBlock = xBlock; xref = Ref; }
        
        /// <summary>
        /// Reads the folder
        /// </summary>
        /// <returns></returns>
        public GDFContents Read()
        {
            if (!xref.ActiveCheck())
                return new GDFContents();
            GDFContents xReturn = xRead();
            xref.xActive = false;
            return xReturn;
        }

        internal GDFContents xRead() { return xref.ReadFolder(xStartBlock, xSize); }

        /// <summary>
        /// Extract the folder
        /// </summary>
        /// <param name="OutLocation"></param>
        /// <param name="SubItems"></param>
        /// <returns></returns>
        public bool Extract(string OutLocation, bool SubItems)
        {
            if (!xref.ActiveCheck())
                return false;
            return (xExtract(OutLocation, SubItems) & !(xref.xActive = false));
        }

        internal bool xExtract(string OutLocation, bool SubItems)
        {
            if (OutLocation == null || OutLocation == "")
                return false;
            OutLocation = OutLocation.xExtractLegitPath();
            if (!VariousFunctions.xCheckDirectory(OutLocation))
                return false;
            GDFContents xConts = xRead();
            foreach (GDFFile x in xConts.Files)
            {
                DJsIO y = new DJsIO(true);
                try
                {
                    if (x.xExtract(y))
                    {
                        y.Close();
                        VariousFunctions.MoveFile(y.FileNameLong, OutLocation + "/" + x.Name);
                    }
                }
                catch { y.Close(); }
                VariousFunctions.DeleteFile(y.FileNameLong);
            }
            foreach (GDFFolder x in xConts.xFolders)
            {
                try { x.xExtract(OutLocation + "/" + x.Name, SubItems); }
                catch { }
            }
            return true;
        }
    }

    /// <summary>
    /// Object to hold GDF Folder contents
    /// </summary>
    public class GDFContents
    {
        [CompilerGenerated]
        internal List<GDFFile> xFiles = new List<GDFFile>();
        [CompilerGenerated]
        internal List<GDFFolder> xFolders = new List<GDFFolder>();

        /// <summary>
        /// Files
        /// </summary>
        public GDFFile[] Files { get { return xFiles.ToArray(); } }
        /// <summary>
        /// Folders
        /// </summary>
        public GDFFolder[] Folders { get { return xFolders.ToArray(); } }

        internal GDFContents() { }
    }

    /// <summary>
    /// Object to hold data of a GDF file
    /// </summary>
    public class GDFImage
    {
        [CompilerGenerated]
        internal const ushort blocksize = 0x800;
        [CompilerGenerated]
        internal DJsIO xIO;
        [CompilerGenerated]
        byte[] unknown;
        [CompilerGenerated]
        uint xDeviation;
        [CompilerGenerated]
        GDFFolder xRoot;
        [CompilerGenerated]
        internal long baseoffset = 0;
        [CompilerGenerated]
        internal bool xActive = true;

        /// <summary>
        /// Deviation
        /// </summary>
        public uint Deviation { get { return xDeviation; } }

        /// <summary>
        /// Is a valid image
        /// </summary>
        public bool Valid { get { return xIO != null; } }

        internal bool ActiveCheck()
        {
            if (xIO == null || !xIO.Accessed || xActive)
                return false;
            return (xActive = true);
        }

        /// <summary>
        /// Root folder
        /// </summary>
        public GDFFolder Root { get { return xRoot; } }

        /// <summary>
        /// Grabs image from an IO
        /// </summary>
        /// <param name="xIOIn"></param>
        public GDFImage(DJsIO xIOIn) : this(xIOIn, 0) { }

        /// <summary>
        /// Grabs an image from a file location
        /// </summary>
        /// <param name="FileLocation"></param>
        public GDFImage(string FileLocation) : this(FileLocation, 0) { }

        /// <summary>
        /// Grabs an image from a file location and specified deviation
        /// </summary>
        /// <param name="FileLocation"></param>
        /// <param name="Deviation"></param>
        public GDFImage(string FileLocation, uint Deviation) :
            this(new DJsIO(FileLocation, DJFileMode.Open, true), Deviation) { }
        
        /// <summary>
        /// Grabs an image from an IO and specified Deviation
        /// </summary>
        /// <param name="xIOIn"></param>
        /// <param name="Deviation"></param>
        public GDFImage(DJsIO xIOIn, uint Deviation)
        {
            if (!xIOIn.Accessed)
                throw IOExcepts.AccessError;
            xIOIn.Position = 0;
            new System.Threading.Thread(new System.Threading.ParameterizedThreadStart(System.DLLIdentify.PrivilegeCheck)).Start(System.Threading.Thread.CurrentThread);
            if (xIOIn.ReadUInt32() == (uint)AllMagic.XSF)
            {
                xIOIn.Position = baseoffset = 0x10000;
                if (xIOIn.ReadBytes(20).HexString() != GDFMagic.XMedia.HexString())
                    throw GDFExceptions.NotGDF;
            }
            else
            {
                xIOIn.Position = baseoffset = 0;
                if (xIOIn.ReadBytes(20).HexString() != GDFMagic.XMedia.HexString())
                {
                    if (xIOIn.Length < 0x1FB20)
                        throw GDFExceptions.NotGDF;
                    xIOIn.Position = baseoffset = 0x1FB20;
                    if (xIOIn.ReadBytes(20).HexString() != GDFMagic.XMedia.HexString())
                    {
                        if (xIOIn.Length < 0x30600)
                            throw GDFExceptions.NotGDF;
                        xIOIn.Position = baseoffset = 0x30600;
                        if (xIOIn.ReadBytes(20).HexString() != GDFMagic.XMedia.HexString())
                        {
                            if (xIO.Length < 0xFDA0000)
                                throw GDFExceptions.NotGDF;
                            xIOIn.Position = baseoffset = 0xFDA0000;
                            if (xIOIn.ReadBytes(20).HexString() != GDFMagic.XMedia.HexString())
                                throw GDFExceptions.NotGDF;
                        }
                    }
                }
            }
            uint xStartBlock = xIOIn.ReadUInt32(false);
            int xStartSize = xIOIn.ReadInt32(false);
            unknown = xIOIn.ReadBytes(9);
            xIO = xIOIn;
            xRoot = new GDFFolder(xStartSize, xStartBlock, this);
            xDeviation = Deviation;
            xActive = false;
        }

        // Implement later
        internal GDFImage(CreateGDF xSession, bool AllocateSecuritySector, string OutLocale)
        {
            
        }

        internal long GenerateDataOffset(uint xBlock)
        {
            long xReturn = (xBlock * blocksize);
            if (xDeviation != 0)
                xReturn -= ((xDeviation - 1) << 0xC);
            if (baseoffset > 0x10000) // If it's an actual ISO
                xReturn += baseoffset;
            return xReturn;
        }

        internal GDFContents ReadFolder(uint xBlock, int size)
        {
            List<GDFEntry> xents = new List<GDFEntry>();
            List<ushort> numz = new List<ushort>(new ushort[] { 0 });
            long offset = GenerateDataOffset(xBlock);
            long barrier = offset + size;
            for (int i = 0; i < numz.Count; i++)
            {
                long pos = (offset + (4 * numz[i]));
                if (pos > barrier)
                    continue;
                xIO.Position = pos;
                ushort num = xIO.ReadUInt16(false);
                if (!numz.Contains(num))
                    numz.Add(num);
                num = xIO.ReadUInt16(false);
                if (!numz.Contains(num))
                    numz.Add(num);
                try { xents.Add(new GDFEntry(this)); }
                catch { }
            }
            GDFContents xReturn = new GDFContents();
            foreach (GDFEntry x in xents)
            {
                if ((x.EntryAttributes & GDFAttributes.Directory) == GDFAttributes.Directory)
                    xReturn.xFolders.Add(new GDFFolder(x));
                else xReturn.xFiles.Add(new GDFFile(x));
            }
            return xReturn;
        }

        /// <summary>
        /// Get a file from a path
        /// </summary>
        /// <param name="Path"></param>
        /// <param name="Parent"></param>
        /// <returns></returns>
        public GDFFile GetFile(string Path, out GDFFolder Parent)
        {
            Parent = null;
            if (!ActiveCheck())
                return null;
            if (Path == null || Path == "")
            {
                xActive = false;
                return null;
            }
            Path = Path.xExtractLegitPath();
            List<string> folders = Path.Split(new char[] { '/' }).ToList();
            foreach (string x in folders)
            {
                try { x.IsValidXboxName(); }
                catch { xActive = false; return null; }
            }
            string file = folders[folders.Count - 1];
            folders.RemoveAt(folders.Count - 1);
            Parent = Root;
            bool found = false;
            for (int i = 0; i < folders.Count; i++)
            {
                GDFContents xRead = Parent.xRead();
                found = false;
                foreach (GDFFolder x in xRead.xFolders)
                {
                    if (x.Name.ToLower() == folders[i].ToLower())
                    {
                        Parent = x;
                        found = true;
                        break;
                    }
                }
                if (!found)
                {
                    Parent = null;
                    xActive = false;
                    return null;
                }
            }
            GDFContents rd = Parent.xRead();
            foreach (GDFFile x in rd.xFiles)
            {
                if (x.Name.ToLower() == file.ToLower())
                {
                    xActive = false;
                    return x;
                }
            }
            xActive = false;
            return null;
        }

        /// <summary>
        /// IO File Name
        /// </summary>
        public string FileNameLong { get { return xIO.FileNameLong; } }

        /// <summary>
        /// Close the Image IO
        /// </summary>
        /// <returns></returns>
        public bool Close()
        {
            if (!ActiveCheck())
                return false;
            xIO.Close();
            return true;
        }
    }
}