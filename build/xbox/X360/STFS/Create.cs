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
using X360.IO;
using X360.Profile;
using X360.Other;
using X360.Security.Cryptography;
using X360.IO.STFSExtensions;

namespace X360.STFS
{
    /// <summary>
    /// Class to hold creation entries
    /// </summary>
    public class CItemEntry
    {
        [CompilerGenerated]
        internal CreateSTFS create;
        [CompilerGenerated]
        internal string xthispath;

        /// <summary>
        /// NOT CASE SENSITIVE
        /// </summary>
        public string Path { get { return xthispath; } }

        internal string getparentpath()
        {
            int idx = xthispath.LastIndexOf('/');
            if (idx == -1)
                return "";
            return xthispath.Substring(0, idx).ToLower();
        }
        /// <summary>
        /// Item name
        /// </summary>
        public string Name
        {
            get { return xthispath.xExtractName(); }
            set
            {
                value.IsValidXboxName();
                if (value.Length > 0x28)
                    value = value.Substring(0, 0x28);
                int idx = xthispath.LastIndexOf('/');
                if (idx == -1)
                    xthispath = value;
                else xthispath = xthispath.Substring(0, idx) + "/" + value;
            }
        }

        internal CItemEntry(string path, ref CreateSTFS xCreate)
        {
            xthispath = path;
            create = xCreate;
        }
    }

    static class CreateTools
    {
        public static uint BlockCount(string file)
        {
            if (!File.Exists(file))
                return Constants.STFSEnd;
            long len = new FileInfo(file).Length;
            return (uint)(((len - 1) / 0x1000) + 1);
        }
    }

    /// <summary>
    /// Class to hold file entries
    /// </summary>
    public sealed class CFileEntry : CItemEntry
    {
        [CompilerGenerated]
        string filelocale = "";

        /// <summary>
        /// Item location
        /// </summary>
        public string FileLocale { get { return filelocale; } }
        /// <summary>
        /// Block count of file
        /// </summary>
        /// <returns></returns>
        public uint BlockCount() { return CreateTools.BlockCount(filelocale); }

        /// <summary>
        /// Grabs the length of the item
        /// </summary>
        /// <returns></returns>
        public int GetLength() { return (int)new FileInfo(filelocale).Length; }

        internal CFileEntry(string xFile, string path, CreateSTFS xCreate)
            : base(path, ref xCreate) { filelocale = xFile; }

        internal CFileEntry(string xFile, string path, ref CreateSTFS xCreate)
            : base(path, ref xCreate) { filelocale = xFile; }
    }

    /// <summary>
    /// Object to hold directory entries
    /// </summary>
    public sealed class CFolderEntry : CItemEntry
    {
        internal CFolderEntry(string path, CreateSTFS xCreate)
            : base(path, ref xCreate) { }

        internal CFolderEntry(string path, ref CreateSTFS xCreate)
            : base(path, ref xCreate){ }

        /// <summary>
        /// Grabs the files under the directory
        /// </summary>
        /// <returns></returns>
        public CFileEntry[] GetFiles()
        {
            List<CFileEntry> xReturn = new List<CFileEntry>();
            foreach (CFileEntry x in create.xFileDirectory)
            {
                if (x.getparentpath() == xthispath.ToLower())
                    xReturn.Add(x);
            }
            return xReturn.ToArray();
        }
    }

    /// <summary>
    /// Sphere colors
    /// </summary>
    public enum SphereColor : byte
    {
        /// <summary>
        /// Default
        /// </summary>
        Default,
        /// <summary>
        /// Gray
        /// </summary>
        Gray,
        /// <summary>
        /// Black
        /// </summary>
        Black,
        /// <summary>
        /// Red-Pink
        /// </summary>
        RedPink,
        /// <summary>
        /// Yellow
        /// </summary>
        Yellow,
        /// <summary>
        /// Blue-Green
        /// </summary>
        BlueGreen,
        /// <summary>
        /// Baby Blue
        /// </summary>
        BabyBlue,
        /// <summary>
        /// Gray-Blue
        /// </summary>
        GrayBlue,
        /// <summary>
        /// Highlight Pink
        /// </summary>
        HighlightPink,
        /// <summary>
        /// Tan
        /// </summary>
        Tan,
        /// <summary>
        /// Brown
        /// </summary>
        Brown,
        /// <summary>
        /// Gold
        /// </summary>
        Gold,
        /// <summary>
        /// Green
        /// </summary>
        Green,
        /// <summary>
        /// Magenta
        /// </summary>
        Magenta,
        /// <summary>
        /// Blue
        /// </summary>
        Blue,
        /// <summary>
        /// Violet
        /// </summary>
        Violet,
        /// <summary>
        /// Light Gray
        /// </summary>
        LightGray
    }

    /// <summary>
    /// Dash Style
    /// </summary>
    public enum DashStyle : byte
    {
        /// <summary>
        /// Default
        /// </summary>
        Default,
        /// <summary>
        /// Dark
        /// </summary>
        Dark,
        /// <summary>
        /// Light
        /// </summary>
        Light }

    /// <summary>
    /// Theme Params
    /// </summary>
    public sealed class ThemeParams
    {
        /// <summary>
        /// Sphere color of instance
        /// </summary>
        public SphereColor Sphere { get; set; }
        /// <summary>
        /// Avatar Lighting Direction 0
        /// </summary>
        public decimal AvatarLightingDirectional0 { get; set; }
        /// <summary>
        /// Avatar Lighting Direction 1
        /// </summary>
        public decimal AvatarLightingDirectional1 { get; set; }
        /// <summary>
        /// Avatar Lighting Direction 2
        /// </summary>
        public decimal AvatarLightingDirectional2 { get; set; }
        /// <summary>
        /// Avatar Lighting Direction 3
        /// </summary>
        public uint AvatarLightingDirectional3 { get; set; }
        /// <summary>
        /// Avatar Ambient
        /// </summary>
        public uint AvatarLightingAmbient { get; set; }
        /// <summary>
        /// Style Type
        /// </summary>
        public DashStyle StyleType { get; set; }
        /// <summary>
        /// Creates an instance of this object
        /// </summary>
        public ThemeParams()
        {
            Sphere = SphereColor.Default;
            StyleType = DashStyle.Default;
            AvatarLightingAmbient = 0x383838FF;
            AvatarLightingDirectional0 = (decimal)-0.5;
            AvatarLightingDirectional1 = (decimal)-0.6123;
            AvatarLightingDirectional2 = (decimal)-1;
            AvatarLightingDirectional3 = 0xB49664FF;
        }
    }

    /// <summary>
    /// Object to create a package
    /// </summary>
    public sealed class CreateSTFS
    {
        [CompilerGenerated]
        uint[] xBckStp = { 0xAA, 0x70E4, 0 };
        [CompilerGenerated]
        internal List<CFileEntry> xFileDirectory = new List<CFileEntry>();
        [CompilerGenerated]
        internal List<CFolderEntry> xFolderDirectory = new List<CFolderEntry>();
        [CompilerGenerated]
        STFSType xStruct = STFSType.Type0;
        /// <summary>
        /// Header meta info
        /// </summary>
        [CompilerGenerated]
        public HeaderData HeaderData = new HeaderData();
        [CompilerGenerated]
        ThemeParams xtheme = new ThemeParams();
        [CompilerGenerated]
        CFolderEntry root;

        /// <summary>
        /// Root Path
        /// </summary>
        public CFolderEntry RootPath { get { return root; } }
        /// <summary>
        /// STFSType
        /// </summary>
        public STFSType STFSType
        {
            get { return xStruct; }
            set
            {
                if (value == STFSType.Type0 || value == STFSType.Type1)
                    xStruct = value;
                else xStruct = STFSType.Type0;
            }
        }
        internal uint[] BlockStep
        {
            get
            {
                xBckStp[2] = (uint)((xStruct == STFSType.Type0) ? 0xFE7DA : 0xFD00B);
                return xBckStp;
            }
        }
        /// <summary>
        /// Theme Settings
        /// </summary>
        public ThemeParams ThemeSettings { get { return xtheme; } }

        // Subtract 1 for prevention of Modular error
        internal byte GetDirectoryCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }

        short UppedDirectCount { get { return (byte)(((xFileDirectory.Count + xFolderDirectory.Count - 1) / 0x40) + 1); } }

        internal uint TotalBlocks
        {
            get
            {
                uint xReturn = GetDirectoryCount;
                foreach (CFileEntry x in xFileDirectory)
                    xReturn += x.BlockCount();
                return xReturn;
            }
        }

        uint UppedTotalBlocks(uint xFileAdd) { return (uint)(UppedDirectCount + xFileAdd); }
        /// <summary>
        /// Initializes an instance of this project
        /// </summary>
        public CreateSTFS() { root = new CFolderEntry("", this); }
        /// <summary>
        /// Adds a file via location and its path
        /// </summary>
        /// <param name="FileLocation"></param>
        /// <param name="FilePath"></param>
        /// <returns></returns>
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
        /// <summary>
        /// Adds a folder
        /// </summary>
        /// <param name="FolderPath"></param>
        /// <returns></returns>
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

        bool containspath(string path)
        {
            foreach (CFolderEntry x in xFolderDirectory)
            {
                if (x.xthispath.ToLower() == path.ToLower())
                    return true;
            }
            return false;
        }

        bool containsfile(string path)
        {
            foreach (CFolderEntry x in xFolderDirectory)
            {
                if (x.xthispath.ToLower() == path.ToLower())
                    return true;
            }
            return false;
        }
        /// <summary>
        /// Deletes a folder
        /// </summary>
        /// <param name="FolderPath"></param>
        /// <returns></returns>
        public bool DeleteFolder(string FolderPath)
        {
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
        }

        /// <summary>
        /// Deleetes a file
        /// </summary>
        /// <param name="FilePath"></param>
        /// <returns></returns>
        public bool DeleteFile(string FilePath)
        {
            FilePath = FilePath.xExtractLegitPath();
            for (int i = 0; i < xFileDirectory.Count; i++)
            {
                if (xFileDirectory[i].xthispath == FilePath.ToLower())
                    xFileDirectory.RemoveAt(i--);
            }
            return true;
        }
        /// <summary>
        /// Grabs a file via path
        /// </summary>
        /// <param name="FilePath"></param>
        /// <returns></returns>
        public CFileEntry GetFile(string FilePath)
        {
            FilePath = FilePath.xExtractLegitPath();
            foreach (CFileEntry x in xFileDirectory)
            {
                if (x.Path.ToLower() == FilePath.ToLower())
                    return x;
            }
            return null;
        }

        /// <summary>
        /// Grabs a folder via path
        /// </summary>
        /// <param name="FolderPath"></param>
        /// <returns></returns>
        public CFolderEntry GetFolder(string FolderPath)
        {
            FolderPath = FolderPath.xExtractLegitPath();
            if (FolderPath == "")
                return root;
            foreach (CFolderEntry x in xFolderDirectory)
            {
                if (x.Path.ToLower() == FolderPath.ToLower())
                    return x;
            }
            return null;
        }
    }
}