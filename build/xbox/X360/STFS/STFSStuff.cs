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
using System.Runtime.CompilerServices;
using System.Text;
using System.Security.Cryptography;
using System.Diagnostics;
using X360.IO;
using X360.Security.Cryptography;
using X360.Other;

namespace X360
{
    /// <summary>
    /// File manipulation type
    /// </summary>
    public enum AddType : byte
    {
        /// <summary>
        /// Do not over write the file if existant
        /// </summary>
        NoOverWrite,
        /// <summary>
        /// Over write the file
        /// </summary>
        Inject,
        /// <summary>
        /// Safely write the new file
        /// </summary>
        Replace
    }
}

namespace X360.STFS
{
    /// <summary>
    /// Xbox Languages
    /// </summary>
    public enum Languages : byte
    {
        /// <summary>
        /// English
        /// </summary>
        English = 0,
        /// <summary>
        /// Japanese
        /// </summary>
        Japanese,
        /// <summary>
        /// German
        /// </summary>
        German,
        /// <summary>
        /// French
        /// </summary>
        French,
        /// <summary>
        /// Spanish
        /// </summary>
        Spanish,
        /// <summary>
        /// Itialian
        /// </summary>
        Italian,
        /// <summary>
        /// Korean
        /// </summary>
        Korean,
        /// <summary>
        /// Chinese
        /// </summary>
        Chinese,
        /// <summary>
        /// Portuguese 
        /// </summary>
        Portuguese
    }

    internal enum HashStatus : byte { Unused = 0, Old, New, Reused }

    internal enum HashFlag : byte
    {
        Unallocated = 0,
        AllocatedFree,
        AllocatedInUseOld,
        AllocatedInUseCurrent
    }

    /// <summary>
    /// STFS structure type
    /// </summary>
    public enum STFSType
    {
        /// <summary>Type 0</summary>
        Type0 = 0,
        /// <summary>Type 1</summary>
        Type1
    }

    /// <summary>
    /// Xbox Package type
    /// </summary>
    public enum PackageType : uint
    {
        /// <summary>No package type</summary>
        None = 0,
        /// <summary>Game save</summary>
        SavedGame,
        /// <summary>Market place item</summary>
        MarketPlace,
        /// <summary>Unknown</summary>
        Publisher,
        /// <summary>IPTV DVR</summary>
        IPTV_DVR = 0xFFD,
        /// <summary>Unknown</summary>
        Xbox360Title = 0x1000,
        /// <summary>IPTV Buffer</summary>
        IPTV_PauseBuffer = 0x2000,
        /// <summary>XNA Game?</summary>
        XNACommunity = 0x3000,
        /// <summary>Hard drive installed game</summary>
        HDDInstalledGame = 0x4000,
        /// <summary>Original game</summary>
        OriginalXboxGame = 0x5000,
        /// <summary>Unknown</summary>
        SocialTitle = 0x6000,
        /// <summary>Games on demand</summary>
        GamesOnDemand = 0x7000,
        /// <summary>Unknown</summary>
        SystemPacks = 0x8000,
        /// <summary>Avatar item</summary>
        AvatarItem = 0x9000,
        /// <summary>Xbox 360 title</summary>
        Profile = 0x10000,
        /// <summary>Xbox profile gamerpictures</summary>
        GamerPicture = 0x20000,
        /// <summary>Xbox theme skin</summary>
        ThematicSkin = 0x30000,
        /// <summary>System cache?</summary>
        Cache = 0x40000,
        /// <summary>Unknown</summary>
        StorageDownload = 0x50000,
        /// <summary>Unknown</summary>
        XboxSavedGame = 0x60000,
        /// <summary>Unknown</summary>
        XboxDownload = 0x70000,
        /// <summary>Game Demo</summary>
        GameDemo = 0x80000,
        /// <summary>Video</summary>
        Video = 0x90000,
        /// <summary>Unknown</summary>
        GameTitle = 0xA0000,
        /// <summary>Unknown</summary>
        Installer = 0xB0000,
        /// <summary>Game trailer</summary>
        GameTrailer = 0xC0000,
        /// <summary></summary>
        Arcade = 0xD0000,
        /// <summary>XNA Launcher?</summary>
        XNA = 0xE0000,
        /// <summary>Xbox Licenses</summary>
        LicenseStore = 0xF0000,
        /// <summary>Marketplace movie</summary>
        Movie = 0x100000,
        /// <summary>Marketplace TV show</summary>
        TV = 0x200000,
        /// <summary>Marketplace Music Video</summary>
        MusicVideo = 0x300000,
        /// <summary>Unknown</summary>
        GameVideo = 0x400000,
        /// <summary>Podcast video</summary>
        PodcastVideo = 0x500000,
        /// <summary>Unknown</summary>
        ViralVideo = 0x600000
    }

    /// <summary>
    /// STFS package magic
    /// </summary>
    public enum PackageMagic : uint
    {
        /// <summary>Console signed</summary>
        CON = 0x434F4E20, // ASCII "CON "
        /// <summary>Xbox Live Server Signed</summary>
        LIVE = 0x4C495645, // ASCII "LIVE"
        /// <summary>Xbox Distribution Signed</summary>
        PIRS = 0x50495253, // ASCII "PIRS"
        /// <summary>Unknown Magic</summary>
        Unknown = 0xFFFFFFF
    }

    internal enum TreeLevel : byte { L0, L1, L2, LT }

    /// <summary>
    /// STFS exceptions
    /// </summary>
    [DebuggerStepThrough]
    public sealed class STFSExcepts
    {
        [CompilerGenerated]
        static readonly Exception xParseError = new Exception("Unknown package parse error");
        [CompilerGenerated]
        static readonly Exception xWriteError = new Exception("Unkown IO writing error");
        [CompilerGenerated]
        static readonly Exception xIOAccess = new Exception("Error when accessing stream");
        [CompilerGenerated]
        static readonly Exception xGeneral = new Exception("Unknown error occurred");
        [CompilerGenerated]
        static readonly Exception xNotStrong = new Exception("Input type not of strong signed");
        [CompilerGenerated]
        static readonly Exception xEntryCount = new Exception("Invalid Entry Count");
        [CompilerGenerated]
        static readonly Exception xType = new Exception("Invalid STFS format");
        [CompilerGenerated]
        static readonly Exception xUnsuccess = new Exception("Package was not parsed correctly");
        [CompilerGenerated]
        static readonly Exception xInUse = new Exception("This package is currently in use");
        [CompilerGenerated]
        static readonly Exception xGame = new Exception("This is an installed game, can't parse");
        [CompilerGenerated]
        static readonly Exception xTheme = new Exception("Cannot add required files for a Themeamatic Skin package");
        [CompilerGenerated]
        static readonly Exception xChars = new Exception("Contains invalid characters");
        [CompilerGenerated]
        static readonly Exception xBlock = new Exception("Invalid Block Pointer");
        [CompilerGenerated]
        static readonly Exception xMaxOver = new Exception("Max block count invalid");

        /// <summary>Parse Error</summary>
        public static Exception ParseError { get { return xParseError; } }
        /// <summary>Write Error</summary>
        public static Exception WriteError { get { return xWriteError; } }
        /// <summary>Stream error</summary>
        public static Exception IOAccess { get { return xIOAccess; } }
        /// <summary>Unknown error</summary>
        public static Exception General { get { return xGeneral; } }
        /// <summary>Not LIVE/PIRS</summary>
        public static Exception NotStrong { get { return xNotStrong; } }
        /// <summary>Entry Count error</summary>
        public static Exception EntryCount { get { return xEntryCount; } }
        /// <summary>Invalid STFS Type</summary>
        public static Exception Type { get { return xType; } }
        /// <summary>Unsuccessful function run</summary>
        public static Exception Unsuccessful { get { return xUnsuccess; } }
        /// <summary>File in use</summary>
        public static Exception InUse { get { return xInUse; }}
        /// <summary>Game package, not STFS</summary>
        public static Exception Game { get { return xGame; }}
        /// <summary>Theme error</summary>
        public static Exception ThemeError { get { return xTheme; } }
        /// <summary>Contains Invalid characters</summary>
        public static Exception InvalChars { get { return xChars; } }
        /// <summary>Bad Block</summary>
        public static Exception InvalBlock { get { return xBlock; } }
        /// <summary>Max count error</summary>
        public static Exception MaxOver { get { return xMaxOver; } }
    }

    /// <summary>
    /// Strong signed indication
    /// </summary>
    public enum StrongSigned
    {
        /// <summary>Xbox Live Server Signed</summary>
        LIVE = 0,
        /// <summary>Microsoft Distributional Strong Signed</summary>
        PIRS
    }

    /// <summary>Parameters to sign</summary>
    [DebuggerStepThrough]
    public sealed class RSAParams
    {
        [CompilerGenerated]
        byte[] xC;
        [CompilerGenerated]
        RSAParameters xK = new RSAParameters();
        [CompilerGenerated]
        PackageMagic xM = new PackageMagic();
        [CompilerGenerated]
        bool xV = false;

        /// <summary>
        /// Returns Certificate (has contents only when from a KV)
        /// </summary>
        public byte[] Certificate { get { return xC; } }

        /// <summary>
        /// Returns the RSA Parameters for this instance (only works if valid)
        /// </summary>
        public RSAParameters RSAKeys { get { return xK; } }

        /// <summary>
        /// Returns the magic type of this instance
        /// </summary>
        public PackageMagic Type { get { return xM; } }

        /// <summary>
        /// Gets a bool whether this instance is valid for Signing or not
        /// </summary>
        public bool Valid { get { return xV; } }

        void XLoadCON(DJsIO xKV)
        {
            int xbase = 0;
            if (xKV.Length == 0x4000)
                xbase = 0x10;
            else if (xKV.Length != 0x3FF0)
                throw CryptoExcepts.KVSize;
            xM = PackageMagic.CON;
            xKV.Position = 0x9B8 + xbase;
            xC = xKV.ReadBytes(0x1A8);
            // D is a constant
            xK.D = global::X360.Properties.Resources.XK0;
            xKV.Position = 0x28C + xbase;
            xK.Exponent = xKV.ReadBytes(4);
            xKV.Position = 0x298 + xbase;
            xK.Modulus = ScrambleMethods.StockScramble(xKV.ReadBytes(0x80), false);
            xK.P = ScrambleMethods.StockScramble(xKV.ReadBytes(0x40), false);
            xK.Q = ScrambleMethods.StockScramble(xKV.ReadBytes(0x40), false);
            xK.DP = ScrambleMethods.StockScramble(xKV.ReadBytes(0x40), false);
            xK.DQ = ScrambleMethods.StockScramble(xKV.ReadBytes(0x40), false);
            xK.InverseQ = ScrambleMethods.StockScramble(xKV.ReadBytes(0x40), false);
            // Checks if the certificate is the same as the imported keys
            byte[] xPiece = xC.BytePiece(0x28, 0x80);
            if (BitConverter.ToString(ScrambleMethods.StockScramble(xPiece, false)) != BitConverter.ToString(xK.Modulus))
                throw CryptoExcepts.CertConflict;
            // Checks if keys work
            try
            {
                RSACryptoServiceProvider x = new RSACryptoServiceProvider();
                x.ImportParameters(xK);
            }
            catch (Exception x) { throw x; }
            xV = true;
        }

        /// <summary>
        /// Import RSA attributes from an Xbox360 Keyvault for CON
        /// </summary>
        /// <param name="xKV"></param>
        public RSAParams(DJsIO xKV) { XLoadCON(xKV); }

        /// <summary>
        /// Impor RSA attributes from an Xbox360 Keyvault location for Con
        /// </summary>
        /// <param name="xKVLocation"></param>
        public RSAParams(string xKVLocation)
        {
            DJsIO xKV = new DJsIO(xKVLocation, DJFileMode.Open, true);
            if (!xKV.Accessed)
                throw STFSExcepts.IOAccess;
            XLoadCON(xKV);
            xKV.Close();
        }

        /// <summary>
        /// Initializes a Strong Signed key type of Kit 360's
        /// </summary>
        /// <param name="xTypeIn"></param>
        public RSAParams(StrongSigned xTypeIn)
        {
            DJsIO xReader = null;
            switch (xTypeIn)
            {
                case StrongSigned.LIVE:
                    xReader = new DJsIO(global::X360.Properties.Resources.XK4, true);
                    break;
                    
                case StrongSigned.PIRS:
                    xReader = new DJsIO(global::X360.Properties.Resources.XK5, true);
                    break;

                default:
                    throw STFSExcepts.NotStrong;
            }
            xK.Exponent = new byte[] { 0, 0, 0, 3 };
            xK.D = global::X360.Properties.Resources.XK3;
            xReader.Position = 0;
            xK.Modulus = xReader.ReadBytes(0x100);
            xK.P = xReader.ReadBytes(0x80);
            xK.Q = xReader.ReadBytes(0x80);
            xK.DP = xReader.ReadBytes(0x80);
            xK.DQ = xReader.ReadBytes(0x80);
            xK.InverseQ = xReader.ReadBytes(0x80);
            xReader.Dispose();
            if (xTypeIn == StrongSigned.LIVE)
                xM = PackageMagic.LIVE;
            else xM = PackageMagic.PIRS;
            xV = true;
        }
    }

    /// <summary>
    /// Object to return status of verification
    /// </summary>
    [DebuggerStepThrough]
    public sealed class Verified
    {
        [CompilerGenerated]
        bool xValid = false;
        [CompilerGenerated]
        long xInputLocale = 0;
        [CompilerGenerated]
        long xVerifyLocale = 0;
        [CompilerGenerated]
        ItemType xThisType = ItemType.Data;
        /// <summary>Input location</summary>
        public long InputLocale { get { return xInputLocale; } }
        /// <summary>Recorded location</summary>
        public long VerifyLocale { get { return xVerifyLocale; } }
        /// <summary>Verification type</summary>
        public ItemType ThisType { get { return xThisType; } }
        /// <summary>Returns if this is verified or invalid</summary>
        public bool IsValid { get { return xValid; } }
        
        internal Verified(ItemType xType, bool xCheck, long xInLoc, long xVerifLoc)
        {
            xValid = xCheck;
            xInputLocale = xInLoc;
            xVerifyLocale = xVerifLoc;
            xThisType = xType;
        }

        internal Verified(ItemType xType) { xThisType = xType; }
    }

    /// <summary>
    /// Verification type
    /// </summary>
    public enum ItemType : byte
    {
        /// <summary>Data block hash</summary>
        Data = 0,
        /// <summary>Hash table level 0 hash</summary>
        TableTree0,
        /// <summary>Hash table level 1 hash</summary>
        TableTree1,
        /// <summary>Master hash</summary>
        Master,
        /// <summary>Header hash</summary>
        Header,
        /// <summary>Data Digest RSA Signature</summary>
        Signature,
        /// <summary>Certificate Digest RSA Signature</summary>
        Certificate
    }
    /// <summary>
    /// Package transferring
    /// </summary>
    public enum TransferLock : byte
    {
        /// <summary>No transferring</summary>
        NoTransfer = 0,
        /// <summary>Allow transferring between the profile</summary>
        ProfileAllowOnly,
        /// <summary>Allow transferring between the device</summary>
        DeviceAllowOnly,
        /// <summary>Allow profile and device transferring</summary>
        AllowTransfer
    }
}