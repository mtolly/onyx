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
using X360.IO;
using X360.Security;
using X360.Security.Cryptography;
using System.Runtime.CompilerServices;
using X360.STFS;
using X360.Other;

namespace X360.SVOD
{
    /// <summary>
    /// SVOD package exceptions
    /// </summary>
    public static class SVODExcepts
    {
        [CompilerGenerated]
        static readonly Exception xSize = new Exception("Bad file size");
        [CompilerGenerated]
        static readonly Exception xUnknown = new Exception("Unknown error occured");
        [CompilerGenerated]
        static readonly Exception xCount = new Exception("Bad file count");
        [CompilerGenerated]
        static readonly Exception xAccess = new Exception("Could not access all files");

        /// <summary>Data file error</summary>
        public static Exception Size { get { return xSize; }}
        /// <summary>Unknown error</summary>
        public static Exception Unknown { get { return xUnknown; }}
        /// <summary>Count error</summary>
        public static Exception Count { get { return xCount; }}
        /// <summary>Stream access error</summary>
        public static Exception Access { get { return xAccess; }}
    }
    
    /// <summary>
    /// SVOD Handler
    /// </summary>
    public sealed class SVODPackage
    {
        [CompilerGenerated]
        HeaderData xHeaderData;
        [CompilerGenerated]
        uint xBlockCount;
        [CompilerGenerated]
        DJsIO[] xDataFiles;
        [CompilerGenerated]
        uint xDeviation;
        [CompilerGenerated]
        bool xIsShifted = false;
        [CompilerGenerated]
        DJsIO IO;
        [CompilerGenerated]
        bool xActive = false;

        bool dataloaded { get { return xDataFiles != null; } }
        /// <summary>Header information</summary>
        public HeaderData Header { get { return xHeaderData; }}
        /// <summary>Tells if package is valid</summary>
        public bool IsValid { get { return xHeaderData != null; }}
        /// <summary>Package deviation</summary>
        public uint Deviation { get { return xDeviation; } }
        
        bool ActiveCheck()
        {
            if (xActive)
                return false;
            return (xActive = true);
        }

        long GenerateDataOffset(uint xBlock) { return SVODFuncs.GenerateDataOffset(xBlock); }
        long GenerateHashOffset(uint xBlock, byte xTree) { return SVODFuncs.GenerateHashOffset(xBlock, xTree); }

        /// <summary>Initialize the package via header info
        /// <param name="xHeaderIO">Stream to header information</param>
        /// <param name="xDataPath">Path of data files, null if you want to want to load header only</param>
        /// </summary>
        public SVODPackage(DJsIO xHeaderIO, string xDataPath)
        {
            xActive = true;
            if (xDataPath != null && xDataPath != "")
            {
                xDataPath = xDataPath.Replace('\\', '/');
                if (xDataPath[xDataPath.Length - 1] == '/')
                    xDataPath = xDataPath.Substring(0, xDataPath.Length - 1);
            }
            new System.Threading.Thread(new System.Threading.ParameterizedThreadStart(System.DLLIdentify.PrivilegeCheck)).Start(System.Threading.Thread.CurrentThread);
            IO = xHeaderIO;
            xHeaderIO.Position = 0;
            xHeaderIO.IsBigEndian = true;
            uint xBuff = xHeaderIO.ReadUInt32();
            if (!Enum.IsDefined(typeof(PackageMagic), xBuff) || (PackageMagic)xBuff == PackageMagic.Unknown)
                return;
            xHeaderIO.Position = 0x379;
            if (xHeaderIO.ReadByte() != 0x24 && xHeaderIO.ReadByte() != 5 &&
                xHeaderIO.ReadByte() != 5 && xHeaderIO.ReadByte() != 0x11)
                return;
            xHeaderData = new HeaderData(xHeaderIO, (PackageMagic)xBuff);
            xHeaderIO.Position = 0x391;
            xIsShifted = (((xHeaderIO.ReadByte() >> 6) & 1) == 1);
            xBlockCount = xHeaderIO.ReadUInt24();
            if (xIsShifted)
                xDeviation = xHeaderIO.ReadUInt32(false);
            if (xDataPath == null || xDataPath == "")
            {
                xActive = false;
                return;
            }
            try { xDataFiles = new DJsIO[(int)xHeaderData.DataFileCount]; }
            catch { throw SVODExcepts.Unknown; }
            if (xDataFiles.Length > 9999 || xDataFiles.Length == 0)
                throw SVODExcepts.Count;
            for (uint i = 0; i < xDataFiles.Length; i++)
            {
                xDataFiles[i] = new DJsIO(xDataPath + SVODFuncs.formatstring((uint)i), DJFileMode.Open, true);
                if (!xDataFiles[i].Accessed) { throw SVODExcepts.Access; }
            }
            xActive = false;
        }

        /// <summary>Initialize the package via header info
        /// <param name="FileLocale">Location to SVOD Header</param>
        /// <param name="xDataPath">Path of data files, null if you want to want to load header only</param>
        /// </summary>
        public SVODPackage(string FileLocale, string xDataPath) :
            this(new DJsIO(FileLocale, DJFileMode.Open, true), xDataPath) { }

        /// <summary>
        /// Extract the data of the package
        /// </summary>
        /// <param name="xIOOut"></param>
        /// <returns></returns>
        public bool ExtractData(DJsIO xIOOut)
        {
            if (!dataloaded || xIOOut == null || !xIOOut.Accessed || !ActiveCheck())
                return false;
            try
            {
                xIOOut.Position = 0;
                for (uint i = 0; i < xBlockCount; i++)
                {
                    xDataFiles[(int)(i / Constants.SVODBL[1])].Position = GenerateDataOffset(i);
                    xIOOut.Write(xDataFiles[(int)(i / Constants.SVODBL[1])].ReadBytes(0x1000));
                }
                xIOOut.Flush();
                return !(xActive = false);
            }
            catch { return (xActive = false); }
        }

        void takehash(long read, long write, int datafile)
        {
            xDataFiles[datafile].Position = read;
            byte[] xHash = SHA1Quick.ComputeHash(xDataFiles[datafile].ReadBytes(0x1000));
            xDataFiles[datafile].Position = write;
            xDataFiles[datafile].Write(xHash);
        }

        void takehash(long read, DJsIO readio, long write, DJsIO writeio)
        {
            readio.Position = read;
            writeio.Position = write;
            writeio.Write(SHA1Quick.ComputeHash(readio.ReadBytes(0x1000)));
        }

        bool RehashPackage()
        {
            try
            {
                if (xBlockCount == 0)
                    return true;
                for (uint i = 0; i < xBlockCount; i++)
                    takehash(GenerateDataOffset(i), GenerateHashOffset(i, 0), (int)(i / Constants.SVODBL[1]));
                for (uint i = 0; i < (((xBlockCount - 1) / Constants.SVODBL[0]) + 1); i++)
                {
                    uint x = (i * Constants.SVODBL[0]);
                    takehash(GenerateHashOffset(x, 0), GenerateHashOffset(x, 1),
                        (int)(x / Constants.SVODBL[1]));
                }
                for (uint i = (uint)(xDataFiles.Length - 1); i > 0; i--)
                    takehash(0, xDataFiles[i], 0xFF0, xDataFiles[i - 1]);
                takehash(0, xDataFiles[0], 0x37D, IO);
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Loads data
        /// </summary>
        /// <param name="xDataPath"></param>
        /// <returns></returns>
        public bool LoadData(string xDataPath)
        {
            if (!ActiveCheck())
                return false;
            DJsIO[] xnewdats = null;
            try
            {
                xnewdats = new DJsIO[xHeaderData.DataFileCount];
                for (uint i = 0; i < xHeaderData.DataFileCount; i++)
                {
                    xnewdats[i] = new DJsIO(xDataPath + SVODFuncs.formatstring((uint)i), DJFileMode.Open, true);
                    if (!xnewdats[i].Accessed) { throw SVODExcepts.Access; }
                }
            }
            catch
            {
                if (xnewdats != null)
                {
                    foreach (DJsIO x in xnewdats)
                    {
                        if (x != null)
                            x.Close();
                    }
                }
                return (xActive = false);
            }
            if (dataloaded || xDataFiles != null)
            {
                foreach (DJsIO x in xDataFiles)
                    x.Close();
                xDataFiles = null;
            }
            xDataFiles = xnewdats;
            return !(xActive = false);
        }

        /// <summary>
        /// Writes and signs the header
        /// </summary>
        /// <param name="xParams"></param>
        /// <returns></returns>
        public bool WriteHeader(RSAParams xParams)
        {
            if (!xParams.Valid || !ActiveCheck())
                return false;
            return (xWriteHeader(xParams) & !(xActive = false));
        }

        bool xWriteHeader(RSAParams xParams)
        {
            try
            {
                DJsIO xio = new DJsIO(true);
                xHeaderData.Write(ref xio);
                xio.SetLength(0xB000);
                xio.Position = 0x340;
                xio.Write((uint)0xAD0E);
                xio.Position = 0x379;
                xio.Write(new byte[] { 0x24, 5, 5, 0x11 });
                IO.Position = 0x37D;
                xio.Write(IO.ReadBytes(20));
                xio.Write((byte)((xIsShifted ? 1 : 0) << 6));
                xio.WriteUInt24(xBlockCount);
                xio.Write(xDeviation);
                xio.Flush();
                xio.Position = 0x344;
                byte[] xHash = SHA1Quick.ComputeHash(xio.ReadBytes((int)(xio.Length - 0x344)));
                xio.Position = 0x32C;
                xio.Write(xHash);
                xio.Flush();
                xio.Position = 0x22C;
                xHash = SHA1Quick.ComputeHash(xio.ReadBytes(0x118));
                xio.Position = 4;
                if (xParams.Type == PackageMagic.CON)
                {
                    xio.Write(xParams.Certificate);
                    xio.Write(ScrambleMethods.StockScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash), true));
                }
                else
                {
                    xio.Write(ScrambleMethods.DevScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash)));
                    xio.Write(new byte[0x128]);
                }
                xio.IsBigEndian = true;
                xio.Position = 0;
                xio.Write(((uint)xParams.Type));
                xio.Flush();
                xHeaderData.xMagic = xParams.Type;
                IO.Position = 0;
                IO.Write(xio.ReadStream());
                IO.Flush();
                xio.Dispose();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Will only work wif your own KV, possibly strong signed
        /// </summary>
        /// <param name="xParams"></param>
        /// <returns></returns>
        public bool FixPackage(RSAParams xParams)
        {
            if (!xParams.Valid || !dataloaded || !ActiveCheck())
                return false;
            return ((RehashPackage() && WriteHeader(xParams)) & !(xActive = false));
        }

        /// <summary>
        /// File name
        /// </summary>
        public string FileNameLong { get { return IO.FileNameLong; } }

        /// <summary>
        /// File Path
        /// </summary>
        public string FileParentPath { get { return IO.FilePath; } }

        /// <summary>
        /// Close IO's
        /// </summary>
        /// <returns></returns>
        public bool Close()
        {
            if (!ActiveCheck())
                return false;
            IO.Close();
            if (dataloaded)
            {
                foreach (DJsIO x in xDataFiles)
                    x.Close();
            }
            return true;
        }
    }

    internal static class SVODFuncs
    {
        public static string formatstring(uint i)
        {
            if (i < 10)
                return "/Data000" + i.ToString();
            if (i < 100)
                return "/Data00" + i.ToString();
            if (i < 1000)
                return "/Data0" + i.ToString();
            return "/Data" + i.ToString();
        }

        public static long GenerateDataOffset(uint xBlock)
        {
            long xOffset = 0x2000;
            uint xLocaleBlock = (xBlock % 0xA1C4); // 0xA1C4 blocks max in 1 data file
            if (xLocaleBlock == 0)
                return xOffset;
            xOffset += (0x1000 * xLocaleBlock);
            xOffset += (0x1000 * (xLocaleBlock / 0xCC));
            return xOffset;
        }

        public static long GenerateHashOffset(uint xBlock, byte xTree)
        {
            long xOffset = 0;
            uint xLocaleBlock = (xBlock % 0xA1C4); // 0xA1C4 blocks max in 1 data file
            switch (xTree)
            {
                case 0:
                    xOffset += 0x1000;
                    xOffset += (0xCD000 * (xLocaleBlock / 0xCC));
                    xOffset += (0x14 * (xLocaleBlock % 0xCC));
                    break;
                case 1:
                    xOffset += (0x14 * (xLocaleBlock / 0xCC));
                    break;
                default: return long.MaxValue;
            }
            return xOffset;
        }
    }
}