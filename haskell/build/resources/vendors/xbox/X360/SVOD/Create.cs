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
using X360.STFS;
using X360.Other;
using X360.Security.Cryptography;
using X360.GDFX;

namespace X360.SVOD
{
    /// <summary>
    /// Create an SVOD package
    /// </summary>
    public class CreateSVOD
    {
        [CompilerGenerated]
        HeaderData xHeader;
        /// <summary>
        /// Save location of the package
        /// </summary>
        [CompilerGenerated]
        public string OutLocation { get; set; }
        [CompilerGenerated]
        GDFImage BaseImage;
        [CompilerGenerated]
        bool xActive = true;
        [CompilerGenerated]
        uint Deviation = 0;

        /// <summary>
        /// Header meta data
        /// </summary>
        public HeaderData PackageHeader { get { return xHeader; }}

        /// <summary>
        /// Create an instance of this project
        /// </summary>
        /// <param name="Image">Image to build from</param>
        /// <param name="OutFolder">Save location</param>
        /// <param name="HeaderInfo">Header info</param>
        public CreateSVOD(GDFImage Image, string OutFolder, HeaderData HeaderInfo)
        {
            if (!Image.Valid)
                return;
            BaseImage = Image;
            OutLocation = OutFolder;
            xHeader = HeaderInfo;
            if (Image.Deviation == 0 && Image.baseoffset != 0)
                Deviation = (uint)((Image.baseoffset / 0x1000) + 1);
            else Deviation = Image.Deviation;
            xActive = false;
        }

        /// <summary>
        /// Builds the package
        /// </summary>
        /// <param name="xParams"></param>
        /// <param name="ContentType"></param>
        /// <returns></returns>
        public bool Create(RSAParams xParams, PackageType ContentType)
        {
            if (BaseImage == null)
                throw new Exception("No image");
            if (!xParams.Valid)
                throw CryptoExcepts.ParamError;
            if (ContentType != PackageType.HDDInstalledGame &&
                ContentType != PackageType.OriginalXboxGame &&
                ContentType != PackageType.GamesOnDemand &&
                ContentType != PackageType.SocialTitle)
                return false;
            if (xActive)
                return false;
            xActive = true;
            DJsIO x = null;
            DJsIO h = null;
            try
            {
                string outlocale = OutLocation.Replace('\\', '/');
                if (outlocale[outlocale.Length - 1] == '/')
                    outlocale = outlocale.Substring(0, outlocale.Length - 1);
                outlocale += '/' + ((uint)ContentType).ToString("X8");
                string DataFolder = outlocale + "/" + xHeader.TitleID.ToString("X8") + ".data";
                if (!VariousFunctions.xCheckDirectory(OutLocation))
                    throw IOExcepts.CreateError;
                if (!VariousFunctions.xCheckDirectory(DataFolder))
                    throw IOExcepts.CreateError;
                uint xBlockCount = 0;
                uint xDataFileCount = 0;
                long xDataLength = 0;
                BaseImage.xIO.Position = BaseImage.baseoffset;
                while (BaseImage.xIO.Position < BaseImage.xIO.Length)
                {
                    if ((xBlockCount % Constants.SVODBL[1]) == 0)
                    {
                        if (x != null)
                        {
                            for (int i = 0; i < 0xCB; i++)
                            {
                                x.Position = SVODFuncs.GenerateHashOffset((uint)(i * Constants.SVODBL[0]), 0);
                                byte[] Data1 = SHA1Quick.ComputeHash(x.ReadBytes(0x1000));
                                x.Position = SVODFuncs.GenerateHashOffset((uint)(i * Constants.SVODBL[0]), 1);
                                x.Write(Data1);
                            }
                            x.Flush();
                            xDataLength += x.Length;
                            x.Close();
                        }
                        x = new DJsIO(DataFolder + SVODFuncs.formatstring(xDataFileCount), DJFileMode.Create, true);
                        xDataFileCount++;
                    }
                    x.Position = SVODFuncs.GenerateDataOffset(xBlockCount);
                    byte[] Data = BaseImage.xIO.ReadBytes(0x1000);
                    x.Write(Data);
                    x.Position = SVODFuncs.GenerateHashOffset(xBlockCount, 0);
                    x.Write(SHA1Quick.ComputeHash(Data));
                    xBlockCount++;
                }
                if (xBlockCount == 0)
                {
                    x.Close();
                    return (xActive = false);
                }
                x.Flush();
                xDataLength += x.Length;
                int lvlct = (int)((((xBlockCount % Constants.SVODBL[1]) - 1) / Constants.SVODBL[0]) + 1);
                for (int i = 0; i < lvlct; i++)
                {
                    x.Position = SVODFuncs.GenerateHashOffset((uint)(i * Constants.SVODBL[0]), 0);
                    byte[] Data1 = SHA1Quick.ComputeHash(x.ReadBytes(0x1000));
                    x.Position = SVODFuncs.GenerateHashOffset((uint)(i * Constants.SVODBL[0]), 1);
                    x.Write(Data1);
                }
                x.Flush();
                x.Close();
                byte[] Hash = null;
                for (int i = (int)(xDataFileCount - 1); i >= 0; i--)
                {
                    x = new DJsIO(DataFolder + SVODFuncs.formatstring((uint)i), DJFileMode.Open, true);
                    if (Hash != null)
                    {
                        x.Position = 0xFF0;
                        x.Write(Hash);
                        x.Flush();
                    }
                    x.Position = 0;
                    Hash = SHA1Quick.ComputeHash(x.ReadBytes(0x1000));
                }
                xHeader.DataFileSize = xDataLength;
                xHeader.DataFileCount = xDataFileCount;
                xHeader.xThisType = ContentType;
                h = new DJsIO(outlocale + "/" + xHeader.TitleID.ToString("X8"), DJFileMode.Create, true);
                xHeader.Write(ref h);
                h.SetLength(0xB000);
                h.Position = 0x340;
                h.Write((uint)0xAD0E);
                h.Position = 0x379;
                h.Write(new byte[] { 0x24, 5, 5, 0x11 });
                h.Write(Hash);
                h.Write((byte)((Deviation == 0) ? 0 : 0x40));
                h.WriteUInt24(xBlockCount);
                h.Write(Deviation, false);
                h.Position = 0x344;
                byte[] xHash = SHA1Quick.ComputeHash(h.ReadBytes((int)(h.Length - 0x344)));
                h.Position = 0x32C;
                h.Write(xHash);
                h.Flush();
                h.Position = 0x22C;
                xHash = SHA1Quick.ComputeHash(h.ReadBytes(0x118));
                h.Position = 4;
                if (xParams.Type == PackageMagic.CON)
                {
                    h.Write(xParams.Certificate);
                    h.Write(ScrambleMethods.StockScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash), true));
                }
                else
                {
                    h.Write(ScrambleMethods.DevScramble(RSAQuick.SignatureGenerate(xParams.RSAKeys, xHash)));
                    h.Write(new byte[0x128]);
                }
                h.IsBigEndian = true;
                h.Position = 0;
                h.Write(((uint)xParams.Type));
                xHeader.xMagic = xParams.Type;
                h.Flush();
                h.Close();
                return !(xActive = false);
            }
            catch
            {
                if (x != null)
                    x.Close();
                if (h != null)
                    h.Close();
                return (xActive = false);
            }
        }
    }
}