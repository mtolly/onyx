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
using System.Diagnostics;
using X360.IO;
using X360.STFS;
using System.Runtime.CompilerServices;

namespace X360.Media
{
    /// <summary>
    /// Object to hold music exceptions
    /// </summary>
    [DebuggerStepThrough]
    public static class MusicExcepts
    {
        [CompilerGenerated]
        static readonly Exception xNotMusic = new Exception("Invalid music package input");
        
        /// <summary>
        /// Not a music package
        /// </summary>
        public static Exception NotMusic { get { return xNotMusic; }}
    }

    /// <summary>
    /// Object to hold music files
    /// </summary>
    public sealed class MusicFile
    {
        [CompilerGenerated]
        string so, al, ar, ge;
        [CompilerGenerated]
        DJsIO IO;
        /// <summary>
        /// Song title
        /// </summary>
        [CompilerGenerated]
        public string Song { get { return so; }}
        /// <summary>
        /// Artist
        /// </summary>
        [CompilerGenerated]
        public string Artist { get { return ar; }}
        /// <summary>
        /// Album
        /// </summary>
        [CompilerGenerated]
        public string Album { get { return al; }}
        /// <summary>
        /// Genre
        /// </summary>
        [CompilerGenerated]
        public string Genre { get { return ge; }}
        /// <summary>
        /// Size of file
        /// </summary>
        [CompilerGenerated]
        public long WMASize { get { return (IO.Length - 0xD08); }}
        [CompilerGenerated]
        bool xActive = false;

        /// <summary>
        /// Initializes this object
        /// </summary>
        /// <param name="FileLocale"></param>
        public MusicFile(string FileLocale)
        {
            xActive = true;
            DJsIO xIO = new DJsIO(FileLocale, DJFileMode.Open, true);
            if (!xIO.Accessed)
                throw STFSExcepts.ParseError;
            try
            {
                xIO.Position = 0;
                xIO.IsBigEndian = true;
                if (xIO.ReadUInt32() != 0x464D494D)
                    throw MusicExcepts.NotMusic;
                xIO.Position = 0xC;
                so = xIO.ReadString(StringForm.Unicode, 0x100);
                al = xIO.ReadString(StringForm.Unicode, 0x100);
                ar = xIO.ReadString(StringForm.Unicode, 0x100);
                xIO.Position += 0x200;
                ge = xIO.ReadString(StringForm.Unicode, 0x100);
                IO = xIO;
                xActive = false;
            }
            catch { xIO.Dispose(); throw STFSExcepts.ParseError; }
        }

        void xExtract(ref DJsIO xIOOut)
        {
            xIOOut.Position = 0;
            IO.Position = 0xD08;
            /* Self Note:
             * not in 0x1000 blocks, just decided to write them
             * in those size to avoid memory conflicts
             * */
            while (IO.Position < IO.Length)
                xIOOut.Write(IO.ReadBytes(0x1000));
            xIOOut.Flush();
        }

        /// <summary>
        /// Extracts the music file
        /// </summary>
        /// <param name="xIOOut"></param>
        /// <returns></returns>
        public bool ExtractWMA(DJsIO xIOOut)
        {
            if (xActive)
                return false;
            xActive = true;
            try
            {
                xExtract(ref xIOOut);
                return !(xActive = false);
            }
            catch { xActive = false; throw STFSExcepts.WriteError; }
        }

        /// <summary>
        /// Extracts the file via end user location
        /// </summary>
        /// <returns></returns>
        public bool ExtractWMA()
        {
            if (xActive)
                return false;
            xActive = true;
            try
            {
                DJsIO xIO = new DJsIO(DJFileMode.Create, "Save Where?", "Windows Music File|*.wma", true);
                if (!xIO.Accessed)
                    return (xActive = false);
                xExtract(ref xIO);
                return !(xActive = false);
            }
            catch { xActive = false; throw STFSExcepts.WriteError; }
        }
    }
}
