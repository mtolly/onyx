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
using System.Web;
using System.IO;
using System.Net;
using System.Reflection;
using System.Windows.Forms;
using X360.IO;

namespace X360
{
    /// <summary>
    /// Object to handle updates
    /// </summary>
    public sealed class UpdateReturn
    {
        [CompilerGenerated]
        internal string newvers = "";
        [CompilerGenerated]
        internal string link = "";
        [CompilerGenerated]
        internal bool reachedserver = false;
        [CompilerGenerated]
        internal bool needsup = false;
        [CompilerGenerated]
        internal string notes = "";

        /// <summary>
        /// New Version String
        /// </summary>
        public string NewVersion { get { return newvers; } }
        /// <summary>
        /// Download Link (if applicable)
        /// </summary>
        public string DownloadLink { get { return link; } }
        /// <summary>
        /// Able to locate server
        /// </summary>
        public bool ServerReached { get { return reachedserver; } }
        /// <summary>
        /// Update needed
        /// </summary>
        public bool NeedsUpdate { get { return needsup; } }
        /// <summary>
        /// Notes on the update (if applicable)
        /// </summary>
        public string UpdateNotes { get { return notes; } }

        internal UpdateReturn() { }
    }
    
    /// <summary>
    /// Object to hold information about this library
    /// </summary>
    public static class XAbout
    {
        /// <summary></summary>
        public const string Development = "The X360.dll code was BUILT and MOST research from alias DJ Shepherd\r\n" +
            "Account block decryption was figured out from the combined help of DJ Shepherd, Gabe K, and Haxalot\r\n" +
            "Extra glitch help from Shadow Lag and friend\r\n" +
            "Some slight STFS help from alias Supermodder and Anthony\r\n" +
            "Very special thanks to Deng4 for information and knowledge on the Xbox DVD format\r\n" +
            "Special shout outs to aliases:\r\n" +
            "Gabe K, Superaison, Rhyu Husky, Mojo, Renegade, Eazy B, Unknown V2, Gee19, GoldBl4d3, Dstruktiv, Deng4, and Squishage (<333)\r\n" +
            "For those that discriminated against me for being gay, backstabbed me, OR did nothing but steal code, the community doesn't need you:\r\n" +
            "(Se7enSins staff, Detox, Dark Slipstream, ShotSpartan, Leon/Stoker/Vene, and Sligstorm)";

        /// <summary></summary>
        public const string Legal = "Microsoft, Xbox 360, and all other related occurences in the X360 Library is registerd to Microsoft (C)\r\n" +
            "Any media, such as pictures or audio, belongs to the respected owners\r\n" +
            "Cryptographic methods belong to the respected founders\r\n" +
            "The programmer of X360.dll does not take credit for the invention of said objects, but merely rewriting them\r\n" +
            "Microsoft, affiliates, and the programmer are not responsible for the actions and consequences one may use with this, only themselves";

        /// <summary></summary>
        public const string Programmer = "Alias: DJ Shepherd\r\n" +
            "Raver (DJ) Name: (DJ) SkunkieButt\r\n" +
            "Programs in C# and C++, age 18 at X360 creation, programming for a year\r\n" +
            "Gay furries for teh winz <3 =^,.,^=\r\n" +
            "Fursona: Dalavin, Skepherd (Skunk - German Shepherd hybrid)\r\n" +
            "http://www.skunkiebutt.com/";

        /// <summary></summary>
        public const string GNUProtected = "This Library is protected under a GPL License, for more information, refer to:\r\n" +
            "X360.PublicResources.GPL\r\n" +
            "or http://www.gnu.org/\r\n\r\n" +
            "Also, as represented by the GPL, any programs used by this must have an open sourced copy released wif it," +
            "which also means you are not allowed to make profit off this app (releasing open source would defeat the purpose anyways). " +
            "You must not obfuscate or \"protect\" my work, but you are allowed to pack the DLL into a file as long as it isn't obfuscating it."  +
            "Any code in here is modifiable (unless otherwise stated) except for the entire About class, TaC (Terms and Conditions), " +
            "and licenses except by the sole creator of the license/terms, and finally, the creator reserves the right to cite the source of this library anywhere where this code is used " + 
            "or endorsed, meaning citing of website, name, etc.  The creator reserves the right to change the TaC from version to version.  You also are to understand that this library " +
            "does not guarantee functionality.  This is a 3rd party creation not built by Microsoft.  The programmer will work to make it perfect and work out any encountered bugs.\r\n\r\n" +
            "By using this library, you agree to these terms and conditions.  These words are not a law governed by countries, but a digital contract governed " +
            "separately by each Country's own law regarding contracts/EULA/Licenses.  If you have any questions or doubts to your Country's, law, please look it up before using this piece.  " +
            "You (as a developer) are reliable for the use of this once you have been acknowledged of the TaC and License.";

        /// <summary></summary>
        public const string Homepage = "http://skunkiebutt.com/";
        /// <summary></summary>
        public const string Donate = "http://skunkiebutt.com/?page_id=158";
        /// <summary></summary>
        public const string ReportBugs = "http://skunkiebutt.com/?page_id=433";
        /// <summary></summary>
        public static string Build { get { return Assembly.GetExecutingAssembly().GetName().Version.ToString(); } }

        /// <summary>
        /// Checks if an update is needed
        /// </summary>
        /// <returns></returns>
        public static UpdateReturn CheckForUpdate()
        {
            try
            {
                UpdateReturn xReturn = new UpdateReturn();
                StreamReader x = X360.Other.VariousFunctions.GetWebPageResponse("http://skunkiebutt.com/ProductCheck.php?product=X360&command=read");
                string version = x.ReadLine();
                string url = x.ReadLine();
                string notes = x.ReadLine();
                x.Dispose();
                xReturn.reachedserver = true;
                if (!(xReturn.needsup = (version != Build)))
                    return xReturn;
                xReturn.newvers = version;
                xReturn.link = url;
                xReturn.notes = notes;
                return xReturn;
            }
            catch { return new UpdateReturn(); }
        }

        /// <summary>
        /// Writes the legal documentation to a local location
        /// </summary>
        public static void WriteLegalLocally()
        {
            DJsIO xIO = new DJsIO(Application.StartupPath + "/" + "X360 READ ME.txt", DJFileMode.Create, true);
            if (!xIO.Accessed)
                return;
            xIO.Position = 0;
            xIO.Write("------------ Legal Info ------------" + Environment.NewLine);
            xIO.Write(Legal + Environment.NewLine);
            xIO.Write("------------------------------------" + Environment.NewLine);
            xIO.Write("------- Terms and Conditions -------" + Environment.NewLine);
            xIO.Write(GNUProtected + Environment.NewLine);
            xIO.Write("------------------------------------" + Environment.NewLine);
            xIO.Write("----------- GNU License ------------" + Environment.NewLine);
            xIO.Write(PublicResources.GPL + Environment.NewLine);
            xIO.Write("------------------------------------");
            xIO.Flush();
            xIO.Dispose();
        }
    }
}