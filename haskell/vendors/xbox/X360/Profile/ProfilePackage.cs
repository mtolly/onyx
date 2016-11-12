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

namespace X360.STFS
{
    /// <summary>
    /// Object to hold user strings
    /// </summary>
    public sealed class UserInfo
    {
        /// <summary>
        /// User bio
        /// </summary>
        [CompilerGenerated]
        public string Bio = null;
        /// <summary>
        /// User motto
        /// </summary>
        [CompilerGenerated]
        public string Motto = null;
        /// <summary>
        /// User name
        /// </summary>
        [CompilerGenerated]
        public string Name = null;
        /// <summary>
        /// User location
        /// </summary>
        [CompilerGenerated]
        public string Location = null;

        internal UserInfo() { }
    }

    /// <summary>
    /// Object to handle profiles
    /// </summary>
    public sealed class ProfilePackage : STFSPackage
    {
        [CompilerGenerated]
        DashGPD xUserGPD = null;
        [CompilerGenerated]
        UserAccount xUserFile = null;

        /// <summary>
        /// Dash GPD
        /// </summary>
        public DashGPD UserGPD { get { return xUserGPD; }}
        /// <summary>
        /// Has Dash and Account
        /// </summary>
        public bool IsValidProfile { get { return (xUserFile != null && xUserFile != null); }}
        /// <summary>
        /// Has Dash GPD
        /// </summary>
        public bool HasDashGPD { get { return (xUserGPD != null); }}
        /// <summary>
        /// Has Account file
        /// </summary>
        public bool HasValidAccount { get { return (xUserFile != null); }}
        /// <summary>
        /// Account
        /// </summary>
        public UserAccount UserFile { get { return xUserFile; }}

        /// <summary>
        /// Initializes an instance
        /// </summary>
        /// <param name="x"></param>
        public ProfilePackage(ref STFSPackage x)
            : base(ref x)
        {
            if (Header.ThisType == PackageType.Profile)
                LoadProfile(true);
        }

        /// <summary>
        /// Initializes from an IO
        /// </summary>
        /// <param name="x"></param>
        /// <param name="LogIn"></param>
        public ProfilePackage(ref DJsIO x, LogRecord LogIn)
            : base(x, LogIn)
        {
            if (Header.ThisType == PackageType.Profile)
                LoadProfile(true);
        }

        /// <summary>
        /// Save dash GPD
        /// </summary>
        /// <returns></returns>
        public bool SaveDash()
        {
            if (!ParseCheck() || !HasDashGPD)
                return false;
            return xSaveDash();
        }

        /// <summary>
        /// Save account
        /// </summary>
        /// <returns></returns>
        public bool SaveAccount()
        {
            if (!ParseCheck() || !HasValidAccount)
                return false;
            return xSaveAccount();
        }

        bool xSaveDash()
        {
            FileEntry x = GetFile("FFFE07D1.gpd");
            if (x == null)
                return false;
            return x.Replace(xUserGPD.xIO);
        }

        bool xSaveAccount()
        {
            FileEntry x = GetFile("Account");
            if (x == null)
                return false;
            return x.Replace(xUserFile.IO);
        }

        /// <summary>
        /// Reads a GPD from the Titles Played index of the User GPD
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="xEntry"></param>
        /// <returns></returns>
        public GameGPD ReadGame(uint ID, out FileEntry xEntry)
        {
            xEntry = null;
            if (!ParseCheck())
                return null;
            if (UserGPD == null)
                return null;
            FileEntry x = GetFile(ID.ToString("X") + ".gpd");
            if (x == null)
                return null;
            DJsIO y = x.GetTempIO(true);
            if (y == null || !y.Accessed)
            {
                y.Close();
                VariousFunctions.DeleteFile(y.FileNameLong);
                return null;
            }
            y.Close();
            GameGPD z = new GameGPD(y.FileNameLong, ID);
            if (z.IsValid)
            {
                xEntry = x;
                return z;
            }
            return null;
        }

        /// <summary>
        /// Loads the profile at paw
        /// </summary>
        /// <returns></returns>
        public bool LoadProfile() { return LoadProfile(true); }

        /// <summary>
        /// Loads the profile information from the package
        /// </summary>
        /// <param name="xboth"></param>
        /// <returns></returns>
        bool LoadProfile(bool xboth)
        {
            if (!ParseCheck())
                return false;
            new System.Threading.Thread(new System.Threading.ParameterizedThreadStart(System.DLLIdentify.PrivilegeCheck)).Start(System.Threading.Thread.CurrentThread);
            bool xreturn = true;
            if (xboth)
            {
                FileEntry xacct = GetFile("Account");
                if (xacct != null && xacct.Size == 404)
                {
                    if (HasValidAccount)
                        xUserFile.IO.Dispose();
                    AddToLog("Parsing Account file");
                    DJsIO xAcctIO = xacct.GetTempIO(true);
                    if (xAcctIO == null || !xAcctIO.Accessed)
                    {
                        if (xAcctIO != null)
                        {
                            xAcctIO.Dispose();
                            VariousFunctions.DeleteFile(xAcctIO.FileNameLong);
                        }
                    }
                    else
                    {
                        xUserFile = new UserAccount(xAcctIO, AccountType.Stock, true);
                        xreturn = xUserFile.Success;
                        if (!xUserFile.Success)
                        {
                            xUserFile = new UserAccount(xAcctIO, AccountType.Kits, true);
                            xreturn = xUserFile.Success;
                            if (!xUserFile.Success)
                                xUserFile = null;
                        }
                    }
                }
            }
            FileEntry xdash = GetFile("FFFE07D1.gpd");
            if (xdash != null)
            {
                AddToLog("Parsing User GPD");
                DJsIO xFFIO = xdash.GetTempIO(true);
                if (xFFIO == null || !xFFIO.Accessed)
                {
                    if (xFFIO != null)
                        VariousFunctions.DeleteFile(xFFIO.FileNameLong);
                }
                else
                {
                    try
                    {
                        xFFIO.Close();
                        if (HasDashGPD)
                            xUserGPD.xIO.Dispose();
                        xUserGPD = new DashGPD(xFFIO.FileNameLong);
                        xreturn &= xUserGPD.IsValid;
                        if (!xUserGPD.IsValid)
                        {
                            xUserGPD = null;
                            xFFIO.Dispose();
                            VariousFunctions.DeleteFile(xFFIO.FileNameLong);
                        }
                    }
                    catch { xUserGPD = null; }
                }
            }
            return xreturn;
        }

        /// <summary>
        /// Makes a copy of an Xbox Live profile into a HDD profile
        /// (Works only for profiles and if not a HDD profile already)
        /// </summary>
        /// <param name="xOutLocation"></param>
        /// <returns></returns>
        public bool MakeHDDAccountCopy(string xOutLocation)
        {
            if (!ParseCheck())
                return false;
            ProfilePackage xPackage = null;
            try
            {
                if (MakeBackup(xOutLocation))
                    xPackage = (ProfilePackage)(new STFSPackage(xOutLocation, null));
                else return false;
                if (!xPackage.ParseSuccess)
                    throw new Exception(); // Goes to catch
                xPackage.UserFile.ForceIntoHDDAccount();
                xPackage.xSaveAccount();
                xPackage.xIO.Close();
                return true;
            }
            catch
            {
                xPackage.xIO.Close();
                VariousFunctions.DeleteFile(xPackage.xIO.FileNameLong);
                return false;
            }
        }

        /// <summary>
        /// Unlocks all achievements either Online or Offline
        /// </summary>
        /// <param name="MinutesBetween"></param>
        /// <returns></returns>
        public bool UnlockAll(uint MinutesBetween)
        {
            if (!ParseCheck())
                return false;
            if (!HasDashGPD)
                return false;
            try
            {
                foreach (TitlePlayedEntry xTitle in UserGPD.TitlesPlayed)
                {
                    FileEntry flent;
                    GameGPD x = ReadGame(xTitle.TitleID, out flent);
                    if (x == null)
                        continue;
                    x.UnlockAll(MinutesBetween);
                    uint data1 = 0;
                    uint data2 = 0;
                    x.FixCredAndCount();
                    if (flent.Replace(x.xIO))
                    {
                        xTitle.EarnedCount = data1;
                        xTitle.EarnedWorth = data2;
                        xTitle.PossibleWorth = x.xCalcGSP();
                        xTitle.xUpdate();
                    }
                }
                xUserGPD.UpdateGamerScore();
                xSaveDash();
                return true;
            }
            catch { return false; }
        }

        // Image x; x.GetThumbnail()

        /// <summary>
        /// Gets Bio, Motto, Name, and Location
        /// </summary>
        /// <returns></returns>
        public UserInfo GetUserStrings()
        {
            if (!ParseCheck() || !HasDashGPD)
                return null;
            UserInfo xReturn = new UserInfo();
            Setting setting = xUserGPD.GetSetting(GPDIDs.GCardName);
            if (setting != null && setting.ContentType == SettingType.Unicode)
                xReturn.Name = (string)setting.Data;
            setting = xUserGPD.GetSetting(GPDIDs.GCardMotto);
            if (setting != null && setting.ContentType == SettingType.Unicode)
                xReturn.Motto = (string)setting.Data;
            setting = xUserGPD.GetSetting(GPDIDs.GCardUserLocale);
            if (setting != null && setting.ContentType == SettingType.Unicode)
                xReturn.Location = (string)setting.Data;
            setting = xUserGPD.GetSetting(GPDIDs.GCardUserBio);
            if (setting != null && setting.ContentType == SettingType.Unicode)
                xReturn.Bio = (string)setting.Data;
            return xReturn;
        }

        /// <summary>
        /// Adds a game to the User GPD
        /// </summary>
        /// <param name="xTitleGPD"></param>
        /// <returns></returns>
        public bool AddGame(GameGPD xTitleGPD)
        {
            if (!ParseCheck())
                return false;
            try
            {
                if (xUserGPD == null || !xUserGPD.xIO.Accessed ||
                    !xTitleGPD.xIO.Accessed || !xTitleGPD.IsValid)
                    return false;
                if (!UserGPD.AddGameViaGPD(xTitleGPD))
                {
                    LoadProfile(false);
                    return false;
                }
                if (!MakeFile(xTitleGPD.TitleID.ToString("X").ToUpper() + ".gpd", xTitleGPD.xIO, AddType.Replace))
                    return false;
                if (!xSaveDash())
                {
                    GetFile(xTitleGPD.TitleID.ToString("X").ToLower() + ".gpd").Delete();
                    LoadProfile(false);
                    return false;
                }
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Adds/Saves profile information, null a string if you don't want to change it, empty string to delete
        /// </summary>
        /// <param name="Name"></param>
        /// <param name="Motto"></param>
        /// <param name="Location"></param>
        /// <param name="Bio"></param>
        /// <returns></returns>
        public bool SaveProfileInfo(string Name, string Motto, string Location, string Bio)
        {
            if (!ParseCheck())
                return false;
            try
            {
                if (Name != null)
                {
                    if (Name != "")
                        xUserGPD.AddSetting((long)GPDIDs.GCardName, Name, true, SyncType.Server);
                    else xUserGPD.DeleteSetting((long)GPDIDs.GCardName);
                }
                if (Motto != null)
                {
                    if (Motto != "")
                        xUserGPD.AddSetting((long)GPDIDs.GCardMotto, Motto, true, SyncType.Server);
                    else xUserGPD.DeleteSetting((long)GPDIDs.GCardMotto);
                }
                if (Location != null)
                {
                    if (Location != "")
                        xUserGPD.AddSetting((long)GPDIDs.GCardUserLocale, Location, true, SyncType.Server);
                    else xUserGPD.DeleteSetting((long)GPDIDs.GCardUserLocale);
                }
                if (Bio != null)
                {
                    if (Bio != "")
                        xUserGPD.AddSetting((long)GPDIDs.GCardUserBio, Bio, true, SyncType.Server);
                    else xUserGPD.DeleteSetting((long)GPDIDs.GCardUserBio);
                }
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Unknown file, but it seems to contain history of achievements or whatever... rape it
        /// </summary>
        /// <returns></returns>
        public bool RemovePhDAT()
        {
            if (!ParseCheck())
                return false;
            FileEntry x = GetFile("cache/ph.dat");
            if (x == null)
                return true;
            return x.Delete();
        }

        /// <summary>
        /// Deletes the files associated to this file (used for closing the file)
        /// </summary>
        /// <returns></returns>
        public bool DeleteProfileFiles()
        {
            if (!ParseCheck())
                return false;
            try
            {
                if (HasDashGPD)
                {
                    UserGPD.xIO.Close();
                    VariousFunctions.DeleteFile(UserGPD.xIO.FileNameLong);
                }
                if (HasValidAccount)
                {
                    UserFile.IO.Close();
                    VariousFunctions.DeleteFile(UserFile.IO.FileNameLong);
                }
                return true;
            }
            catch { return false; }
        }
    }
}