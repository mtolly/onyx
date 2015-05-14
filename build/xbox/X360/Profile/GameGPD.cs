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
using System.Drawing;
using X360.IO;
using X360.Other;
using System.Runtime.CompilerServices;

namespace X360.Profile
{
    /// <summary>
    /// Achievement lock type
    /// </summary>
    public enum FlagType
    {
        /// <summary>
        /// Achievement locked
        /// </summary>
        Locked = 0,
        /// <summary>
        /// Achievement unlocked offline
        /// </summary>
        UnlockedOffline = 0x12,
        /// <summary>
        /// Achievement unlocked online
        /// </summary>
        UnlockedOnline }

    /// <summary>
    /// Auto unlock
    /// </summary>
    public enum UnlockProcess
    {
        /// <summary>
        /// Online
        /// </summary>
        Online,
        /// <summary>
        /// Offline
        /// </summary>
        Offline }

    /// <summary>
    /// Object to hold Achievement entries
    /// </summary>
    public sealed class AchievementEntry : XDBFEntry
    {
        [CompilerGenerated]
        internal int imageID;
        [CompilerGenerated]
        string xTitle = "";
        [CompilerGenerated]
        string xDescription1 = "";
        [CompilerGenerated]
        string xDescription2 = "";
        [CompilerGenerated]
        private byte[] Flg;
        /// <summary>
        /// Achievement worth
        /// </summary>
        [CompilerGenerated]
        public uint Worth;
        [CompilerGenerated]
        int xid = 0;
        [CompilerGenerated]
        long TimeL;
        [CompilerGenerated]
        bool xLoaded = false;

        /// <summary>
        /// Image pointer
        /// </summary>
        public int ImageID { get { return imageID; } }

        /// <summary>
        /// Locked Description
        /// </summary>
        public string Description1
        {
            get { return xDescription1; }
            set
            {
                if (value.Length <= xDescription1.Length)
                    xDescription1 = value.PadRight(xDescription1.Length, ' ');
                else xDescription1 = value.Substring(0, xDescription1.Length);
            }
        }
        /// <summary>
        /// Unlocked Description
        /// </summary>
        public string Description2
        {
            get { return xDescription2; }
            set
            {
                if (value.Length <= xDescription2.Length)
                    xDescription2 = value.PadRight(xDescription2.Length, ' ');
                else xDescription2 = value.Substring(0, xDescription2.Length);
            }
        }
        /// <summary>
        /// Flags
        /// </summary>
        public byte[] Flags { get { return Flg; } }
        /// <summary>
        /// Achievement ID
        /// </summary>
        public int AchievementID { get { return xid; } }
        /// <summary>
        /// Title
        /// </summary>
        public string Title
        {
            get { return xTitle; }
            set
            {
                if (value.Length <= xTitle.Length)
                    xTitle = value.PadRight(xTitle.Length, ' ');
                else xTitle = value.Substring(0, xTitle.Length);
            }
        }
        /// <summary>
        /// Is unlocked
        /// </summary>
        public bool Unlocked { get { return (LockType == FlagType.UnlockedOffline || LockType == FlagType.UnlockedOnline); } }

        internal AchievementEntry(XDBFEntry xEntry) : base(xEntry) { }
        /// <summary>
        /// Set lock type
        /// </summary>
        public FlagType LockType
        {
            get { return (FlagType)Flg[1]; }
            set
            {
                switch (value)
                {
                    case FlagType.UnlockedOffline:
                        Flg[1] = (byte)value;
                        TimeL = 0;
                        break;
                    case FlagType.UnlockedOnline:
                        Flg[1] = (byte)value;
                        TimeL = DateTime.Now.ToFileTime();
                        break;

                    default:
                        Flg[1] = 0;
                        TimeL = 0;
                        break;
                }
            }
        }

        internal bool LoadDetails()
        {
            try
            {
                xRef.xIO.Position = Offset + 4; // int32 for Struct size is constant (0x1C)
                xid = xRef.xIO.ReadInt32();
                imageID = xRef.xIO.ReadInt32();
                Worth = xRef.xIO.ReadUInt32();
                Flg = xRef.xIO.ReadBytes(4);
                TimeL = xRef.xIO.ReadInt64();
                try
                {
                    DJsIO xStrings = new DJsIO(xRef.xIO.ReadBytes(Size - 0x1C), true);
                    xTitle = xStrings.ReadString(StringForm.Unicode);
                    xDescription1 = xStrings.ReadString(StringForm.Unicode);
                    xDescription2 = xStrings.ReadString(StringForm.Unicode);
                }
                catch
                {
                    xTitle = "";
                    xDescription1 = "";
                    xDescription2 = "";
                }
                xLoaded = true;
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Set Unlock time
        /// </summary>
        public DateTime UnlockTime { get { return DateTime.FromFileTime(TimeL); } set { TimeL = value.ToFileTime(); } }

        internal bool xUpdate(bool UpdateSync)
        {
            if (!xLoaded)
                return false;
            try
            {
                xRef.xIO.Position = (Offset + 4);
                xRef.xIO.Write(AchievementID);
                xRef.xIO.Write(imageID);
                xRef.xIO.Write(Worth);
                xRef.xIO.Write(Flg);
                xRef.xIO.Write(TimeL);
                xRef.xIO.Flush();
                if (UpdateSync)
                    xRef.UpdateSync(NameSpace.Achievement, ID, SyncType.Server);
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Update the entry
        /// </summary>
        /// <returns></returns>
        public bool Update()
        {
            if (!xRef.ParseCheck())
                return false;
            return (xUpdate(true) & !(xRef.xActive = false));
        }
    }
    /// <summary>
    /// Object to hold Game GPD data
    /// </summary>
    public sealed class GameGPD : GPD
    {
        /// <summary>
        /// Achievements
        /// </summary>
        public AchievementEntry[] Achievements { get { return xAchievements.ToArray(); } }

        internal uint xCalcGST()
        {
            uint xReturn = 0;
            foreach (AchievementEntry x in Achievements)
            {
                if (x.Unlocked)
                    xReturn += x.Worth;
            }
            return xReturn;
        }
        
        internal uint xCalcUT()
        {
            uint xReturn = 0;
            foreach (AchievementEntry x in Achievements)
            {
                if (x.Unlocked)
                    xReturn++;
            }
            return xReturn;
        }

        internal uint xCalcGSP()
        {
            uint xReturn = 0;
            foreach (AchievementEntry x in Achievements)
                xReturn += x.Worth;
            return xReturn;
        }

        /// <summary>
        /// Calculate the total credit
        /// </summary>
        /// <returns></returns>
        public uint CalculateTotalCred()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = xCalcGST();
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Calculate the achievements that are unlocked
        /// </summary>
        /// <returns></returns>
        public uint CalculateUnlockCount()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = xCalcUT();
            xActive = false;
            return xReturn;
        }
        
        /// <summary>
        /// Calculate the total possible credit
        /// </summary>
        /// <returns></returns>
        public uint CalculatePossibleCred()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = xCalcGSP();
            xActive = false;
            return xReturn;
        }

        internal bool xFC(uint xIn)
        {
            Setting x = xGetSetting(GPDIDs.GCardTitleCreditEarned, SettingType.UInt32);
            if (x == null)
                xAddSetting((long)GPDIDs.GCardTitleCreditEarned, xIn, true, SyncType.Server);
            else
            {
                x.Data = xIn;
                x.xUpdate(SyncType.Server);
            }
            return true;
        }

        /// <summary>
        /// Apply a specified credit
        /// </summary>
        /// <param name="xIn"></param>
        /// <returns></returns>
        public bool FixCredit(uint xIn)
        {
            if (!ParseCheck())
                return false;
            try { return xFC(xIn) & !(xActive = false); }
            catch { return (xActive = false); }
        }

        internal bool xFUC(uint xIn)
        {
            Setting x = xGetSetting(GPDIDs.GCardTitleAchievementsEarned, SettingType.UInt32);
            if (x == null)
                xAddSetting((long)GPDIDs.GCardTitleAchievementsEarned, xIn, true, SyncType.Server);
            else
            {
                x.Data = xIn;
                x.xUpdate(SyncType.Server);
            }
            return true;
        }

        /// <summary>
        /// Apply a specified unlock count
        /// </summary>
        /// <param name="xIn"></param>
        /// <returns></returns>
        public bool FixUnlockedCount(uint xIn)
        {
            if (!ParseCheck())
                return false;
            try { return xFUC(xIn) & !(xActive = false); }
            catch { return (xActive = false); }
        }

        internal bool FCAC() { return (xFUC(xCalcUT()) & xFC(xCalcGST())); }

        /// <summary>
        /// Fix the credit and count
        /// </summary>
        /// <returns></returns>
        public bool FixCredAndCount()
        {
            if (!ParseCheck())
                return false;
            return FCAC() & !(xActive = false);
        }
        
        /// <summary>
        /// Unlock all achievements
        /// </summary>
        /// <param name="MinutesBetween"></param>
        /// <returns></returns>
        public bool UnlockAll(uint MinutesBetween)
        {
            if (!ParseCheck())
                return false;
            try
            {
                DateTime xTime = DateTime.Now;
                foreach (AchievementEntry x in Achievements)
                {
                    if (x.Unlocked)
                        continue;
                    x.LockType = FlagType.UnlockedOnline;
                    x.UnlockTime = xTime;
                    x.xUpdate(true);
                    xTime.AddMinutes((double)MinutesBetween);
                }
                FCAC();
                return !(xActive = false);
            }
            catch { return (xActive = false); }
        }

        int sortsync(SyncPair xb, SyncPair xc) { return xb.ID.CompareTo(xc.ID); }

        internal bool xErase()
        {
            for (int i = 0; i < xImages.Count; i++)
            {
                if (xImages[i].ID == (long)GPDIDs.ThisTitle)
                    continue;
                xFreeEnts.Add(new FreeSpaceEntry(xImages[i]));
                xImages.RemoveAt(i--);
            }
            for (int i = 0; i < xSyncs.Count; i++)
            {
                if (xSyncs[i].NS == NameSpace.Achievement ||
                    xSyncs[i].NS == NameSpace.Setting)
                {
                    xSyncs[i].xLastSync = 0;
                    xSyncs[i].xNext = 1;
                    xSyncs[i].xServerSync = TimeStamps.DateTimeZero.ToBinary();
                    xSyncs[i].xUpdate();
                }
            }
            foreach (AchievementEntry x in xAchievements)
            {
                x.LockType = FlagType.Locked;
                x.xUpdate(true);
            }
            for (int i = 0; i < xUserSettings.Count; i++)
            {
                if (xDeleteSetting(xUserSettings[i].ID))
                    i--;
            }
            for (int i = 0; i < xIndexRecords.Count; i++)
            {
                if (xIndexRecords[i].NS == NameSpace.Achievement)
                {
                    xIndexRecords[i].xpairs.Reverse();
                    xIndexRecords[i].xUpdate();
                }
                else if (xIndexRecords[i].NS == NameSpace.Setting)
                {
                    xFreeEnts.Add(new FreeSpaceEntry(xIndexRecords[i]));
                    xIndexRecords.RemoveAt(i--);
                }
            }
            return PatchFree() & UpdateHeader(true);
        }

        /// <summary>
        /// Erases Achievement unlocks and Sync Table
        /// </summary>
        /// <returns></returns>
        public bool EraseGPD()
        {
            if (!ParseCheck())
                return false;
            try { return xErase() & !(xActive = false); }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Get achievement image via achievement index
        /// </summary>
        /// <param name="xAchievementIndex"></param>
        /// <returns></returns>
        public Image GetAchievementImage(int xAchievementIndex)
        {
            if (xAchievements.Count == 0)
                return PublicResources.NoImage;
            if (xAchievements[xAchievementIndex].Unlocked)
            {
                Image xReturn = GetImageByID(xAchievements[xAchievementIndex].imageID);
                return (xReturn == null) ? PublicResources.NoImage : xReturn;
            }
            else return PublicResources.Locked;
        }

        /// <summary>
        /// Initializes a new case
        /// </summary>
        /// <param name="GPDLocale"></param>
        /// <param name="TitleID"></param>
        public GameGPD(string GPDLocale, uint TitleID) : base(GPDLocale, TitleID)
        {
            if (!IsValid)
                return;
            xTitlesPlayed.Clear();
            for (int i = 0; i < xAchievements.Count; i++)
            {
                if (!xAchievements[i].LoadDetails())
                    xAchievements.RemoveAt(i--);
            }
            xActive = false;
        }
    }
}