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
using X360.IO;
using X360.Other;
using X360.STFS;

namespace X360.Profile
{
    #region Profile Enums
    /// <summary>
    /// Enums for stick movement
    /// </summary>
    public enum ActionMovementStick : uint
    {
        /// <summary></summary>
        LeftStick,
        /// <summary></summary>
        RightStick }

    /// <summary>
    /// Enums for color presets
    /// </summary>
    public enum GeneralColor : uint
    {
        /// <summary></summary>
        None,
        /// <summary></summary>
        Black,
        /// <summary></summary>
        White,
        /// <summary></summary>
        Yellow,
        /// <summary></summary>
        Orange,
        /// <summary></summary>
        Pink,
        /// <summary></summary>
        Red,
        /// <summary></summary>
        Purple,
        /// <summary></summary>
        Blue,
        /// <summary></summary>
        Green,
        /// <summary></summary>
        Brown,
        /// <summary></summary>
        Silver
    }

    /// <summary>
    /// Enums for general difficulty presets
    /// </summary>
    public enum GeneralDifficulty : uint {
        /// <summary></summary>
        Normal,
        /// <summary></summary>
        Easy,
        /// <summary></summary>
        Hard }

    /// <summary>
    /// Enums for sensitivity presets
    /// </summary>
    public enum GeneralSensitivity : uint {
        /// <summary></summary>
        Medium,
        /// <summary></summary>
        Low,
        /// <summary></summary>
        High }

    /// <summary>
    /// Enums for break control presets
    /// </summary>
    public enum RaceBrakeControl : uint {
        /// <summary></summary>
        Trigger,
        /// <summary></summary>
        Button }

    /// <summary>
    /// Enums for race cam presets
    /// </summary>
    public enum RaceCamLocation : uint {
        /// <summary></summary>
        Behind,
        /// <summary></summary>
        InFront,
        /// <summary></summary>
        Inside }

    /// <summary>
    /// Enum for race speed presets
    /// </summary>
    public enum RaceSpeedControl : uint {
        /// <summary></summary>
        Trigger,
        /// <summary></summary>
        Button }

    /// <summary>
    /// Enums for transmission presets
    /// </summary>
    public enum RaceTransition : uint {
        /// <summary></summary>
        Automatic,
        /// <summary></summary>
        Manual }

    /// <summary>
    /// Voice options presets
    /// </summary>
    public enum VoiceOptions : uint {
        /// <summary></summary>
        OnlyHeadset,
        /// <summary></summary>
        OnlySpeaker,
        /// <summary></summary>
        Both }

    /// <summary>
    /// Setting variable types
    /// </summary>
    public enum SettingType : byte
    {
        /// <summary></summary>
        Context,
        /// <summary></summary>
        UInt32,
        /// <summary></summary>
        Int64,
        /// <summary></summary>
        Double,
        /// <summary></summary>
        Unicode,
        /// <summary></summary>
        Float,
        /// <summary></summary>
        Binary,
        /// <summary></summary>
        DateTime,
        /// <summary></summary>
        Null = 0xff
    }

    /// <summary>
    /// Gamerzones presets
    /// </summary>
    public enum GamerZone : uint
    {
        /// <summary></summary>
        Xbox,
        /// <summary></summary>
        Recreation,
        /// <summary></summary>
        Pro,
        /// <summary></summary>
        Family,
        /// <summary></summary>
        Underground,
        /// <summary>NOT SURE IF IT WORKS</summary>
        Cheater,
        /// <summary></summary>
        Unknown = 0xFF
    }

    /// <summary>
    /// Region presets
    /// </summary>
    public enum Region : uint
    {
        /// <summary></summary>
        None,
        /// <summary></summary>
        UnitedArabEmirates,
        /// <summary></summary>
        Albania,
        /// <summary></summary>
        Armenia,
        /// <summary></summary>
        Argentinia,
        /// <summary></summary>
        Austria,
        /// <summary></summary>
        Australia,
        /// <summary></summary>
        Azerbaijan,
        /// <summary></summary>
        Belgium,
        /// <summary></summary>
        Bulgaria,
        /// <summary></summary>
        Bahrain,
        /// <summary></summary>
        BruneiDarussalam,
        /// <summary></summary>
        Bolivia,
        /// <summary></summary>
        Brazil,
        /// <summary></summary>
        Belarus,
        /// <summary></summary>
        Belize,
        /// <summary></summary>
        Canada,
        /// <summary></summary>
        Unknown1,
        /// <summary></summary>
        Switzerland,
        /// <summary></summary>
        Chile,
        /// <summary></summary>
        China,
        /// <summary></summary>
        Colombia,
        /// <summary></summary>
        CostaRica,
        /// <summary></summary>
        CzechRepublic,
        /// <summary></summary>
        Germany,
        /// <summary></summary>
        Denmark,
        /// <summary></summary>
        DominicanRepublic,
        /// <summary></summary>
        Algeria,
        /// <summary></summary>
        Ecuador,
        /// <summary></summary>
        Estonia,
        /// <summary></summary>
        Egypt,
        /// <summary></summary>
        Spain,
        /// <summary></summary>
        Finland,
        /// <summary></summary>
        FaroeIslands,
        /// <summary></summary>
        France,
        /// <summary></summary>
        GreatBritain,
        /// <summary></summary>
        Georgia,
        /// <summary></summary>
        Greece,
        /// <summary></summary>
        Guatemala,
        /// <summary></summary>
        HongKong,
        /// <summary></summary>
        Honduras,
        /// <summary></summary>
        Croatia,
        /// <summary></summary>
        Hungary,
        /// <summary></summary>
        Indonesia,
        /// <summary></summary>
        Ireland,
        /// <summary></summary>
        Israel,
        /// <summary></summary>
        India,
        /// <summary></summary>
        Iraq,
        /// <summary></summary>
        Iran,
        /// <summary></summary>
        Iceland,
        /// <summary></summary>
        Italy,
        /// <summary></summary>
        Jamaica,
        /// <summary></summary>
        Jordan,
        /// <summary></summary>
        Japan,
        /// <summary></summary>
        Kenya,
        /// <summary></summary>
        Kyrgyzstan,
        /// <summary></summary>
        Korea,
        /// <summary></summary>
        Kuwait,
        /// <summary></summary>
        Kazakhstan,
        /// <summary></summary>
        Lebanon,
        /// <summary></summary>
        Liechtenstein,
        /// <summary></summary>
        Lithuania,
        /// <summary></summary>
        Luxembourg,
        /// <summary></summary>
        Latvia,
        /// <summary></summary>
        Libya,
        /// <summary></summary>
        Morocco,
        /// <summary></summary>
        Monaco,
        /// <summary></summary>
        Macodonia,
        /// <summary></summary>
        Mongolia,
        /// <summary></summary>
        Macao,
        /// <summary></summary>
        Maldives,
        /// <summary></summary>
        Mexico,
        /// <summary></summary>
        Malaysia,
        /// <summary></summary>
        Nicaragua,
        /// <summary></summary>
        Netherlands,
        /// <summary></summary>
        Norway,
        /// <summary></summary>
        NewZealand,
        /// <summary></summary>
        Oman,
        /// <summary></summary>
        Panama,
        /// <summary></summary>
        Peru,
        /// <summary></summary>
        Philippines,
        /// <summary></summary>
        Pakistan,
        /// <summary></summary>
        Poland,
        /// <summary></summary>
        PuertoRico,
        /// <summary></summary>
        Portugal,
        /// <summary></summary>
        Paraguay,
        /// <summary></summary>
        Qatar,
        /// <summary></summary>
        Romania,
        /// <summary></summary>
        RussianFederation,
        /// <summary></summary>
        SaudiArabia,
        /// <summary></summary>
        Sweden,
        /// <summary></summary>
        Singapore,
        /// <summary></summary>
        Slovenia,
        /// <summary></summary>
        SlovakRepublic,
        /// <summary></summary>
        Unknown2,
        /// <summary></summary>
        ElSalvador,
        /// <summary></summary>
        Syria,
        /// <summary></summary>
        Thailand,
        /// <summary></summary>
        Tunisia,
        /// <summary></summary>
        Turkey,
        /// <summary></summary>
        TrinidadTobago,
        /// <summary></summary>
        Taiwan,
        /// <summary></summary>
        Ukraine,
        /// <summary></summary>
        UnitedStates,
        /// <summary></summary>
        Urugay,
        /// <summary></summary>
        Uzbekistan,
        /// <summary></summary>
        Venezuela,
        /// <summary></summary>
        VietNam,
        /// <summary></summary>
        Yemen,
        /// <summary></summary>
        SouthAfrica,
        /// <summary></summary>
        Zimbabwe

    }
    #endregion

    /// <summary>
    /// Object to hold title played data
    /// </summary>
    public sealed class TitlePlayedEntry : XDBFEntry
    {
        [CompilerGenerated]
        internal uint xid;
        /// <summary>
        /// Possible achievement count
        /// </summary>
        [CompilerGenerated]
        public uint PossibleCount; // Total number of achievements
        /// <summary>
        /// Earned count
        /// </summary>
        [CompilerGenerated]
        public uint EarnedCount; // How many earned
        /// <summary>
        /// Possible achievement total
        /// </summary>
        [CompilerGenerated]
        public uint PossibleWorth; // Total GS from title
        /// <summary>
        /// Earned total score
        /// </summary>
        [CompilerGenerated]
        public uint EarnedWorth; // Earned GS from title
        [CompilerGenerated]
        long LastLoadedL;
        [CompilerGenerated]
        string xTitle;
        [CompilerGenerated]
        bool xLoaded = false;
        [CompilerGenerated]
        byte[] xReserved;

        /// <summary>
        /// Title ID of entry
        /// </summary>
        public uint TitleID { get { return xid; } }
        /// <summary>
        /// Last played date/time
        /// </summary>
        public DateTime LastLoadedDT
        {
            get { return DateTime.FromFileTime(LastLoadedL); }
            set { LastLoadedL = value.ToFileTime(); }
        }
        /// <summary>
        /// Reserved data
        /// </summary>
        public byte[] Reserved { get { return xReserved; }}
        /// <summary>
        /// Title name
        /// </summary>
        public string Title { get { return xTitle; } }

        internal TitlePlayedEntry(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadDetails()
        {
            try
            {
                xRef.xIO.Position = Offset;
                xid = xRef.xIO.ReadUInt32();
                PossibleCount = xRef.xIO.ReadUInt32();
                EarnedCount = xRef.xIO.ReadUInt32();
                PossibleWorth = xRef.xIO.ReadUInt32();
                EarnedWorth = xRef.xIO.ReadUInt32();
                xReserved = xRef.xIO.ReadBytes(0xC);
                LastLoadedL = xRef.xIO.ReadInt64();
                xTitle = xRef.xIO.ReadString(StringForm.Unicode, Size - 0x28, StringRead.ToNull);
                xLoaded = true;
                return true;
            }
            catch { return false; }
        }

        internal bool xUpdate()
        {
            if (!xLoaded)
                return false;
            try
            {
                xRef.xIO.Position = Offset;
                xRef.xIO.Write(xid);
                xRef.xIO.Write(PossibleCount);
                xRef.xIO.Write(EarnedCount);
                xRef.xIO.Write(PossibleWorth);
                xRef.xIO.Write(EarnedWorth);
                xRef.xIO.Write(xReserved);
                xRef.xIO.Write(LastLoadedL);
                xRef.UpdateSync(NameSpace.Title, ID, SyncType.Server);
                xRef.xIO.Flush();
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Update title information
        /// </summary>
        /// <returns></returns>
        public bool Update()
        {
            if (!xRef.ParseCheck())
                return false;
            return (xUpdate() & !(xRef.xActive = false));
        }
    }

    /// <summary>
    /// Object to handle profile GPD
    /// </summary>
    public sealed class DashGPD : GPD
    {
        /// <summary>
        /// Array to hold titles played
        /// </summary>
        public TitlePlayedEntry[] TitlesPlayed { get { return xTitlesPlayed.ToArray(); } }

        bool xUpdateGS(uint NewGS)
        {
            try
            {
                Setting x = xGetSetting(GPDIDs.GCardCredit, SettingType.UInt32);
                if (x == null)
                    xAddSetting((long)GPDIDs.GCardCredit, NewGS, true, SyncType.Server);
                else
                {
                    x.Data = NewGS;
                    x.xUpdate(SyncType.Server);
                }
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Specify a gamerscore to update to
        /// </summary>
        /// <param name="NewGS"></param>
        /// <returns></returns>
        public bool UpdateGamerScore(uint NewGS)
        {
            if (!ParseCheck())
                return false;
            return (xUpdateGS(NewGS) & !(xActive = false));
        }

        /// <summary>
        /// Updates the gamerscore total
        /// </summary>
        /// <returns></returns>
        public bool UpdateGamerScore()
        {
            if (!ParseCheck())
                return false;
            return (xUpdateGS(xCalcGST()) & !(xActive = false));
        }

        /// <summary>
        /// Grabs current gamerscore
        /// </summary>
        /// <returns></returns>
        public uint GetGamerscore()
        {
            if (!ParseCheck())
                return 0;
            Setting x = xGetSetting(GPDIDs.GCardCredit, SettingType.UInt32);
            xActive = false;
            if (x != null)
                return (uint)x.Data;
            return 0;
        }

        /// <summary>
        /// Calculates the total achievement count possible
        /// </summary>
        /// <returns></returns>
        public uint CalculateAchievementsPossible()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = 0;
            foreach (TitlePlayedEntry x in TitlesPlayed)
                xReturn += x.PossibleCount;
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Calculates the total achievements earned
        /// </summary>
        /// <returns></returns>
        public uint CalculateAchievementsTotal()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = 0;
            foreach (TitlePlayedEntry x in TitlesPlayed)
                xReturn += x.EarnedCount;
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Calculates total possbile gamerscore
        /// </summary>
        /// <returns></returns>
        public uint CalculateGamerScorePossible()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = 0;
            foreach (TitlePlayedEntry x in TitlesPlayed)
                xReturn += x.PossibleWorth;
            xActive = false;
            return xReturn;
        }

        uint xCalcGST()
        {
            uint xReturn = 0;
            foreach (TitlePlayedEntry x in TitlesPlayed)
                xReturn += x.EarnedWorth;
            return xReturn;
        }

        /// <summary>
        /// Calculates Gamerscore earned
        /// </summary>
        /// <returns></returns>
        public uint CalculateGamerScoreTotal()
        {
            if (!ParseCheck())
                return 0;
            uint xReturn = xCalcGST();
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Adds a game through the ID hack
        /// </summary>
        /// <param name="xTitleID"></param>
        /// <returns></returns>
        public bool AddGameViaLiveHack(uint xTitleID)
        {
            if (!ParseCheck())
                return false;
            if (xTitleID == 0 ||
                xTitleID == 0xFFFE07D1 || // Dash
                xTitleID == 0xFFFE07DE) // Avatar?
                return (xActive = false);
            foreach (TitlePlayedEntry x in xTitlesPlayed)
            {
                if (xTitleID == x.TitleID)
                {
                    xActive = false;
                    throw GPDExcepts.HasID;
                }
            }
            try
            {
                if (xTitlesPlayed.Count == 0)
                    return (xActive = false);
                xTitlesPlayed[0].xid = xTitleID;
                return (xTitlesPlayed[0].xUpdate() &&
                !(xActive = false));
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Adds a game via the GPD
        /// </summary>
        /// <param name="xTitle"></param>
        /// <returns></returns>
        public bool AddGameViaGPD(GameGPD xTitle)
        {
            if (!ParseCheck())
                return false;
            if (xTitle.TitleID == 0 ||
                xTitle.TitleID == 0xFFFE07D1 || // Dash
                xTitle.TitleID == 0xFFFE07DE) // Avatar?
                return (xActive = false);
            foreach (TitlePlayedEntry x in xTitlesPlayed)
            {
                if (xTitle.TitleID != x.TitleID)
                    continue;
                xActive = false;
                throw GPDExcepts.HasID;
            }
            try
            {
                string xName = xTitle.GetStringByID((long)GPDIDs.ThisTitle);
                if (xName == null)
                {
                    xActive = false;
                    throw GPDExcepts.NameError;
                }
                if (!xTitle.xErase())
                    return (xActive = false);
                int xsize = 0x28 + ((xName.Length + 1) * 2);
                int xPosition = AllocateData(xsize);
                if (xPosition == -1)
                    return (xActive = false);
                XDBFEntry xEnt = new XDBFEntry(NameSpace.Title, xTitle.TitleID, xPosition, xsize, this);
                xIO.Position = xPosition + HeaderSize;
                xIO.Write(xTitle.TitleID);
                xIO.Write((uint)xTitle.xAchievements.Count);
                xIO.Write(xTitle.xCalcUT());
                xIO.Write(xTitle.xCalcGSP());
                xIO.Write(xTitle.xCalcGST());
                xIO.Write(new byte[0x14]);
                xIO.Write(xName, StringForm.Unicode);
                xIO.Write((short)0);
                xIO.Flush();
                TitlePlayedEntry z = new TitlePlayedEntry(xEnt);
                if (!z.LoadDetails())
                    return (xActive = false);
                xTitlesPlayed.Add(z);
                Setting x = xGetSetting(GPDIDs.GCardTitlesPlayed, SettingType.UInt32);
                if (x == null)
                    xAddSetting((long)GPDIDs.GCardTitlesPlayed, xTitlesPlayed.Count, true, SyncType.Locale);
                else
                {
                    uint xdata = (uint)x.Data;
                    xdata++;
                    x.Data = xdata;
                    x.xUpdate(SyncType.Locale);
                }
                UpdateSync(NameSpace.Title, xTitle.TitleID, SyncType.Server);
                return (UpdateHeader() &
                !(xActive = false));
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Updates a title from it's GPD
        /// </summary>
        /// <param name="xTitle"></param>
        /// <returns></returns>
        public bool UpdateTitle(GameGPD xTitle)
        {
            if (!ParseCheck())
                return false;
            if (xTitle.TitleID == 0 ||
                xTitle.TitleID == 0xFFFE07D1 || // Dash
                xTitle.TitleID == 0xFFFE07DE) // Avatar?
                return (xActive = false);
            try
            {
                xTitle.FCAC();
                bool xfound = false;
                foreach (TitlePlayedEntry x in xTitlesPlayed)
                {
                    if (x.TitleID != xTitle.TitleID)
                        continue;
                    x.EarnedCount = xTitle.CalculateUnlockCount();
                    x.EarnedWorth = xTitle.CalculateTotalCred();
                    x.PossibleWorth = xTitle.CalculatePossibleCred();
                    x.xUpdate();
                    xfound = true;
                    break;
                }
                if (!xfound)
                    return (xActive = false);
                return (xUpdateGS(xCalcGST()) & !(xActive = false));
            }
            catch { return false; }
        }

        /// <summary>
        /// Initializes an instance of this object
        /// </summary>
        /// <param name="GPDLocale"></param>
        public DashGPD(string GPDLocale) : base(GPDLocale, 0xFFFE07D1)
        {
            if (!IsValid)
                return;
            xAchievements.Clear();
            for (int i = 0; i < xTitlesPlayed.Count; i++)
            {
                if (!xTitlesPlayed[i].LoadDetails())
                    xTitlesPlayed.RemoveAt(i--);
            }
            xActive = false;
        }
    }
}