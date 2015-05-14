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
using System.ComponentModel;
using System.Drawing;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Diagnostics;
using X360.IO;
using X360.Other;
using X360.STFS;

namespace X360.Profile
{
    /// <summary>
    /// Object to hold GPD errors
    /// </summary>
    [DebuggerStepThrough]
    public static class GPDExcepts
    {
        [CompilerGenerated]
        static readonly Exception xHasID = new Exception("This GPD Contains this ID already.");
        [CompilerGenerated]
        static readonly Exception xIsntXDBF = new Exception("Not a valid XDBF file.");
        [CompilerGenerated]
        static readonly Exception xNameError = new Exception("GPD does not contain a name");
        [CompilerGenerated]
        static readonly Exception xInUse = new Exception("Current file is in use");
        
        /// <summary>
        /// Has ID
        /// </summary>
        public static Exception HasID { get { return xHasID; }}
        /// <summary>
        /// Not XDBF
        /// </summary>
        public static Exception IsntXDBF { get { return xIsntXDBF; }}
        /// <summary>
        /// Name Error
        /// </summary>
        public static Exception NameError { get { return xNameError; }}
        /// <summary>
        /// Cannot multitask
        /// </summary>
        public static Exception InUse { get { return xInUse; }}
    }
    
    /// <summary> ID's found in GPDs </summary>
    public enum GPDIDs : long
    {
        /// <summary>Data pertaining to the Title</summary>
        ThisTitle = 0x8000,
        /// <summary>Account permissions</summary>
        UserPermissions = 0x10040000,
        /// <summary>Gamer Type</summary>
        UserGamerType = 0x10040001,
        /// <summary>Y Axis inversion setting</summary>
        UserYAxisInversion = 0x10040002,
        /// <summary>Controller Vibration setting</summary>
        UserControllerVibration = 0x10040003,
        /// <summary>Avatar information setting</summary>
        AvatarInformation = 0x63E80044,
        /// <summary>Title reserved data 1</summary>
        TitleSpecific1 = 0x63E83FFF,
        /// <summary>Title reserved data 2</summary>
        TitleSpecific2 = 0x63E83FFE,
        /// <summary>Title reserved data 3</summary>
        TitleSpecific3 = 0x63E83FFD,
        /// <summary>Account Gamerzon</summary>
        GCardZone = 0x10040004,
        /// <summary>Account Region</summary>
        GCardRegion = 0x10040005,
        /// <summary>Account Gamerscore</summary>
        GCardCredit = 0x10040006,
        /// <summary>Unknown</summary>
        UserPresenceState = 0x10040007,
        /// <summary>Has vision camera?</summary>
        GCardHasVision = 0x10040008,
        /// <summary>Account reputation</summary>
        GCardRep = 0x5004000B,
        /// <summary>Account mute setting</summary>
        UserVoiceMuted = 0x1004000C,
        /// <summary>Account voice through speaker setting</summary>
        UserVoiceThruSpeakers = 0x1004000D,
        /// <summary>Account voice volume setting</summary>
        UserVoiceVolume = 0x1004000E,
        /// <summary>Account gamerpicture reference</summary>
        GCardPictureKey = 0x4064000F,
        /// <summary>Account personal picture reference</summary>
        GCardPersonalPicture = 0x40640010,
        /// <summary>Account motto</summary>
        GCardMotto = 0x402C0011,
        /// <summary>Account titles played count</summary>
        GCardTitlesPlayed = 0x10040012,
        /// <summary>Account achievements earned count</summary>
        GCardAchievementsEarned = 0x10040013,
        /// <summary>Account default difficulty setting</summary>
        GCardDifficulty = 0x10040015,
        /// <summary>Account sensitivity setting</summary>
        UserControlSensitivity = 0x10040018,
        /// <summary>Account preferred color 1</summary>
        UserPreferredColor1 = 0x1004001D,
        /// <summary>Account preferred color 2</summary>
        UserPreferredColor2 = 0x1004001E,
        /// <summary>Account auto aim</summary>
        UserAutoAim = 0x10040022,
        /// <summary>Account auto center setting</summary>
        UserAutoCenter = 0x10040023,
        /// <summary>Account movement setting</summary>
        UserActionMovementControl = 0x10040024,
        /// <summary>Account default transmission setting</summary>
        UserRaceTransmission = 0x10040026,
        /// <summary>Account racecam position setting</summary>
        UserRaceCamPosition = 0x10040027,
        /// <summary>Account race breaking setting</summary>
        UserRaceBrakeControl = 0x10040028,
        /// <summary>Account accelarating setting</summary>
        UserRaceAccelControl = 0x10040029,
        /// <summary>Gamerscore earned on a title</summary>
        GCardTitleCreditEarned = 0x10040038,
        /// <summary>Achievement count of a title</summary>
        GCardTitleAchievementsEarned = 0x10040039,
        /// <summary>Unknown</summary>
        UserTier = 0x1004003A,
        /// <summary>Unknown</summary>
        UserMessangerSignUpState = 0x1004003B,
        /// <summary>Account auto sign in</summary>
        UserMessangerAutoSignIn = 0x1004003C,
        /// <summary>Save password</summary>
        UserSaveLIVEPassword = 0x1004003D,
        /// <summary>Show friends to others?</summary>
        UserShowFriends = 0x1004003E,
        /// <summary>Unknown</summary>
        GCardServiceType = 0x1004003F,
        /// <summary>Account name input</summary>
        GCardName = 0x41040040,
        /// <summary>Account location input</summary>
        GCardUserLocale = 0x40520041,
        /// <summary>Unknown</summary>
        GCardUserURL = 0x41900042,
        /// <summary>Account Bio</summary>
        GCardUserBio = 0x43E80043,
        /// <summary>Xbox.com setting</summary>
        WebEmailFormat = 0x10042000,
        /// <summary>Xbox.com setting</summary>
        WebFlags = 0x10042001,
        /// <summary>Unknown</summary>
        WebSpam = 0x10042002,
        /// <summary>Xbox.com setting</summary>
        WebFavGenre = 0x10042003,
        /// <summary>Xbox.com setting</summary>
        WebFavGame = 0x10042004,
        /// <summary>Xbox.com setting</summary>
        WebFavGame1 = 0x10042005,
        /// <summary>Xbox.com setting</summary>
        WebFavGame2 = 0x10042006,
        /// <summary>Xbox.com setting</summary>
        WebFavGame3 = 0x10042007,
        /// <summary>Xbox.com setting</summary>
        WebFavGame4 = 0x10042008,
        /// <summary>Xbox.com setting</summary>
        WebFavGame5 = 0x10042009,
        /// <summary>Xbox.com setting</summary>
        WebPlatformsOwned = 0x1004200A,
        /// <summary>Xbox.com setting</summary>
        WebConnectionSpeed = 0x1004200B,
        /// <summary>Unknown</summary>
        WebFlash = 0x1004200C,
        /// <summary>Unknown</summary>
        WebVideoPref = 0x1004200D,
        /// <summary>Unknown</summary>
        UserCruxMediaPic = 0x406403E8,
        /// <summary>Unknown</summary>
        UserCruxMediaStyle1 = 0x100403EA,
        /// <summary>Unknown</summary>
        UserCruxMediaStyle2 = 0x100403EB,
        /// <summary>Unknown</summary>
        UserCruxMediaStyle3 = 0x100403EC,
        /// <summary>Unknown</summary>
        UserCruxTopAlbum1 = 0x100403ED,
        /// <summary>Unknown</summary>
        UserCruxTopAlbum2 = 0x100403EE,
        /// <summary>Unknown</summary>
        UserCruxTopAlbum3 = 0x100403EF,
        /// <summary>Unknown</summary>
        UserCruxTopAlbum4 = 0x100403F0,
        /// <summary>Unknown</summary>
        UserCruxTopAlbum5 = 0x100403F1,
        /// <summary>Unknown</summary>
        UserCruxOfflineID = 0x603403F2,
        /// <summary>Unknown</summary>
        UserCruxBKGDImage = 0x100403F3,
        /// <summary>Unknown</summary>
        UserCruxLastChangeTime = 0x700803F4,
        /// <summary>Unknown</summary>
        UserCruxTopMusic = 0x60A803F5,
        /// <summary>Unknown</summary>
        UserCruxMotto = 0x410003F6,
        /// <summary>Unknown</summary>
        UserCruxTopMediaID1 = 0x601003F7,
        /// <summary>Unknown</summary>
        UserCruxTopMediaID2 = 0x601003F8,
        /// <summary>Unknown</summary>
        UserCruxTopMediaID3 = 0x601003F9,
        /// <summary>Unknown</summary>
        UserCruxBio = 0x43E803FA,
        /// <summary>Unknown</summary>
        UserCruxBGSmallPublic = 0x406403FD,
        /// <summary>Unknown</summary>
        UserCruxBGLargePublic = 0x406403F,
        /// <summary>GPD Sync ID table</summary>
        IndexRecord = 0x100000000,
        /// <summary>GPD Sync record</summary>
        SyncRecord = 0x200000000,
    }

    /// <summary>
    /// How to preform the XDBF item sync
    /// </summary>
    public enum SyncType : byte
    {
        /// <summary>No sync</summary>
        None,
        /// <summary>Sync to the server</summary>
        Server,
        /// <summary>Sync only locally</summary>
        Locale
    }

    /// <summary>XDBF item types</summary>
    public enum NameSpace : ushort
    {
        /// <summary>Bad item</summary>
        Nothing,
        /// <summary>Achievement entry</summary>
        Achievement,
        /// <summary>Image entry</summary>
        Image,
        /// <summary>Setting entry</summary>
        Setting,
        /// <summary>Title data entry</summary>
        Title,
        /// <summary>Text data</summary>
        String
    }

    #region Classes
    /// <summary>
    /// Object to hold XDBF entries
    /// </summary>
    public class XDBFEntry
    {
        [CompilerGenerated]
        internal int xOffset;
        [CompilerGenerated]
        NameSpace xNS = NameSpace.Nothing;
        [CompilerGenerated]
        internal int xSize;
        [CompilerGenerated]
        long xID;
        [CompilerGenerated]
        internal GPD xRef;

        /// <summary>
        /// Offset in file
        /// </summary>
        public int Offset { get { return xOffset + xRef.HeaderSize; } }
        /// <summary>
        /// Entry type
        /// </summary>
        public NameSpace NS { get { return xNS; } }
        /// <summary>
        /// Size of entry
        /// </summary>
        public int Size { get { return xSize; } }
        /// <summary>
        /// ID of entry
        /// </summary>
        public long ID { get { return xID; } }
        internal bool Valid { get { return (xRef != null); }}

        internal XDBFEntry(GPD GPDRef)
        {
            xRef = GPDRef;
            xNS = (NameSpace)GPDRef.xIO.ReadUInt16();
            xID = GPDRef.xIO.ReadInt64();
            xOffset = GPDRef.xIO.ReadInt32();
            xSize = GPDRef.xIO.ReadInt32();
        }

        internal XDBFEntry(NameSpace ns, long id, int offset, int size, GPD xGPDRef)
        {
            if (xGPDRef.xEntryMax == xGPDRef.xEntryCurrent && !xRef.IncreaseXDBFCount())
                return;
            xNS = ns;
            xID = id;
            xOffset = offset;
            xSize = size;
            xRef = xGPDRef;
        }

        internal XDBFEntry(XDBFEntry xCopy)
        {
            xOffset = xCopy.xOffset;
            xSize = xCopy.xSize;
            xNS = xCopy.xNS;
            xID = xCopy.xID;
            xRef = xCopy.xRef;
        }
    }

    /// <summary>
    /// Object to hold Sync/Entry ID's
    /// </summary>
    public sealed class SyncPair
    {
        [CompilerGenerated]
        internal long xID;
        [CompilerGenerated]
        internal long xSync;
        
        /// <summary>
        /// Entry ID
        /// </summary>
        public long ID { get { return xID; } }
        /// <summary>
        /// Sync ID
        /// </summary>
        public long Sync { get { return xSync; } }
        
        internal SyncPair(long xid, long xsync)
        {
            xID = xid;
            xSync = xsync;
        }
    }

    /// <summary>
    /// Object to hold all sync pairs for namespace
    /// </summary>
    public sealed class RecordEntry : XDBFEntry
    {
        [CompilerGenerated]
        internal List<SyncPair> xpairs = new List<SyncPair>();

        /// <summary>
        /// Sync pairs in record
        /// </summary>
        public SyncPair[] SyncPairs { get { return xpairs.ToArray(); } }

        internal RecordEntry(XDBFEntry xEntry) : base(xEntry) {}

        internal bool xLoadDetails()
        {
            try
            {
                xRef.xIO.Position = Offset;
                DJsIO xbuff = new DJsIO(xRef.xIO.ReadBytes(Size), true);
                xbuff.Position = 0;
                while (xbuff.Position < xbuff.Length)
                {
                    try { xpairs.Add(new SyncPair(xbuff.ReadInt64(), xbuff.ReadInt64())); }
                    catch { }
                }
                return true;
            }
            catch { return false; }
        }

        internal bool xUpdate()
        {
            try
            {
                int xsize = xpairs.Count * 0x10;
                if (Size > xsize)
                {
                    new FreeSpaceEntry(xRef, xOffset + xsize, Size - xsize);
                    xRef.PatchFree();
                }
                else if (Size < xsize)
                {
                    int off = xRef.AllocateData(xsize);
                    if (off == -1)
                        return false;
                    new FreeSpaceEntry(xRef, xOffset, Size);
                    xRef.PatchFree();
                    xOffset = off;
                }
                DJsIO xbuff = new DJsIO(new byte[xsize], true);
                xbuff.Position = 0;
                foreach (SyncPair x in xpairs)
                {
                    xbuff.Write(x.ID);
                    xbuff.Write(x.Sync);
                }
                xbuff.Flush();
                xRef.xIO.Position = Offset;
                xRef.xIO.Write(xbuff.ReadStream());
                xRef.xIO.Flush();
                if (xsize > xSize)
                    xRef.UpdateHeader();
                xSize = xsize;
                return true;
            }
            catch { return false; }
        }

        int SortID(Setting Ee, Setting Er)
        {
            return Ee.ID.CompareTo(Er.ID);
        }

        int SortDate(TitlePlayedEntry Ee, TitlePlayedEntry Er)
        {
            return Ee.LastLoadedDT.CompareTo(Er.LastLoadedDT);
        }
    }

    /// <summary>
    /// Object to hold a GPD entry
    /// </summary>
    public sealed class Setting : XDBFEntry
    {
        [CompilerGenerated]
        byte xContentID;
        [CompilerGenerated]
        int xSettingID;
        [CompilerGenerated]
        object xdata = null;
        [CompilerGenerated]
        int Var2;
        [CompilerGenerated]
        bool xLoaded = false;

        /// <summary>
        /// Data
        /// </summary>
        public object Data
        {
            get { return xdata; }
            set
            {
                switch (value.GetType().ToString().ToLower())
                {
                    case "system.single":
                        if (ContentType != SettingType.Float)
                            return;
                        xdata = value;
                        break;

                    case "system.double":
                        if (ContentType != SettingType.Double)
                            return;
                        xdata = value;
                        break;

                    case "system.int32":
                        if (ContentType != SettingType.UInt32)
                            return;
                        xdata = value;
                        break;

                    case "system.uint32":
                        if (ContentType != SettingType.UInt32)
                            return;
                        xdata = (uint)value;
                        break;

                    case "system.int64":
                        if (ContentType != SettingType.Int64 || ContentType != SettingType.DateTime)
                            return;
                        xdata = value;
                        break;

                    case "system.datetime":
                        {
                            if (ContentType != SettingType.DateTime)
                                return;
                            DateTime xIn = (DateTime)value;
                            xdata = xIn.ToFileTime();
                        }
                        break;

                    case "system.byte[]":
                        {
                            if (ContentType != SettingType.Context || ContentType != SettingType.Binary)
                                return;
                            xdata = (byte[])value;
                        }
                        break;

                    case "system.string":
                        {
                            if (ContentType != SettingType.Unicode)
                                return;
                            xdata = (string)value;
                        }
                        break;

                    default: break;
                }
            }
        }
        /// <summary>
        /// Setting ID
        /// </summary>
        public int SettingID { get { return xSettingID; } }
        /// <summary>
        /// IDType
        /// </summary>
        public GPDIDs IDType { get { return (GPDIDs)ID; } }
        /// <summary>
        /// Content Type
        /// </summary>
        public SettingType ContentType { get { return (SettingType)xContentID; } }

        internal Setting(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadDetails()
        {
            try
            {
                int Var1 = 0;
                xRef.xIO.Position = Offset;
                xSettingID = xRef.xIO.ReadInt32();
                xRef.xIO.Position += 4;
                xContentID = xRef.xIO.ReadByte();
                xRef.xIO.Position += 7;
                switch (ContentType)
                {
                    case SettingType.Binary:
                        Var1 = xRef.xIO.ReadInt32();
                        Var2 = xRef.xIO.ReadInt32();
                        break;

                    case SettingType.Context:
                        xdata = xRef.xIO.ReadInt32();
                        break;

                    case SettingType.DateTime:
                        xdata = xRef.xIO.ReadInt64();
                        break;

                    case SettingType.Double:
                        xdata = xRef.xIO.ReadDouble();
                        break;

                    case SettingType.Float:
                        xdata = xRef.xIO.ReadSingle();
                        break;

                    case SettingType.UInt32:
                        xdata = xRef.xIO.ReadUInt32();
                        break;

                    case SettingType.Int64:
                        xdata = xRef.xIO.ReadInt64();
                        break;

                    case SettingType.Unicode:
                        Var1 = xRef.xIO.ReadInt32();
                        Var2 = xRef.xIO.ReadInt32();
                        break;

                    default: Data = xRef.xIO.ReadBytes(8); break;
                }
                if (Var1 > 0)
                    xdata = xRef.xIO.ReadBytes(Var1);
                if (ContentType == SettingType.Unicode)
                    xdata = Encoding.BigEndianUnicode.GetString((byte[])xdata).Replace("\0", "");
                xLoaded = true;
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Updates the data
        /// </summary>
        /// <param name="xType"></param>
        /// <returns></returns>
        public bool Update(SyncType xType)
        {
            if (!xRef.ParseCheck())
                return false;
            return (xUpdate(xType) & !(xRef.xActive = false));
        }

        internal bool xUpdate(SyncType xType)
        {
            if (!xLoaded)
                return false;
            try
            {
                xRef.xIO.Position = Offset;
                xRef.xIO.Write(xSettingID);
                xRef.xIO.Position += 4;
                xRef.xIO.Write(xContentID);
                xRef.xIO.Position += 7;
                switch (ContentType)
                {
                    case SettingType.Context:
                    case SettingType.Binary:
                        {
                            byte[] xIn = (byte[])xdata;
                            int currentsize = (0x18 + xIn.Length);
                            if (currentsize < xSize)
                                xRef.xFreeEnts.Add(new FreeSpaceEntry(xRef, xOffset + xIn.Length, xSize - currentsize));
                            else if (currentsize > xSize)
                            {
                                int pos = xRef.AllocateData(currentsize);
                                if (pos == -1)
                                    return false;
                                xRef.xIO.Position = pos + xRef.HeaderSize;
                                xRef.xIO.Write(xSettingID);
                                xRef.xIO.Write(new byte[4]);
                                xRef.xIO.Write(xContentID);
                                xRef.xIO.Write(new byte[7]);
                                xRef.xFreeEnts.Add(new FreeSpaceEntry(xRef, xOffset, xSize));
                                xOffset = pos;
                            }
                            xSize = currentsize;
                            xRef.xIO.Write(((byte[])xdata).Length);
                            xRef.xIO.Write(Var2);
                            xRef.xIO.Write((byte[])xdata);
                            xRef.xIO.Flush();
                        }
                        break;

                    case SettingType.Unicode:
                        {
                            string xIn = (string)xdata;
                            int stringlen = ((xIn.Length + 1) * 2);
                            int currentsize = (0x18 + stringlen);
                            if (currentsize < xSize)
                                xRef.xFreeEnts.Add(new FreeSpaceEntry(xRef, xOffset + stringlen, xSize - currentsize));
                            else if (currentsize > xSize)
                            {
                                int pos = xRef.AllocateData(currentsize);
                                if (pos == -1)
                                    return false;
                                xRef.xIO.Position = pos + xRef.HeaderSize;
                                xRef.xIO.Write(xSettingID);
                                xRef.xIO.Write(new byte[4]);
                                xRef.xIO.Write(xContentID);
                                xRef.xIO.Write(new byte[7]);
                                xRef.xFreeEnts.Add(new FreeSpaceEntry(xRef, xOffset, xSize));
                                xOffset = pos;
                            }
                            xSize = currentsize;
                            xRef.xIO.Write(stringlen);
                            xRef.xIO.Write(Var2);
                            xRef.xIO.Write((string)xdata, StringForm.Unicode);
                            xRef.xIO.Write((short)0);
                            xRef.xIO.Flush();
                        }
                        break;

                    case SettingType.DateTime:
                        xRef.xIO.Write((long)xdata);
                        xRef.xIO.Flush();
                        return true;

                    case SettingType.Float:
                        xRef.xIO.Write((float)xdata);
                        xRef.xIO.Flush();
                        break;

                    case SettingType.Double:
                        xRef.xIO.Write((double)xdata);
                        xRef.xIO.Flush();
                        break;

                    case SettingType.UInt32:
                        if (xdata.GetType().ToString().ToLower() == "system.uint32")
                            xRef.xIO.Write((uint)xdata);
                        else xRef.xIO.Write((int)xdata);
                        xRef.xIO.Flush();
                        break;

                    case SettingType.Int64:
                        xRef.xIO.Write((long)xdata);
                        xRef.xIO.Flush();
                        break;

                    default: return true;
                }
                if (xType != SyncType.None)
                    return xRef.UpdateSync(NameSpace.Setting, ID, xType) & xRef.UpdateHeader();
                return xRef.UpdateHeader();
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Object to hold syncing data
    /// </summary>
    public sealed class SyncEntry : XDBFEntry
    {
        [CompilerGenerated]
        internal long xNext = 1;
        [CompilerGenerated]
        internal long xLastSync = 0;
        [CompilerGenerated]
        internal long xServerSync;
        [CompilerGenerated]
        bool xLoaded = false;

        /// <summary>
        /// Last sync time
        /// </summary>
        public DateTime ServerSync { get { return TimeStamps.LongToDateTime(xServerSync); } }
        /// <summary>
        /// Last sync'ed entry
        /// </summary>
        public long Last { get { return xLastSync; }}
        /// <summary>
        /// Next assignable sync ID
        /// </summary>
        public long Next { get { return xNext; }}

        internal SyncEntry(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadSyncs()
        {
            try
            {
                xRef.xIO.Position = Offset;
                xNext = xRef.xIO.ReadInt64();
                xLastSync = xRef.xIO.ReadInt64();
                xServerSync = xRef.xIO.ReadInt64();
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
                xRef.xIO.Write(xNext);
                xRef.xIO.Write(xLastSync);
                xRef.xIO.Write(xServerSync);
                xRef.xIO.Flush();
                return true;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Object to hold GPD images
    /// </summary>
    public sealed class ImageEntry : XDBFEntry
    {
        [CompilerGenerated]
        Image xImage = null;

        bool xLoaded { get { return xImage != null; } }
        /// <summary>
        /// Image
        /// </summary>
        public Image ImageOutput
        {
            get
            {
                if (xLoaded)
                    return xImage;
                else return PublicResources.NoImage;
            }
        }

        internal ImageEntry(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadImage()
        {
            try
            {
                xRef.xIO.Position = Offset;
                xImage = xRef.xIO.ReadBytes(Size).BytesToImage();
                return true;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Object to hold GPD strings
    /// </summary>
    public sealed class StringEntry : XDBFEntry
    {
        /// <summary>
        /// Setting data
        /// </summary>
        [CompilerGenerated]
        public string Data;
        [CompilerGenerated]
        bool xLoaded = false;

        internal StringEntry(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadString()
        {
            try
            {
                xRef.xIO.Position = Offset;
                Data = xRef.xIO.ReadString(StringForm.Unicode, Size / 2);
                xLoaded = true;
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Update the data
        /// </summary>
        /// <returns></returns>
        public bool Update()
        {
            if (!xRef.ParseCheck())
                return false;
            return (xUpdate() & !(xRef.xActive = false));
        }

        internal bool xUpdate()
        {
            if (!xLoaded)
                return false;
            try
            {
                xRef.xIO.Position = Offset;
                xRef.xIO.Write(Data, StringForm.Unicode);
                xRef.xIO.Flush();
                return true;
            }
            catch { return false; }
        }
    }

    /// <summary>
    /// Object to hold unknown entries
    /// </summary>
    public sealed class OtherEntry : XDBFEntry
    {
        [CompilerGenerated]
        byte[] xData;
        [CompilerGenerated]
        bool xLoaded = false;
        
        /// <summary>
        /// Binary Data
        /// </summary>
        public byte[] Data
        {
            get { return xData; }
            set
            {
                if (xData.Length >= value.Length)
                {
                    for (int i = 0; i < xData.Length; i++)
                        xData[i] = value[i];
                }
                else
                {
                    int i = 0;
                    for (int j = 0; j < value.Length; j++)
                    {
                        i = j;
                        xData[j] = value[j];
                    }
                    for (int j = i; j < xData.Length; j++)
                        xData[j] = 0xFF;
                }
            }
        }

        internal OtherEntry(XDBFEntry xEntry) : base(xEntry) { }

        internal bool LoadData()
        {
            try
            {
                xRef.xIO.Position = Offset;
                xData = xRef.xIO.ReadBytes(Size);
                xLoaded = true;
                return true;
            }
            catch { return false; }
        }

        /// <summary>
        /// Update the data
        /// </summary>
        /// <returns></returns>
        public bool Update()
        {
            if (!xRef.ParseCheck())
                return false;
            return (xUpdate() & !(xRef.xActive = false));
        }

        internal bool xUpdate()
        {
            if (!xLoaded)
                return false;
            try
            {
                xRef.xIO.Position = Offset;
                xRef.xIO.Write(xData);
                xRef.xIO.Flush();
                return true;
            }
            catch { return false; }
        }
    }

    internal class FreeSpaceEntry
    {
        [CompilerGenerated]
        internal int Size;
        [CompilerGenerated]
        internal int offset;
        [CompilerGenerated]
        GPD xRef;

        public int Offset { get { return offset + xRef.HeaderSize; } }

        public FreeSpaceEntry(GPD xref)
        {
            xRef = xref;
            offset = xref.xIO.ReadInt32();
            Size = xref.xIO.ReadInt32();
        }

        public FreeSpaceEntry(GPD xref, int xOffset, int xSize)
        {
            if (xref.xFreeMax == xref.xFreeCurrent)
                return;
            xRef = xref;
            offset = xOffset;
            Size = xSize;
            xref.xFreeEnts.Add(this);
        }

        public FreeSpaceEntry(XDBFEntry xent)
        {
            xRef = xent.xRef;
            Size = xent.Size;
            offset = xent.xOffset;
        }

        public bool Allocate(int xSize)
        {
            try
            {
                if (xSize <= Size)
                {
                    Size -= xSize;
                    offset += xSize;
                    return true;
                }
                return false;
            }
            catch { return false; }

        }
    }
    #endregion

    /// <summary>
    /// Object to hold Game Progress Data
    /// </summary>
    public class GPD
    {
        [CompilerGenerated]
        internal bool xActive = false;
        /// <summary>
        /// Title ID associated with the file
        /// </summary>
        [CompilerGenerated]
        public uint TitleID = 0;
        [CompilerGenerated]
        internal DJsIO xIO = null;
        [CompilerGenerated]
        internal int xEntryMax = 0;
        [CompilerGenerated]
        internal int xEntryCurrent = 0;
        [CompilerGenerated]
        internal int xFreeMax = 0;
        [CompilerGenerated]
        internal int xFreeCurrent = 0;
        [CompilerGenerated]
        internal bool xIsInErase = false;
        [CompilerGenerated]
        internal List<AchievementEntry> xAchievements = new List<AchievementEntry>();
        [CompilerGenerated]
        internal List<TitlePlayedEntry> xTitlesPlayed = new List<TitlePlayedEntry>();
        [CompilerGenerated]
        internal List<SyncEntry> xSyncs = new List<SyncEntry>();
        [CompilerGenerated]
        internal List<ImageEntry> xImages = new List<ImageEntry>();
        [CompilerGenerated]
        internal List<StringEntry> xStrings = new List<StringEntry>();
        [CompilerGenerated]
        internal List<Setting> xUserSettings = new List<Setting>();
        [CompilerGenerated]
        internal List<OtherEntry> xUnknownData = new List<OtherEntry>();
        [CompilerGenerated]
        internal List<RecordEntry> xIndexRecords = new List<RecordEntry>();
        [CompilerGenerated]
        internal List<FreeSpaceEntry> xFreeEnts = new List<FreeSpaceEntry>();
        /// <summary>GPD Images</summary>
        public ImageEntry[] Images { get { return xImages.ToArray(); }}
        /// <summary>GPD Strings</summary>
        public StringEntry[] Strings { get { return xStrings.ToArray(); }}
        /// <summary>GPD Settings</summary>
        public Setting[] UserSettings { get { return xUserSettings.ToArray(); }}
        /// <summary>GPD Unknown Data</summary>
        public OtherEntry[] UnknownData { get { return xUnknownData.ToArray(); }}
        internal int HeaderSize { get { return (0x18 + (xEntryMax * 0x12) + (xFreeMax * 8)); }}
        /// <summary>GPD Parsed correctly</summary>
        public bool IsValid { get { return (xIO != null); } }
        /// <summary>GPD Sync Indexes</summary>
        public RecordEntry[] IndexRecords { get { return xIndexRecords.ToArray(); } }
        /// <summary>GPD Sync status'</summary>
        public SyncEntry[] SyncRecords { get { return xSyncs.ToArray(); } }

        /* All functions are self explanitory, here's a general structure:
         * Header
         * - Info
         * - XDBF Entries (Data entries)
         * - Free space entries (old unused data that can be reused)
         * Entries
         * 
         * To comment everything out in GPD's would be useless and repetitive
         * 
         * Achievements, Settings, and TitlesPlayed have syncs associated with them.
         * This is used when updating information to the servers without having to
         * constantly update every single item, it would be a bad system, uneffecient.
         * Each syncable NameSpace has a Sync Entry and an Index Record (I don't know
         * why they wouldn't just combine them, maybe it's a spacial issue).  Sync Entry
         * contains the last synced entry and the next sync ID to be used when needing to
         * assign a sync ID.  To update a sync, you just get the next SyncID, assign it to
         * the respected SyncPair in the IndexRecord, move it to the bottom, and increase
         * the next sync by one.  The Xbox checks it and will sync all ID's that are
         * between the last sync and the next sync.  One trick is you have to make sure you
         * order the entries in the header properly. */

        internal GPD(string GPDLocale, uint xTitleID)
        {
            xActive = true;
            xIO = new DJsIO(GPDLocale, DJFileMode.Open, true);
            if (!xIO.Accessed)
                return;
            TitleID = xTitleID;
            xIO.IsBigEndian = true;
            xIO.Position = 0;
            if (xIO.ReadUInt32() != 0x58444246)
                throw GPDExcepts.IsntXDBF;
            xIO.Position += 4; // Version
            xEntryMax = xIO.ReadInt32();
            xEntryCurrent = xIO.ReadInt32();
            xFreeMax = xIO.ReadInt32();
            xFreeCurrent = xIO.ReadInt32();
            List<XDBFEntry> xEntries = new List<XDBFEntry>();
            try
            {
                for (int i = 0; i < xEntryCurrent; i++)
                    xEntries.Add(new XDBFEntry(this));
                xIO.Position = (0x18 + (xEntryMax * 0x12));
                for (int i = 0; i < xFreeCurrent - 1; i++)
                {
                    FreeSpaceEntry x = new FreeSpaceEntry(this);
                    xFreeEnts.Add(x);
                }
                PatchFree();
                for (int i = 0; i < xEntries.Count; i++)
                {
                    XDBFEntry x = xEntries[i];
                    switch (x.ID)
                    {
                        case (long)GPDIDs.IndexRecord:
                            {
                                RecordEntry xThisRec = new RecordEntry(x);
                                xIndexRecords.Add(xThisRec);
                                if (xThisRec.NS == NameSpace.Achievement && xAchievements.Count == 0)
                                    xIsInErase = true;
                            }
                            break;
                        case (long)GPDIDs.SyncRecord:
                            {
                                SyncEntry xThisSync = new SyncEntry(x);
                                xSyncs.Add(xThisSync);
                            }
                            break;
                        default:
                            {
                                switch (x.NS)
                                {
                                    case NameSpace.Nothing: xEntries.RemoveAt(i--); break;

                                    case NameSpace.Achievement:
                                        {
                                            AchievementEntry xCurrentAchievment = new AchievementEntry(x);
                                            xAchievements.Add(xCurrentAchievment);
                                        }
                                        break;

                                    case NameSpace.Image:
                                        {
                                            if (!ContainsEntry(x))
                                            {
                                                ImageEntry xCurrentImage = new ImageEntry(x);
                                                xImages.Add(xCurrentImage);
                                            }
                                        }
                                        break;

                                    case NameSpace.Setting:
                                        {
                                            if (!ContainsEntry(x))
                                            {
                                                Setting xThisSetting = new Setting(x);
                                                xUserSettings.Add(xThisSetting);
                                            }
                                        }
                                        break;

                                    case NameSpace.Title:
                                        {
                                            if (!ContainsEntry(x))
                                            {
                                                TitlePlayedEntry xTitle = new TitlePlayedEntry(x);
                                                xTitlesPlayed.Add(xTitle);
                                            }
                                        }
                                        break;

                                    case NameSpace.String:
                                        {
                                            if (!ContainsEntry(x))
                                            {
                                                StringEntry x_String = new StringEntry(x);
                                                xStrings.Add(x_String);
                                            }
                                        }
                                        break;

                                    default: xEntries.RemoveAt(i--); xEntryCurrent--; break;
                                }
                            }
                            break;
                    }
                }
                for (int i = 0; i < xUserSettings.Count; i++)
                {
                    if (!xUserSettings[i].LoadDetails())
                    {
                        OtherEntry xUnknown = new OtherEntry(xUserSettings[i]);
                        xUnknownData.Add(xUnknown);
                        xUserSettings.RemoveAt(i--);
                    }
                }
                for (int i = 0; i < xUnknownData.Count; i++)
                {
                    if (!xUnknownData[i].LoadData())
                    {
                        xUnknownData.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                for (int i = 0; i < xImages.Count; i++)
                {
                    if (!xImages[i].LoadImage())
                    {
                        xImages.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                for (int i = 0; i < xStrings.Count; i++)
                {
                    if (!xStrings[i].LoadString())
                    {
                        xStrings.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                for (int i = 0; i < xIndexRecords.Count; i++)
                {
                    if (!xIndexRecords[i].xLoadDetails())
                    {
                        xIndexRecords.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                for (int i = 0; i < xSyncs.Count; i++)
                {
                    if (!xSyncs[i].LoadSyncs())
                    {
                        xSyncs.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                for (int i = 0; i < xFreeEnts.Count; i++)
                {
                    if (xFreeEnts[i].Size == 0)
                    {
                        xFreeEnts.RemoveAt(i--);
                        xEntryCurrent--;
                    }
                }
                xUserSettings.Sort(sortbyid);
                xAchievements.Sort(sortbyid);
            }
            catch (Exception x) { xIO = null; throw x; }
        }

        internal bool IncreaseXDBFCount()
        {
            DJsIO xtemp = new DJsIO(true);
            try
            {
                xtemp.Position = (HeaderSize + (0xFF * 0x12));
                xIO.Position = HeaderSize;
                int len = (int)(xIO.Length - xIO.Position);
                for (int i = 0; i < len; i++)
                    xtemp.Write(xIO.ReadByte());
                xtemp.Position = 0;
                xtemp.Write((uint)AllMagic.XDBF);
                xtemp.Write((int)0x10000);
                xEntryMax += 0xFF;
                xtemp.Write((int)xEntryMax);
                xtemp.Write((int)xEntryCurrent);
                xtemp.Write((int)xFreeCurrent);
                xtemp.Write((int)xFreeMax);
                List<XDBFEntry> xEntries = GetEntries();
                foreach (XDBFEntry x in xEntries)
                {
                    xtemp.Write((ushort)x.NS);
                    xtemp.Write(x.ID);
                    xtemp.Write(x.xOffset);
                    xtemp.Write(x.Size);
                }
                xtemp.Write(new byte[0x12 * (xEntryMax - xEntries.Count)]);
                foreach (FreeSpaceEntry x in xFreeEnts)
                {
                    if (x.Size != 0)
                    {
                        xtemp.Write(x.offset);
                        xtemp.Write(x.Size);
                    }
                }
                int xdatasize = (int)(xtemp.Length - HeaderSize);
                xtemp.Write(xdatasize);
                xtemp.Write((int)(((-1) - xdatasize)));
                xtemp.Write(new byte[0x8 * (xFreeMax - (xFreeEnts.Count + 1))]);
                xtemp.Flush();
                xtemp.Close();
                xIO.Close();
                VariousFunctions.MoveFile(xtemp.FileNameLong, xIO.FileNameLong);
                xIO = new DJsIO(xIO.FileNameLong, DJFileMode.Open, true);
                return true;
            }
            catch { xtemp.Dispose(true); return false; }
        }

        internal bool ParseCheck()
        {
            if (!IsValid)
                throw GPDExcepts.IsntXDBF;
            if (xActive)
                return false;
            return (xActive = true);
        }

        internal Setting xGetSetting(GPDIDs xIDType, SettingType xDataType)
        {
            if (!Enum.IsDefined(typeof(SettingType), xDataType))
                return null;
            foreach (Setting x in xUserSettings)
            {
                if ((long)x.IDType == (long)xIDType && x.ContentType == xDataType)
                    return x;
            }
            return null;
        }

        internal Setting xGetSetting(GPDIDs xIDType)
        {
            foreach (Setting x in xUserSettings)
            {
                if ((long)x.IDType == (long)xIDType)
                    return x;
            }
            return null;
        }

        /// <summary>
        /// Grabs a setting via ID
        /// </summary>
        /// <param name="xIDType"></param>
        /// <returns></returns>
        public Setting GetSetting(GPDIDs xIDType)
        {
            if (!ParseCheck())
                return null;
            Setting xReturn = xGetSetting(xIDType);
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Grabs a setting via ID and Data type
        /// </summary>
        /// <param name="xIDType"></param>
        /// <param name="xDataType"></param>
        /// <returns></returns>
        public Setting GetSetting(GPDIDs xIDType, SettingType xDataType)
        {
            if (!ParseCheck())
                return null;
            Setting xReturn = xGetSetting(xIDType, xDataType);
            xActive = false;
            return xReturn;
        }

        /// <summary>
        /// Grabs a string via its ID
        /// </summary>
        /// <param name="xID"></param>
        /// <returns></returns>
        public string GetStringByID(long xID)
        {
            if (!ParseCheck())
                return null;
            foreach(StringEntry x in xStrings)
            {
                if (x.ID != xID)
                    continue;
                xActive = false;
                return x.Data;
            }
            xActive = false;
            return null;
        }
            
        /// <summary>
        /// Grabs an image via ID
        /// </summary>
        /// <param name="xID"></param>
        /// <returns></returns>
        public Image GetImageByID(long xID)
        {
            if (!ParseCheck())
                return null;
            foreach (ImageEntry x in xImages)
            {
                if (x.ID != xID)
                    continue;
                xActive = false;
                return x.ImageOutput;
            }
            xActive = false;
            return null;
        }

        internal int AllocateData(int xSize)
        {
            int xReturn = (int)xIO.Length - HeaderSize;
            for (int i = 0; i < xFreeEnts.Count; i++)
            {
                if (xFreeEnts[i].Size < xSize)
                    continue;
                xReturn = xFreeEnts[i].offset;
                xFreeEnts[i].Allocate(xSize);
                if (xFreeEnts[i].Size == 0)
                    xFreeEnts.RemoveAt(i);
                return xReturn;
            }
            List<XDBFEntry> xents = GetEntries();
            if (xents.Count < xEntryMax)
                return xReturn;
            else return -1;
        }

        int sortbyid(XDBFEntry x1, XDBFEntry x2) { return x1.ID.CompareTo(x2.ID); }

        XDBFEntry[] SortID(XDBFEntry[] xIn)
        {
            List<XDBFEntry> xTemp = new List<XDBFEntry>();
            foreach (XDBFEntry x in xIn)
                xTemp.Add(x);
            xTemp.Sort(new Comparison<XDBFEntry>(sortbyid));
            return xTemp.ToArray();
        }

        int SortDate(AchievementEntry Ee, AchievementEntry Er)
        {
            return Ee.UnlockTime.CompareTo(Er.UnlockTime);
        }

        int SortID(AchievementEntry Ee, AchievementEntry Er)
        {
            return Ee.ID.CompareTo(Er.ID);
        }

        List<XDBFEntry> GetEntries() { return GetEntries(xIsInErase); }

        List<XDBFEntry> GetEntries(bool EraseMode)
        {
            List<XDBFEntry> xReturn = new List<XDBFEntry>();
            if (EraseMode)
                xReturn.AddRange(SortID(xAchievements.ToArray()));
            foreach (XDBFEntry x in xIndexRecords)
            {
                if (x.NS == NameSpace.Achievement)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            foreach (XDBFEntry x in xSyncs)
            {
                if (x.NS == NameSpace.Achievement)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            if (!EraseMode)
                xReturn.AddRange(SortID(xAchievements.ToArray()));
            xReturn.AddRange(SortID(xImages.ToArray()));
            xReturn.AddRange(SortID(xUserSettings.ToArray()));
            foreach (XDBFEntry x in xIndexRecords)
            {
                if (x.NS == NameSpace.Setting)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            foreach (XDBFEntry x in xSyncs)
            {
                if (x.NS == NameSpace.Setting)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            xReturn.AddRange(SortID(xTitlesPlayed.ToArray()));
            foreach (XDBFEntry x in xIndexRecords)
            {
                if (x.NS == NameSpace.Title)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            foreach (XDBFEntry x in xSyncs)
            {
                if (x.NS == NameSpace.Title)
                {
                    xReturn.Add(x);
                    break;
                }
            }
            xReturn.AddRange(SortID(xStrings.ToArray()));
            return xReturn;
        }

        internal bool UpdateHeader() { return UpdateHeader(false); }

        internal bool UpdateHeader(bool EraseMode)
        {
            try
            {
                List<XDBFEntry> xEntries = GetEntries(EraseMode);
                xIO.Position = 8;
                xIO.Write(xEntryMax);
                xEntryCurrent = xEntries.Count;
                xIO.Write(xEntries.Count);
                xIO.Write(xFreeMax);
                xFreeCurrent = xFreeEnts.Count + 1;
                xIO.Write(xFreeCurrent);
                foreach (XDBFEntry x in xEntries)
                {
                    xIO.Write((ushort)x.NS);
                    xIO.Write(x.ID);
                    xIO.Write(x.xOffset);
                    xIO.Write(x.Size);
                }
                xIO.Write(new byte[0x12 * (xEntryMax - xEntries.Count)]);
                foreach (FreeSpaceEntry x in xFreeEnts)
                {
                    if (x.Size != 0)
                    {
                        xIO.Write(x.offset);
                        xIO.Write(x.Size);
                    }
                }
                int xdatasize = (int)(xIO.Length - HeaderSize);
                xIO.Write(xdatasize);
                xIO.Write((int)(((-1) - xdatasize)));
                xIO.Write(new byte[0x8 * (xFreeMax - (xFreeEnts.Count + 1))]);
                xIO.Flush();
                return true;
            }
            catch { return false; }
        }

        int sortbyoff(FreeSpaceEntry x1, FreeSpaceEntry x2) { return x1.offset.CompareTo(x2.offset); }

        internal bool PatchFree()
        {
            // Combines free space next to eachother
            xFreeEnts.Sort(new Comparison<FreeSpaceEntry>(sortbyoff));
            for (int i = 0; i < xFreeEnts.Count; i++)
            {
                if (i == 0)
                    continue;
                if ((xFreeEnts[i - 1].offset + xFreeEnts[i - 1].Size) < xFreeEnts[i].offset)
                    continue;
                // Last entry size = this entry stop spot - last entry offset
                xFreeEnts[i - 1].Size = ((xFreeEnts[i].offset + xFreeEnts[i].Size) - xFreeEnts[i - 1].offset);
                xFreeEnts.RemoveAt(i--);
            }
            return true;
        }

        internal int sortns(XDBFEntry Ee, XDBFEntry Er) { return ((byte)Ee.NS).CompareTo((byte)Er.NS); }

        internal bool UpdateSync(NameSpace xNS, long ID, SyncType xType)
        {
            if (xType == SyncType.None)
                return true;
            int idx1 = -1, idx2 = -1;
            for (int i = 0; i < xIndexRecords.Count; i++)
            {
                if (xIndexRecords[i].NS != xNS)
                    continue;
                idx1 = i;
                break;
            }
            for (int i = 0; i < xSyncs.Count; i++)
            {
                if (xSyncs[i].NS != xNS)
                    continue;
                idx2 = i;
                break;
            }
            if (idx1 == -1 || idx2 == -1)
            {
                RecordEntry xidx = null;
                SyncEntry xsync = null;
                if (idx1 == -1)
                {
                    int xsize;
                    if (xNS != NameSpace.Achievement)
                        xsize = 0x10;
                    else xsize = (0x10 * xAchievements.Count);
                    int pos = AllocateData(xsize);
                    if (pos == -1)
                        return false;
                    xidx = new RecordEntry(new XDBFEntry(xNS, (long)GPDIDs.IndexRecord, pos, xsize, this));
                    if (xNS == NameSpace.Achievement)
                    {
                        for (int i = 0; i < xAchievements.Count; i++)
                            xidx.xpairs.Add(new SyncPair(xAchievements[i].ID, i + 1));
                    }
                }
                if (idx2 == -1)
                {
                    int pos = AllocateData(0x18);
                    if (pos == -1)
                        return false;
                    xsync = new SyncEntry(new XDBFEntry(xNS, (long)GPDIDs.SyncRecord, pos, 0x18, this));
                    if (xNS == NameSpace.Achievement)
                    {
                        xsync.xLastSync = 0;
                        xsync.xNext = xAchievements.Count + 1;
                    }
                }
                if (xidx != null)
                {
                    xIndexRecords.Add(xidx);
                    xIndexRecords.Sort(new Comparison<RecordEntry>(sortns));
                    idx1 = xIndexRecords.IndexOf(xidx);
                }
                if (xsync != null)
                {
                    xSyncs.Add(xsync);
                    xSyncs.Sort(new Comparison<SyncEntry>(sortns));
                    idx2 = xSyncs.IndexOf(xsync);
                }
            }
            long curnext = xSyncs[idx2].Next;
            for (int i = 0; i < xIndexRecords[idx1].xpairs.Count; i++)
            {
                if (xIndexRecords[idx1].xpairs[i].ID == ID)
                    xIndexRecords[idx1].xpairs.RemoveAt(i--);
            }
            if (xType == SyncType.Server)
                xIndexRecords[idx1].xpairs.Add(new SyncPair(ID, curnext++));
            else xIndexRecords[idx1].xpairs.Add(new SyncPair(ID, 0));
            if (xIndexRecords[idx1].xUpdate())
            {
                xSyncs[idx2].xNext = curnext;
                return xSyncs[idx2].xUpdate();
            }
            return false;
        }

        bool ContainsEntry(XDBFEntry xEnt)
        {
            switch (xEnt.NS)
            {
                case NameSpace.Image:
                    {
                        for (int i = 0; i < xImages.Count; i++)
                        {
                            if (xImages[i].ID != xEnt.ID)
                                continue;
                            new FreeSpaceEntry(xEnt);
                            PatchFree();
                            return true;
                        }
                        return false;
                    }

                case NameSpace.Setting:
                    {
                        for (int i = 0; i < xUserSettings.Count; i++)
                        {
                            if (xUserSettings[i].ID != xEnt.ID)
                                continue;
                            new FreeSpaceEntry(xEnt);
                            PatchFree();
                            return true;
                        }
                        return false;
                    }

                case NameSpace.Title:
                    {
                        for (int i = 0; i < xTitlesPlayed.Count; i++)
                        {
                            if (xTitlesPlayed[i].ID == xEnt.ID)
                            {
                                new FreeSpaceEntry(xEnt);
                                PatchFree();
                                return true;
                            }
                        }
                        return false;
                    }

                case NameSpace.String:
                    {
                        for (int i = 0; i < xStrings.Count; i++)
                        {
                            if (xStrings[i].ID != xEnt.ID)
                                continue;
                            new FreeSpaceEntry(xEnt);
                            PatchFree();
                            return true;
                        }
                        return false;
                    }

                default: return false;
            }
        }

        bool ContainsOther(XDBFEntry xEnt)
        {
            for (int i = 0; i < xUnknownData.Count; i++)
            {
                if (xUnknownData[i].ID != xEnt.ID)
                    continue;
                new FreeSpaceEntry(xEnt);
                PatchFree();
                return true;
            }
            return false;
        }

        int ContainsSetting(long ID)
        {
            for (int i = 0; i < xUserSettings.Count; i++)
            {
                if (xUserSettings[i].ID == ID)
                    return i;
            }
            return -1;
        }

        internal bool xAddSetting(long ID, object Data, bool AutomaticOverwrite, SyncType xSync)
        {
            try
            {
                int idx = ContainsSetting(ID);
                if (idx != -1 && !AutomaticOverwrite)
                    return false;
                int xSize = 0x18;
                long pos = 0;
                SettingType xType = SettingType.Null;
                for (int i = 0; i < 2; i++)
                {
                    switch (Data.GetType().ToString().ToLower())
                    {
                        case "system.single":
                            {
                                if (i == 0)
                                    xType = SettingType.Float;
                                else
                                {
                                    xIO.Write((float)Data);
                                    xIO.Write((int)0);
                                }
                            }
                            break;

                        case "system.double":
                            {
                                if (i == 0)
                                    xType = SettingType.Double;
                                else xIO.Write((double)Data);
                            }
                            break;

                        case "system.int32":
                            {
                                if (i == 0)
                                    xType = SettingType.UInt32;
                                else
                                {
                                    xIO.Write((int)Data);
                                    xIO.Write((int)0);
                                }
                            }
                            break;

                        case "system.uint32":
                            {
                                if (i == 0)
                                    xType = SettingType.UInt32;
                                else
                                {
                                    xIO.Write((uint)Data);
                                    xIO.Write((uint)0);
                                }
                            }
                            break;

                        case "system.int64":
                            {
                                if (i == 0)
                                    xType = SettingType.Int64;
                                else xIO.Write((long)Data);
                            }
                            break;

                        case "system.datetime":
                            {
                                if (i == 0)
                                    xType = SettingType.DateTime;
                                else xIO.Write(((DateTime)Data).ToFileTime());
                            }
                            break;

                        case "system.byte[]":
                            {
                                if (i == 0)
                                {
                                    xType = SettingType.Binary;
                                    byte[] xIn = (byte[])Data;
                                    xSize += xIn.Length;
                                }
                                else
                                {
                                    byte[] xIn = (byte[])Data;
                                    xIO.Write(xIn.Length);
                                    xIO.Write(new byte[4]);
                                    xIO.Write(xIn);
                                }
                            }
                            break;

                        case "system.string":
                            {
                                if (i == 0)
                                {
                                    xType = SettingType.Unicode;
                                    string xIn = (string)Data;
                                    xSize += ((xIn.Length + 1) * 2);
                                }
                                else
                                {
                                    string xIn = (string)Data;
                                    xIO.Write((xIn.Length + 1) * 2);
                                    xIO.Write(new byte[4]);
                                    xIO.Write(xIn, StringForm.Unicode);
                                    xIO.Write((short)0);
                                }
                            }
                            break;

                        default: return (xActive = false);
                    }
                    if (i == 0)
                    {
                        if (xType == SettingType.Null)
                            return false;
                        pos = AllocateData(xSize);
                        if (pos == -1)
                            return false;
                        else if (idx != -1)
                        {
                            xFreeEnts.Add(new FreeSpaceEntry(xUserSettings[idx]));
                            PatchFree();
                            xUserSettings.RemoveAt(idx);
                        }
                        xIO.Position = pos + HeaderSize;
                        xIO.Write((uint)ID);
                        xIO.Write(new byte[4]);
                        xIO.Write((byte)xType);
                        xIO.Write(new byte[7]);
                    }
                    else xIO.Flush();
                }
                XDBFEntry xent = new XDBFEntry(NameSpace.Setting, ID, (int)pos, xSize, this);
                if (!xent.Valid)
                    return false;
                xUserSettings.Add(new Setting(xent));
                xUserSettings[xUserSettings.Count - 1].LoadDetails();
                if (xSync != SyncType.None)
                    UpdateSync(NameSpace.Setting, ID, xSync);
                return UpdateHeader();
            }
            catch { return false; }
        }

        /// <summary>
        /// Adds a setting with sync option
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="Data"></param>
        /// <param name="AutomaticOverwrite"></param>
        /// <param name="xType"></param>
        /// <returns></returns>
        public bool AddSetting(long ID, object Data, bool AutomaticOverwrite, SyncType xType)
        {
            if (!ParseCheck())
                return false;
            return ((xAddSetting(ID, Data, AutomaticOverwrite, xType)) & !(xActive = false));
        }

        /// <summary>
        /// Adds a string
        /// </summary>
        /// <param name="ID"></param>
        /// <param name="xIn"></param>
        /// <param name="AutomaticOverwrite"></param>
        /// <returns></returns>
        public bool AddString(long ID, string xIn, bool AutomaticOverwrite)
        {
            if (!ParseCheck())
                return false;
            try
            {
                int idx = -1;
                for (int i = 0; i < xStrings.Count; i++)
                {
                    if (xStrings[i].ID != ID)
                        continue;
                    if (!AutomaticOverwrite)
                        return (xActive = false);
                    idx = i;
                    break;
                }
                int xoff = AllocateData(xIn.Length * 2);
                if (xoff == -1)
                    return (xActive = false);
                else if (idx != -1)
                {
                    new FreeSpaceEntry(this, xStrings[idx].xOffset, xStrings[idx].Size);
                    PatchFree();
                    xStrings.RemoveAt(idx);
                }
                foreach (RecordEntry x in xIndexRecords)
                {
                    if (x.NS == NameSpace.Setting)
                    {
                        for (int i = 0; i < x.xpairs.Count; i++)
                        {
                            if (x.xpairs[i].ID == ID)
                                x.xpairs.RemoveAt(i--);
                        }
                        x.xUpdate();
                    }
                }
                xIO.Position = xoff + HeaderSize;
                xIO.Write(xIn, StringForm.Unicode);
                StringEntry y = new StringEntry(new XDBFEntry(NameSpace.String, ID, xoff, xIn.Length * 2, this));
                y.LoadString();
                xStrings.Add(y);
                return (UpdateHeader() & !(xActive = false));
            }
            catch { return (xActive = false); }
        }

        /// <summary>
        /// Adds an image
        /// </summary>
        /// <param name="xIn"></param>
        /// <param name="ID"></param>
        /// <param name="AutomaticOverWrite"></param>
        /// <returns></returns>
        public bool AddImage(Image xIn, long ID, bool AutomaticOverWrite)
        {
            if (!ParseCheck())
                return false;
            try
            {
                int idx = -1;
                for (int i = 0; i < xImages.Count; i++)
                {
                    if (ID != xImages[i].ID)
                        continue;
                    if (!AutomaticOverWrite)
                        return (xActive = false);
                    idx = i;
                    break;
                }
                byte[] xData = xIn.ImageToBytes(System.Drawing.Imaging.ImageFormat.Png);
                if (xData.LongLength > 0xFFFF)
                    return (xActive = false);
                int xoff = AllocateData(xData.Length);
                if (xoff == -1)
                    return (xActive = false);
                else if (idx != -1)
                {
                    new FreeSpaceEntry(xImages[idx]);
                    PatchFree();
                    xImages.RemoveAt(idx);
                }
                xIO.Position = xoff + HeaderSize;
                xIO.Write(xData);
                xIO.Flush();
                ImageEntry x = new ImageEntry(new XDBFEntry(NameSpace.Image, ID, xoff, xData.Length, this));
                if (!x.LoadImage())
                    return (xActive = false);
                xImages.Add(x);
                return (UpdateHeader() & !(xActive = false));
            }
            catch { return (xActive = false); }
        }

        internal bool xDeleteSetting(long ID)
        {
            try
            {
                for (int i = 0; i < xUserSettings.Count; i++)
                {
                    if (xUserSettings[i].SettingID != ID)
                        continue;
                    new FreeSpaceEntry(xUserSettings[i]);
                    xUserSettings.RemoveAt(i--);
                }
                foreach (RecordEntry x in xIndexRecords)
                {
                    if (x.NS == NameSpace.Setting)
                    {
                        for (int i = 0; i < x.xpairs.Count; i++)
                        {
                            if (x.xpairs[i].ID == ID)
                                x.xpairs.RemoveAt(i);
                        }
                        x.xUpdate();
                    }
                }
                PatchFree();
                return UpdateHeader();
            }
            catch { return false; }
        }

        /// <summary>
        /// Deletes a setting
        /// </summary>
        /// <param name="ID"></param>
        /// <returns></returns>
        public bool DeleteSetting(long ID)
        {
            if (!ParseCheck())
                return false;
            return xDeleteSetting(ID) & (xActive = false);
        }

        /// <summary>
        /// Closes the IO
        /// </summary>
        /// <returns></returns>
        public bool Close()
        {
            if (!ParseCheck())
                return false;
            xIO.Close();
            xAchievements.Clear();
            xStrings.Clear();
            xImages.Clear();
            xUserSettings.Clear();
            xIndexRecords.Clear();
            xSyncs.Clear();
            xUnknownData.Clear();
            xFreeEnts.Clear();
            return true;
        }

        /// <summary>
        /// File Location
        /// </summary>
        public string FileLocation { get { return xIO.FileNameLong; } }

        /// <summary>
        /// Grabs the stream
        /// </summary>
        /// <returns></returns>
        public DJsIO GetStream() { return xIO; }
    }
}