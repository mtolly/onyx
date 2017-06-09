#pragma once

#include "winnames.h"
#include "../AvatarAsset/AvatarAssetDefinintions.h"
#include <iostream>
#include <vector>

#include "XboxInternals_global.h"

using std::vector;
using std::wstring;

// Description: different types of entries in an Xdbf file
enum EntryType
{
    Achievement = 1,
    Image,
    Setting,
    Title,
    String,
    AvatarAward
};

struct XdbfHeader
{
    DWORD magic;
    DWORD version;
    DWORD entryTableLength;
    DWORD entryCount;
    DWORD freeMemTableLength;
    DWORD freeMemTableEntryCount;
};

struct XdbfEntry
{
    EntryType type;
    UINT64 id;
    DWORD addressSpecifier;
    DWORD length;
};

struct XdbfFreeMemEntry
{
    DWORD addressSpecifier;
    DWORD length;
};

struct SyncEntry
{
    UINT64 entryID;
    UINT64 syncValue;
};

struct SyncList
{
    vector<SyncEntry> synced;
    vector<SyncEntry> toSync;
    bool lengthChanged;

    XdbfEntry entry;
};

struct SyncData
{
    UINT64 nextSyncID, lastSyncID;
    struct tm *lastSyncedTime;

    XdbfEntry entry;
};

struct XdbfEntryGroup
{
    vector<XdbfEntry> entries;
    SyncData syncData;
    SyncList syncs;
};

enum SettingEntryType
{
    Context,
    Int32,
    Int64,
    Double,
    UnicodeString,
    Float,
    Binary,
    TimeStamp
};

struct SettingEntry
{
    SettingEntryType type;
    XdbfEntry entry;
    union
    {
        unsigned int int32;
        INT64 int64;
        double doubleData;
        wstring *str;
        float floatData;
        struct
        {
            BYTE *data;
            DWORD length;
        } binaryData;
        time_t timeStamp;
    };

public:
    SettingEntry()
    {
        type = (SettingEntryType)0;
    }
};

struct StringEntry
{
    wstring ws;

    XdbfEntry entry;
    DWORD initialLength;
};

struct ImageEntry
{
    BYTE *image;
    DWORD length;

    XdbfEntry entry;
    DWORD initialLength;

    ImageEntry()
    {
        image = NULL;
        length = 0;
        initialLength = 0;
    }
};

enum AchievementFlags
{
    Completion = 1,             //The player has completed a game.
    Leveling = 2,               //The player has increased their level.
    Unlock = 3,                 //The player has unlocked a new game feature.
    Event = 4,                  //The player has completed a special goal in the game.
    Tournament = 5,             //The player has received an award in a tournament-level event.
    Checkpoint = 6,             //The player has reached a certain point or completed a specific portion of the game.
    Other = 7,                  //Other achievement type.
    Secret = 8,                 //Achievment details not shown before unlocked.
    UnlockedOnline = 0x10000,   //Indicates the achievement was achieved online.
    Unlocked = 0x20000          //Indicates the achievement was achieved.
};

struct AchievementEntry
{
    XdbfEntry entry;
    DWORD initialLength;

    DWORD structSize;
    DWORD achievementID;
    DWORD imageID;
    DWORD gamerscore;
    DWORD flags;
    unsigned int unlockTime;
    unsigned int unlockTimeMilliseconds;
    wstring name;
    wstring lockedDescription;
    wstring unlockedDescription;
};

enum TitleEntryFlags
{
    SyncAchievement = 1,
    DownloadAchievementImage = 2,
    DownloadAvatarAward = 0x10,
    SyncAvatarAward = 0x20
};

struct TitleEntry
{
    XdbfEntry entry;
    DWORD initialLength;

    DWORD titleID;
    DWORD achievementCount;
    DWORD achievementsUnlocked;
    DWORD totalGamerscore;
    DWORD gamerscoreUnlocked;
    WORD achievementsUnlockedOnline;
    BYTE avatarAwardsEarned;
    BYTE avatarAwardCount;
    BYTE maleAvatarAwardsEarned;
    BYTE maleAvatarAwardCount;
    BYTE femaleAvatarAwardsEarned;
    BYTE femaleAvatarAwardCount;
    DWORD flags;
    time_t lastPlayed;
    wstring gameName;
};

enum XboxLiveCountry
{
    Albania = 2,
    Algeria = 0x1b,
    Argentina = 4,
    Armenia = 3,
    Australia = 6,
    Austria = 5,
    Azerbaijan = 7,
    Bahrain = 10,
    Belarus = 14,
    Belgium = 8,
    Belize = 15,
    Bolivia = 12,
    Brazil = 13,
    BruneiDarussalam = 11,
    Bulgaria = 9,
    Canada = 0x10,
    Chile = 0x13,
    China = 20,
    Colombia = 0x15,
    CostaRica = 0x16,
    Croatia = 0x29,
    CzechRepublic = 0x17,
    Denmark = 0x19,
    DominicanRepublic = 0x1a,
    Ecuador = 0x1c,
    Egypt = 30,
    ElSalvador = 0x5f,
    Estonia = 0x1d,
    FaroeIslands = 0x21,
    Finland = 0x20,
    France = 0x22,
    Georgia = 0x24,
    Germany = 0x18,
    Greece = 0x25,
    Guatemala = 0x26,
    Honduras = 40,
    HongKong = 0x27,
    Hungary = 0x2a,
    Iceland = 0x31,
    India = 0x2e,
    Indonesia = 0x2b,
    Iran = 0x30,
    Iraq = 0x2f,
    Ireland = 0x2c,
    Israel = 0x2d,
    Italy = 50,
    Jamaica = 0x33,
    Japan = 0x35,
    Jordan = 0x34,
    Kazakhstan = 0x3a,
    Kenya = 0x36,
    Korea = 0x38,
    Kuwait = 0x39,
    Kyrgyzstan = 0x37,
    Latvia = 0x3f,
    Lebanon = 0x3b,
    LibyanArabJamahiriya = 0x40,
    Liechtenstein = 60,
    Lithuania = 0x3d,
    Luxembourg = 0x3e,
    Macao = 0x45,
    Macedonia = 0x43,
    Malaysia = 0x48,
    Maldives = 70,
    Mexico = 0x47,
    Monaco = 0x42,
    Mongolia = 0x44,
    Morocco = 0x41,
    Netherlands = 0x4a,
    NewZealand = 0x4c,
    Nicaragua = 0x49,
    Norway = 0x4b,
    Oman = 0x4d,
    Pakistan = 0x51,
    Panama = 0x4e,
    Paraguay = 0x55,
    Peru = 0x4f,
    Philippines = 80,
    Poland = 0x52,
    Portugal = 0x54,
    PuertoRico = 0x53,
    Qatar = 0x56,
    Romania = 0x57,
    RussianFederation = 0x58,
    SaudiArabia = 0x59,
    Singapore = 0x5b,
    Slovakia = 0x5d,
    Slovenia = 0x5c,
    SouthAfrica = 0x6d,
    Spain = 0x1f,
    Sweden = 90,
    Switzerland = 0x12,
    SyrianArabRepublic = 0x60,
    Taiwan = 0x65,
    Thailand = 0x61,
    TrinidadAndTobago = 100,
    Tunisia = 0x62,
    Turkey = 0x63,
    Ukraine = 0x66,
    UnitedArabEmirates = 1,
    UnitedKingdom = 0x23,
    UnitedStates = 0x67,
    UnknownCountry = 0,
    Uruguay = 0x68,
    Uzbekistan = 0x69,
    Venezuela = 0x6a,
    Vietnam = 0x6b,
    Yemen = 0x6c,
    Zimbabwe = 110
};

enum GeneralDifficulty
{
    Medium = 0,
    Easy,
    Hard
};

enum SettingIDs
{
    WebConnectionSpeed = 0x1004200b,
    WebEmailFormat = 0x10042000,
    WebFavoriteGame = 0x10042004,
    WebFavoriteGame1 = 0x10042005,
    WebFavoriteGame2 = 0x10042006,
    WebFavoriteGame3 = 0x10042007,
    WebFavoriteGame4 = 0x10042008,
    WebFavoriteGame5 = 0x10042009,
    WebFavoriteGenre = 0x10042003,
    WebFlags = 0x10042001,
    WebFlash = 0x1004200c,
    WebPlatformsOwned = 0x1004200a,
    WebSpam = 0x10042002,
    WebVideoPreference = 0x1004200d,
    CruxBgLargePublic = 0x406403fe,
    CruxBgSmallPublic = 0x406403fd,
    CruxBio = 0x43e803fa,
    CruxBkgdImage = 0x100403f3,
    CruxLastChangeTime = 0x700803f4,
    CruxMediaMotto = 0x410003f6,
    CruxMediaPicture = 0x406403e8,
    CruxMediaStyle1 = 0x100403ea,
    CruxMediaStyle2 = 0x100403eb,
    CruxMediaStyle3 = 0x100403ec,
    CruxOfflineId = 0x603403f2,
    CruxTopAlbum1 = 0x100403ed,
    CruxTopAlbum2 = 0x100403ee,
    CruxTopAlbum3 = 0x100403ef,
    CruxTopAlbum4 = 0x100403f0,
    CruxTopAlbum5 = 0x100403f1,
    CruxTopMediaid1 = 0x601003f7,
    CruxTopMediaid2 = 0x601003f8,
    CruxTopMediaid3 = 0x601003f9,
    CruxTopMusic = 0x60a803f5,
    FriendsappShowBuddies = 0x1004003e,
    GamerActionAutoAim = 0x10040022,
    GamerActionAutoCenter = 0x10040023,
    GamerActionMovementControl = 0x10040024,
    GamerControlSensitivity = 0x10040018,
    GamerDifficulty = 0x10040015,
    GamerPreferredColorFirst = 0x1004001d,
    GamerPreferredColorSecond = 0x1004001e,
    GamerPresenceUserState = 0x10040007,
    GamerRaceAcceleratorControl = 0x10040029,
    GamerRaceBrakeControl = 0x10040028,
    GamerRaceCameraLocation = 0x10040027,
    GamerRaceTransmission = 0x10040026,
    GamerTier = 0x1004003a,
    GamerType = 0x10040001,
    GamerYaxisInversion = 0x10040002,
    GamercardAchievementsEarned = 0x10040013,
    GamercardAvatarInfo1 = 0x63e80044,
    GamercardAvatarInfo2 = 0x63e80045,
    GamercardCred = 0x10040006,
    GamercardHasVision = 0x10040008,
    GamercardMotto = 0x402c0011,
    GamercardPartyInfo = 0x60800046,
    GamercardPersonalPicture = 0x40640010,
    GamercardPictureKey = 0x4064000f,
    GamercardRegion = 0x10040005,
    GamercardRep = 0x5004000b,
    GamercardServiceTypeFlags = 0x1004003f,
    GamercardTitleAchievementsEarned = 0x10040039,
    GamercardTitleCredEarned = 0x10040038,
    GamercardTitlesPlayed = 0x10040012,
    GamercardUserBio = 0x43e80043,
    GamercardUserLocation = 0x40520041,
    GamercardUserName = 0x41040040,
    GamercardUserUrl = 0x41900042,
    GamercardZone = 0x10040004,
    MessengerAutoSignin = 0x1004003c,
    MessengerSignupState = 0x1004003b,
    OptionControllerVibration = 0x10040003,
    OptionVoiceMuted = 0x1004000c,
    OptionVoiceThruSpeakers = 0x1004000d,
    OptionVoiceVolume = 0x1004000e,
    Permissions = 0x10040000,
    SaveWindowsLivePassword = 0x1004003d,
    TitleSpecific1 = 0x63e83fff,
    TitleSpecific2 = 0x63e83ffe,
    TitleSpecific3 = 0x63e83ffd,
    LastOnLIVE = 0x7008004F,
    YearsOnLive = 0x10040047,
    TitleInformation = 0x8000,
    AvatarImage = 0x8007
};

enum Gamerzone
{
    NoGamerzone = 0,
    Recreational,
    Pro,
    Family,
    Underground
};

struct AvatarAward
{
    XdbfEntry entry;
    DWORD initialSize;

    DWORD structSize;
    DWORD clothingType;
    UINT64 awardFlags;
    DWORD titleID;
    DWORD imageID;
    DWORD flags;
    unsigned int unlockTime;
    unsigned int unlockTimeMilliseconds;
    AssetSubcategory subcategory;
    DWORD colorizable;
    wstring name;
    wstring unlockedDescription;
    wstring lockedDescription;
};

enum AvatarComponentMasks
{
    Body = 2,
    Carryable = 0x1000,
    Earrings = 0x400,
    Glasses = 0x100,
    Gloves = 0x80,
    Hair = 4,
    Hat = 0x40,
    Head = 1,
    Ring = 0x800,
    Shirt = 8,
    Shoes = 0x20,
    Trousers = 0x10,
    Wristwear = 0x200
};
