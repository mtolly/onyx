#include "Gpd/XdbfHelpers.h"
#include "AvatarAsset/AssetHelpers.h"
#include <sstream>

using std::stringstream;

string XdbfHelpers::EntryTypeToString(EntryType type)
{
    if (type < 1 || type > 6)
        throw string("Xdbf: Invalid entry type.\n");

    string strings[6] = { "Achievement", "Image", "Setting", "Title", "String", "Avatar Award" };
    return strings[type - 1];
}

string XdbfHelpers::SettingEntryTypeToString(SettingEntryType type)
{
    if (type < 0 || type > 7)
        throw string("Xdbf: Invalid setting entry type.\n");

    string strings[8] = { "Context", "Int32", "Int64", "Double", "Unicode String", "Float", "Binary", "DataTime" };
    return strings[type];
}

bool XdbfHelpers::CompareEntries(XdbfEntry a, XdbfEntry b)
{
    if (a.type != b.type)
        return a.type < b.type;
    else
        return a.id < b.type;
}

time_t XdbfHelpers::FILETIMEtoTimeT(WINFILETIME time)
{
    INT64 i64 = (((INT64)(time.dwHighDateTime)) << 32) + time.dwLowDateTime;
    return (time_t)((i64 - 116444736000000000) / 10000000);
}

unsigned int XdbfHelpers::FILETIMEtoMilliseconds(WINFILETIME time)
{
    /* Effectively the same as FILETIMEtoTimeT, but we're leaving
      the milliseconds on, and stripping them out with a modulus.

      TODO: Maybe just make this void with two reference parameters
      to fill a time_t and unsigned int.
      */
    INT64 i64 = (((INT64)(time.dwHighDateTime)) << 32) + time.dwLowDateTime;
    return (unsigned int)(((i64 - 116444736000000000) / 10000) % 1000);
}

WINFILETIME XdbfHelpers::TimeTtoFILETIME(time_t time)
{
    WINFILETIME toReturn;

    UINT64 ll = ((UINT64)(time * (UINT64)10000000)) + 116444736000000000;
    toReturn.dwLowDateTime = (DWORD)ll;
    toReturn.dwHighDateTime = ll >> 32;
    return toReturn;
}

WINFILETIME XdbfHelpers::TimeTtoFILETIME(time_t time, unsigned int millis)
{
    // TODO: Can just have the old function call this with 0ms
    millis = millis % 1000; // make sure 0-999 ms

    WINFILETIME toReturn;

    UINT64 ll = ((UINT64)(time * (UINT64)10000000)) + ((UINT64)(millis * (UINT64)10000)) + 116444736000000000;
    toReturn.dwLowDateTime = (DWORD)ll;
    toReturn.dwHighDateTime = ll >> 32;
    return toReturn;
}

string XdbfHelpers::GetAchievementState(AchievementEntry *entry)
{
    if (entry->flags & UnlockedOnline)
        return "Unlocked Online";
    else if (entry->flags & Unlocked)
        return "Unlocked Offline";
    else
        return "Locked";
}

string XdbfHelpers::AchievementTypeToString(AchievementFlags type)
{
    switch (type)
    {
        case Completion:
            return string("Completion");
        case Leveling:
            return string("Leveling");
        case Unlock:
            return string("Unlock");
        case Event:
            return string("Event");
        case Tournament:
            return string("Tournament");
        case Checkpoint:
            return string("Checkpoint");
        case Other:
            return string("Other");
        default:
            throw string("Gpd: Invalid achievement type.\n");
    }
}

string XdbfHelpers::AssetSubcategoryToString(AssetSubcategory category)
{
    return AssetHelpers::AssetSubcategoryToString(category);
}

string XdbfHelpers::AssetGenderToString(AssetGender gender)
{
    return AssetHelpers::AssetGenderToString(gender);
}

string XdbfHelpers::IDtoString(UINT64 id)
{
    switch (id)
    {
        case WebConnectionSpeed:
            return string("Web Connection Speed");
        case WebEmailFormat:
            return string("Web Email Format");
        case WebFavoriteGame:
            return string("Web Favorite Game");
        case WebFavoriteGame1:
            return string("Web Favorite Game1");
        case WebFavoriteGame2:
            return string("Web Favorite Game 2");
        case WebFavoriteGame3:
            return string("Web Favorite Game 3");
        case WebFavoriteGame4:
            return string("Web Favorite Game 4");
        case WebFavoriteGame5:
            return string("Web Favorite Game 5");
        case WebFavoriteGenre:
            return string("Web Favorite Genre");
        case WebFlags:
            return string("Web Flags");
        case WebFlash:
            return string("Web Flash");
        case WebPlatformsOwned:
            return string("Web Platforms Owned");
        case WebSpam:
            return string("Web Spam");
        case WebVideoPreference:
            return string("Web Video Preference");
        case CruxBgLargePublic:
            return string("Crux Bg Large Public");
        case CruxBgSmallPublic:
            return string("Crux Bg Small Public");
        case CruxBio:
            return string("Crux Bio");
        case CruxBkgdImage:
            return string("Crux Bkgd Image");
        case CruxLastChangeTime:
            return string("Crux Last Change Time");
        case CruxMediaMotto:
            return string("Crux Media Motto");
        case CruxMediaPicture:
            return string("Crux Media Picture");
        case CruxMediaStyle1:
            return string("Crux Media Style 1");
        case CruxMediaStyle2:
            return string("Crux Media Style 2");
        case CruxMediaStyle3:
            return string("Crux Media Style 3");
        case CruxOfflineId:
            return string("Crux Offline Id");
        case CruxTopAlbum1:
            return string("Crux Top Album 1");
        case CruxTopAlbum2:
            return string("Crux Top Album 2");
        case CruxTopAlbum3:
            return string("Crux Top Album 3");
        case CruxTopAlbum4:
            return string("Crux Top Album 4");
        case CruxTopAlbum5:
            return string("Crux Top Album 5");
        case CruxTopMediaid1:
            return string("Crux Top Mediaid 1");
        case CruxTopMediaid2:
            return string("Crux Top Mediaid 2");
        case CruxTopMediaid3:
            return string("Crux Top Mediaid 3");
        case CruxTopMusic:
            return string("Crux Top Music");
        case FriendsappShowBuddies:
            return string("Friendsapp Show Buddies");
        case GamerActionAutoAim:
            return string("Gamer Action Auto Aim");
        case GamerActionAutoCenter:
            return string("Gamer Action Auto Center");
        case GamerActionMovementControl:
            return string("Gamer Action Movement Control");
        case GamerControlSensitivity:
            return string("Gamer Control Sensitivity");
        case GamerDifficulty:
            return string("Gamer Difficulty");
        case GamerPreferredColorFirst:
            return string("Gamer Preferred Color First");
        case GamerPreferredColorSecond:
            return string("Gamer Preferred Color Second");
        case GamerPresenceUserState:
            return string("Gamer Presence User State");
        case GamerRaceAcceleratorControl:
            return string("Gamer Race Accelerator Control");
        case GamerRaceBrakeControl:
            return string("Gamer Race Brake Control");
        case GamerRaceCameraLocation:
            return string("Gamer Race Camera Location");
        case GamerRaceTransmission:
            return string("Gamer Race Transmission");
        case GamerTier:
            return string("Gamer Tier");
        case GamerType:
            return string("Gamer Type");
        case YearsOnLive:
            return string("Years On LIVE");
        case GamerYaxisInversion:
            return string("Gamer Yaxis Inversion");
        case GamercardAchievementsEarned:
            return string("Gamercard Achievements Earned");
        case GamercardAvatarInfo1:
            return string("Gamercard Avatar Info 1");
        case GamercardAvatarInfo2:
            return string("Gamercard Avatar Info 2");
        case GamercardCred:
            return string("Gamercard Cred");
        case GamercardHasVision:
            return string("Gamercard Has Vision");
        case GamercardMotto:
            return string("Gamercard Motto");
        case GamercardPartyInfo:
            return string("Gamercard Party Info");
        case GamercardPersonalPicture:
            return string("Gamercard Personal Picture");
        case GamercardPictureKey:
            return string("Gamercard Picture Key");
        case GamercardRegion:
            return string("Gamercard Region");
        case GamercardRep:
            return string("Gamercard Rep");
        case GamercardServiceTypeFlags:
            return string("Gamercard Service Type Flags");
        case GamercardTitleAchievementsEarned:
            return string("Gamercard Title Achievements Earned");
        case GamercardTitleCredEarned:
            return string("Gamercard Title Cred Earned");
        case GamercardTitlesPlayed:
            return string("Gamercard Titles Played");
        case GamercardUserBio:
            return string("Gamercard User Bio");
        case GamercardUserLocation:
            return string("Gamercard User Location");
        case GamercardUserName:
            return string("Gamercard User Name");
        case GamercardUserUrl:
            return string("Gamercard User Url");
        case GamercardZone:
            return string("Gamercard Zone");
        case MessengerAutoSignin:
            return string("Messenger Auto Signin");
        case MessengerSignupState:
            return string("Messenger Signup State");
        case OptionControllerVibration:
            return string("Option Controller Vibration");
        case OptionVoiceMuted:
            return string("Option Voice Muted");
        case OptionVoiceThruSpeakers:
            return string("Option Voice Thru Speakers");
        case OptionVoiceVolume:
            return string("Option Voice Volume");
        case Permissions:
            return string("Permissions");
        case SaveWindowsLivePassword:
            return string("Save Windows Live Password");
        case TitleSpecific1:
            return string("Title Specific 1");
        case TitleSpecific2:
            return string("Title Specific 2");
        case TitleSpecific3:
            return string("Title Specific 3");
        case LastOnLIVE:
            return string("Last On LIVE");
        case TitleInformation:
            return string("Title Information");
        case AvatarImage:
            return string("Avatar Image");
        default:
            stringstream ss;
            ss << "0x" << std::hex << id;
            return ss.str();
    }
}

string XdbfHelpers::RegionIDtoString(XboxLiveCountry id)
{
    switch (id)
    {
        case Albania:
            return string("Albania");
        case Algeria:
            return string("Algeria");
        case Argentina:
            return string("Argentina");
        case Armenia:
            return string("Armenia");
        case Australia:
            return string("Australia");
        case Austria:
            return string("Austria");
        case Azerbaijan:
            return string("Azerbaijan");
        case Bahrain:
            return string("Bahrain");
        case Belarus:
            return string("Belarus");
        case Belgium:
            return string("Belgium");
        case Belize:
            return string("Belize");
        case Bolivia:
            return string("Bolivia");
        case Brazil:
            return string("Brazil");
        case BruneiDarussalam:
            return string("Brunei Darussalam");
        case Bulgaria:
            return string("Bulgaria");
        case Canada:
            return string("Canada");
        case Chile:
            return string("Chile");
        case China:
            return string("China");
        case Colombia:
            return string("Colombia");
        case CostaRica:
            return string("CostaRica");
        case Croatia:
            return string("Croatia");
        case CzechRepublic:
            return string("Czech Republic");
        case Denmark:
            return string("Denmark");
        case DominicanRepublic:
            return string("Dominican Republic");
        case Ecuador:
            return string("Ecuador");
        case Egypt:
            return string("Egypt");
        case ElSalvador:
            return string("ElSalvador");
        case Estonia:
            return string("Estonia");
        case FaroeIslands:
            return string("Faroe Islands");
        case Finland:
            return string("Finland");
        case France:
            return string("France");
        case Georgia:
            return string("Georgia");
        case Germany:
            return string("Germany");
        case Greece:
            return string("Greece");
        case Guatemala:
            return string("Guatemala");
        case Honduras:
            return string("Honduras");
        case HongKong:
            return string("HongKong");
        case Hungary:
            return string("Hungary");
        case Iceland:
            return string("Iceland");
        case India:
            return string("India");
        case Indonesia:
            return string("Indonesia");
        case Iran:
            return string("Iran");
        case Iraq:
            return string("Iraq");
        case Ireland:
            return string("Ireland");
        case Israel:
            return string("Israel");
        case Italy:
            return string("Italy");
        case Jamaica:
            return string("Jamaica");
        case Japan:
            return string("Japan");
        case Jordan:
            return string("Jordan");
        case Kazakhstan:
            return string("Kazakhstan");
        case Kenya:
            return string("Kenya");
        case Korea:
            return string("Korea");
        case Kuwait:
            return string("Kuwait");
        case Kyrgyzstan:
            return string("Kyrgyzstan");
        case Latvia:
            return string("Latvia");
        case Lebanon:
            return string("Lebanon");
        case LibyanArabJamahiriya:
            return string("Libyan Arab Jamahiriya");
        case Liechtenstein:
            return string("Liechtenstein");
        case Lithuania:
            return string("Lithuania");
        case Luxembourg:
            return string("Luxembourg");
        case Macao:
            return string("Macao");
        case Macedonia:
            return string("Macedonia");
        case Malaysia:
            return string("Malaysia");
        case Maldives:
            return string("Maldives");
        case Mexico:
            return string("Mexico");
        case Monaco:
            return string("Monaco");
        case Mongolia:
            return string("Mongolia");
        case Morocco:
            return string("Morocco");
        case Netherlands:
            return string("Netherlands");
        case NewZealand:
            return string("New Zealand");
        case Nicaragua:
            return string("Nicaragua");
        case Norway:
            return string("Norway");
        case Oman:
            return string("Oman");
        case Pakistan:
            return string("Pakistan");
        case Panama:
            return string("Panama");
        case Paraguay:
            return string("Paraguay");
        case Peru:
            return string("Peru");
        case Philippines:
            return string("Philippines");
        case Poland:
            return string("Poland");
        case Portugal:
            return string("Portugal");
        case PuertoRico:
            return string("Puerto Rico");
        case Qatar:
            return string("Qatar");
        case Romania:
            return string("Romania");
        case RussianFederation:
            return string("Russian Federation");
        case SaudiArabia:
            return string("SaudiArabia");
        case Singapore:
            return string("Singapore");
        case Slovakia:
            return string("Slovakia");
        case Slovenia:
            return string("Slovenia");
        case SouthAfrica:
            return string("SouthAfrica");
        case Spain:
            return string("Spain");
        case Sweden:
            return string("Sweden");
        case Switzerland:
            return string("Switzerland");
        case SyrianArabRepublic:
            return string("Syrian Arab Republic");
        case Taiwan:
            return string("Taiwan");
        case Thailand:
            return string("Thailand");
        case TrinidadAndTobago:
            return string("Trinidad And Tobago");
        case Tunisia:
            return string("Tunisia");
        case Turkey:
            return string("Turkey");
        case Ukraine:
            return string("Ukraine");
        case UnitedArabEmirates:
            return string("United Arab Emirates");
        case UnitedKingdom:
            return string("United Kingdom");
        case UnitedStates:
            return string("United States");
        case UnknownCountry:
            return string("Unknown");
        case Uruguay:
            return string("Uruguay");
        case Uzbekistan:
            return string("Uzbekistan");
        case Venezuela:
            return string("Venezuela");
        case Vietnam:
            return string("Vietnam");
        case Yemen:
            return string("Yemen");
        case Zimbabwe:
            return string("Zimbabwe");
        default:
            throw string("Gpd: Error converting region ID to string, invalid region.\n");
    }
}
