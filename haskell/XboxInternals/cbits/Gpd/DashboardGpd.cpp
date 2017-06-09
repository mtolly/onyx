#include "DashboardGpd.h"
#include <sstream>

using std::stringstream;


DashboardGpd::DashboardGpd(string gpdPath) : GpdBase(gpdPath)
{
    init();
}

DashboardGpd::DashboardGpd(FileIO *io) : GpdBase(io)
{
    init();
}

void DashboardGpd::CleanGpd()
{
    xdbf->Clean();
}

void DashboardGpd::init()
{
    // set all the settings defaultly to zero, so there is a way of checking whether or not
    // the entries were actually read in
    gamercardRegion.entry.type =
        gamerzone.entry.type =
            gamerscoreUnlocked.entry.type =
                achievementsUnlocked.entry.type =
                        reputation.entry.type =
                                gamePlayedCount.entry.type =
                                        motto.entry.type =
                                                gamerPictureKey.entry.type =
                                                        gamerName.entry.type =
                                                                gamerLocation.entry.type =
                                                                        avatarInformation.entry.type =
                                                                                avatarImage.entry.type =
                                                                                        yearsOnLive.entry.type =
                                                                                                gamerBio.entry.type = (EntryType)0;

    // read in all the title entries
    for (DWORD i = 0; i < xdbf->titlesPlayed.entries.size(); i++)
        gamesPlayed.push_back(readTitleEntry(xdbf->titlesPlayed.entries.at(i)));

    // find the avatar image
    for (DWORD i = 0; i < images.size(); i++)
        if (images.at(i).entry.id == AvatarImage)
        {
            avatarImage = images.at(i);
            break;
        }

    // read in all of the IMPOTANT setting entries
    for (DWORD i = 0; i < settings.size(); i++)
    {
        switch (settings.at(i).entry.id)
        {
            case GamercardRegion:
                gamercardRegion = settings.at(i);
                break;
            case GamercardZone:
                gamerzone = settings.at(i);
                break;
            case GamercardCred:
                gamerscoreUnlocked = settings.at(i);
                break;
            case GamercardAchievementsEarned:
                achievementsUnlocked = settings.at(i);
                break;
            case GamercardRep:
                reputation = settings.at(i);
                break;
            case GamercardMotto:
                motto = settings.at(i);
                break;
            case GamercardTitlesPlayed:
                gamePlayedCount = settings.at(i);
                break;
            case GamercardPictureKey:
                gamerPictureKey = settings.at(i);
                break;
            case GamercardUserName:
                gamerName = settings.at(i);
                break;
            case GamercardUserLocation:
                gamerLocation = settings.at(i);
                break;
            case GamercardAvatarInfo1:
                avatarInformation = settings.at(i);
                break;
            case YearsOnLive:
                yearsOnLive = settings.at(i);
                break;
            case GamercardUserBio:
                gamerBio = settings.at(i);
                break;
        }
    }
}

TitleEntry DashboardGpd::readTitleEntry(XdbfEntry entry)
{
    // ensure that the entry is a title entry
    if (entry.type != Title)
        throw string("Gpd: Error reading title entry. Specified entry isn't a title.\n");

    TitleEntry toReturn;
    toReturn.entry = entry;

    // seek to title entry position
    io->SetPosition(xdbf->GetRealAddress(entry.addressSpecifier));

    // read the entry
    toReturn.titleID = io->ReadDword();
    toReturn.achievementCount = io->ReadDword();
    toReturn.achievementsUnlocked = io->ReadDword();
    toReturn.totalGamerscore = io->ReadDword();
    toReturn.gamerscoreUnlocked = io->ReadDword();
    toReturn.achievementsUnlockedOnline = io->ReadWord();
    toReturn.avatarAwardsEarned = io->ReadByte();
    toReturn.avatarAwardCount = io->ReadByte();
    toReturn.maleAvatarAwardsEarned = io->ReadByte();
    toReturn.maleAvatarAwardCount = io->ReadByte();
    toReturn.femaleAvatarAwardsEarned = io->ReadByte();
    toReturn.femaleAvatarAwardCount = io->ReadByte();
    toReturn.flags = io->ReadDword();

    // read the last time played
    WINFILETIME time = { io->ReadDword(), io->ReadDword() };
    if (time.dwHighDateTime == 0 && time.dwLowDateTime == 0)
        toReturn.lastPlayed = 0;
    else
        toReturn.lastPlayed = XdbfHelpers::FILETIMEtoTimeT(time);

    // read the game name
    toReturn.gameName = io->ReadWString();

    toReturn.initialLength = entry.length;

    return toReturn;
}

string DashboardGpd::GetSmallBoxArtURL(TitleEntry *entry)
{
    stringstream url;
    url << "http://tiles.xbox.com/consoleAssets/";
    url << std::hex << entry->titleID;
    url << "/en-us/smallboxart.jpg";

    return url.str();
}

string DashboardGpd::GetLargeBoxArtURL(TitleEntry *entry)
{
    stringstream url;
    url << "http://tiles.xbox.com/consoleAssets/";
    url << std::hex << entry->titleID;
    url << "/en-us/largeboxart.jpg";

    return url.str();
}

void DashboardGpd::WriteTitleEntry(TitleEntry *entry)
{
    DWORD calculatedLength = 0x28 + ((entry->gameName.size() + 1) * 2);

    if (calculatedLength != entry->initialLength)
    {
        // adjust the memory if the length changed
        xdbf->DeallocateMemory(xdbf->GetRealAddress(entry->entry.addressSpecifier), entry->entry.length);
        entry->entry.length = calculatedLength;
        entry->entry.addressSpecifier = xdbf->GetSpecifier(xdbf->AllocateMemory(entry->entry.length));
    }

    // seek to the position of the title entry
    io->SetPosition(xdbf->GetRealAddress(entry->entry.addressSpecifier));

    io->Flush();

    // Write the title entry
    io->Write(entry->titleID);
    io->Write(entry->achievementCount);
    io->Write(entry->achievementsUnlocked);
    io->Write(entry->totalGamerscore);
    io->Write(entry->gamerscoreUnlocked);
    io->Write(entry->achievementsUnlockedOnline);
    io->Write(entry->avatarAwardsEarned);
    io->Write(entry->avatarAwardCount);
    io->Write(entry->maleAvatarAwardsEarned);
    io->Write(entry->maleAvatarAwardCount);
    io->Write(entry->femaleAvatarAwardsEarned);
    io->Write(entry->femaleAvatarAwardCount);
    io->Write(entry->flags);

    // Write the time last played
    if (entry->lastPlayed == 0)
        io->SetPosition(xdbf->GetRealAddress(entry->entry.addressSpecifier) + 0x28);
    else
    {
        WINFILETIME time = XdbfHelpers::TimeTtoFILETIME(entry->lastPlayed);
        io->Write(time.dwHighDateTime);
        io->Write(time.dwLowDateTime);
    }

    io->Write(entry->gameName);

    xdbf->UpdateEntry(&entry->entry);

    io->Flush();
}

void DashboardGpd::DeleteTitleEntry(TitleEntry *entry)
{
    // remove the entry from the list
    DWORD i;
    for (i = 0 ; i < gamesPlayed.size(); i++)
    {
        if (gamesPlayed.at(i).entry.id == entry->entry.id)
        {
            gamesPlayed.erase(gamesPlayed.begin() + i);
            break;
        }
    }
    if (i > gamesPlayed.size())
        throw string("Gpd: Error deleting title entry. Title doesn't exist.\n");

    // delete the entry from the file
    xdbf->DeleteEntry(entry->entry);
}

void DashboardGpd::CreateTitleEntry(TitleEntry *entry)
{
    entry->initialLength = 0x28 + ((entry->gameName.size() + 1) * 2);

    // create a new xdbf entry
    entry->entry = xdbf->CreateEntry(Title, entry->titleID, 0x28 + ((entry->gameName.size() + 1) * 2));

    // add the title entry to the vector
    gamesPlayed.push_back(*entry);

    // Write the data to the entry
    WriteTitleEntry(entry);
}

DashboardGpd::~DashboardGpd(void)
{
}
