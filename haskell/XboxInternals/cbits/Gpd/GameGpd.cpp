#include "GameGpd.h"


GameGpd::GameGpd(string filePath) : GpdBase(filePath), filePath(filePath)
{
    init();
    StopWriting();
}

GameGpd::GameGpd(FileIO *io) : GpdBase(io)
{
    init();
}

void GameGpd::CleanGpd()
{
    StartWriting();
    xdbf->Clean();
    io = xdbf->io;
    StopWriting();
}

AchievementEntry GameGpd::readAchievementEntry(XdbfEntry entry)
{
    // ensure that the entry passed in is an achievement entry
    if (entry.type != Achievement)
        throw string("Gpd: Error reading achievement entry. Specified entry isn't an achievement.\n");

    // seek to the begining of the achievement entry
    io->SetPosition(xdbf->GetRealAddress(entry.addressSpecifier));

    // read the entry
    AchievementEntry toReturn;
    toReturn.entry = entry;

    toReturn.structSize = io->ReadDword();
    toReturn.achievementID = io->ReadDword();
    toReturn.imageID = io->ReadDword();
    toReturn.gamerscore = io->ReadDword();
    toReturn.flags = io->ReadDword();

    WINFILETIME timeStamp = { io->ReadDword(), io->ReadDword() };
    toReturn.unlockTime = XdbfHelpers::FILETIMEtoTimeT(timeStamp);
    toReturn.unlockTimeMilliseconds = XdbfHelpers::FILETIMEtoMilliseconds(timeStamp);

    toReturn.name = io->ReadWString();
    toReturn.unlockedDescription = io->ReadWString();
    toReturn.lockedDescription = io->ReadWString();

    toReturn.initialLength = 0x1C + ((toReturn.name.size() + toReturn.unlockedDescription.size() +
            toReturn.lockedDescription.size() + 3) * 2);

    return toReturn;
}

void GameGpd::WriteAchievementEntry(AchievementEntry *entry)
{
    DWORD calculatedLength = 0x1C + ((entry->name.size() + entry->unlockedDescription.size() +
            entry->lockedDescription.size() + 3) * 2);
    if (calculatedLength != entry->initialLength)
    {
        // adjust the memory if the length changed
        xdbf->DeallocateMemory(xdbf->GetRealAddress(entry->entry.addressSpecifier), entry->entry.length);
        entry->entry.length = calculatedLength;
        entry->entry.addressSpecifier = xdbf->GetSpecifier(xdbf->AllocateMemory(entry->entry.length));
    }

    io->Flush();

    // seek to the begining of the entry
    io->SetPosition(xdbf->GetRealAddress(entry->entry.addressSpecifier));

    // Write the entry
    io->Write(entry->structSize);
    io->Write(entry->achievementID);
    io->Write(entry->imageID);
    io->Write(entry->gamerscore);
    io->Write(entry->flags);

    // Write the unlocked time
    if (entry->flags & UnlockedOnline)
    {
        if (entry->unlockTime == 0)
            /*
              Not worrying about whether the milliseconds are 0,
              because a timestamp of 0000-00-00 00:00:00.123
              surely wouldn't be suspicious.
            */
            io->Write((UINT64)0);
        else
        {
            WINFILETIME time = XdbfHelpers::TimeTtoFILETIME(entry->unlockTime, entry->unlockTimeMilliseconds);
            io->Write(time.dwHighDateTime);
            io->Write(time.dwLowDateTime);
        }
    }
    else
        io->SetPosition(xdbf->GetRealAddress(entry->entry.addressSpecifier) + 0x1C);

    // Write the strings
    io->Write(entry->name);
    io->Write(entry->unlockedDescription);
    io->Write(entry->lockedDescription);

    xdbf->UpdateEntry(&entry->entry);

    io->Flush();
}

void GameGpd::CreateAchievement(AchievementEntry *entry, BYTE *thumbnail, DWORD thumbnailLen)
{
    DWORD entryLen = 0x1C + ((entry->name.size() + entry->unlockedDescription.size() +
            entry->lockedDescription.size() + 3) * 2);
    entry->initialLength = entryLen;

    // get the next id for the achievement
    DWORD max = 0;
    for (DWORD i = 0; i < achievements.size(); i++)
    {
        if (achievements.at(i).achievementID > max)
            max = achievements.at(i).achievementID;
    }
    entry->achievementID = max + 1;

    // create a new xdbf entry
    entry->entry = xdbf->CreateEntry(Achievement, entry->achievementID, entryLen);

    // add the entry to the vector
    achievements.push_back(*entry);

    // Write the achievement to the file
    WriteAchievementEntry(entry);

    // make sure an image was provided
    if (thumbnail == NULL)
        return;

    // make sure that the image is a PNG
    if (*(DWORD*)thumbnail != 0x474E5089)
        throw string("Gpd: Error creating image entry. Image must be a PNG.\n");

    // create a new image entry for the thumbnail
    XdbfEntry imageEntry = xdbf->CreateEntry(Image, entry->imageID, thumbnailLen);

    // Write the image to the file
    io->SetPosition(xdbf->GetRealAddress(imageEntry.addressSpecifier));
    io->Write(thumbnail, thumbnailLen);

    // make a copy of the image
    BYTE *imageCopy = new BYTE[thumbnailLen];
    memcpy(imageCopy, thumbnail, thumbnailLen);

    // add the image to the vector
    ImageEntry image;
    image.image = imageCopy;
    image.length = thumbnailLen;
    image.entry = imageEntry;

    images.push_back(image);

    xdbf->UpdateEntry(&entry->entry);
    xdbf->UpdateEntry(&imageEntry);
}

string GameGpd::GetAchievementType(AchievementEntry *entry)
{
    return XdbfHelpers::AchievementTypeToString((AchievementFlags)(entry->flags & 7));
}

void GameGpd::DeleteAchievement(AchievementEntry *entry)
{
    // remove the entry from the list
    DWORD i;
    for (i = 0 ; i < achievements.size(); i++)
    {
        if (achievements.at(i).entry.id == entry->entry.id)
        {
            achievements.erase(achievements.begin() + i);
            break;
        }
    }
    if (i == achievements.size())
        throw string("Gpd: Error deleting achievement. Achievement doesn't exist.\n");

    // delete the entry from the file
    xdbf->DeleteEntry(entry->entry);
}

bool GameGpd::GetAchievementThumbnail(AchievementEntry *entry, ImageEntry *out)
{
    for (DWORD i = 0; i < images.size(); i++)
    {
        if (images.at(i).entry.id == entry->imageID)
        {
            *out = images.at(i);
            return true;
        }
    }
    return false;
}

void GameGpd::UnlockAllAchievementsOffline()
{
    // iterate through all of the achievements
    for (DWORD i = 0; i < achievements.size(); i++)
    {
        // unlock the achievement
        achievements.at(i).flags |= (Unlocked | 0x100000);

        // Write the updated flags to the entry
        io->SetPosition(xdbf->GetRealAddress(achievements.at(i).entry.addressSpecifier) + 0x10);
        io->Write(achievements.at(i).flags);

        // update the sync stuff
        xdbf->UpdateEntry(&achievements.at(i).entry);
    }
}

void GameGpd::init()
{
    // read all of the achievement entries
    for (DWORD i = 0; i < xdbf->achievements.entries.size(); i++)
        achievements.push_back(readAchievementEntry(xdbf->achievements.entries.at(i)));

    // find the thumbnail for the game
    for (DWORD i = 0; i < images.size(); i++)
        if (images.at(i).entry.id == 0x8000)
            thumbnail = images.at(i);

    // find the game name string
    for (DWORD i = 0; i < strings.size(); i++)
        if (strings.at(i).entry.id == 0x8000)
            gameName = strings.at(i);
}

void GameGpd::StartWriting()
{
    io = new FileIO(filePath);
    xdbf->io = io;
}

void GameGpd::StopWriting()
{
    io->Close();
    delete io;
    io = NULL;
    xdbf->io = NULL;
}

GameGpd::~GameGpd(void)
{
    if (!ioPassedIn && io)
        io->Close();
}
