#include "AvatarAwardGpd.h"
#include <sstream>

using std::stringstream;

AvatarAwardGpd::AvatarAwardGpd(string gpdPath) : GpdBase(gpdPath)
{
    init();
}

AvatarAwardGpd::AvatarAwardGpd(FileIO *io) : GpdBase(io)
{
    init();
}

void AvatarAwardGpd::CleanGpd()
{
    xdbf->Clean();
}

void AvatarAwardGpd::init()
{
    // read in all of the avatar award entries
    for (DWORD i = 0; i < xdbf->avatarAwards.entries.size(); i++)
        avatarAwards.push_back(readAvatarAwardEntry(xdbf->avatarAwards.entries.at(i)));
}

AssetGender AvatarAwardGpd::GetAssetGender(struct AvatarAward *award)
{
    return (AssetGender)((award->awardFlags & 0x0000000300000000) >> 32);
}

struct AvatarAward AvatarAwardGpd::readAvatarAwardEntry(XdbfEntry entry)
{
    // make sure the entry passed in is an avatar award
    if (entry.type != AvatarAward)
        throw string("Gpd: Error reading avatar award, specified entry isn't an award.\n");

    struct AvatarAward award;
    award.entry = entry;

    // seek to the address of the award
    io->SetPosition(xdbf->GetRealAddress(entry.addressSpecifier));

    // read the award
    award.structSize = io->ReadDword();
    award.clothingType = io->ReadDword();
    award.awardFlags = io->ReadUInt64();
    award.titleID = io->ReadDword();
    award.imageID = io->ReadDword();
    award.flags = io->ReadDword();

    // read the unlock time
    WINFILETIME time = { io->ReadDword(), io->ReadDword() };
    award.unlockTime = XdbfHelpers::FILETIMEtoTimeT(time);
    award.unlockTimeMilliseconds = XdbfHelpers::FILETIMEtoMilliseconds(time);

    // read the rest of the entry
    award.subcategory = (AssetSubcategory)io->ReadDword();
    award.colorizable = io->ReadDword();
    award.name = io->ReadWString();
    award.unlockedDescription = io->ReadWString();
    award.lockedDescription = io->ReadWString();

    // calculate the initial size of the entry
    award.initialSize = 0x2C + ((award.name.size() + award.unlockedDescription.size() +
            award.lockedDescription.size() + 3) * 2);

    return award;
}

void AvatarAwardGpd::UnlockAllAwards()
{
    // iterate through all of the avtar awards
    for (DWORD i = 0; i < avatarAwards.size(); i++)
    {
        // set it to unlocked
        avatarAwards.at(i).flags |= (Unlocked | 0x100000);

        // Write the flags back to the file
        io->SetPosition(xdbf->GetRealAddress(avatarAwards.at(i).entry.addressSpecifier) + 0x18);
        io->Write(avatarAwards.at(i).flags);

        // update the sync crap
        xdbf->UpdateEntry(&avatarAwards.at(i).entry);
    }

    io->Flush();
}

string AvatarAwardGpd::GetGUID(struct AvatarAward *award)
{
    char guid[38];
    WORD *seg = (WORD*)&award->awardFlags;
    sprintf(guid, "%08x-%04x-%04x-%04x-%04x%08x", award->clothingType, seg[3], seg[2], seg[1], seg[0],
            award->titleID);

    return string(guid);
}

string AvatarAwardGpd::getAwardImageURL(struct AvatarAward *award, bool little)
{
    stringstream url;
    url << "http://avatar.xboxlive.com/global/t.";
    url << std::hex << award->titleID << "/avataritem/";

    url << GetGUID(award).c_str() << ((little) ? "/64" : "/128");

    return url.str();
}

string AvatarAwardGpd::GetLittleAwardImageURL(struct AvatarAward *award)
{
    return getAwardImageURL(award, true);
}

string AvatarAwardGpd::GetLargeAwardImageURL(struct AvatarAward *award)
{
    return getAwardImageURL(award, false);
}

void AvatarAwardGpd::WriteAvatarAward(struct AvatarAward *award)
{
    DWORD calculatedLength = 0x2C + ((award->name.size() + award->unlockedDescription.size() +
            award->lockedDescription.size() + 3) * 2);

    if (calculatedLength != award->initialSize)
    {
        // adjust the memory if the length changed
        xdbf->DeallocateMemory(xdbf->GetRealAddress(award->entry.addressSpecifier), award->entry.length);
        award->entry.length = calculatedLength;
        award->entry.addressSpecifier = xdbf->GetSpecifier(xdbf->AllocateMemory(award->entry.length));
    }

    // seek to the position of the award
    io->SetPosition(xdbf->GetRealAddress(award->entry.addressSpecifier));

    // Write the entry
    io->Write(award->structSize);
    io->Write(award->clothingType);
    io->Write(award->awardFlags);
    io->Write(award->titleID);
    io->Write(award->imageID);
    io->Write(award->flags);

    // Write the unlock time
    WINFILETIME time = XdbfHelpers::TimeTtoFILETIME(award->unlockTime, award->unlockTimeMilliseconds);
    io->Write(time.dwHighDateTime);
    io->Write(time.dwLowDateTime);

    // Write the rest of the entry
    io->Write((DWORD)award->subcategory);
    io->Write((DWORD)award->colorizable);
    io->Write(award->name);
    io->Write(award->unlockedDescription);
    io->Write(award->lockedDescription);

    xdbf->UpdateEntry(&award->entry);

    io->Flush();
}

void AvatarAwardGpd::CreateAvatarAward(struct AvatarAward *award)
{
    award->initialSize = 0x2C + ((award->name.size() + award->unlockedDescription.size() +
            award->lockedDescription.size() + 3) * 2);

    // create a new xdbf entry for the award
    UINT64 entryID = ((UINT64)award->titleID << 32) | ((DWORD)getNextAwardIndex() << 16) |
            GetAssetGender(award);
    award->entry = xdbf->CreateEntry(AvatarAward, entryID, award->initialSize);

    // Write the award to the file
    WriteAvatarAward(award);

    avatarAwards.push_back(*award);
}

void AvatarAwardGpd::DeleteAvatarAward(struct AvatarAward *award)
{
    // remove the entry from the list
    DWORD i;
    for (i = 0 ; i < avatarAwards.size(); i++)
    {
        if (avatarAwards.at(i).entry.id == award->entry.id)
        {
            avatarAwards.erase(avatarAwards.begin() + i);
            break;
        }
    }
    if (i == avatarAwards.size())
        throw string("Gpd: Error deleting avatar award. Award doesn't exist.\n");

    // delete the entry from the file
    xdbf->DeleteEntry(award->entry);
}

WORD AvatarAwardGpd::getNextAwardIndex()
{
    // get the highest index
    WORD highest = 0;
    for (DWORD i = 0; i < xdbf->avatarAwards.entries.size(); i++)
    {
        WORD currentID = (xdbf->avatarAwards.entries.at(i).id & 0xFFFF0000) >> 16;
        if (highest < currentID)
            highest = currentID;
    }

    return highest + 1;
}

AvatarAwardGpd::~AvatarAwardGpd(void)
{
    if (!ioPassedIn)
        io->Close();
}
