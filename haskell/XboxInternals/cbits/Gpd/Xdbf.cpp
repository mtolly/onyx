#include "Xdbf.h"
#include <stdio.h>

Xdbf::Xdbf(string gpdPath) : ioPassedIn(false)
{
    io = new FileIO(gpdPath);

    init();
    readHeader();
    readEntryTable();
    readFreeMemoryTable();
}

Xdbf::Xdbf(FileIO *io) : io(io), ioPassedIn(true)
{
    init();
    readHeader();
    readEntryTable();
    readFreeMemoryTable();
}

void Xdbf::Clean()
{
    // create a temporary file to Write the old Gpd's used memory to
    string tempFileName;

#ifdef _WIN32
    // Opening a file using the path returned by tmpnam() may result in a "permission denied" error on Windows.
    // Not sure why it happens but tweaking the manifest/UAC properties makes a difference.
    char *tempFileName_c = _tempnam(NULL, NULL);
    if (!tempFileName_c)
        throw string("Xdbf: Failed to generate temporary file name.\n");
    tempFileName = string(tempFileName_c);
    free(tempFileName_c);
    tempFileName_c = NULL;
#else
    char tempFileName_c[L_tmpnam];
    if (!tmpnam(tempFileName_c))
        throw string("Xdbf: Failed to generate temporary file name.\n");
    tempFileName = string(tempFileName_c);
#endif

    FileIO tempFile(tempFileName.c_str(), true);

    // Write the old header
    tempFile.SetPosition(0);
    tempFile.Write(header.magic);
    tempFile.Write(header.version);
    tempFile.Write(header.entryTableLength);
    tempFile.Write(header.entryCount);
    tempFile.Write(header.freeMemTableLength);
    tempFile.Write((DWORD)1);

    // seek to the first position in the file where data can be written
    tempFile.SetPosition(GetRealAddress(0) - 1);
    tempFile.Write((BYTE)0);

    tempFile.Flush();

    // Write all of the achievements
    WriteNewEntryGroup(&achievements, &tempFile);

    // Write all of the images
    WriteNewEntryGroup(&images, &tempFile);

    // Write all of the settings
    WriteNewEntryGroup(&settings, &tempFile);

    // Write all of the title entries
    WriteNewEntryGroup(&titlesPlayed, &tempFile);

    // Write all of the strings
    WriteNewEntryGroup(&strings, &tempFile);

    // Write all of the achievements
    WriteNewEntryGroup(&avatarAwards, &tempFile);

    tempFile.Close();
    io->Close();

    // delete the original file
    remove(io->GetFilePath().c_str());

    // move the temp file to the old file's location
    rename(tempFileName.c_str(), io->GetFilePath().c_str());

    string path = io->GetFilePath();
    delete io;
    io = new FileIO(path);

    // Write the updated entry table
    WriteEntryListing();

    // clear all of the existing entries
    achievements.entries.clear();
    images.clear();
    settings.entries.clear();
    titlesPlayed.entries.clear();
    strings.clear();
    avatarAwards.entries.clear();

    // clear the free memory
    freeMemory.clear();

    io->SetPosition(0, ios_base::end);
    XdbfFreeMemEntry  entry = { GetSpecifier(io->GetPosition()), 0xFFFFFFFF - GetSpecifier(io->GetPosition()) };
    freeMemory.push_back(entry);

    WriteFreeMemTable();

    // reload the file listing
    readEntryTable();
}

void Xdbf::WriteNewEntryGroup(XdbfEntryGroup *group, FileIO *newIO)
{
    // iterate through all of the entries
    for (DWORD i = 0; i < group->entries.size(); i++)
        WriteNewEntry(&group->entries.at(i), newIO);

    // Write the sync crap
    WriteNewEntry(&group->syncs.entry, newIO);
    WriteNewEntry(&group->syncData.entry, newIO);
}

void Xdbf::WriteNewEntryGroup(vector<XdbfEntry> *group, FileIO *newIO)
{
    // iterate through all of the entries
    for (DWORD i = 0; i < group->size(); i++)
        WriteNewEntry(&group->at(i), newIO);
}

void Xdbf::WriteNewEntry(XdbfEntry *entry, FileIO *newIO)
{
    // read in the entry data
    BYTE *buffer = new BYTE[entry->length];
    io->SetPosition(GetRealAddress(entry->addressSpecifier));
    io->ReadBytes(buffer, entry->length);

    // update the entry address
    entry->addressSpecifier = GetSpecifier((DWORD)newIO->GetPosition());

    // Write the entry data
    newIO->Write(buffer, entry->length);

    delete[] buffer;
}

void Xdbf::init()
{
    achievements.syncs.lengthChanged = false;
    settings.syncs.lengthChanged = false;
    titlesPlayed.syncs.lengthChanged = false;
    avatarAwards.syncs.lengthChanged = false;

    memset(&achievements.syncs.entry, 0, sizeof(XdbfEntry));
    memset(&settings.syncs.entry, 0, sizeof(XdbfEntry));
    memset(&titlesPlayed.syncs.entry, 0, sizeof(XdbfEntry));
    memset(&avatarAwards.syncs.entry, 0, sizeof(XdbfEntry));

    memset(&achievements.syncData.entry, 0, sizeof(XdbfEntry));
    memset(&settings.syncData.entry, 0, sizeof(XdbfEntry));
    memset(&titlesPlayed.syncData.entry, 0, sizeof(XdbfEntry));
    memset(&avatarAwards.syncData.entry, 0, sizeof(XdbfEntry));
}

Xdbf::~Xdbf()
{
    if (!ioPassedIn)
        io->Close();
}

void Xdbf::readHeader()
{
    // seek to the begining of the file
    io->SetPosition(0);

    // ensure the magic is correct
    header.magic = io->ReadDword();
    if (header.magic != 0x58444246)
        throw string("Xdbf: Invalid magic.\n");

    // read in the rest of the header
    header.version = io->ReadDword();
    header.entryTableLength = io->ReadDword();
    header.entryCount = io->ReadDword();
    header.freeMemTableLength = io->ReadDword();
    header.freeMemTableEntryCount = io->ReadDword();

    // make sure that there is at least free mem table 1 entry
    if (header.freeMemTableEntryCount == 0)
    {
        header.freeMemTableEntryCount = 1;
        WriteHeader();
    }
}

void Xdbf::readEntryTable()
{
    // seek to the begining of the entry table
    io->SetPosition(0x18);

    // read achievement table entries
    readEntryGroup(&achievements, Achievement);

    // read images
    readEntryGroup(&images, Image);

    // read setting table entries
    readEntryGroup(&settings, Setting);

    // read title table entries
    readEntryGroup(&titlesPlayed, Title);

    // read strings
    readEntryGroup(&strings, String);

    // read avatar award table entries
    readEntryGroup(&avatarAwards, AvatarAward);
}

SyncData Xdbf::readSyncData(XdbfEntry entry)
{
    // make sure the entry passed in is sync data
    if (entry.type == Image || entry.type == String || (entry.id != 0x200000000 && entry.id != 2))
        throw string("Xdbf: Error reading sync list. Specified entry isn't a sync list.\n");

    // preserve io position
    DWORD pos = (DWORD)io->GetPosition();

    SyncData data;
    data.entry = entry;

    // seek to the sync data entry in the file
    io->SetPosition(GetRealAddress(entry.addressSpecifier));

    // read the data
    data.nextSyncID = io->ReadUInt64();
    data.lastSyncID = io->ReadUInt64();

    WINFILETIME time = { io->ReadDword(), io->ReadDword() };
    data.lastSyncedTime = (tm*)XdbfHelpers::FILETIMEtoTimeT(time);

    io->SetPosition(pos);

    return data;
}

void Xdbf::WriteSyncData(SyncData *data)
{
    // seek to the sync data position
    io->SetPosition(GetRealAddress(data->entry.addressSpecifier));

    io->Write(data->nextSyncID);
    io->Write(data->lastSyncID);

    // Write the last time synced
    WINFILETIME lastSynced = XdbfHelpers::TimeTtoFILETIME((time_t)data->lastSyncedTime);
    io->Write(lastSynced.dwHighDateTime);
    io->Write(lastSynced.dwLowDateTime);
}

SyncList Xdbf::readSyncList(XdbfEntry entry)
{
    // make sure the entry passed in is a sync list
    if (entry.type == Image || entry.type == String || (entry.id != 0x100000000 && entry.id != 1))
        throw string("Xdbf: Error reading sync list. Specified entry isn't a sync list.\n");

    // preserve io position
    DWORD pos = (DWORD)io->GetPosition();

    SyncList toReturn;
    toReturn.entry = entry;

    // seek to the begining of the entry
    io->SetPosition(GetRealAddress(entry.addressSpecifier));

    // iterate through all the syncs
    for (DWORD i = 0; i < (entry.length / 16); i++)
    {
        // read in the sync
        SyncEntry sync = { io->ReadUInt64(), io->ReadUInt64() };

        // if the sync value is 0, then it was already synced
        if (sync.syncValue == 0)
            toReturn.synced.push_back(sync);
        else
            toReturn.toSync.push_back(sync);
    }

    io->SetPosition(pos);

    return toReturn;
}

void Xdbf::WriteSyncList(SyncList *syncs)
{
    // if the length has changed, then we need to allocated more/less memory
    if (syncs->lengthChanged)
    {
        // free the old memory
        DeallocateMemory(GetRealAddress(syncs->entry.addressSpecifier), syncs->entry.length);

        // update entry length
        syncs->entry.length = (syncs->synced.size() + syncs->toSync.size()) * 0x10;

        // allocate new memory
        syncs->entry.addressSpecifier = GetSpecifier(AllocateMemory(syncs->entry.length));
        syncs->lengthChanged = false;
    }

    // seek to the sync list position
    io->SetPosition(GetRealAddress(syncs->entry.addressSpecifier));

    // Write the synced ones
    for (DWORD i = 0; i < syncs->synced.size(); i++)
    {
        io->Write(syncs->synced.at(i).entryID);
        io->Write(syncs->synced.at(i).syncValue);
    }

    // Write the toSync ones
    for (size_t i = 0; i < syncs->toSync.size(); i++)
    {
        io->Write(syncs->toSync.at(i).entryID);
        io->Write(syncs->toSync.at(i).syncValue);
    }
}

void Xdbf::readFreeMemoryTable()
{
    // seek to the begining of the free memory table
    DWORD tableStartAddr = 0x18 + (header.entryTableLength * 0x12);
    io->SetPosition(tableStartAddr);

    // iterate through all of the free memory table entries
    for (DWORD i = 0; i < header.freeMemTableEntryCount; i++)
    {
        XdbfFreeMemEntry entry = { io->ReadDword(), io->ReadDword() };
        freeMemory.push_back(entry);
    }
}

DWORD Xdbf::GetRealAddress(DWORD specifier)
{
    return specifier + (header.entryTableLength * 0x12) + (header.freeMemTableLength * 8) + 0x18;
}

DWORD Xdbf::GetSpecifier(DWORD address)
{
    DWORD headerSize = (header.entryTableLength * 0x12) + (header.freeMemTableLength * 8) + 0x18;
    if (address < headerSize)
        throw string("Xdbf: Invalid address for converting.\n");
    return address - headerSize;
}

XdbfEntry Xdbf::CreateEntry(EntryType type, UINT64 id, DWORD size)
{
    XdbfEntry entry = { type, id, 0, size };

    header.entryCount++;

    if (id == ((type == AvatarAward) ? 1 : 0x100000000) ||
            id == ((type == AvatarAward) ? 2 : 0x200000000))
    {
        // allocate memory for the entry
        entry.addressSpecifier = GetSpecifier(AllocateMemory(size));
        WriteEntryListing();

        WriteHeader();

        return entry;
    }

    // make sure the entry doesn't already exist
    vector<XdbfEntry> *entries;
    switch (entry.type)
    {
        case Achievement:
            entries = &achievements.entries;
            break;
        case Image:
            entries = &images;
            break;
        case Setting:
            entries = &settings.entries;
            break;
        case Title:
            entries = &titlesPlayed.entries;
            break;
        case String:
            entries = &strings;
            break;
        case AvatarAward:
            entries = &avatarAwards.entries;
            break;
        default:
            throw string("Xdbf: Error creating entry. Invalid entry type.\n");
    }
    for (DWORD i = 0; i < entries->size(); i++)
        if (entries->at(i).id == entry.id)
            throw string("Xdbf: Error creating entry. Entry already exists.\n");

    // allocate memory for the entry
    entry.addressSpecifier = GetSpecifier(AllocateMemory(size));

    // add the entry to the listing, and create a new sync for the entry if needed
    switch (entry.type)
    {
        case Achievement:
        {
            // add the entry to the list
            achievements.entries.push_back(entry);
            // create a sync for the entry
            SyncEntry sync = { entry.id, achievements.syncData.nextSyncID++ };
            achievements.syncs.toSync.push_back(sync);
            achievements.syncs.lengthChanged = true;
            // re-Write the sync list
            WriteSyncList(&achievements.syncs);
            WriteSyncData(&achievements.syncData);
            break;
        }
        case Image:
            images.push_back(entry);
            break;
        case Setting:
        {
            // add the entry to the list
            settings.entries.push_back(entry);

            // create a sync for the entry
            SyncEntry sync;
            sync.entryID = entry.id;
            if (entry.id != GamercardTitleAchievementsEarned && entry.id != GamercardTitleCredEarned &&
                    entry.id != GamercardCred && entry.id != GamercardAchievementsEarned)
                sync.syncValue = settings.syncData.nextSyncID++;
            else
                sync.syncValue = 0;
            settings.syncs.toSync.push_back(sync);
            settings.syncs.lengthChanged = true;

            // re-Write the sync list
            WriteSyncList(&settings.syncs);
            WriteSyncData(&settings.syncData);
            break;
        }
        case Title:
        {
            // add the entry to the list
            titlesPlayed.entries.push_back(entry);
            // create a sync for the entry
            SyncEntry sync = { entry.id, titlesPlayed.syncData.nextSyncID++ };
            titlesPlayed.syncs.toSync.push_back(sync);
            titlesPlayed.syncs.lengthChanged = true;
            // re-Write the sync list
            WriteSyncList(&titlesPlayed.syncs);
            WriteSyncData(&titlesPlayed.syncData);
            break;
        }
        case String:
            strings.push_back(entry);
            break;
        case AvatarAward:
        {
            // add the entry to the list
            avatarAwards.entries.push_back(entry);
            // create a sync for the entry
            SyncEntry sync = { entry.id, avatarAwards.syncData.nextSyncID++ };
            avatarAwards.syncs.toSync.push_back(sync);
            avatarAwards.syncs.lengthChanged = true;
            // re-Write the sync list
            WriteSyncList(&avatarAwards.syncs);
            WriteSyncData(&avatarAwards.syncData);
            break;
        }
    }

    WriteEntryListing();

    WriteHeader();

    return entry;
}

DWORD Xdbf::AllocateMemory(DWORD size)
{
    if (size == 0)
        return GetRealAddress(0);

    DWORD toReturn;

    // first checek and see if we can allocate some of the memory in the free memory table
    size_t index = 0;
    for (; index < freeMemory.size() - 1; index++)
    {
        if (freeMemory.at(index).length >= size)
            break;
    }
    // if the memory wasn't found in the table, then we need to append it to the file
    if (index == (freeMemory.size() - 1))
    {
        // get the position in the file of the memory allocated
        io->SetPosition(0, ios_base::end);
        toReturn = (DWORD)io->GetPosition();

        io->Flush();
        io->SetPosition(size - 1, ios_base::end);
        io->Write((BYTE)0);
        io->Flush();

        updateFreeMemTable();
        WriteFreeMemTable();
    }
    else
    {
        // get the the real address of the free memory in the file
        toReturn = GetRealAddress(freeMemory.at(index).addressSpecifier);

        // erase the entry used from the vector
        XdbfFreeMemEntry temp = freeMemory.at(index);
        freeMemory.erase(freeMemory.begin() + index);

        // update the header
        header.freeMemTableEntryCount--;

        // if there is more memory there than needed, then give some back
        if (size != temp.length)
            DeallocateMemory(GetRealAddress(temp.addressSpecifier + size), temp.length - size);

        // update what needs to be updated
        WriteFreeMemTable();
        WriteHeader();
    }

    return toReturn;
}

void Xdbf::DeallocateMemory(DWORD addr, DWORD size)
{
    if (size == 0)
        return;

    // create a new free mem table entry
    XdbfFreeMemEntry entry;
    entry.addressSpecifier = GetSpecifier(addr);
    entry.length = size;

    // add the new entry to the table
    freeMemory.insert(freeMemory.begin() + (freeMemory.size() - 1), entry);

    // update the header
    header.freeMemTableEntryCount++;
    updateFreeMemTable();

    // update the table
    WriteFreeMemTable();
}

void Xdbf::updateFreeMemTable()
{
    io->SetPosition(0, ios_base::end);
    DWORD len = GetSpecifier(io->GetPosition());

    freeMemory.back().addressSpecifier = len;
    freeMemory.back().length = 0xFFFFFFFF - len;
}

void Xdbf::readEntryGroup(XdbfEntryGroup *group, EntryType type)
{
    XdbfEntry entry = { (EntryType)io->ReadInt16(), io->ReadUInt64(), io->ReadDword(), io->ReadDword() };
    group->syncData.entry.type = (EntryType)0;
    group->syncs.entry.type = (EntryType)0;

    while (entry.type == type)
    {
        if (entry.id == ((type == AvatarAward) ? 1 : 0x100000000))
            group->syncs = readSyncList(entry);
        else if (entry.id == ((type == AvatarAward) ? 2 : 0x200000000))
            group->syncData = readSyncData(entry);
        else
            group->entries.push_back(entry);

        entry.type = (EntryType)io->ReadInt16();
        entry.id = io->ReadUInt64();
        entry.addressSpecifier = io->ReadDword();
        entry.length = io->ReadDword();
    }

    // back the io up 1 entry
    io->SetPosition((DWORD)io->GetPosition() - 0x12);
}

void Xdbf::readEntryGroup(vector<XdbfEntry> *group, EntryType type)
{
    XdbfEntry entry;

    while (true)
    {
        entry.type = (EntryType)io->ReadInt16();
        entry.id = io->ReadUInt64();
        entry.addressSpecifier = io->ReadDword();
        entry.length = io->ReadDword();

        if (entry.type != type)
            break;
        group->push_back(entry);
    }

    // back the io up 1 entry
    io->SetPosition((DWORD)io->GetPosition() - 0x12);
}

void Xdbf::WriteEntryListing()
{
    // seek to the begining of the entry table
    io->SetPosition(0x18);

    // clear the current table
    BYTE buffer[0x12] = {0};
    for (DWORD i = 0; i < header.entryTableLength; i++)
        io->Write(buffer, 0x12);
    io->SetPosition(0x18);
    io->Flush();

    // Write achievement entries
    WriteEntryGroup(&achievements);
    io->Flush();

    // Write image entries
    WriteEntryGroup(&images);
    io->Flush();

    // Write setting entries
    WriteEntryGroup(&settings);
    io->Flush();

    // Write title entries
    WriteEntryGroup(&titlesPlayed);
    io->Flush();

    // Write string entries
    WriteEntryGroup(&strings);
    io->Flush();

    // Write avatar award entries
    WriteEntryGroup(&avatarAwards);
    io->Flush();

    // just to be safe i guess
    io->Flush();
}

void Xdbf::WriteFreeMemTable()
{
    // update the last free memory table entry
    io->SetPosition(0, ios_base::end);
    DWORD temp = GetSpecifier(io->GetPosition());
    freeMemory.at(freeMemory.size() - 1).addressSpecifier = temp;
    freeMemory.at(freeMemory.size() - 1).length = (0xFFFFFFFF - temp);

    // seek to the free memory table position
    io->SetPosition(0x18 + (header.entryTableLength * 0x12));

    // make sure we didn't run out of free memory table space
    if (freeMemory.size() > header.freeMemTableLength)
        Clean();

    // Write the table
    for (DWORD i = 0; i < freeMemory.size(); i++)
    {
        io->Write(freeMemory.at(i).addressSpecifier);
        io->Write(freeMemory.at(i).length);
    }

    // null out the rest of the table
    for (DWORD i = 0; i < (header.freeMemTableLength - freeMemory.size()); i++)
        io->Write((UINT64)0);
}

void Xdbf::WriteHeader()
{
    io->SetPosition(0);
    io->Write(header.magic);
    io->Write(header.version);
    io->Write(header.entryTableLength);
    io->Write(header.entryCount);
    io->Write(header.freeMemTableLength);
    io->Write(header.freeMemTableEntryCount);
}

void Xdbf::WriteEntryGroup(XdbfEntryGroup *group)
{
    std::sort(group->entries.begin(), group->entries.end(), compareEntries);

    if (group->syncs.entry.type == AvatarAward)
    {
        // Write the sync stuffs
        if (group->syncs.entry.type != 0)
            WriteEntry(&group->syncs.entry);
        if (group->syncData.entry.type != 0)
            WriteEntry(&group->syncData.entry);

        // Write all the entries
        for (DWORD i = 0; i < group->entries.size(); i++)
            WriteEntry(&group->entries.at(i));
    }
    else
    {
        // Write all the entries
        for (DWORD i = 0; i < group->entries.size(); i++)
            WriteEntry(&group->entries.at(i));

        // Write the sync stuffs
        if (group->syncs.entry.type != 0)
            WriteEntry(&group->syncs.entry);
        if (group->syncData.entry.type != 0)
            WriteEntry(&group->syncData.entry);
    }
}

void Xdbf::WriteEntryGroup(vector<XdbfEntry> *group)
{
    std::sort(group->begin(), group->end(), compareEntries);

    // Write all the entries
    for (DWORD i = 0; i < group->size(); i++)
        WriteEntry(&group->at(i));
}

void Xdbf::WriteEntry(XdbfEntry *entry)
{
    io->Write((WORD)entry->type);
    io->Write((UINT64)entry->id);
    io->Write((DWORD)entry->addressSpecifier);
    io->Write((DWORD)entry->length);
}

void Xdbf::UpdateEntry(XdbfEntry *entry)
{
    // neither of these entry types have syncs
    if (entry->type == Image || entry->type == String)
        return;

    XdbfEntryGroup *group;
    switch (entry->type)
    {
        case Achievement:
            group = &achievements;
            break;
        case Setting:
            group = &settings;
            break;
        case Title:
            group = &titlesPlayed;
            break;
        case AvatarAward:
            group = &avatarAwards;
            break;
        default:
            throw string("Xdbf: Error updating entry. Invalid entry type.\n");
    }

    // find the entry in the table and update it
    for (DWORD i = 0; i < group->entries.size(); i++)
    {
        if (group->entries.at(i).id == entry->id)
        {
            group->entries.at(i).addressSpecifier = entry->addressSpecifier;
            group->entries.at(i).length = entry->length;
        }
    }

    // if the sync data doesn't exist, then create it
    if (group->syncData.entry.type == 0)
    {
        group->syncData.entry = CreateEntry(entry->type, (entry->type == AvatarAward) ? 2 : 0x200000000,
                0x18);
        group->syncData.lastSyncedTime = NULL;
        group->syncData.lastSyncID = 0;
        group->syncData.nextSyncID = 1;
    }

    // if the sync list doesn't exist, then create it
    if (group->syncs.entry.type == 0)
    {
        group->syncs.entry = CreateEntry(entry->type, (entry->type == AvatarAward) ? 1 : 0x100000000, 0);
        group->syncs.lengthChanged = true;
    }

    bool syncExists = false;

    // find the sync if it isn't already in the queue
    for (size_t i = 0; i < group->syncs.synced.size(); i++)
    {
        if (group->syncs.synced.at(i).entryID == entry->id)
        {
            SyncEntry sync = group->syncs.synced.at(i);

            // erase it from the synced list
            group->syncs.synced.erase(group->syncs.synced.begin() + i);

            // add the sync to the queue
            sync.syncValue = group->syncData.nextSyncID++;
            group->syncs.toSync.push_back(sync);

            syncExists = true;
        }
    }

    // if it is alread in the queue, then push it to the front
    for (int i = 0; i < (int)(group->syncs.toSync.size() - 1); i++)
    {
        if (group->syncs.toSync.at(i).entryID == entry->id)
        {
            SyncEntry sync = group->syncs.toSync.at(i);

            // move the entry to the end
            group->syncs.toSync.erase(group->syncs.toSync.begin() + i);
            sync.syncValue = group->syncData.nextSyncID++;
            group->syncs.toSync.push_back(sync);

            syncExists = true;
        }
    }

    // if the sync entry doesn't exist then we need to create it
    if (!syncExists)
    {
        SyncEntry sync = { entry->id,  group->syncData.nextSyncID++ };
        group->syncs.toSync.push_back(sync);
    }

    // re-Write the new sync list
    WriteSyncList(&group->syncs);

    // re-Write the updated sync data
    WriteSyncData(&group->syncData);

    WriteEntryListing();
}

void Xdbf::ReWriteEntry(XdbfEntry entry, BYTE *entryBuffer)
{
    // get the entry list
    vector<XdbfEntry> *entryList;
    switch (entry.type)
    {
        case Achievement:
            entryList = &achievements.entries;
            break;
        case Image:
            entryList = &images;
            break;
        case Setting:
            entryList = &settings.entries;
            break;
        case Title:
            entryList = &titlesPlayed.entries;
            break;
        case String:
            entryList = &strings;
            break;
        case AvatarAward:
            entryList = &avatarAwards.entries;
            break;
        default:
            throw string("Xdbf: Error rewriting entry, entry type not supported.\n");
    }

    // make sure the entry already exists
    DWORD i;
    for (i = 0; i < entryList->size(); i++)
        if (entryList->at(i).id == entry.id)
            break;
    if (i == entryList->size())
        throw string("Xdbf: Error rewriting entry, entry not found.\n");

    // if the size has changed, then we need to reallocate memory
    if (entry.length != entryList->at(i).length)
    {
        DeallocateMemory(GetRealAddress(entryList->at(i).addressSpecifier), entryList->at(i).length);
        entryList->at(i).addressSpecifier = GetSpecifier(AllocateMemory(entry.length));
        entryList->at(i).length = entry.length;
    }

    // Write the entry
    io->SetPosition(GetRealAddress(entry.addressSpecifier));
    io->Write(entryBuffer, entry.length);

    // update the file
    UpdateEntry(&entryList->at(i));
    WriteEntryListing();
}

void Xdbf::DeleteEntry(XdbfEntry entry)
{
    // make sure that the entry exists
    vector<XdbfEntry> *entries;
    XdbfEntryGroup *group = 0;
    switch (entry.type)
    {
        case Achievement:
            entries = &achievements.entries;
            group = &achievements;
            break;
        case Image:
            entries = &images;
            break;
        case Setting:
            entries = &settings.entries;
            group = &settings;
            break;
        case Title:
            entries = &titlesPlayed.entries;
            group = &titlesPlayed;
            break;
        case String:
            entries = &strings;
            break;
        case AvatarAward:
            entries = &avatarAwards.entries;
            group = &avatarAwards;
            break;
        default:
            throw string("Xdbf: Error deleting entry. Invalid entry type.\n");
    }
    DWORD index;
    for (index = 0; index < entries->size(); index++)
        if (entries->at(index).id == entry.id)
            break;
    // if the entry doesn't exist then we have some problems
    if (index == entries->size())
        throw string("Xdbf: Error deleting entry. Specified entry doesn't exist.");

    // deallocate the entry's memory
    DeallocateMemory(GetRealAddress(entry.addressSpecifier), entry.length);

    // remove the entry from the listing
    entries->erase(entries->begin() + index);

    // delete the sync if needed
    switch (entry.type)
    {
        case Achievement:
        case Setting:
        case Title:
        case AvatarAward:
            // find the sync and delete it
            for (DWORD i = 0; i < group->syncs.synced.size(); i++)
                if (group->syncs.synced.at(i).entryID == entry.id)
                {
                    group->syncs.synced.erase(group->syncs.synced.begin() + i);
                    group->syncs.lengthChanged = true;
                    goto WriteSyncs;
                }
            for (DWORD i = 0; i < group->syncs.toSync.size(); i++)
                if (group->syncs.toSync.at(i).entryID == entry.id)
                {
                    group->syncs.toSync.erase(group->syncs.toSync.begin() + i);
                    group->syncs.lengthChanged = true;
                    break;
                }

WriteSyncs:
            WriteSyncList(&group->syncs);
            break;
        case Image:
        case String:
            break;
        default:
            throw string("Xdbf: Error deleting entry. Invalid entry type.\n");
    }

    // re-Write the entry table
    WriteEntryListing();

    // update the header
    header.entryCount--;
    WriteHeader();
}

void Xdbf::ExtractEntry(XdbfEntry entry, BYTE *outBuffer)
{
    // seek to the begining of the entry
    io->SetPosition((UINT64)GetRealAddress(entry.addressSpecifier));

    // read the bytes of the entry
    io->ReadBytes(outBuffer, entry.length);
}

bool compareEntries(XdbfEntry a, XdbfEntry b)
{
    if (a.type < b.type)
        return true;
    else
        return a.id < b.id;
}
