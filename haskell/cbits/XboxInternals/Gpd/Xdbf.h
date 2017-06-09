#pragma once

#include <iostream>
#include <vector>
#include <algorithm>
#include "IO/FileIO.h"
#include "XdbfDefininitions.h"
#include "XdbfHelpers.h"

#include "XboxInternals_global.h"

using std::string;
using std::vector;

class XBOXINTERNALSSHARED_EXPORT Xdbf
{
public:
    Xdbf(string gpdPath);
    Xdbf(FileIO *io);
    ~Xdbf();

    XdbfEntryGroup achievements;
    vector<XdbfEntry> images;
    XdbfEntryGroup settings;
    XdbfEntryGroup titlesPlayed;
    vector<XdbfEntry> strings;
    XdbfEntryGroup avatarAwards;

    // Description: extract the bytes of a given entry to the out buffer
    void ExtractEntry(XdbfEntry entry, BYTE *outBuffer);

    // Description: delete an entry from the file
    void DeleteEntry(XdbfEntry entry);

    // Description: convert a specifier address into a real address
    DWORD GetRealAddress(DWORD specifier);

    // Description: convert a real address into a specifer
    DWORD GetSpecifier(DWORD address);

    // Description: create a new entry and new sync in the file and return the entry
    XdbfEntry CreateEntry(EntryType type, UINT64 id, DWORD length);

    // Description: allocate memory in the file
    DWORD AllocateMemory(DWORD size);

    // Description: free memory in the file at the specifer address
    void DeallocateMemory(DWORD addr, DWORD size);

    // Description: move the sync to the queue
    void UpdateEntry(XdbfEntry *entry);

    // Description: remove all the unused memory in the file
    void Clean();

    // Description: re-Write an entry
    void ReWriteEntry(XdbfEntry entry, BYTE *entryBuffer);
    FileIO *io;

private:
    bool ioPassedIn;
    XdbfHeader header;
    vector<XdbfFreeMemEntry> freeMemory;

    // Description: read in the Xdbf header
    void readHeader();

    // Description: read in the file table
    void readEntryTable();

    // Description: read the free memory table
    void readFreeMemoryTable();

    // Description: re-Write the entry listing
    void WriteEntryListing();

    // Description: re-Write the free memory table
    void WriteFreeMemTable();

    // Description: re-Write the header
    void WriteHeader();

    // Description: read a sync list based off the entry passed in
    SyncList readSyncList(XdbfEntry entry);

    // Description: Write the sync list back to the file
    void WriteSyncList(SyncList *syncs);

    // Description: read sync data based off the entry passed in
    SyncData readSyncData(XdbfEntry entry);

    // Description: Write the sync data
    void WriteSyncData(SyncData *data);

    // Description: read an entry group from the table that has syncs
    void readEntryGroup(XdbfEntryGroup *group, EntryType type);

    // Description: read an entry group from the table that doesn't have syncs
    void readEntryGroup(vector<XdbfEntry> *group, EntryType type);

    // Description: Write an entry group to the table that has syncs
    void WriteEntryGroup(XdbfEntryGroup *group);

    // Description: Write an entry group to the table that doesn't have syncs
    void WriteEntryGroup(vector<XdbfEntry> *group);

    // Description: null out the structs that need it
    void init();

    // Description: update the final entry in the free memory, stores the file length
    void updateFreeMemTable();

    // Description: Write an entry to the entry table
    void WriteEntry(XdbfEntry *entry);

    // Description: Write an entry group to the file that has syncs, used when cleaning
    void WriteNewEntryGroup(XdbfEntryGroup *group, FileIO *newIO);

    // Description: Write an entry group that doesn't have syncs, used when cleaning
    void WriteNewEntryGroup(vector<XdbfEntry> *group, FileIO *newIO);

    // Description: Write entry to the new file, used when cleaing
    void WriteNewEntry(XdbfEntry *entry, FileIO *newIO);
};

bool compareEntries(XdbfEntry a, XdbfEntry b);
