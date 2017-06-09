#ifndef SVOD_H
#define SVOD_H

#include "IO/FileIO.h"
#include "IO/SvodMultiFileIO.h"
#include "Gdfx.h"
#include "Stfs/XContentHeader.h"
#include <iostream>
#include <vector>
#include "IO/SvodIO.h"
#include <algorithm>
#include "botan/botan.h"
#include "botan/sha160.h"

#include "XboxInternals_global.h"

using std::string;
using std::vector;

class XBOXINTERNALSSHARED_EXPORT SVOD
{
public:
    SVOD(string rootFile);
    ~SVOD();

    XContentHeader *metadata;
    vector<GdfxFileEntry> root;

    // get a SvodIO for the given entry
    SvodIO GetSvodIO(GdfxFileEntry entry);

    // get a SvodIO for the given entry
    SvodIO GetSvodIO(string path);

    // get the address and file index for a sector
    void SectorToAddress(DWORD sector, DWORD *addressInDataFile, DWORD *dataFileIndex);

    // fix all of the hashes in the system
    void Rehash(void (*progress)(DWORD, DWORD, void*) = NULL, void *arg = NULL);

    // fix the RSA signature in the root descriptor
    void Resign(string kvPath);

    // Write a file entry back to the system
    void WriteFileEntry(GdfxFileEntry *entry);

    // get the total number of sectors in the system
    DWORD GetSectorCount();

private:
    string contentDirectory;
    SvodMultiFileIO *io;
    FileIO *rootFile;
    GdfxHeader header;
    DWORD baseAddress;
    DWORD offset;

    // parse the file listing
    void ReadFileListing(vector<GdfxFileEntry> *entryList, DWORD sector, int size, string path);

    // get a file entry from the path, must start with a /
    GdfxFileEntry GetFileEntry(string path, vector<GdfxFileEntry> *listing);

    // hash a 0x1000 byte block
    void HashBlock(BYTE *block, BYTE *outHash);
};

int compareFileEntries(GdfxFileEntry a, GdfxFileEntry b);

#endif // SVOD_H
