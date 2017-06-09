#ifndef GDFX_H
#define GDFX_H

#include "winnames.h"
#include "IO/SvodMultiFileIO.h"
#include <iostream>
#include <vector>

using std::string;
using std::vector;

struct GdfxHeader
{
    BYTE magic[0x14];   // MICROSOFT*XBOX*MEDIA
    DWORD rootSector;
    DWORD rootSize;
    WINFILETIME creationTime;
};

struct GdfxFileEntry
{
    // in the file
    DWORD unknown;
    DWORD sector;
    DWORD size;
    BYTE attributes;
    BYTE nameLen;
    string name;

    // extra stuff
    string filePath;
    vector<GdfxFileEntry> files;
    DWORD address;
    DWORD fileIndex;
};

enum GdfxDirentAttributesutes
{
    GdfxReadOnly = 0x01,
    GdfxHidden = 0x02,
    GdfxSystem = 0x04,
    GdfxDirectory = 0x10,
    GdfxArchive = 0x20,
    GdfxDevice = 0x40,
    GdfxNormal = 0x80
};

// TODO: make this work with the IO interface, and Write header

// read the GDFX header, seek io to position of header beforehand
void GdfxReadHeader(SvodMultiFileIO *io, GdfxHeader *header);

// read the next file entry in the listing, reeturns false on listing end
bool GdfxReadFileEntry(SvodMultiFileIO *io, GdfxFileEntry *entry);

// Write a file entry back to the listing
void GdfxWriteFileEntry(SvodMultiFileIO *io, GdfxFileEntry *entry);

#endif // GDFX_H
