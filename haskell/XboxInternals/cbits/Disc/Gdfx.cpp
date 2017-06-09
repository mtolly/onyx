#include "Gdfx.h"

void GdfxReadHeader(SvodMultiFileIO *io, GdfxHeader *header)
{
    // make sure that the byte order is little endian
    io->SetEndian(LittleEndian);

    // verify the magic
    io->ReadBytes((BYTE*)header->magic, 0x14);
    if (memcmp(header->magic, "MICROSOFT*XBOX*MEDIA", 0x14) != 0)
        throw string("GDFX: Invalid header magic\n");

    // read the rest of the header
    header->rootSector = io->ReadDword();
    header->rootSize = io->ReadDword();
    header->creationTime.dwLowDateTime = io->ReadDword();
    header->creationTime.dwHighDateTime = io->ReadDword();
}

bool GdfxReadFileEntry(SvodMultiFileIO *io, GdfxFileEntry *entry)
{
    // everything's little endian, so let's be safe
    io->SetEndian(LittleEndian);

    io->GetPosition(&entry->address, &entry->fileIndex);

    // check to see if we're at the end
    DWORD nextBytes = io->ReadDword();
    if (nextBytes == 0xFFFFFFFF)
        return false;

    // parse the entry
    entry->unknown = nextBytes;
    entry->sector = io->ReadDword();
    entry->size = io->ReadDword();
    entry->attributes = io->ReadByte();
    entry->nameLen = io->ReadByte();
    entry->name = io->ReadString(entry->nameLen);

    return true;
}

void GdfxWriteFileEntry(SvodMultiFileIO *io, GdfxFileEntry *entry)
{
    // everything's little endian, so let's be safe
    io->SetEndian(LittleEndian);

    // seek to the file entry
    io->SetPosition(entry->address, entry->fileIndex);

    // Write the entry
    io->Write(entry->unknown);
    io->Write(entry->sector);
    io->Write(entry->size);
    io->Write(entry->attributes);
    io->Write(entry->nameLen);
    io->Write(entry->name);
}
