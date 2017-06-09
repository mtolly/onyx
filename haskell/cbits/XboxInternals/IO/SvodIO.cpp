#include "SvodIO.h"

SvodIO::SvodIO(XContentHeader *metadata, GdfxFileEntry entry, SvodMultiFileIO *io) :
    BaseIO(), io(io), metadata(metadata), fileEntry(entry), pos(0)
{
    offset = ((metadata->svodVolumeDescriptor.flags & EnhancedGDFLayout) ? 0x2000 : 0x1000);

    // seek to the file's begining
    DWORD addr, index;
    SectorToAddress(entry.sector, &addr, &index);
    io->SetPosition(addr, index);
}

SvodIO::~SvodIO()
{

}

void SvodIO::SectorToAddress(DWORD sector, DWORD *addressInDataFile, DWORD *dataFileIndex)
{
    DWORD trueSector = (sector - (metadata->svodVolumeDescriptor.dataBlockOffset * 2)) % 0x14388;
    *addressInDataFile = trueSector * 0x800;
    *dataFileIndex = (sector - (metadata->svodVolumeDescriptor.dataBlockOffset * 2)) / 0x14388;

    // for the silly hash tables at the beginning
    *addressInDataFile += offset;

    // for the data hash table(s)
    *addressInDataFile += ((trueSector / 0x198) + ((trueSector % 0x198 == 0 &&
            trueSector != 0) ? 0 : 1)) * 0x1000;
}

void SvodIO::SetPosition(UINT64 address, ios_base::seek_dir dir)
{
    if (dir != std::ios_base::beg)
        throw std::string("SvodIO: Unsupported seek direction\n");

    /* DISCLAIMER: This function is not perfect and will not work for all SVOD systems. If the
       system has more than 204 (0xCC) data files, then this function may not work. */

    DWORD baseAddr, baseIndex, index, addr;
    SectorToAddress(fileEntry.sector, &baseAddr, &baseIndex);

    // the number of bytes between the file start and the end of the hash table before it
    DWORD baseHashOff = (baseAddr - 0x2000) % 0xCD000;

    // the amount of bytes taken up by level0 hash tables inbetween the file start and the seek address
    DWORD totalHashOffset = ((baseHashOff + address) / 0xCC000) << 0xC;

    index = ((baseAddr + address + totalHashOffset) / 0xA290000) + baseIndex;
    addr = (baseAddr + address + totalHashOffset) % 0xA290000;

    // account for level1 hash tables
    addr += ((baseAddr + address + totalHashOffset) / 0xA290000) << 0xC;
    if (addr >= 0xA290000)
    {
        index++;
        addr = (addr % 0xA290000) + 0x2000;
    }

    // seek to the position
    io->SetPosition(addr, index);
    pos = address;
}

UINT64 SvodIO::GetPosition()
{
    return pos;
}

UINT64 SvodIO::Length()
{
    return fileEntry.size;
}

void SvodIO::ReadBytes(BYTE *outBuffer, DWORD len)
{
    // all the SvodIOs are using the same IO underneath, so we have to make sure we're at the correct pos
    SetPosition(pos);
    pos += len;

    // get the current position
    DWORD addr, index;
    io->GetPosition(&addr, &index);

    // calculate the amount of bytes until the next hash table
    DWORD bytesUntilTable = 0xCC000 - ((addr - 0x2000) % 0xCD000);

    // calculate the amount of bytes to read
    DWORD bytesToRead = (bytesUntilTable > len) ? len : bytesUntilTable;

    // read the bytes before the table
    io->ReadBytes(outBuffer, bytesToRead);
    outBuffer += bytesToRead;
    len -= bytesToRead;

    // read in between all the hash tables
    while (len >= 0xCC000)
    {
        // check to see if we're at the end of a data file
        io->GetPosition(&addr, &index);
        if (addr == 0)
            io->SetPosition((DWORD)0x2000, index);
        else
            io->SetPosition((DWORD)addr + 0x1000);

        io->ReadBytes(outBuffer, 0xCC000);
        outBuffer += 0xCC000;
        len -= 0xCC000;
    }

    if (len != 0)
    {
        // check to see if we're at the end of a data file
        io->GetPosition(&addr, &index);
        if (addr == 0)
            io->SetPosition((DWORD)0x2000, index);
        else
            io->SetPosition((DWORD)(addr + 0x1000));

        io->ReadBytes(outBuffer, len);
    }
}

void SvodIO::WriteBytes(BYTE *buffer, DWORD len)
{
    // all the SvodIOs are using the same IO underneath, so we have to make sure we're at the correct pos
    SetPosition(pos);
    pos += len;

    // get the current position
    DWORD addr, index;
    io->GetPosition(&addr, &index);

    // calculate the amount of bytes until the next hash table
    DWORD bytesUntilTable = 0xCC000 - ((addr - 0x2000) % 0xCD000);

    // calculate the amount of bytes to read
    DWORD bytesToWrite = (bytesUntilTable > len) ? len : bytesUntilTable;

    // Write the bytes before the table
    io->WriteBytes(buffer, bytesToWrite);
    buffer += bytesToWrite;
    len -= bytesToWrite;

    // Write in between all the hash tables
    while (len >= 0xCC000)
    {
        // check to see if we're at the end of a data file
        io->GetPosition(&addr, &index);
        if (addr == 0xA290000)
            io->SetPosition((DWORD)0, index + 1);
        else
            io->SetPosition(addr + 0x1000);

        io->WriteBytes(buffer, 0xCC000);
        buffer += 0xCC000;
        len -= 0xCC000;
    }

    if (len != 0)
    {
        // check to see if we're at the end of a data file
        io->GetPosition(&addr, &index);
        if (addr == 0xA290000)
            io->SetPosition((DWORD)0, index + 1);
        else
            io->SetPosition((DWORD)(addr + 0x1000));

        io->WriteBytes(buffer, len);
    }
}

void SvodIO::SaveFile(string savePath, void(*progress)(void*, DWORD, DWORD), void *arg)
{
    FileIO outFile(savePath, true);
    BYTE *buffer = new BYTE[0x10000];
    DWORD fileLen = fileEntry.size;
    DWORD total = (fileLen + 0xFFFF) / 0x10000;
    DWORD cur = 0;

    while (fileLen >= 0x10000)
    {
        ReadBytes(buffer, 0x10000);
        outFile.Write(buffer, 0x10000);
        fileLen -= 0x10000;

        if (progress)
            progress(arg, cur++, total);
    }

    if (fileLen != 0)
    {
        ReadBytes(buffer, fileLen);
        outFile.Write(buffer, fileLen);
    }

    if (progress)
        progress(arg, total, total);

    outFile.Close();
    delete[] buffer;
}

void SvodIO::OverWriteFile(string inPath, void (*progress)(void *, DWORD, DWORD), void *arg)
{
    FileIO inFile(inPath);

    // make sure that the files are the same size
    inFile.SetPosition(0, ios_base::end);
    if ((DWORD)inFile.GetPosition() != fileEntry.size)
        throw string("SVOD: Cannot overWrite file of different length.\n");

    BYTE *buffer = new BYTE[0x10000];
    DWORD fileLen = fileEntry.size;
    DWORD total = (fileLen + 0xFFFF) / 0x10000;
    DWORD cur = 0;

    inFile.SetPosition(0);
    SetPosition(0);

    while (fileLen >= 0x10000)
    {
        inFile.ReadBytes(buffer, 0x10000);
        WriteBytes(buffer, 0x10000);
        fileLen -= 0x10000;

        if (progress)
            progress(arg, cur++, total);
    }

    if (fileLen != 0)
    {
        inFile.ReadBytes(buffer, fileLen);
        WriteBytes(buffer, fileLen);
    }

    if (progress)
        progress(arg, total, total);

    inFile.Close();
    delete[] buffer;
}

void SvodIO::Close()
{
    // all SvodIOs use the same internal io, so we can't close it
}

void SvodIO::Flush()
{
    io->Flush();
}
