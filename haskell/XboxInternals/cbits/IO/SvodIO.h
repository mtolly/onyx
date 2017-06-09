#ifndef SVODIO_H
#define SVODIO_H

#include "../Disc/Gdfx.h"
#include "BaseIO.h"
#include "../Stfs/XContentHeader.h"
#include "XboxInternals_global.h"

class XBOXINTERNALSSHARED_EXPORT SvodIO : public BaseIO
{
public:
    SvodIO(XContentHeader *metadata, GdfxFileEntry entry, SvodMultiFileIO *io);

    virtual ~SvodIO();

    void ReadBytes(BYTE *outBuffer, DWORD len);

    void WriteBytes(BYTE *buffer, DWORD len);

    void SaveFile(string savePath, void(*progress)(void*, DWORD, DWORD) = NULL, void *arg = NULL);

    void OverWriteFile(string inPath, void (*progress)(void*, DWORD, DWORD) = NULL, void *arg = NULL);

    void SetPosition(UINT64 address, std::ios_base::seek_dir dir = std::ios_base::beg);

    UINT64 GetPosition();

    UINT64 Length();

    void Flush();

    void Close();

private:
    void SectorToAddress(DWORD sector, DWORD *addressInDataFile, DWORD *dataFileIndex);

    SvodMultiFileIO *io;
    XContentHeader *metadata;
    GdfxFileEntry fileEntry;
    DWORD pos;
    DWORD offset;
};

#endif // SVODIO_H
