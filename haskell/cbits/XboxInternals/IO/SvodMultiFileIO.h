#ifndef MULTIFILEIO_H
#define MULTIFILEIO_H

#include "IO/FileIO.h"
#include <iostream>
#include <vector>
#include "BaseIO.h"
#include "XboxInternals_global.h"

using std::string;
using std::wstring;
using std::vector;

class XBOXINTERNALSSHARED_EXPORT SvodMultiFileIO : public BaseIO
{
public:
    SvodMultiFileIO(string fileDirectory);
    virtual ~SvodMultiFileIO();

    // seek to a certain address in the file, index of -1 for current file
    void SetPosition(DWORD addressInFile, DWORD fileIndex = -1);

    // get the current position of the io
    void GetPosition(DWORD *addressInFile, DWORD *fileIndex);

    // read len bytes from the current file at the current position into buffer
    void ReadBytes(BYTE *outBuffer, DWORD len);

    // Write len bytes to the file at the current position
    void WriteBytes(BYTE *buffer, DWORD len);

    void Close();

    void Flush();

    UINT64 Length();

    // get the number of files in the directory
    DWORD FileCount();

    // get the number of bytes in the current file
    DWORD CurrentFileLength();

    // unused
    void SetPosition(UINT64 position, std::ios_base::seek_dir dir = std::ios_base::beg);
    UINT64 GetPosition();

private:
    DWORD addressInFile;
    DWORD fileIndex;

    FileIO *currentIO;
    vector<string> files;

    // get all the file names in the directory
    void loadDirectories(string path);
};

#endif // MULTIFILEIO_H
