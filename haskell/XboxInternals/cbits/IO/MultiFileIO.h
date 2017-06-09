/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#ifndef MULTIFILEIO_H
#define MULTIFILEIO_H

#include "FileIO.h"
#include <vector>

class XBOXINTERNALSSHARED_EXPORT MultiFileIO : public BaseIO
{
public:
    MultiFileIO(std::vector<std::string> filePaths);
    MultiFileIO(std::vector<BaseIO*> files);
    virtual ~MultiFileIO();

    // seek to a position in a file
    void SetPosition(UINT64 position, std::ios_base::seek_dir dir = std::ios_base::beg);

    // get current address in the file
    UINT64 GetPosition();

    // get the length of all the files
    UINT64 Length();

    // read len bytes from the current file at the current position into buffer
    void ReadBytes(BYTE *outBuffer, DWORD len);

    // Write len bytes from the current file at the current position into buffer
    void WriteBytes(BYTE *buffer, DWORD len);

    // flushes the stream
    void Flush();

    // closes all the files
    void Close();

private:
    UINT64 pos, lengthOfFiles;
    bool isClosed;
    DWORD currentIOIndex;
    std::vector<BaseIO*> files;

    void calcualteLengthOfAllFiles();
};

#endif // MULTIFILEIO_H
