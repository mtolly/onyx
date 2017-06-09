/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#ifndef DEVICEIO_H
#define DEVICEIO_H

#define FAT_SECTOR_SIZE 0x200

#include "BaseIO.h"
#include "../Fatx/FatxHelpers.h"
#include "XboxInternals_global.h"

class XBOXINTERNALSSHARED_EXPORT DeviceIO : public BaseIO
{
public:
    #ifdef __WIN32
    DeviceIO(void* deviceHandle);
    #endif
    
    DeviceIO(std::string devicePath);
    DeviceIO(std::wstring devicePath);
    virtual ~DeviceIO();

    void ReadBytes(BYTE *outBuffer, DWORD len);

    void WriteBytes(BYTE *buffer, DWORD len);

    void SetPosition(UINT64 address, std::ios_base::seek_dir dir = std::ios_base::beg);

    UINT64 GetPosition();

    UINT64 Length();

    void Close();

    void Flush();

private:
    void loadDevice(std::wstring devicePath);

    UINT64 realPosition();

    class Impl;
    Impl* impl;

    std::string yolo;

    UINT64 pos;
    BYTE lastReadData[FAT_SECTOR_SIZE];
    UINT64 lastReadOffset;
};

#endif // DEVICEIO_H
