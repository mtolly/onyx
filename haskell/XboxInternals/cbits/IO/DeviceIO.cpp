#include "DeviceIO.h"
#include <string.h>
#include <errno.h>

#ifdef _WIN32
#include <windows.h>
#include <WinIoCtl.h>
#else
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#if __APPLE__
#include <sys/disk.h>
#elif __linux
#include <linux/fs.h>
#endif
#include <unistd.h>
#endif

#ifdef __linux
#define SECTOR_COUNT BLKGETSIZE
#define SECTOR_SIZE BLKSSZGET
#elif __APPLE__
#define SECTOR_COUNT DKIOCGETBLOCKCOUNT
#define SECTOR_SIZE DKIOCGETBLOCKSIZE
#endif

class DeviceIO::Impl
{
public:
#ifdef _WIN32
    HANDLE deviceHandle;
    OVERLAPPED offset;
#else
    int device;
    INT64 offset;
#endif
};

#ifdef __WIN32
DeviceIO::DeviceIO(void* deviceHandle) :
    impl(new Impl), lastReadOffset(-1)
{
    pos = 0;
    if ((HANDLE)deviceHandle == INVALID_HANDLE_VALUE)
        throw std::string("DeviceIO: Invalid device handle.\n");

    this->impl->deviceHandle = (HANDLE)deviceHandle;
    memset(&impl->offset, 0, sizeof(OVERLAPPED));
}
#endif

DeviceIO::DeviceIO(std::string devicePath) :
    impl(new Impl), lastReadOffset(-1)
{
    // convert it to a wstring
    std::wstring wsDevicePath;
    wsDevicePath.assign(devicePath.begin(), devicePath.end());

    // load the device
    loadDevice(wsDevicePath);
}

DeviceIO::DeviceIO(std::wstring devicePath) :
    impl(new Impl), lastReadOffset(-1)
{
    // load the device
    loadDevice(devicePath);
}

DeviceIO::~DeviceIO()
{
    if (impl)
    {
        delete impl;
    }
}

void DeviceIO::ReadBytes(BYTE *outBuffer, DWORD len)
{
    UINT64 endingPos = pos + len;
    if ((pos & 0x1FF) == 0 && (len & 0x1FF) == 0)
    {
#ifdef _WIN32
        bool success = ReadFile(
                    impl->deviceHandle,   // Device to read from
                    outBuffer,            // Output buffer
                    len,                  // Length to read
                    NULL,                 // Pointer to the number of bytes read
                    &impl->offset);       // OVERLAPPED structure containing the offset to read from

        if (!success)
            throw std::string("DeviceIO: Error reading from device, may be disconnected.\n");
#else
        read(impl->device, outBuffer, len);
#endif

        SetPosition(endingPos);

        return;
    }

    UINT64 originalPos = pos;
    SetPosition(DOWN_TO_NEAREST_SECTOR(pos));

    if (lastReadOffset != pos)
    {
#ifdef _WIN32
        bool success = ReadFile(
                    impl->deviceHandle,   // Device to read from
                    lastReadData,         // Output buffer
                    FAT_SECTOR_SIZE,                // Length to read
                    NULL,                 // Pointer to the number of bytes read
                    &impl->offset);       // OVERLAPPED structure containing the offset to read from

        if (!success)
            throw std::string("DeviceIO: Error reading from device, may be disconnected.\n");
#else
        read(impl->device, lastReadData, FAT_SECTOR_SIZE);
#endif

        lastReadOffset = pos;
    }

    // copy over the data requested
    WORD bytesLeftInSector = FAT_SECTOR_SIZE - (originalPos & 0x1FF);
    WORD neededBytes = (len > bytesLeftInSector) ? bytesLeftInSector : len;
    memcpy(outBuffer, lastReadData + (originalPos & 0x1FF), neededBytes);

    len -= neededBytes;
    outBuffer += neededBytes;

    if (len == 0)
    {
        SetPosition(endingPos);
        return;
    }

    // seek to the next sector
    SetPosition(pos + FAT_SECTOR_SIZE); // 0x200

    INT64 downTo = DOWN_TO_NEAREST_SECTOR(len);

    // read the consecutive sectors
#ifdef _WIN32
    bool success = ReadFile(
                impl->deviceHandle,                 // Device to read from
                outBuffer,                          // Output buffer
                downTo,                             // Length to read
                NULL,                               // Pointer to the number of bytes read
                &impl->offset);                     // OVERLAPPED structure containing the offset to read from

    if (!success)
        throw std::string("DeviceIO: Error reading from device, may be disconnected.\n");
#else
    read(impl->device, outBuffer, downTo);
#endif

    // update all our values
    SetPosition(DOWN_TO_NEAREST_SECTOR(endingPos));
    outBuffer += downTo;
    len &= 0x1FF;

    if (len == 0)
    {
        SetPosition(endingPos);
        return;
    }

    // read the stragglers
#ifdef _WIN32
    success = ReadFile(
            impl->deviceHandle,                 // Device to read from
            lastReadData,                       // Output buffer
            FAT_SECTOR_SIZE,                              // Length to read
            NULL,                               // Pointer to the number of bytes read
            &impl->offset);                     // OVERLAPPED structure containing the offset to read from

    if (!success)
        throw std::string("DeviceIO: Error reading from device, may be disconnected.\n");
#else
    read(impl->device, lastReadData, FAT_SECTOR_SIZE);
#endif

    lastReadOffset = pos;

    // copy over the requested data
    memcpy(outBuffer, lastReadData, len);

    SetPosition(endingPos);
}

void DeviceIO::WriteBytes(BYTE *buffer, DWORD len)
{
    UINT64 endingPos = pos + len;
    if ((pos & 0x1FF) == 0 && (len & 0x1FF) == 0)
    {
#ifdef _WIN32
        bool success = WriteFile(
                    impl->deviceHandle,     // Device to read from
                    buffer,                 // Output buffer
                    len,                    // Length to read
                    NULL,                   // Pointer to the number of bytes read
                    &impl->offset);         // OVERLAPPED structure containing the offset to read from

        if (!success)
            throw std::string("DeviceIO: Error writing to the device, may be disconnected.\n");
#else
        write(impl->device, buffer, len);
#endif

        SetPosition(endingPos);

        return;
    }

    // nothing to do
    if (len == 0)
        return;

    // We can't Write beyond the end of the stream
    if (pos + len > Length())
        throw std::string("DeviceIO: Cannot Write beyond the end of the stream.\n");


    UINT64 originalPos = pos;
    UINT64 currentSector = DOWN_TO_NEAREST_SECTOR(pos);

    // Write the bytes up to the next sector
    SetPosition(currentSector);
    ReadBytes(lastReadData, FAT_SECTOR_SIZE);
    lastReadOffset = currentSector;
    WORD bytesLeftInSector = FAT_SECTOR_SIZE - (originalPos - currentSector);
    WORD bytesToWrite = (len >= bytesLeftInSector) ? bytesLeftInSector : len;
    memcpy(lastReadData + (originalPos - currentSector), buffer, bytesToWrite);

    // Write the actual data
    SetPosition(currentSector);
#ifdef _WIN32
    DWORD bytesWritten;

    WriteFile(
        impl->deviceHandle,   // Device to read from
        lastReadData,         // Data to Write
        FAT_SECTOR_SIZE,                // Amount of data to Write
        &bytesWritten,        // Pointer to number of bytes written
        &impl->offset);       // OVERLAPPED structure containing the offset to Write from
#else
    write(impl->device, lastReadData, FAT_SECTOR_SIZE);
#endif

    // update the values
    buffer += bytesToWrite;
    currentSector += FAT_SECTOR_SIZE;
    len -= bytesToWrite;

    // Write the rest of the data
    while (len > 0)
    {
        SetPosition(currentSector);
        lastReadOffset = DOWN_TO_NEAREST_SECTOR(pos);
        ReadBytes(lastReadData, FAT_SECTOR_SIZE);
        bytesToWrite = (len >= FAT_SECTOR_SIZE) ? FAT_SECTOR_SIZE : len;
        memcpy(lastReadData, buffer, bytesToWrite);

        // go to the top of the sector
        SetPosition(currentSector);

        // update values
        buffer += bytesToWrite;
        currentSector += FAT_SECTOR_SIZE;
        len -= bytesToWrite;

#ifdef _WIN32
        WriteFile(
            impl->deviceHandle,   // Device to read from
            lastReadData,         // Data to Write
            FAT_SECTOR_SIZE,                // Amount of data to Write
            &bytesWritten,        // Pointer to number of bytes written
            &impl->offset);       // OVERLAPPED structure containing the offset to Write from
#else
        write(impl->device, buffer, FAT_SECTOR_SIZE);
#endif
    }

    // set the position
    SetPosition(endingPos);
}

UINT64 DeviceIO::Length()
{
    UINT64 length;
#ifdef _WIN32
    DISK_GEOMETRY geometry;
    DWORD bytesReturned;

    memset(&geometry, 0, sizeof(DISK_GEOMETRY));
    DeviceIoControl(
        impl->deviceHandle,			// Device to get the geometry from
        IOCTL_DISK_GET_DRIVE_GEOMETRY,	// Action we're taking (getting the geometry)
        NULL,							// Not used since this requires no input data
        0,								// No data
        &geometry,						// Output struct
        sizeof(DISK_GEOMETRY),			// Output buffer size
        &bytesReturned,					// Number of bytes returned
        NULL);							// Not used

    UINT64 cylinders = (UINT64)((UINT64)geometry.Cylinders.HighPart << 32) |
            geometry.Cylinders.LowPart; // Convert the BIG_INTEGER to UINT64
    length = cylinders * (UINT64)geometry.TracksPerCylinder	* (UINT64)geometry.SectorsPerTrack *
             (UINT64)geometry.BytesPerSector;
#else
    UINT64 numberOfSectors = 0;
    int device = impl->device;

    ioctl(device, SECTOR_COUNT, &numberOfSectors);

    UINT64 sectorSize = 0;
    ioctl(device, SECTOR_SIZE, &sectorSize);

    // calculate the length in bytes
    length = (UINT64)numberOfSectors * (UINT64)sectorSize;
#endif

    return length;
}

void DeviceIO::SetPosition(UINT64 address, std::ios_base::seek_dir dir)
{
    if (dir != std::ios_base::beg)
        throw std::string("DeviceIO: Unsupported seek direction\n");

    pos = address;
    address = DOWN_TO_NEAREST_SECTOR(address); // Round the position down to the nearest sector offset

#ifdef _WIN32
    impl->offset.Offset = (DWORD)address;
    impl->offset.OffsetHigh = (DWORD)(address >> 32);
#else
    impl->offset = address;
#endif

#ifdef __linux__
    lseek64(impl->device, address, SEEK_SET);
#else
#ifdef _WIN32
    LONG addressHigh = impl->offset.OffsetHigh;
    SetFilePointer(impl->deviceHandle, impl->offset.Offset, &addressHigh, FILE_BEGIN);
#else
    lseek(impl->device, address, SEEK_SET);
#endif
#endif
}

UINT64 DeviceIO::realPosition()
{
#ifdef _WIN32
    return (UINT64)(((UINT64)impl->offset.OffsetHigh << 32) | impl->offset.Offset);
#else
    return impl->offset;
#endif
}

UINT64 DeviceIO::GetPosition()
{
    return pos;
}

void DeviceIO::Close()
{
#if defined _WIN32
    if (impl->deviceHandle != INVALID_HANDLE_VALUE)
        CloseHandle(impl->deviceHandle);
#else
    close(impl->device);
    impl->device = -1;
#endif
}

void DeviceIO::loadDevice(std::wstring devicePath)
{
#ifdef _WIN32
    pos = 0;
    memset(&impl->offset, 0, sizeof(OVERLAPPED));

    // Attempt to get a handle to the device
    impl->deviceHandle = CreateFile(
                devicePath.c_str(),// File name (device path)
                GENERIC_READ | GENERIC_WRITE,		// Read/Write access
                FILE_SHARE_READ | FILE_SHARE_WRITE,	// Read/Write share
                NULL,								// Not usedC
                OPEN_EXISTING,						// Open the existing device -- fails if it's fucked
                FILE_FLAG_NO_BUFFERING | FILE_FLAG_WRITE_THROUGH | FILE_ATTRIBUTE_DEVICE,	// Flags and attributes
                NULL);								// Ignored

    if (impl->deviceHandle == INVALID_HANDLE_VALUE)
        throw std::string("DeviceIO: Could not open HANDLE for device.\n");
#else

    // need to convert this into a regular string, since open takes a char*
    std::string tempPath(devicePath.begin(), devicePath.end());

    // Open the device
    impl->device = open(tempPath.c_str(), O_RDWR);
    if (impl->device == -1)
        throw std::string("DeviceIO: Error opening device.\n" + std::string(strerror(errno)));
#endif
}

void DeviceIO::Flush()
{
#ifdef __WIN32
    FlushFileBuffers(impl->deviceHandle);
#endif
}
