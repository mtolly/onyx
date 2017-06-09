/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#ifndef FATXCONSTANTS_H
#define FATXCONSTANTS_H

#include "../winnames.h"

#include "../Stfs/StfsDefinitions.h"

#include <vector>
#include <iostream>

#define FAT32 4
#define FAT16 2

#define FATX_MAGIC 0x58544146

#define FATX_ENTRY_SIZE 0x40
#define FATX_ENTRY_MAX_NAME_LENGTH 0x2A
#define FATX_ENTRY_DELETED 0xE5

#define FAT_SECTOR_SIZE 0x200

#define FAT_CLUSTER_AVAILABLE (DWORD)0x00000000
#define FAT_CLUSTER_RESERVED (DWORD)0xfffffff0
#define FAT_CLUSTER_LAST (DWORD)0xffffffff

#define FAT_CLUSTER16_AVAILABLE (WORD)0x0000
#define FAT_CLUSTER16_RESERVED (WORD)0xfff0
#define FAT_CLUSTER16_LAST (WORD)0xffff

class FatxDrive;
struct Partition;

enum FatxDriveType
{
    FatxFlashDrive,
    FatxHarddrive
};

struct SecurityInfo
{
    std::string serialNumber;
    std::string firmwareRevision;
    std::string modelNumber;
    BYTE msLogoHash[0x14];
    DWORD userAddressableSectors;
    BYTE rsaSignature[0x100];
    bool validSignature;

    DWORD msLogoSize;
    BYTE *msLogo;
};

struct FlashDriveConfigurationData
{
    // type1
    Certificate certificate;

    // type2
    BYTE deviceSignature[0x100];

    BYTE deviceID[0x14];
    DWORD securityLength; // must be 0x228 or 0x100
    UINT64 deviceLength;
    WORD readSpeed;
    WORD WriteSpeed;
};

struct FatxFileEntry
{
    Partition *partition;

    BYTE nameLen;
    BYTE fileAttributes;

    std::string name;
    DWORD startingCluster;
    DWORD fileSize;

    // times
    DWORD creationDate;
    DWORD lastWriteDate;
    DWORD lastAccessDate;

    bool readDirectories;
    INT64 address;
    DWORD magic;
    std::vector<FatxFileEntry> cachedFiles;
    std::vector<DWORD> clusterChain;
    std::string path;
};

struct Partition
{
    std::string name;
    INT64 address;
    UINT64 size;
    FatxDrive *drive;

    // header
    DWORD magic;
    DWORD partitionId;
    DWORD sectorsPerCluster;
    DWORD rootDirectoryCluster;

    // partition entries
    FatxFileEntry root;

    // calculated information
    DWORD lastFreeClusterFound;
    BYTE clusterEntrySize;
    DWORD clusterCount;
    DWORD clusterSize;
    UINT64 clusterStartingAddress;
    DWORD fatEntryShift;
    UINT64 allocationTableSize;
    UINT64 freeMemory;
    std::vector<DWORD> freeClusters;
};

enum FatxDirentAttributes
{
    FatxReadOnly = 0x01,
    FatxHidden = 0x02,
    FatxSystem = 0x04,
    FatxDirectory = 0x10,
    FatxArchive = 0x20,
    FatxDevice = 0x40,
    FatxNormal = 0x80
};

struct HddSizes
{
    static const INT64 SystemCache = 0x80000000;
    static const INT64 GameCache = 0xA0E30000;
    static const INT64 SystemPartition = 0x10000000;
    static const INT64 SystemAuxiliary = 0xCE30000;
    static const INT64 SystemExtended = 0x8000000;
};

struct HddOffsets
{
    static const INT64 Data = 0x130EB0000;
    static const INT64 Josh = 0x800;
    static const INT64 SecuritySector = 0x2000;
    static const INT64 SystemCache = 0x80000;
    static const INT64 GameCache = 0x80080000;
    static const INT64 SystemAuxiliary = 0x10C080000;
    static const INT64 SystemExtended = 0x118EB0000;
    static const INT64 SystemPartition = 0x120EB0000;
};

struct UsbOffsets
{
    static const INT64 SystemAuxiliary = 0x8115200;
    static const INT64 SystemExtended = 0x12000400;
    static const INT64 SystemCache = 0x8000400;
    static const INT64 Data = 0x20000000;
};

struct UsbSizes
{
    static const INT64 CacheNoSystem = 0x4000000;
    static const INT64 SystemCache = 0x47FF000;
    static const INT64 SystemAuxiliary = 0x8000000;
    static const INT64 SystemExtended = 0xDFFFC00;
};

#endif // FATXCONSTANTS_H
