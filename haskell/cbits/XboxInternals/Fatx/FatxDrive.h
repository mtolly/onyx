/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#ifndef FATXDRIVE_H
#define FATXDRIVE_H

#include "FatxConstants.h"

#include "../Stfs/XContentHeader.h"
#include "../IO/DeviceIO.h"
#include "../IO/FatxIO.h"
#include "../IO/MemoryIO.h"
#include "../IO/MultiFileIO.h"
#include "../Cryptography/XeKeys.h"
#include "../Cryptography/XeCrypt.h"

#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <cmath>

class XBOXINTERNALSSHARED_EXPORT FatxDrive
{
public:
    FatxDrive(BaseIO *io, FatxDriveType type);
    #ifdef __WIN32
    FatxDrive(void* deviceHandle, FatxDriveType type = FatxHarddrive);
    #endif
    FatxDrive(std::string drivePath, FatxDriveType type = FatxHarddrive);
    FatxDrive(std::wstring drivePath, FatxDriveType type = FatxHarddrive);
    ~FatxDrive();

    // get the drives partitions
    std::vector<Partition*> GetPartitions();

    // get the drive type
    FatxDriveType GetFatxDriveType();

    // get a FatxIO for the given entry
    FatxIO GetFatxIO(FatxFileEntry *entry);

    // populate entry's cachedFiles vector (only if it's a directory)
    void GetChildFileEntries(FatxFileEntry *entry, void(*progress)(void*, bool) = NULL,
            void *arg = NULL);

    // populate entry's clusterChain with its cluster chain
    void ReadClusterChain(FatxFileEntry *entry);

    // save the security blob to local disk
    void ExtractSecurityBlob(std::string path);

    // replace the existing security blob
    void ReplaceSecurityBlob(std::string path);

    // create a new file
    void CreateFileX(FatxFileEntry *parent, std::string name);

    // Writes the a folder named 'folderName', in the 'parent' folder
    FatxFileEntry* CreateFolder(FatxFileEntry *parent, std::string folderName);

    // creates the specified path (even if multiple folders don't exist in it)
    FatxFileEntry *CreatePath(std::string folderPath);

    // get the first 4 bytes of a file
    void GetFileEntryMagic(FatxFileEntry *entry);

    // deletes the entry and all of it's children
    void RemoveFile(FatxFileEntry *entry, void(*progress)(void*) = NULL, void *arg = NULL);

    // inject the file
    void InjectFile(FatxFileEntry *parent, std::string name, std::string filePath,
            void(*progress)(void*, DWORD, DWORD) = NULL, void *arg = NULL);

    // determines if a file at the specified path exists
    bool FileExists(std::string filePath);

    // determines if a file in the specified folder exists
    bool FileExists(FatxFileEntry *folder, std::string fileName, bool checkDeleted = false);

    // get the FatxFileEntry from its path
    FatxFileEntry* GetFileEntry(std::string filePath);

    // sets the drive name
    void SetDriveName(std::wstring name);

    // close the underlying io
    void Close();

    // Write the entire contents of the drive to the local disk
    void CreateBackup(std::string outPath, void(*progress)(void*, DWORD, DWORD) = NULL,
            void *arg = NULL);

    // re-Write the entire contents of the drive using a backup from the local disk
    void RestoreFromBackup(std::string backupPath, void(*progress)(void*, DWORD, DWORD) = NULL,
            void *arg = NULL);

    // get the amount of free bytes on the device
    UINT64 GetFreeMemory(Partition *part, void(*progress)(void*, bool) = NULL, void *arg = NULL);

    // reload the entire drive, called after restoring
    void ReloadDrive();

    // convert a cluster to an offset
    static INT64 ClusterToOffset(Partition *part, DWORD cluster);

    // check to see whether or not a file name is valid
    static bool ValidFileName(std::string fileName);

    // format recovery version, found by Eaton (only on dev kit drives)
    Version lastFormatRecoveryVersion;

    // security sector information (only on hard drives)
    SecurityInfo securityBlob;

    // configuration data (only in flash drives)
    FlashDriveConfigurationData configurationData;

private:
    // Writes the 'newEntry' to disk, in the 'parent' folder
    FatxFileEntry* createFileEntry(FatxFileEntry *parent, FatxFileEntry *newEntry,
            bool errorIfAlreadyExists = true);

    // open up a physical drive
    void loadFatxDrive(std::wstring drivePath);

    // open up a physical drive
    #ifdef __WIN32
    void loadFatxDrive(void* deviceHandle);
    #endif

    // open up a physical drive
    void loadFatxDrive();

    // process a partition and load it with calulated information
    void processBootSector(Partition *part);

    // load all the profiles on the device
    void loadProfiles();

    // inject a range of clustes into the chain
    void injectRange(vector<DWORD> &clusters, Range &range);

    // counts the largest amount of consecutive unset bits
    static BYTE cntlzw(DWORD x);

    // check to see if a certain character is allowed as a file name
    static bool validFileChar(char c);

    BaseIO *io;
    std::vector<Partition*> partitions;
    std::vector<FatxFileEntry*> profiles;
    FatxDriveType type;
};

#endif // FATXDRIVE_H
