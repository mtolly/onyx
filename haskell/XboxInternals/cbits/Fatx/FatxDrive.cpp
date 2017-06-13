/* Most parts of this class were originally developed by Lander Griffith (https://github.com/landr0id/).
   Much of his code is used throughout this class or very slightly modified */

#include "FatxDrive.h"

#ifdef _WIN32
#include <Windows.h>
#undef DeleteFile
#undef ReplaceFile
#else
#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif

FatxDrive::FatxDrive(std::string drivePath, FatxDriveType type)  : type(type)
{
    // convert it to a wstring
    std::wstring wsDrivePath;
    wsDrivePath.assign(drivePath.begin(), drivePath.end());

    // load the device
    loadFatxDrive(wsDrivePath);
}

FatxDrive::FatxDrive(BaseIO *io, FatxDriveType type) : io(io), type(type)
{
    loadFatxDrive();
}

FatxDrive::FatxDrive(std::wstring drivePath, FatxDriveType type) : type(type)
{
    loadFatxDrive(drivePath);
}

#ifdef __WIN32
FatxDrive::FatxDrive(void* deviceHandle, FatxDriveType type) : type(type)
{
    loadFatxDrive(deviceHandle);
}
#endif

std::vector<Partition*> FatxDrive::GetPartitions()
{
    return partitions;
}

FatxDriveType FatxDrive::GetFatxDriveType()
{
    return type;
}

FatxIO FatxDrive::GetFatxIO(FatxFileEntry *entry)
{
    // only if it hasn't been read yet
    if (entry->clusterChain.size() == 0)
        ReadClusterChain(entry);

    return FatxIO(static_cast<DeviceIO*>(io), entry);
}

void FatxDrive::processBootSector(Partition *part)
{
    UINT64 partitionSize = part->size;

    // seek to the partition
    io->SetPosition(part->address);

    // read the header
    part->magic = io->ReadDword();
    if (part->magic != FATX_MAGIC)
    {
        part->address = 0;
        return;
    }

    part->partitionId = io->ReadDword();
    part->sectorsPerCluster = io->ReadDword();
    part->rootDirectoryCluster = io->ReadDword();

    switch(part->sectorsPerCluster)
    {
        case 0x2:
        case 0x4:
        case 0x8:
        case 0x10:
        case 0x20:
        case 0x40:
        case 0x80:
            break;
        default:
            throw std::string("FATX: Found invalid sectors per cluster.\n");
    }

    part->clusterSize = part->sectorsPerCluster << 9;
    BYTE consecutiveZeroes = cntlzw(part->clusterSize);
    int shiftFactor = 0x1F - consecutiveZeroes;

    partitionSize >>= shiftFactor;
    partitionSize++;

    if ((this->type == FatxFlashDrive && part->address == UsbOffsets::Data) ||
            partitionSize >= FAT_CLUSTER16_RESERVED)
        part->fatEntryShift = 2;
    else
        part->fatEntryShift = 1;

    partitionSize <<= part->fatEntryShift;
    partitionSize += 0x1000;
    partitionSize--;

    UINT64 clusters = part->size;
    clusters -= 0x1000;

    partitionSize &= ~0xFFF;
    partitionSize &= 0xFFFFFFFF;

    if (clusters < partitionSize)
        throw std::string("FATX: Volume too small to hold the FAT.\n");

    clusters -= partitionSize;
    clusters >>= (shiftFactor & 0xFFFFFFFFFFFFFF);
    if (clusters > 0xFFFFFFF)
        throw std::string("FATX: Too many clusters.\n");

    part->clusterCount = clusters;
    part->allocationTableSize = partitionSize;
    part->clusterEntrySize = part->fatEntryShift * 2;
    part->clusterStartingAddress = part->address + (INT64)partitionSize + 0x1000;
    part->lastFreeClusterFound = 1;
    part->freeMemory = 0;

    // setup the root
    part->root.startingCluster = part->rootDirectoryCluster;
    part->root.readDirectories = false;
    part->root.name = part->name;
    part->root.partition = part;
    part->root.fileAttributes = FatxDirectory;
    part->root.address = -1;
    part->root.path = "Drive:\\";
    part->drive = this;
}

void FatxDrive::ExtractSecurityBlob(string path)
{
    INT64 originalPos = io->GetPosition();

    // create a file to save to
    FileIO outFile(path, true);

    // seek to the beginning of the security blob
    io->SetPosition(0x2000);

    // allocate memory for the data
    BYTE *buffer = new BYTE[securityBlob.msLogoSize + 0x204];

    // read in the data
    io->ReadBytes(buffer, securityBlob.msLogoSize + 0x204);
    outFile.WriteBytes(buffer, securityBlob.msLogoSize + 0x204);
    outFile.Flush();

    // cleanup
    delete[] buffer;
    outFile.Close();
    io->SetPosition(originalPos);
}

void FatxDrive::ReplaceSecurityBlob(std::string path)
{
    INT64 originalPos = io->GetPosition();

    // open the security blob
    FileIO inFile(path);

    // verify the file size
    if (inFile.Length() != (securityBlob.msLogoSize + 0x204))
        throw std::string("FATX: Invalid security blob, size mismatch.\n");

    // seek to the beginning of the security blob
    io->SetPosition(0x2000);

    // allocate memory for the data
    BYTE *buffer = new BYTE[securityBlob.msLogoSize + 0x204];
    inFile.SetPosition(0);
    inFile.ReadBytes(buffer, securityBlob.msLogoSize + 0x204);

    io->WriteBytes(buffer, securityBlob.msLogoSize + 0x204);
    io->Flush();

    // cleanup
    delete[] buffer;
    inFile.Close();
    io->SetPosition(originalPos);
}

void FatxDrive::CreateFileX(FatxFileEntry *parent, std::string name)
{
    FatxFileEntry newEntry;
    newEntry.name = name;
    newEntry.fileSize = 0;

    createFileEntry(parent, &newEntry);
}

FatxFileEntry* FatxDrive::createFileEntry(FatxFileEntry *parent, FatxFileEntry *newEntry,
        bool errorIfAlreadyExists)
{
    if (!(parent->fileAttributes & FatxDirectory))
        throw std::string("FATX: Parent file entry is not a directory.\n");

    newEntry->path = parent->path + parent->name + "\\";

    // set the times
    DWORD currentTime = MSTimeToDWORD(TimetToMSTime(time(NULL)));
    newEntry->creationDate = currentTime;
    newEntry->lastAccessDate = currentTime;
    newEntry->lastWriteDate = currentTime;

    // set the address to null
    newEntry->address = 0;
    newEntry->startingCluster = 0;

    // get the child entries
    GetChildFileEntries(parent);

    for (size_t i = 0; i < parent->cachedFiles.size(); i++)
        if (parent->cachedFiles.at(i).name == newEntry->name)
        {
            // if it's deleted, it's okay!
            if (parent->cachedFiles.at(i).nameLen != FATX_ENTRY_DELETED)
            {
                if (errorIfAlreadyExists)
                    throw std::string("FATX: Entry already exists.\n");
                else
                    return NULL;
            }

            newEntry->address = parent->cachedFiles.at(i).address;
        }

    // set the name length
    newEntry->nameLen = newEntry->name.length();

    if (newEntry->address == 0)
    {
        UINT64 freeEntryAddress = parent->cachedFiles.size() * FATX_ENTRY_SIZE;
        FatxIO parentIO = GetFatxIO(parent);

        parent->fileSize = parent->cachedFiles.size() * FATX_ENTRY_SIZE;
        if (parent->clusterChain.size() < 1 || parent->fileSize >= parent->partition->clusterSize)
        {
            parentIO.AllocateMemory(FATX_ENTRY_SIZE);
            parentIO.SetPosition(parent->fileSize);
            parentIO.Write((BYTE)0xFF);
        }

        parentIO.SetPosition(freeEntryAddress);
        newEntry->address = parentIO.GetDrivePosition();
    }

    newEntry->partition = parent->partition;

    DWORD fileSize = newEntry->fileSize;
    newEntry->fileSize = 0;

    FatxIO childIO(static_cast<DeviceIO*>(io), newEntry);
    childIO.AllocateMemory(fileSize);
    childIO.WriteEntryToDisk();

    parent->cachedFiles.push_back(*newEntry);
    return &parent->cachedFiles.at(parent->cachedFiles.size() - 1);
}

FatxFileEntry* FatxDrive::CreateFolder(FatxFileEntry *parent, std::string folderName)
{
    if (this->FileExists(parent, folderName))
        throw std::string("FATX: The folder already exists.");

    FatxFileEntry newEntry;
    newEntry.fileSize = FATX_ENTRY_SIZE;
    newEntry.name = folderName;
    newEntry.fileAttributes = FatxDirectory;

    return this->createFileEntry(parent, &newEntry);
}

FatxFileEntry* FatxDrive::CreatePath(std::string folderPath)
{
    FatxFileEntry *lastEntry = GetFileEntry(folderPath);
    if (lastEntry != NULL)
        return lastEntry;

    std::vector<std::string> elems;
    std::stringstream ss(folderPath);
    std::string item;
    while (std::getline(ss, item, '\\'))
        elems.push_back(item);

    if (elems.size() < 3)
        throw std::string("FATX: Invalid folder path given.");

    std::string currentPath = elems.at(0) + "\\" + elems.at(1);

    for (size_t i = 2; i < elems.size(); i++)
    {
        FatxFileEntry newEntry;
        newEntry.fileSize = FATX_ENTRY_SIZE;
        newEntry.name = elems.at(i);
        newEntry.fileAttributes = FatxDirectory;

        lastEntry = this->createFileEntry(GetFileEntry(currentPath), &newEntry, false);
        currentPath += "\\" + elems.at(i);
    }

    return lastEntry;
}

void FatxDrive::RemoveFile(FatxFileEntry *entry, void(*progress)(void*), void *arg)
{
    // read the data
    GetChildFileEntries(entry);
    ReadClusterChain(entry);

    for (size_t i = 0; i < entry->cachedFiles.size(); i++)
        RemoveFile(&entry->cachedFiles.at(i), progress, arg);

    // set all the clusters to available
    entry->clusterChain.push_back(entry->startingCluster);
    FatxIO::SetAllClusters(static_cast<DeviceIO*>(io), entry->partition, entry->clusterChain,
            FAT_CLUSTER_AVAILABLE);

    // generate cluster ranges for fast insertion into the cluster chain
    std::vector<Range> clusterRanges;
    FatxIO::GetConsecutive(entry->clusterChain, clusterRanges, true);

    // inject all of the ranges back into the cluster chain
    for (DWORD i = 1; i < clusterRanges.size(); i++)
    {
        clusterRanges.at(i).start = entry->clusterChain.at(clusterRanges.at(i).start);
        injectRange(entry->partition->freeClusters, clusterRanges.at(i));
    }

    // update the entry
    entry->clusterChain.clear();
    entry->nameLen = FATX_ENTRY_DELETED;

    // update the entry file name lenght to deleted
    io->SetPosition(entry->address);
    io->Write((BYTE)FATX_ENTRY_DELETED);

    if (progress)
        progress(arg);
}

void FatxDrive::InjectFile(FatxFileEntry *parent, std::string name, std::string filePath,
        void (*progress)(void *, DWORD, DWORD), void *arg)
{
    UINT64 fileLength = 0;

#if __WIN32
    // TODO: put windows file length code here
#else
    struct stat64 fileInfo;
    if (stat64(filePath.c_str(), &fileInfo) != 0)
        throw std::string("FATX: Error opening the file.\n");
    fileLength = fileInfo.st_size;
#endif

    if (fileLength >= 4294967296)
        throw std::string("FATX: File too large. All files in this file system must be less than 4GB.\n");

    FatxFileEntry entry;
    entry.name = name;

    // seek to the end of the file
    FileIO inFile(filePath);
    inFile.SetPosition(0, ios_base::end);

    // get the file size
    UINT64 fileSize = inFile.GetPosition();
    entry.fileSize = fileSize;

    // set other stuff
    entry.fileAttributes = 0;
    inFile.SetPosition(0);
    entry.magic = inFile.ReadDword();
    inFile.Close();

    // create the entry
    createFileEntry(parent, &entry);

    FatxIO fatxIO = GetFatxIO(&entry);
    fatxIO.ReplaceFile(filePath, progress, arg);
}

void FatxDrive::GetFileEntryMagic(FatxFileEntry *entry)
{
    if (entry->fileSize < 4 || entry->magic != 0)
        return;

    io->SetPosition(FatxIO::ClusterToOffset(entry->partition, entry->startingCluster));
    entry->magic = io->ReadDword();
}

void FatxDrive::GetChildFileEntries(FatxFileEntry *entry, void(*progress)(void*, bool), void *arg)
{
    // if all entries have been read, skip this
    if (entry->readDirectories || !(entry->fileAttributes & FatxDirectory))
        return;

    // if the cluster chain has already been read, skip this
    if (entry->clusterChain.size() == 0)
        ReadClusterChain(entry);

    // find out how many entries are in a single cluster
    DWORD entriesInCluster = entry->partition->clusterSize / FATX_ENTRY_SIZE;

    bool doneForGood = false;

    // read all entries
    for (size_t i = 0; i < entry->clusterChain.size(); i++)
    {
        UINT64 posCur = FatxIO::ClusterToOffset(entry->partition, entry->clusterChain.at(i));

        // go to the cluster offset
        io->SetPosition(posCur);

        for (DWORD x = 0; x < entriesInCluster; x++)
        {
            // read the name length
            FatxFileEntry newEntry;
            newEntry.nameLen = io->ReadByte();

            // check if there are no more entries
            if (newEntry.nameLen == 0xFF || newEntry.nameLen == 0)
            {
                doneForGood = true;
                break;
            }

            // calcualte the address
            newEntry.address = posCur + (x * 0x40);

            // read the attributes
            newEntry.fileAttributes = io->ReadByte();

            // read the name (0xFF is the null terminator)
            bool subtract = true;
            if (newEntry.nameLen == FATX_ENTRY_DELETED)
                newEntry.name = io->ReadString(-1, 0xFF, true, FATX_ENTRY_MAX_NAME_LENGTH);
            else
            {
                newEntry.name = io->ReadString(newEntry.nameLen);
                subtract = false;
            }

            // seek past the name
            io->SetPosition(io->GetPosition() + (FATX_ENTRY_MAX_NAME_LENGTH - newEntry.name.length()) -
                    subtract);

            // read the rest of the entry information
            newEntry.startingCluster = io->ReadDword();
            if (newEntry.startingCluster == entry->startingCluster)
                throw std::string("FATX: FAT has circular link.\n");

            newEntry.fileSize = io->ReadDword();
            newEntry.creationDate = io->ReadDword();
            newEntry.lastWriteDate = io->ReadDword();
            newEntry.lastAccessDate = io->ReadDword();
            newEntry.partition = entry->partition;
            newEntry.readDirectories = false;
            newEntry.path = entry->path + entry->name + "\\";
            newEntry.magic = 0;

            // add it to the file cache
            entry->cachedFiles.push_back(newEntry);

            // update progress if needed
            if (progress)
                progress(arg, false);
        }

        if (doneForGood)
            break;
    }

    // update progress if needed
    if (progress)
        progress(arg, true);

    entry->fileSize = (entry->cachedFiles.size() * FATX_ENTRY_SIZE);
    entry->readDirectories = true;
}

void FatxDrive::ReadClusterChain(FatxFileEntry *entry)
{
    // clear the current chain
    entry->clusterChain.clear();

    // calculate values
    bool clusterSizeIs2 = (entry->partition->clusterEntrySize == FAT16);
    DWORD lastCluster =  (clusterSizeIs2) ? FAT_CLUSTER16_LAST : FAT_CLUSTER_LAST;
    DWORD availableCluster = (clusterSizeIs2) ? FAT_CLUSTER16_AVAILABLE : FAT_CLUSTER_AVAILABLE;
    INT64 clusterMapAddress = entry->partition->address + 0x1000;

    // start with the starting cluster
    DWORD previousCluster = entry->startingCluster;

    while (previousCluster != lastCluster && previousCluster != availableCluster)
    {
        // add it to the cluster chain
        entry->clusterChain.push_back(previousCluster);

        // seek to the next cluster
        io->SetPosition(clusterMapAddress + (previousCluster * entry->partition->clusterEntrySize));

        // read the cluster
        if (clusterSizeIs2)
            previousCluster = io->ReadWord();
        else
            previousCluster = io->ReadDword();
    }
}

void FatxDrive::Close()
{
    io->Close();
}

void FatxDrive::CreateBackup(std::string outPath, void (*progress)(void *, DWORD, DWORD), void *arg)
{
    // create a file on the local disk to store the backup
    FileIO outBackup(outPath, true);

    UINT64 driveLen = io->Length();
    io->SetPosition(0);

    BYTE *buffer = new BYTE[0x100000];

    // start writing the drive contents to the local disk
    DWORD i = 0;
    UINT64 totalProgress = driveLen / 0x100000;
    while (driveLen >= 0x100000)
    {
        io->ReadBytes(buffer, 0x100000);
        outBackup.WriteBytes(buffer, 0x100000);

        if (progress)
            progress(arg, i, totalProgress);

        driveLen -= 0x100000;
        i++;
    }

    if (driveLen > 0)
    {
        // read the crap at the end
        io->ReadBytes(buffer, driveLen);
        outBackup.WriteBytes(buffer, driveLen);
    }

    if (progress)
        progress(arg, totalProgress, totalProgress);

    outBackup.Close();

    delete[] buffer;
}

void FatxDrive::RestoreFromBackup(std::string backupPath, void (*progress)(void *, DWORD, DWORD),
        void *arg)
{
    /* Here's the thing... fstream is trash. It will only handle files up to 2GB or 4GB,
       at least on my windows 7 machine. That's a huge problem because drive backups will
       most likely be a lot larger than that. SOOOOOO it looks like I'll have to use OS
       specific functions in order to get this to work properly. It makes more sense
       to just make the FileIO class use the OS specific functions to begin with, but
       the ReadFile/WriteFile functions are slower than fstream functions, probably because
       they don't cache a bunch of stuff.
    */

    BYTE *buffer = new BYTE[0x100000];
    UINT64 bytesLeft;

#ifdef __WIN32
    std::wstring wBackupPath;
    wBackupPath.assign(backupPath.begin(), backupPath.end());
    HANDLE hFile = CreateFile(wBackupPath.c_str(), GENERIC_READ, NULL, NULL, OPEN_EXISTING,
            FILE_ATTRIBUTE_NORMAL, NULL);

    if (hFile == INVALID_HANDLE_VALUE)
        throw std::string("FATX: Could not open drive backup.");

    DWORD high;
    DWORD low = GetFileSize(hFile, &high);
    bytesLeft = ((UINT64)high << 32) | low;

    SetFilePointer(hFile, 0, 0, FILE_BEGIN);
#else
    struct stat sb;
    stat(backupPath.c_str(), &sb);
    bytesLeft = sb.st_size;

    int backupFile = open(backupPath.c_str(), O_RDWR);
    if (backupFile == -1)
        throw std::string("FATX: Could not open drive backup.");

    lseek(backupFile, 0, SEEK_SET);
#endif

    io->SetPosition(0);

    UINT64 totalProgress = bytesLeft / 0x100000;
    DWORD i = 0;
    while (bytesLeft >= 0x100000)
    {
#ifdef __WIN32
        high = (i * (UINT64)0x100000) >> 32;
        SetFilePointer(hFile, (i * (UINT64)0x100000) & 0xFFFFFFFF, (PLONG)&high, FILE_BEGIN);

        ReadFile(hFile, buffer, 0x100000, &high, NULL);
#else
        lseek(backupFile, (UINT64)i * (UINT64)0x100000, SEEK_SET);
        read(backupFile, buffer, 0x100000);
#endif
        io->WriteBytes(buffer, 0x100000);
        bytesLeft -= 0x100000;

        if (progress)
            progress(arg, i, totalProgress);
        i++;
    }

    if (bytesLeft > 0)
    {
#ifdef __WIN32
        SetFilePointer(hFile, (i * (UINT64)0x100000) & 0xFFFFFFFF, (PLONG)&high, FILE_BEGIN);
        ReadFile(hFile, buffer, bytesLeft, &high, NULL);
#else
        lseek(backupFile, (UINT64)i * (UINT64)0x100000, SEEK_SET);
        read(backupFile, buffer, bytesLeft);
#endif
        io->WriteBytes(buffer, bytesLeft);
    }

    if (progress)
        progress(arg, totalProgress, totalProgress);

#ifdef __WIN32
    CloseHandle(hFile);
#else
    close(backupFile);
#endif
    delete[] buffer;

    // reload the entire drive
    ReloadDrive();
}

BYTE FatxDrive::cntlzw(DWORD x)
{
    if (x == 0)
        return 0;

    DWORD t = 0x80000000;
    BYTE largestCount = 0, currentCount = 0;
    BYTE n = 0;

    while (n < 32)
    {
        bool bitSet = (x & t) >> (31 - n);
        if (bitSet)
        {
            if (currentCount > largestCount)
                largestCount = currentCount;
            currentCount = 0;
        }
        else
            currentCount++;

        t >>= 1;
        n++;
    }

    if (currentCount > largestCount)
        largestCount = currentCount;

    return largestCount;
}

bool FatxDrive::validFileChar(char c)
{
    if (isalpha(c) || isdigit(c))
        return true;

    switch (c)
    {
        case '!':
        case '#':
        case '$':
        case '%':
        case '&':
        case '\'':
        case '(':
        case ')':
        case '-':
        case '.':
        case '@':
        case '[':
        case ']':
        case '^':
        case '_':
        case '`':
        case '{':
        case '}':
        case '~':
        case ' ':
            return true;
        default:
            return false;
    }
}

FatxDrive::~FatxDrive()
{
    if (type == FatxHarddrive)
    {
        delete[] securityBlob.msLogo;
    }

    for (int i = 0, count = partitions.size(); i < count; i++)
    {
        delete partitions[i];
    }

    io->Close();
    delete io;
}

void FatxDrive::loadProfiles()
{
    // the general path for profiles is Drive:\Content\Content\OFFLINE_XUID\FFFE07D1\00010000\OFFLINE_XUID

    FatxFileEntry *contentRoot = NULL;

    // get the content partition's root
    for (DWORD i = 0; i < partitions.size(); i++)
    {
        if (partitions.at(i)->name == "Content")
            contentRoot = &partitions.at(i)->root;
    }

    // if the content partition doesn't exist, then return
    if (contentRoot == NULL)
        return;

    // get the content folder in the content partition
    GetChildFileEntries(contentRoot);
}

void FatxDrive::injectRange(vector<DWORD> &clusters, Range &range)
{
    // do a binary search to find the position to inject the clusters
    int start = 0, end = clusters.size() - 1, pos = 0;
    while (start < end)
    {
        pos = (start + end) / 2;
        if (clusters.at(pos) < range.start)
            start = pos + 1;
        else if (clusters.at(pos) > range.start)
            end = pos - 1;
        else if (clusters.at(pos) == range.start)
            throw std::string("FATX: Error freeing cluster, cluster already free.\n");
    }

    // generate the range of clusters to inject
    vector<DWORD> sillyRange;
    for (DWORD i = 0; i < range.len; i++)
        sillyRange.push_back(range.start + i);

    // insert that range, yo
    clusters.insert(clusters.begin() + start, sillyRange.begin(), sillyRange.end());
}

void FatxDrive::loadFatxDrive(std::wstring drivePath)
{
    if (type == FatxHarddrive)
    {
        // open the device io
        io = new DeviceIO(drivePath);
    }
    else if (type == FatxFlashDrive)
    {
        std::vector<std::string> dataFiles;
        std::stringstream ss;
        for (int i = 0; i < 3; i++)
        {
            ss << "F:/Xbox360/Data000" << i;
            dataFiles.push_back(ss.str());
            ss.str(std::string());
        }

        io = new MultiFileIO(dataFiles);
    }

    loadFatxDrive();
}

#ifdef __WIN32
void FatxDrive::loadFatxDrive(void* deviceHandle)
{
    // open the device io
    io = new DeviceIO(deviceHandle);

    loadFatxDrive();
}
#endif

void FatxDrive::loadFatxDrive()
{
    // parse the security blob
    if (type == FatxHarddrive)
    {
        io->SetPosition(HddOffsets::SecuritySector);
        securityBlob.serialNumber = io->ReadString(0x14);
        securityBlob.firmwareRevision = io->ReadString(8);
        securityBlob.modelNumber = io->ReadString(0x28);
        io->ReadBytes(securityBlob.msLogoHash, 0x14);
        io->SetEndian(LittleEndian);
        securityBlob.userAddressableSectors = io->ReadDword();
        io->SetEndian(BigEndian);
        io->ReadBytes(securityBlob.rsaSignature, 0x100);
        securityBlob.validSignature = false;

        // TODO: verify the signature

        // seek to the next sector
        io->SetPosition(HddOffsets::SecuritySector + FAT_SECTOR_SIZE);
        securityBlob.msLogoSize = io->ReadDword();
        securityBlob.msLogo = new BYTE[securityBlob.msLogoSize];
        io->ReadBytes(securityBlob.msLogo, securityBlob.msLogoSize);

        // if it's a dev drive then we have to read the partition table
        io->SetPosition(0);

        // parse the version
        lastFormatRecoveryVersion.major = io->ReadWord();
        lastFormatRecoveryVersion.minor = io->ReadWord();
        lastFormatRecoveryVersion.build = io->ReadWord();
        lastFormatRecoveryVersion.revision = io->ReadWord();
    }
    else if (type == FatxFlashDrive)
    {
        io->SetPosition(0x228);

        // read the data after the security information
        io->ReadBytes(configurationData.deviceID, 0x14);
        configurationData.securityLength = io->ReadDword();
        configurationData.deviceLength = io->ReadUInt64();
        configurationData.readSpeed = io->ReadWord();
        configurationData.WriteSpeed = io->ReadWord();

        // check for type 1/2
        io->SetPosition(0);
        if (configurationData.securityLength == 0x228)
            ReadCertificateEx(&configurationData.certificate, io, 0);
        else if (configurationData.securityLength == 0x100)
            io->ReadBytes(configurationData.deviceSignature, 0x100);
        else
            throw std::string("FATX: Device contains invalid configuration data.\n");

        // set this so the next check fails
        lastFormatRecoveryVersion.major = 0;
    }

    // Eaton determined this was a version struct and figured out the minimum version
    if (lastFormatRecoveryVersion.major == 2 && lastFormatRecoveryVersion.build >= 1525 &&
            lastFormatRecoveryVersion.revision >= 1)
    {
        Partition *content = new Partition;
        content->address = (UINT64)io->ReadDword() * FAT_SECTOR_SIZE;
        content->size = (UINT64)io->ReadDword() * FAT_SECTOR_SIZE;
        content->name = "Content";

        Partition *dashboard = new Partition;
        dashboard->address = (UINT64)io->ReadDword() * FAT_SECTOR_SIZE;
        dashboard->size = (UINT64)io->ReadDword() * FAT_SECTOR_SIZE;
        dashboard->name = "Xbox 360 Dashboard";

        partitions.push_back(content);
        partitions.push_back(dashboard);
    }
    else
    {
        // system extended partition initialization
        Partition *systemExtended = new Partition;
        systemExtended->address = (type == FatxHarddrive) ? +HddOffsets::SystemExtended :
                +UsbOffsets::SystemExtended;
        systemExtended->size = (type == FatxHarddrive) ? +HddSizes::SystemExtended :
                +UsbSizes::SystemExtended;
        systemExtended->name = "System Extended";

        // system auxiliary partition initialization
        Partition *systemAuxiliary = new Partition;
        systemAuxiliary->address = (type == FatxHarddrive) ? +HddOffsets::SystemAuxiliary :
                +UsbOffsets::SystemAuxiliary;
        systemAuxiliary->size = (type == FatxHarddrive) ? +HddSizes::SystemAuxiliary :
                +UsbSizes::SystemAuxiliary;
        systemAuxiliary->name = "System Auxiliary";

        if (type == FatxHarddrive)
        {
            // system partition initialization
            Partition *systemPartition = new Partition;
            systemPartition->address = HddOffsets::SystemPartition;
            systemPartition->size = HddSizes::SystemPartition;
            systemPartition->name = "System Partition";
            this->partitions.push_back(systemPartition);
        }

        // system cache partition initialization
        Partition *systemCache = new Partition;
        systemCache->address = (type == FatxHarddrive) ? +HddOffsets::SystemCache :
                +UsbOffsets::SystemCache;
        systemCache->size = (type == FatxHarddrive) ? +HddSizes::SystemCache : +UsbSizes::SystemCache;
        systemCache->name = "System Cache";

        // content partition initialization
        Partition *content = new Partition;
        content->address = (type == FatxHarddrive) ? +HddOffsets::Data : +UsbOffsets::Data;
        content->size = io->Length() - content->address;
        content->name = "Content";

        // add the partitions to the vector
        this->partitions.push_back(systemExtended);
        this->partitions.push_back(systemAuxiliary);
        this->partitions.push_back(systemCache);
        this->partitions.push_back(content);
    }

    // process all bootsectors
    for (size_t i = 0; i < this->partitions.size(); )
    {
        processBootSector(this->partitions.at(i));

        // if there's invalid magic
        if (this->partitions.at(i)->address == 0)
        {
            delete partitions.at(i);
            partitions.erase(partitions.begin() + i);
        }
        else
            i++;
    }
}

UINT64 FatxDrive::GetFreeMemory(Partition *part, void(*progress)(void*, bool), void *arg)
{
    if (part->freeMemory != 0)
        return (UINT64)part->freeClusters.size() * (UINT64)part->clusterSize;

    // allocate memory for a buffer to minimize the amount of reads
    BYTE *buffer = new BYTE[0x50000];

    // seek to the chainmap
    io->SetPosition(part->address + 0x1000);

    // check if it's FAT16
    bool clusterSizeIs2 = (part->clusterEntrySize == FAT16);

    UINT64 bytesLeft = (UINT64)part->clusterCount * (UINT64)part->clusterEntrySize;
    DWORD readSize;
    DWORD x = 0;
    while (bytesLeft > 0)
    {
        // calculate the read size
        readSize = (bytesLeft > 0x50000) ? 0x50000 : bytesLeft;
        bytesLeft -= readSize;

        // read in the segment
        io->ReadBytes(buffer, readSize);
        MemoryIO memory(buffer, 0x50000);
        memory.SetPosition(0);

        // update progress if needed
        if (progress)
            progress(arg, false);

        // iterate through all of the clusters
        if (clusterSizeIs2)
        {
            for (DWORD i = 0; i < (readSize / 2); i++)
            {
                if (memory.ReadWord() == FAT_CLUSTER16_AVAILABLE)
                    part->freeClusters.push_back(x + i);
            }
            x += 0x28000;
        }
        else
        {
            for (DWORD i = 0; i < (readSize / 4); i++)
            {
                DWORD test = memory.ReadDword();
                if (test == FAT_CLUSTER_AVAILABLE)
                    part->freeClusters.push_back(x + i);
            }
            x += 0x14000;
        }
    }

    // calculate the amount of free memory
    part->freeMemory = (UINT64)part->freeClusters.size() * (UINT64)part->clusterSize;

    // cleanup
    delete[] buffer;

    if (progress)
        progress(arg, true);

    return part->freeMemory;
}

void FatxDrive::ReloadDrive()
{
    if (type == FatxHarddrive)
        delete[] securityBlob.msLogo;

    for (int i = 0, count = partitions.size(); i < count; i++)
        delete partitions[i];
    partitions.clear();

    loadFatxDrive();
}

bool FatxDrive::FileExists(std::string filePath)
{
    return !!GetFileEntry(filePath);
}

bool FatxDrive::FileExists(FatxFileEntry *folder, std::string fileName, bool checkDeleted)
{
    GetChildFileEntries(folder);

    for (size_t i = 0; i < folder->cachedFiles.size(); i++)
    {
        // there's a better way to do this, but...
        if (folder->cachedFiles.at(i).name == fileName)
        {
            if (folder->cachedFiles.at(i).nameLen == FATX_ENTRY_DELETED)
            {
                if (checkDeleted)
                    return true;
            }
            else
                return true;
        }
    }

    return false;
}

FatxFileEntry* FatxDrive::GetFileEntry(std::string filePath)
{
    // make sure the path starts with "Drive:\\"
    if (filePath.size() < 7 || filePath.substr(0, 7) != "Drive:\\")
        throw string("FATX: Invalid path name.");

    // strip off the prefix
    filePath = filePath.substr(7);

    // get the partition
    std::string partitionName = filePath.substr(0, filePath.find('\\'));

    if (filePath.find('\\') == std::string::npos)
        filePath = "";
    else
        filePath = filePath.substr(filePath.find('\\') + 1);

    Partition *part = NULL;
    for (DWORD i = 0; i < partitions.size(); i++)
    {
        if (partitions.at(i)->name == partitionName)
        {
            part = partitions.at(i);
            break;
        }
    }

    // if there is no corresponding partition, then the path is invalid
    if (part == NULL)
        return NULL;

    FatxFileEntry *parent = &part->root;
    while ((int)filePath.find('\\') >= 0 || filePath.size() > 0)
    {
        // load all the entries children (not recursively)
        GetChildFileEntries(parent);

        // get the next file name
        std::string fileName = filePath.substr(0, filePath.find('\\'));

        if (filePath.find('\\') == std::string::npos)
            filePath = "";
        else
            filePath = filePath.substr(filePath.find('\\') + 1);

        // check and see if the sub-directory exists
        FatxFileEntry *foundEntry = NULL;
        for (DWORD i = 0; i < parent->cachedFiles.size(); i++)
        {
            if (parent->cachedFiles.at(i).name == fileName &&
                    parent->cachedFiles.at(i).nameLen != FATX_ENTRY_DELETED)
            {
                foundEntry = &parent->cachedFiles.at(i);
                break;
            }
        }

        if (foundEntry == NULL)
            return NULL;

        parent = foundEntry;
    }

    return parent;
}

void FatxDrive::SetDriveName(std::wstring name)
{
    FatxFileEntry *nameEntry = GetFileEntry("Drive:\\Content\\name.txt");

    // if the file doesn't exist, then we need to create it
    if (nameEntry == NULL)
    {
        CreateFileX(GetFileEntry("Drive:\\Content"), "name.txt");
        nameEntry = GetFileEntry("Drive:\\Content\\name.txt");
    }

    FatxIO nameIO = GetFatxIO(nameEntry);

    // update the entry size
    int count = (name.length() * 2) + 2;

    if (count > 0x36)
        throw std::string("FATX: Drive name cannot be larger than 26 characters.\n");

    nameIO.entry->fileSize = count;
    nameIO.WriteEntryToDisk();

    // set the postition
    nameIO.SetPosition(0);

    // the xbox requires that these 2 bytes are at the beginning of the file for whatever reason
    nameIO.Write((WORD)0xFEFF);
    nameIO.Write(name);
}

bool FatxDrive::ValidFileName(std::string fileName)
{
    if (fileName.size() > FATX_ENTRY_MAX_NAME_LENGTH)
        return false;

    for (DWORD i = 0; i < fileName.size(); i++)
        if (!validFileChar(fileName.at(i)))
            return false;

    return true;
}
