#include "FatxDriveDetection.h"
#ifdef _WIN32
#include <windows.h>
#else
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

#ifdef __APPLE__
#include <sys/disk.h>
#include <sys/ioctl.h>
#endif

#ifdef __linux
#include <mntent.h>
#endif
#endif

std::vector<FatxDrive*> FatxDriveDetection::GetAllFatxDrives()
{
    std::vector<std::wstring> logicalDrivePaths = getLogicalDrives();
    std::vector<FatxDrive*> drives;

    std::vector<DeviceIO*> devices = getPhysicalDisks();

    for (size_t i = 0; i < devices.size(); i++)
    {
        if (devices.at(i)->Length() > HddOffsets::Data)
        {
            devices.at(i)->SetPosition(HddOffsets::Data);
            if (devices.at(i)->ReadDword() == FATX_MAGIC)
            {
                FatxDrive *drive = new FatxDrive(static_cast<BaseIO*>(devices.at(i)), FatxHarddrive);
                drives.push_back(drive);
            }
            else
                devices.at(i)->Close();
        }
    }

    std::vector<std::string> dataFiles;
    for (size_t i = 0; i < logicalDrivePaths.size(); i++)
    {
        // clear data files from the previous drive
        dataFiles.clear();

        std::string directory;
        directory.assign(logicalDrivePaths.at(i).begin(), logicalDrivePaths.at(i).end());


#ifdef _WIN32
        WIN32_FIND_DATA fi;

        HANDLE h = FindFirstFile((logicalDrivePaths.at(i) + L"\\Data*").c_str(), &fi);
        if (h != INVALID_HANDLE_VALUE)
        {
            do
            {
                char path[9];
                wcstombs(path, fi.cFileName, wcslen(fi.cFileName) + 1);
                dataFiles.push_back(directory + "\\" + std::string(path));
            }
            while (FindNextFile(h, &fi));

            FindClose(h);

            if (dataFiles.size() >= 3)
            {
                // make sure the data files are loaded in the right order
                std::sort(dataFiles.begin(), dataFiles.end());
                MultiFileIO *io = new MultiFileIO(dataFiles);
                FatxDrive *usbDrive = new FatxDrive(io, FatxFlashDrive);
                drives.push_back(usbDrive);
            }
        }
#else
        DIR *dir = NULL;
        dirent *ent = NULL;
        dir = opendir(directory.c_str());
        if (dir != NULL)
        {
            // search for valid data files
            while ((ent = readdir(dir)) != NULL)
            {
                // the disks start with 'data'
                if (std::string(ent->d_name).substr(0, 4) == "Data")
                    dataFiles.push_back(directory + std::string(ent->d_name));
            }

            if (dataFiles.size() >= 3)
            {
                // make sure the data files are loaded in the right order
                std::sort(dataFiles.begin(), dataFiles.end());
                MultiFileIO *io = new MultiFileIO(dataFiles);
                FatxDrive *usbDrive = new FatxDrive(io, FatxFlashDrive);
                drives.push_back(usbDrive);
            }
        }
#endif
    }

    return drives;
}

std::vector<DeviceIO*> FatxDriveDetection::getPhysicalDisks()
{
    std::vector<DeviceIO*> physicalDiskPaths;
    std::wstringstream ss;

#ifdef _WIN32
    for (int i = 0; i < 16; i++)
    {
        ss << L"\\\\.\\PHYSICALDRIVE" << i;

        HANDLE drive = CreateFile(ss.str().c_str(), GENERIC_READ|GENERIC_WRITE,
                FILE_SHARE_READ|FILE_SHARE_WRITE, 0, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
        if (drive != INVALID_HANDLE_VALUE)
        {
            CloseHandle(drive);

            DeviceIO *io = new DeviceIO(ss.str());
            physicalDiskPaths.push_back(io);
        }

        ss.str(std::wstring());
    }
#else
    DIR *dir = NULL;
    dirent *ent = NULL;
    dir = opendir("/dev/");
    if (dir != NULL)
    {
        // search for valid drives
        while ((ent = readdir(dir)) != NULL)
        {
#ifdef __APPLE__
            // the disks start with 'disk'
            if (std::string(ent->d_name).substr(0, 4) == "disk")
#elif __linux
            // the disks start with 'sd'
            if (std::string(ent->d_name).substr(0, 2) == "sd")
#endif
            {
                std::ostringstream ss;
#ifdef __APPLE__
                ss << "/dev/r";
#elif __linux
                ss << "/dev/";
#endif
                ss << ent->d_name;
                std::string diskPath = ss.str();

                int device;
                if ((device = open(diskPath.c_str(), O_RDWR)) > 0)
                {
                    close(device);

                    DeviceIO *io = new DeviceIO(diskPath);
                    physicalDiskPaths.push_back(io);
                }
            }
        }
    }
    if (dir)
        closedir(dir);
    if (ent)
        delete ent;
#endif

    return physicalDiskPaths;
}

std::vector<std::wstring> FatxDriveDetection::getLogicalDrives()
{
    std::vector<std::wstring> driveStrings;

#ifdef _WIN32
    DWORD drives = GetLogicalDrives();
    std::wstringstream ss;
    char currentChar = 'A';

    for (int i = 0; i < 32; i++)
    {
        if (drives & 1)
        {
            ss << currentChar << ":\\Xbox360";
            driveStrings.push_back(ss.str());
            ss.str(std::wstring());
        }

        drives >>= 1;
        currentChar++;
    }
#elif __APPLE__
    DIR *dir = NULL;
    dirent *ent = NULL;
    dir = opendir("/Volumes/");
    if (dir != NULL)
    {
        std::stringstream path;

        // search for valid flash drives
        while ((ent = readdir(dir)) != NULL)
        {
            try
            {
                // initialize the path
                path << "/Volumes/";
                path << ent->d_name;
                path << "/Xbox360/";

                // get the xbox360 folder path
                std::string xboxDirPath = path.str();

                // add it to our list of drives
                std::wstring widePathStr;
                widePathStr.assign(xboxDirPath.begin(), xboxDirPath.end());
                driveStrings.push_back(widePathStr);
            }
            catch(...)
            {
                // something bad happened
                // skip this device, and try the next one
            }

            // clear the stringstream
            path.str(std::string());
        }
        if (dir)
            closedir(dir);
        if (ent)
            delete ent;
    }
#elif __linux
    DIR *dir;
    struct mntent *ent;
    std::stringstream path;

    FILE *mounts = setmntent("/proc/mounts", "r");

    if (mounts != NULL)
    {
        while ((ent = getmntent(mounts)) != NULL)
        {
            path.str(std::string());
            path << ent->mnt_dir << "/Xbox360/";

            try
            {
                std::string xboxDirPath = path.str();

                if((dir = opendir(xboxDirPath.c_str())) != NULL)
                {
                    std::wstring widePathStr;
                    widePathStr.assign(xboxDirPath.begin(), xboxDirPath.end());
                    driveStrings.push_back(widePathStr);

                    closedir(dir);
                }
            }
            catch(...)
            {
            }
        }

        endmntent(mounts);

        if(ent)
            delete ent;
    }
#endif
    return driveStrings;
}
