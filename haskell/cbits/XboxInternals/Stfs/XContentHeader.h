#pragma once

#include "winnames.h"

#include "StfsConstants.h"
#include "StfsDefinitions.h"
#include "IO/FileIO.h"
#include "../AvatarAsset/AvatarAssetDefinintions.h"
#include "../Gpd/XdbfHelpers.h"
#include "../Cryptography/XeCrypt.h"

#include <iostream>

#include <botan/botan.h>
#include <botan/pubkey.h>
#include <botan/rsa.h>
#include <botan/emsa.h>
#include <botan/sha160.h>
#include <botan/emsa3.h>
#include <botan/look_pk.h>

#include "XboxInternals_global.h"

using std::string;

enum XContentFlags
{
    MetadataIsPEC = 1,
    MetadataSkipRead = 2,
    MetadataDontFreeThumbnails = 4
};

enum OnlineContentResumeState
{
    FileHeadersNotReady = 0x46494C48,
    NewFolder = 0x666F6C64,
    NewFolderResum_Attempt1 = 0x666F6C31,
    NewFolderResumeAttempt2 = 0x666F6C32,
    NewFolderResumeAttemptUnknown = 0x666F6C3F,
    NewFolderResumeAttemptSpecific = 0x666F6C40
};

enum FileSystem
{
    FileSystemSTFS = 0,
    FileSystemSVOD,
    FileSystemFATX
};

class XBOXINTERNALSSHARED_EXPORT XContentHeader
{
public:
    // Description: read in all of the metadata for the package
    XContentHeader(BaseIO *io, DWORD flags = 0);

    // fix the signature in the header
    void ResignHeader(string kvPath);

    // fix the signature in the header
    void ResignHeader(BYTE* kvData, size_t length);

    // fix the signature in the header
    void ResignHeader(BaseIO& kvIo);

    // fix the sha1 hash of the header data
    void FixHeaderHash();

    // Description: Write the console certificate
    void WriteCertificate();

    // Description: Write the volume descriptor
    void WriteVolumeDescriptor();

    // Description: Write all of the metadata
    void WriteMetaData();
    ~XContentHeader();

    Magic magic;

    // only console signed
    Certificate certificate;

    // only strong signed
    BYTE packageSignature[0x100];

    LicenseEntry licenseData[0x10];
    BYTE headerHash[0x14];
    DWORD headerSize;
    ContentType contentType;
    DWORD metaDataVersion;
    UINT64 contentSize;
    DWORD mediaID;
    DWORD version;
    DWORD baseVersion;
    DWORD titleID;
    BYTE platform;
    BYTE executableType;
    BYTE discNumber;
    BYTE discInSet;
    DWORD savegameID;
    BYTE consoleID[5];
    BYTE profileID[8];

    StfsVolumeDescriptor stfsVolumeDescriptor;
    SvodVolumeDescriptor svodVolumeDescriptor;
    FileSystem fileSystem;

    // only in PEC, and im not sure exactly what this byte is but it needs to always be set to 1
    bool enabled;

    // metadata v1
    DWORD dataFileCount;
    UINT64 dataFileCombinedSize;
    BYTE deviceID[0x14];
    wstring displayName;
    wstring displayDescription;
    wstring publisherName;
    wstring titleName;
    BYTE transferFlags;
    DWORD thumbnailImageSize;
    DWORD titleThumbnailImageSize;

    // credit to Eaton for all the extra metadata stuff
    InstallerType installerType;

    // Avatar Asset
    AssetSubcategory subCategory;
    DWORD colorizable;
    BYTE guid[0x10];
    SkeletonVersion skeletonVersion;

    // media
    BYTE seriesID[0x10];
    BYTE seasonID[0x10];
    WORD seasonNumber;
    WORD episodeNumber;

    // installer progress cache data
    OnlineContentResumeState resumeState;
    DWORD currentFileIndex;
    UINT64 currentFileOffset;
    UINT64 bytesProcessed;
    time_t lastModified;
    BYTE cabResumeData[5584];

    // installer update data
    Version installerBaseVersion;
    Version installerVersion;

    BYTE *thumbnailImage;
    BYTE *titleThumbnailImage;

private:
    BaseIO *io;
    DWORD flags;

    void readMetadata();
};

