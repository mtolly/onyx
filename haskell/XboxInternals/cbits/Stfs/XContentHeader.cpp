#include "XContentHeader.h"
#include "IO/MemoryIO.h"
#include <iostream>
#include <sstream>

using namespace std;

XContentHeader::XContentHeader(BaseIO *io, DWORD flags) : installerType((InstallerType)0),
    flags(flags)
{
    // set the io
    this->io = io;

    if ((flags & MetadataSkipRead) == 0)
        readMetadata();
}

void XContentHeader::readMetadata()
{
    // store errors thrown
    stringstream except;

    io->SetPosition(0);

    if ((flags & MetadataIsPEC) == 0)
    {
        magic = (Magic)io->ReadDword();

        if (magic == CON)
            ReadCertificateEx(&certificate, io, 4);
        else if (magic == LIVE || magic == PIRS)
            io->ReadBytes(packageSignature, 0x100);
        else
        {
            except << "XContentHeader: Content signature type 0x" << hex << (DWORD)magic << " is invalid.\n";
            throw except.str();
        }

        io->SetPosition(0x22C);

        // read licensing data
        for(int i = 0; i < 0x10; i++)
        {
            UINT64 tempYo = io->ReadUInt64();
            licenseData[i].type = (LicenseType)(tempYo >> 48);
            licenseData[i].data = (tempYo & 0xFFFFFFFFFFFF);
            licenseData[i].bits = io->ReadDword();
            licenseData[i].flags = io->ReadDword();

            switch (licenseData[i].type)
            {
                case Unused:
                case Unrestricted:
                case ConsoleProfileLicense:
                case WindowsProfileLicense:
                case ConsoleLicense:
                case MediaFlags:
                case KeyVaultPrivileges:
                case HyperVisorFlags:
                case UserPrivileges:
                    break;
                default:
                    except << "XContentHeader: Invalid license type at index " << i << ".\n";
                    throw except.str();
            }
        }

        // header hash / content id
        io->ReadBytes(headerHash, 0x14);

        headerSize = io->ReadDword();

        // to do: make sure it's a valid type
        contentType = (ContentType)io->ReadDword();

        // read metadata information
        metaDataVersion = io->ReadDword();
        contentSize = io->ReadUInt64();
        mediaID = io->ReadDword();
        version = io->ReadDword();
        baseVersion = io->ReadDword();
        titleID = io->ReadDword();
        platform = io->ReadByte();
        executableType = io->ReadByte();
        discNumber = io->ReadByte();
        discInSet = io->ReadByte();
        savegameID = io->ReadDword();
        io->ReadBytes(consoleID, 5);
        io->ReadBytes(profileID, 8);

        // read the file system type
        io->SetPosition(0x3A9);
        fileSystem = (FileSystem)io->ReadDword();
        if (fileSystem > 1)
            throw string("XContentHeader: Invalid file system. Only STFS and SVOD are supported.\n");

        // read volume descriptor
        if (fileSystem == FileSystemSTFS)
            ReadStfsVolumeDescriptorEx(&stfsVolumeDescriptor, io, 0x379);
        else if (fileSystem == FileSystemSVOD)
            ReadSvodVolumeDescriptorEx(&svodVolumeDescriptor, io);

        dataFileCount = io->ReadDword();
        dataFileCombinedSize = io->ReadUInt64();

        // read the avatar metadata if needed
        if (contentType == AvatarItem)
        {
            io->SetPosition(0x3D9);
            io->SwapEndian();

            subCategory = (AssetSubcategory)io->ReadDword();
            colorizable = io->ReadDword();

            io->SwapEndian();

            io->ReadBytes(guid, 0x10);
            skeletonVersion = (SkeletonVersion)io->ReadByte();

            if (skeletonVersion < 1 || skeletonVersion > 3)
                throw string("XContentHeader: Invalid skeleton version.");
        }
        else if (contentType == Video) // there may be other content types with this metadata
        {
            io->SetPosition(0x3D9);

            io->ReadBytes(seriesID, 0x10);
            io->ReadBytes(seasonID, 0x10);

            seasonNumber = io->ReadWord();
            episodeNumber = io->ReadWord();
        }

        // skip padding
        io->SetPosition(0x3FD);

        io->ReadBytes(deviceID, 0x14);
        displayName = io->ReadWString();
        io->SetPosition(0xD11);
        displayDescription = io->ReadWString();
        io->SetPosition(0x1611);
        publisherName = io->ReadWString(0x80);
        io->SetPosition(0x1691);
        titleName = io->ReadWString(0x80);
        io->SetPosition(0x1711);
        transferFlags = io->ReadByte();

        // read image sizes
        thumbnailImageSize = io->ReadDword();
        titleThumbnailImageSize = io->ReadDword();

        thumbnailImage = new BYTE[thumbnailImageSize];
        titleThumbnailImage = new BYTE[titleThumbnailImageSize];

        // read images
        io->ReadBytes(thumbnailImage, thumbnailImageSize);
        io->SetPosition(0x571A);

        io->ReadBytes(titleThumbnailImage, titleThumbnailImageSize);
        io->SetPosition(0x971A);

        if (((headerSize + 0xFFF) & 0xFFFFF000) - 0x971A < 0x15F4)
            return;

        installerType = (InstallerType)io->ReadDword();
        switch (installerType)
        {
            case SystemUpdate:
            case TitleUpdate:
            {
                DWORD tempbv = io->ReadDword();
                installerBaseVersion.major = tempbv >> 28;
                installerBaseVersion.minor = (tempbv >> 24) & 0xF;
                installerBaseVersion.build = (tempbv >> 8) & 0xFFFF;
                installerBaseVersion.revision = tempbv & 0xFF;

                DWORD tempv = io->ReadDword();
                installerVersion.major = tempv >> 28;
                installerVersion.minor = (tempv >> 24) & 0xF;
                installerVersion.build = (tempv >> 8) & 0xFFFF;
                installerVersion.revision = tempv & 0xFF;

                break;
            }
            case SystemUpdateProgressCache:
            case TitleUpdateProgressCache:
            case TitleContentProgressCache:
            {
                resumeState = (OnlineContentResumeState)io->ReadDword();
                currentFileIndex = io->ReadDword();
                currentFileOffset = io->ReadUInt64();
                bytesProcessed = io->ReadUInt64();

                WINFILETIME time;
                time.dwHighDateTime = io->ReadDword();
                time.dwLowDateTime = io->ReadDword();
                lastModified = XdbfHelpers::FILETIMEtoTimeT(time);

                io->ReadBytes(cabResumeData, 0x15D0);
                break;
            }
            case None:
                break;
            default:
                throw string("XContentHeader: Invalid Installer Type value.");
        }

#ifdef DEBUG
        if(metaDataVersion != 2)
            throw string("XContentHeader: Metadata version is not 2.\n");
#endif
    }
    else
    {
        ReadCertificateEx(&certificate, io, 0);
        io->ReadBytes(headerHash, 0x14);

        ReadStfsVolumeDescriptorEx(&stfsVolumeDescriptor, io, 0x244);

        // *skip missing int*
        io->SetPosition(0x26C);
        io->ReadBytes(profileID, 8);
        enabled = (bool)io->ReadByte();
        io->ReadBytes(consoleID, 5);

        // anything between 1 and 0x1000 works, inclusive
        headerSize = 0x1000;
    }
}

void XContentHeader::WriteCertificate()
{
    if (magic != CON && (flags & MetadataIsPEC) == 0)
        throw string("XContentHeader: Error writing certificate. Package is strong signed and therefore doesn't have a certificate.\n");

    WriteCertificateEx(&certificate, io, (flags & MetadataIsPEC) ? 0 : 4);
}

void XContentHeader::FixHeaderHash()
{
    DWORD headerStart = ((flags & MetadataIsPEC) ? 0x23C : 0x344);

    // calculate header size / first hash table address
    DWORD calculated = ((headerSize + 0xFFF) & 0xFFFFF000);
    io->SetPosition(0, ios_base::end);
    calculated = (io->GetPosition() < calculated) ? (DWORD)io->GetPosition() : calculated;
    DWORD realHeaderSize = calculated - headerStart;

    BYTE *data = new BYTE[realHeaderSize];

    // seek to the position
    io->SetPosition(headerStart);
    io->ReadBytes(data, realHeaderSize);

    // hash the data
    Botan::SHA_160 sha1;
    sha1.clear();
    sha1.update(data, realHeaderSize);
    sha1.final(headerHash);

    delete[] data;

    // Write the new hash to the file
    io->SetPosition(((flags & MetadataIsPEC) ? 0x228 : 0x32C));
    io->Write(headerHash, 0x14);
    io->Flush();
}

void XContentHeader::WriteMetaData()
{
    // seek to the begining of the file
    io->SetPosition(0);

    if ((flags & MetadataIsPEC) == 0)
    {

        io->Write((DWORD)magic);

        if (magic == CON)
            WriteCertificate();
        else if (magic == PIRS || magic == LIVE)
            io->Write(packageSignature, 0x100);
        else
        {
            stringstream except;
            except << "XContentHeader: Content signature type 0x" << hex << (DWORD)magic << " is invalid.\n";
            throw except.str();
        }

        // Write the licensing data
        io->SetPosition(0x22C);
        for (DWORD i = 0; i < 0x10; i++)
        {
            io->Write(((UINT64)licenseData[i].type << 48) | (UINT64)licenseData[i].data);
            io->Write(licenseData[i].bits);
            io->Write(licenseData[i].flags);
        }

        io->Write(headerHash, 0x14);
        io->Write(headerSize);
        io->Write((DWORD)contentType);
        io->Write((DWORD)metaDataVersion);
        io->Write((UINT64)contentSize);
        io->Write(mediaID);
        io->Write(version);
        io->Write(baseVersion);
        io->Write(titleID);
        io->Write((BYTE)platform);
        io->Write((BYTE)executableType);
        io->Write((BYTE)discNumber);
        io->Write((BYTE)discInSet);
        io->Write((DWORD)savegameID);
        io->Write(consoleID, 5);
        io->Write(profileID, 8);

        WriteVolumeDescriptor();

        io->Write(dataFileCount);
        io->Write(dataFileCombinedSize);

        // Write the avatar asset metadata if needed
        if (contentType == AvatarItem)
        {
            io->SetPosition(0x3D9);
            io->SwapEndian();

            io->Write((DWORD)subCategory);
            io->Write((DWORD)colorizable);

            io->SwapEndian();

            io->Write(guid, 0x10);
            io->Write((BYTE)skeletonVersion);
        }
        else if (contentType == Video)
        {
            io->SetPosition(0x3D9);

            io->Write(seriesID, 0x10);
            io->ReadBytes(seasonID, 0x10);

            io->Write(seasonNumber);
            io->Write(episodeNumber);
        }

        // skip padding
        io->SetPosition(0x3FD);

        io->Write(deviceID, 0x14);
        io->Write(displayName);

        io->SetPosition(0xD11);
        io->Write(displayDescription);

        io->SetPosition(0x1611);
        io->Write(publisherName);

        io->SetPosition(0x1691);
        io->Write(titleName);

        io->SetPosition(0x1711);
        io->Write((BYTE)transferFlags);

        io->Write(thumbnailImageSize);
        io->Write(titleThumbnailImageSize);

        io->Write(thumbnailImage, thumbnailImageSize);
        io->SetPosition(0x571A);
        io->Write(titleThumbnailImage, titleThumbnailImageSize);

        if (((headerSize + 0xFFF) & 0xFFFFF000) - 0x971A < 0x15F4)
            return;

        io->Write((DWORD)installerType);
        switch (installerType)
        {
            case SystemUpdate:
            case TitleUpdate:
            {
                DWORD tempbv = 0;
                tempbv |= (installerBaseVersion.major & 0xF) << 28;
                tempbv |= (installerBaseVersion.minor & 0xF) << 24;
                tempbv |= (installerBaseVersion.build & 0xFFFF) << 8;
                tempbv |= installerBaseVersion.revision & 0xFF;
                io->Write(tempbv);

                DWORD tempv = 0;
                tempv |= (installerVersion.major & 0xF) << 28;
                tempv |= (installerVersion.minor & 0xF) << 24;
                tempv |= (installerVersion.build & 0xFFFF) << 8;
                tempv |= installerVersion.revision & 0xFF;
                io->Write(tempv);

                break;
            }

            case SystemUpdateProgressCache:
            case TitleUpdateProgressCache:
            case TitleContentProgressCache:
            {
                io->Write((DWORD)resumeState);
                io->Write(currentFileIndex);
                io->Write(currentFileOffset);
                io->Write(bytesProcessed);

                WINFILETIME time = XdbfHelpers::TimeTtoFILETIME(lastModified);
                io->Write(time.dwHighDateTime);
                io->Write(time.dwLowDateTime);

                io->Write(cabResumeData, 0x15D0);
                break;
            }

            case None:
                break;
        }
    }
    else
    {
        memcpy(consoleID, certificate.ownerConsoleID, 5);

        WriteCertificateEx(&certificate, io, 0);
        io->Write(headerHash, 0x14);

        WriteStfsVolumeDescriptorEx(&stfsVolumeDescriptor, io, 0x244);

        // *skip missing int*
        io->SetPosition(0x26C);
        io->Write(profileID, 8);
        io->Write((BYTE)enabled);
        io->Write(consoleID, 5);
    }
    io->Flush();
}

void XContentHeader::WriteVolumeDescriptor()
{
    if (fileSystem == FileSystemSTFS)
        WriteStfsVolumeDescriptorEx(&stfsVolumeDescriptor, io, (flags & MetadataIsPEC) ? 0x244 : 0x379);
    else if (fileSystem == FileSystemSVOD)
        WriteSvodVolumeDescriptorEx(&svodVolumeDescriptor, io);
}

void XContentHeader::ResignHeader(string kvPath)
{
    FileIO kvIo(kvPath);
    ResignHeader(kvIo);
}

void XContentHeader::ResignHeader(BYTE* kvData, size_t length)
{
    MemoryIO kvIo(kvData, length);
    ResignHeader(kvIo);
}

void XContentHeader::ResignHeader(BaseIO& kvIo)
{
    kvIo.SetPosition(0, ios_base::end);

    DWORD adder = 0;
    if (kvIo.GetPosition() == 0x4000)
        adder = 0x10;

    DWORD headerStart, size, hashLoc, toSignLoc, consoleIDLoc;

    // set the headerStart
    if (flags & MetadataIsPEC)
    {
        headerStart = 0x23C;
        hashLoc = 0x228;
        size = 0xDC4;
        toSignLoc = 0x23C;
        consoleIDLoc = 0x275;
    }
    else
    {
        headerStart = 0x344;
        hashLoc = 0x32C;
        size = 0x118;
        toSignLoc = 0x22C;
        consoleIDLoc = 0x36C;
    }

    // calculate header size / first hash table address
    DWORD calculated = ((headerSize + 0xFFF) & 0xFFFFF000);
    io->SetPosition(0, ios_base::end);
    calculated = (io->GetPosition() < calculated) ? (DWORD)io->GetPosition() : calculated;
    DWORD realHeaderSize = calculated - headerStart;

    // read the certificate
    kvIo.SetPosition(0x9B8 + adder);
    certificate.publicKeyCertificateSize = kvIo.ReadWord();
    kvIo.ReadBytes(certificate.ownerConsoleID, 5);

    char tempPartNum[0x15];
    tempPartNum[0x14] = 0;
    kvIo.ReadBytes((BYTE*)tempPartNum, 0x14);
    certificate.ownerConsolePartNumber = string(tempPartNum);

    certificate.ownerConsoleType = (ConsoleType)kvIo.ReadByte();

    char tempGenDate[9] = {0};
    kvIo.ReadBytes((BYTE*)tempGenDate, 8);
    certificate.dateGeneration = string(tempGenDate);

    certificate.publicExponent = kvIo.ReadDword();
    kvIo.ReadBytes(certificate.publicModulus, 0x80);
    kvIo.ReadBytes(certificate.certificateSignature, 0x100);

    // read the keys for signing
    BYTE nData[0x80];
    BYTE pData[0x40];
    BYTE qData[0x40];

    kvIo.SetPosition(0x298 + adder);
    kvIo.ReadBytes(nData, 0x80);
    kvIo.ReadBytes(pData, 0x40);
    kvIo.ReadBytes(qData, 0x40);

    // 8 byte swap all necessary keys
    XeCrypt::BnQw_SwapDwQwLeBe(nData, 0x80);
    XeCrypt::BnQw_SwapDwQwLeBe(pData, 0x40);
    XeCrypt::BnQw_SwapDwQwLeBe(qData, 0x40);

    // get the keys ready for signing

    Botan::BigInt n = Botan::BigInt::decode(nData, 0x80);
    Botan::BigInt p = Botan::BigInt::decode(pData, 0x40);
    Botan::BigInt q = Botan::BigInt::decode(qData, 0x40);

    Botan::AutoSeeded_RNG rng;
    Botan::RSA_PrivateKey pkey(p, q, 0x10001, 0, n);

    // Write the console id
    io->SetPosition(consoleIDLoc);
    io->Write(certificate.ownerConsoleID, 5);

    // read the data to hash
    BYTE *buffer = new BYTE[realHeaderSize];
    io->SetPosition(headerStart);
    io->ReadBytes(buffer, realHeaderSize);

    // hash the header
    Botan::SHA_160 sha1;
    sha1.clear();
    sha1.update(buffer, realHeaderSize);
    sha1.final(headerHash);

    delete[] buffer;

    io->SetPosition(hashLoc);
    io->Write(headerHash, 0x14);

    io->SetPosition(toSignLoc);

    BYTE *dataToSign = new BYTE[size];
    io->ReadBytes(dataToSign, size);

    Botan::PK_Signer signer(pkey, rng, "EMSA3(SHA-160)");

    std::vector<uint8_t> signature = signer.sign_message((unsigned char*)dataToSign, size,
            rng);

    // 8 byte swap the new signature
    XeCrypt::BnQw_SwapDwQwLeBe(signature.data(), 0x80);

    // reverse the new signature every 8 bytes
    for (int i = 0; i < 0x10; i++)
        FileIO::ReverseGenericArray(&signature[i * 8], 1, 8);

    // Write the certficate
    memcpy(certificate.signature, signature.data(), 0x80);
    WriteCertificate();

    delete[] dataToSign;
}

XContentHeader::~XContentHeader()
{
    if ((flags & MetadataIsPEC) == 0 && (flags & MetadataDontFreeThumbnails) == 0)
    {
        delete[] thumbnailImage;
        delete[] titleThumbnailImage;
    }
}
