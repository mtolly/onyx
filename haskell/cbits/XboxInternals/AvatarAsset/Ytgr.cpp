#include "Ytgr.h"

Ytgr::Ytgr(std::string filePath) : ioPassedIn(false)
{
    io = new FileIO(filePath);
    Parse();
}

Ytgr::Ytgr(FileIO *io) : ioPassedIn(true)
{
    this->io = io;
    Parse();
}

Ytgr::~Ytgr()
{
    if (!ioPassedIn)
        delete io;
}

void Ytgr::Parse()
{
    io->SetPosition(0);

    // verify the magic
    magic = io->ReadDword();
    if (magic != 0x59544752)
        throw std::string("Ytgr: Invalid magic.");

    // read the version
    io->SetEndian(LittleEndian);
    DWORD tempV = io->ReadDword();
    XSignerMinimumVersion.major = tempV >> 28;
    XSignerMinimumVersion.minor = (tempV >> 24) & 0xF;
    XSignerMinimumVersion.build = (tempV >> 8) & 0xFFFF;
    XSignerMinimumVersion.revision = tempV & 0xFF;

    structSize = io->ReadDword();
    if (structSize != 0x130)
        throw std::string("Ytgr: Invalid struct size.");

    // parse the timestamp
    WINFILETIME fileTime;
    fileTime.dwLowDateTime = io->ReadDword();
    fileTime.dwHighDateTime = io->ReadDword();
    dateAddedToServer = XdbfHelpers::FILETIMEtoTimeT(fileTime);

    contentLength = io->ReadDword();
    reserved = io->ReadDword();
    io->ReadBytes(contentHash, 0x14);
    io->ReadBytes(rsaSignature, 0x100);

    // verify the signature
    BYTE message[0x30];
    io->SetPosition(0);
    io->ReadBytes(message, 0x30);
    valid = XeKeys::VerifyRSASignature(LIVEKey, message, 0x30, rsaSignature) ||
            XeKeys::VerifyRSASignature(UnknownKey, message, 0x30, rsaSignature);

    // verify the hash
    BYTE block[0x1000];
    BYTE calculatedHash[0x14];
    Botan::SHA_160 sha1;
    sha1.clear();

    io->SetPosition(0x130);
    DWORD tempLen = contentLength;

    while (tempLen >= 0x1000)
    {
        io->ReadBytes(block, 0x1000);
        sha1.update(block, 0x1000);
        tempLen -= 0x1000;
    }
    if (tempLen != 0)
    {
        io->ReadBytes(block, tempLen);
        sha1.update(block, tempLen);
    }

    sha1.final(calculatedHash);

    // make sure both the signature and hash are valid
    valid = valid && !memcmp(contentHash, calculatedHash, 0x14);
}
