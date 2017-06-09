#include "Account.h"

Account::Account(std::string path, bool decrypt, ConsoleType type) : ioPassedIn(false),
    decrypt(decrypt), path(path), type(type)
{
    Botan::LibraryInitializer init;

    if (decrypt)
    {
        decryptAccount(path, &outPath, type);
        io = new FileIO(outPath);
    }
    else
    {
        io = new FileIO(path);
        outPath = path;
    }

    parseFile();
}

void Account::parseFile()
{
    // seek to the begining of the file
    io->SetPosition(0);

    // read the data
    account.reservedFlags = io->ReadDword();
    account.liveFlags = io->ReadDword();
    account.gamertag = io->ReadWString(16);

    io->SetPosition(0x28);
    account.xuid = io->ReadUInt64();
    account.cachedUserFlags = io->ReadDword();
    account.serviceProvider = (XboxLiveServiceProvider)io->ReadDword();

    io->SetPosition(0x38);
    account.passcode[0] = io->ReadByte();
    account.passcode[1] = io->ReadByte();
    account.passcode[2] = io->ReadByte();
    account.passcode[3] = io->ReadByte();

    account.onlineDomain = io->ReadString(20);
    io->SetPosition(0x50);
    account.kerbrosRealm = io->ReadString(24);

    io->ReadBytes(account.onlineKey, 0x10);
}

bool Account::IsPasscodeEnabled()
{
    return (bool)(account.reservedFlags & PasswordProtected);
}

bool Account::IsLiveEnabled()
{
    return (bool)(account.reservedFlags & LiveEnabled);
}

bool Account::IsRecovering()
{
    return (bool)(account.reservedFlags & Recovering);
}

bool Account::IsParentalControlled()
{
    return (bool)(account.cachedUserFlags & 0x1000000);
}

bool Account::IsPaymentInstrumentCreditCard()
{
    return (bool)(account.cachedUserFlags & 1);
}

bool Account::IsXUIDOffline()
{
    return ((account.xuid >> 60) & 0xF) == 0xE;
}

bool Account::IsXUIDOnline()
{
    return ((account.xuid >> 48) & 0xFFFF) == 9;
}

bool Account::IsValidXUID()
{
    return IsXUIDOffline() != IsXUIDOnline();
}

bool Account::IsTeamXUID()
{
    return (account.xuid & 0xFF00000000000140) == 0xFE00000000000100;
}

UINT64 Account::GetXUID()
{
    return account.xuid;
}

void Account::SetPasscodeEnabled(bool b)
{
    if (b)
        account.reservedFlags |= PasswordProtected;
    else
        account.reservedFlags &= (~PasswordProtected);
}

void Account::SetLiveEnabled(bool b)
{
    if (b)
        account.reservedFlags |= LiveEnabled;
    else
    {
        account.reservedFlags &= (~LiveEnabled);
        account.serviceProvider = LiveDisabled;
    }
}

void Account::SetRecovering(bool b)
{
    if (b)
        account.reservedFlags |= Recovering;
    else
        account.reservedFlags &= (~Recovering);
}

void Account::SetParentalControlled(bool b)
{
    if (b)
        account.cachedUserFlags |= 0x1000000;
    else
        account.cachedUserFlags &= (~0x1000000);
}

void Account::SetPaymentInstrumentCreditCard(bool b)
{
    if (b)
        account.cachedUserFlags |= 1;
    else
        account.cachedUserFlags &= 0xFFFFFFFE;
}

void Account::SetXUIDOnline()
{
    account.xuid = (account.xuid & 0xFFFFFFFFFFFF) | 0x9000000000000;
}

void Account::SetXUIDOffline()
{
    account.xuid = 0;
}

void Account::SetXUID(UINT64 xuid)
{
    account.xuid = xuid;
}

void Account::SetSubscriptionTeir(SubscriptionTeir teir)
{
    account.cachedUserFlags &= (~0xF00000);
    account.cachedUserFlags |= (teir << 20);
}

void Account::SetCountry(XboxLiveCountry country)
{
    account.cachedUserFlags &= (~0xFF00);
    account.cachedUserFlags |= (country << 8);
}

void Account::SetLanguage(ConsoleLanguage language)
{
    account.cachedUserFlags &= (~0x3E000000);
    account.cachedUserFlags |= (language << 25);
}

void Account::SetGamertag(wstring gamertag)
{
    account.gamertag = gamertag;
}

void Account::SetPasscode(BYTE *passcode)
{
    memcpy(account.passcode, passcode, 4);
}

void Account::SetOnlineServiceProvider(XboxLiveServiceProvider provider)
{
    account.serviceProvider = provider;
}

void Account::Save(ConsoleType type)
{
    WriteFile();
    io->Close();
    encryptAccount(outPath, type, &path);
}

void Account::decryptAccount(std::string encryptedPath, std::string *outPath, ConsoleType type)
{
    // open the encrypted file
    FileIO encIo(encryptedPath);
    BYTE hmacHash[0x10];
    BYTE rc4Key[0x14];

    // read the hash
    encIo.ReadBytes(hmacHash, 0x10);

    Botan::SHA_160 *sha1 = new Botan::SHA_160;
    Botan::HMAC hmacSha1(sha1);

    // set the hmac-sha1 key
    if (type == Retail)
        hmacSha1.set_key(RETAIL_KEY, 0x10);
    else
        hmacSha1.set_key(DEVKIT_KEY, 0x10);

    // calculate the hmac
    hmacSha1.update(hmacHash, 0x10);
    hmacSha1.final(rc4Key);

    BYTE restOfFile[0x184];
    BYTE confounder[8];
    BYTE payload[0x17C];

    // read the rest of the file
    encIo.ReadBytes(restOfFile, 0x184);

    // decrypt using rc4
    Botan::ARC4 rc4;
    rc4.set_key(rc4Key, 0x10);

    rc4.cipher(restOfFile, restOfFile, 0x184);

    // copy the parts
    memcpy(confounder, restOfFile, 8);
    memcpy(payload, &restOfFile[8], 0x17C);

    hmacSha1.update(confounder, 8);
    hmacSha1.update(payload, 0x17C);

    BYTE confoundPayloadHash[0x14];
    hmacSha1.final(confoundPayloadHash);

    if (memcmp(confoundPayloadHash, hmacHash, 0x10) != 0)
        throw string("Account: Account decryption failed.\n");


    // Write the payload
#ifdef _WIN32
    // Opening a file using the path returned by tmpnam() may result in a "permission denied" error on Windows.
    // Not sure why it happens but tweaking the manifest/UAC properties makes a difference.
    char *outPath_c = _tempnam(NULL, NULL);
    if (!outPath_c)
        throw string("Account: Failed to generate temporary file name.\n");
    *outPath = string(outPath_c);
    free(outPath_c);
    outPath_c = NULL;
#else
    char outPath_c[L_tmpnam];
    if (!tmpnam(outPath_c))
        throw string("Account: Failed to generate temporary file name.\n");
    *outPath = string(outPath_c);
#endif

    FileIO decrypted(*outPath, true);
    decrypted.Write(payload, 0x17C);
    decrypted.Flush();

    // cleanup
    decrypted.Close();
    encIo.Close();
}

void Account::encryptAccount(std::string decryptedPath, ConsoleType type, std::string *outPath)
{
    FileIO decIo(decryptedPath);
    BYTE decryptedData[0x184];

    // copy in the the confounder
    memcpy(decryptedData, CONFOUNDER, 8);

    // read the decrypted data
    decIo.ReadBytes(&decryptedData[8], 0x17C);

    // initialization
    Botan::SHA_160 *sha1 = new Botan::SHA_160;
    Botan::HMAC hmacSha1(sha1);

    // set the hmac-sha1 key
    if (type == Retail)
        hmacSha1.set_key(RETAIL_KEY, 0x10);
    else
        hmacSha1.set_key(DEVKIT_KEY, 0x10);

    // hash the confounder and decrypted data
    BYTE hmacHash[0x14];
    hmacSha1.update(decryptedData, 0x184);
    hmacSha1.final(hmacHash);

    // begin writing the payload
    if (outPath == NULL)
    {
#ifdef _WIN32
        // Opening a file using the path returned by tmpnam() may result in a "permission denied" error on Windows.
        // Not sure why it happens but tweaking the manifest/UAC properties makes a difference.
        char *outPath_c = _tempnam(NULL, NULL);
        if (!outPath_c)
            throw string("Account: Failed to generate temporary file name.\n");
        *outPath = string(outPath_c);
        free(outPath_c);
        outPath_c = NULL;
#else
        char outPath_c[L_tmpnam];
        if (!tmpnam(outPath_c))
            throw string("Account: Failed to generate temporary file name.\n");
        *outPath = string(outPath_c);
#endif
    }

    decIo.Close();
    FileIO encrypted(*outPath, true);
    encrypted.Write(hmacHash, 0x10);

    // generate the rc4 key
    BYTE rc4Key[0x14];
    hmacSha1.update(hmacHash, 0x10);
    hmacSha1.final(rc4Key);

    // encrypt the data
    Botan::ARC4 rc4;
    rc4.set_key(rc4Key, 0x10);

    rc4.cipher1(decryptedData, 0x184);

    // Write the confounder and encrypted data
    encrypted.Write(decryptedData, 0x184);
    encrypted.Flush();

    // clean up
    encrypted.Close();
}

void Account::WriteFile()
{
    // seek to the beginning of the file
    io->SetPosition(0);

    // Write the information
    io->Write(account.reservedFlags);
    io->Write(account.liveFlags);
    io->Write(account.gamertag);

    // null the bytes out after the gamertag
    io->SetPosition(8 + (account.gamertag.length() * 2));
    BYTE temp[0x20] = {0};
    io->Write(temp, 0x20 - (account.gamertag.length() * 2));

    io->SetPosition(0x28);
    io->Write(account.xuid);
    io->Write(account.cachedUserFlags);
    io->Write((DWORD)account.serviceProvider);

    io->SetPosition(0x38);

    io->Write(account.passcode[0]);
    io->Write(account.passcode[1]);
    io->Write(account.passcode[2]);
    io->Write(account.passcode[3]);

    io->Write(account.onlineDomain);

    io->SetPosition(0x50);
    io->Write(account.kerbrosRealm);

    io->SetPosition(0x68);
    io->Write(account.onlineKey, 0x10);
}

SubscriptionTeir Account::GetSubscriptionTeir()
{
    return (SubscriptionTeir)((account.cachedUserFlags & 0xF00000) >> 20);
}

XboxLiveCountry Account::GetCountry()
{
    return (XboxLiveCountry)((account.cachedUserFlags & 0xFF00) >> 8);
}

ConsoleLanguage Account::GetLanguage()
{
    return (ConsoleLanguage)((account.cachedUserFlags & 0x3E000000) >> 25);
}

XboxLiveServiceProvider Account::GetServiceProvider()
{
    return account.serviceProvider;
}

void Account::GetPasscode(BYTE *passcode)
{
    if (!IsPasscodeEnabled())
    {
        memset(passcode, 0, 4);
        return;
    }
    memcpy(passcode, account.passcode, 4);
}

string Account::GetOnlineDomain()
{
    return account.onlineDomain;
}

string Account::GetKerbrosRealm()
{
    return account.kerbrosRealm;
}

void Account::GetOnlineKey(BYTE *outKey)
{
    memcpy(outKey, account.onlineKey, 0x10);
}

wstring Account::GetGamertag()
{
    return account.gamertag;
}

Account::~Account(void)
{
    io->Close();
    delete io;
    remove(outPath.c_str());
}
