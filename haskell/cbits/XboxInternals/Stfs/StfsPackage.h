#pragma once

#include <iostream>
#include <vector>
#include <sstream>
#include <math.h>
#include <map>
#include <time.h>
#include <stdlib.h>
#include "IO/FileIO.h"
#include "XContentHeader.h"

#include <botan/botan.h>
#include <botan/pubkey.h>
#include <botan/rsa.h>
#include <botan/emsa.h>
#include <botan/sha160.h>
#include <botan/emsa3.h>
#include <botan/look_pk.h>

#include "XboxInternals_global.h"

using std::string;
using std::stringstream;
using std::hex;
using std::vector;

struct StfsFileEntry
{
    DWORD entryIndex;
    string name;
    BYTE nameLen;
    BYTE flags;
    INT24 blocksForFile;
    INT24 startingBlockNum;
    WORD pathIndicator;
    DWORD fileSize;
    DWORD createdTimeStamp;
    DWORD accessTimeStamp;
    DWORD fileEntryAddress;
};

struct StfsFileListing
{
    vector<StfsFileEntry> fileEntries;
    vector<StfsFileListing> folderEntries;
    StfsFileEntry folder;
};

#pragma pack(push, 1)
struct HashEntry
{
    BYTE blockHash[0x14];
    BYTE status;
    DWORD nextBlock;
};
#pragma pack(pop)

struct HashTable
{
    Level level;
    DWORD trueBlockNumber;
    DWORD entryCount;
    HashEntry entries[0xAA];
    DWORD addressInFile;
};

enum StfsPackageFlags
{
    StfsPackagePEC = 1,
    StfsPackageCreate = 2,
    StfsPackageFemale = 4     // only used when creating a packge
};

class XBOXINTERNALSSHARED_EXPORT StfsPackage
{
public:
    XContentHeader *metaData;

    // Description: initialize a stfs package from an already opened io
    StfsPackage(BaseIO *io, DWORD flags = 0);

    // Description: initialize a stfs package
    StfsPackage(string packgePath, DWORD flags = 0);

    // Description: get the file listing of the package, forceUpdate reads from the package regardless
    StfsFileListing GetFileListing(bool forceUpdate = false);

    // Description: extract a file to designated file path
    void ExtractFile(string pathInPackage, string outPath, void(*extractProgress)(void*, DWORD,
            DWORD) = NULL, void *arg = NULL);

    // Description: extract a file (by FileEntry) to a designated file path
    void ExtractFile(StfsFileEntry *entry, string outPath, void(*extractProgress)(void*, DWORD,
            DWORD) = NULL, void *arg = NULL);

    // Description: get the file entry of a file's path, sets nameLen to '0' if not found
    StfsFileEntry GetFileEntry(string pathInPackage, bool checkFolders = false,
            StfsFileEntry *newEntry = NULL);

    // Description: get the first 4 bytes of a file
    DWORD GetFileMagic(string pathInPackage);

    // Description: get the first 4 bytes of a file
    DWORD GetFileMagic(StfsFileEntry entry);

    // Description: check if the file exists
    bool FileExists(string pathInPackage);

    // Description: fix all the hashes used in the file
    void Rehash();

    // Description: resign the file
    void Resign(string kvPath);

    // Description: resign the file
    void Resign(BYTE* kvData, size_t length);

    // Description: remove a file entry from the file listing
    void RemoveFile(StfsFileEntry entry);

    // Description: remove a file entry from the file listing
    void RemoveFile(string pathInPackage);

    // Description: inject a file into the package
    StfsFileEntry InjectFile(string path, string pathInPackage, void(*injectProgress)(void*, DWORD,
            DWORD) = NULL, void *arg = NULL);

    // Description: inject raw data into the package
    StfsFileEntry InjectData(BYTE *data, DWORD length, string pathInPackage,
            void(*injectProgress)(void*, DWORD, DWORD) = NULL, void *arg = NULL);

    // Description: replace an existing file into the package
    void ReplaceFile(string path, string pathInPackage, void(*replaceProgress)(void*, DWORD,
            DWORD) = NULL, void *arg = NULL);

    // Description: replace an existing file into the package
    void ReplaceFile(string path, StfsFileEntry *entry, string pathInPackage,
            void(*replaceProgress)(void*, DWORD, DWORD) = NULL, void *arg = NULL);

    // Description: rename an existing file in the package
    void RenameFile(string newName, string pathInPackage);

    // Description: convert a block into an address in the file
    DWORD BlockToAddress(DWORD blockNum);

    // Description: get the address of a hash for a data block
    DWORD GetHashAddressOfBlock(DWORD blockNum);

    // Description: returns whether the 'isPEC' parameter is set
    bool IsPEC();

    // Description: close the io and all other resources used
    void Close();

    // Description: creates a folder in the specified directory
    void CreateFolder(string pathInPackage);

    ~StfsPackage(void);
private:
    StfsFileListing fileListing;
    StfsFileListing writtenToFile;

    BaseIO *io;
    stringstream except;
    bool ioPassedIn;

    Sex packageSex;
    DWORD blockStep[2];
    DWORD firstHashTableAddress;
    BYTE hashOffset;
    Level topLevel;
    HashTable topTable;
    HashTable cached;
    DWORD tablesPerLevel[3];

    DWORD flags;

    // Description: read the file listing from the file
    void ReadFileListing();

    // Description: set the out buffer to the sha1 of the block
    void HashBlock(BYTE *block, BYTE *outBuffer);

    // Description: swap the table used so there is a backup of the data modified
    void SwapTable(DWORD index, Level lvl);

    // Description: extract a block's data
    void ExtractBlock(DWORD blockNum, BYTE *data, DWORD length = 0x1000);

    // Description: convert a block number into a true block number, where the first block is the first hash table
    DWORD ComputeBackingDataBlockNumber(DWORD blockNum);

    // Description: get a block's hash entry
    HashEntry GetBlockHashEntry(DWORD blockNum);

    // Description: get the true block number for the hash table that hashes the block at the level passed in
    DWORD ComputeLevelNBackingHashBlockNumber(DWORD blockNum, Level level);

    // Description: get the true block number for the hash table that hashes the block at level 0
    DWORD ComputeLevel0BackingHashBlockNumber(DWORD blockNum);

    // Description: get the true block number for the hash table that hashes the block at level 1 (female)
    DWORD ComputeLevel1BackingHashBlockNumber(DWORD blockNum);

    // Description: get the true block number for the hash table that hashes the block at level 2
    DWORD ComputeLevel2BackingHashBlockNumber(DWORD blockNum);

    // Descrption: get the address of a hash table in the package
    DWORD GetHashTableAddress(DWORD index, Level lvl);

    // Description: get the base address of a hash table in the package
    DWORD GetBaseHashTableAddress(DWORD index, Level lvl);

    // Description: get the amount of hash entries in a hash table
    DWORD GetHashTableEntryCount(DWORD index, Level lvl);

    // Description: get the hash table at the current index on the specified level
    HashTable GetLevelNHashTable(DWORD index, Level lvl);

    // Description: build the table in memory for preperation to Write
    void BuildTableInMemory(HashTable *table, BYTE *outBuffer);

    // Description: add the file entry to the file listing
    void AddToListing(StfsFileListing *fullListing, StfsFileListing *out);

    // Description: get the file entry from a file's path
    void GetFileEntry(vector<string> locationOfFile, StfsFileListing *start, StfsFileEntry *out,
            StfsFileEntry *newEntry = NULL, bool updateEntry = false, bool checkFolders = false);

    // Description: get the raw file listing
    void GenerateRawFileListing(StfsFileListing *in, vector<StfsFileEntry> *outFiles,
            vector<StfsFileEntry> *outFolders);

    // Description: split a string into multiple substrings
    vector<string> SplitString(string str, string delimeter);

    // Description: get the hash address in the parent hash table to the corresponding table
    DWORD GetTableHashAddress(DWORD index, Level lvl);

    // Description: set the status of the block in the corresponding hash table
    void SetBlockStatus(DWORD blockNum, BlockStatusLevelZero status);

    // Description: set the next block of the current block
    void SetNextBlock(DWORD blockNum, INT24 nextBlockNum);

    // Description: discard the current file listing and reWrite it
    void WriteFileListing(bool usePassed = false, vector<StfsFileEntry> *outFis = NULL,
            vector<StfsFileEntry> *outFos = NULL);

    // Description: Write a file entry at the io's current position
    void WriteFileEntry(StfsFileEntry *entry);

    // Description: allocate a data block in the package, and return a block number
    INT24 AllocateBlock();

    // Description: allocate 'blockCount' consecutive data blocks
    INT24 AllocateBlocks(DWORD blockCount);

    // Description: calculate the level of the topmost hash table
    Level CalcualateTopLevel();

    // Description: get the file listing that matches the directory path
    void FindDirectoryListing(vector<string> locationOfDirectory, StfsFileListing *start,
            StfsFileListing **out);

    // Description: update the entry at the given path
    void UpdateEntry(string pathInPackage, StfsFileEntry entry);

    // Description: get the number of bytes to skip over the hash table
    DWORD GetHashTableSkipSize(DWORD tableAddress);

    // Description: get the number of blocks until the next hash table
    DWORD GetBlocksUntilNextHashTable(DWORD currentBlock);

    // Description: parse the file
    void Parse();

    // Description: initializes the object
    void Init();

    // Description: close the io/cleanup resources
    void Cleanup();
};
