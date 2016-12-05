// 
// ark file object
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _ARK_FILE_H_
#define _ARK_FILE_H_

#include "types.h"
#include "FileEntry.h"
#include <vector>


class ArkFile
{
public:
	ArkFile();
	~ArkFile();
	
	// create a new ark in a given dirname
	// 
	// args:	name of the dir to create ark in
	//			max size of ark files when splitting into multiple files (0 = no max size limit)
	//			flag for whether the new type of encryption is used
	// returns:	true if successfully created ark files
	bool New(const char* dirname, s64 maxArkSize=0, bool newEncryption=true);
	
	// open an ark in a given dirname
	// 
	// args:	name of the dir that contains the ark files
	//			max size of ark files when splitting into multiple files (0 = no max size limit)
	//			flag for whether the new type of encryption is used
	// returns:	true if successfully opened ark files
	bool Open(const char* dirname, s64 maxArkSize=0, bool newEncryption=true);
	
	// close all ark files and free any related resources
	void Close();
	
	// check if ark file is currently open
	// 
	// returns:	true if ark files are currently open
	bool IsOpen() const;
	
	
	// write a file into ark
	// (upon adding, all files flagged as encrypted will be encrypted)
	bool WriteFile(const FileEntry& entry);
	// write a file into ark from currently open file descriptor "srcFd"
	bool WriteFile(FILE* srcFd, const FileEntry& entry);
	
	// reads a file from ark into "destFilename"
	bool ReadFile(const char* destFilename, const FileEntry& entry, bool performDecrypts=true);
	// reads a file from ark into currently open file descriptor "destFd"
	bool ReadFile(FILE* destFd, const FileEntry& entry, bool performDecrypts=true);
	
	// copies a file from this ark into another ark
	bool CopyFrom(ArkFile& rDestArk, const FileEntry& destEntry, const FileEntry& srcEntry);
	// copies a file from another ark into this ark
	bool CopyInto(ArkFile& rSrcArk, const FileEntry& destEntry, const FileEntry& srcEntry);
	
	// get the sizes of the ark files
	bool GetArkSizes(std::vector<s64>& rArkSizes) const;
	
private:
	// from a given offset into an ark, the correct file handle is returned
	// which is pointing at the correct offset in that file
	// 
	// args:	offset into ark
	//			flag or whether to seek outside existing file sizes
	// returns:	file handle if successful
	//			NULL if error
	FILE* GetHandleFromOffset(s64 offset, bool offsetMustExist);
	
	// gets the size of the file without changing the current file position
	// 
	// args:	handle of file to get size of
	// returns:	size of file if successful
	//			< 0 if error
	s64 GetFilesize(FILE* fd) const;
	
	s64 mMaxArkSize;
	bool mNewEncryption;
	std::vector<FILE*> mArkHandles;		// ark file descriptors
	char* mDirname;						// dirname that ark files are in
	unsigned char* mpWorkBuff;			// buffer for file insertion/extraction
	static const int MS_WORK_BUFFER_SIZE = 1*1024*1024;
};


#endif // _ARK_FILE_H_

