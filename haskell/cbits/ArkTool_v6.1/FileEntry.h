// 
// FileEntry objects store the size, location and name of a file
// inside an ark and hdr file. The contents of a file entry can
// be found inside the ark (which is refered to as an "internal" file)
// or in a file on the pc (which is refered as an "external" file).
// 
// When a file is encrypted, it has a 4 byte header for the crypt "key".
// When dealing with these files the 4 bytes is not included in the 
// Filesize() but it is in the Arksize()returned by "size()" but the "encrypted()" function will tell
// you that it should be handled is required.
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _FILE_ENTRY_H_
#define _FILE_ENTRY_H_

#include "types.h"


class FileEntry
{
public:
	// create a file entry that is inside ark file
	FileEntry(const char* arkname, s64 arksize, s64 offset, bool encrypted=false);
	// create a file entry for an external file
	FileEntry(const char* arkname, const char* filename, s64 filesize, s64 offset, bool encrypted=false);
	FileEntry(FileEntry const& source);
	FileEntry& operator=(FileEntry const& source);
	~FileEntry();
	
	bool operator<(FileEntry const& right) const;
	bool operator==(FileEntry const& right) const;
	bool operator!=(FileEntry const& right) const;
	
	const char* Filename() const{ return mFilename; }
	const char* Arkname() const	{ return mArkname; }
	s64  Filesize() const		{ return mFilesize; }	// size of file on pc
	s64  Arksize() const		{ return mArksize; }	// size of file inside ark
	s64  Offset() const			{ return mOffset; }
	bool Encrypted() const		{ return mEncrypted; }
	
	// returns:	true if file is external to the ark
	bool IsExternal() const		{ return mFilename != NULL; }
	
private:
	char* mFilename;
	char* mArkname;
	s64  mFilesize;
	s64  mArksize;
	s64  mOffset;
	bool mEncrypted;
};


#endif // _FILE_ENTRY_H_

