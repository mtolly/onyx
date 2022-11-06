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

#include "FileEntry.h"
#include <string.h>


// file entry that is inside ark file
FileEntry::FileEntry(const char* arkname, s64 arksize, s64 offset, bool encrypted)
	: mFilename(NULL)
	, mArkname(NULL)
	, mFilesize(arksize)
	, mArksize(arksize)
	, mOffset(offset)
	, mEncrypted(encrypted)
{
	if(arkname)
	{
		mArkname = new char[strlen(arkname)+1];
		strcpy(mArkname, arkname);
	}
	if(mEncrypted)
	{
		mFilesize = mArksize - 4;
	}
}

// file entry for an external file
FileEntry::FileEntry(const char* arkname, const char* filename, s64 filesize, s64 offset, bool encrypted)
	: mFilename(NULL)
	, mArkname(NULL)
	, mFilesize(filesize)
	, mArksize(filesize)
	, mOffset(offset)
	, mEncrypted(encrypted)
{
	if(arkname)
	{
		mArkname = new char[strlen(arkname)+1];
		strcpy(mArkname, arkname);
	}
	if(filename)
	{
		mFilename = new char[strlen(filename)+1];
		strcpy(mFilename, filename);
	}
	if(mEncrypted)
	{
		mArksize = mFilesize + 4;
	}
}

FileEntry::FileEntry(FileEntry const& source)
	: mFilename(NULL)
	, mArkname(NULL)
	, mFilesize(source.mFilesize)
	, mArksize(source.mArksize)
	, mOffset(source.mOffset)
	, mEncrypted(source.mEncrypted)
{
	if(source.mArkname)
	{
		mArkname = new char[strlen(source.mArkname)+1];
		strcpy(mArkname, source.mArkname);
	}
	if(source.mFilename)
	{
		mFilename = new char[strlen(source.mFilename)+1];
		strcpy(mFilename, source.mFilename);
	}
}

FileEntry& FileEntry::operator=(FileEntry const& source)
{
	// watch out for self assignment
	if(this != &source)
	{
		if(mFilename)
		{
			delete[] mFilename;
			mFilename = NULL;
		}
		if(source.mFilename)
		{
			mFilename = new char[strlen(source.mFilename)+1];
			strcpy(mFilename, source.mFilename);
		}
		
		if(mArkname)
		{
			delete[] mArkname;
			mArkname = NULL;
		}
		if(source.mArkname)
		{
			mArkname = new char[strlen(source.mArkname)+1];
			strcpy(mArkname, source.mArkname);
		}
		
		mFilesize = source.mFilesize;
		mArksize = source.mArksize;
		mOffset = source.mOffset;
		mEncrypted = source.mEncrypted;
	}
	
	return *this;
}

FileEntry::~FileEntry()
{
	if(mFilename)
	{
		delete[] mFilename;
		mFilename = NULL;
	}
	if(mArkname)
	{
		delete[] mArkname;
		mArkname = NULL;
	}
}


bool FileEntry::operator<(FileEntry const& right) const
{
	// watch out for self comparison??
	return Offset() < right.Offset();
}

bool FileEntry::operator==(FileEntry const& right) const
{
	return Offset() == right.Offset();
}
bool FileEntry::operator!=(FileEntry const& right) const
{
	return Offset() != right.Offset();
}
