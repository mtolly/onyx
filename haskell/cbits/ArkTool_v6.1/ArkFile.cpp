// 
// ark file object
// handles reading and writing ark files
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#include "ArkFile.h"
#include "DtbCrypt.h"
#include "SongCrypt.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>

#define min(a, b) ((a) < (b) ? (a) : (b))


ArkFile::ArkFile()
	: mMaxArkSize(0)
	, mNewEncryption(true)
	, mDirname(NULL)
	, mpWorkBuff(NULL)
{
	mpWorkBuff = new unsigned char[MS_WORK_BUFFER_SIZE];
}

ArkFile::~ArkFile()
{
	delete[] mpWorkBuff;
	mpWorkBuff = NULL;
	Close();
}


// create a new ark in a given dirname
// 
// args:	name of the dir to create ark in
//			max size of ark files when splitting into multiple files (0 = no max size limit)
//			flag for whether the new type of encryption is used
// returns:	true if successfully created ark files
bool ArkFile::New(const char* dirname, s64 maxArkSize, bool newEncryption)
{
	Close();
	
	char filename[260];
	sprintf(filename, "%s%cmain_%d.ark", dirname, DIRSEPCHAR, 0);
	FILE* fd = fopen(filename, "w+b");
	if(fd == NULL)
		return false;
	
	mMaxArkSize = maxArkSize;
	mNewEncryption = newEncryption;
	mDirname = new char[strlen(dirname)+1];
	strcpy(mDirname, dirname);
	mArkHandles.push_back(fd);
	return true;
}

// open an ark in a given dirname
// 
// args:	name of the dir that contains the ark files
//			max size of ark files when splitting into multiple files (0 = no max size limit)
//			flag for whether the new type of encryption is used
// returns:	true if successfully opened ark files
bool ArkFile::Open(const char* dirname, s64 maxArkSize, bool newEncryption)
{
	Close();

	char filename[260];
	for(int i=0; 1; i++)
	{
		sprintf(filename, "%s%cmain_%d.ark", dirname, DIRSEPCHAR, i);
		FILE* fd = fopen(filename, "r+b");
		if(fd == NULL)
			break;
		mArkHandles.push_back(fd);
	}
	
	// should no ark files be allowed?
	// if they are allowed then you can print without requiring ark files
//	if( mArkHandles.empty() )
//		return false;
	
	mMaxArkSize = maxArkSize;
	mNewEncryption = newEncryption;
	mDirname = new char[strlen(dirname)+1];
	strcpy(mDirname, dirname);
	return true;
}


// check if ark file is currently open
// 
// returns:	true if ark files are currently open
bool ArkFile::IsOpen() const
{
	return !mArkHandles.empty();
}

// close ark file
void ArkFile::Close()
{
	mMaxArkSize = 0;
	mNewEncryption = true;
	while( !mArkHandles.empty() )
	{
		FILE* fd = mArkHandles.back();
		mArkHandles.pop_back();
		fclose(fd);
	}
	if(mDirname)
	{
		delete[] mDirname;
		mDirname = NULL;
	}
}


// write a file into ark
// (upon adding, all files flagged as encrypted will be encrypted)
bool ArkFile::WriteFile(const FileEntry& entry)
{
	if( !IsOpen() )
		return false;
	
	// file must be external in order to write it into ark
	if( !entry.IsExternal() )
		return false;
	
	FILE* src_fd = fopen(entry.Filename(), "rb");
	if(src_fd == NULL)
		return false;
	
	bool result = WriteFile(src_fd, entry);
	
	fclose(src_fd);
	return result;
}

bool ArkFile::WriteFile(FILE* srcFd, const FileEntry& entry)
{
	if( !IsOpen() )
		return false;
	
	// this gets a handle to the correct file at the correct offset
	FILE* ark_fd = GetHandleFromOffset(entry.Offset(), false);
	if(ark_fd == NULL)
		return false;
	
	if( entry.Encrypted() )
	{
		// file needs to be encrypted
		unsigned char* temp = new unsigned char[(int)entry.Arksize()];
		*(int*)temp = G_CRYPT_KEY;
		if( fread( temp+4, 1, (int)entry.Filesize(), srcFd) != entry.Filesize() )
		{
			delete[] temp;
			return false;
		}
		if(mNewEncryption)
			dtb_crypt_new(temp, (int)entry.Arksize());
		else
			dtb_crypt_old(temp, (int)entry.Arksize());
		if( fwrite(temp, 1, (int)entry.Arksize(), ark_fd) != entry.Arksize() )
		{
			delete[] temp;
			return false;
		}
		delete[] temp;
		return true;
	}
	else
	{
		for(s64 i=0; i<entry.Arksize(); i+=MS_WORK_BUFFER_SIZE)
		{
			int read_size = (int)(min(entry.Arksize()-i, MS_WORK_BUFFER_SIZE));
			if( fread( mpWorkBuff, 1, read_size, srcFd) != read_size ||
				fwrite(mpWorkBuff, 1, read_size, ark_fd) != read_size )
			{
				return false;
			}
		}
		return true;
	}
}


// reads a file from ark into "destFilename"
bool ArkFile::ReadFile(const char* destFilename, const FileEntry& entry, bool performDecrypts)
{
	if( !IsOpen() )
		return false;
	
	// open destination file
	FILE* dest_fd = fopen(destFilename, "w+b");
	if(dest_fd == NULL)
		return false;
	
	bool result = ReadFile(dest_fd, entry, performDecrypts);
	
	fclose(dest_fd);
	return result;
}

// reads a file from ark into currently open file descriptor "destFd"
bool ArkFile::ReadFile(FILE* destFd, const FileEntry& entry, bool performDecrypts)
{
	if( !IsOpen() )
		return false;
	
	if(destFd == NULL)
		return false;
	
	// test for possible encrypted song files
	bool is_mogg_file	= performDecrypts && (strstr(entry.Arkname(), ".mogg") != 0);
	bool is_pss_file	= performDecrypts && (strstr(entry.Arkname(), ".pss") != 0);
	bool is_vgs_file	= performDecrypts && (strstr(entry.Arkname(), ".vgs") != 0);
	
	if( entry.IsExternal() )
	{
		// read out file from external file on pc
		FILE* src_fd = fopen(entry.Filename(), "rb");
		if(src_fd == NULL)
			return false;

		s64 filesize;
		if( !performDecrypts && entry.Encrypted() )
		{
			// external file is in decrypted form on pc
			// but is required to be read out in encrypted form
			// this occurs when an external file is added to an ark then the ark is "saved as"
			unsigned char* temp = new unsigned char[(int)entry.Arksize()];
			memcpy(temp, &G_CRYPT_KEY, 4);
			if( fread( temp+4, 1, (int)entry.Filesize(), src_fd) != entry.Filesize() )
			{
				delete[] temp;
				return false;
			}
			if( mNewEncryption )
				dtb_crypt_new(temp, (int)entry.Arksize());
			else
				dtb_crypt_old(temp, (int)entry.Arksize());
			if( fwrite(temp, 1, (int)entry.Arksize(), destFd) != entry.Arksize() )
			{
				delete[] temp;
				return false;
			}
			delete[] temp;
			return true;
		}
		else if( performDecrypts && entry.Encrypted() )
		{
			// external file is in decrypted form on pc
			// but is required to be read out in decrypted form
			// so just read it straight out
			filesize = entry.Filesize();
		}
		else if( !entry.Encrypted() )
		{
			// external file either isnt encrypted
			filesize = entry.Arksize();
		}
		for(s64 i=0; i<filesize; i+=MS_WORK_BUFFER_SIZE)
		{
			int read_size = (int)(min(filesize-i, MS_WORK_BUFFER_SIZE));
			if( fread( mpWorkBuff, 1, read_size, src_fd) != read_size ||
				fwrite(mpWorkBuff, 1, read_size, destFd) != read_size )
			{
				fclose(src_fd);
				return false;
			}
		}
		
		bool read_result = true;
		
		// decrypt output song files if they need to be
		if(is_mogg_file)
		{
			s64 end_file_loc = FTELL64(destFd);
			FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
			if( IsMoggEncrypted(destFd) )
				read_result = DecryptMogg(destFd);
			else
				FSEEK64(destFd, end_file_loc, SEEK_SET);
		}
		else if(is_pss_file)
		{
			s64 end_file_loc = FTELL64(destFd);
			FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
			if( IsPssEncrypted(destFd) )
				read_result = DecryptPss(destFd);
			else
				FSEEK64(destFd, end_file_loc, SEEK_SET);
		}
		else if(is_vgs_file)
		{
			s64 end_file_loc = FTELL64(destFd);
			FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
			if( IsVgsEncrypted(destFd) )
				read_result = DecryptVgs(destFd);
			else
				FSEEK64(destFd, end_file_loc, SEEK_SET);
		}
		
		// close external source file
		fclose(src_fd);
		return read_result;
	}
	else
	{
		// read out file from inside ark file
		FILE* src_fd = GetHandleFromOffset(entry.Offset(), true);
		if(src_fd == NULL)
			return false;
		
		if( performDecrypts && entry.Encrypted() )
		{
			// file needs to be decrypted
			unsigned char* temp = new unsigned char[(int)entry.Arksize()];
			if( fread( temp, 1, (int)entry.Arksize(), src_fd) != entry.Arksize() )
			{
				delete[] temp;
				return false;
			}
			if( mNewEncryption )
				dtb_crypt_new(temp, (int)entry.Arksize());
			else
				dtb_crypt_old(temp, (int)entry.Arksize());
			if( fwrite(temp+4, 1, (int)entry.Filesize(), destFd) != entry.Filesize() )
			{
				delete[] temp;
				return false;
			}
			delete[] temp;
			return true;
		}
		else
		{
			for(s64 i=0; i<entry.Arksize(); i+=MS_WORK_BUFFER_SIZE)
			{
				int read_size = (int)(min(entry.Arksize()-i, MS_WORK_BUFFER_SIZE));
				if( fread( mpWorkBuff, 1, read_size, src_fd) != read_size ||
					fwrite(mpWorkBuff, 1, read_size, destFd) != read_size )
				{
					return false;
				}
			}
			bool read_result = true;
			if(is_mogg_file)
			{
				s64 end_file_loc = FTELL64(destFd);
				FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
				if( IsMoggEncrypted(destFd) )
					read_result = DecryptMogg(destFd);
				else
					FSEEK64(destFd, end_file_loc, SEEK_SET);
			}
			else if(is_pss_file)
			{
				s64 end_file_loc = FTELL64(destFd);
				FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
				if( IsPssEncrypted(destFd) )
					read_result = DecryptPss(destFd);
				else
					FSEEK64(destFd, end_file_loc, SEEK_SET);
			}
			else if(is_vgs_file)
			{
				s64 end_file_loc = FTELL64(destFd);
				FSEEK64(destFd, -entry.Arksize(), SEEK_CUR);
				if( IsVgsEncrypted(destFd) )
					read_result = DecryptVgs(destFd);
				else
					FSEEK64(destFd, end_file_loc, SEEK_SET);
			}
			return read_result;
		}
	}
}


// copies a file from this ark into another ark
bool ArkFile::CopyFrom(ArkFile& rDestArk, const FileEntry& destEntry, const FileEntry& srcEntry)
{
	if( !IsOpen() )
		return false;
	
	// get a file descriptor pointing to the location in the destination
	// ark file at which the file should be written
	FILE* dest_fd = rDestArk.GetHandleFromOffset(destEntry.Offset(), false);
	if(dest_fd == NULL)
		return false;
	
	return ReadFile(dest_fd, srcEntry, false);
}
// copies a file from another ark into this ark
bool ArkFile::CopyInto(ArkFile& rSrcArk, const FileEntry& destEntry, const FileEntry& srcEntry)
{
	if( !IsOpen() )
		return false;
	
	// get a file descriptor pointing to the location in the source
	// ark file at which the file should be read from
	FILE* src_fd = rSrcArk.GetHandleFromOffset(srcEntry.Offset(), true);
	if(src_fd == NULL)
		return false;
	
	bool result = WriteFile(src_fd, destEntry);
	return result;
}


// get the sizes of the ark files
bool ArkFile::GetArkSizes(std::vector<s64>& rArkSizes) const
{
	if( !IsOpen() )
		return false;
	
	rArkSizes.clear();
	for(int i=0; i<(int)mArkHandles.size(); i++)
	{
		s64 size = GetFilesize(mArkHandles[i]);
		if(size <= 0 && i > 0)
			return false;
		rArkSizes.push_back(size);
	}
	return rArkSizes.size() > 0;
}


// from a given offset into an ark, the correct file handle is returned
// which is pointing at the correct offset in that file
// (will seek to a new offset outside of current bounds)
// 
// args:	offset into ark
//			flag or whether to seek outside existing file sizes
// returns:	file handle if successful
//			NULL if error
FILE* ArkFile::GetHandleFromOffset(s64 offset, bool offsetMustExist)
{
	s64 filesize;
	for(int i=0; i<(int)mArkHandles.size(); i++)
	{
		if((filesize=GetFilesize(mArkHandles[i])) < 0)
			return NULL;
		if(offset >= filesize)
		{
			// if there is still another ark file after this one
			// then update the offset to be in that ark file
			if(i+1 < (int)mArkHandles.size())
				offset -= filesize;
			else
				break;
		}
		else
		{
			FSEEK64(mArkHandles[i], offset, SEEK_SET);
			return mArkHandles[i];
		}
	}
	
	// offset is outside of the current file space
	if(offsetMustExist)
		return NULL;
	
	// check if we need to open a new ark file
	char zero[1] = {0};
	FILE* fd = NULL;
	if(offset >= mMaxArkSize &&
		mMaxArkSize != 0)
	{
		// pad the current ark file up to the max ark size limit,
		// then create a new ark file and seek to the required offset in it
		fd = mArkHandles[mArkHandles.size()-1];
		FSEEK64(fd, 0, SEEK_END);
		if(FTELL64(fd) < mMaxArkSize)
		{
			FSEEK64(fd, mMaxArkSize-1, SEEK_SET);
			fwrite(zero, 1, 1, fd);
		}
		offset -= mMaxArkSize;
		char new_filename[260];
		sprintf(new_filename, "%s%cmain_%d.ark", mDirname, DIRSEPCHAR, (int) mArkHandles.size());
		FILE* fd = fopen(new_filename, "w+b");
		if(fd == NULL)
			return NULL;
		mArkHandles.push_back(fd);
		if(offset > 0)
		{
			FSEEK64(fd, offset-1, SEEK_SET);
			fwrite(zero, 1, 1, fd);
		}
		return fd;
	}
	else
	{
		// according to posix specifications, an fseek followed by an fwrite
		// should make a file grow to the new end of file mark.
		// (check the file needs to grow first though)
		fd = mArkHandles[mArkHandles.size()-1];
		FSEEK64(fd, 0, SEEK_END);
		if(FTELL64(fd) < offset)
		{
			FSEEK64(fd, offset-1, SEEK_SET);
			fwrite(zero, 1, 1, fd);
		}
		else
		{
			FSEEK64(fd, offset, SEEK_SET);
		}
		return fd;
	}
}



// gets the size of the file without changing the current file position
// 
// args:	handle of file to get size of
// returns:	size of file if successful
//			< 0 if error
s64 ArkFile::GetFilesize(FILE* fd) const
{
	s64 curr_offset = FTELL64(fd);
	if( FSEEK64(fd, 0, SEEK_END) ) return -1;
	s64 size = FTELL64(fd);
	if( FSEEK64(fd, curr_offset, SEEK_SET) ) return -1;
	return size;
}

