// 
// This is an object to handle the hdr and ark files together.
// This is the only object you should need to explicitly use in your code
// (besides the FileEntry objects this returns when iterating through
// all file entries)
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#include <cstring>

#include "types.h"
#include "ArkHdrPair.h"
#include <sys/stat.h>


ArkHdrPair::ArkHdrPair()
	: mHasChanged(false)
	, mNewStyle(true)
	, mVersion(3)
	, mArkDirpath(NULL)
	, mMaxArkSize(0)
{
}

ArkHdrPair::~ArkHdrPair()
{
	Close();
}


// check if the ark/hdr has changed and so requires
// saving with save or save as
// 
// returns:	true if has changed and requires saving
bool ArkHdrPair::HasChanged() const
{
	return mHasChanged;
}


// create a new ark and hdr in a given dir
// 
// args:	name of the dir to create ark and hdr in
//			max size of ark files when splitting into multiple files (0 = no max size limit)
//			flag for whether the ark/hdr are of the "new style"
//				(new style means a new type of encryption and encrypted hdr files)
//				true  = ps2: rockband  x360: gh2, rockband
//				false = ps2: gh1, gh2, gh80s  xbox: kr
//			version of hdr/ark files to create
//				3 = ps2: gh1, gh2, gh80s  xbox: kr  x360: gh2, rockband(usa)
//				4 = ps2: rockband
// returns:	true if successfully created ark files
bool ArkHdrPair::New(const char* arkDirpath, s64 maxArkSize, bool newStyle, int version)
{
	Close();
	mHasChanged = false;
	
	mNewStyle = newStyle;
	mVersion = version;
	mMaxArkSize = maxArkSize;
	
	// no need to create any header, they are created when they are needed
	
	// create ark files
	if( !mArkFile.New(arkDirpath, mMaxArkSize, mNewStyle) )
		return false;
	
	// success
	mArkDirpath = new char[strlen(arkDirpath)+1];
	strcpy(mArkDirpath, arkDirpath);
	return true;
}
// easy ways to create new files for specific games
bool ArkHdrPair::NewPs2Gh1(const char* arkDirpath)		{ return New(arkDirpath, 0, false, 3); }
bool ArkHdrPair::NewPs2Gh2(const char* arkDirpath)		{ return New(arkDirpath, 0, false, 3); }
bool ArkHdrPair::NewPs2Gh80s(const char* arkDirpath)	{ return New(arkDirpath, 0, false, 3); }
bool ArkHdrPair::NewPs2Rockband(const char* arkDirpath)	{ return New(arkDirpath, ARK_SIZE_2GB, true,  4); }
bool ArkHdrPair::NewPs3Rockband(const char* arkDirpath)	{ return New(arkDirpath, ARK_SIZE_2GB, true,  3); }
bool ArkHdrPair::NewXboxKr(const char* arkDirpath)		{ return New(arkDirpath, ARK_SIZE_100MB, false, 3); }
bool ArkHdrPair::NewX360Gh2(const char* arkDirpath)		{ return New(arkDirpath, 0, true,  3); }
bool ArkHdrPair::NewX360Rockband(const char* arkDirpath){ return New(arkDirpath, ARK_SIZE_2GB, true,  3); }


// open an ark and hdr from the given dir
// 
// args:	name of the dir to open the ark and hdr from
//			flag for whether ark file is for xbox360 or not
// returns:	true if successfully opened ark files
bool ArkHdrPair::Open(const char* arkDirpath)
{
	Close();
	mHasChanged = false;
	
	// open header and read it in
	std::vector<s64> ark_sizes;
	char hdr_filename[260];
	sprintf(hdr_filename, "%s%cmain.hdr", arkDirpath, DIRSEPCHAR);
	if( !mHdrFile.Read(hdr_filename, mFileCollection, ark_sizes, mNewStyle, mVersion) )
		return false;
	
	// work out max ark size
	// if only 1 ark, we assume no max size
	// otherwise assume the max is the size of the largest ark
	mMaxArkSize = 0;
	if(ark_sizes.size() > 1)
	{
		for(int i=0; i<(int)ark_sizes.size(); i++)
		{
			if(mMaxArkSize < ark_sizes[i])
				mMaxArkSize = ark_sizes[i];
		}
	}
	
	// open ark files
	if( !mArkFile.Open(arkDirpath, mMaxArkSize, mNewStyle) )
		return false;
	
	// success
	mArkDirpath = new char[strlen(arkDirpath)+1];
	strcpy(mArkDirpath, arkDirpath);
	return true;
}


// save any changes to the ark and hdr files
// (this writes all changes to disk)
// if this isnt called no changes will be made to the ark file
// 
// returns:	true if saved successfully
bool ArkHdrPair::Save()
{
	if( !mArkFile.IsOpen() )
		return false;
//	if( !mHasChanged )
//		return true;
	
	// write all new (external) files to ark file
	FileEntrySetIter iter;
	const FileEntry* p_entry = mFileCollection.First(iter);
	while( p_entry != NULL )
	{
		if( p_entry->IsExternal() )
		{
			// write file into arkfile
			if( !mArkFile.WriteFile(*p_entry) )
				return false;
			
			bool is_encrypted = strstr(p_entry->Arkname(), ".dtb")!=0;
			FileEntry internal_entry(p_entry->Arkname(), p_entry->Arksize(), p_entry->Offset(), is_encrypted);
			
			FileEntrySetIter prev_iter = iter;
			p_entry = mFileCollection.Next(iter);
			
			mFileCollection.Remove(prev_iter);
			if( !mFileCollection.Insert(internal_entry) )
				return false;
		}
		else
		{
			p_entry = mFileCollection.Next(iter);
		}
	}
	
	// get sizes of ark files
	std::vector<s64> ark_sizes;
	if( !mArkFile.GetArkSizes(ark_sizes) )
		return false;
	
	// write out hdr
	char hdr_filename[260];
	sprintf(hdr_filename, "%s%cmain.hdr", mArkDirpath, DIRSEPCHAR);
	if( !mHdrFile.Write(hdr_filename, mFileCollection, ark_sizes, mNewStyle, mVersion) )
		return false;
	
	// success
	mHasChanged = false;
	return true;
}

// save any changes to the ark and hdr files to new ark and hdr files
// (this writes all changes to disk without altering the original ark and header files)
// if this isnt called no changes will be made to the ark file
// 
// Note: You cannot "save as" to the same files you originally opened
// 
// args:	dirpath to save new ark and hdr files to
//			flag for whether to remove any gaps from the ark
// returns:	true if saved successfully
bool ArkHdrPair::SaveAs(const char* newArkDirpath, bool removeGaps)
{
	if( !mArkFile.IsOpen() )
		return false;
	
	// you cannot "save as" to the same files
	if( strcmp(newArkDirpath, mArkDirpath) == 0 )
		return false;
	
	char temp_filename[260];
	sprintf(temp_filename, "%s%cdummy", newArkDirpath, DIRSEPCHAR);
	CreateBaseDirs(temp_filename);
	
	ArkFile new_ark;
	if( !new_ark.New(newArkDirpath, mMaxArkSize, mNewStyle) )
		return false;
	
	// write all files to new ark file
	s64 curr_offset = 0;
	FileEntrySetIter iter;
	const FileEntry* p_entry = mFileCollection.First(iter);
	while( p_entry != NULL )
	{
		// copy from this ark into a new ark
		FileEntry dest_entry(p_entry->Arkname(), p_entry->Filename(), p_entry->Filesize(),
							((removeGaps)?curr_offset:p_entry->Offset()),
							p_entry->Encrypted());
		if( !mArkFile.CopyFrom(new_ark, dest_entry, *p_entry) )
			return false;
		
		// update the file-entry
		if( p_entry->IsExternal() || removeGaps )
		{
			curr_offset += p_entry->Arksize();
			
			bool is_encrypted = (p_entry->IsExternal())
								? (strstr(dest_entry.Arkname(), ".dtb")!=0)
								: (dest_entry.Encrypted());
			FileEntry new_entry(dest_entry.Arkname(), dest_entry.Arksize(),
								dest_entry.Offset(), is_encrypted);
			
			FileEntrySetIter prev_iter = iter;
			p_entry = mFileCollection.Next(iter);
			
			mFileCollection.Remove(prev_iter);
			if( !mFileCollection.Insert(new_entry) )
				return false;
		}
		else
		{
			p_entry = mFileCollection.Next(iter);
		}
	}
	
	// get sizes of ark files
	std::vector<s64> ark_sizes;
	if( !new_ark.GetArkSizes(ark_sizes) )
		return false;
	
	// write out header
	char hdr_filename[260];
	sprintf(hdr_filename, "%s%cmain.hdr", newArkDirpath, DIRSEPCHAR);
	if( !mHdrFile.Write(hdr_filename, mFileCollection, ark_sizes, mNewStyle, mVersion) )
		return false;
	
	// success, so set the new ark file as the current ark file
	new_ark.Close();
	if( !mArkFile.Open(newArkDirpath, mMaxArkSize, mNewStyle) )
		return false;
	mHasChanged = false;
	return true;
}


// close existing ark and hdr files
void ArkHdrPair::Close()
{
	mFileCollection.Clear();
	mArkFile.Close();
	if(mArkDirpath)
	{
		delete[] mArkDirpath;
		mArkDirpath = NULL;
	}
	mMaxArkSize = 0;
	mHasChanged = false;
}


// use there to iterate through all file entries in an ark and hdr.
// you can't alter anything with these pointers, they're just for information gathering
// 
// args:	iterator for iterating through file entries
//			search path for the file entries to return
// returns:	unmodifiable pointer to file entry if it exists for the given search path
//			NULL if no more files exist for the given search path
const FileEntry* ArkHdrPair::First(FileEntrySetIter& rIter, const char* searchFilepath) const
{
	for(const FileEntry* p_entry=mFileCollection.First(rIter); p_entry!=NULL; p_entry=mFileCollection.Next(rIter))
	{
		if( StrcmpWildcard(p_entry->Arkname(), searchFilepath, false) == 0 )
		{
			return p_entry;
		}
	}
	return NULL;
}
const FileEntry* ArkHdrPair::Next(FileEntrySetIter& rIter, const char* searchFilepath) const
{
	for(const FileEntry* p_entry=mFileCollection.Next(rIter); p_entry!=NULL; p_entry=mFileCollection.Next(rIter))
	{
		if( StrcmpWildcard(p_entry->Arkname(), searchFilepath, false) == 0 )
		{
			return p_entry;
		}
	}
	return NULL;
}


// extracts a file from the ark/hdr
// 
// args:	external filename of file on pc that you want to extract to
//			internal filename to insert as in the ark and hdr
//			flag for whether to decrypt files flagged as encrypted
// returns:	true if extracted successfully
bool ArkHdrPair::GetFile(const char* destFilepath, const char* arkFilename, bool performDecrypts)
{
	// no need to search using First and Next as if the filename exists
	// it will be found first go and returned first go
	FileEntrySetIter iter;
	const FileEntry* p_entry = First(iter, arkFilename);
	if(p_entry == NULL)
		return false;
	
	// found file, so read it out
	CreateBaseDirs(destFilepath);
	if( !mArkFile.ReadFile(destFilepath, *p_entry, performDecrypts) )
		return false;
	return true;
}

// adds a file into the ark and hdr
// 
// args:	external filename of file on pc that you want to insert
//			internal filename to insert as in the ark and hdr
//			flag for whether to encrypt files when adding them
//				true:	to encrypt "*.dtb" files when they are added
//				false:	if "*.dtb" files being added are already encrypted
// returns:	true if inserted successfully
bool ArkHdrPair::AddFile(const char* srcFilepath, const char* arkFilename, bool performEncrypts)
{
	// ensure file doesnt already exist in ark
	FileEntrySetIter iter;
	const FileEntry* p_entry = First(iter, arkFilename);
	if(p_entry != NULL)
		return false;
	
	// file doesnt already exist, so add it
	bool encrypted = performEncrypts && (strstr(arkFilename, ".dtb") != 0);
	s64 filesize = GetFilesize(srcFilepath);
	if(filesize < 0)
		return false;
	FileEntry entry(arkFilename, srcFilepath, filesize, 0, encrypted);
	mFileCollection.Add(entry);
	mHasChanged = true;
	return true;
}

// remove a file that exists in the ark and hdr
// 
// args:	filename to remove from the ark and hdr
// returns:	true if removed successfully
//			false otherwise
bool ArkHdrPair::RemoveFile(const char* arkFilename)
{
	// no need to search using First and Next as if the filename exists
	// it will be found first go and returned first go
	FileEntrySetIter iter;
	const FileEntry* p_entry = First(iter, arkFilename);
	if(p_entry == NULL)
		return false;
	
	// found file, so remove it
	mFileCollection.Remove(iter);
	mHasChanged = true;
	return true;
}

// replace an existing file in the ark with a file from the pc
// 
// (This had to be renamed from "ReplaceFile" due to window's stupid
// defines that changed it into ReplaceFileA, therefore causing
// linking errors cause it couldnt find a method called ReplaceFileA)
// 
// args:	external filename of file on pc that you want to insert
//			internal filename to insert as in the ark and hdr
//			flag for whether to encrypt files flagged as encrypted
// returns:	true if replaced successfully
bool ArkHdrPair::ReplaceAFile(const char* srcFilepath, const char* arkFilename, bool performEncrypts)
{
	return	RemoveFile(arkFilename) &&
			AddFile(srcFilepath, arkFilename, performEncrypts);
}

// rename a file from oldArkFilename to newArkFilename
// you can cahnge the full filepath not just the filename using this
// 
// args:	new ark filename
//			old ark filename
// returns:	true if renamed successfully
bool ArkHdrPair::RenameFile(const char* newArkFilename, const char* oldArkFilename)
{
	// get old file entry
	// no need to search using First and Next as if the filename exists
	// it will be found first go and returned first go
	FileEntrySetIter iter;
	const FileEntry* p_entry = First(iter, oldArkFilename);
	if(p_entry == NULL)
		return false;
	
	// create new file entry
	FileEntry new_entry(newArkFilename, p_entry->Filename(),
		p_entry->Filesize(), p_entry->Offset(), p_entry->Encrypted());
	
	// replace file entry
	mFileCollection.Remove(iter);
	mHasChanged = true;
	return mFileCollection.Insert(new_entry);
}


// prints info about all files in the hdr and ark
// 
// args:	filepath to search for and print
//			stream to print to
void ArkHdrPair::Print(const char* searchFilepath, FILE* pStream) const
{
	int entry_count = 0;
	fprintf(pStream, "\n  Offset      Size  Path\n\n");
	FileEntrySetIter iter;
	for(const FileEntry* p_entry=First(iter, searchFilepath); p_entry!=NULL; p_entry=Next(iter, searchFilepath))
	{
		fprintf(pStream, "%8llX  %8llX  %s\n",
			p_entry->Offset(), p_entry->Arksize(),
			p_entry->Arkname() );
		entry_count++;
	}
	fprintf(pStream, "\n  %d files found\n", entry_count);
}


// extracts all files that fit the filename
// 
// args:	directory to extract to
//			filepaths to extract (can use * and ? wildcards)
//			flag for whether to decrypt files flagged as encrypted
//			stream to print extraction info to
// returns:	number of files extracted
int ArkHdrPair::ExtractFiles(const char* extractDirname, const char* searchFilepath,
							 bool performDecrypts, FILE* pStream)
{
	int entry_count = 0;
	FileEntrySetIter iter;
	for(const FileEntry* p_entry=First(iter, searchFilepath); p_entry!=NULL; p_entry=Next(iter, searchFilepath))
	{
		char dest_filepath[260];
		HdrPathToFilepath(dest_filepath, extractDirname, p_entry->Arkname());
		if(pStream)
			fprintf(pStream, "%s\n", dest_filepath);
		CreateBaseDirs(dest_filepath);
		if( !mArkFile.ReadFile(dest_filepath, *p_entry, performDecrypts) )
			return -1;
		entry_count++;
	}
	return entry_count;
}



s64 ArkHdrPair::GetFilesize(const char* filename) const
{
	FILE* fd = fopen(filename, "rb");
	if(fd == NULL)
		return -1;
	if( FSEEK64(fd, 0, SEEK_END) ) { fclose(fd); return -1; }
	s64 size = FTELL64(fd);
	fclose(fd);
	return size;
}

void ArkHdrPair::HdrPathToFilepath(char* filepath, const char* basedir, const char* hdrpath) const
{
	if( memcmp("../../", hdrpath, 6) == 0)
		hdrpath += 6;
	
	sprintf(filepath, "%s/%s", basedir, hdrpath );
	
	// convert '/' into '\\'
	for(int i=0; filepath[i]!='\0'; i++)
	{
		if(filepath[i] == '/')
			filepath[i] = DIRSEPCHAR;
	}
}

void ArkHdrPair::CreateBaseDirs(const char* baseDirpath) const
{
	// ensure base dirs exist
	char dirpath[260];
	for(int i=0; baseDirpath[i]!='\0'; i++)
	{
		if( (baseDirpath[i] == '\\') || (baseDirpath[i] == '/') )
		{
			strcpy(dirpath, baseDirpath);
			dirpath[i] = '\0';
			MKDIR(dirpath);
		}
	}
}


// compare a wildcard string and a normal string
// (works like normal strcmp)
//
// args:	normal string (contains no wildcard characters)
//			wildcard string (may contain wildcard characters)
//			true for case sensitive compare
// returns:	0 if strings are 'equal'
//			not equal otherwise
int  ArkHdrPair::StrcmpWildcard(const char* normalString, const char* wildcardString, bool caseSensitive) const
{
	int normalStringOffset, wildcardStringOffset;
	int normalStringLength   = (int)strlen(normalString);
	int wildcardStringLength = (int)strlen(wildcardString);
	
	// if wildcard string == '*' then it is equal to any string
	if(strcmp(wildcardString, "*") == 0)
		return 0;
	
	// scan thru strings till the end of either string is reached
	for(wildcardStringOffset=0, normalStringOffset=0;
		(wildcardStringOffset<wildcardStringLength) && (normalStringOffset<normalStringLength); )
	{
		// '?' is considered equal to exactly 1 char
		if(wildcardString[wildcardStringOffset] == '?')
		{
			// skip 1 char in both strings without testing for equality
			wildcardStringOffset++;
			normalStringOffset++;
		}
		// '*' is considered equal to '0 to infinite' chars
		else if(wildcardString[wildcardStringOffset] == '*')
		{
			// skip the one or more '*' characters in a row
			while(wildcardString[wildcardStringOffset+1] == '*')
				wildcardStringOffset++;
			// if '*' is the last char in the wildcard string,
			// and up till now the strings have been equal,
			// then the strings are equal
			if(wildcardStringOffset == wildcardStringLength-1)
				return 0;
			// otherwise test the rest of the string for wildcard equality
			if(StrcmpWildcard(&normalString[normalStringOffset], &wildcardString[wildcardStringOffset+1], caseSensitive) == 0)
				return 0;
			normalStringOffset++;
		}
		// if normal character in each filename check for equality
		else if(wildcardString[wildcardStringOffset] == normalString[normalStringOffset])
		{
			wildcardStringOffset++;
			normalStringOffset++;
		}
		// else the strings arent equal
		else
		{
			return 2;
		}
	}
	
	// remove any trailing '*' characters
	while(wildcardString[wildcardStringOffset] == '*')
		wildcardStringOffset++;
	
	// if the ends of both strings are reached at once
	// without returning an error, then strings must be equal
	if((wildcardStringOffset==wildcardStringLength) && (normalStringOffset==normalStringLength))
		return 0;
	
	// if not equal, then return error
	return 1;
}

