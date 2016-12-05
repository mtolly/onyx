// 
// This is an object to handle the hdr and ark files together.
// This is the only object you should need to explicitly use in your code
// (besides the FileEntry objects this returns when iterating through
// all file entries)
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _ARK_HDR_PAIR_H_
#define _ARK_HDR_PAIR_H_

#include "FileCollection.h"
#include "ArkFile.h"
#include "HdrFile.h"
#include <vector>


class ArkHdrPair
{
public:
	ArkHdrPair();
	~ArkHdrPair();
	
	// yes i realise that 2GB is really 2*1024*1024*1024
	// but this leaves a bit of room for safety of getting close
	// to the limit. also i've not actually seen split ark files
	// be bigger than this anyway.
	static const int ARK_SIZE_2GB	= 2000000000;
	static const int ARK_SIZE_100MB	=  100000000;
	
	
	// check if the ark/hdr has changed and so requires
	// saving with save or save as
	// 
	// returns:	true if has changed and requires saving
	bool HasChanged() const;
	

	// create a new ark and hdr in a given dir
	// 
	// args:	name of the dir to create ark and hdr in
	//			max size of ark files when splitting into multiple files (0 = no max size limit)
	//			flag for whether the ark/hdr are of the "new style"
	//				(new style means a new type of encryption and encrypted hdr files)
	//				true  = ps2: rockband  x360: gh2, rockband  ps3: rockband
	//				false = ps2: gh1, gh2, gh80s  xbox: kr
	//			version of hdr/ark files to create
	//				3 = ps2: gh1, gh2, gh80s  xbox: kr  x360: gh2, rockband  ps3: rockband
	//				4 = ps2: rockband
	// returns:	true if successfully created ark files
	bool New(const char* arkDirpath, s64 maxArkSize=0, bool newStyle=true, int version=3);
	// easy ways to create new files for specific games
	bool NewPs2Gh1(const char* arkDirpath);
	bool NewPs2Gh2(const char* arkDirpath);
	bool NewPs2Gh80s(const char* arkDirpath);
	bool NewPs2Rockband(const char* arkDirpath);
	bool NewPs3Rockband(const char* arkDirpath);
	bool NewXboxKr(const char* arkDirpath);
	bool NewX360Gh2(const char* arkDirpath);
	bool NewX360Rockband(const char* arkDirpath);
	
	// open an ark and hdr from the given dir
	// 
	// args:	name of the dir to open the ark and hdr from
	//			flag for whether ark file is for xbox360 or not
	// returns:	true if successfully opened ark files
	bool Open(const char* arkDirpath);
	
	// save any changes to the ark and hdr files
	// (this writes all changes to disk)
	// if this isnt called no changes will be made to the ark file
	// 
	// returns:	true if saved successfully
	bool Save();
	
	// save any changes to the ark and hdr files to new ark and hdr files
	// (this writes all changes to disk without altering the original ark and header files)
	// if this isnt called no changes will be made to the ark file
	// 
	// Note: You cannot "save as" to the same files you originally opened
	// 
	// args:	dirpath to save new ark and hdr files to
	//			flag for whether to remove any gaps from the ark
	// returns:	true if saved successfully
	bool SaveAs(const char* newArkDirpath, bool removeGaps);
	
	// close existing ark and hdr files
	void Close();
	
	
	// extracts a file from the ark/hdr
	// 
	// args:	external filename of file on pc that you want to extract to
	//			internal filename to insert as in the ark and hdr
	//			flag for whether to decrypt files when getting them
	//				true:	to get "*.dtb" files in decrypted form
	//				false:	to get "*.dtb" files in encrypted form
	// returns:	true if extracted successfully
	bool GetFile(const char* destFilepath, const char* arkFilename, bool performDecrypts=true);
	
	// adds a file into the ark and hdr
	// 
	// args:	external filename of file on pc that you want to insert
	//			internal filename to insert as in the ark and hdr
	//			flag for whether to encrypt files when adding them
	//				true:	to encrypt "*.dtb" files when they are added
	//				false:	if "*.dtb" files being added are already encrypted
	// returns:	true if inserted successfully
	bool AddFile(const char* srcFilepath, const char* arkFilename, bool performEncrypts=true);
	
	// remove a file that exists in the ark and hdr
	// 
	// args:	filename to remove from the ark and hdr
	// returns:	true if removed successfully
	//			false otherwise
	bool RemoveFile(const char* arkFilename);
	
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
	bool ReplaceAFile(const char* srcFilepath, const char* arkFilename, bool performEncrypts=true);
	
	// rename a file from oldArkFilename to newArkFilename
	// you can cahnge the full filepath not just the filename using this
	// 
	// args:	new ark filename
	//			old ark filename
	// returns:	true if renamed successfully
	bool RenameFile(const char* newArkFilename, const char* oldArkFilename);
	
	// use there to iterate through all file entries in an ark and hdr.
	// you can't alter anything with these pointers, they're just for information gathering
	// 
	// args:	iterator for iterating through file entries
	//			search path for the file entries to return
	// returns:	unmodifiable pointer to file entry if it exists for the given search path
	//			NULL if no more files exist for the given search path
	const FileEntry* First(FileEntrySetIter& rIter, const char* searchFilepath="*") const;
	const FileEntry* Next( FileEntrySetIter& rIter, const char* searchFilepath="*") const;
	
	
	// prints info about all files in the hdr and ark
	// 
	// args:	filepath to search for and print
	//			stream to print to
	void Print(const char* searchFilepath="*", FILE* pStream=stdout) const;
	
	// extracts all files that fit the filename
	// 
	// args:	directory to extract to
	//			filepaths to extract (can use * and ? wildcards)
	//			flag for whether to decrypt files flagged as encrypted
	//			stream to print extraction info to
	// returns:	number of files extracted
	int ExtractFiles(const char* extractDirname, const char* searchFilepath="*",
					bool allowDecrypts=true, FILE* pStream=NULL);
	
private:
	s64  GetFilesize(const char* filename) const;
	void HdrPathToFilepath(char* filepath, const char* basedir, const char* hdrpath) const;
	void CreateBaseDirs(const char* baseDirpath) const;
	bool ExtractToFile(const char* destFilepath, const FileEntry& entry, bool allowDecrypts=true);
	
	// compare a wildcard string and a normal string
	// (works like normal strcmp)
	//
	// args:	normal string (contains no wildcard characters)
	//			wildcard string (may contain wildcard characters)
	//			true for case sensitive compare
	// returns:	0 if strings are 'equal'
	//			not equal otherwise
	int  StrcmpWildcard(const char *normalString, const char* wildcardString, bool caseSensitive=false) const;
	
	bool mHasChanged;
	bool mNewStyle;
	int mVersion;
	FileCollection mFileCollection;
	HdrFile mHdrFile;
	ArkFile mArkFile;
	char* mArkDirpath;
	s64 mMaxArkSize;
};


#endif // _ARK_HDR_PAIR_H_

