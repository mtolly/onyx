// 
// This object handles hdr files as used in ark/hdr file containers.
// These ark/hdr files are found in Guitar Hero 1, Guitar Hero 2,
// Guitar Hero 80s and Rockband (Possibly other games too).
// 
// The hdr files contain a filetable for the contents of ark files.
// The entire header may be encrypted with "DtbCrypt" functions.
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _HDR_FILE_H_
#define _HDR_FILE_H_

#include "types.h"
#include "FileCollection.h"
#include <vector>
#include <string>


// this is used to insert into the string table when writing out a hdr file
typedef struct {
	std::string str;// string value
	int offset;		// offset of string from start of string table
	int size;		// size of string
	int hash;		// has over this string
} StringInfoType;


class HdrFile
{
public:
	HdrFile();
	~HdrFile();
	
	// read in a header file adding all file entries in it into a file collection
	// 
	// args:	hdr filename to read from
	//			collection to add file entries into
	//			vector to get the sizes of ark files into
	//			gets a flag for whether the file was an encrypted hdr
	//			gets the version of the hdr that was read in
	// returns:	true if added all file entries successfully into collection
	//			false if error adding a file entry
	bool Read(const char* hdrFilename, FileCollection& rCollection,
				std::vector<s64>& rArkSizes, bool& rIsEncrypted, int& rVersion) const;
	
	// write a file collection into a header file
	// 
	// args:	hdr filename to write to
	//			collection of file entries to write out into hdr file
	//			vector to set the sizes of ark files from
	//			flag for whether the hdr is encrypted
	//			version of header to write out
	// returns:	true if successfully wrote out hdr file
	//			false if error writing header
	bool Write(const char* hdrFilename, const FileCollection& collection,
				const std::vector<s64>& arkSizes, bool isEncrypted=true, int version=3) const;
	
private:
	// add a string to the string table
	int AddStringToTable(std::vector<StringInfoType>& rStrTable, const char* str, int maxHash) const;
	
	// add an offset to the offset table
	int AddOffsetToTable(std::vector<int>& rOffsetTable,
								  std::vector<StringInfoType>& strTable,
								  int strIndex) const;
	
	// get the size of the string table in bytes
	int GetStringTableSize(const std::vector<StringInfoType>& strTable) const;
	
	// calc hash for string
	// 
	// args:	string to calc hash over
	//			maximum hash value
	// returns:	hash value for the given string
	int CalcHash(const char* str, int maxHash) const;
};


#endif // _HDR_FILE_H_

