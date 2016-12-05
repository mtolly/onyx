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

#include <cstring>

#include "HdrFile.h"
#include "DtbCrypt.h"
#include <string>
#include <vector>
#include <set>
#include "types.h"


// PACK_START: structs must be packed due to 64bit values not on 64bit alignment
#pragma pack(push, StructPack, 1)

// hdr file's header (version 3)
typedef struct {
	u32 version;			// always 3
	u32 numArks;			// 1 or 2 (number of arks)
	u32 numArks2;			// 1 or 2 (number of arks)
	u32 arkSize[1];			// size of ark files in bytes
} Hdr3Header;

// hdr file's file entry (version 3)
typedef struct {
	u32 offset;				// offset of file inside ark file (in bytes)
	u32 filenameStrIdx;		// index into string-offset table
	u32 dirnameStrIdx;		// index into string-offset table
	u64 size;				// size of file in bytes
} Hdr3FileEntry;


// hdr file's header (version 4)
typedef struct {
	u32 version;			// always 4
	u32 numArks;			// 1/2/5 (number of arks)
	u32 numArks2;			// 1/2/5 (number of arks)
	s64 arkSize[1];			// size of ark files in bytes
} Hdr4Header;

// hdr file's file entry (version 4)
typedef struct {
	s64 offset;				// offset of file inside ark file (in bytes)
	u32 filenameStrIdx;		// index into string-offset table
	u32 dirnameStrIdx;		// index into string-offset table
	s64 size;				// size of file in bytes
} Hdr4FileEntry;

#pragma pack(pop, StructPack)
// PACK_END: structs must be packed due to 64bit values not on 64bit alignment


struct hdr_file_entry_sorter
{
	bool operator () (const Hdr4FileEntry& left, const Hdr4FileEntry& right) const
	{
		if(left.dirnameStrIdx == right.dirnameStrIdx)
		{
			return left.filenameStrIdx < right.filenameStrIdx;
		}
		return left.dirnameStrIdx < right.dirnameStrIdx;
	}
};



HdrFile::HdrFile()
{
}
HdrFile::~HdrFile()
{
}


// read in a header file adding all file entries in it into a file collection
// 
// args:	hdr filename to read from
//			collection to add file entries into
//			vector to get the sizes of ark files into
//			gets a flag for whether the file was an encrypted hdr
//			gets the version of the hdr that was read in
// returns:	true if added all file entries successfully into collection
//			false if error adding a file entry
bool HdrFile::Read(const char* hdrFilename, FileCollection& rCollection,
				   std::vector<s64>& rArkSizes, bool& rIsEncrypted, int& rVersion) const
{
	// buffer header file
	FILE* hfd = fopen(hdrFilename, "rb");
	if(hfd == NULL)
		return false;
	fseek(hfd, 0, SEEK_END);
	int size = ftell(hfd);
	fseek(hfd, 0, SEEK_SET);
	unsigned char* buff = new unsigned char[size];
	fread(buff, 1, size, hfd);
	fclose(hfd);
	
	
	// auto-detect if file is encrypted and decrypt it if required
	unsigned char* hdr_ptr = buff;
	unsigned int version_test = *(unsigned int*)hdr_ptr;
	if(version_test > 5)
	{
		// invalid version, so file must be encrypted
		dtb_crypt_new(hdr_ptr, size);
		hdr_ptr += 4;
		rIsEncrypted = true;
	}
	else
	{
		// valid version, so assume file isnt encrypted
		rIsEncrypted = false;
	}
	
	
	// get file info pointers
	version_test = *(unsigned int*)hdr_ptr;
	int ark_header_size;
	int string_size;
	char* str_ptr;
	int num_offsets;
	int* offset_ptr;
	int num_entries;
	Hdr3FileEntry* entry_3ptr = NULL;
	Hdr4FileEntry* entry_4ptr = NULL;
	if(version_test <= 3)
	{
		// version 3 hdr
		Hdr3Header* header	= (Hdr3Header*)hdr_ptr;
		ark_header_size		= sizeof(Hdr3Header) + (header->numArks-1)*sizeof(header->arkSize[0]);
		string_size			= *(int*)(hdr_ptr + ark_header_size);
		str_ptr				= (char*)(hdr_ptr + ark_header_size + 4);
		num_offsets			= *(int*)(hdr_ptr + ark_header_size + 4 + string_size);
		offset_ptr			=  (int*)(hdr_ptr + ark_header_size + 4 + string_size + 4);
		num_entries			= *(int*)(hdr_ptr + ark_header_size + 4 + string_size + 4 + num_offsets*sizeof(int));
		entry_3ptr			= (Hdr3FileEntry*)(hdr_ptr + ark_header_size + 4 + string_size + 4 + num_offsets*sizeof(int) + 4);
		rVersion			= header->version;
		for(int i=0; i<(int)header->numArks; i++)
		{
			rArkSizes.push_back(header->arkSize[i]);
		}
	}
	else
	{
		// version 4 hdr
		Hdr4Header* header	= (Hdr4Header*)hdr_ptr;
		ark_header_size		= sizeof(Hdr4Header) + (header->numArks-1)*sizeof(header->arkSize[0]);
		string_size			= *(int*)(hdr_ptr + ark_header_size);
		str_ptr				= (char*)(hdr_ptr + ark_header_size + 4);
		num_offsets			= *(int*)(hdr_ptr + ark_header_size + 4 + string_size);
		offset_ptr			=  (int*)(hdr_ptr + ark_header_size + 4 + string_size + 4);
		num_entries			= *(int*)(hdr_ptr + ark_header_size + 4 + string_size + 4 + num_offsets*sizeof(int));
		entry_4ptr			= (Hdr4FileEntry*)(hdr_ptr + ark_header_size + 4 + string_size + 4 + num_offsets*sizeof(int) + 4);
		rVersion			= header->version;
		for(int i=0; i<(int)header->numArks; i++)
		{
			rArkSizes.push_back(header->arkSize[i]);
		}
	}
	
	
	// get all file entries
	for(int i=0; i<num_entries; i++)
	{
		s64 entry_size;
		s64 entry_offset;
		char entry_arkname[260];
		if(version_test <= 3)
		{
			// version 3 hdr
			entry_size = entry_3ptr[i].size;
			entry_offset = entry_3ptr[i].offset;
			sprintf(entry_arkname, "%s/%s", 
				str_ptr+offset_ptr[entry_3ptr[i].dirnameStrIdx], 
				str_ptr+offset_ptr[entry_3ptr[i].filenameStrIdx] );
		}
		else
		{
			// version 4 hdr
			entry_size = entry_4ptr[i].size;
			entry_offset = entry_4ptr[i].offset;
			sprintf(entry_arkname, "%s/%s", 
				str_ptr+offset_ptr[entry_4ptr[i].dirnameStrIdx], 
				str_ptr+offset_ptr[entry_4ptr[i].filenameStrIdx] );
		}
		bool entry_encrypted = strstr(entry_arkname, ".dtb") != 0;
		
		char* entry_arkname_ptr = entry_arkname;
		if( strncmp(entry_arkname, "./", 2) == 0 )
			entry_arkname_ptr = entry_arkname+2;
		FileEntry entry(
			entry_arkname_ptr,
			entry_size,
			entry_offset,
			entry_encrypted );
		if( !rCollection.Insert(entry) )
		{
			delete[] buff;
			return false;
		}
	}
	
	// finish up
	delete[] buff;
	return true;
}


// write a file collection into a header file
// 
// args:	hdr filename to write to
//			collection of file entries to write out into hdr file
//			vector to set the sizes of ark files from
//			flag for whether the hdr is encrypted
//			version of header to write out
// returns:	true if successfully wrote out hdr file
//			false if error writing header
bool HdrFile::Write(const char* hdrFilename, const FileCollection& collection,
			const std::vector<s64>& arkSizes, bool isEncrypted, int version) const
{
	FILE* hfd = fopen(hdrFilename, "w+b");
	if(hfd == NULL)
		return false;
	
	// write out crypt init value if encryption is used
	if(isEncrypted)
	{
		// this value can be anything except 0
		// (pity cos 0 results in unencrypted contents even after crypting)
		fwrite(&G_CRYPT_KEY, 1, sizeof(G_CRYPT_KEY), hfd);
	}
	
	// write out header
	if(version <= 3)
	{
		// version 3 hdr
		Hdr3Header hdr_header;
		hdr_header.version = version;
		hdr_header.numArks  = (unsigned int)arkSizes.size();
		hdr_header.numArks2 = (unsigned int)arkSizes.size();
		hdr_header.arkSize[0] = (unsigned int)arkSizes[0];
		fwrite(&hdr_header, 1, sizeof(hdr_header), hfd);
		for(int i=1; i<(int)hdr_header.numArks; i++)
		{
			unsigned int size = (unsigned int)arkSizes[i];
			fwrite(&size, 1, sizeof(size), hfd);
		}
	}
	else
	{
		// vesrion 4 hdr
		Hdr4Header hdr_header;
		hdr_header.version = version;
		hdr_header.numArks  = (unsigned int)arkSizes.size();
		hdr_header.numArks2 = (unsigned int)arkSizes.size();
		hdr_header.arkSize[0] = arkSizes[0];
		fwrite(&hdr_header, 1, sizeof(hdr_header), hfd);
		for(int i=1; i<(int)hdr_header.numArks; i++)
		{
			s64 size = arkSizes[i];
			fwrite(&size, 1, sizeof(size), hfd);
		}
	}
	
	
	// create tables to store the data required to write to file
	std::vector<StringInfoType>	str_table;			// all strings to write to file
	std::vector<int>			str_offset_table;	// all offsets to write to file
	std::set<Hdr4FileEntry, hdr_file_entry_sorter> file_entry_table;	// all file entries to write to file
	std::set<Hdr4FileEntry, hdr_file_entry_sorter>::iterator file_entry_table_iter;	// all file entries to write to file
	
	
	// calculate the max hash value
	// then fill the string offset table with that many zeroed entries
	int max_hash = (collection.Size() * 2) + 200;
	str_offset_table.assign(max_hash, 0);
	
	
	// insert special empty string
	// this string is used as the first string since an offset
	// value of 0 into the string table (ie the first string)
	// is not allowed since an offset of 0 means to stop searching
	// for the correct file!
	StringInfoType string_info;
	string_info.str = "";
	string_info.offset = 0;
	string_info.size = (int)string_info.str.size() + 1;
	str_table.push_back(string_info);
	
	// insert strings from file entries
	FileEntrySetCIter iter;
	for(const FileEntry* p_entry=collection.First(iter); p_entry!=NULL; p_entry=collection.Next(iter))
	{
		// split filename into dirname and filename
		char dir[260];
		char file[260];
		strcpy(dir, p_entry->Arkname());
		char* str_ptr = strrchr(dir, '/');
		if(str_ptr == NULL)
		{
			// '/' not found
			strcpy(file, dir);
			strcpy(dir, ".");
		}
		else
		{
			strcpy(file, str_ptr+1);
			*str_ptr = '\0';
		}
		
		
		// add strings to the string table and get the index of them in that table
		int dir_index  = AddStringToTable(str_table, dir, max_hash);
		int file_index = AddStringToTable(str_table, file, max_hash);
		
		// add offsets to offset table
		int dir_offset  = AddOffsetToTable(str_offset_table, str_table, dir_index);
		int file_offset = AddOffsetToTable(str_offset_table, str_table, file_index);
		
		// add file entry to file entry set
		Hdr4FileEntry entry;
		entry.dirnameStrIdx	= dir_offset;
		entry.filenameStrIdx= file_offset;
		entry.offset		= p_entry->Offset();
		entry.size			= p_entry->Arksize();
		file_entry_table.insert(entry);
	}
	
	
	// write out string table
	int str_table_size = GetStringTableSize(str_table);
	int num_strings = (int)str_table.size();
	fwrite(&str_table_size, 1, sizeof(str_table_size), hfd);
	for(int i=0; i<num_strings; i++)
	{
		fwrite(str_table[i].str.c_str(), 1, str_table[i].size, hfd);
	}
	
	// write out string offsets table
	int num_offsets = (int)str_offset_table.size();
	fwrite(&num_offsets, 1, sizeof(num_offsets), hfd);
	for(int i=0; i<num_offsets; i++)
	{
		int offset = str_offset_table[i];
		fwrite(&offset, 1, sizeof(offset), hfd);
	}
	
	
	// write out entries table
	int num_entries = (int)file_entry_table.size();
	fwrite(&num_entries, 1, sizeof(num_entries), hfd);
	for(file_entry_table_iter =  file_entry_table.begin();
		file_entry_table_iter != file_entry_table.end();
		file_entry_table_iter++ )
	{
		if(version <= 3)
		{
			// version 3 entry
			const Hdr4FileEntry* entry_ptr = &(*file_entry_table_iter);
			Hdr3FileEntry hdr_entry;
			hdr_entry.dirnameStrIdx	= entry_ptr->dirnameStrIdx;
			hdr_entry.filenameStrIdx= entry_ptr->filenameStrIdx;
			hdr_entry.offset		= (unsigned int)entry_ptr->offset;
			hdr_entry.size			= entry_ptr->size;
			fwrite(&hdr_entry, 1, sizeof(Hdr3FileEntry), hfd);
		}
		else
		{
			// version 4 entry
			const Hdr4FileEntry* entry_ptr = &(*file_entry_table_iter);
			fwrite(entry_ptr, 1, sizeof(Hdr4FileEntry), hfd);
		}
	}
	
	
	// encrypt file if required
	if( isEncrypted )
	{
		fflush(hfd);
		
		int enc_size = ftell(hfd);
		unsigned char* enc_buff = new unsigned char[enc_size];
		fseek(hfd, 0, SEEK_SET);
		fread(enc_buff, 1, enc_size, hfd);
		
		dtb_crypt_new(enc_buff, enc_size);
		fseek(hfd, 0, SEEK_SET);
		fwrite(enc_buff, 1, enc_size, hfd);
	}
	
	// done
	fclose(hfd);
	return true;
}

// adds a string to the table if it doesnt already exist in the table
// then returns the index of the string in the table
// 
// args:	string table
//			string to insert into the table
// returns:	index of string inserted in table
//			(may be a new index or an existing index)
int HdrFile::AddStringToTable(std::vector<StringInfoType>& rStrTable, const char* str, int maxHash) const
{
	StringInfoType new_string;
	new_string.str		= str;
	new_string.size		= (int)new_string.str.size() + 1;

	// check first to see if the string already exists in the table
	for(int i=0; i<(int)rStrTable.size(); i++)
	{
		// size comparison is quicker than strcmp, so do it first
		// then do a strcmp if the sizes are the same
		if(	rStrTable[i].size == new_string.size &&
			rStrTable[i].str == new_string.str )
		{
			// string is already in table, so return its index
			return i;
		}
	}

	// string isnt in table, so add it
	new_string.offset	= GetStringTableSize(rStrTable);
	new_string.hash		= CalcHash(str, maxHash);
	rStrTable.push_back(new_string);
	
	return (int)rStrTable.size() - 1;
}

// add an offset to the offset table
// 
// args:	offset table to insert into
//			string table contain info for insertion
//			index into string table of info for insertion
// returns:	index in offset table inserted at
int HdrFile::AddOffsetToTable(std::vector<int>& rOffsetTable,
							  std::vector<StringInfoType>& strTable,
							  int strIndex) const
{
	int i = strTable[strIndex].hash;
	while(1)
	{
		// check if its free to insert here
		if(	rOffsetTable[i] == 0)
		{
			// update hash to the hash value being used
			strTable[strIndex].hash = i;
			
			// now add the offset to the table
			rOffsetTable[i] = strTable[strIndex].offset;
			return i;
		}
		
		// check if this string is already set here
		if( rOffsetTable[i] == strTable[strIndex].offset )
		{
			return i;
		}
		
		// increment index
		i++;
		if( i >= (int)rOffsetTable.size() )
			i = 0;
	}
}


// get the size of the string table in bytes
int HdrFile::GetStringTableSize(const std::vector<StringInfoType>& strTable) const
{
	if( strTable.empty() )
		return 0;
	return strTable[strTable.size() - 1].offset + strTable[strTable.size() - 1].size;
}


// calc hash for string
// 
// args:	string to calc hash over
//			maximum hash value
// returns:	hash value for the given string
int HdrFile::CalcHash(const char* str, int maxHash) const
{
	int hash = 0;
	while( *str != 0 )
	{
		// only use the 7 bits that ascii characters use
		// when creating the hash value
		hash = (hash * 0x7F) + *str;
		str++;

		// ensure the hash stays within the maxHash limits
		hash = hash - ((hash / maxHash) * maxHash);
	}
	return hash;
}

