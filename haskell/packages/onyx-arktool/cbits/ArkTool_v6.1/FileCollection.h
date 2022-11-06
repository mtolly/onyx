//
// Collection of FileEntry objects that organised one after another.
// If an entry is at offset X, with size S, the next entry must be at X+S or later.
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _FILE_COLLECTION_H_
#define _FILE_COLLECTION_H_

#include <set>
#include "FileEntry.h"

// typedefs for a FileEntry set and an iterator for that set
typedef std::set<FileEntry> FileEntrySet;
typedef FileEntrySet::iterator FileEntrySetIter;
typedef FileEntrySet::const_iterator FileEntrySetCIter;


class FileCollection
{
public:
	FileCollection();
	~FileCollection();
	
	// clear file collection
	void Clear();
	
	// get the number of file entries
	int Size() const;
	
	// add a new entry to the collection
	// (this inserts it whereever there is space, usually at the "end")
	// any offset that is currently set in FileEntry is ignored and replaced
	void Add(const FileEntry& entry);
	
	// attempts to insert an entry at the offset inside fileentry
	// returns:	true if inserted successfully
	bool Insert(const FileEntry& entry);
	
	// remove an existing entry
	void Remove(FileEntrySetIter& rIter);
	
	// gets pointers to entries
	// if no more entries, returns NULL
	const FileEntry* First(FileEntrySetIter& rIter);
	const FileEntry* Next(FileEntrySetIter& rIter);
	
	// gets pointers to entries (constant iterator access)
	// if no more entries, returns NULL
	const FileEntry* First(FileEntrySetCIter& rIter) const;
	const FileEntry* Next(FileEntrySetCIter& rIter) const;
	
private:
	// find the first gap that is at least 'minGapSize' bytes in size
	// returns:	offset of start of gap if successful
	//			< 0 if error
	s64 FindFirstGap(s64 minGapSize);
	
	// find the gap best fits 'minGapSize' bytes without wasting space
	// returns:	offset of start of gap if successful
	//			< 0 if error
	s64 FindBestGap(s64 minGapSize);
	
	// get the end offset where a new file can be inserted at
	s64 EndOffset() const;
	
	
	FileEntrySet mEntries;
	bool mHasGaps;
};


#endif // _FILE_COLLECTION_H_

