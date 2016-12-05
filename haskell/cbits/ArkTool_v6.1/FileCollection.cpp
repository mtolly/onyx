//
// Collection of FileEntry objects that organised one after another.
// If an entry is at offset X, with size S, the next entry must be at X+S or later.
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#include "FileCollection.h"

typedef FileEntrySet::reverse_iterator FileEntrySetRIter;
typedef FileEntrySet::const_reverse_iterator FileEntrySetCRIter;


FileCollection::FileCollection()
	: mHasGaps(true)
{
}
FileCollection::~FileCollection()
{
}

// clear file collection
void FileCollection::Clear()
{
	mEntries.clear();
	mHasGaps = true;
}

// get the number of file entries
int FileCollection::Size() const
{
	return (int)mEntries.size();
}


// add a new entry to the collection
// (this inserts it whereever there is space, usually at the "end")
// any offset that is currently set in FileEntry is ignored and replaced
void FileCollection::Add(const FileEntry& entry)
{
	// check where to insert at
	s64 offset = -1;
	if(mHasGaps)
		offset = FindBestGap(entry.Arksize());
	if(offset < 0)
		offset = EndOffset();
	
	// insert at offset
	FileEntry new_entry(entry.Arkname(), entry.Filename(), entry.Filesize(), offset, entry.Encrypted());
	mEntries.insert(new_entry);
}


// attempts to insert an entry at the offset inside fileentry
// returns:	true if inserted successfully
bool FileCollection::Insert(const FileEntry& entry)
{
	// get the entry that is greater or equal to where this entry wants to be inserted
	// this entry must be at an offset that >= entry.Offset()+entry.Arksize()
	// otherwise this entry cannot be inserted.
	FileEntrySetIter iter = mEntries.lower_bound(entry);
	if(	iter != mEntries.end() &&
		entry.Offset()+entry.Arksize() > iter->Offset() )
	{
		// wont fit, cannot insert
		return false;
	}
	
	// will fit, so insert it
	mEntries.insert(entry);
	return true;
}


// remove an existing entry
void FileCollection::Remove(FileEntrySetIter& rIter)
{
	// if entry is removed from anywhere except the end, then there are now gaps
	FileEntrySetCRIter p_last_entry = mEntries.rbegin();
	if(*rIter != *p_last_entry)
		mHasGaps = true;
	
	mEntries.erase(rIter);
}

// if no more entries, returns NULL
const FileEntry* FileCollection::First(FileEntrySetIter& rIter)
{
	if(mEntries.empty())
		return NULL;
	rIter = mEntries.begin();
	return &(*rIter);
}
const FileEntry* FileCollection::Next(FileEntrySetIter& rIter)
{
	rIter++;
	if(rIter == mEntries.end())
		return NULL;
	return &(*rIter);
}

// gets pointers to entries (constant iterator access)
// if no more entries, returns NULL
const FileEntry* FileCollection::First(FileEntrySetCIter& rIter) const
{
	if(mEntries.empty())
		return NULL;
	rIter = mEntries.begin();
	return &(*rIter);
}
const FileEntry* FileCollection::Next(FileEntrySetCIter& rIter) const
{
	rIter++;
	if(rIter == mEntries.end())
		return NULL;
	return &(*rIter);
}


// find the first gap that is at least 'minGapSize' bytes in size
// returns:	offset of start of gap if successful
//			< 0 if error
s64 FileCollection::FindFirstGap(s64 minGapSize)
{
	if(minGapSize <= 0)
		return -1;
	
	s64 file_end_offset = 0;
	s64 gap_size = 0;
	for(FileEntrySetCIter p_entry=mEntries.begin(); p_entry!=mEntries.end(); p_entry++)
	{
		if(p_entry->Offset() > file_end_offset)
		{
			// gap found
			gap_size = p_entry->Offset() - file_end_offset;
			if(gap_size >= minGapSize)
			{
				// gap is big enough
				return p_entry->Offset();
			}
		}
		
		// update end of file offset
		file_end_offset = p_entry->Offset() + p_entry->Arksize();
	}
	
	// if no gaps were found, clear the "is gaps" flag
	if(gap_size == 0)
		mHasGaps = false;
	
	// gap with large enough size was not found
	return -1;
}

// find the gap best fits 'minGapSize' bytes without wasting space
// returns:	offset of start of gap if successful
//			< 0 if error
s64 FileCollection::FindBestGap(s64 minGapSize)
{
	if(minGapSize <= 0)
		return -1;
	
	s64 curr_gap_size = -1;
	s64 curr_gap_offset = -1;
	
	s64 file_end_offset = 0;
	s64 gap_size = 0;
	for(FileEntrySetCIter p_entry=mEntries.begin(); p_entry!=mEntries.end(); p_entry++)
	{
		if(p_entry->Offset() > file_end_offset)
		{
			// gap found
			gap_size = p_entry->Offset() - file_end_offset;
			
			// if the gap is the exact size, then there is no "better" gap
			if(gap_size == minGapSize)
				return file_end_offset;
			
			// if gap is bigger than required, check if it is teh best gap so far
			else if(gap_size > minGapSize)
			{
				// gap is big enough
				if(	curr_gap_size < 0 ||
					curr_gap_size > gap_size )
				{
					// gap is best fit so far
					curr_gap_size = gap_size;
					curr_gap_offset = file_end_offset;
				}
			}
		}
		
		// update end of file offset
		file_end_offset = p_entry->Offset() + p_entry->Arksize();
	}
	
	// if no gaps were found, clear the "is gaps" flag
	if(gap_size == 0)
		mHasGaps = false;
	
	// return size of best-fit gap (may be -1)
	return curr_gap_offset;
}


// get the end offset where a new file can be inserted at
s64 FileCollection::EndOffset() const
{
	if( mEntries.empty() )
		return 0;
	FileEntrySetCRIter p_entry = mEntries.rbegin();
	return p_entry->Offset() + p_entry->Arksize();
}


