// 
// provides access to the vgs file inside the pss
// without worrying about the pss itself
// 

#ifndef _PSS_VGS_ACCESS_H_
#define _PSS_VGS_ACCESS_H_

#include "types.h"


class PssVgsAccess
{
public:
	PssVgsAccess();
	~PssVgsAccess();
	
	bool Open(FILE* fd);
	bool Open(const char* filename);
	bool Close();
	
	// read vgs data from inside pss file
	// and update file pointer to point past the data just read in
	// 
	// args:	buffer to read into
	//			size of vgs data to read into buffer
	// returns:	size of vgs data successfully read into buffer (in bytes)
	int ReadUpdateOffset(u8* buffer, int size);
	
	// read vgs data from inside pss file
	// but do not update file pointer to point past the data just read in
	// (file pointer points to the start of the data read out)
	// 
	// args:	buffer to read into
	//			size of vgs data to read into buffer
	// returns:	size of vgs data successfully read into buffer (in bytes)
	int ReadNoUpdateOffset(u8* buffer, int size);
	
	// write data in buffer into vgs part of pss file
	// 
	// args:	buffer containing data to write
	//			size of data to write
	// returns:	size of vgs data successfully written to file (in bytes)
	int Write(const u8* buffer, int size);
	
private:
	bool FindNextAudioPacket(int& packetSize);
	
	bool m_ExternalFd;		// flag for whether the file descriptor is opened externally
	FILE* m_Fd;
	
	s64 m_FileOffset;		// offset of the start of the current packet in the file
	int m_PacketOffset;		// offset within the current packet
	int m_PacketSize;		// size of the current packet
	bool m_LastPacketReached;
	
	static const u8 S_FIRST_HEADER[4];
	static const u8 S_START_HEADER[4];
	static const u8 S_VIDEO_HEADER[4];
	static const u8 S_AUDIO_HEADER[4];
	static const u8 S_PAD_HEADER[4];
	static const u8 S_LAST_HEADER[4];
};

#endif // _PSS_VGS_ACCESS_H_

