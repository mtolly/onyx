// 
// provides access to the vgs file inside the pss
// without worrying about the pss itself
// 

#include "types.h"
#include "PssVgsAccess.h"
#include <string.h>

#define min(a, b) (((a) < (b))?(a):(b))

// headers for frame types
const u8 PssVgsAccess::S_FIRST_HEADER[4]	= { 0x00, 0x00, 0x01, 0xBA };
const u8 PssVgsAccess::S_START_HEADER[4]	= { 0x00, 0x00, 0x01, 0xBB };
const u8 PssVgsAccess::S_VIDEO_HEADER[4]	= { 0x00, 0x00, 0x01, 0xE0 };
const u8 PssVgsAccess::S_AUDIO_HEADER[4]	= { 0x00, 0x00, 0x01, 0xBD };
const u8 PssVgsAccess::S_PAD_HEADER[4]		= { 0x00, 0x00, 0x01, 0xBE };
const u8 PssVgsAccess::S_LAST_HEADER[4]		= { 0x00, 0x00, 0x01, 0xB9 };


PssVgsAccess::PssVgsAccess()
	: m_ExternalFd(false)
	, m_Fd(NULL)
	, m_FileOffset(0)
	, m_PacketOffset(0)
	, m_PacketSize(0)
	, m_LastPacketReached(false)
{
}

PssVgsAccess::~PssVgsAccess()
{
	Close();
}

bool PssVgsAccess::Open(FILE* fd)
{
	m_Fd = fd;
	m_ExternalFd = true;
	if(m_Fd == NULL)
		return false;
	
	// check that file is a pss file
	u8 hdr[4];
	s64 offset = FTELL64(m_Fd);
	if( fread(hdr, 1, 4, m_Fd) != 4 ||
		memcmp(hdr, S_FIRST_HEADER, 4) )
	{
		// not a pss file
		Close();
		return false;
	}
	FSEEK64(m_Fd, offset, SEEK_SET);
	
	m_LastPacketReached = false;
	m_FileOffset = 0;
	m_PacketOffset = 0;
	m_PacketSize = 0;
	return true;
}

bool PssVgsAccess::Open(const char* filename)
{
	// open file
	m_Fd = fopen(filename, "r+b");
	if(m_Fd == NULL)
		return false;
	
	bool result = Open(m_Fd);
	m_ExternalFd = false;
	
	if(!result)
		Close();
	return result;
}

bool PssVgsAccess::Close()
{
	if(!m_ExternalFd && m_Fd!=NULL)
		fclose(m_Fd);
	m_Fd = NULL;
	m_FileOffset = 0;
	m_PacketOffset = 0;
	m_PacketSize = 0;
	m_LastPacketReached = false;
	return true;
}


// read vgs data from inside pss file
// and update file pointer to point past the data just read in
// 
// args:	buffer to read into
//			size of vgs data to read into buffer
// returns:	size of vgs data successfully read into buffer (in bytes)
int PssVgsAccess::ReadUpdateOffset(u8* buffer, int size)
{
	int buff_offset = 0;
	int packet_size = 0;
	
	while( FindNextAudioPacket(packet_size) )
	{
		int read_size = min(size-buff_offset, packet_size);
		int result_size = (int)fread(buffer+buff_offset, 1, read_size, m_Fd);
		
		m_PacketOffset += result_size;
		buff_offset += result_size;
		if(result_size != read_size ||
			buff_offset == size )
			break;
	}
	
	return buff_offset;
}

// read vgs data from inside pss file
// but do not update file pointer to point past the data just read in
// (file pointer points to the start of the data read out)
// 
// args:	buffer to read into
//			size of vgs data to read into buffer
// returns:	size of vgs data successfully read into buffer (in bytes)
int PssVgsAccess::ReadNoUpdateOffset(u8* buffer, int size)
{
	s64 save_curr_offset = FTELL64(m_Fd);
	s64 save_file_offset = m_FileOffset;
	int save_packet_offset = m_PacketOffset;
	int save_packet_size = m_PacketSize;
	bool save_last_packet = m_LastPacketReached;
	
	int result = ReadUpdateOffset(buffer, size);
	
	FSEEK64(m_Fd, save_curr_offset, SEEK_SET);
	m_FileOffset = save_file_offset;
	m_PacketOffset = save_packet_offset;
	m_PacketSize = save_packet_size;
	m_LastPacketReached = save_last_packet;
	
	return result;
}

// write data in buffer into vgs part of pss file
// 
// args:	buffer containing data to write
//			size of data to write
// returns:	size of vgs data successfully written to file (in bytes)
int PssVgsAccess::Write(const u8* buffer, int size)
{
	int buff_offset = 0;
	int packet_size = 0;
	
	while( FindNextAudioPacket(packet_size) )
	{
		int write_size = min(size-buff_offset, packet_size);
		int result_size = (int)fwrite(buffer+buff_offset, 1, write_size, m_Fd);
		fflush(m_Fd);
		
		m_PacketOffset += result_size;
		buff_offset += result_size;
		if(result_size != write_size ||
			buff_offset == size )
			break;
	}
	
	return buff_offset;
}


// find the next audio packet in the pss file
// then seek to the start of the data in that packet
// and set the size of the data in 'packetSize'
// 
// args:	variable to get teh size of the audio data in
// returns:	true if found another packet successfully
bool PssVgsAccess::FindNextAudioPacket(int& packetSize)
{
	packetSize = 0;
	
	// check if there is data left in the current packet first
	if(m_PacketOffset < m_PacketSize)
	{
		// get data from current packet
		packetSize = m_PacketSize - m_PacketOffset;
		return true;
	}
	
	// get the next packet
	while(!m_LastPacketReached)
	{
		// seek to the start of the next packet
		if( FSEEK64(m_Fd, m_FileOffset+m_PacketSize, SEEK_SET) )
			return false;
		m_FileOffset += m_PacketSize;
		m_PacketOffset = 0;
		
		// get the header from the next packet
		u8 hdr[4];
		if( fread(hdr, 1, 4, m_Fd) != 4 )
			return false;
		m_PacketOffset += 4;
		
		// calc the size of the next packet
		m_PacketSize = 0;
		bool is_audio = false;
		if(		!memcmp(hdr, S_FIRST_HEADER, 4) )
		{
			// first header on the file
			m_PacketSize = 4 + 10;
		}
		else if(!memcmp(hdr, S_START_HEADER, 4) )
		{
			// header on the start of a bunch of frames
		}
		else if(!memcmp(hdr, S_VIDEO_HEADER, 4) )
		{
			// header on the start of video
		}
		else if(!memcmp(hdr, S_AUDIO_HEADER, 4) )
		{
			// header on the start of audio
			is_audio = true;
		}
		else if(!memcmp(hdr, S_PAD_HEADER, 4) )
		{
			// header on the start of padding
		}
		else if(!memcmp(hdr, S_LAST_HEADER, 4) )
		{
			// last header in file
			m_PacketSize = 4;
			m_LastPacketReached = true;
		}
		else
		{
			// unknown header
		}
		
		if(is_audio)
		{
			// read in size of data that follows header
			if( fread(&hdr, 1, 2, m_Fd) != 2 )
				return false;
			m_PacketOffset += 2;
			m_PacketSize = 4 + 2 + ((hdr[0]<<8) | hdr[1]);
			
			// get the size of the audio data in an audio packet
			if( fread(&hdr, 1, 3, m_Fd) != 3 )
				return false;
			m_PacketOffset += 3;
			packetSize = m_PacketSize - (4 + 2 + 3 + hdr[2] + 4);
			
			// seek to the start of the audio data
			FSEEK64(m_Fd, hdr[2] + 4, SEEK_CUR);
			m_PacketOffset += hdr[2] + 4;
			return true;
		}
		else if(m_PacketSize == 0)
		{
			// read in size of data that follows header
			if( fread(&hdr, 1, 2, m_Fd) != 2 )
				return false;
			m_PacketOffset += 2;
			m_PacketSize = 4 + 2 + ((hdr[0]<<8) | hdr[1]);
			m_PacketOffset += ((hdr[0]<<8) | hdr[1]);
		}
	}
	
	// no more audio headers to retrieve
	return false;
}

