#pragma once

#ifndef _OV_FILE_H_
#include "XiphTypes.h"
#endif

#include "VorbisReader.h"
#include <inttypes.h>
#include <vector>

class VorbisEncrypter
{
public:
	// Construct an encrypter using the given unencrypted file as a source.
	VorbisEncrypter(void* datasource, ov_callbacks cbStruct);
	~VorbisEncrypter();

	// Read encrypted Mogg data. Returns number of elements read.
	size_t ReadRaw(void* buf, size_t elementSize, size_t elements);
private:

	void FixCounter(size_t decryptedPos);
	void EncryptBytes(uint8_t* buffer, size_t offset, size_t count);

	ov_callbacks cb_struct{};
	void* file_ref{ 0 };

	size_t position{ 0 };
	size_t encrypted_length{ 0 };
	std::vector<uint8_t> hmx_header;

	struct {
		int32_t version;
		uint32_t offset;
	} original_file_header;
	aes_ctr_128* initial_counter{ 0 };
	aes_ctr_128 counter{ };
	// MT: above was "aes_ctr_128 counter{ 0 };" but older gcc in Docker build complained.
	// Empty init should zero out the first union member, and all union members are same size here.
};

