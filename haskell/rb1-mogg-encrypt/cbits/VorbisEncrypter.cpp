#include "VorbisEncrypter.h"

#include <time.h>
#include "aes.h"
#include "keys.h"
#include <stdexcept>
#include <cstring>

typedef struct {
	uint32_t a;
	uint32_t b;
} MapEntry;

VorbisEncrypter::VorbisEncrypter(void* datasource, ov_callbacks cbStruct) 
	: file_ref(datasource), cb_struct(cbStruct) {
	cb_struct.seek_func(file_ref, 0, SEEK_END);
	uint32_t total_length = cbStruct.tell_func(file_ref);
	cb_struct.seek_func(file_ref, 0, SEEK_SET);
	if (cbStruct.read_func(&original_file_header, sizeof(original_file_header), 1, file_ref) != 1)
		throw std::runtime_error("Unable to read mogg header.");
	if (original_file_header.version != 0xA)
		throw std::runtime_error("Source mogg must be version 10/0xA (unencrypted).");

	struct {
		uint32_t version;
		uint32_t chunk_size;
		uint32_t num_entries;
	} OggMap;

	if (cbStruct.read_func(&OggMap, sizeof(OggMap), 1, file_ref) != 1)
		throw std::runtime_error("Unable to read OggMap.");
	size_t header_size = 16 /* IV */
		+ sizeof(original_file_header)
		+ sizeof(OggMap)
		+ (OggMap.num_entries * sizeof(MapEntry));
	hmx_header.resize(header_size);
	auto* header_ptr = hmx_header.data();
	auto new_header = original_file_header;
	new_header.version = 0xB;
	new_header.offset = header_size;
	// Copy Mogg header
	memcpy(header_ptr, &new_header, sizeof(new_header));
	header_ptr += sizeof(new_header);
	// Copy OggMap header
	memcpy(header_ptr, &OggMap, sizeof(OggMap));
	header_ptr += sizeof(OggMap);
	// Copy OggMap data
	cb_struct.read_func(header_ptr, sizeof(MapEntry), OggMap.num_entries, file_ref);
	header_ptr += sizeof(MapEntry) * OggMap.num_entries;
	// Generate IV
	srand(time(NULL));
	initial_counter = (aes_ctr_128*)header_ptr;
	// For convenience leave the low 4 IV bytes as zero
	for (int i = 0; i < 4; i++) {
		*header_ptr++ = 0;
	}
	// Generate the remaining 12 IV bytes with rand
	for (int i = 0; i < 12; i++) {
		*header_ptr++ = rand() & 0xFF;
	}
	encrypted_length = total_length - original_file_header.offset + hmx_header.size();
}

VorbisEncrypter::~VorbisEncrypter() {
	cb_struct.close_func(file_ref);
}

size_t VorbisEncrypter::ReadRaw(void* buf, size_t elementSize, size_t elements)
{
	size_t count = elementSize * elements;
	size_t bytesRead = 0;
	size_t offset = 0;
	uint8_t* buffer = (uint8_t*)buf;

	if (position < hmx_header.size()) {
		while (position < hmx_header.size() && offset < count) {
			buffer[offset++] = hmx_header[position++];
			bytesRead++;
		}
	}
	count -= offset;

	if (position + count > encrypted_length) {
		count = encrypted_length - position;
	}
	if (count == 0) return bytesRead / elementSize;

	int32_t size_diff = (int32_t)original_file_header.offset - hmx_header.size();
	cb_struct.seek_func(file_ref, position + size_diff, SEEK_SET);
	size_t actualRead = cb_struct.read_func(buffer + offset, 1, count, file_ref);

	bytesRead += actualRead;
	position += actualRead;

	EncryptBytes(buffer, offset, actualRead);
	return bytesRead / elementSize;
}



// Fixes the counter
void VorbisEncrypter::FixCounter(size_t decryptedPos) {
	counter = *initial_counter;
	uint64_t low = counter.qwords[0];
	counter.qwords[0] += (decryptedPos >> 4);
	if (counter.qwords[0] < low) {
		counter.qwords[1]++;
	}
}

/**
 * buffer: buffer to write into
 * offset: offset into buffer to start writing
 * count: number of bytes to write into buffer
 *
 * This relies on the internal state, specifically that oggPos is set to the
 * last READ location. So if oggPos is 30 and count is 10 it assumes that bytes from 20 to 30 are being decrypted.
 */
void VorbisEncrypter::EncryptBytes(uint8_t* buffer, size_t offset, size_t count)
{
	aes_ctr_128 cryptedCounter;
	size_t decryptedPos = position - count - hmx_header.size();
	int counterLoc = decryptedPos % 16;

	FixCounter(decryptedPos);
	AES128_ECB_encrypt(counter.bytes, ctrKey0B, cryptedCounter.bytes);
	for (int i = 0; i < count; i++)
	{
		if (decryptedPos != 0 && decryptedPos % 16 == 0 && i != 0)
		{
			FixCounter(decryptedPos);
			counterLoc = 0;
			AES128_ECB_encrypt(counter.bytes, ctrKey0B, cryptedCounter.bytes);
		}
		buffer[i + offset] ^= cryptedCounter.bytes[counterLoc]; //decrypt one byte
		decryptedPos++;
		counterLoc++;
	}
}
