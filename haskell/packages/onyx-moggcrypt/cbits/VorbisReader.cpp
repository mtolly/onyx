#include "VorbisReader.h"
#include "keys.h"
#ifndef RB1
#include "keygen.h"
#endif
#include "aes.h"
#include <cstring>
#include <cstdio>

#ifndef VR_NO_DECODE

#include <vorbis/vorbisfile.h>
#include <vorbis/codec.h>

size_t vorbisreader_read_cb(void *ptr, size_t size, size_t nmemb, void *datasource) {
	VorbisReader* vr = (VorbisReader*)datasource;
	return vr->ReadRaw(ptr, size, nmemb);
}
int vorbisreader_seek_cb(void *datasource, ogg_int64_t offset, int whence) {
	VorbisReader* vr = (VorbisReader*)datasource;
	return vr->SeekRaw(offset, whence);
}
int vorbisreader_close_cb(void *datasource) {
	VorbisReader* vr = (VorbisReader*)datasource;
	return 0;
}
long vorbisreader_tell_cb(void *datasource) {
	VorbisReader* vr = (VorbisReader*)datasource;
	return vr->TellRaw();
}

ov_callbacks vr_callbacks = {
	vorbisreader_read_cb,
	vorbisreader_seek_cb,
	vorbisreader_close_cb,
	vorbisreader_tell_cb
};

#endif

int VorbisReader::objcount = 0;

VorbisReader::VorbisReader() : fileRef(0), loaded(false)
{
#ifndef VR_NO_DECODE
	memset(&oggStruct, 0, sizeof(&oggStruct));
#endif
	VorbisReader::objcount++;
}


VorbisReader::~VorbisReader()
{
	Close();
	VorbisReader::objcount--;
}

int VorbisReader::Open(void* datasource, ov_callbacks cbStruct)
{
	if (this->fileRef) {
		this->cbStruct.close_func(this->fileRef);
	}
	this->fileRef = datasource;
	this->cbStruct = cbStruct;

	int result = CheckHmxHeader();
	if (result) return result;

	loaded = true;
	SeekRaw(0, SEEK_SET);


#ifndef VR_NO_DECODE
	result = ov_open_callbacks(this, &oggStruct, 0, 0, vr_callbacks);
	if (result != 0) {
		//loaded = false;
		return result; 
	}

	currentSamplePos = 0;
	totalSampleLength = ov_pcm_total(&oggStruct, 0);
#endif
	return 0;
}

#ifndef VR_NO_DECODE

long decodeToFloats(OggVorbis_File*ovf, float***floats, long sampleFrames)
{
	int bitstream = 0;
	int samples_read = ov_read_float(ovf, floats, sampleFrames, &bitstream);
	if (samples_read < 0) {
		return 0;
	}
	return samples_read;
}

long decodeToShorts(OggVorbis_File*ovf, short* shorts, long channels, long numSampleFrames)
{
	int bitstream = 0;
	int bytesPerSampleFrame = 2 * channels;
	return ov_read(ovf, (char*)shorts, numSampleFrames * bytesPerSampleFrame, 0, 2, 1, &bitstream) / bytesPerSampleFrame;
}

void copyToDoubleBuffer(float ** floats, double* buffer, int channels, long frames)
{
	int idx = 0;
	for (int sample = 0; sample < frames; sample++) {
		for (int channel = 0; channel < channels; channel++) {
			buffer[idx++] = floats[channel][sample];
		}
	}
}

void copyToFloatBuffer(float ** floats, float* buffer, int channels, long frames)
{
	int idx = 0;
	for (int sample = 0; sample < frames; sample++) {
		for (int channel = 0; channel < channels; channel++) {
			buffer[idx++] = floats[channel][sample];
		}
	}
}

// Reads samples as floats. Counts are in sample-frames (that is, actual samples divided by number of channels)
long VorbisReader::ReadSamplesFloat(float* buffer, long numSampleFrames)
{
	if (!loaded) {
		return -1;
	}
	float ** floats;
	long channels = oggStruct.vi->channels;
	long sampleFramesD(0), samples_read(0);
	while (numSampleFrames > 0 && (sampleFramesD = decodeToFloats(&oggStruct, &floats, numSampleFrames)) > 0) {
		copyToFloatBuffer(floats, buffer + samples_read * channels, channels, sampleFramesD);
		samples_read += sampleFramesD;
		numSampleFrames -= sampleFramesD;
	}
	currentSamplePos += samples_read;
	return samples_read;
}

// Reads samples as doubles (actually just upcasts floats to doubles but this helps you avoid an extra copy)
long VorbisReader::ReadSamplesDouble(double* buffer, long numSampleFrames)
{
	if (!loaded) {
		return -1;
	}
	float ** floats;
	long channels = oggStruct.vi->channels;
	long sampleFramesD(0), samples_read(0);
	while (numSampleFrames > 0 && (sampleFramesD = decodeToFloats(&oggStruct, &floats, numSampleFrames)) > 0) {
		copyToDoubleBuffer(floats, buffer + samples_read * channels, channels, sampleFramesD);
		samples_read += sampleFramesD;
		numSampleFrames -= sampleFramesD;
	}
	currentSamplePos += samples_read;
	return samples_read;
}

// Reads samples as 16-bit ints. Counts are in sample-frames (that is, actual samples divided by number of channels)
long VorbisReader::ReadSamplesShort(short* buffer, long numSampleFrames)
{
	if (!loaded) {
		return -1;
	}
	long channels = oggStruct.vi->channels;
	long sampleFramesD, samples_read;
	while (numSampleFrames > 0 && (sampleFramesD = decodeToShorts(&oggStruct, buffer + numSampleFrames * channels, channels, numSampleFrames)) > 0) {
		samples_read += sampleFramesD;
		numSampleFrames -= sampleFramesD;
	}
	currentSamplePos += samples_read;
	return samples_read;
}

// Tries to seek to the given sample index. Returns the sample seeked to, or negative number on failure.
long VorbisReader::SeekSamples(int64_t sampleToSeekTo)
{
	if (!loaded) {
		return -1;
	}
	ov_pcm_seek(&oggStruct, sampleToSeekTo);
	currentSamplePos = ov_pcm_tell(&oggStruct);
	return currentSamplePos;
}

#endif

int VorbisReader::Close()
{
	if (fileRef)
		cbStruct.close_func(fileRef);

#ifndef VR_NO_DECODE
	if(oggStruct.datasource)
		ov_clear(&oggStruct);
	memset(&oggStruct, 0, sizeof(&oggStruct));
#endif

	fileRef = 0;
	loaded = 0;
	return 0;
}

int VorbisReader::SeekRaw(long long offset, int whence)
{
	if (!loaded) {
		return -1;
	}

	switch (whence)
	{
	case SEEK_SET:
		if (offset > oggDataSize) {
			offset = oggDataSize;
		}
		oggPos = offset;
		break;
	case SEEK_CUR:
		if (offset + oggPos > oggDataSize) {
			offset = 0;
		}
		oggPos += offset;
		break;
	case SEEK_END:
		if (oggDataSize + oggPos > oggDataSize) {
			offset = 0;
		}
		oggPos = oggDataSize + offset;
		break;
	}
	return cbStruct.seek_func(fileRef, oggPos + hmxHeaderSize, SEEK_SET);
}

size_t VorbisReader::TellRaw()
{
	return loaded ? oggPos : 0;
}

size_t VorbisReader::SizeRaw()
{
	return oggDataSize;
}

size_t VorbisReader::ReadRaw(void * buf, size_t elementSize, size_t elements)
{
	if (!loaded) return 0;

	size_t count = elementSize * elements;
	size_t bytesRead = 0;
	size_t offset = 0;
	uint8_t * buffer = (uint8_t*)buf;

	if (oggPos < 32) {
		while (oggPos < 32 && count > 0 && offset < count) {
			buffer[offset++] = first32[oggPos++];
			count--;
			bytesRead++;
		}
	}

	if (oggPos + count > oggDataSize) {
		count = oggDataSize - oggPos;
	}

	cbStruct.seek_func(fileRef, oggPos + hmxHeaderSize, SEEK_SET);
	size_t actualRead =	cbStruct.read_func(buffer + offset, 1, count, fileRef);

	bytesRead += actualRead;
	oggPos += actualRead;

	if (version > 0x0A)
		DecryptBytes(buffer, offset, actualRead);
	return bytesRead / elementSize;
}

int VorbisReader::CheckHmxHeader()
{
	struct {
		int32_t version;
		uint32_t offset;
	} HmxHeader;

	cbStruct.seek_func(fileRef, 0, SEEK_END);
	uint32_t total_length = cbStruct.tell_func(fileRef);
	cbStruct.seek_func(fileRef, 0, SEEK_SET);
	if (cbStruct.read_func(&HmxHeader, sizeof(HmxHeader), 1, fileRef) != 1)
		return -1;
	version = HmxHeader.version;
	hmxHeaderSize = HmxHeader.offset;
	this->oggDataSize = total_length - hmxHeaderSize;
	ReadOggMap();

	if (version < 0xA || version > 0x11) {
		return -1;
	}
	
	if (version != 0xA) {
		if (cbStruct.read_func(initial_counter.bytes, 16, 1, fileRef) != 1)
			return -1;

		if (version == 0xB) {
			memcpy(ctr_key, ctrKey0B, 0x10);
			ReadFirst32();
		}
#ifndef RB1
		else if (version <= 0x11) {
			uint64_t tmp;
			cbStruct.read_func(&tmp, 8, 1, fileRef);
			seed_1 = (uint32_t)tmp;
			cbStruct.read_func(&tmp, 8, 1, fileRef);
			seed_2 = (uint32_t)tmp;
			cbStruct.seek_func(fileRef, 16, SEEK_CUR);
			uint8_t key[16];
			cbStruct.read_func(key, 16, 1, fileRef);
			byteGrinder.HvDecrypt(decryptedFileKey, key, version);
			if (version == 0x11) {
				cbStruct.read_func(&tmp, 8, 1, fileRef);
				v17_type = (uint32_t)tmp;
			}
			cbStruct.read_func(&tmp, 8, 1, fileRef);
			keyIndex = ((uint32_t)tmp % 6) + 6;
			SetupCypher();
			ReadFirst32();
		}
#endif
	}
	else {
		cbStruct.seek_func(fileRef, hmxHeaderSize, SEEK_SET);
		cbStruct.read_func(first32, 1, 32, fileRef);
		oggPos = 32;
	}

	if (first32[0] != 'O' || first32[1] != 'g' || first32[2] != 'g' || first32[3] != 'S')
		return -1;

	SeekRaw(0, SEEK_SET);
	return 0;
}

void VorbisReader::ReadOggMap()
{
	uint32_t oggMapEntries;
	cbStruct.seek_func(fileRef, 16, SEEK_SET);
	cbStruct.read_func(&oggMapEntries, 4, 1, fileRef);
	cbStruct.seek_func(fileRef, oggMapEntries << 3, SEEK_CUR);
}

uint32_t flip_endianness(uint32_t val)
{
	return ((val << 24) | ((val & 0xFF00) << 8) | ((val & 0xFF0000) >> 8) | ((val & 0xFF000000) >> 24));
}

int VorbisReader::ReadFirst32()
{
	cbStruct.seek_func(fileRef, hmxHeaderSize, SEEK_SET);
	cbStruct.read_func(first32, 32, 1, fileRef);
	oggPos = 32;
	DecryptBytes(first32, 0, 32);
#ifndef RB1
	uint8_t header[4] = { 'H', 'M', 'X', 'A' };
	if (*(uint32_t*)first32 == *(uint32_t*)header) {
		uint32_t* dwords = (uint32_t*)first32;

		// Generate magic numbers to XOR with important Ogg header values
		dwords[0] = 0x5367674F; // "OggS" in little-endian
		dwords[3] ^= flip_endianness(magicNumberGenerator(seed_1, 1));
		dwords[5] ^= flip_endianness(magicNumberGenerator(seed_2, 2));
	}
#endif
	return 0;
}

void VorbisReader::SetupCypher()
{
#ifndef RB1
	uint8_t masterKey[32];
	uint8_t tmpKey[16];

	returnMasterKey(masterKey);
	if (version == 17) {
		KeyChain::getKey17(keyIndex, tmpKey, masterKey, v17_type);
	}
	else {
		KeyChain::getKey((int)keyIndex, tmpKey, masterKey, version);
	}

	byteGrinder.GrindArray((uint32_t)seed_1, (uint32_t)seed_2, tmpKey, 16, version);

	for (int i = 0; i < 16; i++) {
		ctr_key[i] = tmpKey[i] ^ decryptedFileKey[i];
	}
#endif
}

// Fixes the counter
void VorbisReader::FixCounter(size_t decryptedPos) {
	counter = initial_counter;
	uint64_t low = counter.qwords[0];
	counter.qwords[0] += (decryptedPos >> 4);
	if (counter.qwords[0] < low) {
		counter.qwords[1]++;
	}
}

// Increments a 128-bit counter using 64-bit word size
inline void IncrementCounter(aes_ctr_128* ctr) {
	for (int ptr = 0; ptr < 2; ptr++) {
		ctr->qwords[ptr]++;
		if (ctr->qwords[ptr] != 0)
			break;
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
void VorbisReader::DecryptBytes(uint8_t* buffer, size_t offset, size_t count)
{
	aes_ctr_128 cryptedCounter;
	size_t decryptedPos = oggPos - count;
	int counterLoc = decryptedPos % 16;

	FixCounter(decryptedPos);
	AES128_ECB_encrypt(counter.bytes, ctr_key, cryptedCounter.bytes);
	for (int i = 0; i < count; i++)
	{
		if (decryptedPos != 0 && decryptedPos % 16 == 0 && i != 0)
		{
			FixCounter(decryptedPos);
			counterLoc = 0;
			AES128_ECB_encrypt(counter.bytes, ctr_key, cryptedCounter.bytes);
		}
		buffer[i + offset] ^= cryptedCounter.bytes[counterLoc]; //decrypt one byte
		decryptedPos++;
		counterLoc++;
	}
}
