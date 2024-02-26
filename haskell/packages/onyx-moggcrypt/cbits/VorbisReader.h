#pragma once

#ifndef RB1
#include "ByteGrinder.h"
#endif

#ifndef VR_NO_DECODE
#include <vorbis/vorbisfile.h>
#include <vorbis/codec.h>
#endif

#ifndef _OV_FILE_H_
#include "XiphTypes.h"
#endif

#include <inttypes.h>

union aes_ctr_128 {
	uint64_t qwords[2];
	uint32_t dwords[4];
	uint16_t words[8];
	uint8_t bytes[16];
};


class VorbisReader
{
public:
	static int objcount;

	VorbisReader();
	~VorbisReader();
	
	// Opens a mogg file. Files larger than 2^31 - 1 bytes are not supported. Fortunately such a mogg would be many, many hours long.
	int Open(void* datasource, ov_callbacks cbStruct);

	bool IsOpen() { return fileRef != 0; }

#ifndef VR_NO_DECODE
	// Reads samples as floats. Counts are in sample-frames (that is, actual samples divided by number of channels)
	long ReadSamplesFloat(float* buffer, long numSampleFrames);

	// Reads samples as doubles (actually just upcasts floats to doubles but this helps you avoid an extra copy)
	long ReadSamplesDouble(double* buffer, long numSampleFrames);

	// Reads samples as 16-bit ints. Counts are in sample-frames (that is, actual samples divided by number of channels)
	long ReadSamplesShort(short* buffer, long numSampleFrames);

	// Tries to seek to the given sample index. Returns the sample seeked to, or negative number on failure.
	long SeekSamples(int64_t sampleToSeekTo);

	// Tells the last decoded position.
	long TellSamples() { return currentSamplePos; }

	// Gets the number of channels in the file.
	int GetNumChannels() { return loaded ? oggStruct.vi->channels : 1; }
	
	// Gets the sample rate in hz
	long GetSampleRate() { return loaded ? oggStruct.vi->rate : 1; }

	// Gets the length in samples
	int GetLength() { return loaded ? totalSampleLength : 0; }
#endif

	// Read raw ogg data (virtual file starting after HMX header)
	size_t ReadRaw(void* buffer, size_t elementSize, size_t elements);

	// Seek throw raw ogg data (virtual file starting after HMX header)
	int SeekRaw(long long pos, int whence);

	// Tell postition in raw ogg data(virtual file starting after HMX header)
	size_t TellRaw();

	// Close the underlying ogg file
	int Close();
private:
#ifndef VR_NO_DECODE
	OggVorbis_File oggStruct;
	long currentSamplePos;
	int64_t totalSampleLength;
#endif

	size_t oggPos{ 0 };
	size_t oggDataSize{ 0 };
	bool loaded{ false };
	ov_callbacks cbStruct{};
	void * fileRef;

/***** Decryption Stuff ******/
	int CheckHmxHeader();
	void ReadOggMap();
	void SetupCypher();
	void FixCounter(size_t decryptedPos);
	// Transform HMXA header into OggS
	int ReadFirst32();
	// Decrypts count bytes in the buffer starting at offset
	void DecryptBytes(uint8_t* buffer, size_t offset, size_t count);

	size_t hmxHeaderSize{ 0 };
	aes_ctr_128 initial_counter{};
	aes_ctr_128 counter{};
	uint8_t ctr_key[16];
	uint8_t first32[32];
	int32_t version;

#ifndef RB1
	ByteGrinder byteGrinder;
	uint32_t seed_1;
	uint32_t seed_2;
	uint32_t keyIndex;
	uint32_t v17_type;
	uint8_t  decryptedFileKey[16];
#endif
};

