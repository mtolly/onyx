#include "dsptool.h"

uint32_t getBytesForAdpcmBuffer(uint32_t samples)
{
	uint32_t frames = samples / SAMPLES_PER_FRAME;
	if (samples % SAMPLES_PER_FRAME)
		frames++;

	return frames * BYTES_PER_FRAME;
}

uint32_t getBytesForAdpcmInfo()
{
	return sizeof(ADPCMINFO);
}

uint32_t getBytesForAdpcmSamples(uint32_t samples)
{
	uint32_t extraBytes = 0;
	uint32_t frames = samples / SAMPLES_PER_FRAME;
	uint32_t extraSamples = samples % SAMPLES_PER_FRAME;

	if (extraSamples)
	{
		extraBytes = (extraSamples / 2) + (extraSamples % 2) + 1;
	}

	return BYTES_PER_FRAME * frames + extraBytes;
}

uint32_t getBytesForPcmBuffer(uint32_t samples)
{
	uint32_t frames = samples / SAMPLES_PER_FRAME;
	if (samples % SAMPLES_PER_FRAME)
		frames++;

	return frames * SAMPLES_PER_FRAME * sizeof(int16_t);
}

uint32_t getBytesForPcmSamples(uint32_t samples)
{
	return samples * sizeof(int16_t);
}

uint32_t getNibbleAddress(uint32_t samples)
{
	int frames = samples / SAMPLES_PER_FRAME;
	int extraSamples = samples % SAMPLES_PER_FRAME;

	return NIBBLES_PER_FRAME * frames + extraSamples + 2;
}

uint32_t getNibblesForNSamples(uint32_t samples)
{
	uint32_t frames = samples / SAMPLES_PER_FRAME;
	uint32_t extraSamples = samples % SAMPLES_PER_FRAME;
	uint32_t extraNibbles = extraSamples == 0 ? 0 : extraSamples + 2;

	return NIBBLES_PER_FRAME * frames + extraNibbles;
}

uint32_t getSampleForAdpcmNibble(uint32_t nibble)
{
	uint32_t frames = nibble / NIBBLES_PER_FRAME;
	uint32_t extraNibbles = nibble % NIBBLES_PER_FRAME;
	uint32_t samples = SAMPLES_PER_FRAME * frames;

	return samples + extraNibbles - 2;
}
