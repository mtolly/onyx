#include <stdint.h>
#include "dsptool.h"
#include <limits.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

int DivideByRoundUp(int dividend, int divisor)
{
	return (dividend + divisor - 1) / divisor;
}

char GetHighNibble(char value)
{
	return value >> 4 & 0xF;
}

char GetLowNibble(char value)
{
	return value & 0xF;
}

short Clamp16(int value)
{
	if (value > SHRT_MAX)
		return SHRT_MAX;
	if (value < SHRT_MIN)
		return SHRT_MIN;
	return value;
}

void decode(uint8_t* src, int16_t* dst, ADPCMINFO* cxt, uint32_t samples)
{
	short hist1 = cxt->yn1;
	short hist2 = cxt->yn2;
	short* coefs = cxt->coef;
	int frameCount = DivideByRoundUp(samples, SAMPLES_PER_FRAME);
	int samplesRemaining = samples;

	for (int i = 0; i < frameCount; i++)
	{
		int predictor = GetHighNibble(*src);
		int scale = 1 << GetLowNibble(*src++);
		short coef1 = coefs[predictor * 2];
		short coef2 = coefs[predictor * 2 + 1];

		int samplesToRead = MIN(SAMPLES_PER_FRAME, samplesRemaining);

		for (int s = 0; s < samplesToRead; s++)
		{
			int sample = s % 2 == 0 ? GetHighNibble(*src) : GetLowNibble(*src++);
			sample = sample >= 8 ? sample - 16 : sample;
			sample = (((scale * sample) << 11) + 1024 + (coef1 * hist1 + coef2 * hist2)) >> 11;
			short finalSample = Clamp16(sample);

			hist2 = hist1;
			hist1 = finalSample;

			*dst++ = finalSample;
		}

		samplesRemaining -= samplesToRead;
	}
}

void getLoopContext(uint8_t* src, ADPCMINFO* cxt, uint32_t samples)
{
	short hist1 = cxt->yn1;
	short hist2 = cxt->yn2;
	short* coefs = cxt->coef;
	char ps = 0;
	int frameCount = DivideByRoundUp(samples, SAMPLES_PER_FRAME);
	int samplesRemaining = samples;

	for (int i = 0; i < frameCount; i++)
	{
		ps = *src;
		int predictor = GetHighNibble(*src);
		int scale = 1 << GetLowNibble(*src++);
		short coef1 = coefs[predictor * 2];
		short coef2 = coefs[predictor * 2 + 1];

		int samplesToRead = MIN(SAMPLES_PER_FRAME, samplesRemaining);

		for (int s = 0; s < samplesToRead; s++)
		{
			int sample = s % 2 == 0 ? GetHighNibble(*src) : GetLowNibble(*src++);
			sample = sample >= 8 ? sample - 16 : sample;
			sample = (((scale * sample) << 11) + 1024 + (coef1 * hist1 + coef2 * hist2)) >> 11;
			short finalSample = Clamp16(sample);

			hist2 = hist1;
			hist1 = finalSample;
		}
		samplesRemaining -= samplesToRead;
	}

	cxt->loop_pred_scale = ps;
	cxt->loop_yn1 = hist1;
	cxt->loop_yn2 = hist2;
}
