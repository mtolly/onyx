#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <float.h>
#include <string.h>
#include "dsptool.h"

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

/* Temporal Vector
* A contiguous history of 3 samples starting with
* 'current' and going 2 backwards
*/
typedef double tvec[3];
void correlateCoefs(int16_t* source, uint32_t samples, int16_t* coefsOut);
void DSPEncodeFrame(short pcmInOut[16], int sampleCount, unsigned char adpcmOut[8], const short coefsIn[8][2]);

void encode(int16_t* src, uint8_t* dst, ADPCMINFO* cxt, uint32_t samples)
{
	int16_t* coefs = cxt->coef;
	correlateCoefs(src, samples, coefs);

	int32_t frameCount = samples / SAMPLES_PER_FRAME + (samples % SAMPLES_PER_FRAME != 0);

	int16_t* pcm = src;
	uint8_t* adpcm = dst;
	int16_t pcmFrame[SAMPLES_PER_FRAME + 2] = { 0 };
	uint8_t adpcmFrame[BYTES_PER_FRAME] = { 0 };

	for (int i = 0; i < frameCount; ++i, pcm += SAMPLES_PER_FRAME, adpcm += BYTES_PER_FRAME)
	{
		int32_t sampleCount = MIN(samples - i * SAMPLES_PER_FRAME, SAMPLES_PER_FRAME);
		memset(pcmFrame + 2, 0, SAMPLES_PER_FRAME * sizeof(int16_t));
		memcpy(pcmFrame + 2, pcm, sampleCount * sizeof(int16_t));

		DSPEncodeFrame(pcmFrame, SAMPLES_PER_FRAME, adpcmFrame, (short(*)[2])&coefs[0]);

		pcmFrame[0] = pcmFrame[14];
		pcmFrame[1] = pcmFrame[15];

		memcpy(adpcm, adpcmFrame, getBytesForAdpcmSamples(sampleCount));
	}

	cxt->gain = 0;
	cxt->pred_scale = *dst;
	cxt->yn1 = 0;
	cxt->yn2 = 0;
}

void InnerProductMerge(tvec vecOut, short pcmBuf[14])
{
	for (int i = 0; i <= 2; i++)
	{
		vecOut[i] = 0.0f;
		for (int x = 0; x < 14; x++)
			vecOut[i] -= pcmBuf[x - i] * pcmBuf[x];
	}
}

void OuterProductMerge(tvec mtxOut[3], short pcmBuf[14])
{
	for (int x = 1; x <= 2; x++)
		for (int y = 1; y <= 2; y++)
		{
			mtxOut[x][y] = 0.0;
			for (int z = 0; z < 14; z++)
				mtxOut[x][y] += pcmBuf[z - x] * pcmBuf[z - y];
		}
}

bool AnalyzeRanges(tvec mtx[3], int* vecIdxsOut)
{
	double recips[3];
	double val, tmp, min, max;

	/* Get greatest distance from zero */
	for (int x = 1; x <= 2; x++)
	{
		val = MAX(fabs(mtx[x][1]), fabs(mtx[x][2]));
		if (val < DBL_EPSILON)
			return true;

		recips[x] = 1.0 / val;
	}

	int maxIndex = 0;
	for (int i = 1; i <= 2; i++)
	{
		for (int x = 1; x < i; x++)
		{
			tmp = mtx[x][i];
			for (int y = 1; y < x; y++)
				tmp -= mtx[x][y] * mtx[y][i];
			mtx[x][i] = tmp;
		}

		val = 0.0;
		for (int x = i; x <= 2; x++)
		{
			tmp = mtx[x][i];
			for (int y = 1; y < i; y++)
				tmp -= mtx[x][y] * mtx[y][i];

			mtx[x][i] = tmp;
			tmp = fabs(tmp) * recips[x];
			if (tmp >= val)
			{
				val = tmp;
				maxIndex = x;
			}
		}

		if (maxIndex != i)
		{
			for (int y = 1; y <= 2; y++)
			{
				tmp = mtx[maxIndex][y];
				mtx[maxIndex][y] = mtx[i][y];
				mtx[i][y] = tmp;
			}
			recips[maxIndex] = recips[i];
		}

		vecIdxsOut[i] = maxIndex;

		if (mtx[i][i] == 0.0)
			return true;

		if (i != 2)
		{
			tmp = 1.0 / mtx[i][i];
			for (int x = i + 1; x <= 2; x++)
				mtx[x][i] *= tmp;
		}
	}

	/* Get range */
	min = 1.0e10;
	max = 0.0;
	for (int i = 1; i <= 2; i++)
	{
		tmp = fabs(mtx[i][i]);
		if (tmp < min)
			min = tmp;
		if (tmp > max)
			max = tmp;
	}

	if (min / max < 1.0e-10)
		return true;

	return false;
}

void BidirectionalFilter(tvec mtx[3], int* vecIdxs, tvec vecOut)
{
	double tmp;

	for (int i = 1, x = 0; i <= 2; i++)
	{
		int index = vecIdxs[i];
		tmp = vecOut[index];
		vecOut[index] = vecOut[i];
		if (x != 0)
			for (int y = x; y <= i - 1; y++)
				tmp -= vecOut[y] * mtx[i][y];
		else if (tmp != 0.0)
			x = i;
		vecOut[i] = tmp;
	}

	for (int i = 2; i > 0; i--)
	{
		tmp = vecOut[i];
		for (int y = i + 1; y <= 2; y++)
			tmp -= vecOut[y] * mtx[i][y];
		vecOut[i] = tmp / mtx[i][i];
	}

	vecOut[0] = 1.0;
}

bool QuadraticMerge(tvec inOutVec)
{
	double v0, v1, v2 = inOutVec[2];
	double tmp = 1.0 - (v2 * v2);

	if (tmp == 0.0)
		return true;

	v0 = (inOutVec[0] - (v2 * v2)) / tmp;
	v1 = (inOutVec[1] - (inOutVec[1] * v2)) / tmp;

	inOutVec[0] = v0;
	inOutVec[1] = v1;

	return fabs(v1) > 1.0;
}

void FinishRecord(tvec in, tvec out)
{
	for (int z = 1; z <= 2; z++)
	{
		if (in[z] >= 1.0)
			in[z] = 0.9999999999;
		else if (in[z] <= -1.0)
			in[z] = -0.9999999999;
	}
	out[0] = 1.0;
	out[1] = (in[2] * in[1]) + in[1];
	out[2] = in[2];
}

void MatrixFilter(tvec src, tvec dst)
{
	tvec mtx[3];

	mtx[2][0] = 1.0;
	for (int i = 1; i <= 2; i++)
		mtx[2][i] = -src[i];

	for (int i = 2; i > 0; i--)
	{
		double val = 1.0 - (mtx[i][i] * mtx[i][i]);
		for (int y = 1; y <= i; y++)
			mtx[i - 1][y] = ((mtx[i][i] * mtx[i][y]) + mtx[i][y]) / val;
	}

	dst[0] = 1.0;
	for (int i = 1; i <= 2; i++)
	{
		dst[i] = 0.0;
		for (int y = 1; y <= i; y++)
			dst[i] += mtx[i][y] * dst[i - y];
	}
}

void MergeFinishRecord(tvec src, tvec dst)
{
	tvec tmp;
	double val = src[0];

	dst[0] = 1.0;
	for (int i = 1; i <= 2; i++)
	{
		double v2 = 0.0;
		for (int y = 1; y < i; y++)
			v2 += dst[y] * src[i - y];

		if (val > 0.0)
			dst[i] = -(v2 + src[i]) / val;
		else
			dst[i] = 0.0;

		tmp[i] = dst[i];

		for (int y = 1; y < i; y++)
			dst[y] += dst[i] * dst[i - y];

		val *= 1.0 - (dst[i] * dst[i]);
	}

	FinishRecord(tmp, dst);
}

double ContrastVectors(tvec source1, tvec source2)
{
	double val = (source2[2] * source2[1] + -source2[1]) / (1.0 - source2[2] * source2[2]);
	double val1 = (source1[0] * source1[0]) + (source1[1] * source1[1]) + (source1[2] * source1[2]);
	double val2 = (source1[0] * source1[1]) + (source1[1] * source1[2]);
	double val3 = source1[0] * source1[2];
	return val1 + (2.0 * val * val2) + (2.0 * (-source2[1] * val + -source2[2]) * val3);
}

void FilterRecords(tvec vecBest[8], int exp, tvec records[], int recordCount)
{
	tvec bufferList[8];

	int buffer1[8];
	tvec buffer2;

	int index;
	double value, tempVal = 0;

	for (int x = 0; x < 2; x++)
	{
		for (int y = 0; y < exp; y++)
		{
			buffer1[y] = 0;
			for (int i = 0; i <= 2; i++)
				bufferList[y][i] = 0.0;
		}
		for (int z = 0; z < recordCount; z++)
		{
			index = 0;
			value = 1.0e30;
			for (int i = 0; i < exp; i++)
			{
				tempVal = ContrastVectors(vecBest[i], records[z]);
				if (tempVal < value)
				{
					value = tempVal;
					index = i;
				}
			}
			buffer1[index]++;
			MatrixFilter(records[z], buffer2);
			for (int i = 0; i <= 2; i++)
				bufferList[index][i] += buffer2[i];
		}

		for (int i = 0; i < exp; i++)
			if (buffer1[i] > 0)
				for (int y = 0; y <= 2; y++)
					bufferList[i][y] /= buffer1[i];

		for (int i = 0; i < exp; i++)
			MergeFinishRecord(bufferList[i], vecBest[i]);
	}
}

void correlateCoefs(int16_t* source, uint32_t samples, int16_t* coefsOut)
{
	int numFrames = (samples + 13) / 14;
	int frameSamples;

	short* blockBuffer = (short*)calloc(sizeof(short), 0x3800);
	short pcmHistBuffer[2][14] = { 0 };

	tvec vec1;
	tvec vec2;

	tvec mtx[3];
	int vecIdxs[3];

	tvec* records = (tvec*)calloc(sizeof(tvec), numFrames * 2);
	int recordCount = 0;

	tvec vecBest[8];

	/* Iterate though 1024-block frames */
	for (int x = samples; x > 0;)
	{
		if (x > 0x3800) /* Full 1024-block frame */
		{
			frameSamples = 0x3800;
			x -= 0x3800;
		}
		else /* Partial frame */
		{
			/* Zero lingering block samples */
			frameSamples = x;
			for (int z = 0; z < 14 && z + frameSamples < 0x3800; z++)
				blockBuffer[frameSamples + z] = 0;
			x = 0;
		}

		/* Copy (potentially non-frame-aligned PCM samples into aligned buffer) */
		memcpy(blockBuffer, source, frameSamples * sizeof(short));
		source += frameSamples;


		for (int i = 0; i < frameSamples;)
		{
			for (int z = 0; z < 14; z++)
				pcmHistBuffer[0][z] = pcmHistBuffer[1][z];
			for (int z = 0; z < 14; z++)
				pcmHistBuffer[1][z] = blockBuffer[i++];

			InnerProductMerge(vec1, pcmHistBuffer[1]);
			if (fabs(vec1[0]) > 10.0)
			{
				OuterProductMerge(mtx, pcmHistBuffer[1]);
				if (!AnalyzeRanges(mtx, vecIdxs))
				{
					BidirectionalFilter(mtx, vecIdxs, vec1);
					if (!QuadraticMerge(vec1))
					{
						FinishRecord(vec1, records[recordCount]);
						recordCount++;
					}
				}
			}
		}
	}

	vec1[0] = 1.0;
	vec1[1] = 0.0;
	vec1[2] = 0.0;

	for (int z = 0; z < recordCount; z++)
	{
		MatrixFilter(records[z], vecBest[0]);
		for (int y = 1; y <= 2; y++)
			vec1[y] += vecBest[0][y];
	}
	for (int y = 1; y <= 2; y++)
		vec1[y] /= recordCount;

	MergeFinishRecord(vec1, vecBest[0]);


	int exp = 1;
	for (int w = 0; w < 3;)
	{
		vec2[0] = 0.0;
		vec2[1] = -1.0;
		vec2[2] = 0.0;
		for (int i = 0; i < exp; i++)
			for (int y = 0; y <= 2; y++)
				vecBest[exp + i][y] = (0.01 * vec2[y]) + vecBest[i][y];
		++w;
		exp = 1 << w;
		FilterRecords(vecBest, exp, records, recordCount);
	}

	/* Write output */
	for (int z = 0; z < 8; z++)
	{
		double d;
		d = -vecBest[z][1] * 2048.0;
		if (d > 0.0)
			coefsOut[z * 2] = (d > 32767.0) ? (short)32767 : (short)lround(d);
		else
			coefsOut[z * 2] = (d < -32768.0) ? (short)-32768 : (short)lround(d);

		d = -vecBest[z][2] * 2048.0;
		if (d > 0.0)
			coefsOut[z * 2 + 1] = (d > 32767.0) ? (short)32767 : (short)lround(d);
		else
			coefsOut[z * 2 + 1] = (d < -32768.0) ? (short)-32768 : (short)lround(d);
	}

	/* Free memory */
	free(records);
	free(blockBuffer);
}

/* Make sure source includes the yn values (16 samples total) */
void DSPEncodeFrame(short pcmInOut[16], int sampleCount, unsigned char adpcmOut[8], const short coefsIn[8][2])
{
	int inSamples[8][16];
	int outSamples[8][14];

	int bestIndex = 0;

	int scale[8];
	double distAccum[8];

	/* Iterate through each coef set, finding the set with the smallest error */
	for (int i = 0; i < 8; i++)
	{
		int v1, v2, v3;
		int distance, index;

		/* Set yn values */
		inSamples[i][0] = pcmInOut[0];
		inSamples[i][1] = pcmInOut[1];

		/* Round and clamp samples for this coef set */
		distance = 0;
		for (int s = 0; s < sampleCount; s++)
		{
			/* Multiply previous samples by coefs */
			inSamples[i][s + 2] = v1 = ((pcmInOut[s] * coefsIn[i][1]) + (pcmInOut[s + 1] * coefsIn[i][0])) / 2048;
			/* Subtract from current sample */
			v2 = pcmInOut[s + 2] - v1;
			/* Clamp */
			v3 = (v2 >= 32767) ? 32767 : (v2 <= -32768) ? -32768 : v2;
			/* Compare distance */
			if (abs(v3) > abs(distance))
				distance = v3;
		}

		/* Set initial scale */
		for (scale[i] = 0; (scale[i] <= 12) && ((distance > 7) || (distance < -8)); scale[i]++, distance /= 2)
		{
		}
		scale[i] = (scale[i] <= 1) ? -1 : scale[i] - 2;

		do
		{
			scale[i]++;
			distAccum[i] = 0;
			index = 0;

			for (int s = 0; s < sampleCount; s++)
			{
				/* Multiply previous */
				v1 = ((inSamples[i][s] * coefsIn[i][1]) + (inSamples[i][s + 1] * coefsIn[i][0]));
				/* Evaluate from real sample */
				v2 = (pcmInOut[s + 2] << 11) - v1;
				/* Round to nearest sample */
				v3 = (v2 > 0) ? (int)((double)v2 / (1 << scale[i]) / 2048 + 0.4999999f) : (int)((double)v2 / (1 << scale[i]) / 2048 - 0.4999999f);

				/* Clamp sample and set index */
				if (v3 < -8)
				{
					if (index < (v3 = -8 - v3))
						index = v3;
					v3 = -8;
				}
				else if (v3 > 7)
				{
					if (index < (v3 -= 7))
						index = v3;
					v3 = 7;
				}

				/* Store result */
				outSamples[i][s] = v3;

				/* Round and expand */
				v1 = (v1 + ((v3 * (1 << scale[i])) << 11) + 1024) >> 11;
				/* Clamp and store */
				inSamples[i][s + 2] = v2 = (v1 >= 32767) ? 32767 : (v1 <= -32768) ? -32768 : v1;
				/* Accumulate distance */
				v3 = pcmInOut[s + 2] - v2;
				distAccum[i] += v3 * (double)v3;
			}

			for (int x = index + 8; x > 256; x >>= 1)
				if (++scale[i] >= 12)
					scale[i] = 11;
		} while ((scale[i] < 12) && (index > 1));
	}

	double min = DBL_MAX;
	for (int i = 0; i < 8; i++)
	{
		if (distAccum[i] < min)
		{
			min = distAccum[i];
			bestIndex = i;
		}
	}

	/* Write converted samples */
	for (int s = 0; s < sampleCount; s++)
		pcmInOut[s + 2] = inSamples[bestIndex][s + 2];

	/* Write ps */
	adpcmOut[0] = (char)((bestIndex << 4) | (scale[bestIndex] & 0xF));

	/* Zero remaining samples */
	for (int s = sampleCount; s < 14; s++)
		outSamples[bestIndex][s] = 0;

	/* Write output samples */
	for (int y = 0; y < 7; y++)
	{
		adpcmOut[y + 1] = (char)((outSamples[bestIndex][y * 2] << 4) | (outSamples[bestIndex][y * 2 + 1] & 0xF));
	}
}

void encodeFrame(int16_t* src, uint8_t* dst, int16_t* coefs, uint8_t one)
{
	DSPEncodeFrame(src, 14, dst, (short(*)[2])&coefs[0]);
}
