#include "keygen.h"
#include "keys.h"
#include <cstring>
#include <stdio.h>

#ifndef RB1

char asciiDigitToHex(char a)
{
	//48=0 (subtract 48)
	//65=A (subtract 55)
	//97=a (subtract 87)
	if (a > 96)
		return a - 87;
	else if (a > 64)
		return a - 55;
	else if (a > 47)
		return a - 48;
	else
		return 0;
}

char asciiDigitToHex17(unsigned char a)
{
	if ((unsigned char)(a - 97) > 5u) {
		if ((unsigned char)(a - 65) > 5u)
			return a - 48;
		else
			return a - 55;
	}
	return a - 87;
}

void parseHex16(const unsigned char* str, unsigned char* dest)
{
	for (int count = 0; count < 16; count++) {
		dest[count] = asciiDigitToHex(str[count * 2]) << 4 | asciiDigitToHex(str[count * 2 + 1]);
	}
}

void parseHex16_2(const unsigned char* src, unsigned char* dest, char(*parseNibble)(unsigned char)) {
	for (int count = 0; count < 16; count++) {
		dest[count] = (parseNibble(src[count * 2]) << 4) + parseNibble(src[count * 2 + 1]);
	}
}

static int32_t s_seed = 0xEB;
int32_t mcrandom(int32_t seed)
{
	if (seed) s_seed = seed;
	s_seed = s_seed * 0x19660E + 0x3C6EF35F;
	return s_seed;
}

void mash(unsigned char* x1, const unsigned char* x2)
{
	for (int c = 0; c < 32; c++) {
		x1[c] ^= x2[c];
	}
}

int32_t roll(int32_t arg)
{
	return (arg + 19) % 32;
}

void swap(unsigned char &c1, unsigned char &c2)
{
	unsigned char tmp = c1;
	c1 = c2;
	c2 = tmp;
}

void shuffle1(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++) {
		swap(ptr[roll(i * 4)], ptr[i * 4 + 2]);
		swap(ptr[roll(i * 4 + 3)], ptr[i * 4 + 1]);
	}
}

void shuffle2(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++)	{
		swap(ptr[29 - (i * 4)], ptr[i * 4 + 2]);
		swap(ptr[28 - (i * 4)], ptr[i * 4 + 3]);
	}
}

void shuffle3(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++)	{
		swap(ptr[roll(4 * (7 - i) + 1)], ptr[i * 4 + 2]);
		swap(ptr[4 * (7 - i)], ptr[i * 4 + 3]);
	}
}
void shuffle4(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++)	{
		swap(ptr[29 - (i * 4)], ptr[i * 4 + 2]);
		swap(ptr[roll(4 * (7 - i))], ptr[i * 4 + 3]);
	}
}
void shuffle5(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++)	{
		swap(ptr[29 - (i * 4)], ptr[roll(i * 4 + 2)]);
		swap(ptr[28 - (i * 4)], ptr[i * 4 + 3]);
	}
}
void shuffle6(unsigned char* ptr)
{
	for (int i = 0; i < 8; i++)	{
		swap(ptr[29 - (i * 4)], ptr[i * 4 + 2]);
		swap(ptr[28 - (i * 4)], ptr[roll(i * 4 + 3)]);
	}
}

void supershuffle(unsigned char* buffer)
{
	shuffle1(buffer);
	shuffle2(buffer);
	shuffle3(buffer);
	shuffle4(buffer);
	shuffle5(buffer);
	shuffle6(buffer);
}

void revealKey(const unsigned char* masterKey, unsigned char* revealed)
{
	for (int c = 14; c > 0; c--) {
		supershuffle(revealed);
	}
	//XOR all bytes in hiddenKey with transform
	mash(revealed, masterKey);
}
 
void returnMasterKey(unsigned char* buffer)
{
	// This function does a lot of random shit but all that matters is
	// that getMasher makes the master key just by running that LCG on
	// a seed of 0xEB. It stores it in some random place determined by
	// data script but that also doesn't matter for us. Obscurity does
	// not make good security, apparently!
	KeyChain::getMasher(buffer);
}

void KeyChain::getKey(int offset, unsigned char* key, const unsigned char* masterKey, int version)
{
	unsigned char revealedKey[32];
	memcpy(revealedKey, (hiddenKeys + 32 * offset), 32);
	revealKey(masterKey, revealedKey);
	parseHex16(revealedKey, key);
}

const int kTypeRB4 = 0x1;
const int kTypeDropMix = 0x4;
const int kTypeAudica = 0x8;
const int kTypeFuser = 0xA;

void KeyChain::getKey17(int offset, unsigned char* key, const unsigned char* masterKey, int type)
{
	unsigned char revealedKey[32];
	const unsigned char* keyTable;

	switch (type) {
		case kTypeDropMix:
			keyTable = hiddenKeys_DM;
			break;
		case kTypeRB4:
			keyTable = hiddenKeys_RB4;
			break;
		case kTypeAudica:
			keyTable = hiddenKeys_Audica;
			break;
		case kTypeFuser:
			keyTable = hiddenKeys_FUSER;
			break;
		default:
			// This won't work, but I don't like non-exhaustive switches.
			keyTable = hiddenKeys;
			break;
	}
	memcpy(revealedKey, (keyTable + 32 * offset), 32);
	revealKey(masterKey, revealedKey);
	parseHex16_2(revealedKey, key, asciiDigitToHex17);
}

inline void bswap(int32_t* word)
{
	uint8_t *buf = (uint8_t*)word;
	uint8_t tmp = buf[0];
	buf[0] = buf[3];
	buf[3] = tmp;
	tmp = buf[1];
	buf[1] = buf[2];
	buf[2] = buf[1];
}

void KeyChain::getMasher(unsigned char* buf)
{
	int32_t* buf_long = (int32_t*)buf;

	int n = 1;
	bool littleEndian = *(char *)&n != 1;
	
	for (int i = 0; i < 8; i++) {
		int32_t result = mcrandom(i == 0 ? 0xEB : 0);
		buf_long[i] = result;
		if (littleEndian) {
			bswap(&buf_long[i]);
		}
	}
}

#endif