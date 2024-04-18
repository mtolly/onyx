#include "ByteGrinder.h"
#include "aes.h"
#include "keys.h"
#include <cstring>

#ifndef RB1

ByteGrinder::ByteGrinder()
{
	// Initialize O functions (map data Oxx funcs to opXX function pointers)
	PickOneOf32A(true, 0xD5);
	for (int i = 0; i < 32; i++) {
		O[PickOneOf32A(false, 0)] = opsRB2[i];
	}
	PickOneOf32A(true, 0x23E);
	for (int i = 0; i < 32; i++) {
		O[PickOneOf32A(false, 0) + 32] = opsRB3[i];
	}
}


ByteGrinder::~ByteGrinder()
{
}

void ByteGrinder::HvDecrypt(uint8_t* dest, const uint8_t* src, int version)
{
	const uint8_t* HvKey = HvKeys + (GetEncMethod(version) << 4);
	AES128_ECB_decrypt(src, HvKey, dest);
}

unsigned int ByteGrinder::PickOneOf32A(bool init, unsigned int seed)
{
	if (init) {
		seedA = seed;
		memset(usedUpA, 0, 32);
		return 0x610A660F;
	}
	uint32_t tmp;
	do {
		seedA = (seedA * 0x19660D + 0x3C6EF35F);
		tmp = (seedA >> 2) & 0x1F;
	} while (usedUpA[tmp] == 1);
	usedUpA[tmp] = 1;
	return tmp;
}

unsigned int ByteGrinder::PickOneOf32B(bool init, unsigned int seed)
{
	if (init) {
		seedB = seed;
		memset(usedUpB, 0, 32);
		return 0x610A660F;
	}
	uint32_t tmp;
	do {
		seedB = (seedB * 0x19660D + 0x3C6EF35F);
		tmp = (seedB >> 2) & 0x1F;
	} while (usedUpB[tmp] == 1);
	usedUpB[tmp] = 1;
	return tmp;
}

void ByteGrinder::fillHashMap5Bit(uint32_t seed)
{
	for (int i = 0; i < 256; i++) {
		hashMap[i] = (seed >> 3) & 0x1F;
		seed = (0x19660D * seed + 0x3C6EF35F);
	}
}

void ByteGrinder::fillHashMap6Bit(uint32_t seed)
{
	for (int i = 0; i < 256; i++) {
		hashMap[i] = (seed >> 2) & 0x3F;
		seed = (0x19660D * seed + 0x3C6EF35F);
	}
}

void ByteGrinder::GrindArray(uint32_t seed1, uint32_t seed2, uint8_t * key, int keylength, int version)
{
	if (version > 0x0D) {
		fillHashMap6Bit(seed2);
	}
	else {
		fillHashMap5Bit(seed1);
	}

	// Create Data command string
	uint8_t switchCases[64];
	PickOneOf32B(true, seed2);
	for (int i = 0; i<0x20; i++) {
		int func = PickOneOf32B(false, 0);
		switchCases[i] = func;
	}

	// (Newer crypt versions have 32 more possible ops in the string)
	if (version > 0xD) { 
		PickOneOf32B(true, seed1);
		for (int i = 0x20; i<0x40; i++) {
			int func = PickOneOf32B(false, 0);
			switchCases[i] = func + 0x20;
		}
	}

	// Apply command string over every byte
	uint8_t foo;
	uint8_t* bar = key;
	for (int i = 0; i < keylength; i++) {
		foo = bar[i];
		for (int ix = 0; ix < keylength; ix+=2) {
			foo = O[switchCases[hashMap[bar[ix]]]](bar[ix + 1], foo);
		}
		bar[i] = foo;
	}
}

int GetEncMethod(int version) {
	if (version == 0xE) return 1;
	if (version == 0xF) return 2;
	if (version == 0x10) return 3;
	if (version == 0x11) return 4;
	return 0;
}

uint32_t magicNumberGenerator(uint32_t seed, int type)
{
	if (type == 1) {
		return 0x19660D * (0x19660D * (seed ^ 0x5C5C5C5C) + 0x3C6EF35F) + 0x3C6EF35F;
	}
	else {
		return 0x19660D * (seed ^ 0x36363636) + 0x3C6EF35F;
	}
}

#endif