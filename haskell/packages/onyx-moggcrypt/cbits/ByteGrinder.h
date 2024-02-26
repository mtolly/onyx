#pragma once

#ifndef RB1
#include "inttypes.h"
#include "ops.h"

/*
This seems like a good place to put this:


DataFunc <-> C func

A,B,C,D,E,F,G,H,I,J,K,L,M      returnMasterKey
N(a)   getRandomLong
h(a)   magicNumberGenerator
m(a)   hashTo5Bits
z(a)   hashTo6Bits
x(a)   getRandomSequence32A
y(a)   getRandomSequence32B

{O64 x y}    while(x) { y }
{O65 x}      x.length
{O66 x y...} switch(x){ y... }
{O67 x y}    x[y]
{O68 (x)}    ??? using ??? declare a variable?
{O69 x y}    x := y
{O70 x}      x++

*/

class ByteGrinder
{
public:
	ByteGrinder();
	~ByteGrinder();
	void GrindArray(uint32_t seed1, uint32_t seed2, uint8_t* key, int keylength, int version);
	void HvDecrypt(uint8_t* dest, const uint8_t* src, int version);
	int PickOneOf32A(bool init, int seed);
	int PickOneOf32B(bool init, int seed);
	void fillHashMap5Bit(uint32_t seed);
	void fillHashMap6Bit(uint32_t seed);
private:
	op_func O[64];
	uint8_t usedUpA[32];
	int seedA;
	uint8_t usedUpB[32];
	int seedB;
	uint8_t hashMap[256];
};

int GetEncMethod(int version);
uint32_t magicNumberGenerator(uint32_t seed, int type);

#endif