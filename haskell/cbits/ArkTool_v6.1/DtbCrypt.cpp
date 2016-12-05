// 
// Crypt algorithm for *.dtb and main.hdr files from ark/hdr files.
// These ark/hdr files are found in Guitar Hero 1, Guitar Hero 2,
// Guitar Hero 80s and Rockband (Possibly other games too).
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#include "DtbCrypt.h"


// a default key to use for either crypt algorithm
const unsigned int G_CRYPT_KEY = 0x52534F4C;

unsigned int dtb_xor_x360(unsigned int data)
{
	int val1 = (data / 0x1F31D) * 0xB14;
	int val2 = (data - ((data / 0x1F31D) * 0x1F31D)) * 0x41A7;
	val2 = val2 - val1;
	if(val2 <= 0)
		val2 += 0x7FFFFFFF;
	return val2;
}

// perform crypt over "size" bytes in "buff"
// this is the newer crypt algorithm as used in xbox360 games
// and new ps2 games (ie rockband on the ps2)
void dtb_crypt_new(void* buff, int size)
{
	unsigned int key = *(unsigned int*)buff;
	unsigned char* ptr = (unsigned char*)buff;
	
	for(int i=4; i<size; i++)
	{
		key = dtb_xor_x360(key);
		ptr[i] ^= key;
	}
}




typedef struct {
	int idx1;
	int idx2;
	unsigned int table[0x100];
} CryptTable;

void init_ps2_crypt(CryptTable* table, unsigned int key)
{
	unsigned int val1 = key;
	
	for(int i=0; i<0x100; i++)
	{
		unsigned int val2 = (val1 * 0x41C64E6D) + 0x3039;
		val1 = (val2 * 0x41C64E6D) + 0x3039;
		table->table[i] = (val1 & 0x7FFF0000) | (val2 >> 16);
	}
	
	table->idx1 = 0x00;
	table->idx2 = 0x67;
}

// perform crypt over "size" bytes in "buff"
// this is the older crypt algorithm as used in early ps2 games
void dtb_crypt_old(void* buff, int size)
{
	unsigned int key	= *(unsigned int*)buff;
	unsigned char* ptr	= (unsigned char*)buff;
	
	CryptTable table[0x100];
	init_ps2_crypt(table, key);
	
	for(int i=4; i<size; i++)
	{
		table->table[table->idx1] ^= table->table[table->idx2];
		ptr[i] ^= table->table[table->idx1];
		
		table->idx1 = ((table->idx1 + 1) >= 0xF9) ? 0x00 : (table->idx1 + 1);
		table->idx2 = ((table->idx2 + 1) >= 0xF9) ? 0x00 : (table->idx2 + 1);
	}
}


