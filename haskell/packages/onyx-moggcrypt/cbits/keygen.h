#pragma once
#include <inttypes.h>


#ifndef RB1
extern "C" {

char asciiDigitToHex(char);
void parseHex16(const unsigned char *, unsigned char *);
int32_t mcrandom(int32_t);
void mash(unsigned char* x1, const unsigned char* x2);
int32_t roll(int32_t);
void swap(unsigned char &c1, unsigned char &c2);
void shuffle1(unsigned char* ptr);
void shuffle2(unsigned char* ptr);
void shuffle3(unsigned char* ptr);
void shuffle4(unsigned char* ptr);
void shuffle5(unsigned char* ptr);
void shuffle6(unsigned char* ptr);
void supershuffle(unsigned char* buffer);

void revealKey(const unsigned char* masterKey, unsigned char* revealed);

void returnMasterKey(unsigned char* buf32);

} // extern "C"

class KeyChain {
public:
	static void getKey(int, unsigned char*, const unsigned char*, int version = 12); 
	static void getKey17(int offset, unsigned char* key, const unsigned char* masterKey, int type);
	static int getNumKeys() { return 12; }
	static void getMasher(unsigned char*);
};
#endif