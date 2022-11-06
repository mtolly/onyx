// moggcrypt-cpp.cpp : Defines the entry point for the console application.
//
#define DEBUGLOG(s) std::cout << s << std::endl;
#include <iostream>
#include <cstring>
#include <fstream>
#include "VorbisEncrypter.h"
#include "CCallbacks.h"

int fail(const char* msg) {
	printf("Failed!\n\nMessage: %s\n", msg);
	return -1;
}

extern "C" {

int encryptOgg(const char* in, const char* out) {
	std::ofstream outfile;

	FILE* infile = fopen(in, "rb");
	if (infile == 0) {
		return fail("Could not open input file");
	}

	outfile.open(out, std::ios::out | std::ios::binary);
	if (!outfile.is_open()) {
		return fail("Could not open output file");
	}

	VorbisEncrypter ve(infile, cCallbacks);
	uint8_t buf[8192];
	size_t read = 0;
	do {
		read = ve.ReadRaw(buf, 1, 8192);
		outfile.write((char*)buf, read);
	} while (read != 0);

	return 0;
}

}
