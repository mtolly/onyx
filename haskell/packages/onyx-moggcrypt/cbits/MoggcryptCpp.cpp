// moggcrypt-cpp.cpp : Defines the entry point for the console application.
//
#include <iostream>
#include <cstring>
#include <fstream>
#include "VorbisEncrypter.h"
#include "VorbisReader.h"
#ifndef RB1
#include "BinkReader.h"
#endif
#include "CCallbacks.h"

int fail(const char* msg) {
	printf("Failed!\n\nMessage: %s\n", msg);
	return -1;
}

extern "C" {

int saveOgg(const char* in, const char* out) {
	VorbisReader vr;
	std::ofstream outfile;

	FILE* infile = fopen(in, "rb");
	if (infile == 0) {
		return fail("Could not open input file");
	}

	outfile.open(out, std::ios::out | std::ios::binary);
	if (!outfile.is_open()) {
		return fail("Could not open output file");
	}

	int result;
	if (result = vr.Open((void*)infile, cCallbacks)) {
		return fail("Could not decrypt mogg");
	}

	vr.SeekRaw(0, SEEK_SET);
	uint8_t buf[8192];
	size_t read = 0;
	do {
		read = vr.ReadRaw(buf, 1, 8192);
		outfile.write((char*)buf, read);
	} while (read != 0);

	return 0;
}

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

int saveBik(const char* in, const char* out) {
  std::ofstream outfile;

  FILE* infile = fopen(in, "rb");
  if (infile == 0) {
    return fail("Could not open input file");
  }

  outfile.open(out, std::ios::out | std::ios::binary);
  if (!outfile.is_open()) {
    return fail("Could not open output file");
  }

  int result;
  size_t totalSize;
  uint8_t *data;
  BinkReader br((void*)infile, cCallbacks);
  if (result = br.ReadToArray(&data, &totalSize)) {
    return fail("Could not decrypt bik");
  }

  outfile.write((char*)data, totalSize);

  return 0;
}

}
