#pragma once
#ifndef RB1
#include <inttypes.h>
#include "ByteGrinder.h"
#include "XTEABlockEncrypter.h"

#ifndef VR_NO_DECODE
#include <vorbis/vorbisfile.h>
#include <vorbis/codec.h>
#endif

#ifndef _OV_FILE_H_
#include "XiphTypes.h"
#endif

typedef struct {
  char KIBE[4];
  uint32_t version;
  uint32_t cryptConst0;
  uint32_t cryptConst1;
  uint32_t cryptConst2;
  uint32_t cryptConst3;
  uint64_t XTEA_nonce[2];
  uint8_t fileKey[16];
} KIBEHeader;

class BinkReader
{
public:
  BinkReader(void* datasource, ov_callbacks cbStruct);
  ~BinkReader();
  int ReadToArray(uint8_t** buffer, size_t* size);
private:
  int CheckKIBEHeader();
  ov_callbacks cbStruct;
  void * fileRef;
  KIBEHeader header;
  XTEABlockEncrypter be;
};

#endif