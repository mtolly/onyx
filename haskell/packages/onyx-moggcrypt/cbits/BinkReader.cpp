#ifndef RB1
#include "BinkReader.h"

#include "keys.h"
#include "keygen.h"
#include "bink.h"
#include "ByteGrinder.h"
#include <cstdlib>

BinkReader::BinkReader(void* datasource, ov_callbacks cbStruct) 
  : fileRef(datasource), cbStruct(cbStruct)
{}

BinkReader::~BinkReader()
{
}

int BinkReader::ReadToArray(uint8_t** buffer, size_t* size)
{
  if (CheckKIBEHeader()) return -1;

  cbStruct.seek_func(fileRef, 0x38, SEEK_SET);
  BIKHDR hdr;
  if (cbStruct.read_func(&hdr, sizeof(hdr), 1, fileRef) != 1)
    return -1;

  size_t totalSize = hdr.size + 8;
  uint8_t *outbuf = (uint8_t*)malloc(totalSize + 8);
  if (!outbuf) return -1;

  cbStruct.seek_func(fileRef, 0x38, SEEK_SET);
  if (cbStruct.read_func(outbuf, 1, totalSize, fileRef) != totalSize) {
    free(outbuf);
    return -1;
  }

  size_t start = 44 + 12 * hdr.nAudioTracks + 4 * hdr.nFrames + 4;
  for (size_t pos = start; pos < totalSize; pos += 16) {
    be.Encrypt((XTEABlock*)(outbuf + pos), (XTEABlock*)(outbuf + pos));
  }

  *buffer = outbuf;
  *size = totalSize;
  return 0;
}

int BinkReader::CheckKIBEHeader()
{
  cbStruct.seek_func(fileRef, 0, SEEK_SET);
  if (cbStruct.read_func((void*)&header, 0x38, 1, fileRef) != 1)
    return -1;

  if (header.version > 2) {
    // MT: added so that Beatles files just fail instead of crashing.
    // they have version 3 while normal dlc appears to have version 2
    return -1;
  }

  ByteGrinder byteGrinder;
  uint8_t key_tmp[16], master_key[32];

  returnMasterKey(master_key);
  KeyChain::getKey(header.cryptConst0, key_tmp, master_key);
  byteGrinder.GrindArray(header.cryptConst1, header.cryptConst2, key_tmp, 16,
                         0xD);

  for (int i = 0; i < 16; i++) {
    header.fileKey[i] ^= key_tmp[i];
  }
  be.SetKey(header.fileKey);
  be.SetNonce(header.XTEA_nonce, 0);
  return 0;
}
#endif
