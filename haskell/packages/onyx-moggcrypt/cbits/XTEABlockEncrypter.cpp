#include "XTEABlockEncrypter.h"
#include <cstring>

XTEABlockEncrypter::XTEABlockEncrypter() : nonce()
{
}


XTEABlockEncrypter::~XTEABlockEncrypter()
{
}

void XTEABlockEncrypter::SetKey(const uint8_t *key)
{
  memcpy(this->key, key, 16);
}

void XTEABlockEncrypter::SetNonce(const uint64_t* nonce, uint32_t offset)
{
  this->nonce[0] = nonce[0] + offset;
  this->nonce[1] = nonce[1] + offset;
}

void XTEABlockEncrypter::Encrypt(const XTEABlock *source, XTEABlock *dest)
{
  for (int i = 0; i < 2; i++) {
    uint64_t result = Encipher(this->nonce[i], this->key);
    dest->blocks[i] = source->blocks[i] ^ result;
    this->nonce[i]++;
  }

}


uint64_t XTEABlockEncrypter::Encipher(const uint64_t block, const uint32_t* key) {
  uint64_t blockCopy = block;
  uint32_t* ptr = (uint32_t*)(&blockCopy);
  uint32_t v0 = ptr[0], 
           v1 = ptr[1], 
           sum = 0, delta = 0x9E3779B9;
  for (int i = 0; i < 4; i++) {
    v0 += (((v1 << 4) ^ (v1 >> 5)) + v1) ^ (sum + key[sum & 3]);
    sum += delta;
    v1 += (((v0 << 4) ^ (v0 >> 5)) + v0) ^ (sum + key[(sum >> 11) & 3]);
  }
  ptr[0] = v0;
  ptr[1] = v1;
  return blockCopy;
}

