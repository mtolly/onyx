#pragma once
#include <cinttypes>

typedef union {
  uint64_t blocks[2];
  uint8_t data[16];
} XTEABlock;

class XTEABlockEncrypter
{
public:
  XTEABlockEncrypter();
  ~XTEABlockEncrypter();
  void SetKey(const uint8_t *key);
  void SetNonce(const uint64_t* nonce, uint32_t offset);
  void Encrypt(XTEABlock const *source, XTEABlock *dest);
  uint64_t Encipher(const uint64_t block, const uint32_t *ptr);
private: 
  uint32_t key[4];
  uint64_t nonce[2];
};

