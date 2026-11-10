/*
Replacement for _hs_text_measure_off from the text package (cbits/measure_off.c,
text-2.0.2, Copyright (c) 2021 Andrew Lelechenko, BSD-2-Clause), linked into
the Windows executables in place of text's own copy.

text's version uses AVX-512 whenever the CPU reports it, without checking that
the OS has enabled it. Windows 7 never enables AVX-512, so on AVX-512 CPUs
(Zen 4, Ice Lake, ...) Onyx crashed there with an illegal instruction
(0xc000001d). See haskell/text#566. This copy only uses the SSE2 and plain C
paths, which work everywhere.

Because this object is linked directly into the exe, the linker never pulls
text's measure_off.o out of libHStext.a. If text's version of the function
changes signature in a later text release, update this file to match.
*/

#include <string.h>
#include <stdint.h>
#include <sys/types.h>
#ifdef __x86_64__
#include <emmintrin.h>
#endif

static inline ssize_t measure_off_naive(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
  // Count leading bytes in 8 byte sequence
  while (src < srcend - 7) {
    uint64_t w64;
    memcpy(&w64, src, sizeof(uint64_t));
    w64 = ((w64 << 1) | ~w64) & 0x8080808080808080ULL;
    // popcount of w64 via a multiplication (see text's measure_off.c)
    size_t leads = ((w64 >> 7) * 0x0101010101010101ULL) >> 56;
    if (cnt < leads) break;
    cnt -= leads;
    src += 8;
  }

  // Skip until next leading byte
  while (src < srcend) {
    uint8_t w8 = *src;
    if ((int8_t)w8 >= -0x40) break;
    src++;
  }

  // Finish up with tail
  while (src < srcend && cnt > 0) {
    uint8_t leadByte = *src++;
    cnt--;
    src += (leadByte >= 0xc0) + (leadByte >= 0xe0) + (leadByte >= 0xf0);
  }

  return cnt == 0 ? (ssize_t)(srcend - src) : (ssize_t)(- cnt);
}

// not __builtin_popcount, which GHC's RTS linker doesn't know (GHC #19900)
static inline size_t popcount16(uint16_t x)
{
  x -= (x >> 1) & 0x5555;
  x = (x & 0x3333) + ((x >> 2) & 0x3333);
  x = (x + (x >> 4)) & 0x0f0f;
  return (x >> 8) + (x & 0x00FF);
}

static ssize_t measure_off_sse(const uint8_t *src, const uint8_t *srcend, size_t cnt)
{
#ifdef __x86_64__
  while (src < srcend - 15) {
    __m128i w128 = _mm_loadu_si128((__m128i *)src);
    // Which bytes are either < 128 or >= 192?
    uint16_t mask = _mm_movemask_epi8(_mm_cmpgt_epi8(w128, _mm_set1_epi8(0xBF)));
    size_t leads = popcount16(mask);
    if (cnt < leads) break;
    cnt -= leads;
    src += 16;
  }
#endif

  return measure_off_naive(src, srcend, cnt);
}

ssize_t _hs_text_measure_off(const uint8_t *src, size_t off, size_t len, size_t cnt)
{
  ssize_t ret = measure_off_sse(src + off, src + off + len, cnt);
  return ret >= 0 ? ((ssize_t)len - ret) : (- (cnt + ret));
}
