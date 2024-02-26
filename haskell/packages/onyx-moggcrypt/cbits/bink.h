#pragma once
#include <inttypes.h>

typedef struct {
  char MAGIC[3];
  char rev;
  uint32_t size;
  uint32_t nFrames;
  uint32_t largestFrame;
  uint32_t nFrames2;
  uint32_t vw;
  uint32_t vh;
  uint32_t fpsNum;
  uint32_t fpsDenom;
  uint32_t vFlags;
  uint32_t nAudioTracks;
  /*struct {
    uint16_t unk;
    uint16_t numChannels;
  } audioChannelInfo[nAudioTracks]; // 
  struct {
    uint16_t sampleRate;
    char flagsLower;
    char flagsUpper;
  } audioInfo[nAudioTracks];
  uint32_t trackIds[nAudioTracks]; */
} BIKHDR;