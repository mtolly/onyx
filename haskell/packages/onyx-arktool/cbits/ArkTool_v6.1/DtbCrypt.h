// 
// Crypt algorithm for *.dtb and main.hdr files from ark/hdr files.
// These ark/hdr files are found in Guitar Hero 1, Guitar Hero 2,
// Guitar Hero 80s and Rockband (Possibly other games too).
// 
// xorloser march 2008
// (feel free to use this code but please credit me)
// 

#ifndef _DTB_CRYPT_H_
#define _DTB_CRYPT_H_


// a default key to use for either crypt algorithm
extern const unsigned int G_CRYPT_KEY;

// perform crypt over "size" bytes in "buff"
// this is the newer crypt algorithm as used in xbox360 games
// and new ps2 games (ie rockband on the ps2)
void dtb_crypt_new(void* buff, int size);

// perform crypt over "size" bytes in "buff"
// this is the older crypt algorithm as used in early ps2 games
void dtb_crypt_old(void* buff, int size);


#endif // _DTB_CRYPT_H_

