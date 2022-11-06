// 
// decrypts song files found inside rockband ark
// 
// xorloser april 2008
// (feel free to use this code but please credit me)
// 

#ifndef _SONG_CRYPT_H_
#define _SONG_CRYPT_H_

#include "types.h"


// decrypt mogg file (from xbox360 rockband)
// since mogg files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor/filename
//			input file descriptor/filename
//			size of file data to decrypt (<0 means decrypt all)
// returns:	true if decrypted successfully
bool DecryptMogg(FILE* ofd, FILE* ifd, int decryptSize=-1);
bool DecryptMogg(const char* ofilename, const char* ifilename);
// decrypt file in place
bool DecryptMogg(FILE* fd, int decryptSize=-1);
bool DecryptMogg(const char* filename);
// check if file is encrypted
bool IsMoggEncrypted(FILE* fd);
bool IsMoggEncrypted(const char* filename);


// decrypt vgs file (from ps2 rockband)
// since vgs files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor/filename
//			input file descriptor/filename
//			size of file data to decrypt (<0 means decrypt all)
// returns:	true if decrypted successfully
bool DecryptVgs(FILE* ofd, FILE* ifd, int decryptSize=-1);
bool DecryptVgs(const char* ofilename, const char* ifilename);
// decrypt file in place
bool DecryptVgs(FILE* fd, int decryptSize=-1);
bool DecryptVgs(const char* filename);
// check if file is encrypted
bool IsVgsEncrypted(FILE* fd);
bool IsVgsEncrypted(const char* filename);


// decrypt pss file (from ps2 rockband)
// since pss files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor/filename
//			input file descriptor/filename
//			size of file data to decrypt (<0 means decrypt all)
// returns:	true if decrypted successfully
bool DecryptPss(FILE* ofd, FILE* ifd, int decryptSize=-1);
bool DecryptPss(const char* ofilename, const char* ifilename);
// decrypt file in place
bool DecryptPss(FILE* fd, int decryptSize=-1);
bool DecryptPss(const char* filename);
// check if file is encrypted
bool IsPssEncrypted(FILE* fd);
bool IsPssEncrypted(const char* filename);


#endif // _SONG_CRYPT_H_

