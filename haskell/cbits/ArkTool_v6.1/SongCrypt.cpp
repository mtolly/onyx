// 
// decrypts song files found inside rockband ark
// 
// xorloser april 2008
// (feel free to use this code but please credit me)
// 

#include "types.h"
#include "SongCrypt.h"
#include <string.h>
#include "xyssl/aes.h"
#include "PssVgsAccess.h"

#define min(a, b) (((a) < (b))?(a):(b))

const int TEMP_BUFF_SIZE = 0x1000;

// key for all xbox360 songs
unsigned char G_X360_KEY[] = {
	0x37, 0xB2, 0xE2, 0xB9, 0x1C, 0x74, 0xFA, 0x9E,
	0x38, 0x81, 0x08, 0xEA, 0x36, 0x23, 0xDB, 0xE4
};

// key for all ps2 songs
unsigned char G_PS2_KEY[] = {
	0x20, 0x8B, 0x5A, 0x54, 0xF3, 0x67, 0xB3, 0x7F,
	0xBC, 0xF7, 0xAF, 0xF0, 0xDD, 0x3B, 0xA5, 0xE0
};


typedef struct {
	int version;		// 0xA = unencrypted, 0xB = encrypted
	int headerSize;		// this is the offset of the start of the ogg file
	int numChannels;	// number of channels in the mogg? not 100% sure on this..
	int unknown;		// 0x4E20	20000 (some kind of max packet/chunk size?)
	int numEntries;		// number of MoggEntrys following this
} MoggHeader;

typedef struct {
	unsigned int fileOffset;	// these are multiples of 0x8000
	unsigned int unknown;		// time offset into ogg stream?
} MoggEntry;

typedef struct {
	unsigned char key[16];
} MoggKey;


typedef struct {
	unsigned int data1;
	unsigned int data2;
} VgsEntry;

typedef struct {
	char magic[4];	// "VgS!"
	int  version;
	VgsEntry entry[15];
} VgsHeader;


typedef struct {
/* 00*/int	cipherNum;					// cipher number (should be 0)
/* 04*/int	blockSize;					// block size
/* 08*/int	blockOffset;				// offset
/* 0C*/unsigned char	cipherIn[16];	// cipher in data
/* 8C*/unsigned char	cipherOut[16];	// cipher out data
/*10C*/aes_context	aesCtx;
/*???*/u64	v4Data[2];					// v4 cipher data
/*???*/u32  v4Count;					// v4 count
} CryptInfo;


void CipherInitV3(CryptInfo* cryptInfo, u8 fileKey[16], u8 constantKey[16])
{
	cryptInfo->blockOffset = 0;
	cryptInfo->blockSize = 16;
	memcpy(cryptInfo->cipherIn, fileKey, 16);
	
	aes_setkey_enc(&cryptInfo->aesCtx, constantKey, 128);
	aes_crypt_ecb(&cryptInfo->aesCtx, AES_ENCRYPT, cryptInfo->cipherIn, cryptInfo->cipherOut);
}

void CipherDecryptV3(const unsigned char* dataIn, unsigned char* dataOut,
					int dataSize, CryptInfo* cryptInfo)
{
	for(int i=0; i<dataSize; i++)
	{
		if(cryptInfo->blockOffset == cryptInfo->blockSize)
		{
			unsigned char* ptr = cryptInfo->cipherIn;
			for(int j=0; j<cryptInfo->blockSize; j++)
			{
				(*ptr)++;
				if(*ptr != 0)
					break;
				ptr++;
			}
			
			aes_crypt_ecb(&cryptInfo->aesCtx, AES_ENCRYPT, cryptInfo->cipherIn, cryptInfo->cipherOut);
			cryptInfo->blockOffset = 0;
		}
		
		// this bit only decrypts 1 byte
		dataOut[i] = cryptInfo->cipherOut[cryptInfo->blockOffset] ^ dataIn[i];
		cryptInfo->blockOffset++;
	}
}



// does some crypt over 64bit input and returns a 64bit value
u64 transform_64bits(const void* notUsed, u64 input, const u32 table[4])
{
	u32 hi = (u32)(input >> 32);
	u32 lo = (u32)input;
	u32 tmp;
	u32 num = 0;
	for(int i=0; i<4; i++)
	{
		tmp = (hi << 4) ^ (hi >> 5);
		lo += (tmp + hi) ^ (num + table[num&3]);
		
		num += 0x9E3779B9;
		
		tmp = (lo << 4) ^ (lo >> 5);
		hi += (tmp + lo) ^ (num + table[(num>>11) & 3]);
	}
	return (u64)(((u64)hi << 32) | (lo & 0xFFFFFFFF));
}

void setup_transform_table(u64* output, const u64* input, u32 data)
{
	output[0] = input[0] + data;
	output[1] = input[1] + data;
}

// transforms 128 bits of data
// (this seems to be done over data from a vgs file)
void transform_128bits(u64* data, const u64* input, u64* output)
{
	const u32 crypt_table[4] = {
		0x208B5A54, 0xF367B37F, 0xBCF7AFF0, 0xDD3BA5E0
	};
	
	for(int i=0; i<2; i++)
	{
		u64 result = transform_64bits(data, data[i], crypt_table);
		output[i] = input[i] ^ result;
		data[i]++;
	}
}

void CipherInitV4(CryptInfo* cryptInfo, u8 fileKey[16])
{
	cryptInfo->v4Count = 0;
	memset(cryptInfo->v4Data, 0, 16);
	memcpy(cryptInfo->cipherIn, fileKey, 16);
	
	// sets from crypt_info.v4Data from crypt_info.cipherIn and crypt_info.v4Count
	setup_transform_table(cryptInfo->v4Data, (u64*)cryptInfo->cipherIn, cryptInfo->v4Count);
}

void CipherDecryptV4(const unsigned char* dataIn, unsigned char* dataOut,
					int dataSize, CryptInfo* cryptInfo)
{
	u64* in_ptr  = (u64*)dataIn;
	u64* out_ptr = (u64*)dataOut;
	for(int i=0; i<dataSize/8; i+=2)
	{
		transform_128bits(cryptInfo->v4Data, in_ptr+i, out_ptr+i);
	}
}



void CipherInit(int version, CryptInfo* cryptInfo, u8 fileKey[16], u8 constantKey[16])
{
	if(version == 4)		CipherInitV4(cryptInfo, fileKey);
	else if(version == 3)	CipherInitV3(cryptInfo, fileKey, constantKey);
}

void CipherDecrypt(int version, const unsigned char* dataIn, unsigned char* dataOut,
					int dataSize, CryptInfo* cryptInfo)
{
	if(version == 4)		CipherDecryptV4(dataIn, dataOut, dataSize, cryptInfo);
	else if(version == 3)	CipherDecryptV3(dataIn, dataOut, dataSize, cryptInfo);
}


// decrypt mogg file (from xbox360 rockband)
// since mogg files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
//			flag for whether the input and output files are the same file
// returns:	true if decrypted successfully
bool DecryptMogg(FILE* ofd, FILE* ifd, int decryptSize, bool sameFile)
{
	bool decrypt_all = decryptSize < 0;
	
	// read in header
	MoggHeader mogg_hdr;
	if(sameFile) fflush(ofd);
	if( fread(&mogg_hdr, 1, sizeof(mogg_hdr), ifd) != sizeof(mogg_hdr) ) return false;
	decryptSize -= sizeof(mogg_hdr);
	if(!decrypt_all && decryptSize < 0)
		return false;
	
	// check header version, then write out header
	// (only version 0xB is encrypted)
	if(mogg_hdr.version != 0xB)
		return false;
	// set version as "unencrypted" mogg type
	mogg_hdr.version = 0xA;
	// if key was being removed from the file, the header would be 0x10 bytes smaller
	// mogg_hdr.headerSize -= 0x10;
	if(sameFile) FSEEK64(ofd, -(int)sizeof(mogg_hdr), SEEK_CUR);
	if( fwrite(&mogg_hdr, 1, sizeof(mogg_hdr), ofd) != sizeof(mogg_hdr) ) return false;
	if(sameFile) fflush(ofd);
	
	// write out all entries
	for(int i=0; i<mogg_hdr.numEntries; i++)
	{
		MoggEntry entry;
		if( fread(&entry, 1, sizeof(entry), ifd) != sizeof(entry) ) return false;
		if(sameFile) FSEEK64(ofd, -(int)sizeof(entry), SEEK_CUR);
		if( fwrite(&entry, 1, sizeof(entry), ofd) != sizeof(entry) ) return false;
		if(sameFile) fflush(ofd);
		
		decryptSize -= sizeof(entry);
		if(!decrypt_all && decryptSize < 0)
			return false;
	}
	
	// get key from file and init crypto
	CryptInfo crypt_info;
	u8 file_key[16];
	// if key was being removed from file, we wouldn't write it out here
	if( fread(file_key, 1, 16, ifd) != 16 ) return false;
	if(sameFile) FSEEK64(ofd, -16, SEEK_CUR);
	if( fwrite(file_key, 1, 16, ofd) != 16 ) return false;
	if(sameFile) fflush(ofd);
	CipherInit(3, &crypt_info, file_key, G_X360_KEY);
	decryptSize -= 16;
	if(!decrypt_all && decryptSize < 0)
		return false;
	
	// do decrypt
	unsigned char crypt_buff[TEMP_BUFF_SIZE];
	int crypt_size = TEMP_BUFF_SIZE;
	while(crypt_size == TEMP_BUFF_SIZE)
	{
		int copy_size = decrypt_all ? TEMP_BUFF_SIZE : min(TEMP_BUFF_SIZE, decryptSize);
		crypt_size = (int)fread(crypt_buff, 1, copy_size, ifd);
		CipherDecrypt(3, crypt_buff, crypt_buff, crypt_size, &crypt_info);
		if(sameFile) FSEEK64(ofd, -crypt_size, SEEK_CUR);
		fwrite(crypt_buff, 1, crypt_size, ofd);
		if(sameFile) fflush(ofd);
		
		decryptSize -= TEMP_BUFF_SIZE;
	}
	
	// done
	return true;
}

// decrypt mogg file (from xbox360 rockband)
// since mogg files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
// returns:	true if decrypted successfully
bool DecryptMogg(FILE* ofd, FILE* ifd, int decryptSize)
{
	return DecryptMogg(ofd, ifd, decryptSize, false);
}
bool DecryptMogg(const char* ofilename, const char* ifilename)
{
	FILE* ifd = fopen(ifilename, "rb");
	if(ifd == NULL)
		return false;
	FILE* ofd = fopen(ofilename, "w+b");
	if(ofd == NULL)
	{
		fclose(ifd);
		return false;
	}
	
	bool result = DecryptMogg(ofd, ifd);
	
	fclose(ifd);
	fclose(ofd);
	return result;
}

// decrypt file in place
bool DecryptMogg(FILE* fd, int decryptSize)
{
	return DecryptMogg(fd, fd, decryptSize, true);
}
bool DecryptMogg(const char* filename)
{
	FILE* fd = fopen(filename, "r+b");
	if(fd == NULL)
		return false;
	
	bool result = DecryptMogg(fd);
	
	fclose(fd);
	return result;
}


bool IsMoggEncrypted(FILE* fd)
{
	// read in header
	MoggHeader mogg_hdr;
	if( fread(&mogg_hdr, 1, sizeof(mogg_hdr), fd) != sizeof(mogg_hdr) ) return false;
	FSEEK64(fd, -(int)sizeof(mogg_hdr), SEEK_CUR);
	return mogg_hdr.version == 0x0B;
}

bool IsMoggEncrypted(const char* filename)
{
	FILE* fd = fopen(filename, "rb");
	if(fd == NULL)
		return false;
	
	// test for encrypted
	bool is_encrypted = IsMoggEncrypted(fd);
	
	fclose(fd);
	return is_encrypted;
}




// decrypt vgs file (from ps2 rockband)
// since vgs files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
//			flag for whether the input and output files are the same file
// returns:	true if decrypted successfully
bool DecryptVgs(FILE* ofd, FILE* ifd, int decryptSize, bool sameFile)
{
	if( !IsVgsEncrypted(ifd) )
		return false;
	
	bool decrypt_all = decryptSize < 0;

	// copy out the vgs header
	int vgs_version;
	VgsHeader vgs_header;
	fread(&vgs_header, 1, sizeof(vgs_header), ifd);
	if(sameFile) FSEEK64(ofd, -(int)sizeof(vgs_header), SEEK_CUR);
	vgs_version = vgs_header.version;
	vgs_header.version = 2;
	fwrite(&vgs_header, 1, sizeof(vgs_header), ofd);
	if(sameFile) fflush(ofd);
	decryptSize -= sizeof(vgs_header);
	if(!decrypt_all && decryptSize < 0)
		return false;
	
	// get key from file and init crypto
	CryptInfo crypt_info;
	u8 file_key[16];
	if( fread(file_key, 1, 16, ifd) != 16 ) return false;
	CipherInit(vgs_version, &crypt_info, file_key, G_PS2_KEY);
	decryptSize -= 16;
	if(!decrypt_all && decryptSize < 0)
		return false;
	
	// decrypt
	unsigned char crypt_buff[TEMP_BUFF_SIZE];
	int crypt_size = TEMP_BUFF_SIZE;
	while(crypt_size == TEMP_BUFF_SIZE)
	{
		int copy_size = decrypt_all ? TEMP_BUFF_SIZE : min(TEMP_BUFF_SIZE, decryptSize);
		crypt_size = (int)fread(crypt_buff, 1, copy_size, ifd);
		CipherDecrypt(vgs_version, crypt_buff, crypt_buff, crypt_size, &crypt_info);
		if(sameFile) FSEEK64(ofd, -crypt_size-16, SEEK_CUR);
		fwrite(crypt_buff, 1, crypt_size, ofd);
		if(sameFile) FSEEK64(ofd, 16, SEEK_CUR);
		
		decryptSize -= TEMP_BUFF_SIZE;
	}
	
	// write out key to end of file
	// this isnt needed and may possibly give troubles...
	// but if it does work it makes dealing with file sizes easier :p
	if(sameFile) FSEEK64(ofd, -16, SEEK_CUR);
	fwrite(file_key, 1, 16, ofd);
	return true;
}

// decrypt vgs file (from ps2 rockband)
// since vgs files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
// returns:	true if decrypted successfully
bool DecryptVgs(FILE* ofd, FILE* ifd, int decryptSize)
{
	return DecryptVgs(ofd, ifd, decryptSize, false);
}
bool DecryptVgs(const char* ofilename, const char* ifilename)
{
	FILE* ifd = fopen(ifilename, "rb");
	if(ifd == NULL)
		return false;
	FILE* ofd = fopen(ofilename, "w+b");
	if(ofd == NULL)
	{
		fclose(ifd);
		return false;
	}
	
	bool result = DecryptPss(ofd, ifd);
	
	fclose(ifd);
	fclose(ofd);
	return result;
}

// decrypt file in place
bool DecryptVgs(FILE* fd, int decryptSize)
{
	return DecryptVgs(fd, fd, decryptSize, true);
}
bool DecryptVgs(const char* filename)
{
	FILE* fd = fopen(filename, "r+b");
	if(fd == NULL)
		return false;
	
	bool result = DecryptPss(fd);
	
	fclose(fd);
	return result;
}


bool IsVgsEncrypted(FILE* fd)
{
	VgsHeader vgs_header;
	fread(&vgs_header, 1, sizeof(vgs_header), fd);
	FSEEK64(fd, -(int)sizeof(vgs_header), SEEK_CUR);
	if( memcmp(vgs_header.magic, "VgS!", 4) )
		return false;
	return (vgs_header.version == 3) || (vgs_header.version == 4);
}

bool IsVgsEncrypted(const char* filename)
{
	FILE* fd = fopen(filename, "rb");
	if(fd == NULL)
		return false;
	
	// test for encrypted
	bool is_encrypted = IsVgsEncrypted(fd);
	
	fclose(fd);
	return is_encrypted;
}




// WARNING: THIS DOES NOT CORRECTLY HANDLE the 'decryptSize'.
// if you use this in a manner where you only want to
// decrypt part of a pss file it may overrun the decryptSize limits.
// 
// decrypt pss file (from ps2 rockband)
// since pss files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
//			flag for whether the input and output files are the same file
// returns:	true if decrypted successfully
bool DecryptPss(FILE* ofd, FILE* ifd, int decryptSize, bool sameFile)
{
	// can only decrypt inside a file
	if(sameFile == false)
		return false;
	
	bool decrypt_all = decryptSize < 0;
	
	// open the pss file
	s64 pss_start_offset = FTELL64(ifd);
	PssVgsAccess pss;
	pss.Open(ifd);
	
	// extract the vgs from the pss into a temp file
	FILE* vgsfd = tmpfile();
	if(vgsfd == NULL)
		return false;
	int read_size = TEMP_BUFF_SIZE;
	u8 buffer[TEMP_BUFF_SIZE];
	while(read_size == TEMP_BUFF_SIZE)
	{
		read_size = (int)pss.ReadUpdateOffset(buffer, TEMP_BUFF_SIZE);
		fwrite(buffer, 1, read_size, vgsfd);
	}
	pss.Close();
	fseek(vgsfd, 0, SEEK_SET);
	FSEEK64(ofd, pss_start_offset, SEEK_SET);
	
	// decrypt the vgs
	if( !DecryptVgs(vgsfd) )
	{
		fclose(vgsfd);
		return false;
	}
	fseek(vgsfd, 0, SEEK_SET);
	
	// insert the decrypted vgs back into the pss
	pss.Open(ofd);
	int write_size = TEMP_BUFF_SIZE;
	while(write_size == TEMP_BUFF_SIZE)
	{
		write_size = (int)fread(buffer, 1, TEMP_BUFF_SIZE, vgsfd);
		pss.Write(buffer, write_size);
	}
	pss.Close();
	fclose(vgsfd);
	return true;
}

// decrypt pss file (from ps2 rockband)
// since pss files are so big, this won't decrypt them in memory
// 
// args:	output file descriptor
//			input file descriptor
// returns:	true if decrypted successfully
bool DecryptPss(FILE* ofd, FILE* ifd, int decryptSize)
{
	int saved_decrypt_size = TEMP_BUFF_SIZE;
	bool decrypt_all = decryptSize < 0;

	// copy input file into output file
	s64 output_offset = FTELL64(ofd);
	int read_size = TEMP_BUFF_SIZE;
	u8 buffer[TEMP_BUFF_SIZE];
	while(read_size == TEMP_BUFF_SIZE)
	{
		int copy_size = decrypt_all ? TEMP_BUFF_SIZE : min(TEMP_BUFF_SIZE, decryptSize);
		read_size = (int)fread(buffer, 1, copy_size, ifd);
		fwrite(buffer, 1, read_size, ofd);

		decryptSize -= TEMP_BUFF_SIZE;
	}
	FSEEK64(ofd, output_offset, SEEK_SET);
	
	// now decrypt output file
	return DecryptPss(ofd, ofd, saved_decrypt_size, true);
}
bool DecryptPss(const char* ofilename, const char* ifilename)
{
	FILE* ifd = fopen(ifilename, "rb");
	if(ifd == NULL)
		return false;
	FILE* ofd = fopen(ofilename, "w+b");
	if(ofd == NULL)
	{
		fclose(ifd);
		return false;
	}
	
	bool result = DecryptPss(ofd, ifd);
	
	fclose(ifd);
	fclose(ofd);
	return result;
}

// decrypt file in place
bool DecryptPss(FILE* fd, int decryptSize)
{
	return DecryptPss(fd, fd, decryptSize, true);
}
bool DecryptPss(const char* filename)
{
	FILE* fd = fopen(filename, "r+b");
	if(fd == NULL)
		return false;
	
	bool result = DecryptPss(fd);
	
	fclose(fd);
	return result;
}


bool IsPssEncrypted(FILE* fd)
{
	s64 offset = FTELL64(fd);
	
	// read in vgs header
	bool result = true;
	u8 buffer[8];
	PssVgsAccess pss;
	if( !pss.Open(fd) ||
		pss.ReadUpdateOffset(buffer, 8) != 8 ||
		!pss.Close() )
	{
		// error reading from pss
		result = false;
	}
	else
	{
		result =(memcmp(buffer, "VgS!", 4)==0) &&
				(*(int*)(buffer+4) == 3 || *(int*)(buffer+4) == 4 );
	}
	
	FSEEK64(fd, offset, SEEK_SET);
	return true;
}

bool IsPssEncrypted(const char* filename)
{
	FILE* fd = fopen(filename, "rb");
	if(fd == NULL)
		return false;
	
	// test for encrypted
	bool is_encrypted = IsPssEncrypted(fd);
	
	fclose(fd);
	return is_encrypted;
}


