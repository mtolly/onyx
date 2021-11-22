// Copyright (C) 2014       Hykem <hykem@hotmail.com>
// Licensed under the terms of the GNU GPL, version 3
// http://www.gnu.org/licenses/gpl-3.0.txt

#include "make_npdata.h"

// Main crypto functions.
void get_rif_key(unsigned char* rap, unsigned char* rif)
{
	int i;
	int round;

	unsigned char key[0x10];
	unsigned char iv[0x10];
	memset(key, 0, 0x10);
	memset(iv, 0, 0x10);

	// Initial decrypt.
	aescbc128_decrypt(RAP_KEY, iv, rap, key, 0x10);

	// rap2rifkey round.
	for (round = 0; round < 5; ++round)
	{
		for (i = 0; i < 16; ++i)
		{
			int p = RAP_PBOX[i];
			key[p] ^= RAP_E1[p];
		}
		for (i = 15; i >= 1; --i)
		{
			int p = RAP_PBOX[i];
			int pp = RAP_PBOX[i - 1];
			key[p] ^= key[pp];
		}
		int o = 0;
		for (i = 0; i < 16; ++i)
		{
			int p = RAP_PBOX[i];
			unsigned char kc = key[p] - o;
			unsigned char ec2 = RAP_E2[p];
			if (o != 1 || kc != 0xFF)
			{
				o = kc < ec2 ? 1 : 0;
				key[p] = kc - ec2;
			}
			else if (kc == 0xFF)
			{
				key[p] = kc - ec2;
			}
			else
			{
				key[p] = kc;
			}
		}
	}

	memcpy(rif, key, 0x10);
}

void generate_key(int crypto_mode, int version, unsigned char *key_final, unsigned char *iv_final, unsigned char *key, unsigned char *iv)
{
	int mode = (int) (crypto_mode & 0xF0000000);
	switch (mode) {
	case 0x10000000:
		// Encrypted ERK.
		// Decrypt the key with EDAT_KEY + EDAT_IV and copy the original IV.
		aescbc128_decrypt(version ? EDAT_KEY_1 : EDAT_KEY_0, EDAT_IV, key, key_final, 0x10);
		memcpy(iv_final, iv, 0x10);
		break;
	case 0x20000000:
		// Default ERK.
		// Use EDAT_KEY and EDAT_IV.
		memcpy(key_final, version ? EDAT_KEY_1 : EDAT_KEY_0, 0x10);
		memcpy(iv_final, EDAT_IV, 0x10);
		break;
	case 0x00000000:
		// Unencrypted ERK.
		// Use the original key and iv.
		memcpy(key_final, key, 0x10);
		memcpy(iv_final, iv, 0x10);
		break;
	};
}

void generate_hash(int hash_mode, int version, unsigned char *hash_final, unsigned char *hash)
{
	int mode = (int) (hash_mode & 0xF0000000);
	switch (mode) {
	case 0x10000000:
		// Encrypted HASH.
		// Decrypt the hash with EDAT_KEY + EDAT_IV.
		aescbc128_decrypt(version ? EDAT_KEY_1 : EDAT_KEY_0, EDAT_IV, hash, hash_final, 0x10);
		break;
	case 0x20000000:
		// Default HASH.
		// Use EDAT_HASH.
		memcpy(hash_final, version ? EDAT_HASH_1 : EDAT_HASH_0, 0x10);
		break;
	case 0x00000000:
		// Unencrypted ERK.
		// Use the original hash.
		memcpy(hash_final, hash, 0x10);
		break;
	};
}

bool decrypt(int hash_mode, int crypto_mode, int version, unsigned char *in, unsigned char *out, int length, unsigned char *key, unsigned char *iv, unsigned char *hash, unsigned char *test_hash) 
{
	// Setup buffers for key, iv and hash.
	unsigned char key_final[0x10] = {};
	unsigned char iv_final[0x10] = {};
	unsigned char hash_final_10[0x10] = {};
	unsigned char hash_final_14[0x14] = {};

	// Generate crypto key and hash.
	generate_key(crypto_mode, version, key_final, iv_final, key, iv);
	if ((hash_mode & 0xFF) == 0x01)
		generate_hash(hash_mode, version, hash_final_14, hash);
	else
		generate_hash(hash_mode, version, hash_final_10, hash);

	if ((crypto_mode & 0xFF) == 0x01)  // No algorithm.
	{
		memcpy(out, in, length);
	}
	else if ((crypto_mode & 0xFF) == 0x02)  // AES128-CBC
	{
		aescbc128_decrypt(key_final, iv_final, in, out, length);
	}
	else
	{
		printf("ERROR: Unknown crypto algorithm!\n");
		return false;
	}

	if ((hash_mode & 0xFF) == 0x01) // 0x14 SHA1-HMAC
	{
		return hmac_hash_compare(hash_final_14, 0x14, in, length, test_hash);
	}
	else if ((hash_mode & 0xFF) == 0x02)  // 0x10 AES-CMAC
	{
		return cmac_hash_compare(hash_final_10, 0x10, in, length, test_hash);
	}
	else if ((hash_mode & 0xFF) == 0x04) //0x10 SHA1-HMAC
	{
		return hmac_hash_compare(hash_final_10, 0x10, in, length, test_hash);
	}
	else
	{
		printf("ERROR: Unknown hashing algorithm!\n");
		return false;
	}
}

bool encrypt(int hash_mode, int crypto_mode, int version, unsigned char *in, unsigned char *out, int length, unsigned char *key, unsigned char *iv, unsigned char *hash, unsigned char *test_hash) 
{
	// Setup buffers for key, iv and hash.
	unsigned char key_final[0x10] = {};
	unsigned char iv_final[0x10] = {};
	unsigned char hash_final_10[0x10] = {};
	unsigned char hash_final_14[0x14] = {};

	// Generate crypto key and hash.
	generate_key(crypto_mode, version, key_final, iv_final, key, iv);
	if ((hash_mode & 0xFF) == 0x01)
		generate_hash(hash_mode, version, hash_final_14, hash);
	else
		generate_hash(hash_mode, version, hash_final_10, hash);

	if ((crypto_mode & 0xFF) == 0x01)  // No algorithm.
	{
		memcpy(out, in, length);
	}
	else if ((crypto_mode & 0xFF) == 0x02)  // AES128-CBC
	{
		aescbc128_encrypt(key_final, iv_final, in, out, length);
	}
	else
	{
		printf("ERROR: Unknown crypto algorithm!\n");
		return false;
	}

	if ((hash_mode & 0xFF) == 0x01) // 0x14 SHA1-HMAC
	{
		hmac_hash_forge(hash_final_14, 0x14, out, length, test_hash);
		return true;
	}
	else if ((hash_mode & 0xFF) == 0x02)  // 0x10 AES-CMAC
	{
		cmac_hash_forge(hash_final_10, 0x10, out, length, test_hash);
		return true;
	}
	else if ((hash_mode & 0xFF) == 0x04) //0x10 SHA1-HMAC
	{
		hmac_hash_forge(hash_final_10, 0x10, out, length, test_hash);
		return true;
	}
	else
	{
		printf("ERROR: Unknown hashing algorithm!\n");
		return false;
	}
}

// EDAT/SDAT functions.
unsigned char* dec_section(unsigned char* metadata)
{
	unsigned char *dec = (unsigned char *) malloc (0x10);
	dec[0x00] = (metadata[0xC] ^ metadata[0x8] ^ metadata[0x10]);
	dec[0x01] = (metadata[0xD] ^ metadata[0x9] ^ metadata[0x11]);
	dec[0x02] = (metadata[0xE] ^ metadata[0xA] ^ metadata[0x12]);
	dec[0x03] = (metadata[0xF] ^ metadata[0xB] ^ metadata[0x13]);
	dec[0x04] = (metadata[0x4] ^ metadata[0x8] ^ metadata[0x14]);
	dec[0x05] = (metadata[0x5] ^ metadata[0x9] ^ metadata[0x15]);
	dec[0x06] = (metadata[0x6] ^ metadata[0xA] ^ metadata[0x16]);
	dec[0x07] = (metadata[0x7] ^ metadata[0xB] ^ metadata[0x17]);
	dec[0x08] = (metadata[0xC] ^ metadata[0x0] ^ metadata[0x18]);
	dec[0x09] = (metadata[0xD] ^ metadata[0x1] ^ metadata[0x19]);
	dec[0x0A] = (metadata[0xE] ^ metadata[0x2] ^ metadata[0x1A]);
	dec[0x0B] = (metadata[0xF] ^ metadata[0x3] ^ metadata[0x1B]);
	dec[0x0C] = (metadata[0x4] ^ metadata[0x0] ^ metadata[0x1C]);
	dec[0x0D] = (metadata[0x5] ^ metadata[0x1] ^ metadata[0x1D]);
	dec[0x0E] = (metadata[0x6] ^ metadata[0x2] ^ metadata[0x1E]);
	dec[0x0F] = (metadata[0x7] ^ metadata[0x3] ^ metadata[0x1F]);
	return dec;
}

unsigned char* get_block_key(int block, NPD_HEADER *npd)
{
	unsigned char empty_key[0x10] = {};
	unsigned char *src_key = (npd->version <= 1) ? empty_key : npd->dev_hash;
	unsigned char *dest_key = (unsigned char *) malloc (0x10);
	memcpy(dest_key, src_key, 0xC);
	dest_key[0xC] = (block >> 24 & 0xFF);
	dest_key[0xD] = (block >> 16 & 0xFF);
	dest_key[0xE] = (block >> 8 & 0xFF);
	dest_key[0xF] = (block & 0xFF);
	return dest_key;
}

// EDAT/SDAT decryption.
int decrypt_data(FILE *in, FILE *out, EDAT_HEADER *edat, NPD_HEADER *npd, unsigned char* crypt_key, bool verbose)
{
	// Get metadata info and setup buffers.
	int block_num = (int) ((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_section_size = ((edat->flags & EDAT_COMPRESSED_FLAG) != 0 || (edat->flags & EDAT_FLAG_0x20) != 0) ? 0x20 : 0x10;
	int metadata_offset = 0x100;

	unsigned char *enc_data;
	unsigned char *dec_data;
	unsigned char *b_key;
	unsigned char *iv;

	unsigned char hash[0x10];
	unsigned char key_result[0x10];
	unsigned char hash_result[0x14];
	long offset = 0;
	int length = 0;
	int compression_end = 0;
	unsigned char empty_iv[0x10] = {};

	// Decrypt the metadata.
	int i;
	for (i = 0; i < block_num; i++)
	{
		fseek(in, metadata_offset + i * metadata_section_size, SEEK_SET);	
		memset(hash_result, 0, 0x14);

		if ((edat->flags & EDAT_COMPRESSED_FLAG) != 0)
		{
			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);
			fread(metadata, 0x20, 1, in);

			// If the data is compressed, decrypt the metadata.
			unsigned char *result = dec_section(metadata);
			offset = ((se32(*(int*)&result[0]) << 4) | (se32(*(int*)&result[4])));
			length = se32(*(int*)&result[8]);
			compression_end = se32(*(int*)&result[12]);
			free(result);

			memcpy(hash_result, metadata, 0x10);
		}
		else if ((edat->flags & EDAT_FLAG_0x20) != 0)
		{
			// If FLAG 0x20, the metadata precedes each data block.
			fseek(in, metadata_offset + i * metadata_section_size + length, SEEK_SET);

			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);
			fread(metadata, 0x20, 1, in);

			// If FLAG 0x20 is set, apply custom xor.
			int j;
			for (j = 0; j < 0x10; j++)
				hash_result[j] = (unsigned char) (metadata[j] ^ metadata[j + 0x10]);

			offset = metadata_offset + i * edat->block_size + (i + 1) * metadata_section_size;
			length = edat->block_size;

			if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
				length = (int) (edat->file_size % edat->block_size);
		}
		else
		{
			fread(hash_result, 0x10, 1, in);
			offset = metadata_offset + i * edat->block_size + block_num * metadata_section_size;
			length = edat->block_size;

			if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
				length = (int) (edat->file_size % edat->block_size);
		}

		// Locate the real data.
		int pad_length = length;
		length = (int) ((pad_length + 0xF) & 0xFFFFFFF0);
		fseek(in, offset, SEEK_SET);

		// Setup buffers for decryption and read the data.
		enc_data = (unsigned char *) malloc (length);
		dec_data = (unsigned char *) malloc (length);
		memset(hash, 0, 0x10);
		memset(key_result, 0, 0x10);
		fread(enc_data, length, 1, in);

		// Generate a key for the current block.
		b_key = get_block_key(i, npd);

		// Encrypt the block key with the crypto key.
		aesecb128_encrypt(crypt_key, b_key, key_result);
		if ((edat->flags & EDAT_FLAG_0x10) != 0)
			aesecb128_encrypt(crypt_key, key_result, hash);  // If FLAG 0x10 is set, encrypt again to get the final hash.
		else
			memcpy(hash, key_result, 0x10);

		// Setup the crypto and hashing mode based on the extra flags.
		int crypto_mode = ((edat->flags & EDAT_FLAG_0x02) == 0) ? 0x2 : 0x1;
		int hash_mode;

		if ((edat->flags  & EDAT_FLAG_0x10) == 0)
			hash_mode = 0x02;
		else if ((edat->flags & EDAT_FLAG_0x20) == 0)
			hash_mode = 0x04;
		else
			hash_mode = 0x01;

		if ((edat->flags  & EDAT_ENCRYPTED_KEY_FLAG) != 0)
		{
			crypto_mode |= 0x10000000;
			hash_mode |= 0x10000000;
		}

		if ((edat->flags  & EDAT_DEBUG_DATA_FLAG) != 0) 
		{
			// Reset the flags.
			crypto_mode |= 0x01000000;
			hash_mode |= 0x01000000;
			// Simply copy the data without the header or the footer.
			memcpy(dec_data, enc_data, length);
		}
		else
		{
			// IV is null if NPD version is 1 or 0.
			iv = (npd->version <= 1) ? empty_iv : npd->digest;
			// Call main crypto routine on this data block.
			if (!decrypt(hash_mode, crypto_mode, (npd->version == 4), enc_data, dec_data, length, key_result, iv, hash, hash_result))
			{
				if (verbose)
					printf("WARNING: Block at offset 0x%08x has invalid hash!\n", offset);
			}
		}

		// Apply additional compression if needed and write the decrypted data.
		if (((edat->flags & EDAT_COMPRESSED_FLAG) != 0) && compression_end)
		{
			int decomp_size = (int)edat->file_size;
			unsigned char *decomp_data = (unsigned char *) malloc (decomp_size);
			memset(decomp_data, 0, decomp_size);

			if (verbose)
				printf("Decompressing data...\n");

			int res = decompress(decomp_data, dec_data, decomp_size);
			fwrite(decomp_data, res, 1, out);

			if (verbose)
			{
				printf("Compressed block size: %d\n", pad_length);
				printf("Decompressed block size: %d\n", res);
			}

			edat->file_size -= res;

			if (edat->file_size == 0) 
			{
				if (res < 0)
				{
					printf("EDAT/SDAT decompression failed!\n");
					return 1;
				}
				else
					printf("EDAT/SDAT successfully decompressed!\n");	
			}

			free(decomp_data);
		}
		else
		{
			fwrite(dec_data, pad_length, 1, out);
		}

		free(enc_data);
		free(dec_data);
	}

	return 0;
}

int check_data(unsigned char *key, EDAT_HEADER *edat, NPD_HEADER *npd, FILE *f, bool verbose)
{
	fseek(f, 0, SEEK_SET);
	unsigned char header[0xA0];
	unsigned char empty_header[0xA0];
	unsigned char header_hash[0x10];
	unsigned char metadata_hash[0x10];
	memset(header, 0, 0xA0);
	memset(empty_header, 0, 0xA0);
	memset(header_hash, 0, 0x10);
	memset(metadata_hash, 0, 0x10);

	// Check NPD version and flags.
	if ((npd->version == 0) || (npd->version == 1))
	{
		if (edat->flags & 0x7EFFFFFE) 
		{
			printf("ERROR: Bad header flags!\n");
			return 1;
		}
	}
	else if (npd->version == 2) 
	{
		if (edat->flags & 0x7EFFFFE0) 
		{
			printf("ERROR: Bad header flags!\n");
			return 1;
		}
	}
	else if ((npd->version == 3) || (npd->version == 4))
	{
		if (edat->flags & 0x7EFFFFC0)
		{
			printf("ERROR: Bad header flags!\n");
			return 1;
		}
	}
	else
	{
		printf("ERROR: Unknown version!\n");
		return 1;
	}

	// Read in the file header.
	fread(header, 0xA0, 1, f);

	// Read in the header and metadata section hashes.
	fseek(f, 0x90, SEEK_SET);
	fread(metadata_hash, 0x10, 1, f);
	fread(header_hash, 0x10, 1, f);

	// Setup the hashing mode and the crypto mode used in the file.
	int crypto_mode = 0x1;
	int hash_mode = ((edat->flags & EDAT_ENCRYPTED_KEY_FLAG) == 0) ? 0x00000002 : 0x10000002;
	if ((edat->flags & EDAT_DEBUG_DATA_FLAG) != 0)
	{
		printf("DEBUG data detected!\n");
		hash_mode |= 0x01000000;
	}

	// Setup header key and iv buffers.
	unsigned char header_key[0x10];
	unsigned char header_iv[0x10];
	memset(header_key, 0, 0x10);
	memset(header_iv, 0, 0x10);

	// Test the header hash (located at offset 0xA0).
	if (!decrypt(hash_mode, crypto_mode, (npd->version == 4), header, empty_header, 0xA0, header_key, header_iv, key, header_hash))
	{
		if (verbose)
			printf("WARNING: Header hash is invalid!\n");
	}

	// Parse the metadata info.
	int metadata_section_size = ((edat->flags & EDAT_COMPRESSED_FLAG) != 0 || (edat->flags & EDAT_FLAG_0x20) != 0) ? 0x20 : 0x10;
	if (((edat->flags & EDAT_COMPRESSED_FLAG) != 0))
		printf("COMPRESSED data detected!\n");

	int block_num = (int) ((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_offset = 0x100;
	int metadata_size =  metadata_section_size * block_num;

	int bytes_read = 0;
	long bytes_to_read = metadata_size;
	unsigned char *metadata = (unsigned char *) malloc (metadata_size);
	unsigned char *empty_metadata = (unsigned char *) malloc (metadata_size);

	while (bytes_to_read > 0)
	{
		// Locate the metadata blocks.
		int block_size = (0x3C00 > bytes_to_read) ? (int) bytes_to_read : 0x3C00;  // 0x3C00 is the maximum block size.
		fseek(f, metadata_offset + bytes_read, SEEK_SET);

		// Read in the metadata.
		fread(metadata + bytes_read, block_size, 1, f);

		// Adjust sizes.
		bytes_read += block_size;
		bytes_to_read -= block_size;
	}

	// Test the metadata section hash (located at offset 0x90).
	if (!decrypt(hash_mode, crypto_mode, (npd->version == 4), metadata, empty_metadata, metadata_size, header_key, header_iv, key, metadata_hash))
	{
		if (verbose)
			printf("WARNING: Metadata section hash is invalid!\n");
	}

	// Cleanup.
	free(metadata);
	free(empty_metadata);

	return 0;
}

void validate_npd_hashes(const char* file_name, unsigned char *klicensee, NPD_HEADER *npd, bool verbose)
{
	int title_hash_result = 0;
	int dev_hash_result = 0;

	int file_name_length = strlen(file_name);
	unsigned char *buf = (unsigned char *) malloc (0x30 + file_name_length);
	unsigned char dev[0x60];
	unsigned char key[0x10];
	memset(dev, 0, 0x60);
	memset(key, 0, 0x10);

	// Build the title buffer (content_id + file_name).
	memcpy(buf, npd->content_id, 0x30);
	memcpy(buf + 0x30, file_name, file_name_length);

	// Build the dev buffer (first 0x60 bytes of NPD header in big-endian).
	memcpy(dev, npd, 0x60);

	// Fix endianness.
	int version = se32(npd->version);
	int license = se32(npd->license);
	int type = se32(npd->type);
	memcpy(dev + 0x4, &version, 4);
	memcpy(dev + 0x8, &license, 4);
	memcpy(dev + 0xC, &type, 4);

	// Hash with NPDRM_OMAC_KEY_3 and compare with title_hash.
	title_hash_result = cmac_hash_compare(NPDRM_OMAC_KEY_3, 0x10, buf, 0x30 + file_name_length, npd->title_hash);

	if (verbose)
	{
		if (title_hash_result)
			printf("NPD title hash is valid!\n");
		else 
			printf("WARNING: NPD title hash is invalid!\n");
	}

	// Check for an empty dev_hash (can't validate if devklic is NULL);
	bool isDevklicEmpty = true;
	int i;
	for (i = 0; i < 0x10; i++)
	{
		if (klicensee[i] != 0)
		{
			isDevklicEmpty = false;
			break;
		}
	}

	if (isDevklicEmpty)
	{
		if (verbose)
			printf("NPD dev hash is empty!\n");
	}
	else
	{
		// Generate klicensee xor key.
		xor(key, klicensee, NPDRM_OMAC_KEY_2, 0x10);

		// Hash with generated key and compare with dev_hash.
		dev_hash_result = cmac_hash_compare(key, 0x10, dev, 0x60, npd->dev_hash);

		if (verbose)
		{
			if (dev_hash_result)
				printf("NPD dev hash is valid!\n");
			else 
				printf("WARNING: NPD dev hash is invalid!\n");
		}
	}

	free(buf);
}

bool extract_data(FILE *input, FILE *output, const char* input_file_name, unsigned char* devklic, unsigned char* rifkey, bool verbose)
{
	// Setup NPD and EDAT/SDAT structs.
	NPD_HEADER *NPD = (NPD_HEADER *) malloc (sizeof(NPD_HEADER));
	EDAT_HEADER *EDAT = (EDAT_HEADER *) malloc (sizeof(EDAT_HEADER));

	// Read in the NPD and EDAT/SDAT headers.
	char npd_header[0x80];
	char edat_header[0x10];
	fread(npd_header, sizeof(npd_header), 1, input);
	fread(edat_header, sizeof(edat_header), 1, input);

	memcpy(NPD->magic, npd_header, 4);
	NPD->version = se32(*(int*)&npd_header[4]);
	NPD->license = se32(*(int*)&npd_header[8]);
	NPD->type = se32(*(int*)&npd_header[12]);
	memcpy(NPD->content_id, (unsigned char*)&npd_header[16], 0x30);
	memcpy(NPD->digest, (unsigned char*)&npd_header[64], 0x10);
	memcpy(NPD->title_hash, (unsigned char*)&npd_header[80], 0x10);
	memcpy(NPD->dev_hash, (unsigned char*)&npd_header[96], 0x10);
	NPD->unk1 = se64(*(u64*)&npd_header[112]);
	NPD->unk2 = se64(*(u64*)&npd_header[120]);

	unsigned char npd_magic[4] = {0x4E, 0x50, 0x44, 0x00};  //NPD0
	if(memcmp(NPD->magic, npd_magic, 4))
	{
		printf("ERROR: File has invalid NPD header.");
		return 1;
	}

	EDAT->flags = se32(*(int*)&edat_header[0]);
	EDAT->block_size = se32(*(int*)&edat_header[4]);
	EDAT->file_size = se64(*(u64*)&edat_header[8]);

	printf("NPD HEADER\n");
	printf("NPD version: %d\n", NPD->version);
	printf("NPD license: %d\n", NPD->license);
	printf("NPD type: %d\n", NPD->type);
	printf("\n");

	// Set decryption key.
	unsigned char key[0x10];
	memset(key, 0, 0x10);

	// Check EDAT/SDAT flag.
	if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
		printf("SDAT HEADER\n");
		printf("SDAT flags: 0x%08X\n", EDAT->flags);
		printf("SDAT block size: 0x%08X\n", EDAT->block_size);
		printf("SDAT file size: 0x%08X\n", EDAT->file_size);
		printf("\n");

		// Generate SDAT key.
		xor(key, NPD->dev_hash, SDAT_KEY, 0x10);
	}
	else
	{
		printf("EDAT HEADER\n");
		printf("EDAT flags: 0x%08X\n", EDAT->flags);
		printf("EDAT block size: 0x%08X\n", EDAT->block_size);
		printf("EDAT file size: 0x%08X\n", EDAT->file_size);
		printf("\n");

		// Perform header validation (optional step for EDAT only).
		validate_npd_hashes(input_file_name, devklic, NPD, verbose);

		// Select EDAT key.
		if ((NPD->license & 0x3) == 0x3)      // Type 3: Use supplied devklic.
			memcpy(key, devklic, 0x10);
		else if (((NPD->license & 0x1) == 0x1) || ((NPD->license & 0x2) == 0x2)) // Type 1 or 2: Use key from RAP file (RIF key).
		{
			memcpy(key, rifkey, 0x10);

			// Make sure we don't have an empty RIF key.
			int i, test = 0;
			for (i = 0; i < 0x10; i++)
			{
				if (key[i] != 0)
				{
					test = 1;
					break;
				}
			}

			if (!test)
			{
				printf("ERROR: A valid RAP file is needed for this EDAT file!");
				return 1;
			}
		}

		if (verbose)
		{
			int i;
			printf("DEVKLIC: ");
			for (i = 0; i < 0x10; i++)
				printf("%02X", devklic[i]);
			printf("\n");

			printf("RIF KEY: ");
			for (i = 0; i < 0x10; i++)
				printf("%02X", rifkey[i]);
			printf("\n");
		}
	}

	if (verbose)
	{
		int i;
		printf("DECRYPTION KEY: ");
		for(i = 0; i < 0x10; i++)
			printf("%02X", key[i]);
		printf("\n\n");
	}

	printf("Parsing data...\n");
	if (check_data(key, EDAT, NPD, input, verbose))
		printf("Parsing failed!\n");
	else
		printf("File successfully parsed!\n");

	printf("\n");

	printf("Decrypting data...\n");
	if (decrypt_data(input, output, EDAT, NPD, key, verbose))
		printf("Decryption failed!");
	else
		printf("File successfully decrypted!\n");

	free(NPD);
	free(EDAT);

	return 0;
}

// EDAT/SDAT encryption.
int encrypt_data(FILE *in, FILE *out, EDAT_HEADER *edat, NPD_HEADER *npd, unsigned char* crypt_key, bool verbose)
{
	// Set metadata info and setup buffers.
	int block_num = (int) ((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_offset = 0x100;

	unsigned char *enc_data;
	unsigned char *dec_data;
	unsigned char *b_key;
	unsigned char *iv;

	unsigned char hash[0x10];
	unsigned char key_result[0x10];
	unsigned char hash_result[0x14];
	long offset = 0;
	int length = 0;
	int compression_end = 0;
	unsigned char empty_iv[0x10] = {};

	// Build special data footers for each version.
	unsigned char edat_footer_v1[0x10] = {0x45, 0x44, 0x41, 0x54, 0x41, 0x20, 0x70, 0x61, 0x63, 0x6B, 0x61, 0x67, 0x65, 0x72, 0x00, 0x00};
	unsigned char edat_footer_v2[0x10] = {0x45, 0x44, 0x41, 0x54, 0x41, 0x20, 0x32, 0x2E, 0x34, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};
	unsigned char edat_footer_v3[0x10] = {0x45, 0x44, 0x41, 0x54, 0x41, 0x20, 0x33, 0x2E, 0x33, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};
	unsigned char edat_footer_v4[0x10] = {0x45, 0x44, 0x41, 0x54, 0x41, 0x20, 0x34, 0x2E, 0x30, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};
	unsigned char sdat_footer_v1[0x10] = {0x53, 0x44, 0x41, 0x54, 0x41, 0x20, 0x70, 0x61, 0x63, 0x6B, 0x61, 0x67, 0x65, 0x72, 0x00, 0x00};
	unsigned char sdat_footer_v2[0x10] = {0x53, 0x44, 0x41, 0x54, 0x41, 0x20, 0x32, 0x2E, 0x34, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};
	unsigned char sdat_footer_v3[0x10] = {0x53, 0x44, 0x41, 0x54, 0x41, 0x20, 0x33, 0x2E, 0x33, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};
	unsigned char sdat_footer_v4[0x10] = {0x53, 0x44, 0x41, 0x54, 0x41, 0x20, 0x34, 0x2E, 0x30, 0x2E, 0x30, 0x2E, 0x57, 0x00, 0x00, 0x00};

	// Encrypt the data and generate the metadata.
	int i;
	for (i = 0; i < block_num; i++)
	{
		memset(hash_result, 0, 0x14);

		offset = i * edat->block_size;
		length = edat->block_size;

		if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
			length = (int) (edat->file_size % edat->block_size);

		// Locate the real data.
		int pad_length = length;
		length = (int) ((pad_length + 0xF) & 0xFFFFFFF0);
		fseek(in, offset, SEEK_SET);

		// Setup buffers for encryption and read the data.
		enc_data = (unsigned char *) malloc (length);
		dec_data = (unsigned char *) malloc (length);
		memset(enc_data, 0, length);
		memset(dec_data, 0, length);
		memset(hash, 0, 0x10);
		memset(key_result, 0, 0x10);
		fread(dec_data, pad_length, 1, in);

		// Generate a key for the current block.
		b_key = get_block_key(i, npd);

		// Encrypt the block key with the crypto key.
		aesecb128_encrypt(crypt_key, b_key, key_result);
		if ((edat->flags & EDAT_FLAG_0x10) != 0)
			aesecb128_encrypt(crypt_key, key_result, hash);  // If FLAG 0x10 is set, encrypt again to get the final hash.
		else
			memcpy(hash, key_result, 0x10);

		// Setup the crypto and hashing mode based on the extra flags.
		int crypto_mode = ((edat->flags & EDAT_FLAG_0x02) == 0) ? 0x2 : 0x1;
		int hash_mode;

		if ((edat->flags  & EDAT_FLAG_0x10) == 0)
			hash_mode = 0x02;
		else if ((edat->flags & EDAT_FLAG_0x20) == 0)
			hash_mode = 0x04;
		else
			hash_mode = 0x01;

		if ((edat->flags  & EDAT_ENCRYPTED_KEY_FLAG) != 0)
		{
			crypto_mode |= 0x10000000;
			hash_mode |= 0x10000000;
		}

		if ((edat->flags  & EDAT_DEBUG_DATA_FLAG) != 0) 
		{
			// Reset the flags.
			crypto_mode |= 0x01000000;
			hash_mode |= 0x01000000;
			// Simply copy the data.
			memcpy(enc_data, dec_data, length);
		}
		else
		{
			// IV is null if NPD version is 1 or 0.
			iv = (npd->version <= 1) ? empty_iv : npd->digest;
			// Call main crypto routine on this data block.
			if(!encrypt(hash_mode, crypto_mode, (npd->version == 4), dec_data, enc_data, length, key_result, iv, hash, hash_result))
			{
				if (verbose)
					printf("WARNING: Block at offset 0x%08x got invalid forged hash!\n", offset);
			}
		}

		// Write the metadata and the encrypted data.
		if ((edat->flags & EDAT_COMPRESSED_FLAG) != 0)
		{
			long data_offset = metadata_offset + i * edat->block_size + block_num * 0x20;
			
			// If the data is compressed, encrypt the metadata.
			unsigned char dec_metadata[0x20];
			unsigned char enc_metadata[0x20];
			memset(dec_metadata, 0, 0x20);
			memset(enc_metadata, 0, 0x20);

			// Build the metadata section.
			u64 data_offset_be = se64(data_offset);
			int length_be = se32(pad_length);
			int compression_end_be = se32(compression_end);
			memcpy(dec_metadata, hash_result, 0x10);
			memcpy(dec_metadata + 0x10, &data_offset_be, 8);
			memcpy(dec_metadata + 0x10 + 8, &length_be, 4);
			memcpy(dec_metadata + 0x10 + 12, &compression_end_be, 4);

			// Encrypt the metadata section.
			memcpy(enc_metadata, dec_metadata, 0x10);
			memcpy(enc_metadata + 0x10, dec_section(dec_metadata), 0x10);
			
			// Write the encrypted metadata.
			fseek(out, metadata_offset + i * 0x20, SEEK_SET);
			fwrite(enc_metadata, 0x20, 1, out);

			// Write the encrypted data.
			fseek(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}
		else if ((edat->flags & EDAT_FLAG_0x20) != 0)
		{
			int data_offset = metadata_offset + i * edat->block_size + (i + 1) * 0x20;

			// If FLAG 0x20 is set, apply custom xor.
			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);
			
			// Use a fake XOR value and build the metadata.
			unsigned char hash_result_1[0x10];
			memset(hash_result_1, 0, 0x10);
			unsigned char hash_result_2[0x10];
			memset(hash_result_2, 0, 0x10);
			prng(hash_result_2, 0x10);

			int j;
			for (j = 0; j < 0x10; j++)
				hash_result_1[j] = (unsigned char) (hash_result[j] ^ hash_result_2[j]);
			
			// Set the metadata.
			memcpy(metadata, hash_result_1, 0x10);
			memcpy(metadata + 0x10, hash_result_2, 0x10);

			// Write the encrypted metadata.
			fseek(out, metadata_offset + i * 0x20 + offset, SEEK_SET);
			fwrite(metadata, 0x20, 1, out);

			// Write the encrypted data.
			fseek(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}
		else
		{
			int data_offset = metadata_offset + i * edat->block_size + block_num * 0x10;
			
			// Write the encrypted metadata.
			fseek(out, metadata_offset + i * 0x10, SEEK_SET);
			fwrite(hash_result, 0x10, 1, out);

			// Write the encrypted data.
			fseek(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}

		free(enc_data);
		free(dec_data);
	}

	// Append the special version footer.
	if ((npd->version == 0) || (npd->version == 1))
	{
		if ((edat->flags & SDAT_FLAG) == SDAT_FLAG)
			fwrite(sdat_footer_v1, 0x10, 1, out);
		else
			fwrite(edat_footer_v1, 0x10, 1, out);
	}
	else if (npd->version == 2) 
	{
		if ((edat->flags & SDAT_FLAG) == SDAT_FLAG)
			fwrite(sdat_footer_v2, 0x10, 1, out);
		else
			fwrite(edat_footer_v2, 0x10, 1, out);
	}
	else if (npd->version == 3)
	{
		if ((edat->flags & SDAT_FLAG) == SDAT_FLAG)
			fwrite(sdat_footer_v3, 0x10, 1, out);
		else
			fwrite(edat_footer_v3, 0x10, 1, out);
	}
	else if (npd->version == 4)
	{
		if ((edat->flags & SDAT_FLAG) == SDAT_FLAG)
			fwrite(sdat_footer_v4, 0x10, 1, out);
		else
			fwrite(edat_footer_v4, 0x10, 1, out);
	}

	return 0;
}

int forge_data(unsigned char *key, EDAT_HEADER *edat, NPD_HEADER *npd, FILE *f)
{
	unsigned char header[0xA0];
	unsigned char empty_header[0xA0];
	unsigned char header_hash[0x10];
	unsigned char metadata_hash[0x10];
	unsigned char signature[0x50];
	memset(header, 0, 0xA0);
	memset(empty_header, 0, 0xA0);
	memset(header_hash, 0, 0x10);
	memset(metadata_hash, 0, 0x10);
	memset(signature, 0, 0x50);

	// Setup the hashing mode and the crypto mode used in the file.
	int crypto_mode = 0x1;
	int hash_mode = ((edat->flags & EDAT_ENCRYPTED_KEY_FLAG) == 0) ? 0x00000002 : 0x10000002;
	if ((edat->flags & EDAT_DEBUG_DATA_FLAG) != 0)
		hash_mode |= 0x01000000;

	// Setup header key and iv buffers.
	unsigned char header_key[0x10];
	unsigned char header_iv[0x10];
	memset(header_key, 0, 0x10);
	memset(header_iv, 0, 0x10);

	// Parse the metadata info.
	int metadata_section_size = ((edat->flags & EDAT_COMPRESSED_FLAG) != 0 || (edat->flags & EDAT_FLAG_0x20) != 0) ? 0x20 : 0x10;
	int block_num = (int) ((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_offset = 0x100;
	int metadata_size =  metadata_section_size * block_num;

	int bytes_read = 0;
	long bytes_to_read = metadata_size;
	unsigned char *metadata = (unsigned char *) malloc (metadata_size);
	unsigned char *empty_metadata = (unsigned char *) malloc (metadata_size);

	while (bytes_to_read > 0)
	{
		// Locate the metadata blocks.
		int block_size = (0x3C00 > bytes_to_read) ? (int) bytes_to_read : 0x3C00;  // 0x3C00 is the maximum block size.
		fseek(f, metadata_offset + bytes_read, SEEK_SET);

		// Read in the metadata.
		fread(metadata + bytes_read, block_size, 1, f);

		// Adjust sizes.
		bytes_read += block_size;
		bytes_to_read -= block_size;
	}

	// Generate the metadata section hash (located at offset 0x90).
	encrypt(hash_mode, crypto_mode, (npd->version == 4), metadata, empty_metadata, metadata_size, header_key, header_iv, key, metadata_hash);

	// Write back the forged metadata section hash.
	fseek(f, 0x90, SEEK_SET);
	fwrite(metadata_hash, 0x10, 1, f);

	// Read in the file header.
	fseek(f, 0, SEEK_SET);
	fread(header, 0xA0, 1, f);

	// Generate the header hash (located at offset 0xA0).
	encrypt(hash_mode, crypto_mode, (npd->version == 4), header, empty_header, 0xA0, header_key, header_iv, key, header_hash);

	// Write back the forged header section hash.
	fseek(f, 0xA0, SEEK_SET);
	fwrite(header_hash, 0x10, 1, f);

	// The ECDSA signature field is not verified (except for PS2 Classics).
	// Fill with random data for now.
	fseek(f, 0xB0, SEEK_SET);
	prng(signature, 0x50);
	fwrite(signature, 0x50, 1, f);

	// Cleanup.
	free(metadata);
	free(empty_metadata);

	return 0;
}

void forge_npd_title_hash(const char* file_name, NPD_HEADER *npd)
{
	int file_name_length = strlen(file_name);
	unsigned char *buf = (unsigned char *) malloc (0x30 + file_name_length);
	unsigned char title_hash[0x10];
	memset(title_hash, 0, 0x10);

	// Build the title buffer (content_id + file_name).
	memcpy(buf, npd->content_id, 0x30);
	memcpy(buf + 0x30, file_name, file_name_length);

	// Forge with NPDRM_OMAC_KEY_3 and create the title hash.
	cmac_hash_forge(NPDRM_OMAC_KEY_3, 0x10, buf, 0x30 + file_name_length, title_hash);

	// Write the key in the NPD header.
	memcpy(npd->title_hash, title_hash, 0x10);

	free(buf);
}

void forge_npd_dev_hash(unsigned char *klicensee, NPD_HEADER *npd)
{
	unsigned char key[0x10];
	unsigned char dev[0x60];
	unsigned char dev_hash[0x10];
	memset(key, 0, 0x10);
	memset(dev, 0, 0x60);
	memset(dev_hash, 0, 0x10);

	// Build the dev buffer (first 0x60 bytes of NPD header in big-endian).
	memcpy(dev, npd, 0x60);

	// Fix endianness.
	int version = se32(npd->version);
	int license = se32(npd->license);
	int type = se32(npd->type);
	memcpy(dev + 0x4, &version, 4);
	memcpy(dev + 0x8, &license, 4);
	memcpy(dev + 0xC, &type, 4);

	// Generate klicensee xor key.
	xor(key, klicensee, NPDRM_OMAC_KEY_2, 0x10);

	// Forge with the generated key and create the dev hash.
	cmac_hash_forge(key, 0x10, dev, 0x60, dev_hash);

	// Write the key in the NPD header.
	memcpy(npd->dev_hash, dev_hash, 0x10);
}

bool pack_data(FILE *input, FILE *output, const char* input_file_name, unsigned char* content_id, unsigned char* devklic, unsigned char* rifkey, int version, int license, int type, int block, bool useCompression, bool isSDAT, bool verbose)
{
	// Get file size.
	fseek(input, 0, SEEK_END);
	int input_file_size = ftell(input);
	fseek(input, 0, SEEK_SET);

	// Setup NPD and EDAT/SDAT structs.
	NPD_HEADER *NPD = (NPD_HEADER *) malloc (sizeof(NPD_HEADER));
	EDAT_HEADER *EDAT = (EDAT_HEADER *) malloc (sizeof(EDAT_HEADER));

	// Forge NPD header.
	unsigned char npd_magic[4] = {0x4E, 0x50, 0x44, 0x00};  //NPD0
	unsigned char fake_digest[0x10];
	memset(fake_digest, 0, 0x10);
	prng(fake_digest, 0x10);
	memcpy(NPD->magic, npd_magic, 4);
	NPD->version = version;
	NPD->license = license;
	NPD->type = type;
	memcpy(NPD->content_id, content_id, 0x30);
	memcpy(NPD->digest, fake_digest, 0x10);
	forge_npd_title_hash(input_file_name, NPD);
	forge_npd_dev_hash(devklic, NPD);
	NPD->unk1 = 0;
	NPD->unk2 = 0;

	// Forge EDAT/SDAT header with fixed values for flags and block size.
	if (version == 1)
	{
		EDAT->flags = 0x00;
		if (isSDAT) EDAT->flags |= SDAT_FLAG;
		if (useCompression) EDAT->flags |= EDAT_COMPRESSED_FLAG;
	}
	else if (version == 2)
	{
		EDAT->flags = 0x0C;
		if (isSDAT) EDAT->flags |= SDAT_FLAG;
		if (useCompression) EDAT->flags |= EDAT_COMPRESSED_FLAG;
	}
	else
	{
		EDAT->flags = 0x3C;
		if (isSDAT) EDAT->flags |= SDAT_FLAG;
		if (useCompression) EDAT->flags |= EDAT_COMPRESSED_FLAG;
	}

	if (block == 32)
		EDAT->block_size = 0x8000;
	else
		EDAT->block_size = 0x4000;
	
	EDAT->file_size = input_file_size;

	printf("NPD HEADER\n");
	printf("NPD version: %d\n", NPD->version);
	printf("NPD license: %d\n", NPD->license);
	printf("NPD type: %d\n", NPD->type);
	printf("\n");

	// Set encryption key.
	unsigned char key[0x10];
	memset(key, 0, 0x10);

	// Check EDAT/SDAT flag.
	if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
		printf("SDAT HEADER\n");
		printf("SDAT flags: 0x%08X\n", EDAT->flags);
		printf("SDAT block size: 0x%08X\n", EDAT->block_size);
		printf("SDAT file size: 0x%08X\n", EDAT->file_size);
		printf("\n");

		// Generate SDAT key.
		xor(key, NPD->dev_hash, SDAT_KEY, 0x10);
	}
	else
	{
		printf("EDAT HEADER\n");
		printf("EDAT flags: 0x%08X\n", EDAT->flags);
		printf("EDAT block size: 0x%08X\n", EDAT->block_size);
		printf("EDAT file size: 0x%08X\n", EDAT->file_size);
		printf("\n");

		// Select EDAT key.
		if ((NPD->license & 0x3) == 0x3)      // Type 3: Use supplied devklic.
			memcpy(key, devklic, 0x10);
		else if (((NPD->license & 0x1) == 0x1) || ((NPD->license & 0x2) == 0x2)) // Type 1 or 2: Use key from RAP file (RIF key).
		{
			memcpy(key, rifkey, 0x10);

			// Make sure we don't have an empty RIF key.
			int i, test = 0;
			for (i = 0; i < 0x10; i++)
			{
				if (key[i] != 0)
				{
					test = 1;
					break;
				}
			}

			if (!test)
			{
				printf("ERROR: A valid RAP file is needed for this EDAT file!");
				return 1;
			}
		}

		if (verbose)
		{
			int i;
			printf("DEVKLIC: ");
			for (i = 0; i < 0x10; i++)
				printf("%02X", devklic[i]);
			printf("\n");

			printf("RIF KEY: ");
			for (i = 0; i < 0x10; i++)
				printf("%02X", rifkey[i]);
			printf("\n");
		}
	}

	if (verbose)
	{
		int i;
		printf("ENCRYPTION KEY: ");
		for(i = 0; i < 0x10; i++)
			printf("%02X", key[i]);
		printf("\n\n");
	}

	// Write forged NPD header.
	int version_be = se32(NPD->version);
	int license_be = se32(NPD->license);
	int type_be = se32(NPD->type);
	u64 unk1_be = se64(NPD->unk1);
	u64 unk2_be = se64(NPD->unk2);
	fwrite(NPD->magic, sizeof(NPD->magic), 1, output);
	fwrite(&version_be, sizeof(version_be), 1, output);
	fwrite(&license_be, sizeof(license_be), 1, output);
	fwrite(&type_be, sizeof(type_be), 1, output);
	fwrite(NPD->content_id, sizeof(NPD->content_id), 1, output);
	fwrite(NPD->digest, sizeof(NPD->digest), 1, output);
	fwrite(NPD->title_hash, sizeof(NPD->title_hash), 1, output);
	fwrite(NPD->dev_hash, sizeof(NPD->dev_hash), 1, output);
	fwrite(&unk1_be, sizeof(unk1_be), 1, output);
	fwrite(&unk2_be, sizeof(unk2_be), 1, output);

	// Write forged EDAT/SDAT header.
	int flags_be = se32(EDAT->flags);
	int block_size_be = se32(EDAT->block_size);
	u64 file_size_be = se64(EDAT->file_size);
	fwrite(&flags_be, sizeof(flags_be), 1, output);
	fwrite(&block_size_be, sizeof(block_size_be), 1, output);
	fwrite(&file_size_be, sizeof(file_size_be), 1, output);

	printf("Encrypting data...\n");
	if (encrypt_data(input, output, EDAT, NPD, key, verbose))
		printf("Encryption failed!");
	else
		printf("File successfully encrypted!");

	printf("\n");

	printf("Forging data...\n");
	if (forge_data(key, EDAT, NPD, output))
		printf("Forging failed!\n");
	else
		printf("File successfully forged!\n");

	free(NPD);
	free(EDAT);

	return 0;
}

void print_usage()
{
	printf("**********************************************************************\n\n");
	printf("make_npdata v1.2 - PS3 EDAT/SDAT file encrypter/decrypter/bruteforcer.\n");
	printf("                 - Written by Hykem (C).\n\n");
	printf("**********************************************************************\n\n");
	printf("Usage: make_npdata [-v] -e <input> <output> <version> <license>\n");
	printf("                           <type> <format> <block> <compress>\n");
	printf("                           <cID> <klic> <rap>\n");
	printf("       make_npdata [-v] -d <input> <output> <klic> <rap>\n");
	printf("       make_npdata [-v] -b <input> <source> <mode>\n\n");
	printf("- Modes:\n");
	printf("[-v]: Verbose mode\n");
	printf("[-e]: Encryption mode\n");
	printf("[-d]: Decryption mode\n");
	printf("[-b]: Bruteforce mode\n");
	printf("\n");
	printf("- Encryption mode only:\n");
	printf("<version>:  1 - EDAT/SDAT version 1\n");
	printf("            2 - EDAT/SDAT version 2\n");
	printf("            3 - EDAT/SDAT version 3\n");
	printf("            4 - EDAT/SDAT version 4\n");
	printf("<license>:  0 - Debug license (SDAT)\n");
	printf("            1 - Network license (not supported)\n");
	printf("            2 - Local license (uses RAP file as key)\n");
	printf("            3 - Free license (uses klic as key)\n");
	printf("<type>:     00 - Common\n");
	printf("            01 - PS2 EDAT\n");
	printf("            32 - PSP Remasters\n");
	printf("            33 - Modules (disc bind)\n");
	printf("            48 - Unknown\n");
	printf("<format>:   0 - EDAT\n");
	printf("            1 - SDAT\n");
	printf("<block>:    16 - Default block size\n");
	printf("            32 - Maximum block size\n");
	printf("<compress>: 0 - Disable compression\n");
	printf("            1 - Enable compression\n");
	printf("<cID>: Content ID (XXYYYY-AAAABBBBB_CC-DDDDDDDDDDDDDDDD)\n");
	printf("\n");
	printf("- Encryption and decryption modes:\n");
	printf("<klic>: 0 - No key (SDAT)\n");
	printf("        1 - NPDRM OMAC key 1 (free license key)\n");
	printf("        2 - NPDRM OMAC key 2\n");
	printf("        3 - NPDRM OMAC key 3\n");
	printf("        4 - PS3 key (klic_dec_key)\n");
	printf("        5 - PSX key (PSOne Classics)\n");
	printf("        6 - PSP key 1 (PSP Minis)\n");
	printf("        7 - PSP key 2 (PSP Remasters)\n");
	printf("        8 - Custom key (read from klic.bin)\n");
	printf("<rap>: RAP file for encryption/decryption (optional)\n");
	printf("\n");
	printf("- Bruteforce mode:\n");
	printf("<source>: ELF file source for klic\n");
	printf("<mode>: 0 - Binary\n");
	printf("        1 - Text\n");
}

int make_npdata_main(int argc, char **argv)
{
	if (argc < 4)
	{
		print_usage();
		return 0;
	}

	// Keep track of the each argument's offset.
	int arg_offset = 0;

	// Check if we're using verbose mode.
	bool verbose = false;
	if (!strcmp(argv[1], "-v"))
	{
		verbose = true;
		arg_offset++;
	}

	// Check which mode we're using (encryption/decryption/bruteforce).
	if ((!strcmp(argv[arg_offset + 1], "-e")) && (argc > (arg_offset + 11)))
	{
		// Skip the mode argument.
		arg_offset++;

		// Get the input and output files.
		const char *input_file_name = argv[arg_offset + 1];
		const char *output_file_name = argv[arg_offset + 2];
		FILE* input = fopen(input_file_name, "rb");
		FILE* output = fopen(output_file_name, "wb+");

		// Read all the necessary parameters for encryption.
		int version = atoi(argv[arg_offset + 3]);
		int license = atoi(argv[arg_offset + 4]);
		int type = atoi(argv[arg_offset + 5]);
		int format = atoi(argv[arg_offset + 6]);
		int block = atoi(argv[arg_offset + 7]);
		int compression = atoi(argv[arg_offset + 8]);

		char cID[0x30];
		memset(cID, 0, 0x30);
		strcpy(cID, argv[arg_offset + 9]);

		// Check for invalid parameters.
		if (((version > 4) && (version < 1))
			|| ((license > 3) && (license < 0)
			|| ((format > 1) && (format < 0))
			|| ((block != 16) && (block != 32))
			|| ((compression > 1) && (compression < 0))
			|| (cID == NULL)))
		{
			printf("ERROR: Invalid parameters!\n");
			return 0;
		}

		// Select the EDAT key mode.
		int edat_mode = atoi(argv[arg_offset + 10]);
		unsigned char rifkey[0x10];
		unsigned char devklic[0x10];
		memset(rifkey, 0, 0x10);
		memset(devklic, 0, 0x10);

		switch (edat_mode) 
		{
		case 0:
			break;
		case 1:
			memcpy(devklic, NPDRM_OMAC_KEY_1, 0x10);
			break;
		case 2:
			memcpy(devklic, NPDRM_OMAC_KEY_2, 0x10);
			break;
		case 3:
			memcpy(devklic, NPDRM_OMAC_KEY_3, 0x10);
			break;
		case 4:
			memcpy(devklic, NPDRM_KLIC_KEY, 0x10);
			break;
		case 5:
			memcpy(devklic, NPDRM_PSX_KEY, 0x10);
			break;
		case 6:
			memcpy(devklic, NPDRM_PSP_KEY_1, 0x10);
			break;
		case 7:
			memcpy(devklic, NPDRM_PSP_KEY_2, 0x10);
			break;
		case 8: 
			{
				// Read a custom binary klic from klic.bin (user supplied file).
				FILE* klic_file = fopen("klic.bin", "rb");

				if (klic_file != NULL)
				{
					unsigned char custom_klic[0x10];
					fread(custom_klic, 0x10, 1, klic_file);
					memcpy(devklic, custom_klic, 0x10);
					fclose(klic_file);
				}
				else
				{
					printf("ERROR: Please place your binary custom klic in a klic.bin file!\n");
					return 0;
				}

				break;
			}
		default:
			printf("ERROR: Invalid mode!\n");
			return 0;
		}

		// Read the RAP file, if provided.
		if (argv[arg_offset + 11] != NULL)
		{
			FILE* rap = fopen(argv[arg_offset + 11], "rb");

			unsigned char rapkey[0x10];
			memset(rapkey, 0, 0x10);

			fread(rapkey, sizeof(rapkey), 1, rap);

			get_rif_key(rapkey, rifkey);

			fclose(rap);
		}

		// Delete the bad output file if any errors arise.
		if (pack_data(input, output, output_file_name, (unsigned char *)cID, devklic, rifkey, version, license, type, block, compression ? true : false, format ? true : false, verbose))
		{
			fclose(input);
			fclose(output);
			remove(output_file_name);
			return 0;
		}

		// Cleanup.
		fclose(input);
		fclose(output);
		return 0;
	}
	else if (!strcmp(argv[arg_offset + 1], "-d") && (argc > (arg_offset + 4)))
	{
		// Skip the mode argument.
		arg_offset++;

		// Get the input and output files.
		const char *input_file_name = argv[arg_offset + 1];
		const char *output_file_name = argv[arg_offset + 2];
		FILE* input = fopen(input_file_name, "rb");
		FILE* output = fopen(output_file_name, "wb");

		// Select the EDAT key mode.
		int edat_mode = atoi(argv[arg_offset + 3]);
		unsigned char rifkey[0x10];
		unsigned char devklic[0x10];
		memset(rifkey, 0, 0x10);
		memset(devklic, 0, 0x10);

		switch (edat_mode) 
		{
		case 0:
			break;
		case 1:
			memcpy(devklic, NPDRM_OMAC_KEY_1, 0x10);
			break;
		case 2:
			memcpy(devklic, NPDRM_OMAC_KEY_2, 0x10);
			break;
		case 3:
			memcpy(devklic, NPDRM_OMAC_KEY_3, 0x10);
			break;
		case 4:
			memcpy(devklic, NPDRM_KLIC_KEY, 0x10);
			break;
		case 5:
			memcpy(devklic, NPDRM_PSX_KEY, 0x10);
			break;
		case 6:
			memcpy(devklic, NPDRM_PSP_KEY_1, 0x10);
			break;
		case 7:
			memcpy(devklic, NPDRM_PSP_KEY_2, 0x10);
			break;
		case 8: 
			{
				// Read a custom binary klic from klic.bin (user supplied file).
				FILE* klic_file = fopen("klic.bin", "rb");

				if (klic_file != NULL)
				{
					unsigned char custom_klic[0x10];
					fread(custom_klic, 0x10, 1, klic_file);
					memcpy(devklic, custom_klic, 0x10);
					fclose(klic_file);
				}
				else
				{
					printf("ERROR: Please place your binary custom klic in a klic.bin file!\n");
					return 0;
				}

				break;
			}
		default:
			printf("ERROR: Invalid mode!\n");
			return 0;
		}

		// Read the RAP file, if provided.
		if (argv[arg_offset + 4] != NULL)
		{
			FILE* rap = fopen(argv[arg_offset + 4], "rb");

			unsigned char rapkey[0x10];
			memset(rapkey, 0, 0x10);

			fread(rapkey, sizeof(rapkey), 1, rap);

			get_rif_key(rapkey, rifkey);

			fclose(rap);
		}

		// Delete the bad output file if any errors arise.
		if (extract_data(input, output, input_file_name, devklic, rifkey, verbose))
		{
			fclose(input);
			fclose(output);
			remove(output_file_name);
			return 0;
		}

		// Cleanup.
		fclose(input);
		fclose(output);
		return 0;
	}
	else if (!strcmp(argv[arg_offset + 1], "-b") && (argc > (arg_offset + 3)))
	{
		// Skip the mode argument.
		arg_offset++;

		// Get the input and source files.
		const char *input_file_name = argv[arg_offset + 1];
		const char *source_file_name = argv[arg_offset + 2];
		FILE* input = fopen(input_file_name, "rb");
		FILE* source = fopen(source_file_name, "rb");

		// Read data as plain binary or as text.
		int mode = 0;
		if (argv[arg_offset + 3] != NULL)
		{
			mode = atoi(argv[arg_offset + 3]);
			if ((mode != 0) && (mode != 1))
			{
				printf("ERROR: Invalid parameters!\n");
				return 0;
			}
		}

		// Get the source file size.
		fseek(source, 0, SEEK_END);
		int source_file_size = ftell(source);
		fseek(source, 0, SEEK_SET);

		// Set up testing keys and hash.
		unsigned char test_key[0x10];
		unsigned char test_klicensee[0x10];
		unsigned char test_dev_hash[0x10];
		memset(test_key, 0, 0x10);
		memset(test_klicensee, 0, 0x10);
		memset(test_dev_hash, 0, 0x10);

		// Buffer to handle klicensee as text.
		char test_klicensee_text[0x20];
		memset(test_klicensee_text, 0, 0x20);

		// Read the file's header magic and seek back.
		unsigned char magic[0x4];
		fread(magic, 0x4, 1, input);
		fseek(input, 0, SEEK_SET);

		// If header starts with SCE, the file is a SELF or SPRX.
		// If not, assume regular EDAT/SDAT (NPD).
		unsigned char sce_magic[4] = {0x53, 0x43, 0x45, 0x00};  //SCE0
		if(!memcmp(magic, sce_magic, 4))
		{
			// File is SCE, read the NPD dev_hash offset from the
			// first 0x10 bytes of the SCE header and seek to the NPD area.	
			unsigned char sce_header[0x10];
			fread(sce_header, 0x10, 1, input);
			short npd_offset = se16(*(short*)&sce_header[0xE]) - 0x60;
			fseek(input, npd_offset, SEEK_SET);

			if (verbose)
			{
				printf("SCE file detected!\n");
				printf("NPD offset inside SCE: 0x%08x\n", npd_offset);
			}
		}

		// Read the first 0x60 bytes of the NPD header.
		unsigned char npd_buf[0x60];
		fread(npd_buf, 0x60, 1, input);

		// Read the NPD header dev_hash.
		fread(test_dev_hash, 0x10, 1, input);

		if (verbose)
		{
			if (mode)
				printf("MODE: Text\n");
			else
				printf("MODE: Binary\n");

			int i;
			printf("DEV HASH: ");
			for(i = 0; i < 0x10; i++)
				printf("%02X", test_dev_hash[i]);
			printf("\n\n");
		}

		printf("Bruteforcing klic...\n");

		int i;
		bool found = false;
		for (i = 0; i < source_file_size; i ++)
		{
			// Iterate the source file and generate klicensee xor key.
			fseek(source, i, SEEK_SET);

			// If reading in text mode, convert the hexadecimal string to binary data.
			if (mode)
			{
				fread(test_klicensee_text, 0x20, 1, source);
				// If the string is not a valid hexadecimal string, continue the loop.
				if (!is_hex(test_klicensee_text, 0x20)) continue;
				hex_to_bytes(test_klicensee, test_klicensee_text, 0x20);
			}
			else
				fread(test_klicensee, 0x10, 1, source);

			xor(test_key, test_klicensee, NPDRM_OMAC_KEY_2, 0x10);
			if (cmac_hash_compare(test_key, 0x10, npd_buf, 0x60, test_dev_hash))
			{
				found = true;
				break;
			}
		}

		if (found)
		{
			printf("Found valid klic! Saved to klic.bin\n");
			FILE* klic = fopen("klic.bin", "wb");
			fwrite(test_klicensee, 0x10, 1, klic);
			fclose(klic);
		}
		else
			printf("Failed to bruteforce klic!\n");

		// Cleanup.
		fclose(input);
		fclose(source);
		return 0;
	}
	else
	{
		// Invalid mode.
		print_usage();
		return 0;
	}
}

// Added for Onyx access
// (Also avoids main's segfault in case of missing input file)
bool pack_data_from_paths
  ( const char* input_path
  , const char* output_path
  , const char* hash_file_name
  , unsigned char* content_id
  , unsigned char* devklic
  , unsigned char* rifkey
  , int version
  , int license
  , int type
  , int block
  , bool useCompression
  , bool isSDAT
  , bool verbose
  )
{
	FILE* input = fopen(input_path, "rb");
	FILE* output = fopen(output_path, "wb+");

	if (!input || !output)
	{
		if (input)  fclose(input);
		if (output) fclose(output);
		return true; // non-zero means error
	}

	bool result = pack_data(input, output, hash_file_name, content_id, devklic, rifkey, version, license, type, block, useCompression, isSDAT, verbose);

	fclose(input);
	fclose(output);
	return result;
}

