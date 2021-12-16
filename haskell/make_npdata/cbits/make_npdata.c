// Copyright (C) 2014       Hykem <hykem@hotmail.com>
// Licensed under the terms of the GNU GPL, version 3
// http://www.gnu.org/licenses/gpl-3.0.txt

#include "make_npdata.h"

#define fseeko64 fseeko
#define ftello64 ftello

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
	int mode = (int)(crypto_mode & 0xF0000000);
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
	int mode = (int)(hash_mode & 0xF0000000);
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
		return hmac_hash_compare(hash_final_14, 0x14, in, length, test_hash, 0x14);
	}
	else if ((hash_mode & 0xFF) == 0x02)  // 0x10 AES-CMAC
	{
		return cmac_hash_compare(hash_final_10, 0x10, in, length, test_hash, 0x10);
	}
	else if ((hash_mode & 0xFF) == 0x04) //0x10 SHA1-HMAC
	{
		return hmac_hash_compare(hash_final_10, 0x10, in, length, test_hash, 0x10);
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
	unsigned char *dec = (unsigned char *) malloc(0x10);
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
	unsigned char *dest_key = (unsigned char *) malloc(0x10);
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
	int block_num = (int)((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_section_size = ((edat->flags & EDAT_COMPRESSED_FLAG) != 0 || (edat->flags & EDAT_FLAG_0x20) != 0) ? 0x20 : 0x10;
	int metadata_offset = 0x100;

	unsigned char *enc_data;
	unsigned char *dec_data;
	unsigned char *b_key;
	unsigned char *iv;

	unsigned char hash[0x10];
	unsigned char key_result[0x10];
	unsigned char hash_result[0x14];
	memset(hash, 0, 0x10);
	memset(key_result, 0, 0x10);
	memset(hash_result, 0, 0x14);

	unsigned long long offset = 0;
	unsigned long long metadata_sec_offset = 0;
	int length = 0;
	int compression_end = 0;
	unsigned char empty_iv[0x10] = {};

	// Decrypt the metadata.
	int i;
	for (i = 0; i < block_num; i++)
	{
		memset(hash_result, 0, 0x14);

		if ((edat->flags & EDAT_COMPRESSED_FLAG) != 0)
		{
			metadata_sec_offset = metadata_offset + (unsigned long long) i * metadata_section_size;
			fseeko64(in, metadata_sec_offset, SEEK_SET);

			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);
			fread(metadata, 0x20, 1, in);

			// If the data is compressed, decrypt the metadata.
			// NOTE: For NPD version 1 the metadata is not encrypted.
			if (npd->version <= 1)
			{
				offset = se64(*(unsigned long long*)&metadata[0x10]);
				length = se32(*(int*)&metadata[0x18]);
				compression_end = se32(*(int*)&metadata[0x1C]);
			}
			else
			{
				unsigned char *result = dec_section(metadata);
				offset = se64(*(unsigned long long*)&result[0]);
				length = se32(*(int*)&result[8]);
				compression_end = se32(*(int*)&result[12]);
				free(result);
			}

			memcpy(hash_result, metadata, 0x10);
		}
		else if ((edat->flags & EDAT_FLAG_0x20) != 0)
		{
			// If FLAG 0x20, the metadata precedes each data block.
			metadata_sec_offset = metadata_offset + (unsigned long long) i * (metadata_section_size + length);
			fseeko64(in, metadata_sec_offset, SEEK_SET);

			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);
			fread(metadata, 0x20, 1, in);
			memcpy(hash_result, metadata, 0x14);

			// If FLAG 0x20 is set, apply custom xor.
			int j;
			for (j = 0; j < 0x10; j++)
				hash_result[j] = (unsigned char)(metadata[j] ^ metadata[j + 0x10]);

			offset = metadata_sec_offset + 0x20;
			length = edat->block_size;

			if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
				length = (int)(edat->file_size % edat->block_size);
		}
		else
		{
			metadata_sec_offset = metadata_offset + (unsigned long long) i * metadata_section_size;
			fseeko64(in, metadata_sec_offset, SEEK_SET);

			fread(hash_result, 0x10, 1, in);
			offset = metadata_offset + (unsigned long long) i * edat->block_size + (unsigned long long) block_num * metadata_section_size;
			length = edat->block_size;

			if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
				length = (int)(edat->file_size % edat->block_size);
		}

		// Locate the real data.
		int pad_length = length;
		length = (int)((pad_length + 0xF) & 0xFFFFFFF0);

		// Setup buffers for decryption and read the data.
		enc_data = (unsigned char *) malloc(length);
		dec_data = (unsigned char *) malloc(length);
		memset(enc_data, 0, length);
		memset(dec_data, 0, length);
		memset(hash, 0, 0x10);
		memset(key_result, 0, 0x10);

		fseeko64(in, offset, SEEK_SET);
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
					printf("WARNING: Block at offset 0x%llx has invalid hash!\n", offset);
					
				return 1;
			}
		}

		// Apply additional compression if needed and write the decrypted data.
		if (((edat->flags & EDAT_COMPRESSED_FLAG) != 0) && compression_end)
		{
			int decomp_size = (int)edat->file_size;
			unsigned char *decomp_data = (unsigned char *) malloc(decomp_size);
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
	fseeko64(f, 0, SEEK_SET);
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
	fseeko64(f, 0x90, SEEK_SET);
	fread(metadata_hash, 0x10, 1, f);
	fread(header_hash, 0x10, 1, f);

	// Setup the hashing mode and the crypto mode used in the file.
	int crypto_mode = 0x1;
	int hash_mode = ((edat->flags & EDAT_ENCRYPTED_KEY_FLAG) == 0) ? 0x00000002 : 0x10000002;
	if ((edat->flags & EDAT_DEBUG_DATA_FLAG) != 0)
	{
		hash_mode |= 0x01000000;

		if (verbose)
			printf("DEBUG data detected!\n");
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

		// If the header hash test fails and the data is not DEBUG, then RAP/RIF/KLIC key is invalid.
		if ((edat->flags & EDAT_DEBUG_DATA_FLAG) != EDAT_DEBUG_DATA_FLAG)
		{
			printf("ERROR: RAP/RIF/KLIC key is invalid!\n");
			return 1;
		}
	}

	// Parse the metadata info.
	int metadata_section_size = ((edat->flags & EDAT_COMPRESSED_FLAG) != 0 || (edat->flags & EDAT_FLAG_0x20) != 0) ? 0x20 : 0x10;
	if (((edat->flags & EDAT_COMPRESSED_FLAG) != 0))
	{
		if (verbose)
			printf("COMPRESSED data detected!\n");
	}

	int block_num = (int)((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_offset = 0x100;
	int metadata_size = metadata_section_size * block_num;
	long long metadata_section_offset = metadata_offset;

	long bytes_read = 0;
	long bytes_to_read = metadata_size;
	unsigned char *metadata = (unsigned char *) malloc(metadata_size);
	unsigned char *empty_metadata = (unsigned char *) malloc(metadata_size);

	while (bytes_to_read > 0)
	{
		// Locate the metadata blocks.
		fseeko64(f, metadata_section_offset, SEEK_SET);

		// Read in the metadata.
		fread(metadata + bytes_read, metadata_section_size, 1, f);

		// Adjust sizes.
		bytes_read += metadata_section_size;
		bytes_to_read -= metadata_section_size;

		if (((edat->flags & EDAT_FLAG_0x20) != 0)) // Metadata block before each data block.
			metadata_section_offset += (metadata_section_size + edat->block_size);
		else
			metadata_section_offset += metadata_section_size;
	}

	// Test the metadata section hash (located at offset 0x90).
	if (!decrypt(hash_mode, crypto_mode, (npd->version == 4), metadata, empty_metadata, metadata_size, header_key, header_iv, key, metadata_hash))
	{
		if (verbose)
			printf("WARNING: Metadata section hash is invalid!\n");
	}

	// Checking ECDSA signatures.
	if ((edat->flags & EDAT_DEBUG_DATA_FLAG) == 0)
	{
		printf("Checking signatures...\n");

		// Setup buffers.
		unsigned char metadata_signature[0x28];
		unsigned char header_signature[0x28];
		unsigned char signature_hash[20];
		unsigned char signature_r[0x15];
		unsigned char signature_s[0x15];
		unsigned char zero_buf[0x15];
		memset(metadata_signature, 0, 0x28);
		memset(header_signature, 0, 0x28);
		memset(signature_hash, 0, 20);
		memset(signature_r, 0, 0x15);
		memset(signature_s, 0, 0x15);
		memset(zero_buf, 0, 0x15);

		// Setup ECDSA curve and public key.
		ecdsa_set_curve(VSH_CURVE_P, VSH_CURVE_A, VSH_CURVE_B, VSH_CURVE_N, VSH_CURVE_GX, VSH_CURVE_GY);
		ecdsa_set_pub(VSH_PUB);


		// Read in the metadata and header signatures.
		fseeko64(f, 0xB0, SEEK_SET);
		fread(metadata_signature, 0x28, 1, f);
		fseeko64(f, 0xD8, SEEK_SET);
		fread(header_signature, 0x28, 1, f);

		// Checking metadata signature.
		// Setup signature r and s.
		memcpy(signature_r + 01, metadata_signature, 0x14);
		memcpy(signature_s + 01, metadata_signature + 0x14, 0x14);
		if ((!memcmp(signature_r, zero_buf, 0x15)) || (!memcmp(signature_s, zero_buf, 0x15)))
			printf("Metadata signature is invalid!\n");
		else
		{
			// Setup signature hash.
			if ((edat->flags & EDAT_FLAG_0x20) != 0) //Sony failed again, they used buffer from 0x100 with half size of real metadata.
			{
				int metadata_buf_size = block_num * 0x10;
				unsigned char *metadata_buf = (unsigned char *) malloc(metadata_buf_size);
				fseeko64(f, metadata_offset, SEEK_SET);
				fread(metadata_buf, metadata_buf_size, 1, f);
				sha1(metadata_buf, metadata_buf_size, signature_hash);
				free(metadata_buf);
			}
			else
				sha1(metadata, metadata_size, signature_hash);

			if (!ecdsa_verify(signature_hash, signature_r, signature_s))
			{
				printf("Metadata signature is invalid!\n");
				if (((unsigned long long)edat->block_size * block_num) > 0x100000000)
					printf("*Due to large file size, metadata signature status may be incorrect!\n");
			}
			else
				printf("Metadata signature is valid!\n");
		}


		// Checking header signature.
		// Setup header signature r and s.
		memset(signature_r, 0, 0x15);
		memset(signature_s, 0, 0x15);
		memcpy(signature_r + 01, header_signature, 0x14);
		memcpy(signature_s + 01, header_signature + 0x14, 0x14);

		if ((!memcmp(signature_r, zero_buf, 0x15)) || (!memcmp(signature_s, zero_buf, 0x15)))
			printf("Header signature is invalid!\n");
		else
		{
			// Setup header signature hash.
			memset(signature_hash, 0, 20);
			unsigned char *header_buf = (unsigned char *) malloc(0xD8);
			fseeko64(f, 0x00, SEEK_SET);
			fread(header_buf, 0xD8, 1, f);
			sha1(header_buf, 0xD8, signature_hash );
			free(header_buf);

			if (ecdsa_verify(signature_hash, signature_r, signature_s))
				printf("Header signature is valid!\n");
			else
				printf("Header signature is invalid!\n");
		}
	}

	// Cleanup.
	free(metadata);
	free(empty_metadata);

	return 0;
}

int validate_npd_hashes(const char* file_name, unsigned char *klicensee, NPD_HEADER *npd, bool verbose)
{
	int title_hash_result = 0;
	int dev_hash_result = 0;

	int file_name_length = strlen(file_name);
	unsigned char *buf = (unsigned char *) malloc(0x30 + file_name_length);
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
	title_hash_result = cmac_hash_compare(NPDRM_OMAC_KEY_3, 0x10, buf, 0x30 + file_name_length, npd->title_hash, 0x10);

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

		// Allow empty dev hash.
		dev_hash_result = 1;
	}
	else
	{
		// Generate klicensee xor key.
		xor(key, klicensee, NPDRM_OMAC_KEY_2, 0x10);

		// Hash with generated key and compare with dev_hash.
		dev_hash_result = cmac_hash_compare(key, 0x10, dev, 0x60, npd->dev_hash, 0x10);

		if (verbose)
		{
			if (dev_hash_result)
				printf("NPD dev hash is valid!\n");
			else
				printf("WARNING: NPD dev hash is invalid!\n");
		}
	}

	free(buf);

	return (title_hash_result && dev_hash_result);
}

bool extract_data(FILE *input, FILE *output, const char* input_file_name, unsigned char* devklic, unsigned char* rifkey, bool verbose)
{
	// Setup NPD and EDAT/SDAT structs.
	NPD_HEADER *NPD = (NPD_HEADER *) malloc(sizeof(NPD_HEADER));
	EDAT_HEADER *EDAT = (EDAT_HEADER *) malloc(sizeof(EDAT_HEADER));

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
	if (memcmp(NPD->magic, npd_magic, 4))
	{
		printf("ERROR: File has invalid NPD header.");
		return 1;
	}

	EDAT->flags = se32(*(int*)&edat_header[0]);
	EDAT->block_size = se32(*(int*)&edat_header[4]);
	EDAT->file_size = se64(*(u64*)&edat_header[8]);

	printf("NPD file: %s\n", input_file_name);
	printf("NPD version: %d\n", NPD->version);
	printf("NPD license: %d\n", NPD->license);
	printf("NPD type: %x\n", NPD->type);
    if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
	printf("\n");
	}
	else
	{
	printf("NPD content ID: %s\n\n", NPD->content_id);
	}
	
	// Set decryption key.
	unsigned char key[0x10];
	memset(key, 0, 0x10);

	// Check EDAT/SDAT flag.
	if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
		printf("SDAT HEADER\n");
		printf("SDAT flags: 0x%08X\n", EDAT->flags);
		printf("SDAT block size: 0x%08X\n", EDAT->block_size);
		printf("SDAT file size: 0x%llX\n", EDAT->file_size);
		printf("\n");

		// Generate SDAT key.
		xor(key, NPD->dev_hash, SDAT_KEY, 0x10);
	}
	else
	{
		printf("EDAT HEADER\n");
		printf("EDAT flags: 0x%08X\n", EDAT->flags);
		printf("EDAT block size: 0x%08X\n", EDAT->block_size);
		printf("EDAT file size: 0x%llX\n", EDAT->file_size);
		printf("\n");

		// Perform header validation (EDAT only).
		char real_file_name[MAX_PATH];
		extract_file_name(input_file_name, real_file_name);
		if (!validate_npd_hashes(real_file_name, devklic, NPD, verbose))
		{
			// Ignore header validation in DEBUG data.
			if ((EDAT->flags & EDAT_DEBUG_DATA_FLAG) != EDAT_DEBUG_DATA_FLAG)
			{
				printf("ERROR: NPD hash validation failed!\n");
				return 1;
			}
		}

		// Select EDAT key.
		if ((NPD->license & 0x3) == 0x3)           // Type 3: Use supplied devklic.
			memcpy(key, devklic, 0x10);
		else if (((NPD->license & 0x2) == 0x2)     // Type 2: Use key from RAP file (RIF key).
			|| ((NPD->license & 0x1) == 0x1))      // Type 1: Use network activation (RIF key).
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
				printf("ERROR: A valid RAP/RIF file is needed for this EDAT file!");
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
		for (i = 0; i < 0x10; i++)
			printf("%02X", key[i]);
		printf("\n\n");
	}

	printf("Parsing data...\n");
	if (check_data(key, EDAT, NPD, input, verbose))
	{
		printf("Parsing failed!\n");
		return 1;
	}
	else
		printf("File successfully parsed!\n");

	printf("\n");

	printf("Decrypting data...\n");
	if (decrypt_data(input, output, EDAT, NPD, key, verbose))
	{
		printf("Decryption failed!");
		return 1;
	}
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
	long long offset = 0;
	int length = 0;
	int compression_end = 0;
	unsigned char empty_iv[0x10] = {};

	// Build special data footers for each version.
	unsigned char edat_footer_v1[0x10] = {0x45,0x44,0x41,0x54,0x41,0x20,0x70,0x61,0x63,0x6B,0x61,0x67,0x65,0x72,0x00,0x00};
	unsigned char edat_footer_v2[0x10] = {0x45,0x44,0x41,0x54,0x41,0x20,0x32,0x2E,0x34,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};
	unsigned char edat_footer_v3[0x10] = {0x45,0x44,0x41,0x54,0x41,0x20,0x33,0x2E,0x33,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};
	unsigned char edat_footer_v4[0x10] = {0x45,0x44,0x41,0x54,0x41,0x20,0x34,0x2E,0x30,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};
	unsigned char sdat_footer_v1[0x10] = {0x53,0x44,0x41,0x54,0x41,0x20,0x70,0x61,0x63,0x6B,0x61,0x67,0x65,0x72,0x00,0x00};
	unsigned char sdat_footer_v2[0x10] = {0x53,0x44,0x41,0x54,0x41,0x20,0x32,0x2E,0x34,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};
	unsigned char sdat_footer_v3[0x10] = {0x53,0x44,0x41,0x54,0x41,0x20,0x33,0x2E,0x33,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};
	unsigned char sdat_footer_v4[0x10] = {0x53,0x44,0x41,0x54,0x41,0x20,0x34,0x2E,0x30,0x2E,0x30,0x2E,0x57,0x00,0x00,0x00};

	// Encrypt the data and generate the metadata.
	int i;
	for (i = 0; i < block_num; i++)
	{
		memset(hash_result, 0, 0x14);

		offset = (unsigned long long)i * edat->block_size;
		length = edat->block_size;

		if ((i == (block_num - 1)) && (edat->file_size % edat->block_size))
			length = (int)(edat->file_size % edat->block_size);

		// Locate the real data.
		int pad_length = length;
		length = (int)((pad_length + 0xF) & 0xFFFFFFF0);
		fseeko64(in, offset, SEEK_SET);

		// Setup buffers for encryption and read the data.
		enc_data = (unsigned char *) malloc(length);
		dec_data = (unsigned char *) malloc(length);
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

		if ((edat->flags & EDAT_FLAG_0x10) == 0)
			hash_mode = 0x02;
		else if ((edat->flags & EDAT_FLAG_0x20) == 0)
			hash_mode = 0x04;
		else
			hash_mode = 0x01;

		if ((edat->flags & EDAT_ENCRYPTED_KEY_FLAG) != 0)
		{
			crypto_mode |= 0x10000000;
			hash_mode |= 0x10000000;
		}

		if ((edat->flags & EDAT_DEBUG_DATA_FLAG) != 0)
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
			if (!encrypt(hash_mode, crypto_mode, (npd->version == 4), dec_data, enc_data, length, key_result, iv, hash, hash_result))
			{
				if (verbose)
					printf("WARNING: Block at offset 0x%08x got invalid forged hash!\n", offset);
			}
		}

		// Write the metadata and the encrypted data.
		if ((edat->flags & EDAT_COMPRESSED_FLAG) != 0)
		{
			unsigned long long data_offset = metadata_offset + (unsigned long long) i * edat->block_size + (unsigned long long) block_num * 0x20;

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
			// NOTE: For NPD version 1 the metadata is not encrypted.
			if (npd->version <= 1)
			{
				memcpy(enc_metadata, dec_metadata, 0x20);
			}
			else
			{
				memcpy(enc_metadata, dec_metadata, 0x10);
				memcpy(enc_metadata + 0x10, dec_section(dec_metadata), 0x10);
			}

			// Write the encrypted metadata if DEBUG flag is not set.
			if ((edat->flags & EDAT_DEBUG_DATA_FLAG) == 0)
			{
				fseeko64(out, metadata_offset + (unsigned long long) i * 0x20, SEEK_SET);
				fwrite(enc_metadata, 0x20, 1, out);
			}

			// Write the encrypted data.
			fseeko64(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}
		else if ((edat->flags & EDAT_FLAG_0x20) != 0)
		{
			unsigned long long data_offset = metadata_offset + (unsigned long long) i * edat->block_size + ((unsigned long long) i + 1) * 0x20;

			// If FLAG 0x20 is set, apply custom xor.
			unsigned char metadata[0x20];
			memset(metadata, 0, 0x20);

			// Use a fake XOR value and build the metadata.
			unsigned char hash_result_1[0x10];
			memset(hash_result_1, 0, 0x10);
			unsigned char hash_result_2[0x10];
			memset(hash_result_2, 0, 0x10);
			prng(hash_result_2, 0x10);
			memcpy(hash_result_2, hash_result + 0x10, 0x04);

			int j;
			for (j = 0; j < 0x10; j++)
				hash_result_1[j] = (unsigned char) (hash_result[j] ^ hash_result_2[j]);

			// Set the metadata.
			memcpy(metadata, hash_result_1, 0x10);
			memcpy(metadata + 0x10, hash_result_2, 0x10);

			// Write the encrypted metadata if DEBUG flag is not set.
			if ((edat->flags & EDAT_DEBUG_DATA_FLAG) == 0)
			{
				fseeko64(out, metadata_offset + (unsigned long long) i * 0x20 + offset, SEEK_SET);
				fwrite(metadata, 0x20, 1, out);
			}

			// Write the encrypted data.
			fseeko64(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}
		else
		{
			unsigned long long data_offset = metadata_offset + (unsigned long long) i * edat->block_size + (unsigned long long) block_num * 0x10;

			// Write the encrypted metadata if DEBUG flag is not set.
			if ((edat->flags & EDAT_DEBUG_DATA_FLAG) == 0)
			{
				fseeko64(out, metadata_offset + (unsigned long long) i * 0x10, SEEK_SET);
				fwrite(hash_result, 0x10, 1, out);
			}

			// Write the encrypted data.
			fseeko64(out, data_offset, SEEK_SET);
			fwrite(enc_data, length, 1, out);
		}

		free(enc_data);
		free(dec_data);
	}

	// Before appending the footer, if the file is empty, seek to the metadata offset.
	if (edat->file_size == 0)
		fseeko64(out, metadata_offset, SEEK_SET);

	// Append the special version footer.
	if ((npd->version == 0) || (npd->version == 1))
	{
		if ((edat->flags & SDAT_FLAG) == SDAT_FLAG)
			fwrite(sdat_footer_v1, 0x10, 1, out);
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
	unsigned char metadata_signature[0x28] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
	unsigned char header_signature[0x28] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};
	memset(header, 0, 0xA0);
	memset(empty_header, 0, 0xA0);
	memset(header_hash, 0, 0x10);
	memset(metadata_hash, 0, 0x10);
	memset(metadata_signature, 0, 0x28);
	memset(header_signature, 0, 0x28);

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
	int block_num = (int)((edat->file_size + edat->block_size - 1) / edat->block_size);
	int metadata_offset = 0x100;
	int metadata_size = metadata_section_size * block_num;
	long long metadata_section_offset = metadata_offset;

	long bytes_read = 0;
	long bytes_to_read = metadata_size;
	unsigned char *metadata = (unsigned char *) malloc(metadata_size);
	unsigned char *empty_metadata = (unsigned char *) malloc(metadata_size);

	while (bytes_to_read > 0)
	{
		// Locate the metadata blocks.
		fseeko64(f, metadata_section_offset, SEEK_SET);

		// Read in the metadata.
		fread(metadata + bytes_read, metadata_section_size, 1, f);

		// Adjust sizes.
		bytes_read += metadata_section_size;
		bytes_to_read -= metadata_section_size;

		if (((edat->flags & EDAT_FLAG_0x20) != 0)) // Metadata block before each data block.
			metadata_section_offset += (metadata_section_size + edat->block_size);
		else
			metadata_section_offset += metadata_section_size;
	}

	// Generate the metadata section hash (located at offset 0x90).
	encrypt(hash_mode, crypto_mode, (npd->version == 4), metadata, empty_metadata, metadata_size, header_key, header_iv, key, metadata_hash);

	// Write back the forged metadata section hash.
	fseeko64(f, 0x90, SEEK_SET);
	fwrite(metadata_hash, 0x10, 1, f);

	// Read in the file header.
	fseeko64(f, 0, SEEK_SET);
	fread(header, 0xA0, 1, f);

	// Generate the header hash (located at offset 0xA0).
	encrypt(hash_mode, crypto_mode, (npd->version == 4), header, empty_header, 0xA0, header_key, header_iv, key, header_hash);

	// Write back the forged header section hash.
	fseeko64(f, 0xA0, SEEK_SET);
	fwrite(header_hash, 0x10, 1, f);

	// ECDSA metadata signature (fill with random data for now).
	fseeko64(f, 0xB0, SEEK_SET);
	fwrite(metadata_signature, 0x28, 1, f);

	// ECDSA header signature (fill with random data for now).
	fseeko64(f, 0xD8, SEEK_SET);
	fwrite(header_signature, 0x28, 1, f);

	// Cleanup.
	free(metadata);
	free(empty_metadata);

	return 0;
}

void forge_npd_title_hash(const char* file_name, NPD_HEADER *npd)
{
	int file_name_length = strlen(file_name);
	unsigned char *buf = (unsigned char *) malloc(0x30 + file_name_length);
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

bool pack_data(FILE *input, FILE *output, const char* input_file_name, unsigned char* content_id, unsigned char* devklic, unsigned char* rifkey, int version, int license, int type, int block, bool useCompression, bool isEDAT, bool isFinalized, bool verbose)
{
	// Get file size.
	fseeko64(input, 0, SEEK_END);
	long long input_file_size = ftello64(input);
	fseeko64(input, 0, SEEK_SET);

	// Setup NPD and EDAT/SDAT structs.
	NPD_HEADER *NPD = (NPD_HEADER *) malloc(sizeof(NPD_HEADER));
	EDAT_HEADER *EDAT = (EDAT_HEADER *) malloc(sizeof(EDAT_HEADER));

	// Forge NPD header.
	unsigned char npd_magic[4] = {0x4E, 0x50, 0x44, 0x00};  //NPD0
	unsigned char fake_digest[0x10] = {0x47,0x6F,0x6F,0x64,0x4C,0x75,0x63,0x6B,0x46,0x72,0x6F,0x6D,0x50,0x53,0x50,0x78};
	memcpy(NPD->magic, npd_magic, 4);
	memcpy(NPD->digest, fake_digest, 0x10);
	NPD->version = ((version == 1) && (!isFinalized)) ? 0 : version;

	// Check for debug or finalized data.
	if (isFinalized)
	{
		NPD->license = license;
		NPD->type = type;
		memcpy(NPD->content_id, content_id, 0x30);
		memcpy(NPD->digest, fake_digest, 0x10);
		char real_file_name[MAX_PATH];
		extract_file_name(input_file_name, real_file_name);
		forge_npd_title_hash(real_file_name, NPD);
		forge_npd_dev_hash(devklic, NPD);
		NPD->unk1 = 0;
		NPD->unk2 = 0;
	}

	// Forge EDAT/SDAT header with fixed values for flags and block size.
	if (version == 1)
	{
		// Version 1 is only valid for EDAT files.
		if (isEDAT)
		{
			EDAT->flags = 0x00;
			if (useCompression) EDAT->flags |= EDAT_COMPRESSED_FLAG;
			if (!isFinalized) EDAT->flags |= EDAT_DEBUG_DATA_FLAG;
		}
		else
		{
			printf("ERROR: Invalid version for SDAT!");
			return 1;
		}
	}
	else if (version == 2)
	{
		EDAT->flags = 0x0C;
		if (useCompression) EDAT->flags |= EDAT_COMPRESSED_FLAG;
		if (!isEDAT) EDAT->flags |= SDAT_FLAG;
		if (!isFinalized) EDAT->flags |= EDAT_DEBUG_DATA_FLAG;
	}
	else
	{
		// Version 3 and 4 use 0x3C and 0x0D flags (there is no 0x3D flag).
		EDAT->flags = 0x3C;
		if (useCompression) EDAT->flags = (0x0C | EDAT_COMPRESSED_FLAG);
		if (!isEDAT) EDAT->flags |= SDAT_FLAG;
		if (!isFinalized) EDAT->flags |= EDAT_DEBUG_DATA_FLAG;
	}

	EDAT->block_size = block * 1024;
	EDAT->file_size = input_file_size;

	printf("NPD file: %s\n", input_file_name);
	printf("NPD version: %d\n", NPD->version);
	printf("NPD license: %d\n", NPD->license);
	printf("NPD type: %x\n", NPD->type);
	if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
	printf("\n");
	}
	else
	{
	printf("NPD content ID: %s\n\n", NPD->content_id);
	}

	// Set encryption key.
	unsigned char key[0x10];
	memset(key, 0, 0x10);

	// Check EDAT/SDAT flag.
	if ((EDAT->flags & SDAT_FLAG) == SDAT_FLAG)
	{
		printf("SDAT HEADER\n");
		printf("SDAT flags: 0x%08X\n", EDAT->flags);
		printf("SDAT block size: 0x%08X\n", EDAT->block_size);
		printf("SDAT file size: 0x%llX\n", EDAT->file_size);
		printf("\n");

		// Generate SDAT key.
		xor(key, NPD->dev_hash, SDAT_KEY, 0x10);
	}
	else
	{
		printf("EDAT HEADER\n");
		printf("EDAT flags: 0x%08X\n", EDAT->flags);
		printf("EDAT block size: 0x%08X\n", EDAT->block_size);
		printf("EDAT file size: 0x%llX\n", EDAT->file_size);
		printf("\n");

		// Select EDAT key.
		if ((NPD->license & 0x3) == 0x3)        // Type 3: Use supplied devklic.
			memcpy(key, devklic, 0x10);
		else if ((NPD->license & 0x2) == 0x2)   // Type 2: Use key from RAP file (RIF key).
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
				printf("ERROR: A valid RAP/RIF file is needed for this EDAT file!");
				return 1;
			}
		}
		else if ((NPD->license & 0x1) == 0x1)    // Type 1: Use network activation.
		{
			printf("ERROR: Network license not supported!");
			return 1;
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
		for (i = 0; i < 0x10; i++)
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
	{
		printf("Encryption failed!\n");
		return 1;
	}
	else
		printf("File successfully encrypted!\n");

	// Only forge finalized data.
	if (isFinalized)
	{
		printf("Forging data...\n");
		if (forge_data(key, EDAT, NPD, output))
		{
			printf("Forging failed!\n");
			return 1;
		}
		else
			printf("File successfully forged!\n\n");
	}

	free(NPD);
	free(EDAT);

	return 0;
}

void print_usage()
{
	printf("***************************************************************************\n\n");
	printf("make_npdata v1.3.4 - PS3 EDAT/SDAT file encrypter/decrypter/bruteforcer.\n");
	printf("                   - Written by Hykem (C).\n\n");
	printf("***************************************************************************\n\n");
	printf("Usage: make_npdata [-v] -e <input> <output> <format> <data> <version>\n");
	printf("                           <compress> <block> <license> <type> <cID>\n");
	printf("                           <klic> <rap/rif>\n");
	printf("       make_npdata [-v] -d <input> <output> <klic> <rap/rif>\n");
	printf("       make_npdata [-v] -b <input> <source> <mode>\n\n");
	printf("- Modes:\n");
	printf("[-v]: Verbose mode\n");
	printf("[-e]: Encryption mode\n");
	printf("[-d]: Decryption mode\n");
	printf("[-b]: Bruteforce mode\n");
	printf("\n");
	printf("- Encryption mode only:\n");
	printf("<format>:   0 - SDAT\n");
	printf("            1 - EDAT\n");
	printf("<data>:     0 - Debug data\n");
	printf("            1 - Finalized data\n");
	printf("<version>:  1 - EDAT version 1\n");
	printf("            2 - EDAT/SDAT version 2\n");
	printf("            3 - EDAT/SDAT version 3\n");
	printf("            4 - EDAT/SDAT version 4\n");
	printf("<compress>: 0 - Disable compression\n");
	printf("            1 - Enable compression\n");
	printf("<block>:    Block size in KB (1, 2, 4, 8, 16, 32)\n");
	printf("\n");
	printf("- Finalized EDAT only:\n");
	printf("<license>:  1 - Network license (not supported)\n");
	printf("            2 - Local license (uses RAP file as key)\n");
	printf("            3 - Free license (uses klic as key)\n");
	printf("<type>:     00 - Common\n");
	printf("            01 - PS2 EDAT and Theme/Avatar/Activation key\n");
	printf("            20 - PSP Remasters (disc bind)\n");
	printf("            21 - Modules (disc bind)\n");
	printf("            30 - Unknown\n");
	printf("<cID>:      Content ID (XXYYYY-AAAABBBBB_CC-DDDDDDDDDDDDDDDD)\n");
	printf("\n");
	printf("- Encryption and decryption modes:\n");
	printf("<klic>: 0 - No key\n");
	printf("        1 - NPDRM OMAC key 1 (free license key)\n");
	printf("        2 - NPDRM OMAC key 2\n");
	printf("        3 - NPDRM OMAC key 3\n");
	printf("        4 - PS3 key (klic_dec_key)\n");
	printf("        5 - PSX key (PSOne Classics)\n");
	printf("        6 - PSP key 1 (PSP Minis)\n");
	printf("        7 - PSP key 2 (PSP Remasters)\n");
	printf("        8 - Custom key (read from input or klic.bin file)\n");
	printf("<rap/rif>: RAP file for encryption/decryption or rifkey.bin (optional)\n");
	printf("\n");
	printf("- Bruteforce mode:\n");
	printf("<source>: ELF file source for klic\n");
	printf("<mode>: 0 - Binary\n");
	printf("        1 - Text\n");
	printf("        2 - Unicode text\n");
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
	if ((!strcmp(argv[arg_offset + 1], "-e")) && (argc > (arg_offset + 7)))
	{
		// Skip the mode argument.
		arg_offset++;

		// Get the input and output files.
		const char *input_file_name = argv[arg_offset + 1];
		const char *output_file_name = argv[arg_offset + 2];
		FILE* input = fopen(input_file_name, "rb");

		// Check input file.
		if (input == NULL)
		{
			printf("ERROR: Please check your input file!\n");
			fclose(input);
			return 0;
		}

		// Read all the necessary parameters for encryption.
		int format = atoi(argv[arg_offset + 3]);
		int data = atoi(argv[arg_offset + 4]);
		int version = atoi(argv[arg_offset + 5]);
		int compression = atoi(argv[arg_offset + 6]);
		int block = atoi(argv[arg_offset + 7]);

		// Check for invalid parameters.
		if (((format > 1) || (format < 0))
			|| ((data > 1) || (data < 0))
			|| ((version > 4) || (version < 1))
			|| ((compression > 1) || (compression < 0))
			|| (((block != 1) && (block != 2) && (block != 4) 
				&& (block != 8) && (block != 16) && (block != 32))))
		{
			printf("ERROR: Invalid parameters!\n");
			fclose(input);
			return 0;
		}

		// Optional parameters.
		int license = 0;
		int type = 0;
		char cID[0x30];
		memset(cID, 0, 0x30);

		// Set emtpy key and klic.
		unsigned char rifkey[0x10];
		unsigned char devklic[0x10];
		memset(rifkey, 0, 0x10);
		memset(devklic, 0, 0x10);

		// Finalized EDAT mode only.
		if (format && data)
		{
			// Check for additional EDAT parameters.
			if (argc < (arg_offset + 12))
			{
				printf("ERROR: Not enough parameters for finalized EDAT!\n");
				fclose(input);
				return 0;
			}

			license = atoi(argv[arg_offset + 8]);
			type = strtol(argv[arg_offset + 9], NULL, 16);
			strcpy(cID, argv[arg_offset + 10]);

			if (((license > 3) || (license < 1)) || (cID == NULL))
			{
				printf("ERROR: Invalid finalized EDAT parameters!\n");
				fclose(input);
				return 0;
			}

			// Select the EDAT key mode.
			int edat_mode = atoi(argv[arg_offset + 11]);

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
				// Read from console input.
				if (argv[arg_offset + 12] != NULL)
				{
					if (is_hex(argv[arg_offset + 12], 0x20))
					{
						unsigned char custom_klic[0x10];
						hex_to_bytes(custom_klic, argv[arg_offset + 12], 0x20);
						memcpy(devklic, custom_klic, 0x10);
						arg_offset++;

						break;
					}
				}

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
					fclose(input);
					fclose(klic_file);
					return 0;
				}

				break;
			}
			default:
				printf("ERROR: Invalid klic mode!\n");
				fclose(input);
				return 0;
			}

			// Read the RAP file, if provided.
			if (argv[arg_offset + 12] != NULL)
			{
				FILE* rap = fopen(argv[arg_offset + 12], "rb");
				unsigned char rapkey[0x10];
				memset(rapkey, 0, 0x10);

				// Special file name to bypass conversion and read a RIF key directly.
				char real_rap_name[MAX_PATH];
				extract_file_name((argv[arg_offset + 12]), real_rap_name);
				if (!strcmp(real_rap_name, "rifkey.bin"))
				{
					if (rap == NULL)
					{
						printf("ERROR: Please place your binary RIF key in a rifkey.bin file!\n");
						fclose(input);
						return 0;
					}
					fread(rifkey, sizeof(rifkey), 1, rap);
				}
				else
				{
					if (rap == NULL)
					{
						printf("ERROR: Please place your binary RAP key in a rap file!\n");
						fclose(input);
						return 0;
					}
					fread(rapkey, sizeof(rapkey), 1, rap);
					get_rif_key(rapkey, rifkey);
				}

				fclose(rap);
			}
		}

		FILE* output = fopen(output_file_name, "wb+");

		// Delete the bad output file if any errors arise.
		if (pack_data(input, output, output_file_name, (unsigned char *)cID, devklic, rifkey, version, license, type, block, compression ? true : false, format ? true : false, data ? true : false, verbose))
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

		// Check input file.
		if (input == NULL)
		{
			printf("ERROR: Please check your input file!\n");
			return 0;
		}

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
			// Read from console input.
			if (argv[arg_offset + 4] != NULL)
			{
				if (is_hex(argv[arg_offset + 4], 0x20))
				{
					unsigned char custom_klic[0x10];
					hex_to_bytes(custom_klic, argv[arg_offset + 4], 0x20);
					memcpy(devklic, custom_klic, 0x10);
					arg_offset++;

					break;
				}
			}

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
				fclose(input);
				fclose(klic_file);
				return 0;
			}

			break;
		}
		default:
			printf("ERROR: Invalid klic mode!\n");
			fclose(input);
			return 0;
		}

		// Read the RAP file, if provided.
		if (argv[arg_offset + 4] != NULL)
		{
			FILE* rap = fopen(argv[arg_offset + 4], "rb");
			unsigned char rapkey[0x10];
			memset(rapkey, 0, 0x10);

			// Special file name to bypass conversion and read a RIF key directly.
			char real_rap_name[MAX_PATH];
			extract_file_name((argv[arg_offset + 4]), real_rap_name);
			if (!strcmp(real_rap_name, "rifkey.bin"))
			{
				if (rap == NULL)
				{
					printf("ERROR: Please place your binary RIF key in a rifkey.bin file!\n");
					fclose(input);
					return 0;
				}
				fread(rifkey, sizeof(rifkey), 1, rap);
			}
			else
			{
				if (rap == NULL)
				{
					printf("ERROR: Please place your binary RAP key in a rap file!\n");
					fclose(input);
					return 0;
				}
				fread(rapkey, sizeof(rapkey), 1, rap);
				get_rif_key(rapkey, rifkey);
			}

			fclose(rap);
		}

		FILE* output = fopen(output_file_name, "wb");

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
		
		// Check input and source files.
		if (input == NULL)
		{
			printf("ERROR: Please check your input file!\n");
			return 0;
		}

		if (source == NULL)
		{
			printf("ERROR: Please check your source file!\n");
			fclose(input);
			return 0;
		}
		
		// Read data as plain binary or as text.
		int mode = 0;
		if (argv[arg_offset + 3] != NULL)
		{
			mode = atoi(argv[arg_offset + 3]);
			if ((mode != 0) && (mode != 1) && (mode != 2))
			{
				printf("ERROR: Invalid parameters!\n");
				fclose(input);
				fclose(source);
				return 0;
			}
		}

		// Get the source file size.
		fseeko64(source, 0, SEEK_END);
		long long source_file_size = ftello64(source);
		fseeko64(source, 0, SEEK_SET);

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

		// Buffer to handle klicensee as unicode text.
		char test_klicensee_unicode_text[0x40];
		memset(test_klicensee_unicode_text, 0, 0x40);

		// Read the file's header magic and seek back.
		unsigned char magic[0x4];
		fread(magic, 0x4, 1, input);
		fseeko64(input, 0, SEEK_SET);

		// If header starts with SCE, the file is a SELF or SPRX.
		// If not, assume regular EDAT/SDAT (NPD).
		unsigned char sce_magic[4] = { 0x53, 0x43, 0x45, 0x00 };  //SCE0
		if (!memcmp(magic, sce_magic, 4))
		{
			// File is SCE, read the NPD dev_hash offset from the
			// first 0x10 bytes of the SCE header and seek to the NPD area.	
			unsigned char sce_header[0x10];
			fread(sce_header, 0x10, 1, input);
			short npd_offset = se16(*(short*)&sce_header[0xE]) - 0x60;
			fseeko64(input, npd_offset, SEEK_SET);

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
			if (mode == 1)
				printf("MODE: Text\n");
			else if (mode == 2)
				printf("MODE: Unicode text\n");
			else
				printf("MODE: Binary\n");

			int i;
			printf("DEV HASH: ");
			for (i = 0; i < 0x10; i++)
				printf("%02X", test_dev_hash[i]);
			printf("\n\n");
		}

		printf("Bruteforcing klic...\n");

		int i;
		bool found = false;
		for (i = 0; i < source_file_size; i++)
		{
			// Iterate the source file and generate klicensee xor key.
			fseeko64(source, i, SEEK_SET);

			// If reading in text mode, convert the hexadecimal string to binary data.
			if (mode == 1)
			{
				fread(test_klicensee_text, 0x20, 1, source);

				// If the string is not a valid hexadecimal string, continue the loop.
				if (!is_hex(test_klicensee_text, 0x20)) continue;
				hex_to_bytes(test_klicensee, test_klicensee_text, 0x20);
			}
			else if (mode == 2)
			{
				fread(test_klicensee_unicode_text, 0x40, 1, source);

				// Convert unicode fullwidth to plain text.
				int uni_count;
				int txt_count = 0;
				for (uni_count = 0; uni_count < 0x40; uni_count += 2)
					test_klicensee_text[txt_count++] = test_klicensee_unicode_text[uni_count + 1];

				// If the string is not a valid hexadecimal string, continue the loop.
				if (!is_hex(test_klicensee_text, 0x20)) continue;
				hex_to_bytes(test_klicensee, test_klicensee_text, 0x20);
			}
			else
				fread(test_klicensee, 0x10, 1, source);

			xor(test_key, test_klicensee, NPDRM_OMAC_KEY_2, 0x10);
			if (cmac_hash_compare(test_key, 0x10, npd_buf, 0x60, test_dev_hash, 0x10))
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
	, bool isEDAT
	, bool isFinalized
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

	bool result = pack_data(input, output, hash_file_name, content_id, devklic, rifkey, version, license, type, block, useCompression, isEDAT, isFinalized, verbose);

	fclose(input);
	fclose(output);
	return result;
}

bool extract_data_from_paths(const char *input_path, const char *output_path, const char* input_file_name, unsigned char* devklic, unsigned char* rifkey, bool verbose)
{
	FILE* input = fopen(input_path, "rb");
	FILE* output = fopen(output_path, "wb+");

	if (!input || !output)
	{
		if (input)  fclose(input);
		if (output) fclose(output);
		return true; // non-zero means error
	}

	bool result = extract_data(input, output, input_file_name, devklic, rifkey, verbose);

	fclose(input);
	fclose(output);
	return result;
}
