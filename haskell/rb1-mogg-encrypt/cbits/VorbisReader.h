#pragma once

#ifndef _OV_FILE_H_
#include "XiphTypes.h"
#endif

#include <inttypes.h>

union aes_ctr_128 {
	uint64_t qwords[2];
	uint32_t dwords[4];
	uint16_t words[8];
	uint8_t bytes[16];
};

