#pragma once

#include <stdio.h>
#include <stdint.h>
typedef int64_t ogg_int64_t;
typedef struct {
	size_t(*read_func)  (void *ptr, size_t size, size_t nmemb, void *datasource);
	int(*seek_func)  (void *datasource, ogg_int64_t offset, int whence);
	int(*close_func) (void *datasource);
	long(*tell_func)  (void *datasource);
} ov_callbacks;
