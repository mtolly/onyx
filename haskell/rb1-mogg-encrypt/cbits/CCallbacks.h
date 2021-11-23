#pragma once

#ifndef _OV_FILE_H_
#include "XiphTypes.h"
#endif

size_t mogg_read(void *ptr, size_t size, size_t nmemb, void *datasource) {
	return fread(ptr, size, nmemb, (FILE*)datasource);
}
int mogg_seek(void *datasource, ogg_int64_t offset, int whence) {
	return fseek((FILE*)datasource, offset, whence);
}
int mogg_close(void *datasource) {
	return fclose((FILE*)datasource);
}
long mogg_tell(void *datasource) {
	return ftell((FILE*)datasource);
}

ov_callbacks cCallbacks = {
	mogg_read,
	mogg_seek,
	mogg_close,
	mogg_tell
};
