#include "XiphTypes.h"
#include "VorbisReader.h"
#include <cstdlib>

extern "C" {

ov_callbacks* onyx_new_ov_callbacks
  ( size_t(*read_func)  (void *ptr, size_t size, size_t nmemb, void *datasource)
  , int(*seek_func)  (void *datasource, ogg_int64_t offset, int whence)
  , int(*close_func) (void *datasource)
  , long(*tell_func)  (void *datasource)
  )
{
  ov_callbacks* cb = new ov_callbacks();
  cb->read_func  = read_func;
  cb->seek_func  = seek_func;
  cb->close_func = close_func;
  cb->tell_func  = tell_func;
  return cb;
}

void onyx_delete_ov_callbacks(ov_callbacks* cb) {
  delete cb;
}

void* onyx_VorbisReader_Open(void* src, ov_callbacks* cb) {
  VorbisReader* vr = new VorbisReader();
  if (vr->Open(src, *cb)) {
    delete vr;
    return nullptr;
  } else {
    return vr;
  }
}

void onyx_delete_VorbisReader(void* vr) {
  delete ((VorbisReader*) vr);
}

int onyx_VorbisReader_SeekRaw(void* vr, long long offset, int whence) {
  return ((VorbisReader*) vr)->SeekRaw(offset, whence);
}

size_t onyx_VorbisReader_TellRaw(void* vr) {
  return ((VorbisReader*) vr)->TellRaw();
}

size_t onyx_VorbisReader_SizeRaw(void* vr) {
  return ((VorbisReader*) vr)->SizeRaw();
}

size_t onyx_VorbisReader_ReadRaw(void* vr, void *buf, size_t elementSize, size_t elements) {
  return ((VorbisReader*) vr)->ReadRaw(buf, elementSize, elements);
}

}
