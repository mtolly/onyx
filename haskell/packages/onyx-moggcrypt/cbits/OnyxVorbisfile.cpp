#include "vorbis/vorbisfile.h"
#include <cstdlib>

extern "C" {

int onyx_ov_open_callbacks
  ( void *datasource, OggVorbis_File *vf
  , const char *initial, long ibytes, ov_callbacks *callbacks
  )
{
  // just dereference the callbacks pointer
  return ov_open_callbacks(datasource, vf, initial, ibytes, *callbacks);
}

}
