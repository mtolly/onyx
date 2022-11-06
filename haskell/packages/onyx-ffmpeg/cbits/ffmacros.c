#include "libavutil/avutil.h"

int hs_AVERROR_EOF() {
  // this is a weird macro so we can't bind to it directly with c2hs
  return AVERROR_EOF;
}
