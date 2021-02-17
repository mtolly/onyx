#include <time.h>
#include <stdint.h>

void hs_getTimeMonotonic(int64_t *seconds, uint32_t *nanoseconds) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  *seconds = (int64_t) ts.tv_sec;
  *nanoseconds = (uint32_t) ts.tv_nsec;
}
