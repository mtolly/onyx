#ifndef OPS_H
#define OPS_H

#include "inttypes.h"

typedef uint8_t (*op_func)(uint8_t, uint8_t);

#ifdef __cplusplus
extern "C" {
#endif

extern op_func opsRB2[32];

extern op_func opsRB3[32];

#ifdef __cplusplus
}
#endif

#endif // OPS_H