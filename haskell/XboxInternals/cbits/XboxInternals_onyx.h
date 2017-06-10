#include <stdint.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

void buildSTFSPackage
( wchar_t *packageName
, wchar_t *packageDescription
, uint32_t titleID
, char **paths
, char **pathsInPackage
, int countFiles
, char *stfsOut
);

#ifdef __cplusplus
}
#endif
