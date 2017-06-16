#include <stdint.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

int buildSTFSPackage
( wchar_t *packageName
, wchar_t *packageDescription
, wchar_t *publisherName
, wchar_t *titleName
, uint32_t titleID
, char **dirs
, int countDirs
, char **paths
, char **pathsInPackage
, int countFiles
, uint8_t *thumb
, size_t thumbLen
, uint8_t *thumbTitle
, size_t thumbTitleLen
, uint8_t *kvBin
, size_t kvBinLen
, char *stfsOut
, char *errOut
);

#ifdef __cplusplus
}
#endif
