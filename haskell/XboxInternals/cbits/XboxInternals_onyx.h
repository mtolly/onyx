#include <stdint.h>
#include <wchar.h>

#ifdef __cplusplus
extern "C" {
#endif

int buildSTFSPackage
( const wchar_t *packageName
, const wchar_t *packageDescription
, const wchar_t *publisherName
, const wchar_t *titleName
, uint32_t titleID
, const char **dirs
, int countDirs
, const char **paths
, const char **pathsInPackage
, int countFiles
, uint8_t *thumb
, size_t thumbLen
, uint8_t *thumbTitle
, size_t thumbTitleLen
, uint8_t *kvBin
, size_t kvBinLen
, const char *stfsOut
, char *errOut
);

#ifdef __cplusplus
}
#endif
