// C interface to ArkHdrPair so Onyx can bind to it.

#include "stdbool.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef void* ArkTool;
ArkTool ark_new();
void ark_delete(ArkTool p);

typedef void* ArkFileIterator;
ArkFileIterator ark_new_iterator();
void ark_delete_iterator(ArkFileIterator p);

typedef void* ArkFileEntry;

bool ark_Open(ArkTool p, const char* arkDirpath);
bool ark_Save(ArkTool p);
void ark_Close(ArkTool p);
bool ark_GetFile(ArkTool p, const char* destFilepath, const char* arkFilename, bool performDecrypts);
bool ark_AddFile(ArkTool p, const char* srcFilepath, const char* arkFilename, bool performEncrypts);
bool ark_RemoveFile(ArkTool p, const char* arkFilename);
bool ark_ReplaceAFile(ArkTool p, const char* srcFilepath, const char* arkFilename, bool performEncrypts);
bool ark_RenameFile(ArkTool p, const char* newArkFilename, const char* oldArkFilename);
const ArkFileEntry ark_First(ArkTool p, ArkFileIterator rIter, const char* searchFilepath);
const ArkFileEntry ark_Next (ArkTool p, ArkFileIterator rIter, const char* searchFilepath);

const char* ark_Filename(ArkFileEntry file);
const char* ark_Arkname(ArkFileEntry file);

#ifdef __cplusplus
}
#endif
