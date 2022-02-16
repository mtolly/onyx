// C interface to ArkHdrPair so Onyx can bind to it.

#include "Onyx.h"
#include "ArkHdrPair.h"
#include "SongCrypt.h"

extern "C" {

ArkTool ark_new() {
  return (ArkTool) new ArkHdrPair;
}
void ark_delete(ArkTool p) {
  delete (ArkHdrPair *) p;
}

ArkFileIterator ark_new_iterator() {
  return (ArkFileIterator) new FileEntrySetIter;
}
void ark_delete_iterator(ArkFileIterator p) {
  delete (FileEntrySetIter *) p;
}

bool ark_Open(ArkTool p, const char* arkDirpath) {
  return ((ArkHdrPair *)p)->Open(arkDirpath);
}
bool ark_Save(ArkTool p) {
  return ((ArkHdrPair *)p)->Save();
}
void ark_Close(ArkTool p) {
  ((ArkHdrPair *)p)->Close();
}
bool ark_GetFile(ArkTool p, const char* destFilepath, const char* arkFilename, bool performDecrypts) {
  return ((ArkHdrPair *)p)->GetFile(destFilepath, arkFilename, performDecrypts);
}
bool ark_AddFile(ArkTool p, const char* srcFilepath, const char* arkFilename, bool performEncrypts) {
  return ((ArkHdrPair *)p)->AddFile(srcFilepath, arkFilename, performEncrypts);
}
bool ark_RemoveFile(ArkTool p, const char* arkFilename) {
  return ((ArkHdrPair *)p)->RemoveFile(arkFilename);
}
bool ark_ReplaceAFile(ArkTool p, const char* srcFilepath, const char* arkFilename, bool performEncrypts) {
  return ((ArkHdrPair *)p)->ReplaceAFile(srcFilepath, arkFilename, performEncrypts);
}
bool ark_RenameFile(ArkTool p, const char* newArkFilename, const char* oldArkFilename) {
  return ((ArkHdrPair *)p)->RenameFile(newArkFilename, oldArkFilename);
}
const ArkFileEntry ark_First(ArkTool p, ArkFileIterator rIter, const char* searchFilepath) {
  return (ArkFileEntry *) (((ArkHdrPair *)p)->First(*((FileEntrySetIter *) rIter), searchFilepath));
}
const ArkFileEntry ark_Next (ArkTool p, ArkFileIterator rIter, const char* searchFilepath) {
  return (ArkFileEntry *) (((ArkHdrPair *)p)->Next(*((FileEntrySetIter *) rIter), searchFilepath));
}

const char* ark_Filename(ArkFileEntry file) {
  return ((FileEntry *) file)->Filename();
}
const char* ark_Arkname(ArkFileEntry file) {
  return ((FileEntry *) file)->Arkname();
}

bool ark_DecryptVgs(const char* ofilename, const char* ifilename) {
  return DecryptVgs(ofilename, ifilename);
}

}
