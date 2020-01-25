#include "winfiles.h"
#include <stddef.h>
#include <stdio.h>
#include <shlobj.h>

void onyx_ShowFiles(wchar_t *dir, wchar_t **files, int len) {
  // initialize COM (SDL does this itself but I don't think FLTK does)
  CoInitializeEx(NULL, COINIT_MULTITHREADED);

  ITEMIDLIST *pidl = ILCreateFromPathW(dir);
  if (pidl) {
    const ITEMIDLIST *pidls[len];
    int i = 0;
    while (i < len) {
      pidls[i] = ILCreateFromPathW(files[i]);
      if (pidls[i]) {
        i++;
      } else {
        break;
      }
    }
    if (i == len) {
      HRESULT res = SHOpenFolderAndSelectItems(pidl, len, pidls, 0);
      // if (res == S_OK) successful
    }
    for (int j = 0; j < i; j++) {
      ILFree((ITEMIDLIST *) pidls[j]);
    }
    ILFree(pidl);
  }

  // tear down COM
  CoUninitialize();
}
