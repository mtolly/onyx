#include "XboxInternals_onyx.h"
#include <iostream>

#include "Stfs/StfsPackage.h"

extern "C" {

void buildSTFSPackage
( wchar_t *packageName
, wchar_t *packageDescription
, uint32_t titleID
, char **paths
, char **pathsInPackage
, int countFiles
, char *stfsOut
) {

  StfsPackage pkg(stfsOut, StfsPackageCreate);
  pkg.metaData->titleID = titleID;
  pkg.metaData->displayName = packageName;
  pkg.metaData->displayDescription = packageDescription;

  for (int i = 0; i < countFiles; i++) {
    pkg.InjectFile(paths[i], pathsInPackage[i]);
  }

  pkg.Close();

}

}
