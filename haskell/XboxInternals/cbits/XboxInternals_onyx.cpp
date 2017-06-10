#include "XboxInternals_onyx.h"
#include <iostream>

#include "Stfs/StfsPackage.h"

extern "C" {

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
) {

  try {

    StfsPackage pkg(stfsOut, StfsPackageCreate);
    pkg.metaData->titleID = titleID;
    pkg.metaData->displayName = packageName;
    pkg.metaData->displayDescription = packageDescription;
    pkg.metaData->publisherName = publisherName;
    pkg.metaData->titleName = titleName;
    pkg.metaData->thumbnailImageSize = thumbLen;
    pkg.metaData->thumbnailImage = thumb;
    pkg.metaData->titleThumbnailImageSize = thumbTitleLen;
    pkg.metaData->titleThumbnailImage = thumbTitle;
    pkg.metaData->contentType = SavedGame;

    for (int i = 0; i < countDirs; i++) {
      pkg.CreateFolder(dirs[i]);
    }
    for (int i = 0; i < countFiles; i++) {
      pkg.InjectFile(paths[i], pathsInPackage[i]);
    }

    pkg.Rehash();
    pkg.Resign(kvBin, kvBinLen);
    pkg.metaData->licenseData[0].bits = 1;
    pkg.Rehash();
    pkg.Resign(kvBin, kvBinLen);

    pkg.Close();

  } catch (string s) {

    std::cout << s << "\n";
    return 0;

  }

  return 1;

}

}
