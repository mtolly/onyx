#include "macfiles.h"
#include <appkit/appkit.h>

void onyx_ShowFiles(char **files, int len) {
  NSMutableArray *urls = [[NSMutableArray alloc] init];
  for (int i = 0; i < len; i++) {
    NSString *str = [NSString stringWithUTF8String:files[i]];
    [urls addObject:[NSURL fileURLWithPath:str]];
  }
  [[NSWorkspace sharedWorkspace] activateFileViewerSelectingURLs:urls];
}
