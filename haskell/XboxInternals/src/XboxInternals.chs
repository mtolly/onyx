module XboxInternals (buildSTFSPackage) where

import Data.Word (Word32)
import Foreign (withMany, Ptr, withArray)
import Foreign.C (withCWString, withCString, CString, CInt, CWchar)

#include "XboxInternals_onyx.h"

withCStrings :: [FilePath] -> (Ptr CString -> IO a) -> IO a
withCStrings fps f = withMany withCString fps $ \pcs -> withArray pcs f

{#typedef wchar_t CWchar#}
{#default in `String' [wchar_t *] withCWString*#}
{#default out `String' [wchar_t *] peekCWString*#}

{#fun buildSTFSPackage as buildSTFSPackage_c
  { `String' -- ^ package name
  , `String' -- ^ package description
  , `Word32' -- ^ title ID
  , withCStrings* `[FilePath]' -- ^ paths of input files
  , withCStrings* `[FilePath]' -- ^ paths in the new package
  , `CInt' -- ^ number of input files
  , withCString* `FilePath' -- ^ stfs out
  } -> `()'
#}

buildSTFSPackage
  :: String -- ^ package name
  -> String -- ^ package description
  -> Word32 -- ^ title ID
  -> [(FilePath, FilePath)] -- ^ pairs of (input file, path in new package)
  -> FilePath -- ^ stfs out
  -> IO ()
buildSTFSPackage pn pd tid files = buildSTFSPackage_c pn pd tid
  (map fst files)
  (map snd files)
  (fromIntegral $ length files)
