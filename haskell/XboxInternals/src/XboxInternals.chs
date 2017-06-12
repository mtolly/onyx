{-# LANGUAGE LambdaCase #-}
module XboxInternals (buildSTFSPackage) where

import Data.Word (Word32)
import Foreign
import Foreign.C
import qualified Data.ByteString as B

#include "XboxInternals_onyx.h"

withCStrings :: [FilePath] -> (Ptr CString -> IO a) -> IO a
withCStrings fps f = withMany withCString fps $ \pcs -> withArray pcs f

withBS :: B.ByteString -> ((Ptr Word8, CSize) -> IO a) -> IO a
withBS bs f = B.useAsCStringLen bs $ \(p, len) -> f (castPtr p, fromIntegral len)

{#typedef wchar_t CWchar#}
{#default in `String' [wchar_t *] withCWString*#}
{#default out `String' [wchar_t *] peekCWString*#}

{#typedef size_t CSize#}

{#typedef uint8_t Word8#}

{#fun buildSTFSPackage as buildSTFSPackage_c
  { `String' -- ^ package name
  , `String' -- ^ package description
  , `String' -- ^ publisher name
  , `String' -- ^ title name
  , `Word32' -- ^ title ID
  , withCStrings* `[FilePath]' -- ^ paths of directories to make, in order
  , `CInt' -- ^ number of directories to make
  , withCStrings* `[FilePath]' -- ^ paths of input files
  , withCStrings* `[FilePath]' -- ^ paths in the new package
  , `CInt' -- ^ number of input files
  , withBS* `B.ByteString'& -- ^ thumbnail data
  , withBS* `B.ByteString'& -- ^ title thumbnail data
  , withBS* `B.ByteString'& -- ^ kv data
  , withCString* `FilePath' -- ^ stfs out
  } -> `CInt'
#}

buildSTFSPackage
  :: String -- ^ package name
  -> String -- ^ package description
  -> String -- ^ publisher name
  -> String -- ^ title name
  -> Word32 -- ^ title ID
  -> [FilePath] -- ^ paths of directories to make, in order
  -> [(FilePath, FilePath)] -- ^ pairs of (input file, path in new package)
  -> B.ByteString -- ^ thumbnail data
  -> B.ByteString -- ^ title thumbnail data
  -> B.ByteString -- ^ kv data
  -> FilePath -- ^ stfs out
  -> IO Bool
buildSTFSPackage pn pd pub game tid dirs files thumb title kv out = let
  fixSlashes = map $ \case '/' -> '\\'; c -> c
  in (/= 0) <$> buildSTFSPackage_c pn pd pub game tid
    (map fixSlashes dirs)
    (fromIntegral $ length dirs)
    (map fst files)
    (map (fixSlashes . snd) files)
    (fromIntegral $ length files)
    thumb title kv
    out
