module ArkTool where

import           Control.Exception (bracket)
import qualified Data.ByteString   as B
import           Data.Monoid       ((<>))
import           Foreign
import           Foreign.C

#include "ArkTool_v6.1/Onyx.h"

{#pointer ArkTool as ArkTool newtype #}
{#pointer ArkFileIterator as ArkFileIterator newtype #}
{#pointer ArkFileEntry as ArkFileEntry newtype #}

{#fun ark_new
  {} -> `ArkTool'
#}

{#fun ark_delete
  { `ArkTool'
  } -> `()'
#}

{#fun ark_new_iterator
  {} -> `ArkFileIterator'
#}

{#fun ark_delete_iterator
  { `ArkFileIterator'
  } -> `()'
#}

withByteString :: B.ByteString -> (CString -> IO a) -> IO a
withByteString = B.useAsCString

peekByteString :: CString -> IO B.ByteString
peekByteString = B.packCString

{#fun ark_Open
  { `ArkTool'
  , withCString* `FilePath' -- ^ ark directory
  } -> `Bool'
#}

{#fun ark_Save
  { `ArkTool'
  } -> `Bool'
#}

{#fun ark_Close
  { `ArkTool'
  } -> `()'
#}

{#fun ark_GetFile
  { `ArkTool'
  , withCString* `FilePath' -- ^ destination filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform decryption
  } -> `Bool'
#}

{#fun ark_AddFile
  { `ArkTool'
  , withCString* `FilePath' -- ^ source filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform encryption
  } -> `Bool'
#}

{#fun ark_RemoveFile
  { `ArkTool'
  , withByteString* `B.ByteString' -- ^ filename in ark
  } -> `Bool'
#}

{#fun ark_ReplaceAFile
  { `ArkTool'
  , withCString* `FilePath' -- ^ source filepath
  , withByteString* `B.ByteString' -- ^ filename in ark
  , `Bool' -- ^ whether to perform encryption
  } -> `Bool'
#}

{#fun ark_First
  { `ArkTool'
  , `ArkFileIterator'
  , withByteString* `B.ByteString' -- ^ search filepath
  } -> `ArkFileEntry'
#}

{#fun ark_Next
  { `ArkTool'
  , `ArkFileIterator'
  , withByteString* `B.ByteString' -- ^ search filepath
  } -> `ArkFileEntry'
#}

{#fun ark_Arkname
  { `ArkFileEntry'
  } -> `B.ByteString' peekByteString*
#}

searchFiles :: ArkTool -> B.ByteString -> IO [ArkFileEntry]
searchFiles ark pat = bracket ark_new_iterator ark_delete_iterator $ \iter -> do
  let go entry@(ArkFileEntry p) = if p == nullPtr
        then return []
        else (entry :) <$> (ark_Next ark iter pat >>= go)
  ark_First ark iter pat >>= go
