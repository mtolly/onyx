module Onyx.Harmonix.Ark.ArkTool where

import           Control.Exception (bracket, bracket_)
import           Control.Monad     (unless)
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

wrapArk :: String -> IO Bool -> IO ()
wrapArk s f = do
  b <- f
  unless b $ fail $ "ARK operation failed: " <> s

withArk :: FilePath -> (ArkTool -> IO a) -> IO a
withArk gen act = bracket ark_new ark_delete $ \ark -> do
  bracket_ (wrapArk ("opening folder " <> gen) $ ark_Open ark gen) (ark_Close ark) $ do
    act ark

ark_GetFile' :: ArkTool -> FilePath -> B.ByteString -> Bool -> IO ()
ark_GetFile' ark fp fpInArk enc
  = wrapArk ("extracting file " <> show fpInArk <> " to " <> show fp)
  $ ark_GetFile ark fp fpInArk enc

ark_ReplaceAFile' :: ArkTool -> FilePath -> B.ByteString -> Bool -> IO ()
ark_ReplaceAFile' ark fp fpInArk enc
  = wrapArk ("replacing file " <> show fpInArk <> " with " <> show fp)
  $ ark_ReplaceAFile ark fp fpInArk enc

ark_AddFile' :: ArkTool -> FilePath -> B.ByteString -> Bool -> IO ()
ark_AddFile' ark fp fpInArk enc
  = wrapArk ("inserting file " <> show fp <> " at " <> show fpInArk)
  $ ark_AddFile ark fp fpInArk enc

ark_RemoveFile' :: ArkTool -> B.ByteString -> IO ()
ark_RemoveFile' ark fpInArk
  = wrapArk ("removing file " <> show fpInArk)
  $ ark_RemoveFile ark fpInArk

ark_Save' :: ArkTool -> IO ()
ark_Save' ark = wrapArk "saving ARK" $ ark_Save ark

{#fun ark_DecryptVgs
  { withCString* `FilePath'
  , withCString* `FilePath'
  } -> `Bool'
#}
