{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Ark (replaceSong) where

import           Control.Exception (bracket, bracket_)
import           Control.Monad     (forM_, unless)
import qualified Data.ByteString   as B
import qualified Data.DTA          as D
import           Data.Monoid       ((<>))
import           Foreign
import           Foreign.C
import qualified System.IO         as IO
import           System.IO.Temp    (withSystemTempFile)

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

-- | Replaces a song in Guitar Hero II (U).
replaceSong
  :: FilePath -- ^ the @GEN@ folder containing ark and hdr files
  -> B.ByteString -- ^ the folder name (and DTB key) of the song to replace
  -> [D.Chunk B.ByteString] -- ^ the DTB snippet to insert into @config/gen/songs.dtb@
  -> [(B.ByteString, FilePath)] -- ^ the files to copy into the song folder, e.g. @("yyz.mid", "some/dir/notes.mid")@
  -> IO ()
replaceSong gen key snippet files = do
  let wrap msg f = f >>= \b -> unless b $ fail msg
  bracket ark_new ark_delete $ \ark -> do
    let open = wrap "Couldn't open the ARK file" $ ark_Open ark gen
    bracket_ open (ark_Close ark) $ do
      withSystemTempFile "songs.dtb" $ \fdtb hdl -> do
        IO.hClose hdl
        wrap "Couldn't find songs.dtb in the ARK." $
          ark_GetFile ark fdtb "config/gen/songs.dtb" True
        D.DTA z (D.Tree _ chunks) <- D.readFileDTB fdtb
        let adjust chunk = case chunk of
              D.Parens (D.Tree _ (D.Key k : _)) | k == key ->
                D.Parens $ D.Tree 0 $ D.Key k : map adjustSnippet snippet
              _ -> chunk
            adjustSnippet = \case
              String s -> String $ adjustString s
              Parens t -> Parens $ adjustTree t
              Braces t -> Braces $ adjustTree t
              Brackets t -> Brackets $ adjustTree t
              chunk -> chunk
            adjustTree (Tree tid chunks) = Tree tid $ map adjustSnippet chunks
            adjustString str = case B.breakSubstring "$SONGKEY" str of
              (h, t) | not $ T.null t -> adjustString $ h <> key <> T.drop 8 t
              _ -> str
        D.writeFileDTB fdtb $ D.renumberFrom 1 $ D.DTA z $ D.Tree 0 $ map adjust chunks
        wrap "Couldn't update songs.dtb in the ARK." $
          ark_ReplaceAFile ark fdtb "config/gen/songs.dtb" True
        entries <- searchFiles ark $ "songs/" <> key <> "/*"
        forM_ entries $ \entry -> do
          name <- ark_Arkname entry
          wrap ("Couldn't remove file from ARK: " <> show name) $
            ark_RemoveFile ark name
        forM_ files $ \(arkName, localPath) -> do
          let arkPath = "songs/" <> key <> "/" <> arkName
          wrap ("Couldn't add file " <> show localPath <> " to ARK as " <> show arkPath) $
            ark_AddFile ark localPath arkPath True -- encryption doesn't matter
        wrap "Couldn't save the updated ARK file." $ ark_Save ark
