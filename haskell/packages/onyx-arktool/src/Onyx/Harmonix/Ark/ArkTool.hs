{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.Harmonix.Ark.ArkTool where

import           Control.Exception     (bracket, bracket_)
import           Control.Monad         (unless)
import qualified Data.ByteString       as B
import           Data.Monoid           ((<>))
import           Foreign
import           Foreign.C
import qualified Language.C.Inline.Cpp as C

newtype ArkHdrPair       = ArkHdrPair       (Ptr ArkHdrPair      )
newtype FileEntrySetIter = FileEntrySetIter (Ptr FileEntrySetIter)
newtype FileEntry        = FileEntry        (Ptr FileEntry       )

C.context $ C.cppCtx <> C.cppTypePairs
  [ ("ArkHdrPair"      , [t| ArkHdrPair       |])
  , ("FileEntrySetIter", [t| FileEntrySetIter |])
  , ("FileEntry"       , [t| FileEntry        |])
  ]

C.include "ArkTool_v6.1/ArkHdrPair.h"
C.include "ArkTool_v6.1/SongCrypt.h"

ark_new :: IO ArkHdrPair
ark_new = ArkHdrPair <$>
  [C.exp| ArkHdrPair* { new ArkHdrPair } |]

ark_delete :: ArkHdrPair -> IO ()
ark_delete (ArkHdrPair p) =
  [C.block| void { delete $(ArkHdrPair* p); } |]

ark_new_iterator :: IO FileEntrySetIter
ark_new_iterator = FileEntrySetIter <$>
  [C.exp| FileEntrySetIter* { new FileEntrySetIter } |]

ark_delete_iterator :: FileEntrySetIter -> IO ()
ark_delete_iterator (FileEntrySetIter p) =
  [C.block| void { delete $(FileEntrySetIter* p); } |]

arkCheck :: String -> IO CBool -> IO ()
arkCheck err f = do
  b <- f
  unless (b /= 0) $ fail $ "ARK operation failed: " <> err

ark_Open
  :: ArkHdrPair
  -> FilePath -- ^ ark directory
  -> IO ()
ark_Open (ArkHdrPair p) arkDirpath = withCString arkDirpath $ \p_arkDirpath ->
  arkCheck ("opening ark from " <> arkDirpath)
    [C.exp| bool { $(ArkHdrPair* p)->Open($(char* p_arkDirpath)) } |]

ark_Save :: ArkHdrPair -> IO ()
ark_Save (ArkHdrPair p) =
  arkCheck "saving ark" [C.exp| bool { $(ArkHdrPair* p)->Save() } |]

ark_Close :: ArkHdrPair -> IO ()
ark_Close (ArkHdrPair p) =
  [C.block| void { $(ArkHdrPair* p)->Close(); } |]

ark_TryGetFile
  :: ArkHdrPair
  -> FilePath -- ^ destination filepath
  -> B.ByteString -- ^ filename in ark
  -> Bool -- ^ whether to perform decryption
  -> IO Bool
ark_TryGetFile (ArkHdrPair p) destFilepath arkFilename performDecrypts =
  withCString destFilepath $ \p_destFilepath -> do
    B.useAsCString arkFilename $ \p_arkFilename -> do
      let c_performDecrypts = fromBool performDecrypts
      toBool <$> [C.exp| bool {
        $(ArkHdrPair* p)->GetFile
          ( $(char* p_destFilepath)
          , $(char* p_arkFilename)
          , $(bool c_performDecrypts)
          )
        } |]

ark_GetFile
  :: ArkHdrPair
  -> FilePath -- ^ destination filepath
  -> B.ByteString -- ^ filename in ark
  -> Bool -- ^ whether to perform decryption
  -> IO ()
ark_GetFile p destFilepath arkFilename performDecrypts = do
  b <- ark_TryGetFile p destFilepath arkFilename performDecrypts
  unless b $ fail $ "ARK operation failed: extracting file " <> show arkFilename <> " to " <> show destFilepath

ark_AddFile
  :: ArkHdrPair
  -> FilePath -- ^ source filepath
  -> B.ByteString -- ^ filename in ark
  -> Bool -- ^ whether to perform encryption
  -> IO ()
ark_AddFile (ArkHdrPair p) srcFilepath arkFilename performEncrypts =
  withCString srcFilepath $ \p_srcFilepath -> do
    B.useAsCString arkFilename $ \p_arkFilename -> do
      let c_performEncrypts = fromBool performEncrypts
      arkCheck ("inserting file " <> show srcFilepath <> " at " <> show arkFilename)
        [C.exp| bool {
          $(ArkHdrPair* p)->AddFile
            ( $(char* p_srcFilepath)
            , $(char* p_arkFilename)
            , $(bool c_performEncrypts)
            )
        } |]

ark_RemoveFile
  :: ArkHdrPair
  -> B.ByteString -- ^ filename in ark
  -> IO ()
ark_RemoveFile (ArkHdrPair p) arkFilename =
  B.useAsCString arkFilename $ \p_arkFilename -> do
    arkCheck ("removing file " <> show arkFilename)
      [C.exp| bool { $(ArkHdrPair* p)->RemoveFile($(char* p_arkFilename)) } |]

ark_ReplaceAFile
  :: ArkHdrPair
  -> FilePath -- ^ source filepath
  -> B.ByteString -- ^ filename in ark
  -> Bool -- ^ whether to perform encryption
  -> IO ()
ark_ReplaceAFile (ArkHdrPair p) srcFilepath arkFilename performEncrypts =
  withCString srcFilepath $ \p_srcFilepath -> do
    B.useAsCString arkFilename $ \p_arkFilename -> do
      let c_performEncrypts = fromBool performEncrypts
      arkCheck ("replacing file " <> show arkFilename <> " with " <> show srcFilepath)
        [C.exp| bool {
          $(ArkHdrPair* p)->ReplaceAFile
            ( $(char* p_srcFilepath)
            , $(char* p_arkFilename)
            , $(bool c_performEncrypts)
            )
        } |]

ark_First
  :: ArkHdrPair
  -> FileEntrySetIter
  -> B.ByteString -- ^ search filepath
  -> IO FileEntry
ark_First (ArkHdrPair p) (FileEntrySetIter rIter) searchFilepath = do
  B.useAsCString searchFilepath $ \c_searchFilepath -> do
    FileEntry <$>
      [C.exp| const FileEntry* {
        $(ArkHdrPair* p)->First
          ( *$(FileEntrySetIter* rIter)
          , $(char* c_searchFilepath)
          )
      } |]

ark_Next
  :: ArkHdrPair
  -> FileEntrySetIter
  -> B.ByteString -- ^ search filepath
  -> IO FileEntry
ark_Next (ArkHdrPair p) (FileEntrySetIter rIter) searchFilepath = do
  B.useAsCString searchFilepath $ \c_searchFilepath -> do
    FileEntry <$>
      [C.exp| const FileEntry* {
        $(ArkHdrPair* p)->Next
          ( *$(FileEntrySetIter* rIter)
          , $(char* c_searchFilepath)
          )
      } |]

ark_Arkname :: FileEntry -> IO B.ByteString
ark_Arkname (FileEntry p) = do
  s <- [C.exp| const char* { $(FileEntry* p)->Arkname() } |]
  B.packCString s

searchFiles :: ArkHdrPair -> B.ByteString -> IO [FileEntry]
searchFiles ark pat = bracket ark_new_iterator ark_delete_iterator $ \iter -> do
  let go entry@(FileEntry p) = if p == nullPtr
        then return []
        else (entry :) <$> (ark_Next ark iter pat >>= go)
  ark_First ark iter pat >>= go

wrapArk :: String -> IO Bool -> IO ()
wrapArk s f = do
  b <- f
  unless b $ fail $ "ARK operation failed: " <> s

withArk :: FilePath -> (ArkHdrPair -> IO a) -> IO a
withArk gen act = bracket ark_new ark_delete $ \ark -> do
  bracket_ (ark_Open ark gen) (ark_Close ark) $ do
    act ark

ark_DecryptVgs :: FilePath -> FilePath -> IO Bool
ark_DecryptVgs ofilename ifilename = do
  withCString ofilename $ \c_ofilename -> do
    withCString ifilename $ \c_ifilename -> do
      toBool <$>
        [C.exp| bool {
          DecryptVgs($(char* c_ofilename), $(char* c_ifilename))
        }|]
