{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.GUI.Util (askFolder, Fl_UTF8(..), withMaybeUTF8, peekMaybeUTF8) where

import           Control.Exception        (bracket)
import qualified Data.ByteString          as B
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Foreign
import           Foreign.C
import qualified Language.C.Inline.Cpp    as C
import           Onyx.GUI.Binding

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Native_File_Chooser.H"

class Fl_UTF8 s where
  withUTF8 :: s -> (CString -> IO a) -> IO a
  peekUTF8 :: CString -> IO s

instance Fl_UTF8 B.ByteString where
  withUTF8 = B.useAsCString
  peekUTF8 = B.packCString

-- TODO use Data.Text.Foreign after we upgrade text package
instance Fl_UTF8 T.Text where
  withUTF8 = withUTF8 . TE.encodeUtf8
  peekUTF8 = fmap (TE.decodeUtf8With lenientDecode) . peekUTF8

instance Fl_UTF8 String where
  withUTF8 = withUTF8 . T.pack
  peekUTF8 = fmap T.unpack . peekUTF8

withMaybeUTF8 :: (Fl_UTF8 s) => Maybe s -> (CString -> IO a) -> IO a
withMaybeUTF8 = maybe ($ nullPtr) withUTF8

peekMaybeUTF8 :: (Fl_UTF8 s) => CString -> IO (Maybe s)
peekMaybeUTF8 p = if p == nullPtr
  then return Nothing
  else Just <$> peekUTF8 p

askFolderResult :: Maybe FilePath -> IO (Maybe FilePath)

#ifdef WINDOWS

-- seems this has to be 'safe'! 'unsafe' crashes occasionally
foreign import ccall safe "OnyxPickFolder"
  c_PickFolder :: CWString -> IO CWString

askFolderResult initial = maybe ($ nullPtr) withCWString initial \ws -> do
  res <- c_PickFolder ws
  if res == nullPtr
    then return Nothing
    else Just <$> peekCWString res

#else

askFolderResult initial = maybe ($ nullPtr) withUTF8 initial \s -> bracket
  [C.exp| Fl_Native_File_Chooser* {
    new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_DIRECTORY)
  } |]
  (\p -> [C.block| void { delete $(Fl_Native_File_Chooser* p); } |])
  \picker -> [C.block| const char* {
    Fl_Native_File_Chooser* picker = $(Fl_Native_File_Chooser* picker);
    picker->title("Select folder");
    if ($(char* s)) {
      picker->directory($(char* s));
    }
    if (picker->show() == 0) {
      return picker->filename();
    }
    else {
      return nullptr;
    }
  } |] >>= peekMaybeUTF8

#endif

askFolder :: Maybe FilePath -> (FilePath -> IO ()) -> IO ()
askFolder initial fn = askFolderResult initial >>= mapM_ fn
