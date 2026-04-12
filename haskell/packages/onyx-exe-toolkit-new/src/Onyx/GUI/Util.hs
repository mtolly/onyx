{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Onyx.GUI.Util (askFolder) where

import           Control.Exception     (bracket)
import           Foreign               (nullPtr)
import           Foreign.C             (CString, peekCString, withCString)
import           Onyx.GUI.Binding

#ifdef WINDOWS

import           Foreign
import           Foreign.C

#else

import qualified Language.C.Inline.Cpp as C

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Native_File_Chooser.H"

#endif

withUTF8 :: String -> (CString -> IO a) -> IO a
withUTF8 = withCString -- TODO actually force utf-8

peekUTF8 :: CString -> IO String
peekUTF8 = peekCString -- TODO actually force utf-8

peekMaybeUTF8 :: CString -> IO (Maybe String)
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
