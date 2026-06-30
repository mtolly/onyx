{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.GUI.Util
( askFolder
, Fl_UTF8(..), withMaybeUTF8, peekMaybeUTF8
, checkValue
, Cast(..)
, setCallback
, withNativeFileChooser
, setLabelTooltip
) where

import           Control.Exception        (bracket)
import           Control.Monad            (forM_)
import qualified Data.ByteString          as B
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Data.Text.Encoding.Error (lenientDecode)
import           Foreign
import           Foreign.C
import qualified Language.C.Inline.Cpp    as C
import           Onyx.GUI.Binding
import           Onyx.Localization        (L, MessageOutput (..), l10n)

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Check_Button.H"
C.include "FL/Fl_Round_Button.H"
C.include "FL/Fl_Counter.H"
C.include "FL/Fl_Input.H"
C.include "FL/Fl_Window.H"
C.include "FL/Fl_Tabs.H"
C.include "FL/Fl_Menu_Button.H"
C.include "FL/Fl_Native_File_Chooser.H"
C.include "FL/Fl_Hor_Nice_Slider.H"
C.include "FL/Fl_Choice.H"
C.include "hs_dnd_button.h"

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

class Cast a b where
  cast :: Ptr a -> Ptr b

instance Cast Fl_Widget Fl_Widget where cast = id
instance Cast Fl_Button Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Button* p) } |]
instance Cast Fl_Check_Button Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Check_Button* p) } |]
instance Cast Fl_Round_Button Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Round_Button* p) } |]
instance Cast Fl_Menu_Button Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Menu_Button* p) } |]
instance Cast Fl_Counter Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Counter* p) } |]
instance Cast Fl_Input Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Input* p) } |]
instance Cast Fl_Window Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Window* p) } |]
instance Cast Fl_Tabs Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Tabs* p) } |]
instance Cast Fl_Hor_Nice_Slider Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Hor_Nice_Slider* p) } |]
instance Cast Fl_Choice Fl_Widget where cast p = [C.pure| Fl_Widget* { $(Fl_Choice* p) } |]
instance Cast HsDndButton Fl_Widget where cast p = [C.pure| Fl_Widget* { $(HsDndButton* p) } |]

checkValue :: Ptr Fl_Check_Button -> IO Bool
checkValue check = toBool <$> [C.exp| int { $(Fl_Check_Button* check)->value() } |]

-- TODO eventually we probably want to free these when widgets are deleted.
-- fltkhs made subclasses of everything for that purpose
setCallback :: (Cast a Fl_Widget) => Ptr a -> IO () -> IO ()
setCallback (cast -> btn) act = do
  cb <- makeCallback \_ _ -> act
  [C.block| void { $(Fl_Widget* btn)->callback($(Fl_Callback cb)); } |]

withNativeFileChooser :: CInt -> (Ptr Fl_Native_File_Chooser -> IO a) -> IO a
withNativeFileChooser typ = bracket
  [C.exp| Fl_Native_File_Chooser* {
    new Fl_Native_File_Chooser((Fl_Native_File_Chooser::Type)$(int typ))
  } |]
  \p -> [C.block| void { delete $(Fl_Native_File_Chooser* p); } |]

askFolderResult :: (?loc :: L) => Maybe FilePath -> IO (Maybe FilePath)

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

askFolderResult initial = maybe ($ nullPtr) withUTF8 initial \s ->
  withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_DIRECTORY } |]
  \picker -> withUTF8 (l10n "file-picker-select-folder") \title -> [C.block| const char* {
    Fl_Native_File_Chooser* picker = $(Fl_Native_File_Chooser* picker);
    picker->title($(char* title));
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

askFolder :: (?loc :: L) => Maybe FilePath -> (FilePath -> IO ()) -> IO ()
askFolder initial fn = askFolderResult initial >>= mapM_ fn

setLabelTooltip :: (Cast a Fl_Widget) => MessageOutput -> Ptr a -> IO ()
setLabelTooltip mo ptr = do
  let widget = cast ptr
  withUTF8 mo.output \s -> [C.block| void { $(Fl_Widget* widget)->copy_label($(char* s)); } |]
  forM_ (lookup "tooltip" mo.attributes) \t -> withUTF8 t \s ->
    [C.block| void { $(Fl_Widget* widget)->copy_tooltip($(char* s)); } |]
