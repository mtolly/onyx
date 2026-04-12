{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.GUI.Binding where

import           Foreign
import qualified Language.C.Inline.Cpp as C

newtype Widget = Widget (Ptr Widget)
newtype Window = Window (Ptr Window)
newtype NativeFileChooser = NativeFileChooser (Ptr NativeFileChooser)

type Callback = Ptr Widget -> Ptr () -> IO ()

flContext :: C.Context
flContext = C.cppCtx <> C.cppTypePairs
  [ ("Fl_Widget", [t| Widget |])
  , ("Fl_Window", [t| Window |])
  , ("Fl_Callback", [t| FunPtr Callback |])
  , ("Fl_Native_File_Chooser", [t| NativeFileChooser |])
  ]
