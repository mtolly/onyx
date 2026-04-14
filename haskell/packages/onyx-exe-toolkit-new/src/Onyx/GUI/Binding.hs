{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.GUI.Binding where

import           Foreign
import           Foreign.C             (CUInt)
import qualified Language.C.Inline.Cpp as C

newtype Fl_Widget = Fl_Widget (Ptr Fl_Widget)
newtype Fl_Window = Fl_Window (Ptr Fl_Window)
newtype Fl_Group = Fl_Group (Ptr Fl_Group)
newtype Fl_Box = Fl_Box (Ptr Fl_Box)
newtype Fl_Button = Fl_Button (Ptr Fl_Button)
newtype Fl_Terminal = Fl_Terminal (Ptr Fl_Terminal)
newtype Fl_Menu_Bar = Fl_Menu_Bar (Ptr Fl_Menu_Bar)
newtype Fl_Sys_Menu_Bar = Fl_Sys_Menu_Bar (Ptr Fl_Sys_Menu_Bar)
newtype Fl_Native_File_Chooser = Fl_Native_File_Chooser (Ptr Fl_Native_File_Chooser)

type Callback = Ptr Fl_Widget -> Ptr () -> IO ()

newtype Fl_Color = Fl_Color CUInt

flContext :: C.Context
flContext = C.cppCtx <> C.cppTypePairs
  [ ("Fl_Widget", [t| Fl_Widget |])
  , ("Fl_Window", [t| Fl_Window |])
  , ("Fl_Group", [t| Fl_Group |])
  , ("Fl_Box", [t| Fl_Box |])
  , ("Fl_Button", [t| Fl_Button |])
  , ("Fl_Terminal", [t| Fl_Terminal |])
  , ("Fl_Menu_Bar", [t| Fl_Menu_Bar |])
  , ("Fl_Sys_Menu_Bar", [t| Fl_Sys_Menu_Bar |])
  , ("Fl_Native_File_Chooser", [t| Fl_Native_File_Chooser |])
  , ("Fl_Callback", [t| FunPtr Callback |])
  , ("Fl_Color", [t| Fl_Color |])
  ]

foreign import ccall "wrapper"
  makeCallback
    ::            Callback
    -> IO (FunPtr Callback)
