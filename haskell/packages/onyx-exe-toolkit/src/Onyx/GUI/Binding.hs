{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.GUI.Binding where

import           Foreign
import           Foreign.C             (CInt, CString, CUInt)
import qualified Language.C.Inline.Cpp as C

newtype Fl_Widget = Fl_Widget (Ptr Fl_Widget)
newtype Fl_Window = Fl_Window (Ptr Fl_Window)
newtype Fl_Group = Fl_Group (Ptr Fl_Group)
newtype Fl_Box = Fl_Box (Ptr Fl_Box)
newtype Fl_Button = Fl_Button (Ptr Fl_Button)
newtype Fl_Check_Button = Fl_Check_Button (Ptr Fl_Check_Button)
newtype Fl_Round_Button = Fl_Round_Button (Ptr Fl_Round_Button)
newtype Fl_Input = Fl_Input (Ptr Fl_Input)
newtype Fl_Counter = Fl_Counter (Ptr Fl_Counter)
newtype Fl_Choice = Fl_Choice (Ptr Fl_Choice)
newtype Fl_Menu_Button = Fl_Menu_Button (Ptr Fl_Menu_Button)
newtype Fl_Simple_Terminal = Fl_Simple_Terminal (Ptr Fl_Simple_Terminal)
newtype Fl_Menu_Bar = Fl_Menu_Bar (Ptr Fl_Menu_Bar)
newtype Fl_Sys_Menu_Bar = Fl_Sys_Menu_Bar (Ptr Fl_Sys_Menu_Bar)
newtype Fl_Native_File_Chooser = Fl_Native_File_Chooser (Ptr Fl_Native_File_Chooser)
newtype Fl_Pack = Fl_Pack (Ptr Fl_Pack)
newtype Fl_Scroll = Fl_Scroll (Ptr Fl_Scroll)
newtype Fl_Tabs = Fl_Tabs (Ptr Fl_Tabs)
newtype Fl_Slider = Fl_Slider (Ptr Fl_Slider)
newtype Fl_Hor_Nice_Slider = Fl_Hor_Nice_Slider (Ptr Fl_Hor_Nice_Slider)
newtype Fl_Value_Slider = Fl_Value_Slider (Ptr Fl_Value_Slider)
newtype Fl_PNG_Image = Fl_PNG_Image (Ptr Fl_PNG_Image)
newtype Fl_Tree = Fl_Tree (Ptr Fl_Tree)
newtype Fl_Tree_Item = Fl_Tree_Item (Ptr Fl_Tree_Item)
newtype Fl_Gl_Window = Fl_Gl_Window (Ptr Fl_Gl_Window)
newtype HsGlWindow = HsGlWindow (Ptr HsGlWindow)
newtype Fl_Menu_Item = Fl_Menu_Item (Ptr Fl_Menu_Item)
newtype Fl_Center = Fl_Center (Ptr Fl_Center)
newtype HsScrubber = HsScrubber (Ptr HsScrubber)
newtype HsDndButton = HsDndButton (Ptr HsDndButton)
newtype HsDndGroup = HsDndGroup (Ptr HsDndGroup)
newtype HsDndBox = HsDndBox (Ptr HsDndBox)
newtype HsTree = HsTree (Ptr HsTree)
-- FLUID widget_classes, generated from the layouts in fluid
newtype FileLoadPanel = FileLoadPanel (Ptr FileLoadPanel)
newtype FilePickerRow = FilePickerRow (Ptr FilePickerRow)

type Callback = Ptr Fl_Widget -> Ptr () -> IO ()
type DrawCallback = Ptr () -> IO ()
type TimeoutCallback = Ptr () -> IO ()
type ScrubberHandleCallback = Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
type DndCallback = Ptr () -> CString -> IO ()
type OpenCallback = CString -> IO ()
type TreeHandleCallback = Ptr () -> CInt -> IO CInt

newtype Fl_Color = Fl_Color CUInt

flContext :: C.Context
flContext = C.cppCtx <> C.cppTypePairs
  [ ("Fl_Widget", [t| Fl_Widget |])
  , ("Fl_Window", [t| Fl_Window |])
  , ("Fl_Group", [t| Fl_Group |])
  , ("Fl_Box", [t| Fl_Box |])
  , ("Fl_Button", [t| Fl_Button |])
  , ("Fl_Check_Button", [t| Fl_Check_Button |])
  , ("Fl_Round_Button", [t| Fl_Round_Button |])
  , ("Fl_Input", [t| Fl_Input |])
  , ("Fl_Counter", [t| Fl_Counter |])
  , ("Fl_Choice", [t| Fl_Choice |])
  , ("Fl_Menu_Button", [t| Fl_Menu_Button |])
  , ("Fl_Simple_Terminal", [t| Fl_Simple_Terminal |])
  , ("Fl_Menu_Bar", [t| Fl_Menu_Bar |])
  , ("Fl_Sys_Menu_Bar", [t| Fl_Sys_Menu_Bar |])
  , ("Fl_Native_File_Chooser", [t| Fl_Native_File_Chooser |])
  , ("Fl_Pack", [t| Fl_Pack |])
  , ("Fl_Scroll", [t| Fl_Scroll |])
  , ("Fl_Tabs", [t| Fl_Tabs |])
  , ("Fl_Slider", [t| Fl_Slider |])
  , ("Fl_Hor_Nice_Slider", [t| Fl_Hor_Nice_Slider |])
  , ("Fl_Value_Slider", [t| Fl_Value_Slider |])
  , ("Fl_PNG_Image", [t| Fl_PNG_Image |])
  , ("Fl_Tree", [t| Fl_Tree |])
  , ("Fl_Tree_Item", [t| Fl_Tree_Item |])
  , ("Fl_Gl_Window", [t| Fl_Gl_Window |])
  , ("HsGlWindow", [t| HsGlWindow |])
  , ("Fl_Menu_Item", [t| Fl_Menu_Item |])
  , ("Fl_Center", [t| Fl_Center |])
  , ("HsScrubber", [t| HsScrubber |])
  , ("HsDndButton", [t| HsDndButton |])
  , ("HsDndGroup", [t| HsDndGroup |])
  , ("HsDndBox", [t| HsDndBox |])
  , ("HsTree", [t| HsTree |])
  , ("FileLoadPanel", [t| FileLoadPanel |])
  , ("FilePickerRow", [t| FilePickerRow |])
  , ("Fl_Callback", [t| FunPtr Callback |])
  , ("Fl_Color", [t| Fl_Color |])
  ]

foreign import ccall "wrapper"
  makeCallback
    ::            Callback
    -> IO (FunPtr Callback)

foreign import ccall "wrapper"
  makeDrawCallback
    ::            DrawCallback
    -> IO (FunPtr DrawCallback)

foreign import ccall "wrapper"
  makeTimeoutCallback
    ::            TimeoutCallback
    -> IO (FunPtr TimeoutCallback)

foreign import ccall "wrapper"
  makeScrubberHandleCallback
    ::            ScrubberHandleCallback
    -> IO (FunPtr ScrubberHandleCallback)

foreign import ccall "wrapper"
  makeDndCallback
    ::            DndCallback
    -> IO (FunPtr DndCallback)

foreign import ccall "wrapper"
  makeOpenCallback
    ::            OpenCallback
    -> IO (FunPtr OpenCallback)

foreign import ccall "wrapper"
  makeTreeHandleCallback
    ::            TreeHandleCallback
    -> IO (FunPtr TreeHandleCallback)
