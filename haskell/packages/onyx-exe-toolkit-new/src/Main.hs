{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Foreign
import           Foreign.C
import qualified Language.C.Inline.Cpp as C
-- import qualified Language.C.Inline.Unsafe as CU

newtype Widget = Widget (Ptr Widget)

C.context $ C.cppCtx <> C.cppTypePairs
  [ ("Fl_Widget", [t| Widget |])
  ]

C.include "FL/Fl.H"
C.include "FL/Fl_Window.H"
C.include "FL/Fl_Button.H"

foreign import ccall "wrapper"
  makeCallback
    ::            (Ptr Widget -> IO ())
    -> IO (FunPtr (Ptr Widget -> IO ()))

{-
TODO we need to tie calling freeHaskellFunPtr (on callbacks and other methods)
to widget destruction, like fltkhs does.
currently leaning towards, we just patch Fl_Widget::~Fl_Widget() to do this
-}

main :: IO ()
main = do
  cb <- makeCallback \_ -> putStrLn "hi from haskell"
  n <- withMany withCString ["onyx"] \xs -> withArray xs \p -> [C.block| int {
    Fl::scheme("gtk+");
    Fl::lock(); // this is required to get threads to work properly
    Fl_Window *window = new Fl_Window(340, 180);
    Fl_Button *box = new Fl_Button(20, 40, 300, 100, "Hello, World!");
    box->labelfont(FL_BOLD + FL_ITALIC);
    box->labelsize(36);
    box->labeltype(FL_SHADOW_LABEL);
    box->callback($(void(*cb)(Fl_Widget *widget)));
    window->end();
    window->show(1, $(char** p));
    return Fl::run();
  } |]
  print n
