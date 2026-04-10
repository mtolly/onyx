{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Foreign
import           Foreign.C
import qualified Language.C.Inline.Cpp as C

C.context C.cppCtx

C.include "FL/Fl.H"
C.include "FL/Fl_Window.H"
C.include "FL/Fl_Box.H"

main :: IO ()
main = do
  n <- withMany withCString ["onyx"] \xs -> withArray xs \p -> [C.block| int {
    Fl_Window *window = new Fl_Window(340, 180);
    Fl_Box *box = new Fl_Box(20, 40, 300, 100, "Hello, World!");
    box->box(FL_UP_BOX);
    box->labelfont(FL_BOLD + FL_ITALIC);
    box->labelsize(36);
    box->labeltype(FL_SHADOW_LABEL);
    window->end();
    window->show(1, $(char** p));
    return Fl::run();
  } |]
  print n
