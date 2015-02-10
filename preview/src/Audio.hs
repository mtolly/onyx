{-# LANGUAGE JavaScriptFFI #-}
module Audio
( Howl, load, play, pause
) where

import GHCJS.Marshal
import GHCJS.Types

data Howl_
type Howl = JSRef Howl_

foreign import javascript interruptible
  "var h = new Howl({ urls: $1, onload: function(){ $c(h); } });"
  js_load :: JSRef [String] -> IO Howl

load :: [String] -> IO Howl
load files = toJSRef files >>= js_load

foreign import javascript unsafe
  "$1.play();"
  play :: Howl -> IO ()

foreign import javascript unsafe
  "$1.pause();"
  pause :: Howl -> IO ()
