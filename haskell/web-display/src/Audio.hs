{-# LANGUAGE JavaScriptFFI #-}
module Audio
( Howl, SoundID, load, play, pause, stop, getPos, setPos, getDuration, setPosSafe
) where

import           GHCJS.Marshal
import           GHCJS.Types

newtype Howl = Howl JSVal

newtype SoundID = SoundID JSVal

foreign import javascript interruptible
  "var h = new Howl({ urls: $1, onload: function(){ $c(h); } });"
  js_load :: JSVal -> IO Howl

load :: [String] -> IO Howl
load files = toJSVal files >>= js_load

foreign import javascript interruptible
  "$1.play($c);"
  play :: Howl -> IO SoundID

foreign import javascript unsafe
  "$2.pause($1);"
  pause :: SoundID -> Howl -> IO ()

foreign import javascript unsafe
  "$2.stop($1);"
  stop :: SoundID -> Howl -> IO ()

foreign import javascript unsafe
  "$2.pos(null, $1)"
  getPos :: SoundID -> Howl -> IO Double

-- | If the sound is paused, this also starts playing it.
foreign import javascript unsafe
  "$3.pos($1, $2);"
  setPos :: Double -> SoundID -> Howl -> IO ()

foreign import javascript unsafe
  "$1._duration"
  getDuration :: Howl -> IO Double

setPosSafe :: Double -> SoundID -> Howl -> IO ()
setPosSafe t sid h = do
  dur <- getDuration h
  setPos (min t dur) sid h
