{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.Parser.Report as Report

data Canvas_
type Canvas = JSRef Canvas_

data Context_
type Context = JSRef Context_

foreign import javascript unsafe
  "document.getElementById('the-canvas')"
  theCanvas :: Canvas

foreign import javascript unsafe
  "$1.getContext('2d')"
  context2d :: Canvas -> Context

foreign import javascript unsafe
  "$5.fillRect($1, $2, $3, $4);"
  fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe
  "$2.fillStyle = $1;"
  setFillStyle :: JSString -> Context -> IO ()

foreign import javascript unsafe
  "$4.fillText($1, $2, $3);"
  fillText :: JSString -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe
  "$2.font = $1;"
  setFont :: JSString -> Context -> IO ()

data Howl_
type Howl = JSRef Howl_

foreign import javascript interruptible
  "(function(){ var h = new Howl({urls: $1, onload: function(){ $c(h); }}); })();"
  loadHowl :: JSRef [String] -> IO Howl

foreign import javascript unsafe
  "$1.play()"
  playHowl :: Howl -> IO ()

foreign import javascript interruptible
  "requestAnimationFrame($c);"
  requestAnimationFrame :: IO ()

foreign import javascript interruptible
  "hs_ajaxBinary($1, $c);"
  ajaxBinary :: JSString -> IO (JSRef ())

ajaxByteString :: String -> IO B.ByteString
ajaxByteString s = ajaxBinary (toJSString s) >>= bufferByteString 0 0

main :: IO ()
main = do
  howl <- toJSRef ["another-day/song-countin.ogg"] >>= loadHowl
  putStrLn "Loaded Howl."
  bs <- ajaxByteString "another-day/notes.mid"
  putStrLn "Got MIDI as ByteString."
  case Report.result $ Load.maybeFromByteString $ BL.fromStrict bs of
    Left err -> error err
    Right mid -> let
      ctx = context2d theCanvas
      loop :: Int -> IO ()
      loop i = do
        setFillStyle "red" ctx
        fillRect 0 0 500 500 ctx
        setFillStyle "blue" ctx
        fillRect 25 25 500 500 ctx
        setFillStyle "yellow" ctx
        fillRect 50 50 500 500 ctx
        setFillStyle "white" ctx
        setFont "20pt Helvetica" ctx
        fillText (toJSString $ show i) 10 35 ctx
        requestAnimationFrame
        loop $ i + 1
      in do
        let F.Cons _ _ trks = mid
        putStrLn $ "Parsed MIDI with " ++ show (length trks) ++ " tracks."
        playHowl howl
        loop 0
