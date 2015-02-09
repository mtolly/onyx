{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHCJS.Types
import GHCJS.Foreign
import Control.Monad (forever)
import Control.Concurrent (threadDelay)

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

main :: IO ()
main = do
  let ctx = context2d theCanvas
      loop i = do
        setFillStyle "red" ctx
        fillRect 0 0 500 500 ctx
        setFillStyle "blue" ctx
        fillRect 25 25 500 500 ctx
        setFillStyle "yellow" ctx
        fillRect 50 50 500 500 ctx
        setFillStyle "white" ctx
        fillText (toJSString $ show i) 10 40 ctx
        threadDelay 16000
        loop $ i + 1
  setFont "20pt Helvetica" ctx
  loop 0
