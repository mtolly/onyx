{-# LANGUAGE JavaScriptFFI #-}
module Draw
( Canvas, Context
, theCanvas, context2d
, fillRect, setFillStyle, fillText, setFont
, requestAnimationFrame
, Image, loadImage, drawImage
) where

import GHCJS.Types

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

foreign import javascript interruptible
  "requestAnimationFrame($c);"
  requestAnimationFrame :: IO ()

data Image_
type Image = JSRef Image_

foreign import javascript interruptible
  "var i = new Image(); i.addEventListener('load', function(){ $c(i); }); i.src = $1;"
  loadImage :: JSString -> IO Image

foreign import javascript unsafe
  "$6.drawImage($1, $2, $3, $4, $5);"
  drawImage :: Image -> Double -> Double -> Double -> Double -> Context -> IO ()
