module Draw where

import Prelude
import qualified Graphics.Canvas as C
import Data.Time
import Images
import Control.Monad.Reader.Trans
import Control.Monad.Eff
import Data.DOM.Simple.Window
import Data.Int (toNumber)
import DOM

import Song

type Settings =
  { seeGuitar  :: Boolean
  , seeBass    :: Boolean
  , seeKeys    :: Boolean
  , seeProKeys :: Boolean
  , seeDrums   :: Boolean
  }

data App
  = Paused
    { pausedSongTime :: Seconds
    , settings :: Settings
    }
  | Playing
    { startedPageTime :: Seconds
    , startedSongTime :: Seconds
    , settings :: Settings
    }

type DrawStuff =
  { time :: Seconds
  , app :: App
  , song :: Song
  , getImage :: ImageID -> C.CanvasImageSource
  , canvas :: C.CanvasElement
  , context :: C.Context2D
  }

type Draw e = ReaderT DrawStuff (Eff (canvas :: C.Canvas | e))

setFillStyle :: forall e. String -> Draw e Unit
setFillStyle s = do
  ctx <- map (\x -> x.context) ask
  lift $ void $ C.setFillStyle s ctx

fillRect :: forall e. C.Rectangle -> Draw e Unit
fillRect rect = do
  ctx <- map (\x -> x.context) ask
  lift $ void $ C.fillRect ctx rect

drawImage :: forall e. ImageID -> Number -> Number -> Draw e Unit
drawImage iid x y = do
  ctx <- map (\x -> x.context) ask
  img <- map (\x -> x.getImage) ask
  lift $ void $ C.drawImage ctx (img iid) x y

draw :: forall e. Draw (dom :: DOM | e) Unit
draw = do
  stuff <- ask
  windowW <- lift $ innerWidth  globalWindow
  windowH <- lift $ innerHeight globalWindow
  lift $ C.setCanvasWidth  windowW stuff.canvas
  lift $ C.setCanvasHeight windowH stuff.canvas
  setFillStyle "rgb(54,59,123)"
  fillRect { x: 0.0, y: 0.0, w: windowW, h: windowH }
  let playPause = case stuff.app of
        Paused  _ -> Image_button_play
        Playing _ -> Image_button_pause
  drawImage playPause (toNumber _M) (windowH - toNumber _M - toNumber _B)
  let timelineH = windowH - 3.0 * toNumber _M - toNumber _B - 2.0
      filled = case stuff.time / (case stuff.song of Song o -> o.end) of
        Seconds n -> n
  setFillStyle "black"
  fillRect { x: toNumber _M, y: toNumber _M, w: toNumber _B, h: timelineH + 2.0 }
  setFillStyle "white"
  fillRect { x: toNumber _M + 1.0, y: toNumber _M + 1.0, w: toNumber _B - 2.0, h: timelineH }
  setFillStyle "rgb(100,130,255)"
  fillRect
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , w: toNumber _B - 2.0
    , h: timelineH * filled
    }

_M :: Int
_M = 20 -- margin

_B :: Int
_B = 41 -- height/width of buttons
