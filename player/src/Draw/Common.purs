module Draw.Common where

import           Prelude

import           Data.Int           (floor, toNumber)
import           Data.Maybe         (Maybe (..))
import           Data.Set           as Set
import           Data.Time.Duration (Seconds (..))
import           Data.Tuple         (Tuple)
import           Effect             (Effect)
import           Graphics.Canvas    as C
import           Math               (pi)

import           Images             (ImageID)
import           OnyxMap            as Map
import           Song               (FlexPart, Song)
import           Style              (customize)

type App =
  { time :: AppTime
  , menuOpen :: Boolean
  , settings :: Settings
  }

data AppTime
  = Paused
    { pausedSongTime :: Seconds
    }
  | Playing
    { startedPageTime :: Seconds
    , startedSongTime :: Seconds
    }

-- this is a plain JS object because we pass it to ffi code to make the menu
type Settings =
  { parts :: Array
    { partName :: String
    , flexParts :: Array
      { partType :: String
      , enabled :: Boolean
      }
    }
  }

type DrawStuff =
  { time :: Seconds
  , app :: App
  , song :: Song
  , getImage :: ImageID -> C.CanvasImageSource
  , canvas :: C.CanvasElement
  , context :: C.Context2D
  , pxToSecsVert :: Int -> Seconds -- pixels from bottom -> now-offset in seconds
  , secsToPxVert :: Seconds -> Int -- now-offset in seconds -> pixels from bottom
  , pxToSecsHoriz :: Int -> Seconds -- pixels from left -> now-offset in seconds
  , secsToPxHoriz :: Seconds -> Int -- now-offset in seconds -> pixels from left
  , minY :: Int
  , maxY :: Int
  }

type Draw a = DrawStuff -> Effect a

setFillStyle :: String -> Draw Unit
setFillStyle s = onContext \ctx -> C.setFillStyle ctx s

fillRect :: C.Rectangle -> Draw Unit
fillRect rect = onContext \ctx -> C.fillRect ctx rect

strokeRect :: C.Rectangle -> Draw Unit
strokeRect rect = onContext \ctx -> C.strokeRect ctx rect

fillEllipse :: { x :: Number, y :: Number, rx :: Number, ry :: Number } -> Draw Unit
fillEllipse o dstuff = do
  let ctx = dstuff.context
      width = o.rx * 2.0
      height = o.ry * 2.0
      maxDiameter = max width height
      scaleX = width / maxDiameter
      scaleY = height / maxDiameter
  C.save ctx
  C.translate ctx { translateX: o.x, translateY: o.y }
  C.scale ctx { scaleX: scaleX, scaleY: scaleY }
  C.beginPath ctx
  C.arc ctx { x: 0.0, y: 0.0, radius: maxDiameter / 2.0, start: 0.0, end: 2.0 * pi }
  C.fill ctx
  C.closePath ctx
  C.restore ctx

fillCircle :: { x :: Number, y :: Number, r :: Number } -> Draw Unit
fillCircle o = fillEllipse { x: o.x, y: o.y, rx: o.r, ry: o.r }

drawImage :: ImageID -> Number -> Number -> Draw Unit
drawImage iid x y dstuff =
  onContext (\ctx -> C.drawImage ctx (dstuff.getImage iid) x y) dstuff

onContext :: (C.Context2D -> Effect Unit) -> Draw Unit
onContext act dstuff = act dstuff.context

measureText :: String -> Draw C.TextMetrics
measureText str dstuff = C.measureText dstuff.context str

showTimestamp :: Seconds -> String
showTimestamp (Seconds s) = let
  mins = floor $ s / 60.0
  secs = floor $ s - toNumber (mins * 60)
  msecs = floor $ (s - toNumber (mins * 60) - toNumber secs) * 1000.0
  pad2 str n = if n < 10 then str <> show n else show n
  pad3 str n = if n < 100 then str <> pad2 str n else show n
  in show mins <> ":" <> pad2 "0" secs <> "." <> pad3 "0" msecs

drawLane
  :: { x :: Int, y :: Int, width :: Int, height :: Int }
  -> Draw Unit
drawLane obj stuff = do
  setFillStyle customize.freeformLane stuff
  fillRect
    { x: toNumber obj.x
    , y: toNumber obj.y
    , width: toNumber obj.width
    , height: toNumber obj.height
    } stuff
  let rx = toNumber obj.width / 2.0
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber obj.y
    , rx: rx
    , ry: 15.0
    } stuff
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber $ obj.y + obj.height
    , rx: rx
    , ry: 15.0
    } stuff

zoomAscDoPadding :: forall k a m. (Ord k) => (Monad m) => k -> k -> Map.Map k a -> (k -> a -> m Unit) -> m Unit
zoomAscDoPadding k1 k2 m act = do
  case Map.lookupLE k1 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> do
      -- hack for vocal slides: two padding events before the left edge
      -- so that "(note start)+ (note end) (screen left edge) (note start)" works
      case Map.lookupLT k m of
        Nothing -> pure unit
        Just { key: k', value: v' } -> act k' v'
      act k v
  Map.zoomAscDo k1 k2 m act
  case Map.lookupGE k2 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v

zoomDescDoPadding :: forall k a m. (Ord k) => (Monad m) => k -> k -> Map.Map k a -> (k -> a -> m Unit) -> m Unit
zoomDescDoPadding k1 k2 m act = do
  case Map.lookupGE k2 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v
  Map.zoomDescDo k1 k2 m act
  case Map.lookupLE k1 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v

slide :: Number -> Number -> Number -> Number -> Number -> Number
slide t1 t2 tx v1 v2 = if t1 == t2
  then (v1 + v2) / 2.0
  else v1 + (v2 - v1) * ((tx - t1) / (t2 - t1))

secToNum :: Seconds -> Number
secToNum (Seconds n) = n
