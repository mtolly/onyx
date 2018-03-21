module Draw.Common where

import           Prelude

import           Control.Monad.Eff  (Eff)
import           Data.Int           (floor, toNumber)
import           Data.Maybe         (Maybe (..))
import           Data.Set           as Set
import           Data.Time.Duration (Seconds (..))
import           Data.Tuple         (Tuple)
import           Graphics.Canvas    as C
import           Math               (pi)

import           Images             (ImageID)
import           OnyxMap            as Map
import           Song               (FlexPart, Song)
import           Style              (customize)

type Settings = Set.Set (Tuple String FlexPart)

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
  , pxToSecsVert :: Int -> Seconds -- pixels from bottom -> now-offset in seconds
  , secsToPxVert :: Seconds -> Int -- now-offset in seconds -> pixels from bottom
  , pxToSecsHoriz :: Int -> Seconds -- pixels from left -> now-offset in seconds
  , secsToPxHoriz :: Seconds -> Int -- now-offset in seconds -> pixels from left
  }

type Draw e a = DrawStuff -> Eff (canvas :: C.CANVAS | e) a

setFillStyle :: forall e. String -> Draw e Unit
setFillStyle s = onContext $ C.setFillStyle s

fillRect :: forall e. C.Rectangle -> Draw e Unit
fillRect rect = onContext \ctx -> C.fillRect ctx rect

strokeRect :: forall e. C.Rectangle -> Draw e Unit
strokeRect rect = onContext \ctx -> C.strokeRect ctx rect

fillEllipse :: forall e. { x :: Number, y :: Number, rx :: Number, ry :: Number } -> Draw e Unit
fillEllipse o dstuff = do
  let ctx = dstuff.context
      width = o.rx * 2.0
      height = o.ry * 2.0
      maxDiameter = max width height
      scaleX = width / maxDiameter
      scaleY = height / maxDiameter
  void $ C.save ctx
  void $ C.translate { translateX: o.x, translateY: o.y } ctx
  void $ C.scale { scaleX: scaleX, scaleY: scaleY } ctx
  void $ C.beginPath ctx
  void $ C.arc ctx { x: 0.0, y: 0.0, r: maxDiameter / 2.0, start: 0.0, end: 2.0 * pi}
  void $ C.fill ctx
  void $ C.closePath ctx
  void $ C.restore ctx

fillCircle :: forall e. { x :: Number, y :: Number, r :: Number } -> Draw e Unit
fillCircle o = fillEllipse { x: o.x, y: o.y, rx: o.r, ry: o.r }

drawImage :: forall e. ImageID -> Number -> Number -> Draw e Unit
drawImage iid x y dstuff =
  onContext (\ctx -> C.drawImage ctx (dstuff.getImage iid) x y) dstuff

onContext :: forall e. (C.Context2D -> Eff (canvas :: C.CANVAS | e) C.Context2D) -> Draw e Unit
onContext act dstuff = void $ act dstuff.context

measureText :: forall e. String -> Draw e C.TextMetrics
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
  :: forall e
  .  { x :: Int, y :: Int, w :: Int, h :: Int }
  -> Draw e Unit
drawLane obj stuff = do
  setFillStyle customize.freeformLane stuff
  fillRect
    { x: toNumber obj.x
    , y: toNumber obj.y
    , w: toNumber obj.w
    , h: toNumber obj.h
    } stuff
  let rx = toNumber obj.w / 2.0
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber obj.y
    , rx: rx
    , ry: 15.0
    } stuff
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber $ obj.y + obj.h
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
