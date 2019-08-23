module Draw.Common where

import           Prelude

import           Data.Int           (round, floor, toNumber)
import           Data.Int.Bits      (shr)
import           Data.Maybe         (Maybe (..))
import           Data.Time.Duration (Seconds (..))
import           Effect             (Effect)
import           Graphics.Canvas    as C
import           Math               (pi)
import Data.String (toUpper)

import           Images             (ImageID)
import           OnyxMap            as Map
import           Song               (Song(..), Beats(..), Beat(..))
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
-- (except for the Drawer which the JS doesn't touch)
type Settings =
  { parts :: Array
    { partName :: String
    , flexParts :: Array
      { typeName :: String
      , typeIcon :: ImageID
      , typeIconURL :: String
      , typeVertical :: Boolean
      , difficulties :: Array
        { diffName :: String
        , enabled :: Boolean
        , draw :: Drawer
        }
      }
    }
  , autoplay :: Boolean
  , leftyFlip :: Boolean
  , staticVert :: Boolean
  }

type BadgeInfo =
  { name :: String
  , icon :: ImageID
  , difficulty :: String
  }

newtype Drawer = Drawer (BadgeInfo -> Int -> Draw Int)

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

drawLaneCap
  :: forall r
  .  { x :: Int, y :: Int, width :: Int | r }
  -> Draw Unit
drawLaneCap obj stuff = do
  setFillStyle customize.freeformLane stuff
  let rx = toNumber obj.width / 2.0
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber obj.y
    , rx: rx
    , ry: 15.0
    } stuff

drawLaneBody
  :: { x :: Int, y :: Int, width :: Int, height :: Int }
  -> Draw Unit
drawLaneBody obj stuff = do
  setFillStyle customize.freeformLane stuff
  fillRect
    { x: toNumber obj.x
    , y: toNumber obj.y
    , width: toNumber obj.width
    , height: toNumber obj.height
    } stuff

drawLane
  :: { x :: Int, y :: Int, width :: Int, height :: Int }
  -> Draw Unit
drawLane obj stuff = do
  drawLaneBody obj stuff
  drawLaneCap obj stuff
  drawLaneCap (obj { y = obj.y + obj.height }) stuff

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

drawBeats :: (Seconds -> Int) -> { x :: Int, width :: Int, minSecs :: Seconds, maxSecs :: Seconds } -> Draw Unit
drawBeats secsToPxVert dims stuff = do
  setFillStyle customize.highwayLine stuff
  let lines = case stuff.song of
        Song o -> case o.beats of
          Beats o' -> o'.lines
  Map.zoomDescDo dims.minSecs dims.maxSecs lines \secs evt -> do
    let y = secsToPxVert secs
        h = case evt of
          Bar      -> customize.highwayBarHeight
          Beat     -> customize.highwayBeatHeight
          HalfBeat -> customize.highwayHalfBeatHeight
    setFillStyle customize.highwayLine stuff
    fillRect { x: toNumber dims.x, y: toNumber $ y - shr h 1, width: toNumber dims.width, height: toNumber h } stuff

foreign import setBaseline :: C.Context2D -> String -> Effect Unit
foreign import roundRect
  :: C.Context2D
  ->  { x :: Number
      , y :: Number
      , width :: Number
      , height :: Number
      , radius :: Number
      , fill :: Boolean
      , stroke :: Boolean
      }
  -> Effect Unit

drawBadge :: BadgeInfo -> { x :: Number, y :: Number } -> Draw Unit
drawBadge badge posn stuff = do
  let space = toNumber customize.labelIconWidth * (5.0 / 3.0)
      textPart = toUpper badge.name
      textDiff = badge.difficulty
  C.setFont stuff.context $ show (round $ space * 0.3) <> "px Varela Round"
  setBaseline stuff.context "middle"
  C.setTextAlign stuff.context C.AlignLeft
  measurePart <- C.measureText stuff.context textPart
  let iconWidth = space * 0.6
  measureDiff <- C.measureText stuff.context textDiff
  let gap = space * 0.12
      totalWidth = measurePart.width + gap + iconWidth + gap + measureDiff.width
      totalX = posn.x - totalWidth / 2.0
      r n = toNumber $ round n
  -- background
  C.setFillStyle stuff.context "#7a4b0d"
  C.setStrokeStyle stuff.context "black"
  C.setLineWidth stuff.context 1.0
  roundRect stuff.context
    { x: r $ totalX - gap
    , y: r $ posn.y - space * 0.25
    , width: r $ gap + totalWidth + gap
    , height: r $ space * 0.5
    , radius: gap
    , fill: true
    , stroke: true
    }
  -- part name
  C.setFillStyle stuff.context "white"
  C.fillText stuff.context textPart (r totalX) (r $ posn.y + 1.0)
  -- icon
  C.drawImageScale
    stuff.context
    (stuff.getImage badge.icon)
    (r $ totalX + measurePart.width + gap)
    (r $ posn.y - iconWidth / 2.0)
    (r iconWidth)
    (r iconWidth)
  -- difficulty letter
  C.fillText stuff.context textDiff (r $ totalX + measurePart.width + gap + iconWidth + gap) (r $ posn.y + 1.0)
  setBaseline stuff.context "alphabetic"

drawBadgeVertical :: BadgeInfo -> Int -> Int -> Draw Unit
drawBadgeVertical badge trackX trackWidth stuff = do
  windowH <- C.getCanvasHeight stuff.canvas
  let x = toNumber trackX + toNumber trackWidth / 2.0
      y = windowH - toNumber customize.labelPositionVert
  drawBadge badge { x: x, y: y } stuff

drawBadgeHorizontal :: BadgeInfo -> Int -> Int -> Draw Unit
drawBadgeHorizontal badge trackY trackHeight stuff = do
  C.translate stuff.context
    { translateX: toNumber customize.labelPositionHoriz
    , translateY: toNumber trackY + toNumber trackHeight / 2.0
    }
  C.rotate stuff.context (-1.5707963267948966)
  drawBadge badge { x: 0.0, y: 0.0 } stuff
  C.setTransform stuff.context
    { m11: 1.0
    , m12: 0.0
    , m21: 0.0
    , m22: 1.0
    , m31: 0.0
    , m32: 0.0
    }
