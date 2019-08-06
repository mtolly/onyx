module Draw.Dance where

import           Prelude
import           Song               (Dance(..), Sustainable(..), DanceType(..))
import           Graphics.Canvas    as C
import           Style              (customize)
import           Data.Foldable      (for_)
import           Data.Time.Duration (Seconds, negateDuration)
import           Data.Int           (round, toNumber, quot)
import           Images
import           Math               (pi)
import           OnyxMap            as Map
import           Draw.Common        (onContext, Draw, drawBeats, drawImage, fillEllipse,
                                     fillRect, secToNum, setFillStyle, BadgeInfo, drawBadgeVertical)

drawRotated
  :: Int
  -> ImageID
  -> { x :: Number, y :: Number, w :: Number, h :: Number }
  -> Draw Unit
drawRotated rots iid loc dstuff = do
  C.withContext dstuff.context do
    C.translate dstuff.context
      { translateX: loc.x + loc.w / 2.0
      , translateY: loc.y + loc.h / 2.0
      }
    C.rotate dstuff.context $ toNumber rots * (pi / 2.0)
    drawImage iid (loc.w * -0.5) (loc.h * -0.5) dstuff

drawDance :: Dance -> BadgeInfo -> Int -> Draw Int
drawDance (Dance dance) badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      drawH = stuff.maxY - stuff.minY
      arrowWidth = 29
      halfArrowWidth = quot arrowWidth 2
      buffer = 4
      widthHighway = buffer * 5 + arrowWidth * 4
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber widthHighway, height: toNumber drawH } stuff
  setFillStyle customize.highwayRailing stuff
  let dividers0 = [0, widthHighway - 2]
      dividers1 = [1, widthHighway - 1]
  for_ dividers0 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ dividers1 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  -- Beats
  drawBeats secsToPxVert
    { x: targetX + 2
    , width: widthHighway - 4
    , minSecs: minSecs
    , maxSecs: maxSecs
    } stuff
  -- Target
  drawRotated 3 image_highway_target_arrow
    { x: toNumber $ targetX + buffer
    , y: toNumber $ targetY - halfArrowWidth
    , w: toNumber arrowWidth
    , h: toNumber arrowWidth
    } stuff
  drawRotated 2 image_highway_target_arrow
    { x: toNumber $ targetX + buffer * 2 + arrowWidth
    , y: toNumber $ targetY - halfArrowWidth
    , w: toNumber arrowWidth
    , h: toNumber arrowWidth
    } stuff
  drawImage     image_highway_target_arrow
    (toNumber $ targetX + buffer * 3 + arrowWidth * 2)
    (toNumber $ targetY - halfArrowWidth)
    stuff
  drawRotated 1 image_highway_target_arrow
    { x: toNumber $ targetX + buffer * 4 + arrowWidth * 3
    , y: toNumber $ targetY - halfArrowWidth
    , w: toNumber arrowWidth
    , h: toNumber arrowWidth
    } stuff
  -- Notes
  let arrows =
        [ { fn: _.left , i: 0, rotate: 3 }
        , { fn: _.down , i: 1, rotate: 2 }
        , { fn: _.up   , i: 2, rotate: 0 }
        , { fn: _.right, i: 3, rotate: 1 }
        ]
  for_ arrows \obj -> do
    zoomDesc (obj.fn dance.notes) \secs evt -> let
      withNoteType ntype = do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                let opacity = (futureSecs + 0.1) / 0.05
                    x' = targetX + buffer * (1 + obj.i) + arrowWidth * obj.i
                    y' = targetY - halfArrowWidth
                C.setGlobalAlpha stuff.context opacity
                case obj.rotate of
                  0 -> drawImage image_highway_target_arrow_hit (toNumber x') (toNumber y') stuff
                  n -> drawRotated n image_highway_target_arrow_hit
                    { x: toNumber x'
                    , y: toNumber y'
                    , w: toNumber arrowWidth
                    , h: toNumber arrowWidth
                    } stuff
                C.setGlobalAlpha stuff.context 1.0
              else pure unit
          else do
            let y = secsToPxVert secs
                img = case ntype of
                  NoteMine -> image_gem_mine
                  _        -> image_gem_red_arrow -- TODO colors, lifts
                x' = targetX + buffer * (1 + obj.i) + arrowWidth * obj.i
                y' = y - halfArrowWidth
            case obj.rotate of
              0 -> drawImage img (toNumber x') (toNumber y') stuff
              n -> drawRotated n img
                { x: toNumber x'
                , y: toNumber y'
                , w: toNumber arrowWidth
                , h: toNumber arrowWidth
                } stuff
      in case evt of
        Note    ntype -> withNoteType ntype
        Sustain ntype -> withNoteType ntype
        SustainEnd    -> pure unit
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
