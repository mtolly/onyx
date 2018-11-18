module Draw.Amplitude where

import           Prelude
import           Song               (AmpNote(..), Amplitude(..))
import           Graphics.Canvas    as C
import           Style              (customize)
import           Data.Foldable      (for_)
import           Data.Time.Duration (Seconds, negateDuration)
import           Data.Int           (round, toNumber)
import           Images
import           OnyxMap            as Map
import           Draw.Common        (Draw, drawBeats, drawImage, fillEllipse,
                                     fillRect, secToNum, setFillStyle, BadgeInfo, drawBadgeVertical)

drawAmplitude :: Amplitude -> BadgeInfo -> Int -> Draw Int
drawAmplitude (Amplitude amp) badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      drawH = stuff.maxY - stuff.minY
      widthHighway = 105
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
  -- Highway
  let highwayColor = case amp.instrument of
        "Drums" -> "#dd73ca"
        "Bass" -> "#739ddd"
        "Synth" -> "#cec371"
        "Vocal" -> "#73dd81"
        "Guitar" -> "#ef9558"
        _ -> customize.highway
  setFillStyle highwayColor stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber widthHighway, height: toNumber drawH } stuff
  setFillStyle customize.highwayRailing stuff
  let dividers0 = [0, 103]
      dividers1 = [1, 104]
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
  drawImage image_highway_catch_target (toNumber targetX) (toNumber targetY - 6.0) stuff
  -- Notes
  zoomDesc amp.notes \secs note -> do
    let futureSecs = secToNum $ secs <> negateDuration stuff.time
    if stuff.app.settings.autoplay && futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
            setFillStyle (customize.sustainBlue.hit opacity) stuff
            fillEllipse
              { x: case note of
                L -> toNumber targetX + 19.5
                M -> toNumber targetX + 52.5
                R -> toNumber targetX + 85.5
              , y: toNumber targetY + 0.5
              , rx: 16.5
              , ry: 7.5
              } stuff
          else pure unit
      else do
        -- note is in the future
        let y = secsToPxVert secs
            x = case note of
              L -> targetX + 4
              M -> targetX + 37
              R -> targetX + 70
        drawImage image_gem_catch (toNumber x) (toNumber $ y - 6) stuff
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
