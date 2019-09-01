module Draw.Five (drawFiveFast) where

import           Prelude

import           Data.Array              (cons, range, snoc)
import           Data.Foldable           (for_)
import           Data.Int                (round, toNumber)
import           Data.List               as L
import           Data.Maybe              (Maybe (..), isJust, isNothing)
import           Data.Time.Duration      (Seconds, negateDuration)
import           Data.Tuple              (Tuple (..))
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, drawLaneCap, drawLaneBody, fillRect,
                                          secToNum, setFillStyle, drawBeats, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song
import           Style                   (customize)
import Data.Newtype (unwrap)

drawFiveFast :: FiveFast -> BadgeInfo -> Int -> Draw Int
drawFiveFast ff badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      widthFret = customize.widthStandardFret
      widthHighway = 5 * widthFret + 2
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomAsc  = addMax $ addMin $ L.fromFoldable $ Map.doTupleArray (Map.zoomAscDo minSecs maxSecs ff)
      zoomDesc = L.reverse zoomAsc

      addMin L.Nil = let
        s = case Map.lookupLE minSecs ff of
          Nothing -> emptyFiveState
          Just o -> extendFive _.future o.value
        in L.Cons (Tuple minSecs s) L.Nil
      addMin xs@(L.Cons (Tuple _ fs) _) = L.Cons (Tuple minSecs (extendFive _.past fs)) xs

      addMax (L.Cons t@(Tuple _ fs) L.Nil) = L.Cons t $ L.Cons (Tuple maxSecs (extendFive _.future fs)) L.Nil
      addMax (L.Cons t rest) = L.Cons t $ addMax rest
      addMax L.Nil = let
        s = case Map.lookupGE maxSecs ff of
          Nothing -> emptyFiveState
          Just o -> extendFive _.past o.value
        in L.Cons (Tuple maxSecs s) L.Nil

      targetY = secsToPxVert stuff.time
      handedness n = if stuff.app.settings.leftyFlip then 4 - n else n
      drawH = stuff.maxY - stuff.minY

  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber $ widthFret * 5 + 2, height: toNumber drawH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let drawSolos (L.Cons (Tuple s1 (FiveState fs1)) rest@(L.Cons (Tuple s2 _) _)) = do
        when fs1.solo.future do
          let y1 = secsToPxVert s1
              y2 = secsToPxVert s2
          for_ (map (\i -> i * widthFret + 2) $ range 0 4) \offsetX -> do
            fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, width: toNumber $ customize.widthStandardFret - 2, height: toNumber $ y1 - y2 } stuff
        drawSolos rest
      drawSolos _ = pure unit
  drawSolos zoomAsc
  -- Solo edges
  for_ zoomAsc \(Tuple secs (FiveState fs)) -> when fs.solo.now do
    let y = secsToPxVert secs
    setFillStyle customize.highwaySoloEdge stuff
    fillRect { x: toNumber targetX, y: toNumber y, width: toNumber widthHighway, height: 1.0 } stuff
  -- Beats
  -- TODO zoom into the beats map only once for all highways
  drawBeats secsToPxVert
    { x: targetX
    , width: widthHighway
    , minSecs: minSecs
    , maxSecs: maxSecs
    } stuff
  -- Railings
  setFillStyle customize.highwayRailing stuff
  for_ (map (\i -> i * widthFret) $ range 0 5) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 5) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  -- Info for fret-specific stuff
  let colors =
        [ { c: _.open  , lane: _.open  , x:            0 * widthFret + 1, strum: image_gem_open  , hopo: image_gem_open_hopo, tap: image_gem_open_tap
          , shades: customize.sustainPurple, open: true, target: Nothing
          }
        , { c: _.green , lane: _.green , x: handedness 0 * widthFret + 1, strum: image_gem_green , hopo: image_gem_green_hopo, tap: image_gem_green_tap
          , shades: customize.sustainGreen, open: false, target: Just image_highway_target_green
          }
        , { c: _.red   , lane: _.red   , x: handedness 1 * widthFret + 1, strum: image_gem_red   , hopo: image_gem_red_hopo, tap: image_gem_red_tap
          , shades: customize.sustainRed, open: false, target: Just image_highway_target_red
          }
        , { c: _.yellow, lane: _.yellow, x: handedness 2 * widthFret + 1, strum: image_gem_yellow, hopo: image_gem_yellow_hopo, tap: image_gem_yellow_tap
          , shades: customize.sustainYellow, open: false, target: Just image_highway_target_yellow
          }
        , { c: _.blue  , lane: _.blue  , x: handedness 3 * widthFret + 1, strum: image_gem_blue  , hopo: image_gem_blue_hopo, tap: image_gem_blue_tap
          , shades: customize.sustainBlue, open: false, target: Just image_highway_target_blue
          }
        , { c: _.orange, lane: _.orange, x: handedness 4 * widthFret + 1, strum: image_gem_orange, hopo: image_gem_orange_hopo, tap: image_gem_orange_tap
          , shades: customize.sustainOrange, open: false, target: Just image_highway_target_orange
          }
        ]
  -- Lanes
  for_ colors \{x: offsetX, lane: gem, open: isOpen, target: target} -> let
    goCap xs@(L.Cons (Tuple s1 (FiveState fs1)) _) = do
      let this = gem $ unwrap fs1.lanes
          past   = this.past   || (not isOpen && fs1.bre.past  )
          future = this.future || (not isOpen && fs1.bre.future)
      when (past /= future) $ drawLaneCap
        { x: targetX + offsetX + 1 + if isOpen then 1 * widthFret else 0
        , y: secsToPxVert s1
        , width: widthFret - 2 + if isOpen then 2 * widthFret else 0
        } stuff
      goBody xs
    goCap L.Nil = pure unit
    goBody (L.Cons (Tuple s1 (FiveState fs1)) rest@(L.Cons (Tuple s2 _) _)) = do
      when ((gem $ unwrap fs1.lanes).future || (fs1.bre.future && not isOpen)) let
        y1 = secsToPxVert s1
        y2 = secsToPxVert s2
        in drawLaneBody
          { x: targetX + offsetX + 1 + if isOpen then 1 * widthFret else 0
          , y: y2
          , width: widthFret - 2 + if isOpen then 2 * widthFret else 0
          , height: y1 - y2
          } stuff
      goCap rest
    goBody xs = goCap $ L.drop 1 xs
    in goCap zoomAsc
  -- Target
  for_ colors \{x: offsetX, target: targetImage} -> case targetImage of
    Just img -> drawImage img (toNumber $ targetX + offsetX) (toNumber targetY - 5.0) stuff
    Nothing  -> pure unit
  -- Sustains
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades, open: isOpen } -> do
    let offsetX' = if isOpen then 1 * widthFret + 1 else offsetX
        hitAtY = if stuff.app.settings.autoplay then targetY else stuff.maxY + 50
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = stuff.app.settings.autoplay && (targetY < ystart || targetY < yend)
              shades = if energy
                then customize.sustainEnergy
                else normalShades
              h = yend' - ystart' + 1
              extraWidth = if isOpen then 2 * widthFret else 0
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX' + 14, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX' + 22 + extraWidth, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX' + 15, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX' + 16, y: toNumber ystart', width: 5.0 + toNumber extraWidth, height: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX' + 21 + extraWidth, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1 + extraWidth, height: 8.0 } stuff
        drawSustains (L.Cons (Tuple s1 (FiveState fs1)) rest@(L.Cons (Tuple s2 _) _)) = do
          case (getColor $ unwrap fs1.notes).future of
            Nothing -> pure unit
            Just energy -> drawSustainBlock (secsToPxVert s2) (secsToPxVert s1) energy
          drawSustains rest
        drawSustains _ = pure unit
    drawSustains zoomAsc
  -- Sustain endings (draw these first in case a sustain goes right up to next note)
  for_ colors \{ c: getColor, x: offsetX, open: isOpen } -> do
    setFillStyle customize.sustainBorder stuff
    for_ zoomDesc \(Tuple secs (FiveState fs)) -> do
      let o = getColor $ unwrap fs.notes
      when (isJust o.past && isNothing o.future) do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            trailX = if isOpen then 1 * widthFret + 1 else offsetX
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else fillRect
            { x: toNumber $ targetX + trailX + 14
            , y: toNumber $ secsToPxVert secs
            , width: toNumber $ if isOpen then 9 + 2 * widthFret else 9
            , height: 1.0
            } stuff
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, tap: tapImage, shades: shades, open: isOpen } -> do
    for_ zoomDesc \(Tuple secs (FiveState fs)) -> case (getColor $ unwrap fs.notes).now of
      Nothing -> pure unit
      Just sht -> do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            trailX = if isOpen then 1 * widthFret + 1 else offsetX
            extraWidth = if isOpen then 2 * widthFret else 0
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle (shades.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + trailX + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1 + extraWidth, height: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = fs.energy.future
                img = case sht of
                  Strum -> if isEnergy then (if isOpen then image_gem_open_energy      else image_gem_energy     ) else strumImage
                  HOPO  -> if isEnergy then (if isOpen then image_gem_open_energy_hopo else image_gem_energy_hopo) else hopoImage
                  Tap   -> if isEnergy then (if isOpen then image_gem_open_energy_tap  else image_gem_energy_tap ) else tapImage
                x' = targetX + offsetX
                y' = if isOpen then y - 3 else y - 5
            drawImage img (toNumber x') (toNumber y') stuff
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
