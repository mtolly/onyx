module Draw.Five (drawFive) where

import           Prelude

import           Data.Array              (cons, range, snoc)
import           Data.Foldable           (for_)
import           Data.Int                (round, toNumber)
import           Data.List               as L
import           Data.Maybe              (Maybe (..))
import           Data.Time.Duration      (Seconds, negateDuration)
import           Data.Tuple              (Tuple (..))
import           Effect.Exception.Unsafe (unsafeThrow)
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, drawLane, fillRect,
                                          secToNum, setFillStyle, drawBeats, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song                    (Five (..), GuitarNoteType (..),
                                          Sustainable (..))
import           Style                   (customize)

drawFive :: Five -> BadgeInfo -> Int -> Draw Int
drawFive (Five five) badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      widthFret = customize.widthStandardFret
      widthHighway = 5 * widthFret + 2
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if stuff.app.settings.leftyFlip then 4 - n else n
      drawH = stuff.maxY - stuff.minY
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber $ widthFret * 5 + 2, height: toNumber drawH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let startsAsSolo = case Map.lookupLE minSecs five.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc five.solo)
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
        when b1 $ for_ (map (\i -> i * widthFret + 2) $ range 0 4) \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, width: toNumber $ customize.widthStandardFret - 2, height: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc five.solo \secs _ -> do
    let y = secsToPxVert secs
    setFillStyle customize.highwaySoloEdge stuff
    fillRect { x: toNumber targetX, y: toNumber y, width: toNumber widthHighway, height: 1.0 } stuff
  -- Beats
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
  -- Lanes
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
  for_ colors \{x: offsetX, lane: gem, open: isOpen, target: target} -> let
    thisLane = case target of
      Nothing -> gem five.lanes -- open note, don't do bre
      Just _  -> Map.union five.bre $ gem five.lanes
    startsAsLane = case Map.lookupLE minSecs thisLane of
      Nothing           -> false
      Just { value: v } -> v
    laneEdges
      = L.fromFoldable
      $ cons (Tuple minSecs startsAsLane)
      $ flip snoc (Tuple maxSecs false)
      $ Map.doTupleArray (zoomAsc thisLane)
    drawLanes L.Nil            = pure unit
    drawLanes (L.Cons _ L.Nil) = pure unit
    drawLanes (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
      let y1 = secsToPxVert s1
          y2 = secsToPxVert s2
      when b1 $ drawLane
        { x: targetX + offsetX + 1 + if isOpen then 1 * widthFret else 0
        , y: y2
        , width: widthFret - 2 + if isOpen then 2 * widthFret else 0
        , height: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Target
  for_ colors \{x: offsetX, target: targetImage} -> case targetImage of
    Just img -> drawImage img (toNumber $ targetX + offsetX) (toNumber targetY - 5.0) stuff
    Nothing  -> pure unit
  -- Sustains
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades, open: isOpen } -> do
    let thisColor = getColor five.notes
        offsetX' = if isOpen then 1 * widthFret + 1 else offsetX
        isEnergy secs = case Map.lookupLE secs five.energy of
          Nothing           -> false
          Just { value: v } -> v
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
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisColor of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) stuff.maxY $ isEnergy secsStart
            go false rest
          _ -> unsafeThrow "during grybo drawing: found a sustain end not preceded by sustain start"
        go true (L.Cons (Tuple _ SustainEnd) rest) = go false rest
        go _ (L.Cons (Tuple _ (Note _)) rest) = go false rest
        go _ (L.Cons (Tuple secsStart (Sustain _)) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> stuff.minY
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) $ isEnergy secsStart
          go true rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisColor) of
      L.Nil -> case Map.lookupLT (pxToSecsVert stuff.maxY) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain _ } ->
          drawSustainBlock stuff.minY stuff.maxY $ isEnergy secsStart
        _ -> pure unit
      events -> go false events
  -- Sustain endings (draw these first in case a sustain goes right up to next note)
  for_ colors \{ c: getColor, x: offsetX, open: isOpen } -> do
    setFillStyle customize.sustainBorder stuff
    zoomDesc (getColor five.notes) \secs evt -> case evt of
      SustainEnd -> do
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
      _ -> pure unit
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, tap: tapImage, shades: shades, open: isOpen } -> do
    zoomDesc (getColor five.notes) \secs evt -> let
      withNoteType sht = do
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
                isEnergy = case Map.lookupLE secs five.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                img = case sht of
                  Strum -> if isEnergy then (if isOpen then image_gem_open_energy      else image_gem_energy     ) else strumImage
                  HOPO  -> if isEnergy then (if isOpen then image_gem_open_energy_hopo else image_gem_energy_hopo) else hopoImage
                  Tap   -> if isEnergy then (if isOpen then image_gem_open_energy_tap  else image_gem_energy_tap ) else tapImage
                x' = targetX + offsetX
                y' = if isOpen then y - 3 else y - 5
            drawImage img (toNumber x') (toNumber y') stuff
      in case evt of
        Note    sht -> withNoteType sht
        Sustain sht -> withNoteType sht
        SustainEnd  -> pure unit
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
