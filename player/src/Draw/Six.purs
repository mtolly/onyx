module Draw.Six (drawSix) where

import           Prelude

import           Data.Array              (cons, range, snoc)
import           Data.Foldable           (for_)
import           Data.Int                (round, toNumber)
import           Data.List               as L
import           Data.Maybe              (Maybe (..))
import           Data.Time.Duration      (Seconds, negateDuration)
import           Data.Tuple              (Tuple (..))
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, fillRect, secToNum,
                                          setFillStyle, drawBeats, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song                    (GuitarNoteType (..), Six (..),
                                          Sustainable (..))
import           Style                   (customize)

data SixColor
  = SixOpen
  | SixBlack
  | SixWhite
  | SixBoth

drawSix :: Six -> BadgeInfo -> Int -> Draw Int
drawSix (Six six) badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      widthFret = customize.widthStandardFret
      widthHighway = 3 * widthFret + 2
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if stuff.app.settings.leftyFlip then 2 - n else n
      drawH = stuff.maxY - stuff.minY
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: 110.0, height: toNumber drawH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let startsAsSolo = case Map.lookupLE minSecs six.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc six.solo)
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
        when b1 $ for_ (map (\i -> i * widthFret + 2) $ range 0 2) \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, width: toNumber $ widthFret - 2, height: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc six.solo \secs _ -> do
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
  for_ (map (\i -> i * widthFret) $ range 0 3) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 3) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  -- TODO lanes, bre
  -- Target
  drawImage image_highway_ghl_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  let colors =
        [ { c: _.open, x:            0 * widthFret + 1, color: SixOpen  }
        , { c: _.b1  , x: handedness 0 * widthFret + 1, color: SixBlack }
        , { c: _.b2  , x: handedness 1 * widthFret + 1, color: SixBlack }
        , { c: _.b3  , x: handedness 2 * widthFret + 1, color: SixBlack }
        , { c: _.w1  , x: handedness 0 * widthFret + 1, color: SixWhite }
        , { c: _.w2  , x: handedness 1 * widthFret + 1, color: SixWhite }
        , { c: _.w3  , x: handedness 2 * widthFret + 1, color: SixWhite }
        , { c: _.bw1 , x: handedness 0 * widthFret + 1, color: SixBoth  }
        , { c: _.bw2 , x: handedness 1 * widthFret + 1, color: SixBoth  }
        , { c: _.bw3 , x: handedness 2 * widthFret + 1, color: SixBoth  }
        ]
      getShades sc = case sc of
        SixWhite -> if stuff.app.settings.leftyFlip then customize.sustainBlackGHL else customize.sustainWhiteGHL
        SixBoth  -> customize.sustainBothGHL
        SixBlack -> if stuff.app.settings.leftyFlip then customize.sustainWhiteGHL else customize.sustainBlackGHL
        SixOpen  -> customize.sustainOpenGHL
      blackImages = { strum: image_gem_black, hopo: image_gem_black_hopo, tap: image_gem_black_tap, energy: image_gem_ghl_energy }
      whiteImages = { strum: image_gem_white, hopo: image_gem_white_hopo, tap: image_gem_white_tap, energy: image_gem_ghl_energy }
      getGemImages sc = case sc of
        SixBlack -> if stuff.app.settings.leftyFlip then whiteImages else blackImages
        SixWhite -> if stuff.app.settings.leftyFlip then blackImages else whiteImages
        SixBoth  -> if stuff.app.settings.leftyFlip
          then { strum: image_gem_whiteblack, hopo: image_gem_whiteblack_hopo, tap: image_gem_whiteblack_tap, energy: image_gem_ghl_energy }
          else { strum: image_gem_blackwhite, hopo: image_gem_blackwhite_hopo, tap: image_gem_blackwhite_tap, energy: image_gem_ghl_energy }
        SixOpen  -> { strum: image_gem_openghl, hopo: image_gem_openghl_hopo, tap: image_gem_openghl_tap, energy: image_gem_openghl_energy }
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let thisEvents = getEvents six.notes
        offsetX' = case thisColor of
          SixOpen -> 1 * widthFret + 1
          _       -> offsetX
        isEnergy secs = case Map.lookupLE secs six.energy of
          Nothing           -> false
          Just { value: v } -> v
        hitAtY = if stuff.app.settings.autoplay then targetY else stuff.maxY + 50
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = stuff.app.settings.autoplay && (targetY < ystart || targetY < yend)
              shades = if energy
                then customize.sustainEnergy
                else getShades thisColor
              h = yend' - ystart' + 1
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX' + 14, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX' + 22, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX' + 15, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX' + 16, y: toNumber ystart', width: 5.0, height: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX' + 21, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff

        go Nothing L.Nil = pure unit
        go sust (L.Cons (Tuple secs v) rest) = do
          case sust of
            Nothing -> pure unit
            Just secsStart -> drawSustainBlock (secsToPxVert secs) (secsToPxVert secsStart) $ isEnergy secsStart
          let newSustain = case v of
                Sustain _ -> Just secs
                _         -> Nothing
          go newSustain rest
        go (Just secsStart) L.Nil = do
          drawSustainBlock stuff.minY (secsToPxVert secsStart) $ isEnergy secsStart

        startSustain = case Map.lookupLT (pxToSecsVert stuff.maxY) thisEvents of
          Just { key: secsStart, value: Sustain _ } -> Just secsStart
          _                                         -> Nothing

    go startSustain $ L.fromFoldable $ Map.doTupleArray (zoomAsc thisEvents)
  -- Sustain ends
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    setFillStyle customize.sustainBorder stuff
    zoomDesc (getEvents six.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            trailX = case thisColor of
              SixOpen -> 1 * widthFret + 1
              _       -> offsetX
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else fillRect
            { x: toNumber $ targetX + trailX + 14
            , y: toNumber $ secsToPxVert secs
            , width: 9.0
            , height: 1.0
            } stuff
      _ -> pure unit
  -- Notes
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let {strum: strumImage, hopo: hopoImage, tap: tapImage, energy: energyOverlay} = getGemImages thisColor
    zoomDesc (getEvents six.notes) \secs evt -> let
      withNoteType sht = do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            trailX = case thisColor of
              SixOpen -> 1 * widthFret + 1
              _       -> offsetX
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle ((getShades thisColor).hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + trailX + 1, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = case Map.lookupLE secs six.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                img = case sht of
                  Strum -> strumImage
                  HOPO  -> hopoImage
                  Tap   -> tapImage
                x' = targetX + offsetX
                y' = case thisColor of
                  SixOpen -> y - 3
                  _       -> y - 5
            drawImage img (toNumber x') (toNumber y') stuff
            when isEnergy $ drawImage energyOverlay (toNumber x') (toNumber y') stuff
      in case evt of
        Note    sht -> withNoteType sht
        Sustain sht -> withNoteType sht
        SustainEnd  -> pure unit
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
