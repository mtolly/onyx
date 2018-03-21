module Draw.Six (drawSix) where

import           Prelude

import           Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import           Data.Array                         (cons, range, snoc)
import           Data.Foldable                      (for_)
import           Data.Int                           (round, toNumber)
import           Data.List                          as L
import           Data.Maybe                         (Maybe (..))
import           Data.Time.Duration                 (Seconds)
import           Data.Tuple                         (Tuple (..))
import           Graphics.Canvas                    as C

import           Draw.Common                        (Draw, drawImage, fillRect,
                                                     secToNum, setFillStyle)
import           Images                             (ImageID (..))
import           OnyxMap                            as Map
import           Song                               (Beat (..), Beats (..),
                                                     GuitarNoteType (..),
                                                     Six (..), Song (..),
                                                     Sustainable (..))
import           Style                              (customize)

data SixColor
  = SixOpen
  | SixBlack
  | SixWhite
  | SixBoth

drawSix :: forall e. Six -> Int -> Draw e Int
drawSix (Six six) targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) + stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs - stuff.time)
      widthFret = customize.widthStandardFret
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if customize.leftyFlip then 2 - n else n
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: 0.0, w: 110.0, h: toNumber windowH } stuff
  setFillStyle customize.highwayRailing stuff
  for_ (map (\i -> i * widthFret) $ range 0 3) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 3) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
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
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: toNumber $ widthFret - 2, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc six.solo \secs _ -> do
    drawImage Image_highway_ghl_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- TODO lanes, bre
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
    case evt of
      Bar      -> drawImage Image_highway_ghl_bar      (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage Image_highway_ghl_beat     (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> pure unit
  -- Target
  drawImage Image_highway_ghl_target (toNumber targetX) (toNumber targetY - 5.0) stuff
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
        SixWhite -> customize.sustainWhiteGHL
        SixBoth  -> customize.sustainBothGHL
        SixBlack -> customize.sustainBlackGHL
        SixOpen  -> customize.sustainOpenGHL
      getGemImages sc = case sc of
        SixBlack -> { strum: Image_gem_black, hopo: Image_gem_black_hopo, tap: Image_gem_black_tap, energy: Image_gem_ghl_energy }
        SixWhite -> { strum: Image_gem_white, hopo: Image_gem_white_hopo, tap: Image_gem_white_tap, energy: Image_gem_ghl_energy }
        SixBoth  -> { strum: Image_gem_blackwhite, hopo: Image_gem_blackwhite_hopo, tap: Image_gem_blackwhite_tap, energy: Image_gem_ghl_energy }
        SixOpen  -> { strum: Image_gem_openghl, hopo: Image_gem_openghl_hopo, tap: Image_gem_openghl_tap, energy: Image_gem_openghl_energy }
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let thisEvents = getEvents six.notes
        offsetX' = case thisColor of
          SixOpen -> 1 * widthFret + 1
          _       -> offsetX
        isEnergy secs = case Map.lookupLE secs six.energy of
          Nothing           -> false
          Just { value: v } -> v
        hitAtY = if customize.autoplay then targetY else windowH + 100
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = customize.autoplay && (targetY < ystart || targetY < yend)
              shades = if energy
                then customize.sustainEnergy
                else getShades thisColor
              h = yend' - ystart' + 1
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX' + 14, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX' + 22, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX' + 15, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX' + 16, y: toNumber ystart', w: 5.0, h: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX' + 21, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisEvents of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) windowH $ isEnergy secsStart
            go false rest
          _ -> unsafeThrow "during ghl drawing: found a sustain end not preceded by sustain start"
        go true (L.Cons (Tuple _ SustainEnd) rest) = go false rest
        go _ (L.Cons (Tuple _ (Note _)) rest) = go false rest
        go _ (L.Cons (Tuple secsStart (Sustain _)) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> 0
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) $ isEnergy secsStart
          go true rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisEvents) of
      L.Nil -> case Map.lookupLT (pxToSecsVert windowH) thisEvents of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain _ } ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> pure unit
      events -> go false events
  -- Sustain ends
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    zoomDesc (getEvents six.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs - stuff.time
            trailX = case thisColor of
              SixOpen -> 1 * widthFret + 1
              _       -> offsetX
        if customize.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else drawImage Image_sustain_end
            (toNumber $ targetX + trailX)
            (toNumber $ secsToPxVert secs)
            stuff
      _ -> pure unit
  -- Notes
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let {strum: strumImage, hopo: hopoImage, tap: tapImage, energy: energyOverlay} = getGemImages thisColor
    zoomDesc (getEvents six.notes) \secs evt -> let
      withNoteType sht = do
        let futureSecs = secToNum $ secs - stuff.time
            trailX = case thisColor of
              SixOpen -> 1 * widthFret + 1
              _       -> offsetX
        if customize.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle ((getShades thisColor).hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + trailX + 1, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
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
  pure $ targetX + (3 * widthFret + 2) + customize.marginWidth
