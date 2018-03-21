module Draw.Protar (drawProtar) where

import           Prelude

import           Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import           Data.Array                         (cons, index, range, snoc)
import           Data.Foldable                      (for_)
import           Data.Int                           (round, toNumber)
import           Data.List                          as L
import           Data.Maybe                         (Maybe (..), isNothing)
import           Data.Time.Duration                 (Seconds)
import           Data.Tuple                         (Tuple (..))
import           Graphics.Canvas                    as C

import           Draw.Common                        (Draw, drawImage, drawLane,
                                                     fillRect, secToNum,
                                                     setFillStyle)
import           Images                             (ImageID (..), protarFrets)
import           OnyxMap                            as Map
import           Song                               (Beat (..), Beats (..),
                                                     GuitarNoteType (..),
                                                     Protar (..),
                                                     ProtarNote (..), Song (..),
                                                     Sustainable (..))
import           Style                              (customize)

drawProtar :: forall e. Protar -> Int -> Draw e Int
drawProtar (Protar protar) targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) + stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs - stuff.time)
      widthFret = customize.widthProtarFret
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if customize.leftyFlip then 5 - n else n
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: 0.0, w: toNumber $ widthFret * 6 + 2, h: toNumber windowH } stuff
  setFillStyle customize.highwayRailing stuff
  for_ (map (\i -> i * widthFret) $ range 0 6) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 6) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let startsAsSolo = case Map.lookupLE minSecs protar.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc protar.solo)
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
        when b1 $ for_ (map (\i -> i * widthFret + 2) $ range 0 5) \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: toNumber $ widthFret - 2, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc protar.solo \secs _ -> do
    drawImage Image_highway_grybo_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        [ {x: handedness 0 * widthFret + 2, gem: _.s6}
        , {x: handedness 1 * widthFret + 2, gem: _.s5}
        , {x: handedness 2 * widthFret + 2, gem: _.s4}
        , {x: handedness 3 * widthFret + 2, gem: _.s3}
        , {x: handedness 4 * widthFret + 2, gem: _.s2}
        , {x: handedness 5 * widthFret + 2, gem: _.s1}
        ]
  for_ lanes \{x: offsetX, gem: gem} -> let
    thisLane = Map.union protar.bre $ gem protar.lanes
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
        { x: targetX + offsetX
        , y: y2
        , w: widthFret - 2
        , h: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
    case evt of
      Bar      -> drawImage Image_highway_grybo_bar       (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage Image_highway_protar_beat     (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> drawImage Image_highway_protar_halfbeat (toNumber targetX) (toNumber y      ) stuff
  -- Target
  drawImage
    (if customize.leftyFlip then Image_highway_protar_target_lefty else Image_highway_protar_target)
    (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  let colors =
        [ { c: _.s6, x: handedness 0 * widthFret + 1, strum: Image_gem_red_pro   , hopo: Image_gem_red_pro_hopo   , tap: Image_gem_red_pro_tap
          , shades: customize.sustainRed
          }
        , { c: _.s5, x: handedness 1 * widthFret + 1, strum: Image_gem_green_pro , hopo: Image_gem_green_pro_hopo , tap: Image_gem_green_pro_tap
          , shades: customize.sustainGreen
          }
        , { c: _.s4, x: handedness 2 * widthFret + 1, strum: Image_gem_orange_pro, hopo: Image_gem_orange_pro_hopo, tap: Image_gem_orange_pro_tap
          , shades: customize.sustainOrange
          }
        , { c: _.s3, x: handedness 3 * widthFret + 1, strum: Image_gem_blue_pro  , hopo: Image_gem_blue_pro_hopo  , tap: Image_gem_blue_pro_tap
          , shades: customize.sustainBlue
          }
        , { c: _.s2, x: handedness 4 * widthFret + 1, strum: Image_gem_yellow_pro, hopo: Image_gem_yellow_pro_hopo, tap: Image_gem_yellow_pro_tap
          , shades: customize.sustainYellow
          }
        , { c: _.s1, x: handedness 5 * widthFret + 1, strum: Image_gem_purple_pro, hopo: Image_gem_purple_pro_hopo, tap: Image_gem_purple_pro_tap
          , shades: customize.sustainPurple
          }
        ]
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades } -> do
    let thisColor = getColor protar.notes
        isEnergy secs = case Map.lookupLE secs protar.energy of
          Nothing           -> false
          Just { value: v } -> v
        hitAtY = if customize.autoplay then targetY else windowH + 100
        drawSustainBlock ystart yend energy mute = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = customize.autoplay && (targetY < ystart || targetY < yend)
              shades =
                if energy    then customize.sustainEnergy
                else if mute then customize.sustainProtarMute
                else              normalShades
              h = yend' - ystart' + 1
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX + 11, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX + 19, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX + 12, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX + 13, y: toNumber ystart', w: 5.0, h: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX + 18, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisColor of
          Just { key: secsStart, value: Sustain (ProtarNote o) } -> do
            drawSustainBlock (secsToPxVert secsEnd) windowH (isEnergy secsStart) (isNothing o.fret)
            go false rest
          _ -> unsafeThrow "during protar drawing: found a sustain end not preceded by sustain start"
        go true (L.Cons (Tuple _ SustainEnd) rest) = go false rest
        go _ (L.Cons (Tuple _ (Note _)) rest) = go false rest
        go _ (L.Cons (Tuple secsStart (Sustain (ProtarNote o))) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> 0
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) (isEnergy secsStart) (isNothing o.fret)
          go true rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisColor) of
      L.Nil -> case Map.lookupLT (pxToSecsVert windowH) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain (ProtarNote o) } ->
          drawSustainBlock 0 windowH (isEnergy secsStart) (isNothing o.fret)
        _ -> pure unit
      events -> go false events
  -- Sustain ends
  for_ colors \{ c: getColor, x: offsetX } -> do
    zoomDesc (getColor protar.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs - stuff.time
        if customize.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else drawImage Image_sustain_end
            (toNumber $ targetX + offsetX - 3)
            (toNumber $ secsToPxVert secs)
            stuff
      _ -> pure unit
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, tap: tapImage, shades: shades } -> do
    zoomDesc (getColor protar.notes) \secs evt -> let
      withNoteType obj = do
        let futureSecs = secToNum $ secs - stuff.time
        if customize.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle (shades.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = case Map.lookupLE secs protar.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                fretImage i = case index protarFrets i of
                  Just x  -> x
                  Nothing -> Image_pro_fret_00 -- whatever
            case obj.fret of
              Just fret -> let
                gemImg = case obj.noteType of
                  Strum -> if isEnergy then Image_gem_energy_pro      else strumImage
                  HOPO  -> if isEnergy then Image_gem_energy_pro_hopo else hopoImage
                  Tap   -> if isEnergy then Image_gem_energy_pro_tap  else tapImage
                in do
                  drawImage gemImg           (toNumber $ targetX + offsetX) (toNumber $ y - 10) stuff
                  drawImage (fretImage fret) (toNumber $ targetX + offsetX) (toNumber $ y - 10) stuff
              Nothing -> let
                gemImg = case obj.noteType of
                  Strum -> if isEnergy then Image_gem_energy_mute      else Image_gem_mute
                  HOPO  -> if isEnergy then Image_gem_energy_mute_hopo else Image_gem_mute_hopo
                  Tap   -> if isEnergy then Image_gem_energy_mute_tap  else Image_gem_mute_tap
                in drawImage gemImg (toNumber $ targetX + offsetX) (toNumber $ y - 10) stuff
      in case evt of
        Note    (ProtarNote obj) -> withNoteType obj
        Sustain (ProtarNote obj) -> withNoteType obj
        SustainEnd               -> pure unit
  pure $ targetX + (widthFret * 6 + 2) + customize.marginWidth
