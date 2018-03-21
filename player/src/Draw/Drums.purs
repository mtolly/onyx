module Draw.Drums (drawDrums) where

import           Prelude

import           Data.Array         (cons, range, snoc)
import           Data.Foldable      (for_)
import           Data.Int           (round, toNumber)
import           Data.List          as L
import           Data.Maybe         (Maybe (..), fromMaybe)
import           Data.Time.Duration (Seconds)
import           Data.Tuple         (Tuple (..))
import           Graphics.Canvas    as C

import           Draw.Common        (Draw, drawImage, drawLane, fillRect,
                                     secToNum, setFillStyle)
import           Images             (ImageID (..))
import           OnyxMap            as Map
import           Song               (Beat (..), Beats (..), Drums (..),
                                     Gem (..), Song (..))
import           Style              (customize)

drawDrums :: forall e. Drums -> Int -> Draw e Int
drawDrums (Drums drums) targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) + stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs - stuff.time)
      widthFret = customize.widthStandardFret
      numLanes = if drums.mode5 then 5 else 4
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      trackWidth = numLanes * widthFret + 2
      handedness n = if customize.leftyFlip then numLanes - 1 - n else n
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: 0.0, w: toNumber trackWidth, h: toNumber windowH } stuff
  setFillStyle customize.highwayRailing stuff
  let dividers0 = map (\i -> i * widthFret    ) $ range 0 $ if drums.mode5 then 5 else 4
      dividers1 = map (\i -> i * widthFret + 1) $ range 0 $ if drums.mode5 then 5 else 4
  for_ dividers0 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ dividers1 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle customize.highwaySolo stuff
  let startsAsSolo = case Map.lookupLE minSecs drums.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc drums.solo)
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
            dividers2 = map (\i -> i * widthFret + 2) $ range 0 $ if drums.mode5 then 4 else 3
        when b1 $ for_ dividers2 \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: toNumber $ widthFret - 2, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc drums.solo \secs _ -> do
    let img = if drums.mode5 then Image_highway_grybo_solo_edge else Image_highway_drums_solo_edge
    drawImage img (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        -- TODO kick
        [ {x: handedness 0              * widthFret + 2, gem: Red }
        , {x: handedness 1              * widthFret + 2, gem: YCym}
        , {x: handedness 1              * widthFret + 2, gem: YTom}
        , {x: handedness 2              * widthFret + 2, gem: BCym}
        , {x: handedness 2              * widthFret + 2, gem: BTom}
        , {x: handedness 3              * widthFret + 2, gem: OCym}
        , {x: handedness (numLanes - 1) * widthFret + 2, gem: GCym}
        , {x: handedness (numLanes - 1) * widthFret + 2, gem: GTom}
        ]
  for_ lanes \{x: offsetX, gem: gem} -> let
    thisLane = Map.union drums.bre
      $ fromMaybe Map.empty $ Map.lookup gem drums.lanes
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
  let imgBar  = if drums.mode5 then Image_highway_grybo_bar      else Image_highway_drums_bar
      imgBeat = if drums.mode5 then Image_highway_grybo_beat     else Image_highway_drums_beat
      imgHalf = if drums.mode5 then Image_highway_grybo_halfbeat else Image_highway_drums_halfbeat
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
    case evt of
      Bar      -> drawImage imgBar  (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage imgBeat (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> drawImage imgHalf (toNumber targetX) (toNumber y      ) stuff
  -- Target
  let imgTarget = if drums.mode5 then Image_highway_drums5_target else Image_highway_drums_target
  drawImage imgTarget (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Kick notes
  let imgKick   = if drums.mode5 then Image_gem_open        else Image_gem_kick
      imgKickOD = if drums.mode5 then Image_gem_open_energy else Image_gem_kick_energy
  zoomDesc drums.notes \secs evts -> do
    let futureSecs = secToNum $ secs - stuff.time
    if customize.autoplay && futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
                kick = do
                  if drums.mode5
                    then setFillStyle (customize.sustainPurple.hit opacity) stuff
                    else setFillStyle (customize.sustainOrange.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY - 5, w: toNumber $ numLanes * widthFret - 1, h: 1.0 } stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY + 4, w: toNumber $ numLanes * widthFret - 1, h: 1.0 } stuff
            for_ evts \e -> case e of
              Kick -> kick
              _    -> pure unit
          else pure unit
      else do
        -- note is in the future
        let y = secsToPxVert secs
            isEnergy = case Map.lookupLE secs drums.energy of
              Just {value: bool} -> bool
              Nothing            -> false
        for_ evts \e -> case e of
          Kick -> drawImage
            (if isEnergy then imgKickOD else imgKick)
            (toNumber $ targetX + 0 * widthFret + 1)
            (toNumber $ y - 3) stuff
          _ -> pure unit
  -- Hand notes
  zoomDesc drums.notes \secs evts -> do
    let futureSecs = secToNum $ secs - stuff.time
    if customize.autoplay && futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
                red = do
                  setFillStyle (customize.sustainRed.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
                yellow = do
                  setFillStyle (customize.sustainYellow.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 1 * widthFret + 2, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
                blue = do
                  setFillStyle (customize.sustainBlue.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 2 * widthFret + 2, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
                orange = do
                  setFillStyle (customize.sustainOrange.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 3 * widthFret + 2, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
                green = do
                  setFillStyle (customize.sustainGreen.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + (numLanes - 1) * widthFret + 2, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
                red' = if customize.leftyFlip then green else red
                yellow' = if customize.leftyFlip then (if drums.mode5 then orange else blue) else yellow
                blue' = if customize.leftyFlip then (if drums.mode5 then blue else yellow) else blue
                orange' = if customize.leftyFlip then yellow else orange
                green' = if customize.leftyFlip then red else green
            for_ evts \e -> case e of
              Kick -> pure unit
              Red  -> red'
              YCym -> yellow'
              YTom -> yellow'
              BCym -> blue'
              BTom -> blue'
              OCym -> orange'
              GCym -> green'
              GTom -> green'
          else pure unit
      else do
        -- note is in the future
        let y = secsToPxVert secs
            isEnergy = case Map.lookupLE secs drums.energy of
              Just {value: bool} -> bool
              Nothing            -> false
        for_ evts \e -> case e of
          Kick -> pure unit
          Red  -> drawImage
            (if isEnergy then Image_gem_energy else if customize.leftyFlip then Image_gem_green else Image_gem_red)
            (toNumber $ targetX + handedness 0 * widthFret + 1)
            (toNumber $ y - 5) stuff
          YTom -> drawImage
            (if isEnergy then Image_gem_energy else if customize.leftyFlip
              then (if drums.mode5 then Image_gem_orange else Image_gem_blue)
              else Image_gem_yellow)
            (toNumber $ targetX + handedness 1 * widthFret + 1)
            (toNumber $ y - 5) stuff
          YCym -> drawImage
            (if isEnergy then Image_gem_energy_cymbal else if customize.leftyFlip
              then (if drums.mode5 then Image_gem_orange_cymbal else Image_gem_blue_cymbal)
              else Image_gem_yellow_cymbal)
            (toNumber $ targetX + handedness 1 * widthFret + 1)
            (toNumber $ y - 8) stuff
          BTom -> drawImage
            (if isEnergy then Image_gem_energy else if customize.leftyFlip
              then (if drums.mode5 then Image_gem_blue else Image_gem_yellow)
              else Image_gem_blue)
            (toNumber $ targetX + handedness 2 * widthFret + 1)
            (toNumber $ y - 5) stuff
          BCym -> drawImage
            (if isEnergy then Image_gem_energy_cymbal else if customize.leftyFlip
              then (if drums.mode5 then Image_gem_blue_cymbal else Image_gem_yellow_cymbal)
              else Image_gem_blue_cymbal)
            (toNumber $ targetX + handedness 2 * widthFret + 1)
            (toNumber $ y - 8) stuff
          OCym -> drawImage
            (if isEnergy then Image_gem_energy_cymbal else if customize.leftyFlip then Image_gem_yellow_cymbal else Image_gem_orange_cymbal)
            (toNumber $ targetX + handedness 3 * widthFret + 1)
            (toNumber $ y - 8) stuff
          GTom -> drawImage
            (if isEnergy then Image_gem_energy else if customize.leftyFlip then Image_gem_red else Image_gem_green)
            (toNumber $ targetX + handedness (numLanes - 1) * widthFret + 1)
            (toNumber $ y - 5) stuff
          GCym -> drawImage
            (if isEnergy then Image_gem_energy_cymbal else if customize.leftyFlip then Image_gem_red_cymbal else Image_gem_green_cymbal)
            (toNumber $ targetX + handedness (numLanes - 1) * widthFret + 1)
            (toNumber $ y - 8) stuff
  -- Return targetX of next track
  pure $ targetX + trackWidth + customize.marginWidth
