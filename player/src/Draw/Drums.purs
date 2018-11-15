module Draw.Drums (drawDrums, drawDrumsPerspective) where

import           Prelude
import           Math               (pow, log)

import           Data.Array         (cons, range, snoc)
import           Data.Foldable      (for_)
import           Data.Int           (round, toNumber)
import           Data.List          as L
import           Data.Maybe         (Maybe (..), fromMaybe)
import           Data.Time.Duration (Seconds (..), negateDuration)
import           Data.Tuple         (Tuple (..))
import           Graphics.Canvas    as C

import           Draw.Common        (Draw, drawImage, drawLane, fillRect,
                                     secToNum, setFillStyle, drawBeats)
import           Images
import           OnyxMap            as Map
import           Song               (Drums (..), Gem (..))
import           Style              (customize)

drawDrumsPerspective :: Drums -> C.Rectangle -> Number -> Draw Int
drawDrumsPerspective (Drums drums) rect horizonY stuff = do
  let ctx = stuff.context
      numLanes = if drums.mode5 then 5.0 else 4.0
      horizonX = rect.x + rect.width * 0.5
      strikeY = rect.y + rect.height * 0.7
      strikeHeight = rect.height * 0.09
      strikeTop = strikeY - strikeHeight * 0.5
      strikeBottom = strikeY + strikeHeight * 0.5
      viewToHit = Seconds 0.75
      divideSecs (Seconds x) (Seconds y) = x / y
      divideSecsNum (Seconds x) y = Seconds (x / y)
      timesNumSecs x (Seconds y) = Seconds (x * y)
      screenfulPercent = (strikeY - rect.y) / (strikeY - horizonY)
      screenfulFlip = 1.0 / (1.0 - screenfulPercent)
      secsToY secs = let
        timeBeforeHit = secs <> negateDuration stuff.time
        screenfuls = divideSecs timeBeforeHit viewToHit
        fracToHorizon = 1.0 - pow screenfulFlip (negate screenfuls)
        in strikeY - fracToHorizon * (strikeY - horizonY)
      -- following was algebraically reversed from secsToY
      yToSecs y = timesNumSecs (negate $ log ((y - strikeY) / (strikeY - horizonY) + 1.0) / log screenfulFlip) viewToHit <> stuff.time

      strikeWidth = rect.width * ((strikeY - horizonY) / (rect.y + rect.height - horizonY))

      -- Converts from
      -- X (0 is highway left rail, 1 is right rail)
      -- Y (same scale as X at the strike line, but vertical, 0 is floor and positive goes up)
      -- Z (seconds in absolute time)
      -- to real window pixel position.
      xyzToRender o = let
        -- first calculate what output Y is if input Y is 0, using input Z only
        timeBeforeHit = o.z <> negateDuration stuff.time
        screenfuls = divideSecs timeBeforeHit viewToHit
        fracToHorizon = 1.0 - pow screenfulFlip (negate screenfuls) -- 0 at strike, 1 at horizon
        floorY = strikeY - fracToHorizon * (strikeY - horizonY) -- Y in window if input Y is 0
        -- now X, just scale it from the strike width to the computed width
        renderX = horizonX + (o.x - 0.5) * strikeWidth * (1.0 - fracToHorizon)
        renderY = floorY - o.y * strikeWidth * (1.0 - fracToHorizon)
        in {x: renderX, y: renderY}

      line a b = let
        a' = xyzToRender a
        b' = xyzToRender b
        in do
          C.beginPath ctx
          C.moveTo ctx a'.x a'.y
          C.lineTo ctx b'.x b'.y
          C.stroke ctx

      makeRect tl br = let
        tl' = xyzToRender tl
        br' = xyzToRender br
        in C.rect ctx {x: tl'.x, y: tl'.y, width: br'.x - tl'.x, height: br'.y - tl'.y}

      makeCube o = let
        frontTL = xyzToRender {x: o.x1, y: o.y1, z: o.z1}
        frontTR = xyzToRender {x: o.x2, y: o.y1, z: o.z1}
        frontBL = xyzToRender {x: o.x1, y: o.y2, z: o.z1}
        frontBR = xyzToRender {x: o.x2, y: o.y2, z: o.z1}
        backTL = xyzToRender {x: o.x1, y: o.y1, z: o.z2}
        backTR = xyzToRender {x: o.x2, y: o.y1, z: o.z2}
        backBL = xyzToRender {x: o.x1, y: o.y2, z: o.z2}
        backBR = xyzToRender {x: o.x2, y: o.y2, z: o.z2}
        backLeftHidden = frontBL.x <= backBL.x
        backRightHidden = backBR.x <= frontBR.x
        drawFace xy xys = do
          C.beginPath ctx
          C.moveTo ctx xy.x xy.y
          for_ xys \p -> C.lineTo ctx p.x p.y
          C.closePath ctx
          C.stroke ctx
          C.fill ctx
        in do
          C.setStrokeStyle ctx "black"
          C.setLineWidth ctx 2.0
          drawFace frontTL [frontTR, frontBR, frontBL]
          drawFace frontTL [frontTR, backTR, backTL]
          unless backLeftHidden $ drawFace frontTL [frontBL, backBL, backTL]
          unless backRightHidden $ drawFace frontTR [frontBR, backBR, backTR]

      maxSecs = yToSecs $ rect.y - 50.0
      minSecs = yToSecs $ rect.y + rect.height + 50.0
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      railingsX y = let
        inward = rect.width * (0.5 * ((y - horizonY) / (rect.y + rect.height - horizonY)))
        in {left: horizonX - inward, right: horizonX + inward}
  C.save ctx
  C.rect ctx rect
  C.clip ctx
  C.setFillStyle ctx "rgba(255,255,255,0.4)"
  C.setStrokeStyle ctx "black"
  C.setLineWidth ctx 3.0
  C.beginPath ctx
  C.moveTo ctx rect.x (rect.y + rect.height)
  C.lineTo ctx (rect.x + rect.width * 0.5) horizonY
  C.lineTo ctx (rect.x + rect.width) (rect.y + rect.height)
  C.fill ctx
  C.stroke ctx
  do
    let o = railingsX strikeTop
    C.beginPath ctx
    C.moveTo ctx o.left strikeTop
    C.lineTo ctx o.right strikeTop
    C.stroke ctx
  do
    let o = railingsX strikeBottom
    C.beginPath ctx
    C.moveTo ctx o.left strikeBottom
    C.lineTo ctx o.right strikeBottom
    C.stroke ctx
  C.setLineWidth ctx 2.0
  for_ [0.25, 0.5, 0.75] \frac -> do
    C.beginPath ctx
    C.moveTo ctx horizonX horizonY
    C.lineTo ctx (rect.x + rect.width * frac) (rect.y + rect.height)
    C.stroke ctx
  -- notes
  zoomDesc drums.notes \secs evts -> do
    let y = secsToY secs
        o = railingsX y
        isEnergy = case Map.lookupLE secs drums.energy of
          Just {value: bool} -> bool
          Nothing            -> false
    C.setStrokeStyle ctx "black"
    C.setLineWidth ctx $ secsToY (secs <> Seconds (-0.002)) - secsToY (secs <> Seconds 0.002)
    for_ evts \e -> let
      fracs = case e of
        Kick -> {left: 0.0, right: 1.0, color: if drums.mode5 then "#af3abc" else "#e8ad19", height: 0.02}
        Red  -> {left: 0.0 / numLanes, right: 1.0 / numLanes, color: "#e55050", height: 0.05}
        YTom -> {left: 1.0 / numLanes, right: 2.0 / numLanes, color: "#e1ea2c", height: 0.05}
        YCym -> {left: 1.0 / numLanes, right: 2.0 / numLanes, color: "#e1ea2c", height: 0.05}
        BTom -> {left: 2.0 / numLanes, right: 3.0 / numLanes, color: "#2c8eea", height: 0.05}
        BCym -> {left: 2.0 / numLanes, right: 3.0 / numLanes, color: "#2c8eea", height: 0.05}
        GTom -> {left: (numLanes - 1.0) / numLanes, right: 1.0, color: "#51e84e", height: 0.05}
        GCym -> {left: (numLanes - 1.0) / numLanes, right: 1.0, color: "#51e84e", height: 0.05}
        OCym -> {left: 3.0 / numLanes, right: 4.0 / numLanes, color: "#e8ad19", height: 0.05}
      in do
        C.setFillStyle ctx fracs.color
        makeCube
          { x1: fracs.left
          , x2: fracs.right
          , y1: fracs.height
          , y2: 0.0
          , z1: secs <> Seconds (-0.007)
          , z2: secs <> Seconds   0.007
          }
  C.restore ctx
  pure $ round (rect.x + rect.width) + customize.marginWidth

drawDrums :: Drums -> Int -> Draw Int
drawDrums (Drums drums) targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      widthFret = customize.widthStandardFret
      numLanes = if drums.mode5 then 5 else 4
      widthHighway = numLanes * widthFret + 2
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      handedness n = if stuff.app.settings.leftyFlip then numLanes - 1 - n else n
      drawH = stuff.maxY - stuff.minY
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: toNumber stuff.minY, width: toNumber widthHighway, height: toNumber drawH } stuff
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
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, width: toNumber $ widthFret - 2, height: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc drums.solo \secs _ -> do
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
  let dividers0 = map (\i -> i * widthFret    ) $ range 0 $ if drums.mode5 then 5 else 4
      dividers1 = map (\i -> i * widthFret + 1) $ range 0 $ if drums.mode5 then 5 else 4
  for_ dividers0 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ dividers1 \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: toNumber stuff.minY, width: 1.0, height: toNumber drawH } stuff
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
        , width: widthFret - 2
        , height: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Target
  drawImage image_highway_target_red    (toNumber $ targetX + 0 * widthFret + 1) (toNumber targetY - 5.0) stuff
  drawImage image_highway_target_yellow (toNumber $ targetX + 1 * widthFret + 1) (toNumber targetY - 5.0) stuff
  drawImage image_highway_target_blue   (toNumber $ targetX + 2 * widthFret + 1) (toNumber targetY - 5.0) stuff
  if drums.mode5
    then do
      drawImage image_highway_target_orange (toNumber $ targetX + 3 * widthFret + 1) (toNumber targetY - 5.0) stuff
      drawImage image_highway_target_green  (toNumber $ targetX + 4 * widthFret + 1) (toNumber targetY - 5.0) stuff
    else
      drawImage image_highway_target_green  (toNumber $ targetX + 3 * widthFret + 1) (toNumber targetY - 5.0) stuff
  -- Kick notes
  let imgKick   = if drums.mode5 then image_gem_open        else image_gem_kick
      imgKickOD = if drums.mode5 then image_gem_open_energy else image_gem_kick_energy
  zoomDesc drums.notes \secs evts -> do
    let futureSecs = secToNum $ secs <> negateDuration stuff.time
    if stuff.app.settings.autoplay && futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
                kick = do
                  if drums.mode5
                    then setFillStyle (customize.sustainPurple.hit opacity) stuff
                    else setFillStyle (customize.sustainOrange.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY - 5, width: toNumber $ numLanes * widthFret - 1, height: 1.0 } stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY + 4, width: toNumber $ numLanes * widthFret - 1, height: 1.0 } stuff
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
    let futureSecs = secToNum $ secs <> negateDuration stuff.time
    if stuff.app.settings.autoplay && futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
                red = do
                  setFillStyle (customize.sustainRed.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 0 * widthFret + 2, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
                yellow = do
                  setFillStyle (customize.sustainYellow.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 1 * widthFret + 2, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
                blue = do
                  setFillStyle (customize.sustainBlue.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 2 * widthFret + 2, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
                orange = do
                  setFillStyle (customize.sustainOrange.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + 3 * widthFret + 2, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
                green = do
                  setFillStyle (customize.sustainGreen.hit opacity) stuff
                  fillRect { x: toNumber $ targetX + (numLanes - 1) * widthFret + 2, y: toNumber $ targetY - 4, width: toNumber $ widthFret - 1, height: 8.0 } stuff
                red' = if stuff.app.settings.leftyFlip then green else red
                yellow' = if stuff.app.settings.leftyFlip then (if drums.mode5 then orange else blue) else yellow
                blue' = if stuff.app.settings.leftyFlip then (if drums.mode5 then blue else yellow) else blue
                orange' = if stuff.app.settings.leftyFlip then yellow else orange
                green' = if stuff.app.settings.leftyFlip then red else green
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
            (if isEnergy then image_gem_energy else if stuff.app.settings.leftyFlip then image_gem_green else image_gem_red)
            (toNumber $ targetX + handedness 0 * widthFret + 1)
            (toNumber $ y - 5) stuff
          YTom -> drawImage
            (if isEnergy then image_gem_energy else if stuff.app.settings.leftyFlip
              then (if drums.mode5 then image_gem_orange else image_gem_blue)
              else image_gem_yellow)
            (toNumber $ targetX + handedness 1 * widthFret + 1)
            (toNumber $ y - 5) stuff
          YCym -> drawImage
            (if isEnergy then image_gem_energy_cymbal else if stuff.app.settings.leftyFlip
              then (if drums.mode5 then image_gem_orange_cymbal else image_gem_blue_cymbal)
              else image_gem_yellow_cymbal)
            (toNumber $ targetX + handedness 1 * widthFret + 1)
            (toNumber $ y - 8) stuff
          BTom -> drawImage
            (if isEnergy then image_gem_energy else if stuff.app.settings.leftyFlip
              then (if drums.mode5 then image_gem_blue else image_gem_yellow)
              else image_gem_blue)
            (toNumber $ targetX + handedness 2 * widthFret + 1)
            (toNumber $ y - 5) stuff
          BCym -> drawImage
            (if isEnergy then image_gem_energy_cymbal else if stuff.app.settings.leftyFlip
              then (if drums.mode5 then image_gem_blue_cymbal else image_gem_yellow_cymbal)
              else image_gem_blue_cymbal)
            (toNumber $ targetX + handedness 2 * widthFret + 1)
            (toNumber $ y - 8) stuff
          OCym -> drawImage
            (if isEnergy then image_gem_energy_cymbal else if stuff.app.settings.leftyFlip then image_gem_yellow_cymbal else image_gem_orange_cymbal)
            (toNumber $ targetX + handedness 3 * widthFret + 1)
            (toNumber $ y - 8) stuff
          GTom -> drawImage
            (if isEnergy then image_gem_energy else if stuff.app.settings.leftyFlip then image_gem_red else image_gem_green)
            (toNumber $ targetX + handedness (numLanes - 1) * widthFret + 1)
            (toNumber $ y - 5) stuff
          GCym -> drawImage
            (if isEnergy then image_gem_energy_cymbal else if stuff.app.settings.leftyFlip then image_gem_red_cymbal else image_gem_green_cymbal)
            (toNumber $ targetX + handedness (numLanes - 1) * widthFret + 1)
            (toNumber $ y - 8) stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
