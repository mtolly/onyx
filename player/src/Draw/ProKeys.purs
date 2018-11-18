module Draw.ProKeys (drawProKeys) where

import           Prelude

import           Data.Array              (cons, length, snoc, take, zip, (..))
import           Data.Foldable           (elem, for_, sum)
import           Data.Int                (round, toNumber)
import           Data.Int.Bits           (shr)
import           Data.List               as L
import           Data.Maybe              (Maybe (..), fromMaybe)
import           Data.Time.Duration      (Seconds, negateDuration)
import           Data.Tuple              (Tuple (..))
import           Effect.Exception.Unsafe (unsafeThrow)
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, drawLane, fillRect,
                                          onContext, secToNum, setFillStyle,
                                          strokeRect, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song                    (Beat (..), Beats (..), Pitch (..),
                                          ProKeys (..), Range (..), Song (..),
                                          Sustainable (..))
import           Style                   (customize)

data PKHighway
  = RailingLight
  | RailingDark
  | WhiteKey
  | WhiteKeyShort
  | BlackKey

pkHighway :: L.List PKHighway
pkHighway = L.fromFoldable
  [ RailingLight, RailingDark, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKeyShort
  , RailingLight, RailingDark, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKeyShort
  , RailingLight, RailingDark, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKeyShort
  , RailingLight, RailingDark, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKey, BlackKey, WhiteKeyShort
  , RailingLight, RailingDark, WhiteKeyShort
  , RailingLight, RailingDark
  ]

inits :: forall a. Array a -> Array (Array a)
inits ary = map (\n -> take n ary) (0 .. length ary)

pitchList :: Array { pitch :: Pitch, offsetX :: Int, isBlack :: Boolean }
pitchList = do
  let allPitches = [RedC,RedCs,RedD,RedDs,RedE,YellowF,YellowFs,YellowG,YellowGs,YellowA,YellowAs,YellowB,BlueC,BlueCs,BlueD,BlueDs,BlueE,GreenF,GreenFs,GreenG,GreenGs,GreenA,GreenAs,GreenB,OrangeC]
      isBlack p = elem p [RedCs,RedDs,YellowFs,YellowGs,YellowAs,BlueCs,BlueDs,GreenFs,GreenGs,GreenAs]
  Tuple pitch lowerPitches <- zip allPitches $ inits allPitches
  pure
    { pitch: pitch
    , offsetX: 1 + sum (map (\p -> if isBlack p then 10 else 12) lowerPitches)
    , isBlack: isBlack pitch
    }

data HackBool = False | True

drawProKeys :: ProKeys -> BadgeInfo -> Int -> Draw Int
drawProKeys (ProKeys pk) badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
      drawH = stuff.maxY - stuff.minY
      widthHighway = 282
  -- Highway
  let drawHighway _    L.Nil                 = pure unit
      drawHighway xpos (L.Cons chunk chunks) = do
        let params = case chunk of
              RailingLight  -> { color: customize.highwayRailing , width: 1  }
              RailingDark   -> { color: customize.highwayDivider , width: 1  }
              WhiteKey      -> { color: customize.highway        , width: 11 }
              WhiteKeyShort -> { color: customize.highway        , width: 10 }
              BlackKey      -> { color: customize.highwayBlackKey, width: 11 }
        setFillStyle params.color stuff
        fillRect { x: toNumber xpos, y: toNumber stuff.minY, width: toNumber params.width, height: toNumber drawH } stuff
        drawHighway (xpos + params.width) chunks
  drawHighway targetX pkHighway
  -- Solo highway
  let startsAsSolo = case Map.lookupLE minSecs pk.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc pk.solo)
      drawSoloHighway _    _  _  L.Nil                 = pure unit
      drawSoloHighway xpos y1 y2 (L.Cons chunk chunks) = do
        let params = case chunk of
              RailingLight  -> { color: Nothing                           , width: 1  }
              RailingDark   -> { color: Nothing                           , width: 1  }
              WhiteKey      -> { color: Just customize.highwaySolo        , width: 11 }
              WhiteKeyShort -> { color: Just customize.highwaySolo        , width: 10 }
              BlackKey      -> { color: Just customize.highwaySoloBlackKey, width: 11 }
        case params.color of
          Nothing -> pure unit
          Just c  -> do
            setFillStyle c stuff
            fillRect { x: toNumber xpos, y: toNumber y1, width: toNumber params.width, height: toNumber $ y2 - y1 } stuff
        drawSoloHighway (xpos + params.width) y1 y2 chunks
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        when b1 $ drawSoloHighway targetX (secsToPxVert s1) (secsToPxVert s2) pkHighway
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc pk.solo \secs _ -> do
    let y = secsToPxVert secs
    setFillStyle customize.highwaySoloEdge stuff
    fillRect { x: toNumber targetX, y: toNumber y, width: toNumber widthHighway, height: 1.0 } stuff
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
        h = case evt of
          Bar      -> customize.highwayBarHeight
          Beat     -> customize.highwayBeatHeight
          HalfBeat -> customize.highwayHalfBeatHeight
    setFillStyle customize.highwayLine stuff
    fillRect { x: toNumber targetX + 1.0, y: toNumber $ y - shr h 1, width: toNumber widthHighway - 1.0, height: toNumber h } stuff
  -- Lanes
  for_ pitchList \{pitch: pitch, offsetX: offsetX, isBlack: isBlack} -> let
    thisLane = Map.union pk.bre
      $ fromMaybe Map.empty $ Map.lookup pitch pk.lanes
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
        { x: targetX + offsetX + if isBlack then 0 else 1
        , y: y2
        , width: 11
        , height: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Target
  drawImage image_highway_prokeys_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Ranges
  setFillStyle customize.proKeysRangeOverlay stuff
  let rangeEdges
        = L.fromFoldable
        $ cons (Tuple minSecs $ map _.value $ Map.lookupLE minSecs pk.ranges)
        $ flip snoc (Tuple maxSecs Nothing)
        $ map (map Just) $ Map.doTupleArray (zoomAsc pk.ranges)
      drawRanges L.Nil = pure unit
      drawRanges (L.Cons _ L.Nil) = pure unit
      drawRanges (L.Cons (Tuple s1 rng) rest@(L.Cons (Tuple s2 _) _)) = do
        case rng of
          Nothing -> pure unit
          Just r -> let
            y = toNumber (secsToPxVert s1)
            h = toNumber (secsToPxVert s2) - y
            rects = case r of
              RangeC -> [{x: toNumber $ targetX + 192, y: y, width: 90.0, height: h}]
              RangeD -> [{x: toNumber $ targetX + 2, y: y, width: 22.0, height: h}, {x: toNumber $ targetX + 203, y: y, width: 79.0, height: h}]
              RangeE -> [{x: toNumber $ targetX + 2, y: y, width: 44.0, height: h}, {x: toNumber $ targetX + 225, y: y, width: 57.0, height: h}]
              RangeF -> [{x: toNumber $ targetX + 2, y: y, width: 56.0, height: h}, {x: toNumber $ targetX + 247, y: y, width: 35.0, height: h}]
              RangeG -> [{x: toNumber $ targetX + 2, y: y, width: 78.0, height: h}, {x: toNumber $ targetX + 270, y: y, width: 12.0, height: h}]
              RangeA -> [{x: toNumber $ targetX + 2, y: y, width: 100.0, height: h}]
            in for_ rects \rect -> fillRect rect stuff
        drawRanges rest
  drawRanges rangeEdges
  -- Sustains
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    let thisPitch = fromMaybe Map.empty $ Map.lookup pitch pk.notes
        isEnergy secs = case Map.lookupLE secs pk.energy of
          Just {value: bool} -> bool
          Nothing            -> false
        hitAtY = if stuff.app.settings.autoplay then targetY else stuff.maxY + 50
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = stuff.app.settings.autoplay && (targetY < ystart || targetY < yend)
              shades = if energy
                then if isBlack
                  then customize.sustainBlackKeyEnergy
                  else customize.sustainEnergy
                else if isBlack
                  then customize.sustainBlackKey
                  else customize.sustainWhiteKey
              h = yend' - ystart' + 1
              offsetX' = offsetX + if isBlack then 0 else 1
          setFillStyle customize.sustainBorder stuff
          fillRect { x: toNumber $ targetX + offsetX' + 2, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX' + 8, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX' + 3, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX' + 4, y: toNumber ystart', width: 3.0, height: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX' + 7, y: toNumber ystart', width: 1.0, height: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, width: if isBlack then 9.0 else 11.0, height: 8.0 } stuff
        go False (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisPitch of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) stuff.maxY $ isEnergy secsStart
            go False rest
          _ -> unsafeThrow "during prokeys drawing: found a sustain end not preceded by sustain start"
        go True (L.Cons (Tuple _ SustainEnd) rest) = go False rest
        go _ (L.Cons (Tuple _ (Note (_ :: Unit))) rest) = go False rest
        go _ (L.Cons (Tuple secsStart (Sustain (_ :: Unit))) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> stuff.minY
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) $ isEnergy secsStart
          go True rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisPitch) of
      L.Nil -> case Map.lookupLT (pxToSecsVert stuff.maxY) thisPitch of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain (_ :: Unit) } ->
          drawSustainBlock stuff.minY stuff.maxY $ isEnergy secsStart
        _ -> pure unit
      events -> go False events
  -- Sustain ends
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    setFillStyle customize.sustainBorder stuff
    zoomDesc (fromMaybe Map.empty $ Map.lookup pitch pk.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else fillRect
            { x: toNumber $ targetX + offsetX + if isBlack then 2 else 3
            , y: toNumber $ secsToPxVert secs
            , width: 7.0
            , height: 1.0
            } stuff
      _ -> pure unit
  -- Notes
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    zoomDesc (fromMaybe Map.empty $ Map.lookup pitch pk.notes) \secs evt -> case evt of
      SustainEnd -> pure unit
      _          -> do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            isGlissando = case Map.lookupLE secs pk.gliss of
              Just {value: bool} -> bool
              Nothing            -> false
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                let colors = if isBlack then customize.sustainBlackKey else customize.sustainWhiteKey
                setFillStyle (colors.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, width: if isBlack then 9.0 else 11.0, height: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = case Map.lookupLE secs pk.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                img = if isEnergy
                  then if isBlack then image_gem_blackkey_energy else image_gem_whitekey_energy
                  else if isBlack then image_gem_blackkey        else image_gem_whitekey
            drawImage img (toNumber $ targetX + offsetX) (toNumber $ y - 5) stuff
            when isGlissando do
              onContext (\ctx -> C.setStrokeStyle ctx customize.glissandoBorder) stuff
              onContext (\ctx -> C.setLineWidth ctx 1.0) stuff
              strokeRect { x: toNumber (targetX + offsetX) + 0.5, y: toNumber y - 4.5, width: 12.0, height: 9.0 } stuff
  -- Draw badge below target
  drawBadgeVertical badge targetX widthHighway stuff
  -- Return targetX of next track
  pure $ targetX + widthHighway + customize.marginWidth
