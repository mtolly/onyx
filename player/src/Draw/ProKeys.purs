module Draw.ProKeys (drawProKeysFast) where

import           Prelude

import           Data.Array              (length, take, zip, (..))
import           Data.Foldable           (elem, for_, sum)
import           Data.Int                (round, toNumber)
import           Data.Int.Bits           (shr)
import           Data.List               as L
import           Data.Maybe              (Maybe (..), isJust, isNothing, maybe)
import           Data.Time.Duration      (negateDuration)
import           Data.Tuple              (Tuple (..))
import           Graphics.Canvas         as C

import           Draw.Common             (Draw, drawImage, drawLane, fillRect,
                                          onContext, secToNum, setFillStyle,
                                          strokeRect, BadgeInfo, drawBadgeVertical)
import           Images
import           OnyxMap                 as Map
import           Song
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

drawProKeysFast :: ProKeysFast -> BadgeInfo -> Int -> Draw Int
drawProKeysFast pkf badge targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) <> stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs <> negateDuration stuff.time)
      maxSecs = pxToSecsVert $ stuff.minY - 50
      minSecs = pxToSecsVert $ stuff.maxY + 50
      zoomAsc  = addMax $ addMin $ L.fromFoldable $ Map.doTupleArray (Map.zoomAscDo minSecs maxSecs pkf)
      zoomDesc = L.reverse zoomAsc

      addMin L.Nil = let
        s = case Map.lookupLE minSecs pkf of
          Nothing -> emptyProKeysState
          Just o -> extendPKFuture o.value
        in L.Cons (Tuple minSecs s) L.Nil
      addMin xs@(L.Cons (Tuple _ pks) _) = L.Cons (Tuple minSecs (extendPKPast pks)) xs

      addMax (L.Cons t@(Tuple _ pks) L.Nil) = L.Cons t $ L.Cons (Tuple maxSecs (extendPKFuture pks)) L.Nil
      addMax (L.Cons t rest) = L.Cons t $ addMax rest
      addMax L.Nil = let
        s = case Map.lookupGE maxSecs pkf of
          Nothing -> emptyProKeysState
          Just o -> extendPKPast o.value
        in L.Cons (Tuple maxSecs s) L.Nil

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
  let drawSoloHighway _    _  _  L.Nil                 = pure unit
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
      drawSolos (L.Cons (Tuple s1 (ProKeysState pks1)) rest@(L.Cons (Tuple s2 (ProKeysState pks2)) _)) = do
        when pks1.solo.future $ drawSoloHighway targetX (secsToPxVert s1) (secsToPxVert s2) pkHighway
        drawSolos rest
      drawSolos _ = pure unit
  drawSolos zoomAsc
  -- Solo edges
  for_ zoomAsc \(Tuple secs (ProKeysState pks)) -> when pks.solo.now do
    let y = secsToPxVert secs
    setFillStyle customize.highwaySoloEdge stuff
    fillRect { x: toNumber targetX, y: toNumber y, width: toNumber widthHighway, height: 1.0 } stuff
  -- Beats
  -- TODO zoom into the beats map only once for all highways
  Map.zoomDescDo minSecs maxSecs (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
        h = case evt of
          Bar      -> customize.highwayBarHeight
          Beat     -> customize.highwayBeatHeight
          HalfBeat -> customize.highwayHalfBeatHeight
    setFillStyle customize.highwayLine stuff
    fillRect { x: toNumber targetX + 1.0, y: toNumber $ y - shr h 1, width: toNumber widthHighway - 1.0, height: toNumber h } stuff
  -- Lanes
  -- TODO only draw lane caps when necessary
  for_ pitchList \{pitch: pitch, offsetX: offsetX, isBlack: isBlack} -> let
    drawLanes (L.Cons (Tuple s1 (ProKeysState pks1)) rest@(L.Cons (Tuple s2 (ProKeysState pks2)) _)) = do
      when (maybe false _.future (Map.lookup pitch pks1.lanes) || pks1.bre.future) do
        let y1 = secsToPxVert s1
            y2 = secsToPxVert s2
        drawLane
          { x: targetX + offsetX + if isBlack then 0 else 1
          , y: y2
          , width: 11
          , height: y1 - y2
          } stuff
      drawLanes rest
    drawLanes _ = pure unit
    in drawLanes zoomAsc
  -- Target
  drawImage image_highway_prokeys_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Ranges
  setFillStyle customize.proKeysRangeOverlay stuff
  let drawRange s1 s2 rng = let
        y = toNumber (secsToPxVert s1)
        h = toNumber (secsToPxVert s2) - y
        rects = case rng of
          RangeC -> [{x: toNumber $ targetX + 192, y: y, width: 90.0, height: h}]
          RangeD -> [{x: toNumber $ targetX + 2, y: y, width: 22.0, height: h}, {x: toNumber $ targetX + 203, y: y, width: 79.0, height: h}]
          RangeE -> [{x: toNumber $ targetX + 2, y: y, width: 44.0, height: h}, {x: toNumber $ targetX + 225, y: y, width: 57.0, height: h}]
          RangeF -> [{x: toNumber $ targetX + 2, y: y, width: 56.0, height: h}, {x: toNumber $ targetX + 247, y: y, width: 35.0, height: h}]
          RangeG -> [{x: toNumber $ targetX + 2, y: y, width: 78.0, height: h}, {x: toNumber $ targetX + 270, y: y, width: 12.0, height: h}]
          RangeA -> [{x: toNumber $ targetX + 2, y: y, width: 100.0, height: h}]
        in for_ rects \rect -> fillRect rect stuff
      drawRanges (L.Cons (Tuple s1 (ProKeysState pks1)) rest@(L.Cons (Tuple s2 (ProKeysState pks2)) _)) = do
        case pks1.ranges.future of
          Nothing -> pure unit
          Just rng -> drawRange s1 s2 rng
        drawRanges rest
      drawRanges _ = pure unit
  drawRanges zoomAsc
  -- Sustains
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    let hitAtY = if stuff.app.settings.autoplay then targetY else stuff.maxY + 50
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
            fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, width: if isBlack then 9.0 else 11.0, height: 8.0 } stuff
        drawSustains (L.Cons (Tuple s1 (ProKeysState pks1)) rest@(L.Cons (Tuple s2 (ProKeysState pks2)) _)) = do
          case Map.lookup pitch pks1.notes >>= _.future of
            Nothing -> pure unit
            Just energy -> drawSustainBlock (secsToPxVert s2) (secsToPxVert s1) energy
          drawSustains rest
        drawSustains _ = pure unit
    drawSustains zoomAsc
  -- Sustain ends
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    setFillStyle customize.sustainBorder stuff
    for_ zoomDesc \(Tuple secs (ProKeysState pks)) -> case Map.lookup pitch pks.notes of
      Nothing -> pure unit
      Just o -> when (isJust o.past && isNothing o.future) do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
        if stuff.app.settings.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else fillRect
            { x: toNumber $ targetX + offsetX + if isBlack then 2 else 3
            , y: toNumber $ secsToPxVert secs
            , width: 7.0
            , height: 1.0
            } stuff
  -- Notes
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    for_ zoomDesc \(Tuple secs (ProKeysState pks)) -> case Map.lookup pitch pks.notes >>= _.now of
      Nothing -> pure unit
      Just _  -> do
        let futureSecs = secToNum $ secs <> negateDuration stuff.time
            isGlissando = pks.gliss.future
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
                isEnergy = pks.energy.future
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
