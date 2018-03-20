module Draw (Settings, App(..), DrawStuff(), draw, _M, _B, getWindowDims) where

import Prelude
import Graphics.Canvas as C
import Data.Time.Duration (Seconds(..))
import Control.Monad.Eff (Eff)
import Data.Int (toNumber, round, floor)
import DOM (DOM)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Array (reverse, uncons, cons, snoc, take, zip, (..), length, concat, range, index)
import Data.List as L
import Data.Tuple (Tuple(..))
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Foldable (elem, sum, for_)
import Control.MonadPlus (guard)
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Math (pi)
import Data.Either (either)
import Data.Set as Set
import Data.Traversable (traverse_)

import Song
import Images (ImageID(..), protarFrets)
import OnyxMap as Map
import Style (customize)

foreign import getWindowDims :: forall e. Eff (dom :: DOM | e) {w :: Number, h :: Number}

type Settings = Set.Set (Tuple String FlexPart)

data App
  = Paused
    { pausedSongTime :: Seconds
    , settings :: Settings
    }
  | Playing
    { startedPageTime :: Seconds
    , startedSongTime :: Seconds
    , settings :: Settings
    }

type DrawStuff =
  { time :: Seconds
  , app :: App
  , song :: Song
  , getImage :: ImageID -> C.CanvasImageSource
  , canvas :: C.CanvasElement
  , context :: C.Context2D
  , pxToSecsVert :: Int -> Seconds -- pixels from bottom -> now-offset in seconds
  , secsToPxVert :: Seconds -> Int -- now-offset in seconds -> pixels from bottom
  , pxToSecsHoriz :: Int -> Seconds -- pixels from left -> now-offset in seconds
  , secsToPxHoriz :: Seconds -> Int -- now-offset in seconds -> pixels from left
  }

type Draw e a = DrawStuff -> Eff (canvas :: C.CANVAS | e) a

setFillStyle :: forall e. String -> Draw e Unit
setFillStyle s = onContext $ C.setFillStyle s

fillRect :: forall e. C.Rectangle -> Draw e Unit
fillRect rect = onContext \ctx -> C.fillRect ctx rect

strokeRect :: forall e. C.Rectangle -> Draw e Unit
strokeRect rect = onContext \ctx -> C.strokeRect ctx rect

fillEllipse :: forall e. { x :: Number, y :: Number, rx :: Number, ry :: Number } -> Draw e Unit
fillEllipse o dstuff = do
  let ctx = dstuff.context
      width = o.rx * 2.0
      height = o.ry * 2.0
      maxDiameter = max width height
      scaleX = width / maxDiameter
      scaleY = height / maxDiameter
  void $ C.save ctx
  void $ C.translate { translateX: o.x, translateY: o.y } ctx
  void $ C.scale { scaleX: scaleX, scaleY: scaleY } ctx
  void $ C.beginPath ctx
  void $ C.arc ctx { x: 0.0, y: 0.0, r: maxDiameter / 2.0, start: 0.0, end: 2.0 * pi}
  void $ C.fill ctx
  void $ C.closePath ctx
  void $ C.restore ctx

fillCircle :: forall e. { x :: Number, y :: Number, r :: Number } -> Draw e Unit
fillCircle o = fillEllipse { x: o.x, y: o.y, rx: o.r, ry: o.r }

drawImage :: forall e. ImageID -> Number -> Number -> Draw e Unit
drawImage iid x y dstuff =
  onContext (\ctx -> C.drawImage ctx (dstuff.getImage iid) x y) dstuff

onContext :: forall e. (C.Context2D -> Eff (canvas :: C.CANVAS | e) C.Context2D) -> Draw e Unit
onContext act dstuff = void $ act dstuff.context

measureText :: forall e. String -> Draw e C.TextMetrics
measureText str dstuff = C.measureText dstuff.context str

showTimestamp :: Seconds -> String
showTimestamp (Seconds s) = let
  mins = floor $ s / 60.0
  secs = floor $ s - toNumber (mins * 60)
  msecs = floor $ (s - toNumber (mins * 60) - toNumber secs) * 1000.0
  pad2 str n = if n < 10 then str <> show n else show n
  pad3 str n = if n < 100 then str <> pad2 str n else show n
  in show mins <> ":" <> pad2 "0" secs <> "." <> pad3 "0" msecs

draw :: forall e. Draw (dom :: DOM | e) Unit
draw stuff = do
  {w: windowW, h: windowH} <- getWindowDims
  void $ C.setCanvasWidth  windowW stuff.canvas
  void $ C.setCanvasHeight windowH stuff.canvas
  setFillStyle customize.background stuff
  fillRect { x: 0.0, y: 0.0, w: windowW, h: windowH } stuff
  -- Draw timestamp
  onContext (C.setFont customize.timestampFont) stuff
  setFillStyle customize.timestampColor stuff
  let timeStr = showTimestamp stuff.time
  metric <- measureText timeStr stuff
  onContext
    (\ctx -> C.fillText ctx
      (showTimestamp stuff.time)
      (windowW - 20.0 - metric.width)
      (windowH - 20.0)
    ) stuff
  -- Draw the visible instrument tracks in sequence
  let drawTracks targetX trks = case uncons trks of
        Nothing -> pure unit
        Just {head: trk, tail: trkt} -> do
          drawResult <- trk targetX
          case drawResult of
            Just targetX' -> drawTracks targetX' trkt
            Nothing       -> drawTracks targetX  trkt
  let drawButtons _ L.Nil             = pure unit
      drawButtons y (L.Cons iid iids) = do
        drawImage iid (toNumber $ _M + _B + _M) (toNumber y) stuff
        drawButtons (y - _M - _B) iids
      song = case stuff.song of Song s -> s
      settings = case stuff.app of
        Paused  o -> o.settings
        Playing o -> o.settings
  drawTracks (_M + _B + _M + _B + _M) $ concat $ flip map song.parts \(Tuple part (Flex flex)) ->
    [ \i -> drawPart flex.five    (Set.member $ Tuple part FlexFive   ) drawFive    i stuff
    , \i -> drawPart flex.six     (Set.member $ Tuple part FlexSix    ) drawSix     i stuff
    , \i -> drawPart flex.drums   (Set.member $ Tuple part FlexDrums  ) drawDrums   i stuff
    , \i -> drawPart flex.prokeys (Set.member $ Tuple part FlexProKeys) drawProKeys i stuff
    , \i -> drawPart flex.protar  (Set.member $ Tuple part FlexProtar ) drawProtar  i stuff
    ]
  flip traverse_ song.parts \(Tuple part (Flex flex)) -> do
    void $ drawPart flex.vocal (Set.member $ Tuple part FlexVocal) drawVocal 0 stuff
  drawButtons (round windowH - _M - _B) $ L.fromFoldable $ reverse $ concat $ flip map song.parts \(Tuple part (Flex flex)) -> concat
    [ guard (isJust flex.five   ) *>
      [ if Set.member (Tuple part FlexFive) settings
        then case part of
          "bass" -> Image_button_bass
          "keys" -> Image_button_keys
          _ -> Image_button_guitar
        else case part of
          "bass" -> Image_button_bass_off
          "keys" -> Image_button_keys_off
          _ -> Image_button_guitar_off
      ]
    , guard (isJust flex.six    ) *>
      [ if Set.member (Tuple part FlexSix) settings
        then case part of
          "bass" -> Image_button_bass6
          _ -> Image_button_guitar6
        else case part of
          "bass" -> Image_button_bass6_off
          _ -> Image_button_guitar6_off
      ]
    , guard (isJust flex.drums  ) *>
      [ if Set.member (Tuple part FlexDrums  ) settings
        then Image_button_drums
        else Image_button_drums_off
      ]
    , guard (isJust flex.prokeys) *>
      [ if Set.member (Tuple part FlexProKeys) settings
        then Image_button_prokeys
        else Image_button_prokeys_off
      ]
    , guard (isJust flex.protar ) *>
      [ if Set.member (Tuple part FlexProtar ) settings
        then case part of
          "bass" -> Image_button_probass
          _ -> Image_button_proguitar
        else case part of
          "bass" -> Image_button_probass_off
          _ -> Image_button_proguitar_off
      ]
    , guard (isJust flex.vocal  ) *>
      [ if Set.member (Tuple part FlexVocal  ) settings
        then Image_button_vocal
        else Image_button_vocal_off
      ]
    ]
  let playPause = case stuff.app of
        Paused  _ -> Image_button_play
        Playing _ -> Image_button_pause
  drawImage playPause (toNumber _M) (windowH - toNumber _M - toNumber _B) stuff
  let timelineH = windowH - 3.0 * toNumber _M - toNumber _B - 2.0
      filled = unSeconds (stuff.time) / unSeconds (case stuff.song of Song o -> o.end)
      unSeconds (Seconds s) = s
  setFillStyle customize.progressBorder stuff
  fillRect { x: toNumber _M, y: toNumber _M, w: toNumber _B, h: timelineH + 2.0 } stuff
  setFillStyle customize.progressEmpty stuff
  fillRect { x: toNumber _M + 1.0, y: toNumber _M + 1.0, w: toNumber _B - 2.0, h: timelineH } stuff
  setFillStyle customize.progressFilled stuff
  fillRect
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , w: toNumber _B - 2.0
    , h: timelineH * filled
    } stuff

-- | Height/width of margins
_M :: Int
_M = customize.marginWidth

-- | Height/width of buttons
_B :: Int
_B = customize.buttonWidth

drawPart
  :: forall e a r
  .  Maybe a
  -> (Settings -> Boolean)
  -> (a -> Int -> Draw e r)
  -> Int
  -> Draw e (Maybe r)
drawPart getPart see drawIt targetX stuff = do
  let settings = case stuff.app of
        Paused  o -> o.settings
        Playing o -> o.settings
  case getPart of
    Just part | see settings -> map Just $ drawIt part targetX stuff
    _                        -> pure Nothing

drawLane
  :: forall e
  .  { x :: Int, y :: Int, w :: Int, h :: Int }
  -> Draw e Unit
drawLane obj stuff = do
  setFillStyle customize.freeformLane stuff
  fillRect
    { x: toNumber obj.x
    , y: toNumber obj.y
    , w: toNumber obj.w
    , h: toNumber obj.h
    } stuff
  let rx = toNumber obj.w / 2.0
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber obj.y
    , rx: rx
    , ry: 15.0
    } stuff
  fillEllipse
    { x: toNumber obj.x + rx
    , y: toNumber $ obj.y + obj.h
    , rx: rx
    , ry: 15.0
    } stuff

drawFive :: forall e. Five -> Int -> Draw e Int
drawFive (Five five) targetX stuff = do
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
      handedness n = if customize.leftyFlip then 4 - n else n
  -- Highway
  setFillStyle customize.highway stuff
  fillRect { x: toNumber targetX, y: 0.0, w: toNumber $ widthFret * 5 + 2, h: toNumber windowH } stuff
  setFillStyle customize.highwayRailing stuff
  for_ (map (\i -> i * widthFret) $ range 0 5) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle customize.highwayDivider stuff
  for_ (map (\i -> i * widthFret + 1) $ range 0 5) \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
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
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: toNumber $ customize.widthStandardFret - 2, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc five.solo \secs _ -> do
    drawImage Image_highway_grybo_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        -- TODO open
        [ {x: handedness 0 * widthFret + 2, gem: _.green }
        , {x: handedness 1 * widthFret + 2, gem: _.red   }
        , {x: handedness 2 * widthFret + 2, gem: _.yellow}
        , {x: handedness 3 * widthFret + 2, gem: _.blue  }
        , {x: handedness 4 * widthFret + 2, gem: _.orange}
        ]
  for_ lanes \{x: offsetX, gem: gem} -> let
    thisLane = Map.union five.bre $ gem five.lanes
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
      Bar      -> drawImage Image_highway_grybo_bar      (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage Image_highway_grybo_beat     (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> drawImage Image_highway_grybo_halfbeat (toNumber targetX) (toNumber y      ) stuff
  -- Target
  drawImage
    (if customize.leftyFlip then Image_highway_grybo_target_lefty else Image_highway_grybo_target)
    (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  let colors =
        [ { c: _.open  , x:            0 * widthFret + 1, strum: Image_gem_open  , hopo: Image_gem_open_hopo, tap: Image_gem_open_tap
          , shades: customize.sustainPurple, open: true
          }
        , { c: _.green , x: handedness 0 * widthFret + 1, strum: Image_gem_green , hopo: Image_gem_green_hopo, tap: Image_gem_green_tap
          , shades: customize.sustainGreen, open: false
          }
        , { c: _.red   , x: handedness 1 * widthFret + 1, strum: Image_gem_red   , hopo: Image_gem_red_hopo, tap: Image_gem_red_tap
          , shades: customize.sustainRed, open: false
          }
        , { c: _.yellow, x: handedness 2 * widthFret + 1, strum: Image_gem_yellow, hopo: Image_gem_yellow_hopo, tap: Image_gem_yellow_tap
          , shades: customize.sustainYellow, open: false
          }
        , { c: _.blue  , x: handedness 3 * widthFret + 1, strum: Image_gem_blue  , hopo: Image_gem_blue_hopo, tap: Image_gem_blue_tap
          , shades: customize.sustainBlue, open: false
          }
        , { c: _.orange, x: handedness 4 * widthFret + 1, strum: Image_gem_orange, hopo: Image_gem_orange_hopo, tap: Image_gem_orange_tap
          , shades: customize.sustainOrange, open: false
          }
        ]
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades, open: isOpen } -> do
    let thisColor = getColor five.notes
        offsetX' = if isOpen then 2 * widthFret + 1 else offsetX
        isEnergy secs = case Map.lookupLE secs five.energy of
          Nothing           -> false
          Just { value: v } -> v
        hitAtY = if customize.autoplay then targetY else windowH + 100
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = customize.autoplay && (targetY < ystart || targetY < yend)
              shades = if energy
                then customize.sustainEnergy
                else normalShades
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
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisColor of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) windowH $ isEnergy secsStart
            go false rest
          _ -> unsafeThrow "during grybo drawing: found a sustain end not preceded by sustain start"
        go true (L.Cons (Tuple _ SustainEnd) rest) = go false rest
        go _ (L.Cons (Tuple _ (Note _)) rest) = go false rest
        go _ (L.Cons (Tuple secsStart (Sustain _)) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> 0
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) $ isEnergy secsStart
          go true rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisColor) of
      L.Nil -> case Map.lookupLT (pxToSecsVert windowH) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain _ } ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> pure unit
      events -> go false events
  -- Sustain endings (draw these first in case a sustain goes right up to next note)
  for_ colors \{ c: getColor, x: offsetX, open: isOpen } -> do
    zoomDesc (getColor five.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs - stuff.time
            trailX = if isOpen then 2 * widthFret + 1 else offsetX
        if customize.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else drawImage Image_sustain_end
            (toNumber $ targetX + trailX)
            (toNumber $ secsToPxVert secs)
            stuff
      _ -> pure unit
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, tap: tapImage, shades: shades, open: isOpen } -> do
    zoomDesc (getColor five.notes) \secs evt -> let
      withNoteType sht = do
        let futureSecs = secToNum $ secs - stuff.time
            trailX = if isOpen then 2 * widthFret + 1 else offsetX
        if customize.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                setFillStyle (shades.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + trailX + 1, y: toNumber $ targetY - 4, w: toNumber $ widthFret - 1, h: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = case Map.lookupLE secs five.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                img = case sht of
                  Strum -> if isEnergy then (if isOpen then Image_gem_open_energy      else Image_gem_energy     ) else strumImage
                  HOPO  -> if isEnergy then (if isOpen then Image_gem_open_energy_hopo else Image_gem_energy_hopo) else hopoImage
                  Tap   -> if isEnergy then (if isOpen then Image_gem_open_energy_tap  else Image_gem_energy_tap ) else tapImage
                x' = targetX + offsetX
                y' = if isOpen then y - 3 else y - 5
            drawImage img (toNumber x') (toNumber y') stuff
      in case evt of
        Note    sht -> withNoteType sht
        Sustain sht -> withNoteType sht
        SustainEnd  -> pure unit
  pure $ targetX + (widthFret * 5 + 2) + _M

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
  pure $ targetX + (3 * widthFret + 2) + _M

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
  pure $ targetX + (widthFret * 6 + 2) + _M

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
  pure $ targetX + trackWidth + _M

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

drawProKeys :: forall e. ProKeys -> Int -> Draw e Int
drawProKeys (ProKeys pk) targetX stuff = do
  windowH <- map round $ C.getCanvasHeight stuff.canvas
  let pxToSecsVert px = stuff.pxToSecsVert (windowH - px) + stuff.time
      secsToPxVert secs = windowH - stuff.secsToPxVert (secs - stuff.time)
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
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
        fillRect { x: toNumber xpos, y: 0.0, w: toNumber params.width, h: toNumber windowH } stuff
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
              RailingLight  -> { color: Nothing                   , width: 1  }
              RailingDark   -> { color: Nothing                   , width: 1  }
              WhiteKey      -> { color: Just customize.highwaySolo, width: 11 }
              WhiteKeyShort -> { color: Just customize.highwaySolo, width: 10 }
              BlackKey      -> { color: Just customize.highwaySoloBlackKey    , width: 11 }
        case params.color of
          Nothing -> pure unit
          Just c  -> do
            setFillStyle c stuff
            fillRect { x: toNumber xpos, y: toNumber y1, w: toNumber params.width, h: toNumber $ y2 - y1 } stuff
        drawSoloHighway (xpos + params.width) y1 y2 chunks
      drawSolos L.Nil            = pure unit
      drawSolos (L.Cons _ L.Nil) = pure unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        when b1 $ drawSoloHighway targetX (secsToPxVert s1) (secsToPxVert s2) pkHighway
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc pk.solo \secs _ -> do
    drawImage Image_highway_prokeys_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
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
        , w: 11
        , h: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
    case evt of
      Bar      -> drawImage Image_highway_prokeys_bar      (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage Image_highway_prokeys_beat     (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> drawImage Image_highway_prokeys_halfbeat (toNumber targetX) (toNumber y      ) stuff
  -- Target
  drawImage Image_highway_prokeys_target (toNumber targetX) (toNumber targetY - 5.0) stuff
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
              RangeC -> [{x: toNumber $ targetX + 192, y: y, w: 90.0, h: h}]
              RangeD -> [{x: toNumber $ targetX + 2, y: y, w: 22.0, h: h}, {x: toNumber $ targetX + 203, y: y, w: 79.0, h: h}]
              RangeE -> [{x: toNumber $ targetX + 2, y: y, w: 44.0, h: h}, {x: toNumber $ targetX + 225, y: y, w: 57.0, h: h}]
              RangeF -> [{x: toNumber $ targetX + 2, y: y, w: 56.0, h: h}, {x: toNumber $ targetX + 247, y: y, w: 35.0, h: h}]
              RangeG -> [{x: toNumber $ targetX + 2, y: y, w: 78.0, h: h}, {x: toNumber $ targetX + 270, y: y, w: 12.0, h: h}]
              RangeA -> [{x: toNumber $ targetX + 2, y: y, w: 100.0, h: h}]
            in for_ rects \rect -> fillRect rect stuff
        drawRanges rest
  drawRanges rangeEdges
  -- Sustains
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    let thisPitch = fromMaybe Map.empty $ Map.lookup pitch pk.notes
        isEnergy secs = case Map.lookupLE secs pk.energy of
          Just {value: bool} -> bool
          Nothing            -> false
        hitAtY = if customize.autoplay then targetY else windowH + 100
        drawSustainBlock ystart yend energy = when (ystart < hitAtY || yend < hitAtY) do
          let ystart' = min ystart hitAtY
              yend'   = min yend   hitAtY
              sustaining = customize.autoplay && (targetY < ystart || targetY < yend)
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
          fillRect { x: toNumber $ targetX + offsetX' + 2, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          fillRect { x: toNumber $ targetX + offsetX' + 8, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.light stuff
          fillRect { x: toNumber $ targetX + offsetX' + 3, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          setFillStyle shades.normal stuff
          fillRect { x: toNumber $ targetX + offsetX' + 4, y: toNumber ystart', w: 3.0, h: toNumber h } stuff
          setFillStyle shades.dark stuff
          fillRect { x: toNumber $ targetX + offsetX' + 7, y: toNumber ystart', w: 1.0, h: toNumber h } stuff
          when sustaining do
            setFillStyle shades.light stuff
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: if isBlack then 9.0 else 11.0, h: 8.0 } stuff
        go False (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisPitch of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) windowH $ isEnergy secsStart
            go False rest
          _ -> unsafeThrow "during prokeys drawing: found a sustain end not preceded by sustain start"
        go True (L.Cons (Tuple _ SustainEnd) rest) = go False rest
        go _ (L.Cons (Tuple _ (Note (_ :: Unit))) rest) = go False rest
        go _ (L.Cons (Tuple secsStart (Sustain (_ :: Unit))) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> 0
                L.Cons (Tuple secsEnd _) _ -> secsToPxVert secsEnd
          drawSustainBlock pxEnd (secsToPxVert secsStart) $ isEnergy secsStart
          go True rest
        go _ L.Nil = pure unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisPitch) of
      L.Nil -> case Map.lookupLT (pxToSecsVert windowH) thisPitch of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain (_ :: Unit) } ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> pure unit
      events -> go False events
  -- Sustain ends
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    zoomDesc (fromMaybe Map.empty $ Map.lookup pitch pk.notes) \secs evt -> case evt of
      SustainEnd -> do
        let futureSecs = secToNum $ secs - stuff.time
        if customize.autoplay && futureSecs <= 0.0
          then pure unit -- note is in the past or being hit now
          else drawImage Image_sustain_key_end
            (toNumber $ targetX + offsetX - if isBlack then 1 else 0)
            (toNumber $ secsToPxVert secs)
            stuff
      _ -> pure unit
  -- Notes
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    zoomDesc (fromMaybe Map.empty $ Map.lookup pitch pk.notes) \secs evt -> case evt of
      SustainEnd -> pure unit
      _          -> do
        let futureSecs = secToNum $ secs - stuff.time
            isGlissando = case Map.lookupLE secs pk.gliss of
              Just {value: bool} -> bool
              Nothing            -> false
        if customize.autoplay && futureSecs <= 0.0
          then do
            -- note is in the past or being hit now
            if (-0.1) < futureSecs
              then do
                let colors = if isBlack then customize.sustainBlackKey else customize.sustainWhiteKey
                setFillStyle (colors.hit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: if isBlack then 9.0 else 11.0, h: 8.0 } stuff
              else pure unit
          else do
            let y = secsToPxVert secs
                isEnergy = case Map.lookupLE secs pk.energy of
                  Just {value: bool} -> bool
                  Nothing            -> false
                img = if isEnergy
                  then if isBlack then Image_gem_blackkey_energy else Image_gem_whitekey_energy
                  else if isBlack then Image_gem_blackkey        else Image_gem_whitekey
            drawImage img (toNumber $ targetX + offsetX) (toNumber $ y - 5) stuff
            when isGlissando do
              onContext (C.setStrokeStyle customize.glissandoBorder) stuff
              onContext (C.setLineWidth 1.0) stuff
              strokeRect { x: toNumber (targetX + offsetX) + 0.5, y: toNumber y - 4.5, w: 12.0, h: 9.0 } stuff
  pure $ targetX + 282 + _M

zoomAscDoPadding :: forall k a m. (Ord k) => (Monad m) => k -> k -> Map.Map k a -> (k -> a -> m Unit) -> m Unit
zoomAscDoPadding k1 k2 m act = do
  case Map.lookupLE k1 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> do
      -- hack for vocal slides: two padding events before the left edge
      -- so that "(note start)+ (note end) (screen left edge) (note start)" works
      case Map.lookupLT k m of
        Nothing -> pure unit
        Just { key: k', value: v' } -> act k' v'
      act k v
  Map.zoomAscDo k1 k2 m act
  case Map.lookupGE k2 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v

zoomDescDoPadding :: forall k a m. (Ord k) => (Monad m) => k -> k -> Map.Map k a -> (k -> a -> m Unit) -> m Unit
zoomDescDoPadding k1 k2 m act = do
  case Map.lookupGE k2 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v
  Map.zoomDescDo k1 k2 m act
  case Map.lookupLE k1 m of
    Nothing -> pure unit
    Just { key: k, value: v } -> act k v

slide :: Number -> Number -> Number -> Number -> Number -> Number
slide t1 t2 tx v1 v2 = if t1 == t2
  then (v1 + v2) / 2.0
  else v1 + (v2 - v1) * ((tx - t1) / (t2 - t1))

secToNum :: Seconds -> Number
secToNum (Seconds n) = n

drawVocal :: forall e. Vocal -> Int -> Draw e Int
drawVocal (Vocal v) targetY stuff = do
  windowW <- map round $ C.getCanvasWidth stuff.canvas
  let pxToSecsHoriz px = stuff.pxToSecsHoriz px + stuff.time
      secsToPxHoriz secs = stuff.secsToPxHoriz $ secs - stuff.time
      minSecs = pxToSecsHoriz (-100)
      maxSecs = pxToSecsHoriz $ windowW + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = zoomDescDoPadding minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = zoomAscDoPadding minSecs maxSecs
      targetX = secsToPxHoriz stuff.time
  setFillStyle customize.vocalNoteArea stuff
  fillRect { x: 0.0, y: toNumber targetY + 25.0, w: toNumber windowW, h: 130.0 } stuff
  setFillStyle customize.lyricLaneBottom stuff
  fillRect { x: 0.0, y: toNumber targetY + 155.0, w: toNumber windowW, h: 25.0 } stuff
  setFillStyle customize.lyricLaneTop stuff
  fillRect { x: 0.0, y: toNumber targetY, w: toNumber windowW, h: 25.0 } stuff
  -- Draw note pitches
  -- TODO: draw all pitch lines before talkies
  -- TODO: draw harmony unisons better
  let thisRange = case Map.lookupLE stuff.time v.ranges of
        Nothing -> { min: 36.0, max: 84.0 }
        Just { value: VocalRange rmin rmax } -> { min: toNumber rmin, max: toNumber rmax }
        Just { key: t1, value: VocalRangeShift } -> case Map.lookupGT stuff.time v.ranges of
          Just { key: t2, value: VocalRange bmin bmax } -> case Map.lookupLT t1 v.ranges of
            Just { value: VocalRange amin amax } ->
              { min: slide (secToNum t1) (secToNum t2) (secToNum stuff.time) (toNumber amin) (toNumber bmin)
              , max: slide (secToNum t1) (secToNum t2) (secToNum stuff.time) (toNumber amax) (toNumber bmax)
              }
            _ -> unsafeThrow "not a valid range shift"
          _ -> unsafeThrow "not a valid range shift"
      pitchToY p = toNumber targetY + slide thisRange.min thisRange.max (toNumber p) 143.0 37.0
      drawLines :: Maybe (Tuple Seconds Int) -> L.List (Tuple Seconds VocalNote) -> Eff (canvas :: C.CANVAS | e) Unit
      drawLines (Just (Tuple t1 p1)) evts@(L.Cons (Tuple t2 (VocalStart lyric (Just p2))) _) | lyric == "+" || lyric == "+$" = do
        -- draw line from (t1,p1) to (t2,p2)
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p1)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p2)) stuff
        drawLines Nothing evts
      drawLines (Just _) evts = drawLines Nothing evts -- ignore last note-off because no slide
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ (Just p))) (L.Cons (Tuple t2 VocalEnd) rest)) = do
        -- draw line from (t1,p) to (t2,p)
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p)) stuff
        drawLines (Just (Tuple t2 p)) rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ (Just p))) rest@(L.Cons (Tuple t2 (VocalStart _ _)) _)) = do
        -- draw line from (t1,p) to (t2,p)
        -- this case only happens in sloppy vox charts with no gap between notes
        onContext (\ctx -> C.moveTo ctx (toNumber $ secsToPxHoriz t1) (pitchToY p)) stuff
        onContext (\ctx -> C.lineTo ctx (toNumber $ secsToPxHoriz t2) (pitchToY p)) stuff
        drawLines (Just (Tuple t2 p)) rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ Nothing)) (L.Cons (Tuple t2 VocalEnd) rest)) = do
        -- draw talky from t1 to t2
        fillRect { x: toNumber $ secsToPxHoriz t1, y: toNumber targetY + 25.0, w: toNumber $ secsToPxHoriz t2 - secsToPxHoriz t1, h: 130.0 } stuff
        drawLines Nothing rest
      drawLines Nothing (L.Cons (Tuple t1 (VocalStart _ Nothing)) rest@(L.Cons (Tuple t2 (VocalStart _ _)) _)) = do
        -- draw talky from t1 to t2
        -- this case only happens in sloppy vox charts with no gap between notes
        fillRect { x: toNumber $ secsToPxHoriz t1, y: toNumber targetY + 25.0, w: toNumber $ secsToPxHoriz t2 - secsToPxHoriz t1, h: 130.0 } stuff
        drawLines Nothing rest
      drawLines Nothing (L.Cons (Tuple _ (VocalStart _ _)) L.Nil) = pure unit -- off-screen
      drawLines _ L.Nil = pure unit
      drawLines Nothing (L.Cons (Tuple _ VocalEnd) rest) = drawLines Nothing rest
      lineParts =
        [ { part: v.harm2, line: customize.harm2Pitch, talky: customize.harm2Talky, width: 6.0 }
        , { part: v.harm3, line: customize.harm3Pitch, talky: customize.harm3Talky, width: 5.0 }
        , { part: v.harm1, line: customize.harm1Pitch, talky: customize.harm1Talky, width: 4.0 }
        ]
  onContext (C.setLineCap C.Round) stuff
  for_ lineParts \o -> do
    onContext C.beginPath stuff
    onContext (C.setStrokeStyle o.line) stuff
    onContext (C.setLineWidth o.width) stuff
    onContext (C.setFillStyle o.talky) stuff
    drawLines Nothing $ L.fromFoldable $ Map.doTupleArray (zoomAsc o.part)
    onContext C.stroke stuff
    onContext C.closePath stuff
  -- Draw text
  let lyricParts =
        [ { part: v.harm1, y: targetY + 174, isHarm3: false }
        , { part: v.harm2, y: targetY + 20, isHarm3: false }
        , { part: v.harm3, y: targetY + 20, isHarm3: true }
        ]
      harm2Lyric t = case Map.lookup t v.harm2 of
        Nothing -> Nothing
        Just VocalEnd -> Nothing
        Just (VocalStart lyric _) -> Just lyric
      getLyrics
        :: Boolean
        -> L.List (Tuple Seconds VocalNote)
        -> L.List {time :: Seconds, lyric :: String, isTalky :: Boolean}
      getLyrics isHarm3 = L.mapMaybe \(Tuple t vn) -> case vn of
        VocalEnd -> Nothing
        VocalStart lyric pitch
          | lyric == "+" -> Nothing
          | R.test (either unsafeThrow id $ R.regex "\\$$" noFlags) lyric -> Nothing
          | isHarm3 && harm2Lyric t == Just lyric -> Nothing
          | otherwise -> Just
            { time: t
            , lyric: R.replace (either unsafeThrow id $ R.regex "=$" noFlags) "-" lyric
            , isTalky: isNothing pitch
            }
          -- TODO: support §
      drawLyrics
        :: Number
        -> Number
        -> L.List {time :: Seconds, lyric :: String, isTalky :: Boolean}
        -> Eff (canvas :: C.CANVAS | e) Unit
      drawLyrics _    _     L.Nil           = pure unit
      drawLyrics minX textY (L.Cons o rest) = do
        let textX = max minX $ toNumber $ secsToPxHoriz o.time
        onContext (C.setFont $ if o.isTalky
          then customize.lyricFontTalky
          else customize.lyricFont
          ) stuff
        setFillStyle (case Map.lookupLE o.time v.energy of
          Nothing -> customize.lyricColor
          Just { value: isEnergy } ->
            if isEnergy then customize.lyricColorEnergy else customize.lyricColor
          ) stuff
        metric <- measureText o.lyric stuff
        onContext (\ctx -> C.fillText ctx o.lyric textX textY) stuff
        drawLyrics (textX + metric.width + 5.0) textY rest
      mergeTime
        :: forall a t. (Ord t)
        => L.List {time :: t | a}
        -> L.List {time :: t | a}
        -> L.List {time :: t | a}
      mergeTime L.Nil ly = ly
      mergeTime lx L.Nil = lx
      mergeTime lx@(L.Cons x tx) ly@(L.Cons y ty) = if x.time <= y.time
        then L.Cons x $ mergeTime tx ly
        else L.Cons y $ mergeTime lx ty
  drawLyrics (-999.0) (toNumber targetY + 174.0) $
    getLyrics false $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm1)
  drawLyrics (-999.0) (toNumber targetY + 20.0) $ mergeTime
    (getLyrics false $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm2))
    (getLyrics true  $ L.fromFoldable $ Map.doTupleArray (zoomAsc v.harm3))
  -- Draw percussion notes
  zoomDesc v.percussion \t (_ :: Unit) -> if t > stuff.time
    then do
      setFillStyle customize.percussionOuter stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 11.0 } stuff
      setFillStyle customize.percussionInner stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 9.0 } stuff
    else do
      let opacity = (secToNum (t - stuff.time) + 0.1) / 0.05
      when (opacity > 0.0) $ do
        setFillStyle (customize.percussionHit opacity) stuff
        fillCircle { x: toNumber $ secsToPxHoriz stuff.time, y: toNumber targetY + 90.0, r: 11.0 } stuff
  -- Draw phrase ends
  setFillStyle customize.vocalPhraseEnd stuff
  zoomDesc v.phrases \t (_ :: Unit) -> do
    fillRect { x: toNumber (secsToPxHoriz t) - 1.0, y: toNumber targetY + 25.0, w: 3.0, h: 130.0 } stuff
  -- Draw target line
  setFillStyle customize.vocalTargetLine stuff
  fillRect { x: toNumber targetX - 1.0, y: toNumber targetY + 25.0, w: 3.0, h: 130.0 } stuff
  pure $ targetY + 180 + _M
