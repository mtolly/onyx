module Draw (Settings, App(..), DrawStuff(), draw, _M, _B, getWindowDims) where

import Prelude
import Graphics.Canvas as C
import Data.Time.Duration (Seconds(..))
import Control.Monad.Eff (Eff)
import Data.Int (toNumber, round)
import DOM (DOM)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Array (reverse, uncons, cons, snoc, take, zip, (..), length, concat)
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
import Images (ImageID(..))
import OnyxMap as Map

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

draw :: forall e. Draw (dom :: DOM | e) Unit
draw stuff = do
  {w: windowW, h: windowH} <- getWindowDims
  void $ C.setCanvasWidth  windowW stuff.canvas
  void $ C.setCanvasHeight windowH stuff.canvas
  setFillStyle "rgb(54,59,123)" stuff
  fillRect { x: 0.0, y: 0.0, w: windowW, h: windowH } stuff
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
  setFillStyle "black" stuff
  fillRect { x: toNumber _M, y: toNumber _M, w: toNumber _B, h: timelineH + 2.0 } stuff
  setFillStyle "white" stuff
  fillRect { x: toNumber _M + 1.0, y: toNumber _M + 1.0, w: toNumber _B - 2.0, h: timelineH } stuff
  setFillStyle "rgb(100,130,255)" stuff
  fillRect
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , w: toNumber _B - 2.0
    , h: timelineH * filled
    } stuff

-- | Height/width of margins
_M :: Int
_M = 20

-- | Height/width of buttons
_B :: Int
_B = 41

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
  setFillStyle "rgb(60,60,60)" stuff
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
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
  -- Highway
  setFillStyle "rgb(126,126,150)" stuff
  fillRect { x: toNumber targetX, y: 0.0, w: 182.0, h: toNumber windowH } stuff
  setFillStyle "rgb(184,185,204)" stuff
  for_ [0, 36, 72, 108, 144, 180] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle "black" stuff
  for_ [1, 37, 73, 109, 145, 181] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle "rgb(91,137,185)" stuff
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
        when b1 $ for_ [2, 38, 74, 110, 146] \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 34.0, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc five.solo \secs _ -> do
    drawImage Image_highway_grybo_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        -- TODO open
        [ {x:   2, gem: _.green }
        , {x:  38, gem: _.red   }
        , {x:  74, gem: _.yellow}
        , {x: 110, gem: _.blue  }
        , {x: 146, gem: _.orange}
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
        , w: 34
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
  drawImage Image_highway_grybo_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  let colors =
        [ { c: _.open  , x: 1  , strum: Image_gem_open  , hopo: Image_gem_open_hopo, tap: Image_gem_open_tap
          , shades: { light: "rgb(214,154,242)", normal: "rgb(167, 25,241)", dark: "rgb(128, 12,188)" }
          , hit: \o -> "rgba(210,162,255," <> show o <> ")"
          , open: true
          }
        , { c: _.green , x: 1  , strum: Image_gem_green , hopo: Image_gem_green_hopo, tap: Image_gem_green_tap
          , shades: { light: "rgb(135,247,126)", normal: "rgb( 21,218,  2)", dark: "rgb( 13,140,  2)" }
          , hit: \o -> "rgba(190,255,192," <> show o <> ")"
          , open: false
          }
        , { c: _.red   , x: 37 , strum: Image_gem_red   , hopo: Image_gem_red_hopo, tap: Image_gem_red_tap
          , shades: { light: "rgb(247,127,158)", normal: "rgb(218,  2, 62)", dark: "rgb(140,  2, 40)" }
          , hit: \o -> "rgba(255,188,188," <> show o <> ")"
          , open: false
          }
        , { c: _.yellow, x: 73 , strum: Image_gem_yellow, hopo: Image_gem_yellow_hopo, tap: Image_gem_yellow_tap
          , shades: { light: "rgb(247,228,127)", normal: "rgb(218,180,  2)", dark: "rgb(140,115,  3)" }
          , hit: \o -> "rgba(255,244,151," <> show o <> ")"
          , open: false
          }
        , { c: _.blue  , x: 109, strum: Image_gem_blue  , hopo: Image_gem_blue_hopo, tap: Image_gem_blue_tap
          , shades: { light: "rgb(119,189,255)", normal: "rgb(  2,117,218)", dark: "rgb(  3, 76,140)" }
          , hit: \o -> "rgba(190,198,255," <> show o <> ")"
          , open: false
          }
        , { c: _.orange, x: 145, strum: Image_gem_orange, hopo: Image_gem_orange_hopo, tap: Image_gem_orange_tap
          , shades: { light: "rgb(255,183,119)", normal: "rgb(218, 97,  4)", dark: "rgb(140, 63,  3)" }
          , hit: \o -> "rgba(231,196,112," <> show o <> ")"
          , open: false
          }
        ]
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades, open: isOpen } -> do
    let thisColor = getColor five.notes
        offsetX' = if isOpen then 73 else offsetX
        isEnergy secs = case Map.lookupLE secs five.energy of
          Nothing           -> false
          Just { value: v } -> v
        drawSustainBlock ystart yend energy = when (ystart < targetY || yend < targetY) do
          let ystart' = min ystart targetY
              yend'   = min yend   targetY
              sustaining = targetY < ystart || targetY < yend
              shades = if energy
                then { light: "rgb(137,235,204)", normal: "rgb(138,192,175)", dark: "rgb(124,158,149)" }
                else normalShades
              h = yend' - ystart' + 1
          setFillStyle "black" stuff
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
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
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
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, tap: tapImage, hit: shadeHit, open: isOpen } -> do
    zoomDesc (getColor five.notes) \secs evt -> do
      let futureSecs = secToNum $ secs - stuff.time
      if futureSecs <= 0.0
        then do
          -- note is in the past or being hit now
          let offsetX' = if isOpen then 73 else offsetX
          if (-0.1) < futureSecs
            then case evt of
              SustainEnd -> pure unit
              _ -> do
                setFillStyle (shadeHit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
            else pure unit
        else do
          let y = secsToPxVert secs
              isEnergy = case Map.lookupLE secs five.energy of
                Just {value: bool} -> bool
                Nothing            -> false
              img = case evt of
                SustainEnd    -> Image_sustain_end
                Note    Strum -> if isEnergy then (if isOpen then Image_gem_open_energy      else Image_gem_energy     ) else strumImage
                Sustain Strum -> if isEnergy then (if isOpen then Image_gem_open_energy      else Image_gem_energy     ) else strumImage
                Note    HOPO  -> if isEnergy then (if isOpen then Image_gem_open_energy_hopo else Image_gem_energy_hopo) else hopoImage
                Sustain HOPO  -> if isEnergy then (if isOpen then Image_gem_open_energy_hopo else Image_gem_energy_hopo) else hopoImage
                Note    Tap   -> if isEnergy then (if isOpen then Image_gem_open_energy_tap  else Image_gem_energy_tap ) else tapImage
                Sustain Tap   -> if isEnergy then (if isOpen then Image_gem_open_energy_tap  else Image_gem_energy_tap ) else tapImage
              x' = targetX + case evt of
                SustainEnd -> if isOpen then 73 else offsetX
                _          -> offsetX
              y' = case evt of
                SustainEnd -> y
                _          -> if isOpen then y - 3 else y - 5
          drawImage img (toNumber x') (toNumber y') stuff
  pure $ targetX + 182 + _M

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
      maxSecs = pxToSecsVert (-100)
      minSecs = pxToSecsVert $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPxVert stuff.time
  -- Highway
  setFillStyle "rgb(126,126,150)" stuff
  fillRect { x: toNumber targetX, y: 0.0, w: 110.0, h: toNumber windowH } stuff
  setFillStyle "rgb(184,185,204)" stuff
  for_ [0, 36, 72, 108] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle "black" stuff
  for_ [1, 37, 73, 109] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle "rgb(91,137,185)" stuff
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
        when b1 $ for_ [2, 38, 74] \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 34.0, h: toNumber $ y1 - y2 } stuff
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
        [ { c: _.open, x: 1, color: SixOpen }
        , { c: _.b1, x: 1, color: SixBlack }
        , { c: _.b2, x: 37, color: SixBlack }
        , { c: _.b3, x: 73, color: SixBlack }
        , { c: _.w1, x: 1, color: SixWhite }
        , { c: _.w2, x: 37, color: SixWhite }
        , { c: _.w3, x: 73, color: SixWhite }
        , { c: _.bw1, x: 1, color: SixBoth }
        , { c: _.bw2, x: 37, color: SixBoth }
        , { c: _.bw3, x: 73, color: SixBoth }
        ]
      getShades sc = case sc of
        SixBlack -> { light: "rgb(121,121,121)", normal: "rgb(70,70,70)"   , dark: "rgb(45,45,45)"    }
        _        -> { light: "rgb(220,220,220)", normal: "rgb(181,181,181)", dark: "rgb(162,162,162)" }
      getHitShade _ o = "rgba(200,200,200," <> show o <> ")"
      getGemImages sc = case sc of
        SixBlack -> { strum: Image_gem_black, hopo: Image_gem_black_hopo, tap: Image_gem_black_tap, energy: Image_gem_ghl_energy }
        SixWhite -> { strum: Image_gem_white, hopo: Image_gem_white_hopo, tap: Image_gem_white_tap, energy: Image_gem_ghl_energy }
        SixBoth  -> { strum: Image_gem_blackwhite, hopo: Image_gem_blackwhite_hopo, tap: Image_gem_blackwhite_tap, energy: Image_gem_ghl_energy }
        SixOpen  -> { strum: Image_gem_openghl, hopo: Image_gem_openghl_hopo, tap: Image_gem_openghl_tap, energy: Image_gem_openghl_energy }
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let thisEvents = getEvents six.notes
        offsetX' = case thisColor of
          SixOpen -> 37
          _ -> offsetX
        isEnergy secs = case Map.lookupLE secs six.energy of
          Nothing           -> false
          Just { value: v } -> v
        drawSustainBlock ystart yend energy = when (ystart < targetY || yend < targetY) do
          let ystart' = min ystart targetY
              yend'   = min yend   targetY
              sustaining = targetY < ystart || targetY < yend
              shades = if energy
                then { light: "rgb(137,235,204)", normal: "rgb(138,192,175)", dark: "rgb(124,158,149)" }
                else getShades thisColor
              h = yend' - ystart' + 1
          setFillStyle "black" stuff
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
            fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
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
  -- Notes
  for_ colors \{ c: getEvents, x: offsetX, color: thisColor } -> do
    let {strum: strumImage, hopo: hopoImage, tap: tapImage, energy: energyOverlay} = getGemImages thisColor
    zoomDesc (getEvents six.notes) \secs evt -> do
      let futureSecs = secToNum $ secs - stuff.time
      if futureSecs <= 0.0
        then do
          -- note is in the past or being hit now
          let offsetX' = case thisColor of
                SixOpen -> 37
                _ -> offsetX
          if (-0.1) < futureSecs
            then case evt of
              SustainEnd -> pure unit
              _ -> do
                setFillStyle (getHitShade thisColor $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX' + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
            else pure unit
        else do
          let y = secsToPxVert secs
              isEnergy = case Map.lookupLE secs six.energy of
                Just {value: bool} -> bool
                Nothing            -> false
              img = case evt of
                SustainEnd    -> Image_sustain_end
                Note    Strum -> strumImage
                Sustain Strum -> strumImage
                Note    HOPO  -> hopoImage
                Sustain HOPO  -> hopoImage
                Note    Tap   -> tapImage
                Sustain Tap   -> tapImage
              x' = targetX + case evt of
                SustainEnd -> case thisColor of
                  SixOpen -> 37
                  _ -> offsetX
                _          -> offsetX
              y' = case evt of
                SustainEnd -> y
                _          -> case thisColor of
                  SixOpen -> y - 3
                  _ -> y - 5
          drawImage img (toNumber x') (toNumber y') stuff
          case evt of
            SustainEnd -> pure unit
            _ -> when isEnergy $ drawImage energyOverlay (toNumber x') (toNumber y') stuff
  pure $ targetX + 110 + _M

drawProtar :: forall e. Protar -> Int -> Draw e Int
drawProtar (Protar protar) targetX stuff = do
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
  setFillStyle "rgb(126,126,150)" stuff
  fillRect { x: toNumber targetX, y: 0.0, w: 182.0, h: toNumber windowH } stuff
  setFillStyle "rgb(184,185,204)" stuff
  for_ [0, 30, 60, 90, 120, 150, 180] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle "black" stuff
  for_ [1, 31, 61, 91, 121, 151, 181] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle "rgb(91,137,185)" stuff
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
        when b1 $ for_ [2, 32, 62, 92, 122, 152] \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 28.0, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc protar.solo \secs _ -> do
    drawImage Image_highway_grybo_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        [ {x:   2, gem: _.s6}
        , {x:  32, gem: _.s5}
        , {x:  62, gem: _.s4}
        , {x:  92, gem: _.s3}
        , {x: 122, gem: _.s2}
        , {x: 152, gem: _.s1}
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
        , w: 28
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
  drawImage Image_highway_protar_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Sustains
  let colors =
        [ { c: _.s6, x: 1  , strum: Image_gem_red_pro   , hopo: Image_gem_red_pro_hopo
          , shades: { light: "rgb(247,127,158)", normal: "rgb(218,  2, 62)", dark: "rgb(140,  2, 40)" }
          , hit: \o -> "rgba(255,188,188," <> show o <> ")"
          }
        , { c: _.s5, x: 31 , strum: Image_gem_green_pro , hopo: Image_gem_green_pro_hopo
          , shades: { light: "rgb(135,247,126)", normal: "rgb( 21,218,  2)", dark: "rgb( 13,140,  2)" }
          , hit: \o -> "rgba(190,255,192," <> show o <> ")"
          }
        , { c: _.s4, x: 61 , strum: Image_gem_orange_pro, hopo: Image_gem_orange_pro_hopo
          , shades: { light: "rgb(255,183,119)", normal: "rgb(218, 97,  4)", dark: "rgb(140, 63,  3)" }
          , hit: \o -> "rgba(231,196,112," <> show o <> ")"
          }
        , { c: _.s3, x: 91, strum: Image_gem_blue_pro  , hopo: Image_gem_blue_pro_hopo
          , shades: { light: "rgb(119,189,255)", normal: "rgb(  2,117,218)", dark: "rgb(  3, 76,140)" }
          , hit: \o -> "rgba(190,198,255," <> show o <> ")"
          }
        , { c: _.s2, x: 121, strum: Image_gem_yellow_pro, hopo: Image_gem_yellow_pro_hopo
          , shades: { light: "rgb(247,228,127)", normal: "rgb(218,180,  2)", dark: "rgb(140,115,  3)" }
          , hit: \o -> "rgba(255,244,151," <> show o <> ")"
          }
        , { c: _.s1, x: 151, strum: Image_gem_purple_pro, hopo: Image_gem_purple_pro_hopo
          , shades: { light: "rgb(214,154,242)", normal: "rgb(167, 25,241)", dark: "rgb(128, 12,188)" }
          , hit: \o -> "rgba(210,162,255," <> show o <> ")"
          }
        ]
  for_ colors \{ c: getColor, x: offsetX, shades: normalShades } -> do
    let thisColor = getColor protar.notes
        isEnergy secs = case Map.lookupLE secs protar.energy of
          Nothing           -> false
          Just { value: v } -> v
        drawSustainBlock ystart yend energy = when (ystart < targetY || yend < targetY) do
          let ystart' = min ystart targetY
              yend'   = min yend   targetY
              sustaining = targetY < ystart || targetY < yend
              shades = if energy
                then { light: "rgb(137,235,204)", normal: "rgb(138,192,175)", dark: "rgb(124,158,149)" }
                else normalShades
              h = yend' - ystart' + 1
          setFillStyle "black" stuff
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
            fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: 29.0, h: 8.0 } stuff
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisColor of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPxVert secsEnd) windowH $ isEnergy secsStart
            go false rest
          _ -> unsafeThrow "during protar drawing: found a sustain end not preceded by sustain start"
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
  -- Notes
  for_ colors \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, hit: shadeHit } -> do
    zoomDesc (getColor protar.notes) \secs evt -> do
      let futureSecs = secToNum $ secs - stuff.time
      if futureSecs <= 0.0
        then do
          -- note is in the past or being hit now
          if (-0.1) < futureSecs
            then case evt of
              SustainEnd -> pure unit
              _ -> do
                setFillStyle (shadeHit $ (futureSecs + 0.1) / 0.05) stuff
                fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: 29.0, h: 8.0 } stuff
            else pure unit
        else do
          let y = secsToPxVert secs
              isEnergy = case Map.lookupLE secs protar.energy of
                Just {value: bool} -> bool
                Nothing            -> false
              fretImage  0 = Image_pro_fret_00
              fretImage  1 = Image_pro_fret_01
              fretImage  2 = Image_pro_fret_02
              fretImage  3 = Image_pro_fret_03
              fretImage  4 = Image_pro_fret_04
              fretImage  5 = Image_pro_fret_05
              fretImage  6 = Image_pro_fret_06
              fretImage  7 = Image_pro_fret_07
              fretImage  8 = Image_pro_fret_08
              fretImage  9 = Image_pro_fret_09
              fretImage 10 = Image_pro_fret_10
              fretImage 11 = Image_pro_fret_11
              fretImage 12 = Image_pro_fret_12
              fretImage 13 = Image_pro_fret_13
              fretImage 14 = Image_pro_fret_14
              fretImage 15 = Image_pro_fret_15
              fretImage 16 = Image_pro_fret_16
              fretImage 17 = Image_pro_fret_17
              fretImage 18 = Image_pro_fret_18
              fretImage 19 = Image_pro_fret_19
              fretImage 20 = Image_pro_fret_20
              fretImage 21 = Image_pro_fret_21
              fretImage 22 = Image_pro_fret_22
              fretImage _  = unsafeThrow "invalid fret number"
          case evt of
            SustainEnd                                                -> drawImage Image_sustain_end                                                      (toNumber $ targetX + offsetX - 3) (toNumber   y      ) stuff
            Note    (ProtarNote { noteType: Strum, fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute      else Image_gem_mute)      (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: Strum, fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute      else Image_gem_mute)      (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Note    (ProtarNote { noteType: HOPO , fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute_hopo else Image_gem_mute_hopo) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: HOPO , fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute_hopo else Image_gem_mute_hopo) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Note    (ProtarNote { noteType: Tap  , fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute_hopo else Image_gem_mute_hopo) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: Tap  , fret: Nothing   }) -> drawImage (if isEnergy then Image_gem_energy_mute_hopo else Image_gem_mute_hopo) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Note    (ProtarNote { noteType: Strum, fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro      else strumImage) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: Strum, fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro      else strumImage) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Note    (ProtarNote { noteType: HOPO , fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro_hopo else hopoImage ) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: HOPO , fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro_hopo else hopoImage ) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Note    (ProtarNote { noteType: Tap  , fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro_hopo else hopoImage ) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
            Sustain (ProtarNote { noteType: Tap  , fret: Just fret }) -> do
              drawImage (if isEnergy then Image_gem_energy_pro_hopo else hopoImage ) (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
              drawImage (fretImage fret)                                             (toNumber $ targetX + offsetX    ) (toNumber $ y - 10 ) stuff
  pure $ targetX + 182 + _M

drawDrums :: forall e. Drums -> Int -> Draw e Int
drawDrums (Drums drums) targetX stuff = do
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
  setFillStyle "rgb(126,126,150)" stuff
  fillRect { x: toNumber targetX, y: 0.0, w: 146.0, h: toNumber windowH } stuff
  setFillStyle "rgb(184,185,204)" stuff
  for_ [0, 36, 72, 108, 144] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  setFillStyle "black" stuff
  for_ [1, 37, 73, 109, 145] \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH } stuff
  -- Solo highway
  setFillStyle "rgb(91,137,185)" stuff
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
        when b1 $ for_ [2, 38, 74, 110] \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 34.0, h: toNumber $ y1 - y2 } stuff
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc drums.solo \secs _ -> do
    drawImage Image_highway_drums_solo_edge (toNumber targetX) (toNumber $ secsToPxVert secs) stuff
  -- Lanes
  let lanes =
        -- TODO kick
        [ {x:   2, gem: Red }
        , {x:  38, gem: YCym}
        , {x:  38, gem: YTom}
        , {x:  74, gem: BCym}
        , {x:  74, gem: BTom}
        , {x: 110, gem: GCym}
        , {x: 110, gem: GTom}
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
        , w: 34
        , h: y1 - y2
        } stuff
      drawLanes rest
    in drawLanes laneEdges
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) \secs evt -> do
    let y = secsToPxVert secs
    case evt of
      Bar      -> drawImage Image_highway_drums_bar      (toNumber targetX) (toNumber y - 1.0) stuff
      Beat     -> drawImage Image_highway_drums_beat     (toNumber targetX) (toNumber y - 1.0) stuff
      HalfBeat -> drawImage Image_highway_drums_halfbeat (toNumber targetX) (toNumber y      ) stuff
  -- Target
  drawImage Image_highway_drums_target (toNumber targetX) (toNumber targetY - 5.0) stuff
  -- Notes
  zoomDesc drums.notes \secs evts -> do
    let futureSecs = secToNum $ secs - stuff.time
    if futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = (futureSecs + 0.1) / 0.05
                kick = do
                  setFillStyle ("rgba(231, 196, 112, " <> show opacity <> ")") stuff
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY - 5, w: 143.0, h: 1.0 } stuff
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY + 4, w: 143.0, h: 1.0 } stuff
                red = do
                  setFillStyle ("rgba(255, 188, 188, " <> show opacity <> ")") stuff
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
                yellow = do
                  setFillStyle ("rgba(255, 244, 151, " <> show opacity <> ")") stuff
                  fillRect { x: toNumber $ targetX + 38, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
                blue = do
                  setFillStyle ("rgba(190, 198, 255, " <> show opacity <> ")") stuff
                  fillRect { x: toNumber $ targetX + 74, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
                green = do
                  setFillStyle ("rgba(190, 255, 192, " <> show opacity <> ")") stuff
                  fillRect { x: toNumber $ targetX + 110, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 } stuff
            for_ evts \e -> case e of
              Kick -> kick
              Red  -> red
              YCym -> yellow
              YTom -> yellow
              BCym -> blue
              BTom -> blue
              GCym -> green
              GTom -> green
          else pure unit
      else do
        -- note is in the future
        let y = secsToPxVert secs
            isEnergy = case Map.lookupLE secs drums.energy of
              Just {value: bool} -> bool
              Nothing            -> false
        for_ evts \e -> case e of
          Kick -> drawImage (if isEnergy then Image_gem_kick_energy   else Image_gem_kick         ) (toNumber $ targetX + 1  ) (toNumber $ y - 3) stuff
          Red  -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_red          ) (toNumber $ targetX + 1  ) (toNumber $ y - 5) stuff
          YTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_yellow       ) (toNumber $ targetX + 37 ) (toNumber $ y - 5) stuff
          YCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_yellow_cymbal) (toNumber $ targetX + 37 ) (toNumber $ y - 8) stuff
          BTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_blue         ) (toNumber $ targetX + 73 ) (toNumber $ y - 5) stuff
          BCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_blue_cymbal  ) (toNumber $ targetX + 73 ) (toNumber $ y - 8) stuff
          GTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_green        ) (toNumber $ targetX + 109) (toNumber $ y - 5) stuff
          GCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_green_cymbal ) (toNumber $ targetX + 109) (toNumber $ y - 8) stuff
  -- TODO: draw all kicks before starting hand gems
  -- Return targetX of next track
  pure $ targetX + 146 + _M

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
              RailingLight  -> { color: "rgb(184,185,205)", width: 1 }
              RailingDark   -> { color: "black"           , width: 1 }
              WhiteKey      -> { color: "rgb(126,126,150)", width: 11 }
              WhiteKeyShort -> { color: "rgb(126,126,150)", width: 10 }
              BlackKey      -> { color: "rgb(105,105,129)", width: 11 }
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
              RailingLight  -> { color: Nothing                , width: 1  }
              RailingDark   -> { color: Nothing                , width: 1  }
              WhiteKey      -> { color: Just "rgb( 91,137,185)", width: 11 }
              WhiteKeyShort -> { color: Just "rgb( 91,137,185)", width: 10 }
              BlackKey      -> { color: Just "rgb( 73,111,149)", width: 11 }
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
  setFillStyle "rgba(0,0,0,0.3)" stuff
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
        drawSustainBlock ystart yend energy = when (ystart < targetY || yend < targetY) do
          let ystart' = min ystart targetY
              yend'   = min yend   targetY
              sustaining = targetY < ystart || targetY < yend
              shades = if energy
                then if isBlack
                  then { light: "rgb( 52,148,117)", normal: "rgb( 71,107, 95)", dark: "rgb( 69, 83, 79)" }
                  else { light: "rgb(137,235,204)", normal: "rgb(138,192,175)", dark: "rgb(124,158,149)" }
                else if isBlack
                  then { light: "rgb(175, 83,201)", normal: "rgb(147, 49,175)", dark: "rgb(123, 42,150)" }
                  else { light: "rgb(199,134,218)", normal: "rgb(184,102,208)", dark: "rgb(178, 86,204)" }
              h = yend' - ystart' + 1
              offsetX' = offsetX + if isBlack then 0 else 1
          setFillStyle "black" stuff
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
  -- Notes
  for_ pitchList \{ pitch: pitch, offsetX: offsetX, isBlack: isBlack } -> do
    zoomDesc (fromMaybe Map.empty $ Map.lookup pitch pk.notes) \secs evt -> do
      let futureSecs = secToNum $ secs - stuff.time
          isGlissando = case Map.lookupLE secs pk.gliss of
            Just {value: bool} -> bool
            Nothing            -> false
      if futureSecs <= 0.0
        then do
          -- note is in the past or being hit now
          if (-0.1) < futureSecs
            then case evt of
              SustainEnd -> pure unit
              _ -> do
                setFillStyle ("rgba(227,193,238," <> show ((futureSecs + 0.1) / 0.05) <> ")") stuff
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
          case evt of
            SustainEnd          -> drawImage Image_sustain_key_end (toNumber $ targetX + offsetX - if isBlack then 1 else 0) (toNumber   y    ) stuff
            Note    (_ :: Unit) -> drawImage img                   (toNumber $ targetX + offsetX                           ) (toNumber $ y - 5) stuff
            Sustain (_ :: Unit) -> drawImage img                   (toNumber $ targetX + offsetX                           ) (toNumber $ y - 5) stuff
          when isGlissando case evt of
            SustainEnd -> pure unit
            _ -> do
              onContext (C.setStrokeStyle "white") stuff
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
  setFillStyle "rgba(0,0,0,0.6)" stuff
  fillRect { x: 0.0, y: toNumber targetY + 25.0, w: toNumber windowW, h: 130.0 } stuff
  setFillStyle "rgba(0,27,89,0.85)" stuff
  fillRect { x: 0.0, y: toNumber targetY + 155.0, w: toNumber windowW, h: 25.0 } stuff
  setFillStyle "rgba(87,55,0,0.85)" stuff
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
        [ { part: v.harm2, line: "rgb(189,67,0)"  , talky: "rgba(189,67,0,0.6)"  , width: 6.0 }
        , { part: v.harm3, line: "rgb(225,148,22)", talky: "rgba(225,148,22,0.6)", width: 5.0 }
        , { part: v.harm1, line: "rgb(46,229,223)", talky: "rgba(46,229,223,0.6)", width: 4.0 }
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
          then "bold italic 17px sans-serif"
          else "bold 17px sans-serif"
          ) stuff
        setFillStyle (case Map.lookupLE o.time v.energy of
          Nothing -> "white"
          Just { value: isEnergy } -> if isEnergy then "yellow" else "white"
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
      setFillStyle "#d9d9d9" stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 11.0 } stuff
      setFillStyle "#00b9c9" stuff
      fillCircle { x: toNumber $ secsToPxHoriz t, y: toNumber targetY + 90.0, r: 9.0 } stuff
    else do
      let opacity = (secToNum (t - stuff.time) + 0.1) / 0.05
      when (opacity > 0.0) $ do
        setFillStyle ("rgba(255, 255, 255, " <> show opacity <> ")") stuff
        fillCircle { x: toNumber $ secsToPxHoriz stuff.time, y: toNumber targetY + 90.0, r: 11.0 } stuff
  -- Draw phrase ends
  setFillStyle "#bbb" stuff
  zoomDesc v.phrases \t (_ :: Unit) -> do
    fillRect { x: toNumber (secsToPxHoriz t) - 1.0, y: toNumber targetY + 25.0, w: 3.0, h: 130.0 } stuff
  -- Draw target line
  setFillStyle "#ddd" stuff
  fillRect { x: toNumber targetX - 1.0, y: toNumber targetY + 25.0, w: 3.0, h: 130.0 } stuff
  pure $ targetY + 180 + _M
