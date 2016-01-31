module Draw where

import Prelude
import qualified Graphics.Canvas as C
import Data.Time
import Images
import Control.Monad.Reader.Trans
import Control.Monad.Eff
import Data.DOM.Simple.Window
import Data.Int (toNumber, round)
import DOM
import qualified OnyxMap as Map
import Data.Traversable (for)
import Data.Maybe
import Data.Array (uncons, cons, snoc)
import qualified Data.List as L
import Data.Tuple
import Control.Monad (when)
import Control.Monad.Eff.Exception.Unsafe (unsafeThrow)
import Data.Ord (min)

import Song

type Settings =
  { seeGuitar  :: Boolean
  , seeBass    :: Boolean
  , seeKeys    :: Boolean
  , seeProKeys :: Boolean
  , seeDrums   :: Boolean
  }

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
  , pxToSecs :: Int -> Seconds -- pixels from bottom -> now-offset in seconds
  , secsToPx :: Seconds -> Int -- now-offset in seconds -> pixels from bottom
  }

type Draw e = ReaderT DrawStuff (Eff (canvas :: C.Canvas | e))

askStuff :: forall e. Draw e DrawStuff
askStuff = ask

setFillStyle :: forall e. String -> Draw e Unit
setFillStyle s = do
  ctx <- map _.context ask
  lift $ void $ C.setFillStyle s ctx

fillRect :: forall e. C.Rectangle -> Draw e Unit
fillRect rect = do
  ctx <- map _.context ask
  lift $ void $ C.fillRect ctx rect

drawImage :: forall e. ImageID -> Number -> Number -> Draw e Unit
drawImage iid x y = do
  ctx <- map _.context ask
  img <- map _.getImage ask
  lift $ void $ C.drawImage ctx (img iid) x y

draw :: forall e. Draw (dom :: DOM | e) Unit
draw = do
  stuff <- ask
  windowW <- lift $ innerWidth  globalWindow
  windowH <- lift $ innerHeight globalWindow
  lift $ C.setCanvasWidth  windowW stuff.canvas
  lift $ C.setCanvasHeight windowH stuff.canvas
  setFillStyle "rgb(54,59,123)"
  fillRect { x: 0.0, y: 0.0, w: windowW, h: windowH }
  let playPause = case stuff.app of
        Paused  _ -> Image_button_play
        Playing _ -> Image_button_pause
  drawImage playPause (toNumber _M) (windowH - toNumber _M - toNumber _B)
  let timelineH = windowH - 3.0 * toNumber _M - toNumber _B - 2.0
      filled = case stuff.time / (case stuff.song of Song o -> o.end) of
        Seconds n -> n
  setFillStyle "black"
  fillRect { x: toNumber _M, y: toNumber _M, w: toNumber _B, h: timelineH + 2.0 }
  setFillStyle "white"
  fillRect { x: toNumber _M + 1.0, y: toNumber _M + 1.0, w: toNumber _B - 2.0, h: timelineH }
  setFillStyle "rgb(100,130,255)"
  fillRect
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , w: toNumber _B - 2.0
    , h: timelineH * filled
    }
  -- Draw the visible instrument tracks in sequence
  let drawTracks targetX trks = case uncons trks of
        Nothing -> return unit
        Just {head: trk, tail: trkt} -> do
          drawResult <- trk targetX
          case drawResult of
            Just targetX' -> drawTracks targetX' trkt
            Nothing       -> drawTracks targetX  trkt
  drawTracks (_M + _B + _M + _B + _M)
    [ drawPart (\(Song o) -> o.guitar) _.seeGuitar drawFive
    , drawPart (\(Song o) -> o.bass  ) _.seeBass   drawFive
    , drawPart (\(Song o) -> o.drums ) _.seeDrums  drawDrums
    , drawPart (\(Song o) -> o.keys  ) _.seeKeys   drawFive
    ]

-- | Height/width of margins
_M :: Int
_M = 20

-- | Height/width of buttons
_B :: Int
_B = 41

drawPart
  :: forall e a
  .  (Song -> Maybe a)
  -> (Settings -> Boolean)
  -> (a -> Int -> Draw e Int)
  -> Int
  -> Draw e (Maybe Int)
drawPart getPart see drawIt targetX = do
  stuff <- askStuff
  let settings = case stuff.app of
        Paused  o -> o.settings
        Playing o -> o.settings
  case getPart stuff.song of
    Just part | see settings -> map Just $ drawIt part targetX
    _                        -> return Nothing

drawFive :: forall e. Five -> Int -> Draw e Int
drawFive (Five five) targetX = do
  stuff <- askStuff
  windowH <- map round $ lift $ C.getCanvasHeight stuff.canvas
  let pxToSecs px = stuff.pxToSecs (windowH - px) + stuff.time
      secsToPx secs = windowH - stuff.secsToPx (secs - stuff.time)
      maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPx stuff.time
  -- Highway
  setFillStyle "rgb(126,126,150)"
  fillRect { x: toNumber targetX, y: 0.0, w: 182.0, h: toNumber windowH }
  setFillStyle "rgb(184,185,204)"
  void $ for [0, 36, 72, 108, 144, 180] $ \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH }
  setFillStyle "black"
  void $ for [1, 37, 73, 109, 145, 181] $ \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH }
  -- Solo highway
  setFillStyle "rgb(91,137,185)"
  let startsAsSolo = case Map.lookupLE minSecs five.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc five.solo)
      drawSolos L.Nil            = return unit
      drawSolos (L.Cons _ L.Nil) = return unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ void $ for [2, 38, 74, 110, 146] $ \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 34.0, h: toNumber $ y1 - y2 }
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc five.solo $ \secs _ -> do
    drawImage Image_highway_drums_solo_edge (toNumber targetX) (toNumber $ secsToPx secs)
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) $ \secs evt -> do
    let y = secsToPx secs
    case evt of
      Bar      -> drawImage Image_highway_grybo_bar      (toNumber targetX) (toNumber y - 1.0)
      Beat     -> drawImage Image_highway_grybo_beat     (toNumber targetX) (toNumber y - 1.0)
      HalfBeat -> drawImage Image_highway_grybo_halfbeat (toNumber targetX) (toNumber y)
  -- Target
  drawImage Image_highway_grybo_target (toNumber targetX) (toNumber targetY - 5.0)
  -- Sustains
  let colors =
        [ { c: _.green , x: 1  , strum: Image_gem_green , hopo: Image_gem_green_hopo 
          , shades: { light: "rgb(135,247,126)", normal: "rgb( 21,218,  2)", dark: "rgb( 13,140,  2)" }
          , hit: \o -> "rgba(190,255,192," <> show o <> ")"
          }
        , { c: _.red   , x: 37 , strum: Image_gem_red   , hopo: Image_gem_red_hopo   
          , shades: { light: "rgb(247,127,158)", normal: "rgb(218,  2, 62)", dark: "rgb(140,  2, 40)" }
          , hit: \o -> "rgba(255,188,188," <> show o <> ")"
          }
        , { c: _.yellow, x: 73 , strum: Image_gem_yellow, hopo: Image_gem_yellow_hopo
          , shades: { light: "rgb(247,228,127)", normal: "rgb(218,180,  2)", dark: "rgb(140,115,  3)" }
          , hit: \o -> "rgba(255,244,151," <> show o <> ")"
          }
        , { c: _.blue  , x: 109, strum: Image_gem_blue  , hopo: Image_gem_blue_hopo  
          , shades: { light: "rgb(119,189,255)", normal: "rgb(  2,117,218)", dark: "rgb(  3, 76,140)" }
          , hit: \o -> "rgba(190,198,255," <> show o <> ")"
          }
        , { c: _.orange, x: 145, strum: Image_gem_orange, hopo: Image_gem_orange_hopo
          , shades: { light: "rgb(255,183,119)", normal: "rgb(218, 97,  4)", dark: "rgb(140, 63,  3)" }
          , hit: \o -> "rgba(231,196,112," <> show o <> ")"
          }
        ]
  void $ for colors $ \{ c: getColor, x: offsetX, shades: normalShades } -> do
    let thisColor = getColor five.notes
        isEnergy secs = case Map.lookupLE secs five.energy of
          Nothing           -> false
          Just { value: v } -> v
        drawSustainBlock ystart yend energy = when (ystart < targetY || yend < targetY) $ do
          let ystart' = min ystart targetY
              yend'   = min yend   targetY
              sustaining = targetY < ystart || targetY < yend
              shades = if energy
                then { light: "rgb(137,235,204)", normal: "rgb(138,192,175)", dark: "rgb(124,158,149)" }
                else normalShades
              h = yend' - ystart' + 1
          setFillStyle "black"
          fillRect { x: toNumber $ targetX + offsetX + 14, y: toNumber ystart', w: 1.0, h: toNumber h }
          fillRect { x: toNumber $ targetX + offsetX + 22, y: toNumber ystart', w: 1.0, h: toNumber h }
          setFillStyle shades.light
          fillRect { x: toNumber $ targetX + offsetX + 15, y: toNumber ystart', w: 1.0, h: toNumber h }
          setFillStyle shades.normal
          fillRect { x: toNumber $ targetX + offsetX + 16, y: toNumber ystart', w: 5.0, h: toNumber h }
          setFillStyle shades.dark
          fillRect { x: toNumber $ targetX + offsetX + 21, y: toNumber ystart', w: 1.0, h: toNumber h }
          when sustaining $ do
            setFillStyle shades.light
            fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
        go false (L.Cons (Tuple secsEnd SustainEnd) rest) = case Map.lookupLT secsEnd thisColor of
          Just { key: secsStart, value: Sustain _ } -> do
            drawSustainBlock (secsToPx secsEnd) windowH $ isEnergy secsStart
            go false rest
          _ -> unsafeThrow "during grybo drawing: found a sustain end not preceded by sustain start"
        go true (L.Cons (Tuple _ SustainEnd) rest) = go false rest
        go _ (L.Cons (Tuple _ (Note _)) rest) = go false rest
        go _ (L.Cons (Tuple secsStart (Sustain _)) rest) = do
          let pxEnd = case rest of
                L.Nil                      -> 0
                L.Cons (Tuple secsEnd _) _ -> secsToPx secsEnd
          drawSustainBlock pxEnd (secsToPx secsStart) $ isEnergy secsStart
          go true rest
        go _ L.Nil = return unit
    case L.fromFoldable $ Map.doTupleArray (zoomAsc thisColor) of
      L.Nil -> case Map.lookupLT (pxToSecs windowH) thisColor of
        -- handle the case where the entire screen is the middle of a sustain
        Just { key: secsStart, value: Sustain _ } ->
          drawSustainBlock 0 windowH $ isEnergy secsStart
        _ -> return unit
      events -> go false events
  -- Notes
  void $ for colors $ \{ c: getColor, x: offsetX, strum: strumImage, hopo: hopoImage, hit: shadeHit } -> do
    zoomDesc (getColor five.notes) $ \secs evt -> do
      let futureSecs = case secs - stuff.time of Seconds s -> s
      if futureSecs <= 0.0
        then do
          -- note is in the past or being hit now
          if (-0.1) < futureSecs
            then do
              let opacity = round $ ((futureSecs + 0.1) / 0.05) * 255.0
                  opacity' = if opacity < 0 then 0 else if opacity > 255 then 255 else opacity
              case evt of
                SustainEnd -> return unit
                _ -> do
                  setFillStyle $ shadeHit opacity'
                  fillRect { x: toNumber $ targetX + offsetX + 1, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
            else return unit
        else do
          let y = secsToPx secs
              isEnergy = case Map.lookupLE secs five.energy of
                Just {value: bool} -> bool
                Nothing            -> false
          case evt of
            SustainEnd    -> drawImage Image_sustain_end                                        (toNumber $ targetX + offsetX) (toNumber   y     )
            Note    Strum -> drawImage (if isEnergy then Image_gem_energy      else strumImage) (toNumber $ targetX + offsetX) (toNumber $ y - 5 )
            Sustain Strum -> drawImage (if isEnergy then Image_gem_energy      else strumImage) (toNumber $ targetX + offsetX) (toNumber $ y - 5 )
            Note    HOPO  -> drawImage (if isEnergy then Image_gem_energy_hopo else hopoImage ) (toNumber $ targetX + offsetX) (toNumber $ y - 5 )
            Sustain HOPO  -> drawImage (if isEnergy then Image_gem_energy_hopo else hopoImage ) (toNumber $ targetX + offsetX) (toNumber $ y - 5 )
  return $ targetX + 182 + _M

drawDrums :: forall e. Drums -> Int -> Draw e Int
drawDrums (Drums drums) targetX = do
  stuff <- askStuff
  windowH <- map round $ lift $ C.getCanvasHeight stuff.canvas
  let pxToSecs px = stuff.pxToSecs (windowH - px) + stuff.time
      secsToPx secs = windowH - stuff.secsToPx (secs - stuff.time)
      maxSecs = pxToSecs (-100)
      minSecs = pxToSecs $ windowH + 100
      zoomDesc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomDesc = Map.zoomDescDo minSecs maxSecs
      zoomAsc :: forall v m. (Monad m) => Map.Map Seconds v -> (Seconds -> v -> m Unit) -> m Unit
      zoomAsc = Map.zoomAscDo minSecs maxSecs
      targetY = secsToPx stuff.time
  -- Highway
  setFillStyle "rgb(126,126,150)"
  fillRect { x: toNumber targetX, y: 0.0, w: 146.0, h: toNumber windowH }
  setFillStyle "rgb(184,185,204)"
  void $ for [0, 36, 72, 108, 144] $ \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH }
  setFillStyle "black"
  void $ for [1, 37, 73, 109, 145] $ \offsetX -> do
    fillRect { x: toNumber $ targetX + offsetX, y: 0.0, w: 1.0, h: toNumber windowH }
  -- Solo highway
  setFillStyle "rgb(91,137,185)"
  let startsAsSolo = case Map.lookupLE minSecs drums.solo of
        Nothing           -> false
        Just { value: v } -> v
      soloEdges
        = L.fromFoldable
        $ cons (Tuple minSecs startsAsSolo)
        $ flip snoc (Tuple maxSecs false)
        $ Map.doTupleArray (zoomAsc drums.solo)
      drawSolos L.Nil            = return unit
      drawSolos (L.Cons _ L.Nil) = return unit
      drawSolos (L.Cons (Tuple s1 b1) rest@(L.Cons (Tuple s2 _) _)) = do
        let y1 = secsToPx s1
            y2 = secsToPx s2
        when b1 $ void $ for [2, 38, 74, 110] $ \offsetX -> do
          fillRect { x: toNumber $ targetX + offsetX, y: toNumber y2, w: 34.0, h: toNumber $ y1 - y2 }
        drawSolos rest
  drawSolos soloEdges
  -- Solo edges
  zoomDesc drums.solo $ \secs _ -> do
    drawImage Image_highway_drums_solo_edge (toNumber targetX) (toNumber $ secsToPx secs)
  -- Beats
  zoomDesc (case stuff.song of Song o -> case o.beats of Beats o' -> o'.lines) $ \secs evt -> do
    let y = secsToPx secs
    case evt of
      Bar      -> drawImage Image_highway_drums_bar      (toNumber targetX) (toNumber y - 1.0)
      Beat     -> drawImage Image_highway_drums_beat     (toNumber targetX) (toNumber y - 1.0)
      HalfBeat -> drawImage Image_highway_drums_halfbeat (toNumber targetX) (toNumber y)
  -- Target
  drawImage Image_highway_drums_target (toNumber targetX) (toNumber targetY - 5.0)
  -- Notes
  zoomDesc drums.notes $ \secs evts -> do
    let futureSecs = case secs - stuff.time of Seconds s -> s
    if futureSecs <= 0.0
      then do
        -- note is in the past or being hit now
        if (-0.1) < futureSecs
          then do
            let opacity = round $ ((futureSecs + 0.1) / 0.05) * 255.0
                opacity' = if opacity < 0 then 0 else if opacity > 255 then 255 else opacity
                kick = do
                  setFillStyle $ "rgba(231, 196, 112, " <> show opacity' <> ")"
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY - 5, w: 143.0, h: 1.0 }
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY + 4, w: 143.0, h: 1.0 }
                red = do
                  setFillStyle $ "rgba(255, 188, 188, " <> show opacity' <> ")"
                  fillRect { x: toNumber $ targetX + 2, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
                yellow = do
                  setFillStyle $ "rgba(255, 244, 151, " <> show opacity' <> ")"
                  fillRect { x: toNumber $ targetX + 38, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
                blue = do
                  setFillStyle $ "rgba(190, 198, 255, " <> show opacity' <> ")"
                  fillRect { x: toNumber $ targetX + 74, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
                green = do
                  setFillStyle $ "rgba(190, 255, 192, " <> show opacity' <> ")"
                  fillRect { x: toNumber $ targetX + 110, y: toNumber $ targetY - 4, w: 35.0, h: 8.0 }
            void $ for evts $ \e -> case e of
              Kick -> kick
              Red  -> red
              YCym -> yellow
              YTom -> yellow
              BCym -> blue
              BTom -> blue
              GCym -> green
              GTom -> green
          else return unit
      else do
        -- note is in the future
        let y = secsToPx secs
            isEnergy = case Map.lookupLE secs drums.energy of
              Just {value: bool} -> bool
              Nothing            -> false
        void $ for evts $ \e -> case e of
          Kick -> drawImage (if isEnergy then Image_gem_kick_energy   else Image_gem_kick         ) (toNumber $ targetX + 1  ) (toNumber $ y - 3)
          Red  -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_red          ) (toNumber $ targetX + 1  ) (toNumber $ y - 5)
          YTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_yellow       ) (toNumber $ targetX + 37 ) (toNumber $ y - 5)
          YCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_yellow_cymbal) (toNumber $ targetX + 37 ) (toNumber $ y - 8)
          BTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_blue         ) (toNumber $ targetX + 73 ) (toNumber $ y - 5)
          BCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_blue_cymbal  ) (toNumber $ targetX + 73 ) (toNumber $ y - 8)
          GTom -> drawImage (if isEnergy then Image_gem_energy        else Image_gem_green        ) (toNumber $ targetX + 109) (toNumber $ y - 5)
          GCym -> drawImage (if isEnergy then Image_gem_energy_cymbal else Image_gem_green_cymbal ) (toNumber $ targetX + 109) (toNumber $ y - 8)
  -- TODO: draw all kicks before starting hand gems
  -- Return targetX of next track
  return $ targetX + 146 + _M
