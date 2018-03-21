module Draw (draw, getWindowDims, _M, _B) where

import           Prelude

import           Control.Monad.Eff  (Eff)
import           Control.MonadPlus  (guard)
import           Data.Array         (concat, reverse, uncons)
import           Data.Int           (round, toNumber)
import           Data.List          as L
import           Data.Maybe         (Maybe (..), isJust)
import           Data.Set           as Set
import           Data.Time.Duration (Seconds (..))
import           Data.Traversable   (traverse_)
import           Data.Tuple         (Tuple (..))
import           DOM                (DOM)
import           Graphics.Canvas    as C

import           Draw.Common        (App (..), Draw, Settings, drawImage,
                                     fillRect, measureText, onContext,
                                     setFillStyle, showTimestamp)
import           Draw.Drums         (drawDrums)
import           Draw.Five          (drawFive)
import           Draw.ProKeys       (drawProKeys)
import           Draw.Protar        (drawProtar)
import           Draw.Six           (drawSix)
import           Draw.Vocal         (drawVocal)
import           Images             (ImageID (..))
import           Song               (Flex (..), FlexPart (..), Song (..))
import           Style              (customize)

foreign import getWindowDims :: forall e. Eff (dom :: DOM | e) {w :: Number, h :: Number}

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
  -- void $ C.save stuff.context
  -- void $ C.beginPath stuff.context
  -- void $ C.rect stuff.context { x: 0.0, y: toNumber stuff.minY, w: windowW, h: toNumber $ stuff.maxY - stuff.minY }
  -- void $ C.clip stuff.context
  drawTracks (_M + _B + _M + _B + _M) $ concat $ flip map song.parts \(Tuple part (Flex flex)) ->
    [ \i -> drawPart flex.five    (Set.member $ Tuple part FlexFive   ) drawFive    i stuff
    , \i -> drawPart flex.six     (Set.member $ Tuple part FlexSix    ) drawSix     i stuff
    , \i -> drawPart flex.drums   (Set.member $ Tuple part FlexDrums  ) drawDrums   i stuff
    , \i -> drawPart flex.prokeys (Set.member $ Tuple part FlexProKeys) drawProKeys i stuff
    , \i -> drawPart flex.protar  (Set.member $ Tuple part FlexProtar ) drawProtar  i stuff
    ]
  -- void $ C.restore stuff.context
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

-- | Height/width of margins
_M :: Int
_M = customize.marginWidth

-- | Height/width of buttons
_B :: Int
_B = customize.buttonWidth
