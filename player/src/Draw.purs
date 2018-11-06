module Draw (draw, getWindowDims, numMod, _M, _B) where

import           Prelude

import           Control.MonadZero  (guard)
import           Data.Array         (concat, uncons)
import           Data.Foldable      (for_)
import           Data.Int           (round, toNumber)
import           Data.Maybe         (Maybe (..))
import           Data.Time.Duration (Seconds (..))
import           Data.Traversable   (or)
import           Data.Tuple         (Tuple (..))
import           Effect             (Effect)
import           Graphics.Canvas    as C

import           Draw.Common        (AppTime (..), Draw, drawImage,
                                     fillRect, onContext, setFillStyle,
                                     showTimestamp, DrawStuff)
import           Draw.Drums         (drawDrums)
import           Draw.Five          (drawFive)
import           Draw.ProKeys       (drawProKeys)
import           Draw.Protar        (drawProtar)
import           Draw.Six           (drawSix)
import           Draw.Vocal         (drawVocal)
import           Draw.Amplitude     (drawAmplitude)
import           Images             (ImageID (..))
import           OnyxMap            as Map
import           Song               (Flex (..), Song (..), Difficulties)
import           Style              (customize)

foreign import getWindowDims :: Effect {width :: Number, height :: Number}

foreign import numMod :: Number -> Number -> Number

draw :: Draw Unit
draw stuff = do
  dims@{width: windowW, height: windowH} <- getWindowDims
  cdims <- C.getCanvasDimensions stuff.canvas
  when (cdims /= dims) (C.setCanvasDimensions stuff.canvas dims)
  setFillStyle customize.background stuff
  fillRect { x: 0.0, y: 0.0, width: windowW, height: windowH } stuff
  let song = case stuff.song of Song s -> s

  -- Draw timestamp and section name
  setFillStyle customize.timestampColor stuff
  onContext
    (\ctx -> do
      C.setFont ctx customize.timestampFont
      C.setTextAlign ctx C.AlignRight
      C.fillText ctx
        (showTimestamp stuff.time)
        (windowW - 20.0)
        (windowH - 20.0)
      for_ (Map.lookupLE stuff.time song.sections) \o -> C.fillText ctx
        o.value
        (windowW - 20.0)
        (windowH - 45.0)
    ) stuff

  -- Draw the visible instrument tracks in sequence
  let drawTracks targetX trks = case uncons trks of
        Nothing -> pure targetX
        Just {head: trk, tail: trkt} -> do
          targetX' <- trk targetX
          drawTracks targetX' trkt
      partEnabled pname ptype dname sets = or do
        part <- sets.parts
        guard $ part.partName == pname
        fpart <- part.flexParts
        guard $ fpart.partType == ptype
        diff <- fpart.difficulties
        guard $ diff.diffName == dname
        pure diff.enabled
      drawInst
        :: forall a
        .  DrawStuff
        -> String
        -> Flex
        -> String
        -> (Flex -> Maybe (Difficulties a))
        -> (a -> Int -> Draw Int)
        -> Int
        -> Effect Int
      drawInst someStuff part flex ptype fn drawer i = case fn flex of
        Nothing -> pure i
        Just fpart -> drawTracks i $ flip map fpart \(Tuple dname diff) -> \j ->
          if partEnabled part ptype dname someStuff.app.settings
            then drawer diff j someStuff
            else pure j
      drawNonVocals someStuff = void $ drawTracks (_M + _B + _M) $ concat $ flip map song.parts \(Tuple part flex) ->
        [ drawInst someStuff part flex "five"      (\(Flex f) -> f.five     ) drawFive
        , drawInst someStuff part flex "six"       (\(Flex f) -> f.six      ) drawSix
        , drawInst someStuff part flex "drums"     (\(Flex f) -> f.drums    ) drawDrums
        , drawInst someStuff part flex "prokeys"   (\(Flex f) -> f.prokeys  ) drawProKeys
        , drawInst someStuff part flex "protar"    (\(Flex f) -> f.protar   ) drawProtar
        , drawInst someStuff part flex "amplitude" (\(Flex f) -> f.amplitude) drawAmplitude
        ]
  -- first draw everything but vocals
  if stuff.app.settings.staticVert
    then let
      betweenTargets = windowH * 0.65
      Seconds now = stuff.time
      bottomTarget = windowH - toNumber customize.targetPositionVert - numMod (now * customize.trackSpeed) betweenTargets
      topTarget = bottomTarget - betweenTargets
      unseenTarget = bottomTarget + betweenTargets
      drawTarget y = do
        let drawBottom = y + toNumber customize.targetPositionVert
            drawTop = drawBottom - betweenTargets + toNumber _M
        when (0.0 < drawBottom && drawTop < windowH) do
          void $ C.save stuff.context
          void $ C.beginPath stuff.context
          void $ C.rect stuff.context { x: 0.0, y: drawTop, width: windowW, height: drawBottom - drawTop }
          void $ C.clip stuff.context
          drawNonVocals stuff
            { pxToSecsVert = \px -> Seconds $ (toNumber px - windowH + y) / customize.trackSpeed
            , secsToPxVert = \(Seconds secs) -> round $ windowH - y + customize.trackSpeed * secs
            }
          void $ C.restore stuff.context
      in do
        -- TODO just draw once and then copy
        drawTarget bottomTarget
        drawTarget topTarget
        drawTarget unseenTarget
    else drawNonVocals stuff
  -- then draw vocals
  void $ drawTracks 0 $ flip map song.parts \(Tuple part flex) ->
    drawInst stuff part flex "vocal" (\(Flex f) -> f.vocal) drawVocal

  -- draw the progress bar and buttons on the side
  let playPause = case stuff.app.time of
        Paused  _ -> Image_button_play
        Playing _ -> Image_button_pause
  drawImage playPause (toNumber _M) (windowH - 2.0 * toNumber _M - 2.0 * toNumber _B) stuff
  drawImage Image_button_gear (toNumber _M) (windowH - toNumber _M - toNumber _B) stuff
  let timelineH = windowH - 4.0 * toNumber _M - 2.0 * toNumber _B - 2.0
      filled = unSeconds (stuff.time) / unSeconds song.end
      unSeconds (Seconds s) = s
  setFillStyle customize.progressBorder stuff
  fillRect { x: toNumber _M, y: toNumber _M, width: toNumber _B, height: timelineH + 2.0 } stuff
  setFillStyle customize.progressEmpty stuff
  fillRect { x: toNumber _M + 1.0, y: toNumber _M + 1.0, width: toNumber _B - 2.0, height: timelineH } stuff
  setFillStyle customize.progressFilled stuff
  fillRect
    { x: toNumber _M + 1.0
    , y: toNumber _M + 1.0 + timelineH * (1.0 - filled)
    , width: toNumber _B - 2.0
    , height: timelineH * filled
    } stuff
  setFillStyle customize.progressSection stuff
  for_ (Map.keys song.sections) \secs -> do
    let offset = timelineH - timelineH * (unSeconds secs / unSeconds song.end)
    fillRect
      { x: toNumber _M + toNumber (_B - customize.progressSectionWidth) / 2.0
      , y: toNumber _M + 1.0 + offset
      , width: toNumber customize.progressSectionWidth
      , height: 1.0
      } stuff

-- | Height/width of margins
_M :: Int
_M = customize.marginWidth

-- | Height/width of buttons
_B :: Int
_B = customize.buttonWidth
