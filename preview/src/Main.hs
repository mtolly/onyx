{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Concurrent.STM           (atomically, newTChan,
                                                   tryReadTChan, writeTChan)
import           Control.Exception                (evaluate)
import           Control.Monad                    (forM, forM_, unless)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import           Data.List                        (sort, transpose)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Time.Clock                  (diffUTCTime, getCurrentTime)
import           GHCJS.Foreign                    (ForeignRetention (..),
                                                   asyncCallback, fromJSString,
                                                   toJSString)
import           GHCJS.Types                      (JSFun, JSRef, JSString,
                                                   castRef, isNull)
import           Numeric.NonNegative.Class        ((-|))
import qualified Sound.MIDI.Util                  as U
import           Text.Printf                      (printf)

import qualified Audio
import           Draw
import           Midi

data App = App
  { images        :: ImageID -> Image
  , gems          :: Map.Map U.Seconds [Gem ProType]
  , beatLines     :: Map.Map U.Seconds BeatEvent
  , timeToMeasure :: U.Seconds -> U.MeasureBeats
  }

zoom :: (Ord k) => k -> k -> Map.Map k a -> Map.Map k a
zoom lb ub m = let
  (_, upper) = Map.split lb m
  (lower, _) = Map.split ub upper
  in lower

draw :: U.Seconds -> App -> IO ()
draw posn app = do
  let gems'      = zoom (posn -| 1) (posn + 1) $ gems      app
      beatLines' = zoom (posn -| 1) (posn + 1) $ beatLines app
      ctx = context2d theCanvas
  drawImage (images app Image_RBN_background1) 0  0  640 480 ctx
  drawImage (images app Image_track_drum     ) 50 50 540 430 ctx
  let beatPosnNow, beatPosnFuture :: (Double, Double, Double, Double)
      beatPosnNow    = (320, 373.735, 431.201, 28.529)
      beatPosnFuture = (320, 175.58 , 234    , 17.888)
      futureTime = 0.75 :: Double
      getIllustratorPx now4 future4 eventPosn = let
        secOffset = realToFrac eventPosn - realToFrac posn :: Double
        nowToFuturePosn = case secOffset / futureTime of
          r | r >= 0    -> r ** 0.8
            | otherwise -> r * 1.25
        (xn, yn, wn, hn) = now4
        (xf, yf, wf, hf) = future4
        in  ( xn + nowToFuturePosn * (xf - xn)
            , yn + nowToFuturePosn * (yf - yn)
            , wn + nowToFuturePosn * (wf - wn)
            , hn + nowToFuturePosn * (hf - hn)
            )
      beatIllustratorPx    = getIllustratorPx beatPosnNow      beatPosnFuture
      gemIllustratorPx gem = getIllustratorPx (gemPosnNow gem) (gemPosnFuture gem)
      gemPosnNow, gemPosnFuture :: Gem ProType -> (Double, Double, Double, Double)
      gemPosnNow = \case
        Kick         -> (320  , 370.5  , 457, 27    )
        Red          -> (170.5, 362.5  , 91 , 45    )
        Pro Yellow _ -> (270.5, 360.47 , 91 , 45    )
        Pro Blue   _ -> (369.5, 360.47 , 86 , 46.289)
        Pro Green  _ -> (468  , 362.596, 86 , 46.289)
      gemPosnFuture = \case
        Kick         -> (320    , 175.577, 256   , 19.292)
        Red          -> (235.215, 169.926, 50.976, 32.137)
        Pro Yellow _ -> (291.233, 168.409, 50.976, 32.137)
        Pro Blue   _ -> (346.69 , 168.412, 48.175, 33.057)
        Pro Green  _ -> (401.869, 169.929, 48.175, 33.057)
      -- scales illustrator dims given a reference image and an actual image
      scaleTo :: (Double, Double) -> (Double, Double) ->
        (Double, Double, Double, Double) -> (Double, Double, Double, Double)
      (w1, h1) `scaleTo` (w2, h2) = \(x, y, w, h) ->
        (x, y, w * w2 / w1, h * h2 / h1)
      scaleSmash = (128, 64) `scaleTo` (256, 256)
      scaleSmashFlare = (128, 64) `scaleTo` (128, 128)
      scaleKickFlash = (1024, 64) `scaleTo` (1224, 512)
      getGemImages :: Gem ProType -> U.Seconds ->
        [(ImageID, (Double, Double, Double, Double))]
      getGemImages gem gemSecs = let
        secOffset = realToFrac gemSecs - realToFrac posn :: Double
        brokenDims = gemIllustratorPx gem posn
        movingDims = gemIllustratorPx gem gemSecs
        image = case gem of
          Kick              -> Image_gem_kick
          Red               -> Image_gem_red
          Pro Yellow Tom    -> Image_gem_yellow
          Pro Blue   Tom    -> Image_gem_blue
          Pro Green  Tom    -> Image_gem_green
          Pro Yellow Cymbal -> Image_gem_cym_yellow
          Pro Blue   Cymbal -> Image_gem_cym_blue
          Pro Green  Cymbal -> Image_gem_cym_green
        brokenAnim = case gem of
          Kick           -> transpose [kickFlashes]
          Pro ybg Cymbal -> transpose [flaresPro ybg]
          Pro ybg Tom    -> transpose [smashes, flaresPro ybg]
          Red            -> transpose [smashes, flaresRed]
          where flaresRed = do
                  flare <- replicate 3 Image_smash_flare_red
                  return (flare, scaleSmashFlare brokenDims)
                flaresPro ybg = do
                  flare <- replicate 3 $ case ybg of
                    Yellow -> Image_smash_flare_yellow
                    Blue   -> Image_smash_flare_blue
                    Green  -> Image_smash_flare_green
                  return (flare, scaleSmashFlare brokenDims)
                smashes = do
                  smash <- [Image_smash_1 .. Image_smash_10]
                  return (smash, scaleSmash brokenDims)
                kickFlashes = do
                  flash <- [Image_kick_flash_1 .. Image_kick_flash_7]
                  return (flash, scaleKickFlash brokenDims)
        in if secOffset <= 0
          then concat $ take 1 $ drop (floor $ secOffset * (-50)) brokenAnim
          else [(image, movingDims)]
      -- converts from adobe illustrator xywh to canvas xywh
      -- (illustrator uses rect center for x/y instead of rect top-left)
      getRealPx (x, y, w, h) = (x - 0.5 * w, y - 0.5 * h, w, h)
      opacity eventPosn = let
        secOffset = realToFrac eventPosn - realToFrac posn :: Double
        in if secOffset > 0.8 then 1 - (secOffset - 0.8) * 5 else 1
  forM_ (reverse $ Map.assocs beatLines') $ \(beatSecs, beat) -> let
    (x, y, w, h) = getRealPx $ beatIllustratorPx beatSecs
    image = images app $ case beat of
      Bar      -> Image_measure
      Beat     -> Image_beat_marker
      HalfBeat -> Image_half_beat_marker
    in do
        setGlobalAlpha (opacity beatSecs) ctx
        drawImage image x y w h ctx
        setGlobalAlpha 1 ctx
  forM_ (reverse $ Map.assocs gems') $ \(gemSecs, gemList) ->
    forM_ (sort gemList) $ \gem -> do -- sort puts Kick first
      setGlobalAlpha (opacity gemSecs) ctx
      forM_ (getGemImages gem gemSecs) $ \(iid, illdims) -> let
        (x, y, w, h) = getRealPx illdims
        in drawImage (images app iid) x y w h ctx
      setGlobalAlpha 1 ctx
  setFillStyle "white" ctx
  setFont "20px monospace" ctx
  let dposn = realToFrac posn :: Double
      mins = floor $ dposn / 60 :: Int
      secs = dposn - fromIntegral mins * 60 :: Double
      (msr, bts) = timeToMeasure app posn
      timestamp = printf "Time: %02d:%06.3f | Measure: %03d | Beat: %06.3f"
        mins secs (msr + 1) (realToFrac bts + 1 :: Double) :: String
  fillText timestamp 10 20 ctx

data Event
  = PlayPause
  | SeekTo Double
  deriving (Eq, Ord, Show)

foreign import javascript unsafe
  "document.getElementById($1)"
  js_getElementById :: JSString -> IO (JSRef a)

foreign import javascript unsafe
  "$2.addEventListener($1, $3);"
  js_addEventListener :: JSString -> JSRef a -> JSFun (IO ()) -> IO ()

addEventListener :: String -> String -> IO () -> IO ()
addEventListener event eltid f = do
  elt <- js_getElementById $ toJSString eltid
  jf <- asyncCallback (DomRetain $ castRef elt) f
  js_addEventListener (toJSString event) elt jf

data Input_
type Input = JSRef Input_

foreign import javascript unsafe
  "$1.value"
  js_value :: Input -> IO JSString

foreign import javascript unsafe
  "$2.value = $1;"
  js_setValue :: JSString -> Input -> IO ()

foreign import javascript unsafe
  "lookupGET($1)"
  js_lookupGET :: JSString -> IO JSString

lookupGET :: String -> IO (Maybe String)
lookupGET k = js_lookupGET (toJSString k) >>= \ref -> return $ if isNull ref
  then Nothing
  else Just $ fromJSString ref

foreign import javascript unsafe
  "document.getElementById('the-log').insertAdjacentHTML('beforeend', $1);"
  js_logHTML :: JSString -> IO ()

logLine :: String -> IO ()
logLine s = js_logHTML $ toJSString $ s ++ "<br />"

-- | log fn that also saves the value to a global var
foreign import javascript unsafe
  " window.hslogn = window.hslogn ? window.hslogn + 1 : 1; \
  \ console.log($1); \
  \ window['hslogged' + window.hslogn] = $1; \
  \ console.log('saved to hslogged' + window.hslogn); "
  consoleLog :: JSRef a -> IO ()

main :: IO ()
main = do

  equeue <- atomically newTChan
  addEventListener "click" "button-play-pause" $ atomically $ writeTChan equeue PlayPause
  addEventListener "change" "the-slider" $ do
    str <- fmap fromJSString $ js_getElementById (toJSString "the-slider") >>= js_value
    atomically $ writeTChan equeue $ SeekTo $ read str
  userDragging <- newIORef False
  addEventListener "mousedown" "the-slider" $ writeIORef userDragging True
  addEventListener "mouseup"   "the-slider" $ writeIORef userDragging False
  logLine "Hooked up buttons."

  artist <- fmap (fromMaybe "dream-theater") $ lookupGET "artist"
  title  <- fmap (fromMaybe "6-00"         ) $ lookupGET "title"
  let root = printf "songs/%s/%s/" artist title
  logLine $ "Loading song from " ++ root
  howlSong <- Audio.load
    [ root ++ "/gen/album/2p/preview-audio.ogg"
    , root ++ "/gen/album/2p/preview-audio.mp3"
    ]
  logLine "Loaded audio."
  mid <- loadMidi $ root ++ "/gen/album/2p/notes.mid"
  logLine "Loaded MIDI."

  case U.decodeFile mid of
    Right _ -> undefined
    Left trks -> let
      tmap = U.makeTempoMap $ head trks
      mmap = U.makeMeasureMap U.Error $ head trks
      findTrack s = foldr RTB.merge RTB.empty $ filter (\t -> U.trackName t == Just s) trks
      gemTrack :: RTB.T U.Seconds (Gem ProType)
      gemTrack = U.applyTempoTrack tmap $ pickExpert $ assignToms
        $ RTB.mapMaybe readDrumEvent $ findTrack "PART DRUMS"
      pickExpert = RTB.mapMaybe $ \(d, x) -> case d of
        Expert -> Just x
        _      -> Nothing
      gemMap :: Map.Map U.Seconds [Gem ProType]
      gemMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
        RTB.collectCoincident gemTrack
      beatTrack :: RTB.T U.Seconds BeatEvent
      beatTrack = U.applyTempoTrack tmap $ insertHalfBeats $
        RTB.mapMaybe readBeatEvent $ findTrack "BEAT"
      beatMap :: Map.Map U.Seconds BeatEvent
      beatMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 beatTrack
      endEvent :: Maybe U.Seconds
      endEvent = let
        endEvents = RTB.filter isEndEvent $ findTrack "EVENTS"
        in case RTB.viewL endEvents of
          Just ((bts, _), _) -> Just $ U.applyTempoMap tmap bts
          Nothing            -> Nothing
      in do
        _ <- evaluate gemMap
        imgs <- fmap Map.fromList $ forM [minBound .. maxBound] $ \iid -> do
          img <- loadImage $ "rbprev/" ++ drop 6 (show iid) ++ ".png"
          return (iid, img)
        let app = App
              { images = \iid -> case Map.lookup iid imgs of
                  Just img -> img
                  Nothing  -> error $ "panic! couldn't find image " ++ show iid
              , gems = gemMap
              , beatLines = beatMap
              , timeToMeasure = U.applyMeasureMap mmap . U.unapplyTempoMap tmap
              }
        draw 0 app
        songID  <- Audio.play howlSong
        start <- getCurrentTime
        dur <- case endEvent of
          Just dur -> return dur
          Nothing  -> Audio.getDuration howlSong
        let updateSlider secs = do
              drag <- readIORef userDragging
              unless drag $ do
                elt <- js_getElementById $ toJSString "the-slider"
                js_setValue (toJSString $ show (realToFrac $ secs / dur :: Double)) elt
            playing startUTC startSecs = do
              nowUTC <- getCurrentTime
              let nowSecs = realToFrac (diffUTCTime nowUTC startUTC) + startSecs
              updateSlider nowSecs
              draw nowSecs app
              requestAnimationFrame
              if dur <= nowSecs
                then do
                  Audio.pause songID howlSong
                  paused nowSecs
                else atomically (tryReadTChan equeue) >>= \case
                  Nothing -> playing startUTC startSecs
                  Just PlayPause -> do
                    Audio.pause songID howlSong
                    paused nowSecs
                  Just (SeekTo p) -> do
                    let newSecs = dur * realToFrac p
                    Audio.setPosSafe newSecs songID howlSong
                    playing nowUTC newSecs
            paused nowSecs = do
              requestAnimationFrame
              atomically (tryReadTChan equeue) >>= \case
                Nothing -> paused nowSecs
                Just PlayPause -> do
                  Audio.setPosSafe nowSecs songID howlSong
                  startUTC <- getCurrentTime
                  playing startUTC nowSecs
                Just (SeekTo p) -> do
                  let newSecs = dur * realToFrac p
                  draw newSecs app
                  updateSlider newSecs
                  paused newSecs
        playing start 0

data ImageID
  = Image_RBN_background1
  | Image_beat_marker
  | Image_gem_blue
  | Image_gem_cym_blue
  | Image_gem_cym_green
  | Image_gem_cym_style
  | Image_gem_cym_yellow
  | Image_gem_green
  | Image_gem_hopo_blue
  | Image_gem_hopo_green
  | Image_gem_hopo_orange
  | Image_gem_hopo_red
  | Image_gem_hopo_style
  | Image_gem_hopo_yellow
  | Image_gem_kick
  | Image_gem_kick_style
  | Image_gem_orange
  | Image_gem_red
  | Image_gem_style
  | Image_gem_yellow
  | Image_half_beat_marker
  | Image_kick_flash_1
  | Image_kick_flash_2
  | Image_kick_flash_3
  | Image_kick_flash_4
  | Image_kick_flash_5
  | Image_kick_flash_6
  | Image_kick_flash_7
  | Image_measure
  | Image_smash_1
  | Image_smash_2
  | Image_smash_3
  | Image_smash_4
  | Image_smash_5
  | Image_smash_6
  | Image_smash_7
  | Image_smash_8
  | Image_smash_9
  | Image_smash_10
  | Image_smash_flare_blue
  | Image_smash_flare_green
  | Image_smash_flare_orange
  | Image_smash_flare_red
  | Image_smash_flare_style
  | Image_smash_flare_yellow
  | Image_sustain_blue
  | Image_sustain_blue_hi
  | Image_sustain_green
  | Image_sustain_green_hi
  | Image_sustain_orange
  | Image_sustain_orange_hi
  | Image_sustain_red
  | Image_sustain_red_hi
  | Image_sustain_style
  | Image_sustain_style_hi
  | Image_sustain_yellow
  | Image_sustain_yellow_hi
  | Image_track_drum
  | Image_track_guitar
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

