{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import Control.Monad (forM, forM_, unless)
import qualified Sound.MIDI.Util as U
import qualified Data.Map as Map
import Data.Time.Clock
import Control.Exception (evaluate)
import Text.Printf (printf)
import Data.List (sort)

import GHCJS.Foreign
import GHCJS.Types

import Control.Concurrent.STM
import Data.IORef

import qualified Audio
import Draw
import Midi

data App = App
  { images :: ImageID -> Image
  , gems :: Map.Map U.Seconds [Gem ProType]
  , timeToMeasure :: U.Seconds -> U.MeasureBeats
  }

draw :: U.Seconds -> App -> IO ()
draw posn app = do
  let (_,  gems') = Map.split (if posn < seePast then 0 else posn - seePast) $ gems app
      (gems'', _) = Map.split (posn + seeFuture) gems'
      ctx = context2d theCanvas
      seePast = 0.05
      seeFuture = 1
  drawImage (images app Image_RBN_background1) 0 0 640 480 ctx
  drawImage (images app Image_track_drum) 50 50 540 430 ctx
  let posnsNow, posnsFuture :: Gem ProType -> (Double, Double, Double, Double)
      posnsNow = \case
        Kick         -> (320, 370.5, 457, 27)
        Red          -> (170.5, 362.5, 91, 45)
        Pro Yellow _ -> (270.5, 360.47, 91, 45)
        Pro Blue   _ -> (369.5, 360.47, 86, 46.289)
        Pro Green  _ -> (468, 362.596, 86, 46.289)
      posnsFuture = \case
        Kick         -> (320, 175.577, 256, 19.292)
        Red          -> (235.215, 169.926, 50.976, 32.137)
        Pro Yellow _ -> (291.233, 168.409, 50.976, 32.137)
        Pro Blue   _ -> (346.69, 168.412, 48.175, 33.057)
        Pro Green  _ -> (401.869, 169.929, 48.175, 33.057)
      futureTime = 0.75 :: Double
      getIllustratorPx gem secOffset = let
        nowToFuturePosn = (secOffset / futureTime) ** 0.75
        (xn, yn, wn, hn) = posnsNow gem
        (xf, yf, wf, hf) = posnsFuture gem
        in  ( xn + nowToFuturePosn * (xf - xn)
            , yn + nowToFuturePosn * (yf - yn)
            , wn + nowToFuturePosn * (wf - wn)
            , hn + nowToFuturePosn * (hf - hn)
            )
      getRealPx gem secOffset = let
        (x, y, w, h) = getIllustratorPx gem secOffset
        in (x - 0.5 * w, y - 0.5 * h, w, h)
  forM_ (reverse $ Map.assocs gems'') $ \(gemSecs, gemList) ->
    forM_ (sort gemList) $ \gem -> let -- sort puts Kick first
      secOffset = realToFrac gemSecs - realToFrac posn :: Double
      (x, y, w, h) = getRealPx gem secOffset
      image = images app $ case gem of
        Kick              -> Image_gem_kick
        Red               -> Image_gem_red
        Pro Yellow Tom    -> Image_gem_yellow
        Pro Blue   Tom    -> Image_gem_blue
        Pro Green  Tom    -> Image_gem_green
        Pro Yellow Cymbal -> Image_gem_cym_yellow
        Pro Blue   Cymbal -> Image_gem_cym_blue
        Pro Green  Cymbal -> Image_gem_cym_green
      in do
        setGlobalAlpha (if secOffset > 0.8 then 1 - (secOffset - 0.8) * 5 else 1) ctx
        drawImage image x y w h ctx
        setGlobalAlpha 1 ctx
  setFillStyle "white" ctx
  setFont "20px monospace" ctx
  let dposn = realToFrac posn :: Double
      mins = floor $ dposn / 60 :: Int
      secs = dposn - fromIntegral mins * 60 :: Double
      timestamp = printf "Time: %02d:%06.3f | Measure: %03d | Beat: %06.3f"
        mins secs (msr + 1) (realToFrac bts + 1 :: Double) :: String
      (msr, bts) = timeToMeasure app posn
  fillText timestamp 10 20 ctx

data Event
  = PlayPause
  | SeekTo Double
  | UserDragging Bool
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

foreign import javascript unsafe
  "$1.value"
  js_value :: JSRef a -> IO (JSRef b)

foreign import javascript unsafe
  "$2.value = $1;"
  js_setValue :: JSString -> JSRef a -> IO ()

main :: IO ()
main = do
  equeue <- atomically newTChan
  addEventListener "click" "button-play-pause" $ atomically $ writeTChan equeue PlayPause
  addEventListener "change" "the-slider" $ do
    str <- fmap fromJSString $ js_getElementById (toJSString "the-slider") >>= js_value
    atomically $ writeTChan equeue $ SeekTo $ read str
  userDragging <- newIORef False
  addEventListener "mousedown" "the-slider" $ writeIORef userDragging True
  addEventListener "mouseup" "the-slider" $ writeIORef userDragging False
  putStrLn "Hooked up buttons."
  howlSong <- Audio.load ["another-day/song-countin.ogg", "another-day/song-countin.mp3"]
  howlDrums <- Audio.load ["another-day/drums.ogg", "another-day/drums.mp3"]
  putStrLn "Loaded audio."
  mid <- loadMidi "another-day/notes.mid"
  putStrLn "Loaded MIDI."
  case U.decodeFile mid of
    Right _ -> undefined
    Left trks -> let
      tmap = U.makeTempoMap $ head trks
      mmap = U.makeMeasureMap U.Error $ head trks
      trk = foldr RTB.merge RTB.empty $ filter (\t -> U.trackName t == Just "PART DRUMS") trks
      gemTrack :: RTB.T U.Seconds (Gem ProType)
      gemTrack = U.applyTempoTrack tmap $ pickExpert $ assignToms $ RTB.mapMaybe readDrumEvent trk
      pickExpert = RTB.mapMaybe $ \(d, x) -> case d of
        Expert -> Just x
        _      -> Nothing
      gemMap :: Map.Map U.Seconds [Gem ProType]
      gemMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
        RTB.collectCoincident gemTrack
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
              , timeToMeasure = U.applyMeasureMap mmap . U.unapplyTempoMap tmap
              }
        draw 0 app
        songID  <- Audio.play howlSong
        drumsID <- Audio.play howlDrums
        start <- getCurrentTime
        let updateSlider secs = do
              drag <- readIORef userDragging
              unless drag $ do
                dur <- Audio.getDuration howlSong
                elt <- js_getElementById $ toJSString "the-slider"
                js_setValue (toJSString $ show $ realToFrac secs / dur) elt
            playing startUTC startSecs = do
              nowUTC <- getCurrentTime
              let nowSecs = realToFrac (diffUTCTime nowUTC startUTC) + startSecs
              updateSlider nowSecs
              draw nowSecs app
              requestAnimationFrame
              atomically (tryReadTChan equeue) >>= \case
                Nothing -> playing startUTC startSecs
                Just PlayPause -> do
                  Audio.pause songID howlSong
                  Audio.pause drumsID howlDrums
                  paused nowSecs
                Just (SeekTo p) -> do
                  dur <- Audio.getDuration howlSong
                  let newSecs = realToFrac $ dur * p :: U.Seconds
                  Audio.setPosSafe (realToFrac newSecs) songID howlSong
                  Audio.setPosSafe (realToFrac newSecs) drumsID howlDrums
                  playing nowUTC newSecs
                Just (UserDragging b) -> do
                  writeIORef userDragging b
                  playing startUTC startSecs
            paused nowSecs = do
              -- draw nowSecs app
              requestAnimationFrame
              atomically (tryReadTChan equeue) >>= \case
                Nothing -> paused nowSecs
                Just PlayPause -> do
                  Audio.setPosSafe (realToFrac nowSecs) songID howlSong
                  Audio.setPosSafe (realToFrac nowSecs) drumsID howlDrums
                  startUTC <- getCurrentTime
                  playing startUTC nowSecs
                Just (SeekTo p) -> do
                  dur <- Audio.getDuration howlSong
                  let newSecs = realToFrac $ dur * p :: U.Seconds
                  Audio.setPosSafe (realToFrac newSecs) songID howlSong
                  Audio.pause songID howlSong
                  Audio.setPosSafe (realToFrac newSecs) drumsID howlDrums
                  Audio.pause drumsID howlDrums
                  draw newSecs app
                  updateSlider newSecs
                  paused newSecs
                Just (UserDragging b) -> do
                  writeIORef userDragging b
                  paused nowSecs
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
  | Image_smash_10
  | Image_smash_2
  | Image_smash_3
  | Image_smash_4
  | Image_smash_5
  | Image_smash_6
  | Image_smash_7
  | Image_smash_8
  | Image_smash_9
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
