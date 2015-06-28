{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           Control.Concurrent.STM (atomically, newTChan, tryReadTChan,
                                         writeTChan)
import           Control.Exception      (evaluate)
import           Control.Monad          (forM, unless)
import           Data.IORef             (newIORef, readIORef, writeIORef)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Time              (diffUTCTime, getCurrentTime)
import           GHCJS.Foreign          (ForeignRetention (..), asyncCallback,
                                         fromJSString, toJSString)
import           GHCJS.Types            (JSFun, JSRef, JSString, castRef,
                                         isNull)
import           Text.Printf            (printf)

import qualified RockBand.File          as File
import           StackTrace

import qualified Audio
import           Canvas
import           Draw
import           Jasmid
import           Midi

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

  file <- printStackTraceIO $ File.readMIDIFile mid
  logLine "Parsed MIDI."

  imgs <- fmap Map.fromList $ forM [minBound .. maxBound] $ \iid -> do
    img <- loadImage $ "rbprev/" ++ drop 6 (show iid) ++ ".png"
    return (iid, img)
  preview <- evaluate $ buildPreview file
  let images iid = case Map.lookup iid imgs of
        Just img -> img
        Nothing  -> error $ "panic! couldn't find image " ++ show iid
      performDraw t = mapM_ (canvasDraw images $ context2d theCanvas) $ draw t preview
  performDraw 0
  songID  <- Audio.play howlSong
  start <- getCurrentTime
  dur <- case theEnd preview of
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
        performDraw nowSecs
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
            performDraw newSecs
            updateSlider newSecs
            paused newSecs
  playing start 0
