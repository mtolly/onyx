{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (forM)

data Canvas_
type Canvas = JSRef Canvas_

data Context_
type Context = JSRef Context_

foreign import javascript unsafe
  "document.getElementById('the-canvas')"
  theCanvas :: Canvas

foreign import javascript unsafe
  "$1.getContext('2d')"
  context2d :: Canvas -> Context

foreign import javascript unsafe
  "$5.fillRect($1, $2, $3, $4);"
  fillRect :: Double -> Double -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe
  "$2.fillStyle = $1;"
  setFillStyle :: JSString -> Context -> IO ()

foreign import javascript unsafe
  "$4.fillText($1, $2, $3);"
  fillText :: JSString -> Double -> Double -> Context -> IO ()

foreign import javascript unsafe
  "$2.font = $1;"
  setFont :: JSString -> Context -> IO ()

data Howl_
type Howl = JSRef Howl_

foreign import javascript interruptible
  "(function(){ var h = new Howl({urls: $1, onload: function(){ $c(h); }}); })();"
  loadHowl :: JSRef [String] -> IO Howl

foreign import javascript unsafe
  "$1.play()"
  playHowl :: Howl -> IO ()

foreign import javascript interruptible
  "requestAnimationFrame($c);"
  requestAnimationFrame :: IO ()

data MidiFile_
type MidiFile = JSRef MidiFile_

foreign import javascript interruptible
  "jasmid_loadMidi($1, $c);"
  jasmid_loadMidi :: JSString -> IO MidiFile

(^.) :: (FromJSRef b) => JSRef a -> String -> IO b
obj ^. prop = getProp prop obj >>= fromJSRef >>= \res -> case res of
  Just x -> return x
  Nothing -> error $ "(^.): fromJSRef failed on property " ++ show prop
infixl 1 ^.

fromJasmid :: MidiFile -> IO F.T
fromJasmid jmid = do
  res <- (jmid ^. "header") >>=  (^. "ticksPerBeat") :: IO Int
  jtrks <- jmid ^. "tracks" :: IO [JSRef ()]
  trks <- forM jtrks $ \jtrk -> do
    len <- jtrk ^. "length" :: IO Int
    fmap RTB.fromPairList $ forM [0 .. len - 1] $ \i -> do
      jevt <- indexArray i $ castRef jtrk :: IO (JSRef ())
      delta <- jevt ^. "deltaTime" :: IO Int
      etype <- jevt ^. "type" :: IO String
      esubtype <- jevt ^. "subtype" :: IO String
      let unrecognized = consoleLog jevt >> error "Unrecognized jasmid event"
      evt <- case etype of
        "meta" -> case esubtype of
          "trackName" -> fmap (E.MetaEvent . Meta.TrackName) $ jevt ^. "text"
          "setTempo" -> do
            uspqn <- jevt ^. "microsecondsPerBeat" :: IO Int
            return $ E.MetaEvent $ Meta.SetTempo $ fromIntegral uspqn
          "timeSignature" -> return $ E.MetaEvent $ Meta.TextEvent "TODO: time signature"
          "endOfTrack" -> return $ E.MetaEvent Meta.EndOfTrack
          "text" -> fmap (E.MetaEvent . Meta.TextEvent) $ jevt ^. "text"
          "lyrics" -> fmap (E.MetaEvent . Meta.Lyric) $ jevt ^. "text"
          _ -> unrecognized
        "channel" -> case esubtype of
          "noteOn" -> do
            ch  <- fmap C.toChannel  $ jevt ^. "channel"
            vel <- fmap V.toVelocity $ jevt ^. "velocity"
            p   <- fmap V.toPitch    $ jevt ^. "noteNumber"
            return $ E.MIDIEvent $ C.Cons ch $ C.Voice $ V.NoteOn p vel
          "noteOff" -> do
            ch  <- fmap C.toChannel  $ jevt ^. "channel"
            vel <- fmap V.toVelocity $ jevt ^. "velocity"
            p   <- fmap V.toPitch    $ jevt ^. "noteNumber"
            return $ E.MIDIEvent $ C.Cons ch $ C.Voice $ V.NoteOff p vel
          _ -> unrecognized
        _ -> unrecognized
      return (fromIntegral delta, evt)
  return $ F.Cons F.Parallel (F.Ticks $ fromIntegral res) trks

foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSRef a -> IO ()

main :: IO ()
main = do
  howl <- toJSRef ["another-day/song-countin.ogg"] >>= loadHowl
  putStrLn "Loaded Howl."
  jmid <- jasmid_loadMidi "another-day/notes.mid"
  putStrLn "Loaded MIDI with jasmid."
  mid <- fromJasmid jmid
  print $ take 100 $ show mid
  let ctx = context2d theCanvas
      loop :: Int -> IO ()
      loop i = do
        setFillStyle "red" ctx
        fillRect 0 0 500 500 ctx
        setFillStyle "blue" ctx
        fillRect 25 25 500 500 ctx
        setFillStyle "yellow" ctx
        fillRect 50 50 500 500 ctx
        setFillStyle "white" ctx
        setFont "20pt Helvetica" ctx
        fillText (toJSString $ show i) 10 35 ctx
        requestAnimationFrame
        loop $ i + 1
  playHowl howl
  loop 0
