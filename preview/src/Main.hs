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
import qualified Data.EventList.Absolute.TimeBody as ATB
import Control.Monad (forM, when, forever)
import qualified Sound.MIDI.Util as U
import qualified Data.Map as Map
import Data.Time.Clock
import Control.Exception (evaluate)
import Text.Printf

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

data Gem = Kick | Red | Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

isGem :: E.T -> Maybe Gem
isGem (E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel)))) | V.fromVelocity vel /= 0
  = case V.fromPitch p of
    96 -> Just Kick
    97 -> Just Red
    98 -> Just Yellow
    99 -> Just Blue
    100 -> Just Green
    _ -> Nothing
isGem _ = Nothing

foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSRef a -> IO ()

draw :: UTCTime -> Map.Map U.Seconds [Gem] -> IO ()
draw start gems = do
  now <- getCurrentTime
  let posn = realToFrac $ diffUTCTime now start :: U.Seconds
      (_,  gems') = Map.split (if posn < 0.02 then 0 else posn - 0.02) gems
      (gems'', _) = Map.split (posn + 0.1) gems'
      activeNow = concat $ Map.elems gems''
      ctx = context2d theCanvas
  setFillStyle "white" ctx
  fillRect 0 0 640 480 ctx
  when (Kick `elem` activeNow) $ do
    setFillStyle "orange" ctx
    fillRect 0 100 400 25 ctx
  when (Red `elem` activeNow) $ do
    setFillStyle "red" ctx
    fillRect 0 0 100 100 ctx
  when (Yellow `elem` activeNow) $ do
    setFillStyle "yellow" ctx
    fillRect 100 0 100 100 ctx
  when (Blue `elem` activeNow) $ do
    setFillStyle "blue" ctx
    fillRect 200 0 100 100 ctx
  when (Green `elem` activeNow) $ do
    setFillStyle "green" ctx
    fillRect 300 0 100 100 ctx
  setFillStyle "black" ctx
  setFont "20px monospace" ctx
  let dposn = realToFrac posn :: Double
      mins = floor $ dposn / 60 :: Int
      secs = dposn - fromIntegral mins * 60 :: Double
      timestamp = printf "%02d:%06.3f" mins secs :: String
  fillText (toJSString timestamp) 10 150 ctx

main :: IO ()
main = do
  howlSong <- toJSRef ["another-day/song-countin.ogg"] >>= loadHowl
  howlDrums <- toJSRef ["another-day/drums.ogg"] >>= loadHowl
  putStrLn "Loaded Howl."
  jmid <- jasmid_loadMidi "another-day/notes.mid"
  putStrLn "Loaded MIDI with jasmid."
  mid <- fromJasmid jmid
  putStrLn "Deserialized jasmid MIDI."
  case U.decodeFile mid of
    Right _ -> undefined
    Left trks -> let
      tmap = U.makeTempoMap $ head trks
      trk = foldr RTB.merge RTB.empty $ filter (\t -> U.trackName t == Just "PART DRUMS") trks
      gemTrack :: RTB.T U.Seconds Gem
      gemTrack = U.applyTempoTrack tmap $ RTB.mapMaybe isGem trk
      gemMap :: Map.Map U.Seconds [Gem]
      gemMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
        RTB.collectCoincident gemTrack
      in do
        _ <- evaluate gemMap
        start <- getCurrentTime
        playHowl howlSong
        playHowl howlDrums
        forever $ do
          draw start gemMap
          requestAnimationFrame
