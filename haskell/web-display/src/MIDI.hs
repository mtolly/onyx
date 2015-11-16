{-# LANGUAGE OverloadedStrings #-}
module MIDI (readMIDI) where

import GHCJS.Types
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal
import qualified Data.ByteString.Char8 as B8
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import Data.JSString (pack)
import qualified Data.EventList.Relative.TimeBody as RTB
import Control.Monad (forM)

(^.) :: (FromJSVal b) => JSVal -> JSString -> IO b
obj ^. prop = getProp prop (Object obj) >>= fromJSVal >>= \res -> case res of
  Just x -> return x
  Nothing -> error $ "(^.): fromJSVal failed on property " ++ show prop
infixl 1 ^.

foreign import javascript unsafe "MidiFile($1)"
  js_readMIDI :: JSString -> IO Object

foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSVal -> IO ()

foreign import javascript unsafe "$2[$1]"
  indexArray :: Int -> JSVal -> IO JSVal

readMIDI :: B8.ByteString -> IO F.T
readMIDI bs = do
  obj <- js_readMIDI $ pack $ B8.unpack bs
  let Object jmid = obj
  res <- (jmid ^. "header") >>=  (^. "ticksPerBeat") :: IO Int
  jtrks <- jmid ^. "tracks" :: IO [JSVal]
  trks <- forM jtrks $ \jtrk -> do
    len <- jtrk ^. "length" :: IO Int
    fmap RTB.fromPairList $ forM [0 .. len - 1] $ \i -> do
      jevt <- indexArray i jtrk :: IO JSVal
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
          "timeSignature" -> do
            n <- jevt ^. "numerator"
            d <- jevt ^. "denominator"
            m <- jevt ^. "metronome"
            ts <- jevt ^. "thirtyseconds"
            return $ E.MetaEvent $ Meta.TimeSig n d m ts
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
