{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE JavaScriptFFI #-}
module Jasmid
( loadMidi
) where

import           Control.Monad                    (forM)
import qualified Data.EventList.Relative.TimeBody as RTB
import           GHCJS.Marshal
import           GHCJS.Types
import qualified Data.JSString as JSStr
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V

foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSRef a -> IO ()

foreign import javascript unsafe "$2[$1]"
  js_getProp :: JSStr.JSString -> JSRef a -> IO (JSRef b)

getProp :: String -> JSRef a -> IO (JSRef b)
getProp = js_getProp . JSStr.pack

foreign import javascript unsafe "$2[$1]"
  indexArray :: Int -> JSRef a -> IO (JSRef b)

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

-- | If the given number is @2 ^ n@ where @n@ is a non-negative integer,
-- returns @n@.
logBase2 :: Integer -> Maybe Integer
logBase2 x = go 0 1 where
  go !p !y = case compare x y of
    EQ -> Just p
    GT -> go (p + 1) (y * 2)
    LT -> Nothing

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
          "timeSignature" -> do
            n <- jevt ^. "numerator"
            d <- jevt ^. "denominator" :: IO Int
            m <- jevt ^. "metronome"
            ts <- jevt ^. "thirtyseconds"
            case fmap fromIntegral $ logBase2 $ fromIntegral d of
              Just d' -> return $ E.MetaEvent $ Meta.TimeSig n d' m ts
              Nothing -> unrecognized
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

loadMidi :: String -> IO F.T
loadMidi s = jasmid_loadMidi (JSStr.pack s) >>= fromJasmid
