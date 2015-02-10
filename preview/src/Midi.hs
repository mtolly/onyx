{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
module Midi
( loadMidi
, ProColor(..), ProType(..), Gem(..), Difficulty(..), DrumEvent(..)
, readDrumEvent, assignToms
) where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Numeric.NonNegative.Class as NNC
import Control.Monad (forM)

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

foreign import javascript unsafe
  "console.log($1);"
  consoleLog :: JSRef a -> IO ()

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

loadMidi :: String -> IO F.T
loadMidi s = jasmid_loadMidi (toJSString s) >>= fromJasmid

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ProType = Tom | Cymbal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Gem t = Kick | Red | Pro ProColor t
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data DrumEvent
  = Toms ProColor Bool -- ^ this event must come before 'Note' for 'RTB.normalize'
  | Note Difficulty (Gem ())
  deriving (Eq, Ord, Show, Read)

readDrumEvent :: E.T -> Maybe DrumEvent
readDrumEvent e = let
  noteOn p = case V.fromPitch p of
    60  -> Just $ Note Easy Kick
    61  -> Just $ Note Easy Red
    62  -> Just $ Note Easy $ Pro Yellow ()
    63  -> Just $ Note Easy $ Pro Blue   ()
    64  -> Just $ Note Easy $ Pro Green  ()

    72  -> Just $ Note Medium Kick
    73  -> Just $ Note Medium Red
    74  -> Just $ Note Medium $ Pro Yellow ()
    75  -> Just $ Note Medium $ Pro Blue   ()
    76  -> Just $ Note Medium $ Pro Green  ()

    84  -> Just $ Note Hard Kick
    85  -> Just $ Note Hard Red
    86  -> Just $ Note Hard $ Pro Yellow ()
    87  -> Just $ Note Hard $ Pro Blue   ()
    88  -> Just $ Note Hard $ Pro Green  ()

    96  -> Just $ Note Expert Kick
    97  -> Just $ Note Expert Red
    98  -> Just $ Note Expert $ Pro Yellow ()
    99  -> Just $ Note Expert $ Pro Blue   ()
    100 -> Just $ Note Expert $ Pro Green  ()

    110 -> Just $ Toms Yellow True
    111 -> Just $ Toms Blue   True
    112 -> Just $ Toms Green  True
    _   -> Nothing
  noteOff p = case V.fromPitch p of
    110 -> Just $ Toms Yellow False
    111 -> Just $ Toms Blue   False
    112 -> Just $ Toms Green  False
    _   -> Nothing
  in case e of
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))) ->
      (if V.fromVelocity vel == 0 then noteOff else noteOn) p
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> noteOff p
    _ -> Nothing

assignToms :: (NNC.C t) => RTB.T t DrumEvent -> RTB.T t (Difficulty, Gem ProType)
assignToms = go False False False . RTB.normalize where
  go !y !b !g rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> case x of
      Toms color istom -> RTB.delay dt $ case color of
        Yellow -> go istom b     g     rtb'
        Blue   -> go y     istom g     rtb'
        Green  -> go y     b     istom rtb'
      Note diff gem -> case gem of
        Kick -> RTB.cons dt (diff, Kick) $ go y b g rtb'
        Red -> RTB.cons dt (diff, Red) $ go y b g rtb'
        Pro color () -> let
          new = Pro color $ case color of
            Yellow -> if y then Tom else Cymbal
            Blue   -> if b then Tom else Cymbal
            Green  -> if g then Tom else Cymbal
          in RTB.cons dt (diff, new) $ go y b g rtb'
