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
import Data.List (isPrefixOf)

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
loadMidi s = jasmid_loadMidi (toJSString s) >>= fromJasmid

data ProColor = Yellow | Blue | Green
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ProType = Cymbal | Tom
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data Gem t = Kick | Red | Pro ProColor t
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)
data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data DrumEvent
  = ProType ProColor ProType
  -- ^ must come before 'Note' for 'RTB.normalize'
  -- also, Cymbal < Tom so note-off comes before note-on
  | Discobeat Difficulty Bool -- ^ must come before 'Note' for 'RTB.normalize'
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

    110 -> Just $ ProType Yellow Tom
    111 -> Just $ ProType Blue   Tom
    112 -> Just $ ProType Green  Tom
    _   -> Nothing
  noteOff p = case V.fromPitch p of
    110 -> Just $ ProType Yellow Cymbal
    111 -> Just $ ProType Blue   Cymbal
    112 -> Just $ ProType Green  Cymbal
    _   -> Nothing
  in case e of
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn p vel))) ->
      (if V.fromVelocity vel == 0 then noteOff else noteOn) p
    E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> noteOff p
    E.MetaEvent (Meta.TextEvent s) -> case s of
      "[mix 0 drums0d]" -> Just $ Discobeat Easy True
      "[mix 1 drums0d]" -> Just $ Discobeat Medium True
      "[mix 2 drums0d]" -> Just $ Discobeat Hard True
      "[mix 3 drums0d]" -> Just $ Discobeat Expert True
      _ | "[mix 0 drums" `isPrefixOf` s -> Just $ Discobeat Easy False
        | "[mix 1 drums" `isPrefixOf` s -> Just $ Discobeat Medium False
        | "[mix 2 drums" `isPrefixOf` s -> Just $ Discobeat Hard False
        | "[mix 3 drums" `isPrefixOf` s -> Just $ Discobeat Expert False
        | otherwise -> Nothing
    _ -> Nothing

data DrumState = DrumState
  { yellowType  :: ProType
  , blueType    :: ProType
  , greenType   :: ProType
  , easyDisco   :: Bool
  , mediumDisco :: Bool
  , hardDisco   :: Bool
  , expertDisco :: Bool
  }

defDrumState :: DrumState
defDrumState = DrumState Cymbal Cymbal Cymbal False False False False

assignToms :: (NNC.C t) => RTB.T t DrumEvent -> RTB.T t (Difficulty, Gem ProType)
assignToms = go defDrumState . RTB.normalize where
  go ds rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') -> case x of
      ProType color ptype -> RTB.delay dt $ case color of
        Yellow -> go ds{ yellowType = ptype } rtb'
        Blue   -> go ds{ blueType   = ptype } rtb'
        Green  -> go ds{ greenType  = ptype } rtb'
      Discobeat diff b -> RTB.delay dt $ case diff of
        Easy   -> go ds{ easyDisco   = b } rtb'
        Medium -> go ds{ mediumDisco = b } rtb'
        Hard   -> go ds{ hardDisco   = b } rtb'
        Expert -> go ds{ expertDisco = b } rtb'
      Note diff gem -> case gem of
        Kick -> RTB.cons dt (diff, Kick) $ go ds rtb'
        Red -> if isDisco
          then RTB.cons dt (diff, Pro Yellow Cymbal) $ go ds rtb'
          else RTB.cons dt (diff, Red) $ go ds rtb'
        Pro color () -> let
          new = case color of
            Yellow -> if isDisco
              then Red
              else Pro Yellow $ yellowType ds
            Blue   -> Pro Blue $ blueType ds
            Green  -> Pro Green $ greenType ds
          in RTB.cons dt (diff, new) $ go ds rtb'
        where isDisco = case diff of
                Easy -> easyDisco ds
                Medium -> mediumDisco ds
                Hard -> hardDisco ds
                Expert -> expertDisco ds
