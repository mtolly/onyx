{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE LambdaCase    #-}
module Midi
( loadMidi
, ExtendedBeat(..), insertHalfBeats
, Preview(..), buildPreview
) where

import           Control.Monad                    (forM)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import qualified RockBand.File                    as File
import qualified RockBand.Beat                    as Beat
import           RockBand.Drums
import           RockBand.Events
import           RockBand.Common
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util                  as U
import qualified Data.EventList.Absolute.TimeBody as ATB

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

data ExtendedBeat
  = Bar
  | Beat
  | HalfBeat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

insertHalfBeats :: RTB.T U.Beats ExtendedBeat -> RTB.T U.Beats ExtendedBeat
insertHalfBeats = let
  f rtb = case RTB.viewL rtb of
    Nothing              -> RTB.empty
    Just ((dt, x), rtb') -> RTB.cons dt x $ g rtb'
  g rtb = case RTB.viewL rtb of
    Nothing -> RTB.empty
    Just ((dt, x), rtb') ->
      RTB.cons (dt * 0.5) HalfBeat $ RTB.cons (dt * 0.5) x $ g rtb'
  in f

data Preview = Preview
  { gems          :: Map.Map U.Seconds [Gem ProType]
  , beatLines     :: Map.Map U.Seconds ExtendedBeat
  , timeToMeasure :: U.Seconds -> U.MeasureBeats
  , theEnd        :: Maybe U.Seconds
  }

buildPreview :: File.Song U.Beats -> Preview
buildPreview file = let
  tmap = File.s_tempos file
  mmap = File.s_signatures file
  trks = File.s_tracks file
  theDrums  = foldr RTB.merge RTB.empty [ trk | File.PartDrums trk <- trks ]
  theBeat   = foldr RTB.merge RTB.empty [ trk | File.Beat      trk <- trks ]
  theEvents = foldr RTB.merge RTB.empty [ trk | File.Events    trk <- trks ]
  gemTrack :: RTB.T U.Seconds (Gem ProType)
  gemTrack = U.applyTempoTrack tmap $ pickExpert $ assignToms theDrums
  pickExpert = RTB.mapMaybe $ \(d, x) -> case d of
    Expert -> Just x
    _      -> Nothing
  gemMap :: Map.Map U.Seconds [Gem ProType]
  gemMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 $
    RTB.collectCoincident gemTrack
  beatTrack :: RTB.T U.Seconds ExtendedBeat
  beatTrack = U.applyTempoTrack tmap $ insertHalfBeats $ extendBeats theBeat
  beatMap :: Map.Map U.Seconds ExtendedBeat
  beatMap = Map.fromAscList $ ATB.toPairList $ RTB.toAbsoluteEventList 0 beatTrack
  endEvent :: Maybe U.Seconds
  endEvent = case RTB.viewL $ RTB.filter (== End) theEvents of
    Just ((bts, _), _) -> Just $ U.applyTempoMap tmap bts
    Nothing            -> Nothing
  extendBeats = fmap $ \case
    Beat.Bar  -> Bar
    Beat.Beat -> Beat
  in Preview
    { gems = gemMap
    , beatLines = beatMap
    , timeToMeasure = U.applyMeasureMap mmap . U.unapplyTempoMap tmap
    , theEnd = endEvent
    }
