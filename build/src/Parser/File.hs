{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.File where

import qualified Sound.MIDI.File as F
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Numeric.NonNegative.Class as NNC
import Control.Monad (forM, forM_, liftM)
import Data.Maybe (catMaybes, fromJust)

import StackTrace
import Parser.Base
import qualified Parser.Drums as Drums
import qualified Parser.Events as Events
import qualified Parser.Beat as Beat
import qualified Parser.Countin as Countin
import qualified Parser.FiveButton as FiveButton
import qualified Parser.Vocals as Vocals
import qualified Parser.ProKeys as ProKeys
import qualified Parser.ProGuitar as ProGuitar
import qualified Parser.TH as TH

data Track t
  = PartDrums               (RTB.T t      Drums.Event)
  | PartGuitar              (RTB.T t FiveButton.Event)
  | PartBass                (RTB.T t FiveButton.Event)
  | PartKeys                (RTB.T t FiveButton.Event)
  | PartRealGuitar          (RTB.T t  ProGuitar.Event)
  | PartRealGuitar22        (RTB.T t  ProGuitar.Event)
  | PartRealBass            (RTB.T t  ProGuitar.Event)
  | PartRealBass22          (RTB.T t  ProGuitar.Event)
  | PartRealKeys Difficulty (RTB.T t    ProKeys.Event)
  | PartKeysAnimLH          (RTB.T t    ProKeys.Event)
  | PartKeysAnimRH          (RTB.T t    ProKeys.Event)
  | PartVocals              (RTB.T t     Vocals.Event)
  | Harm1                   (RTB.T t     Vocals.Event)
  | Harm2                   (RTB.T t     Vocals.Event)
  | Harm3                   (RTB.T t     Vocals.Event)
  | Countin                 (RTB.T t    Countin.Event)
  | Events                  (RTB.T t     Events.Event)
  | Beat                    (RTB.T t       Beat.Event)
  deriving (Eq, Ord, Show)

data Song t = Song
  { s_tempos     :: U.TempoMap
  , s_signatures :: U.MeasureMap
  , s_tracks     :: [Track t]
  } deriving (Eq, Ord, Show)

-- | TODO: handle a non-encodeable time signature
showMIDIFile :: Song U.Beats -> F.T
showMIDIFile s = let
  tempos = fmap U.showTempo $ U.tempoMapToBPS $ s_tempos s
  sigs = fmap (fromJust . U.showSignature) $ U.measureMapToLengths $ s_signatures s
  tempoTrk = U.setTrackName "onyxbuild" $ RTB.merge tempos sigs
  in U.encodeFileBeats F.Parallel 480 $ tempoTrk : map showTrack (s_tracks s)

showTrack :: Track U.Beats -> RTB.T U.Beats E.T
showTrack = \case
  PartDrums           t -> U.setTrackName "PART DRUMS"          $ TH.unparseAll TH.unparseOne t
  PartGuitar          t -> U.setTrackName "PART GUITAR"         $ TH.unparseAll TH.unparseOne t
  PartBass            t -> U.setTrackName "PART BASS"           $ TH.unparseAll TH.unparseOne t
  PartKeys            t -> U.setTrackName "PART KEYS"           $ TH.unparseAll TH.unparseOne t
  PartRealGuitar      t -> U.setTrackName "PART REAL_GUITAR"    $ TH.unparseAll TH.unparseOne t
  PartRealGuitar22    t -> U.setTrackName "PART REAL_GUITAR_22" $ TH.unparseAll TH.unparseOne t
  PartRealBass        t -> U.setTrackName "PART REAL_BASS"      $ TH.unparseAll TH.unparseOne t
  PartRealBass22      t -> U.setTrackName "PART REAL_BASS_22"   $ TH.unparseAll TH.unparseOne t
  PartRealKeys Easy   t -> U.setTrackName "PART REAL_KEYS_E"    $ TH.unparseAll TH.unparseOne t
  PartRealKeys Medium t -> U.setTrackName "PART REAL_KEYS_M"    $ TH.unparseAll TH.unparseOne t
  PartRealKeys Hard   t -> U.setTrackName "PART REAL_KEYS_H"    $ TH.unparseAll TH.unparseOne t
  PartRealKeys Expert t -> U.setTrackName "PART REAL_KEYS_X"    $ TH.unparseAll TH.unparseOne t
  PartKeysAnimLH      t -> U.setTrackName "PART KEYS_ANIM_LH"   $ TH.unparseAll TH.unparseOne t
  PartKeysAnimRH      t -> U.setTrackName "PART KEYS_ANIM_RH"   $ TH.unparseAll TH.unparseOne t
  PartVocals          t -> U.setTrackName "PART VOCALS"         $ TH.unparseAll TH.unparseOne t
  Harm1               t -> U.setTrackName "HARM1"               $ TH.unparseAll TH.unparseOne t
  Harm2               t -> U.setTrackName "HARM2"               $ TH.unparseAll TH.unparseOne t
  Harm3               t -> U.setTrackName "HARM3"               $ TH.unparseAll TH.unparseOne t
  Countin             t -> U.setTrackName "countin"             $ TH.unparseAll TH.unparseOne t
  Events              t -> U.setTrackName "EVENTS"              $ TH.unparseAll TH.unparseOne t
  Beat                t -> U.setTrackName "BEAT"                $ TH.unparseAll TH.unparseOne t

readMIDIFile :: (Monad m) => F.T -> StackTraceT m (Song U.Beats)
readMIDIFile mid = case U.decodeFile mid of
  Right _ -> fatal "SMPTE tracks not supported"
  Left trks -> let
    (tempoTrk, restTrks) = case trks of
      t : ts -> (t, ts)
      []     -> (RTB.empty, [])
    mmap = U.makeMeasureMap U.Error tempoTrk
    in do
      songTrks <- forM (zip ([1..] :: [Int]) restTrks) $ \(i, trk) ->
        inside ("track " ++ show i ++ " (0 is tempo track)") $ optional $ parseTrack mmap trk
      return $ Song
        { s_tempos     = U.makeTempoMap tempoTrk
        , s_signatures = mmap
        , s_tracks     = catMaybes songTrks
        }

-- | Strips comments and track names from the track before handing it to a track parser.
stripTrack :: (NNC.C t) => RTB.T t E.T -> RTB.T t E.T
stripTrack = RTB.filter $ \e -> case e of
  E.MetaEvent (Meta.TextEvent ('#' : _)) -> False
  E.MetaEvent (Meta.TrackName _        ) -> False
  _                                      -> True

makeTrackParser :: (Monad m, Ord a) =>
  (TH.ParseOne U.Beats E.T a) -> U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (RTB.T U.Beats a)
makeTrackParser p mmap trk = do
  let (good, bad) = TH.parseAll p $ stripTrack trk
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 bad) $ \(bts, e) ->
    inside (showPosition $ U.applyMeasureMap mmap bts) $ warn $ "Unrecognized event: " ++ show e
  return good

parseTrack :: (Monad m) => U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (Track U.Beats)
parseTrack mmap t = case U.trackName t of
  Nothing -> fatal "Track with no name"
  Just s -> inside ("track named " ++ show s) $ case s of
    "PART DRUMS"          -> liftM PartDrums             $ makeTrackParser TH.parseOne mmap t
    "PART GUITAR"         -> liftM PartGuitar            $ makeTrackParser TH.parseOne mmap t
    "PART BASS"           -> liftM PartBass              $ makeTrackParser TH.parseOne mmap t
    "PART KEYS"           -> liftM PartKeys              $ makeTrackParser TH.parseOne mmap t
    "PART REAL_GUITAR"    -> liftM PartRealGuitar        $ makeTrackParser TH.parseOne mmap t
    "PART REAL_GUITAR_22" -> liftM PartRealGuitar22      $ makeTrackParser TH.parseOne mmap t
    "PART REAL_BASS"      -> liftM PartRealBass          $ makeTrackParser TH.parseOne mmap t
    "PART REAL_BASS_22"   -> liftM PartRealBass22        $ makeTrackParser TH.parseOne mmap t
    "PART REAL_KEYS_E"    -> liftM (PartRealKeys Easy  ) $ makeTrackParser TH.parseOne mmap t
    "PART REAL_KEYS_M"    -> liftM (PartRealKeys Medium) $ makeTrackParser TH.parseOne mmap t
    "PART REAL_KEYS_H"    -> liftM (PartRealKeys Hard  ) $ makeTrackParser TH.parseOne mmap t
    "PART REAL_KEYS_X"    -> liftM (PartRealKeys Expert) $ makeTrackParser TH.parseOne mmap t
    "PART KEYS_ANIM_LH"   -> liftM PartKeysAnimLH        $ makeTrackParser TH.parseOne mmap t
    "PART KEYS_ANIM_RH"   -> liftM PartKeysAnimRH        $ makeTrackParser TH.parseOne mmap t
    "PART VOCALS"         -> liftM PartVocals            $ makeTrackParser TH.parseOne mmap t
    "HARM1"               -> liftM Harm1                 $ makeTrackParser TH.parseOne mmap t
    "HARM2"               -> liftM Harm2                 $ makeTrackParser TH.parseOne mmap t
    "HARM3"               -> liftM Harm3                 $ makeTrackParser TH.parseOne mmap t
    "countin"             -> liftM Countin               $ makeTrackParser TH.parseOne mmap t
    "EVENTS"              -> liftM Events                $ makeTrackParser TH.parseOne mmap t
    "BEAT"                -> liftM Beat                  $ makeTrackParser TH.parseOne mmap t
    _ -> fatal "Unrecognized track name"

showPosition :: U.MeasureBeats -> String
showPosition (m, b) =
  "measure " ++ show (m + 1) ++ ", beat " ++ show (realToFrac b + 1 :: Double)
