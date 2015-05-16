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
import qualified Parser.TH as TH

data Track t
  = PartDrums (RTB.T t Drums.Event     )
  | PartGuitar (RTB.T t FiveButton.Event)
  | PartBass   (RTB.T t FiveButton.Event)
  | PartKeys   (RTB.T t FiveButton.Event)
  | PartRealKeys Difficulty (RTB.T t ProKeys.Event)
  | PartKeysAnimLH          (RTB.T t ProKeys.Event)
  | PartKeysAnimRH          (RTB.T t ProKeys.Event)
  | PartVocals (RTB.T t Vocals.Event    )
  | Harm1      (RTB.T t Vocals.Event    )
  | Harm2      (RTB.T t Vocals.Event    )
  | Harm3      (RTB.T t Vocals.Event    )
  | Countin (RTB.T t Countin.Event   )
  | Events  (RTB.T t Events.Event    )
  | Beat    (RTB.T t Beat.Event      )
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
  PartDrums  t -> U.setTrackName "PART DRUMS"  $ TH.unparseAll unparseDrums t
  PartGuitar t -> U.setTrackName "PART GUITAR" $ TH.unparseAll unparseFiveButton t
  PartBass   t -> U.setTrackName "PART BASS"   $ TH.unparseAll unparseFiveButton t
  PartKeys   t -> U.setTrackName "PART KEYS"   $ TH.unparseAll unparseFiveButton t
  PartRealKeys d t -> U.setTrackName ("PART REAL_KEYS_" ++ take 1 (show d)) $ TH.unparseAll unparseProKeys t
  PartKeysAnimLH t -> U.setTrackName "PART KEYS_ANIM_LH" $ TH.unparseAll unparseProKeys t
  PartKeysAnimRH t -> U.setTrackName "PART KEYS_ANIM_RH" $ TH.unparseAll unparseProKeys t
  PartVocals t -> U.setTrackName "PART VOCALS" $ TH.unparseAll unparseVocals  t 
  Harm1      t -> U.setTrackName "HARM1"       $ TH.unparseAll unparseVocals  t
  Harm2      t -> U.setTrackName "HARM2"       $ TH.unparseAll unparseVocals  t
  Harm3      t -> U.setTrackName "HARM3"       $ TH.unparseAll unparseVocals  t
  Countin    t -> U.setTrackName "countin"     $ TH.unparseAll unparseCountin t
  Events     t -> U.setTrackName "EVENTS"      $ TH.unparseAll unparseEvents  t
  Beat       t -> U.setTrackName "BEAT"        $ TH.unparseAll unparseBeat    t

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

makeTrackParser :: (Monad m) =>
  (E.T -> Maybe [a]) -> U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (RTB.T U.Beats a)
makeTrackParser p mmap trk = do
  let (good, bad) = RTB.partitionMaybe p $ stripTrack trk
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 bad) $ \(bts, e) ->
    inside (showPosition $ U.applyMeasureMap mmap bts) $ warn $ "Unrecognized event: " ++ show e
  return $ RTB.flatten good

makeTrackParser' :: (Monad m, Ord a) =>
  (TH.ParseOne U.Beats E.T a) -> U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (RTB.T U.Beats a)
makeTrackParser' p mmap trk = do
  let (good, bad) = TH.parseAll p $ stripTrack trk
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 bad) $ \(bts, e) ->
    inside (showPosition $ U.applyMeasureMap mmap bts) $ warn $ "Unrecognized event: " ++ show e
  return good

parseTrack :: (Monad m) => U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (Track U.Beats)
parseTrack mmap t = case U.trackName t of
  Nothing -> fatal "Track with no name"
  Just s -> inside ("track named " ++ show s) $ case s of
    "PART DRUMS"  -> liftM PartDrums  $ makeTrackParser' parseDrums          mmap t
    "PART GUITAR" -> liftM PartGuitar $ makeTrackParser' parseFiveButton     mmap t
    "PART BASS"   -> liftM PartBass   $ makeTrackParser' parseFiveButton     mmap t
    "PART KEYS"   -> liftM PartKeys   $ makeTrackParser' parseFiveButton     mmap t
    "PART REAL_KEYS_E" -> liftM (PartRealKeys Easy  ) $ makeTrackParser' parseProKeys mmap t
    "PART REAL_KEYS_M" -> liftM (PartRealKeys Medium) $ makeTrackParser' parseProKeys mmap t
    "PART REAL_KEYS_H" -> liftM (PartRealKeys Hard  ) $ makeTrackParser' parseProKeys mmap t
    "PART REAL_KEYS_X" -> liftM (PartRealKeys Expert) $ makeTrackParser' parseProKeys mmap t
    "PART KEYS_ANIM_LH" -> liftM PartKeysAnimLH $ makeTrackParser' parseProKeys mmap t
    "PART KEYS_ANIM_RH" -> liftM PartKeysAnimRH $ makeTrackParser' parseProKeys mmap t
    "PART VOCALS" -> liftM PartVocals $ makeTrackParser' parseVocals mmap t
    "HARM1"       -> liftM Harm1      $ makeTrackParser' parseVocals mmap t
    "HARM2"       -> liftM Harm2      $ makeTrackParser' parseVocals mmap t
    "HARM3"       -> liftM Harm3      $ makeTrackParser' parseVocals mmap t
    "countin"     -> liftM Countin    $ makeTrackParser' parseCountin        mmap t
    "EVENTS"      -> liftM Events     $ makeTrackParser' parseEvents         mmap t
    "BEAT"        -> liftM Beat       $ makeTrackParser' parseBeat           mmap t
    _ -> fatal "Unrecognized track name"

showPosition :: U.MeasureBeats -> String
showPosition (m, b) =
  "measure " ++ show (m + 1) ++ ", beat " ++ show (realToFrac b + 1 :: Double)

parseDrums :: (NNC.C t) => TH.ParseOne t E.T Drums.Event
parseDrums = $(fst Drums.rosetta)
unparseDrums :: TH.UnparseOne U.Beats E.T Drums.Event
unparseDrums = $(snd Drums.rosetta)

parseBeat :: (NNC.C t) => TH.ParseOne t E.T Beat.Event
parseBeat = $(fst Beat.rosetta)
unparseBeat :: TH.UnparseOne U.Beats E.T Beat.Event
unparseBeat = $(snd Beat.rosetta)

parseCountin :: (NNC.C t) => TH.ParseOne t E.T Countin.Event
parseCountin = $(fst Countin.rosetta)
unparseCountin :: (NNC.C t) => TH.UnparseOne t E.T Countin.Event
unparseCountin = $(snd Countin.rosetta)

parseFiveButton :: (NNC.C t) => TH.ParseOne t E.T FiveButton.Event
parseFiveButton = $(fst FiveButton.rosetta)
unparseFiveButton :: (NNC.C t) => TH.UnparseOne t E.T FiveButton.Event
unparseFiveButton = $(snd FiveButton.rosetta)

parseProKeys :: (NNC.C t) => TH.ParseOne t E.T ProKeys.Event
parseProKeys = $(fst ProKeys.rosetta)
unparseProKeys :: TH.UnparseOne U.Beats E.T ProKeys.Event
unparseProKeys = $(snd ProKeys.rosetta)

parseEvents :: (NNC.C t) => TH.ParseOne t E.T Events.Event
parseEvents = $(fst Events.rosetta)
unparseEvents :: TH.UnparseOne U.Beats E.T Events.Event
unparseEvents = $(snd Events.rosetta)

parseVocals :: (NNC.C t) => TH.ParseOne t E.T Vocals.Event
parseVocals = $(fst Vocals.rosetta)
unparseVocals :: TH.UnparseOne U.Beats E.T Vocals.Event
unparseVocals = $(snd Vocals.rosetta)
