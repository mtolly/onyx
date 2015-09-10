{-# LANGUAGE LambdaCase #-}
module RockBand.File where

import           Control.Monad                    (forM, forM_, liftM)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Maybe                       (catMaybes, fromJust)
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

import qualified RockBand.Beat                    as Beat
import           RockBand.Common
import qualified RockBand.Countin                 as Countin
import qualified RockBand.Drums                   as Drums
import qualified RockBand.Events                  as Events
import qualified RockBand.FiveButton              as FiveButton
import           RockBand.Parse
import qualified RockBand.ProGuitar               as ProGuitar
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as Vocals
import           StackTrace

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
  | RawTrack                (RTB.T t              E.T)
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
  PartDrums           t -> U.setTrackName "PART DRUMS"          $ unparseAll unparseOne t
  PartGuitar          t -> U.setTrackName "PART GUITAR"         $ unparseAll unparseOne t
  PartBass            t -> U.setTrackName "PART BASS"           $ unparseAll unparseOne t
  PartKeys            t -> U.setTrackName "PART KEYS"           $ unparseAll unparseOne t
  PartRealGuitar      t -> U.setTrackName "PART REAL_GUITAR"    $ unparseAll unparseOne t
  PartRealGuitar22    t -> U.setTrackName "PART REAL_GUITAR_22" $ unparseAll unparseOne t
  PartRealBass        t -> U.setTrackName "PART REAL_BASS"      $ unparseAll unparseOne t
  PartRealBass22      t -> U.setTrackName "PART REAL_BASS_22"   $ unparseAll unparseOne t
  PartRealKeys Easy   t -> U.setTrackName "PART REAL_KEYS_E"    $ unparseAll unparseOne t
  PartRealKeys Medium t -> U.setTrackName "PART REAL_KEYS_M"    $ unparseAll unparseOne t
  PartRealKeys Hard   t -> U.setTrackName "PART REAL_KEYS_H"    $ unparseAll unparseOne t
  PartRealKeys Expert t -> U.setTrackName "PART REAL_KEYS_X"    $ unparseAll unparseOne t
  PartKeysAnimLH      t -> U.setTrackName "PART KEYS_ANIM_LH"   $ unparseAll unparseOne t
  PartKeysAnimRH      t -> U.setTrackName "PART KEYS_ANIM_RH"   $ unparseAll unparseOne t
  PartVocals          t -> U.setTrackName "PART VOCALS"         $ unparseAll unparseOne t
  Harm1               t -> U.setTrackName "HARM1"               $ unparseAll unparseOne t
  Harm2               t -> U.setTrackName "HARM2"               $ unparseAll unparseOne t
  Harm3               t -> U.setTrackName "HARM3"               $ unparseAll unparseOne t
  Countin             t -> U.setTrackName "countin"             $ unparseAll unparseOne t
  Events              t -> U.setTrackName "EVENTS"              $ unparseAll unparseOne t
  Beat                t -> U.setTrackName "BEAT"                $ unparseAll unparseOne t
  RawTrack            t -> t

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
  ParseOne U.Beats E.T a -> U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (RTB.T U.Beats a)
makeTrackParser p mmap trk = do
  let (good, bad) = parseAll p $ stripTrack trk
  forM_ (ATB.toPairList $ RTB.toAbsoluteEventList 0 bad) $ \(bts, e) ->
    inside (showPosition $ U.applyMeasureMap mmap bts) $ warn $ "Unrecognized event: " ++ show e
  return good

parseTrack :: (Monad m) => U.MeasureMap -> RTB.T U.Beats E.T -> StackTraceT m (Track U.Beats)
parseTrack mmap t = case U.trackName t of
  Nothing -> fatal "Track with no name"
  Just s -> inside ("track named " ++ show s) $ case s of
    "PART DRUMS"          -> liftM PartDrums             $ makeTrackParser parseOne mmap t
    "PART GUITAR"         -> liftM PartGuitar            $ makeTrackParser parseOne mmap t
    "PART BASS"           -> liftM PartBass              $ makeTrackParser parseOne mmap t
    "PART KEYS"           -> liftM PartKeys              $ makeTrackParser parseOne mmap t
    "PART REAL_GUITAR"    -> liftM PartRealGuitar        $ makeTrackParser parseOne mmap t
    "PART REAL_GUITAR_22" -> liftM PartRealGuitar22      $ makeTrackParser parseOne mmap t
    "PART REAL_BASS"      -> liftM PartRealBass          $ makeTrackParser parseOne mmap t
    "PART REAL_BASS_22"   -> liftM PartRealBass22        $ makeTrackParser parseOne mmap t
    "PART REAL_KEYS_E"    -> liftM (PartRealKeys Easy  ) $ makeTrackParser parseOne mmap t
    "PART REAL_KEYS_M"    -> liftM (PartRealKeys Medium) $ makeTrackParser parseOne mmap t
    "PART REAL_KEYS_H"    -> liftM (PartRealKeys Hard  ) $ makeTrackParser parseOne mmap t
    "PART REAL_KEYS_X"    -> liftM (PartRealKeys Expert) $ makeTrackParser parseOne mmap t
    "PART KEYS_ANIM_LH"   -> liftM PartKeysAnimLH        $ makeTrackParser parseOne mmap t
    "PART KEYS_ANIM_RH"   -> liftM PartKeysAnimRH        $ makeTrackParser parseOne mmap t
    "PART VOCALS"         -> liftM PartVocals            $ makeTrackParser parseOne mmap t
    "HARM1"               -> liftM Harm1                 $ makeTrackParser parseOne mmap t
    "HARM2"               -> liftM Harm2                 $ makeTrackParser parseOne mmap t
    "HARM3"               -> liftM Harm3                 $ makeTrackParser parseOne mmap t
    "countin"             -> liftM Countin               $ makeTrackParser parseOne mmap t
    "EVENTS"              -> liftM Events                $ makeTrackParser parseOne mmap t
    "BEAT"                -> liftM Beat                  $ makeTrackParser parseOne mmap t
    _ -> fatal "Unrecognized track name"

showPosition :: U.MeasureBeats -> String
showPosition (m, b) =
  "measure " ++ show (m + 1) ++ ", beat " ++ show (realToFrac b + 1 :: Double)

playGuitarFile :: (NNC.C t) => [Int] -> [Int] -> Song t -> Song t
playGuitarFile goffs boffs s =
  s { s_tracks = map RawTrack $ s_tracks s >>= playGuitarTrack goffs boffs }

playGuitarTrack :: (NNC.C t) => [Int] -> [Int] -> Track t -> [RTB.T t E.T]
playGuitarTrack goffs boffs = \case
  PartRealGuitar   t -> gtr "GTR" t
  PartRealGuitar22 t -> gtr "GTR22" t
  PartRealBass     t -> bass "BASS" t
  PartRealBass22   t -> bass "BASS22" t
  _ -> []
  where gtr = go ProGuitar.standardGuitar goffs
        bass = go ProGuitar.standardBass boffs
        go stdtuning offs name trk = let
          tuning = zipWith (+) stdtuning offs
          expert = flip RTB.mapMaybe trk $ \case
            ProGuitar.DiffEvent Expert evt -> Just evt
            _                              -> Nothing
          in do
            (str, notes) <- ProGuitar.playGuitar tuning expert
            return $ U.setTrackName (name ++ "_" ++ show str) notes

copyExpert :: (NNC.C t) => Track t -> Track t
copyExpert = \case
  PartDrums        t -> PartDrums        $      Drums.copyExpert t
  PartGuitar       t -> PartGuitar       $ FiveButton.copyExpert t
  PartBass         t -> PartBass         $ FiveButton.copyExpert t
  PartKeys         t -> PartKeys         $ FiveButton.copyExpert t
  PartRealGuitar   t -> PartRealGuitar   $  ProGuitar.copyExpert t
  PartRealGuitar22 t -> PartRealGuitar22 $  ProGuitar.copyExpert t
  PartRealBass     t -> PartRealBass     $  ProGuitar.copyExpert t
  PartRealBass22   t -> PartRealBass22   $  ProGuitar.copyExpert t
  trk                -> trk

autoHandPosition :: (NNC.C t) => Track t -> Track t
autoHandPosition = \case
  PartRealGuitar   t -> PartRealGuitar   $ ProGuitar.autoHandPosition t
  PartRealGuitar22 t -> PartRealGuitar22 $ ProGuitar.autoHandPosition t
  PartRealBass     t -> PartRealBass     $ ProGuitar.autoHandPosition t
  PartRealBass22   t -> PartRealBass22   $ ProGuitar.autoHandPosition t
  trk                -> trk

eachTrack :: (Track t -> Track t) -> Song t -> Song t
eachTrack f s = s { s_tracks = map f $ s_tracks s }
