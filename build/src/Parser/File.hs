{-# LANGUAGE LambdaCase #-}
module Parser.File where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Numeric.NonNegative.Class as NNC
import qualified Sound.MIDI.File.Load as Load
import Data.Either (lefts, rights)

import Parser
import qualified Parser.Drums as Drums

data Track t
  = PartDrums (RTB.T t Drums.Event)
  deriving (Eq, Ord, Show)

isTrackName :: E.T -> Bool
isTrackName (E.MetaEvent (Meta.TrackName _)) = True
isTrackName _                                = False

data Song t = Song
  { s_tempos     :: U.TempoMap
  , s_signatures :: U.MeasureMap
  , s_tracks     :: [Track t]
  , s_warnings   :: [String]
  } deriving (Eq, Ord, Show)

readMIDIFile :: FilePath -> IO (Song U.Beats)
readMIDIFile fp = do
  mid <- Load.fromFile fp
  case U.decodeFile mid of
    Right _ ->
      error "Sound.MIDI.RockBand.File.readMIDIFile: SMPTE tracks not supported"
    Left trks -> let
      (tempoTrk, restTrks) = case trks of
        t : ts -> (t, ts)
        []     -> (RTB.empty, [])
      rbResults :: [Either String (Track U.Beats, RTB.T U.Beats String)]
      rbResults = map readTrack restTrks
      rbTracks  = map fst $ rights rbResults
      messages  = lefts rbResults ++ concatMap (messagesFromTrack . snd) (rights rbResults)
      mmap = U.makeMeasureMap U.Error tempoTrk
      messagesFromTrack = map makeMessage . ATB.toPairList . RTB.toAbsoluteEventList 0
      makeMessage (bts, msg) = let
        (msr, inmsr) = U.applyMeasureMap mmap bts
        in "Measure " ++ show (msr + 1) ++ ", beat " ++
          show (realToFrac inmsr + 1 :: Double) ++ ": " ++ msg
      in return $ Song
        { s_tempos     = U.makeTempoMap tempoTrk
        , s_signatures = mmap
        , s_tracks     = rbTracks
        , s_warnings   = messages
        }

readTrack :: (NNC.C t) => RTB.T t E.T -> Either String (Track t, RTB.T t String)
readTrack t = case U.trackName t of
  Nothing -> Left "Track with no name"
  Just s -> case s of
    "PART DRUMS" -> Right $ let
      (drumEvents, midiEvents) = RTB.partitionMaybe Drums.readEvent t
      makeWarning e = "Unrecognized event in PART DRUMS: " ++ show e
      in ( PartDrums $ RTB.flatten drumEvents
         , fmap makeWarning $ RTB.filter (not . isTrackName) midiEvents
         )
    _ -> Left $ "Unrecognized track name: " ++ show s

parseTrack :: (Monad m, NNC.C t) => RTB.T t E.T -> ParserT m (Track t, RTB.T t String)
parseTrack t = case U.trackName t of
  Nothing -> fatal "Track with no name"
  Just s -> case s of
    "PART DRUMS" -> inside "PART DRUMS track" $ let
      (drumEvents, midiEvents) = RTB.partitionMaybe Drums.readEvent t
      makeWarning e = "Unrecognized event: " ++ show e
      in return ( PartDrums $ RTB.flatten drumEvents
                , fmap makeWarning $ RTB.filter (not . isTrackName) midiEvents
                )
    _ -> fatal $ "Unrecognized track name: " ++ show s

showPosition :: U.MeasureBeats -> String
showPosition (m, b) =
  "measure " ++ show (m + 1) ++ ", beat " ++ show (realToFrac b + 1 :: Double)
