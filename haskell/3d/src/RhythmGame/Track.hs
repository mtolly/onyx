{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module RhythmGame.Track where

import           Control.Arrow                    (first)
import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import           Data.Char                        (toLower)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (mapMaybe)
import qualified Data.Text                        as T
import qualified FeedBack.Load                    as FB
import           Numeric.NonNegative.Class        ((-|))
import qualified Reaper.Extract                   as RPP
import qualified Reaper.Parse                     as RPP
import qualified Reaper.Scan                      as RPP
import qualified RhythmGame.Drums                 as RGDrums
import qualified RockBand.Codec.Beat              as Beat
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil, pattern Wait)
import           Scripts                          (loadMIDI)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension)
import           Text.Decode                      (decodeGeneral)

data PreviewTrack
  = PreviewDrums (RGDrums.Track Double (D.Gem D.ProType))
  | PreviewFive -- TODO
  deriving (Show)

computeTracks
  :: Bool
  -> RBFile.Song (RBFile.FixedFile U.Beats)
  -> [(T.Text, PreviewTrack)]
computeTracks auto song = let
  tempos = RBFile.s_tempos song
  drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
  drums' diff
    = Map.fromList
    $ map (first $ realToFrac . U.applyTempoMap tempos)
    $ ATB.toPairList
    $ RTB.toAbsoluteEventList 0
    $ RTB.collectCoincident
    $ fmap (if auto then RGDrums.Autoplay else RGDrums.Upcoming)
    $ D.computePro diff drums
  drumTrack diff = RGDrums.Track (drums' diff) Map.empty 0 0.2 beats
  beats = let
    sourceMidi = Beat.beatLines $ RBFile.fixedBeat $ RBFile.s_tracks song
    source = if RTB.null sourceMidi
      then RTB.empty -- TODO use makeBeatTrack
      else sourceMidi
    makeBeats _         RNil            = RNil
    makeBeats _         (Wait 0 e rest) = Wait 0 (Just e) $ makeBeats False rest
    makeBeats firstLine (Wait t e rest) = let
      t' = t -| 0.5
      in (if firstLine then Wait 0 Nothing else id)
        $ RTB.delay (t - t')
        $ makeBeats True
        $ Wait (t -| 0.5) e rest
    in Map.fromList
      $ map (first $ realToFrac . U.applyTempoMap tempos)
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0
      $ makeBeats True source
  in concat
    [ [ ("Drums (X+)", PreviewDrums $ drumTrack Nothing)
      | not $ RTB.null $ D.drumKick2x drums
      ]
    , flip mapMaybe [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")] $ \(diff, letter) -> do
      guard $ maybe False (not . RTB.null . D.drumGems)
        $ Map.lookup diff $ D.drumDifficulties drums
      return
        ( "Drums (" <> letter <> ")"
        , PreviewDrums $ drumTrack $ Just diff
        )
    ]

loadTracks
  :: (SendMessage m, MonadIO m)
  => Bool
  -> FilePath
  -> StackTraceT m [(T.Text, PreviewTrack)]
loadTracks auto f = do
  song <- case map toLower $ takeExtension f of
    ".rpp" -> do
      txt <- stackIO $ T.unpack . decodeGeneral <$> B.readFile f
      RBFile.interpretMIDIFile $ RPP.getMIDI $ RPP.parse $ RPP.scan txt
    ".chart" -> do
      chart <- FB.chartToBeats <$> FB.loadChartFile f
      FB.chartToMIDI chart
    _ -> loadMIDI f
  return $ computeTracks auto song
