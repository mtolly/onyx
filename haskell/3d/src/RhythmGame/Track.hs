{-# LANGUAGE LambdaCase        #-}
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
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import qualified FeedBack.Load                    as FB
import           Guitars
import           Numeric.NonNegative.Class        ((-|))
import qualified Reaper.Extract                   as RPP
import qualified Reaper.Parse                     as RPP
import qualified Reaper.Scan                      as RPP
import qualified RhythmGame.PNF                   as PNF
import qualified RockBand.Codec.Beat              as Beat
import qualified RockBand.Codec.Drums             as D
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as F
import           RockBand.Common
import           Scripts                          (loadMIDI)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension)
import           Text.Decode                      (decodeGeneral)

data PreviewTrack
  = PreviewDrums (Map.Map Double (PNF.CommonState (PNF.DrumState (D.Gem D.ProType))))
  | PreviewFive (Map.Map Double (PNF.CommonState (PNF.GuitarState (Maybe F.Color))))
  deriving (Show)

computeTracks
  :: RBFile.Song (RBFile.FixedFile U.Beats)
  -> [(T.Text, PreviewTrack)]
computeTracks song = let

  rtbToMap
    = Map.fromList
    . map (first realToFrac)
    . ATB.toPairList
    . RTB.toAbsoluteEventList 0
    . U.applyTempoTrack tempos
  tempos = RBFile.s_tempos song
  toggle
    = PNF.makeToggle
    . rtbToMap
    . RTB.normalize
  diffPairs = [(Expert, "X"), (Hard, "H"), (Medium, "M"), (Easy, "E")]

  drumSrc = RBFile.fixedPartDrums $ RBFile.s_tracks song
  drumTrack diff = let
    drumMap :: Map.Map Double [D.Gem D.ProType]
    drumMap
      = rtbToMap
      $ RTB.collectCoincident
      $ D.computePro diff drumSrc
    drumStates = (\((a, b), c) -> PNF.DrumState a b c) <$> do
      (Set.fromList <$> drumMap)
        `PNF.zipStateMaps` Map.empty -- TODO lanes
        `PNF.zipStateMaps` toggle (D.drumActivation drumSrc) -- TODO remove BRE (act with [coda])
    in (\(((((a, b), c), d), e)) -> PNF.CommonState a b c d e) <$> do
      drumStates
        `PNF.zipStateMaps` toggle (D.drumOverdrive drumSrc)
        `PNF.zipStateMaps` Map.empty -- TODO get BRE from drumActivation + [coda]
        `PNF.zipStateMaps` toggle (D.drumSolo drumSrc)
        `PNF.zipStateMaps` fmap Just beats

  fiveTrack src isKeys diff = let
    thisDiff = fromMaybe mempty $ Map.lookup diff $ F.fiveDifficulties src
    assigned :: RTB.T U.Beats (LongNote Bool (Maybe F.Color, StrumHOPOTap))
    assigned
      = splitEdges
      $ fmap (\(a, (b, c)) -> (a, b, c))
      $ applyStatus1 False (RTB.normalize $ F.fiveOverdrive src)
      $ applyForces (getForces5 thisDiff)
      $ strumHOPOTap' (if isKeys then HOPOsRBKeys else HOPOsRBGuitar) hopoThreshold
      $ openNotes' thisDiff
    assignedMap :: Map.Map Double (Map.Map (Maybe F.Color) (PNF.PNF PNF.IsOverdrive StrumHOPOTap))
    assignedMap
      = rtbToMap
      $ buildFiveStatus PNF.empty
      $ RTB.collectCoincident
      $ assigned
    buildFiveStatus _    RNil                 = RNil
    buildFiveStatus prev (Wait dt edges rest) = let
      applyEdge edge = case edge of
        Blip _ (color, sht) -> Map.alter (\case
          Nothing             -> Just $ PNF.N sht
          Just PNF.Empty      -> Just $ PNF.N sht
          Just (PNF.P  wasOD) -> Just $ PNF.PN wasOD sht
          Just (PNF.PF wasOD) -> Just $ PNF.PN wasOD sht
          x -> x -- shouldn't happen
          ) color
        NoteOn od (color, sht) -> Map.alter (\case
          Nothing             -> Just $ PNF.NF sht od
          Just PNF.Empty      -> Just $ PNF.NF sht od
          Just (PNF.P  wasOD) -> Just $ PNF.PNF wasOD sht od
          Just (PNF.PF wasOD) -> Just $ PNF.PNF wasOD sht od
          x -> x -- shouldn't happen
          ) color
        NoteOff (color, _) -> Map.update (\case
          PNF.PF wasOD -> Just $ PNF.P wasOD
          x -> Just x -- could happen if Blip or NoteOn was applied first
          ) color
      this = foldr applyEdge (PNF.after prev) edges
      in Wait dt this $ buildFiveStatus this rest
    hopoThreshold = 170 / 480 :: U.Beats -- TODO support different threshold
    fiveStates = (\((a, b), c) -> PNF.GuitarState a b c) <$> do
      assignedMap
        `PNF.zipStateMaps` Map.empty -- TODO tremolo
        `PNF.zipStateMaps` Map.empty -- TODO trill
    in (\(((((a, b), c), d), e)) -> PNF.CommonState a b c d e) <$> do
      fiveStates
        `PNF.zipStateMaps` toggle (F.fiveOverdrive src)
        `PNF.zipStateMaps` toggle (F.fiveBRE src)
        `PNF.zipStateMaps` toggle (F.fiveSolo src)
        `PNF.zipStateMaps` fmap Just beats

  beats :: Map.Map Double (Maybe Beat.BeatEvent)
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
    [ do
      (name, isKeys, src) <-
        [ ("Guitar", False, RBFile.fixedPartGuitar $ RBFile.s_tracks song)
        , ("Bass", False, RBFile.fixedPartBass $ RBFile.s_tracks song)
        , ("Keys", True, RBFile.fixedPartKeys $ RBFile.s_tracks song)
        ]
      (diff, letter) <- diffPairs
      guard $ maybe False (not . RTB.null . F.fiveGems)
        $ Map.lookup diff $ F.fiveDifficulties src
      return
        ( name <> " (" <> letter <> ")"
        , PreviewFive $ fiveTrack src isKeys diff
        )
    , [ ("Drums (X+)", PreviewDrums $ drumTrack Nothing)
      | not $ RTB.null $ D.drumKick2x drumSrc
      ]
    , flip mapMaybe diffPairs $ \(diff, letter) -> do
      guard $ maybe False (not . RTB.null . D.drumGems)
        $ Map.lookup diff $ D.drumDifficulties drumSrc
      return
        ( "Drums (" <> letter <> ")"
        , PreviewDrums $ drumTrack $ Just diff
        )
    ]

loadTracks
  :: (SendMessage m, MonadIO m)
  => FilePath
  -> StackTraceT m [(T.Text, PreviewTrack)]
loadTracks f = do
  song <- case map toLower $ takeExtension f of
    ".rpp" -> do
      txt <- stackIO $ T.unpack . decodeGeneral <$> B.readFile f
      RBFile.interpretMIDIFile $ RPP.getMIDI $ RPP.parse $ RPP.scan txt
    ".chart" -> do
      chart <- FB.chartToBeats <$> FB.loadChartFile f
      FB.chartToMIDI chart
    _ -> loadMIDI f
  return $ computeTracks song
