{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.MIDI.Track.Mania where

import           Control.Monad                    (guard)
import           Control.Monad.Codec
import           Data.Bifunctor
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     ((<&>))
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, listToMaybe)
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.PhaseShift.Dance
import qualified Sound.MIDI.Message.Channel       as C
import qualified Sound.MIDI.Message.Channel.Voice as V

data ManiaTrack t = ManiaTrack
  { notes     :: RTB.T t (Edge () (Int, NoteType))
  , overdrive :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ManiaTrack t)

instance TraverseTrack ManiaTrack where
  traverseTrack fn (ManiaTrack a b) = ManiaTrack <$> fn a <*> fn b

instance ParseTrack ManiaTrack where
  parseTrack = do
    overdrive <- (.overdrive) =. edges 116
    notes     <- (.notes    ) =. fatBlips (1/8) Codec
      { codecIn = slurpTrack \mt -> let
        applyPitchEdges (pitch, thisEdges) = thisEdges <&> bimap
          (const ())
          (\chan -> (V.fromPitch pitch, fromMaybe NoteNormal $ lookup (C.fromChannel chan) channelMap))
        notes
          = foldr RTB.merge RTB.empty
          $ map applyPitchEdges
          $ Map.toList mt.midiNotes
        mt' = mt { midiNotes = Map.empty }
        in (notes, mt')
      , codecOut = makeTrackBuilder $ fmap $ makeEdge' . bimap
        (const 96)
        (\(k, noteType) -> (fromEnum noteType, k))
      }
    return ManiaTrack{..}

getManiaNormalNotes, getEncoreNotes :: (NNC.C t) => ManiaTrack t -> RTB.T t (Edge () Int)
getManiaNormalNotes = getManiaNormalNotes' False
getEncoreNotes      = getManiaNormalNotes' True

getManiaNormalNotes' :: (NNC.C t) => Bool -> ManiaTrack t -> RTB.T t (Edge () Int)
getManiaNormalNotes' keepLift mt = RTB.mapMaybe
  (\case
    EdgeOn () (k, noteType) -> guard (keep noteType) >> Just (EdgeOn () k)
    EdgeOff   (k, noteType) -> guard (keep noteType) >> Just (EdgeOff   k)
  )
  mt.notes
  where keep = \case
          NoteNormal -> True
          NoteMine   -> False
          NoteLift   -> keepLift
          NoteRoll   -> True

danceDifficultyName :: SMDifficulty -> T.Text
danceDifficultyName = \case
  SMBeginner  -> "beginner"
  SMEasy      -> "easy"
  SMMedium    -> "medium"
  SMHard      -> "hard"
  SMChallenge -> "challenge"

danceToMania :: DanceTrack t -> Map.Map T.Text (ManiaTrack t)
danceToMania dt = Map.fromList do
  diff <- [minBound .. maxBound] :: [SMDifficulty]
  dd <- toList $ Map.lookup diff dt.danceDifficulties
  let mt = ManiaTrack
        { notes = fmap (fmap $ first fromEnum) dd.danceNotes
        , overdrive = dt.danceOverdrive
        }
  return (danceDifficultyName diff, mt)

maniaToDance :: (NNC.C t) => [(SMDifficulty, T.Text)] -> Map.Map T.Text (ManiaTrack t) -> DanceTrack t
maniaToDance diffMapping trks = DanceTrack
  { danceDifficulties = Map.fromList do
    (smdiff, maniaDiff) <- diffMapping
    maniaTrack <- toList $ Map.lookup maniaDiff trks
    let toArrows = mapM \(k, noteType) -> do
          guard $ k <= 3
          Just (toEnum k, noteType)
    return (smdiff, DanceDifficulty $ RTB.mapMaybe toArrows maniaTrack.notes)
  , danceOverdrive = fromMaybe RTB.empty $ listToMaybe do
    (_, maniaDiff) <- diffMapping
    maniaTrack <- toList $ Map.lookup maniaDiff trks
    filter (not . RTB.null) [maniaTrack.overdrive]
  }
