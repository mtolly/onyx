{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
module Onyx.MIDI.Track.Mania where

import           Control.Monad                    (guard)
import           Control.Monad.Codec
import           Data.Bifunctor
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
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
  { maniaNotes     :: RTB.T t (Edge () (Int, NoteType))
  , maniaOverdrive :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ManiaTrack t)

instance TraverseTrack ManiaTrack where
  traverseTrack fn (ManiaTrack a b) = ManiaTrack <$> fn a <*> fn b

instance ParseTrack ManiaTrack where
  parseTrack = do
    maniaOverdrive <- maniaOverdrive =. edges 116
    maniaNotes     <- maniaNotes     =. fatBlips (1/8) Codec
      { codecIn = slurpTrack $ \mt -> let
        applyPitchEdges (pitch, thisEdges) = flip fmap thisEdges $ bimap
          (const ())
          (\chan -> (V.fromPitch pitch, fromMaybe NoteNormal $ lookup (C.fromChannel chan) channelMap))
        notes
          = foldr RTB.merge RTB.empty
          $ map applyPitchEdges
          $ Map.toList $ midiNotes mt
        mt' = mt { midiNotes = Map.empty }
        in (notes, mt')
      , codecOut = makeTrackBuilder $ fmap $ makeEdge' . bimap
        (const 96)
        (\(k, noteType) -> (fromEnum noteType, k))
      }
    return ManiaTrack{..}

getManiaNormalNotes :: (NNC.C t) => ManiaTrack t -> RTB.T t (Edge () Int)
getManiaNormalNotes mt = RTB.mapMaybe
  (\case
    EdgeOn () (k, noteType) -> guard (keep noteType) >> Just (EdgeOn () k)
    EdgeOff   (k, noteType) -> guard (keep noteType) >> Just (EdgeOff   k)
  )
  (maniaNotes mt)
  where keep = \case
          NoteNormal -> True
          NoteMine   -> False
          NoteLift   -> False
          NoteRoll   -> True

danceDifficultyName :: SMDifficulty -> T.Text
danceDifficultyName = \case
  SMBeginner  -> "beginner"
  SMEasy      -> "easy"
  SMMedium    -> "medium"
  SMHard      -> "hard"
  SMChallenge -> "challenge"

danceToMania :: DanceTrack t -> Map.Map T.Text (ManiaTrack t)
danceToMania dt = Map.fromList $ do
  diff <- [minBound .. maxBound] :: [SMDifficulty]
  dd <- toList $ Map.lookup diff dt.danceDifficulties
  let mt = ManiaTrack
        { maniaNotes = fmap (fmap $ first fromEnum) dd.danceNotes
        , maniaOverdrive = dt.danceOverdrive
        }
  return (danceDifficultyName diff, mt)

maniaToDance :: (NNC.C t) => [(SMDifficulty, T.Text)] -> Map.Map T.Text (ManiaTrack t) -> DanceTrack t
maniaToDance diffMapping trks = DanceTrack
  { danceDifficulties = Map.fromList $ do
    (smdiff, maniaDiff) <- diffMapping
    maniaTrack <- toList $ Map.lookup maniaDiff trks
    let toArrows = mapM $ \(k, noteType) -> do
          guard $ k <= 3
          Just (toEnum k, noteType)
    return (smdiff, DanceDifficulty $ RTB.mapMaybe toArrows maniaTrack.maniaNotes)
  , danceOverdrive = fromMaybe RTB.empty $ listToMaybe $ do
    (_, maniaDiff) <- diffMapping
    maniaTrack <- toList $ Map.lookup maniaDiff trks
    filter (not . RTB.null) [maniaTrack.maniaOverdrive]
  }
