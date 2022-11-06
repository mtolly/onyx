{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.MelodysEscape where

import           Control.Monad.Codec
import           Control.Monad.Random
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (intercalate)
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common                 (Edge, each, edgeBlips,
                                                   joinEdges,
                                                   minSustainLengthRB,
                                                   splitEdges)
import           Onyx.MIDI.Read
import qualified Sound.MIDI.Util                  as U

data MelodyTrack t = MelodyTrack
  { melodyIntensity :: RTB.T t (Intensity, Bool)
  , melodyNotes     :: RTB.T t (Edge () NoteType)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (MelodyTrack t)

instance TraverseTrack MelodyTrack where
  traverseTrack fn (MelodyTrack a b) = MelodyTrack <$> fn a <*> fn b

data NoteType
  = Obstacle Direction
  | Color    Direction
  | Cutscene
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via GenericFullEnum NoteType

data Intensity = Low | Neutral | High | ExtraHigh
  deriving (Eq, Ord, Show, Enum, Bounded)

data Direction = U | R | L | D
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ParseTrack MelodyTrack where
  parseTrack = do
    melodyNotes <- (melodyNotes =.) $ fatBlips (1/8) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
      Obstacle D -> 60
      Obstacle L -> 61
      Obstacle R -> 62
      Obstacle U -> 63
      Cutscene   -> 64
      Color D    -> 72
      Color L    -> 73
      Color R    -> 74
      Color U    -> 75
    melodyIntensity <- (melodyIntensity =.) $ condenseMap $ eachKey each $ edges . \case
      Low       -> 84
      Neutral   -> 85
      High      -> 86
      ExtraHigh -> 87
    return MelodyTrack{..}

secondsToTicks :: U.Seconds -> Int
secondsToTicks = ceiling . (* (44100/1024))

intensityCharacter :: Intensity -> Char
intensityCharacter = \case
  Low       -> 'L'
  Neutral   -> 'N'
  High      -> 'H'
  ExtraHigh -> 'E'

noteTypeCharacter :: NoteType -> Char
noteTypeCharacter = \case
  Obstacle D -> 'D'
  Obstacle L -> 'L'
  Obstacle R -> 'R'
  Obstacle U -> 'U'
  Cutscene   -> 'A'
  Color    D -> '2'
  Color    L -> '4'
  Color    R -> '6'
  Color    U -> '8'

writeTransitions :: U.TempoMap -> MelodyTrack U.Beats -> String
writeTransitions tmap = let
  writePair ((t1, i1), (t2, i2)) = concat
    [ [intensityCharacter i1]
    , ":"
    , show (max 1 $ secondsToTicks t1)
    , "-"
    , [intensityCharacter i2]
    , ":"
    , show (max 1 $ secondsToTicks t2)
    ]
  makePairs ((t1, (i1, False)) : (t2, (i2, True)) : xs) =
    if i1 == i2
      then makePairs xs
      else ((t1, i1), (t2, i2)) : makePairs xs
  makePairs [(_, (_, False))] = []
  makePairs xs = error $ "MelodysEscape.writeTransitions: invalid events: " ++ show xs
  in intercalate ";" . map writePair . makePairs . ((0, (Low, False)) :)
    . ATB.toPairList . RTB.toAbsoluteEventList 0
    . U.applyTempoTrack tmap . melodyIntensity

writeNotes :: U.TempoMap -> MelodyTrack U.Beats -> String
writeNotes tmap = let
  writePair (t, ((), nt, mlen)) = concat
    [ show $ secondsToTicks t
    , ":"
    , [noteTypeCharacter nt]
    , case mlen of
      Nothing  -> ""
      Just len -> '-' : show (secondsToTicks len)
    ]
  in intercalate ";" . map writePair . ATB.toPairList . RTB.toAbsoluteEventList 0
    . joinEdges . U.applyTempoTrack tmap . splitEdges
    . edgeBlips minSustainLengthRB . melodyNotes

-- | Any time 2 or more notes appear, randomly delete all but one of them.
randomNotes :: (NNC.C t, MonadRandom m) => MelodyTrack t -> m (MelodyTrack t)
randomNotes mel = do
  notes' <- traverse uniform $ RTB.collectCoincident $ melodyNotes mel
  return mel{ melodyNotes = notes' }

randomNotesIO :: (NNC.C t) => MelodyTrack t -> IO (MelodyTrack t)
randomNotesIO = evalRandIO . randomNotes
