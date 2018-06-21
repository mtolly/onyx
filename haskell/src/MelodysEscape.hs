{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module MelodysEscape where

import           Control.Monad.Codec
import           Control.Monad.Random
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (elemIndex, intercalate)
import           Data.Maybe                       (fromJust)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common                  (each)
import qualified Sound.MIDI.Util                  as U

data MelodyTrack t = MelodyTrack
  { melodyIntensity :: RTB.T t (Intensity, Bool)
  , melodyNotes     :: RTB.T t (NoteType, Maybe t)
  } deriving (Eq, Ord, Show)

instance (NNC.C t) => Semigroup (MelodyTrack t) where
  (<>) (MelodyTrack a1 a2) (MelodyTrack b1 b2) = MelodyTrack
    (RTB.merge a1 b1)
    (RTB.merge a2 b2)

instance (NNC.C t) => Monoid (MelodyTrack t) where
  mempty = MelodyTrack RTB.empty RTB.empty

instance TraverseTrack MelodyTrack where
  traverseTrack fn (MelodyTrack a b) = MelodyTrack <$> fn a <*> traverseBlipSustain fn b

data NoteType
  = Obstacle Direction
  | Color    Direction
  | Cutscene
  deriving (Eq, Ord, Show, Read)

allNoteTypes :: [NoteType]
allNoteTypes = concat
  [ [Obstacle] <*> each
  , [Color] <*> each
  , [Cutscene]
  ]

instance Enum NoteType where
  toEnum = (allNoteTypes !!)
  fromEnum x = fromJust $ elemIndex x allNoteTypes

instance Bounded NoteType where
  minBound = head allNoteTypes
  maxBound = last allNoteTypes

data Intensity = Low | Neutral | High | ExtraHigh
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Direction = U | R | L | D
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance ParseTrack MelodyTrack where
  parseTrack = do
    melodyNotes <- (melodyNotes =.) $ fatBlips (1/8) $ blipSustainRB $ condenseMap $ eachKey each $ matchEdges . edges . \case
      Obstacle D -> 60
      Obstacle L -> 61
      Obstacle R -> 62
      Obstacle U -> 63
      Cutscene -> 64
      Color D -> 72
      Color L -> 73
      Color R -> 74
      Color U -> 75
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

writeTransitions :: MelodyTrack U.Seconds -> String
writeTransitions = let
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
    . ATB.toPairList . RTB.toAbsoluteEventList 0 . melodyIntensity

writeNotes :: MelodyTrack U.Seconds -> String
writeNotes = let
  writePair (t, (nt, mlen)) = concat
    [ show $ secondsToTicks t
    , ":"
    , [noteTypeCharacter nt]
    , case mlen of
      Nothing  -> ""
      Just len -> '-' : show (secondsToTicks len)
    ]
  in intercalate ";" . map writePair . ATB.toPairList
    . RTB.toAbsoluteEventList 0 . melodyNotes

-- | Any time 2 or more notes appear, randomly delete all but one of them.
randomNotes :: (NNC.C t, MonadRandom m) => MelodyTrack t -> m (MelodyTrack t)
randomNotes mel = do
  notes' <- traverse uniform $ RTB.collectCoincident $ melodyNotes mel
  return mel{ melodyNotes = notes' }

randomNotesIO :: (NNC.C t) => MelodyTrack t -> IO (MelodyTrack t)
randomNotesIO = evalRandIO . randomNotes
