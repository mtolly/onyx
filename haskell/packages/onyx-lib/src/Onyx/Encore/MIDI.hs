{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Encore.MIDI where

import           Control.Monad                    (guard)
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.Guitar                      (applyStatus)
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.Drums            (Animation,
                                                   parseDrumAnimation)
import           Onyx.MIDI.Track.FiveFret         (Color (..), FretPosition,
                                                   HandMap, StrumMap)
import           Onyx.MIDI.Track.Mania            (ManiaTrack (..))
import           Onyx.PhaseShift.Dance            (NoteType (..))
import           Onyx.Sections                    (Section, addSectionNumbers,
                                                   simpleSection)
import qualified Sound.MIDI.Util                  as U

data EncorePart t = EncorePart
  { difficulties :: Map.Map Difficulty (EncoreDifficulty t)
  , overdrive    :: RTB.T t Bool
  , solo         :: RTB.T t Bool -- Encore
  , mood         :: RTB.T t Mood
  , instrument   :: RTB.T t CharInstrument
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EncorePart t)

instance TraverseTrack EncorePart where
  traverseTrack fn (EncorePart a b c d e) = EncorePart
    <$> traverse (traverseTrack fn) a
    <*> fn b
    <*> fn c
    <*> fn d
    <*> fn e

data EncoreDifficulty t = EncoreDifficulty
  { gems  :: RTB.T t (Edge () Color)
  , lifts :: RTB.T t (Edge () Color)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EncoreDifficulty t)

instance TraverseTrack EncoreDifficulty where
  traverseTrack fn (EncoreDifficulty a b) = EncoreDifficulty <$> fn a <*> fn b

nullEncore :: EncorePart t -> Bool
nullEncore = all (RTB.null . (.gems)) . toList . (.difficulties)

data CharInstrument = CharGuitar | CharBass | CharKeytar
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command CharInstrument where
  fromCommand = \case
    CharGuitar -> ["guitar"]
    CharBass   -> ["bass"  ]
    CharKeytar -> ["keytar"]
  toCommand = reverseLookup each fromCommand

instance ParseTrack EncorePart where
  parseTrack = do
    difficulties <- (=.) (.difficulties) $ eachKey each \diff -> fatBlips (1/8) do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      gems <- (.gems) =. do
        translateEdges $ condenseMap $ eachKey each $ edges . \case
          Green  -> base + 0
          Red    -> base + 1
          Yellow -> base + 2
          Blue   -> base + 3
          Orange -> base + 4
      lifts <- (.lifts) =. do
        translateEdges $ condenseMap $ eachKey each $ edges . \case
          Green  -> base + 6
          Red    -> base + 7
          Yellow -> base + 8
          Blue   -> base + 9
          Orange -> base + 10
      return EncoreDifficulty{..}
    solo       <- (.solo      ) =. edges 101
    overdrive  <- (.overdrive ) =. edges 116
    mood       <- (.mood      ) =. command
    instrument <- (.instrument) =. command
    return EncorePart{..}

data EncoreGuitar t = EncoreGuitar
  { part         :: EncorePart t
  , handMap      :: RTB.T t HandMap
  , strumMap     :: RTB.T t StrumMap
  , fretPosition :: RTB.T t (FretPosition, Bool)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EncoreGuitar t)

instance TraverseTrack EncoreGuitar where
  traverseTrack fn (EncoreGuitar a b c d) = EncoreGuitar <$> traverseTrack fn a <*> fn b <*> fn c <*> fn d

instance ParseTrack EncoreGuitar where
  parseTrack = do
    part         <- (.part        ) =. parseTrack
    handMap      <- (.handMap     ) =. command
    strumMap     <- (.strumMap    ) =. command
    fretPosition <- (.fretPosition) =. do
      condenseMap $ eachKey each \posn -> edges $ fromEnum posn + 40
    return EncoreGuitar{..}

data EncoreDrums t = EncoreDrums
  { part      :: EncorePart t
  , animation :: RTB.T t Animation
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (EncoreDrums t)

instance TraverseTrack EncoreDrums where
  traverseTrack fn (EncoreDrums a b) = EncoreDrums <$> traverseTrack fn a <*> fn b

instance ParseTrack EncoreDrums where
  parseTrack = do
    part      <- (.part     ) =. parseTrack
    animation <- (.animation) =. parseDrumAnimation
    return EncoreDrums{..}

newtype SectionTrack t = SectionTrack
  { events :: RTB.T t T.Text
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (SectionTrack t)

{-
seen events:

[intro]
[verse]
[prechorus]
[chorus]
[bridge]
[build]
[drop]
[breakdown]
[outro]
[solo_guitar]
[solo_vocals]
[solo_keys]
[solo_drums]
[solo_bass]
-}

instance TraverseTrack SectionTrack where
  traverseTrack fn (SectionTrack a) = SectionTrack <$> fn a

instance ParseTrack SectionTrack where
  parseTrack = do
    events <- (.events) =. commandMatch'
      (\case [x] -> Just x; _ -> Nothing)
      return
    return SectionTrack{..}

encorePart :: EncorePart U.Beats -> [(T.Text, ManiaTrack U.Beats)]
encorePart fp = do
  diff <- [Easy, Medium, Hard, Expert]
  let name = T.toLower $ T.pack $ show diff
  fd <- toList $ Map.lookup diff fp.difficulties
  let mania = encoreToMania fd
  guard $ not $ RTB.null mania
  return (name, ManiaTrack
    { notes     = mania
    , overdrive = fp.overdrive
    })

encoreToMania :: EncoreDifficulty U.Beats -> RTB.T U.Beats (Edge () (Int, NoteType))
encoreToMania fd = let
  base = joinEdgesSimple fd.gems
  liftStatus = fmap
    (\case
      EdgeOn () color -> (color, True )
      EdgeOff   color -> (color, False)
    ) fd.lifts
  in splitEdgesSimple $ fmap
    (\(liftsHere, ((), color, len)) -> let
      noteType = if elem color liftsHere then NoteLift else NoteNormal
      in ((), (fromEnum color, noteType), len)
    ) (applyStatus liftStatus base)

convertEncoreSections :: SectionTrack t -> RTB.T t Section
convertEncoreSections sectionTrack = fmap
  (\(name, num) -> let
    name' = case name of
      "solo_guitar" -> "gtr_solo"
      "solo_vocals" -> "vocal_solo"
      "solo_keys"   -> "keyboard_solo"
      "solo_drums"  -> "drum_solo"
      "solo_bass"   -> "bass_solo"
      _             -> name
    in simpleSection case num of
      Nothing -> name'
      Just n  -> T.unwords [name', T.pack $ show n]
  ) (addSectionNumbers sectionTrack.events)
