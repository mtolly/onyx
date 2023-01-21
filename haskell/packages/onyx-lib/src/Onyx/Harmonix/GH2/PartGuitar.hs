{- |
PART GUITAR, PART GUITAR COOP, PART BASS, PART RHYTHM
-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.Harmonix.GH2.PartGuitar where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import           Onyx.MIDI.Track.FiveFret         (Color (..),
                                                   FretPosition (..))
import qualified Sound.MIDI.Util                  as U

data HandMap
  = HandMap_Default
  | HandMap_DropD2
  | HandMap_Solo
  | HandMap_NoChords
  | HandMap_AllChords -- seen in GH1
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

data StrumMap
  = StrumMap_SlapBass
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

data Tempo
  = HalfTempo
  | NormalTempo
  | DoubleTempo
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Command Tempo where
  fromCommand = \case
    HalfTempo   -> ["half_tempo"]
    NormalTempo -> ["normal_tempo"]
    DoubleTempo -> ["double_tempo"]
  toCommand = reverseLookup each fromCommand

data PartTrack t = PartTrack
  { partDifficulties :: Map.Map Difficulty (PartDifficulty t)
  , partFretPosition :: RTB.T t (FretPosition, Bool)
  , partIdle         :: RTB.T t ()
  , partPlay         :: RTB.T t ()
  , partHandMap      :: RTB.T t HandMap
  , partStrumMap     :: RTB.T t StrumMap
  , partWail         :: RTB.T t Bool -- ^ headbang or similar. probably like [intense] in RB
  , partSolo         :: RTB.T t Bool -- ^ fire hands, special animations
  , partOwFace       :: RTB.T t Bool
  , partTempo        :: RTB.T t Tempo
  , partUnknown110   :: RTB.T t Bool
  -- ^ from mariteaux: note 110 on part guitar will cause a "disappointment"
  -- sound effect to play if the player misses any and all notes during its duration

  -- following is GH2DX added. note, encoding is blips for start/end
  -- instead of a long note, due to easier scripting implementation
  , partSoloEdge     :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (PartTrack t)

instance TraverseTrack PartTrack where
  traverseTrack fn (PartTrack a b c d e f g h i j k l) = PartTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k
    <*> fn l

nullPart :: PartTrack t -> Bool
nullPart = all (RTB.null . partGems) . toList . partDifficulties

data PartDifficulty t = PartDifficulty
  { partStarPower  :: RTB.T t Bool
  , partPlayer1    :: RTB.T t Bool
  , partPlayer2    :: RTB.T t Bool
  , partGems       :: RTB.T t (Edge () Color)
  -- following are added for future GH2DX compatibility
  , partForceHOPO  :: RTB.T t Bool
  , partForceStrum :: RTB.T t Bool
  , partForceTap   :: RTB.T t Bool
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (PartDifficulty t)

instance TraverseTrack PartDifficulty where
  traverseTrack fn (PartDifficulty a b c d e f g) = PartDifficulty
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g

instance ParseTrack PartTrack where
  parseTrack = do
    -- TODO
    -- 38 and 39 seen in Institutionalized, Jessica, John the Fisherman
    -- Strutter has 119 on PART RHYTHM and BAND BASS
    partHandMap  <- partHandMap  =. command
    partStrumMap <- partStrumMap =. command
    partTempo    <- partTempo    =. command
    partIdle     <- partIdle     =. commandMatch ["idle"]
    partPlay     <- partPlay     =. commandMatch ["play"]
    partFretPosition <- (partFretPosition =.) $ condenseMap $ eachKey each
      $ \posn -> edges $ fromEnum posn + 40
    partWail <- (partWail =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      True  -> ["wail_on"]
      False -> ["wail_off"]
    partSolo <- (partSolo =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      True  -> ["solo_on"]
      False -> ["solo_off"]
    partOwFace <- (partOwFace =.) $ condenseMap_ $ eachKey each $ commandMatch . \case
      True  -> ["ow_face_on"]
      False -> ["ow_face_off"]
    partUnknown110 <- partUnknown110 =. edges 110
    partDifficulties <- (partDifficulties =.) $ eachKey each parseDifficulty
    -- GH2DX added. pitch chosen to match RB3 protar/prokeys
    partSoloEdge <- partSoloEdge =. blip 115
    return PartTrack{..}

parseDifficulty :: (Monad m) => Difficulty -> TrackCodec m U.Beats (PartDifficulty U.Beats)
parseDifficulty diff = do
  let base = case diff of
        Easy   -> 60
        Medium -> 72
        Hard   -> 84
        Expert -> 96
  partStarPower <- partStarPower =. edges (base + 7)
  partPlayer1   <- partPlayer1   =. edges (base + 9)
  partPlayer2   <- partPlayer2   =. edges (base + 10)
  partGems      <- (partGems =.) $ fatBlips (1/8) $ translateEdges $ condenseMap $ eachKey each $ edges . \case
    Green  -> base + 0
    Red    -> base + 1
    Yellow -> base + 2
    Blue   -> base + 3
    Orange -> base + 4
  -- tentative format for future GH2DX support.
  -- hopo/strum like RB; taps like CH's newer non-sysex format (but separated per difficulty)
  partForceHOPO  <- partForceHOPO  =. edges (base + 5)
  partForceStrum <- partForceStrum =. edges (base + 6)
  partForceTap   <- partForceTap   =. edges (base + 8)
  return PartDifficulty{..}
