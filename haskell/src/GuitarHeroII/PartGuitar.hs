{- |
PART GUITAR, PART GUITAR COOP, PART BASS, PART RHYTHM
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarHeroII.PartGuitar where

import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Five              (Color (..),
                                                   FretPosition (..))
import           RockBand.Common

data HandMap
  = HandMap_Default
  | HandMap_DropD2
  | HandMap_Solo
  | HandMap_NoChords
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command HandMap where
  fromCommand hm = ["map", T.pack $ show hm]
  toCommand = reverseLookup each fromCommand

data StrumMap
  = StrumMap_SlapBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command StrumMap where
  fromCommand sm = ["map", T.pack $ show sm]
  toCommand = reverseLookup each fromCommand

data Tempo
  = HalfTempo
  | NormalTempo
  | DoubleTempo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
  , partUnknown110   :: RTB.T t Bool -- ^ some kind of sustain thing?
  } deriving (Eq, Ord, Show)

instance TraverseTrack PartTrack where
  traverseTrack fn (PartTrack a b c d e f g h i j k) = PartTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f
    <*> fn g <*> fn h <*> fn i <*> fn j <*> fn k

instance (NNC.C t) => Monoid (PartTrack t) where
  mempty = PartTrack Map.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
    RTB.empty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (PartTrack a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11)
    (PartTrack b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11)
    = PartTrack
      (Map.unionWith mappend a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)
      (RTB.merge a5 b5)
      (RTB.merge a6 b6)
      (RTB.merge a7 b7)
      (RTB.merge a8 b8)
      (RTB.merge a9 b9)
      (RTB.merge a10 b10)
      (RTB.merge a11 b11)

data PartDifficulty t = PartDifficulty
  { partStarPower :: RTB.T t Bool
  , partPlayer1   :: RTB.T t Bool
  , partPlayer2   :: RTB.T t Bool
  , partGems      :: RTB.T t (Color, Maybe t)
  } deriving (Eq, Ord, Show)

instance TraverseTrack PartDifficulty where
  traverseTrack fn (PartDifficulty a b c d) = PartDifficulty
    <$> fn a <*> fn b <*> fn c <*> traverseBlipSustain fn d

instance (NNC.C t) => Monoid (PartDifficulty t) where
  mempty = PartDifficulty RTB.empty RTB.empty RTB.empty RTB.empty
  mappend
    (PartDifficulty a1 a2 a3 a4)
    (PartDifficulty b1 b2 b3 b4)
    = PartDifficulty
      (RTB.merge a1 b1)
      (RTB.merge a2 b2)
      (RTB.merge a3 b3)
      (RTB.merge a4 b4)

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
    partDifficulties <- (partDifficulties =.) $ eachKey each $ \diff -> do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
      partStarPower <- partStarPower =. edges (base + 7)
      partPlayer1   <- partPlayer1   =. edges (base + 9)
      partPlayer2   <- partPlayer2   =. edges (base + 10)
      partGems      <- (partGems =.) $ fatBlips (1/8) $ blipSustainRB $ condenseMap $ eachKey each $ matchEdges . edges . \case
        Green  -> base + 0
        Red    -> base + 1
        Yellow -> base + 2
        Blue   -> base + 3
        Orange -> base + 4
      return PartDifficulty{..}
    return PartTrack{..}
