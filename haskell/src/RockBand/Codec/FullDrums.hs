{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.FullDrums where

import           Control.Monad.Codec
import           Control.Monad.Trans.Writer       (execWriter, tell)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Profunctor                  (dimap)
import qualified Data.Text                        as T
import           DeriveHelpers
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Codec.Drums             (Hand (..))
import           RockBand.Common

data FullDrumTrack t = FullDrumTrack
  { fdDifficulties :: Map.Map Difficulty (FullDrumDifficulty t)
  , fdKick2        :: RTB.T t () -- TODO (gem type ?) and velocity
  , fdLanes        :: RTB.T t (Edge () FullGem)
  , fdOverdrive    :: RTB.T t Bool
  , fdActivation   :: RTB.T t Bool
  , fdSolo         :: RTB.T t Bool
  , fdSticking     :: RTB.T t Hand
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FullDrumTrack t)

instance TraverseTrack FullDrumTrack where
  traverseTrack fn (FullDrumTrack a b c d e f g) = FullDrumTrack
    <$> traverse (traverseTrack fn) a
    <*> fn b <*> fn c <*> fn d <*> fn e <*> fn f <*> fn g

data FullDrumDifficulty t = FullDrumDifficulty
  { fdGems :: RTB.T t (FullGem, FullGemType, FullVelocity)
  , fdFlam :: RTB.T t ()
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (FullDrumDifficulty t)

instance TraverseTrack FullDrumDifficulty where
  traverseTrack fn (FullDrumDifficulty a b) = FullDrumDifficulty <$> fn a <*> fn b

data FullGem
  = Kick
  | Snare
  | Hihat
  | HihatFoot
  | CrashL
  | Tom1
  | Tom2
  | Tom3
  | CrashR
  | Ride
  deriving (Eq, Ord, Show, Enum, Bounded)

data FullGemType
  = GemNormal
  | GemHihatOpen
  | GemHihatClosed
  | GemCymbalChoke
  | GemRim
  deriving (Eq, Ord, Show, Enum, Bounded)

data FullVelocity
  = VelocityGhost
  | VelocityNormal
  | VelocityAccent
  deriving (Eq, Ord, Show, Enum, Bounded)

instance ParseTrack FullDrumTrack where
  parseTrack = do
    fdSolo <- fdSolo =. edges 115
    fdOverdrive <- fdOverdrive =. edges 116
    fdActivation <- fdActivation =. edges 120
    fdLanes <- (fdLanes =.) $ translateEdges
      $ condenseMap $ eachKey each $ \drum -> case drum of
        Kick -> edges $ 84 + 0
        Snare -> edges $ 84 + 1
        Hihat -> edges $ 84 + 2
        HihatFoot -> return mempty
        CrashL -> edges $ 84 + 3
        Tom1 -> edges $ 84 + 4
        Tom2 -> edges $ 84 + 5
        Tom3 -> edges $ 84 + 6
        CrashR -> edges $ 84 + 7
        Ride -> edges $ 84 + 8
    fdSticking <- (fdSticking =.) $ condenseMap_ $ eachKey each $ blip . \case
      RH -> 109
      LH -> 108
    fdDifficulties <- (fdDifficulties =.) $ eachKey each $ \diff -> fatBlips (1/8) $ do
      let base = case diff of
            Easy   -> 24
            Medium -> 48
            Hard   -> 72
            Expert -> 96
          decodeCV (drum, (c, v)) = let
            gtype = case c of
              1 -> GemHihatOpen
              2 -> GemHihatClosed
              3 -> GemCymbalChoke
              4 -> GemRim
              _ -> GemNormal
            vel = case v of
              1   -> VelocityGhost
              127 -> VelocityAccent
              _   -> VelocityNormal
            in (drum, gtype, vel)
          encodeCV (drum, gtype, vel) = let
            c = case gtype of
              GemNormal      -> 0
              GemHihatOpen   -> 1
              GemHihatClosed -> 2
              GemCymbalChoke -> 3
              GemRim         -> 4
            v = case vel of
              VelocityGhost  -> 1
              VelocityNormal -> 96
              VelocityAccent -> 127
            in (drum, (c, v))
      fdGems <- fdGems =. do
        dimap (fmap encodeCV) (fmap decodeCV) $ condenseMap $ eachKey each $ \drum -> do
          blipCV $ base + case drum of
            HihatFoot -> -2
            Kick      -> 0
            Snare     -> 1
            Hihat     -> 2
            CrashL    -> 3
            Tom1      -> 4
            Tom2      -> 5
            Tom3      -> 6
            CrashR    -> 7
            Ride      -> 8
      fdFlam <- fdFlam =. blip (base + 10)
      return FullDrumDifficulty{..}
    fdKick2 <- fdKick2 =. fatBlips (1/8) (blip 95)
    return FullDrumTrack{..}

fullDrumNoteNames :: [(Int, T.Text)]
fullDrumNoteNames = execWriter $ do
  o 120 "DRUM FILL"
  o 116 "OVERDRIVE"
  o 115 "SOLO"
  x 114
  o 109 "RIGHT HAND"
  o 108 "LEFT HAND"
  let difficulty letter base = do
        x (base + 11)
        o (base + 10) $ letter <> " Flam"
        x (base + 9 )
        o (base + 8 ) $ letter <> " Ride"
        o (base + 7 ) $ letter <> " Crash R"
        o (base + 6 ) $ letter <> " Tom 3"
        o (base + 5 ) $ letter <> " Tom 2"
        o (base + 4 ) $ letter <> " Tom 1"
        o (base + 3 ) $ letter <> " Crash L"
        o (base + 2 ) $ letter <> " Hihat"
        o (base + 1 ) $ letter <> " Snare"
        o base        $ letter <> " Kick"
        o (base - 2 ) $ letter <> " Hihat Pedal"
  difficulty "X" 96
  o 95 "X+ Kick 2"
  x (84 + 9)
  o (84 + 8) $ "Lane Ride"
  o (84 + 7) $ "Lane Crash R"
  o (84 + 6) $ "Lane Tom 3"
  o (84 + 5) $ "Lane Tom 2"
  o (84 + 4) $ "Lane Tom 1"
  o (84 + 3) $ "Lane Crash L"
  o (84 + 2) $ "Lane Hihat"
  o (84 + 1) $ "Lane Snare"
  o 84       $ "Lane Kick"
  difficulty "H" 72
  difficulty "M" 48
  difficulty "E" 24
  where o k v = tell [(k, v)]
        x k = tell [(k, "----")]

getDifficulty :: (NNC.C t) => Maybe Difficulty -> FullDrumTrack t -> RTB.T t (FullGem, FullGemType, FullVelocity)
getDifficulty diff trk = let
  base = fdGems $ fromMaybe mempty $ Map.lookup (fromMaybe Expert diff) $ fdDifficulties trk
  in case diff of
    Nothing -> RTB.merge base $ fmap (\() -> (Kick, GemNormal, VelocityNormal)) $ fdKick2 trk
    _       -> base
