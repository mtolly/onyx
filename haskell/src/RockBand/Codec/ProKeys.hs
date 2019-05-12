{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module RockBand.Codec.ProKeys where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           MergeMonoid
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = RangeC | RangeD | RangeE | RangeF | RangeG | RangeA
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Pitch = RedYellow Key | BlueGreen Key | OrangeC
  deriving (Eq, Ord, Show, Read)

instance Enum Pitch where
  fromEnum (RedYellow k) = fromEnum k
  fromEnum (BlueGreen k) = fromEnum k + 12
  fromEnum OrangeC       = 24

  toEnum i = case divMod i 12 of
    (0, j) -> RedYellow $ toEnum j
    (1, j) -> BlueGreen $ toEnum j
    (2, 0) -> OrangeC
    _      -> error $ "No pro keys Pitch for: fromEnum " ++ show i

  succ (RedYellow B) = BlueGreen C
  succ (RedYellow k) = RedYellow $ succ k
  succ (BlueGreen B) = OrangeC
  succ (BlueGreen k) = BlueGreen $ succ k
  succ OrangeC       = error "no succ for pro keys Pitch OrangeC"

  pred (RedYellow C) = error "no pred for pro keys Pitch: RedYellow C"
  pred (RedYellow k) = RedYellow $ pred k
  pred (BlueGreen C) = RedYellow B
  pred (BlueGreen k) = BlueGreen $ pred k
  pred OrangeC       = BlueGreen B

instance Bounded Pitch where
  minBound = RedYellow minBound
  maxBound = OrangeC

data ProKeysTrack t = ProKeysTrack
  { pkLanes     :: RTB.T t LaneRange
  , pkTrainer   :: RTB.T t Trainer
  , pkMood      :: RTB.T t Mood
  , pkSolo      :: RTB.T t Bool
  , pkGlissando :: RTB.T t Bool
  , pkTrill     :: RTB.T t Bool
  , pkOverdrive :: RTB.T t Bool
  , pkBRE       :: RTB.T t Bool
  , pkNotes     :: RTB.T t (Pitch, Maybe t)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProKeysTrack t)

nullPK :: ProKeysTrack t -> Bool
nullPK = RTB.null . pkNotes

instance TraverseTrack ProKeysTrack where
  traverseTrack fn (ProKeysTrack a b c d e f g h i) = ProKeysTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> traverseBlipSustain fn i

instance ParseTrack ProKeysTrack where
  parseTrack = do
    pkTrainer   <- pkTrainer   =. let
      parse = readCommand' >=> \case (t, k) | k == T.pack "key" -> Just t; _ -> Nothing
      unparse t = showCommand' (t, T.pack "key")
      in single parse unparse
    pkMood      <- pkMood      =. command
    pkGlissando <- pkGlissando =. edges 126
    pkTrill     <- pkTrill     =. edges 127
    pkOverdrive <- pkOverdrive =. edges 116
    pkSolo      <- pkSolo      =. edges 115
    pkBRE       <- pkBRE       =. edges 120
    pkNotes     <- (pkNotes    =.) $ fatBlips (1/8) $ blipSustainRB $ condenseMap $ eachKey each
      $ \k -> matchEdges $ edges $ fromEnum k + 48
    pkLanes     <- (pkLanes    =.) $ statusBlips $ condenseMap_ $ eachKey each $ blip . \case
      RangeC -> 0
      RangeD -> 2
      RangeE -> 4
      RangeF -> 5
      RangeG -> 7
      RangeA -> 9
    return ProKeysTrack{..}

-- | Moves the first range event to time zero, to work around a Phase Shift bug.
fixPSRange :: (NNC.C t) => ProKeysTrack t -> ProKeysTrack t
fixPSRange trk = let
  fix ranges = case RTB.viewL ranges of
    Just ((t1, range1), ranges') -> case RTB.viewL ranges' of
      Just ((t2, range2), ranges'') ->
        RTB.cons NNC.zero range1 $ RTB.cons (NNC.add t1 t2) range2 ranges''
      Nothing -> RTB.singleton NNC.zero range1
    Nothing -> RTB.empty
  in trk { pkLanes = fix $ pkLanes trk }
