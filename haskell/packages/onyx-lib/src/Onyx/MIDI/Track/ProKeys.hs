{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
module Onyx.MIDI.Track.ProKeys where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import qualified Numeric.NonNegative.Class        as NNC
import           Onyx.DeriveHelpers
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read
import qualified Sound.MIDI.Util                  as U

-- | There are six playable ranges, each of which covers 10 white keys, plus
-- all the black keys within. They are named here according to their lowest key.
data LaneRange = RangeC | RangeD | RangeE | RangeF | RangeG | RangeA
  deriving (Eq, Ord, Show, Enum, Bounded)

data Pitch = RedYellow Key | BlueGreen Key | OrangeC
  deriving (Eq, Ord, Show, Generic)
  deriving (Enum, Bounded) via GenericFullEnum Pitch

data ProKeysTrack t = ProKeysTrack
  { pkLanes     :: RTB.T t LaneRange
  , pkTrainer   :: RTB.T t Trainer
  , pkMood      :: RTB.T t Mood
  , pkSolo      :: RTB.T t Bool
  , pkGlissando :: RTB.T t Bool
  , pkTrill     :: RTB.T t Bool
  , pkOverdrive :: RTB.T t Bool
  , pkBRE       :: RTB.T t Bool
  , pkNotes     :: RTB.T t (Edge () Pitch)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProKeysTrack t)

nullPK :: ProKeysTrack t -> Bool
nullPK = RTB.null . pkNotes

instance ChopTrack ProKeysTrack where
  chopTake t pk = ProKeysTrack
    { pkLanes     = U.trackTake  t $ pkLanes     pk
    , pkTrainer   = U.trackTake  t $ pkTrainer   pk
    , pkMood      = U.trackTake  t $ pkMood      pk
    , pkSolo      = chopTakeBool t $ pkSolo      pk
    , pkGlissando = chopTakeBool t $ pkGlissando pk
    , pkTrill     = chopTakeBool t $ pkTrill     pk
    , pkOverdrive = chopTakeBool t $ pkOverdrive pk
    , pkBRE       = chopTakeBool t $ pkBRE       pk
    , pkNotes     = chopTakeEdge t $ pkNotes     pk
    }
  chopDrop t pk = ProKeysTrack
    { pkLanes     = chopDropStatus t $ pkLanes     pk
    , pkTrainer   = U.trackDrop    t $ pkTrainer   pk
    , pkMood      = chopDropStatus t $ pkMood      pk
    , pkSolo      = chopDropBool   t $ pkSolo      pk
    , pkGlissando = chopDropBool   t $ pkGlissando pk
    , pkTrill     = chopDropBool   t $ pkTrill     pk
    , pkOverdrive = chopDropBool   t $ pkOverdrive pk
    , pkBRE       = chopDropBool   t $ pkBRE       pk
    , pkNotes     = chopDropEdge   t $ pkNotes     pk
    }

instance TraverseTrack ProKeysTrack where
  traverseTrack fn (ProKeysTrack a b c d e f g h i) = ProKeysTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i

instance ParseTrack ProKeysTrack where
  parseTrack = do
    pkTrainer   <- pkTrainer   =. let
      parse = toCommand >=> \case (t, k) | k == T.pack "key" -> Just t; _ -> Nothing
      unparse t = fromCommand (t, T.pack "key")
      in commandMatch' parse unparse
    pkMood      <- pkMood      =. command
    pkGlissando <- pkGlissando =. edges 126
    pkTrill     <- pkTrill     =. edges 127
    pkOverdrive <- pkOverdrive =. edges 116
    pkSolo      <- pkSolo      =. edges 115
    pkBRE       <- pkBRE       =. edges 120
    chordSnap [48 .. 72]
    pkNotes     <- (pkNotes    =.) $ fatBlips (1/8) $ translateEdges
      $ condenseMap $ eachKey each $ \k -> edges $ fromEnum k + 48
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
