{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards     #-}
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
  { lanes     :: RTB.T t LaneRange
  , trainer   :: RTB.T t Trainer
  , mood      :: RTB.T t Mood
  , solo      :: RTB.T t Bool
  , glissando :: RTB.T t Bool
  , trill     :: RTB.T t Bool
  , overdrive :: RTB.T t Bool
  , bre       :: RTB.T t Bool
  , notes     :: RTB.T t (Edge () Pitch)
  } deriving (Eq, Ord, Show, Generic)
    deriving (Semigroup, Monoid, Mergeable) via GenericMerge (ProKeysTrack t)

nullPK :: ProKeysTrack t -> Bool
nullPK = RTB.null . (.notes)

instance ChopTrack ProKeysTrack where
  chopTake t pk = ProKeysTrack
    { lanes     = U.trackTake  t pk.lanes
    , trainer   = U.trackTake  t pk.trainer
    , mood      = U.trackTake  t pk.mood
    , solo      = chopTakeBool t pk.solo
    , glissando = chopTakeBool t pk.glissando
    , trill     = chopTakeBool t pk.trill
    , overdrive = chopTakeBool t pk.overdrive
    , bre       = chopTakeBool t pk.bre
    , notes     = chopTakeEdge t pk.notes
    }
  chopDrop t pk = ProKeysTrack
    { lanes     = chopDropStatus t pk.lanes
    , trainer   = U.trackDrop    t pk.trainer
    , mood      = chopDropStatus t pk.mood
    , solo      = chopDropBool   t pk.solo
    , glissando = chopDropBool   t pk.glissando
    , trill     = chopDropBool   t pk.trill
    , overdrive = chopDropBool   t pk.overdrive
    , bre       = chopDropBool   t pk.bre
    , notes     = chopDropEdge   t pk.notes
    }

instance TraverseTrack ProKeysTrack where
  traverseTrack fn (ProKeysTrack a b c d e f g h i) = ProKeysTrack
    <$> fn a <*> fn b <*> fn c <*> fn d <*> fn e
    <*> fn f <*> fn g <*> fn h <*> fn i

instance ParseTrack ProKeysTrack where
  parseTrack = do
    trainer   <- (.trainer  ) =. let
      parse = toCommand >=> \case (t, k) | k == T.pack "key" -> Just t; _ -> Nothing
      unparse :: Trainer -> [T.Text]
      unparse t = fromCommand (t, T.pack "key")
      in commandMatch' parse unparse
    mood      <- (.mood     ) =. command
    glissando <- (.glissando) =. edges 126
    trill     <- (.trill    ) =. edges 127
    overdrive <- (.overdrive) =. edges 116
    solo      <- (.solo     ) =. edges 115
    bre       <- (.bre      ) =. edges 120
    chordSnap [48 .. 72]
    notes     <- (.notes    ) =. do
      fatBlips (1/8) $ translateEdges $ condenseMap $ eachKey each \k -> edges $ fromEnum k + 48
    lanes     <- (.lanes    ) =. do
      statusBlips $ condenseMap_ $ eachKey each $ blip . \case
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
  in trk { lanes = fix trk.lanes }
