{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}
module RockBand.Codec.ProKeys where

import           Control.Monad                    ((>=>))
import           Control.Monad.Codec
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Text                        as T
import           RockBand.Codec
import           RockBand.Common
import           RockBand.ProKeys                 (LaneRange (..), Pitch (..))

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
  } deriving (Eq, Ord, Show)

instance ParseTrack ProKeysTrack where
  parseTrack = do
    pkLanes     <- (pkLanes    =.) $ condenseMap_ $ eachKey each $ blip . \case
      RangeC -> 0
      RangeD -> 2
      RangeE -> 4
      RangeF -> 5
      RangeG -> 7
      RangeA -> 9
    pkTrainer   <- pkTrainer   =. let
      parse = readCommand' >=> \case (t, k) | k == T.pack "key" -> Just t; _ -> Nothing
      unparse t = showCommand' (t, T.pack "key")
      in single parse unparse
    pkMood      <- pkMood      =. command
    pkSolo      <- pkSolo      =. edges 103
    pkGlissando <- pkGlissando =. edges 126
    pkTrill     <- pkTrill     =. edges 127
    pkOverdrive <- pkOverdrive =. edges 116
    pkBRE       <- pkBRE       =. edgesBRE [120 .. 124]
    pkNotes     <- (pkNotes    =.) $ blipSustainRB $ condenseMap $ eachKey each
      $ \k -> matchEdges $ edges $ fromEnum k + 48
    return ProKeysTrack{..}
