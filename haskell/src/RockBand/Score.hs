{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE LambdaCase     #-}
module RockBand.Score where

import           Control.Applicative  (liftA2)
import           Control.Monad        (guard)
import qualified RockBand.Codec.Drums as RBDrums
import qualified RockBand.Codec.File  as RBFile
import           RockBand.Common      (Difficulty (..))
import qualified Sound.MIDI.Util      as U

data ScoreTrack
  = ScoreGuitar
  | ScoreBass
  | ScoreDrums
  | ScoreVocals
  | ScoreKeys
  | ScoreProGuitar
  | ScoreProBass
  | ScoreProDrums
  | ScoreProKeys
  | ScoreProGuitar22
  | ScoreProBass22
  deriving (Eq, Show, Enum, Bounded)

data Stars a = Stars
  { stars1    :: a
  , stars2    :: a
  , stars3    :: a
  , stars4    :: a
  , stars5    :: a
  , starsGold :: a
  } deriving (Eq, Show, Functor, Foldable)

instance Applicative Stars where
  pure x = Stars x x x x x x
  Stars f1 f2 f3 f4 f5 fg <*> Stars x1 x2 x3 x4 x5 xg
    = Stars (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5) (fg xg)

new_instrument_thresholds :: ScoreTrack -> Stars Float
new_instrument_thresholds = \case
  ScoreGuitar -> Stars 0.06 0.12 0.2 0.47 0.78 1.15
  ScoreBass -> Stars 0.05 0.1 0.19 0.47 0.78 1.15
  ScoreDrums -> Stars 0.06 0.12 0.2 0.45 0.75 1.09
  ScoreProDrums -> Stars 0.06 0.12 0.2 0.45 0.75 1.09 -- not a separate line in dtb
  ScoreVocals -> Stars 0.05 0.11 0.19 0.46 0.77 1.06
  ScoreKeys -> Stars 0.06 0.12 0.2 0.47 0.78 1.15
  ScoreProGuitar -> Stars 0.06 0.12 0.2 0.47 0.78 1.15
  ScoreProBass -> Stars 0.05 0.1 0.19 0.47 0.78 1.15
  ScoreProGuitar22 -> Stars 0.06 0.12 0.2 0.47 0.78 1.15
  ScoreProBass22 -> Stars 0.05 0.1 0.19 0.47 0.78 1.15
  ScoreProKeys -> Stars 0.06 0.12 0.2 0.47 0.78 1.15

new_bonus_thresholds :: Stars Float
new_bonus_thresholds = Stars 0.05 0.1 0.2 0.3 0.4 0.95

baseAndSolo :: RBFile.FixedFile U.Beats -> (ScoreTrack, Difficulty) -> (Int, Int)
baseAndSolo mid (scoreTrack, diff) = let
  getDrums gem = let
    trk = RBFile.fixedPartDrums mid
    gems = RBDrums.computePro (Just diff) trk
    base = RBDrums.baseScore gem gems
    solo = RBDrums.perfectSoloBonus (RBDrums.drumSolo trk) gems
    in (base, solo)
  in case scoreTrack of
    ScoreDrums    -> getDrums 25
    ScoreProDrums -> getDrums 30
    _             -> (0, 0) -- TODO

starCutoffs :: RBFile.FixedFile U.Beats -> [(ScoreTrack, Difficulty)] -> Stars (Maybe Int)
starCutoffs mid trks = let
  new_num_instruments_multiplier = case length trks of
    1 -> 1.0
    2 -> 1.26
    3 -> 1.52
    _ -> 1.8
  sumOfBases :: Stars Float
  sumOfBases = foldr (liftA2 (+)) (pure 0) $ flip map trks $ \trk@(scoreTrack, _) -> let
    (base, solo) = baseAndSolo mid trk
    in liftA2 (+) ((fromIntegral base *) <$> new_instrument_thresholds scoreTrack)
      ((fromIntegral solo *) <$> new_bonus_thresholds)
  allCutoffs = Just . (floor :: Float -> Int) . (new_num_instruments_multiplier *) <$> sumOfBases
  allExpert = all ((== Expert) . snd) trks
  in allCutoffs { starsGold = guard allExpert >> starsGold allCutoffs }
