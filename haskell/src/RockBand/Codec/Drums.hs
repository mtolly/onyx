{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module RockBand.Codec.Drums where

import           Control.Monad                    (guard, (>=>))
import           Control.Monad.Codec
import           Data.Default.Class               (Default (..))
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import           Data.Profunctor                  (dimap)
import           Guitars                          (applyStatus)
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec
import           RockBand.Common
import           RockBand.Drums                   (Animation (..), Audio (..),
                                                   Disco (..), Gem (..),
                                                   Hand (..), Hit (..),
                                                   PSGem (..), ProColor (..),
                                                   ProType (..))
import qualified RockBand.PhaseShiftMessage       as PS

data DrumTrack t = DrumTrack
  { drumMood         :: RTB.T t Mood
  , drumToms         :: RTB.T t (ProColor, ProType)
  , drumSingleRoll   :: RTB.T t Bool
  , drumDoubleRoll   :: RTB.T t Bool
  , drumOverdrive    :: RTB.T t Bool -- ^ white notes to gain energy
  , drumActivation   :: RTB.T t Bool -- ^ drum fill to activate Overdrive, or BRE
  , drumSolo         :: RTB.T t Bool
  , drumPlayer1      :: RTB.T t Bool
  , drumPlayer2      :: RTB.T t Bool
  , drumDifficulties :: Map.Map Difficulty (DrumDifficulty t)
  , drumKick2x       :: RTB.T t ()
  , drumAnimation    :: RTB.T t Animation
  } deriving (Eq, Ord, Show)

data DrumDifficulty t = DrumDifficulty
  { drumMix         :: RTB.T t (Audio, Disco)
  , drumPSModifiers :: RTB.T t (PSGem, Bool)
  , drumGems        :: RTB.T t (Gem ())
  } deriving (Eq, Ord, Show)

instance Default (DrumDifficulty t) where
  def = DrumDifficulty RTB.empty RTB.empty RTB.empty

parseProType :: (Monad m, NNC.C t) => Int -> TrackEvent m t ProType
parseProType
  = dimap
    (fmap $ \case Tom -> True; Cymbal -> False)
    (fmap $ \b -> if b then Tom else Cymbal)
  . edges

instance ParseTrack DrumTrack where
  parseTrack = do
    drumMood <- drumMood =. command
    drumToms <- (drumToms =.) $ condenseMap $ eachKey each $ parseProType . \case
      Yellow -> 110
      Blue   -> 111
      Green  -> 112
    drumSingleRoll <- drumSingleRoll =. edges 126
    drumDoubleRoll <- drumDoubleRoll =. edges 127
    drumOverdrive <- drumOverdrive =. edges 116
    drumActivation <- drumActivation =. edgesBRE [120 .. 124]
    drumSolo <- drumSolo =. edges 103
    drumPlayer1 <- drumPlayer1 =. edges 105
    drumPlayer2 <- drumPlayer2 =. edges 106
    drumDifficulties <- (drumDifficulties =.) $ eachKey each $ \diff -> do
      let base = case diff of
            Easy   -> 60
            Medium -> 72
            Hard   -> 84
            Expert -> 96
          allDrums = [Kick, Red, Pro Yellow (), Pro Blue (), Pro Green (), Orange]
      drumGems <- (drumGems =.) $ condenseMap_ $ eachKey allDrums $ \drum -> do
        blip $ base + case drum of
          Kick          -> 0
          Red           -> 1
          Pro Yellow () -> 2
          Pro Blue   () -> 3
          Pro Green  () -> 4
          Orange        -> 5
      drumMix <- drumMix =. let
        parse = readCommand' >=> \(diff', aud, dsc) -> guard (diff == diff') >> Just (aud, dsc)
        unparse (aud, dsc) = showCommand' (diff :: Difficulty, aud :: Audio, dsc :: Disco)
        in single parse unparse
      drumPSModifiers <- (drumPSModifiers =.) $ condenseMap $ eachKey each $ sysexPS diff . \case
        Rimshot  -> PS.SnareRimshot
        HHOpen   -> PS.HihatOpen
        HHSizzle -> PS.HihatSizzle
        HHPedal  -> PS.HihatPedal
      return DrumDifficulty{..}
    drumKick2x <- drumKick2x =. blip 95 -- TODO this should probably be blip-grouped with expert track
    drumAnimation <- (drumAnimation =.) $ condenseMap_ $ eachKey each $ \case
      KickRF -> blip 24
      HihatOpen b -> edge 25 b
      Snare HardHit LH -> blip 26
      Snare HardHit RH -> blip 27
      Snare SoftHit LH -> blip 28
      Snare SoftHit RH -> blip 29
      Hihat LH -> blip 30
      Hihat RH -> blip 31
      PercussionRH -> blip 32
      -- 33 unused
      Crash1 HardHit LH -> blip 34
      Crash1 SoftHit LH -> blip 35
      Crash1 HardHit RH -> blip 36
      Crash1 SoftHit RH -> blip 37
      Crash2 HardHit RH -> blip 38
      Crash2 SoftHit RH -> blip 39
      Crash2RHChokeLH -> blip 40
      Crash1RHChokeLH -> blip 41
      Ride RH -> blip 42
      Ride LH -> blip 43
      Crash2 HardHit LH -> blip 44
      Crash2 SoftHit LH -> blip 45
      Tom1 LH -> blip 46
      Tom1 RH -> blip 47
      Tom2 LH -> blip 48
      Tom2 RH -> blip 49
      FloorTom LH -> blip 50
      FloorTom RH -> blip 51
      RideSide True -> commandMatch ["ride_side_true"]
      RideSide False -> commandMatch ["ride_side_false"]
    return DrumTrack{..}

computePro :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (Gem ProType)
computePro diff trk = let
  toms = fmap (fmap $ \case Tom -> True; Cymbal -> False) $ drumToms trk
  this = fromMaybe def $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  disco = fmap (\(_aud, dsc) -> ((), dsc == Disco)) $ drumMix this
  applied = applyStatus disco $ applyStatus toms $ case diff of
    Nothing -> RTB.merge (fmap (\() -> Kick) $ drumKick2x trk) $ drumGems this
    _       -> drumGems this
  in flip fmap applied $ \case
    (instantDisco, (instantToms, gem)) -> case gem of
      Kick -> Kick
      Red -> if isDisco
        then Pro Yellow Cymbal
        else Red
      Pro Yellow () | isDisco -> Red
      Pro color () -> Pro color $ if elem color instantToms then Tom else Cymbal
      Orange -> Orange -- probably shouldn't happen
      where isDisco = not $ null instantDisco

computePSReal :: (NNC.C t) => Maybe Difficulty -> DrumTrack t -> RTB.T t (Either PSGem (Gem ProType))
computePSReal diff trk = let
  pro = computePro diff trk
  this = fromMaybe def $ Map.lookup (fromMaybe Expert diff) $ drumDifficulties trk
  applied = applyStatus (drumPSModifiers this) pro
  in flip fmap applied $ \case
    (mods, Red)
      | elem Rimshot mods -> Left Rimshot
    (mods, Pro Yellow Cymbal)
      | elem HHOpen mods -> Left HHOpen
      | elem HHPedal mods -> Left HHPedal
      | elem HHSizzle mods -> Left HHSizzle
    (_, gem) -> Right gem

psRealToPro :: (NNC.C t) => DrumTrack t -> DrumTrack t
psRealToPro trk = trk
  { drumDifficulties = flip Map.mapWithKey (drumDifficulties trk) $ \diff this -> let
    -- this will fail if you use discobeat on the real track, so don't do that
    merged = RTB.merge (Left <$> computePSReal (Just diff) trk) (Right <$> drumGems this)
    eachInstant xs = flip filter [ x | Right x <- xs ] $ \case
      Pro Yellow () -> notElem (Left $ Left HHPedal) xs
      _             -> True
    in this
      { drumPSModifiers = RTB.empty
      , drumGems = RTB.flatten $ fmap eachInstant $ RTB.collectCoincident merged
      }
  }

baseScore :: RTB.T t (Gem ProType) -> Int
baseScore = sum . fmap gemScore where
  gemScore = \case
    Kick         -> 30
    Red          -> 30
    Pro _ Cymbal -> 30
    Pro _ Tom    -> 25
    Orange       -> 30 -- no actual answer

perfectSoloBonus :: (NNC.C t, Ord a) => RTB.T t Bool -> RTB.T t (Gem a) -> Int
perfectSoloBonus solo gems = sum $ fmap score $ applyStatus (fmap ((),) solo) gems where
  score ([], _) = 0
  score _       = 100
