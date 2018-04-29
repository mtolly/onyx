{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TupleSections         #-}
module RockBand.Legacy.Drums
( Animation(..), Audio(..), Disco(..), Gem(..)
, Hand(..), Hit(..), PSGem(..), ProColor(..), ProType(..)
, Event(..), DiffEvent(..)
, psRealToPro
, baseScore
, perfectSoloBonus
, drumsFromLegacy, drumsToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Drums
import           RockBand.Common

-- | Constructors are ordered for optimal processing with 'RTB.normalize'.
-- For example, 'Note' comes last so that everything else is processed first.
-- Also, @'ProType' _ 'Cymbal'@ comes before @'ProType' _ 'Tom'@, since the
-- note-on ('Tom') should supercede the simultaneous note-off ('Cymbal').
data Event
  = Mood       Mood
  | ProType    ProColor ProType
  | SingleRoll Bool
  | DoubleRoll Bool
  | Overdrive  Bool -- ^ white notes to gain energy
  | Activation Bool -- ^ drum fill to activate Overdrive, or BRE
  | Solo       Bool
  | Player1    Bool
  | Player2    Bool
  | DiffEvent  Difficulty DiffEvent
  | Kick2x -- ^ Used as input to the build tool for 2x Bass Pedal notes
  | Animation  Animation
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Mix Audio Disco
  | PSHihatOpen    Bool
  | PSHihatPedal   Bool
  | PSSnareRimshot Bool
  | PSHihatSizzle  Bool
  | Note (Gem ())
  deriving (Eq, Ord, Show, Read)

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing

drumsFromLegacy :: (NNC.C t) => RTB.T t Event -> DrumTrack t
drumsFromLegacy leg = DrumTrack
  { drumDifficulties = Map.fromList $ do
    d <- each
    let leg' = RTB.mapMaybe (\case DiffEvent d' e | d == d' -> Just e; _ -> Nothing) leg
    return (d, DrumDifficulty
      { drumMix         = RTB.mapMaybe (\case Mix x y -> Just (x, y); _ -> Nothing) leg'
      , drumPSModifiers = flip RTB.mapMaybe leg' $ \case
        PSHihatOpen    b -> Just (HHOpen, b)
        PSHihatPedal   b -> Just (HHPedal, b)
        PSSnareRimshot b -> Just (Rimshot, b)
        PSHihatSizzle  b -> Just (HHSizzle, b)
        _ -> Nothing
      , drumGems        = RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg'
      })
  , drumMood         = RTB.mapMaybe (\case Mood x -> Just x; _ -> Nothing) leg
  , drumToms         = RTB.mapMaybe (\case ProType x y -> Just (x, y); _ -> Nothing) leg
  , drumSingleRoll   = RTB.mapMaybe (\case SingleRoll x -> Just x; _ -> Nothing) leg
  , drumDoubleRoll   = RTB.mapMaybe (\case DoubleRoll x -> Just x; _ -> Nothing) leg
  , drumOverdrive    = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , drumActivation   = RTB.mapMaybe (\case Activation x -> Just x; _ -> Nothing) leg
  , drumSolo         = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  , drumPlayer1      = RTB.mapMaybe (\case Player1 x -> Just x; _ -> Nothing) leg
  , drumPlayer2      = RTB.mapMaybe (\case Player2 x -> Just x; _ -> Nothing) leg
  , drumKick2x       = RTB.mapMaybe (\case Kick2x -> Just (); _ -> Nothing) leg
  , drumAnimation    = RTB.mapMaybe (\case Animation x -> Just x; _ -> Nothing) leg
  }

drumsToLegacy :: (NNC.C t) => DrumTrack t -> RTB.T t Event
drumsToLegacy o = foldr RTB.merge RTB.empty
  [ Mood            <$> drumMood o
  , uncurry ProType <$> drumToms o
  , SingleRoll      <$> drumSingleRoll  o
  , DoubleRoll      <$> drumDoubleRoll o
  , Overdrive       <$> drumOverdrive o
  , Activation      <$> drumActivation o
  , Solo            <$> drumSolo o
  , Player1         <$> drumPlayer1 o
  , Player2         <$> drumPlayer2 o
  , const Kick2x    <$> drumKick2x o
  , Animation       <$> drumAnimation o
  , foldr RTB.merge RTB.empty $ do
    (diff, fd) <- Map.toList $ drumDifficulties o
    map (fmap $ DiffEvent diff)
      [ uncurry Mix <$> drumMix fd
      , flip fmap (drumPSModifiers fd) $ \case
        (HHOpen, b) -> PSHihatOpen b
        (HHPedal, b) -> PSHihatPedal b
        (Rimshot, b) -> PSSnareRimshot b
        (HHSizzle, b) -> PSHihatSizzle b
      , Note <$> drumGems fd
      ]
  ]
