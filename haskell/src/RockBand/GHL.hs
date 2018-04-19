-- | The \"Clone Hero Live\" MIDI format.
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module RockBand.GHL
( Fret(..)
, Event(..)
, DiffEvent(..)
, sixFromLegacy, sixToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Six
import           RockBand.Common
import           RockBand.FiveButton              (StrumHOPOTap (..))

data Event
  = Overdrive                 Bool
  | Solo                      Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Force StrumHOPOTap Bool
  | Note (LongNote () (Maybe Fret))
  deriving (Eq, Ord, Show, Read)

sixFromLegacy :: (NNC.C t) => RTB.T t Event -> SixTrack t
sixFromLegacy leg = SixTrack
  { sixDifficulties = Map.fromList $ do
    d <- each
    let leg' = RTB.mapMaybe (\case DiffEvent d' e | d == d' -> Just e; _ -> Nothing) leg
    return (d, SixDifficulty
      { sixForceStrum = RTB.mapMaybe (\case Force Strum x -> Just x; _ -> Nothing) leg'
      , sixForceHOPO  = RTB.mapMaybe (\case Force HOPO x -> Just x; _ -> Nothing) leg'
      , sixTap        = RTB.mapMaybe (\case Force Tap x -> Just x; _ -> Nothing) leg'
      , sixGems       = fmap (\((), c, len) -> (c, len)) $ joinEdges $ RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg'
      })
  , sixOverdrive    = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , sixSolo         = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  }

sixToLegacy :: (NNC.C t) => SixTrack t -> RTB.T t Event
sixToLegacy o = foldr RTB.merge RTB.empty
  [ Overdrive <$> sixOverdrive o
  , Solo      <$> sixSolo      o
  , foldr RTB.merge RTB.empty $ do
    (diff, fd) <- Map.toList $ sixDifficulties o
    map (fmap $ DiffEvent diff)
      [ Force Strum <$> sixForceStrum fd
      , Force HOPO <$> sixForceHOPO fd
      , Force Tap <$> sixTap fd
      , fmap Note $ splitEdges $ fmap (\(c, len) -> ((), c, len)) $ sixGems fd
      ]
  ]

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing
