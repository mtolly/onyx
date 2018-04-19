-- | Parser used for all the GRYBO instruments (basic guitar, bass, and keys).
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module RockBand.FiveButton
( Color(..)
, FretPosition(..)
, HandMap(..)
, OnyxCloseEvent(..)
, StrumMap(..)
, StrumHOPOTap(..)
, Event(..)
, DiffEvent(..)
, fiveFromLegacy, fiveToLegacy
) where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.Map                         as Map
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Codec.Five
import           RockBand.Common

data Event
  = Mood                      Mood
  | HandMap                HandMap
  | StrumMap              StrumMap
  | FretPosition FretPosition Bool
  | Tremolo                   Bool
  | Trill                     Bool
  | Overdrive                 Bool
  | BRE                       Bool
  | Solo                      Bool
  | Player1                   Bool
  | Player2                   Bool
  | DiffEvent Difficulty DiffEvent
  deriving (Eq, Ord, Show, Read)

data DiffEvent
  = Force StrumHOPOTap Bool
  | OpenNotes Bool
  | OnyxClose Int
  | Note (LongNote () Color)
  deriving (Eq, Ord, Show, Read)

fiveFromLegacy :: (NNC.C t) => RTB.T t Event -> FiveTrack t
fiveFromLegacy leg = FiveTrack
  { fiveDifficulties = Map.fromList $ do
    d <- each
    let leg' = RTB.mapMaybe (\case DiffEvent d' e | d == d' -> Just e; _ -> Nothing) leg
    return (d, FiveDifficulty
      { fiveForceStrum = RTB.mapMaybe (\case Force Strum x -> Just x; _ -> Nothing) leg'
      , fiveForceHOPO  = RTB.mapMaybe (\case Force HOPO x -> Just x; _ -> Nothing) leg'
      , fiveTap        = RTB.mapMaybe (\case Force Tap x -> Just x; _ -> Nothing) leg'
      , fiveOpen       = RTB.mapMaybe (\case OpenNotes x -> Just x; _ -> Nothing) leg'
      , fiveOnyxClose  = RTB.mapMaybe (\case OnyxClose x -> Just x; _ -> Nothing) leg'
      , fiveGems       = fmap (\((), c, len) -> (c, len)) $ joinEdges $ RTB.mapMaybe (\case Note x -> Just x; _ -> Nothing) leg'
      })
  , fiveMood         = RTB.mapMaybe (\case Mood x -> Just x; _ -> Nothing) leg
  , fiveHandMap      = RTB.mapMaybe (\case HandMap x -> Just x; _ -> Nothing) leg
  , fiveStrumMap     = RTB.mapMaybe (\case StrumMap x -> Just x; _ -> Nothing) leg
  , fiveFretPosition = RTB.mapMaybe (\case FretPosition x y -> Just (x, y); _ -> Nothing) leg
  , fiveTremolo      = RTB.mapMaybe (\case Tremolo x -> Just x; _ -> Nothing) leg
  , fiveTrill        = RTB.mapMaybe (\case Trill x -> Just x; _ -> Nothing) leg
  , fiveOverdrive    = RTB.mapMaybe (\case Overdrive x -> Just x; _ -> Nothing) leg
  , fiveBRE          = RTB.mapMaybe (\case BRE x -> Just x; _ -> Nothing) leg
  , fiveSolo         = RTB.mapMaybe (\case Solo x -> Just x; _ -> Nothing) leg
  , fivePlayer1      = RTB.mapMaybe (\case Player1 x -> Just x; _ -> Nothing) leg
  , fivePlayer2      = RTB.mapMaybe (\case Player2 x -> Just x; _ -> Nothing) leg
  }

fiveToLegacy :: (NNC.C t) => FiveTrack t -> RTB.T t Event
fiveToLegacy o = foldr RTB.merge RTB.empty
  [ Mood     <$> fiveMood     o
  , HandMap  <$> fiveHandMap  o
  , StrumMap <$> fiveStrumMap o
  , uncurry FretPosition <$> fiveFretPosition o
  , Tremolo   <$> fiveTremolo   o
  , Trill     <$> fiveTrill     o
  , Overdrive <$> fiveOverdrive o
  , BRE       <$> fiveBRE       o
  , Solo      <$> fiveSolo      o
  , Player1   <$> fivePlayer1   o
  , Player2   <$> fivePlayer2   o
  , foldr RTB.merge RTB.empty $ do
    (diff, fd) <- Map.toList $ fiveDifficulties o
    map (fmap $ DiffEvent diff)
      [ Force Strum <$> fiveForceStrum fd
      , Force HOPO <$> fiveForceHOPO fd
      , Force Tap <$> fiveTap fd
      , OpenNotes <$> fiveOpen fd
      , OnyxClose <$> fiveOnyxClose fd
      , fmap Note $ splitEdges $ fmap (\(c, len) -> ((), c, len)) $ fiveGems fd
      ]
  ]

instance HasDiffEvent DiffEvent Event where
  makeDiffEvent = DiffEvent
  unmakeDiffEvent = \case
    DiffEvent d e -> Just (d, e)
    _             -> Nothing
