{- |
BAND KEYS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.BandKeys where

import           RockBand.Parse

data Event
  = Idle
  | Play
  | HalfTempo
  | NormalTempo
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing $

  [ commandPair ["idle"] [p| Idle |]
  , commandPair ["play"] [p| Play |]
  , commandPair ["half_tempo"] [p| HalfTempo |]
  , commandPair ["normal_tempo"] [p| NormalTempo |]
  ]
