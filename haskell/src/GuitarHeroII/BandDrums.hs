{- |
BAND DRUMS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.BandDrums where

import           RockBand.Parse

data Event
  = Idle
  | Play
  | NoBeat
  | AllBeat
  | HalfTime
  | HalfTempo -- ^ tempo, time, what is difference???
  | DoubleTime
  | DoubleTempo
  | NormalTempo
  | Kick
  | Crash
  | Mystery65
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing $

  [ blip 36 [p| Kick |]
  , blip 37 [p| Crash |]
  -- Laughtrack has 48 and 49, I think they are Kick and Crash (mistakenly up an octave)
  , blip 65 [p| Mystery65 |]

  , commandPair ["idle"] [p| Idle |]
  , commandPair ["play"] [p| Play |]
  , commandPair ["nobeat"] [p| NoBeat |]
  , commandPair ["allbeat"] [p| AllBeat |] -- Jessica has [all_beat], soy bomb has [allplay]
  , commandPair ["half_time"] [p| HalfTime |] -- new black has [halftime]
  , commandPair ["half_tempo"] [p| HalfTempo |]
  -- War Pigs has [double]
  , commandPair ["double_time"] [p| DoubleTime |] -- Who Was In My Room has [doubletime]
  , commandPair ["double_tempo"] [p| DoubleTempo |]
  , commandPair ["normal_tempo"] [p| NormalTempo |]

  ]
