{- |
BAND BASS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.BandBass where

import           RockBand.Parse

data Event
  = Idle
  | Play
  | HalfTempo
  | NormalTempo
  | DoubleTempo
  | Strum
  | Mystery61
  -- [wail_on] [wail_off]
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing $

  [ blip 36 [p| Strum |]
  , blip 61 [p| Mystery61 |]

  , commandPair ["idle"] [p| Idle |]
  , commandPair ["play"] [p| Play |]
  , commandPair ["half_tempo"] [p| HalfTempo |]
  , commandPair ["normal_tempo"] [p| NormalTempo |]
  , commandPair ["double_tempo"] [p| DoubleTempo |]

  ]
