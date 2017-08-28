{- |
TRIGGERS
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module GuitarHeroII.Triggers where

import           RockBand.Parse

data Event
  = PracticeKick
  | PracticeSnare
  | PracticeHihat
  -- Trogdor has 36 and 38
  | Unknown48
  | Unknown49
  | Unknown50
  | Unknown52 Bool
  -- Tonight I'm Gonna Rock You Tonight has pitch 60
  deriving (Eq, Ord, Show, Read)

instanceMIDIEvent [t| Event |] Nothing $

  [ blip 24 [p| PracticeKick |]
  , blip 25 [p| PracticeSnare |]
  , blip 26 [p| PracticeHihat |]

  , blip 48 [p| Unknown48 |]
  , blip 49 [p| Unknown49 |]
  , blip 50 [p| Unknown50 |]
  , edge 52 $ applyB [p| Unknown52 |]

  ]
