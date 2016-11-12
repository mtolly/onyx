{-# LANGUAGE TemplateHaskell #-}
module RockBand.Beat where

import           RockBand.Parse

data Event = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instanceMIDIEvent [t| Event |]

  [ blip 12 [p| Bar  |]
  , blip 13 [p| Beat |]
  ]
