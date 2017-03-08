{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
module RockBand.Beat where

import           Data.Data
import           RockBand.Parse

data Event = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)

instanceMIDIEvent [t| Event |] Nothing

  [ blip 12 [p| Bar  |]
  , blip 13 [p| Beat |]
  ]
