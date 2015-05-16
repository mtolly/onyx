{-# LANGUAGE TemplateHaskell #-}
module Parser.Beat where

import Parser.TH
import Language.Haskell.TH

data Event = Bar | Beat
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

rosetta :: (Q Exp, Q Exp)
rosetta = translation
  [ blip 12 [p| Bar  |]
  , blip 13 [p| Beat |]
  ]
