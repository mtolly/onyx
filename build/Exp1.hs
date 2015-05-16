{-# LANGUAGE TemplateHaskell #-}
module Main where

import Exp2
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB

main :: IO ()
main = print $ parseAll parser $ RTB.fromPairList
  [ (0, makeEdge 0 True)
  , (0, makeEdge 3 True)
  , (0, makeEdge 2 True)
  , (0, makeEdge 1 True)
  , (0, makeEdge 4 True)
  , (4, makeEdge 0 False)
  , (0, makeEdge 1 False)
  , (6, makeEdge 3 False)
  , (0, makeEdge 2 False)
  , (0, makeEdge 4 False)
  ]

data Event
  = Blip
  | Length Bool
  | Trio Bool
  deriving (Eq, Ord, Show, Read)

parser   ::   ParseOne U.Beats E.T Event
unparser :: UnparseOne U.Beats E.T Event
(parser, unparser) = $(translation
  [ blip 0 [p| Blip |]
  , edge 1 $ applyB [p| Length |]
  , edges [2, 3, 4] $ applyB [p| Trio |]
  ])
