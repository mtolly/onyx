{-# LANGUAGE LambdaCase #-}
module Parser.Events where

import Parser.Base
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U

data Event
  = MusicStart
  | MusicEnd
  | End
  | PracticeSection String
  deriving (Eq, Ord, Show, Read)

readEvent :: E.T -> Maybe [Event]
readEvent e = case e of
  E.MetaEvent (Meta.TextEvent s) -> readCommand s >>= \case
    ["music_start"      ] -> one $ MusicStart
    ["music_end"        ] -> one $ MusicEnd
    ["end"              ] -> one $ End
    ['p':'r':'c':'_':sec] -> one $ PracticeSection sec
    _                     -> Nothing
  _ -> Nothing
  where one x = Just [x]

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = RTB.singleton 0 . E.MetaEvent . Meta.TextEvent . showCommand . \case
  MusicStart          -> ["music_start"]
  MusicEnd            -> ["music_end"  ]
  End                 -> ["end"        ]
  PracticeSection sec -> ["prc_" ++ sec]
