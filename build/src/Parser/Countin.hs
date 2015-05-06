module Parser.Countin where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U

data Event = CountinHere
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: E.T -> Maybe [Event]
readEvent e = case e of
  E.MetaEvent (Meta.TextEvent "countin_here") -> Just [CountinHere]
  _                                           -> Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent CountinHere = RTB.singleton 0 $ E.MetaEvent $ Meta.TextEvent "countin_here"
