{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Parser.Events where

import Parser.Base
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Sound.MIDI.Util as U
import Control.Applicative ((<|>))

data Event
  = Simple Simple
  | PracticeSection String
  | PracticeKick
  | PracticeSnare
  | PracticeHihat
  deriving (Eq, Ord, Show, Read)

data Simple
  = MusicStart
  | MusicEnd
  | End
  | Coda
  | CrowdRealtime
  | CrowdIntense
  | CrowdNormal
  | CrowdMellow
  | CrowdNoclap
  | CrowdClap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Simple where
  fromCommand = autoFromCommand
  toCommand = reverseLookup each fromCommand

readEvent :: E.T -> Maybe [Event]
readEvent e
  =   do
    fmap (\s -> [Simple s]) $ readCommand' e
  <|> do
    readCommand' e >>= \case
      ['p':'r':'c':'_':sec] -> Just [PracticeSection sec]
      _                     -> Nothing
  <|> do
    case e of
      MIDINote p b -> case V.fromPitch p of
        24 -> Just [PracticeKick  | b]
        25 -> Just [PracticeSnare | b]
        26 -> Just [PracticeHihat | b]
        _  -> Nothing
      _ -> Nothing

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  Simple s -> RTB.singleton 0 $ showCommand' s
  PracticeSection sec -> RTB.singleton 0 $ showCommand' ["prc_" ++ sec]
  PracticeKick  -> blip $ V.toPitch 24
  PracticeSnare -> blip $ V.toPitch 25
  PracticeHihat -> blip $ V.toPitch 26
