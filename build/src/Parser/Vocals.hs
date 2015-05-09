{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Vocals where

import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB
import Parser.Base
import Control.Applicative ((<$>), (<*>))
import Data.Char (toLower)

data Event
  = LyricShift
  | Mood Mood
  | Lyric String
  | Percussion -- ^ playable percussion note
  | PercussionSound -- ^ nonplayable percussion note, only triggers sound sample
  | PercussionAnimation PercussionType Bool
  | Phrase Bool -- ^ General phrase marker (RB3) or Player 1 phrases (pre-RB3)
  | Phrase2 Bool -- ^ Pre-RB3, used for 2nd player phrases in Tug of War
  | Overdrive Bool
  | RangeShift Bool
  | Note Int Bool -- ^ Int is a MIDI pitch from 36 to 84
  deriving (Eq, Ord, Show, Read)

data PercussionType
  = Tambourine
  | Cowbell
  | Clap
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

readEvent :: E.T -> Maybe [Event]
readEvent (MIDINote p b) = case V.fromPitch p of
  0 -> Just [RangeShift b]
  1 -> Just [LyricShift | b]
  i | 36 <= i && i <= 84 -> Just [Note i b]
  96 -> Just [Percussion | b]
  97 -> Just [PercussionSound | b]
  105 -> Just [Phrase b]
  106 -> Just [Phrase2 b]
  116 -> Just [Overdrive b]
  _ -> Nothing
readEvent (E.MetaEvent (Meta.Lyric str)) = Just [Lyric str]
readEvent (E.MetaEvent (Meta.TextEvent str)) = case str of
  (readCommand -> Just (typ, b)) -> Just [PercussionAnimation typ b]
  (readCommand -> Just mood) -> Just [Mood mood]
  _ -> Just [Lyric str] -- unrecognized text is a lyric by default
readEvent _ = Nothing

instance Command (PercussionType, Bool) where
  fromCommand (typ, b) = [map toLower (show typ) ++ if b then "_start" else "_end"]
  toCommand = reverseLookup ((,) <$> each <*> each) fromCommand

showEvent :: Event -> RTB.T U.Beats E.T
showEvent = \case
  LyricShift -> blip' 1
  Mood m -> RTB.singleton 0 $ showCommand' m
  Lyric s -> RTB.singleton 0 $ E.MetaEvent $ Meta.Lyric s
  Percussion -> blip' 96
  PercussionSound -> blip' 97
  PercussionAnimation typ b -> RTB.singleton 0 $ showCommand' (typ, b)
  Phrase b -> RTB.singleton 0 $ edge' 105 b
  Phrase2 b -> RTB.singleton 0 $ edge' 106 b
  Overdrive b -> RTB.singleton 0 $ edge' 116 b
  RangeShift b -> RTB.singleton 0 $ edge' 0 b
  Note i b -> RTB.singleton 0 $ edge' i b
