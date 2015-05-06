{- | Datatypes and functions used across multiple MIDI parsers. -}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.Base where

import Data.Char (isSpace)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB

data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

pattern MIDINote p b <- (isNote -> Just (p, b))

isNote :: E.T -> Maybe (V.Pitch, Bool)
isNote = \case
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOn  p v))) -> Just (p, V.fromVelocity v /= 0)
  E.MIDIEvent (C.Cons _ (C.Voice (V.NoteOff p _))) -> Just (p, False                )
  _                                                -> Nothing

edge :: V.Pitch -> Bool -> E.T
edge p b = E.MIDIEvent $ C.Cons (C.toChannel 0) $ C.Voice $
  V.NoteOn p $ V.toVelocity $ if b then 96 else 0

edge' :: Int -> Bool -> E.T
edge' = edge . V.toPitch

blip :: V.Pitch -> RTB.T U.Beats E.T
blip p = RTB.fromPairList
  [ (0   , edge p True )
  , (1/32, edge p False)
  ]

-- | Turns a text event like @\"[foo bar baz]\"@ into @[\"foo\", \"bar\", \"baz\"]@.
readCommand :: String -> Maybe [String]
readCommand s =  case dropWhile isSpace s of
  '[' : s'    -> case dropWhile isSpace $ reverse s' of
    ']' : s'' -> Just $ words $ reverse s''
    _         -> Nothing
  _           -> Nothing

-- | Opposite of 'readCommand'.
showCommand :: [String] -> String
showCommand ws = "[" ++ unwords ws ++ "]"
