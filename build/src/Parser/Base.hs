{- | Datatypes and functions used across multiple MIDI parsers. -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Parser.Base where

import Data.Char (isSpace, isUpper, toLower)
import Data.List (stripPrefix)
import Text.Read (readMaybe)
import qualified Sound.MIDI.File.Event as E
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.Message.Channel as C
import qualified Sound.MIDI.Message.Channel.Voice as V
import qualified Sound.MIDI.Util as U
import qualified Data.EventList.Relative.TimeBody as RTB

-- | Class for events which are stored as a @\"[x y z]\"@ text event.
class Command a where
  toCommand :: [String] -> Maybe a
  fromCommand :: a -> [String]

reverseLookup :: (Eq b) => [a] -> (a -> b) -> b -> Maybe a
reverseLookup xs f y = let
  pairs = [ (f x, x) | x <- xs ]
  in lookup y pairs

each :: (Enum a, Bounded a) => [a]
each = [minBound .. maxBound]

-- | Turns @FooBarBaz@ into @["foo_bar_baz"]@.
autoFromCommand :: (Show a) => a -> [String]
autoFromCommand = (: []) . f . show where
  f "" = ""
  f (c : cs) = toLower c : g cs
  g "" = ""
  g (c : cs) = if isUpper c
    then '_' : toLower c : g cs
    else c : g cs

data Mood = IdleRealtime | Idle | IdleIntense | Play | Mellow | Intense | PlaySolo
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance Command Mood where
  fromCommand = autoFromCommand
  toCommand = reverseLookup each fromCommand

instance Command [String] where
  toCommand   = Just
  fromCommand = id

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

-- | Makes a note on\/off pair of the smallest allowed length.
blip :: V.Pitch -> RTB.T U.Beats E.T
blip p = RTB.fromPairList
  [ (0   , edge p True )
  , (1/32, edge p False)
  ]

blip' :: Int -> RTB.T U.Beats E.T
blip' = blip . V.toPitch

readCommand' :: (Command a) => E.T -> Maybe a
readCommand' (E.MetaEvent (Meta.TextEvent s)) = readCommand s
readCommand' _ = Nothing

-- | Turns a string like @\"[foo bar baz]\"@ into some parsed type.
readCommand :: (Command a) => String -> Maybe a
readCommand s =  case dropWhile isSpace s of
  '[' : s'    -> case dropWhile isSpace $ reverse s' of
    ']' : s'' -> toCommand $ words $ reverse s''
    _         -> Nothing
  _           -> Nothing

showCommand' :: (Command a) => a -> E.T
showCommand' = E.MetaEvent . Meta.TextEvent . showCommand

-- | Opposite of 'readCommand'.
showCommand :: (Command a) => a -> String
showCommand ws = "[" ++ unwords (fromCommand ws) ++ "]"

data Trainer
  = TrainerBegin Int
  | TrainerNorm Int
  | TrainerEnd Int
  deriving (Eq, Ord, Show, Read)

instance Command (Trainer, String) where
  fromCommand (t, s) = case t of
    TrainerBegin i -> ["begin_" ++ s, "song_trainer_" ++ s ++ "_" ++ show i]
    TrainerNorm  i -> [ "norm_" ++ s, "song_trainer_" ++ s ++ "_" ++ show i]
    TrainerEnd   i -> [  "end_" ++ s, "song_trainer_" ++ s ++ "_" ++ show i]
  toCommand [x, stripPrefix "song_trainer_" -> Just y] = case x of
    (stripPrefix "begin_" -> Just s) -> f s TrainerBegin
    (stripPrefix "norm_"  -> Just s) -> f s TrainerNorm
    (stripPrefix "end_"   -> Just s) -> f s TrainerEnd
    _ -> Nothing
    where f s con = case stripPrefix s y of
            Just ('_' : (readMaybe -> Just i)) -> Just (con i, s)
            Just (readMaybe -> Just i) -> Just (con i, s)
            _ -> Nothing
  toCommand _ = Nothing
