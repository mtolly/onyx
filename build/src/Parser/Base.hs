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
