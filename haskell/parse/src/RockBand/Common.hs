{- | Datatypes and functions used across multiple MIDI parsers. -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module RockBand.Common where

import           Control.Monad                    (guard)
import           Data.Char                        (isSpace, isUpper, toLower)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.List                        (stripPrefix)
import           Language.Haskell.TH
import qualified Numeric.NonNegative.Class        as NNC
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import           Text.Read                        (readMaybe)

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
    TrainerNorm  i -> [ s ++ "_norm", "song_trainer_" ++ s ++ "_" ++ show i]
    TrainerEnd   i -> [  "end_" ++ s, "song_trainer_" ++ s ++ "_" ++ show i]
  toCommand [x, stripPrefix "song_trainer_" -> Just y] = case x of
    (stripPrefix "begin_" -> Just s) -> f s TrainerBegin
    (stripSuffix "_norm"  -> Just s) -> f s TrainerNorm
    (stripPrefix "end_"   -> Just s) -> f s TrainerEnd
    _ -> Nothing
    where f s con = case stripPrefix s y of
            Just ('_' : (readMaybe -> Just i)) -> Just (con i, s)
            Just (readMaybe -> Just i) -> Just (con i, s)
            _ -> Nothing
          stripSuffix sfx s = fmap reverse $ stripPrefix (reverse sfx) (reverse s)
  toCommand _ = Nothing

data Key = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

keyP :: Int -> Q Pat
keyP = \case
  0  -> [p| C  |]
  1  -> [p| Cs |]
  2  -> [p| D  |]
  3  -> [p| Ds |]
  4  -> [p| E  |]
  5  -> [p| F  |]
  6  -> [p| Fs |]
  7  -> [p| G  |]
  8  -> [p| Gs |]
  9  -> [p| A  |]
  10 -> [p| As |]
  11 -> [p| B  |]
  i  -> error $ "keyP: can't make Key pattern from " ++ show i

baseCopyExpert
  :: (NNC.C t, Ord a)
  => (Difficulty -> d -> a)
  -> (a -> Maybe (Difficulty, d))
  -> RTB.T t a
  -> RTB.T t a
baseCopyExpert differ undiffer rtb = let
  (diffEvents, rtb') = RTB.partitionMaybe undiffer rtb
  [e, m, h, x] = flip map [Easy, Medium, Hard, Expert] $ \diff ->
    flip RTB.mapMaybe diffEvents $ \(diff', evt) -> guard (diff == diff') >> return evt
  e' = fmap (differ Easy  ) $ if RTB.null e then x else e
  m' = fmap (differ Medium) $ if RTB.null m then x else m
  h' = fmap (differ Hard  ) $ if RTB.null h then x else h
  x' = fmap (differ Expert) x
  in foldr RTB.merge rtb' [e', m', h', x']
