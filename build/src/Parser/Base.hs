{- | Datatypes and functions used across multiple MIDI parsers. -}
module Parser.Base where

import Data.Char (isSpace)

data Difficulty = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

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
