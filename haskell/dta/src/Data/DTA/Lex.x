{
-- | Generated lexer for text @.dta@ files.
{-# OPTIONS_GHC -w #-}
module Data.DTA.Lex (scan, Token(..), AlexPosn(..)) where

import Data.Int (Int32)
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

-- Whitespace and line comments.
$white+ ;
\; [^\n]* ;

-- Numbers. Longest match rule means N.N is float, not int.
\-? $digit+ { \pn str -> (pn, Int $ read str) }
\-? $digit+ (\. $digit+)? (e \-? $digit+)? { \pn str -> (pn, Float $ read str) }

-- Variable names.
\$ ($alpha | $digit | _)+ { \pn str -> (pn, Var $ tail str) }

-- This reserved word needs to come before the general keyword rule.
"kDataUnhandled" { \pn _ -> (pn, Unhandled) }
-- Raw keywords. Note: these can start with digits, like "3sand7s", as long as
-- they also have letters in them.
($alpha | $digit | _ | \/ | \.)+ { \pn str -> (pn, Key str) }
-- Quoted keywords.
' ([^'] | \\')* ' { \pn str -> (pn, Key $ readKey str) }

-- Quoted strings.
\" [^\"]* \" { \pn str -> (pn, String $ readString str) }

-- Preprocessor commands.
\#ifdef { \pn _ -> (pn, IfDef) }
\#else { \pn _ -> (pn, Else) }
\#endif { \pn _ -> (pn, EndIf) }
\#define { \pn _ -> (pn, Define) }
\#include { \pn _ -> (pn, Include) }
\#merge { \pn _ -> (pn, Merge) }
\#ifndef { \pn _ -> (pn, IfNDef) }

-- Subtrees.
\( { \pn _ -> (pn, LParen) }
\) { \pn _ -> (pn, RParen) }
\{ { \pn _ -> (pn, LBrace) }
\} { \pn _ -> (pn, RBrace) }
\[ { \pn _ -> (pn, LBracket) }
\] { \pn _ -> (pn, RBracket) }

{

data Token
  = Int Int32
  | Float Float
  | Var String
  | Key String
  | Unhandled
  | IfDef
  | Else
  | EndIf
  | LParen
  | RParen
  | LBrace
  | RBrace
  | String String
  | LBracket
  | RBracket
  | Define
  | Include
  | Merge
  | IfNDef
  deriving (Eq, Ord, Show, Read)

-- | Reads a single-quoted string, by converting it to a double-quoted one.
readKey :: String -> String
readKey = readString . go where
  go ('\'':xs) = '"' : go xs        -- string begin/end -> double-quote
  go ('"':xs) = '\\' : 'q' : go xs  -- double-quote gets encoded as \q
  go ('\\':x:xs) = '\\' : x : go xs -- any escaped char can remain escaped
  go (x:xs) = x : go xs             -- all other chars are unchanged
  go [] = []

-- | Reads the special format for double-quoted strings.
readString :: String -> String
readString = read . go where
  go ('\\' : 'q' : rest) = '"' : go rest
  go ""                  = ""
  go (c : rest)          = c : go rest

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

}
