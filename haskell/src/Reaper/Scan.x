{
{-# OPTIONS_GHC -w #-}
module Reaper.Scan (scan, Token(..), AlexPosn(..)) where
}

%wrapper "posn"

tokens :-

[\ \t] ;

[\r\n] { \pn _ -> (pn, Newline) }
\< { \pn _ -> (pn, AngleL) }
\> { \pn _ -> (pn, AngleR) }

-- This is a bit ridiculous
\" [^\"]* \" { \pn str -> (pn, Atom $ dropEdges str) }
\' [^\']* \' { \pn str -> (pn, Atom $ dropEdges str) }
\` [^\`]* \` { \pn str -> (pn, Atom $ dropEdges str) }

(. # [ $white \" \' \` \< \> ]) (. # $white)* { \pn str -> (pn, Atom str) }

{

dropEdges :: String -> String
dropEdges = reverse . drop 1 . reverse . drop 1

data Token
  = Newline
  | AngleL
  | AngleR
  | Atom String
  deriving (Eq, Ord, Show, Read)

scan :: String -> [(AlexPosn, Token)]
scan = alexScanTokens

}
