{
-- | Generated parser for text @.dta@ files.
module Data.DTA.Parse (parse, parseEither, parseStack, parseStackPositions) where

import Data.DTA.Base
import qualified Data.Text as T
import qualified Data.DTA.Lex as L
import Control.Monad.Trans.StackTrace (StackTraceT, fatal, inside)
}

%name parseEitherPositions
%tokentype { (L.AlexPosn, L.Token s) }
%error { Left }
%monad { Either [(L.AlexPosn, L.Token s)] }

%token
  int { (_, L.Int _) }
  float { (_, L.Float _) }
  var { (_, L.Var _) }
  key { (_, L.Key _) }
  unhandled { (_, L.Unhandled) }
  ifdef { (_, L.IfDef) }
  else { (_, L.Else) }
  endif { (_, L.EndIf) }
  '(' { (_, L.LParen) }
  ')' { (_, L.RParen) }
  '{' { (_, L.LBrace) }
  '}' { (_, L.RBrace) }
  string { (_, L.String _) }
  '[' { (_, L.LBracket) }
  ']' { (_, L.RBracket) }
  define { (_, L.Define) }
  include { (_, L.Include) }
  merge { (_, L.Merge) }
  ifndef { (_, L.IfNDef) }

%%

Chunks : Chunk Chunks { $1 : $2 }
       | { [] }

Subtree : Chunks { Tree 0 (map snd $1) }

-- the lambdas here are ugly, but I don't know of a way
-- to use Happy's $$ in the token list to turn (posn, Int x) into (posn, x)
Chunk : int { fmap (\(L.Int x) -> Int x) $1 }
      | float { fmap (\(L.Float x) -> Float x) $1 }
      | var { fmap (\(L.Var x) -> Var x) $1 }
      | key { fmap (\(L.Key x) -> Key x) $1 }
      | unhandled { fmap (const Unhandled) $1 }
      | ifdef key { fmap (\(L.Key x) -> IfDef x) $2 }
      | else { fmap (const Else) $1 }
      | endif { fmap (const EndIf) $1 }
      | '(' Subtree ')' { fmap (const (Parens $2)) $1 }
      | '{' Subtree '}' { fmap (const (Braces $2)) $1 }
      | string { fmap (\(L.String x) -> String x) $1 }
      | '[' Subtree ']' { fmap (const (Brackets $2)) $1 }
      | define key { fmap (\(L.Key x) -> Define x) $2 }
      | include key { fmap (\(L.Key x) -> Include x) $2 }
      | merge key { fmap (\(L.Key x) -> Merge x) $2 }
      | ifndef key { fmap (\(L.Key x) -> IfNDef x) $2 }

{

showError :: (Show s) => [(L.AlexPosn, L.Token s)] -> String
showError [] = "Parse error at end of file"
showError ((L.AlexPn _ ln col, tok) : _) =
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

fatalError :: (Show s, Monad m) => [(L.AlexPosn, L.Token s)] -> StackTraceT m a
fatalError [] = inside "End of file" $ fatal "Parse error"
fatalError ((L.AlexPn _ ln col, tok) : _)
  = inside ("Line " ++ show ln ++ ", column " ++ show col)
  $ inside ("Token " ++ show tok)
  $ fatal "Parse error"

parse :: (Show s) => [(L.AlexPosn, L.Token s)] -> DTA s
parse = either (error . showError) id . parseEither

parseStack :: (Show s, Monad m) => [(L.AlexPosn, L.Token s)] -> StackTraceT m (DTA s)
parseStack = either fatalError return . parseEither

parseEither :: [(L.AlexPosn, L.Token s)] -> Either [(L.AlexPosn, L.Token s)] (DTA s)
parseEither = fmap (DTA 0 . Tree 0 . map snd) . parseEitherPositions

parseStackPositions :: (Show s) => (Monad m) => [(L.AlexPosn, L.Token s)] -> StackTraceT m [(L.AlexPosn, Chunk s)]
parseStackPositions = either fatalError return . parseEitherPositions

}
