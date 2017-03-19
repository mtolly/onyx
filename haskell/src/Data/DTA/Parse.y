{
-- | Generated parser for text @.dta@ files.
module Data.DTA.Parse (parse, parseEither, parseStack) where

import Data.DTA.Base
import qualified Data.Text as T
import qualified Data.DTA.Lex as L
import Control.Monad.Trans.StackTrace (StackTraceT, fatal, inside)
}

%name parseEither
%tokentype { (L.AlexPosn, L.Token T.Text) }
%error { parseError }
%monad { Either [(L.AlexPosn, L.Token T.Text)] }

%token
  int { (_, L.Int $$) }
  float { (_, L.Float $$) }
  var { (_, L.Var $$) }
  key { (_, L.Key $$) }
  unhandled { (_, L.Unhandled) }
  ifdef { (_, L.IfDef) }
  else { (_, L.Else) }
  endif { (_, L.EndIf) }
  '(' { (_, L.LParen) }
  ')' { (_, L.RParen) }
  '{' { (_, L.LBrace) }
  '}' { (_, L.RBrace) }
  string { (_, L.String $$) }
  '[' { (_, L.LBracket) }
  ']' { (_, L.RBracket) }
  define { (_, L.Define) }
  include { (_, L.Include) }
  merge { (_, L.Merge) }
  ifndef { (_, L.IfNDef) }

%%

File : Tree { DTA 0 $1 }

Tree : Chunks { Tree 0 $1 }

Chunks : Chunk Chunks { $1 : $2 }
       | { [] }

Chunk : int { Int $1 }
      | float { Float $1 }
      | var { Var $1 }
      | key { Key $1 }
      | unhandled { Unhandled }
      | ifdef key { IfDef $2 }
      | else { Else }
      | endif { EndIf }
      | '(' Tree ')' { Parens $2 }
      | '{' Tree '}' { Braces $2 }
      | string { String $1 }
      | '[' Tree ']' { Brackets $2 }
      | define key { Define $2 }
      | include key { Include $2 }
      | merge key { Merge $2 }
      | ifndef key { IfNDef $2 }

{

-- | If instead of this error, "Internal Happy error" is sometimes printed, make
-- sure you are using Happy 1.18.7 or later.
parseError :: e -> Either e a
parseError = Left

showError :: [(L.AlexPosn, L.Token T.Text)] -> String
showError [] = "Parse error at end of file"
showError ((L.AlexPn _ ln col, tok) : _) =
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

fatalError :: (Monad m) => [(L.AlexPosn, L.Token T.Text)] -> StackTraceT m a
fatalError [] = inside "End of file" $ fatal "Parse error"
fatalError ((L.AlexPn _ ln col, tok) : _)
  = inside ("Line " ++ show ln ++ ", column " ++ show col)
  $ inside ("Token " ++ show tok)
  $ fatal "Parse error"

parse :: [(L.AlexPosn, L.Token T.Text)] -> DTA T.Text
parse = either (error . showError) id . parseEither

parseStack :: (Monad m) => [(L.AlexPosn, L.Token T.Text)] -> StackTraceT m (DTA T.Text)
parseStack = either fatalError return . parseEither

}
