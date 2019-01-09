{
module FeedBack.Parse (parse, parseEither, parseStack) where

import FeedBack.Base
import FeedBack.Scan
import qualified Data.Text as T
import Control.Monad.Trans.StackTrace (StackTraceT, fatal, inside)
}

%name parseEither
%tokentype { (AlexPosn, Token) }
%error { parseError }
%monad { Either [(AlexPosn, Token)] }

%token
  nl  { (_, Newline) }
  '{' { (_, BraceL) }
  '}' { (_, BraceR) }
  '[' { (_, BracketL) }
  ']' { (_, BracketR) }
  '=' { (_, Equals) }
  int  { (_, TAtom (Int  $$)) }
  real { (_, TAtom (Real $$)) }
  str  { (_, TAtom (Str  $$)) }

%%

File : Newlines0 Sections0 { $2 }

Sections0 :                   { [] }
          | Section Sections0 { $1 : $2 }

Newlines1 : nl Newlines0 { () }

Newlines0 : nl Newlines0 { () }
          |              { () }

Section :
  '[' TrackName ']' Newlines0
  '{' Newlines0
  Lines0
  '}' Newlines0
  { ($2, $7) }

TrackName : str           { $1 }
          | str TrackName { T.unwords [$1, $2] }

Lines0 : { [] }
       | Line Lines0 { $1 : $2 }

Atom : int  { Int  $1 }
     | real { Real $1 }
     | str  { Str  $1 }

Line : Atom '=' Atoms1 Newlines1 { ($1, $3) }

Atoms1 : Atom        { [$1] }
       | Atom Atoms1 { $1 : $2 }

{

-- | If instead of this error, "Internal Happy error" is sometimes printed, make
-- sure you are using Happy 1.18.7 or later.
parseError :: e -> Either e a
parseError = Left

showError :: [(AlexPosn, Token)] -> String
showError [] = "Parse error at end of file"
showError ((AlexPn _ ln col, tok) : _) =
  "Parse error at " ++ show ln ++ ":" ++ show col ++ ", token " ++ show tok

fatalError :: (Monad m) => [(AlexPosn, Token)] -> StackTraceT m a
fatalError [] = inside "End of file" $ fatal "Parse error"
fatalError ((AlexPn _ ln col, tok) : _)
  = inside ("Line " ++ show ln ++ ", column " ++ show col)
  $ inside ("Token " ++ show tok)
  $ fatal "Parse error"

parse :: [(AlexPosn, Token)] -> [RawSection]
parse = either (error . showError) id . parseEither

parseStack :: (Monad m) => [(AlexPosn, Token)] -> StackTraceT m [RawSection]
parseStack = either fatalError return . parseEither

}
