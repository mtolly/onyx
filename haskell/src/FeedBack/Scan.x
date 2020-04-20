{
{-# OPTIONS_GHC -w #-}
module FeedBack.Scan (scanStack, Token(..), AlexPosn(..)) where

import Control.Monad.Trans.StackTrace (StackTraceT, fatal)
import qualified Data.Text as T
import FeedBack.Base (Atom(..))
}

%wrapper "monad"

$digit = 0-9
$rawfirst = [^\n\{\}\[\]\"$white$digit]
$rawnext = [^\n\{\}\[\]\"$white]

tokens :-

[\ \t\r] ;

\n { emit $ const Newline }
\{ { emit $ const BraceL }
\} { emit $ const BraceR }
\[ { emit $ const BracketL }
\] { emit $ const BracketR }
\= { emit $ const Equals }

(\+ | \-)? $digit+ { emit $ TAtom . Int . read . dropWhile (== '+') }
(\+ | \-)? $digit+ "." $digit+ { emit $ \s -> let
  (whole, '.' : part) = break (== '.') s
  wholeRat = fromInteger $ read $ dropWhile (== '+') whole
  partDenom = fromInteger $ 10 ^ length part
  partRat = fromInteger (read part) / partDenom
  in TAtom $ Real $ wholeRat + partRat
  }

$rawfirst $rawnext* { emit $ TAtom . Str . T.pack }
\" ([^\"\n\r] | \"\/\")* \" { emit $ TAtom . Str . fixQuotes . T.tail . T.init . T.pack }
\" ([^\"\n\r] | \"\/\")*    { emit $ TAtom . Str . fixQuotes . T.tail          . T.pack }

{

fixQuotes :: T.Text -> T.Text
fixQuotes = T.intercalate (T.pack "\"") . T.splitOn (T.pack "\"/\"")

emit :: (String -> a) -> AlexInput -> Int -> Alex (Maybe (AlexPosn, a))
emit f (pn, _, _, str) len = return $ Just $ (pn, f $ take len str)

data Token
  = Newline
  | BraceL
  | BraceR
  | BracketL
  | BracketR
  | Equals
  | TAtom Atom
  deriving (Eq, Ord, Show)

scanAll :: Alex [(AlexPosn, Token)]
scanAll = do
  res <- alexMonadScan
  case res of
    Nothing   -> return []
    Just pair -> (pair :) <$> scanAll

scanEither :: T.Text -> Either String [(AlexPosn, Token)]
scanEither t = runAlex (T.unpack t) scanAll

scan :: T.Text -> [(AlexPosn, Token)]
scan = either error id . scanEither

scanStack :: (Monad m) => T.Text -> StackTraceT m [(AlexPosn, Token)]
scanStack = either fatal return . scanEither

alexEOF :: Alex (Maybe (AlexPosn, Token))
alexEOF = return Nothing

}
