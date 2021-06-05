{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module FeedBack.Scan2 (scanStack, scanEither, Token(..), AlexPosn(..), scan) where

import           Control.Monad.Trans.StackTrace (StackTraceT, fatal)
import           Data.Char                      (isDigit)
import           Data.Foldable                  (toList)
import qualified Data.Sequence                  as Seq
import qualified Data.Text                      as T
import           FeedBack.Base                  (Atom (..))

-- }

-- %wrapper "monad"

-- $digit = 0-9
-- $rawfirst = [^\n\{\}\[\]\"$white$digit]
-- $rawnext = [^\n\{\}\[\]\"$white]

-- tokens :-

-- [\ \t\r] ;

-- \n { emit $ const Newline }
-- \{ { emit $ const BraceL }
-- \} { emit $ const BraceR }
-- \[ { emit $ const BracketL }
-- \] { emit $ const BracketR }
-- \= { emit $ const Equals }

-- (\+ | \-)? $digit+ { emit $ TAtom . Int . read . dropWhile (== '+') }
-- (\+ | \-)? $digit+ "." $digit+ { emit $ \s -> let
--   (whole, '.' : part) = break (== '.') s
--   wholeRat = fromInteger $ read $ dropWhile (== '+') whole
--   partDenom = fromInteger $ 10 ^ length part
--   partRat = fromInteger (read part) / partDenom
--   in TAtom $ Real $ wholeRat + partRat
--   }

-- $rawfirst $rawnext* { emit $ TAtom . Str . T.pack }
-- \" ([^\"\n\r] | \"\/\")* \" { emit $ TAtom . Str . fixQuotes . T.tail . T.init . T.pack }
-- \" ([^\"\n\r] | \"\/\")*    { emit $ TAtom . Str . fixQuotes . T.tail          . T.pack }

-- {

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alex_tab_size :: Int
alex_tab_size = 8

--------

data TP = TP !T.Text !AlexPosn

alexMoveMany :: AlexPosn -> T.Text -> AlexPosn
alexMoveMany = T.foldl' alexMove

skipSpace :: TP -> TP
skipSpace (TP t p) = case T.span (`elem` (" \t\r" :: String)) t of
  (spaces, rest) -> (TP rest $ alexMoveMany p spaces)

-- | Assumes spaces have already been skipped.
scanToken :: TP -> Either String (TP, (AlexPosn, Token))
scanToken (TP t p) = case T.uncons t of
  Nothing -> Left "panic! scanToken shouldn't be given an empty string"
  Just (c, t') -> case c of
    '\n' -> Right (TP t' $ alexMove p c, (p, Newline))
    '{' -> Right (TP t' $ alexMove p c, (p, BraceL))
    '}' -> Right (TP t' $ alexMove p c, (p, BraceR))
    '[' -> Right (TP t' $ alexMove p c, (p, BracketL))
    ']' -> Right (TP t' $ alexMove p c, (p, BracketR))
    '=' -> Right (TP t' $ alexMove p c, (p, Equals))
    '"' -> let
      inQuotesLength = eatQuotedChars t'
      (quoted, after) = T.splitAt inQuotesLength t'
      -- below also allows strings that go to end of line without terminating quote
      tokenLength = 1 + inQuotesLength + if "\"" `T.isPrefixOf` after then 1 else 0
      (tokenStr, afterToken) = T.splitAt tokenLength t
      in Right (TP afterToken $ alexMoveMany p tokenStr, (p, TAtom $ Str $ fixQuotes quoted))
    '+' -> number True t' 1
    '-' -> number False t' 1
    _ -> if isDigit c
      then number True t 0
      else let
        (raw, after) = case T.findIndex (`elem` (" \n\r\t{}[]\"" :: String)) t' of
          Just i  -> T.splitAt (i + 1) t
          Nothing -> (t, "")
        in Right (TP after $ alexMoveMany p raw, (p, TAtom $ Str raw))
  where number positive numStart offset = do
          (afterNum, tok, len) <- applyPosn p $ eatNumber positive numStart
          return (TP afterNum $ alexMoveMany p $ T.take (len + offset) t, (p, tok))

eatQuotedChars :: T.Text -> Int
eatQuotedChars = go 0 where
  go !n t = case T.span (`notElem` ("\"\n\r" :: String)) t of
    (x, y) -> case T.stripPrefix "\"/\"" y of
      Nothing -> n + T.length x
      Just z  -> go (n + T.length x + 3) z

-- | Reads either "digits" or "digits.digits"
eatNumber :: Bool -> T.Text -> Either String (T.Text, Token, Int)
eatNumber positive t = case eatDigits t of
  (_, _, 0) -> Left "Expected digits at start of number"
  (t', whole, wholeLen) -> case T.stripSuffix "." t' of
    Nothing -> Right (t', TAtom $ Int $ if positive then whole else negate whole, wholeLen)
    Just t'' -> case eatDigits t'' of
      (_, _, 0) -> Left "Expected digits after decimal point"
      (t''', frac, fracLen) -> Right
        ( t'''
        , TAtom $ Real $ (if positive then id else negate)
          $ fromIntegral whole + fromIntegral frac / (10 ^ fracLen)
        , wholeLen + 1 + fracLen
        )

eatDigits :: T.Text -> (T.Text, Integer, Int)
eatDigits t = let
  (x, y) = T.span isDigit t
  in (y, read $ T.unpack x, T.length x) -- TODO avoid read

applyPosn :: AlexPosn -> Either String a -> Either String a
applyPosn _                 r@(Right _) = r
applyPosn (AlexPn _ ln col) (Left s)    = Left $
  "Token error at " ++ show ln ++ ":" ++ show col ++ ": " ++ s

-- \n { emit $ const Newline }
-- \{ { emit $ const BraceL }
-- \} { emit $ const BraceR }
-- \[ { emit $ const BracketL }
-- \] { emit $ const BracketR }
-- \= { emit $ const Equals }

-- (\+ | \-)? $digit+ { emit $ TAtom . Int . read . dropWhile (== '+') }
-- (\+ | \-)? $digit+ "." $digit+ { emit $ \s -> let
--   (whole, '.' : part) = break (== '.') s
--   wholeRat = fromInteger $ read $ dropWhile (== '+') whole
--   partDenom = fromInteger $ 10 ^ length part
--   partRat = fromInteger (read part) / partDenom
--   in TAtom $ Real $ wholeRat + partRat
--   }

-- $rawfirst $rawnext* { emit $ TAtom . Str . T.pack }
-- \" ([^\"\n\r] | \"\/\")* \" { emit $ TAtom . Str . fixQuotes . T.tail . T.init . T.pack }
-- \" ([^\"\n\r] | \"\/\")*    { emit $ TAtom . Str . fixQuotes . T.tail          . T.pack }

scanEither :: T.Text -> Either String [(AlexPosn, Token)]
scanEither t = let
  go !toks tp = case skipSpace tp of
    TP "" _ -> return toks
    tp'     -> do
      (tp'', ptok) <- scanToken tp'
      go (ptok : toks) tp''
  in go [] $ TP t alexStartPos

--------

fixQuotes :: T.Text -> T.Text
fixQuotes = T.intercalate (T.pack "\"") . T.splitOn (T.pack "\"/\"")

data Token
  = Newline
  | BraceL
  | BraceR
  | BracketL
  | BracketR
  | Equals
  | TAtom Atom
  | TokenError String
  deriving (Eq, Ord, Show)

scanStack :: (Monad m) => T.Text -> StackTraceT m [(AlexPosn, Token)]
scanStack = either fatal return . scanEither

scan :: T.Text -> [(AlexPosn, Token)]
scan t = let
  go tp = case skipSpace tp of
    TP "" _ -> []
    tp'@(TP _ p) -> case scanToken tp' of
      Left err           -> [(p, TokenError err)]
      Right (tp'', ptok) -> ptok : go tp''
  in go $ TP t alexStartPos
