{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
module Onyx.Harmonix.DTA.Scan (scan, scanEither, scanStack, Token(..), AlexPosn(..)) where

import           Control.Monad.Trans.State.Strict
import           Data.Bifunctor                   (first)
import           Data.Char                        (isAlpha, isSpace)
import           Data.Foldable                    (foldl', toList)
import           Data.Functor                     (void)
import           Data.Int
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as T
import           Onyx.StackTrace
import           Text.Read                        (readMaybe)

data Token s
  = Int Int32
  | Float Float
  | Var s
  | Sym s
  | Unhandled
  | IfDef
  | Else
  | EndIf
  | LParen
  | RParen
  | LBrace
  | RBrace
  | String s
  | LBracket
  | RBracket
  | Define
  | Include
  | Merge
  | IfNDef
  | Autorun
  | Undef
  deriving (Eq, Ord, Show, Read)

scanEither :: T.Text -> Either String [(AlexPosn, Token T.Text)]
scanEither t = first showError $ evalState parseAllTokens (alexStartPos, t)
  where showError (AlexPn _ line column, s) = concat
          [ "DTA token error at line " ++ (show line) ++ ", column " ++ (show column) ++ ": "
          , s
          ]

scan :: T.Text -> [(AlexPosn, Token T.Text)]
scan = either error id . scanEither

scanStack :: (Monad m) => T.Text -> StackTraceT m [(AlexPosn, Token T.Text)]
scanStack = either fatal return . scanEither

-- (position type taken from alex)

-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of characters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.

data AlexPosn = AlexPn !Int !Int !Int
  deriving (Eq, Show, Ord)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (c+alex_tab_size-((c-1) `mod` alex_tab_size))
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)

alex_tab_size :: Int
alex_tab_size = 8

---------------------------------

type Parse = State (AlexPosn, T.Text)

data TokenResult
  = EOF
  | ParseError AlexPosn String
  | ParseSuccess AlexPosn (Token T.Text)

parseAllTokens :: Parse (Either (AlexPosn, String) [(AlexPosn, Token T.Text)])
parseAllTokens = go Seq.empty where
  go cur = parseToken >>= \case
    EOF              -> return $ Right $ toList cur
    ParseError   p s -> return $ Left (p, s)
    ParseSuccess p t -> go $ cur Seq.|> (p, t)

parseToken :: Parse TokenResult
parseToken = do
  skipWhile isSpace
  (p, t) <- get
  case T.uncons t of
    Nothing -> return EOF
    Just (c, t') -> do
      let consumeFirst = put (alexMove p c, t')
      if  | "/*" `T.isPrefixOf` t -> blockComment >>= \case
            True  -> parseToken
            False -> return $ ParseError p "Block comment doesn't have an end (*/)"
          | "//" `T.isPrefixOf` t || ";" `T.isPrefixOf` t -> do
            skipWhile (/= '\n')
            parseToken
          | "#" `T.isPrefixOf` t -> do
            consumeFirst
            pp <- consumeWhile isAlpha
            case pp of
              "ifdef"   -> return $ ParseSuccess p IfDef
              "else"    -> return $ ParseSuccess p Else
              "endif"   -> return $ ParseSuccess p EndIf
              "define"  -> return $ ParseSuccess p Define
              "include" -> return $ ParseSuccess p Include
              "merge"   -> return $ ParseSuccess p Merge
              "ifndef"  -> return $ ParseSuccess p IfNDef
              "autorun" -> return $ ParseSuccess p Autorun
              "undef"   -> return $ ParseSuccess p Undef
              _         -> return $ ParseError p $ "Unrecognized preprocessor command: " <> show pp
          | otherwise -> case c of
            '('  -> consumeFirst >> return (ParseSuccess p LParen)
            ')'  -> consumeFirst >> return (ParseSuccess p RParen)
            '{'  -> consumeFirst >> return (ParseSuccess p LBrace)
            '}'  -> consumeFirst >> return (ParseSuccess p RBrace)
            '['  -> consumeFirst >> return (ParseSuccess p LBracket)
            ']'  -> consumeFirst >> return (ParseSuccess p RBracket)
            '$'  -> do
              consumeFirst
              x <- consumeThing
              return $ ParseSuccess p $ Var x
            '"'  -> consumeFirst >> stringLiteral >>= return . \case
              Just s  -> ParseSuccess p $ String s
              Nothing -> ParseError p "String literal doesn't have an end quote (\")"
            '\'' -> consumeFirst >> quotedSymbol >>= return . \case
              Just s  -> ParseSuccess p $ Sym s
              Nothing -> ParseError p "Quoted symbol literal doesn't have an end quote (')"
            _    -> do
              x <- consumeThing
              -- haskell Int32/Float read instances:
              -- * support '-' but not '+'
              -- * support 'e' notation in Float only
              -- * support '0x' notation (and '0o') in Int32 only
              -- * does not support leading dot floats (seen in Amplitude PS3)
              let maybeNumString = case dropWhile (== '+') $ T.unpack x of
                    leadingDot@('.' : _) -> '0' : leadingDot
                    str                  -> str
              return $ ParseSuccess p $ case x of
                "kDataUnhandled" -> Unhandled
                _                -> case readMaybe maybeNumString of
                  Just int -> Int int
                  Nothing  -> case readMaybe maybeNumString of
                    Just float -> Float float
                    Nothing    -> Sym x

-- Returns Nothing if string not terminated correctly
stringLiteral :: Parse (Maybe T.Text)
stringLiteral = do
  (p, orig) <- get
  let go !n !t = case T.uncons t of
        Just ('\\', t') -> if T.null t'
          then Nothing -- backslash then eof, thus unterminated
          else go (n + 2) (T.drop 1 t')
        Just ('\"', _ ) -> Just n
        Just (_   , t') -> go (n + 1) t'
        Nothing         -> Nothing -- unterminated
  case go 0 orig of
    Nothing -> return Nothing
    Just len -> case T.splitAt (len + 1) orig of
      (x, y) -> let
        newPosn = foldl' alexMove p $ T.unpack x
        afterEscapes
          = T.replace "\\n" "\n"
          $ T.replace "\\q" "\""
          $ T.dropEnd 1 x
        in put (newPosn, y) >> return (Just afterEscapes)

-- Returns Nothing if symbol not terminated correctly
quotedSymbol :: Parse (Maybe T.Text)
quotedSymbol = do
  (p, orig) <- get
  let go !n !t = case T.uncons t of
        Just ('\\', t') -> if T.null t'
          then Nothing -- backslash then eof, thus unterminated
          else go (n + 2) (T.drop 1 t')
        Just ('\'', _ ) -> Just n
        Just (_   , t') -> go (n + 1) t'
        Nothing         -> Nothing -- unterminated
  case go 0 orig of
    Nothing -> return Nothing
    Just len -> case T.splitAt (len + 1) orig of
      (x, y) -> let
        newPosn = foldl' alexMove p $ T.unpack x
        afterEscapes
          = T.replace "\\'" "'"
          $ T.dropEnd 1 x
        in put (newPosn, y) >> return (Just afterEscapes)

-- Returns True if comment is terminated correctly
blockComment :: Parse Bool
blockComment = do
  skipWhile (/= '*')
  (_, t) <- get
  if T.null t
    then return False
    else if "*/" `T.isPrefixOf` t
      then consumeChar >> consumeChar >> return True
      else consumeChar >> blockComment

-- "identifier" logic for unquoted symbols, numbers, and variable names
consumeThing :: Parse T.Text
consumeThing = do
  (p, orig) <- get
  let go !n !t = case T.uncons t of
        Just ('/', t') | "*" `T.isPrefixOf` t'                -> n
        Just (c  , t') | not $ isSpace c || T.elem c "()[]{}" -> go (n + 1) t'
        _                                                     -> n
      len = go 0 orig
  case T.splitAt len orig of
    (x, y) -> let
      newPosn = foldl' alexMove p $ T.unpack x
      in put (newPosn, y) >> return x

consumeChar :: Parse (Maybe Char)
consumeChar = do
  (p, t) <- get
  case T.uncons t of
    Just (c, t') -> do
      put (alexMove p c, t')
      return $ Just c
    Nothing -> return Nothing

skipWhile :: (Char -> Bool) -> Parse ()
skipWhile = void . consumeWhile

consumeWhile :: (Char -> Bool) -> Parse T.Text
consumeWhile f = do
  (p, t) <- get
  case T.span f t of
    (x, y) -> let
      newPosn = foldl' alexMove p $ T.unpack x
      in put (newPosn, y) >> return x
