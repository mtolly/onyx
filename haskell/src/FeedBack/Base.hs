{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module FeedBack.Base where

import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Micro, Milli)
import qualified Data.HashMap.Strict              as Map
import qualified Data.Text                        as T
import qualified Numeric.NonNegative.Wrapper      as NN

-- import qualified Data.Attoparsec.Text as P
-- import Data.Attoparsec.Text (Parser)
-- import Control.Applicative ((<|>), many)
-- import Control.Monad.Trans.StackTrace (inside, fatal, StackTraceT)

data Atom
  = Int !Integer
  | Real !Rational
  | Str !T.Text
  deriving (Eq, Ord, Show)

type RawSection = (T.Text, RawLines)
type RawLines = [(Atom, [Atom])]

type Ticks = NN.Integer

data Event t
  = TimeSig Integer Integer
  | BPM Milli
  | Anchor Micro
  | Event T.Text
  | Note Integer t
  | Special Integer t
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Chart t = Chart
  { chartSong   :: Map.HashMap T.Text Atom
  , chartTracks :: Map.HashMap T.Text (RTB.T t (Event t))
  } deriving (Eq, Show)

instance Functor Chart where
  fmap f (Chart song trks) =
    Chart song $ flip fmap trks $ RTB.mapTime f . RTB.mapBody (fmap f)

--------------------

{-

token :: Parser a -> Parser a
token = (P.skipWhile P.isHorizontalSpace >>)

pFile :: Parser [RawSection]
pFile = P.skipSpace *> many pSection <* P.skipSpace <* P.endOfInput

pSection :: Parser RawSection
pSection = do

  _ <- token $ P.char '['
  name <- token pTrackName
  _ <- token $ P.char ']'
  P.skipSpace

  _ <- token $ P.char '{'
  P.skipSpace
  contents <- many pLine
  P.skipSpace
  _ <- token $ P.char '}'
  P.skipSpace

  return (name, contents)

pTrackName :: Parser T.Text
pTrackName = T.unwords <$> P.many1 (token pStr)

pStr :: Parser T.Text
pStr
  = do
    c <- P.satisfy $ P.notInClass " \t\r\n{}[]\"0123456789"
    cs <- P.takeWhile $ P.notInClass " \t\r\n{}[]\""
    return $ T.cons c cs
  <|> do
    _ <- P.char '"'
    str <- many
      $ do
        P.satisfy $ P.notInClass "\"\n\r"
      <|> do
        _ <- P.string "\"/\""
        return '"'
    _ <- P.char '"' <|> do
      P.peekChar >>= \case
        Just '\n' -> return '"'
        Nothing   -> return '"'
        Just _    -> mempty
    return $ T.pack str

pInt :: Parser Integer
pInt = P.signed P.decimal

pReal :: Parser Rational
pReal = P.rational

pAtom :: Parser Atom
pAtom
  =   fmap Int  pInt
  <|> fmap Real pReal
  <|> fmap Str  pStr
  -- these are just so we don't fail on parsing "E [start]"
  -- (this inserts extra spaces but whatever)
  <|> (P.char '[' >> return (Str "["))
  <|> (P.char ']' >> return (Str "]"))

pLine :: Parser (Atom, [Atom])
pLine = do
  x <- token pAtom
  _ <- token $ P.char '='
  xs <- P.many1 $ token pAtom
  P.skipMany1 P.endOfLine
  return (x, xs)

scanParseStack :: (Monad m) => T.Text -> StackTraceT m (T.Text, [RawSection])
scanParseStack = go . P.parse pFile where
  go = \case
    P.Fail _ layers err -> foldr inside (fatal err) layers
    P.Partial cont      -> go $ cont ""
    P.Done s res        -> return (s, res)

-}
