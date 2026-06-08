{- |
(MT: mostly unedited claude output)

Parses Unity TextMesh Pro \/ UI Toolkit rich text markup into a flat sequence
of chunks: runs of untagged text interleaved with markup tags.

This is a purely syntactic pass. It does /not/ enforce nesting, match opening
tags with closing tags, validate tag names, or interpret attribute values
(pixels, em, percentages, hex colors, ...). Those belong in a later semantic
layer built on top of 'parseRichText'.

The TMP syntax is deliberately loose, and we mirror that:

  * @\<b\>@                          -- a bare tag
  * @\<color=red\>@                  -- a tag with an inline value
  * @\<color=\"red\"\>@              -- the value may be quoted
  * @\< color = red \>@              -- whitespace is allowed around everything
  * @\<sprite name=\"x\" index=3\>@  -- tags may carry named attributes
  * @\</color\>@                     -- closing tag
  * @\<#0000FF\>@                    -- color shorthand (equivalent to @\<color=#0000FF\>@)

Anything that opens with @\<@ but does not parse as a well-formed tag is kept
verbatim as plain text, so 'parseRichText' never fails on arbitrary input.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Util.Text.RichText
( RichText(..)
, Tag(..)
, parseRichText
) where

import           Control.Applicative ((<|>))
import           Control.Monad       (void)
import           Data.Char           (isAlpha, isAlphaNum, isHexDigit, isSpace)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Void           (Void)
import           Text.Megaparsec     (Parsec, eof, many, option, optional,
                                      runParser, satisfy, single, takeWhile1P,
                                      takeWhileP, try)

-- | A single chunk of parsed rich text.
data RichText
  = Plain  T.Text -- ^ A run of untagged text.
  | Markup Tag    -- ^ A single markup tag.
  deriving (Eq, Show)

-- | A markup tag, e.g. @\<color=red\>@ or @\</b\>@.
data Tag = Tag
  { closing    :: Bool
    -- ^ 'True' for closing tags such as @\</color\>@.
  , name       :: T.Text
    -- ^ The tag name, lower-cased. The @\<#rrggbb\>@ color shorthand is
    -- normalized to the name @\"color\"@ with the hex string as 'value'.
  , value      :: Maybe T.Text
    -- ^ The inline value attached to the name, e.g. @red@ in @\<color=red\>@.
    -- Quotes are stripped. 'Nothing' if the tag has no @=value@.
  , attributes :: [(T.Text, T.Text)]
    -- ^ Named attributes, e.g. @[(\"name\", \"x\"), (\"index\", \"3\")]@ for
    -- @\<sprite name=\"x\" index=3\>@. Attribute names are lower-cased and an
    -- attribute with no value yields an empty string.
  } deriving (Eq, Show)

type Parser = Parsec Void T.Text

-- | Parse rich text into a flat list of chunks. Never fails: input that is not
-- valid markup comes back as 'Plain' text.
parseRichText :: T.Text -> [RichText]
parseRichText t = case runParser (chunks <* eof) "" t of
  Right rs -> rs
  Left  _  -> [Plain t] -- unreachable: 'chunks' consumes any input
  where chunks = coalesce <$> many richChunk

-- | A tag if one parses here, otherwise a run of plain text. Always consumes at
-- least one character so @many@ terminates.
richChunk :: Parser RichText
richChunk
  =   Markup <$> try tag
  <|> Plain  <$> plainRun

-- | A maximal run of non-@\<@ characters, or a single stray @\<@ that did not
-- begin a valid tag.
plainRun :: Parser T.Text
plainRun
  =   takeWhile1P Nothing (/= '<')
  <|> (T.singleton <$> single '<')

tag :: Parser Tag
tag = do
  void $ single '<'
  sc
  closing <- option False $ True <$ (single '/' >> sc)
  (name, val) <- hashShorthand <|> namedHead
  attrs <- many attribute
  sc
  void $ single '>'
  return Tag
    { closing    = closing
    , name       = name
    , value      = val
    , attributes = attrs
    }

-- | The @\<#rrggbb\>@ / @\<#rgb\>@ / @\<#rrggbbaa\>@ color shorthand. Reported
-- as the @color@ tag, since TMP treats it as exactly that.
hashShorthand :: Parser (T.Text, Maybe T.Text)
hashShorthand = do
  void $ single '#'
  hex <- takeWhile1P Nothing isHexDigit
  return ("color", Just $ T.cons '#' hex)

-- | A tag name followed by an optional inline @=value@.
namedHead :: Parser (T.Text, Maybe T.Text)
namedHead = do
  name <- identifier
  val  <- optional . try $ equals *> value
  return (T.toLower name, val)

-- | A whitespace-separated named attribute, e.g. @name=\"x\"@ or a bare flag.
attribute :: Parser (T.Text, T.Text)
attribute = try $ do
  space1
  name <- identifier
  val  <- optional . try $ equals *> value
  return (T.toLower name, fromMaybe "" val)

-- | An attribute\/value, either double-quoted (may contain spaces) or a bare
-- token running up to the next whitespace or @\>@.
value :: Parser T.Text
value
  =   (single '"' *> takeWhileP Nothing (/= '"') <* single '"')
  <|> takeWhile1P Nothing (\c -> not (isSpace c) && c /= '>')

-- | A tag or attribute name: a letter followed by letters, digits, @-@ or @_@
-- (covers @font-weight@, @line-height@, @margin-left@, ...).
identifier :: Parser T.Text
identifier = do
  c  <- satisfy isAlpha
  cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '-' || x == '_')
  return $ T.cons c cs

-- | An @=@ surrounded by optional whitespace, as in @< color = red >@.
equals :: Parser ()
equals = sc *> single '=' *> sc

-- | Zero or more whitespace characters.
sc :: Parser ()
sc = void $ takeWhileP Nothing isSpace

-- | One or more whitespace characters.
space1 :: Parser ()
space1 = void $ takeWhile1P Nothing isSpace

-- | Merge adjacent 'Plain' chunks into single runs.
coalesce :: [RichText] -> [RichText]
coalesce (Plain a : Plain b : rest) = coalesce $ Plain (a <> b) : rest
coalesce (x : rest)                 = x : coalesce rest
coalesce []                         = []
