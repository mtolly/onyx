{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.Fluent.Syntax where

import           Control.Monad   (guard, replicateM)
import           Data.Foldable   (toList)
import           Data.Functor    (void)
import           Data.Maybe      (fromMaybe, isJust, mapMaybe)
import           Data.Scientific (Scientific)
import           Data.String     (IsString (..))
import qualified Data.Text       as T
import           Data.Void       (Void)
import           Text.Megaparsec

{-
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import Text.Pretty.Simple

testme :: IO ()
testme = do
  -- txt <- TE.decodeUtf8 <$> B.readFile ("/stuff/git/fluent/fluent/test/fixtures/"<>s<>".ftl")
  txt <- TE.decodeUtf8 <$> B.readFile "/stuff/git/fluent/fluent/test/benchmarks/gecko_strings.ftl"
  pPrint $ runParser resource "test file" txt
-}

type Parser = Parsec Void T.Text

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return $ x : xs

pieces :: [Parser T.Text] -> Parser T.Text
pieces = fmap T.concat . sequence

optional' :: Parser T.Text -> Parser T.Text
optional' = fmap (fromMaybe "") . optional

--------------------------------------------------------------------------------

data ResourceItem
  = Entry Entry
  | BlankBlock T.Text
  | Junk T.Text
  deriving (Show)

resource :: Parser [ResourceItem]
resource = many
  $   try (fmap Entry      entry      )
  <|> try (fmap BlankBlock blank_block)
  <|>      fmap Junk       junk

data Entry
  = EMessage Identifier Message
  | ETerm    Identifier Term
  | ECommentLine T.Text
  deriving (Show)

entry :: Parser Entry
entry
  =   try (fmap (uncurry EMessage) (message <* line_end))
  <|> try (fmap (uncurry ETerm   ) (term    <* line_end))
  <|>      fmap ECommentLine       commentLine

data Message = Message (Maybe Pattern) [Attribute]
  deriving (Show)

message :: Parser (Identifier, Message)
message = do
  ident <- identifier
  void $ optional blank_inline
  void $ single '='
  void $ optional blank_inline
  element1 <- try (Left <$> pattern) <|> (Right <$> attribute)
  rest <- many $ try attribute
  return (ident, Message
    (case element1 of Left pat -> Just pat; _ -> Nothing)
    (case element1 of Left _ -> rest; Right attr -> attr : rest)
    )

data Term = Term Pattern [Attribute]
  deriving (Show)

term :: Parser (Identifier, Term)
term = do
  void $ single '-'
  ident <- identifier
  void $ optional blank_inline
  void $ single '='
  void $ optional blank_inline
  pat <- pattern
  attrs <- many $ try attribute
  return (ident, Term pat attrs)

commentLine :: Parser T.Text
commentLine = do
  hashes <- try "###" <|> try "##" <|> "#"
  comment <- optional $ do
    void $ single ' '
    chars <- many $ try comment_char
    return $ T.pack $ ' ' : chars
  end <- line_end
  return $ T.concat $ [hashes] <> toList comment <> [end]

comment_char :: Parser Char
comment_char = (line_end' >> fail "end of line") <|> anySingle

junk :: Parser T.Text
junk = do
  ln <- junk_line
  let azAZ c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  rest <- many $ try
    $ ((void "#" <|> void "-" <|> void (satisfy azAZ)) >> fail "end of junk")
    <|> junk_line
  return $ T.concat $ ln : rest

junk_line :: Parser T.Text
junk_line = do
  nonNewlines <- takeWhile1P (Just "non-newline") $ \c -> c /= '\n'
  end <- "\n" <|> (eof >> return "")
  guard $ not $ T.null nonNewlines && T.null end
  return $ nonNewlines <> end

data Attribute = Attribute Identifier Pattern
  deriving (Show)

attribute :: Parser Attribute
attribute = do
  void line_end
  void $ optional blank'
  void $ single '.'
  ident <- identifier
  void $ optional blank_inline
  void $ single '='
  void $ optional blank_inline
  pat <- pattern
  return $ Attribute ident pat

type Pattern = [PatternElement]

pattern :: Parser Pattern
pattern = do
  els <- many1 $ try patternElement
  -- remove common leading space from block lines
  let spaceLengths = flip mapMaybe els $ \case
        BlockText      s   -> Just $ T.length $ T.takeWhile (== ' ') s
        BlockPlaceable s _ -> Just $ T.length $ T.takeWhile (== ' ') s
        _                  -> Nothing
      dropSpaces = case spaceLengths of
        n : ns -> T.drop $ foldr min n ns
        []     -> id
  return $ flip map els $ \case
    BlockText      s   -> BlockText      (dropSpaces s)
    BlockPlaceable s x -> BlockPlaceable (dropSpaces s) x
    inline             -> inline

data PatternElement
  = InlineText T.Text
  | BlockText T.Text
  | InlinePlaceable Expression
  | BlockPlaceable T.Text Expression
  deriving (Show)

patternElement :: Parser PatternElement
patternElement = try inline_text <|> try block_text <|> try inline_placeable <|> block_placeable

inline_text :: Parser PatternElement
inline_text = InlineText <$> inline_text'

inline_text' :: Parser T.Text
inline_text' = T.pack <$> many1 (try text_char)

block_text :: Parser PatternElement
block_text = do
  void blank_block
  sp <- blank_inline
  c <- indented_char
  t <- optional inline_text'
  return $ BlockText $ sp <> T.singleton c <> fromMaybe "" t

inline_placeable :: Parser PatternElement
inline_placeable = InlinePlaceable <$> inline_placeable'

inline_placeable' :: Parser Expression
inline_placeable' = do
  void "{"
  void $ optional blank'
  e <- try (fmap ESelect selectExpression) <|> fmap EInline inlineExpression
  void $ optional blank'
  void "}"
  return e

block_placeable :: Parser PatternElement
block_placeable = do
  void blank_block
  sp <- optional blank_inline
  BlockPlaceable (fromMaybe "" sp) <$> inline_placeable'

data Expression
  = EInline InlineExpression
  | ESelect SelectExpression
  deriving (Show)

data Value
  = VString T.Text
  | VNumber Scientific
  deriving (Eq, Show)

instance IsString Value where
  fromString = VString . T.pack

data InlineExpression
  = IValue Value
  | IFunction Identifier [Argument]
  | IMessage Identifier (Maybe T.Text)
  | ITerm Identifier (Maybe T.Text) (Maybe [Argument])
  | IVariable T.Text
  | IPlaceable PatternElement
  deriving (Show)

inlineExpression :: Parser InlineExpression
inlineExpression
  =   try (fmap IValue valueLiteral)
  <|> try functionReference
  <|> try messageReference
  <|> try termReference
  <|> try variableReference
  <|> fmap IPlaceable inline_placeable

valueLiteral :: Parser Value
valueLiteral
  =   fmap VString stringLiteral
  <|> fmap VNumber numberLiteralScientific

stringLiteral :: Parser T.Text
stringLiteral = do
  void "\""
  cs <- many $ try quoted_char
  void "\""
  return $ T.pack cs

numberLiteralScientific :: Parser Scientific
numberLiteralScientific = read . T.unpack <$> numberLiteral

numberLiteral :: Parser T.Text
numberLiteral = pieces
  [ optional' "-"
  , digits
  , optional' $ pieces
    [ "."
    , digits
    ]
  ]

functionReference :: Parser InlineExpression
functionReference = do
  ident <- identifier
  args <- callArguments
  return $ IFunction ident args

messageReference :: Parser InlineExpression
messageReference = do
  ident <- identifier
  attr <- optional attributeAccessor
  return $ IMessage ident attr

termReference :: Parser InlineExpression
termReference = do
  void "-"
  ident <- identifier
  attr <- optional attributeAccessor
  args <- optional $ try callArguments
  return $ ITerm ident attr args

variableReference :: Parser InlineExpression
variableReference = do
  void "$"
  fmap IVariable identifier

attributeAccessor :: Parser T.Text
attributeAccessor = "." >> identifier

data Argument
  = NamedArgument Identifier Value
  | InlineArgument InlineExpression
  deriving (Show)

callArguments :: Parser [Argument]
callArguments = do
  void $ optional blank'
  void "("
  void $ optional blank'
  args <- argument_list
  void $ optional blank'
  void ")"
  return args

argument_list :: Parser [Argument]
argument_list = do
  notFinal <- many $ try $ do
    arg <- argument
    void $ optional blank'
    void ","
    void $ optional blank'
    return arg
  final <- optional argument
  return $ notFinal <> toList final

argument :: Parser Argument
argument = try namedArgument <|> fmap InlineArgument inlineExpression

namedArgument :: Parser Argument
namedArgument = do
  ident <- identifier
  void $ optional blank'
  void ":"
  void $ optional blank'
  x <- valueLiteral
  return $ NamedArgument ident x

data SelectExpression = SelectExpression InlineExpression VariantList
  deriving (Show)

selectExpression :: Parser SelectExpression
selectExpression = do
  e <- inlineExpression
  void $ optional blank'
  void "->"
  void $ optional blank_inline
  vl <- variant_list
  return $ SelectExpression e vl

data VariantList = VariantList [Variant] Variant [Variant]
  deriving (Show)

variant_list :: Parser VariantList
variant_list = do
  vars1 <- many $ try variant
  dv <- defaultVariant
  vars2 <- many $ try variant
  void line_end
  return $ VariantList vars1 dv vars2

data Variant = Variant (Either Scientific T.Text) Pattern
  deriving (Show)

variant :: Parser Variant
variant = do
  void line_end
  void $ optional blank'
  k <- variantKey
  void $ optional blank_inline
  pat <- pattern
  return $ Variant k pat

defaultVariant :: Parser Variant
defaultVariant = do
  void line_end
  void $ optional blank'
  void "*"
  k <- variantKey
  void $ optional blank_inline
  pat <- pattern
  return $ Variant k pat

variantKey :: Parser (Either Scientific T.Text)
variantKey = do
  void "["
  void $ optional blank'
  x <- fmap Left numberLiteralScientific <|> fmap Right identifier
  void $ optional blank'
  void "]"
  return x

type Identifier = T.Text

identifier :: Parser Identifier
identifier = do
  x <- satisfy $ \c -> ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
  xs <- takeWhileP (Just "identifier post-first character") $ \c -> or
    [ 'a' <= c && c <= 'z'
    , 'A' <= c && c <= 'Z'
    , '0' <= c && c <= '9'
    , c == '_'
    , c == '-'
    ]
  return $ T.cons x xs

--------------------------------------------------------------------------------

text_char :: Parser Char
text_char
  = (("{" <|> "}" <|> line_end) >> fail "not text_char")
  <|> anySingle

indented_char :: Parser Char
indented_char
  = (("[" <|> "*" <|> ".") >> fail "not indented_char")
  <|> text_char

quoted_char :: Parser Char
quoted_char = let
  normal
    =   ("\""     >> fail "not quoted_char")
    <|> ("\\"     >> fail "not quoted_char")
    <|> (line_end >> fail "not quoted_char")
    <|> anySingle
  hex = satisfy $ \c -> or
    [ '0' <= c && c <= '9'
    , 'a' <= c && c <= 'f'
    , 'A' <= c && c <= 'F'
    ]
  hexes n = do
    ns <- replicateM n hex
    return (toEnum $ read $ "0x" <> ns :: Char)
  special_escape
    = try ("\\\"" >> return '"' )
    <|>   ("\\\\" >> return '\\')
  unicode_escape_4 = do
    void "\\u"
    hexes 4
  unicode_escape_6 = do
    void "\\U"
    hexes 6
  in try normal <|> try special_escape <|> try unicode_escape_4 <|> unicode_escape_6

digits :: Parser T.Text
digits = takeWhile1P (Just "digit") $ \c -> '0' <= c && c <= '9'

blank_inline :: Parser T.Text
blank_inline = takeWhile1P (Just "space") (== ' ')

line_end :: Parser T.Text
line_end = "\r\n" <|> "\n" <|> (eof >> return "")

line_end' :: Parser T.Text
line_end' = "\r\n" <|> "\n"

blank_block :: Parser T.Text
-- probably a better way to do this
blank_block = fmap (T.concat . concat) $ many1 $ try $ do
  x <- optional blank_inline
  y <- line_end
  guard $ isJust x || not (T.null y)
  return $ toList x <> [y]

blank :: Parser ()
blank = void $ many1 $ try $ blank_inline <|> line_end

blank' :: Parser ()
blank' = void $ many1 $ try $ blank_inline <|> line_end'
