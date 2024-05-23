{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Text.Fluent.Logic where

import           Control.Monad          (guard)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString        as B
import qualified Data.HashMap.Strict    as HM
import           Data.Maybe             (fromMaybe, mapMaybe)
import           Data.Scientific
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           System.Info            (os)
import           Text.Fluent.Syntax
import           Text.Megaparsec
import           Text.Read              (readMaybe)

data Fluent = Fluent
  { messages :: HM.HashMap Identifier Message
  , terms    :: HM.HashMap Identifier Term
  } deriving (Show)

loadFTL :: (MonadIO m, MonadFail m) => FilePath -> m Fluent
loadFTL f = do
  t <- liftIO $ TE.decodeUtf8 <$> B.readFile f
  case parse resource f t of
    Left  err -> fail $ show err
    Right res -> return Fluent
      { messages = HM.fromList [ (i, x) | Entry (EMessage i x) <- res ]
      , terms    = HM.fromList [ (i, x) | Entry (ETerm    i x) <- res ]
      }

getMessage :: Identifier -> HM.HashMap Identifier Value -> Fluent -> Maybe (T.Text, [(Identifier, T.Text)])
getMessage k args fluent = HM.lookup k fluent.messages >>= \(Message pat attrs) -> let
  result = runPattern args (fromMaybe [] pat) fluent
  attrResults = do
    Attribute attr attrPat <- attrs
    return (attr, runPattern args attrPat fluent)
  in Just (result, attrResults)

runPattern :: HM.HashMap Identifier Value -> Pattern -> Fluent -> T.Text
runPattern args pat fluent = let
  str = T.concat $ concat $ flip map pat $ \case
    InlineText      t   -> [t]
    BlockText       t   -> ["\n", t]
    InlinePlaceable   x -> [runExpression args x fluent]
    BlockPlaceable  t x -> ["\n", t, runExpression args x fluent]
  in case pat of
    -- remove first newline before block (text/placeable) if it's first pattern element
    BlockText     {} : _ -> T.drop 1 str
    BlockPlaceable{} : _ -> T.drop 1 str
    _                    -> str

runExpression :: HM.HashMap Identifier Value -> Expression -> Fluent -> T.Text
runExpression args x fluent = case runExpressionValue args x fluent of
  VString t -> t
  VNumber n -> numberToString n

runExpressionValue :: HM.HashMap Identifier Value -> Expression -> Fluent -> Value
runExpressionValue args x fluent = case x of
  EInline ie -> case ie of
    IValue v -> v
    IFunction fun _funArgs -> case fun of
      "PLATFORM" -> case os of
        "mingw32" -> "windows"
        "darwin"  -> "macos"
        "linux"   -> "linux"
        _         -> "other"
      -- TODO: NUMBER, DATETIME
      _ -> VString $ "{" <> fun <> "()" <> "}"
    IMessage msg maybeAttr -> VString $ case getMessage msg args fluent of
      Nothing -> "{" <> msg <> "}"
      Just (res, attrs) -> case maybeAttr of
        Nothing -> res
        Just attr -> case lookup attr attrs of
          Nothing        -> "{" <> msg <> "." <> attr <> "}"
          Just attrValue -> attrValue
    ITerm t maybeAttr termArgs -> VString $ case HM.lookup t fluent.terms of
      Nothing -> "{-" <> t <> "}"
      Just (Term pat attrs) -> let
        fullArgs = case termArgs of
          Nothing -> args
          Just newArgs -> let
            newArgs' = HM.fromList $ flip mapMaybe newArgs $ \case
              NamedArgument name arg -> Just (name, arg)
              InlineArgument _       -> Nothing -- parses but nonsensical
            -- union prefers left if both have a key
            in HM.union newArgs' args
        in case maybeAttr of
          Nothing -> runPattern fullArgs pat fluent
          Just attr -> case [ a | Attribute k a <- attrs, k == attr ] of
            []    -> "{-" <> t <> "." <> attr <> "}"
            a : _ -> runPattern fullArgs a fluent
    IVariable v -> case HM.lookup v args of
      Nothing  -> VString $ "{$" <> v <> "}"
      Just val -> val
    IPlaceable pe -> VString $ runPattern args [pe] fluent
  ESelect (SelectExpression ie (VariantList vs1 vdef vs2)) -> let
    obj = runExpressionValue args (EInline ie) fluent
    objNumber = case obj of
      VNumber n -> Just n
      VString s -> readMaybe $ T.unpack s
    variants = vs1 <> [vdef] <> vs2
    matchVariant (Variant matcher pat) = do
      guard $ case matcher of
        Left  num   -> objNumber == Just num
        Right ident -> obj == VString ident || case ident of
          -- TODO plural categories, per locale
          _ -> False
      Just $ runPattern args pat fluent
    in VString $ case mapMaybe matchVariant variants of
      [] -> case vdef of
        Variant _ pat -> runPattern args pat fluent
      res : _ -> res

-- TODO any recursive references should be detected and stopped with {???}

-- TODO format options
numberToString :: Scientific -> T.Text
numberToString n = let
  x = T.pack $ formatScientific Fixed Nothing n
  in fromMaybe x $ T.stripSuffix ".0" x
