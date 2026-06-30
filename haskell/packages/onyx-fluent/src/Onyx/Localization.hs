{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module Onyx.Localization
( L, Identifier, MessageOutput(..)
, getAttribute
, l10n, l10nArgs, l10nFull
, vString, vNumber, vBool
, Language(..)
, loadLanguages
, loadLanguage
) where

import           Control.Monad       (forM)
import           Data.Aeson.Types    (prependFailure, typeMismatch)
import           Data.Char           (toLower)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (catMaybes, fromMaybe)
import           Data.Scientific     (Scientific)
import qualified Data.Text           as T
import           Data.Yaml           ((.:))
import qualified Data.Yaml           as Y
import           System.Directory    (listDirectory)
import           System.FilePath     (takeExtension, (</>))
import           Text.Fluent.Locale
import           Text.Fluent.Logic
import           Text.Fluent.Syntax

type L = Fluent

l10n :: (?loc :: L) => Identifier -> T.Text
l10n k = l10nArgs k []

l10nArgs :: (?loc :: L) => Identifier -> [(Identifier, Value)] -> T.Text
l10nArgs k args = (l10nFull k args).output

l10nFull :: (?loc :: L) => Identifier -> [(Identifier, Value)] -> MessageOutput
l10nFull k args = getMessage k (HM.fromList args) ?loc

vString :: T.Text -> Value
vString = VString

vNumber :: Scientific -> Value
vNumber = VNumber

vBool :: Bool -> Value
vBool b = vString $ if b then "true" else "false"

data Language = Language
  { code   :: T.Text
  , name   :: T.Text
  , nameEn :: T.Text
  , plural :: T.Text
  , fluent :: [T.Text]
  } deriving (Show)

instance Y.FromJSON Language where
  parseJSON (Y.Object v) = Language
    <$> v .: "code"
    <*> v .: "name"
    <*> v .: "name-en"
    <*> v .: "plural"
    <*> v .: "fluent"
  parseJSON invalid = prependFailure "parsing language file failed, " $ typeMismatch "Object" invalid

loadLanguages :: FilePath -> IO [Language]
loadLanguages root = fmap catMaybes $ listDirectory root >>= mapM \f ->
  case map toLower $ takeExtension f of
    ".yml" -> Just <$> Y.decodeFileThrow (root </> f)
    _      -> return Nothing

loadLanguage :: FilePath -> Language -> IO L
loadLanguage root lang = do
  ftls <- forM lang.fluent \f -> loadFTL $ root </> T.unpack f
  return Fluent
    { ftl    = mconcat ftls
    , plural = fromMaybe (const PluralOther) $ HM.lookup lang.plural pluralRules
    }
