{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Reaper.Base where

import           Control.Monad.IO.Class    (MonadIO (liftIO))
import qualified Data.ByteString           as B
import           Data.List                 (foldl')
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Prettyprinter             ((<+>))
import qualified Prettyprinter             as PP
import           Prettyprinter.Render.Text (renderStrict)

data Element = Element T.Text [T.Text] (Maybe [Element])
  deriving (Eq, Ord, Show)

writeRPP :: (MonadIO m) => FilePath -> Element -> m ()
writeRPP path = let
  opts = PP.defaultLayoutOptions
    { PP.layoutPageWidth = PP.Unbounded
    }
  -- TODO is UTF-8 correct
  in liftIO . B.writeFile path . TE.encodeUtf8 . renderStrict . PP.layoutPretty opts . showElement

showElement :: Element -> PP.Doc ()
showElement (Element k ks Nothing) = foldl' (<+>) (showAtom k) (map showAtom ks)
showElement (Element k ks (Just sub)) = let
  start = "<" <> foldl' (<+>) (showAtom k) (map showAtom ks)
  sublines = PP.nest 2 $ PP.vcat $ map showElement sub
  end = ">"
  in PP.vcat [start, sublines, end]

showAtom :: T.Text -> PP.Doc ()
showAtom s = PP.pretty $ case (T.any (== '"') s, T.any (== '\'') s, T.any (== ' ') s) of
  (True, True, _)       -> "`" <> T.map removeTick s <> "`"
  (False, False, False) -> s
  (False, _, True)      -> wrap '"'
  (False, True, False)  -> wrap '"'
  (True, False, _)      -> wrap '\''
  where wrap c = T.concat [T.singleton c, s, T.singleton c]
        removeTick = \case '`' -> '\''; c -> c
