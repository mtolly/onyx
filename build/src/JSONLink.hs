{-# LANGUAGE OverloadedStrings #-}
module JSONLink where

import qualified Data.Aeson as A
import qualified Data.Text as T
import Control.Applicative ((<$>))
import qualified Data.Traversable as Tr
import qualified Data.HashMap.Strict as M
import System.FilePath ((</>), takeDirectory)
import qualified Data.Yaml as Y

readJSON :: FilePath -> IO A.Value
readJSON f = do
  let dir = takeDirectory f
  Just val <- Y.decodeFile f
  let go :: A.Value -> IO A.Value
      go v = case v of
        A.Object o -> goPairs M.empty $ M.toList o
        A.Array a -> A.Array <$> Tr.mapM go a
        _ -> return v
      goPairs :: A.Object -> [(T.Text, A.Value)] -> IO A.Value
      goPairs o [] = return $ A.Object o
      goPairs o ((k, v) : rest) = if k == "file-include"
        then case v of
          A.String t -> do
            v' <- readJSON $ dir </> T.unpack t
            case v' of
              A.Object o' -> goPairs (M.union o o') rest
              _ -> error "included non-object"
          _ -> error "included non-string"
        else if "file-" `T.isPrefixOf` k
          then let
            v' = case v of
              A.String t -> A.String $ T.pack $ dir </> T.unpack t
              _ -> error "file is not string"
            in goPairs (M.insert k v' o) rest
          else go v >>= \v' -> goPairs (M.insert k v' o) rest
  go val
