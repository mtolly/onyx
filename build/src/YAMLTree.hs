{- |
A convention for YAML files to include references to other files.

If an object has the key \"file-include\" paired with a relative filename
(or list of filenames), those are YAML files whose pairs will be inserted into the object.
The values of the existing object (the one doing the including) take priority in a collision.

If an object has some other key starting with \"file-\" paired with a relative filename
(or list of filenames), that filename will be kept accurate regardless of what other
chain of files the object gets included in.

For example, @A.yml@ includes @B/C.yml@ which includes a reference to @D.png@.
The reference to @D.png@ is actually @B/D.png@ because it is relative to @B/C.yml@.
So, when the contents of @B/C.yml@ get glued into @A.yml@,
the filename will be edited into @B/D.png@.
-}
{-# LANGUAGE OverloadedStrings #-}
module YAMLTree (readYAMLTree) where

import qualified Data.Aeson as A
import qualified Data.Yaml as Y
import qualified Data.Text as T
import Control.Applicative ((<$>), (<|>))
import qualified Data.Traversable as Tr
import qualified Data.HashMap.Strict as M
import System.FilePath ((</>), takeDirectory)
import Data.List (foldl')

stringOrStrings :: Y.Value -> A.Result (Either String [String])
stringOrStrings v =
  fmap Left (A.fromJSON v) <|> fmap Right (A.fromJSON v)

readYAMLTree :: FilePath -> IO Y.Value
readYAMLTree f = do
  Just val <- Y.decodeFile f
  go val
  where dir = takeDirectory f
        go :: Y.Value -> IO Y.Value
        go v = case v of
          Y.Object o -> goPairs M.empty $ M.toList o
          Y.Array a -> Y.Array <$> Tr.mapM go a
          _ -> return v
        goPairs :: Y.Object -> [(T.Text, Y.Value)] -> IO Y.Value
        goPairs o [] = return $ Y.Object o
        goPairs o ((k, v) : rest) = case T.stripPrefix "file-" k of
          Just "include" -> case stringOrStrings v of
            A.Success e -> do
              let files = either (: []) id e
              vs <- mapM (readYAMLTree . (dir </>)) files
              case mapM A.fromJSON vs of
                A.Success objs -> goPairs (foldl' M.union o objs) rest
                A.Error s -> fail s
            A.Error s -> fail s
          Just _ -> case stringOrStrings v of
            A.Success e -> let
              v' = case e of
                Left  s  -> A.toJSON $ dir </> s
                Right ss -> A.toJSON $ map (dir </>) ss
              in goPairs (M.insert k v' o) rest
            A.Error s -> fail s
          _ -> go v >>= \v' -> goPairs (M.insert k v' o) rest
