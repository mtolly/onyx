{-# LANGUAGE LambdaCase #-}
module Main where

import Config
import Control.Monad.Trans.Reader
import StackTrace
import System.Environment (getArgs)
import YAMLTree

main :: IO ()
main = getArgs >>= \case
  [yml] -> do
    val <- readYAMLTree yml
    songYaml <- runReaderT (printStackTraceIO traceJSON) val
    print (songYaml :: SongYaml)
  _ -> error "one arg plz (song.yml)"
