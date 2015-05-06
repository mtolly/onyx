module Main where

import System.Environment
import Control.Monad
import qualified Parser
import qualified Parser.File
import qualified Sound.MIDI.File.Load as Load

main :: IO ()
main = do
  fs <- getArgs
  forM_ fs $ \f -> do
    Load.fromFile f >>= Parser.printParserIO . Parser.File.readMIDIFile >> return ()
