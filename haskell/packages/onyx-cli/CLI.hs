module Main where

import           Control.Exception            (displayException)
import           Control.Monad                (unless)
import           Control.Monad.Trans.Resource (runResourceT)
import           Onyx.CommandLine
import           Onyx.StackTrace
import           System.Environment           (getArgs)
import           System.Exit
import           System.IO                    (hPutStr, hPutStrLn, stderr)

main :: IO ()
main = do
  argv <- getArgs
  res <- logStdout $ do
    files <- mapStackTraceT (mapQueueLog runResourceT) $ commandLine argv
    unless (null files) $ lg $ unlines $ "Done! Created files:" : files
  case res of
    Right () -> return ()
    Left msgs -> do
      hPutStrLn stderr "ERROR!"
      hPutStr stderr $ displayException msgs
      exitFailure
