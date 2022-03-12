{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           CommandLine                    (commandLine)
import           Control.Exception              (displayException)
import           Control.Monad                  (unless)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import           GUI.FLTK                       (launchGUI)
import           System.Environment             (getArgs)
import           System.Exit
import           System.Info                    (os)
import           System.IO                      (hPutStr, hPutStrLn, stderr)
import           System.Process

checkShell :: (SendMessage m, MonadIO m) => String -> StackTraceT m ()
checkShell s = liftIO (readCreateProcessWithExitCode (shell s) "") >>= \case
  (ExitSuccess  , _, _) -> return ()
  (ExitFailure _, _, _) -> warn "An external program was not found on your PATH."

main :: IO ()
main = do
#ifdef WINDOWS
  initCOM
#endif
  argv <- getArgs
  case argv of
    [] -> launchGUI
    _  -> do
      res <- logStdout $ do
        case os of
          "mingw32" -> return ()
          _ -> do
            inside "checking if Wine is installed" $ checkShell "wine --version"
        files <- mapStackTraceT (mapQueueLog runResourceT) $ commandLine argv
        unless (null files) $ lg $ unlines $ "Done! Created files:" : files
      case res of
        Right () -> return ()
        Left msgs -> do
          hPutStrLn stderr "ERROR!"
          hPutStr stderr $ displayException msgs
          exitFailure

#ifdef WINDOWS
foreign import ccall unsafe "onyxInitCOM"
  initCOM :: IO ()
#endif
