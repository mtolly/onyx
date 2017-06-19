{-# LANGUAGE LambdaCase #-}
module Main (main) where

import           CommandLine                    (commandLine)
import           Control.Exception              (displayException)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           GUI                            (launchGUI)
import           OSFiles                        (useResultFiles)
import           System.Environment             (getArgs)
import           System.Exit
import           System.Info                    (os)
import           System.IO                      (hPutStr, hPutStrLn, stderr)
import           System.Process

checkShell :: (MonadIO m) => String -> StackTraceT m ()
checkShell s = liftIO (readCreateProcessWithExitCode (shell s) "") >>= \case
  (ExitSuccess  , _, _) -> return ()
  (ExitFailure _, _, _) -> warn "An external program was not found on your PATH."

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> launchGUI
    _  -> do
      (res, Messages warns) <- runStackTraceT $ do
        case os of
          "mingw32" -> return ()
          _ -> do
            inside "checking if Wine is installed" $ checkShell "wine --version"
        commandLine argv >>= useResultFiles
      mapM_ printWarning warns
      case res of
        Right () -> return ()
        Left msgs -> do
          hPutStrLn stderr "ERROR!"
          hPutStr stderr $ displayException msgs
          exitFailure
