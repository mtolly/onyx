{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Main (main) where

import           Control.Exception            (displayException)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Functor                 ((<&>))
import           Data.Maybe                   (fromMaybe)
import           Onyx.CommandLine             (commandLine)
import           Onyx.GUI                     (launchGUI)
import           Onyx.GUI.Fluid               (setGlobalL)
import           Onyx.Localization
import           Onyx.Preferences             (Preferences (prefLanguageCode),
                                               readPreferences)
import           Onyx.Resources               (getResourcesPath)
import           Onyx.StackTrace
import           System.Environment           (getArgs)
import           System.Exit
import           System.Info                  (os)
import           System.IO                    (hPutStr, hPutStrLn, stderr)
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

  code <- logStdout readPreferences <&> fromMaybe "en" . \case
    Right prefs -> prefs.prefLanguageCode
    Left _      -> Nothing
  loc <- getResourcesPath "localization" >>= \root -> loadL root code
  let ?loc = loc
  -- FLUID-generated windows localize their text from C++, which can't see the
  -- implicit parameter
  setGlobalL loc

  argv <- getArgs
  case argv of
    []          -> launchGUI []
    "open" : xs -> launchGUI xs
    _           -> do
      res <- logStdout do
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
