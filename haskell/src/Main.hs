{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           CommandLine
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Exception              (bracket, bracket_,
                                                 displayException)
import           Control.Monad.Extra
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Control.Monad.Trans.StackTrace
import           Foreign.C                      (peekCString)
import           SDL                            (($=))
import qualified SDL
import           System.Environment             (getArgs)
import           System.Exit
import           System.Info                    (os)
import           System.IO                      (hPutStr, hPutStrLn, stderr)
import           System.Process
import           TinyFileDialogs

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
            inside "checking if Mono is installed" $ checkShell "mono --version"
        commandLine argv
      mapM_ printWarning warns
      case res of
        Right () -> return ()
        Left msgs -> do
          hPutStrLn stderr "ERROR!"
          hPutStr stderr $ displayException msgs
          exitFailure

launchGUI :: IO ()
launchGUI = bracket_ SDL.initializeAll SDL.quit $ do
  let windowConf = SDL.defaultWindow { SDL.windowResizable = True, SDL.windowHighDPI = True }
  bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
    bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
      SDL.rendererDrawColor rend $= SDL.V4 255 0 0 255
      forever $ do
        SDL.clear rend
        SDL.present rend
        threadDelay 5000
        evts <- SDL.pollEvents
        forM_ evts $ \e -> case SDL.eventPayload e of
          SDL.QuitEvent -> exitSuccess
          SDL.DropEvent (SDL.DropEventData cstr) -> do
            peekCString cstr >>= putStrLn
            SDL.rendererDrawColor rend $= SDL.V4 0 0 255 255
          SDL.MouseButtonEvent SDL.MouseButtonEventData{ SDL.mouseButtonEventMotion = SDL.Pressed } -> void $ forkIO $ do
            colorChooser "Pick a window color." (255, 255, 255) >>= \case
              Nothing -> return ()
              Just (r, g, b) -> SDL.rendererDrawColor rend $= SDL.V4 r g b 255
          _ -> return ()
