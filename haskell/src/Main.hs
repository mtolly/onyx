{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           CommandLine
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Exception              (bracket, bracket_,
                                                 displayException)
import           Control.Monad.Extra
import           Control.Monad.Trans.StackTrace
import           Foreign.C                      (peekCString)
import           SDL                            (($=))
import qualified SDL
import           System.Environment             (getArgs)
import           System.Exit                    (exitFailure, exitSuccess)
import           System.IO                      (hPutStr, hPutStrLn, stderr)
import           TinyFileDialogs

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [] -> launchGUI
    _  -> runStackTraceT (commandLine argv) >>= \(res, warns) -> do
      hPutStr stderr $ displayException warns
      case res of
        Right () -> return ()
        Left errs -> do
          hPutStrLn stderr "ERROR:"
          hPutStr stderr $ displayException errs
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
