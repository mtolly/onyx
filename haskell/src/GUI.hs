{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception      (bracket, bracket_)
import           Control.Monad.Extra
import qualified Data.ByteString        as B
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign                (Ptr, castPtr)
import           Foreign.C              (CInt (..), peekCString)
import           Resources              (pentatonicTTF)
import           SDL                    (($=))
import qualified SDL
import           SDL.Raw                (Color (..), RWops, rwFromConstMem)
import qualified SDL.TTF                as TTF
import           SDL.TTF.FFI            (TTFFont)
import           System.Exit
import           TinyFileDialogs

foreign import ccall unsafe "TTF_OpenFontRW"
  openFontRW :: Ptr RWops -> CInt -> CInt -> IO TTFFont

loadBSFont :: B.ByteString -> Int -> IO TTFFont
loadBSFont bs pts = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
  rw  <- rwFromConstMem (castPtr ptr) (fromIntegral len)
  ttf <- openFontRW rw 1 (fromIntegral pts)
  return ttf

launchGUI :: IO ()
launchGUI = bracket_ SDL.initializeAll SDL.quit $ TTF.withInit $ do
  penta <- loadBSFont pentatonicTTF 50
  hello <- TTF.renderUTF8Blended penta "Hello!" (Color 255 255 255 255)
  let windowConf = SDL.defaultWindow { SDL.windowResizable = True, SDL.windowHighDPI = False }
  bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
    bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
      SDL.rendererDrawColor rend $= SDL.V4 100 100 100 255
      tex <- SDL.createTextureFromSurface rend hello
      dims <- SDL.surfaceDimensions hello
      SDL.get (SDL.rendererLogicalSize rend) >>= print
      SDL.get (SDL.rendererScale rend) >>= print
      SDL.get (SDL.windowSize window) >>= print
      forever $ do
        SDL.clear rend
        SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) dims
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
