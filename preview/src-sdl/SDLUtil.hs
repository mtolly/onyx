{-# LANGUAGE LambdaCase #-}
module SDLUtil where

import Foreign
import Foreign.C
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Control.Exception (bracket, bracket_)
import Control.Monad (unless)

-- | Extracts and throws an SDL error if the action returns a null pointer.
notNull :: IO (Ptr a) -> IO (Ptr a)
notNull act = do
  p <- act
  if p == nullPtr
    then SDL.getError >>= peekCString >>= error
    else return p

-- | Extracts and throws an SDL error if the action doesn't return zero.
zero :: (Eq a, Num a) => IO a -> IO ()
zero act = do
  n <- act
  unless (n == 0) $ SDL.getError >>= peekCString >>= error

withSDL :: [SDL.InitFlag] -> IO a -> IO a
withSDL flags = bracket_
  (zero $ SDL.init $ foldr (.|.) 0 flags)
  SDL.quit

withSDLImage :: [Image.InitFlag] -> IO a -> IO a
withSDLImage flags = bracket_
  (Image.imgInit flags)
  Image.imgQuit

withWindowAndRenderer :: String -> CInt -> CInt -> Word32
  -> (SDL.Window -> SDL.Renderer -> IO a) -> IO a
withWindowAndRenderer name w h flags act = bracket
  (withCString name $ \s -> notNull $ SDL.createWindow
    s -- title
    SDL.SDL_WINDOWPOS_UNDEFINED -- x
    SDL.SDL_WINDOWPOS_UNDEFINED -- y
    w -- width
    h -- height
    flags -- flags
    )
  SDL.destroyWindow
  (\window -> bracket
    (notNull $ SDL.createRenderer window (-1) 0)
    SDL.destroyRenderer
    (\renderer -> act window renderer)
    )

-- | Returns Just an event if there is one currently in the queue.
pollEvent :: IO (Maybe SDL.Event)
pollEvent = alloca $ \pevt -> SDL.pollEvent pevt >>= \case
  1 -> fmap Just $ peek pevt
  _ -> return Nothing

withImage :: SDL.Renderer -> FilePath -> (SDL.Texture -> IO a) -> IO a
withImage render fp = bracket
  (Image.imgLoadTexture render fp >>= either error return)
  SDL.destroyTexture
