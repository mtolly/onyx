{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NondecreasingIndentation #-}
module Main where

import System.Environment
import System.FilePath ((</>), (<.>))
import qualified Sound.MIDI.File.Load as Load
import qualified RockBand.File                    as File
import           StackTrace
import           Control.Exception                (evaluate)
import           Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Image as Image
import Foreign

import Midi
import SDLUtil
import Draw

main :: IO ()
main = getArgs >>= \case
  ["--list-options"] -> return () -- be quiet fish
  [dir] -> do
    preview <- Load.fromFile (dir </> "gen/album/2p/notes.mid")
      >>= printStackTraceIO . File.readMIDIFile
      >>= evaluate . buildPreview
    withSDL [SDL.SDL_INIT_TIMER, SDL.SDL_INIT_VIDEO] $ do
    withSDLImage [Image.InitPNG] $ do
    withWindowAndRenderer "onyxpreview" 640 480 0 $ \_window render -> do
    let imageIDs = [minBound .. maxBound] :: [ImageID]
        imagePath iid = "www/rbprev" </> drop 6 (show iid) <.> "png"
    withMany (withImage render . imagePath) imageIDs $ \texs -> do
    let imageTable = zip imageIDs texs
        image iid = case lookup iid imageTable of
          Nothing  -> error $ "panic! no texture found for " ++ show iid
          Just tex -> tex
        performDraw t = do
          forM_ (draw t preview) $ \case
            DrawImage iid (x, y, w, h) opacity -> do
              let tex = image iid
              zero $ SDL.setTextureAlphaMod tex $ round $ opacity * 255
              with (SDL.Rect (round x) (round y) (round w) (round h)) $ \rect ->
                zero $ SDL.renderCopy render tex nullPtr rect
              zero $ SDL.setTextureAlphaMod tex 255
            Text _ _ _ -> return ()
          SDL.renderPresent render
    performDraw 80
    threadDelay 2000000
  _ -> do
    prog <- getProgName
    error $ "usage: " ++ prog ++ " .../songs/the-artist/the-song/"
