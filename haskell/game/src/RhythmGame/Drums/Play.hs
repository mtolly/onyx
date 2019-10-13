{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NegativeLiterals  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RhythmGame.Drums.Play where

import RhythmGame.Drums
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (when)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Trans.State
import qualified RockBand.Codec.Drums      as D
import qualified SDL

playDrums :: SDL.Window -> Track Double (D.Gem ()) -> IO ()
playDrums window trk = flip evalStateT trk $ do
  initTime <- SDL.ticks
  glStuff <- liftIO loadGLStuff
  let loop = SDL.pollEvents >>= processEvents >>= \b -> when b $ do
        timestamp <- SDL.ticks
        modify $ updateTime $ fromIntegral (timestamp - initTime) / 1000
        draw
        liftIO $ threadDelay 5000
        loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        SDL.KeyboardEvent SDL.KeyboardEventData
          { SDL.keyboardEventKeyMotion = SDL.Pressed
          , SDL.keyboardEventKeysym = ksym
          , SDL.keyboardEventRepeat = False
          } -> do
            let hit gem = modify $ hitPad t gem
                t = fromIntegral (SDL.eventTimestamp e - initTime) / 1000
            case SDL.keysymScancode ksym of
              SDL.ScancodeV     -> hit D.Red
              SDL.ScancodeB     -> hit $ D.Pro D.Yellow ()
              SDL.ScancodeN     -> hit $ D.Pro D.Blue ()
              SDL.ScancodeM     -> hit $ D.Pro D.Green ()
              SDL.ScancodeSpace -> hit D.Kick
              _                 -> return ()
            processEvents es
        _ -> processEvents es
      draw = do
        trk' <- get
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        liftIO $ drawDrumsFull glStuff (WindowDims w h) trk'
        SDL.glSwapWindow window
  loop

previewDrums :: SDL.Window -> IO (Track Double (D.Gem D.ProType)) -> IO Double -> IO ()
previewDrums window getTrack getTime = do
  glStuff <- loadGLStuff
  let loop = SDL.pollEvents >>= processEvents >>= \b -> when b $ do
        t <- getTime
        trk <- getTrack
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        drawDrumsFull glStuff (WindowDims w h) trk { trackTime = t }
        SDL.glSwapWindow window
        threadDelay 5000
        loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        _             -> processEvents es
  loop
