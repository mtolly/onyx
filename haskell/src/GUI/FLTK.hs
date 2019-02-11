{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.FLTK where

import Graphics.UI.FLTK.LowLevel.FL (setScheme, run, flush, redraw, getProgramShouldQuit, waitFor)
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import Graphics.UI.TinyFileDialogs (openFileDialog)
import Control.Monad.Trans.StackTrace
import Control.Monad.Trans.Resource (runResourceT, release)
import OpenProject
import qualified Data.Text as T
import Control.Concurrent.STM.TChan
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically)
import Config
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)

data WindowState
  = LoadSong
    { wsWindow :: FL.Ref FL.Window
    , wsLoadButton :: FL.Ref FL.Button
    }
  | LoadedSong
    { wsWindow :: FL.Ref FL.Window
    , wsGroup :: FL.Ref FL.Group
    , wsProject :: Project
    }

data Event
  = EventLoad FilePath
  | EventClose

launchGUI :: IO ()
launchGUI = do
  _ <- setScheme "gtk+"
  evts <- newTChanIO
  let newWindow = do
        window <- FL.windowNew
          (FL.Size (FL.Width 115) (FL.Height 100))
          Nothing
          Nothing
        fillWindow window
      resizeWindow :: FL.Ref FL.Window -> FL.Size -> IO ()
      resizeWindow window size = do
        x <- FL.getX window
        y <- FL.getY window
        FL.resize window $ FL.Rectangle (FL.Position x y) size
      fillWindow :: FL.Ref FL.Window -> IO WindowState
      fillWindow window = do
        FL.begin window -- I think windowNew does this automatically
        btn <- FL.buttonNew
          (FL.Rectangle (FL.Position (FL.X 10) (FL.Y 30)) (FL.Size (FL.Width 95) (FL.Height 30)))
          (Just "Load song")
        FL.setLabelsize btn (FL.FontSize 10)
        FL.setCallback btn $ \_ -> do
          openFileDialog "Load song" "" [] "Songs" False >>= \case
            Just [f] -> atomically $ writeTChan evts $ EventLoad $ T.unpack f
            _        -> return ()
        FL.end window
        FL.showWidget window
        return $ LoadSong window btn
  runResourceT $ logStdout $ let
    loop ws = liftIO getProgramShouldQuit >>= \case
      True  -> return ()
      False -> let
        process ws' = liftIO (atomically $ tryReadTChan evts) >>= \case
          Nothing -> return ws'
          Just e -> case e of
            EventLoad f -> case ws' of
              LoadSong window btn -> errorToEither (openProject f) >>= \case
                Left _err -> process ws'
                Right proj -> do
                  group <- liftIO $ do
                    FL.destroy btn
                    -- 10px padding, window 300px wide, text inputs 25px high, button 95x30
                    let windowSize = FL.Size
                          (FL.Width 300)
                          (FL.Height $ 10 + 25 + 10 + 25 + 10 + 25 + 10 + 30 + 10)
                    resizeWindow window windowSize
                    FL.begin window
                    group <- FL.groupNew
                      (FL.Rectangle (FL.Position (FL.X 0) (FL.Y 0)) windowSize)
                      Nothing
                    forM_ (zip [0..] [_title, _artist, _album]) $ \(i, fn) -> do
                      input <- FL.inputNew
                        (FL.Rectangle (FL.Position (FL.X 10) (FL.Y $ 10 + i * (25 + 10))) (FL.Size (FL.Width 280) (FL.Height 25)))
                        Nothing
                        Nothing
                      FL.setValue input $ fromMaybe "" $ fn $ _metadata $ projectSongYaml proj
                    btn' <- FL.buttonNew
                      (FL.Rectangle (FL.Position (FL.X 10) (FL.Y $ 10 + 3 * (25 + 10))) (FL.Size (FL.Width 95) (FL.Height 30)))
                      (Just "Close song")
                    FL.setLabelsize btn' (FL.FontSize 10)
                    FL.setCallback btn' $ \_ -> atomically $ writeTChan evts EventClose
                    FL.end group
                    FL.end window
                    redraw
                    return group
                  process $ LoadedSong window group proj
              _ -> process ws'
            EventClose -> case ws' of
              LoadedSong window group proj -> do
                mapM_ release $ projectRelease proj
                liftIO $ FL.destroy group
                liftIO $ resizeWindow window $ FL.Size (FL.Width 115) (FL.Height 100)
                filled <- liftIO $ fillWindow window
                liftIO $ redraw
                process filled
              _ -> process ws'
        in liftIO (waitFor 1e20) >> process ws >>= loop
    in liftIO newWindow >>= loop
  flush -- dunno if required
