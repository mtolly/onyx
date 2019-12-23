{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                          (loadYaml)
import           Config
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (bracket, bracket_, throwIO)
import           Control.Monad                  (forM, forM_, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.HashMap.Strict            as HM
import qualified Data.Text                      as T
import           Graphics.GL.Core33
import           Import                         (importSTFS)
import qualified RhythmGame.Audio               as RGAudio
import           RhythmGame.Graphics            (WindowDims (..), drawTracks,
                                                 loadGLStuff)
import           RhythmGame.Track
import           SDL                            (($=))
import qualified SDL
import           System.Environment             (getArgs)
import           System.FilePath                ((</>))
import           Text.Read                      (readMaybe)

main :: IO ()
main = getArgs >>= \case

  [con] -> void $ runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
    _ <- importSTFS 0 con Nothing dir
    trks <- loadTracks $ dir </> "notes.mid"
    stackIO $ forM_ (zip [0..] trks) $ \(i, (name, _)) -> do
      putStrLn $ show (i :: Int) <> ": " <> T.unpack name

  con : strIndexes -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      indexes <- forM strIndexes $ maybe (fatal "Invalid track number") return . readMaybe
      _ <- importSTFS 0 con Nothing dir
      allTracks <- loadTracks $ dir </> "notes.mid"
      let trks = map (snd . (allTracks !!)) indexes
      yml <- loadYaml $ dir </> "song.yml"
      (pans, vols) <- case HM.toList $ _plans yml of
        [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
        _                   -> fatal "Couldn't find pans and vols after importing STFS"
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
                , SDL.glMultisampleSamples = 4
                }
              }
        bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
          SDL.windowMinimumSize window $= SDL.V2 800 600
          bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
            RGAudio.playMOGG pans vols (dir </> "audio.mogg") $ do
              playTracks window trks
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Usage: onyx-game song_rb3con [track_number]"

playTracks :: SDL.Window -> [PreviewTrack] -> IO ()
playTracks window trks = do
  initTime <- SDL.ticks
  glStuff <- loadGLStuff
  let loop = do
        frameStart <- fromIntegral <$> SDL.ticks
        SDL.pollEvents >>= processEvents >>= \b -> when b $ do
          timestamp <- SDL.ticks
          draw $ fromIntegral (timestamp - initTime) / 1000
          frameEnd <- fromIntegral <$> SDL.ticks
          threadDelay $ (16 - (frameEnd - frameStart)) * 1000
          loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        _             -> processEvents es
      draw t = do
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        drawTracks glStuff (WindowDims w h) t trks
        SDL.glSwapWindow window
  loop
