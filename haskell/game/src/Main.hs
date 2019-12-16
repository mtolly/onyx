{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                          (loadYaml)
import           Config
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (bracket, bracket_, throwIO)
import           Control.Monad                  (forM_, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map.Strict                as Map
import qualified Data.Text                      as T
import           Graphics.GL.Core33
import           Import                         (importSTFS)
import qualified RhythmGame.Audio               as RGAudio
import           RhythmGame.Graphics            (WindowDims (..), drawDrums,
                                                 drawFive, drawTrack,
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

  [con, si] -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      i <- maybe (fatal "Invalid track number") return $ readMaybe si
      _ <- importSTFS 0 con Nothing dir
      trks <- loadTracks $ dir </> "notes.mid"
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
            threadDelay 1000000 -- this prevents a weird crash, see https://github.com/haskell-game/sdl2/issues/176
            RGAudio.playMOGG pans vols (dir </> "audio.mogg") $ do
              playTrack window $ case drop i trks of
                []           -> PreviewDrums Map.empty
                (_, trk) : _ -> trk
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Usage: onyx-game song_rb3con [track_number]"

playTrack :: SDL.Window -> PreviewTrack -> IO ()
playTrack window trk = do
  initTime <- SDL.ticks
  glStuff <- loadGLStuff
  let loop = SDL.pollEvents >>= processEvents >>= \b -> when b $ do
        timestamp <- SDL.ticks
        draw $ fromIntegral (timestamp - initTime) / 1000
        threadDelay 5000
        loop
      processEvents [] = return True
      processEvents (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return False
        _             -> processEvents es
      draw t = do
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        case trk of
          PreviewDrums m -> drawTrack drawDrums glStuff (WindowDims w h) t m
          PreviewFive m  -> drawTrack drawFive glStuff (WindowDims w h) t m
        SDL.glSwapWindow window
  loop
