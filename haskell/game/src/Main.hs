{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                          (loadYaml)
import           Config
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (bracket, bracket_, throwIO)
import           Control.Monad                  (forM, forM_, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Fixed
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
    trks <- fmap previewTracks $ loadTracks $ dir </> "notes.mid"
    stackIO $ forM_ (zip [0..] trks) $ \(i, (name, _)) -> do
      putStrLn $ show (i :: Int) <> ": " <> T.unpack name

  con : strIndexes -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      indexes <- forM strIndexes $ maybe (fatal "Invalid track number") return . readMaybe
      _ <- importSTFS 0 con Nothing dir
      allTracks <- fmap previewTracks $ loadTracks $ dir </> "notes.mid"
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
            RGAudio.withAL $ do
              RGAudio.withMOGG (dir </> "audio.mogg") $ \ogg -> do
                playTracks window trks pans vols ogg
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Usage: onyx-game song_rb3con [track_number]"

data AppState = AppState
  { songTime       :: Milli
  , sdlStartedPlay :: Maybe (Milli, IO ())
  }

playTracks :: SDL.Window -> [PreviewTrack] -> [Float] -> [Float] -> FilePath -> IO ()
playTracks window trks pans vols ogg = do
  glStuff <- loadGLStuff
  let ticksMilli :: IO Milli
      ticksMilli = MkFixed . fromIntegral <$> SDL.ticks
      delayMilli :: Milli -> IO ()
      delayMilli (MkFixed m) = threadDelay $ fromIntegral m * 1000
      loop prevState = do
        frameStart <- ticksMilli
        SDL.pollEvents >>= processEvents prevState >>= \case
          Nothing -> return ()
          Just appState -> do
            timestamp <- ticksMilli
            draw $ case sdlStartedPlay appState of
              Nothing       -> songTime appState
              Just (tks, _) -> songTime appState + (timestamp - tks)
            frameEnd <- ticksMilli
            delayMilli $ 0.016 - (frameEnd - frameStart)
            loop appState
      processEvents s [] = return $ Just s
      processEvents s (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return Nothing
        SDL.KeyboardEvent SDL.KeyboardEventData
          { SDL.keyboardEventKeyMotion = SDL.Pressed
          , SDL.keyboardEventRepeat = False
          , SDL.keyboardEventKeysym = SDL.Keysym
            { SDL.keysymScancode = SDL.ScancodeSpace
            }
          } -> do
            s' <- case sdlStartedPlay s of
              Nothing -> do
                ca <- RGAudio.sourceOGGFrom (realToFrac $ songTime s) ogg
                stop <- RGAudio.playSource pans vols ca
                timestamp <- ticksMilli
                return s { sdlStartedPlay = Just (timestamp, stop) }
              Just (started, stop) -> do
                timestamp <- ticksMilli
                let current = songTime s + (timestamp - started)
                stop
                return s { songTime = current, sdlStartedPlay = Nothing }
            processEvents s' es
        _             -> processEvents s es
      draw t = do
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        drawTracks glStuff (WindowDims w h) (realToFrac t) trks
        SDL.glSwapWindow window
  loop $ AppState { songTime = 0, sdlStartedPlay = Nothing }
