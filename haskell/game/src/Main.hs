{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                          (loadYaml)
import           Config
import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Exception              (bracket, bracket_, throwIO)
import           Control.Monad                  (forever)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as B8
import           Data.Connection                (Connection (..))
import           Data.Functor                   (void)
import qualified Data.HashMap.Strict            as HM
import           Data.IORef                     (newIORef, readIORef,
                                                 writeIORef)
import qualified Data.Map.Strict                as Map
import           Graphics.GL.Core33
import           Import                         (importSTFS)
import qualified RhythmGame.Audio               as RGAudio
import qualified RhythmGame.Drums               as RGDrums
import qualified RhythmGame.Drums.Play          as RGDrums
import           RhythmGame.Track
import           SDL                            (($=))
import qualified SDL
import           System.Environment             (getArgs)
import           System.FilePath                (takeDirectory, takeFileName,
                                                 (</>))
import           System.FSNotify
import qualified System.IO.Streams              as Streams
import qualified System.IO.Streams.TCP          as TCP
import           Text.Read                      (readMaybe)

main :: IO ()
main = getArgs >>= \case

  ["server", mid] -> do
    res <- runResourceT $ logStdout $ do

      let loadTrack = do
            trks <- loadTracks True mid
            -- TODO actual way to select track
            return $ case [ trk | (_, PreviewDrums trk) <- trks ] of
              trk : _ -> trk
              []      -> RGDrums.Track Map.empty Map.empty 0 0.2 Map.empty
      varTrack <- loadTrack >>= liftIO . newIORef
      let midFileName = takeFileName mid
      liftIO $ void $ forkIO $ withManager $ \wm -> do
        let test = \case
              Added    f _ _ -> takeFileName f == midFileName
              Modified f _ _ -> takeFileName f == midFileName
              _              -> False
        void $ watchDir wm (takeDirectory mid) test $ \_ -> do
          putStrLn $ "Reloading from " <> mid
          void $ runResourceT $ logStdout $ loadTrack >>= liftIO . writeIORef varTrack
        forever $ threadDelay 1000000

      time <- liftIO $ newIORef 0
      liftIO $ void $ forkIO $ do
        putStrLn "Launching server..."
        sock <- TCP.bindAndListen 1024 4938
        let connectLoop = do
              putStrLn "Waiting for connection..."
              conn <- TCP.accept sock
              putStrLn "Connected."
              let readLoop dat = do
                    req <- Streams.read $ source conn
                    case req of
                      Just bs -> let
                        dat' = dat <> bs
                        in case reverse $ B8.split '|' dat' of
                          after : s : _ -> do
                            mapM_ (writeIORef time) $ readMaybe $ B8.unpack s
                            readLoop after
                          _ -> readLoop dat'
                      Nothing -> putStrLn "Connection closed." >> connectLoop
              readLoop B.empty
        connectLoop
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
            RGDrums.previewDrums window (readIORef varTrack) $ readIORef time
    case res of
      Left err -> throwIO err
      Right () -> return ()

  [con] -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      _ <- importSTFS 0 con Nothing dir
      trks <- loadTracks False $ dir </> "notes.mid"
      let trk = case [ d | (_, PreviewDrums d) <- trks ] of
            drums : _ -> fmap void drums
            []        -> RGDrums.Track Map.empty Map.empty 0 0.2 Map.empty
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
              RGDrums.playDrums window trk
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Usage: onyx-game song_rb3con"
