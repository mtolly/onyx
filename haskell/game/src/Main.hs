{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                            (loadYaml)
import           Config
import           Control.Arrow                    (first)
import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Exception                (bracket, bracket_, throwIO)
import           Control.Monad                    (forever)
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Resource     (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (toLower)
import           Data.Connection                  (Connection (..))
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     (void)
import qualified Data.HashMap.Strict              as HM
import           Data.IORef                       (newIORef, readIORef,
                                                   writeIORef)
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           Graphics.GL.Core33
import           Import                           (importSTFS)
import qualified Reaper.Extract                   as RPP
import qualified Reaper.Parse                     as RPP
import qualified Reaper.Scan                      as RPP
import qualified RhythmGame.Audio                 as RGAudio
import qualified RhythmGame.Drums                 as RGDrums
import qualified RhythmGame.Drums.Play            as RGDrums
import           RockBand.Codec.Drums
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Common                  (Difficulty (..))
import           Scripts                          (loadMIDI)
import           SDL                              (($=))
import qualified SDL
import qualified Sound.MIDI.Util                  as U
import           System.Environment               (getArgs)
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   takeFileName, (</>))
import           System.FSNotify
import qualified System.IO.Streams                as Streams
import qualified System.IO.Streams.TCP            as TCP
import           Text.Decode                      (decodeGeneral)
import           Text.Read                        (readMaybe)

main :: IO ()
main = getArgs >>= \case

  ["scene"] -> do
    res <- runResourceT $ logStdout $ do
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
                }
              }
        bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
          SDL.windowMinimumSize window $= SDL.V2 800 600
          bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
            threadDelay 1000000 -- this prevents a weird crash, see https://github.com/haskell-game/sdl2/issues/176
            RGDrums.playScene window
    case res of
      Left err -> throwIO err
      Right () -> return ()

  ["server", mid] -> do
    res <- runResourceT $ logStdout $ do

      let loadTrack = do
            song <- case map toLower $ takeExtension mid of
              ".rpp" -> do
                txt <- liftIO $ T.unpack . decodeGeneral <$> B.readFile mid
                RBFile.interpretMIDIFile $ RPP.getMIDI $ RPP.parse $ RPP.scan txt
              _ -> loadMIDI mid
            let tempos = RBFile.s_tempos song
                drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
                drums'
                  = Map.fromList
                  $ map (first $ realToFrac . U.applyTempoMap tempos)
                  $ ATB.toPairList
                  $ RTB.toAbsoluteEventList 0
                  $ RTB.collectCoincident
                  $ fmap RGDrums.Autoplay
                  $ computePro Nothing drums
            return $ RGDrums.Track drums' Map.empty 0 0.2
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
              let readLoop = do
                    req <- Streams.read $ source conn
                    case req of
                      Just x -> do
                        case readMaybe $ B8.unpack x of
                          Nothing -> putStrLn $ "Couldn't read as Double: " <> show x
                          Just d  -> print d >> writeIORef time d
                        readLoop
                      Nothing -> putStrLn "Connection closed." >> connectLoop
              readLoop
        connectLoop
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
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
      song <- loadMIDI $ dir </> "notes.mid"
      let tempos = RBFile.s_tempos song
          drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
          drums'
            = Map.fromList
            $ map (first $ realToFrac . U.applyTempoMap tempos)
            $ ATB.toPairList
            $ RTB.toAbsoluteEventList 0
            $ RTB.collectCoincident
            $ fmap RGDrums.Upcoming
            $ drumGems
            $ fromMaybe mempty
            $ Map.lookup Expert
            $ drumDifficulties drums
      yml <- loadYaml $ dir </> "song.yml"
      (pans, vols) <- case HM.toList $ _plans yml of
        [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
        _                   -> fatal "Couldn't find pans and vols after importing STFS"
      let trk = RGDrums.Track drums' Map.empty 0 0.2
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Normal 3 3
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
