{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Build                          (loadYaml)
import           Config
import           Control.Concurrent             (threadDelay)
import           Control.Exception              (bracket, bracket_, throwIO)
import           Control.Monad                  (forM, forM_, guard, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Resource   (runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Fixed
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map.Strict                as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as T
import           Graphics.GL.Core33
import           Import                         (importSTFS)
import qualified RhythmGame.Audio               as RGAudio
import           RhythmGame.Graphics            (WindowDims (..),
                                                 drawDrumPlayFull, drawTracks,
                                                 loadGLStuff)
import qualified RhythmGame.PNF                 as PNF
import           RhythmGame.Track
import qualified RockBand.Codec.Drums           as D
import           SDL                            (($=))
import qualified SDL
import           System.Environment             (getArgs)
import           System.FilePath                ((</>))
import           Text.Read                      (readMaybe)

main :: IO ()
main = getArgs >>= \case

  ["play", con, strIndex] -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      index <- maybe (fatal "Invalid track number") return $ readMaybe strIndex
      _ <- importSTFS 0 con Nothing dir
      allTracks <- fmap (concat . previewTracks) $ loadTracks Nothing $ dir </> "notes.mid"
      drums <- case snd $ allTracks !! index of
        PreviewDrums drums -> return drums
        _                  -> fatal "Not a drums track"
      yml <- loadYaml $ dir </> "song.yml"
      (pans, vols) <- case HM.toList $ _plans (yml :: SongYaml FilePath) of
        [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
        _                   -> fatal "Couldn't find pans and vols after importing STFS"
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Debug 4 3
                -- , SDL.glMultisampleSamples = 4
                }
              }
        bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
          SDL.windowMinimumSize window $= SDL.V2 800 600
          bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
            RGAudio.withAL $ \_openedAudio -> do
              RGAudio.withMOGG (dir </> "audio.mogg") $ \ogg -> do
                playDrumTrack window drums pans vols ogg
    case res of
      Left err -> throwIO err
      Right () -> return ()

  [con] -> do
    res <- runResourceT $ logStdout $ tempDir "onyx_game" $ \dir -> do
      _ <- importSTFS 0 con Nothing dir
      trks <- fmap (map snd . concat . map (take 1) . previewTracks) $ loadTracks Nothing $ dir </> "notes.mid"
      yml <- loadYaml $ dir </> "song.yml"
      (pans, vols) <- case HM.toList $ _plans (yml :: SongYaml FilePath) of
        [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
        _                   -> fatal "Couldn't find pans and vols after importing STFS"
      liftIO $ bracket_ SDL.initializeAll SDL.quit $ do
        let windowConf = SDL.defaultWindow
              { SDL.windowResizable = True
              , SDL.windowHighDPI = False
              , SDL.windowInitialSize = SDL.V2 800 600
              , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                { SDL.glProfile = SDL.Core SDL.Debug 4 3
                -- , SDL.glMultisampleSamples = 4
                }
              }
        bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
          SDL.windowMinimumSize window $= SDL.V2 800 600
          bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
            RGAudio.withAL $ \_openedAudio -> do
              RGAudio.withMOGG (dir </> "audio.mogg") $ \ogg -> do
                playTracks window trks pans vols ogg
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Drag a CON file onto this exe."

data AppState = AppState
  { songTime       :: Milli
  , sdlStartedPlay :: Maybe (Milli, RGAudio.AudioHandle)
  }

playDrumTrack
  :: SDL.Window
  -> Map.Map Double (PNF.CommonState (PNF.DrumState (D.Gem D.ProType)))
  -> [Float]
  -> [Float]
  -> FilePath
  -> IO ()
playDrumTrack window trk pans vols ogg = do
  Right glStuff <- logStdout loadGLStuff
  let ticksMilli :: IO Milli
      ticksMilli = MkFixed . fromIntegral <$> SDL.ticks
      delayMilli :: Milli -> IO ()
      delayMilli (MkFixed m) = threadDelay $ fromIntegral m * 1000
      eventMilli :: SDL.Event -> Milli
      eventMilli = MkFixed . fromIntegral . SDL.eventTimestamp
      halfWindow :: Double
      halfWindow = 0.05 -- 50 ms on each side
  ca <- RGAudio.oggSecsSpeed 0 Nothing ogg
  _audioHandle <- RGAudio.playSource pans vols 1 ca
  startedAt <- ticksMilli
  let loop prevState = do
        -- print $ PNF.drumEvents $ prevState
        frameStart <- ticksMilli
        let timePassed = PNF.applyDrumEvent (realToFrac $ frameStart - startedAt) Nothing halfWindow prevState
        SDL.pollEvents >>= processEvents timePassed >>= \case
          Nothing -> return ()
          Just dps -> do
            timestamp <- ticksMilli
            let t = timestamp - startedAt
            SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
            drawDrumPlayFull glStuff (WindowDims w h) (realToFrac t) 1 dps
            SDL.glSwapWindow window
            frameEnd <- ticksMilli
            delayMilli $ 0.016 - (frameEnd - frameStart)
            loop dps
      processEvents s [] = return $ Just s
      processEvents s (e : es) = case SDL.eventPayload e of
        SDL.QuitEvent -> return Nothing
        SDL.KeyboardEvent SDL.KeyboardEventData
          { SDL.keyboardEventKeyMotion = SDL.Pressed
          , SDL.keyboardEventRepeat = False
          , SDL.keyboardEventKeysym = SDL.Keysym
            { SDL.keysymScancode = scan
            }
          } -> let
            eventTime = realToFrac $ eventMilli e - startedAt
            applyHit pad = processEvents
              (PNF.applyDrumEvent eventTime (Just pad) halfWindow s)
              es
            in case scan of
              SDL.ScancodeSpace -> applyHit D.Kick
              SDL.ScancodeV     -> applyHit D.Red
              SDL.ScancodeB     -> applyHit $ D.Pro D.Yellow D.Tom
              SDL.ScancodeN     -> applyHit $ D.Pro D.Blue D.Tom
              SDL.ScancodeM     -> applyHit $ D.Pro D.Green D.Tom
              SDL.ScancodeG     -> applyHit $ D.Pro D.Yellow D.Cymbal
              SDL.ScancodeH     -> applyHit $ D.Pro D.Blue D.Cymbal
              SDL.ScancodeJ     -> applyHit $ D.Pro D.Green D.Cymbal
              _                 -> processEvents s es
        _ -> processEvents s es
  loop PNF.DrumPlayState
    { PNF.drumEvents = []
    , PNF.drumTrack = trk
    , PNF.drumNoteTimes = Set.fromList $ do
      (cst, cs) <- Map.toList trk
      guard $ not $ Set.null $ PNF.drumNotes $ PNF.commonState cs
      return cst
    }

playTracks :: SDL.Window -> [PreviewTrack] -> [Float] -> [Float] -> FilePath -> IO ()
playTracks window trks pans vols ogg = do
  Right glStuff <- logStdout loadGLStuff
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
                ca <- RGAudio.oggSecsSpeed (realToFrac $ songTime s) Nothing ogg
                audioHandle <- RGAudio.playSource pans vols 1 ca
                timestamp <- ticksMilli
                return s { sdlStartedPlay = Just (timestamp, audioHandle) }
              Just (started, audioHandle) -> do
                timestamp <- ticksMilli
                let current = songTime s + (timestamp - started)
                RGAudio.audioStop audioHandle
                return s { songTime = current, sdlStartedPlay = Nothing }
            processEvents s' es
        _             -> processEvents s es
      draw t = do
        SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
        drawTracks glStuff (WindowDims w h) (realToFrac t) 1 trks
        SDL.glSwapWindow window
  loop $ AppState { songTime = 0, sdlStartedPlay = Nothing }
