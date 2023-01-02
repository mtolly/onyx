{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Control.Concurrent           (threadDelay)
import           Control.Exception            (bracket, bracket_, throwIO)
import           Control.Monad                (forM, forM_, guard, void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Fixed
import qualified Data.HashMap.Strict          as HM
import           Data.List                    (isPrefixOf)
import qualified Data.Map.Strict              as Map
import qualified Data.Set                     as Set
import qualified Data.Text                    as T
import qualified Data.Vector.Storable         as VS
import           Data.Word                    (Word8)
import           Graphics.GL.Core33
import qualified Onyx.Game.Audio              as RGAudio
import           Onyx.Game.Graphics           (WindowDims (..),
                                               drawDrumPlayFull, drawTracks,
                                               loadGLStuff)
import qualified Onyx.Game.Time               as PNF
import           Onyx.Game.Track
import           Onyx.Import
import qualified Onyx.MIDI.Track.Drums.Full   as FD
import           Onyx.Project
import           Onyx.StackTrace
import qualified SDL
import           SDL                          (($=))
import qualified Sound.RtMidi                 as Rt
import           System.Environment           (getArgs)
import           System.FilePath              (takeDirectory, (</>))
import           Text.Read                    (readMaybe)

importOne :: (SendMessage m, MonadResource m) => FilePath -> StackTraceT m Project
importOne f = findAllSongs f >>= \case
  [imp] -> impProject imp
  []    -> fatal "No songs"
  _     -> fatal "More than 1 song"

main :: IO ()
main = getArgs >>= \case

  ["play", con, strIndex] -> do
    res <- runResourceT $ logStdout $ do
      index <- maybe (fatal "Invalid track number") return $ readMaybe strIndex
      proj <- importOne con
      let dir = takeDirectory $ projectLocation proj
      song <- loadTracks (projectSongYaml proj) $ dir </> "notes.mid"
      let allTracks = concat $ previewTracks song
      (drums, layout) <- case snd $ allTracks !! index of
        PreviewDrumsFull layout drums -> return (drums, layout)
        _                             -> fatal "Not a DTX drums track"
      let planName = fst $ head $ HM.toList (projectSongYaml proj).plans
      RGAudio.projectAudio planName proj >>= \case
        Nothing -> return ()
        Just audio -> liftIO $ bracket_ SDL.initializeAll SDL.quit $ let
          windowConf = SDL.defaultWindow
            { SDL.windowResizable = True
            , SDL.windowHighDPI = False
            , SDL.windowInitialSize = SDL.V2 800 600
            , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Normal 3 3
              -- , SDL.glMultisampleSamples = 4
              }
            }
          in bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
            SDL.windowMinimumSize window $= SDL.V2 800 600
            bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
              RGAudio.withAL $ \_openedAudio -> do
                playDrumTrack window song layout drums audio
    case res of
      Left err -> throwIO err
      Right () -> return ()

  [con] -> void $ runResourceT $ logStdout $ do
    proj <- importOne con
    let dir = takeDirectory $ projectLocation proj
    trks <- fmap (concat . previewTracks) $ loadTracks (projectSongYaml proj) $ dir </> "notes.mid"
    stackIO $ forM_ (zip [0..] trks) $ \(i, (name, _)) -> do
      putStrLn $ show (i :: Int) <> ": " <> T.unpack name

  con : strIndexes -> do
    res <- runResourceT $ logStdout $ do
      indexes <- forM strIndexes $ maybe (fatal "Invalid track number") return . readMaybe
      proj <- importOne con
      let dir = takeDirectory $ projectLocation proj
      song <- loadTracks (projectSongYaml proj) $ dir </> "notes.mid"
      let allTracks = concat $ previewTracks song
          trks = map (snd . (allTracks !!)) indexes
          planName = fst $ head $ HM.toList (projectSongYaml proj).plans
      RGAudio.projectAudio planName proj >>= \case
        Nothing -> return ()
        Just audio -> liftIO $ bracket_ SDL.initializeAll SDL.quit $ let
          windowConf = SDL.defaultWindow
            { SDL.windowResizable = True
            , SDL.windowHighDPI = False
            , SDL.windowInitialSize = SDL.V2 800 600
            , SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
              { SDL.glProfile = SDL.Core SDL.Normal 3 3
              -- , SDL.glMultisampleSamples = 4
              }
            }
          in bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
            SDL.windowMinimumSize window $= SDL.V2 800 600
            bracket (SDL.glCreateContext window) (\ctx -> glFinish >> SDL.glDeleteContext ctx) $ \_ctx -> do
              RGAudio.withAL $ \_openedAudio -> do
                playTracks window song trks audio
    case res of
      Left err -> throwIO err
      Right () -> return ()

  _ -> error "Usage: onyx-game song_rb3con [track_number]"

data AppState = AppState
  { songTime       :: Milli
  , sdlStartedPlay :: Maybe (Milli, RGAudio.AudioHandle)
  }

-- TODO figure out how to get more accurate timing information
startMIDIListen :: (IO [VS.Vector Word8] -> IO a) -> IO a
startMIDIListen inner = do
  dev <- Rt.defaultInput
  ports <- Rt.listPorts dev
  case filter (\(_, name) -> "mio" `isPrefixOf` name) ports of
    [] -> do
      putStrLn "Warning: couldn't find the MIDI input"
      inner $ return []
    (port, _) : _ -> bracket_ (Rt.openPort dev port "Onyx-RtMidi") (Rt.closePort dev) $ do
      let go = do
            (_, v) <- Rt.getMessage dev
            if VS.null v
              then return []
              else (v :) <$> go
      inner go

playDrumTrack
  :: SDL.Window
  -> PreviewSong
  -> FullDrumLayout
  -> Map.Map Double (PNF.CommonState (PNF.DrumState FD.FullDrumNote FD.FullGem))
  -> (Double -> Maybe Double -> Float -> IO RGAudio.AudioHandle)
  -> IO ()
playDrumTrack window song layout trk audioPlayer = do
  Right glStuff <- logStdout $ loadGLStuff 1 song
  let ticksMilli :: IO Milli
      ticksMilli = MkFixed . fromIntegral <$> SDL.ticks
      delayMilli :: Milli -> IO ()
      delayMilli (MkFixed m) = threadDelay $ fromIntegral m * 1000
      eventMilli :: SDL.Event -> Milli
      eventMilli = MkFixed . fromIntegral . SDL.eventTimestamp
      halfWindow :: Double
      halfWindow = 0.05 -- 50 ms on each side
  _audioHandle <- audioPlayer 0 Nothing 1
  startedAt <- ticksMilli
  startMIDIListen $ \getMIDI -> do
    let loop prevState = do
          -- print $ PNF.drumEvents $ prevState
          frameStart <- ticksMilli
          let songTime = realToFrac $ frameStart - startedAt
              timePassed = PNF.applyFullDrumEvent songTime Nothing halfWindow prevState
          midi <- getMIDI
          sdl <- SDL.pollEvents
          processMIDI timePassed (map (songTime ,) midi) >>= (`processEvents` sdl) >>= \case
            Nothing -> return ()
            Just dps -> do
              timestamp <- ticksMilli
              let t = timestamp - startedAt
              SDL.V2 w h <- fmap fromIntegral <$> SDL.glGetDrawableSize window
              drawDrumPlayFull glStuff (WindowDims w h) (realToFrac t) 1 layout dps
              SDL.glSwapWindow window
              frameEnd <- ticksMilli
              delayMilli $ 0.016 - (frameEnd - frameStart)
              loop dps
        processMIDI s [] = return s
        processMIDI s ((time, e) : es) = case VS.toList e of
          [0x99, pitch, vel] | vel /= 0 -> let
            applyHit pad = processMIDI
              (PNF.applyFullDrumEvent time (Just $ PNF.FDInputHit $ PNF.FullDrumHit pad False $ realToFrac vel / 127) halfWindow s)
              es
            in case pitch of
              -- numbers from my default yamaha setup
              36 -> applyHit FD.Kick
              57 -> applyHit FD.Kick -- second kick plugged in as pad 9
              38 -> applyHit FD.Snare
              40 -> applyHit FD.Snare
              37 -> applyHit FD.Snare -- rim
              48 -> applyHit FD.Tom1
              47 -> applyHit FD.Tom2
              43 -> applyHit FD.Tom3
              46 -> applyHit FD.Hihat -- open
              78 -> applyHit FD.Hihat -- open
              42 -> applyHit FD.Hihat -- closed
              79 -> applyHit FD.Hihat -- closed
              -- 44 is hihat foot close, we ignore and use CC
              51 -> applyHit FD.CrashL
              52 -> applyHit FD.CrashL
              53 -> applyHit FD.CrashL
              49 -> applyHit FD.CrashR
              55 -> applyHit FD.CrashR
              59 -> applyHit FD.CrashR
              15 -> applyHit FD.Ride -- plugged in as pad 10
              _  -> processMIDI s es
          [0xB9, 4, value] -> let
            isOpen = value < 64 -- 127 closed to 0 open
            fdgs = case PNF.fdEvents s of
              (_, (_, gs)) : _ -> gs
              []               -> PNF.initialFDState
            newEvent = if isOpen
              then PNF.FDInputHihatOpen
              else PNF.FDInputHit $ PNF.FullDrumHit FD.HihatFoot False 1
            in if isOpen /= PNF.fdHihatOpen fdgs
              then processMIDI (PNF.applyFullDrumEvent time (Just newEvent) halfWindow s) es
              else processMIDI s es
          _ -> processMIDI s es
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
                (PNF.applyFullDrumEvent eventTime (Just $ PNF.FDInputHit $ PNF.FullDrumHit pad False 1) halfWindow s)
                es
              in case scan of
                SDL.ScancodeSpace -> applyHit FD.Kick
                SDL.ScancodeV     -> applyHit FD.Snare
                SDL.ScancodeB     -> applyHit FD.Tom1
                SDL.ScancodeN     -> applyHit FD.Tom2
                SDL.ScancodeM     -> applyHit FD.Tom3
                SDL.ScancodeC     -> applyHit FD.Hihat
                SDL.ScancodeF     -> applyHit FD.CrashL
                SDL.ScancodeK     -> applyHit FD.CrashR
                SDL.ScancodeComma -> applyHit FD.Ride
                SDL.ScancodeX     -> applyHit FD.HihatFoot
                _                 -> processEvents s es
          _ -> processEvents s es
    loop PNF.FullDrumPlayState
      { PNF.fdEvents = []
      , PNF.fdTrack = trk
      , PNF.fdNoteTimes = Set.fromList $ do
        (cst, cs) <- Map.toList trk
        guard $ not $ Set.null $ PNF.drumNotes $ PNF.commonState cs
        return cst
      }

playTracks :: SDL.Window -> PreviewSong -> [PreviewTrack] -> (Double -> Maybe Double -> Float -> IO RGAudio.AudioHandle) -> IO ()
playTracks window song trks audioPlayer = do
  Right glStuff <- logStdout $ loadGLStuff 1 song
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
                audioHandle <- audioPlayer (realToFrac $ songTime s) Nothing 1
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
        drawTracks glStuff (WindowDims w h) (realToFrac t) 1 Nothing trks
        SDL.glSwapWindow window
  loop $ AppState { songTime = 0, sdlStartedPlay = Nothing }
