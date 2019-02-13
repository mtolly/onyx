{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module GUI.FLTK where

import           Config
import           Control.Concurrent               (ThreadId, forkIO)
import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                as Exc
import           Control.Monad                    (forM_, void)
import           Control.Monad.IO.Class           (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.Resource     (ResourceT, release,
                                                   resourceForkIO, runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import qualified Graphics.UI.FLTK.LowLevel.FL as FLTK
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS as FL
import           Graphics.UI.TinyFileDialogs      (openFileDialog)
import           OpenProject

data WindowState
  = LoadSong
    { wsWindow     :: FL.Ref FL.Window
    , wsLoadButton :: FL.Ref FL.Button
    }
  | LoadedSong
    { wsWindow  :: FL.Ref FL.Window
    , wsGroup   :: FL.Ref FL.Group
    , wsProject :: Project
    }

data Event
  = EventLoad FilePath
  | EventLoaded Project
  | EventClose
  | EventFail Message
  | EventMsg (MessageLevel, Message)

type Onyx = StackTraceT (QueueLog (ReaderT (Event -> IO ()) (ResourceT IO)))

getEventSink :: Onyx (Event -> IO ())
getEventSink = lift $ lift ask

logToSink
  :: (MonadIO m)
  => (Event -> IO ())
  -> StackTraceT (QueueLog m) a
  -> m (Either Messages a)
logToSink sink = logIO $ sink . EventMsg

forkOnyx :: Onyx () -> Onyx ThreadId
forkOnyx act = do
  sink <- getEventSink
  chan <- lift $ lift ask
  lift $ lift $ lift $ resourceForkIO $ do
    res <- runReaderT (logToSink sink act) chan
    case res of
      Right () -> return ()
      Left (Messages msgs) -> forM_ msgs $ \msg -> do
        liftIO $ sink $ EventFail msg

launchGUI :: IO ()
launchGUI = do
  _ <- FLTK.setScheme "gtk+"
  void FLTK.lock -- this is required to get threads to work properly

  -- terminal
  termWindow <- FL.windowNew
    (FL.Size (FL.Width 500) (FL.Height 400))
    Nothing
    Nothing
  term <- FL.simpleTerminalNew
    (FL.Rectangle (FL.Position (FL.X 10) (FL.Y 10)) (FL.Size (FL.Width 480) (FL.Height 380)))
    Nothing
  FL.setStayAtBottom term True
  FL.end termWindow
  FL.showWidget termWindow

  evts <- newTChanIO
  let sink e = do
        atomically $ writeTChan evts e
        FLTK.awake -- this makes waitFor finish so we can process the event
      logChan = logIO $ sink . EventMsg
      newWindow = do
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
        FL.setCallback btn $ \_ -> void $ forkIO $ do
          openFileDialog "Load song" "" [] "Songs" False >>= \case
            Just [f] -> sink $ EventLoad $ T.unpack f
            _        -> return ()
        FL.end window
        FL.showWidget window
        return $ LoadSong window btn
      addTerm pair = do
        -- TODO this should use `append` method but it's not bound for some reason
        txt <- FL.getText term
        let newtxt = T.pack $ case pair of
              (MessageLog    , msg) -> messageString msg
              (MessageWarning, msg) -> "Warning: " ++ Exc.displayException msg
        FL.setText term $ txt <> newtxt <> "\n"
  void $ runResourceT $ (`runReaderT` sink) $ logChan $ let
    loop ws = liftIO FLTK.getProgramShouldQuit >>= \case
      True  -> return ()
      False -> let
        process ws' = liftIO (atomically $ tryReadTChan evts) >>= \case
          Nothing -> return ws'
          Just e -> case e of
            EventMsg pair -> do
              liftIO $ addTerm pair
              process ws'
            EventFail msg -> do
              liftIO $ addTerm (MessageWarning, msg)
              process ws'
            EventLoaded proj -> case ws' of
              LoadSong window btn -> do
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
                  FL.setCallback btn' $ \_ -> sink EventClose
                  FL.end group
                  FL.end window
                  FLTK.redraw
                  return group
                process $ LoadedSong window group proj
              _ -> process ws'
            EventLoad f -> do
              case ws' of
                LoadSong _ btn -> liftIO $ do
                  FL.setLabel btn "Loading…"
                  FLTK.redraw
                _ -> return ()
              void $ forkOnyx $ errorToEither (openProject f) >>= \case
                Left (Messages msgs) -> liftIO $ forM_ msgs $ \msg -> do
                  liftIO $ sink $ EventFail msg
                Right proj -> liftIO $ sink $ EventLoaded proj
              process ws'
            EventClose -> case ws' of
              LoadedSong window group proj -> do
                mapM_ release $ projectRelease proj
                liftIO $ FL.destroy group
                liftIO $ resizeWindow window $ FL.Size (FL.Width 115) (FL.Height 100)
                filled <- liftIO $ fillWindow window
                liftIO $ FLTK.redraw
                process filled
              _ -> process ws'
        in liftIO (FLTK.waitFor 1e20) >> process ws >>= loop
    in liftIO newWindow >>= loop
  FLTK.flush -- dunno if required
