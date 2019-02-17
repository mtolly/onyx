{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module GUI.FLTK where

import           Config
import           Control.Concurrent                        (ThreadId, forkIO)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                         as Exc
import           Control.Monad                             (forM_, void)
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Reader                (ReaderT, ask,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT, release,
                                                            resourceForkIO,
                                                            runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Maybe                                (fromMaybe)
import qualified Data.Text                                 as T
import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Graphics.UI.TinyFileDialogs               (openFileDialog)
import           OpenProject
import           System.FilePath                           (takeDirectory,
                                                            (</>))

data WindowState
  = LoadSong
    { wsWindow     :: FL.Ref FL.Window
    , wsLoadButton :: FL.Ref FL.Button
    }
  | LoadedSong
    { wsWindow  :: FL.Ref FL.Window
    , wsTabs    :: FL.Ref FL.Tabs
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
        FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
        fillWindow window
      resizeWindow :: FL.Ref FL.Window -> FL.Size -> IO ()
      resizeWindow window size = do
        x <- FL.getX window
        y <- FL.getY window
        FL.resize window $ FL.Rectangle (FL.Position x y) size
      fillWindow :: FL.Ref FL.Window -> IO WindowState
      fillWindow window = do
        FL.begin window -- I think windowNew does this automatically
        btn <- FL.buttonCustom
          (FL.Rectangle (FL.Position (FL.X 10) (FL.Y 30)) (FL.Size (FL.Width 95) (FL.Height 30)))
          (Just "Load song")
          Nothing
          $ Just $ let
            dnd ref = \case
              FLE.DndEnter -> return $ Right () -- we want dnd events
              FLE.DndDrag -> return $ Right ()
              FLE.DndLeave -> return $ Right ()
              FLE.DndRelease -> return $ Right () -- give us the text!
              FLE.Paste -> do
                str <- FLTK.eventText
                print str -- TODO report this fltkhs bug.
                -- we have to force the text here before we return,
                -- otherwise it gets collected on the C side
                sink $ EventLoad $ T.unpack str
                return $ Right ()
              e -> FL.handleSuper ref e
            in FL.defaultCustomWidgetFuncs { FL.handleCustom = Just dnd }
        FL.setLabelsize btn (FL.FontSize 13)
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
                tabs <- liftIO $ do
                  FL.destroy btn
                  let windowWidth = 800
                      windowHeight = 600
                      windowSize = FL.Size (FL.Width windowWidth) (FL.Height windowHeight)
                      tabHeight = 25
                      innerHeight = windowHeight - tabHeight
                      innerSize = FL.Size (FL.Width windowWidth) (FL.Height innerHeight)
                  resizeWindow window windowSize
                  FL.begin window
                  tabs <- FL.tabsNew
                    (FL.Rectangle (FL.Position (FL.X 0) (FL.Y 0)) windowSize)
                    Nothing
                  let tab :: T.Text -> (FL.Ref FL.Group -> IO a) -> IO a
                      tab name fn = do
                        group <- FL.groupNew
                          (FL.Rectangle (FL.Position (FL.X 0) (FL.Y tabHeight)) innerSize)
                          (Just name)
                        x <- fn group
                        FL.end group
                        return x
                      padded t r b l size@(FL.Size (FL.Width w) (FL.Height h)) fn = do
                        group <- FL.groupNew
                          (FL.Rectangle
                            (FL.Position (FL.X 0) (FL.Y 0))
                            (FL.Size (FL.Width (w + l + r)) (FL.Height (h + t + b)))
                          )
                          Nothing
                        x <- fn $ FL.Rectangle
                          (FL.Position (FL.X l) (FL.Y t))
                          size
                        FL.end group
                        FL.setResizable group $ Just x
                  meta <- tab "Metadata" $ \meta -> do
                    pack <- FL.packNew
                      (FL.Rectangle
                        (FL.Position (FL.X 0) (FL.Y tabHeight))
                        (FL.Size (FL.Width $ windowWidth - 200) (FL.Height innerHeight))
                      )
                      Nothing
                    forM_ (zip (True : repeat False) [("Title", _title), ("Artist", _artist), ("Album", _album)]) $ \(top, (str, fn)) -> do
                      padded (if top then 10 else 5) 10 5 100 (FL.Size (FL.Width 500) (FL.Height 25)) $ \rect -> do
                        input <- FL.inputNew
                          rect
                          (Just str)
                          (Just FL.FlNormalInput) -- this is required for labels to work. TODO report bug in binding's "inputNew"
                        FL.setLabelsize input $ FL.FontSize 13
                        FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
                        FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeftTop]
                        void $ FL.setValue input $ fromMaybe "" $ fn $ _metadata $ projectSongYaml proj
                        return input
                    padded 5 10 5 10 (FL.Size (FL.Width 500) (FL.Height 35)) $ \rect -> do
                      closeBtn <- FL.buttonNew rect (Just "Close song")
                      FL.setLabelsize closeBtn (FL.FontSize 13)
                      FL.setCallback closeBtn $ \_ -> sink EventClose
                      return closeBtn
                    FL.end pack
                    packRight <- FL.packNew
                      (FL.Rectangle
                        (FL.Position (FL.X (windowWidth - 200)) (FL.Y tabHeight))
                        (FL.Size (FL.Width 200) (FL.Height innerHeight))
                      )
                      Nothing
                    padded 10 10 0 0 (FL.Size (FL.Width 190) (FL.Height 190)) $ \rect -> do
                      cover <- FL.buttonNew rect Nothing
                      Right png <- FL.pngImageNew $ T.pack $ takeDirectory (projectLocation proj) </> "gen/cover.png"
                      FL.scale png (FL.Size (FL.Width 180) (FL.Height 180)) (Just True) (Just False)
                      FL.setImage cover $ Just png
                      return cover
                    FL.end packRight
                    FL.setResizable meta $ Just pack
                    return meta
                  tab "Audio" $ \_ -> return ()
                  tab "Instruments" $ \_ -> return ()
                  tab "Rock Band 3" $ \_ -> return ()
                  tab "Rock Band 2" $ \_ -> return ()
                  tab "Clone Hero/Phase Shift" $ \_ -> return ()
                  FL.end tabs
                  FL.setResizable tabs $ Just meta
                  FL.end window
                  FL.setResizable window $ Just tabs
                  FLTK.redraw
                  return tabs
                process $ LoadedSong window tabs proj
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
                Right proj -> do
                  void $ shakeBuild1 proj [] "gen/cover.png"
                  liftIO $ sink $ EventLoaded proj
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
