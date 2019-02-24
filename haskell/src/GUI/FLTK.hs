{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module GUI.FLTK where

import           Config
import           Control.Concurrent                        (ThreadId, forkIO)
import           Control.Concurrent.MVar                   (newMVar, putMVar,
                                                            takeMVar)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan
import qualified Control.Exception                         as Exc
import           Control.Monad                             (forM, forM_, void,
                                                            when)
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Reader                (ReaderT, ask,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT, release,
                                                            resourceForkIO,
                                                            runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe)
import qualified Data.Text                                 as T
import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Position (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..), X (..),
                                                            Y (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           OpenProject
import           System.FilePath                           (takeDirectory,
                                                            (</>))
import           System.Info                               (os)

data Event
  = EventIO (IO ())
  | EventOnyx (Onyx ())
  | EventLoad FilePath
  | EventLoaded Project
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

resizeWindow :: FL.Ref FL.Window -> Size -> IO ()
resizeWindow window size = do
  x <- FL.getX window
  y <- FL.getY window
  FL.resize window $ Rectangle (Position x y) size

startLoad :: FilePath -> Onyx ()
startLoad f = do
  sink <- getEventSink
  void $ forkOnyx $ errorToEither (openProject f) >>= \case
    Left (Messages msgs) -> liftIO $ forM_ msgs $ \msg -> do
      liftIO $ sink $ EventFail msg
    Right proj -> do
      void $ shakeBuild1 proj [] "gen/cover.png"
      liftIO $ sink $ EventLoaded proj

chopRight :: Int -> Rectangle -> (Rectangle, Rectangle)
chopRight n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width $ w - n) (Height h))
  , Rectangle (Position (X $ x + w - n) (Y y)) (Size (Width n) (Height h))
  )

chopLeft :: Int -> Rectangle -> (Rectangle, Rectangle)
chopLeft n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width n) (Height h))
  , Rectangle (Position (X $ x + n) (Y y)) (Size (Width $ w - n) (Height h))
  )

chopTop :: Int -> Rectangle -> (Rectangle, Rectangle)
chopTop n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height n))
  , Rectangle (Position (X x) (Y $ y + n)) (Size (Width w) (Height $ h - n))
  )

chopBottom :: Int -> Rectangle -> (Rectangle, Rectangle)
chopBottom n (Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) =
  ( Rectangle (Position (X x) (Y y)) (Size (Width w) (Height $ h - n))
  , Rectangle (Position (X x) (Y $ y + h - n)) (Size (Width w) (Height n))
  )

trimClock :: Int -> Int -> Int -> Int -> Rectangle -> Rectangle
trimClock t r b l rect = let
  (_, rect1) = chopTop    t rect
  (rect2, _) = chopRight  r rect1
  (rect3, _) = chopBottom b rect2
  (_, rect4) = chopLeft   l rect3
  in rect4

padded
  :: (FL.Parent a FL.Widget)
  => Int -> Int -> Int -> Int
  -> Size
  -> (Rectangle -> IO (FL.Ref a))
  -> IO ()
padded t r b l size@(Size (Width w) (Height h)) fn = do
  group <- FL.groupNew
    (Rectangle
      (Position (X 0) (Y 0))
      (Size (Width (w + l + r)) (Height (h + t + b)))
    )
    Nothing
  x <- fn $ Rectangle
    (Position (X l) (Y t))
    size
  FL.end group
  FL.setResizable group $ Just x

tab :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Group -> IO a) -> IO a
tab rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
  group <- FL.groupNew innerRect (Just name)
  res <- fn innerRect group
  FL.end group
  return res

tabScroll :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Scrolled -> IO a) -> IO a
tabScroll rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
      scrollSize = 15
      (scrollArea, _) = chopRight scrollSize innerRect
  scroll <- FL.scrolledNew innerRect (Just name)
  FL.setBox scroll FLE.ThinUpFrame
  FL.setType scroll FL.VerticalAlwaysScrollBar
  FL.setScrollbarSize scroll scrollSize
  res <- fn scrollArea scroll
  FL.end scroll
  return res

launchWindow :: Project -> IO ()
launchWindow proj = do
  let windowSize = Size (Width 800) (Height 600)
      windowRect = Rectangle
        (Position (X 0) (Y 0))
        windowSize
  window <- FL.windowNew windowSize Nothing Nothing
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRangeWithArgs window (Size (Width 800) (Height 100)) FL.defaultOptionalSizeRangeArgs
    { FL.maxw = Just 800
    }
  FL.begin window
  tabs <- FL.tabsNew
    (Rectangle (Position (X 0) (Y 0)) windowSize)
    Nothing
  meta <- tabScroll windowRect "Metadata" $ \innerRect meta -> do
    let (rectLeft, rectRight) = chopRight 200 innerRect
    pack <- FL.packNew rectLeft Nothing
    forM_ (zip (True : repeat False) [("Title", _title), ("Artist", _artist), ("Album", _album)]) $ \(top, (str, fn)) -> do
      padded (if top then 10 else 5) 10 5 100 (Size (Width 500) (Height 25)) $ \rect -> do
        input <- FL.inputNew
          rect
          (Just str)
          (Just FL.FlNormalInput) -- this is required for labels to work. TODO report bug in binding's "inputNew"
        FL.setLabelsize input $ FL.FontSize 13
        FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
        FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeftTop]
        void $ FL.setValue input $ fromMaybe "" $ fn $ _metadata $ projectSongYaml proj
        return input
    FL.end pack
    packRight <- FL.packNew rectRight Nothing
    padded 10 10 0 0 (Size (Width 190) (Height 190)) $ \rect -> do
      cover <- FL.buttonNew rect Nothing
      Right png <- FL.pngImageNew $ T.pack $ takeDirectory (projectLocation proj) </> "gen/cover.png"
      FL.scale png (Size (Width 180) (Height 180)) (Just True) (Just False)
      FL.setImage cover $ Just png
      return cover
    FL.end packRight
    FL.setResizable meta $ Just pack
    return meta
  tab windowRect "Audio" $ \_ _ -> return ()
  tab windowRect "Instruments" $ \_ _ -> return ()
  tab windowRect "Rock Band 3" $ \_ _ -> return ()
  tab windowRect "Rock Band 2" $ \_ _ -> return ()
  tab windowRect "Clone Hero/Phase Shift" $ \_ _ -> return ()
  FL.end tabs
  FL.setResizable tabs $ Just meta
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ \_ -> do
    FL.hide window
    mapM_ release $ projectRelease proj
  FL.showWidget window

launchBatch :: (Event -> IO ()) -> [FilePath] -> IO ()
launchBatch sink startFiles = do
  loadedFiles <- newMVar startFiles
  let windowSize = Size (Width 800) (Height 600)
      windowRect = Rectangle
        (Position (X 0) (Y 0))
        windowSize
  window <- FL.windowNew windowSize Nothing Nothing
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  songs <- tab windowRect "Songs" $ \rect songs -> do
    let rect' = trimClock 10 10 10 10 rect
    term <- FL.simpleTerminalNew rect' Nothing
    FL.setResizable songs $ Just term
    let updateFiles f = do
          fs <- f <$> takeMVar loadedFiles
          FL.setText term $ T.pack $ unlines fs
          putMVar loadedFiles fs
    updateFiles id
    void $ FL.boxCustom rect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just
        $ dragAndDrop (\newFiles -> sink $ EventIO $ updateFiles (++ newFiles))
        . (\_ _ -> return $ Left FL.UnknownEvent)
      }
    return songs
  tab windowRect "Rock Band 3" $ \_ _ -> return ()
  tab windowRect "Rock Band 2" $ \_ _ -> return ()
  tab windowRect "Clone Hero/Phase Shift" $ \_ _ -> return ()
  FL.end tabs
  FL.setResizable tabs $ Just songs
  FL.end window
  FL.setResizable window $ Just tabs
  FL.showWidget window

{-
-- doesn't appear to be in the bundled code
type OpenCallback = CString -> IO ()
foreign import ccall unsafe "fl_open_callback"
  fl_open_callback :: FunPtr OpenCallback -> IO ()
foreign import ccall "wrapper"
  mkOpenCallback :: OpenCallback -> IO (FunPtr OpenCallback)
-}

dragAndDrop
  :: ([String] -> IO ())
  -> (FLE.Event -> IO (Either a ()))
  -> FLE.Event
  -> IO (Either a ())
dragAndDrop f fallback = \case
  FLE.DndEnter -> return $ Right () -- we want dnd events
  FLE.DndDrag -> return $ Right ()
  FLE.DndLeave -> return $ Right ()
  FLE.DndRelease -> return $ Right () -- give us the text!
  FLE.Paste -> do
    str <- FLTK.eventText
    print str -- TODO report this fltkhs bug.
    -- we have to force the text here before we return,
    -- otherwise it gets collected on the C side
    () <- f $ lines $ T.unpack str
    -- lines is because multiple files are separated by \n
    return $ Right ()
  e -> fallback e

macOS :: Bool
macOS = os == "darwin"

launchGUI :: IO ()
launchGUI = do
  _ <- FLTK.setScheme "gtk+"
  void FLTK.lock -- this is required to get threads to work properly

  evts <- newTChanIO
  let sink e = do
        atomically $ writeTChan evts e
        FLTK.awake -- this makes waitFor finish so we can process the event

  -- terminal
  termWindow <- FL.windowNew
    (Size (Width 500) (Height 400))
    Nothing
    Nothing
  let termMenuHeight = if macOS then 0 else 30
  term <- FL.simpleTerminalNew
    (Rectangle
      (Position (X 10) (Y $ 10 + termMenuHeight))
      (Size (Width 480) (Height $ 340 - termMenuHeight))
    )
    Nothing
  FL.setStayAtBottom term True
  let loadSongs = mapM_ $ sink . EventLoad
      loadDialog = sink $ EventIO $ do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseMultiFile
        FL.setTitle picker "Load song"
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> do
            n <- FL.getCount picker
            fs <- forM [0 .. n - 1] $ FL.getFilenameAt picker . FL.AtIndex
            loadSongs $ map T.unpack $ catMaybes fs
          _ -> return ()
  buttonLoad <- FL.buttonCustom
    (Rectangle (Position (X 10) (Y 360)) (Size (Width 235) (Height 30)))
    (Just "Load song")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs { FL.handleCustom = Just $ dragAndDrop loadSongs . FL.handleSuper }
  FL.setLabelsize buttonLoad (FL.FontSize 13)
  FL.setCallback buttonLoad $ \_ -> void $ forkIO loadDialog
  buttonBatch <- FL.buttonCustom
    (Rectangle (Position (X 255) (Y 360)) (Size (Width 235) (Height 30)))
    (Just "Batch process")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs { FL.handleCustom = Just $ dragAndDrop (launchBatch sink) . FL.handleSuper }
  FL.setLabelsize buttonBatch (FL.FontSize 13)
  FL.setCallback buttonBatch $ \_ -> launchBatch sink []
  menu <- FL.sysMenuBarNew
    (Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height termMenuHeight)))
    Nothing
  let menuFn :: IO () -> FL.Ref FL.MenuItem -> IO ()
      menuFn = const
  void $ FL.add menu
    "File/Open…"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'o')
    (Just $ menuFn $ void $ forkIO loadDialog)
    (FL.MenuItemFlags [FL.MenuItemNormal])
  when macOS $ void $ FL.add menu
    "View/Show Console"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.Kb_CtrlState] $ FL.NormalKeyType '`')
    (Just $ menuFn $ FL.showWidget termWindow)
    (FL.MenuItemFlags [FL.MenuItemNormal])
  FL.end termWindow
  FL.setResizable termWindow $ Just term
  FL.showWidget termWindow

  let logChan = logIO $ sink . EventMsg
      addTerm pair = do
        -- TODO this should use `append` method but it's not bound for some reason
        txt <- FL.getText term
        let newtxt = T.pack $ case pair of
              (MessageLog    , msg) -> messageString msg
              (MessageWarning, msg) -> "Warning: " ++ Exc.displayException msg
        FL.setText term $ txt <> newtxt <> "\n"
      wait = if macOS
        then FLTK.waitFor 1e20 >> return True
        else fmap (/= 0) FLTK.wait
  void $ runResourceT $ (`runReaderT` sink) $ logChan $ let
    process = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ addTerm pair
          EventFail   msg  -> liftIO $ addTerm (MessageWarning, msg)
          EventLoaded proj -> liftIO $ launchWindow proj
          EventLoad   f    -> startLoad f
          EventIO     act  -> liftIO act
          EventOnyx   act  -> act
        process
    loop = liftIO FLTK.getProgramShouldQuit >>= \case
      True  -> return ()
      False -> liftIO wait >>= \case
        False -> return ()
        True  -> process >> loop
    in loop
  FLTK.flush -- dunno if required
