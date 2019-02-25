{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module GUI.FLTK where

import           Config
import           Control.Concurrent                        (ThreadId)
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

halvesHoriz :: Rectangle -> (Rectangle, Rectangle)
halvesHoriz rect@(Rectangle _ (Size (Width w) _)) = chopLeft (quot w 2) rect

halvesVert :: Rectangle -> (Rectangle, Rectangle)
halvesVert rect@(Rectangle _ (Size _ (Height h))) = chopTop (quot h 2) rect

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
        FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
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
  FL.setCallback window $ windowCloser $ mapM_ release $ projectRelease proj
  FL.showWidget window

-- | Ignores the Escape key, and runs a resource-release function after close.
windowCloser :: IO () -> FL.Ref FL.Window -> IO ()
windowCloser finalize window = do
  evt <- FLTK.event
  key <- FLTK.eventKey
  case (evt, key) of
    (FLE.Shortcut, FL.SpecialKeyType FLE.Kb_Escape) -> return ()
    _ -> do
      FL.hide window
      finalize

launchBatch :: (Event -> IO ()) -> [FilePath] -> IO ()
launchBatch sink startFiles = do
  loadedFiles <- newMVar startFiles
  let windowSize = Size (Width 800) (Height 400)
      windowRect = Rectangle
        (Position (X 0) (Y 0))
        windowSize
  window <- FL.windowNew windowSize Nothing $ Just "Batch Process"
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRangeWithArgs window (Size (Width 800) (Height 400)) FL.defaultOptionalSizeRangeArgs
    { FL.maxw = Just 800
    }
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  songs <- tab windowRect "Songs" $ \rect songs -> do
    let (labelRect, belowLabel) = chopTop 40 rect
        (termRect, buttonsRect) = chopBottom 50 belowLabel
        labelRect' = trimClock 10 10 5 10 labelRect
        termRect' = trimClock 5 10 5 10 termRect
        buttonsRect' = trimClock 5 10 10 10 buttonsRect
        (btnRectA, btnRectB) = halvesHoriz buttonsRect'
        (btnRectA', _) = chopRight 5 btnRectA
        (_, btnRectB') = chopLeft 5 btnRectB
    label <- FL.boxNew labelRect' $ Just "0 files loaded."
    term <- FL.simpleTerminalNew termRect' Nothing
    FL.setResizable songs $ Just term
    let updateFiles f = do
          fs <- f <$> takeMVar loadedFiles
          FL.setText term $ T.pack $ unlines fs
          FL.setLabel label $ T.pack $ case length fs of
            1 -> "1 file loaded."
            n -> show n ++ " files loaded."
          putMVar loadedFiles fs
    updateFiles id
    void $ FL.boxCustom termRect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just
        $ dragAndDrop (\newFiles -> sink $ EventIO $ updateFiles (++ newFiles))
        . (\_ _ -> return $ Left FL.UnknownEvent)
      }
    btnA <- FL.buttonNew btnRectA' $ Just "Add Song"
    FL.setCallback btnA $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseMultiFile
      FL.setTitle picker "Load song"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> do
          n <- FL.getCount picker
          fs <- forM [0 .. n - 1] $ FL.getFilenameAt picker . FL.AtIndex
          updateFiles (++ (map T.unpack $ catMaybes fs))
        _ -> return ()
    btnB <- FL.buttonNew btnRectB' $ Just "Clear Songs"
    FL.setCallback btnB $ \_ -> sink $ EventIO $ updateFiles $ const []
    return songs
  tab windowRect "Rock Band 3" $ \innerRect rb3 -> do
    pack <- FL.packNew innerRect Nothing
    padded 10 400 5 100 (Size (Width 300) (Height 35)) $ \rect -> do
      speed <- FL.counterNew rect (Just "Speed (%)")
      FL.setLabelsize speed $ FL.FontSize 13
      FL.setLabeltype speed FLE.NormalLabelType FL.ResolveImageLabelDoNothing
      FL.setAlign speed $ FLE.Alignments [FLE.AlignTypeLeft]
      FL.setStep speed 1
      FL.setLstep speed 5
      FL.setMinimum speed 1
      void $ FL.setValue speed 100
      FL.setTooltip speed "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
      return speed
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect -> do
      box <- FL.checkButtonNew rect (Just "Tom Markers for non-Pro Drums")
      FL.setTooltip box "When importing from a FoF/PS/CH chart where no Pro Drums are detected, tom markers will be added over the whole drum chart if this box is checked."
      return box
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect -> do
      box <- FL.checkButtonNew rect (Just "Drop HOPO/tap open notes")
      FL.setTooltip box "When checked, open notes on guitar/bass which are also HOPO or tap notes will be removed, instead of becoming green notes."
      return box
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect -> do
      let (rectAB, rectCD) = halvesHoriz rect
          (rectA, rectB) = halvesHoriz rectAB
          (rectC, rectD) = halvesHoriz rectCD
      optA <- FL.roundButtonNew rectA (Just "G/B/K unchanged")
      optB <- FL.roundButtonNew rectB (Just "Copy G to K")
      optC <- FL.roundButtonNew rectC (Just "Swap G and K")
      optD <- FL.roundButtonNew rectD (Just "Swap B and K")
      void $ FL.setValue optA True
      let opts = [optA, optB, optC, optD]
      forM_ opts $ \opt -> FL.setCallback opt $ \_ -> do
        forM_ opts $ \opt' -> FL.setValue opt' $ opt == opt'
      FL.groupNew rect Nothing
    padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect -> do
      let (fullA, fullB) = halvesHoriz rect
          (rectA, _) = chopRight 10 fullA
          (_, rectB) = chopLeft 10 fullB
      _con <- FL.buttonNew rectA $ Just "Create CON files"
      _magma <- FL.buttonNew rectB $ Just "Create Magma projects"
      FL.groupNew rect Nothing
    FL.end pack
    FL.setResizable rb3 $ Just pack
  tab windowRect "Rock Band 2" $ \_ _ -> return ()
  tab windowRect "Clone Hero/Phase Shift" $ \_ _ -> return ()
  FL.end tabs
  FL.setResizable tabs $ Just songs
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser $ return ()
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
    (Just "Onyx Console")
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
  FL.setCallback buttonLoad $ \_ -> loadDialog
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
    (Just $ menuFn loadDialog)
    (FL.MenuItemFlags [FL.MenuItemNormal])
  void $ FL.add menu
    "File/Close Window"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'w')
    (Just $ menuFn $ sink $ EventIO $ FLTK.firstWindow >>= \case
      Just window -> FL.doCallback window
      Nothing     -> return ()
    )
    (FL.MenuItemFlags [FL.MenuItemNormal])
  when macOS $ void $ FL.add menu
    "View/Show Console"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.Kb_CtrlState] $ FL.NormalKeyType '`')
    (Just $ menuFn $ FL.showWidget termWindow)
    (FL.MenuItemFlags [FL.MenuItemNormal])
  FL.end termWindow
  FL.setResizable termWindow $ Just term
  FL.setCallback termWindow $ windowCloser $ return ()
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
