{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}
module GUI.FLTK where

import           CommandLine                               (copyDirRecursive)
import           Config
import           Control.Concurrent                        (MVar, ThreadId,
                                                            killThread, newMVar,
                                                            putMVar, readMVar,
                                                            takeMVar)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan              (newTChanIO,
                                                            tryReadTChan,
                                                            writeTChan)
import qualified Control.Exception                         as Exc
import           Control.Monad                             (ap, forM, forM_,
                                                            liftM, void, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Reader                (ReaderT, ask, local,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT, release,
                                                            resourceForkIO,
                                                            runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Default.Class                        (def)
import           Data.Functor.Const                        (Const (..))
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, isJust)
import qualified Data.Text                                 as T
import qualified Data.Yaml                                 as Y
import           Foreign                                   (Ptr)
import           Foreign.C                                 (CString,
                                                            withCString)
import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Position (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..), X (..),
                                                            Y (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           JSONData                                  (toJSON)
import           OpenProject
import           RockBand.Codec.File                       (FlexPartName (..))
import qualified System.Directory                          as Dir
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
  :: Int -> Int -> Int -> Int
  -> Size
  -> (Rectangle -> IO a)
  -> IO a
padded t r b l size@(Size (Width w) (Height h)) fn = do
  group <- FL.groupNew
    (Rectangle
      (Position (X 0) (Y 0))
      (Size (Width (w + l + r)) (Height (h + t + b)))
    )
    Nothing
  let rect = Rectangle (Position (X l) (Y t)) size
  x <- fn rect
  box <- FL.boxNew rect Nothing
  FL.end group
  FL.setResizable group $ Just box
  return x

makeTab :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Group -> IO a) -> IO a
makeTab rect name fn = do
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
  meta <- tabScroll windowRect "Metadata" $ \rect meta -> do
    let (rectLeft, rectRight) = chopRight 200 rect
    pack <- FL.packNew rectLeft Nothing
    forM_ (zip (True : repeat False) [("Title", _title), ("Artist", _artist), ("Album", _album)]) $ \(top, (str, fn)) -> do
      padded (if top then 10 else 5) 10 5 100 (Size (Width 500) (Height 25)) $ \rect' -> do
        input <- FL.inputNew
          rect'
          (Just str)
          (Just FL.FlNormalInput) -- this is required for labels to work. TODO report bug in binding's "inputNew"
        FL.setLabelsize input $ FL.FontSize 13
        FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
        FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
        void $ FL.setValue input $ fromMaybe "" $ fn $ _metadata $ projectSongYaml proj
        return ()
    FL.end pack
    packRight <- FL.packNew rectRight Nothing
    padded 10 10 0 0 (Size (Width 190) (Height 190)) $ \rect' -> do
      cover <- FL.buttonNew rect' Nothing
      Right png <- FL.pngImageNew $ T.pack $ takeDirectory (projectLocation proj) </> "gen/cover.png"
      FL.scale png (Size (Width 180) (Height 180)) (Just True) (Just False)
      FL.setImage cover $ Just png
      return ()
    FL.end packRight
    FL.setResizable meta $ Just pack
    return meta
  makeTab windowRect "Audio" $ \_ _ -> return ()
  makeTab windowRect "Instruments" $ \_ _ -> return ()
  makeTab windowRect "Rock Band 3" $ \_ _ -> return ()
  makeTab windowRect "Rock Band 2" $ \_ _ -> return ()
  makeTab windowRect "Clone Hero/Phase Shift" $ \_ _ -> return ()
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

forceProDrums :: SongYaml -> SongYaml
forceProDrums song = song
  { _parts = flip fmap (_parts song) $ \part -> case partDrums part of
    Nothing    -> part
    Just drums -> part
      { partDrums = Just drums
        { drumsMode = case drumsMode drums of
          Drums4 -> DrumsPro
          mode   -> mode
        }
      }
  }

dropOpenHOPOs :: Bool -> SongYaml -> SongYaml
dropOpenHOPOs b song = song
  { _parts = flip fmap (_parts song) $ \part -> case partGRYBO part of
    Nothing    -> part
    Just grybo -> part
      { partGRYBO = Just grybo { gryboDropOpenHOPOs = b }
      }
  }

saveProject :: Project -> SongYaml -> IO Project
saveProject proj song = do
  Y.encodeFile (projectLocation proj) $ toJSON song
  return proj { projectSongYaml = song }

data GBKOption
  = GBKUnchanged
  | CopyGuitarToKeys
  | SwapGuitarKeys
  | SwapBassKeys
  deriving (Eq, Show)

applyGBK :: GBKOption -> SongYaml -> TargetRB3 -> TargetRB3
applyGBK gbk song tgt = case gbk of
  GBKUnchanged -> tgt
  CopyGuitarToKeys -> if hasFive FlexGuitar
    then tgt { rb3_Keys = FlexGuitar }
    else tgt
  SwapGuitarKeys -> if hasFive FlexKeys
    then tgt { rb3_Guitar = FlexKeys, rb3_Keys = FlexGuitar }
    else tgt
  SwapBassKeys -> if hasFive FlexKeys
    then tgt { rb3_Bass = FlexKeys, rb3_Keys = FlexBass }
    else tgt
  where hasFive flex = isJust $ getPart flex song >>= partGRYBO

batchPageRB3
  :: Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> ([TargetRB3], SongYaml)) -> Bool -> IO ())
  -> IO ()
batchPageRB3 rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 400 5 100 (Size (Width 300) (Height 35)) $ \rect' -> do
    speed <- FL.counterNew rect' (Just "Speed (%)")
    FL.setLabelsize speed $ FL.FontSize 13
    FL.setLabeltype speed FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign speed $ FLE.Alignments [FLE.AlignTypeLeft]
    FL.setStep speed 1
    FL.setLstep speed 5
    FL.setMinimum speed 1
    void $ FL.setValue speed 100
    FL.setTooltip speed "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
    return $ (/ 100) <$> FL.getValue speed
  getToms <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Tom Markers for non-Pro Drums")
    FL.setTooltip box "When importing from a FoF/PS/CH chart where no Pro Drums are detected, tom markers will be added over the whole drum chart if this box is checked."
    return $ FL.getValue box
  getDropOpen <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Drop HOPO/tap open notes")
    FL.setTooltip box "When checked, open notes on guitar/bass which are also HOPO or tap notes will be removed, instead of becoming green notes."
    return $ FL.getValue box
  getGBK <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (rectAB, rectCD) = halvesHoriz rect'
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
    return $ FL.getValue optB >>= \case
      True -> return CopyGuitarToKeys
      False -> FL.getValue optC >>= \case
        True -> return SwapGuitarKeys
        False -> FL.getValue optD >>= \case
          True -> return SwapBassKeys
          False -> return GBKUnchanged
  let getTargetSong = do
        speed <- getSpeed
        toms <- getToms
        dropOpen <- getDropOpen
        gbk <- getGBK
        return $ \proj -> let
          tgt = (applyGBK gbk yaml) def
            { rb3_Common = (rb3_Common def)
              { tgt_Speed = Just speed
              }
            }
          yaml
            = dropOpenHOPOs dropOpen
            $ (if toms then id else forceProDrums)
            $ projectSongYaml proj
          in ([tgt], yaml)
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (fullA, fullB) = halvesHoriz rect'
        (rectA, _) = chopRight 10 fullA
        (_, rectB) = chopLeft 10 fullB
    btnCON <- FL.buttonNew rectA $ Just "Create CON files"
    btnMagma <- FL.buttonNew rectB $ Just "Create Magma projects"
    FL.setCallback btnCON $ \_ -> do
      settings <- getTargetSong
      build settings True
    FL.setCallback btnMagma $ \_ -> do
      settings <- getTargetSong
      build settings False
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

launchBatch :: (Event -> IO ()) -> [FilePath] -> IO ()
launchBatch sink startFiles = mdo
  loadedFiles <- newMVar startFiles
  let windowSize = Size (Width 800) (Height 400)
      windowRect = Rectangle
        (Position (X 0) (Y 0))
        windowSize
  window <- FL.windowNew windowSize Nothing $ Just "Batch Process"
  FLE.rgbColorWithRgb (94,94,94) >>= FL.setColor window -- TODO report incorrect Char binding type for rgbColorWithGrayscale
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRangeWithArgs window (Size (Width 800) (Height 400)) FL.defaultOptionalSizeRangeArgs
    { FL.maxw = Just 800
    }
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  let setTabColor :: FL.Ref FL.Group -> FLE.Color -> IO ()
      setTabColor tab color = do
        FL.setColor tab color
        FL.setSelectionColor tab color
  tabSongs <- makeTab windowRect "Songs" $ \rect tab -> do
    FLE.rgbColorWithRgb (167,229,165) >>= setTabColor tab
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
    FL.setResizable tab $ Just term
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
    return tab
  functionColor <- FLE.rgbColorWithRgb (229,165,183)
  functionTabs <- sequence
    [ makeTab windowRect "Rock Band 3 (360)" $ \rect tab -> do
      setTabColor tab functionColor
      batchPageRB3 rect tab $ \settings buildCON -> sink $ EventOnyx $ let
        start buildType useResult = do
          files <- stackIO $ readMVar loadedFiles
          startTasks $ zip files $ flip map files $ \f -> withProject f $ \proj -> do
            let (targets, yaml) = settings proj
                addSegments target p = p
                  { projectTemplate = concat
                    [ projectTemplate p
                    , case tgt_Speed $ rb3_Common target of
                      Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                      _               -> ""
                    ]
                  }
            forM targets $ \target -> do
              proj' <- stackIO $ addSegments target <$> saveProject proj yaml
              buildType target proj' >>= useResult proj'
        in if buildCON
          then start buildRB3CON $ \proj result -> let
            fout = projectTemplate proj <> "_rb3con" -- TODO add speed, etc.
            in stackIO (Dir.copyFile result fout) >> return fout
          else start buildMagmaV2 $ \proj result -> let
            dout = projectTemplate proj <> "_project" -- TODO add speed, etc.
            in copyDirRecursive result dout >> return dout
      return tab
    , makeTab windowRect "Rock Band 2 (360)" $ \_ tab -> do
      setTabColor tab functionColor
      return tab
    , makeTab windowRect "Clone Hero/Phase Shift" $ \_ tab -> do
      setTabColor tab functionColor
      return tab
    , makeTab windowRect "Rock Band 3 (Wii)" $ \_ tab -> do
      setTabColor tab functionColor
      return tab
    , makeTab windowRect "Preview" $ \_ tab -> do
      setTabColor tab functionColor
      return tab
    ]
  let nonTermTabs = tabSongs : functionTabs
  (tabTask, labelTask, termTask) <- makeTab windowRect "Task" $ \rect tab -> do
    FLE.rgbColorWithRgb (239,201,148) >>= setTabColor tab
    let (labelRect, belowLabel) = chopTop 40 rect
        (termRect, buttonRect) = chopBottom 50 belowLabel
        labelRect' = trimClock 10 10 5 10 labelRect
        termRect' = trimClock 5 10 5 10 termRect
        buttonRect' = trimClock 5 10 10 10 buttonRect
    label <- FL.boxNew labelRect' $ Just "…"
    term <- FL.simpleTerminalNew termRect' Nothing
    FL.setAnsi term True
    FL.setStayAtBottom term True
    FL.setResizable tab $ Just term
    btnA <- FL.buttonNew buttonRect' $ Just "Cancel"
    FL.setCallback btnA $ \_ -> sink $ EventIO cancelTasks
    return (tab, label, term)
  FL.deactivate tabTask
  taskStatus <- newMVar Nothing :: IO (MVar (Maybe (Int, Int, [FilePath], Int, ThreadId)))
  let taskMessage :: (MessageLevel, Message) -> IO ()
      taskMessage = sink . EventIO . addTerm termTask . toTermMessage
      cancelTasks = do
        takeMVar taskStatus >>= \case
          Just (_, _, _, _, tid) -> do
            addTerm termTask $ TermLog "Cancelled tasks."
            killThread tid
          Nothing                -> return ()
        putMVar taskStatus Nothing
        reenableTabs
      reenableTabs = do
        mapM_ FL.activate nonTermTabs
        FLTK.redraw
      updateStatus fn = takeMVar taskStatus >>= \case
        Nothing        -> putMVar taskStatus Nothing -- maybe task was cancelled
        Just oldStatus -> do
          let status@(done, total, success, failure, _) = fn oldStatus
          FL.setLabel labelTask $ T.pack $ unwords
            [ show done <> "/" <> show total
            , "tasks complete:"
            , show $ length success
            , "succeeded,"
            , show failure
            , "failed"
            ]
          if done >= total
            then do
              reenableTabs
              putMVar taskStatus Nothing
            else putMVar taskStatus $ Just status
      startTasks :: [(FilePath, Onyx [FilePath])] -> Onyx ()
      startTasks tasks = liftIO (takeMVar taskStatus) >>= \case
        Just status -> liftIO $ putMVar taskStatus $ Just status -- shouldn't happen
        Nothing -> do
          tid <- forkOnyx $ replaceQueueLog taskMessage $ forM_ (zip [1..] tasks) $ \(i, (fin, task)) -> do
            liftIO $ sink $ EventIO $ addTerm termTask $ TermStart ("Task " <> show (i :: Int)) fin
            errorToEither task >>= \case
              Left e -> liftIO $ sink $ EventIO $ do
                forM_ (getMessages e) $ addTerm termTask . TermError
                updateStatus $ \case
                  (done, total, success, failure, tid) -> (done + 1, total, success, failure + 1, tid)
              Right fs -> liftIO $ sink $ EventIO $ do
                forM_ fs $ \f -> addTerm termTask $ TermSuccess $ "Created file: " <> f
                updateStatus $ \case
                  (done, total, success, failure, tid) -> (done + 1, total, success ++ fs, failure, tid)
          liftIO $ do
            putMVar taskStatus $ Just (0, length tasks, [], 0, tid)
            FL.activate tabTask
            mapM_ FL.deactivate nonTermTabs
            void $ FL.setValue tabs $ Just tabTask
            updateTabsColor tabs
            updateStatus id
  FL.end tabs
  updateTabsColor tabs
  FL.setCallback tabs updateTabsColor
  FL.setResizable tabs $ Just tabSongs
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser $ takeMVar taskStatus >>= \case
    Just (_, _, _, _, tid) -> killThread tid
    Nothing                -> return ()
  FL.showWidget window

updateTabsColor :: FL.Ref FL.Tabs -> IO ()
updateTabsColor tabs = FL.getValue tabs >>= \case
  Nothing  -> return ()
  Just tab -> FL.getSelectionColor tab >>= FL.setSelectionColor tabs

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
    () <- f $ lines $ T.unpack str
    -- lines is because multiple files are separated by \n
    return $ Right ()
  e -> fallback e

replaceQueueLog :: ((MessageLevel, Message) -> IO ()) -> Onyx a -> Onyx a
replaceQueueLog q = mapStackTraceT $ QueueLog . local (const q) . fromQueueLog

data TermMessage
  = TermStart String String -- "Task 4" "/path/to/file"
  | TermLog String
  | TermWarning Message
  | TermError Message
  | TermSuccess String

toTermMessage :: (MessageLevel, Message) -> TermMessage
toTermMessage (MessageLog    , msg) = TermLog $ messageString msg
toTermMessage (MessageWarning, msg) = TermWarning msg

addTerm :: FL.Ref FL.SimpleTerminal -> TermMessage -> IO ()
addTerm term pair = do
  let newtxt = case pair of
        TermStart x y   -> "\ESC[46m" <> x <> "\ESC[0m" <> ": " <> y
        TermLog str     -> str
        TermWarning msg -> "\ESC[33mWarning\ESC[0m: " <> Exc.displayException msg
        TermError msg   -> "\ESC[41mERROR!\ESC[0m " <> Exc.displayException msg
        TermSuccess str -> "\ESC[42mSuccess!\ESC[0m " <> str
  FL.withRef term $ \ptr ->
    withCString (newtxt <> "\n") $ \cs ->
      c_append ptr cs

-- present in C layer but missing from Haskell
foreign import ccall unsafe "Fl_Simple_Terminal_append"
  c_append :: Ptr () -> CString -> IO ()

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
  FLE.rgbColorWithRgb (114,74,124) >>= FL.setColor termWindow
  let termMenuHeight = if macOS then 0 else 30
  term <- FL.simpleTerminalNew
    (Rectangle
      (Position (X 10) (Y $ 10 + termMenuHeight))
      (Size (Width 480) (Height $ 340 - termMenuHeight))
    )
    Nothing
  FL.setAnsi term True
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
  FLE.rgbColorWithRgb (216,125,154) >>= FL.setColor buttonLoad
  FL.setLabelsize buttonLoad (FL.FontSize 13)
  FL.setCallback buttonLoad $ \_ -> loadDialog
  buttonBatch <- FL.buttonCustom
    (Rectangle (Position (X 255) (Y 360)) (Size (Width 235) (Height 30)))
    (Just "Batch process")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs { FL.handleCustom = Just $ dragAndDrop (launchBatch sink) . FL.handleSuper }
  FLE.rgbColorWithRgb (141,136,224) >>= FL.setColor buttonBatch
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
    "File/Batch Process"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'b')
    (Just $ menuFn $ launchBatch sink [])
    (FL.MenuItemFlags [FL.MenuItemNormal])
  void $ FL.add menu
    "File/Close Window"
    (Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'w')
    (Just $ menuFn $ sink $ EventIO $ FLTK.firstWindow >>= \case
      -- TODO firstWindow usually works, unless you open windows and don't do anything with them
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
      wait = if macOS
        then FLTK.waitFor 1e20 >> return True
        else fmap (/= 0) FLTK.wait
  void $ runResourceT $ (`runReaderT` sink) $ logChan $ let
    process = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ addTerm term $ toTermMessage pair
          EventFail   msg  -> liftIO $ addTerm term $ TermError msg
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

--
-- WIP form-building stuff
--

-- this is a silly usage of codec but whatever
type ControlFor = CodecFor (Const ()) IOIO
type Control a = ControlFor a a
newtype IOIO a = IOIO (IO (IO a))

instance Functor IOIO where
  fmap = liftM
instance Applicative IOIO where
  (<*>) = ap
  pure = return
instance Monad IOIO where
  return = IOIO . return . return
  IOIO f >>= g = IOIO $ do
    getter1 <- f
    x <- getter1
    case g x of
      IOIO getter2 -> getter2

makeControl :: (c -> IO (IO a)) -> ControlFor c a
makeControl f = Codec
  { codecIn = Const ()
  , codecOut = IOIO . f
  }

runControl :: c -> ControlFor c a -> IO (IO a)
runControl c cdc = case codecOut cdc c of IOIO x -> x

checkBox :: FL.Rectangle -> Maybe T.Text -> (FL.Ref FL.CheckButton -> IO ()) -> Control Bool
checkBox rect label initfn = makeControl $ \b -> do
  box <- FL.checkButtonNew rect label
  void $ FL.setValue box b
  initfn box
  return $ FL.getValue box
