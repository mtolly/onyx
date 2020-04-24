{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module GUI.FLTK (launchGUI) where

import           Audio                                     (Audio (..),
                                                            buildSource',
                                                            runAudio)
import           Build                                     (targetTitle,
                                                            toValidFileName)
import           Codec.Picture                             (readImage,
                                                            savePngImage,
                                                            writePng)
import           CommandLine                               (blackVenue,
                                                            copyDirRecursive,
                                                            runDolphin)
import           Config
import           Control.Concurrent                        (MVar, ThreadId,
                                                            forkIO, killThread,
                                                            modifyMVar,
                                                            modifyMVar_,
                                                            newChan, newMVar,
                                                            putMVar, readChan,
                                                            readMVar, takeMVar,
                                                            threadDelay,
                                                            writeChan)
import           Control.Concurrent.Async                  (async,
                                                            waitAnyCancel)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan              (newTChanIO,
                                                            tryReadTChan,
                                                            writeTChan)
import qualified Control.Exception                         as Exc
import           Control.Monad.Catch                       (catchIOError)
import           Control.Monad.Extra                       (forM, forM_, guard,
                                                            join, unless, void,
                                                            when, (>=>))
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe                 (MaybeT (..))
import           Control.Monad.Trans.Reader                (ReaderT, ask, local,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT, allocate,
                                                            register, release,
                                                            resourceForkIO,
                                                            runResourceT)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer                (WriterT,
                                                            execWriterT, tell)
import qualified Data.Aeson                                as A
import           Data.Binary.Put                           (runPut)
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import           Data.Char                                 (isSpace, toLower,
                                                            toUpper)
import qualified Data.Connection                           as Conn
import           Data.Default.Class                        (Default, def)
import           Data.Fixed                                (Milli)
import           Data.Foldable                             (toList)
import qualified Data.HashMap.Strict                       as HM
import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)
import           Data.List.Extra                           (findIndex)
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, isJust,
                                                            listToMaybe,
                                                            mapMaybe)
import           Data.Monoid                               (Endo (..))
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time                                 (getCurrentTime)
import           Data.Time.Clock.System                    (SystemTime (..),
                                                            getSystemTime)
import           Data.Version                              (showVersion)
import           DryVox
import           Foreign                                   (Ptr)
import           Foreign.C                                 (CString)
import           Genre                                     (FullGenre (..),
                                                            interpretGenre)
import qualified Graphics.UI.FLTK.LowLevel.Base.GlWindow   as FLGL
import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
                                                            Position (..),
                                                            Rectangle (..),
                                                            Size (..),
                                                            Width (..), X (..),
                                                            Y (..))
import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
import           Graphics.UI.FLTK.LowLevel.GlWindow        ()
import           Graphics.UI.FLTK.LowLevel.X               (openCallback)
import           Image                                     (readRBImageMaybe)
import           JSONData                                  (toJSON,
                                                            yamlEncodeFile)
import           MoggDecrypt                               (oggToMogg)
import           Network.HTTP.Req                          ((/:))
import qualified Network.HTTP.Req                          as Req
import qualified Network.Socket                            as Socket
import           Numeric                                   (readHex)
import           OpenProject
import           OSFiles                                   (commonDir,
                                                            osOpenFile,
                                                            osShowFolder)
import           Paths_onyxite_customs_tool                (version)
import           ProKeysRanges                             (closeShiftsFile)
import           Reaper.Build                              (makeReaper)
import           Reductions                                (simpleReduce)
import           Resources                                 (getResourcesPath)
import           RhythmGame.Audio                          (AudioHandle (..),
                                                            projectAudio,
                                                            withAL)
import qualified RhythmGame.Graphics                       as RGGraphics
import           RhythmGame.Track
import           RockBand.Codec                            (mapTrack)
import           RockBand.Codec.File                       (FlexPartName (..))
import qualified RockBand.Codec.File                       as RBFile
import           RockBand.Common                           (RB3Instrument (..))
import qualified RockBand.Common                           as RB
import           RockBand.Milo                             (autoLipsync,
                                                            beatlesLipsync,
                                                            englishVowels,
                                                            germanVowels,
                                                            gh2Lipsync,
                                                            packMilo,
                                                            putLipsync,
                                                            putVocFile,
                                                            spanishVowels,
                                                            unpackMilo)
import           RockBand.Score
import           RockBand.SongCache                        (fixSongCache)
import           RockBand3                                 (BasicTiming (..))
import qualified Sound.File.Sndfile                        as Snd
import qualified Sound.MIDI.Util                           as U
import qualified System.Directory                          as Dir
import           System.FilePath                           (dropExtension,
                                                            takeDirectory,
                                                            takeExtension,
                                                            takeFileName,
                                                            (-<.>), (<.>),
                                                            (</>))
import qualified System.FSNotify                           as FS
import           System.Info                               (os)
import qualified System.IO.Streams                         as Streams
import qualified System.IO.Streams.TCP                     as TCP
import           Text.Read                                 (readMaybe)

#ifdef WINDOWS
import           Foreign                                   (intPtrToPtr, peek)
import           Graphics.Win32                            (loadIcon)
import           System.Win32                              (HINSTANCE)
#endif

data Event
  = EventIO (IO ())
  | EventOnyx (Onyx ())
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
    runReaderT (logToSink sink act) chan >>= logErrors_ sink

embedOnyx :: (Event -> IO ()) -> Onyx a -> IO (Maybe a)
embedOnyx sink act = runResourceT $
  runReaderT (logToSink sink act) sink >>= logErrors sink

logErrors :: (MonadIO m) => (Event -> IO ()) -> Either Messages a -> m (Maybe a)
logErrors sink = \case
  Right x              -> return $ Just x
  Left (Messages msgs) -> do
    liftIO $ forM_ msgs $ sink . EventFail
    return Nothing

logErrors_ :: (MonadIO m) => (Event -> IO ()) -> Either Messages () -> m ()
logErrors_ sink = void . logErrors sink

safeOnyx :: Onyx () -> Onyx ()
safeOnyx f = getEventSink >>= \sink -> errorToEither f >>= logErrors_ sink

resizeWindow :: FL.Ref FL.Window -> Size -> IO ()
resizeWindow window size = do
  x <- FL.getX window
  y <- FL.getY window
  FL.resize window $ Rectangle (Position x y) size

promptLoad :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> Bool -> IO ()
promptLoad sink makeMenuBar hasAudio = do
  picker <- FL.nativeFileChooserNew $ Just FL.BrowseMultiFile
  FL.setTitle picker "Load song"
  FL.showWidget picker >>= \case
    FL.NativeFileChooserPicked -> do
      n <- FL.getCount picker
      fs <- forM [0 .. n - 1] $ FL.getFilenameAt picker . FL.AtIndex
      sink $ EventOnyx $ startLoad makeMenuBar hasAudio $
        map T.unpack $ catMaybes fs
    _ -> return ()

startLoad :: (Width -> Bool -> IO Int) -> Bool -> [FilePath] -> Onyx ()
startLoad makeMenuBar hasAudio fs = do
  sink <- getEventSink
  void $ forkOnyx $ do
    results <- mapM (errorToEither . findAllSongs) fs
    liftIO
      $ mapM_ (sink . EventFail)
      $ concat [ msgs | Left (Messages msgs) <- results ]
    case concat [ imps | Right imps <- results ] of
      []    -> fatal "No songs found!"
      [imp] -> continueImport makeMenuBar hasAudio imp
      imps  -> stackIO $ sink $ EventIO $ multipleSongsWindow sink makeMenuBar hasAudio imps

continueImport
  :: (Width -> Bool -> IO Int)
  -> Bool
  -> Importable (QueueLog (ReaderT (Event -> IO ()) (ResourceT IO)))
  -> Onyx ()
continueImport makeMenuBar hasAudio imp = do
  sink <- getEventSink
  proj <- impProject imp
  void $ shakeBuild1 proj [] "gen/cover.png"
  maybeAudio <- if hasAudio then projectAudio proj else return Nothing
  stackIO $ sink $ EventIO $ launchWindow sink makeMenuBar proj maybeAudio

multipleSongsWindow
  :: (Event -> IO ())
  -> (Width -> Bool -> IO Int)
  -> Bool
  -> [Importable (QueueLog (ReaderT (Event -> IO ()) (ResourceT IO)))]
  -> IO ()
multipleSongsWindow sink makeMenuBar hasAudio imps = do
  let windowWidth = Width 500
      windowHeight = Height 400
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew
    windowSize
    Nothing
    (Just "Select Songs")
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
      (songsArea, trimClock 10 0 0 0 -> buttonsArea) =
        chopBottom 40 $ trimClock 10 10 10 10 windowRect
      [areaSelect, trimClock 0 0 0 10 -> areaOpen] = splitHorizN 2 buttonsArea
  tree <- FL.treeNew songsArea Nothing
  FL.end tree
  FL.rootLabel tree "Songs"
  FL.setSelectmode tree FLE.TreeSelectNone
  FL.setShowcollapse tree False
  Just root <- FL.root tree
  checks <- forM imps $ \imp -> do
    let entry = T.concat
          [ fromMaybe "Untitled" $ impTitle imp
          , maybe "" (\art -> " (" <> art <> ")") $ impArtist imp
          ]
        dummyRect = Rectangle (Position (X 0) (Y 0)) (Size (Width 400) (Height 25))
    Just itemSong <- FL.addAt tree "" root
    check <- FL.checkButtonNew dummyRect $ Just entry
    void $ FL.setValue check False
    FL.setWidget itemSong $ Just check
    return check
  selectButton <- FL.buttonNew areaSelect $ Just "Select All/None"
  openButton <- FL.buttonNew areaOpen $ Just "Open Songs"
  let updateButtons = do
        bools <- mapM FL.getValue checks
        let newValue = not $ and bools
            checked = length $ filter id bools
        FL.setLabel selectButton $ if newValue
          then "Select all"
          else "Select none"
        FL.setLabel openButton $ case checked of
          0 -> "Close"
          1 -> "Open 1 song"
          _ -> T.pack $ "Open " <> show checked <> " songs"
        color <- if checked == 0 then defaultColor else taskColor
        FL.setColor openButton color
  updateButtons
  FL.setCallback selectButton $ \_ -> do
    bools <- mapM FL.getValue checks
    let newValue = not $ and bools
    mapM_ (\check -> FL.setValue check newValue) checks
    updateButtons
  FL.setCallback openButton $ \_ -> do
    bools <- mapM FL.getValue checks
    forM_ (zip bools imps) $ \(b, imp) -> when b $ do
      sink $ EventOnyx $ void $ forkOnyx $ do
        continueImport makeMenuBar hasAudio imp
    FL.hide window
  forM_ checks $ \check -> FL.setCallback check $ \_ -> updateButtons
  FL.end window
  FL.setResizable window $ Just tree
  FL.sizeRange window windowSize
  FL.showWidget window

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

splitHorizN :: Int -> Rectangle -> [Rectangle]
splitHorizN 1 rect = [rect]
splitHorizN n rect@(Rectangle _ (Size (Width w) _)) = let
  (rectFirst, rectRest) = chopLeft (quot w n) rect
  in rectFirst : splitHorizN (n - 1) rectRest

splitVertN :: Int -> Rectangle -> [Rectangle]
splitVertN 1 rect = [rect]
splitVertN n rect@(Rectangle _ (Size _ (Height h))) = let
  (rectFirst, rectRest) = chopTop (quot h n) rect
  in rectFirst : splitVertN (n - 1) rectRest

trimClock :: Int -> Int -> Int -> Int -> Rectangle -> Rectangle
trimClock t r b l rect = let
  (_, rect1) = chopTop    t rect
  (rect2, _) = chopRight  r rect1
  (rect3, _) = chopBottom b rect2
  (_, rect4) = chopLeft   l rect3
  in rect4

padded
  :: (MonadIO m)
  => Int -> Int -> Int -> Int
  -> Size
  -> (Rectangle -> m a)
  -> m a
padded t r b l size@(Size (Width w) (Height h)) fn = do
  group <- liftIO $ FL.groupNew
    (Rectangle
      (Position (X 0) (Y 0))
      (Size (Width (w + l + r)) (Height (h + t + b)))
    )
    Nothing
  let rect = Rectangle (Position (X l) (Y t)) size
  box <- liftIO $ FL.boxNew rect Nothing
  liftIO $ FL.setResizable group $ Just box
  x <- fn rect
  liftIO $ FL.end group
  return x

-- | Ported from Erco's Fl_Center class
centerFixed :: Rectangle -> IO a -> IO a
centerFixed rect makeChildren = do
  let centerX :: FL.Ref FL.Group -> FL.Ref FL.WidgetBase -> IO X
      centerX this wid = do
        X x <- FL.getX this
        Width w <- FL.getW this
        Width widW <- FL.getW wid
        return $ X $ x + quot w 2 - quot widW 2
      centerY :: FL.Ref FL.Group -> FL.Ref FL.WidgetBase -> IO Y
      centerY this wid = do
        Y y <- FL.getY this
        Height h <- FL.getH this
        Height widH <- FL.getH wid
        return $ Y $ y + quot h 2 - quot widH 2
      myResize :: FL.Ref FL.Group -> Rectangle -> IO ()
      myResize this rect' = do
        FL.resizeWidgetBase (FL.safeCast this) rect'
        children <- FL.children this
        forM_ [0 .. children - 1] $ \i -> do
          mcw <- FL.getChild this $ FL.AtIndex i
          forM_ mcw $ \cw -> do
            newX <- centerX this cw
            newY <- centerY this cw
            newW <- FL.getW cw
            newH <- FL.getH cw
            FL.resize cw $ Rectangle
              (Position newX newY)
              (Size newW newH)
  group <- FL.groupCustom rect Nothing Nothing FL.defaultCustomWidgetFuncs
    { FL.resizeCustom = Just myResize
    }
  x <- makeChildren
  FL.end group
  myResize group rect
  return x

makeTab :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Group -> IO a) -> IO a
makeTab rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
  group <- FL.groupNew innerRect (Just name)
  res <- fn innerRect group
  FL.end group
  return res

startAnimation :: IO () -> IO (IO ())
startAnimation redraw = do
  stopper <- newIORef False
  let loop = readIORef stopper >>= \case
        True -> return ()
        False -> do
          redraw
          threadDelay 16666
          loop
  _ <- forkIO loop
  return $ writeIORef stopper True

data Playing = Playing
  { playStarted       :: SystemTime
  , playSpeed         :: Double -- 1 = 100%
  , playStopAnimation :: IO ()
  , playAudioHandle   :: AudioHandle
  }

data SongState = SongState
  { songTime    :: Double
  , songPlaying :: Maybe Playing
  }

currentSongTime :: SystemTime -> SongState -> Double
currentSongTime stime ss = case songPlaying ss of
  Nothing -> songTime ss
  Just p  -> songTime ss + let
    -- this might lose precision?
    timeToDouble t = realToFrac (systemSeconds t) + realToFrac (systemNanoseconds t) / 1000000000
    in (timeToDouble stime - timeToDouble (playStarted p)) * playSpeed p

type BuildYamlControl a = WriterT (IO (Endo a)) IO

launchWindow :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> Project -> Maybe (Double -> Maybe Double -> Float -> IO AudioHandle) -> IO ()
launchWindow sink makeMenuBar proj maybeAudio = mdo
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew
    windowSize
    Nothing
    (Just $ fromMaybe "Song" $ _title $ _metadata $ projectSongYaml proj)
    -- TODO if title has slash like "Pupa / Cocoon" this makes weird new menus
    -- on Mac that can crash (bc you can reopen a closed song window and try to
    -- delete the temp folder a second time). to fix properly I think we need
    -- to set window_menu_style on the SysMenuBar (not in fltkhs yet)
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  behindTabsColor >>= FL.setColor window
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRange window $ Size (Width 800) (Height 500)
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  (modifyMeta, metaTab) <- makeTab windowRect "Metadata" $ \rect tab -> do
    homeTabColor >>= setTabColor tab
    let (rectSplit, rectRest) = chopTop 200 rect
        (rectLeft, rectRight) = chopRight 200 rectSplit
        (rectResize, _) = chopRight 200 rectRest
    yamlModifier <- fmap (fmap appEndo) $ execWriterT $ do
      {- TODO:
        song key -- two dropdowns (key + tonality)
        anim tempo -- dropdown with disableable number box (like rank)?
        rating -- dropdown
        preview start/end time -- ???
        band difficulty (maybe should go on instruments page?) -- dropdown with disableable number box
      -}
      pack <- liftIO $ FL.packNew rectLeft Nothing
      let fullWidth = padded 5 10 5 100 (Size (Width 500) (Height 30))
          simpleText lbl = simpleText' lbl FL.FlNormalInput
          simpleText' lbl intype getter rect' = liftIO $ do
            input <- FL.inputNew
              rect'
              (Just lbl)
              (Just intype) -- TODO if Nothing here, labels don't work. should report bug in binding's "inputNew"
            FL.setLabelsize input $ FL.FontSize 13
            FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
            FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
            void $ FL.setValue input $ fromMaybe "" $ getter $ _metadata $ projectSongYaml proj
            return input
          applyToMetadata :: (Maybe T.Text -> Metadata f -> Metadata f) -> FL.Ref FL.Input -> BuildYamlControl (SongYaml f) ()
          applyToMetadata setter input = tell $ do
            val <- FL.getValue input
            let val' = guard (val /= "") >> Just val
            return $ Endo $ \yaml -> yaml { _metadata = setter val' $ _metadata yaml }
          applyIntToMetadata :: (Maybe Int -> Metadata f -> Metadata f) -> FL.Ref FL.Input -> BuildYamlControl (SongYaml f) ()
          applyIntToMetadata setter input = tell $ do
            val <- FL.getValue input
            let val' = readMaybe $ T.unpack val
            return $ Endo $ \yaml -> yaml { _metadata = setter val' $ _metadata yaml }
          simpleTextGet lbl getter setter rect' = do
            input <- simpleText lbl getter rect'
            applyToMetadata setter input
          simpleTextGetInt lbl getter setter rect' = do
            input <- simpleText' lbl FL.FlIntInput (fmap (T.pack . show) . getter) rect'
            applyIntToMetadata setter input
          simpleCheck lbl getter setter rect' = do
            input <- liftIO $ FL.checkButtonNew rect' $ Just lbl
            liftIO $ void $ FL.setValue input $ getter $ _metadata $ projectSongYaml proj
            liftIO $ FL.setLabelsize input $ FL.FontSize 13
            tell $ do
              b <- FL.getValue input
              return $ Endo $ \yaml -> yaml { _metadata = setter b $ _metadata yaml }
      void $ liftIO $ FL.boxNew (Rectangle (Position (X 0) (Y 0)) (Size (Width 800) (Height 5))) Nothing
      fullWidth $ simpleTextGet "Title"  _title  $ \mstr meta -> meta { _title  = mstr }
      fullWidth $ \rect' -> do
        let (artistArea, trimClock 0 0 0 50 -> yearArea) = chopRight 120 rect'
        simpleTextGet "Artist" _artist (\mstr meta -> meta { _artist = mstr }) artistArea
        simpleTextGetInt "Year" _year (\mint meta -> meta { _year = mint }) yearArea
      fullWidth $ \rect' -> do
        let (albumArea, trimClock 0 0 0 30 -> trackNumArea) = chopRight 120 rect'
        simpleTextGet "Album" _album (\mstr meta -> meta { _album = mstr }) albumArea
        simpleTextGetInt "#" _trackNumber (\mint meta -> meta { _trackNumber = mint }) trackNumArea
      fullWidth $ simpleTextGet "Author" _author $ \mstr meta -> meta { _author = mstr }
      (genreInput, subgenreInput) <- fullWidth $ \rect' -> let
        [trimClock 0 50 0 0 -> genreArea, trimClock 0 0 0 50 -> subgenreArea] = splitHorizN 2 rect'
        in do
          genreInput    <- simpleText "Genre"    _genre    genreArea
          subgenreInput <- simpleText "Subgenre" _subgenre subgenreArea
          applyToMetadata (\mstr meta -> meta { _genre    = mstr }) genreInput
          applyToMetadata (\mstr meta -> meta { _subgenre = mstr }) subgenreInput
          return (genreInput, subgenreInput)
      liftIO $ FL.end pack
      packRight <- liftIO $ FL.packNew rectRight Nothing
      padded 10 10 0 0 (Size (Width 190) (Height 190)) $ \rect' -> mdo
        cover <- liftIO $ FL.buttonCustom rect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
          { FL.handleCustom = Just
            $ dragAndDrop (mapM_ swapImage . listToMaybe)
            . FL.handleButtonBase
            . FL.safeCast
          }
        let setPNG defaultImage f = do
              Right png <- FL.pngImageNew $ T.pack f
              FL.scale png (Size (Width 180) (Height 180))
                (Just False) -- not proportional (stretch to square, like onyx does)
                (Just True) -- can expand
              FL.setImage cover $ Just png
              return (png, guard (not defaultImage) >> Just f)
        currentImage <- liftIO
          $ setPNG True (takeDirectory (projectLocation proj) </> "gen/cover.png")
          >>= newMVar
        let swapImage f = sink $ EventOnyx $ void $ forkOnyx $ void $ errorToWarning $ do
              inside ("Loading image: " <> f) $ do
                let newPath = takeDirectory (projectLocation proj) </> "new-cover.png"
                if elem (takeExtension f) [".png_xbox", ".png_wii"]
                  then stackIO (BL.readFile f) >>= maybe
                    (fatal "Couldn't read RB image file")
                    (stackIO . writePng newPath)
                    . readRBImageMaybe
                  else stackIO (readImage f) >>= either fatal (stackIO . savePngImage newPath)
                stackIO $ sink $ EventIO $ do
                  oldImage <- modifyMVar currentImage $ \(oldImage, _) -> do
                    newPair <- setPNG False newPath
                    return (newPair, oldImage)
                  FL.redraw cover
                  FL.destroy oldImage
        liftIO $ FL.setCallback cover $ \_ -> do
          picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
          FL.setTitle picker "Select album art"
          FL.setFilter picker "*.{png,jpg,jpeg,png_xbox,png_wii}"
          FL.showWidget picker >>= \case
            FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
              Nothing              -> return ()
              Just (T.unpack -> f) -> swapImage f
            _ -> return ()
        tell $ do
          (_, mpath) <- readMVar currentImage
          return $ Endo $ case mpath of
            Nothing   -> id
            Just path -> \yaml -> yaml
              { _metadata = (_metadata yaml)
                { _fileAlbumArt = Just path
                }
              }
      liftIO $ FL.end packRight
      packBottom <- liftIO $ FL.packNew rectRest Nothing
      padded 10 10 5 10 (Size (Width 500) (Height 30)) $ \rect' -> liftIO $ do
        calcGenreBox <- FL.boxNew rect' Nothing
        let calcGenre = do
              g <- FL.getValue genreInput
              s <- FL.getValue subgenreInput
              let FullGenre{..} = interpretGenre
                    (guard (g /= "") >> Just g)
                    (guard (s /= "") >> Just s)
              FL.setLabel calcGenreBox $ T.concat
                -- TODO show in-game names, not dta names
                [ "RB3: ", rbn2Genre, " (", rbn2Subgenre, "), "
                , "RB2: ", rbn1Genre, " (", rbn1Subgenre, ")"
                ]
        calcGenre
        FL.setCallback genreInput    $ \_ -> calcGenre
        FL.setCallback subgenreInput $ \_ -> calcGenre
        FL.setWhen genreInput    [FLE.WhenChanged]
        FL.setWhen subgenreInput [FLE.WhenChanged]
      padded 5 10 5 10 (Size (Width 500) (Height 30)) $ \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
        simpleCheck "Convert"     _convert    (\b meta -> meta { _convert    = b }) r1
        simpleCheck "Rhythm Keys" _rhythmKeys (\b meta -> meta { _rhythmKeys = b }) r2
        simpleCheck "Rhythm Bass" _rhythmBass (\b meta -> meta { _rhythmBass = b }) r3
        simpleCheck "Auto EMH"    _catEMH     (\b meta -> meta { _catEMH     = b }) r4
        simpleCheck "Expert Only" _expertOnly (\b meta -> meta { _expertOnly = b }) r5
        simpleCheck "Cover"       _cover      (\b meta -> meta { _cover      = b }) r6
      padded 5 10 5 10 (Size (Width 500) (Height 30)) $ \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
            editLangs f meta = meta
              { _languages = f $ _languages meta
              }
            lang l = simpleCheck l
              (elem l . _languages)
              (\b -> editLangs $ if b then (l :) else id)
        lang "English" r1
        lang "French" r2
        lang "Italian" r3
        lang "Spanish" r4
        lang "German" r5
        lang "Japanese" r6
        -- language-clear function at the bottom; this makes it happen first
        -- (note how function composition happens via the Writer monad)
        tell $ return $ Endo $ \yaml -> yaml
          { _metadata = editLangs (const []) $ _metadata yaml
          }
      liftIO $ FL.end packBottom
      resizeBox <- liftIO $ FL.boxNew rectResize Nothing
      liftIO $ FL.setResizable tab $ Just resizeBox
    return (yamlModifier, tab)
  (modifyInsts, instTab) <- makeTab windowRect "Instruments" $ \rect tab -> do
    homeTabColor >>= setTabColor tab
    let instRect = trimClock 10 10 10 10 rect
    tree <- FL.treeNew instRect Nothing
    FL.end tree
    FL.rootLabel tree "Instruments"
    FL.setSelectmode tree FLE.TreeSelectNone
    FL.setShowcollapse tree False
    Just root <- FL.root tree
    let dummyRect = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 100))
        makeDifficulty :: FL.Ref FL.TreeItem -> Difficulty -> IO (IO Difficulty)
        makeDifficulty itemParent diff = do
          Just itemGroup <- FL.addAt tree "" itemParent
          group <- FL.groupNew dummyRect Nothing
          let (choiceArea, textArea) = chopLeft 100 dummyRect
          choice <- FL.choiceNew choiceArea Nothing
          let tierCount = 7
          forM_ [1 .. tierCount] $ \i -> FL.addName choice $ T.pack $ "Tier " <> show (i :: Int)
          FL.addName choice "Rank"
          input <- FL.inputNew textArea Nothing $ Just FL.FlNormalInput
          let setChoice = void . FL.setValue choice . FL.MenuItemByIndex . FL.AtIndex
              controlRank = do
                FL.AtIndex i <- FL.getValue choice
                if i == tierCount
                  then FL.activate   input
                  else FL.deactivate input
          case diff of
            Tier i -> setChoice $ (max 1 $ min tierCount $ fromIntegral i) - 1
            Rank r -> do
              setChoice tierCount
              void $ FL.setValue input $ T.pack $ show r
          controlRank
          FL.setCallback choice $ \_ -> controlRank
          FL.end group
          FL.setWidget itemGroup $ Just group
          return $ do
            FL.AtIndex i <- FL.getValue choice
            if i == tierCount
              then Rank . fromMaybe 0 . readMaybe . T.unpack <$> FL.getValue input
              else return $ Tier $ fromIntegral i + 1
    getBandDiff <- makeDifficulty root $ _difficulty $ _metadata $ projectSongYaml proj
    getNewParts <- fmap catMaybes $ forM (HM.toList $ getParts $ _parts $ projectSongYaml proj) $ \(fpart, part) ->
      if part == def
        then return Nothing
        else do
          Just itemInst <- FL.addAt tree (T.toTitle $ RBFile.getPartName fpart) root
          let addType lbl extra = do
                Just itemCheck <- FL.addAt tree "" itemInst
                check <- FL.checkButtonNew dummyRect $ Just lbl
                void $ FL.setValue check True
                FL.setWidget itemCheck $ Just check
                fn <- extra itemCheck
                return $ \curPart -> do
                  isChecked <- FL.getValue check
                  fn isChecked curPart
              makeChoice :: (Enum a, Bounded a) => FL.Ref FL.TreeItem -> a -> (a -> T.Text) -> IO (IO a)
              makeChoice itemParent cur getLabel = do
                Just itemChoice <- FL.addAt tree "" itemParent
                choice <- FL.choiceNew dummyRect Nothing
                forM_ [minBound .. maxBound] $ FL.addName choice . getLabel
                void $ FL.setValue choice $ FL.MenuItemByIndex $ FL.AtIndex $ fromEnum cur
                FL.setWidget itemChoice $ Just choice
                return $ (\(FL.AtIndex i) -> toEnum i) <$> FL.getValue choice
          mbGRYBO <- forM (partGRYBO part) $ \pg -> addType "5-Fret" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ gryboDifficulty pg
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { partGRYBO = guard isChecked >> Just pg { gryboDifficulty = diff } }
          mbGHL <- forM (partGHL part) $ \pg -> addType "6-Fret" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ ghlDifficulty pg
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { partGHL = guard isChecked >> Just pg { ghlDifficulty = diff } }
          mbProKeys <- forM (partProKeys part) $ \pk -> addType "Pro Keys" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ pkDifficulty pk
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { partProKeys = guard isChecked >> Just pk { pkDifficulty = diff } }
          mbProGuitar <- forM (partProGuitar part) $ \pg -> addType "Pro Guitar" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ pgDifficulty pg
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { partProGuitar = guard isChecked >> Just pg { pgDifficulty = diff } }
          mbDrums <- forM (partDrums part) $ \pd -> addType "Drums" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ drumsDifficulty pd
            getMode <- makeChoice itemCheck (drumsMode pd) $ \case
              Drums4    -> "4-Lane Drums"
              Drums5    -> "5-Lane Drums"
              DrumsPro  -> "Pro Drums"
              DrumsReal -> "Phase Shift Real Drums"
            getKicks <- makeChoice itemCheck (drumsKicks pd) $ \case
              Kicks1x   -> "1x Bass Pedal"
              Kicks2x   -> "2x Bass Pedal"
              KicksBoth -> "1x+2x Bass Pedal (PS X+ or C3 format)"
            getKit <- makeChoice itemCheck (drumsKit pd) $ \case
              HardRockKit   -> "Hard Rock Kit"
              ArenaKit      -> "Arena Kit"
              VintageKit    -> "Vintage Kit"
              TrashyKit     -> "Trashy Kit"
              ElectronicKit -> "Electronic Kit"
            return $ \isChecked curPart -> do
              diff <- getDiff
              mode <- getMode
              kicks <- getKicks
              kit <- getKit
              return curPart
                { partDrums = guard isChecked >> Just pd
                  { drumsDifficulty = diff
                  , drumsMode = mode
                  , drumsKicks = kicks
                  , drumsKit = kit
                  }
                }
          mbVocal <- forM (partVocal part) $ \pv -> addType "Vocals" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck $ vocalDifficulty pv
            getCount <- makeChoice itemCheck (vocalCount pv) $ \case
              Vocal1 -> "Solo"
              Vocal2 -> "Harmonies (2)"
              Vocal3 -> "Harmonies (3)"
            return $ \isChecked curPart -> do
              diff <- getDiff
              count <- getCount
              return curPart
                { partVocal = guard isChecked >> Just pv
                  { vocalDifficulty = diff
                  , vocalCount = count
                  }
                }
          return $ Just $ do
            let modifiers = catMaybes [mbGRYBO, mbGHL, mbProKeys, mbProGuitar, mbDrums, mbVocal]
            newPart <- foldl (>>=) (return part) modifiers
            return (fpart, newPart)
    let editParts = do
          newParts <- sequence getNewParts
          bandDiff <- getBandDiff
          return $ \yaml -> yaml
            { _parts = Parts $ HM.fromList newParts
            , _metadata = (_metadata yaml) { _difficulty = bandDiff }
            }
    FL.setResizable tab $ Just tree
    return (editParts, tab)
  let fullProjModify :: Project -> IO Project
      fullProjModify p = do
        modifiers <- sequence [modifyMeta, modifyInsts]
        let newYaml = foldr ($) (projectSongYaml p) modifiers
        saveProject p newYaml
  (_previewTab, cleanupGL) <- makeTab windowRect "Preview" $ \rect tab -> mdo
    homeTabColor >>= setTabColor tab
    let (topControlsArea1, glArea) = chopTop 40 rect
        (trimClock 0 5 0 5 -> volPlayButtonArea, topControlsArea2) = chopLeft 160 topControlsArea1
        (trimClock 8 5 8 0 -> volSliderArea, trimClock 5 0 5 5 -> playButtonArea) = chopRight 50 volPlayButtonArea
        (trimClock 8 5 8 5 -> scrubberArea, topControlsArea3) = chopRight 255 topControlsArea2
        (trimClock 5 5 5 5 -> timestampArea, trimClock 5 5 5 5 -> speedArea) = chopLeft 105 topControlsArea3
    topControls <- FL.groupNew topControlsArea1 Nothing
    volSlider <- FL.horNiceSliderNew volSliderArea Nothing
    homeTabColor >>= FL.setColor volSlider
    FL.setMinimum volSlider 0
    FL.setMaximum volSlider 1
    void $ FL.setValue volSlider 1
    FL.setCallback volSlider $ \_ -> do
      ss <- readIORef varTime
      case songPlaying ss of
        Nothing -> return ()
        Just p -> do
          v <- FL.getValue volSlider
          audioSetGain (playAudioHandle p) (realToFrac v)
    playButton <- FL.buttonCustom playButtonArea (Just "@>") Nothing $ Just FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just $ \ref e -> case e of
        -- support play/pause with space even if button is not focused
        FLE.Shortcut -> FLTK.eventKey >>= \case
          FL.NormalKeyType ' ' -> FL.activeR ref >>= \case
            True  -> togglePlay >> return (Right ())
            False -> FL.handleButtonBase (FL.safeCast ref) e
          _ -> FL.handleButtonBase (FL.safeCast ref) e
        _ -> FL.handleButtonBase (FL.safeCast ref) e
      }
    FL.deactivate playButton
    scrubber <- FL.horNiceSliderNew scrubberArea Nothing
    homeTabColor >>= FL.setColor scrubber
    FL.setMinimum scrubber 0
    FL.setMaximum scrubber 100
    FL.setStep scrubber 1
    void $ FL.setValue scrubber 0
    FL.deactivate scrubber
    timestampLabel <- FL.boxNew timestampArea Nothing
    let updateTimestamp = do
          tcur <- FL.getValue scrubber
          tmax <- FL.getMaximum scrubber
          let formatTime n = T.pack $ case quotRem (floor n :: Int) 60 of
                (m, s) -> show m <> ":" <> if s < 10
                  then "0" <> show s
                  else show s
          FL.setLabel timestampLabel $ T.concat
            [ formatTime $ min tcur tmax
            , " / "
            , formatTime tmax
            ]
    (getSpeed, counter) <- speedPercent' False speedArea
    FL.end topControls
    FL.setResizable topControls $ Just scrubber
    varSong <- newIORef Nothing
    sink $ EventOnyx $ void $ forkOnyx $ do
      song <- loadTracks $ takeDirectory (projectLocation proj) </> "notes.mid"
      stackIO $ sink $ EventIO $ do
        writeIORef varSong $ Just song
        FL.setMaximum scrubber $ fromInteger $ ceiling $ U.applyTempoMap
          (previewTempo song)
          (timingEnd $ previewTiming song)
        updateTimestamp
        FL.activate scrubber
        FL.activate playButton
    let initState = SongState 0 Nothing
    varState <- newMVar initState
    varTime <- newIORef initState
    (groupGL, redrawGL) <- previewGroup
      sink
      glArea
      (maybe [] previewTracks <$> readIORef varSong)
      (currentSongTime <$> getSystemTime <*> readIORef varTime)
      getSpeed
    FL.setResizable tab $ Just groupGL
    let takeState = takeMVar varState
        putState ss = do
          putMVar varState ss
          writeIORef varTime ss
        stopPlaying :: Double -> Playing -> IO Double
        stopPlaying t ps = do
          stime <- getSystemTime
          playStopAnimation ps
          audioStop $ playAudioHandle ps
          return $ currentSongTime stime $ SongState t $ Just ps
        startPlaying :: Double -> IO SongState
        startPlaying t = do
          speed <- getSpeed
          handle <- case maybeAudio of
            Just f  -> do
              gain <- FL.getValue volSlider
              f t (guard (speed /= 1) >> Just speed) (realToFrac gain)
            Nothing -> return AudioHandle
              { audioStop = return ()
              , audioSetGain = \_ -> return ()
              }
          curTime <- getSystemTime
          stopAnim <- startAnimation $ do
            t' <- currentSongTime <$> getSystemTime <*> readIORef varTime
            sink $ EventIO $ do
              void $ FL.setValue scrubber t'
              updateTimestamp
              redrawGL
          let ps = Playing
                { playStarted = curTime
                , playSpeed = speed
                , playStopAnimation = stopAnim
                , playAudioHandle = handle
                }
          return SongState { songTime = t, songPlaying = Just ps }
    FL.setCallback scrubber $ \_ -> do
      secs <- FL.getValue scrubber
      ss <- takeState
      ss' <- case songPlaying ss of
        Nothing -> do
          sink $ EventIO updateTimestamp
          return ss { songTime = secs }
        Just ps -> do
          _ <- stopPlaying (songTime ss) ps
          startPlaying secs
      putState ss'
      redrawGL
    let togglePlay = do
          ss <- takeState
          ss' <- case songPlaying ss of
            Nothing -> do
              sink $ EventIO $ FL.setLabel playButton "@square"
              startPlaying $ songTime ss
            Just ps -> do
              sink $ EventIO $ FL.setLabel playButton "@>"
              t <- stopPlaying (songTime ss) ps
              return $ SongState t Nothing
          putState ss'
    FL.setCallback playButton $ \_ -> togglePlay
    FL.setCallback counter $ \_ -> do
      ss <- takeState
      ss' <- case songPlaying ss of
        Nothing -> redrawGL >> return ss
        Just ps -> stopPlaying (songTime ss) ps >>= startPlaying
      putState ss'
    let cleanup = do
          ss <- takeState
          case songPlaying ss of
            Nothing -> return ()
            Just ps -> void $ stopPlaying (songTime ss) ps
    return (tab, cleanup)
    {-

    paused:
      user hits play:
        start playing from current position and speed
        change button icon to pause
      user moves scrubber:
        change position shown on track
      user changes speed:
        nothing
    playing:
      user hits pause:
        stop (set position to calculated current position)
        change button icon to play
      user moves scrubber:
        stop (set position to new position)
        start playing from new position
      user changes speed:
        stop (set position to calculated current position)
        start playing at the new speed
      a frame's time passes:
        calculate current position
        redraw the track
        set scrubber position
      window closes:
        stop audio

    -}
  _starsTab <- makeTab windowRect "Stars" $ \rect tab -> do
    homeTabColor >>= setTabColor tab
    pack <- FL.packNew rect Nothing
    padded 5 10 5 10 (Size (Width 800) (Height 50)) $ \rect' -> do
      void $ FL.boxNew rect' $ Just "Rock Band 3 Star Cutoffs"
    FL.end pack
    sink $ EventOnyx $ void $ forkOnyx $ do
      let input = takeDirectory (projectLocation proj) </> "notes.mid"
      mid <- RBFile.loadMIDI input
      let foundTracks = getScoreTracks $ RBFile.s_tracks mid
      stackIO $ sink $ EventIO $ mdo
        FL.begin pack
        getTracks <- padded 5 10 5 10 (Size (Width 800) (Height 50)) $ \rect' -> do
          starSelectors rect' foundTracks updateLabel
            [ ("Guitar", [ScoreGuitar, ScoreProGuitar])
            , ("Bass"  , [ScoreBass  , ScoreProBass  ])
            , ("Keys"  , [ScoreKeys  , ScoreProKeys  ])
            , ("Drums" , [ScoreDrums , ScoreProDrums ])
            , ("Vocal" , [ScoreVocals, ScoreHarmonies])
            ]
        [l1S, l2S, l3S, l4S, l5S, lGS] <- padded 3 10 3 10 (Size (Width 800) (Height 40)) $ \bottomArea -> do
          forM (splitHorizN 6 bottomArea) $ \cell -> do
            cutoffLabel <- FL.boxNew cell Nothing
            FL.setLabelfont cutoffLabel FLE.helveticaBold
            return cutoffLabel
        let updateLabel = do
              stars <- tracksToStars <$> getTracks
              let commafy n = T.pack $ reverse $ go $ reverse $ show n
                  go (x : y : z : rest@(_ : _))
                    = [x, y, z, ','] ++ go rest
                  go xs = xs
              FL.setLabel l1S $ maybe "" (\n -> "1*: " <> commafy n) $ stars1    stars
              FL.setLabel l2S $ maybe "" (\n -> "2*: " <> commafy n) $ stars2    stars
              FL.setLabel l3S $ maybe "" (\n -> "3*: " <> commafy n) $ stars3    stars
              FL.setLabel l4S $ maybe "" (\n -> "4*: " <> commafy n) $ stars4    stars
              FL.setLabel l5S $ maybe "" (\n -> "5*: " <> commafy n) $ stars5    stars
              FL.setLabel lGS $ maybe "" (\n -> "G*: " <> commafy n) $ starsGold stars
        updateLabel
        FL.end pack
        FLTK.redraw
    FL.setResizable tab $ Just pack
    return tab
  rb3Tab <- makeTab windowRect "RB3 (360)" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageRB3 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            RB3CON   _ -> "Building RB3 CON"
            RB3Magma _ -> "Building Magma project"
          task = case create of
            RB3CON fout -> do
              tmp <- buildRB3CON tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB3Magma dout -> do
              tmp <- buildMagmaV2 tgt proj'
              copyDirRecursive tmp dout
              return [dout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  rb2Tab <- makeTab windowRect "RB2 (360)" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageRB2 sink rect tab proj $ \tgt fout -> do
      proj' <- fullProjModify proj
      let name = "Building RB2 CON"
          task = do
            tmp <- buildRB2CON tgt proj'
            stackIO $ Dir.copyFile tmp fout
            return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  psTab <- makeTab windowRect "CH/PS" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPagePS sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            PSDir _ -> "Building CH/PS song folder"
            PSZip _ -> "Building CH/PS zip file"
          task = case create of
            PSDir dout -> do
              tmp <- buildPSDir tgt proj'
              copyDirRecursive tmp dout
              return [dout]
            PSZip fout -> do
              tmp <- buildPSZip tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  utilsTab <- makeTab windowRect "Utilities" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    pack <- FL.packNew rect Nothing
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
      btn <- FL.buttonNew rect' $ Just "Build web preview"
      taskColor >>= FL.setColor btn
      FL.setCallback btn $ \_ -> sink $ EventIO $ do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveDirectory
        FL.setTitle picker "Save web preview folder"
        FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_player"
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
            Nothing -> return ()
            Just f  -> do
              proj' <- fullProjModify proj
              let task = do
                    tmp <- buildPlayer Nothing proj'
                    copyDirRecursive tmp f
                    return [f]
              sink $ EventOnyx $ startTasks [("Build web preview", task)]
          _ -> return ()
      return ()
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
      btn <- FL.buttonNew rect' $ Just "Produce MIDI with automatic reductions"
      taskColor >>= FL.setColor btn
      FL.setCallback btn $ \_ -> sink $ EventIO $ do
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
        FL.setTitle picker "Save MIDI file"
        isDir <- Dir.doesDirectoryExist $ projectSource proj
        FL.setDirectory picker $ T.pack $ if isDir
          then projectSource proj
          else takeDirectory $ projectSource proj
        FL.setFilter picker "*.mid"
        FL.setPresetFile picker "reduced.mid" -- .mid gets chopped off on mac
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
            Nothing -> return ()
            Just f  -> let
              ext = map toLower $ takeExtension f
              f' = if elem ext [".mid", ".midi"]
                then f
                else f <.> "mid"
              in sink $ EventOnyx $ let
                task = do
                  simpleReduce (takeDirectory (projectLocation proj) </> "notes.mid") f'
                  return [f']
                in startTasks [(f', task)]
          _ -> return ()
      return ()
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
      btn <- FL.buttonNew rect' $ Just "Find hanging Pro Keys notes"
      taskColor >>= FL.setColor btn
      FL.setCallback btn $ \_ -> sink $ EventOnyx $ do
        startTasks [("Pro Keys range check", proKeysHanging Nothing proj >> return [])]
    FL.end pack
    FL.setResizable tab $ Just pack
    return tab
  let tabsToDisable = [metaTab, instTab, rb3Tab, rb2Tab, psTab, utilsTab]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" $ \rect tab -> do
    taskColor >>= setTabColor tab
    FL.deactivate tab
    let cbStart = do
          FL.activate tab
          mapM_ FL.deactivate tabsToDisable
          void $ FL.setValue tabs $ Just tab
          updateTabsColor tabs
        cbEnd = do
          mapM_ FL.activate tabsToDisable
    taskOutputPage rect tab sink cbStart cbEnd
  FL.end tabs
  updateTabsColor tabs
  FL.setCallback tabs updateTabsColor
  FL.setResizable tabs $ Just metaTab
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser $ do
    cancelTasks
    mapM_ release $ projectRelease proj
    cleanupGL
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

forceProDrums :: SongYaml f -> SongYaml f
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

saveProject :: Project -> SongYaml FilePath -> IO Project
saveProject proj song = do
  yamlEncodeFile (projectLocation proj) $ toJSON song
  return proj { projectSongYaml = song }

data GBKOption
  = GBKUnchanged
  | CopyGuitarToKeys
  | SwapGuitarKeys
  | SwapBassKeys
  deriving (Eq, Show)

applyGBK :: GBKOption -> SongYaml f -> TargetRB3 f -> TargetRB3 f
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

applyGBK2 :: GBKOption -> SongYaml f -> TargetRB2 -> TargetRB2
applyGBK2 gbk song tgt = case gbk of
  GBKUnchanged -> tgt
  CopyGuitarToKeys -> tgt
  SwapGuitarKeys -> if hasFive FlexKeys
    then tgt { rb2_Guitar = FlexKeys }
    else tgt
  SwapBassKeys -> if hasFive FlexKeys
    then tgt { rb2_Bass = FlexKeys }
    else tgt
  where hasFive flex = isJust $ getPart flex song >>= partGRYBO

data RB3Create
  = RB3CON FilePath
  | RB3Magma FilePath

data PSCreate
  = PSDir FilePath
  | PSZip FilePath

horizRadio' :: Rectangle -> Maybe (IO ()) -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio' _    _  []   = error "horizRadio: empty option list"
horizRadio' rect cb opts = do
  let rects = splitHorizN (length opts) rect
  btns <- forM (zip opts rects) $ \((label, _, b), rectButton) -> do
    btn <- FL.roundButtonNew rectButton $ Just label
    void $ FL.setValue btn b
    return btn
  forM_ btns $ \opt -> FL.setCallback opt $ \_ -> do
    forM_ btns $ \opt' -> FL.setValue opt' $ opt == opt'
    sequence_ cb
  return $ do
    bools <- mapM FL.getValue btns
    return $ fmap (\(_, (_, x, _)) -> x) $ listToMaybe $ filter fst $ zip bools opts

horizRadio :: Rectangle -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio rect = horizRadio' rect Nothing

speedPercent' :: Bool -> Rectangle -> IO (IO Double, FL.Ref FL.Counter)
speedPercent' isConverter rect = do
  speed <- FL.counterNew rect $ if isConverter
    then Just "Speed (%)"
    else Nothing
  FL.setLabelsize speed $ FL.FontSize 13
  FL.setLabeltype speed FLE.NormalLabelType FL.ResolveImageLabelDoNothing
  FL.setAlign speed $ FLE.Alignments [FLE.AlignTypeLeft]
  FL.setStep speed 1
  FL.setLstep speed 5
  FL.setMinimum speed 1
  void $ FL.setValue speed 100
  FL.setTooltip speed $ if isConverter
    then "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
    else "Speed (%)"
  return ((/ 100) <$> FL.getValue speed, speed)

speedPercent :: Bool -> Rectangle -> IO (IO Double)
speedPercent isConverter rect = fst <$> speedPercent' isConverter rect

batchPageRB2
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> ([(TargetRB2, FilePath)], SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageRB2 sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> let
    centerRect = trimClock 0 250 0 250 rect'
    in centerFixed rect' $ speedPercent True centerRect
  getGBK <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("Guitar/Bass", GBKUnchanged, True)
      , ("Keys on Guitar", SwapGuitarKeys, False)
      , ("Keys on Bass", SwapBassKeys, False)
      ]
    return $ fromMaybe GBKUnchanged <$> fn
  getKicks <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("1x Bass Pedal", Kicks1x, False)
      , ("2x Bass Pedal", Kicks2x, False)
      , ("Both", KicksBoth, True)
      ]
    return $ fromMaybe KicksBoth <$> fn
  let getTargetSong usePath template = do
        speed <- getSpeed
        gbk <- getGBK
        kicks <- getKicks
        return $ \proj -> let
          tgt = applyGBK2 gbk yaml def
            { rb2_Common = (rb2_Common def)
              { tgt_Speed = Just speed
              }
            }
          kicksConfigs = case (kicks, maybe Kicks1x drumsKicks $ getPart FlexDrums yaml >>= partDrums) of
            (_        , Kicks1x) -> [(False, ""   )]
            (Kicks1x  , _      ) -> [(False, "_1x")]
            (Kicks2x  , _      ) -> [(True , "_2x")]
            (KicksBoth, _      ) -> [(False, "_1x"), (True, "_2x")]
          fout kicksLabel = T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ RB2 tgt
            , let
              modifiers = T.concat
                [ T.pack $ case tgt_Speed $ rb2_Common tgt of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                , kicksLabel
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          yaml
            = forceProDrums
            $ projectSongYaml proj
          in
            ( [ (tgt { rb2_2xBassPedal = is2x }, usePath $ fout kicksLabel)
              | (is2x, kicksLabel) <- kicksConfigs
              ]
            , yaml
            )
  makeTemplateRunner
    sink
    "Create CON files"
    "%input_dir%/%input_base%%modifiers%_rb2con"
    (getTargetSong id >=> build)
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

type MIDIFunction
  =  RBFile.Song (RBFile.FixedFile U.Beats)
  -> RBFile.Song (RBFile.FixedFile U.Beats)

batchPageDolphin
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> (FilePath -> Maybe MIDIFunction -> Bool -> IO ())
  -> IO ()
batchPageDolphin sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getNoFills <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Remove drum fills")
    return $ FL.getValue box
  getForce22 <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Only show Squier (22-fret) Pro Guitar/Bass")
    return $ FL.getValue box
  getUnmute22 <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Unmute muted Pro Guitar/Bass notes above fret 22")
    return $ FL.getValue box
  getPreview <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Generate preview audio")
    return $ FL.getValue box
  let getMIDIFunction = do
        noFills <- getNoFills
        force22 <- getForce22
        unmute22 <- getUnmute22
        return $ do
          guard $ or [noFills, force22, unmute22]
          Just
            $ (if noFills then RBFile.wiiNoFills else id)
            . (if force22 then RBFile.wiiMustang22 else id)
            . (if unmute22 then RBFile.wiiUnmute22 else id)
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Create .app files"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseDirectory
      FL.setTitle picker "Location for .app files"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> do
            midfn <- getMIDIFunction
            preview <- getPreview
            build f midfn preview
        _ -> return ()
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

batchPagePS
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> (TargetPS FilePath, PSCreate)) -> IO ())
  -> IO ()
batchPagePS sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> let
    centerRect = trimClock 0 250 0 250 rect'
    in centerFixed rect' $ speedPercent True centerRect
  let getTargetSong usePath template = do
        speed <- getSpeed
        return $ \proj -> let
          defPS = def :: TargetPS FilePath
          tgt = defPS
            { ps_Common = (ps_Common defPS)
              { tgt_Speed = Just speed
              }
            }
          fout = T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ PS tgt
            , let
              modifiers = T.pack $ case tgt_Speed $ ps_Common tgt of
                Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                _               -> ""
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          in (tgt, usePath fout)
  makeTemplateRunner
    sink
    "Create PS folders"
    "%input_dir%/%artist% - %title%"
    (getTargetSong PSDir >=> build)
  makeTemplateRunner
    sink
    "Create PS zips"
    "%input_dir%/%input_base%%modifiers%_ps.zip"
    (getTargetSong PSZip >=> build)
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songIDBox
  :: Rectangle
  -> (Maybe (Either Integer T.Text) -> tgt -> tgt)
  -> BuildYamlControl tgt ()
songIDBox rect f = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ FL.checkButtonNew checkArea (Just "Specific Song ID")
  input <- liftIO $ FL.inputNew
    inputArea
    Nothing
    (Just FL.FlNormalInput) -- required for labels to work
  let controlInput = do
        b <- FL.getValue check
        (if b then FL.activate else FL.deactivate) input
  liftIO controlInput
  liftIO $ FL.setCallback check $ \_ -> controlInput
  tell $ do
    b <- FL.getValue check
    s <- FL.getValue input
    return $ Endo $ f $ do
      guard $ b && s /= ""
      Just $ case readMaybe $ T.unpack s of
        Nothing -> Right s
        Just i  -> Left i

starSelectors
  :: Rectangle
  -> [(ScoreTrack, RB.Difficulty, Int, Int)]
  -> IO ()
  -> [(T.Text, [ScoreTrack])]
  -> IO (IO [(ScoreTrack, RB.Difficulty, Int, Int)])
starSelectors rect foundTracks updateLabel slots = let
  rects = splitHorizN (length slots) rect
  instSelector ((lbl, trackFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        matchTracks = [ t | t@(strack, _, _, _) <- foundTracks, elem strack trackFilter ]
    choice <- FL.choiceNew r' $ Just lbl
    FL.setAlign choice $ FLE.Alignments [FLE.AlignTypeTop]
    FL.addName choice "(none)"
    forM_ matchTracks $ \(strack, diff, _, _) -> FL.addName choice $
      scoreTrackName strack <> " " <> case diff of
        RB.Easy   -> "(E)"
        RB.Medium -> "(M)"
        RB.Hard   -> "(H)"
        RB.Expert -> "(X)"
    void $ FL.setValue choice $ FL.MenuItemByIndex $ FL.AtIndex 0
    FL.setCallback choice $ \_ -> updateLabel
    return $ do
      FL.AtIndex i <- FL.getValue choice
      return $ case drop (i - 1) matchTracks of
        t : _ | i /= 0 -> [t]
        _              -> []
  in mconcat $ map instSelector $ zip slots rects

partSelectors
  :: (Default tgt)
  => Rectangle
  -> Project
  -> [(T.Text, tgt -> FlexPartName, FlexPartName -> tgt -> tgt, Part FilePath -> Bool)]
  -> BuildYamlControl tgt ()
partSelectors rect proj slots = let
  rects = splitHorizN (length slots) rect
  fparts = do
    (fpart, part) <- HM.toList $ getParts $ _parts $ projectSongYaml proj
    guard $ part /= def
    return (fpart, T.toTitle $ RBFile.getPartName fpart, part)
  instSelector ((lbl, getter, setter, partFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        fparts' = [ t | t@(_, _, part) <- fparts, partFilter part ]
    choice <- liftIO $ do
      choice <- FL.choiceNew r' $ Just lbl
      FL.setAlign choice $ FLE.Alignments [FLE.AlignTypeTop]
      FL.addName choice "(none)"
      forM_ fparts' $ \(_, opt, _) -> FL.addName choice opt
      void $ FL.setValue choice $ FL.MenuItemByIndex $ FL.AtIndex $
        case findIndex (\(fpart, _, _) -> fpart == getter def) fparts' of
          Nothing -> 0 -- (none)
          Just i  -> i + 1
      return choice
    tell $ do
      FL.AtIndex i <- FL.getValue choice
      return $ Endo $ setter $ case drop (i - 1) fparts' of
        (fpart, _, _) : _ | i /= 0 -> fpart
        _                          -> FlexExtra "undefined"
  in mapM_ instSelector $ zip slots rects

customTitleSuffix
  :: (Event -> IO ())
  -> Rectangle
  -> IO T.Text
  -> (Maybe T.Text -> tgt -> tgt)
  -> BuildYamlControl tgt (IO ())
customTitleSuffix sink rect getSuffix setSuffix = do
  let (checkArea, inputArea) = chopLeft 200 rect
  check <- liftIO $ FL.checkButtonNew checkArea (Just "Custom Title Suffix")
  input <- liftIO $ FL.inputNew
    inputArea
    Nothing
    (Just FL.FlNormalInput) -- required for labels to work
  let controlInput = do
        b <- FL.getValue check
        if b
          then FL.activate input
          else do
            sfx <- getSuffix
            void $ FL.setValue input $ T.strip sfx
            FL.deactivate input
  liftIO $ sink $ EventIO controlInput -- have to delay; can't call now due to dependency loop!
  liftIO $ FL.setCallback check $ \_ -> controlInput
  tell $ do
    b <- FL.getValue check
    s <- FL.getValue input
    return $ Endo $ setSuffix $ guard b >> Just (T.strip s)
  return controlInput

songPageRB3
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetRB3 FilePath -> RB3Create -> IO ())
  -> IO ()
songPageRB3 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT $ do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo $ \rb3 ->
        rb3 { rb3_Common = (rb3_Common rb3) { tgt_Speed = Just speed } }
      return counter
    box2x <- fullWidth 35 $ \rect' -> do
      box <- liftIO $ FL.checkButtonNew rect' (Just "2x Bass Pedal drums")
      tell $ FL.getValue box >>= \b -> return $ Endo $ \rb3 ->
        rb3 { rb3_2xBassPedal = b }
      return box
    fullWidth 35 $ \rect' -> songIDBox rect' $ \sid rb3 ->
      rb3 { rb3_SongID = sid }
    fullWidth 50 $ \rect' -> partSelectors rect' proj
      [ ( "Guitar", rb3_Guitar, (\v rb3 -> rb3 { rb3_Guitar = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProGuitar p))
        )
      , ( "Bass"  , rb3_Bass  , (\v rb3 -> rb3 { rb3_Bass   = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProGuitar p))
        )
      , ( "Keys"  , rb3_Keys  , (\v rb3 -> rb3 { rb3_Keys   = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProKeys p))
        )
      , ( "Drums" , rb3_Drums , (\v rb3 -> rb3 { rb3_Drums  = v })
        , (\p -> isJust $ partDrums p)
        )
      , ( "Vocal" , rb3_Vocal , (\v rb3 -> rb3 { rb3_Vocal  = v })
        , (\p -> isJust $ partVocal p)
        )
      ]
    fullWidth 35 $ \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \rb3 -> return $ targetTitle
          (projectSongYaml proj)
          (RB3 rb3 { rb3_Common = (rb3_Common rb3) { tgt_Title = Just "" } })
        )
        (\msfx rb3 -> rb3
          { rb3_Common = (rb3_Common rb3)
            { tgt_Label = msfx
            }
          }
        )
      liftIO $ FL.setCallback counterSpeed $ \_ -> controlInput
      liftIO $ FL.setCallback box2x $ \_ -> controlInput
  let makeTarget = fmap ($ def) targetModifier
  fullWidth 35 $ \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create CON file"
    FL.setCallback btn1 $ \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB3 CON file"
      FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_rb3con" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ RB3CON f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create Magma project"
    FL.setCallback btn2 $ \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveDirectory
      FL.setTitle picker "Save Magma v2 project"
      FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_project" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ RB3Magma f
        _ -> return ()
    color <- taskColor
    FL.setColor btn1 color
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPageRB2
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetRB2 -> FilePath -> IO ())
  -> IO ()
songPageRB2 sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT $ do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo $ \rb2 ->
        rb2 { rb2_Common = (rb2_Common rb2) { tgt_Speed = Just speed } }
      return counter
    box2x <- fullWidth 35 $ \rect' -> do
      box <- liftIO $ FL.checkButtonNew rect' (Just "2x Bass Pedal drums")
      tell $ FL.getValue box >>= \b -> return $ Endo $ \rb2 ->
        rb2 { rb2_2xBassPedal = b }
      return box
    fullWidth 35 $ \rect' -> songIDBox rect' $ \sid rb2 ->
      rb2 { rb2_SongID = sid }
    fullWidth 50 $ \rect' -> partSelectors rect' proj
      [ ( "Guitar", rb2_Guitar, (\v rb2 -> rb2 { rb2_Guitar = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProGuitar p))
        )
      , ( "Bass"  , rb2_Bass  , (\v rb2 -> rb2 { rb2_Bass   = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProGuitar p))
        )
      , ( "Drums" , rb2_Drums , (\v rb2 -> rb2 { rb2_Drums  = v })
        , (\p -> isJust $ partDrums p)
        )
      , ( "Vocal" , rb2_Vocal , (\v rb2 -> rb2 { rb2_Vocal  = v })
        , (\p -> isJust $ partVocal p)
        )
      ]
    fullWidth 35 $ \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \rb2 -> return $ targetTitle
          (projectSongYaml proj)
          (RB2 rb2 { rb2_Common = (rb2_Common rb2) { tgt_Title = Just "" } })
        )
        (\msfx rb2 -> rb2
          { rb2_Common = (rb2_Common rb2)
            { tgt_Label = msfx
            }
          }
        )
      liftIO $ FL.setCallback counterSpeed $ \_ -> controlInput
      liftIO $ FL.setCallback box2x $ \_ -> controlInput
  let makeTarget = fmap ($ def) targetModifier
  fullWidth 35 $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Create CON file"
    FL.setCallback btn $ \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save RB2 CON file"
      FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_rb2con" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

songPagePS
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> Project
  -> (TargetPS FilePath -> PSCreate -> IO ())
  -> IO ()
songPagePS sink rect tab proj build = mdo
  pack <- FL.packNew rect Nothing
  let fullWidth h = padded 5 10 5 10 (Size (Width 800) (Height h))
  targetModifier <- fmap (fmap appEndo) $ execWriterT $ do
    counterSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> do
      let centerRect = trimClock 0 250 0 250 rect'
      (getSpeed, counter) <- liftIO $
        centerFixed rect' $ speedPercent' True centerRect
      tell $ getSpeed >>= \speed -> return $ Endo $ \ps ->
        ps { ps_Common = (ps_Common ps) { tgt_Speed = Just speed } }
      return counter
    fullWidth 50 $ \rect' -> partSelectors rect' proj
      [ ( "Guitar"     , ps_Guitar    , (\v ps -> ps { ps_Guitar       = v })
        , (\p -> isJust (partGRYBO p) || isJust (partGHL p) || isJust (partProGuitar p))
        )
      , ( "Bass"       , ps_Bass      , (\v ps -> ps { ps_Bass         = v })
        , (\p -> isJust (partGRYBO p) || isJust (partGHL p) || isJust (partProGuitar p))
        )
      , ( "Keys"       , ps_Keys      , (\v ps -> ps { ps_Keys         = v })
        , (\p -> isJust (partGRYBO p) || isJust (partProKeys p))
        )
      , ( "Drums"      , ps_Drums     , (\v ps -> ps { ps_Drums        = v })
        , (\p -> isJust $ partDrums p)
        )
      , ( "Vocal"      , ps_Vocal     , (\v ps -> ps { ps_Vocal        = v })
        , (\p -> isJust $ partVocal p)
        )
      , ( "Rhythm"     , ps_Rhythm    , (\v ps -> ps { ps_Rhythm       = v })
        , (\p -> isJust $ partGRYBO p)
        )
      , ( "Guitar Coop", ps_GuitarCoop, (\v ps -> ps { ps_GuitarCoop   = v })
        , (\p -> isJust $ partGRYBO p)
        )
      ]
    fullWidth 35 $ \rect' -> do
      controlInput <- customTitleSuffix sink rect'
        (makeTarget >>= \ps -> return $ targetTitle
          (projectSongYaml proj)
          (PS ps { ps_Common = (ps_Common ps) { tgt_Title = Just "" } })
        )
        (\msfx ps -> ps
          { ps_Common = (ps_Common ps)
            { tgt_Label = msfx
            }
          }
        )
      liftIO $ FL.setCallback counterSpeed $ \_ -> controlInput
  let makeTarget = fmap ($ def) targetModifier
  fullWidth 35 $ \rect' -> do
    let [trimClock 0 5 0 0 -> r1, trimClock 0 0 0 5 -> r2] = splitHorizN 2 rect'
    btn1 <- FL.buttonNew r1 $ Just "Create CH/PS song folder"
    FL.setCallback btn1 $ \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveDirectory
      FL.setTitle picker "Save CH/PS song folder"
      FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_chps" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ PSDir f
        _ -> return ()
    btn2 <- FL.buttonNew r2 $ Just "Create CH/PS zip file"
    FL.setCallback btn2 $ \_ -> do
      tgt <- makeTarget
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save CH/PS zip file"
      FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_chps.zip" -- TODO add modifiers
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> build tgt $ PSZip f
        _ -> return ()
    color <- FLE.rgbColorWithRgb (179,221,187)
    FL.setColor btn1 color
    FL.setColor btn2 color
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

batchPageRB3
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> ([(TargetRB3 FilePath, RB3Create)], SongYaml FilePath)) -> IO ())
  -> IO ()
batchPageRB3 sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 0 5 0 (Size (Width 800) (Height 35)) $ \rect' -> let
    centerRect = trimClock 0 250 0 250 rect'
    in centerFixed rect' $ speedPercent True centerRect
  getToms <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    box <- FL.checkButtonNew rect' (Just "Convert non-Pro Drums to all toms")
    FL.setTooltip box "When importing from a FoF/PS/CH chart where no Pro Drums are detected, tom markers will be added over the whole drum chart if this box is checked."
    return $ FL.getValue box
  getGBK <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("G/B/K unchanged", GBKUnchanged, True)
      , ("Copy G to K", CopyGuitarToKeys, False)
      , ("Swap G and K", SwapGuitarKeys, False)
      , ("Swap B and K", SwapBassKeys, False)
      ]
    return $ fromMaybe GBKUnchanged <$> fn
  getKicks <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("1x Bass Pedal", Kicks1x, False)
      , ("2x Bass Pedal", Kicks2x, False)
      , ("Both", KicksBoth, True)
      ]
    return $ fromMaybe KicksBoth <$> fn
  let getTargetSong usePath template = do
        speed <- getSpeed
        toms <- getToms
        gbk <- getGBK
        kicks <- getKicks
        return $ \proj -> let
          defRB3 = def :: TargetRB3 FilePath
          tgt = applyGBK gbk yaml defRB3
            { rb3_Common = (rb3_Common defRB3)
              { tgt_Speed = Just speed
              }
            }
          kicksConfigs = case (kicks, maybe Kicks1x drumsKicks $ getPart FlexDrums yaml >>= partDrums) of
            (_        , Kicks1x) -> [(False, ""   )]
            (Kicks1x  , _      ) -> [(False, "_1x")]
            (Kicks2x  , _      ) -> [(True , "_2x")]
            (KicksBoth, _      ) -> [(False, "_1x"), (True, "_2x")]
          fout kicksLabel = T.unpack $ foldr ($) template
            [ templateApplyInput proj $ Just $ RB3 tgt
            , let
              modifiers = T.concat
                [ T.pack $ case tgt_Speed $ rb3_Common tgt of
                  Just n | n /= 1 -> "_" <> show (round $ n * 100 :: Int)
                  _               -> ""
                , kicksLabel
                ]
              in T.intercalate modifiers . T.splitOn "%modifiers%"
            ]
          yaml
            = (if toms then id else forceProDrums)
            $ projectSongYaml proj
          in
            ( [ (tgt { rb3_2xBassPedal = is2x }, usePath $ fout kicksLabel)
              | (is2x, kicksLabel) <- kicksConfigs
              ]
            , yaml
            )
  makeTemplateRunner
    sink
    "Create CON files"
    "%input_dir%/%input_base%%modifiers%_rb3con"
    (getTargetSong RB3CON >=> build)
  makeTemplateRunner
    sink
    "Create Magma projects"
    "%input_dir%/%input_base%%modifiers%_project"
    (getTargetSong RB3Magma >=> build)
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

templateApplyInput :: Project -> Maybe (Target FilePath) -> T.Text -> T.Text
templateApplyInput proj mtgt txt = foldr ($) txt
  [ T.intercalate (T.pack $ takeDirectory $ projectTemplate proj) . T.splitOn "%input_dir%"
  , T.intercalate (T.pack $ takeFileName $ projectTemplate proj) . T.splitOn "%input_base%"
  , T.intercalate (toValidFileName title) . T.splitOn "%title%"
  , T.intercalate (toValidFileName $ getArtist $ _metadata $ projectSongYaml proj) . T.splitOn "%artist%"
  , T.intercalate (toValidFileName $ getAlbum $ _metadata $ projectSongYaml proj) . T.splitOn "%album%"
  , T.intercalate (toValidFileName $ getAuthor $ _metadata $ projectSongYaml proj) . T.splitOn "%author%"
  ] where
    title = case mtgt of
      Nothing  -> getTitle $ _metadata $ projectSongYaml proj
      Just tgt -> targetTitle (projectSongYaml proj) tgt

makeTemplateRunner :: (Event -> IO ()) -> T.Text -> T.Text -> (T.Text -> IO ()) -> IO ()
makeTemplateRunner sink buttonText defTemplate useTemplate = do
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (buttonRect, notButton) = chopLeft 250 rect'
        (_, rectA) = chopLeft 80 notButton
        (inputRect, rectB) = chopRight 100 rectA
        (_, rectC) = chopRight 90 rectB
        (browseRect, _) = chopLeft 40 rectC
        (_, rectD) = chopRight 50 rectC
        (_, resetRect) = chopRight 40 rectD
    button <- FL.buttonNew buttonRect $ Just buttonText
    taskColor >>= FL.setColor button
    input <- FL.inputNew
      inputRect
      (Just "Template")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    void $ FL.setValue input defTemplate
    FL.setTooltip input $ T.unlines
      [ "Template for where to create new files."
      , "  %input_dir% - folder containing the input"
      , "  %input_base% - input filename by itself, extension removed"
      , "  %modifiers% - added distinguishing features e.g. speed modifier"
      , "  %title% - title from song's metadata (including modifiers)"
      , "  %artist% - artist from song's metadata"
      , "  %album% - album from song's metadata"
      , "  %author% - author from song's metadata"
      ]
    FL.setCallback button $ \_ -> FL.getValue input >>= useTemplate
    browseButton <- FL.buttonNew browseRect $ Just "@fileopen"
    FL.setCallback browseButton $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseDirectory
      FL.setTitle picker "Location for output files"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing  -> return ()
          Just dir -> do
            val <- FL.getValue input
            void $ FL.setValue input $ T.pack $ dir </> takeFileName (T.unpack val)
        _ -> return ()
    resetButton <- FL.buttonNew resetRect $ Just "@undo"
    FL.setCallback resetButton $ \_ -> sink $ EventIO $ do
      void $ FL.setValue input defTemplate
    return ()

batchPagePreview
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> FilePath) -> IO ())
  -> IO ()
batchPagePreview sink rect tab build = do
  pack <- FL.packNew rect Nothing
  makeTemplateRunner
    sink
    "Build web previews"
    "%input_dir%/%input_base%_player"
    (\template -> build $ \proj -> T.unpack $ templateApplyInput proj Nothing template)
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

taskOutputPage
  :: Rectangle
  -> FL.Ref FL.Group
  -> (Event -> IO ()) -- event sink
  -> IO () -- callback when starting a task
  -> IO () -- callback when ending a task
  -> IO
    ( [(FilePath, Onyx [FilePath])] -> Onyx () -- returns function to start a task
    , IO () -- cancels a task if running
    )
taskOutputPage rect tab sink cbStart cbEnd = mdo
  -- make the tab
  let (labelRect, belowLabel) = chopTop 40 rect
      (termRect, buttonRect) = chopBottom 50 belowLabel
      labelRect' = trimClock 10 10 5 10 labelRect
      termRect' = trimClock 5 10 5 10 termRect
      buttonRect' = trimClock 5 10 10 10 buttonRect
  labelTask <- FL.boxNew labelRect' $ Just "…"
  termTask <- FL.simpleTerminalNew termRect' Nothing
  FL.setHistoryLines termTask $ FL.Lines (-1) -- unlimited
  FL.setAnsi termTask True
  FL.setStayAtBottom termTask True
  FL.setResizable tab $ Just termTask
  cancelButton <- FL.buttonNew buttonRect' $ Just "Cancel"
  FL.setCallback cancelButton $ \_ -> sink $ EventIO cancelTasks
  showButton <- FL.buttonNew buttonRect' $ Just "Show created files"
  FL.setCallback showButton $ \_ -> sink $ EventIO $ do
    res <- takeMVar taskStatus
    case res of
      Left (Just (dir, files)) -> osShowFolder dir files
      _                        -> return () -- shouldn't happen
    putMVar taskStatus res
  FL.hide showButton
  -- functions
  taskStatus <- newMVar (Left Nothing) :: IO (MVar (Either
      (Maybe (FilePath, [FilePath])) -- finished, maybe have files to show the user
      (Int, Int, [[FilePath]], Int, ThreadId) -- tasks in progress
    ))
  let taskMessage :: (MessageLevel, Message) -> IO ()
      taskMessage = sink . EventIO . addTerm termTask . toTermMessage
      cbStart' = do
        cbStart
        FL.activate cancelButton
        FL.showWidget cancelButton
        FL.hide showButton
        FLTK.redraw
      cbEnd' = do
        cbEnd
        FL.deactivate cancelButton
        FLTK.redraw
      putFinalResults success = do
        common <- commonDir $ concat success
        putMVar taskStatus $ Left common
        case common of
          Just _  -> do
            FL.showWidget showButton
            FL.hide cancelButton
          Nothing -> return ()
      cancelTasks = do
        takeMVar taskStatus >>= \case
          Right (_, _, success, _, tid) -> do
            addTerm termTask $ TermLog "Cancelled tasks."
            killThread tid
            putFinalResults success
          Left res                -> putMVar taskStatus $ Left res
        cbEnd'
      updateStatus fn = takeMVar taskStatus >>= \case
        Left res       -> putMVar taskStatus $ Left res -- maybe task was cancelled
        Right oldStatus -> do
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
              cbEnd'
              putFinalResults success
            else putMVar taskStatus $ Right status
      startTasks :: [(String, Onyx [FilePath])] -> Onyx ()
      startTasks tasks = liftIO (takeMVar taskStatus) >>= \case
        Right status -> liftIO $ putMVar taskStatus $ Right status -- shouldn't happen
        Left _ -> do
          tid <- forkOnyx $ replaceQueueLog taskMessage $ forM_ (zip [1..] tasks) $ \(i, (taskName, task)) -> do
            liftIO $ sink $ EventIO $ addTerm termTask $ TermStart ("Task " <> show (i :: Int)) taskName
            errorToEither task >>= \case
              Left e -> liftIO $ sink $ EventIO $ do
                forM_ (getMessages e) $ addTerm termTask . TermError
                updateStatus $ \case
                  (done, total, success, failure, tid) -> (done + 1, total, success, failure + 1, tid)
              Right fs -> liftIO $ sink $ EventIO $ do
                addTerm termTask $ TermSuccess $ case fs of
                  [] -> ""
                  _  -> unlines $ "Created files:" : map ("  " <>) fs
                updateStatus $ \case
                  (done, total, success, failure, tid) -> (done + 1, total, fs : success, failure, tid)
          liftIO $ do
            putMVar taskStatus $ Right (0, length tasks, [], 0, tid)
            cbStart'
            updateStatus id
  return (startTasks, cancelTasks)

homeTabColor, functionTabColor, taskColor, globalLogColor, miscColor, loadSongColor, batchProcessColor, behindTabsColor, defaultColor :: IO FLE.Color
homeTabColor      = FLE.rgbColorWithRgb (209,177,224)
functionTabColor  = FLE.rgbColorWithRgb (224,210,177)
taskColor         = FLE.rgbColorWithRgb (179,221,187)
globalLogColor    = FLE.rgbColorWithRgb (114,74,124)
miscColor         = FLE.rgbColorWithRgb (177,173,244)
loadSongColor     = FLE.rgbColorWithRgb (186,229,181)
batchProcessColor = FLE.rgbColorWithRgb (237,173,193)
behindTabsColor   = FLE.rgbColorWithRgb (94,94,94) -- TODO report incorrect Char binding type for rgbColorWithGrayscale
defaultColor      = FLE.rgbColorWithRgb (192,192,192)

setTabColor :: FL.Ref FL.Group -> FLE.Color -> IO ()
setTabColor tab color = do
  FL.setColor tab color
  FL.setSelectionColor tab color

data AudioSpec = AudioSpec
  { audioPath     :: FilePath
  , audioFormat   :: T.Text
  , audioFrames   :: Int
  , audioRate     :: Double
  , audioChannels :: Int
  }

getAudioSpec :: FilePath -> IO (Maybe AudioSpec)
getAudioSpec f = Exc.try (Snd.getFileInfo f) >>= \case
  Left e -> let
    _ = e :: Snd.Exception
    in return Nothing -- TODO handle MP3
  Right info -> let
    withFormat fmt = return $ Just AudioSpec
      { audioPath = f
      , audioFormat = fmt
      , audioFrames = Snd.frames info
      , audioRate = realToFrac $ Snd.samplerate info
      , audioChannels = Snd.channels info
      }
    in case Snd.headerFormat $ Snd.format info of
      Snd.HeaderFormatWav  -> withFormat "WAVE"
      Snd.HeaderFormatOgg  -> withFormat "Ogg Vorbis"
      Snd.HeaderFormatFlac -> withFormat "FLAC"
      _                    -> return Nothing

miscPageMilo
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMilo sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Extract .milo to folder"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> do
      picker1 <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker1 "Load .milo"
      FL.setFilter picker1 "*.{milo_xbox,milo_ps3,milo_wii}"
      FL.showWidget picker1 >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker1 >>= \case
          Nothing  -> return ()
          Just finText@(T.unpack -> fin) -> do
            picker2 <- FL.nativeFileChooserNew $ Just FL.BrowseSaveDirectory
            FL.setTitle picker2 "Create folder"
            FL.setPresetFile picker2 $ finText <> "-extract"
            FL.showWidget picker2 >>= \case
              FL.NativeFileChooserPicked -> FL.getFilename picker2 >>= \case
                Nothing -> return ()
                Just (T.unpack -> dout) -> sink $ EventOnyx $ let
                  task = do
                    unpackMilo fin dout
                    return [dout]
                  in startTasks [("Extract " <> fin, task)]
              _ -> return ()
        _ -> return ()
    return ()
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Repack .milo from folder"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> do
      picker1 <- FL.nativeFileChooserNew $ Just FL.BrowseDirectory
      FL.setTitle picker1 "Select extracted folder"
      FL.showWidget picker1 >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker1 >>= \case
          Nothing -> return ()
          Just dinText@(T.unpack -> din) -> do
            picker2 <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
            FL.setTitle picker2 "Save .milo"
            FL.setPresetFile picker2 $ case T.stripSuffix "-extract" dinText of
              Just stripped -> stripped
              Nothing       -> dinText <> ".milo"
            FL.setFilter picker2 "*.{milo_xbox,milo_ps3,milo_wii}"
            FL.showWidget picker2 >>= \case
              FL.NativeFileChooserPicked -> FL.getFilename picker2 >>= \case
                Nothing -> return ()
                Just (T.unpack -> fout) -> sink $ EventOnyx $ let
                  task = do
                    packMilo din fout
                    return [fout]
                  in startTasks [("Repack " <> din, task)]
              _ -> return ()
        _ -> return ()
    return ()
  {-
  padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    group <- FL.groupNew rect' Nothing
    let (trimClock 0 10 0 100 -> dropdownArea, btnArea) = chopRight 160 rect'
    choice <- FL.choiceNew dropdownArea $ Just "Contents"
    mapM_ (FL.addName choice)
      [ "RB2, empty"
      , "RB2, 1 lipsync"
      , "RB3, empty"
      , "RB3, 1 lipsync"
      , "RB3, 2 lipsync"
      , "RB3, 3 lipsync"
      , "RB3, venue"
      , "RB3, 1 lipsync + venue"
      , "RB3, 2 lipsync + venue"
      , "RB3, 3 lipsync + venue"
      ]
    btn <- FL.buttonNew btnArea $ Just "Create .milo"
    -- TODO
    FL.end group
    FL.setResizable group $ Just choice
    return ()
  -}
  FL.end pack
  FL.setResizable tab $ Just pack

miscPageLipsync
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageLipsync sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "MIDI file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load MIDI file"
      FL.setFilter picker "*.{mid,midi}" -- TODO also handle .chart?
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  getVocalTrack <- padded 2 10 2 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("PART VOCALS", Just Nothing, True)
      , ("[PART] HARM1", Just $ Just Vocal1, False)
      , ("[PART] HARM2", Just $ Just Vocal2, False)
      , ("[PART] HARM3", Just $ Just Vocal3, False)
      , ("(blank)", Nothing, False)
      ]
    return $ join <$> fn
  let getSelectedVox = \case
        Nothing            -> const mempty
        Just Nothing       -> RBFile.fixedPartVocals . RBFile.s_tracks
        Just (Just Vocal1) -> RBFile.fixedHarm1      . RBFile.s_tracks
        Just (Just Vocal2) -> RBFile.fixedHarm2      . RBFile.s_tracks
        Just (Just Vocal3) -> RBFile.fixedHarm3      . RBFile.s_tracks
      defaultSuffix = \case
        Nothing            -> "-blank"
        Just Nothing       -> "-solovox"
        Just (Just Vocal1) -> "-harm1"
        Just (Just Vocal2) -> "-harm2"
        Just (Just Vocal3) -> "-harm3"
  getVowels <- padded 2 10 2 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("English", englishVowels, True)
      , ("German", germanVowels, False)
      , ("Spanish", spanishVowels, False)
      ]
    return $ fromMaybe englishVowels <$> fn
  padded 5 5 5 5 (Size (Width 800) (Height 35)) $ \rect' -> do
    let [areaVoc, areaRB, areaTBRB] = map (trimClock 0 5 0 5) $ splitHorizN 3 rect'
        lipsyncButton area label ext fn = do
          btn <- FL.buttonNew area $ Just label
          taskColor >>= FL.setColor btn
          FL.setCallback btn $ \_ -> sink $ EventIO $ do
            input <- pickedFile
            voc <- getVocalTrack
            vowels <- getVowels
            picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
            FL.setTitle picker "Save lipsync file"
            FL.setFilter picker $ "*." <> T.pack ext
            FL.setPresetFile picker $ T.pack $ (dropExtension input ++ defaultSuffix voc) <.> ext
            FL.showWidget picker >>= \case
              FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
                Nothing -> return ()
                Just f -> sink $ EventOnyx $ let
                  task = do
                    mid <- RBFile.loadMIDI input
                    stackIO $ BL.writeFile f $ fn vowels $
                      mapTrack (U.applyTempoTrack $ RBFile.s_tempos mid) $ getSelectedVox voc mid
                    return [f]
                  in startTasks [(T.unpack label <> ": " <> input, task)]
              _ -> return ()
          return btn
    void $ lipsyncButton areaVoc "Make .voc (GH2)" "voc"
      $ \vowels -> runPut . putVocFile . gh2Lipsync vowels
    void $ lipsyncButton areaRB "Make .lipsync (RB2/RB3)" "lipsync"
      $ \vowels -> runPut . putLipsync . autoLipsync vowels
    void $ lipsyncButton areaTBRB "Make .lipsync (TBRB)" "lipsync"
      $ \vowels -> runPut . putLipsync . beatlesLipsync vowels
    return ()
  let dryvoxButton label fn = padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
        btn <- FL.buttonNew rect' $ Just label
        taskColor >>= FL.setColor btn
        FL.setCallback btn $ \_ -> sink $ EventIO $ do
          input <- pickedFile
          voc <- getVocalTrack
          picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
          FL.setTitle picker "Save lipsync audio"
          FL.setFilter picker "*.wav"
          FL.setPresetFile picker $ T.pack $ (dropExtension input ++ defaultSuffix voc) <.> "wav"
          FL.showWidget picker >>= \case
            FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
              Nothing -> return ()
              Just f  -> sink $ EventOnyx $ let
                task = do
                  mid <- RBFile.loadMIDI input
                  let trk = mapTrack (U.applyTempoTrack $ RBFile.s_tempos mid) $ getSelectedVox voc mid
                  src <- toDryVoxFormat <$> fn trk
                  runAudio src f
                  return [f]
                in startTasks [(T.unpack label <> ": " <> input, task)]
            _ -> return ()
  dryvoxButton "Make sine wave dry vox from MIDI" $ return . sineDryVox
  pickedAudio <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "Audio file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load song or vocals audio"
      FL.setFilter picker "*.{wav,ogg,mp3,flac}"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  dryvoxButton "Make clipped dry vox from MIDI and audio" $ \trk -> do
    audio <- stackIO $ pickedAudio
    src <- buildSource' $ Input audio
    return $ clipDryVox (isJust <$> vocalTubes trk) src
  FL.end pack
  FL.setResizable tab $ Just pack

miscPageBlack
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageBlack sink rect tab startTasks = do
  loadedFiles <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      startRect' = trimClock 5 10 10 10 startRect
  group <- fileLoadWindow filesRect sink "Song" "Songs" (modifyMVar_ loadedFiles) [] findSongs $ \imp -> let
    entry = T.concat
      [ fromMaybe "Untitled" $ impTitle imp
      , maybe "" (\art -> " (" <> art <> ")") $ impArtist imp
      ]
    sublines = concat
      [ case impAuthor imp of
        Just author | T.any (not . isSpace) author -> ["Author: " <> author]
        _                                          -> []
      , ["Format: " <> impFormat imp]
      , ["Path: " <> T.pack (impPath imp) <> index]
      ]
    index = case impIndex imp of
      Nothing -> ""
      Just i  -> T.pack $ " (#" <> show i <> ")"
    in (entry, sublines)
  btn <- FL.buttonNew startRect' $ Just "Modify CON files"
  taskColor >>= FL.setColor btn
  FL.setResizable tab $ Just group
  FL.setCallback btn $ \_ -> sink $ EventIO $ do
    files <- map impPath <$> readMVar loadedFiles
    sink $ EventOnyx $ startTasks $ do
      f <- files
      return (f, blackVenue f >> return [f])

miscPageMOGG
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMOGG sink rect tab startTasks = mdo
  loadedAudio <- newMVar []
  let (filesRect, startRect) = chopBottom 50 rect
      startRect' = trimClock 5 10 10 10 startRect
      isSingleOgg = \case
        [aud] | audioFormat aud == "Ogg Vorbis" -> Just aud
        _                                       -> Nothing
      modifyAudio :: ([AudioSpec] -> IO [AudioSpec]) -> IO ()
      modifyAudio f = do
        newAudio <- modifyMVar loadedAudio $ \auds -> (\x -> (x, x)) <$> f auds
        let chans = sum $ map audioChannels newAudio
        sink $ EventIO $ do
          FL.setLabel btn $ let
            op = case isSingleOgg newAudio of
              Just _  -> "Ogg to MOGG, no re-encode"
              Nothing -> "Combine into MOGG"
            in op <> " (" <> T.pack (show chans) <> " channels)"
          if chans == 0
            then FL.deactivate btn
            else FL.activate   btn
  group <- fileLoadWindow filesRect sink "Audio" "Audio" modifyAudio []
    (\f -> liftIO $ ([],) . toList <$> getAudioSpec f)
    $ \info -> let
      entry = T.pack $ audioPath info
      sublines =
        [ audioFormat info <> ", " <> T.pack (show $ audioChannels info)
          <> case audioChannels info of 1 -> " channel"; _ -> " channels"
        , let
          time = realToFrac (audioFrames info) / audioRate info
          mins = floor $ time / 60 :: Int
          secs = realToFrac $ time - realToFrac mins * 60 :: Milli
          zero = if secs < 10 then "0" else ""
          stamp = T.pack $ show mins <> ":" <> zero <> show secs
          -- `time` package can do this once 1.9 is used in GHC 8.8
          in stamp <> " (" <> T.pack (show $ audioFrames info) <> " frames @ " <> T.pack (show $ audioRate info) <> "Hz)"
        ]
      in (entry, sublines)
  btn <- FL.buttonNew startRect' Nothing
  taskColor >>= FL.setColor btn
  FL.setResizable tab $ Just group
  FL.setCallback btn $ \_ -> sink $ EventIO $ do
    audio <- readMVar loadedAudio
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save MOGG file"
    case audio of
      [aud] -> FL.setDirectory picker $ T.pack $ takeDirectory $ audioPath aud
      _     -> return ()
    FL.setFilter picker "*.mogg"
    FL.setPresetFile picker $ case audio of
      [aud] -> T.pack $ takeFileName (audioPath aud) -<.> "mogg"
      _     -> "out.mogg"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> let
          ext = map toLower $ takeExtension f
          f' = if ext == ".mogg"
            then f
            else f <.> "mogg"
          in sink $ EventOnyx $ startTasks $ case isSingleOgg audio of
            Just aud -> [("Ogg to MOGG", oggToMogg (audioPath aud) f' >> return [f'])]
            Nothing  -> let
              task = tempDir "makemogg" $ \tmp -> do
                let ogg = tmp </> "temp.ogg"
                src <- buildSource' $ Merge $ map (Input . audioPath) audio
                runAudio src ogg
                oggToMogg ogg f'
                return [f']
              in [("MOGG file creation", task)]
      _ -> return ()

miscPageMIDI
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMIDI sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "MIDI file")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load MIDI file"
      FL.setFilter picker "*.{mid,midi}" -- TODO also handle .chart?
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Fill in lower difficulties and drum animations"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save reduced MIDI file"
      FL.setFilter picker "*.mid"
      FL.setPresetFile picker $ T.pack $ input -<.> "reduced.mid"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> let
            ext = map toLower $ takeExtension f
            f' = if elem ext [".mid", ".midi"]
              then f
              else f <.> "mid"
            in sink $ EventOnyx $ let
              task = do
                simpleReduce input f'
                return [f']
              in startTasks [("Reduce MIDI: " <> input, task)]
        _ -> return ()
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Find hanging Pro Keys notes"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      sink $ EventOnyx $ let
        task = do
          mid <- RBFile.loadMIDI input
          lg $ closeShiftsFile mid
          return []
        in startTasks [("Pro Keys range check: " <> input, task)]
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Make REAPER project with RB template"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save REAPER project"
      FL.setFilter picker "*.RPP"
      FL.setPresetFile picker $ T.pack $ input -<.> "RPP"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
          Nothing -> return ()
          Just f  -> let
            ext = map toUpper $ takeExtension f
            f' = if ext == ".RPP"
              then f
              else f <.> "RPP"
            in sink $ EventOnyx $ let
              task = do
                makeReaper [] input input [] f'
                return [f']
              in startTasks [("Make REAPER project: " <> input, task)]
        _ -> return ()
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

miscPageSongCache
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageSongCache sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFile <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "Song cache")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load RB3 song cache"
      FL.setFilter picker "songcache"
      FL.showWidget picker >>= \case
        FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
          Nothing -> return ()
          Just f  -> void $ FL.setValue input f
        _                          -> return ()
    return $ fmap T.unpack $ FL.getValue input
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Update outdated songs in cache"
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      sink $ EventOnyx $ let
        task = do
          fixSongCache input
          return [input]
        in startTasks [("Fix song cache: " <> input, task)]
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

launchMisc :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
launchMisc sink makeMenuBar = mdo
  let windowWidth = Width 800
      windowHeight = Height 400
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew windowSize Nothing $ Just "Tools"
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  behindTabsColor >>= FL.setColor window
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRange window windowSize
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  functionTabs <- sequence
    [ makeTab windowRect "MIDI functions" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMIDI sink rect tab startTasks
      return tab
    , makeTab windowRect "MOGG creator" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMOGG sink rect tab startTasks
      return tab
    , makeTab windowRect "Lipsync + dry vox" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageLipsync sink rect tab startTasks
      return tab
    , makeTab windowRect ".milo functions" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMilo sink rect tab startTasks
      return tab
    , makeTab windowRect "Black VENUE" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageBlack sink rect tab startTasks
      return tab
    {-
    , makeTab windowRect "RB3 song cache" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageSongCache sink rect tab startTasks
      return tab
    -}
    ]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" $ \rect tab -> do
    taskColor >>= setTabColor tab
    FL.deactivate tab
    let cbStart = do
          FL.activate tab
          mapM_ FL.deactivate functionTabs
          void $ FL.setValue tabs $ Just tab
          updateTabsColor tabs
        cbEnd = do
          mapM_ FL.activate functionTabs
    taskOutputPage rect tab sink cbStart cbEnd
  FL.end tabs
  updateTabsColor tabs
  FL.setCallback tabs updateTabsColor
  FL.setResizable tabs $ Just $ head functionTabs
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser cancelTasks
  FL.showWidget window

watchSong :: IO () -> FilePath -> Onyx (IO [[(T.Text, PreviewTrack)]], IO ())
watchSong notify mid = do
  varTrack <- loadTracks mid >>= liftIO . newIORef
  chan <- liftIO newChan
  let midFileName = takeFileName mid
      sendClose = do
        t <- liftIO getCurrentTime
        writeChan chan $ FS.Unknown "" t "STOP_WATCH"
  _ <- forkOnyx $ do
    wm <- liftIO FS.startManager
    let test = \case
          FS.Added    f _ _ -> takeFileName f == midFileName
          FS.Modified f _ _ -> takeFileName f == midFileName
          _                 -> False
    _ <- liftIO $ FS.watchDirChan wm (takeDirectory mid) test chan
    let go = liftIO (readChan chan) >>= \case
          FS.Unknown _ _ "STOP_WATCH" -> liftIO $ FS.stopManager wm
          _ -> do
            lg $ "Reloading from " <> mid
            safeOnyx $ do
              loadTracks mid >>= liftIO . writeIORef varTrack
              liftIO notify
            go
    go
  return (previewTracks <$> readIORef varTrack, sendClose)

launchTimeServer
  :: (Event -> IO ())
  -> IORef Double
  -> FL.Ref FL.Input
  -> FL.Ref FL.Button
  -> FL.Ref FL.Box
  -> IO (IO ())
launchTimeServer sink varTime inputPort button label = do
  -- TODO something is wrong here on Windows;
  -- stopping the server before a connection comes in
  -- seems to kill the thread (GUI stops updating)
  presses <- newChan
  FL.setCallback button $ \_ -> writeChan presses ()
  tid <- forkIO $ runResourceT $ let
    goOffline :: ResourceT IO ()
    goOffline = do
      liftIO $ do
        FL.setLabel button "Start"
        FL.setLabel label "Server offline."
        sink $ EventIO FLTK.redraw
      () <- liftIO $ readChan presses
      s <- liftIO $ FL.getValue inputPort
      case readMaybe $ T.unpack s of
        Nothing -> goOffline
        Just p -> catchIOError
          (Just <$> allocate (TCP.bindAndListen 1024 p) Socket.close)
          (\e -> liftIO (sink $ EventOnyx $ lg $ "Failed to start server: " <> show e) >> return Nothing)
          >>= maybe goOffline (goOnline p)
    goOnline port ps@(sockKey, sock) = do
      liftIO $ do
        FL.setLabel button "Stop"
        FL.setLabel label $ "Waiting for connections on port " <> T.pack (show port) <> "."
        sink $ EventIO FLTK.redraw
      asyncStop <- liftIO $ async $ Left <$> readChan presses
      asyncConn <- liftIO $ async $ Right <$> TCP.accept sock
      liftIO (snd <$> waitAnyCancel [asyncStop, asyncConn]) >>= \case
        Left () -> release sockKey >> goOffline
        Right conn -> do
          connKey <- register $ Conn.close conn
          liftIO $ do
            FL.setLabel label "Connected."
            sink $ EventIO FLTK.redraw
          goConnected B.empty port ps (connKey, conn)
    goConnected dat port ps@(sockKey, _sock) pc@(connKey, conn) = do
      asyncStop <- liftIO $ async $ Left <$> readChan presses
      asyncData <- liftIO $ async $ Right <$> Streams.read (Conn.source conn)
      liftIO (snd <$> waitAnyCancel [asyncStop, asyncData]) >>= \case
        Left () -> release connKey >> release sockKey >> goOffline
        Right Nothing -> release connKey >> goOnline port ps
        Right (Just bs) -> let
          dat' = dat <> bs
          in case reverse $ B8.split '|' dat' of
            after : s : _ -> do
              forM_ (readMaybe $ B8.unpack s) $ \d -> liftIO $ sink $ EventIO $ do
                old <- readIORef varTime
                when (old /= d) $ do
                  writeIORef varTime d
                  FLTK.redraw
              goConnected after port ps pc
            _ -> goConnected dat' port ps pc
    in goOffline
  return $ killThread tid

data GLStatus = GLPreload | GLLoaded RGGraphics.GLStuff | GLFailed

previewGroup
  :: (Event -> IO ())
  -> Rectangle
  -> IO [[(T.Text, PreviewTrack)]]
  -> IO Double
  -> IO Double
  -> IO (FL.Ref FL.Group, IO ())
previewGroup sink rect getTracks getTime getSpeed = do
  let (glArea, bottomControlsArea) = chopBottom 40 rect
      partSelectArea = trimClock 6 15 6 15 bottomControlsArea

  wholeGroup <- FL.groupNew rect Nothing

  bottomControlsGroup <- FL.groupNew bottomControlsArea Nothing
  trackMenu <- FL.menuButtonNew partSelectArea $ Just "Select Tracks"
  currentParts <- newIORef []
  let selectedNames = do
        items <- FL.getMenu trackMenu
        fmap catMaybes $ forM items $ \case
          Nothing -> return Nothing
          Just item -> FL.getFlags item >>= \case
            Just (FL.MenuItemFlags flags) | elem FL.MenuItemValue flags -> do
              Just <$> FL.getText item
            _ -> return Nothing
      updateParts redraw names = sink $ EventIO $ do
        cur <- readIORef currentParts
        when (cur /= names) $ do
          selected <- case cur of
            [] -> return $ names >>= take 1
            _  -> selectedNames
          FL.clear trackMenu
          forM_ (concat names) $ \t -> do
            let flags = FL.MenuItemToggle : [FL.MenuItemValue | elem t selected]
            FL.add trackMenu t Nothing
              (Nothing :: Maybe (FL.Ref FL.MenuItem -> IO ()))
              (FL.MenuItemFlags flags)
          writeIORef currentParts names
          when redraw $ sink $ EventIO FLTK.redraw
  getTracks >>= updateParts False . map (map fst)
  FL.setCallback trackMenu $ \_ -> do
    sink $ EventIO $ FLTK.redraw
    sink $ EventIO $ void $ FL.popup trackMenu -- reopen menu (TODO find a way to not close it at all)
  FL.end bottomControlsGroup
  FL.setResizable bottomControlsGroup $ Just trackMenu

  varStuff <- newMVar GLPreload
  let draw :: FL.Ref FL.GlWindow -> IO ()
      draw wind = do
        mstuff <- modifyMVar varStuff $ \case
          GLPreload -> embedOnyx sink RGGraphics.loadGLStuff >>= \case
            Nothing -> return (GLFailed, Nothing)
            Just s  -> return (GLLoaded s, Just s)
          loaded@(GLLoaded s) -> return (loaded, Just s)
          GLFailed -> return (GLFailed, Nothing)
        forM_ mstuff $ \stuff -> do
          t <- getTime
          speed <- getSpeed
          trks <- getTracks
          updateParts True $ map (map fst) trks -- TODO does this need to be done in a sink event
          selected <- selectedNames
          w <- FL.pixelW wind
          h <- FL.pixelH wind
          let flatTrks = concat trks
          RGGraphics.drawTracks stuff (RGGraphics.WindowDims w h) t speed
            $ mapMaybe (`lookup` flatTrks) selected
  -- TODO do we want to set "FL.setUseHighResGL True" here for mac?
  glwindow <- FLGL.glWindowCustom
    (rectangleSize glArea)
    (Just $ rectanglePosition glArea)
    Nothing -- label
    (Just draw)
    FL.defaultCustomWidgetFuncs
    FL.defaultCustomWindowFuncs
  FL.end glwindow
  FL.setMode glwindow $ FLE.Modes [FLE.ModeOpenGL3, FLE.ModeDepth, FLE.ModeRGB8, FLE.ModeDouble, FLE.ModeAlpha, FLE.ModeMultisample]

  FL.end wholeGroup
  FL.setResizable wholeGroup $ Just glwindow
  return (wholeGroup, FL.redraw glwindow)

launchPreview :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> FilePath -> Onyx ()
launchPreview sink makeMenuBar mid = mdo
  (getTracks, stopWatch) <- watchSong (sink $ EventIO redraw) mid
  redraw <- liftIO $ do

    let windowWidth = Width 800
        windowHeight = Height 600
        windowSize = Size windowWidth windowHeight
    window <- FL.windowNew windowSize Nothing $ Just "Onyx Preview"
    menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
    let (_, windowRect) = chopTop menuHeight $ Rectangle
          (Position (X 0) (Y 0))
          windowSize
        (controlsArea, belowTopControls) = chopTop 40 windowRect
        (trimClock 6 3 6 66 -> portArea, controls1) = chopLeft 150 controlsArea
        (trimClock 6 3 6 3 -> buttonArea, controls2) = chopLeft 100 controls1
        labelArea = trimClock 6 6 6 3 controls2

    controlsGroup <- FL.groupNew controlsArea Nothing

    inputPort <- FL.inputNew
      portArea
      (Just "Port")
      (Just FL.FlNormalInput) -- required for label to work
    FL.setLabelsize inputPort $ FL.FontSize 13
    FL.setLabeltype inputPort FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign inputPort $ FLE.Alignments [FLE.AlignTypeLeft]
    void $ FL.setValue inputPort "4938"

    buttonServer <- FL.buttonNew buttonArea $ Just "..."
    FL.setCallback buttonServer $ \_ -> sink $ EventIO $ return ()

    labelServer <- FL.boxNew
      labelArea
      (Just "...")
    FL.setLabel labelServer "..."

    FL.end controlsGroup
    FL.setResizable controlsGroup $ Just labelServer

    varTime <- newIORef 0
    (groupGL, redrawGL) <- previewGroup
      sink
      belowTopControls
      getTracks
      (readIORef varTime)
      (return 1)

    stopServer <- launchTimeServer
      sink
      varTime
      inputPort
      buttonServer
      labelServer

    FL.end window
    FL.setResizable window $ Just groupGL
    FL.setCallback window $ windowCloser $ do
      stopServer
      stopWatch

    FL.showWidget window
    return redrawGL
  return ()

promptPreview :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
promptPreview sink makeMenuBar = sink $ EventIO $ do
  picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
  FL.setTitle picker "Load MIDI, .chart, or REAPER project"
  FL.setFilter picker "*.{mid,midi,RPP,chart}"
  FL.showWidget picker >>= \case
    FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
      Nothing -> return ()
      Just f  -> sink $ EventOnyx $ launchPreview sink makeMenuBar $ T.unpack f
    _                          -> return ()

fileLoadWindow
  :: Rectangle
  -> (Event -> IO ())
  -> T.Text -- ^ singular
  -> T.Text -- ^ plural
  -> (([a] -> IO [a]) -> IO ()) -- ^ read and/or modify the current list of files
  -> [FilePath] -- ^ initial paths to start searching
  -> (FilePath -> Onyx ([FilePath], [a])) -- ^ one step of file search process
  -> (a -> (T.Text, [T.Text])) -- ^ display an entry in the tree
  -> IO (FL.Ref FL.Group)
fileLoadWindow rect sink single plural modifyFiles startFiles step display = mdo
  group <- FL.groupNew rect Nothing
  let (labelRect, belowLabel) = chopTop 40 rect
      (termRect, buttonsRect) = chopBottom 50 belowLabel
      labelRect' = trimClock 10 10 5 10 labelRect
      termRect' = trimClock 5 10 5 10 termRect
      buttonsRect' = trimClock 5 10 10 10 buttonsRect
      [btnRectA, btnRectB] = splitHorizN 2 buttonsRect'
      (btnRectA', _) = chopRight 5 btnRectA
      (_, btnRectB') = chopLeft 5 btnRectB
  label <- FL.boxNew labelRect' Nothing
  tree <- FL.treeCustom termRect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
    { FL.handleCustom = Just $ \_ evt -> case evt of
      FLE.Keydown -> do
        cmd <- FLTK.eventCommand
        let upToSong :: FL.Ref FL.TreeItem -> MaybeT IO (FL.Ref FL.TreeItem)
            upToSong i = lift (FL.getDepth i) >>= \case
              0 -> MaybeT $ return Nothing
              1 -> return i
              _ -> MaybeT (FL.getParent i) >>= upToSong
            getSongFocus :: MaybeT IO (FL.Ref FL.TreeItem, Int, FL.Ref FL.TreeItem)
            getSongFocus = do
              item <- MaybeT $ FL.getItemFocus tree
              songItem <- upToSong item
              root <- MaybeT $ FL.root tree
              FL.AtIndex ix <- MaybeT $ FL.findChild root
                $ FL.TreeItemPointerLocator $ FL.TreeItemPointer songItem
              return (root, ix, songItem)
        FLTK.eventKey >>= \case
          FL.SpecialKeyType FLE.Kb_Up | cmd -> do
            void $ runMaybeT $ do
              (root, ix, _songItem) <- getSongFocus
              unless (ix == 0) $ lift $ swapFiles root (ix - 1) ix
            return $ Right ()
          FL.SpecialKeyType FLE.Kb_Down | cmd -> do
            void $ runMaybeT $ do
              (root, ix, _songItem) <- getSongFocus
              len <- liftIO $ FL.children root
              unless (ix == len - 1) $ lift $ swapFiles root ix (ix + 1)
            return $ Right ()
          FL.SpecialKeyType FLE.Kb_Delete -> do
            void $ runMaybeT $ do
              (_root, ix, songItem) <- getSongFocus
              lift $ removeFile songItem ix
            return $ Right ()
          _ -> FL.handleTreeBase (FL.safeCast tree) evt
      _ -> FL.handleTreeBase (FL.safeCast tree) evt
    }
  FL.end tree
  FL.setResizable group $ Just tree
  let updateLabel fs = FL.setLabel label $ T.pack $ case length fs of
        1 -> "1 file loaded."
        n -> show n ++ " files loaded."
      clearFiles = modifyFiles $ \_ -> do
        FL.clear tree
        _ <- FL.add tree ""
        FL.rootLabel tree plural
        updateLabel []
        FLTK.redraw
        return []
      swapFiles :: FL.Ref FL.TreeItem -> Int -> Int -> IO ()
      swapFiles root index1 index2 = modifyFiles $ \fs -> do
        let fs' = zipWith (\i _ -> fs !! if i == index1 then index2 else if i == index2 then index1 else i) [0..] fs
        void $ FL.swapChildren root (FL.AtIndex index1) (FL.AtIndex index2)
        updateLabel fs'
        FLTK.redraw
        return fs'
      removeFile songItem index = modifyFiles $ \fs -> do
        let fs' = take index fs ++ drop (index + 1) fs
        void $ FL.remove tree songItem
        updateLabel fs'
        FLTK.redraw
        return fs'
      addFiles [] = return ()
      addFiles gs = modifyFiles $ \fs -> do
        Just root <- FL.root tree
        forM_ gs $ \imp -> do
          let (entry, sublines) = display imp
          Just item <- FL.addAt tree entry root
          forM_ sublines $ \subline -> void $ FL.addAt tree subline item
          FL.close item
        let fs' = fs ++ gs
        updateLabel fs'
        FLTK.redraw
        return fs'
      searchSongs [] = return ()
      searchSongs (loc : locs) = do
        (children, imps) <- step loc
        stackIO $ sink $ EventIO $ addFiles imps
        searchSongs $ locs ++ children
      forkSearch = void . forkOnyx . searchSongs
  clearFiles
  sink $ EventOnyx $ forkSearch startFiles
  void $ FL.boxCustom termRect' Nothing Nothing $ Just FL.defaultCustomWidgetFuncs
    { FL.handleCustom = Just
      $ dragAndDrop (sink . EventOnyx . forkSearch)
      . (\_ _ -> return $ Left FL.UnknownEvent)
    }
  btnA <- FL.buttonNew btnRectA' $ Just $ "Add " <> single
  FL.setCallback btnA $ \_ -> sink $ EventIO $ do
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseMultiFile
    FL.setTitle picker $ "Load " <> single
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> do
        n <- FL.getCount picker
        fs <- forM [0 .. n - 1] $ FL.getFilenameAt picker . FL.AtIndex
        sink $ EventOnyx $ forkSearch $ map T.unpack $ catMaybes fs
      _ -> return ()
  btnB <- FL.buttonNew btnRectB' $ Just $ "Clear " <> plural
  FL.setCallback btnB $ \_ -> sink $ EventIO clearFiles
  FL.end group
  return group

launchBatch :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> [FilePath] -> IO ()
launchBatch sink makeMenuBar startFiles = mdo
  loadedFiles <- newMVar []
  let windowWidth = Width 800
      windowHeight = Height 400
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew windowSize Nothing $ Just "Batch Process"
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  behindTabsColor >>= FL.setColor window
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRange window windowSize
  FL.begin window
  tabs <- FL.tabsNew windowRect Nothing
  (tabSongs, filterParts) <- makeTab windowRect "Songs" $ \rect tab -> do
    homeTabColor >>= setTabColor tab
    let (songsRect, importToggles) = chopBottom 40 rect
    group <- fileLoadWindow songsRect sink "Song" "Songs" (modifyMVar_ loadedFiles) startFiles findSongs $ \imp -> let
      entry = T.concat
        [ fromMaybe "Untitled" $ impTitle imp
        , maybe "" (\art -> " (" <> art <> ")") $ impArtist imp
        ]
      sublines = concat
        [ case impAuthor imp of
          Just author | T.any (not . isSpace) author -> ["Author: " <> author]
          _                                          -> []
        , ["Format: " <> impFormat imp]
        , ["Path: " <> T.pack (impPath imp) <> index]
        ]
      index = case impIndex imp of
        Nothing -> ""
        Just i  -> T.pack $ " (#" <> show i <> ")"
      in (entry, sublines)
    getter <- do
      let subrects = splitHorizN 7 importToggles
          insts =
            [ ("Guitar", Just Guitar)
            , ("Bass", Just Bass)
            , ("Drums", Just Drums)
            , ("Keys", Just Keys)
            , ("Vocals", Just Vocal)
            , ("Other", Nothing)
            ]
      void $ FL.boxNew (head subrects) $ Just "Import:"
      getters <- forM (zip insts $ drop 1 subrects) $ \((txt, inst), subrect) -> do
        btn <- FL.checkButtonNew subrect $ Just txt
        void $ FL.setValue btn True
        return $ do
          b <- FL.getValue btn
          return $ guard b >> Just inst
      return $ \songYaml -> do
        active <- catMaybes <$> sequence getters
        return songYaml
          { _parts = Parts $ flip HM.filterWithKey (getParts $ _parts songYaml)
            $ \part _ -> case part of
              FlexGuitar -> elem (Just Guitar) active
              FlexBass   -> elem (Just Bass  ) active
              FlexDrums  -> elem (Just Drums ) active
              FlexKeys   -> elem (Just Keys  ) active
              FlexVocal  -> elem (Just Vocal ) active
              _          -> elem Nothing       active
          }
    FL.setResizable tab $ Just group
    return (tab, getter)
  let doImport imp fn = do
        proj <- impProject imp
        x <- fn proj
        mapM_ release $ projectRelease proj
        return x
  functionTabs <- sequence
    [ makeTab windowRect "Rock Band 3 (360)" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageRB3 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (targets, yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          forM targets $ \(target, creator) -> do
            case creator of
              RB3CON fout -> do
                tmp <- buildRB3CON target proj'
                stackIO $ Dir.copyFile tmp fout
                return fout
              RB3Magma dout -> do
                tmp <- buildMagmaV2 target proj'
                copyDirRecursive tmp dout
                return dout
      return tab
    , makeTab windowRect "Rock Band 2 (360)" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageRB2 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (targets, yaml) = settings proj
          forM targets $ \(target, fout) -> do
            proj' <- stackIO $ filterParts yaml >>= saveProject proj
            tmp <- buildRB2CON target proj'
            stackIO $ Dir.copyFile tmp fout
            return fout
      return tab
    , makeTab windowRect "Clone Hero/Phase Shift" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPagePS sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts (projectSongYaml proj) >>= saveProject proj
          case creator of
            PSDir dout -> do
              tmp <- buildPSDir target proj'
              copyDirRecursive tmp dout
              return [dout]
            PSZip fout -> do
              tmp <- buildPSZip target proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
      return tab
    , makeTab windowRect "Rock Band 3 (Wii)" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageDolphin sink rect tab $ \dirout midfn preview -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        let task = case filter (not . ("STFS" `T.isInfixOf`) . impFormat) files of
              []   -> runDolphin (map impPath files) midfn preview dirout
              imps -> fatal $ unlines
                $ "Dolphin conversion currently only supports STFS files. The following files should be converted first:"
                : map impPath imps
        startTasks [(".app creation", task)]
      return tab
    , makeTab windowRect "Preview" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPagePreview sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let dout = settings proj
          proj' <- stackIO $ filterParts (projectSongYaml proj) >>= saveProject proj
          tmp <- buildPlayer Nothing proj'
          copyDirRecursive tmp dout
          return [dout]
      return tab
    ]
  let nonTermTabs = tabSongs : functionTabs
  (startTasks, cancelTasks) <- makeTab windowRect "Task" $ \rect tab -> do
    taskColor >>= setTabColor tab
    FL.deactivate tab
    let cbStart = do
          FL.activate tab
          mapM_ FL.deactivate nonTermTabs
          void $ FL.setValue tabs $ Just tab
          updateTabsColor tabs
        cbEnd = do
          mapM_ FL.activate nonTermTabs
    taskOutputPage rect tab sink cbStart cbEnd
  FL.end tabs
  updateTabsColor tabs
  FL.setCallback tabs updateTabsColor
  FL.setResizable tabs $ Just tabSongs
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser cancelTasks
  FL.showWidget window

updateTabsColor :: FL.Ref FL.Tabs -> IO ()
updateTabsColor tabs = FL.getValue tabs >>= \case
  Nothing  -> return ()
  Just tab -> FL.getSelectionColor tab >>= FL.setSelectionColor tabs

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
    -- linux uses URL encoding starting with file://
    -- and then using percent encoding for non-ascii chars
    let removeProtocol s = case T.stripPrefix "file://" s of
          Nothing -> s
          Just s' -> TE.decodeUtf8 $ percentDecode s'
        percentDecode :: T.Text -> B.ByteString
        percentDecode s = case T.uncons s of
          Just ('%', t) -> case readHex $ T.unpack $ T.take 2 t of
            [(n, "")] -> B.cons n $ percentDecode $ T.drop 2 t
            _         -> B.cons 37 {- % -} $ percentDecode t -- shouldn't happen but whatever
          Just (h  , t) -> TE.encodeUtf8 (T.singleton h) <> percentDecode t
          Nothing       -> ""
    () <- f $ map (T.unpack . removeProtocol) $ T.lines str
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
  let newtxt = T.pack $ case pair of
        TermStart x y   -> "\ESC[46m" <> x <> "\ESC[0m" <> ": " <> y
        TermLog str     -> str
        TermWarning msg -> "\ESC[33mWarning\ESC[0m: " <> Exc.displayException msg
        TermError msg   -> "\ESC[41mERROR!\ESC[0m " <> Exc.displayException msg
        TermSuccess str -> "\ESC[42mSuccess!\ESC[0m " <> str
  FL.withRef term $ \ptr ->
    B.useAsCString (TE.encodeUtf8 $ T.strip newtxt <> "\n") $ \cs ->
      c_append ptr cs

-- present in C layer but missing from Haskell
foreign import ccall unsafe "Fl_Simple_Terminal_append"
  c_append :: Ptr () -> CString -> IO ()
-- foreign import ccall safe "Fl_Window_show_with_args" -- must be safe! hangs if unsafe
--   c_show_with_args :: Ptr () -> CInt -> Ptr CString -> IO ()

macOS :: Bool
macOS = os == "darwin"

isNewestRelease :: (Ordering -> IO ()) -> IO ()
isNewestRelease cb = do
  let addr = Req.https "api.github.com" /: "repos" /: "mtolly" /: "onyxite-customs" /: "releases" /: "latest"
  rsp <- Req.runReq Req.defaultHttpConfig $ Req.req Req.GET addr Req.NoReqBody Req.jsonResponse $ Req.header "User-Agent" "mtolly/onyxite-customs"
  case Req.responseBody rsp of
    A.Object obj -> case HM.lookup "name" obj of
      Just (A.String str) -> cb $ case (readMaybe $ T.unpack str, readMaybe $ showVersion version) of
        (Just latest, Just this) -> compare (this :: Integer) latest
        _                        -> if T.unpack str == showVersion version then EQ else LT
      _                   -> return ()
    _            -> return ()

#ifdef WINDOWS
foreign import ccall "&fl_display" fl_display :: Ptr HINSTANCE
#endif

launchGUI :: IO ()
launchGUI = withAL $ \hasAudio -> do
  _ <- FLTK.setScheme "gtk+"
  void FLTK.lock -- this is required to get threads to work properly

  evts <- newTChanIO
  let sink e = do
        atomically $ writeTChan evts e
        FLTK.awake -- this makes waitFor finish so we can process the event

  -- terminal
  let consoleWidth = Width 500
      consoleHeight = Height 400
  termWindow <- FL.windowNew
    (Size consoleWidth consoleHeight)
    Nothing
    (Just "Onyx Console")
  FL.setXclass termWindow "Onyx" -- this sets it as the default
#ifdef WINDOWS
  peek fl_display >>= \disp -> do
    icon <- loadIcon (Just disp) $ intPtrToPtr 1
    FL.setIconRaw termWindow icon
#else
#ifndef MACOSX
  -- linux icon (not working?)
  Right icon <- getResourcesPath "icon.png" >>= FL.pngImageNew . T.pack
  FL.setIcon termWindow $ Just icon
#endif
#endif
  FL.sizeRange termWindow $ Size consoleWidth consoleHeight
  globalLogColor >>= FL.setColor termWindow
  let makeMenuBar width includeConsole = do
        let menuHeight = if macOS then 0 else 30
            menuRect = Rectangle (Position (X 0) (Y 0)) (Size width (Height menuHeight))
            menuFn :: IO () -> FL.Ref FL.MenuItem -> IO ()
            menuFn = const
            menuOptions =
              [ ( "File/Open Song"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'o'
                , Just $ promptLoad sink makeMenuBar hasAudio
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Batch Process"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'b'
                , Just $ launchBatch sink makeMenuBar []
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Tools"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 't'
                , Just $ launchMisc sink makeMenuBar
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              {-
              , ( "File/Live Preview"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'p'
                , Just $ promptPreview sink makeMenuBar
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              -}
              , ( "File/Close Window"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'w'
                , Just $ sink $ EventIO $ FLTK.firstWindow >>= \case
                    -- TODO this sometimes hides the GLWindow inside a song window! maybe only on windows
                    Just window -> FL.doCallback window
                    Nothing     -> return ()
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              ] ++ do
                guard includeConsole
                return
                  ( "View/Show Console"
                  , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.Kb_CtrlState] $ FL.NormalKeyType '`'
                  , Just $ FL.showWidget termWindow
                  , FL.MenuItemFlags [FL.MenuItemNormal]
                  )
        if macOS
          then do
            menu <- FL.sysMenuBarNew menuRect Nothing
            -- TODO need to set window_menu_style to avoid weird slash submenu,
            -- see details in launchWindow
            forM_ menuOptions $ \(a, b, c, d) -> do
              FL.add menu a b (fmap menuFn c) d
          else do
            menu <- FL.menuBarNew menuRect Nothing
            forM_ menuOptions $ \(a, b, c, d) -> do
              FL.add menu a b (fmap menuFn c) d
            FL.setBox menu FLE.EngravedBox
            FLE.rgbColorWithRgb (0x32, 0x30, 0x30) >>= FL.setDownColor menu
        return menuHeight
  menuHeight <- makeMenuBar consoleWidth macOS
  buttonGithub <- FL.buttonNew
    (Rectangle
      (Position (X 10) (Y $ 10 + menuHeight))
      (Size (Width 100) (Height 25))
    )
    (Just "Homepage")
  FL.setCallback buttonGithub $ \_ ->
    osOpenFile "https://github.com/mtolly/onyxite-customs/releases"
  labelLatest <- FL.boxNew
    (Rectangle
      (Position (X 120) (Y $ 10 + menuHeight))
      (Size (Width 370) (Height 25))
    )
    (Just "Checking for updates…")
  _ <- forkIO $ isNewestRelease $ \comp -> sink $ EventIO $ do
    FL.setLabel labelLatest $ case comp of
      EQ -> "You are using the latest version."
      LT -> "New version available!"
      GT -> "Prerelease version"
    when (comp /= EQ) $ FL.setLabelcolor labelLatest FLE.whiteColor
  term <- FL.simpleTerminalNew
    (Rectangle
      (Position (X 10) (Y $ 45 + menuHeight))
      (Size (Width 480) (Height $ 305 - menuHeight))
    )
    Nothing
  FL.setHistoryLines term $ FL.Lines (-1) -- unlimited
  FL.setAnsi term True
  FL.setStayAtBottom term True

  let bottomBar = Rectangle (Position (X 5) (Y 360)) (Size (Width 490) (Height 30))
      [areaOpen, areaBatch, areaMisc] = map (trimClock 0 5 0 5) $ splitHorizN 3 bottomBar
  buttonOpen <- FL.buttonCustom
    areaOpen
    (Just "Load a song")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just $ dragAndDrop (sink . EventOnyx . startLoad makeMenuBar hasAudio) . FL.handleButtonBase . FL.safeCast
      }
  loadSongColor >>= FL.setColor buttonOpen
  FL.setCallback buttonOpen $ \_ -> promptLoad sink makeMenuBar hasAudio
  buttonBatch <- FL.buttonCustom
    areaBatch
    (Just "Batch process")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just $ dragAndDrop (launchBatch sink makeMenuBar) . FL.handleButtonBase . FL.safeCast
      }
  batchProcessColor >>= FL.setColor buttonBatch
  FL.setCallback buttonBatch $ \_ -> launchBatch sink makeMenuBar []
  buttonMisc <- FL.buttonNew
    areaMisc
    (Just "Other tools")
  miscColor >>= FL.setColor buttonMisc
  FL.setCallback buttonMisc $ \_ -> launchMisc sink makeMenuBar
  forM_ [buttonOpen, buttonBatch, buttonMisc] $ \btn ->
    FL.setLabelsize btn $ FL.FontSize 13

  FL.end termWindow
  FL.setResizable termWindow $ Just term
  FL.setCallback termWindow $ windowCloser $ return ()
  FL.showWidget termWindow

  -- support drag and drop onto mac app icon
  void $ openCallback $ Just $
    sink . EventOnyx . startLoad makeMenuBar hasAudio . (: []) . T.unpack

  {-
  -- on linux, supposedly you need to show(argc,argv) for icon to work
  FL.withRef termWindow $ \pwin -> do
    withMany withCString ["onyx"] $ \ps -> do
      withArrayLen ps $ \argc argv -> do
        c_show_with_args pwin (fromIntegral argc) argv
  -}

  let logChan = logIO $ sink . EventMsg
      wait = if macOS
        then FLTK.waitFor 1e20 >> return True
        else fmap (/= 0) FLTK.wait
  -- TODO: catch errors that reach top level,
  -- and close the GUI with a nice error message
  addTerm term $ TermLog $
    "\ESC[45mOnyx\ESC[0m Music Game Toolkit, version " <> showVersion version
  addTerm term $ TermLog "Select an option below to get started."
  void $ runResourceT $ (`runReaderT` sink) $ logChan $ let
    process = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ addTerm term $ toTermMessage pair
          EventFail   msg  -> liftIO $ addTerm term $ TermError msg
          EventIO     act  -> liftIO act
          EventOnyx   act  -> safeOnyx act
        process
    loop = liftIO FLTK.getProgramShouldQuit >>= \case
      True  -> return ()
      False -> liftIO wait >>= \case
        False -> return ()
        True  -> process >> loop
    in do
      unless hasAudio $ warn
        "Couldn't open audio device"
      loop
  FLTK.flush -- dunno if required
