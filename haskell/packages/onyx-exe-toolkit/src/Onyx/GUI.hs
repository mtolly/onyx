{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI (launchGUI) where

import           Onyx.GUI.Batch
import           Onyx.GUI.Core
import           Onyx.GUI.Misc
import           Onyx.GUI.Song

import           Codec.Picture                             (readImage,
                                                            savePngImage,
                                                            writePng)
import           Control.Concurrent                        (MVar, ThreadId,
                                                            forkIO, killThread,
                                                            modifyMVar,
                                                            modifyMVar_,
                                                            newChan, newMVar,
                                                            putMVar, readChan,
                                                            readMVar, takeMVar,
                                                            withMVar, writeChan)
import           Control.Concurrent.Async                  (async,
                                                            waitAnyCancel)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan              (newTChanIO,
                                                            tryReadTChan,
                                                            writeTChan)
import qualified Control.Exception                         as Exc
import           Control.Monad.Catch                       (catchIOError)
import           Control.Monad.Extra                       (forM, forM_, guard,
                                                            unless, void, when,
                                                            (>=>))
import           Control.Monad.IO.Class                    (MonadIO (..),
                                                            liftIO)
import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.Maybe                 (MaybeT (..))
import           Control.Monad.Trans.Reader                (local, mapReaderT,
                                                            runReaderT)
import           Control.Monad.Trans.Resource              (ResourceT, allocate,
                                                            register, release,
                                                            runResourceT)
import           Control.Monad.Trans.Writer                (execWriterT, tell)
import qualified Data.Aeson                                as A
import qualified Data.Aeson.KeyMap                         as KM
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8
import qualified Data.ByteString.Lazy                      as BL
import           Data.Char                                 (isSpace, toLower,
                                                            toUpper)
import           Data.Conduit.Audio                        (integralSample,
                                                            mapSamples)
import qualified Data.Connection                           as Conn
import           Data.Fixed                                (Milli)
import           Data.Foldable                             (toList)
import qualified Data.HashMap.Strict                       as HM
import           Data.Int                                  (Int64)
import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)
import           Data.List.Extra                           (elemIndex, nubOrd)
import qualified Data.List.NonEmpty                        as NE
import qualified Data.Map                                  as Map
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, isJust,
                                                            isNothing,
                                                            listToMaybe,
                                                            mapMaybe)
import           Data.Monoid                               (Endo (..))
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time                                 (getCurrentTime)
import           Data.Time.Clock.System                    (SystemTime (..))
import           Data.Version                              (showVersion)
import           Data.Word                                 (Word32)
import           Foreign                                   (Ptr, alloca,
                                                            freeHaskellFunPtr,
                                                            peek)
import           Foreign.C                                 (CString)
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
import           Graphics.UI.FLTK.LowLevel.X               (openCallback)
import           Network.HTTP.Req                          ((/:))
import qualified Network.HTTP.Req                          as Req
import qualified Network.Socket                            as Socket
import           Numeric                                   (showHex)
import           Onyx.Audio                                (Audio (..),
                                                            buildSource',
                                                            makeXMAFSB3,
                                                            makeXMAPieces,
                                                            runAudio)
import           Onyx.Audio.FSB                            (emitFSB,
                                                            ghBandXMAtoFSB4)
import           Onyx.Audio.VGS                            (writeVGSMultiRate)
import           Onyx.Build                                (NameRule (..),
                                                            validFileName,
                                                            validFileNamePiece)
import           Onyx.Build.RB3CH                          (BasicTiming (..))
import           Onyx.Codec.JSON                           (toJSON,
                                                            yamlEncodeFile)
import           Onyx.CommandLine                          (recursiveChartToMidi)
import           Onyx.FeedBack.Load                        (loadMIDIOrChart,
                                                            loadRawMIDIOrChart)
import           Onyx.Game.Audio                           (AudioHandle (..),
                                                            projectAudio,
                                                            withAL)
import qualified Onyx.Game.Graphics                        as RGGraphics
import           Onyx.Game.Track
import           Onyx.Genre                                (FullGenre (..),
                                                            interpretGenre)
import           Onyx.GUI.Util                             (askFolder)
import           Onyx.Harmonix.Ark.GH2                     (GH2InstallLocation (..))
import qualified Onyx.Harmonix.DTA.Serialize.GH2           as D
import           Onyx.Harmonix.DTA.Serialize.Magma         (Gender (..))
import           Onyx.Harmonix.MOGG                        (oggToMogg)
import           Onyx.Harmonix.RockBand.Score
import           Onyx.Harmonix.RockBand.SongCache          (hardcodeSongCacheIDs)
import           Onyx.Image.DXT                            (readRBImageMaybe)
import           Onyx.Import
import           Onyx.Import.GuitarHero2                   (Setlist (..),
                                                            loadSetlistFull)
import           Onyx.Keys.Ranges                          (closeShiftsFile)
import           Onyx.MIDI.Common                          (RB3Instrument (..))
import qualified Onyx.MIDI.Common                          as RB
import           Onyx.MIDI.Track.File                      (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                      as F
import           Onyx.Preferences                          (MagmaSetting (..),
                                                            Preferences (..),
                                                            TrueDrumLayoutHint (..),
                                                            applyThreads,
                                                            readPreferences,
                                                            savePreferences)
import           Onyx.Project
import           Onyx.QuickConvert
import           Onyx.Reaper.Build                         (TuningInfo (..),
                                                            makeReaper)
import           Onyx.Reductions                           (simpleReduce)
import           Onyx.Resources                            (getResourcesPath)
import           Onyx.StackTrace
import           Onyx.Util.Files                           (commonDir,
                                                            copyDirRecursive,
                                                            osOpenFile,
                                                            osShowFolder)
import           Onyx.Util.Handle                          (saveHandleFolder)
import qualified Onyx.Xbox.STFS                            as STFS
import           Paths_onyx_exe_toolkit                    (version)
import qualified Sound.File.Sndfile                        as Snd
import qualified Sound.MIDI.File.Save                      as Save
import qualified Sound.MIDI.Util                           as U
import qualified System.Directory                          as Dir
import           System.FilePath                           (dropExtension,
                                                            dropTrailingPathSeparator,
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
import           Foreign                                   (intPtrToPtr)
import           Graphics.Win32                            (loadIcon)
import           System.Win32                              (HINSTANCE)
#endif

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

importWithPreferences :: Importable OnyxInner -> Onyx Project
importWithPreferences imp = do
  projInit <- impProject imp
  -- only edit yml and resave if it's a temp-imported project, not one of my song.yml
  if isJust $ projectRelease projInit
    then do
      prefs <- readPreferences
      let applyBlackVenue, applyDecryptSilent, applyDetectMuted
            :: SongYaml FilePath -> SongYaml FilePath
          applyBlackVenue yaml = if prefBlackVenue prefs
            then yaml { global = yaml.global { autogenTheme = Nothing } }
            else yaml
          applyDecryptSilent yaml = yaml
            { plans = flip fmap yaml.plans $ \case
              MoggPlan mogg -> MoggPlan mogg { decryptSilent = prefDecryptSilent prefs }
              plan          -> plan
            }
          applyDetectMuted yaml = yaml
            { parts = flip fmap yaml.parts $ \part -> part
              { grybo = flip fmap part.grybo $ \grybo -> grybo
                { detectMutedOpens = prefDetectMuted prefs
                }
              }
            }
      stackIO $ saveProject projInit $ applyDetectMuted $ applyBlackVenue $ applyDecryptSilent $ projectSongYaml projInit
    else return projInit

continueImport
  :: (Width -> Bool -> IO Int)
  -> Bool
  -> Importable OnyxInner
  -> Onyx ()
continueImport makeMenuBar hasAudio imp = do
  sink <- getEventSink
  -- TODO this can potentially not clean up the temp folder if interrupted during import
  proj <- importWithPreferences imp
  albumArt <- shakeBuild1 proj [] "gen/cover.png"
  -- quick hack to select an audio plan on my projects
  let withPlan k = do
        let planMidi = "gen/plan" </> T.unpack k </> "processed.mid"
        planMidi' <- shakeBuild1 proj [] planMidi
        song <- loadTracks (projectSongYaml proj) $ takeDirectory (projectLocation proj) </> planMidi'
        void $ forkOnyx $ projectAudio k proj >>= withAudio song
      withNoPlan = do
        song <- loadTracks (projectSongYaml proj) $ takeDirectory (projectLocation proj) </> "notes.mid"
        withAudio song Nothing
      withAudio song maybeAudio = stackIO $ sink $ EventOnyx $ do
        prefs <- readPreferences
        let ?preferences = prefs
        stackIO $ launchWindow sink makeMenuBar proj song maybeAudio albumArt
      selectPlan [] = withNoPlan
      selectPlan [k] = withPlan k
      selectPlan (k : ks) = stackIO $ sink $ EventIO $ do
        n <- FL.flChoice "Select an audio plan" (T.pack $ show ks) (Just $ T.pack $ show k) Nothing
        sink $ EventOnyx $ case n of
          1 -> withPlan k
          _ -> selectPlan ks
  if hasAudio
    then selectPlan $ HM.keys (projectSongYaml proj).plans
    else withNoPlan

multipleSongsWindow
  :: (Event -> IO ())
  -> (Width -> Bool -> IO Int)
  -> Bool
  -> [Importable OnyxInner]
  -> IO ()
multipleSongsWindow sink makeMenuBar hasAudio imps = mdo
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

  scroll <- FL.scrolledCustom
    songsArea
    Nothing
    Nothing
    $ Just FL.defaultCustomWidgetFuncs
      { FL.resizeCustom = Just $ \this newRect -> do
        Rectangle pos (Size (Width _) h) <- FL.getRectangle pack
        let newWidth = case rectangleSize newRect of
              Size (Width w') _ -> Width $ w' - barSize
        FL.resize pack $ Rectangle pos $ Size newWidth h
        FL.resizeScrolledBase (FL.safeCast this) newRect
      }
  FL.setBox scroll FLE.EngravedBox
  FL.setType scroll FL.VerticalAlwaysScrollBar
  let barSize = 15
  FL.setScrollbarSize scroll barSize
  let (songsInnerArea, _) = chopRight barSize songsArea
  pack <- FL.packNew songsInnerArea Nothing
  checks <- forM imps $ \imp -> do
    let entry = T.concat
          [ fromMaybe "Untitled" $ impTitle imp
          , maybe "" (\art -> " (" <> art <> ")") $ impArtist imp
          , if imp2x imp then " (2x)" else ""
          ]
        dummyRect = Rectangle (Position (X 0) (Y 0)) (Size (Width 400) (Height 25))
    check <- FL.checkButtonNew dummyRect $ Just entry
    void $ FL.setValue check False
    return check
  FL.end pack
  FL.end scroll
  FLE.rgbColorWithRgb (255,255,255) >>= FL.setColor scroll

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
  FL.setResizable window $ Just scroll
  FL.sizeRange window windowSize
  FL.showWidget window

foreign import ccall unsafe "hs_getTimeMonotonic"
  hs_getTimeMonotonic :: Ptr Int64 -> Ptr Word32 -> IO ()

getTimeMonotonic :: IO SystemTime
getTimeMonotonic = alloca $ \psecs -> alloca $ \pnsecs -> do
  hs_getTimeMonotonic psecs pnsecs
  secs <- peek psecs
  nsecs <- peek pnsecs
  return MkSystemTime
    { systemSeconds     = secs
    , systemNanoseconds = nsecs
    }

startAnimation :: Double -> IO () -> IO (IO ())
startAnimation frameTime redraw = mdo
  stopper <- newIORef False
  let loop fp = do
        freeHaskellFunPtr fp
        readIORef stopper >>= \case
          True  -> return ()
          False -> mdo
            redraw
            fp2 <- FLTK.repeatTimeout frameTime $ loop fp2
            return ()
  fp1 <- FLTK.addTimeout frameTime $ loop fp1
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

launchWindow
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> (Width -> Bool -> IO Int)
  -> Project
  -> PreviewSong
  -> Maybe (Double -> Maybe Double -> Float -> IO AudioHandle)
  -> FilePath
  -> IO ()
launchWindow sink makeMenuBar proj song maybeAudio albumArt = mdo
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew
    windowSize
    Nothing
    (Just $ T.replace "/" "_" $ fromMaybe "Song" (projectSongYaml proj).metadata.title)
    -- if the window title has a slash like "Pupa / Cocoon" this makes weird new menus
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
            void $ FL.setValue input $ fromMaybe "" $ getter (projectSongYaml proj).metadata
            return input
          applyToMetadata :: (Maybe T.Text -> Metadata f -> Metadata f) -> FL.Ref FL.Input -> BuildYamlControl (SongYaml f) ()
          applyToMetadata setter input = tell $ do
            val <- FL.getValue input
            let val' = guard (val /= "") >> Just val
            return $ Endo $ \yaml -> yaml { metadata = setter val' yaml.metadata }
          applyIntToMetadata :: (Maybe Int -> Metadata f -> Metadata f) -> FL.Ref FL.Input -> BuildYamlControl (SongYaml f) ()
          applyIntToMetadata setter input = tell $ do
            val <- FL.getValue input
            let val' = readMaybe $ T.unpack val
            return $ Endo $ \yaml -> yaml { metadata = setter val' yaml.metadata }
          simpleTextGet lbl getter setter rect' = do
            input <- simpleText lbl getter rect'
            applyToMetadata setter input
          simpleTextGetInt lbl getter setter rect' = do
            input <- simpleText' lbl FL.FlIntInput (fmap (T.pack . show) . getter) rect'
            applyIntToMetadata setter input
          simpleCheck lbl getter setter rect' = do
            input <- liftIO $ FL.checkButtonNew rect' $ Just lbl
            liftIO $ void $ FL.setValue input $ getter (projectSongYaml proj).metadata
            liftIO $ FL.setLabelsize input $ FL.FontSize 13
            tell $ do
              b <- FL.getValue input
              return $ Endo $ \yaml -> yaml { metadata = setter b yaml.metadata }
      void $ liftIO $ FL.boxNew (Rectangle (Position (X 0) (Y 0)) (Size (Width 800) (Height 5))) Nothing
      fullWidth $ simpleTextGet "Title" (.title)  $ \mstr meta -> meta { title  = mstr }
      fullWidth $ \rect' -> do
        let (artistArea, trimClock 0 0 0 50 -> yearArea) = chopRight 120 rect'
        simpleTextGet "Artist" (.artist) (\mstr meta -> meta { artist = mstr }) artistArea
        simpleTextGetInt "Year" (.year) (\mint meta -> meta { year = mint }) yearArea
      fullWidth $ \rect' -> do
        let (albumArea, trimClock 0 0 0 30 -> trackNumArea) = chopRight 120 rect'
        simpleTextGet "Album" (.album) (\mstr meta -> meta { album = mstr }) albumArea
        simpleTextGetInt "#" (.trackNumber) (\mint meta -> meta { trackNumber = mint }) trackNumArea
      fullWidth $ simpleTextGet "Author" (.author) $ \mstr meta -> meta { author = mstr }
      (genreInput, subgenreInput) <- fullWidth $ \rect' -> let
        [trimClock 0 50 0 0 -> genreArea, trimClock 0 0 0 50 -> subgenreArea] = splitHorizN 2 rect'
        in do
          genreInput    <- simpleText "Genre"    (.genre   ) genreArea
          subgenreInput <- simpleText "Subgenre" (.subgenre) subgenreArea
          applyToMetadata (\mstr meta -> meta { genre    = mstr }) genreInput
          applyToMetadata (\mstr meta -> meta { subgenre = mstr }) subgenreInput
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
          $ setPNG True (takeDirectory (projectLocation proj) </> albumArt)
          >>= newMVar
        let swapImage f = sink $ EventOnyx $ void $ forkOnyx $ void $ errorToWarning $ do
              inside ("Loading image: " <> f) $ do
                let newPath = takeDirectory (projectLocation proj) </> "new-cover.png"
                if elem (takeExtension f) [".png_xbox", ".png_wii"]
                  then stackIO (BL.readFile f) >>= maybe
                    (fatal "Couldn't read RB image file")
                    (stackIO . writePng newPath)
                    . readRBImageMaybe False
                    -- TODO support .png_ps3
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
              { metadata = yaml.metadata
                { fileAlbumArt = Just path
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
        simpleCheck "Convert"     (.convert   ) (\b meta -> meta { convert    = b }) r1
        simpleCheck "Rhythm Keys" (.rhythmKeys) (\b meta -> meta { rhythmKeys = b }) r2
        simpleCheck "Rhythm Bass" (.rhythmBass) (\b meta -> meta { rhythmBass = b }) r3
        simpleCheck "Auto EMH"    (.catEMH    ) (\b meta -> meta { catEMH     = b }) r4
        simpleCheck "Expert Only" (.expertOnly) (\b meta -> meta { expertOnly = b }) r5
        simpleCheck "Cover"       (.cover     ) (\b meta -> meta { cover      = b }) r6
      padded 5 10 5 10 (Size (Width 500) (Height 30)) $ \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
            editLangs f meta = meta
              { languages = f meta.languages
              }
            lang l = simpleCheck l
              (elem l . (.languages))
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
          { metadata = editLangs (const []) yaml.metadata
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
            Tier i -> setChoice $ max 1 (min tierCount $ fromIntegral i) - 1
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
    getBandDiff <- makeDifficulty root (projectSongYaml proj).metadata.difficulty
    getNewParts <- fmap catMaybes $ forM (HM.toList (projectSongYaml proj).parts.getParts) $ \(fpart, part) ->
      if part == emptyPart
        then return Nothing
        else do
          Just itemInst <- FL.addAt tree (T.toTitle $ F.getPartName fpart) root
          let addType lbl extra = do
                Just itemCheck <- FL.addAt tree "" itemInst
                check <- FL.checkButtonNew dummyRect $ Just lbl
                void $ FL.setValue check True
                FL.setWidget itemCheck $ Just check
                fn <- extra itemCheck
                return $ \curPart -> do
                  isChecked <- FL.getValue check
                  fn isChecked curPart
              makeChoiceFrom :: (Eq a) => [a] -> FL.Ref FL.TreeItem -> a -> (a -> T.Text) -> IO (IO a)
              makeChoiceFrom opts itemParent cur getLabel = do
                Just itemChoice <- FL.addAt tree "" itemParent
                choice <- FL.choiceNew dummyRect Nothing
                forM_ opts $ FL.addName choice . getLabel
                void $ FL.setValue choice $ FL.MenuItemByIndex $ FL.AtIndex $ fromMaybe 0 $ elemIndex cur opts
                FL.setWidget itemChoice $ Just choice
                return $ (\(FL.AtIndex i) -> opts !! i) <$> FL.getValue choice
              makeChoice :: (Eq a, Enum a, Bounded a) => FL.Ref FL.TreeItem -> a -> (a -> T.Text) -> IO (IO a)
              makeChoice = makeChoiceFrom [minBound .. maxBound]
          mbGRYBO <- forM part.grybo $ \pg -> addType "5-Fret" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { grybo = guard isChecked >> Just (pg :: PartGRYBO) { difficulty = diff } }
          mbGHL <- forM part.ghl $ \pg -> addType "6-Fret" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { ghl = guard isChecked >> Just (pg :: PartGHL) { difficulty = diff } }
          mbProKeys <- forM part.proKeys $ \pk -> addType "Pro Keys" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pk.difficulty
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { proKeys = guard isChecked >> Just (pk :: PartProKeys) { difficulty = diff } }
          mbProGuitar <- forM part.proGuitar $ \pg -> addType "Pro Guitar" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return $ \isChecked curPart -> do
              diff <- getDiff
              return curPart { proGuitar = guard isChecked >> Just (pg :: PartProGuitar FilePath) { difficulty = diff } }
          mbDrums <- forM part.drums $ \pd -> addType "Drums" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pd.difficulty
            getMode <- makeChoice itemCheck pd.mode $ \case
              Drums4    -> "4-Lane Drums"
              Drums5    -> "5-Lane Drums"
              DrumsPro  -> "Pro Drums"
              DrumsReal -> "Phase Shift Real Drums"
              DrumsTrue -> "True Drums"
            getKicks <- makeChoice itemCheck pd.kicks $ \case
              Kicks1x   -> "1x Bass Pedal"
              Kicks2x   -> "2x Bass Pedal"
              KicksBoth -> "1x+2x Bass Pedal (PS X+ or C3 format)"
            getKit <- makeChoice itemCheck pd.kit $ \case
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
              return (curPart :: Part FilePath)
                { drums = guard isChecked >> Just pd
                  { difficulty = diff
                  , mode = mode
                  , kicks = kicks
                  , kit = kit
                  }
                }
          mbVocal <- forM part.vocal $ \pv -> addType "Vocals" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pv.difficulty
            getCount <- makeChoice itemCheck pv.count $ \case
              Vocal1 -> "Solo"
              Vocal2 -> "Harmonies (2)"
              Vocal3 -> "Harmonies (3)"
            getGender <- makeChoiceFrom [Nothing, Just Male, Just Female] itemCheck pv.gender $ \case
              Nothing     -> "Unspecified Gender"
              Just Male   -> "Male"
              Just Female -> "Female"
            return $ \isChecked curPart -> do
              diff   <- getDiff
              count  <- getCount
              gender <- getGender
              return (curPart :: Part FilePath)
                { vocal = guard isChecked >> Just pv
                  { difficulty = diff
                  , count      = count
                  , gender     = gender
                  }
                }
          mbMania <- forM part.mania $ \pm -> addType "Mania" $ \_itemCheck -> do
            return $ \isChecked curPart -> do
              return curPart { mania = guard isChecked >> Just pm }
          mbDance <- forM part.dance $ \pd -> addType "Dance" $ \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pd.difficulty
            return $ \isChecked curPart -> do
              diff <- getDiff
              return (curPart :: Part FilePath) { dance = guard isChecked >> Just (pd :: PartDance) { difficulty = diff } }
          return $ Just $ do
            let modifiers = catMaybes [mbGRYBO, mbGHL, mbProKeys, mbProGuitar, mbDrums, mbVocal, mbMania, mbDance]
            newPart <- foldl (>>=) (return part) modifiers
            return (fpart, newPart)
    let editParts = do
          newParts <- sequence getNewParts
          bandDiff <- getBandDiff
          return $ \yaml -> yaml
            { parts    = Parts $ HM.fromList newParts
            , metadata = yaml.metadata { difficulty = bandDiff }
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
        (trimClock 8 5 8 5 -> scrubberArea, trimClock 5 5 5 5 -> speedArea) = chopRight 150 topControlsArea2
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
    filledColor <- FLE.rgbColorWithRgb (13, 131, 216)
    emptyColor <- FLE.rgbColorWithRgb (75, 75, 75)
    scrubberDragResume <- newIORef Nothing
    scrubber <- FL.sliderCustom scrubberArea Nothing
      (Just $ \s -> do
        scrubberRect@(Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))) <- FL.getRectangle s
        FL.flcSetColor emptyColor
        FL.flcRectf scrubberRect
        FL.flcSetColor FLE.blackColor
        FL.flcRect scrubberRect
        -- TODO the following 2 functions might need to be made unsafe c2hs bindings to solve the flickering!
        v <- FL.getValue s
        vmax <- FL.getMaximum s
        let progressWidth = floor $ realToFrac w * (v / vmax)
            progressRect = Rectangle (Position (X x) (Y y)) (Size (Width progressWidth) (Height h))
        when (progressWidth >= 2) $ do
          FL.flcSetColor filledColor
          FL.flcRectf progressRect
          FL.flcSetColor FLE.blackColor
          FL.flcRect progressRect
        -- FL.drawSliderBase $ FL.safeCast s
        let drawSections times end = do
              FL.flcLineStyle
                (FL.LineDrawStyle Nothing Nothing Nothing)
                (Just $ Width 1)
                Nothing
              FL.flcSetColor FLE.whiteColor
              forM_ times $ \t -> let
                x' = x + floor (realToFrac w * (t / end)) - 1
                in when (x' > x) $ FL.flcLine
                  (Position (X x') (Y $ y + 3))
                  (Position (X x') (Y $ y + h - 4))
        case Map.keys $ previewSections song of
          []    -> return ()
          times -> drawSections times $ realToFrac $ U.applyTempoMap (previewTempo song) $ timingEnd $ previewTiming song
      ) $ Just FL.defaultCustomWidgetFuncs
        { FL.handleCustom = Just $ \ref evt -> let
          updateTime = do
            Rectangle (Position (X x) _) (Size (Width w) _) <- FL.getRectangle ref
            X mouseX <- FLTK.eventX
            timeMin <- FL.getMinimum ref
            timeMax <- FL.getMaximum ref
            let newTime = max timeMin $ min timeMax $ timeMin +
                  (timeMax - timeMin) * (fromIntegral (mouseX - x) / fromIntegral w)
            _ <- FL.setValue ref newTime
            return newTime
          in case evt of
            FLE.Push -> do
              -- stop if playing, set position
              secs <- updateTime
              ss <- takeState
              case songPlaying ss of
                Nothing -> writeIORef scrubberDragResume $ Just False
                Just ps -> do
                  writeIORef scrubberDragResume $ Just True
                  void $ stopPlaying (songTime ss) ps
              putState $ SongState secs Nothing
              redrawGL
              FLTK.setFocus ref
              return $ Right ()
            FLE.Drag -> do
              -- set position
              secs <- updateTime
              ss <- takeState
              putState ss { songTime = secs }
              redrawGL
              return $ Right ()
            FLE.Release -> do
              -- set position, resume if was playing
              secs <- updateTime
              ss <- takeState
              ss' <- readIORef scrubberDragResume >>= \case
                Just True -> startPlaying secs
                _         -> return ss { songTime = secs }
              writeIORef scrubberDragResume Nothing
              putState ss'
              redrawGL
              return $ Right ()
            _ -> FL.handleSliderBase (FL.safeCast ref) evt
        }
    FL.setType scrubber FL.HorFillSliderType
    -- FL.setSelectionColor scrubber FLE.cyanColor
    homeTabColor >>= FL.setColor scrubber
    FL.setMinimum scrubber 0
    FL.setMaximum scrubber 100
    FL.setStep scrubber 1
    void $ FL.setValue scrubber 0
    FL.deactivate scrubber
    (getSpeed, counter) <- speedPercent' False speedArea
    FL.end topControls
    FL.setResizable topControls $ Just scrubber
    -- the following used to be done in a thread, to call loadTracks after window launch
    FL.setMaximum scrubber $ fromInteger $ ceiling $ U.applyTempoMap
      (previewTempo song)
      (timingEnd $ previewTiming song)
    FL.activate scrubber
    FL.activate playButton
    -- end separate thread section
    let initState = SongState 0 Nothing
    varState <- newMVar initState
    varTime <- newIORef initState
    (groupGL, redrawGL, deleteGL) <- previewGroup
      sink
      glArea
      (return song)
      (currentSongTime <$> getTimeMonotonic <*> readIORef varTime)
      getSpeed
    FL.setResizable tab $ Just groupGL
    let takeState = takeMVar varState
        putState ss = do
          putMVar varState ss
          writeIORef varTime ss
        stopPlaying :: Double -> Playing -> IO Double
        stopPlaying t ps = do
          stime <- getTimeMonotonic
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
          curTime <- getTimeMonotonic
          stopAnim <- startAnimation (recip $ realToFrac $ prefPreviewFPS ?preferences) $ do
            _ <- FLTK.lock
            t' <- currentSongTime <$> getTimeMonotonic <*> readIORef varTime
            void $ FL.setValue scrubber t'
            redrawGL
            FLTK.unlock
          let ps = Playing
                { playStarted = curTime
                , playSpeed = speed
                , playStopAnimation = stopAnim
                , playAudioHandle = handle
                }
          return SongState { songTime = t, songPlaying = Just ps }
    FL.setCallback scrubber $ \_ -> do
      -- we no longer fire the callback for mouse events, so this is just for keyboard arrows
      secs <- FL.getValue scrubber
      ss <- takeState
      ss' <- case songPlaying ss of
        Nothing -> return ss { songTime = secs }
        Just ps -> do
          _ <- stopPlaying (songTime ss) ps
          startPlaying secs
      putState ss'
      redrawGL
    let togglePlay = readIORef scrubberDragResume >>= \case
          Just _  -> return () -- we are dragging the scrubber, ignore this space press
          Nothing -> do
            ss <- takeState
            ss' <- case songPlaying ss of
              Nothing -> do
                -- should probably rework this, since the square label is not
                -- applied until after we start playing. really we should have
                -- a "loading" label in between
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
          deleteGL -- TODO remove this when we delete the whole song window instead
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
    FL.end pack
    sink $ EventOnyx $ void $ forkOnyx $ do
      let input = takeDirectory (projectLocation proj) </> "notes.mid"
      mid <- F.loadMIDI input
      let fixed = F.onyxToFixed $ F.s_tracks mid
          foundTracksRB3 = getScoreTracks    fixed
          foundTracksGH2 = getScoreTracksGH2 fixed
      -- TODO this is a hack to not hold onto the whole midi file in memory, should find a better way!
      stackIO $ void $ Exc.evaluate $ length $ show (foundTracksRB3, foundTracksGH2)
      stackIO $ sink $ EventIO $ mdo
        FL.begin pack

        let commafy n = T.pack $ reverse $ go $ reverse $ show n
            go (x : y : z : rest@(_ : _))
              = [x, y, z, ','] ++ go rest
            go xs = xs

        -- RB3
        padded 5 10 5 10 (Size (Width 800) (Height 50)) $ \rect' -> do
          void $ FL.boxNew rect' $ Just "Rock Band 3 Star Cutoffs"
        getTracksRB3 <- padded 5 10 5 10 (Size (Width 800) (Height 50)) $ \rect' -> do
          starSelectors rect' foundTracksRB3 updateLabelRB3 scoreTrackName
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
        let updateLabelRB3 = do
              stars <- tracksToStars <$> getTracksRB3
              FL.setLabel l1S $ maybe "" (\n -> "1*: " <> commafy n) $ stars1    stars
              FL.setLabel l2S $ maybe "" (\n -> "2*: " <> commafy n) $ stars2    stars
              FL.setLabel l3S $ maybe "" (\n -> "3*: " <> commafy n) $ stars3    stars
              FL.setLabel l4S $ maybe "" (\n -> "4*: " <> commafy n) $ stars4    stars
              FL.setLabel l5S $ maybe "" (\n -> "5*: " <> commafy n) $ stars5    stars
              FL.setLabel lGS $ maybe "" (\n -> "G*: " <> commafy n) $ starsGold stars
        updateLabelRB3

        -- GH2
        padded 5 10 5 10 (Size (Width 800) (Height 50)) $ \rect' -> do
          void $ FL.boxNew rect' $ Just "Guitar Hero 1/2 Star Cutoffs"
        getTracksGH2 <- padded 5 150 5 150 (Size (Width 800) (Height 50)) $ \rect' -> do
          starSelectors rect' foundTracksGH2 updateLabelGH2 scoreTrackNameGH2
            [ ("Lead", [ScoreGH2Guitar])
            , ("Coop", [ScoreGH2Bass, ScoreGH2Rhythm])
            ]
        [ghBase, ghFour, gh2Five, gh1Five] <- padded 3 10 3 10 (Size (Width 800) (Height 40)) $ \bottomArea -> do
          forM (splitHorizN 4 bottomArea) $ \cell -> do
            cutoffLabel <- FL.boxNew cell Nothing
            FL.setLabelfont cutoffLabel FLE.helveticaBold
            return cutoffLabel
        let updateLabelGH2 = do
              bases <- map (\(_, _, base) -> base) <$> getTracksGH2
              let base = sum bases
                  mult n = floor $ toRational base * n :: Int
              if length bases <= 1
                then do
                  -- single player
                  FL.setLabel ghBase  $ "Base: "       <> commafy base
                  FL.setLabel ghFour  $ "4*: "         <> commafy (mult 2  )
                  FL.setLabel gh2Five $ "5* (GH2): "   <> commafy (mult 2.8)
                  FL.setLabel gh1Five $ "5* (GH1): "   <> commafy (mult 3  )
                else do
                  -- coop
                  FL.setLabel ghBase  $ "Base: " <> commafy base
                  FL.setLabel ghFour  "" -- 4 is the lowest score in coop
                  FL.setLabel gh2Five $ "5* (GH2): " <> commafy (mult 2)
                  FL.setLabel gh1Five ""
        updateLabelGH2

        FL.end pack
        FLTK.redraw
    FL.setResizable tab $ Just pack
    return tab
  rb3Tab <- makeTab windowRect "RB3" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageRB3 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            RB3CON   _ -> "Building RB3 CON (360)"
            RB3PKG   _ -> "Building RB3 PKG (PS3)"
            RB3Magma _ -> "Building Magma project"
          task = case create of
            RB3CON fout -> do
              tmp <- buildRB3CON tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB3PKG fout -> do
              tmp <- buildRB3PKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB3Magma dout -> do
              tmp <- buildMagmaV2 tgt proj'
              copyDirRecursive tmp dout
              return [dout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  rb2Tab <- makeTab windowRect "RB2" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageRB2 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            RB2CON _ -> "Building RB2 CON (360)"
            RB2PKG _ -> "Building RB2 PKG (PS3)"
          task = case create of
            RB2CON fout -> do
              tmp <- buildRB2CON tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB2PKG fout -> do
              tmp <- buildRB2PKG tgt proj'
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
  gh1Tab <- makeTab windowRect "GH1" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageGH1 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            GH1ARK{}    -> "Adding to GH1 ARK file"
            GH1DIYPS2{} -> "Creating GH1 DIY folder (PS2)"
          task = case create of
            GH1ARK fout -> do
              installGH1 tgt proj' fout
              return [fout]
            GH1DIYPS2 fout -> do
              makeGH1DIY tgt proj' fout
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  gh2Tab <- makeTab windowRect "GH2" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageGH2 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            GH2LIVE{}   -> "Building GH2 LIVE file"
            GH2ARK{}    -> "Adding to GH2 ARK file"
            GH2DIYPS2{} -> "Creating GH2 DIY folder (PS2)"
          task = case create of
            GH2LIVE fout    -> do
              tmp <- buildGH2LIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warn "Make sure you combine songs into packs before playing! Loading more than 16 package files will corrupt your GH2 save."
              return [fout]
            GH2ARK fout loc -> case loc of
              GH2AddBonus -> do
                installGH2 tgt proj' fout
                return [fout]
              _ -> fatal "TODO other GH2 destinations"
            GH2DIYPS2 fout -> do
              makeGH2DIY tgt proj' fout
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  gh3Tab <- makeTab windowRect "GH3" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageGH3 sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            GH3LIVE{} -> "Building GH3 LIVE file"
            GH3PKG{}  -> "Building GH3 PKG file"
          task = case create of
            GH3LIVE fout -> do
              tmp <- buildGH3LIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warn "Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
              return [fout]
            GH3PKG fout -> do
              tmp <- buildGH3PKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warn "Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  worTab <- makeTab windowRect "GH:WoR" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    songPageGHWOR sink rect tab proj $ \tgt create -> do
      proj' <- fullProjModify proj
      let name = case create of
            GHWORLIVE{} -> "Building GH:WoR LIVE file"
            GHWORPKG{}  -> "Building GH:WoR PKG file"
          task = case create of
            GHWORLIVE fout -> do
              tmp <- buildGHWORLIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            GHWORPKG fout -> do
              tmp <- buildGHWORPKG tgt proj'
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
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
        FL.setTitle picker "Save web preview folder"
        FL.setPresetFile picker $ T.pack $ projectTemplate proj <> "_player"
        forM_ (prefDirPreview ?preferences) $ FL.setDirectory picker . T.pack
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
  let tabsToDisable = [metaTab, instTab, rb3Tab, rb2Tab, psTab, gh1Tab, gh2Tab, gh3Tab, worTab, utilsTab]
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
    cleanupGL
    mapM_ release $ projectRelease proj
    -- TODO fix final bugs relating to this
    -- modifyMVar_ doesWindowExist $ \_ -> do
    --   FL.destroy window
    --   return False
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

saveProject :: Project -> SongYaml FilePath -> IO Project
saveProject proj song = do
  yamlEncodeFile (projectLocation proj) $ toJSON song
  -- don't need to delete gen folder anymore now that it's a newly created one
  return proj { projectSongYaml = song }

makePS3PackPresetDropdown :: (Event -> IO ()) -> Rectangle -> IO (IO QuickPS3Folder)
makePS3PackPresetDropdown _sink rect = do
  let labelOne = "Combine into one new USRDIR subfolder per pack"
      labelSeparate = "Each song gets a new USRDIR subfolder"
      labelCustomInitial = "Custom USRDIR subfolder..."
      labelCustom txt = "Custom folder: " <> txt
      customPrompt = "Enter custom USRDIR subfolder for your pack. (A number will be appended if creating more than one pack.)"
  menu <- FL.menuButtonNew rect $ Just labelOne
  ref <- newIORef QCOneFolder
  void $ FL.add menu labelOne Nothing
    ((Just $ \_ -> do
      writeIORef ref QCOneFolder
      FL.setLabel menu labelOne
    ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
    (FL.MenuItemFlags [])
  void $ FL.add menu labelSeparate Nothing
    ((Just $ \_ -> do
      writeIORef ref QCSeparateFolders
      FL.setLabel menu labelSeparate
    ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
    (FL.MenuItemFlags [])
  void $ FL.add menu labelCustomInitial Nothing
    ((Just $ \_ -> do
      defValue <- flip fmap (readIORef ref) $ \case
        QCCustomFolder bs -> Just $ TE.decodeUtf8 bs
        _                 -> Nothing
      FL.flInput customPrompt defValue >>= \case
        Nothing  -> return () -- leave as whatever it was
        Just ""  -> return () -- ignore empty input
        Just txt -> do
          writeIORef ref $ QCCustomFolder $ TE.encodeUtf8 txt
          FL.setLabel menu $ labelCustom txt
    ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
    (FL.MenuItemFlags [])
  return $ readIORef ref

makeModeDropdown :: Rectangle -> [(T.Text, a)] -> (a -> IO ()) -> IO ()
makeModeDropdown rect opts withOpt = do
  let (initialLabel, initialOpt) = head opts
  menu <- FL.menuButtonNew rect $ Just initialLabel
  forM_ opts $ \(label, opt) -> do
    -- need to escape in menu items (but not button label) to not make submenus
    let label' = T.replace "/" "\\/" label
    FL.add menu label' Nothing
      ((Just $ \_ -> do
        withOpt opt
        FL.setLabel menu label
      ) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
      (FL.MenuItemFlags [])
  withOpt initialOpt

starSelectors
  :: (Eq track)
  => Rectangle
  -> [(track, RB.Difficulty, base)]
  -> IO ()
  -> (track -> T.Text)
  -> [(T.Text, [track])]
  -> IO (IO [(track, RB.Difficulty, base)])
starSelectors rect foundTracks updateLabel trackToName slots = let
  rects = splitHorizN (length slots) rect
  instSelector ((lbl, trackFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        matchTracks = [ t | t@(strack, _, _) <- foundTracks, elem strack trackFilter ]
    choice <- FL.choiceNew r' $ Just lbl
    FL.setAlign choice $ FLE.Alignments [FLE.AlignTypeTop]
    FL.addName choice "(none)"
    forM_ matchTracks $ \(strack, diff, _) -> FL.addName choice $
      trackToName strack <> " " <> case diff of
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

_selectArkDestination
  :: (Event -> IO ())
  -> FilePath
  -> (GH2InstallLocation -> IO ())
  -> Onyx ()
_selectArkDestination _sink gen withLocation = do
  setlist <- loadSetlistFull gen
  stackIO $ mdo

    let windowWidth = Width 500
        windowHeight = Height 400
        windowSize = Size windowWidth windowHeight
    window <- FL.windowNew
      windowSize
      Nothing
      (Just "Select destination in .ARK")
    let windowRect = Rectangle (Position (X 0) (Y 0)) windowSize

    scroll <- FL.scrolledCustom
      windowRect
      Nothing
      Nothing
      $ Just FL.defaultCustomWidgetFuncs
        { FL.resizeCustom = Just $ \this newRect -> do
          Rectangle pos (Size (Width _) h) <- FL.getRectangle pack
          let newWidth = case rectangleSize newRect of
                Size (Width w') _ -> Width $ w' - barSize
          FL.resize pack $ Rectangle pos $ Size newWidth h
          FL.resizeScrolledBase (FL.safeCast this) newRect
        }
    FL.setBox scroll FLE.EngravedBox
    FL.setType scroll FL.VerticalAlwaysScrollBar
    let barSize = 15
    FL.setScrollbarSize scroll barSize
    let (songsInnerArea, _) = chopRight barSize windowRect
    pack <- FL.packNew songsInnerArea Nothing

    let dummyRect = Rectangle (Position (X 0) (Y 0)) (Size (Width 400) (Height 25))
        songButton (k, pkg) = do
          btn <- FL.buttonNew dummyRect $ Just $ D.name pkg <> " (" <> D.artist pkg <> ")"
          void $ FL.setCallback btn $ \_ -> do
            withLocation $ GH2Replace k
            FL.hide window
        addButton maybeTier = do
          btn <- FL.buttonNew dummyRect $ Just $ T.pack $ case maybeTier of
            Nothing -> "Add to Bonus"
            Just i  -> "Add to Tier " <> show (i + 1)
          void $ FL.setCallback btn $ \_ -> do
            withLocation $ maybe GH2AddBonus GH2AddTier maybeTier
            FL.hide window

    forM_ (zip [0..] setlist.campaign) $ \(tierIndex, (_, songs)) -> do
      _ <- FL.boxNew dummyRect $ Just $ T.pack $ "Tier " <> show (tierIndex + 1 :: Int)
      addButton $ Just tierIndex
      mapM_ songButton songs

    _ <- FL.boxNew dummyRect $ Just "Bonus Songs"
    addButton Nothing
    mapM_ (songButton . fst) setlist.bonus

    FL.end pack
    FL.end scroll
    FLE.rgbColorWithRgb (255,255,255) >>= FL.setColor scroll

    FL.end window
    FL.setResizable window $ Just scroll
    FL.sizeRange window windowSize
    FL.showWidget window

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

miscPagePacks
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPagePacks sink rect tab startTasks = mdo
  loadedSTFS <- newMVar []
  let (filesRect, bottomRect) = chopBottom 140 rect
      (metaRect, startRect) = chopBottom 50 bottomRect
      (trimClock 0 5 5 10 -> gameRect, chopLeft 100 -> (_, nameDescRect)) = chopLeft 250 metaRect
      [trimClock 0 10 5 5 -> nameRect, trimClock 0 10 5 5 -> descRect] = splitVertN 2 nameDescRect
      [chopRight 5 -> (conRect, _), trimClock 0 5 0 5 -> liveRect, chopLeft 5 -> (_, rawRect)]
        = splitHorizN 3 $ trimClock 5 10 10 10 startRect
      modifyButtons :: ([STFSSpec] -> IO [STFSSpec]) -> IO ()
      modifyButtons f = do
        newSTFS <- modifyMVar loadedSTFS $ \stfs -> (\x -> (x, x)) <$> f stfs
        sink $ EventIO $ do
          if null newSTFS
            then mapM_ FL.deactivate [btnCON, btnLIVE, btnRaw]
            else mapM_ FL.activate   [btnCON, btnLIVE, btnRaw]
          FL.setLabel gameBox $ case newSTFS of
            info : _ -> T.unlines
              [ "Game (from first in list):"
              , STFS.md_TitleName $ stfsMeta info
              , "(" <> T.toUpper (T.pack $ showHex (STFS.md_TitleID $ stfsMeta info) "") <> ")"
              ]
            []       -> ""
  group <- fileLoadWindow filesRect sink "CON/LIVE" "CON/LIVE" modifyButtons [] searchSTFS
    $ \info -> let
      entry = T.pack $ stfsPath info
      sublines = concat
        [ take 1 $ STFS.md_DisplayName $ stfsMeta info
        , return $ T.unwords
          [ "Game:"
          , STFS.md_TitleName $ stfsMeta info
          , "(" <> T.toUpper (T.pack $ showHex (STFS.md_TitleID $ stfsMeta info) "") <> ")"
          ]
        ]
      in (entry, sublines)

  let doPrompt output = sink $ EventIO $ do
        stfs <- readMVar loadedSTFS
        picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
        FL.setTitle picker "Save STFS file"
        case stfs of
          f : _ -> FL.setDirectory picker $ T.pack $ takeDirectory $ stfsPath f
          _     -> return ()
        FL.showWidget picker >>= \case
          FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
            Nothing -> return ()
            Just f  -> sink $ EventOnyx $ startTasks $ let
              task = do
                packName <- stackIO $ FL.getValue nameInput
                packDesc <- stackIO $ FL.getValue descInput
                let makeSTFS isLIVE = let
                      applyOpts o = o
                        { STFS.createNames        = [packName]
                        , STFS.createDescriptions = [packDesc]
                        , STFS.createLIVE         = isLIVE
                        }
                      in STFS.makePack (map stfsPath stfs) applyOpts f
                maybeArtDir <- case output of
                  PackCON       -> makeSTFS False
                  PackLIVE      -> makeSTFS True
                  PackExtracted -> do
                    roots <- stackIO $ mapM (STFS.getSTFSFolder . stfsPath) stfs
                    merged <- STFS.packCombineFolders roots
                    stackIO $ saveHandleFolder merged f
                    -- TODO add album_art save? probably check what gh2dx xenia setup expects
                    return Nothing
                return $ f : toList maybeArtDir
              taskLabel = case output of
                PackExtracted -> "Save pack as extracted contents: " <> f
                PackCON       -> "Save pack as CON: " <> f
                PackLIVE      -> "Save pack as LIVE: " <> f
              in [(taskLabel, task)]
          _ -> return ()

  gameBox <- FL.boxNew gameRect Nothing

  nameInput <- liftIO $ FL.inputNew
    nameRect
    (Just "Pack name")
    (Just FL.FlNormalInput)
  descInput <- liftIO $ FL.inputNew
    descRect
    (Just "Pack description")
    (Just FL.FlNormalInput)

  btnCON <- FL.buttonNew conRect $ Just "Make CON pack (RB3/RB2)"
  taskColor >>= FL.setColor btnCON
  FL.setCallback btnCON $ \_ -> doPrompt PackCON

  btnLIVE <- FL.buttonNew liveRect $ Just "Make LIVE pack (all other games)"
  taskColor >>= FL.setColor btnLIVE
  FL.setCallback btnLIVE $ \_ -> doPrompt PackLIVE

  btnRaw <- FL.buttonNew rawRect $ Just "Make extracted folder"
  taskColor >>= FL.setColor btnRaw
  FL.setCallback btnRaw $ \_ -> doPrompt PackExtracted

  FL.setResizable tab $ Just group

data Pack360Output
  = PackCON
  | PackLIVE
  | PackExtracted

data QuickConvertMode
  = QCInPlace
  | QCOneToOne
  | QCMakePacks
  | QCMakeSongs
  | QCDolphin
  deriving (Eq)

pageQuickConvert
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
pageQuickConvert sink rect tab startTasks = mdo
  loadedFiles <- newMVar []
  let (mainRect, processorsRect) = chopRight 200 rect
      (filesRect, trimClock 0 0 5 0 -> bottomRect) = chopBottom 131 mainRect
      (trimClock 5 10 5 10 -> row2, row34) = chopBottom 84 bottomRect
      [row3, row4] = map (trimClock 5 10 5 10) $ splitVertN 2 row34
      computePacks = runMaybeT $ do
        files <- lift $ readMVar loadedFiles
        maxSizeInput <- MaybeT $ readIORef packMaxSizeRef
        maxSizeMB <- MaybeT $ readMaybe . T.unpack <$> FL.getValue maxSizeInput
        let maxSizeBytes = maxSizeMB * 1024 * 1024
        return $ organizePacks maxSizeBytes $ concatMap quickInputSongs files
      updatePackGo = readIORef packGoRef >>= \case
        Nothing -> return ()
        Just go -> computePacks >>= \case
          Nothing -> do
            FL.setLabel go "Invalid pack size"
            FL.deactivate go
          Just [] -> do
            FL.setLabel go "No songs added"
            FL.deactivate go
          Just packs -> do
            FL.setLabel go $ T.pack $ case length packs of
              1 -> "Make 1 pack"
              n -> "Make " <> show n <> " packs"
            FL.activate go
      updateInPlace = readIORef inPlaceGoRef >>= \case
        Nothing -> return ()
        Just go -> do
          files <- readMVar loadedFiles
          let isValidInPlace qinput = case quickInputFormat qinput of
                QCInputTwoWay _ -> True
                _               -> False
          if all isValidInPlace files
            then do
              FL.setLabel go "Start"
              FL.activate go
            else do
              FL.setLabel go "Unsupported format: transform in place supports CON/LIVE/PKG"
              FL.deactivate go
      updateFiles f = do
        modifyMVar_ loadedFiles f
        sink $ EventIO $ updatePackGo >> updateInPlace
  filesGroup <- fileLoadWindow filesRect sink "Rock Band song" "Rock Band songs" updateFiles [] searchQuickSongs
    $ \qinput -> let
      entry = T.pack $ quickInputPath qinput
      subline = T.pack $ case length $ quickInputSongs qinput of
        1 -> "1 song"
        n -> show n <> " songs"
      in (entry, [subline])

  -- right side: processors to transform midis and other files
  packProcessors <- FL.packNew (trimClock 5 5 5 0 processorsRect) Nothing
  let processorBox = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 45))
      procFolder f qsong = do
        newFiles <- f $ quickSongFiles qsong
        return qsong { quickSongFiles = newFiles }
      processors :: [(T.Text, QuickSong -> Onyx QuickSong)]
      processors =
        [ (,) "Black VENUE" $ \qsong -> do
          let isRB3 = qdtaRB3 $ quickSongDTA qsong
          procFolder (applyToMIDI $ blackVenue isRB3) qsong
        , (,) "No Overdrive" $ procFolder $ applyToMIDI noOverdrive
        , (,) "No lanes (G/B/K)" $ procFolder $ applyToMIDI noLanesGBK
        , (,) "No lanes (drums)" $ procFolder $ applyToMIDI noLanesDrums
        , (,) "No drum fills" $ procFolder $ applyToMIDI noDrumFills
        , (,) "Force 22-fret protar" $ procFolder $ applyToMIDI mustang22
        , (,) "Unmute >22-fret protar" $ procFolder $ applyToMIDI unmuteOver22
        , (,) "Decompress .milo_*" $ procFolder decompressMilos
        , (,) "Author to DTA tag" $ return . transferAuthorToTag
        , (,) "Strip title tags" $ return . stripTitleTags
        ]
  processorGetters <- forM processors $ \(label, proc) -> do
    check <- FL.checkButtonNew processorBox $ Just label
    void $ FL.setValue check False
    return $ do
      checked <- FL.getValue check
      return $ if checked then proc else return
  let getMIDITransform = do
        procs <- sequence processorGetters
        return $ foldr (>=>) return procs
  FL.end packProcessors

  -- row2: select mode
  let withQCMode mode = sink $ EventIO $ do
        if mode == QCInPlace   then FL.showWidget inPlace     else FL.hide inPlace
        if mode == QCOneToOne  then FL.showWidget oneToOne    else FL.hide oneToOne
        if mode == QCMakePacks then FL.showWidget makePacks   else FL.hide makePacks
        if mode == QCMakeSongs then FL.showWidget makeSongs   else FL.hide makeSongs
        if mode == QCDolphin   then FL.showWidget makeDolphin else FL.hide makeDolphin
  makeModeDropdown row2
    [ ("Transform in place: replace input files with new versions", QCInPlace  )
    , ("One to one: each input file produces a new output file"   , QCOneToOne )
    , ("Make packs: combine songs up to a maximum file size"      , QCMakePacks)
    , ("Make songs: produce a single file for each song"          , QCMakeSongs)
    , ("Make Dolphin (Wii) pack"                                  , QCDolphin  )
    ] withQCMode

  let quickTemplate :: Preferences -> QuickConvertFormat -> FilePath -> T.Text -> T.Text -> Maybe QuickDTA -> FilePath
      quickTemplate prefs fmt fin ext txt qdta = fixXbox $ validFileName NameRulePC $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
        [ T.intercalate (T.pack $ takeDirectory fin) . T.splitOn "%input_dir%"
        , T.intercalate (validFileNamePiece NameRulePC $ dropExt $ T.pack $ takeFileName fin) . T.splitOn "%input_base%"
        , T.intercalate ext . T.splitOn "%ext%"
        , T.intercalate (validFileNamePiece NameRulePC $ maybe "" qdtaTitle qdta) . T.splitOn "%title%"
        , T.intercalate (validFileNamePiece NameRulePC $ fromMaybe "" $ qdta >>= qdtaArtist) . T.splitOn "%artist%"
        ] where dropExt f = fromMaybe f $ T.stripSuffix ".pkg" f
                fixXbox = case fmt of
                  QCFormatCON  -> trimXbox prefs
                  QCFormatLIVE -> trimXbox prefs
                  QCFormatPKG  -> id

      artistTitle qsong = T.intercalate " - " $ concat
        [ toList $ qdtaArtist $ quickSongDTA qsong
        , [qdtaTitle $ quickSongDTA qsong]
        ]

      defaultTitle qsongs = case qsongs of
        [qsong]   -> artistTitle qsong
        []        -> "(empty)"
        qsong : _ -> T.pack (show $ length qsongs) <> " songs incl. " <> artistTitle qsong

  -- row3/row4: mode-specific stuff
  let (formatArea, trimClock 0 0 0 10 -> ps3OptionsArea) = chopLeft 150 row3
      (ps3EncryptArea, ps3FolderArea) = chopLeft 225 ps3OptionsArea
      makeCheckEncrypt = do
        btn <- FL.checkButtonNew ps3EncryptArea $ Just "Encrypt .mid.edat"
        void $ FL.setValue btn $ prefPS3Encrypt ?preferences
        return $ FL.getValue btn
      makeFormatSelect ps3On ps3Off = do
        ref <- newIORef QCFormatCON
        makeModeDropdown formatArea
          [ ("CON (360)" , QCFormatCON)
          , ("LIVE (360)", QCFormatLIVE)
          , ("PKG (PS3)" , QCFormatPKG)
          ] $ \x -> sink $ EventIO $ do
            writeIORef ref x
            case x of
              QCFormatPKG -> ps3On
              _           -> ps3Off
        return $ readIORef ref
      modeGroup :: IO () -> IO (FL.Ref FL.Group)
      modeGroup inner = do
        group <- FL.groupNew row34 Nothing
        inner
        FL.end group
        return group

  -- Transform in place (each input is replaced with a file, package type preserved)
  --   2nd row: ps3 only: enc/unenc midi select, keep folders / one (new) folder / single-song (new) folders
  --   3rd row: go button
  inPlaceGoRef <- newIORef Nothing
  inPlace <- modeGroup $ do
    _ <- FL.boxNew formatArea $ Just "For PS3 files:"
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ ("Keep original USRDIR subfolders", Nothing)
      , ("Combine into one new USRDIR subfolder", Just QCOneFolder)
      , ("Each song gets a new USRDIR subfolder", Just QCSeparateFolders)
      ]
    btnGo <- FL.buttonNew row4 $ Just "Start"
    taskColor >>= FL.setColor btnGo
    FL.setCallback btnGo $ \_ -> sink $ EventOnyx $ do
      files <- stackIO $ readMVar loadedFiles
      enc <- stackIO getEncrypt
      ps3Folder <- stackIO getFolderSetting
      midiTransform <- stackIO getMIDITransform
      startTasks $ flip map files $ \qinput -> let
        fin = quickInputPath qinput
        qsongs = quickInputSongs qinput
        task = do
          qsongs' <- mapM midiTransform qsongs
          let tmp = fin <> ".tmp"
              isRB3 = any (qdtaRB3 . quickSongDTA) qsongs'
              ps3Settings = QuickPS3Settings
                { qcPS3Folder  = ps3Folder
                , qcPS3Encrypt = enc
                , qcPS3RB3     = isRB3
                }
              xboxSettings live = stackIO $
                (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                -- default values should never be used (source will be stfs already)
                (maybe "Onyx Quick Convert" (T.concat . take 1 . STFS.md_DisplayName       ) $ quickInputXbox qinput)
                (maybe ""                   (T.concat . take 1 . STFS.md_DisplayDescription) $ quickInputXbox qinput)
                live
          case quickInputFormat qinput of
            QCInputTwoWay fmt -> case fmt of
              QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS qsongs' opts tmp
              QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS qsongs' opts tmp
              QCFormatPKG  -> saveQuickSongsPKG  qsongs' ps3Settings tmp
            fmt -> fatal $ "Unsupported format for transform mode (" <> show fmt <> ")"
          stackIO $ Dir.renameFile tmp fin
          return [fin]
        in (fin, task)
    writeIORef inPlaceGoRef $ Just btnGo

  -- One to one (each input file gets one new output file, of a single package type)
  --   2nd row: con/pkg/live, if pkg then (enc/unenc midi select, original/separate/combined songs.dta select)
  --   3rd row: template box with default as %dir%/%base%-convert(.pkg), go button
  oneToOne <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      (FL.activate   ps3Options)
      (FL.deactivate ps3Options)
    ps3Options <- FL.groupNew ps3OptionsArea Nothing
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ ("Keep original USRDIR subfolders (if any)", Nothing)
      , ("Combine into one new USRDIR subfolder per file", Just QCOneFolder)
      , ("Each song gets a new USRDIR subfolder", Just QCSeparateFolders)
      ]
    FL.end ps3Options
    void $ makeTemplateRunner'
      sink
      row4
      "Start"
      "%input_dir%/%input_base%_convert%ext%"
      $ \template -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        fmt <- stackIO getFormat
        enc <- stackIO getEncrypt
        ps3Folder <- stackIO getFolderSetting
        midiTransform <- stackIO getMIDITransform
        newPreferences <- readPreferences
        startTasks $ flip map files $ \qinput -> let
          fin = quickInputPath qinput
          qsongs = quickInputSongs qinput
          task = do
            qsongs' <- mapM midiTransform qsongs
            let ext = case fmt of
                  QCFormatCON  -> ""
                  QCFormatLIVE -> ""
                  QCFormatPKG  -> ".pkg"
                fout = quickTemplate newPreferences fmt fin ext template Nothing
                isRB3 = any (qdtaRB3 . quickSongDTA) qsongs'
                ps3Settings = QuickPS3Settings
                  { qcPS3Folder  = ps3Folder
                  , qcPS3Encrypt = enc
                  , qcPS3RB3     = isRB3
                  }
                xboxSettings live = stackIO $
                  (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                  (maybe (defaultTitle qsongs') (T.concat . take 1 . STFS.md_DisplayName       ) $ quickInputXbox qinput)
                  (maybe ""                     (T.concat . take 1 . STFS.md_DisplayDescription) $ quickInputXbox qinput)
                  live
            case fmt of
              QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS qsongs' opts fout
              QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS qsongs' opts fout
              QCFormatPKG  -> saveQuickSongsPKG qsongs' ps3Settings fout
            return [fout]
          in (fin, task)

  -- Make packs (songs are combined into packs, up to a maximum size)
  --   2nd row: con/pkg/live, if pkg then (enc/unenc midi select, separate/combined songs.dta select)
  --   3rd row: max size, go button (opens a save-as dialog, used as pack name or template for multiple)
  packGoRef <- newIORef Nothing
  packMaxSizeRef <- newIORef Nothing
  makePacks <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      (FL.activate   ps3Options)
      (FL.deactivate ps3Options)
    ps3Options <- FL.groupNew ps3OptionsArea Nothing
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePS3PackPresetDropdown sink ps3FolderArea
    FL.end ps3Options
    let (trimClock 0 10 0 150 -> maxSizeArea, goArea) = chopLeft 300 row4
    maxSizeInput <- FL.inputNew
      maxSizeArea
      (Just "Max Pack Size (MiB)")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize maxSizeInput $ FL.FontSize 13
    FL.setLabeltype maxSizeInput FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign maxSizeInput $ FLE.Alignments [FLE.AlignTypeLeft]
    void $ FL.setValue maxSizeInput "4000"
    FL.setCallback maxSizeInput $ \_ -> updatePackGo
    FL.setWhen maxSizeInput [FLE.WhenChanged]
    writeIORef packMaxSizeRef $ Just maxSizeInput
    btnGo <- FL.buttonNew goArea $ Just "Save pack as…"
    taskColor >>= FL.setColor btnGo
    FL.setCallback btnGo $ \_ -> sink $ EventIO $ do
      fmt <- getFormat
      enc <- getEncrypt
      ps3Folder <- getFolderSetting
      midiTransform <- getMIDITransform
      computePacks >>= \case
        Nothing    -> return () -- shouldn't happen, button is deactivated
        Just packs -> do
          picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
          FL.setTitle picker $ case packs of
            [_] -> "Save pack file"
            _   -> "Select template for packs (a number will be added for each pack)"
          FL.showWidget picker >>= \case
            FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
              Nothing -> return ()
              Just userPath -> let
                packNumber n = case packs of
                  [_] -> ""
                  _   -> "-" <> show n
                packNumberUSRDIR n = case packs of
                  [_] -> ""
                  _   -> B8.pack $ show n
                getOutputPath n = case fmt of
                  QCFormatPKG -> dropExtension userPath <> packNumber n <> ".pkg"
                  _           -> userPath <> packNumber n
                in sink $ EventOnyx $ startTasks $ flip map (zip [1..] packs) $ \(i, qsongs) -> let
                  fout = getOutputPath i
                  task = do
                    mapM_ (lg . T.unpack . artistTitle) qsongs
                    qsongs' <- mapM midiTransform qsongs
                    let isRB3 = any (qdtaRB3 . quickSongDTA) qsongs'
                        ps3Folder' = case ps3Folder of
                          QCCustomFolder bs -> QCCustomFolder $ bs <> packNumberUSRDIR i
                          _                 -> ps3Folder
                        ps3Settings = QuickPS3Settings
                          { qcPS3Folder  = Just ps3Folder'
                          , qcPS3Encrypt = enc
                          , qcPS3RB3     = isRB3
                          }
                        xboxSettings live = stackIO $
                          (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                          (defaultTitle qsongs')
                          ""
                          live
                    case fmt of
                      QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS qsongs' opts fout
                      QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS qsongs' opts fout
                      QCFormatPKG  -> saveQuickSongsPKG qsongs' ps3Settings fout
                    return [fout]
                  in ("Pack #" <> show (i :: Int), task)
            _ -> return ()
    writeIORef packGoRef $ Just btnGo

  -- Make songs (each song gets one new output file)
  --   2nd row: con/pkg/live, if pkg then (enc/unenc midi select)
  --   3rd row: go button (opens pick folder dialog, songs get auto names inside)
  makeSongs <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      (FL.activate   ps3Options)
      (FL.deactivate ps3Options)
    ps3Options <- FL.groupNew ps3OptionsArea Nothing
    getEncrypt <- makeCheckEncrypt
    FL.end ps3Options
    void $ makeTemplateRunner'
      sink
      row4
      "Select folder…"
      "%artist% - %title%%ext%"
      $ \template -> sink $ EventIO $ do
        files <- readMVar loadedFiles
        fmt <- getFormat
        enc <- getEncrypt
        midiTransform <- getMIDITransform
        askFolder (takeDirectory . quickInputPath <$> listToMaybe files) $ \dout -> do
          let inputSongs = files >>= \f -> map (f,) (quickInputSongs f)
          sink $ EventOnyx $ do
            newPreferences <- readPreferences
            startTasks $ flip map inputSongs $ \(qinput, qsong) -> let
              ext = case fmt of
                QCFormatCON  -> ""
                QCFormatLIVE -> ""
                QCFormatPKG  -> ".pkg"
              fout = dout </> quickTemplate newPreferences fmt (quickInputPath qinput) ext template (Just $ quickSongDTA qsong)
              isRB3 = qdtaRB3 $ quickSongDTA qsong
              task = do
                qsong' <- midiTransform qsong
                let ps3Settings = QuickPS3Settings
                      { qcPS3Folder  = Just QCSeparateFolders
                      , qcPS3Encrypt = enc
                      , qcPS3RB3     = isRB3
                      }
                    xboxSettings live = stackIO $
                      (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                      (artistTitle qsong)
                      ""
                      live
                case fmt of
                  QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS [qsong'] opts fout
                  QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS [qsong'] opts fout
                  QCFormatPKG  -> saveQuickSongsPKG [qsong'] ps3Settings fout
                return [fout]
              in (T.unpack $ artistTitle qsong, task)

  -- Wii (Dolphin) pack
  --   2nd row: checkbox for preview audio
  --   3rd row: go button (opens pick folder dialog, app files go inside)
  makeDolphin <- modeGroup $ mdo
    boxPrev <- FL.checkButtonNew row3 $ Just "Try to generate preview audio"
    void $ FL.setValue boxPrev False
    btnGo <- FL.buttonNew row4 $ Just "Select folder…"
    taskColor >>= FL.setColor btnGo
    FL.setCallback btnGo $ \_ -> sink $ EventIO $ do
      files <- readMVar loadedFiles
      tryPreview <- FL.getValue boxPrev
      midiTransform <- getMIDITransform
      askFolder (prefDirWii ?preferences) $ \dout -> sink $ EventOnyx $ startTasks $ let
        settings = QuickDolphinSettings
          { qcDolphinPreview = tryPreview
          }
        task = do
          qsongs <- mapM midiTransform $ concatMap quickInputSongs files
          saveQuickSongsDolphin qsongs settings dout
        in [("Make Dolphin pack", task)]

  FL.setResizable tab $ Just filesGroup

miscPageMOGG
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMOGG sink rect tab startTasks = mdo
  loadedAudio <- newMVar []
  let (filesRect, startRect) = chopBottom 95 rect
      (startRect1, startRect2) = chopBottom 50 startRect
      [chopRight 5 -> (moggRect, _), chopLeft 5 -> (_, vgsRect)] = splitHorizN 2 $ trimClock 5 10 5 10 startRect1
      [chopRight 5 -> (fsb3Rect, _), chopLeft 5 -> (_, fsb4Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect2
      isSingleOgg = \case
        [aud] | audioFormat aud == "Ogg Vorbis" -> Just aud
        _                                       -> Nothing
      modifyAudio :: ([AudioSpec] -> IO [AudioSpec]) -> IO ()
      modifyAudio f = do
        newAudio <- modifyMVar loadedAudio $ \auds -> (\x -> (x, x)) <$> f auds
        let chans = sum $ map audioChannels newAudio
        sink $ EventIO $ do
          FL.setLabel btnMogg $ let
            op = case isSingleOgg newAudio of
              Just _  -> "Ogg to MOGG, no re-encode"
              Nothing -> "Combine into MOGG"
            in op <> " (" <> T.pack (show chans) <> " channels)"
          FL.setLabel btnVgs $ "Combine into VGS (" <> T.pack (show chans) <> " channels)"
          FL.setLabel btnFsb3 $ T.concat
            [ "Make FSB3-XMA1 ("
            , T.pack $ show $ length newAudio
            , " streams, "
            , T.pack $ show chans
            , " channels total)"
            ]
          FL.setLabel btnFsb4 $ T.concat
            [ "Make FSB4-XMA2 ("
            , T.pack $ show chans
            , " channels)"
            ]
          if chans == 0
            then mapM_ FL.deactivate [btnMogg, btnVgs, btnFsb3, btnFsb4]
            else mapM_ FL.activate   [btnMogg, btnVgs, btnFsb3, btnFsb4]
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

  btnMogg <- FL.buttonNew moggRect Nothing
  taskColor >>= FL.setColor btnMogg
  FL.setResizable tab $ Just group
  FL.setCallback btnMogg $ \_ -> sink $ EventIO $ do
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
                case map audioRate audio of
                  []             -> return () -- shouldn't happen
                  rates@(r : rs) -> unless (all (== r) rs) $ do
                    fatal $ "All files must have the same sample rate, but found: " <> show (nubOrd rates)
                ne <- maybe (fatal "No input files") return $ NE.nonEmpty audio
                src <- buildSource' $ Merge $ fmap (Input . audioPath) ne
                runAudio src ogg
                oggToMogg ogg f'
                return [f']
              in [("MOGG file creation", task)]
      _ -> return ()

  btnVgs <- FL.buttonNew vgsRect Nothing
  taskColor >>= FL.setColor btnVgs
  FL.setResizable tab $ Just group
  FL.setCallback btnVgs $ \_ -> sink $ EventIO $ do
    audio <- readMVar loadedAudio
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save VGS file"
    case audio of
      [aud] -> FL.setDirectory picker $ T.pack $ takeDirectory $ audioPath aud
      _     -> return ()
    FL.setFilter picker "*.vgs"
    FL.setPresetFile picker $ case audio of
      [aud] -> T.pack $ takeFileName (audioPath aud) -<.> "vgs"
      _     -> "out.vgs"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> let
          ext = map toLower $ takeExtension f
          f' = if ext == ".vgs"
            then f
            else f <.> "vgs"
          in sink $ EventOnyx $ startTasks $ let
            task = do
              srcs <- mapM (buildSource' . Input . audioPath) audio
              stackIO $ runResourceT $ writeVGSMultiRate f' $ map (mapSamples integralSample) srcs
              return [f']
            in [("VGS file creation", task)]
      _ -> return ()

  btnFsb3 <- FL.buttonNew fsb3Rect Nothing
  taskColor >>= FL.setColor btnFsb3
  FL.setResizable tab $ Just group
  FL.setCallback btnFsb3 $ \_ -> sink $ EventIO $ do
    audio <- readMVar loadedAudio
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save FSB3 file"
    case audio of
      [aud] -> FL.setDirectory picker $ T.pack $ takeDirectory $ audioPath aud
      _     -> return ()
    FL.setFilter picker "*.fsb"
    FL.setPresetFile picker $ case audio of
      [aud] -> T.pack $ takeFileName (audioPath aud) -<.> "fsb"
      _     -> "out.fsb"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> let
          ext = map toLower $ takeExtension f
          f' = if ext == ".fsb"
            then f
            else f <.> "fsb"
          in sink $ EventOnyx $ startTasks $ let
            task = tempDir "makefsb3" $ \tmp -> do
              wavs <- forM (zip [0..] audio) $ \(i, aud) -> do
                src <- buildSource' $ Input $ audioPath aud
                let wav = tmp </> show (i :: Int) <.> "wav"
                runAudio src wav
                return wav
              makeXMAFSB3 [ (B8.pack $ takeFileName wav, wav) | wav <- wavs ] f'
              return [f']
            in [("FSB3 file creation", task)]
      _ -> return ()

  btnFsb4 <- FL.buttonNew fsb4Rect Nothing
  taskColor >>= FL.setColor btnFsb4
  FL.setResizable tab $ Just group
  FL.setCallback btnFsb4 $ \_ -> sink $ EventIO $ do
    audio <- readMVar loadedAudio
    picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
    FL.setTitle picker "Save FSB4 file"
    case audio of
      [aud] -> FL.setDirectory picker $ T.pack $ takeDirectory $ audioPath aud
      _     -> return ()
    FL.setFilter picker "*.fsb"
    FL.setPresetFile picker $ case audio of
      [aud] -> T.pack $ takeFileName (audioPath aud) -<.> "fsb"
      _     -> "out.fsb"
    FL.showWidget picker >>= \case
      FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
        Nothing -> return ()
        Just f  -> let
          ext = map toLower $ takeExtension f
          f' = if ext == ".fsb"
            then f
            else f <.> "fsb"
          in sink $ EventOnyx $ startTasks $ let
            task = tempDir "makefsb4" $ \tmp -> do
              let wav = tmp </> "audio.wav"
              audio' <- maybe (fatal "Panic! No audio files") return $ NE.nonEmpty audio
              src <- buildSource' $ Merge $ fmap (Input . audioPath) audio'
              runAudio src wav
              fsb <- makeXMAPieces (Right wav) >>= ghBandXMAtoFSB4
              stackIO $ BL.writeFile f' $ emitFSB fsb
              return [f']
            in [("FSB4 file creation", task)]
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
    let (_, rectA) = chopLeft 120 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "MIDI or .chart")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ do
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
      FL.setTitle picker "Load MIDI or .chart file"
      FL.setFilter picker "*.{mid,midi,chart}"
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
          mid <- loadMIDIOrChart input
          lg $ T.unpack $ closeShiftsFile mid
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
                makeReaper (TuningInfo [] 0) input input [] f'
                return [f']
              in startTasks [("Make REAPER project: " <> input, task)]
        _ -> return ()
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do

    let (rectLeft, trimClock 0 0 0 10 -> rectRight) = chopRight 150 rect'

    btn1 <- FL.buttonNew rectLeft $ Just "Clean convert .chart to MIDI"
    taskColor >>= FL.setColor btn1
    FL.setCallback btn1 $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
      FL.setTitle picker "Save MIDI file"
      FL.setFilter picker "*.mid"
      FL.setPresetFile picker $ T.pack $ input -<.> ".mid"
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
                mid <- loadRawMIDIOrChart input
                stackIO $ Save.toFile f' $ TE.encodeUtf8 <$> mid
                return [f']
              in startTasks [("Convert to MIDI: " <> input, task)]
        _ -> return ()

    btn2 <- FL.buttonNew rectRight $ Just "Batch"
    taskColor >>= FL.setColor btn2
    FL.setCallback btn2 $ \_ -> sink $ EventIO $ askFolder Nothing $ \dir -> do
      sink $ EventOnyx $ let
        task = do
          results <- recursiveChartToMidi dir
          when (null results) $ lg "No .chart files found."
          return results
        in startTasks [("Convert .chart to MIDI (batch): " <> dir, task)]

  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

miscPageHardcodeSongCache
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageHardcodeSongCache sink rect tab startTasks = do
  pack <- FL.packNew rect Nothing
  pickedFolder <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- FL.inputNew
      inputRect
      (Just "CON folder")
      (Just FL.FlNormalInput) -- required for labels to work
    FL.setLabelsize input $ FL.FontSize 13
    FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
    FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
    pick <- FL.buttonNew pickRect $ Just "@fileopen"
    FL.setCallback pick $ \_ -> sink $ EventIO $ askFolder Nothing $ \f -> do
      void $ FL.setValue input $ T.pack f
    return $ fmap T.unpack $ FL.getValue input
  pickedCache <- padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
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
    btn <- FL.buttonNew rect' $ Just "Apply numeric IDs from song cache"
    taskColor >>= FL.setColor btn
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      dir <- pickedFolder
      cache <- pickedCache
      let cache' = case cache of
            "" -> dir </> "songcache"
            _  -> cache
      sink $ EventOnyx $ let
        task = do
          hardcodeSongCacheIDs cache' dir
          return []
        in startTasks [("Hardcode song cache: " <> dir, task)]
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

launchQuickConvert'
  :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
launchQuickConvert' sink makeMenuBar = sink $ EventOnyx $ do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchQuickConvert sink makeMenuBar

launchQuickConvert :: (?preferences :: Preferences) => (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
launchQuickConvert sink makeMenuBar = mdo
  let windowWidth = Width 1000
      windowHeight = Height 600
      windowSize = Size windowWidth windowHeight
  window <- FL.windowNew windowSize Nothing $ Just "Quick Convert"
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
    [ makeTab windowRect "RB quick convert + pack creator" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      pageQuickConvert sink rect tab startTasks
      return tab
    , makeTab windowRect "GH2 pack creator" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPagePacks sink rect tab startTasks
      return tab
    , makeTab windowRect "RB legacy CON->PKG" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageCONtoPKG sink rect tab startTasks
      return tab
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

launchMisc :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
launchMisc sink makeMenuBar = mdo
  let windowWidth = Width 900
      windowHeight = Height 600
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
    [ makeTab windowRect "MIDI stuff" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMIDI sink rect tab startTasks
      return tab
    , makeTab windowRect "MOGG/VGS/FSB" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMOGG sink rect tab startTasks
      return tab
    , makeTab windowRect "Lipsync" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageLipsync sink rect tab startTasks
      return tab
    , makeTab windowRect "Dry vox" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageDryVox sink rect tab startTasks
      return tab
    , makeTab windowRect ".milo" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageMilo sink rect tab startTasks
      return tab
    , makeTab windowRect "RB3 cache" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageHardcodeSongCache sink rect tab startTasks
      return tab
    , makeTab windowRect "GH3 cache" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageGH3SongCache sink rect tab startTasks
      return tab
    , makeTab windowRect "GH:WoR cache" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageWoRSongCache sink rect tab startTasks
      return tab
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

_watchSong :: IO () -> FilePath -> Onyx (IO PreviewSong, IO ())
_watchSong notify mid = do
  let fakeYaml = undefined -- TODO
  varTrack <- loadTracks fakeYaml mid >>= liftIO . newIORef
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
              loadTracks fakeYaml mid >>= liftIO . writeIORef varTrack
              liftIO notify
            go
    go
  return (readIORef varTrack, sendClose)

_launchTimeServer
  :: (Event -> IO ())
  -> IORef Double
  -> FL.Ref FL.Input
  -> FL.Ref FL.Button
  -> FL.Ref FL.Box
  -> IO (IO ())
_launchTimeServer sink varTime inputPort button label = do
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
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> IO PreviewSong
  -> IO Double
  -> IO Double
  -> IO (FL.Ref FL.Group, IO (), IO ())
previewGroup sink rect getSong getTime getSpeed = do
  let (glArea, bottomControlsArea) = chopBottom 40 rect
      [partSelectHalf, bgSelectHalf] = splitHorizN 2 bottomControlsArea
      partSelectArea = trimClock 6 7 6 14 partSelectHalf
      bgSelectArea   = trimClock 6 14 6 7 bgSelectHalf

  wholeGroup <- FL.groupNew rect Nothing

  bottomControlsGroup <- FL.groupNew bottomControlsArea Nothing
  bottomSizeRef <- FL.boxNew bottomControlsArea Nothing
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
  initSong <- getSong
  updateParts False $ map (map fst) $ previewTracks initSong
  FL.setCallback trackMenu $ \_ -> do
    sink $ EventIO FLTK.redraw
    sink $ EventIO $ void $ FL.popup trackMenu -- reopen menu (TODO find a way to not close it at all)
  bgMenu <- FL.menuButtonNew bgSelectArea $ Just "Background"
  let bgs = previewBG initSong
      initialBG = fmap snd $ listToMaybe bgs
  currentBG <- newIORef initialBG
  let allBGs = ("None", Nothing) : [(t, Just bg) | (t, bg) <- bgs]
  forM_ allBGs $ \(t, bg) -> do
    FL.add bgMenu t Nothing
      ((Just $ \_ -> writeIORef currentBG bg) :: Maybe (FL.Ref FL.MenuItem -> IO ()))
      (FL.MenuItemFlags $ FL.MenuItemRadio : [FL.MenuItemValue | bg == initialBG])
  FL.end bottomControlsGroup
  FL.setResizable bottomControlsGroup $ Just bottomSizeRef

  varStuff <- newMVar GLPreload
  let draw :: FL.Ref FL.GlWindow -> IO ()
      draw wind = do
        mstuff <- modifyMVar varStuff $ \case
          GLPreload -> do
            -- Just get scale factor (both hidpi and fltk's ctrl +/-) once at load.
            -- TODO support changing it after load
            scale <- FL.pixelsPerUnit wind
            embedOnyx sink (RGGraphics.loadGLStuff scale initSong) >>= \case
              Nothing -> return (GLFailed, Nothing)
              Just s  -> return (GLLoaded s, Just s)
          loaded@(GLLoaded s) -> return (loaded, Just s)
          GLFailed -> return (GLFailed, Nothing)
        forM_ mstuff $ \stuff -> do
          t <- getTime
          speed <- getSpeed
          trks <- fmap previewTracks getSong
          bg <- readIORef currentBG
          updateParts True $ map (map fst) trks -- TODO does this need to be done in a sink event
          selected <- selectedNames
          w <- FL.pixelW wind
          h <- FL.pixelH wind
          let flatTrks = concat trks
          RGGraphics.drawTracks stuff (RGGraphics.WindowDims w h) t speed bg (prefTrueLayout ?preferences)
            $ mapMaybe (`lookup` flatTrks) selected
  -- TODO add an option to use `FLTK.setUseHighResGL True`
  -- This appears to always be forced true on hidpi Linux. Not sure of Windows.
  -- But on Mac by default I believe the GL resolution is non-retina unless you set this.
  -- Either way FL.pixelsPerUnit will handle the UI correctly
  FLTK.setUseHighResGL False
  glwindow <- FLGL.glWindowCustom
    (rectangleSize glArea)
    (Just $ rectanglePosition glArea)
    Nothing -- label
    (Just draw)
    FL.defaultCustomWidgetFuncs
    FL.defaultCustomWindowFuncs
  FL.end glwindow
  let deleteGL = do
        withMVar varStuff $ \case
          GLLoaded s -> RGGraphics.stopVideoLoaders s
          _          -> return ()
        sink $ EventIO $ FLTK.deleteWidget glwindow
  FL.setMode glwindow $ FLE.Modes [FLE.ModeOpenGL3, FLE.ModeDepth, FLE.ModeRGB8, FLE.ModeAlpha, FLE.ModeMultisample]

  FL.end wholeGroup
  FL.setResizable wholeGroup $ Just glwindow
  return (wholeGroup, FL.redraw glwindow, deleteGL)

_launchPreview :: (?preferences :: Preferences) => (Event -> IO ()) -> (Width -> Bool -> IO Int) -> FilePath -> Onyx ()
_launchPreview sink makeMenuBar mid = mdo
  (getTracks, stopWatch) <- _watchSong (sink $ EventIO redraw) mid
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
    (groupGL, redrawGL, _deleteGL) <- previewGroup
      sink
      belowTopControls
      getTracks
      (readIORef varTime)
      (return 1)

    stopServer <- _launchTimeServer
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

_promptPreview :: (?preferences :: Preferences) => (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
_promptPreview sink makeMenuBar = sink $ EventIO $ do
  picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
  FL.setTitle picker "Load MIDI, .chart, or REAPER project"
  FL.setFilter picker "*.{mid,midi,RPP,chart}"
  FL.showWidget picker >>= \case
    FL.NativeFileChooserPicked -> FL.getFilename picker >>= \case
      Nothing -> return ()
      Just f  -> sink $ EventOnyx $ _launchPreview sink makeMenuBar $ T.unpack f
    _                          -> return ()

launchBatch'
  :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> [FilePath] -> IO ()
launchBatch' sink makeMenuBar startFiles = sink $ EventOnyx $ do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchBatch sink makeMenuBar startFiles

launchBatch
  :: (?preferences :: Preferences)
  => (Event -> IO ()) -> (Width -> Bool -> IO Int) -> [FilePath] -> IO ()
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
        , if imp2x imp then " (2x)" else ""
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
        return (songYaml :: SongYaml FilePath)
          { parts = Parts $ flip HM.filterWithKey songYaml.parts.getParts
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
  -- keep track of release fns we should run when canceling batch process or closing the window.
  -- otherwise these folders will not be released until onyx closes
  cleanupOnCancel <- newMVar HM.empty
  let doImport imp fn = do
        proj <- importWithPreferences imp
        stackIO $ forM_ (projectRelease proj) $ \k -> do
          modifyMVar_ cleanupOnCancel $ return . HM.insert (projectLocation proj) k
        res <- errorToEither $ fn proj
        stackIO $ forM_ (projectRelease proj) $ \k -> modifyMVar_ cleanupOnCancel $ \m -> do
          release k
          return $ HM.delete (projectLocation proj) m
        either throwNoContext return res
  functionTabs <- sequence
    [ makeTab windowRect "RB3" $ \rect tab -> do
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
              RB3PKG fout -> do
                tmp <- buildRB3PKG target proj'
                stackIO $ Dir.copyFile tmp fout
                return fout
              RB3Magma dout -> do
                tmp <- buildMagmaV2 target proj'
                copyDirRecursive tmp dout
                return dout
      return tab
    , makeTab windowRect "RB2" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageRB2 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (targets, yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          forM targets $ \(target, creator) -> do
            case creator of
              RB2CON fout -> do
                tmp <- buildRB2CON target proj'
                stackIO $ Dir.copyFile tmp fout
                return fout
              RB2PKG fout -> do
                tmp <- buildRB2PKG target proj'
                stackIO $ Dir.copyFile tmp fout
                return fout
      return tab
    , makeTab windowRect "Clone Hero" $ \rect tab -> do
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
    , makeTab windowRect "GH1" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageGH1 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts (projectSongYaml proj) >>= saveProject proj
          case creator of
            GH1ARK fout -> do
              installGH1 target proj' fout
              return [fout]
            GH1DIYPS2 fout -> do
              makeGH1DIY target proj' fout
              return [fout]
      return tab
    , makeTab windowRect "GH2" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageGH2 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts (projectSongYaml proj) >>= saveProject proj
          case creator of
            GH2LIVE fout    -> do
              tmp <- buildGH2LIVE target proj'
              stackIO $ Dir.copyFile tmp fout
              warn "Make sure you combine songs into packs (go to 'Other tools') before playing! Loading more than 16 package files will corrupt your GH2 save."
              return [fout]
            GH2ARK fout loc -> case loc of
              GH2AddBonus -> do
                installGH2 target proj' fout
                return [fout]
              _ -> fatal "TODO other GH2 destinations"
            GH2DIYPS2 fout -> do
              makeGH2DIY target proj' fout
              return [fout]
      return tab
    , makeTab windowRect "GH3" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageGH3 sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts (projectSongYaml proj) >>= saveProject proj
          fout <- case creator of
            GH3LIVE fout -> do
              tmp <- buildGH3LIVE target proj'
              stackIO $ Dir.copyFile tmp fout
              return fout
            GH3PKG fout -> do
              tmp <- buildGH3PKG target proj'
              stackIO $ Dir.copyFile tmp fout
              return fout
          warn "Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
          return [fout]
      return tab
    , makeTab windowRect "GH:WoR" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      batchPageGHWOR sink rect tab $ \settings -> sink $ EventOnyx $ do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map impPath files) $ flip map files $ \f -> doImport f $ \proj -> do
          let ((target, creator), yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          let warnWoR = warn "Make sure you create a WoR Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
          case creator of
            GHWORLIVE fout -> do
              tmp <- buildGHWORLIVE target proj'
              stackIO $ Dir.copyFile tmp fout
              warnWoR
              return [fout]
            GHWORPKG fout -> do
              tmp <- buildGHWORPKG target proj'
              stackIO $ Dir.copyFile tmp fout
              warnWoR
              return [fout]
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
          modifyMVar_ cleanupOnCancel $ \m -> do
            -- putStrLn $ "cleanup on cancel: " <> show (HM.keys m)
            mapM_ release $ HM.elems m
            return HM.empty
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

replaceQueueLog :: ((MessageLevel, Message) -> IO ()) -> Onyx a -> Onyx a
replaceQueueLog q = mapStackTraceT $ QueueLog . local (const q) . fromQueueLog

_localResources :: Onyx a -> Onyx a
_localResources = mapStackTraceT $ QueueLog . mapReaderT (mapReaderT $ liftIO . runResourceT) . fromQueueLog

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
  let newtxt = filterUserError $ T.pack $ case pair of
        TermStart x y   -> "\ESC[46m" <> x <> "\ESC[0m" <> ": " <> y
        TermLog str     -> str
        TermWarning msg -> "\ESC[33mWarning\ESC[0m: " <> Exc.displayException msg
        TermError msg   -> "\ESC[41mERROR!\ESC[0m " <> Exc.displayException msg
        TermSuccess str -> "\ESC[42mSuccess!\ESC[0m " <> str
      -- this comes from the default exception type but it's confusing to users
      filterUserError = T.replace "user error " ""
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
    A.Object obj -> case KM.lookup "name" obj of
      Just (A.String str) -> cb $ case (readMaybe $ T.unpack str, readMaybe $ showVersion version) of
        (Just latest, Just this) -> compare (this :: Integer) latest
        _                        -> if T.unpack str == showVersion version then EQ else LT
      _                   -> return ()
    _            -> return ()

#ifdef WINDOWS
foreign import ccall "&fl_display" fl_display :: Ptr HINSTANCE
#endif

launchPreferences :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> Onyx ()
launchPreferences sink makeMenuBar = do
  loadedPrefs <- readPreferences
  let windowWidth = Width 800
      windowHeight = Height 400
      windowSize = Size windowWidth windowHeight
      leaveLeftLabelSpace = snd . chopLeft 180
      lineBox = padded 5 10 5 10 (Size windowWidth (Height 40))
      bottomHeight = 55
      folderBox rect label initValue = do
        let rectA = leaveLeftLabelSpace rect
            (inputRect, rectB) = chopRight 100 rectA
            (_, rectC) = chopRight 90 rectB
            (browseRect, _) = chopLeft 40 rectC
            (_, rectD) = chopRight 50 rectC
            (_, resetRect) = chopRight 40 rectD
        input <- FL.inputNew
          inputRect
          (Just label)
          (Just FL.FlNormalInput) -- required for labels to work
        FL.setLabelsize input $ FL.FontSize 13
        FL.setLabeltype input FLE.NormalLabelType FL.ResolveImageLabelDoNothing
        FL.setAlign input $ FLE.Alignments [FLE.AlignTypeLeft]
        void $ FL.setValue input initValue
        browseButton <- FL.buttonNew browseRect $ Just "@fileopen"
        FL.setCallback browseButton $ \_ -> sink $ EventIO $ askFolder Nothing $ \dir -> do
          void $ FL.setValue input $ T.pack dir
        resetButton <- FL.buttonNew resetRect $ Just "@undo"
        FL.setCallback resetButton $ \_ -> sink $ EventIO $ do
          void $ FL.setValue input ""
        return $ (\case "" -> Nothing; x -> Just $ T.unpack x) <$> FL.getValue input
  stackIO $ do
    window <- FL.windowNew windowSize Nothing $ Just "Onyx Preferences"
    behindTabsColor >>= FL.setColor window
    menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
    let (tabsRect, bottomRect) = chopBottom bottomHeight $ snd $ chopTop menuHeight $ Rectangle
          (Position (X 0) (Y 0))
          windowSize
    FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
    FL.sizeRange window windowSize
    FL.begin window
    tabs <- FL.tabsNew tabsRect Nothing
    let combinePrefs :: [IO (a -> a)] -> IO (a -> a)
        combinePrefs = fmap (foldr (.) id) . sequence
    (tab1, pref1) <- makeTab tabsRect "General" $ \rect tab -> do
      pack <- FL.packNew rect Nothing
      fn <- combinePrefs <$> sequence
        [ lineBox $ \box -> do
          let [checkBox, _, counterBox, _] = splitHorizN 4 box
          check <- FL.checkButtonNew checkBox $ Just "Unlimited cores"
          void $ FL.setValue check $ isNothing $ prefThreads loadedPrefs
          counter <- FL.counterNew counterBox $ Just "Core limit"
          FL.setLabeltype counter FLE.NormalLabelType FL.ResolveImageLabelDoNothing
          FL.setAlign counter $ FLE.Alignments [FLE.AlignTypeLeft]
          FL.setType counter FL.SimpleCounterType -- only one set of left/right buttons
          FL.setStep counter 1
          FL.setMinimum counter 1
          FL.setMaximum counter 16
          void $ FL.setValue counter $ maybe 1 fromIntegral $ prefThreads loadedPrefs
          let updateCounter = FL.getValue check >>= \case
                True  -> FL.deactivate counter
                False -> FL.activate   counter
          FL.setCallback check $ \_ -> updateCounter
          updateCounter
          return $ do
            unlimited <- FL.getValue check
            cores <- FL.getValue counter
            return $ \prefs -> prefs { prefThreads = if unlimited then Nothing else Just $ round cores }
        , do
          check <- lineBox $ \box -> FL.checkButtonNew box $ Just "For non-open-note games, convert \"muted strums\" to Harmonix style"
          FL.setTooltip check $ T.unwords
            [ "When checked, sequences of the form \"chord, strummed open notes, strummed chord\" are assumed to represent muted strums."
            , "Instead of always becoming green, these open notes will become the lowest note of the following chord,"
            , "or of the preceding chord if it is closer to the open notes."
            ]
          void $ FL.setValue check $ prefDetectMuted loadedPrefs
          return $ (\b prefs -> prefs { prefDetectMuted = b }) <$> FL.getValue check
        ]
      FL.end pack
      return (tab, fn)
    restPrefs <- sequence
      [ makeTab tabsRect "Rock Band" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            getMagma <- lineBox $ \box -> horizRadio box
              [ ("Magma required" , MagmaRequire, prefMagma loadedPrefs == MagmaRequire)
              , ("Magma optional" , MagmaTry    , prefMagma loadedPrefs == MagmaTry    )
              , ("Don't use Magma", MagmaDisable, prefMagma loadedPrefs == MagmaDisable)
              ]
            return $ maybe id (\v prefs -> prefs { prefMagma = v }) <$> getMagma
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Always export black VENUE track"
            void $ FL.setValue check $ prefBlackVenue loadedPrefs
            return $ (\b prefs -> prefs { prefBlackVenue = b }) <$> FL.getValue check
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Label 2x kick charts as (2x Bass Pedal) by default"
            void $ FL.setValue check $ prefLabel2x loadedPrefs
            return $ (\b prefs -> prefs { prefLabel2x = b }) <$> FL.getValue check
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Modify tempos in RB export to follow Magma rules"
            FL.setTooltip check $ T.unwords
              [ "Magma requires MIDI files to use tempos in the range of 40 to 300 BPM."
              , "This is a more restricted range than the games actually require."
              ]
            void $ FL.setValue check $ prefLegalTempos loadedPrefs
            return $ (\b prefs -> prefs { prefLegalTempos = b }) <$> FL.getValue check
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "Guitar Hero" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ lineBox $ \box -> do
            let [_, middleBox, _] = splitHorizN 3 box
            gh2OffsetCounter <- FL.counterNew middleBox $ Just "GH2 Audio Offset (ms)"
            FL.setLabeltype gh2OffsetCounter FLE.NormalLabelType FL.ResolveImageLabelDoNothing
            FL.setAlign gh2OffsetCounter $ FLE.Alignments [FLE.AlignTypeLeft]
            FL.setStep gh2OffsetCounter 1
            FL.setLstep gh2OffsetCounter 10
            FL.setTooltip gh2OffsetCounter $ T.unwords
              [ "Adjust audio offset only for Guitar Hero II output, intended to compensate for emulator delay."
              , "Positive values mean audio will be pulled earlier, to account for delay."
              , "Negative values mean audio will be pushed later."
              ]
            void $ FL.setValue gh2OffsetCounter $ prefGH2Offset loadedPrefs * 1000
            return $ (\ms prefs -> prefs { prefGH2Offset = ms / 1000 }) <$> FL.getValue gh2OffsetCounter
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Sort GH2 bonus songs when adding to .ARK"
            void $ FL.setValue check $ prefSortGH2 loadedPrefs
            return $ (\b prefs -> prefs { prefSortGH2 = b }) <$> FL.getValue check
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "For GH2 and GH3, sort by artist first (instead of title)"
            void $ FL.setValue check $ prefArtistSort loadedPrefs
            FL.setTooltip check $ T.unwords
              [ "This affects GH2 .ARK bonus songs (when the above box is checked)"
              , "and GH3 DLC (as sorted in the Song Cache file)."
              ]
            return $ (\b prefs -> prefs { prefArtistSort = b }) <$> FL.getValue check
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "Clone Hero" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox $ \box -> folderBox box "Default CH folder" $ T.pack $ fromMaybe "" $ prefDirCH loadedPrefs
            return $ (\mdir prefs -> prefs { prefDirCH = mdir }) <$> getDir
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "360" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox $ \box -> folderBox box "Default CON/LIVE folder" $ T.pack $ fromMaybe "" $ prefDirRB loadedPrefs
            return $ (\mdir prefs -> prefs { prefDirRB = mdir }) <$> getDir
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Ensure valid filenames for Xbox 360 USB or HDD drives"
            void $ FL.setValue check $ prefTrimXbox loadedPrefs
            FL.setTooltip check $ T.unwords
              [ "When checked, the filenames of CON or LIVE files will be trimmed to a"
              , "max of 42 characters, and will also have plus and comma characters"
              , "replaced (a restriction for files on a 360 hard drive, but not USB)."
              ]
            return $ (\b prefs -> prefs { prefTrimXbox = b }) <$> FL.getValue check
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Use true number IDs instead of symbols on Xbox 360 RB files"
            void $ FL.setValue check $ prefRBNumberID loadedPrefs
            FL.setTooltip check $ T.unwords
              [ "When checked, Rock Band files will use a random integer for song_id."
              , "Otherwise, an 'o' is prefixed to form a symbol, and the game will"
              , "generate a random ID. A true integer is required for some contexts"
              , "such as RGH online play and PS3 conversion, and also maintains scores"
              , "even if the song cache is regenerated."
              ]
            return $ (\b prefs -> prefs { prefRBNumberID = b }) <$> FL.getValue check
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "PS3" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Encrypt .mid.edat files for PS3"
            void $ FL.setValue check $ prefPS3Encrypt loadedPrefs
            FL.setTooltip check $ T.unwords
              [ "When checked, .mid.edat files for PS3 customs will be encrypted."
              , "This is required for a real console, but optional (and can be less"
              , "convenient) when playing on emulator."
              ]
            return $ (\b prefs -> prefs { prefPS3Encrypt = b }) <$> FL.getValue check
          -- TODO default PKG output folder
          -- TODO folder to install direct into RPCS3 hdd
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "Wii" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox $ \box -> folderBox box "Default Wii .app folder" $ T.pack $ fromMaybe "" $ prefDirWii loadedPrefs
            return $ (\mdir prefs -> prefs { prefDirWii = mdir }) <$> getDir
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "3D Preview" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            getMSAA <- lineBox $ \box -> horizRadio box
              [ ("No MSAA" , Nothing, prefMSAA loadedPrefs == Nothing)
              , ("MSAA 2x" , Just 2 , prefMSAA loadedPrefs == Just 2 )
              , ("MSAA 4x" , Just 4 , prefMSAA loadedPrefs == Just 4 )
              , ("MSAA 8x" , Just 8 , prefMSAA loadedPrefs == Just 8 )
              , ("MSAA 16x", Just 16, prefMSAA loadedPrefs == Just 16)
              ]
            return $ maybe id (\v prefs -> prefs { prefMSAA = v }) <$> getMSAA
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "FXAA"
            void $ FL.setValue check $ prefFXAA loadedPrefs
            return $ (\b prefs -> prefs { prefFXAA = b }) <$> FL.getValue check
          , lineBox $ \box -> do
            let [_, middleBox, _] = splitHorizN 3 box
            fpsCounter <- FL.counterNew middleBox $ Just "Frames per second"
            FL.setLabeltype fpsCounter FLE.NormalLabelType FL.ResolveImageLabelDoNothing
            FL.setAlign fpsCounter $ FLE.Alignments [FLE.AlignTypeLeft]
            FL.setStep fpsCounter 1
            FL.setLstep fpsCounter 10
            FL.setMinimum fpsCounter 1
            void $ FL.setValue fpsCounter $ fromIntegral $ prefPreviewFPS loadedPrefs
            return $ (\n prefs -> prefs { prefPreviewFPS = round n }) <$> FL.getValue fpsCounter
          , lineBox $ \box -> do
            let [trimClock 0 5 0 0 -> box1, trimClock 0 0 0 5 -> box2] = splitHorizN 2 box
                layoutLeftOpenHand = listToMaybe $ flip mapMaybe (prefTrueLayout loadedPrefs) $ \case
                  TDLeftCrossHand -> Just False
                  TDLeftOpenHand  -> Just True
                  _               -> Nothing
                layoutRightNearCrash = listToMaybe $ flip mapMaybe (prefTrueLayout loadedPrefs) $ \case
                  TDRightFarCrash  -> Just False
                  TDRightNearCrash -> Just True
                  _                -> Nothing
            getLeftSide <- makeDropdown box1
              $ ("True drums hihat side: default"   , Nothing   , layoutLeftOpenHand == Nothing   ) NE.:|
              [ ("True drums hihat side: cross hand", Just False, layoutLeftOpenHand == Just False)
              , ("True drums hihat side: open hand" , Just True , layoutLeftOpenHand == Just True )
              ]
            getRightSide <- makeDropdown box2
              $ ("True drums ride side: default"   , Nothing   , layoutRightNearCrash == Nothing   ) NE.:|
              [ ("True drums ride side: far crash" , Just False, layoutRightNearCrash == Just False)
              , ("True drums ride side: near crash", Just True , layoutRightNearCrash == Just True )
              ]
            return $ do
              leftSide  <- getLeftSide
              rightSide <- getRightSide
              return $ \prefs -> prefs
                { prefTrueLayout = concat
                  [ case leftSide of
                    Nothing    -> []
                    Just False -> [TDLeftCrossHand]
                    Just True  -> [TDLeftOpenHand ]
                  , case rightSide of
                    Nothing    -> []
                    Just False -> [TDRightFarCrash ]
                    Just True  -> [TDRightNearCrash]
                  ]
                }
          -- TODO lefty flip
          ]
        FL.end pack
        return fn
      , makeTab tabsRect "Audio" $ \rect _tab -> do
        pack <- FL.packNew rect Nothing
        fn <- combinePrefs <$> sequence
          [ do
            sliderQuality <- lineBox $ \box -> FL.horValueSliderNew (leaveLeftLabelSpace box) $ Just "OGG Vorbis quality"
            FL.setLabelsize sliderQuality $ FL.FontSize 13
            FL.setLabeltype sliderQuality FLE.NormalLabelType FL.ResolveImageLabelDoNothing
            FL.setAlign sliderQuality $ FLE.Alignments [FLE.AlignTypeLeft]
            FL.setMinimum sliderQuality 0
            FL.setMaximum sliderQuality 10
            void $ FL.setValue sliderQuality $ prefOGGQuality loadedPrefs * 10
            return $ (\v prefs -> prefs { prefOGGQuality = v / 10 }) <$> FL.getValue sliderQuality
          , do
            check <- lineBox $ \box -> FL.checkButtonNew box $ Just "Treat encrypted MOGGs as silent instead of an error"
            void $ FL.setValue check $ prefDecryptSilent loadedPrefs
            return $ (\b prefs -> prefs { prefDecryptSilent = b }) <$> FL.getValue check
          ]
        FL.end pack
        return fn
      ]
    FL.end tabs
    FL.setResizable tabs $ Just tab1
    let (bottomLeft, bottomRight) = chopRight 300 bottomRect
        [trimClock 8 4 8 8 -> rectButtonA, trimClock 8 8 8 4 -> rectButtonB] = splitHorizN 2 bottomRight
    bottomArea <- FL.groupNew bottomRect Nothing
    bottomLeftBox <- FL.boxNew bottomLeft Nothing
    FL.setResizable bottomArea $ Just bottomLeftBox
    saveButton <- FL.buttonNew rectButtonA $ Just "Save"
    taskColor >>= FL.setColor saveButton
    FL.setCallback saveButton $ \_ -> do
      newPrefs <- ($ loadedPrefs) <$> combinePrefs (pref1 : restPrefs)
      let magmaWarning = T.unlines
            [ "Warning! Disabling Magma can result in Rock Band files that crash the game,"
            , "as Onyx does not yet check all of the error cases that Magma does."
            , "Please test any charts thoroughly before distributing to others!"
            ]
          continueSave = do
            savePreferences newPrefs
            FL.hide window
      if prefMagma loadedPrefs == MagmaRequire && prefMagma newPrefs /= MagmaRequire
        then FL.flChoice magmaWarning "Cancel" (Just "OK") Nothing >>= \case
          1 -> continueSave
          _ -> return ()
        else continueSave
    cancelButton <- FL.buttonNew rectButtonB $ Just "Cancel"
    FL.setCallback cancelButton $ \_ -> FL.hide window
    FL.end bottomArea
    FL.end window
    FL.setResizable window $ Just tabs
    FL.showWidget window

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
      consoleHeight = Height 440
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
                , Just $ launchBatch' sink makeMenuBar []
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Quick Convert"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'u'
                , Just $ launchQuickConvert' sink makeMenuBar
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
                , Just $ sink $ EventIO $ FLTK.firstWindow >>= let
                    -- Can't reproduce consistently, but sometimes on Windows, firstWindow returns one of
                    -- the preview windows (a non-root GLWindow). So we go up the chain to be sure
                    findRootWindow :: FL.Ref FL.GroupBase -> IO ()
                    findRootWindow w = do
                      parent <- FL.getParent w
                      case parent of
                        Nothing -> FL.doCallback w
                        Just p  -> findRootWindow p
                    in mapM_ $ findRootWindow . FL.safeCast
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "Edit/Preferences"
                , Nothing -- maybe Cmd+, on Mac
                , Just $ sink $ EventOnyx $ launchPreferences sink makeMenuBar
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "Help/Readme"
                , Nothing
                , Just $ sink $ EventIO $ do
                    getResourcesPath "README.html" >>= osOpenFile
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "Help/Update history"
                , Nothing
                , Just $ sink $ EventIO $ do
                    getResourcesPath "CHANGES.txt" >>= osOpenFile
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "Help/License"
                , Nothing
                , Just $ sink $ EventIO $ do
                    getResourcesPath "LICENSE.txt" >>= osOpenFile
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "Help/Credits"
                , Nothing
                , Just $ sink $ EventIO $ do
                    getResourcesPath "CREDITS.txt" >>= osOpenFile
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

  let bottomBar1 = Rectangle (Position (X 5) (Y 360)) (Size (Width 490) (Height 30))
      bottomBar2 = Rectangle (Position (X 5) (Y 400)) (Size (Width 490) (Height 30))
      [areaOpen, areaBatch] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar1
      [areaQuick, areaMisc] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar2

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
      { FL.handleCustom = Just $ dragAndDrop (launchBatch' sink makeMenuBar) . FL.handleButtonBase . FL.safeCast
      }
  batchProcessColor >>= FL.setColor buttonBatch
  FL.setCallback buttonBatch $ \_ -> launchBatch' sink makeMenuBar []

  buttonQuick <- FL.buttonNew
    areaQuick
    (Just "Quick convert/pack")
  quickConvertColor >>= FL.setColor buttonQuick
  FL.setCallback buttonQuick $ \_ -> launchQuickConvert' sink makeMenuBar

  buttonMisc <- FL.buttonNew
    areaMisc
    (Just "Other tools")
  miscColor >>= FL.setColor buttonMisc
  FL.setCallback buttonMisc $ \_ -> launchMisc sink makeMenuBar

  forM_ [buttonOpen, buttonBatch, buttonQuick, buttonMisc] $ \btn ->
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
    -- when we have processed a max number of messages, we want to do a quick
    -- redraw and then continue processing. hopefully awake can be used when
    -- fltk's not in control? documentation is unclear
    process 0               = liftIO FLTK.awake
    process messageCapacity = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ addTerm term $ toTermMessage pair
          EventFail   msg  -> liftIO $ addTerm term $ TermError msg
          EventIO     act  -> liftIO act
          EventOnyx   act  -> safeOnyx act
        process $ messageCapacity - 1
    loop = liftIO FLTK.getProgramShouldQuit >>= \case
      True  -> return ()
      False -> liftIO wait >>= \case
        False -> return ()
        True  -> process maxMessagesAtOnce >> loop
    maxMessagesAtOnce = 20 :: Int
    in do
      readPreferences >>= stackIO . applyThreads
      unless hasAudio $ warn
        "Couldn't open audio device"
      loop
  FLTK.flush -- dunno if required
