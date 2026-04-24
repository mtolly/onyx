{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI (launchGUI) where

import           Codec.Picture                     (readImage, savePngImage,
                                                    writePng)
import           Control.Concurrent                (MVar, ThreadId, forkIO,
                                                    killThread, modifyMVar,
                                                    modifyMVar_, newMVar,
                                                    putMVar, readMVar, takeMVar,
                                                    withMVar)
import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TChan      (newTChanIO, tryReadTChan,
                                                    writeTChan)
import qualified Control.Exception                 as Exc
import           Control.Monad.Extra               (forM, forM_, guard, unless,
                                                    void, when, (>=>))
import           Control.Monad.IO.Class            (MonadIO (..), liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Maybe         (MaybeT (..))
import           Control.Monad.Trans.Reader        (local, mapReaderT,
                                                    runReaderT)
import           Control.Monad.Trans.Resource      (release, runResourceT)
import           Control.Monad.Trans.Writer        (execWriterT, tell)
import qualified Data.Aeson                        as A
import qualified Data.Aeson.KeyMap                 as KM
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as B8
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (isSpace, toLower, toUpper)
import           Data.Coerce                       (coerce)
import           Data.Conduit.Audio                (integralSample, mapSamples)
import           Data.Function                     ((&))
import           Data.Functor                      ((<&>))
import qualified Data.Set                          as Set
import           Onyx.GUI.Batch
import           Onyx.GUI.Core
import           Onyx.GUI.Misc
import           Onyx.GUI.Song
import           Optics                            (mapped, (%), (%~), (.~))
-- import qualified Data.Connection                           as Conn
import           Data.Bits                         ((.&.))
import           Data.Fixed                        (Milli)
import           Data.Foldable                     (toList)
import qualified Data.HashMap.Strict               as HM
import           Data.Int                          (Int64)
import           Data.IORef                        (newIORef, readIORef,
                                                    writeIORef)
import           Data.List.Extra                   (elemIndex, nubOrd)
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust, isNothing,
                                                    listToMaybe, mapMaybe)
import           Data.Monoid                       (Endo (..))
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Data.Time.Clock.System            (SystemTime (..))
import qualified Data.Vector.Storable              as VS
import           Data.Version                      (showVersion)
import           Data.Word                         (Word32)
import           Foreign                           (Ptr, alloca,
                                                    freeHaskellFunPtr, nullPtr,
                                                    peek, toBool, withArray)
import           Foreign.C                         (CDouble, CInt)
import           Network.HTTP.Req                  ((/:))
import qualified Network.HTTP.Req                  as Req
import           Numeric                           (showHex)
import           Onyx.Audio                        (Audio (..), buildSource',
                                                    makeXMAFSB3, makeXMAPieces,
                                                    runAudio)
import           Onyx.Audio.FSB                    (emitFSB, ghBandXMAtoFSB4)
import           Onyx.Audio.VGS                    (writeVGSMultiRate)
import           Onyx.Build                        (NameRule (..),
                                                    validFileName,
                                                    validFileNamePiece)
import           Onyx.Build.RB3CH                  (BasicTiming (..))
import           Onyx.Codec.JSON                   (toJSON, yamlEncodeFile)
import           Onyx.CommandLine                  (recursiveChartToMidi)
import           Onyx.FeedBack.Load                (loadMIDIOrChart,
                                                    loadRawMIDIOrChart)
import           Onyx.Game.Audio                   (AudioHandle (..),
                                                    GainLevels (..),
                                                    ProjectAudio (..),
                                                    projectAudio, withAL)
import qualified Onyx.Game.Graphics                as RGGraphics
import           Onyx.Game.Track
import           Onyx.Genre                        (FullGenre (..),
                                                    interpretGenre)
import           Onyx.GUI.Binding
import           Onyx.GUI.Util                     (askFolder, checkValue,
                                                    peekUTF8, withMaybeUTF8,
                                                    withUTF8)
import           Onyx.Harmonix.Ark.GH2             (GH2InstallLocation (..))
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Harmonix.MOGG                (oggToMoggFiles)
import           Onyx.Harmonix.RockBand.Score
import           Onyx.Harmonix.RockBand.SongCache  (hardcodeSongCacheIDs)
import           Onyx.Image.DXT                    (readRBImageMaybe)
import           Onyx.Import
import           Onyx.Keys.Ranges                  (closeShiftsFile)
import           Onyx.MIDI.Common                  (RB3Instrument (..))
import qualified Onyx.MIDI.Common                  as RB
import qualified Onyx.MIDI.Track.File              as F
import           Onyx.Preferences                  (CHAudioFormat (..),
                                                    EliteDrumLayoutHint (..),
                                                    MagmaSetting (..),
                                                    Preferences (..),
                                                    RBEncoding (..),
                                                    applyThreads,
                                                    getDefaultPS3Dir,
                                                    readPreferences,
                                                    savePreferences)
import           Onyx.Project
import           Onyx.QuickConvert.FretsOnFire     as QCH
import           Onyx.QuickConvert.RockBand        as QRB
import qualified Onyx.QuickConvert.RockRevolution  as RR
import           Onyx.Reaper.Build                 (TuningInfo (..), makeReaper)
import           Onyx.Reductions                   (simpleReduce)
import           Onyx.Resources                    (getResourcesPath)
import           Onyx.StackTrace
import           Onyx.Util.Files                   (commonDir, copyDirRecursive,
                                                    osOpenFile, osShowFolder)
import           Onyx.Util.Handle                  (fileReadable,
                                                    saveHandleFolder)
import qualified Onyx.Xbox.STFS                    as STFS
import           Paths_onyx_exe_toolkit_new        (version)
import qualified Sound.File.Sndfile                as Snd
import qualified Sound.MIDI.File.Save              as Save
import qualified Sound.MIDI.Util                   as U
import qualified System.Directory                  as Dir
import           System.FilePath                   (dropExtension,
                                                    dropTrailingPathSeparator,
                                                    takeDirectory,
                                                    takeExtension, takeFileName,
                                                    (-<.>), (<.>), (</>))
import           System.Info                       (os)
import           System.Random                     (randomIO)
-- import qualified System.IO.Streams                         as Streams
-- import qualified System.IO.Streams.TCP                     as TCP
import           Text.Read                         (readMaybe)

#ifdef WINDOWS
import           Foreign                           (intPtrToPtr)
import           Graphics.Win32                    (loadIcon)
import           System.Win32                      (HINSTANCE)
#endif

{-
import           Control.Concurrent                        (newChan, readChan,
                                                            writeChan)
import           Control.Concurrent.Async                  (async,
                                                            waitAnyCancel)
import           Control.Monad.Catch                       (catchIOError)
import           Control.Monad.Trans.Resource              (ResourceT, allocate,
                                                            register)
import           Data.IORef                                (IORef)
import           Data.Time                                 (getCurrentTime)
import qualified Network.Socket                            as Socket
import qualified System.FSNotify                           as FS
-}

import qualified Language.C.Inline.Cpp             as C

C.context flContext

C.include "FL/Fl.H"
C.include "FL/Fl_Window.H"
C.include "FL/Fl_Double_Window.H"
C.include "FL/Fl_Group.H"
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Check_Button.H"
C.include "FL/Fl_Box.H"
C.include "FL/Fl_Input.H"
C.include "FL/Fl_Pack.H"
C.include "Fl_Simple_Terminal.h"
C.include "FL/Fl_Menu_Bar.H"
C.include "FL/Fl_Sys_Menu_Bar.H"
C.include "FL/Fl_Tabs.H"
C.include "FL/Fl_Native_File_Chooser.H"
C.include "FL/Fl_Gl_Window.H"
C.include "FL/Fl_Hor_Nice_Slider.H"
C.include "FL/Fl_Slider.H"
C.include "FL/Fl_Menu_Button.H"
C.include "FL/Fl_Counter.H"
C.include "FL/Fl_Value_Slider.H"
C.include "FL/Fl_Choice.H"
C.include "FL/Fl_Scroll.H"
C.include "FL/Fl_Tree.H"
C.include "FL/Fl_Tree_Item.H"
C.include "FL/Fl_PNG_Image.H"
C.include "FL/fl_ask.H"
C.include "FL/fl_draw.H"
C.include "hs_gl_window.h"
C.include "hs_scrubber.h"
C.include "hs_dnd_button.h"
C.include "FL/platform.H"

promptLoad :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> Bool -> IO ()
promptLoad sink makeMenuBar hasAudio = do
  picker <- [C.exp| Fl_Native_File_Chooser* {
    new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_MULTI_FILE)
  } |]
  [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Load song"); } |]
  result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
  when (result == 0) do
    n <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->count() } |]
    fs <- forM [0 .. n - 1] \i -> do
      cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename($(int i)) } |]
      peekUTF8 cstr
    sink $ EventOnyx $ startLoad makeMenuBar hasAudio fs
  [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]

startLoad :: (Width -> Bool -> IO CInt) -> Bool -> [FilePath] -> Onyx ()
startLoad makeMenuBar hasAudio fs = do
  sink <- getEventSink
  void $ forkOnyx do
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
  projInit <- imp.impProject
  -- only edit yml and resave if it's a temp-imported project, not one of my song.yml
  if isJust projInit.projectRelease
    then do
      prefs <- readPreferences
      let applyBlackVenue, applyDecryptSilent, applyDetectMuted
            :: SongYaml FilePath -> SongYaml FilePath
          applyBlackVenue = if prefs.prefBlackVenue
            then #global % #autogenTheme .~ Nothing
            else id
          applyDecryptSilent = #plans % mapped %~ \case
            MoggPlan mogg -> MoggPlan mogg { decryptSilent = prefs.prefDecryptSilent }
            plan          -> plan
          applyDetectMuted
            = #parts % mapped % #grybo % mapped % #detectMutedOpens
            .~ prefs.prefDetectMuted
      stackIO $ saveProject projInit $ applyDetectMuted $ applyBlackVenue $ applyDecryptSilent projInit.projectSongYaml
    else return projInit

continueImport
  :: (Width -> Bool -> IO CInt)
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
        song <- loadTracks proj.projectSongYaml $ takeDirectory proj.projectLocation </> planMidi'
        void $ forkOnyx $ projectAudio k proj >>= withAudio song
      withNoPlan = do
        song <- loadTracks proj.projectSongYaml $ takeDirectory proj.projectLocation </> "notes.mid"
        withAudio song Nothing
      withAudio song maybeAudio = stackIO $ sink $ EventOnyx do
        prefs <- readPreferences
        let ?preferences = prefs
        stackIO $ launchWindow sink makeMenuBar proj song maybeAudio albumArt imp.impFormatRB
      selectPlan [] = withNoPlan
      selectPlan [k] = withPlan k
      selectPlan (k : ks) = stackIO $ sink $ EventIO do
        withUTF8 (show ks) \cks -> withUTF8 (show k) \ck -> do
          n <- [C.exp| int { fl_choice("Select an audio plan", $(char* cks), $(char* ck), nullptr) } |]
          sink $ EventOnyx case n of
            1 -> withPlan k
            _ -> selectPlan ks
  if hasAudio
    then selectPlan $ HM.keys proj.projectSongYaml.plans
    else withNoPlan

multipleSongsWindow
  :: (Event -> IO ())
  -> (Width -> Bool -> IO CInt)
  -> Bool
  -> [Importable OnyxInner]
  -> IO ()
multipleSongsWindow sink makeMenuBar hasAudio imps = do
  let windowWidth = Width 500
      windowHeight = Height 400
      windowSize = Size windowWidth windowHeight
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), "Select Songs") } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
      (songsArea, trimClock 10 0 0 0 -> buttonsArea) =
        chopBottom 40 $ trimClock 10 10 10 10 windowRect
      [areaSelect, trimClock 0 0 0 10 -> areaOpen] = splitHorizN 2 buttonsArea

  scroll <- withRect songsArea \sx sy sw sh ->
    [C.exp| Fl_Scroll* { new Fl_Scroll($(int sx), $(int sy), $(int sw), $(int sh)) } |]
  let barSize = 15 :: CInt
  -- NOTE: skipping custom resize handler for scrollbar width adjustment
  [C.block| void {
    Fl_Scroll* s = $(Fl_Scroll* scroll);
    s->box(FL_ENGRAVED_BOX);
    s->type(Fl_Scroll::VERTICAL_ALWAYS);
    s->scrollbar_size($(int barSize));
    s->color(fl_rgb_color(255, 255, 255));
  } |]
  let (songsInnerArea, _) = chopRight (fromIntegral barSize) songsArea
  pack <- withRect songsInnerArea \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  checks <- forM imps \imp -> do
    -- TODO ampersands in the label text here turn into "underline next character".
    -- need to figure out how to escape
    let entry = T.concat
          [ fromMaybe "Untitled" imp.impTitle
          , maybe "" (\art -> " (" <> art <> ")") imp.impArtist
          , if imp.imp2x then " (2x)" else ""
          ]
    check <- [C.exp| Fl_Check_Button* { new Fl_Check_Button(0, 0, 400, 25) } |]
    withUTF8 entry \clabel ->
      [C.block| void {
        $(Fl_Check_Button* check)->copy_label($(char* clabel));
        $(Fl_Check_Button* check)->value(0);
      } |]
    return check
  [C.block| void { $(Fl_Pack* pack)->end(); } |]
  [C.block| void { $(Fl_Scroll* scroll)->end(); } |]

  selectButton <- withRect areaSelect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
  openButton <- withRect areaOpen \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
  let updateButtons = do
        bools <- mapM checkValue checks
        let newValue = not $ and bools
            checked = length $ filter id bools
            selectLabel, openLabel :: T.Text
            selectLabel = if newValue then "Select all" else "Select none"
            openLabel = case checked of
              0 -> "Close"
              1 -> "Open 1 song"
              _ -> "Open " <> T.pack (show checked) <> " songs"
        withUTF8 selectLabel \csel ->
          [C.block| void { $(Fl_Button* selectButton)->copy_label($(char* csel)); } |]
        withUTF8 openLabel \copen ->
          [C.block| void { $(Fl_Button* openButton)->copy_label($(char* copen)); } |]
        let color = if checked == 0 then defaultColor else taskColor
        [C.block| void { $(Fl_Button* openButton)->color($(Fl_Color color)); } |]
        [C.block| void { $(Fl_Button* openButton)->redraw(); } |]
  updateButtons
  cbSelect <- makeCallback \_ _ -> do
    bools <- mapM checkValue checks
    let newValue = if and bools then 0 else 1 :: CInt
    forM_ checks \check ->
      [C.block| void { $(Fl_Check_Button* check)->value($(int newValue)); } |]
    updateButtons
  [C.block| void { $(Fl_Button* selectButton)->callback($(Fl_Callback cbSelect)); } |]
  cbOpen <- makeCallback \_ _ -> do
    bools <- mapM checkValue checks
    forM_ (zip bools imps) \(b, imp) -> when b do
      sink $ EventOnyx $ void $ forkOnyx do
        continueImport makeMenuBar hasAudio imp
    [C.block| void { $(Fl_Window* window)->hide(); } |]
  [C.block| void { $(Fl_Button* openButton)->callback($(Fl_Callback cbOpen)); } |]
  forM_ checks \check -> do
    cbCheck <- makeCallback \_ _ -> updateButtons
    [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]

  let scrollAsGroup = [C.exp| Fl_Group* { (Fl_Group*)$(Fl_Scroll* scroll) } |]
  scrollGroup <- scrollAsGroup
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Group* scrollGroup));
    $(Fl_Window* window)->size_range(500, 400);
    $(Fl_Window* window)->show();
  } |]

foreign import ccall unsafe "hs_getTimeMonotonic"
  hs_getTimeMonotonic :: Ptr Int64 -> Ptr Word32 -> IO ()

getTimeMonotonic :: IO SystemTime
getTimeMonotonic = alloca \psecs -> alloca \pnsecs -> do
  hs_getTimeMonotonic psecs pnsecs
  secs <- peek psecs
  nsecs <- peek pnsecs
  return MkSystemTime
    { systemSeconds     = secs
    , systemNanoseconds = nsecs
    }

-- | Calls an IO action at a regular interval using Fl::add_timeout.
-- Returns an IO action that stops the animation.
startAnimation :: Double -> IO () -> IO (IO ())
startAnimation frameTime redraw = mdo
  stopper <- newIORef False
  let cft = coerce frameTime :: CDouble
      loop _userData = do
        readIORef stopper >>= \case
          True  -> freeHaskellFunPtr fp
          False -> do
            redraw
            [C.block| void {
              Fl::repeat_timeout($(double cft), $(void(*fp)(void*)), nullptr);
            } |]
  fp <- makeTimeoutCallback loop
  [C.block| void {
    Fl::add_timeout($(double cft), $(void(*fp)(void*)), nullptr);
  } |]
  return $ writeIORef stopper True

data Playing = Playing
  { playStarted       :: SystemTime
  , playSpeed         :: Double
  , playStopAnimation :: IO ()
  , playAudioHandle   :: AudioHandle
  }

data SongState = SongState
  { songTime    :: Double
  , songPlaying :: Maybe Playing
  }

currentSongTime :: SystemTime -> SongState -> Double
currentSongTime stime ss = case ss.songPlaying of
  Nothing -> ss.songTime
  Just p  -> ss.songTime + let
    timeToDouble t = realToFrac (systemSeconds t) + realToFrac (systemNanoseconds t) / 1000000000
    in (timeToDouble stime - timeToDouble p.playStarted) * p.playSpeed

launchWindow
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> (Width -> Bool -> IO CInt)
  -> Project
  -> PreviewSong
  -> Maybe ProjectAudio
  -> FilePath
  -> Bool -- True if this song could be loaded by Quick Convert
  -> IO ()
launchWindow sink makeMenuBar proj song maybeAudio albumArt isRB = mdo
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
      title = T.unpack $ T.replace "/" "_" $ fromMaybe "Song" proj.projectSongYaml.metadata.title
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h)) } |]
  withUTF8 title \ctitle ->
    [C.block| void { $(Fl_Window* window)->copy_label($(char* ctitle)); } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
    $(Fl_Window* window)->size_range(800, 500);
  } |]

  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]

  (modifyMeta, _metaTab) <- makeTab windowRect "Metadata" \rect tab -> do
    setTabColor tab homeTabColor
    let (rectSplit, rectRest) = chopTop 200 rect
        (rectLeft, rectRight) = chopRight 200 rectSplit
        (rectResize, _) = chopRight 200 rectRest
    yamlModifier <- fmap (fmap appEndo) $ execWriterT do
      pack <- liftIO $ withRect rectLeft \px py pw ph ->
        [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
      let fullWidth = padded 5 10 5 100 (Size (Width 500) (Height 30))
          simpleText :: T.Text -> CInt -> (Metadata FilePath -> Maybe T.Text) -> Rectangle -> IO (Ptr Fl_Input)
          simpleText lbl inputType getter rect' = withRect rect' \ix iy iw ih -> do
            input <- [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
            let ctype = inputType
            withUTF8 lbl \clabel ->
              [C.block| void {
                Fl_Input* inp = $(Fl_Input* input);
                inp->copy_label($(char* clabel));
                inp->labelsize(13);
                inp->align(FL_ALIGN_LEFT);
                inp->input_type($(int ctype));
              } |]
            let initVal = fromMaybe "" $ getter proj.projectSongYaml.metadata
            withUTF8 initVal \cval ->
              [C.block| void { $(Fl_Input* input)->value($(char* cval)); } |]
            return input
          flNormalInput = [C.pure| int { FL_NORMAL_INPUT } |]
          flIntInput = [C.pure| int { FL_INT_INPUT } |]
          applyToMetadata :: (Maybe T.Text -> Metadata FilePath -> Metadata FilePath) -> Ptr Fl_Input -> BuildYamlControl (SongYaml FilePath) ()
          applyToMetadata setter input = tell do
            val <- peekInputValue input
            let val' = guard (val /= "") >> Just val
            return $ Endo $ #metadata %~ setter val'
          applyIntToMetadata :: (Maybe Int -> Metadata FilePath -> Metadata FilePath) -> Ptr Fl_Input -> BuildYamlControl (SongYaml FilePath) ()
          applyIntToMetadata setter input = tell do
            val <- peekInputValue input
            let val' = readMaybe $ T.unpack val
            return $ Endo $ #metadata %~ setter val'
          simpleTextGet lbl getter setter rect' = do
            input <- liftIO $ simpleText lbl flNormalInput getter rect'
            applyToMetadata setter input
          simpleTextGetInt lbl getter setter rect' = do
            input <- liftIO $ simpleText lbl flIntInput (fmap (T.pack . show) . getter) rect'
            applyIntToMetadata setter input
          simpleCheck
            :: T.Text
            -> (Metadata FilePath -> Bool)
            -> (Bool -> Metadata FilePath -> Metadata FilePath)
            -> Rectangle
            -> BuildYamlControl (SongYaml FilePath) ()
          simpleCheck lbl getter setter rect' = do
            input <- liftIO do
              btn <- withRect rect' \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 lbl \clabel ->
                [C.block| void {
                  $(Fl_Check_Button* btn)->copy_label($(char* clabel));
                  $(Fl_Check_Button* btn)->labelsize(13);
                } |]
              let v = if getter proj.projectSongYaml.metadata then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            tell do
              b <- checkValue input
              return $ Endo $ #metadata %~ setter b
      liftIO do
        _ <- [C.exp| Fl_Box* { new Fl_Box(0, 0, 800, 5) } |]
        return ()
      fullWidth $ simpleTextGet "Title" (.title) (#title .~)
      fullWidth \rect' -> do
        let (artistArea, trimClock 0 0 0 50 -> yearArea) = chopRight 120 rect'
        simpleTextGet "Artist" (.artist) (#artist .~) artistArea
        simpleTextGetInt "Year" (.year) (#year .~) yearArea
      fullWidth \rect' -> do
        let (albumArea, trimClock 0 0 0 30 -> trackNumArea) = chopRight 120 rect'
        simpleTextGet "Album" (.album) (#album .~) albumArea
        simpleTextGetInt "#" (.trackNumber) (#trackNumber .~) trackNumArea
      fullWidth $ simpleTextGet "Author" (.author) (#author .~)
      (genreInput, subgenreInput) <- fullWidth \rect' -> let
        [trimClock 0 50 0 0 -> genreArea, trimClock 0 0 0 50 -> subgenreArea] = splitHorizN 2 rect'
        in do
          genreInput    <- liftIO $ simpleText "Genre"    flNormalInput (.genre   ) genreArea
          subgenreInput <- liftIO $ simpleText "Subgenre" flNormalInput (.subgenre) subgenreArea
          applyToMetadata (#genre .~) genreInput
          applyToMetadata (#subgenre .~) subgenreInput
          return (genreInput, subgenreInput)
      liftIO [C.block| void { $(Fl_Pack* pack)->end(); } |]
      packRight <- liftIO $ withRect rectRight \px py pw ph ->
        [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
      padded 10 10 0 0 (Size (Width 190) (Height 190)) \rect' -> mdo
        cover <- liftIO $ withRect rect' \bx by bw bh ->
          [C.exp| HsDndButton* { new HsDndButton($(int bx), $(int by), $(int bw), $(int bh)) } |]
        let setPNG defaultImage f = do
              png <- withUTF8 f \cpath ->
                [C.exp| Fl_PNG_Image* { new Fl_PNG_Image($(char* cpath)) } |]
              [C.block| void {
                Fl_PNG_Image* img = $(Fl_PNG_Image* png);
                img->scale(180, 180, 0, 1);
                ((Fl_Button*)$(HsDndButton* cover))->image(img);
              } |]
              return (png, guard (not defaultImage) >> Just f)
        currentImage <- liftIO
          $ setPNG True (takeDirectory proj.projectLocation </> albumArt)
          >>= newMVar
        let swapImage f = sink $ EventOnyx $ void $ forkOnyx $ void $ errorToWarning do
              inside ("Loading image: " <> f) do
                let newPath = takeDirectory proj.projectLocation </> "new-cover.png"
                if elem (takeExtension f) [".png_xbox", ".png_wii"]
                  then stackIO (BL.readFile f) >>= maybe
                    (fatal "Couldn't read RB image file")
                    (stackIO . writePng newPath)
                    . readRBImageMaybe False
                    -- TODO support .png_ps3
                  else stackIO (readImage f) >>= either fatal (stackIO . savePngImage newPath)
                stackIO $ sink $ EventIO do
                  oldImage <- modifyMVar currentImage \(oldImage, _) -> do
                    newPair <- setPNG False newPath
                    return (newPair, oldImage)
                  [C.block| void { $(HsDndButton* cover)->redraw(); } |]
                  [C.block| void { delete $(Fl_PNG_Image* oldImage); } |]
        liftIO do
          cbCover <- makeCallback \_ _ -> do
            picker <- [C.exp| Fl_Native_File_Chooser* {
              new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_FILE)
            } |]
            [C.block| void {
              $(Fl_Native_File_Chooser* picker)->title("Select album art");
              $(Fl_Native_File_Chooser* picker)->filter("*.{png,jpg,jpeg,png_xbox,png_wii}");
            } |]
            result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
            when (result == 0) do
              cpath <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
              path <- peekUTF8 cpath
              swapImage path
          [C.block| void { $(HsDndButton* cover)->callback($(Fl_Callback cbCover)); } |]
          setDndButtonCallback cover (mapM_ swapImage . listToMaybe)
        tell do
          (_, mpath) <- readMVar currentImage
          return $ Endo case mpath of
            Nothing   -> id
            Just path -> \yaml -> (yaml :: SongYaml FilePath)
              & #metadata % #fileAlbumArt .~ Just path
      liftIO [C.block| void { $(Fl_Pack* packRight)->end(); } |]
      packBottom <- liftIO $ withRect rectRest \px py pw ph ->
        [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
      padded 10 10 5 10 (Size (Width 500) (Height 30)) \rect' -> liftIO do
        calcGenreBox <- withRect rect' \bx by bw bh ->
          [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
        let calcGenre = do
              g <- peekInputValue genreInput
              s <- peekInputValue subgenreInput
              let FullGenre{..} = interpretGenre
                    (guard (g /= "") >> Just g)
                    (guard (s /= "") >> Just s)
              -- TODO show in-game names, not dta names
              withUTF8 (T.concat
                [ "RB3: ", rbn2Genre, " (", rbn2Subgenre, "), "
                , "RB2: ", rbn1Genre, " (", rbn1Subgenre, ")"
                ]) \clabel ->
                [C.block| void { $(Fl_Box* calcGenreBox)->copy_label($(char* clabel)); } |]
        calcGenre
        cbGenre <- makeCallback \_ _ -> calcGenre
        [C.block| void {
          $(Fl_Input* genreInput)->callback($(Fl_Callback cbGenre));
          $(Fl_Input* genreInput)->when(FL_WHEN_CHANGED);
          $(Fl_Input* subgenreInput)->callback($(Fl_Callback cbGenre));
          $(Fl_Input* subgenreInput)->when(FL_WHEN_CHANGED);
        } |]
      padded 5 10 5 10 (Size (Width 500) (Height 30)) \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
        simpleCheck "Convert"     (.convert   ) (\b meta -> meta { convert    = b }) r1
        simpleCheck "Rhythm Keys" (.rhythmKeys) (\b meta -> meta { rhythmKeys = b }) r2
        simpleCheck "Rhythm Bass" (.rhythmBass) (\b meta -> meta { rhythmBass = b }) r3
        simpleCheck "Auto EMH"    (.catEMH    ) (\b meta -> meta { catEMH     = b }) r4
        simpleCheck "Expert Only" (.expertOnly) (\b meta -> meta { expertOnly = b }) r5
        simpleCheck "Cover"       (.cover     ) (\b meta -> meta { cover      = b }) r6
      padded 5 10 5 10 (Size (Width 500) (Height 30)) \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
            editLangs f meta = meta
              { languages = f meta.languages
              }
            lang l = simpleCheck l
              (elem l . (.languages))
              (\b -> editLangs if b then (l :) else id)
        lang "English" r1
        lang "French" r2
        lang "Italian" r3
        lang "Spanish" r4
        lang "German" r5
        lang "Japanese" r6
        -- language-clear function at the bottom; this makes it happen first
        tell $ return $ Endo \yaml -> (yaml :: SongYaml FilePath)
          & #metadata .~ editLangs (const []) yaml.metadata
      liftIO [C.block| void { $(Fl_Pack* packBottom)->end(); } |]
      resizeBox <- liftIO $ withRect rectResize \bx by bw bh ->
        [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
      liftIO [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Box* resizeBox)); } |]
    return (yamlModifier, tab)

  (modifyInsts, _instTab) <- makeTab windowRect "Instruments" \rect tab -> do
    setTabColor tab homeTabColor
    let instRect = trimClock 10 10 10 10 rect
    tree <- withRect instRect \tx ty tw th ->
      [C.exp| Fl_Tree* { new Fl_Tree($(int tx), $(int ty), $(int tw), $(int th)) } |]
    [C.block| void {
      Fl_Tree* t = $(Fl_Tree* tree);
      t->end();
      t->root_label("Instruments");
      t->selectmode(FL_TREE_SELECT_NONE);
      t->showcollapse(0);
    } |]
    root <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->root() } |]
    let dummyRect = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 100))
        makeDifficulty :: Ptr Fl_Tree_Item -> Difficulty -> IO (IO Difficulty)
        makeDifficulty itemParent diff = do
          itemGroup <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->add($(Fl_Tree_Item* itemParent), "") } |]
          group <- withRect dummyRect \gx gy gw gh ->
            [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
          let (choiceArea, textArea) = chopLeft 100 dummyRect
          choice <- withRect choiceArea \cx cy cw ch ->
            [C.exp| Fl_Choice* { new Fl_Choice($(int cx), $(int cy), $(int cw), $(int ch)) } |]
          let tierCount = 7 :: CInt
          forM_ [1 .. tierCount] \i -> do
            let label = "Tier " <> show i
            withUTF8 label \clabel ->
              [C.block| void { $(Fl_Choice* choice)->add($(char* clabel)); } |]
          [C.block| void { $(Fl_Choice* choice)->add("Rank"); } |]
          input <- withRect textArea \ix iy iw ih ->
            [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
          let flNormalInput = [C.pure| int { FL_NORMAL_INPUT } |]
          [C.block| void { $(Fl_Input* input)->input_type($(int flNormalInput)); } |]
          let setChoice i = [C.block| void { $(Fl_Choice* choice)->value($(int i)); } |]
              controlRank = do
                i <- [C.exp| int { $(Fl_Choice* choice)->value() } |]
                if i == tierCount
                  then [C.block| void { $(Fl_Input* input)->activate(); } |]
                  else [C.block| void { $(Fl_Input* input)->deactivate(); } |]
          case diff of
            Tier i -> setChoice $ max 0 (min (tierCount - 1) (fromIntegral i - 1))
            Rank r -> do
              setChoice tierCount
              withUTF8 (show r) \cval ->
                [C.block| void { $(Fl_Input* input)->value($(char* cval)); } |]
          controlRank
          cbChoice <- makeCallback \_ _ -> controlRank
          [C.block| void { $(Fl_Choice* choice)->callback($(Fl_Callback cbChoice)); } |]
          [C.block| void { $(Fl_Group* group)->end(); } |]
          let groupAsWidget = [C.exp| Fl_Widget* { (Fl_Widget*)$(Fl_Group* group) } |]
          groupWidget <- groupAsWidget
          [C.block| void { $(Fl_Tree_Item* itemGroup)->widget($(Fl_Widget* groupWidget)); } |]
          return do
            i <- [C.exp| int { $(Fl_Choice* choice)->value() } |]
            if i == tierCount
              then Rank . fromMaybe 0 . readMaybe . T.unpack <$> peekInputValue input
              else return $ Tier $ fromIntegral i + 1
    getBandDiff <- makeDifficulty root proj.projectSongYaml.metadata.difficulty
    getNewParts <- fmap catMaybes $ forM (HM.toList proj.projectSongYaml.parts.getParts) \(fpart, part) ->
      if part == emptyPart
        then return Nothing
        else do
          let partName = T.toTitle $ F.getPartName fpart
          itemInst <- withUTF8 partName \cname ->
            [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->add($(Fl_Tree_Item* root), $(char* cname)) } |]
          let addType :: T.Text -> (Ptr Fl_Tree_Item -> IO (Bool -> Part FilePath -> IO (Part FilePath))) -> IO (Part FilePath -> IO (Part FilePath))
              addType lbl extra = do
                itemCheck <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->add($(Fl_Tree_Item* itemInst), "") } |]
                check <- withRect dummyRect \bx by bw bh ->
                  [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
                withUTF8 lbl \clabel ->
                  [C.block| void { $(Fl_Check_Button* check)->copy_label($(char* clabel)); } |]
                [C.block| void { $(Fl_Check_Button* check)->value(1); } |]
                let checkAsWidget = [C.exp| Fl_Widget* { (Fl_Widget*)$(Fl_Check_Button* check) } |]
                checkWidget <- checkAsWidget
                [C.block| void { $(Fl_Tree_Item* itemCheck)->widget($(Fl_Widget* checkWidget)); } |]
                fn <- extra itemCheck
                return \curPart -> do
                  isChecked <- checkValue check
                  fn isChecked curPart
              makeChoiceFrom :: (Eq a) => [a] -> Ptr Fl_Tree_Item -> a -> (a -> T.Text) -> IO (IO a)
              makeChoiceFrom opts itemParent cur getLabel = do
                itemChoice <- [C.exp| Fl_Tree_Item* { $(Fl_Tree* tree)->add($(Fl_Tree_Item* itemParent), "") } |]
                choice <- withRect dummyRect \cx cy cw ch ->
                  [C.exp| Fl_Choice* { new Fl_Choice($(int cx), $(int cy), $(int cw), $(int ch)) } |]
                forM_ opts \opt -> withUTF8 (getLabel opt) \clabel ->
                  [C.block| void { $(Fl_Choice* choice)->add($(char* clabel)); } |]
                let idx = fromMaybe 0 $ elemIndex cur opts :: Int
                    cidx = fromIntegral idx :: CInt
                [C.block| void { $(Fl_Choice* choice)->value($(int cidx)); } |]
                let choiceAsWidget = [C.exp| Fl_Widget* { (Fl_Widget*)$(Fl_Choice* choice) } |]
                choiceWidget <- choiceAsWidget
                [C.block| void { $(Fl_Tree_Item* itemChoice)->widget($(Fl_Widget* choiceWidget)); } |]
                return $ do
                  i <- [C.exp| int { $(Fl_Choice* choice)->value() } |]
                  return $ opts !! fromIntegral i
              makeChoice :: (Eq a, Enum a, Bounded a) => Ptr Fl_Tree_Item -> a -> (a -> T.Text) -> IO (IO a)
              makeChoice = makeChoiceFrom [minBound .. maxBound]
          mbGRYBO <- forM part.grybo \pg -> addType "5-Fret" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #grybo .~ do
                guard isChecked
                Just $ (pg :: ModeFive) & #difficulty .~ diff
          mbGHL <- forM part.ghl \pg -> addType "6-Fret" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #ghl .~ do
                guard isChecked
                Just $ (pg :: ModeSix) & #difficulty .~ diff
          mbProKeys <- forM part.proKeys \pk -> addType "Pro Keys" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pk.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #proKeys .~ do
                guard isChecked
                Just $ (pk :: ModeProKeys) & #difficulty .~ diff
          mbProGuitar <- forM part.proGuitar \pg -> addType "Pro Guitar" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #proGuitar .~ do
                guard isChecked
                Just $ (pg :: ModeProGuitar FilePath) & #difficulty .~ diff
          mbDrums <- forM part.drums \pd -> addType "Drums" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pd.difficulty
            getMode <- makeChoice itemCheck pd.mode \case
              Drums4     -> "4-Lane Drums"
              Drums5     -> "5-Lane Drums"
              DrumsPro   -> "Pro Drums"
              DrumsReal  -> "Phase Shift Real Drums"
              DrumsElite -> "Elite Drums"
            getKicks <- makeChoice itemCheck pd.kicks \case
              Kicks1x   -> "1x Bass Pedal"
              Kicks2x   -> "2x Bass Pedal"
              KicksBoth -> "1x+2x Bass Pedal (PS X+ or C3 format)"
            getKit <- makeChoice itemCheck pd.kit \case
              HardRockKit   -> "Hard Rock Kit"
              ArenaKit      -> "Arena Kit"
              VintageKit    -> "Vintage Kit"
              TrashyKit     -> "Trashy Kit"
              ElectronicKit -> "Electronic Kit"
            return \isChecked curPart -> do
              diff <- getDiff
              mode <- getMode
              kicks <- getKicks
              kit <- getKit
              return $ (curPart :: Part FilePath) & #drums .~ do
                guard isChecked
                Just $ pd
                  & #difficulty .~ diff
                  & #mode       .~ mode
                  & #kicks      .~ kicks
                  & #kit        .~ kit
          mbVocal <- forM part.vocal \pv -> addType "Vocals" \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pv.difficulty
            getCount <- makeChoice itemCheck pv.count \case
              Vocal1 -> "Solo"
              Vocal2 -> "Harmonies (2)"
              Vocal3 -> "Harmonies (3)"
            getGender <- makeChoiceFrom [Nothing, Just Male, Just Female] itemCheck pv.gender \case
              Nothing     -> "Unspecified Gender"
              Just Male   -> "Male"
              Just Female -> "Female"
            return \isChecked curPart -> do
              diff   <- getDiff
              count  <- getCount
              gender <- getGender
              return $ (curPart :: Part FilePath) & #vocal .~ do
                guard isChecked
                Just $ pv
                  & #difficulty .~ diff
                  & #count      .~ count
                  & #gender     .~ gender
          mbMania <- forM part.mania \pm -> addType "Mania" \_itemCheck -> do
            return \isChecked curPart -> do
              -- TODO difficulty
              return curPart { mania = guard isChecked >> Just pm }
          return $ Just do
            let modifiers = catMaybes [mbGRYBO, mbGHL, mbProKeys, mbProGuitar, mbDrums, mbVocal, mbMania]
            newPart <- foldl (>>=) (return part) modifiers
            return (fpart, newPart)
    let editParts = do
          newParts <- sequence getNewParts
          bandDiff <- getBandDiff
          return \yaml -> yaml
            & #parts                  .~ Parts (HM.fromList newParts)
            & #metadata % #difficulty .~ bandDiff
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Tree* tree)); } |]
    return (editParts, tab)

  -- Preview tab
  (_previewTab, cleanupGL) <- makeTab windowRect "Preview" \rect tab -> mdo
    setTabColor tab homeTabColor
    let (topControlsArea1, glArea) = chopTop 40 rect
        (trimClock 0 5 0 5 -> volPlayButtonArea, topControlsArea2) = chopLeft 160 topControlsArea1
        (trimClock 8 5 8 0 -> volSliderArea, trimClock 5 0 5 5 -> playButtonArea) = chopRight 50 volPlayButtonArea
        (trimClock 8 5 8 5 -> scrubberArea, trimClock 5 5 5 5 -> speedArea) = chopRight 150 topControlsArea2
    topControls <- withRect topControlsArea1 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    enabledAudioParts <- newIORef
      $ Set.fromList
      $ filter (/= "Crowd")
      $ maybe [] (.parts) maybeAudio
    volSlider <- withRect volSliderArea \sx sy sw sh ->
      [C.exp| Fl_Hor_Nice_Slider* { new Fl_Hor_Nice_Slider($(int sx), $(int sy), $(int sw), $(int sh)) } |]
    [C.block| void {
      $(Fl_Hor_Nice_Slider* volSlider)->minimum(0);
      $(Fl_Hor_Nice_Slider* volSlider)->maximum(1);
      $(Fl_Hor_Nice_Slider* volSlider)->value(1);
      $(Fl_Hor_Nice_Slider* volSlider)->color($(Fl_Color homeTabColor));
    } |]
    let updateVolume = do
          ss <- readIORef varTime
          case ss.songPlaying of
            Nothing -> return ()
            Just p -> do
              v <- [C.exp| double { $(Fl_Hor_Nice_Slider* volSlider)->value() } |]
              currentEnabledAudio <- readIORef enabledAudioParts
              p.playAudioHandle.audioSetGain GainLevels
                { global = realToFrac v
                , parts = maybe [] (.parts) maybeAudio <&> \name ->
                  (name, if elem name currentEnabledAudio then 1 else 0)
                }
    cbVolSlider <- makeCallback \_ _ -> updateVolume
    [C.block| void { $(Fl_Hor_Nice_Slider* volSlider)->callback($(Fl_Callback cbVolSlider)); } |]
    playButton <- withRect playButtonArea \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@>") } |]
    [C.block| void { $(Fl_Button* playButton)->deactivate(); } |]
    scrubberDragResume <- newIORef Nothing
    scrubber <- withRect scrubberArea \sx sy sw sh ->
      [C.exp| HsScrubber* { new HsScrubber($(int sx), $(int sy), $(int sw), $(int sh)) } |]
    let Fl_Color filledColorVal = fl_rgb_color 13 131 216
        Fl_Color emptyColorVal = fl_rgb_color 75 75 75
    [C.block| void {
      $(HsScrubber* scrubber)->minimum(0);
      $(HsScrubber* scrubber)->maximum(100);
      $(HsScrubber* scrubber)->step(1);
      $(HsScrubber* scrubber)->value(0);
      $(HsScrubber* scrubber)->deactivate();
    } |]
    (getSpeed, counter) <- speedPercent' False speedArea
    [C.block| void { $(Fl_Group* topControls)->end(); } |]
    [C.block| void { $(Fl_Group* topControls)->resizable($(HsScrubber* scrubber)); } |]
    -- Set up song length
    let songLength :: Double
        songLength = realToFrac $ U.applyTempoMap
          song.previewTempo
          song.previewTiming.timingEnd
        csongLength = coerce songLength :: CDouble
    -- Set up scrubber colors and section markers (all drawing done in C++)
    [C.block| void {
      $(HsScrubber* scrubber)->set_colors($(unsigned int filledColorVal), $(unsigned int emptyColorVal));
    } |]
    let sectionTimes = Map.keys $ previewSections song
    case sectionTimes of
      [] -> return ()
      times -> withArray (coerce times :: [CDouble]) \pTimes -> do
        let count = fromIntegral (length times) :: CInt
        [C.block| void {
          $(HsScrubber* scrubber)->set_sections($(double* pTimes), $(int count), $(double csongLength));
        } |]
    [C.block| void {
      $(HsScrubber* scrubber)->maximum($(double csongLength));
      $(HsScrubber* scrubber)->activate();
      $(Fl_Button* playButton)->activate();
    } |]
    let initState = SongState 0 Nothing
    varState <- newMVar initState
    varTime <- newIORef initState
    (groupGL, redrawGL, deleteGL) <- previewGroup
      sink
      glArea
      (return song)
      (currentSongTime <$> getTimeMonotonic <*> readIORef varTime)
      getSpeed
      ((\state -> (.playAudioHandle) <$> state.songPlaying) <$> readIORef varTime)
      (maybe [] (.parts) maybeAudio)
      (readIORef enabledAudioParts)
      (\new -> do
        writeIORef enabledAudioParts new
        updateVolume
      )
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* groupGL)); } |]
    let takeState = takeMVar varState
        putState ss = do
          putMVar varState ss
          writeIORef varTime ss
        stopPlaying :: Double -> Playing -> IO Double
        stopPlaying t ps = do
          stime <- getTimeMonotonic
          ps.playStopAnimation
          ps.playAudioHandle.audioStop
          return $ currentSongTime stime $ SongState t $ Just ps
        startPlaying :: Double -> IO SongState
        startPlaying t = do
          speed <- getSpeed
          let dummyHandle = AudioHandle
                { audioStop = return ()
                , audioSetGain = \_ -> return ()
                , audioWaveform = return VS.empty
                }
          handle <- case maybeAudio of
            Just audio -> do
              gain <- [C.exp| double { $(Fl_Hor_Nice_Slider* volSlider)->value() } |]
              currentEnabledAudio <- readIORef enabledAudioParts
              Exc.catch @Exc.IOException
                (audio.play t (guard (speed /= 1) >> Just speed) GainLevels
                  { global = realToFrac gain
                  , parts = maybe [] (.parts) maybeAudio <&> \name ->
                    (name, if elem name currentEnabledAudio then 1 else 0)
                  })
                (\_ -> return dummyHandle)
            Nothing -> return dummyHandle
          curTime <- getTimeMonotonic
          stopAnim <- startAnimation (recip $ realToFrac ?preferences.prefPreviewFPS) do
            t' <- currentSongTime <$> getTimeMonotonic <*> readIORef varTime
            [C.block| void { Fl::lock(); } |]
            let ct' = coerce t' :: CDouble
            [C.block| void { $(HsScrubber* scrubber)->value($(double ct')); } |]
            redrawGL
            [C.block| void { Fl::unlock(); } |]
            when (t' >= songLength) do
              ss <- takeState
              ss' <- case ss.songPlaying of
                Nothing -> return ss
                Just ps -> do
                  _ <- stopPlaying songLength ps
                  return $ SongState songLength Nothing
              putState ss'
              [C.block| void { $(Fl_Button* playButton)->label("@>"); } |]
          let ps = Playing
                { playStarted = curTime
                , playSpeed = speed
                , playStopAnimation = stopAnim
                , playAudioHandle = handle
                }
          return SongState { songTime = t, songPlaying = Just ps }
    -- Scrubber handle callback for push/drag/release
    let updateScrubberTime = do
          sx <- [C.exp| int { $(HsScrubber* scrubber)->x() } |]
          sw <- [C.exp| int { $(HsScrubber* scrubber)->w() } |]
          mouseX <- [C.exp| int { Fl::event_x() } |]
          timeMin <- [C.exp| double { $(HsScrubber* scrubber)->minimum() } |]
          timeMax <- [C.exp| double { $(HsScrubber* scrubber)->maximum() } |]
          let newTime = max (coerce timeMin) $ min (coerce timeMax) $
                coerce timeMin + (coerce timeMax - coerce timeMin)
                * (fromIntegral (mouseX - sx) / fromIntegral sw) :: Double
              cnewTime = coerce newTime :: CDouble
          [C.block| void { $(HsScrubber* scrubber)->value($(double cnewTime)); } |]
          return newTime
    fpScrubberHandle <- makeScrubberHandleCallback \_ cevent _ _ _ _ -> do
      let event = fromIntegral cevent :: Int
          flPush    = [C.pure| int { FL_PUSH } |]
          flDrag    = [C.pure| int { FL_DRAG } |]
          flRelease = [C.pure| int { FL_RELEASE } |]
      if | fromIntegral event == flPush -> do
             secs <- updateScrubberTime
             ss <- takeState
             case ss.songPlaying of
               Nothing -> writeIORef scrubberDragResume $ Just False
               Just ps -> do
                 writeIORef scrubberDragResume $ Just True
                 void $ stopPlaying ss.songTime ps
             putState $ SongState secs Nothing
             redrawGL
             [C.block| void { Fl::focus($(HsScrubber* scrubber)); } |]
             return 1
         | fromIntegral event == flDrag -> do
             secs <- updateScrubberTime
             ss <- takeState
             putState ss { songTime = secs }
             redrawGL
             return 1
         | fromIntegral event == flRelease -> do
             secs <- updateScrubberTime
             ss <- takeState
             ss' <- readIORef scrubberDragResume >>= \case
               Just True -> startPlaying secs
               _         -> return ss { songTime = secs }
             writeIORef scrubberDragResume Nothing
             putState ss'
             redrawGL
             return 1
         | otherwise -> return (-1) -- fall through to default handler
    [C.block| void {
      $(HsScrubber* scrubber)->set_handle_callback(
        $(int (*fpScrubberHandle)(void*, int, int, int, int, int)), NULL);
    } |]
    let togglePlay = readIORef scrubberDragResume >>= \case
          Just _  -> return () -- we are dragging the scrubber, ignore
          Nothing -> do
            ss <- takeState
            ss' <- case ss.songPlaying of
              Nothing -> do
                [C.block| void { $(Fl_Button* playButton)->label("@square"); } |]
                startPlaying ss.songTime
              Just ps -> do
                [C.block| void { $(Fl_Button* playButton)->label("@>"); } |]
                t <- stopPlaying ss.songTime ps
                return $ SongState t Nothing
            putState ss'
    cbPlayButton <- makeCallback \_ _ -> togglePlay
    [C.block| void {
      $(Fl_Button* playButton)->callback($(Fl_Callback cbPlayButton));
      $(Fl_Button* playButton)->shortcut(' ');
    } |]
    cbCounter <- makeCallback \_ _ -> do
      ss <- takeState
      ss' <- case ss.songPlaying of
        Nothing -> redrawGL >> return ss
        Just ps -> stopPlaying ss.songTime ps >>= startPlaying
      putState ss'
    [C.block| void { $(Fl_Counter* counter)->callback($(Fl_Callback cbCounter)); } |]
    let cleanup = do
          ss <- takeState
          case ss.songPlaying of
            Nothing -> return ()
            Just ps -> void $ stopPlaying ss.songTime ps
          deleteGL
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

  _starsTab <- makeTab windowRect "Stars" \rect tab -> do
    setTabColor tab homeTabColor
    pack <- withRect rect \px py pw ph ->
      [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
    [C.block| void { $(Fl_Pack* pack)->end(); } |]
    sink $ EventOnyx $ void $ forkOnyx do
      let input = takeDirectory proj.projectLocation </> "notes.mid"
      mid <- F.loadMIDI input
      let fixed = F.onyxToFixed mid.tracks
          foundTracksRB3 = getScoreTracks    fixed
          foundTracksGH2 = getScoreTracksGH2 fixed
      -- TODO this is a hack to not hold onto the whole midi file in memory, should find a better way!
      stackIO $ void $ Exc.evaluate $ length $ show (foundTracksRB3, foundTracksGH2)
      stackIO $ sink $ EventIO $ mdo
        [C.block| void { $(Fl_Pack* pack)->begin(); } |]

        -- TODO when we add localization support, need to support different separators
        -- (e.g. Spanish should use periods)
        let commafy n = T.pack $ reverse $ go $ reverse $ show n
            go (x : y : z : rest@(_ : _))
              = [x, y, z, ','] ++ go rest
            go xs = xs

        -- RB3
        padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          void $ withRect rect' \bx by bw bh ->
            [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "Rock Band 3 Star Cutoffs") } |]
        getTracksRB3 <- padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          starSelectors rect' foundTracksRB3 updateLabelRB3 scoreTrackName
            [ ("Guitar", [ScoreGuitar, ScoreProGuitar])
            , ("Bass"  , [ScoreBass  , ScoreProBass  ])
            , ("Keys"  , [ScoreKeys  , ScoreProKeys  ])
            , ("Drums" , [ScoreDrums , ScoreProDrums ])
            , ("Vocal" , [ScoreVocals, ScoreHarmonies])
            ]
        [l1S, l2S, l3S, l4S, l5S, lGS] <- padded 3 10 3 10 (Size (Width 800) (Height 40)) \bottomArea ->
          forM (splitHorizN 6 bottomArea) \cell -> do
            cutoffLabel <- withRect cell \bx by bw bh ->
              [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
            [C.block| void { $(Fl_Box* cutoffLabel)->labelfont(FL_HELVETICA_BOLD); } |]
            return cutoffLabel
        let updateLabelRB3 = do
              stars <- tracksToStars <$> getTracksRB3
              let setLabel box txt = withUTF8 txt \clabel ->
                    [C.block| void { $(Fl_Box* box)->copy_label($(char* clabel)); } |]
              setLabel l1S $ maybe "" (\n -> "1*: " <> commafy n) stars.stars1
              setLabel l2S $ maybe "" (\n -> "2*: " <> commafy n) stars.stars2
              setLabel l3S $ maybe "" (\n -> "3*: " <> commafy n) stars.stars3
              setLabel l4S $ maybe "" (\n -> "4*: " <> commafy n) stars.stars4
              setLabel l5S $ maybe "" (\n -> "5*: " <> commafy n) stars.stars5
              setLabel lGS $ maybe "" (\n -> "G*: " <> commafy n) stars.starsGold
        updateLabelRB3

        -- GH2
        padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          void $ withRect rect' \bx by bw bh ->
            [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "Guitar Hero 1/2 Star Cutoffs") } |]
        getTracksGH2 <- padded 5 150 5 150 (Size (Width 800) (Height 50)) \rect' ->
          starSelectors rect' foundTracksGH2 updateLabelGH2 scoreTrackNameGH2
            [ ("Lead", [ScoreGH2Guitar])
            , ("Coop", [ScoreGH2Bass, ScoreGH2Rhythm])
            ]
        [ghBase, ghFour, gh2Five, gh1Five] <- padded 3 10 3 10 (Size (Width 800) (Height 40)) \bottomArea ->
          forM (splitHorizN 4 bottomArea) \cell -> do
            cutoffLabel <- withRect cell \bx by bw bh ->
              [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
            [C.block| void { $(Fl_Box* cutoffLabel)->labelfont(FL_HELVETICA_BOLD); } |]
            return cutoffLabel
        let updateLabelGH2 = do
              bases <- map (\(_, _, base) -> base) <$> getTracksGH2
              let base = sum bases
                  mult n = floor $ toRational base * n :: Int
                  setLabel box txt = withUTF8 txt \clabel ->
                    [C.block| void { $(Fl_Box* box)->copy_label($(char* clabel)); } |]
              if length bases <= 1
                then do
                  setLabel ghBase  $ "Base: "       <> commafy base
                  setLabel ghFour  $ "4*: "         <> commafy (mult 2  )
                  setLabel gh2Five $ "5* (GH2): "   <> commafy (mult 2.8)
                  setLabel gh1Five $ "5* (GH1): "   <> commafy (mult 3  )
                else do
                  setLabel ghBase  $ "Base: " <> commafy base
                  setLabel ghFour  ("" :: T.Text)
                  setLabel gh2Five $ "5* (GH2): " <> commafy (mult 2)
                  setLabel gh1Five ("" :: T.Text)
        updateLabelGH2

        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        [C.block| void { Fl::redraw(); } |]
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
    return tab

  let fullProjModify :: [SongYaml FilePath -> SongYaml FilePath] -> Project -> IO Project
      fullProjModify extraMods p = do
        modifiers <- sequence [modifyMeta, modifyInsts]
        let newYaml = foldr ($) p.projectSongYaml $ modifiers <> extraMods
        saveProject p newYaml

  rb3Tab <- makeTab windowRect "RB3" \rect tab -> do
    setTabColor tab functionTabColor
    songPageRB3 sink rect tab proj \qcPossible tgt create -> do
      proj' <- fullProjModify [] proj
      let name = case create of
            RB3CON      _ -> "Building RB3 CON (360)"
            RB3PKG      _ -> "Building RB3 PKG (PS3)"
            RB3Magma    _ -> "Building Magma project"
            RB3LoosePS3 _ -> "Building RB3 PS3 folders"
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
            RB3LoosePS3 dout -> do
              folder <- buildRB3LoosePS3 tgt proj'
              stackIO $ installPS3Folder "BLUS30463" (Just "pkg-contents/rb3") folder dout
          go = startTasks [(name, task)]
      sink $ EventOnyx if qcPossible && isRB
        then warnQuickConvert 1 sink go
        else go
    return tab
  rb2Tab <- makeTab windowRect "RB2" \rect tab -> do
    setTabColor tab functionTabColor
    songPageRB2 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = case create of
            RB2CON      _ -> "Building RB2 CON (360)"
            RB2PKG      _ -> "Building RB2 PKG (PS3)"
            RB2LoosePS3 _ -> "Building RB2 PS3 folders"
          task = case create of
            RB2CON fout -> do
              tmp <- buildRB2CON tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB2PKG fout -> do
              tmp <- buildRB2PKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RB2LoosePS3 dout -> do
              folder <- buildRB2LoosePS3 tgt proj'
              stackIO $ installPS3Folder "BLUS30050" (Just "pkg-contents/rb2") folder dout
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  psTab <- makeTab windowRect "CH/PS" \rect tab -> do
    setTabColor tab functionTabColor
    songPagePS sink rect tab proj \tgt create -> sink $ EventOnyx do
      newPreferences <- readPreferences
      proj' <- stackIO $ fullProjModify [applyDownmix newPreferences] proj
      let name = case create of
            PSDir _ -> "Building CH/PS song folder"
            PSZip _ -> "Building CH/PS zip file"
            PSSng _ -> "Building CH/PS .sng file"
          tgt' = (tgt :: TargetPS FilePath)
            & #audioFormat .~ case newPreferences.prefCHAudioFormat of
              CHAudioOggVorbis -> "ogg"
              CHAudioOpus      -> "opus"
          task = case create of
            PSDir dout -> do
              tmp <- buildPSDir tgt' proj'
              copyDirRecursive tmp dout
              return [dout]
            PSZip fout -> do
              tmp <- buildPSZip tgt' proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            PSSng fout -> do
              tmp <- buildPSSng tgt' proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
      startTasks [(name, task)]
    return tab
  gh1Tab <- makeTab windowRect "GH1" \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH1 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
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
  gh2Tab <- makeTab windowRect "GH2" \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH2 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
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
  gh3Tab <- makeTab windowRect "GH3" \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH3 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = case create of
            GH3LIVE{}     -> "Building GH3 LIVE file"
            GH3PKG{}      -> "Building GH3 PKG file"
            GH3LoosePS3{} -> "Building GH3 PS3 folders"
          warnCache = warn "Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
          task = case create of
            GH3LIVE fout -> do
              tmp <- buildGH3LIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warnCache
              return [fout]
            GH3PKG fout -> do
              tmp <- buildGH3PKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warnCache
              return [fout]
            GH3LoosePS3 dout -> do
              folder <- buildGH3LoosePS3 tgt proj'
              results <- stackIO $ installPS3Folder "BLUS30074" (Just "pkg-contents/gh3") folder dout
              warnCache
              return results
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  worTab <- makeTab windowRect "GH:WoR" \rect tab -> do
    setTabColor tab functionTabColor
    songPageGHWOR sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = case create of
            GHWORLIVE{}     -> "Building GH:WoR LIVE file"
            GHWORPKG{}      -> "Building GH:WoR PKG file"
            GHWORLoosePS3{} -> "Building GH:WoR PS3 folders"
          task = case create of
            GHWORLIVE fout -> do
              tmp <- buildGHWORLIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            GHWORPKG fout -> do
              tmp <- buildGHWORPKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            GHWORLoosePS3 dout -> do
              folder <- buildGHWORLoosePS3 tgt proj'
              stackIO $ installPS3Folder "BLUS30487" (Just "pkg-contents/ghwor") folder dout
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  rrTab <- makeTab windowRect "RR" \rect tab -> do
    setTabColor tab functionTabColor
    songPageRR sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = case create of
            RRLIVE{} -> "Building RR LIVE file"
            RRPKG{}  -> "Building RR PKG file"
          task = case create of
            RRLIVE fout -> do
              tmp <- buildRRLIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            RRPKG fout -> do
              tmp <- buildRRPKG tgt proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  utilsTab <- makeTab windowRect "Utilities" \rect tab -> do
    setTabColor tab functionTabColor
    pack <- withRect rect \px py pw ph ->
      [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Build web preview") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cbBtn <- makeCallback \_ _ -> sink $ EventIO do
        let presetName = T.pack $ proj.projectTemplate <> "_player"
        saveFileChooser "Save web preview folder" presetName (T.pack <$> ?preferences.prefDirPreview) \f -> do
          proj' <- fullProjModify [] proj
          let task = do
                tmp <- buildPlayer Nothing proj'
                copyDirRecursive tmp f
                return [f]
          sink $ EventOnyx $ startTasks [("Build web preview", task)]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cbBtn)); } |]
    -- TODO this needs to somehow start from a pre-import midi file if present
    -- so we don't mess up things like encore pad tracks
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Produce MIDI with automatic reductions") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cbBtn <- makeCallback \_ _ -> sink $ EventIO do
        picker <- [C.exp| Fl_Native_File_Chooser* {
          new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_SAVE_FILE)
        } |]
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save MIDI file"); } |]
        isDir <- Dir.doesDirectoryExist proj.projectSource
        let dir = if isDir then proj.projectSource else takeDirectory proj.projectSource
        withUTF8 dir \cdir ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->filter("*.mid");
          $(Fl_Native_File_Chooser* picker)->preset_file("reduced.mid");
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          f <- peekUTF8 cstr
          when (not $ null f) do
            let ext = map toLower $ takeExtension f
                f' = if elem ext [".mid", ".midi"] then f else f <.> "mid"
            sink $ EventOnyx $ startTasks [(f', do
              simpleReduce (takeDirectory proj.projectLocation </> "notes.mid") f'
              return [f'])]
        [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cbBtn)); } |]
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Find hanging Pro Keys notes") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cbBtn <- makeCallback \_ _ -> sink $ EventOnyx do
        startTasks [("Pro Keys range check", proKeysHanging Nothing proj >> return [])]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cbBtn)); } |]
    [C.block| void { $(Fl_Pack* pack)->end(); } |]
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
    return tab

  let tabsToDisable = [rb3Tab, rb2Tab, psTab, gh1Tab, gh2Tab, gh3Tab, worTab, rrTab, utilsTab]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" \rect tab -> do
    setTabColor tab taskColor
    [C.block| void { $(Fl_Group* tab)->deactivate(); } |]
    let cbStart = do
          [C.block| void { $(Fl_Group* tab)->activate(); } |]
          forM_ tabsToDisable \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tab)); } |]
          updateTabsColor tabs
        cbEnd = do
          forM_ tabsToDisable \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage rect tab sink cbStart cbEnd

  [C.block| void { $(Fl_Tabs* tabs)->end(); } |]
  updateTabsColor tabs
  cbTabs <- makeCallback \_ _ -> updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->callback($(Fl_Callback cbTabs)); } |]
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* rb3Tab)); } |]

  cbWindow <- makeCallback \_ _ -> windowCloser
    (cancelTasks >> cleanupGL >> mapM_ release proj.projectRelease)
    window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->callback($(Fl_Callback cbWindow));
    $(Fl_Window* window)->show();
  } |]

-- | Ignores the Escape key, and runs a resource-release function after close.
windowCloser :: IO () -> Ptr Fl_Window -> IO ()
windowCloser finalize window = do
  isEscape <- [C.exp| bool {
    Fl::event() == FL_SHORTCUT && Fl::event_key() == FL_Escape
  } |]
  unless (toBool isEscape) do
    [C.block| void { $(Fl_Window* window)->hide(); } |]
    finalize

saveProject :: Project -> SongYaml FilePath -> IO Project
saveProject proj song = do
  yamlEncodeFile proj.projectLocation $ toJSON song
  return proj { projectSongYaml = song }

makePS3PackPresetDropdown :: (Event -> IO ()) -> Rectangle -> IO (IO QuickPS3Folder)
makePS3PackPresetDropdown _sink rect = do
  let labelOne = "Combine into one new USRDIR subfolder per pack"
      labelSeparate = "Each song gets a new USRDIR subfolder"
      labelCustomInitial = "Custom USRDIR subfolder..."
      labelCustom txt = "Custom folder: " <> txt
      customPrompt :: T.Text
      customPrompt = "Enter custom USRDIR subfolder for your pack. (A number will be appended if creating more than one pack.)"
  menu <- withRect rect \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh)) } |]
  withUTF8 labelOne \clabel ->
    [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* clabel)); } |]
  ref <- newIORef QCOneFolder
  let addItem label action = do
        menuCb <- makeCallback \_ _ -> action
        withUTF8 (T.replace "/" "\\/" label) \clabel' ->
          [C.block| void {
            $(Fl_Menu_Button* menu)->add($(char* clabel'), 0, $(Fl_Callback menuCb));
          } |]
  addItem labelOne do
    writeIORef ref QCOneFolder
    withUTF8 labelOne \newLabel ->
      [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
  addItem labelSeparate do
    writeIORef ref QCSeparateFolders
    withUTF8 labelSeparate \newLabel ->
      [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
  addItem labelCustomInitial do
    defValue <- readIORef ref <&> \case
      QCCustomFolder bs -> Just $ TE.decodeUtf8 bs
      _                 -> Nothing
    withUTF8 customPrompt \cprompt -> do
      result <- withMaybeUTF8 defValue \cdef -> [C.exp| const char* {
        fl_input("%s", $(char* cdef), $(char* cprompt))
      } |]
      if result == nullPtr
        then return () -- cancelled, leave as whatever it was
        else do
          txt <- T.pack <$> peekUTF8 result
          if T.null txt
            then return () -- ignore empty input
            else do
              writeIORef ref $ QCCustomFolder $ TE.encodeUtf8 txt
              withUTF8 (labelCustom txt) \newLabel ->
                [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
  return $ readIORef ref

makeModeDropdown :: Rectangle -> [(T.Text, a)] -> (a -> IO ()) -> IO ()
makeModeDropdown rect opts withOpt = do
  let (initialLabel, initialOpt) = head opts
  menu <- withRect rect \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh)) } |]
  withUTF8 initialLabel \clabel ->
    [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* clabel)); } |]
  forM_ opts \(label, opt) -> do
    let label' = T.unpack $ T.replace "/" "\\/" label
    menuCb <- makeCallback \_ _ -> do
      withOpt opt
      withUTF8 label \newLabel ->
        [C.block| void { $(Fl_Menu_Button* menu)->copy_label($(char* newLabel)); } |]
    withUTF8 label' \clabel' ->
      [C.block| void {
        $(Fl_Menu_Button* menu)->add($(char* clabel'), 0, $(Fl_Callback menuCb));
      } |]
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
  rects = splitHorizN (fromIntegral $ length slots) rect
  instSelector ((lbl, trackFilter), r) = do
    let r' = trimClock 20 5 5 5 r
        matchTracks = [ t | t@(strack, _, _) <- foundTracks, elem strack trackFilter ]
    choice <- withRect r' \cx cy cw ch ->
      [C.exp| Fl_Choice* { new Fl_Choice($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    withUTF8 lbl \clabel ->
      [C.block| void {
        $(Fl_Choice* choice)->copy_label($(char* clabel));
        $(Fl_Choice* choice)->align(FL_ALIGN_TOP);
      } |]
    [C.block| void { $(Fl_Choice* choice)->add("(none)"); } |]
    forM_ matchTracks \(strack, diff, _) -> do
      let itemLabel = trackToName strack <> " " <> case diff of
            RB.Easy   -> "(E)"
            RB.Medium -> "(M)"
            RB.Hard   -> "(H)"
            RB.Expert -> "(X)"
      withUTF8 itemLabel \citem ->
        [C.block| void { $(Fl_Choice* choice)->add($(char* citem)); } |]
    [C.block| void { $(Fl_Choice* choice)->value(0); } |]
    cbChoice <- makeCallback \_ _ -> updateLabel
    [C.block| void { $(Fl_Choice* choice)->callback($(Fl_Callback cbChoice)); } |]
    return do
      i <- [C.exp| int { $(Fl_Choice* choice)->value() } |]
      return case drop (fromIntegral i - 1) matchTracks of
        t : _ | i /= 0 -> [t]
        _              -> []
  in mconcat $ map instSelector $ zip slots rects

-- _selectArkDestination not translated

taskOutputPage
  :: Rectangle
  -> Ptr Fl_Group
  -> (Event -> IO ()) -- event sink
  -> IO () -- callback when starting a task
  -> IO () -- callback when ending a task
  -> IO
    ( [(String, Onyx [FilePath])] -> Onyx () -- returns function to start a task
    , IO () -- cancels a task if running
    )
taskOutputPage rect tab sink cbStart cbEnd = do
  let (labelRect, belowLabel) = chopTop 40 rect
      (termRect, buttonRect) = chopBottom 50 belowLabel
      labelRect' = trimClock 10 10 5 10 labelRect
      termRect' = trimClock 5 10 5 10 termRect
      buttonRect' = trimClock 5 10 10 10 buttonRect
  labelTask <- withRect labelRect' \lx ly lw lh ->
    [C.exp| Fl_Box* { new Fl_Box($(int lx), $(int ly), $(int lw), $(int lh), "\xe2\x80\xa6") } |]
  termTask <- withRect termRect' \tx ty tw th ->
    [C.block| Fl_Simple_Terminal* {
      Fl_Simple_Terminal* t = new Fl_Simple_Terminal($(int tx), $(int ty), $(int tw), $(int th));
      t->history_lines(-1);
      t->ansi(true);
      t->stay_at_bottom(true);
      return t;
    } |]
  [C.block| void {
    $(Fl_Group* tab)->resizable($(Fl_Simple_Terminal* termTask));
  } |]
  cancelButton <- withRect buttonRect' \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Cancel") } |]
  showButton <- withRect buttonRect' \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Show created files") } |]
  [C.block| void { $(Fl_Button* showButton)->hide(); } |]

  -- functions
  taskStatus <- newMVar (Left Nothing) :: IO (MVar (Either
      (Maybe (FilePath, [FilePath])) -- finished, maybe have files to show the user
      (Int, Int, [[FilePath]], Int, ThreadId) -- tasks in progress
    ))
  let taskMessage :: (MessageLevel, Message) -> IO ()
      taskMessage = sink . EventIO . addTerm termTask . toTermMessage
      cbStart' = do
        cbStart
        [C.block| void {
          $(Fl_Button* cancelButton)->activate();
          $(Fl_Button* cancelButton)->show();
          $(Fl_Button* showButton)->hide();
          Fl::redraw();
        } |]
      cbEnd' = do
        cbEnd
        [C.block| void {
          $(Fl_Button* cancelButton)->deactivate();
          Fl::redraw();
        } |]
      putFinalResults success = do
        common <- commonDir $ concat success
        putMVar taskStatus $ Left common
        case common of
          Just _  -> [C.block| void {
            $(Fl_Button* showButton)->show();
            $(Fl_Button* cancelButton)->hide();
          } |]
          Nothing -> return ()
      cancelTasks = do
        takeMVar taskStatus >>= \case
          Right (_, _, success, _, tid) -> do
            addTerm termTask $ TermLog "Cancelled tasks."
            killThread tid
            putFinalResults success
          Left res -> putMVar taskStatus $ Left res
        cbEnd'
      updateStatus fn = takeMVar taskStatus >>= \case
        Left res -> putMVar taskStatus $ Left res -- maybe task was cancelled
        Right oldStatus -> do
          let status@(done, total, success, failure, _) = fn oldStatus
          withUTF8 (unwords
            [ show done <> "/" <> show total
            , "tasks complete:"
            , show $ length success
            , "succeeded,"
            , show failure
            , "failed"
            ]) \cs -> [C.block| void { $(Fl_Box* labelTask)->copy_label($(char* cs)); } |]
          if done >= total
            then do
              cbEnd'
              putFinalResults success
            else putMVar taskStatus $ Right status
      startTasks :: [(String, Onyx [FilePath])] -> Onyx ()
      startTasks tasks = liftIO (takeMVar taskStatus) >>= \case
        Right status -> liftIO $ putMVar taskStatus $ Right status -- shouldn't happen
        Left _ -> do
          tid <- forkOnyx $ replaceQueueLog taskMessage $ forM_ (zip [1..] tasks) \(i, (taskName, task)) -> do
            liftIO $ sink $ EventIO $ addTerm termTask $ TermStart ("Task " <> show (i :: Int)) taskName
            errorToEither task >>= \case
              Left e -> liftIO $ sink $ EventIO do
                forM_ e.getMessages $ addTerm termTask . TermError
                updateStatus \case
                  (done, total, success, failure, tid') -> (done + 1, total, success, failure + 1, tid')
              Right fs -> liftIO $ sink $ EventIO do
                addTerm termTask $ TermSuccess case fs of
                  [] -> ""
                  _  -> unlines $ "Created files:" : map ("  " <>) fs
                updateStatus \case
                  (done, total, success, failure, tid') -> (done + 1, total, fs : success, failure, tid')
          liftIO do
            putMVar taskStatus $ Right (0, length tasks, [], 0, tid)
            cbStart'
            updateStatus id

  cbCancel <- makeCallback \_ _ -> sink $ EventIO cancelTasks
  [C.block| void { $(Fl_Button* cancelButton)->callback($(Fl_Callback cbCancel)); } |]
  cbShow <- makeCallback \_ _ -> sink $ EventIO do
    res <- takeMVar taskStatus
    case res of
      Left (Just (dir, files)) -> osShowFolder dir files
      _                        -> return ()
    putMVar taskStatus res
  [C.block| void { $(Fl_Button* showButton)->callback($(Fl_Callback cbShow)); } |]

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
  -> Ptr Fl_Group
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
        newSTFS <- modifyMVar loadedSTFS \stfs -> (\x -> (x, x)) <$> f stfs
        sink $ EventIO do
          if null newSTFS
            then do
              [C.block| void { $(Fl_Button* btnCON)->deactivate(); } |]
              [C.block| void { $(Fl_Button* btnLIVE)->deactivate(); } |]
              [C.block| void { $(Fl_Button* btnRaw)->deactivate(); } |]
            else do
              [C.block| void { $(Fl_Button* btnCON)->activate(); } |]
              [C.block| void { $(Fl_Button* btnLIVE)->activate(); } |]
              [C.block| void { $(Fl_Button* btnRaw)->activate(); } |]
          let gameLabel = case newSTFS of
                info : _ -> T.unlines
                  [ "Game (from first in list):"
                  , info.stfsMeta.md_TitleName
                  , "(" <> T.toUpper (T.pack $ showHex info.stfsMeta.md_TitleID "") <> ")"
                  ]
                []       -> ""
          withUTF8 gameLabel \clbl ->
            [C.block| void { $(Fl_Box* gameBox)->copy_label($(char* clbl)); } |]
  group <- fileLoadWindow filesRect sink "CON/LIVE" "CON/LIVE" modifyButtons [] searchSTFS
    \info -> let
      entry = T.pack $ stfsPath info
      sublines = concat
        [ take 1 info.stfsMeta.md_DisplayName
        , return $ T.unwords
          [ "Game:"
          , info.stfsMeta.md_TitleName
          , "(" <> T.toUpper (T.pack $ showHex info.stfsMeta.md_TitleID "") <> ")"
          ]
        ]
      in (entry, sublines)

  let doPrompt output = sink $ EventIO do
        stfs <- readMVar loadedSTFS
        picker <- [C.exp| Fl_Native_File_Chooser* {
          new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_SAVE_FILE)
        } |]
        [C.block| void { $(Fl_Native_File_Chooser* picker)->title("Save STFS file"); } |]
        case stfs of
          f : _ -> withUTF8 (takeDirectory $ stfsPath f) \cdir ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->directory($(char* cdir)); } |]
          _     -> return ()
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          f <- peekUTF8 cstr
          sink $ EventOnyx $ startTasks let
            task = do
              packName <- stackIO $ peekInputValue nameInput
              packDesc <- stackIO $ peekInputValue descInput
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
        [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]

  gameBox <- withRect gameRect \bx by bw bh ->
    [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]

  nameInput <- withRect nameRect \ix iy iw ih -> do
    inp <- [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih), "Pack name") } |]
    return inp
  descInput <- withRect descRect \ix iy iw ih -> do
    inp <- [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih), "Pack description") } |]
    return inp

  cbCON <- makeCallback \_ _ -> doPrompt PackCON
  btnCON <- withRect conRect \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make CON pack (RB3/RB2)") } |]
    [C.block| void {
      $(Fl_Button* btn)->color($(Fl_Color taskColor));
      $(Fl_Button* btn)->callback($(Fl_Callback cbCON));
    } |]
    return btn

  cbLIVE <- makeCallback \_ _ -> doPrompt PackLIVE
  btnLIVE <- withRect liveRect \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make LIVE pack (all other games)") } |]
    [C.block| void {
      $(Fl_Button* btn)->color($(Fl_Color taskColor));
      $(Fl_Button* btn)->callback($(Fl_Callback cbLIVE));
    } |]
    return btn

  cbRaw <- makeCallback \_ _ -> doPrompt PackExtracted
  btnRaw <- withRect rawRect \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make extracted folder") } |]
    [C.block| void {
      $(Fl_Button* btn)->color($(Fl_Color taskColor));
      $(Fl_Button* btn)->callback($(Fl_Callback cbRaw));
    } |]
    return btn

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]

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

pageQuickConvertRB
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
pageQuickConvertRB sink rect tab startTasks = mdo
  loadedFiles <- newMVar []
  let (mainRect, processorsRect) = chopRight 200 rect
      (filesRect, trimClock 0 0 5 0 -> bottomRect) = chopBottom 131 mainRect
      (trimClock 5 10 5 10 -> row2, row34) = chopBottom 84 bottomRect
      [row3, row4] = map (trimClock 5 10 5 10) $ splitVertN 2 row34
      computePacks = runMaybeT do
        files <- lift $ readMVar loadedFiles
        maxSizeInput <- MaybeT $ readIORef packMaxSizeRef
        maxSizeMB <- MaybeT $ readMaybe . T.unpack <$> peekInputValue maxSizeInput
        let maxSizeBytes = maxSizeMB * 1024 * 1024
        return $ organizePacks maxSizeBytes $ concatMap (.quickInputSongs) files
      updatePackGo = readIORef packGoRef >>= \case
        Nothing -> return ()
        Just go -> computePacks >>= \case
          Nothing -> do
            [C.block| void { $(Fl_Button* go)->label("Invalid pack size"); } |]
            [C.block| void { $(Fl_Button* go)->deactivate(); } |]
          Just [] -> do
            [C.block| void { $(Fl_Button* go)->label("No songs added"); } |]
            [C.block| void { $(Fl_Button* go)->deactivate(); } |]
          Just packs -> do
            let lbl = case length packs of
                  1 -> "Make 1 pack"
                  n -> "Make " <> show n <> " packs"
            withUTF8 lbl \clbl ->
              [C.block| void { $(Fl_Button* go)->copy_label($(char* clbl)); } |]
            [C.block| void { $(Fl_Button* go)->activate(); } |]
      updateInPlace = readIORef inPlaceGoRef >>= \case
        Nothing -> return ()
        Just go -> do
          files <- readMVar loadedFiles
          let isValidInPlace :: QuickInput -> Bool
              isValidInPlace qinput = case qinput.quickInputFormat of
                QCInputTwoWay _ -> True
                _               -> False
          if all isValidInPlace files
            then do
              [C.block| void { $(Fl_Button* go)->label("Start"); } |]
              [C.block| void { $(Fl_Button* go)->activate(); } |]
            else do
              [C.block| void { $(Fl_Button* go)->label(
                "Unsupported format: transform in place supports CON/LIVE/PKG"
              ); } |]
              [C.block| void { $(Fl_Button* go)->deactivate(); } |]
      updateFiles f = do
        modifyMVar_ loadedFiles f
        sink $ EventIO $ updatePackGo >> updateInPlace
  (filesGroup, clearFiles) <- fileLoadWindow' filesRect sink "Rock Band song" "Rock Band songs" updateFiles [] searchQuickSongs
    \qinput -> let
      entry = T.pack qinput.quickInputPath
      subline = T.pack case length qinput.quickInputSongs of
        1 -> "1 song"
        n -> show n <> " songs"
      in (entry, [subline])

  -- right side: processors
  packProcessors <- withRect (trimClock 5 5 5 0 processorsRect) \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let processorBox = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 40))
      procFolder f qsong = do
        newFiles <- f qsong.quickSongFiles
        return qsong { quickSongFiles = newFiles }
      processors :: [(T.Text, T.Text, QuickSong -> Onyx QuickSong)]
      processors =
        [ (,,) "Black VENUE"
          "Replaces the VENUE track with one that tries to provide as close to a black background as possible. Currently works on pre-RB3 songs, and songs in RBN2 format; does not work with RB3 songs using .milo venue format."
          \qsong -> do
            let isRB3 = qsong.quickSongDTA.qdtaRB3
            procFolder (QRB.applyToMIDI $ return . blackVenue isRB3) qsong
        , (,,) "No Overdrive"
          "Removes overdrive phrases from all instruments."
          $ procFolder $ QRB.applyToMIDI $ return . noOverdrive
        , (,,) "No lanes (G/B/K)"
          "Removes trill and tremolo lanes from Guitar/Bass/Keys and their Pro versions. Does not remove glissandos from Pro Keys."
          $ procFolder $ QRB.applyToMIDI $ return . noLanesGBK
        , (,,) "No lanes (drums)"
          "Removes one-pad and two-pad rolls/swells from Drums."
          $ procFolder $ QRB.applyToMIDI $ return . noLanesDrums
        , (,,) "No drum fills"
          "Removes all activation fills from Drums. Does not remove a Big Rock Ending. This function is mostly useful for recording videos."
          $ procFolder $ QRB.applyToMIDI $ return . noDrumFills
        , (,,) "Force 22-fret protar"
          "For Pro Guitar/Bass with separate Mustang and Squier versions, removes the Mustang version, so all controllers will play the Squier version. This function is mostly useful for recording videos."
          $ procFolder $ QRB.applyToMIDI $ return . mustang22
        , (,,) "Unmute >22-fret protar"
          "Any muted notes over fret 22 on Pro Guitar/Bass are unmuted. This allows you to pass them through Magma by muting, and then unmute them to record videos. Note, such notes cannot be hit by normal input."
          $ procFolder $ QRB.applyToMIDI $ return . unmuteOver22
        , (,,) "Decompress .milo_*"
          "Converts any .milo_xbox, etc. files to uncompressed form. ForgeTool may require this step to read the files for RB4 conversion."
          $ procFolder decompressMilos
        , (,,) "Author to DTA tag"
          "Copies author information from the C3 Magma comment format to the RB2/RB3 Deluxe game-readable format."
          $ return . transferAuthorToTag
        , (,,) "Strip title tags"
          "Removes the following tags from song titles: (2x Bass Pedal), and author signatures such as (Z) (X) (O) etc."
          $ return . stripTitleTags
        , (,,) "Note shuffle"
          "Shuffles notes on 5-fret and drum parts."
          \qsong -> do
            shuffled <- noteShuffle qsong
            seed <- randomIO
            return $ addTitleTag "(Shuffle)" $ refreshSong seed shuffled
        ]
  processorGetters <- forM processors \(label, tooltip, processor) -> do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    withUTF8 label \clbl ->
      [C.block| void { $(Fl_Check_Button* check)->copy_label($(char* clbl)); } |]
    withUTF8 tooltip \ctip ->
      [C.block| void { $(Fl_Check_Button* check)->copy_tooltip($(char* ctip)); } |]
    return do
      checked <- checkValue check
      return if checked then processor else return
  let getSongTransform = do
        procs <- sequence processorGetters
        return $ foldr (>=>) return procs
  getDecryptMOGGs <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Decrypt MOGGs") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "Decrypt all MOGG files. Note that unencrypted MOGGs will not work on PS3 RB3 without RB3Enhanced."
    ); } |]
    return $ checkValue check
  [C.block| void { $(Fl_Pack* packProcessors)->end(); } |]

  -- row2: select mode
  let withQCMode mode = sink $ EventIO do
        if mode == QCInPlace   then [C.block| void { $(Fl_Group* inPlace)->show(); } |]     else [C.block| void { $(Fl_Group* inPlace)->hide(); } |]
        if mode == QCOneToOne  then [C.block| void { $(Fl_Group* oneToOne)->show(); } |]    else [C.block| void { $(Fl_Group* oneToOne)->hide(); } |]
        if mode == QCMakePacks then [C.block| void { $(Fl_Group* makePacks)->show(); } |]   else [C.block| void { $(Fl_Group* makePacks)->hide(); } |]
        if mode == QCMakeSongs then [C.block| void { $(Fl_Group* makeSongs)->show(); } |]   else [C.block| void { $(Fl_Group* makeSongs)->hide(); } |]
        if mode == QCDolphin   then [C.block| void { $(Fl_Group* makeDolphin)->show(); } |] else [C.block| void { $(Fl_Group* makeDolphin)->hide(); } |]
  makeModeDropdown row2
    [ ("Transform in place: replace input files with new versions", QCInPlace  )
    , ("One to one: each input file produces a new output file"   , QCOneToOne )
    , ("Make packs: combine songs up to a maximum file size"      , QCMakePacks)
    , ("Make songs: produce a single file for each song"          , QCMakeSongs)
    , ("Make Dolphin (Wii) pack"                                  , QCDolphin  )
    ] withQCMode

  let quickTemplateRB :: Preferences -> QuickConvertFormat -> FilePath -> T.Text -> T.Text -> Maybe QuickDTA -> FilePath
      quickTemplateRB prefs fmt fin ext txt qdta = fixXbox $ validFileName NameRulePC $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
        [ T.intercalate (T.pack $ takeDirectory fin) . T.splitOn "%input_dir%"
        , T.intercalate (validFileNamePiece NameRulePC $ dropExt $ T.pack $ takeFileName fin) . T.splitOn "%input_base%"
        , T.intercalate ext . T.splitOn "%ext%"
        , T.intercalate (validFileNamePiece NameRulePC $ maybe "" (.qdtaTitle) qdta) . T.splitOn "%title%"
        , T.intercalate (validFileNamePiece NameRulePC $ fromMaybe "" $ qdta >>= (.qdtaArtist)) . T.splitOn "%artist%"
        ] where dropExt f = fromMaybe f $ T.stripSuffix ".pkg" f
                fixXbox = case fmt of
                  QCFormatCON  -> trimXbox prefs
                  QCFormatLIVE -> trimXbox prefs
                  QCFormatPKG  -> id

      artistTitle :: QuickSong -> T.Text
      artistTitle qsong = T.intercalate " - " $ concat
        [ toList qsong.quickSongDTA.qdtaArtist
        , [qsong.quickSongDTA.qdtaTitle]
        ]

      defaultTitle qsongs = case qsongs of
        [qsong]   -> artistTitle qsong
        []        -> "(empty)"
        qsong : _ -> T.pack (show $ length qsongs) <> " songs incl. " <> artistTitle qsong

  -- row3/row4: mode-specific stuff
  let (formatArea, trimClock 0 0 0 10 -> ps3OptionsArea) = chopLeft 150 row3
      (ps3EncryptArea, ps3FolderArea) = chopLeft 225 ps3OptionsArea
      makeCheckEncrypt = do
        btn <- withRect ps3EncryptArea \cx cy cw ch ->
          [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Encrypt .mid.edat") } |]
        when ?preferences.prefPS3Encrypt $
          [C.block| void { $(Fl_Check_Button* btn)->value(1); } |]
        return $ checkValue btn
      makeFormatSelect :: IO () -> IO () -> IO () -> IO () -> IO (IO QuickConvertFormatOutput)
      makeFormatSelect ps3On ps3Off foldersOn foldersOff = do
        ref <- newIORef $ QCOutputTwoWay QCFormatCON
        makeModeDropdown formatArea
          [ ("CON (360)"    , QCOutputTwoWay QCFormatCON )
          , ("LIVE (360)"   , QCOutputTwoWay QCFormatLIVE)
          , ("PKG (PS3)"    , QCOutputTwoWay QCFormatPKG )
          , ("Folders (PS3)", QCOutputLoosePS3           )
          ] \x -> sink $ EventIO do
            writeIORef ref x
            case x of
              QCOutputTwoWay QCFormatPKG -> ps3On
              QCOutputLoosePS3           -> ps3On
              _                          -> ps3Off
            case x of
              QCOutputLoosePS3 -> foldersOn
              _                -> foldersOff
        return $ readIORef ref
      modeGroup :: IO () -> IO (Ptr Fl_Group)
      modeGroup inner = do
        grp <- withRect row34 \gx gy gw gh ->
          [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
        inner
        [C.block| void { $(Fl_Group* grp)->end(); } |]
        return grp

  -- Transform in place
  inPlaceGoRef <- newIORef Nothing
  inPlace <- modeGroup do
    _ <- withRect formatArea \bx by bw bh ->
      [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "For PS3 files:") } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ ("Keep original USRDIR subfolders", Nothing)
      , ("Combine into one new USRDIR subfolder", Just QCOneFolder)
      , ("Each song gets a new USRDIR subfolder", Just QCSeparateFolders)
      ]
    cbGo <- makeCallback \_ _ -> sink $ EventOnyx do
      files <- stackIO $ readMVar loadedFiles
      enc <- stackIO getEncrypt
      ps3Folder <- stackIO getFolderSetting
      songTransform <- stackIO getSongTransform
      decryptMOGGs <- stackIO getDecryptMOGGs
      stackIO clearFiles
      startTasks $ files <&> \qinput -> let
        fin = qinput.quickInputPath
        qsongs = qinput.quickInputSongs
        task = do
          qsongs' <- mapM songTransform qsongs
          let tmp = fin <> ".tmp"
              isRB3 = any (.quickSongDTA.qdtaRB3) qsongs'
              ps3Settings = QuickPS3Settings
                { qcPS3Folder      = ps3Folder
                , qcPS3EncryptMIDI = enc
                , qcPS3EncryptMOGG = not decryptMOGGs
                , qcPS3RB3         = isRB3
                }
              xboxSettings live = stackIO $
                (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                (maybe "Onyx Quick Convert" (T.concat . take 1 . (.md_DisplayName       )) qinput.quickInputXbox)
                (maybe ""                   (T.concat . take 1 . (.md_DisplayDescription)) qinput.quickInputXbox)
                live
          case qinput.quickInputFormat of
            QCInputTwoWay fmt -> case fmt of
              QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongs' opts tmp
              QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongs' opts tmp
              QCFormatPKG  -> saveQuickSongsPKG  qsongs' ps3Settings tmp
            fmt -> fatal $ "Unsupported format for transform mode (" <> show fmt <> ")"
          stackIO $ Dir.renameFile tmp fin
          return [fin]
        in (fin, task)
    btnGo <- withRect row4 \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Start") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]
      return btn
    writeIORef inPlaceGoRef $ Just btnGo

  -- One to one
  oneToOne <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      ([C.block| void { $(Fl_Group* ps3Options)->activate(); } |])
      ([C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |])
      ([C.block| void { $(Fl_Group* groupStartFile)->hide(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->show(); } |])
      ([C.block| void { $(Fl_Group* groupStartFile)->show(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->hide(); } |])
    ps3Options <- withRect ps3OptionsArea \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ ("Keep original USRDIR subfolders (if any)", Nothing)
      , ("Combine into one new USRDIR subfolder per file", Just QCOneFolder)
      , ("Each song gets a new USRDIR subfolder", Just QCSeparateFolders)
      ]
    [C.block| void { $(Fl_Group* ps3Options)->end(); } |]
    groupStartFile <- withRect row4 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    void $ makeTemplateRunner'
      sink
      row4
      "Start"
      "%input_dir%/%input_base%_convert%ext%"
      \template -> sink $ EventOnyx $ stackIO getFormat >>= \case
        QCOutputLoosePS3   -> return ()
        QCOutputTwoWay fmt -> do
          files <- stackIO $ readMVar loadedFiles
          enc <- stackIO getEncrypt
          ps3Folder <- stackIO getFolderSetting
          songTransform <- stackIO getSongTransform
          decryptMOGGs <- stackIO getDecryptMOGGs
          newPreferences <- readPreferences
          startTasks $ files <&> \qinput -> let
            fin = qinput.quickInputPath
            qsongs = qinput.quickInputSongs
            task = do
              qsongs' <- mapM songTransform qsongs
              let ext = case fmt of
                    QCFormatCON  -> ""
                    QCFormatLIVE -> ""
                    QCFormatPKG  -> ".pkg"
                  fout = quickTemplateRB newPreferences fmt fin ext template Nothing
                  isRB3 = any (.quickSongDTA.qdtaRB3) qsongs'
                  ps3Settings = QuickPS3Settings
                    { qcPS3Folder      = ps3Folder
                    , qcPS3EncryptMIDI = enc
                    , qcPS3EncryptMOGG = not decryptMOGGs
                    , qcPS3RB3         = isRB3
                    }
                  xboxSettings live = stackIO $
                    (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                    (maybe (defaultTitle qsongs') (T.concat . take 1 . (.md_DisplayName       )) qinput.quickInputXbox)
                    (maybe ""                     (T.concat . take 1 . (.md_DisplayDescription)) qinput.quickInputXbox)
                    live
              case fmt of
                QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongs' opts fout
                QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongs' opts fout
                QCFormatPKG  -> saveQuickSongsPKG qsongs' ps3Settings fout
              return [fout]
            in (fin, task)
    [C.block| void { $(Fl_Group* groupStartFile)->end(); } |]
    groupStartLoosePS3 <- withRect row4 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    cbLoosePS3 <- makeCallback \_ _ -> do
      files <- readMVar loadedFiles
      enc <- getEncrypt
      ps3Folder <- getFolderSetting
      songTransform <- getSongTransform
      decryptMOGGs <- getDecryptMOGGs
      sink $ EventOnyx do
        defDir <- getDefaultPS3Dir
        stackIO $ askFolder defDir \dout -> do
          sink $ EventOnyx $ startTasks $ files <&> \qinput -> let
            fin = qinput.quickInputPath
            qsongs = qinput.quickInputSongs
            task = do
              qsongs' <- mapM songTransform qsongs
              let isRB3 = any (.quickSongDTA.qdtaRB3) qsongs'
                  ps3Settings = QuickPS3Settings
                    { qcPS3Folder      = ps3Folder
                    , qcPS3EncryptMIDI = enc
                    , qcPS3EncryptMOGG = not decryptMOGGs
                    , qcPS3RB3         = isRB3
                    }
              saveQuickSongsPS3Folder qsongs' ps3Settings dout
            in (fin, task)
    withRect row4 \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Start") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbLoosePS3));
      } |]
    [C.block| void { $(Fl_Group* groupStartLoosePS3)->end(); } |]

  -- Make packs
  packGoRef <- newIORef Nothing
  packMaxSizeRef <- newIORef Nothing
  makePacks <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      ([C.block| void { $(Fl_Group* ps3Options)->activate(); } |])
      ([C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |])
      (return ())
      (return ())
    ps3Options <- withRect ps3OptionsArea \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePS3PackPresetDropdown sink ps3FolderArea
    [C.block| void { $(Fl_Group* ps3Options)->end(); } |]
    let (trimClock 0 10 0 150 -> maxSizeArea, goArea) = chopLeft 300 row4
    maxSizeInput <- withRect maxSizeArea \ix iy iw ih -> do
      inp <- [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih), "Max Pack Size (MiB)") } |]
      [C.block| void {
        $(Fl_Input* inp)->labelsize(13);
        $(Fl_Input* inp)->align(FL_ALIGN_LEFT);
        $(Fl_Input* inp)->value("4000");
      } |]
      return inp
    cbMaxSize <- makeCallback \_ _ -> updatePackGo
    [C.block| void {
      $(Fl_Input* maxSizeInput)->callback($(Fl_Callback cbMaxSize));
      $(Fl_Input* maxSizeInput)->when(FL_WHEN_CHANGED);
    } |]
    writeIORef packMaxSizeRef $ Just maxSizeInput
    cbGo <- makeCallback \_ _ -> do
      enc <- getEncrypt
      ps3Folder <- getFolderSetting
      songTransform <- getSongTransform
      decryptMOGGs <- getDecryptMOGGs
      computePacks >>= \case
        Nothing    -> return ()
        Just packs -> let
          packNumber n = case packs of
            [_] -> ""
            _   -> "-" <> show n
          packNumberUSRDIR n = case packs of
            [_] -> ""
            _   -> "-" <> B8.pack (show n)
          in sink $ EventOnyx $ stackIO getFormat >>= \case
            QCOutputLoosePS3 -> do
              defDir <- getDefaultPS3Dir
              stackIO $ askFolder defDir \dout -> do
                sink $ EventOnyx $ startTasks $ zip [1..] packs <&> \(i, qsongs) -> let
                  task = do
                    mapM_ (lg . T.unpack . artistTitle) qsongs
                    qsongs' <- mapM songTransform qsongs
                    let isRB3 = any (.quickSongDTA.qdtaRB3) qsongs'
                        ps3Folder' = case ps3Folder of
                          QCCustomFolder bs -> QCCustomFolder $ bs <> packNumberUSRDIR i
                          _                 -> ps3Folder
                        ps3Settings = QuickPS3Settings
                          { qcPS3Folder      = Just ps3Folder'
                          , qcPS3EncryptMIDI = enc
                          , qcPS3EncryptMOGG = not decryptMOGGs
                          , qcPS3RB3         = isRB3
                          }
                    saveQuickSongsPS3Folder qsongs' ps3Settings dout
                  in ("Pack #" <> show (i :: Int), task)
            QCOutputTwoWay fmt -> stackIO do
              picker <- [C.exp| Fl_Native_File_Chooser* {
                new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_SAVE_FILE)
              } |]
              let titleStr = case packs of
                    [_] -> "Save pack file"
                    _   -> "Select template for packs (a number will be added for each pack)"
              withUTF8 (titleStr :: String) \ctitle ->
                [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* ctitle)); } |]
              result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
              when (result == 0) do
                cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
                userPath <- peekUTF8 cstr
                let getOutputPath n = case fmt of
                      QCFormatPKG -> dropExtension userPath <> packNumber n <> ".pkg"
                      _           -> userPath <> packNumber n
                sink $ EventOnyx do
                  newPreferences <- readPreferences
                  startTasks $ zip [1..] packs <&> \(i, qsongs) -> let
                    fout = getOutputPath i
                    task = do
                      mapM_ (lg . T.unpack . artistTitle) qsongs
                      qsongs' <- mapM songTransform qsongs
                      let isRB3 = any (.quickSongDTA.qdtaRB3) qsongs'
                          maybeEncoding = if isRB3
                            then newPreferences.prefPackEncoding
                            else Just Latin1
                          qsongsReencode = case maybeEncoding of
                            Nothing          -> qsongs'
                            Just newEncoding -> qsongs' <&> \qsong -> qsong
                              { quickSongDTA = enforceEncoding newEncoding qsong.quickSongDTA
                              }
                          ps3Folder' = case ps3Folder of
                            QCCustomFolder bs -> QCCustomFolder $ bs <> packNumberUSRDIR i
                            _                 -> ps3Folder
                          ps3Settings = QuickPS3Settings
                            { qcPS3Folder      = Just ps3Folder'
                            , qcPS3EncryptMIDI = enc
                            , qcPS3EncryptMOGG = not decryptMOGGs
                            , qcPS3RB3         = isRB3
                            }
                          xboxSettings live = stackIO $
                            (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                            (defaultTitle qsongsReencode)
                            ""
                            live
                      case fmt of
                        QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongsReencode opts fout
                        QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS decryptMOGGs qsongsReencode opts fout
                        QCFormatPKG  -> saveQuickSongsPKG qsongs' ps3Settings fout
                      return [fout]
                    in ("Pack #" <> show (i :: Int), task)
              [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]
    btnGo <- withRect goArea \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Save pack as…") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]
      return btn
    writeIORef packGoRef $ Just btnGo

  -- Make songs
  makeSongs <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      ([C.block| void { $(Fl_Group* ps3Options)->activate(); } |])
      ([C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |])
      ([C.block| void { $(Fl_Group* groupStartFile)->hide(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->show(); } |])
      ([C.block| void { $(Fl_Group* groupStartFile)->show(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->hide(); } |])
    ps3Options <- withRect ps3OptionsArea \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    getEncrypt <- makeCheckEncrypt
    [C.block| void { $(Fl_Group* ps3Options)->end(); } |]
    groupStartFile <- withRect row4 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    void $ makeTemplateRunner'
      sink
      row4
      "Select folder…"
      "%artist% - %title%%ext%"
      \template -> sink $ EventIO $ getFormat >>= \case
        QCOutputLoosePS3   -> return ()
        QCOutputTwoWay fmt -> do
          files <- readMVar loadedFiles
          enc <- getEncrypt
          songTransform <- getSongTransform
          decryptMOGGs <- getDecryptMOGGs
          askFolder (takeDirectory . (.quickInputPath) <$> listToMaybe files) \dout -> do
            let inputSongs = files >>= \f -> map (f,) f.quickInputSongs
            sink $ EventOnyx do
              newPreferences <- readPreferences
              startTasks $ inputSongs <&> \(qinput, qsong) -> let
                ext = case fmt of
                  QCFormatCON  -> ""
                  QCFormatLIVE -> ""
                  QCFormatPKG  -> ".pkg"
                fout = dout </> quickTemplateRB newPreferences fmt qinput.quickInputPath ext template (Just qsong.quickSongDTA)
                isRB3 = qsong.quickSongDTA.qdtaRB3
                task = do
                  qsong' <- songTransform qsong
                  let ps3Settings = QuickPS3Settings
                        { qcPS3Folder      = Just QCSeparateFolders
                        , qcPS3EncryptMIDI = enc
                        , qcPS3EncryptMOGG = not decryptMOGGs
                        , qcPS3RB3         = isRB3
                        }
                      xboxSettings live = stackIO $
                        (if isRB3 then STFS.rb3STFSOptions else STFS.rb2STFSOptions)
                        (artistTitle qsong)
                        ""
                        live
                  case fmt of
                    QCFormatCON  -> xboxSettings False >>= \opts -> saveQuickSongsSTFS decryptMOGGs [qsong'] opts fout
                    QCFormatLIVE -> xboxSettings True  >>= \opts -> saveQuickSongsSTFS decryptMOGGs [qsong'] opts fout
                    QCFormatPKG  -> saveQuickSongsPKG [qsong'] ps3Settings fout
                  return [fout]
                in (T.unpack $ artistTitle qsong, task)
    [C.block| void { $(Fl_Group* groupStartFile)->end(); } |]
    groupStartLoosePS3 <- withRect row4 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    cbLoosePS3 <- makeCallback \_ _ -> do
      files <- readMVar loadedFiles
      enc <- getEncrypt
      songTransform <- getSongTransform
      decryptMOGGs <- getDecryptMOGGs
      sink $ EventOnyx do
        defDir <- getDefaultPS3Dir
        stackIO $ askFolder defDir \dout -> do
          let inputSongs = files >>= \f -> map (f,) f.quickInputSongs
          sink $ EventOnyx $ startTasks $ inputSongs <&> \(_qinput, qsong) -> let
            isRB3 = qsong.quickSongDTA.qdtaRB3
            task = do
              qsong' <- songTransform qsong
              let ps3Settings = QuickPS3Settings
                    { qcPS3Folder      = Just QCSeparateFolders
                    , qcPS3EncryptMIDI = enc
                    , qcPS3EncryptMOGG = not decryptMOGGs
                    , qcPS3RB3         = isRB3
                    }
              saveQuickSongsPS3Folder [qsong'] ps3Settings dout
            in (T.unpack $ artistTitle qsong, task)
    withRect row4 \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Start") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbLoosePS3));
      } |]
    [C.block| void { $(Fl_Group* groupStartLoosePS3)->end(); } |]

  -- Wii (Dolphin) pack
  makeDolphin <- modeGroup $ mdo
    boxPrev <- withRect row3 \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Try to generate preview audio") } |]
    cbGo <- makeCallback \_ _ -> sink $ EventIO do
      files <- readMVar loadedFiles
      tryPreview <- checkValue boxPrev
      songTransform <- getSongTransform
      askFolder ?preferences.prefDirWii \dout -> sink $ EventOnyx $ startTasks let
        settings = QuickDolphinSettings
          { qcDolphinPreview = tryPreview
          }
        task = do
          qsongs <- mapM songTransform $ concatMap (.quickInputSongs) files
          saveQuickSongsDolphin qsongs settings dout
        in [("Make Dolphin pack", task)]
    withRect row4 \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Select folder…") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* filesGroup)); } |]

pageRenumberRR
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
pageRenumberRR sink rect tab startTasks = mdo
  loadedFiles <- newMVar []
  let (filesRect, trimClock 5 10 10 10 -> bottomRect) = chopBottom 47 rect
      updateFiles = modifyMVar_ loadedFiles
  (filesGroup, clearFiles) <- fileLoadWindow' filesRect sink "RR song" "RR songs" updateFiles [] searchRockRevolution
    \(path, songs) -> let
      entry = T.pack path
      subline = case length songs of
        1 -> "1 song"
        n -> T.pack $ show n <> " songs"
      in (entry, [subline])

  cbGo <- makeCallback \_ _ -> do
    songsOld <- readMVar loadedFiles
    sink $ EventOnyx do
      stackIO clearFiles
      startTasks $ return let
        name = "Renumbering Rock Revolution song IDs"
        task = do
          songsNew <- stackIO $ RR.renumberFrom 2000 songsOld
          forM songsNew \(f, songs) -> do
            lg f
            forM_ songs \song -> do
              titleArtist <- stackIO $ RR.getTitleArtist song
              lg $ T.unpack $ song.songID <> ": " <> case titleArtist of
                Nothing              -> "(unknown)"
                Just (title, artist) -> title <> " (" <> artist <> ")"
            let tmp = f <> ".tmp"
            case songs of
              []       -> return ()
              song : _ -> stackIO do
                if song.ps3
                  then RR.saveSongsPS3 tmp songs
                  else RR.saveSongs360 tmp songs
                Dir.renameFile tmp f
            return f
        in (name, task)
  withRect bottomRect \bx by bw bh -> do
    btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Start") } |]
    [C.block| void {
      $(Fl_Button* btn)->color($(Fl_Color taskColor));
      $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
    } |]

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* filesGroup)); } |]

data QuickFoFTask
  = QFoFInPlace
  | QFoFKeepOriginal
  | QFoFDeleteOriginal
  deriving (Eq)

pageQuickConvertCH
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
pageQuickConvertCH sink rect tab startTasks = mdo
  loadedFiles <- newMVar []
  let (mainRect, processorsRect) = chopRight 200 rect
      (filesRect, trimClock 0 0 5 0 -> bottomRect) = chopBottom 131 mainRect
      (trimClock 5 10 5 10 -> row2, row34) = chopBottom 84 bottomRect
      [row3, row4] = map (trimClock 5 10 5 10) $ splitVertN 2 row34
      updateFiles = modifyMVar_ loadedFiles
  (filesGroup, clearFiles) <- fileLoadWindow' filesRect sink "CH song" "CH songs" updateFiles [] searchQuickFoF
    \qfof -> let
      entry = T.pack $ qfof.location
      subline = case qfof.format of
        FoFFormat FoFFolder -> "folder format"
        FoFFormat FoFSNG    -> ".sng format"
        FoFEncore           -> "Encore format"
      in (entry, [subline])

  -- right side: processors to transform midis and other files
  packProcessors <- withRect (trimClock 5 5 5 0 processorsRect) \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let processorBox = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 45))

  (checkMIDI, processorMIDI) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), ".chart to MIDI") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "Converts all .chart files to .mid."
    ); } |]
    cbCheck <- makeCallback \_ _ -> do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkPSTapOpen)->value(0); } |]
    [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]
    return (check, do
      checked <- checkValue check
      return if checked then convertMIDI else return
      )

  (checkPSTapOpen, processorPSTapOpen) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "PS tap/open format") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "Encodes five-fret tap and open notes using the older Phase Shift format (System Exclusive messages). This will pull back open extended sustains to not overlap other notes, and remove open notes in chords."
    ); } |]
    cbCheck <- makeCallback \_ _ -> do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkMIDI)->value(1); } |]
    [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]
    return (check, do
      checked <- checkValue check
      return if checked then psTapOpenFormats else return
      )

  (checkOgg, processorOgg) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Audio to .ogg") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "Reencode audio files as Ogg Vorbis if they are not already."
    ); } |]
    cbCheck <- makeCallback \_ _ -> do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkOpus)->value(0); } |]
    [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]
    return (check, do
      checked <- checkValue check
      return if checked then stackIO . convertOgg else return
      )
  (checkOpus, processorOpus) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Audio to .opus") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "Reencode audio file as Opus if they are not already."
    ); } |]
    cbCheck <- makeCallback \_ _ -> do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkOgg)->value(0); } |]
    [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]
    return (check, do
      checked <- checkValue check
      return if checked then stackIO . convertOpus else return
      )

  processorRealToElite <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Real Drums to Elite") } |]
    [C.block| void { $(Fl_Check_Button* check)->tooltip(
      "If a song has Phase Shift Real Drums charted, adds a conversion to Elite Drums."
    ); } |]
    return do
      checked <- checkValue check
      return if checked then fillEliteFromPSReal else return

  let getTransform = do
        procs <- sequence [processorMIDI, processorPSTapOpen, processorOgg, processorOpus, processorRealToElite]
        return $ foldr (>=>) return procs
  [C.block| void { $(Fl_Pack* packProcessors)->end(); } |]

  -- row2: select mode
  let withQCMode mode = sink $ EventIO do
        if mode == Nothing        then [C.block| void { $(Fl_Group* inPlace)->show(); } |]   else [C.block| void { $(Fl_Group* inPlace)->hide(); } |]
        if mode == Just FoFFolder then [C.block| void { $(Fl_Group* toFolders)->show(); } |] else [C.block| void { $(Fl_Group* toFolders)->hide(); } |]
        if mode == Just FoFSNG    then [C.block| void { $(Fl_Group* toSNGs)->show(); } |]    else [C.block| void { $(Fl_Group* toSNGs)->hide(); } |]
  makeModeDropdown row2
    [ ("Transform in place: replace input files with new versions", Nothing       )
    , ("To folders: save each song as a folder"                   , Just FoFFolder)
    , ("To SNG: save each song as an .sng file"                   , Just FoFSNG   )
    ] withQCMode

  let quickTemplate :: FilePath -> T.Text -> T.Text -> Maybe QuickFoF -> FilePath
      quickTemplate fin ext txt qfof = validFileName NameRulePCUnicode $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
        [ T.intercalate (T.pack $ takeDirectory fin) . T.splitOn "%input_dir%"
        , T.intercalate (validFileNamePiece NameRulePCUnicode $ dropExt $ T.pack $ takeFileName fin) . T.splitOn "%input_base%"
        , T.intercalate ext . T.splitOn "%ext%"
        , T.intercalate (validFileNamePiece NameRulePCUnicode $ fofMetadata "name") . T.splitOn "%title%"
        , T.intercalate (validFileNamePiece NameRulePCUnicode $ fofMetadata "artist") . T.splitOn "%artist%"
        ] where dropExt f = fromMaybe f $ T.stripSuffix ".sng" f
                fofMetadata k = fromMaybe "" $ qfof >>= lookup k . (.metadata)

  -- row3/row4: mode-specific stuff
  let modeGroup :: IO () -> IO (Ptr Fl_Group)
      modeGroup inner = do
        grp <- withRect row34 \gx gy gw gh ->
          [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
        inner
        [C.block| void { $(Fl_Group* grp)->end(); } |]
        return grp

  -- Transform in place
  inPlace <- modeGroup do
    cbGo <- makeCallback \_ _ -> do
      qfofs <- readMVar loadedFiles
      transform <- getTransform
      let warningText = T.unlines $ concat
            [ ["This will modify the selected songs, and the old versions will be deleted."]
            , do
              guard $ any (\qfof -> qfof.format == FoFEncore) qfofs
              [   ""
                , "WARNING: This will convert Encore-format songs into CH-compatible ones, removing the original files."
                , "Select a different mode if you want to keep them."
                ]
            ]
      withUTF8 warningText \cmsg -> do
        result <- [C.exp| int { fl_choice("%s", "Cancel", "Start", nullptr, $(char* cmsg)) } |]
        when (result == 1) do
          sink $ EventOnyx do
            stackIO clearFiles
            startTasks $ qfofs <&> \orig -> let
              loc = orig.location
              task = do
                qfof <- transform orig
                stackIO case qfof.format of
                  FoFFormat FoFFolder -> saveQuickFoFFolder loc qfof
                  FoFFormat FoFSNG    -> fillRequiredMetadata qfof >>= saveQuickFoFSNG loc
                  FoFEncore           -> saveQuickFoFFolder loc qfof
                return [loc]
              in (loc, task)
    withRect row4 \bx by bw bh -> do
      btn <- [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Start") } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]

  let computeTasks qfofs ext template deleteOriginals = forM qfofs \qfof -> do
        fin  <- Dir.canonicalizePath qfof.location
        fout <- Dir.canonicalizePath $ quickTemplate qfof.location ext template $ Just qfof
        let task = if fin == fout
              then QFoFInPlace
              else if deleteOriginals then QFoFDeleteOriginal else QFoFKeepOriginal
        return (qfof, fin, task, fout)
      promptTasks :: [(QuickFoF, FilePath, QuickFoFTask, FilePath)] -> IO () -> IO ()
      promptTasks tasks f = let
        numInPlace        = length [ () | (_, _, QFoFInPlace       , _) <- tasks ]
        numKeepOriginal   = length [ () | (_, _, QFoFKeepOriginal  , _) <- tasks ]
        numDeleteOriginal = length [ () | (_, _, QFoFDeleteOriginal, _) <- tasks ]
        numDeleteEncore   = length do
          (qfof, _, taskType, _) <- tasks
          guard $ qfof.format == FoFEncore
          guard $ elem taskType [QFoFInPlace, QFoFDeleteOriginal]
          return ()
        numSongs 1 = "1 song"
        numSongs n = T.pack (show n) <> " songs"
        warningText = T.unlines $ concat
          [ ["Based on the filename template, these operations will be performed:"]
          , guard (numInPlace /= 0) >> ["- " <> numSongs numInPlace <> " will be overwritten with new versions"]
          , guard (numKeepOriginal /= 0) >> ["- " <> numSongs numKeepOriginal <> " will be converted to new versions"]
          , guard (numDeleteOriginal /= 0) >> ["- " <> numSongs numDeleteOriginal <> " will be converted to new versions, and the old versions will be deleted"]
          , do
            guard $ numDeleteEncore /= 0
            ["", "WARNING: Some Encore-format songs will be removed/replaced by CH-compatible ones"]
          ]
        in withUTF8 warningText \cmsg -> do
          result <- [C.exp| int { fl_choice("%s", "Cancel", "Start", nullptr, $(char* cmsg)) } |]
          when (result == 1) f

  -- new folders
  toFolders <- modeGroup do
    checkDeleteOriginals <- withRect row3 \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Delete original files") } |]
    void $ makeTemplateRunner'
      sink
      row4
      "Start"
      "%input_dir%/%input_base%"
      \template -> do
        deleteOriginals <- checkValue checkDeleteOriginals
        qfofs <- readMVar loadedFiles
        transform <- getTransform
        tasks <- computeTasks qfofs "" template deleteOriginals
        promptTasks tasks $ sink $ EventOnyx do
          stackIO clearFiles
          startTasks $ tasks <&> \(orig, _, taskType, fout) -> let
            task = do
              qfof <- transform orig
              stackIO $ saveQuickFoFFolder fout qfof
              case taskType of
                QFoFDeleteOriginal -> stackIO $ Dir.removePathForcibly orig.location
                _                  -> return ()
              return [fout]
            in (fout, task)

  -- new sng
  toSNGs <- modeGroup do
    checkDeleteOriginals <- withRect row3 \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), "Delete original files") } |]
    void $ makeTemplateRunner'
      sink
      row4
      "Start"
      "%input_dir%/%input_base%.sng"
      \template -> do
        deleteOriginals <- checkValue checkDeleteOriginals
        qfofs <- readMVar loadedFiles
        transform <- getTransform
        tasks <- computeTasks qfofs "sng" template deleteOriginals
        promptTasks tasks $ sink $ EventOnyx do
          stackIO clearFiles
          startTasks $ tasks <&> \(orig, _, taskType, fout) -> let
            task = do
              qfof <- transform orig
              stackIO $ fillRequiredMetadata qfof >>= saveQuickFoFSNG fout
              case taskType of
                QFoFDeleteOriginal -> stackIO $ Dir.removePathForcibly orig.location
                _                  -> return ()
              return [fout]
            in (fout, task)

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* filesGroup)); } |]

miscPageMOGG
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMOGG sink rect tab startTasks = mdo
  loadedAudio <- newMVar []
  let (filesRect, startRect) = chopBottom 95 rect
      (startRect1, startRect2) = chopBottom 50 startRect
      [chopRight 5 -> (moggRect, _), chopLeft 5 -> (_, vgsRect)] = splitHorizN 2 $ trimClock 5 10 5 10 startRect1
      [chopRight 5 -> (fsb3Rect, _), chopLeft 5 -> (_, fsb4Rect)] = splitHorizN 2 $ trimClock 5 10 10 10 startRect2
      isSingleOgg = \case
        [aud] | aud.audioFormat == "Ogg Vorbis" -> Just aud
        _                                       -> Nothing
      modifyAudio :: ([AudioSpec] -> IO [AudioSpec]) -> IO ()
      modifyAudio f = do
        newAudio <- modifyMVar loadedAudio \auds -> (\x -> (x, x)) <$> f auds
        let chans = sum $ map (.audioChannels) newAudio
        sink $ EventIO do
          let moggLabel = T.unpack $ let
                op = case isSingleOgg newAudio of
                  Just _  -> "Ogg to MOGG, no re-encode"
                  Nothing -> "Combine into MOGG"
                in op <> " (" <> T.pack (show chans) <> " channels)"
              vgsLabel = T.unpack $ "Combine into VGS (" <> T.pack (show chans) <> " channels)"
              fsb3Label = T.unpack $ T.concat
                [ "Make FSB3-XMA1 ("
                , T.pack $ show $ length newAudio
                , " streams, "
                , T.pack $ show chans
                , " channels total)"
                ]
              fsb4Label = T.unpack $ T.concat
                [ "Make FSB4-XMA2 ("
                , T.pack $ show chans
                , " channels)"
                ]
          withUTF8 moggLabel \cl -> [C.block| void { $(Fl_Button* btnMogg)->copy_label($(char* cl)); } |]
          withUTF8 vgsLabel  \cl -> [C.block| void { $(Fl_Button* btnVgs)->copy_label($(char* cl)); } |]
          withUTF8 fsb3Label \cl -> [C.block| void { $(Fl_Button* btnFsb3)->copy_label($(char* cl)); } |]
          withUTF8 fsb4Label \cl -> [C.block| void { $(Fl_Button* btnFsb4)->copy_label($(char* cl)); } |]
          if chans == 0
            then forM_ [btnMogg, btnVgs, btnFsb3, btnFsb4] \b ->
              [C.block| void { $(Fl_Button* b)->deactivate(); } |]
            else forM_ [btnMogg, btnVgs, btnFsb3, btnFsb4] \b ->
              [C.block| void { $(Fl_Button* b)->activate(); } |]
  group <- fileLoadWindow filesRect sink "Audio" "Audio" modifyAudio []
    (\f -> liftIO $ ([],) . toList <$> getAudioSpec f)
    \info -> let
      entry = T.pack info.audioPath
      sublines =
        [ info.audioFormat <> ", " <> T.pack (show info.audioChannels)
          <> case info.audioChannels of 1 -> " channel"; _ -> " channels"
        , let
          time = realToFrac info.audioFrames / info.audioRate
          mins = floor $ time / 60 :: Int
          secs = realToFrac $ time - realToFrac mins * 60 :: Milli
          zero = if secs < 10 then "0" else ""
          stamp = T.pack $ show mins <> ":" <> zero <> show secs
          in stamp <> " (" <> T.pack (show info.audioFrames) <> " frames @ " <> T.pack (show info.audioRate) <> "Hz)"
        ]
      in (entry, sublines)

  let makeSaveAudioButton rect' filterStr defaultExt task' = do
        btn <- withRect rect' \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
        cb <- makeCallback \_ _ -> sink $ EventIO do
          audio <- readMVar loadedAudio
          let title = "Save " <> T.unpack defaultExt <> " file"
              preset = case audio of
                [aud] -> Just $ T.pack $ takeFileName aud.audioPath -<.> T.unpack defaultExt
                _     -> Just $ "out." <> defaultExt
          saveFileChooser (T.pack title) (T.pack filterStr) preset \f -> let
              ext = map toLower $ takeExtension f
              f' = if ext == "." ++ T.unpack defaultExt then f else f <.> T.unpack defaultExt
              in sink $ EventOnyx $ task' audio f'
        [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cb)); } |]
        return btn

  btnMogg <- makeSaveAudioButton moggRect "*.mogg" "mogg" \audio f' ->
    startTasks case isSingleOgg audio of
      Just aud -> [("Ogg to MOGG", oggToMoggFiles aud.audioPath f' >> return [f'])]
      Nothing  -> let
        task = tempDir "makemogg" \tmp -> do
          let ogg = tmp </> "temp.ogg"
          case map (.audioRate) audio of
            []             -> return ()
            rates@(r : rs) -> unless (all (== r) rs) do
              fatal $ "All files must have the same sample rate, but found: " <> show (nubOrd rates)
          ne <- maybe (fatal "No input files") return $ NE.nonEmpty audio
          src <- buildSource' $ Merge $ fmap (Input . (.audioPath)) ne
          runAudio src ogg
          oggToMoggFiles ogg f'
          return [f']
        in [("MOGG file creation", task)]

  btnVgs <- makeSaveAudioButton vgsRect "*.vgs" "vgs" \audio f' ->
    startTasks let
      task = do
        srcs <- mapM (buildSource' . Input . (.audioPath)) audio
        stackIO $ runResourceT $ writeVGSMultiRate f' $ map (mapSamples integralSample) srcs
        return [f']
      in [("VGS file creation", task)]

  btnFsb3 <- makeSaveAudioButton fsb3Rect "*.fsb" "fsb" \audio f' ->
    startTasks let
      task = tempDir "makefsb3" \tmp -> do
        wavs <- forM (zip [0..] audio) \(i, aud) -> do
          src <- buildSource' $ Input aud.audioPath
          let wav = tmp </> show (i :: Int) <.> "wav"
          runAudio src wav
          return wav
        makeXMAFSB3 [ (B8.pack $ takeFileName wav, wav) | wav <- wavs ] f'
        return [f']
      in [("FSB3 file creation", task)]

  btnFsb4 <- makeSaveAudioButton fsb4Rect "*.fsb" "fsb" \audio f' ->
    startTasks let
      task = tempDir "makefsb4" \tmp -> do
        let wav = tmp </> "audio.wav"
        audio' <- maybe (fatal "Panic! No audio files") return $ NE.nonEmpty audio
        src <- buildSource' $ Merge $ fmap (Input . (.audioPath)) audio'
        runAudio src wav
        fsb <- makeXMAPieces (fileReadable wav) >>= ghBandXMAtoFSB4
        stackIO $ BL.writeFile f' $ emitFSB fsb
        return [f']
      in [("FSB4 file creation", task)]

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]

miscPageMIDI
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMIDI sink rect tab startTasks = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  (pathInput, pickedFile) <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 120 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("MIDI or .chart"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    cbPick <- makeCallback \_ _ -> sink $ EventIO do
      picker <- [C.exp| Fl_Native_File_Chooser* {
        new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_FILE)
      } |]
      [C.block| void {
        $(Fl_Native_File_Chooser* picker)->title("Load MIDI or .chart file");
        $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi,chart}");
      } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        withUTF8 (f :: String) \cf ->
          [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
        [C.block| void { $(Fl_Input* input)->do_callback(); } |]
      [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]
    [C.block| void { $(Fl_Button* pick)->callback($(Fl_Callback cbPick)); } |]
    return (input, do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr)

  midiButtons <- sequence

    [ padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Fill in lower difficulties and drum animations") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cb <- makeCallback \_ _ -> sink $ EventIO do
        input <- pickedFile
        saveFileChooser "Save reduced MIDI file" "*.mid" (Just $ T.pack $ input -<.> "reduced.mid") \f -> let
          ext = map toLower $ takeExtension f
          f' = if elem ext [".mid", ".midi"] then f else f <.> "mid"
          in sink $ EventOnyx let
            task = do
              simpleReduce input f'
              return [f']
            in startTasks [("Reduce MIDI: " <> input, task)]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cb)); } |]
      return btn

    , padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Find hanging Pro Keys notes") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cb <- makeCallback \_ _ -> sink $ EventIO do
        input <- pickedFile
        sink $ EventOnyx let
          task = do
            mid <- loadMIDIOrChart input
            lg $ T.unpack $ closeShiftsFile mid
            return []
          in startTasks [("Pro Keys range check: " <> input, task)]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cb)); } |]
      return btn

    , padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Make REAPER project with RB template") } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      cb <- makeCallback \_ _ -> sink $ EventIO do
        input <- pickedFile
        saveFileChooser "Save REAPER project" "*.RPP" (Just $ T.pack $ input -<.> "RPP") \f -> let
          ext = map toUpper $ takeExtension f
          f' = if ext == ".RPP" then f else f <.> "RPP"
          in sink $ EventOnyx let
            task = do
              makeReaper (TuningInfo [] 0) input input [] f'
              return [f']
            in startTasks [("Make REAPER project: " <> input, task)]
      [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cb)); } |]
      return btn

    , padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
      let (rectLeft, trimClock 0 0 0 10 -> rectRight) = chopRight 150 rect'
      btn1 <- withRect rectLeft \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Clean convert .chart to MIDI") } |]
      [C.block| void { $(Fl_Button* btn1)->color($(Fl_Color taskColor)); } |]
      cb1 <- makeCallback \_ _ -> sink $ EventIO do
        input <- pickedFile
        saveFileChooser "Save MIDI file" "*.mid" (Just $ T.pack $ input -<.> ".mid") \f -> let
          ext = map toLower $ takeExtension f
          f' = if elem ext [".mid", ".midi"] then f else f <.> "mid"
          in sink $ EventOnyx let
            task = do
              mid <- loadRawMIDIOrChart input
              stackIO $ Save.toFile f' $ TE.encodeUtf8 <$> mid
              return [f']
            in startTasks [("Convert to MIDI: " <> input, task)]
      [C.block| void { $(Fl_Button* btn1)->callback($(Fl_Callback cb1)); } |]

      -- note, this button doesn't disable when path box is empty since it doesn't use it
      btn2 <- withRect rectRight \bx by bw bh ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Batch") } |]
      [C.block| void { $(Fl_Button* btn2)->color($(Fl_Color taskColor)); } |]
      cb2 <- makeCallback \_ _ -> sink $ EventIO $ askFolder Nothing \dir ->
        sink $ EventOnyx let
          task = do
            results <- recursiveChartToMidi dir
            when (null results) $ lg "No .chart files found."
            return results
          in startTasks [("Convert .chart to MIDI (batch): " <> dir, task)]
      [C.block| void { $(Fl_Button* btn2)->callback($(Fl_Callback cb2)); } |]

      return btn1

    ]

  -- disable midi buttons if the path box is empty
  let updatePathInput = do
        cstr <- [C.exp| const char* { $(Fl_Input* pathInput)->value() } |]
        t <- peekUTF8 cstr
        if any (not . isSpace) (t :: String)
          then forM_ midiButtons \b -> [C.block| void { $(Fl_Button* b)->activate(); } |]
          else forM_ midiButtons \b -> [C.block| void { $(Fl_Button* b)->deactivate(); } |]
  updatePathInput
  cbPath <- makeCallback \_ _ -> sink $ EventIO updatePathInput
  [C.block| void {
    $(Fl_Input* pathInput)->callback($(Fl_Callback cbPath));
    $(Fl_Input* pathInput)->when(FL_WHEN_CHANGED);
  } |]

  [C.block| void {
    $(Fl_Pack* pack)->end();
    $(Fl_Group* tab)->resizable($(Fl_Pack* pack));
  } |]

miscPageHardcodeSongCache
  :: (Event -> IO ())
  -> Rectangle
  -> Ptr Fl_Group
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageHardcodeSongCache sink rect tab startTasks = do
  pack <- withRect rect \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  pickedFolder <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("CON folder"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    cbPick <- makeCallback \_ _ -> sink $ EventIO $ askFolder Nothing \f ->
      withUTF8 f \cf ->
        [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
    [C.block| void { $(Fl_Button* pick)->callback($(Fl_Callback cbPick)); } |]
    return do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr
  pickedCache <- padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    let (_, rectA) = chopLeft 100 rect'
        (inputRect, rectB) = chopRight 50 rectA
        (_, pickRect) = chopRight 40 rectB
    input <- withRect inputRect \ix iy iw ih ->
      [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
    [C.block| void {
      $(Fl_Input* input)->labelsize(13);
      $(Fl_Input* input)->align(FL_ALIGN_LEFT);
    } |]
    [C.block| void { $(Fl_Input* input)->label("Song cache"); } |]
    pick <- withRect pickRect \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
    cbPick <- makeCallback \_ _ -> sink $ EventIO do
      picker <- [C.exp| Fl_Native_File_Chooser* {
        new Fl_Native_File_Chooser(Fl_Native_File_Chooser::BROWSE_FILE)
      } |]
      [C.block| void {
        $(Fl_Native_File_Chooser* picker)->title("Load RB3 song cache");
        $(Fl_Native_File_Chooser* picker)->filter("songcache");
      } |]
      result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
      when (result == 0) do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
        f <- peekUTF8 cstr
        withUTF8 (f :: String) \cf ->
          [C.block| void { $(Fl_Input* input)->value($(char* cf)); } |]
      [C.block| void { delete $(Fl_Native_File_Chooser* picker); } |]
    [C.block| void { $(Fl_Button* pick)->callback($(Fl_Callback cbPick)); } |]
    return do
      cstr <- [C.exp| const char* { $(Fl_Input* input)->value() } |]
      peekUTF8 cstr
  padded 5 10 10 10 (Size (Width 800) (Height 35)) \rect' -> do
    btn <- withRect rect' \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Apply numeric IDs from song cache") } |]
    [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
    cb <- makeCallback \_ _ -> sink $ EventIO do
      dir <- pickedFolder
      cache <- pickedCache
      let cache' = case cache of
            "" -> dir </> "songcache"
            _  -> cache
      sink $ EventOnyx let
        task = do
          hardcodeSongCacheIDs cache' dir
          return []
        in startTasks [("Hardcode song cache: " <> dir, task)]
    [C.block| void { $(Fl_Button* btn)->callback($(Fl_Callback cb)); } |]
  [C.block| void {
    $(Fl_Pack* pack)->end();
    $(Fl_Group* tab)->resizable($(Fl_Pack* pack));
  } |]

launchQuickConvert' :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchQuickConvert' sink makeMenuBar = sink $ EventOnyx do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchQuickConvert sink makeMenuBar

launchQuickConvert :: (?preferences :: Preferences) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchQuickConvert sink makeMenuBar = mdo
  let windowWidth = Width 1000
      windowHeight = Height 600
      windowSize = Size windowWidth windowHeight
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), "Quick Convert") } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
    $(Fl_Window* window)->size_range(1000, 600);
  } |]
  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]
  functionTabs <- sequence
    [ makeTab windowRect "RB quick convert + pack creator" \rect tab -> do
      setTabColor tab functionTabColor
      pageQuickConvertRB sink rect tab startTasks
      return tab
    , makeTab windowRect "CH quick convert" \rect tab -> do
      setTabColor tab functionTabColor
      pageQuickConvertCH sink rect tab startTasks
      return tab
    , makeTab windowRect "RR renumber" \rect tab -> do
      setTabColor tab functionTabColor
      pageRenumberRR sink rect tab startTasks
      return tab
    , makeTab windowRect "GH2 pack creator" \rect tab -> do
      setTabColor tab functionTabColor
      miscPagePacks sink rect tab startTasks
      return tab
    , makeTab windowRect "RB legacy CON->PKG" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageCONtoPKG sink rect tab startTasks
      return tab
    ]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" \rect tab -> do
    setTabColor tab taskColor
    [C.block| void { $(Fl_Group* tab)->deactivate(); } |]
    let cbStart = do
          [C.block| void { $(Fl_Group* tab)->activate(); } |]
          forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tab)); } |]
          updateTabsColor tabs
        cbEnd = do
          forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage rect tab sink cbStart cbEnd

  [C.block| void { $(Fl_Tabs* tabs)->end(); } |]
  updateTabsColor tabs
  cbTabs <- makeCallback \_ _ -> updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->callback($(Fl_Callback cbTabs)); } |]
  let firstTab = head functionTabs
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* firstTab)); } |]

  cbWindow <- makeCallback \_ _ -> windowCloser cancelTasks window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->callback($(Fl_Callback cbWindow));
    $(Fl_Window* window)->show();
  } |]

launchMisc :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchMisc sink makeMenuBar = mdo
  let windowWidth = Width 900
      windowHeight = Height 600
      windowSize = Size windowWidth windowHeight
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), "Tools") } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
    $(Fl_Window* window)->size_range(900, 600);
  } |]
  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]
  functionTabs <- sequence
    [ makeTab windowRect "MIDI stuff" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageMIDI sink rect tab startTasks
      return tab
    , makeTab windowRect "MOGG/VGS/FSB" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageMOGG sink rect tab startTasks
      return tab
    , makeTab windowRect "Lipsync" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageLipsync sink rect tab startTasks
      return tab
    , makeTab windowRect "Dry vox" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageDryVox sink rect tab startTasks
      return tab
    , makeTab windowRect ".milo" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageMilo sink rect tab startTasks
      return tab
    , makeTab windowRect "RB3 cache" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageHardcodeSongCache sink rect tab startTasks
      return tab
    , makeTab windowRect "GH3 cache" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageGH3SongCache sink rect tab startTasks
      return tab
    , makeTab windowRect "GH:WoR cache" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageWoRSongCache sink rect tab startTasks
      return tab
    , makeTab windowRect "CON to PKG" \rect tab -> do
      setTabColor tab functionTabColor
      miscPageCONtoPKG sink rect tab startTasks
      return tab
    ]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" \rect tab -> do
    setTabColor tab taskColor
    [C.block| void { $(Fl_Group* tab)->deactivate(); } |]
    let cbStart = do
          [C.block| void { $(Fl_Group* tab)->activate(); } |]
          forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tab)); } |]
          updateTabsColor tabs
        cbEnd = do
          forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage rect tab sink cbStart cbEnd

  [C.block| void { $(Fl_Tabs* tabs)->end(); } |]
  updateTabsColor tabs
  cbTabs <- makeCallback \_ _ -> updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->callback($(Fl_Callback cbTabs)); } |]
  let firstTab = head functionTabs
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* firstTab)); } |]

  cbWindow <- makeCallback \_ _ -> windowCloser cancelTasks window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->callback($(Fl_Callback cbWindow));
    $(Fl_Window* window)->show();
  } |]

-- _watchSong, _launchTimeServer

data GLStatus = GLPreload | GLLoaded RGGraphics.GLStuff | GLFailed

previewGroup
  :: (?preferences :: Preferences)
  => (Event -> IO ())
  -> Rectangle
  -> IO PreviewSong
  -> IO Double
  -> IO Double
  -> IO (Maybe AudioHandle)
  -> [T.Text]
  -> IO (Set.Set T.Text)
  -> (Set.Set T.Text -> IO ())
  -> IO (Ptr Fl_Group, IO (), IO ())
previewGroup sink rect getSong getTime getSpeed getAudioHandle audioParts getAudio setAudio = do
  let (glArea, bottomControlsArea) = chopBottom 40 rect
      [ trimClock 6 7 6 14 -> partSelectArea,
        trimClock 6 7 6 7 -> audioSelectArea,
        trimClock 6 14 6 7 -> bgSelectArea ] = splitHorizN 3 bottomControlsArea

  wholeGroup <- withRect rect \gx gy gw gh ->
    [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]

  bottomControlsGroup <- withRect bottomControlsArea \gx gy gw gh ->
    [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
  bottomSizeRef <- withRect bottomControlsArea \bx by bw bh ->
    [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]

  -- Track selection menu
  trackMenu <- withRect partSelectArea \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh), "View Tracks") } |]
  currentParts <- newIORef ([] :: [[T.Text]])
  let updateParts doRedraw names = sink $ EventIO do
        cur <- readIORef currentParts
        when (cur /= names) do
          -- Get currently selected items before clearing
          selected <- case cur of
            [] -> return $ names >>= take 1
            _  -> getSelectedMenuItems trackMenu
          [C.block| void { $(Fl_Menu_Button* trackMenu)->clear(); } |]
          forM_ (concat names) \t -> do
            let isSelected = elem t selected
                flags = [C.pure| int { FL_MENU_TOGGLE } |]
                  + if isSelected then [C.pure| int { FL_MENU_VALUE } |] else 0
            withUTF8 t \ct ->
              [C.block| void {
                $(Fl_Menu_Button* trackMenu)->add($(char* ct), 0, 0, 0, $(int flags));
              } |]
          writeIORef currentParts names
          when doRedraw $ sink $ EventIO $ [C.block| void { Fl::redraw(); } |]

  initSong <- getSong
  updateParts False $ map (map fst) $ previewTracks initSong
  cbTrackMenu <- makeCallback \_ _ -> do
    sink $ EventIO $ [C.block| void { Fl::redraw(); } |]
    -- reopen menu (TODO find a way to not close it at all)
    sink $ EventIO $ [C.block| void { $(Fl_Menu_Button* trackMenu)->popup(); } |]
  [C.block| void { $(Fl_Menu_Button* trackMenu)->callback($(Fl_Callback cbTrackMenu)); } |]

  -- Audio selection menu
  audioMenu <- withRect audioSelectArea \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh), "Audio") } |]
  initialAudio <- getAudio
  forM_ audioParts \t -> do
    let isSelected = Set.member t initialAudio
        flags = [C.pure| int { FL_MENU_TOGGLE } |]
          + (if isSelected then [C.pure| int { FL_MENU_VALUE } |] else 0)
    cbAudio <- makeCallback \_ _ -> do
      currentAudio <- getAudio
      setAudio if Set.member t currentAudio
        then Set.delete t currentAudio
        else Set.insert t currentAudio
      -- reopen menu (TODO find a way to not close it at all)
      sink $ EventIO $ [C.block| void { $(Fl_Menu_Button* audioMenu)->popup(); } |]
    withUTF8 t \ct ->
      [C.block| void {
        $(Fl_Menu_Button* audioMenu)->add($(char* ct), 0, $(Fl_Callback cbAudio), 0, $(int flags));
      } |]

  -- Background selection menu
  bgMenu <- withRect bgSelectArea \mx my mw mh ->
    [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh), "Background") } |]
  let bgs = [(t, Just bg) | (t, bg) <- previewBG initSong]
      emptyBG = ("None", Nothing)
      initialBG = fromMaybe emptyBG $ listToMaybe bgs
      allBGs = emptyBG : bgs
  currentBG <- newIORef $ snd initialBG
  forM_ allBGs \(t, bg) -> do
    let isSelected = t == fst initialBG
        radioFlags = [C.pure| int { FL_MENU_RADIO } |]
          + if isSelected then [C.pure| int { FL_MENU_VALUE } |] else 0
    cbBG <- makeCallback \_ _ -> writeIORef currentBG bg
    withUTF8 t \ct ->
      [C.block| void {
        $(Fl_Menu_Button* bgMenu)->add($(char* ct), 0, $(Fl_Callback cbBG), 0, $(int radioFlags));
      } |]

  [C.block| void { $(Fl_Group* bottomControlsGroup)->end(); } |]
  [C.block| void { $(Fl_Group* bottomControlsGroup)->resizable($(Fl_Box* bottomSizeRef)); } |]

  -- GL window
  varStuff <- newMVar GLPreload
  glwindow <- withRect glArea \gx gy gw gh ->
    [C.exp| HsGlWindow* { new HsGlWindow($(int gx), $(int gy), $(int gw), $(int gh)) } |]
  [C.block| void {
    $(HsGlWindow* glwindow)->mode(FL_OPENGL3 | FL_DEPTH | FL_RGB8 | FL_ALPHA | FL_MULTISAMPLE);
    $(HsGlWindow* glwindow)->end();
  } |]
  drawCb <- makeDrawCallback \_ -> do
    mstuff <- modifyMVar varStuff \case
      GLPreload -> do
        scale <- [C.exp| float { $(HsGlWindow* glwindow)->pixels_per_unit_public() } |]
        embedOnyx sink (RGGraphics.loadGLStuff (realToFrac scale) $ Just initSong) >>= \case
          Nothing -> return (GLFailed, Nothing)
          Just s  -> return (GLLoaded s, Just s)
      loaded@(GLLoaded s) -> return (loaded, Just s)
      GLFailed -> return (GLFailed, Nothing)
    forM_ mstuff \stuff -> do
      t <- getTime
      speed <- getSpeed
      trks <- fmap previewTracks getSong
      bg <- readIORef currentBG
      updateParts True $ map (map fst) trks
      selected <- getSelectedMenuItems trackMenu
      w <- [C.exp| int { $(HsGlWindow* glwindow)->pixel_w_public() } |]
      h <- [C.exp| int { $(HsGlWindow* glwindow)->pixel_h_public() } |]
      let flatTrks = concat trks
      mAudioHandle <- getAudioHandle
      RGGraphics.drawTracks stuff (RGGraphics.WindowDims (fromIntegral w) (fromIntegral h)) t speed bg ?preferences.prefEliteLayout
        (flip mapMaybe selected \name -> (name,) <$> lookup name flatTrks)
        mAudioHandle
  -- TODO add an option to use high-res GL on Mac.
  -- This appears to always be forced true on hidpi Linux. Not sure of Windows.
  -- But on Mac by default I believe the GL resolution is non-retina unless you set this.
  -- Either way pixelsPerUnit will handle the UI correctly.
  [C.block| void {
    $(HsGlWindow* glwindow)->set_draw_callback($(void (*drawCb)(void*)), nullptr);
  } |]

  let deleteGL = do
        withMVar varStuff \case
          GLLoaded s -> RGGraphics.stopVideoLoaders s
          _          -> return ()
        -- TODO: delete glwindow widget
      redrawGL = [C.block| void { $(HsGlWindow* glwindow)->redraw(); } |]

  [C.block| void { $(Fl_Group* wholeGroup)->end(); } |]
  [C.block| void { $(Fl_Group* wholeGroup)->resizable((Fl_Gl_Window*)$(HsGlWindow* glwindow)); } |]
  return (wholeGroup, redrawGL, deleteGL)

-- _launchPreview, _promptPreview

applyDownmix :: Preferences -> SongYaml f -> SongYaml f
applyDownmix prefs song = if prefs.prefCHDownmix
  then mixDownStems song
  else song

launchBatch' :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> [FilePath] -> IO ()
launchBatch' sink makeMenuBar startFiles = sink $ EventOnyx do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchBatch sink makeMenuBar startFiles

warnQuickConvert :: Int -> (Event -> IO ()) -> Onyx () -> Onyx ()
warnQuickConvert count sink go = let
  warningText = T.concat
    [ "You appear to be converting "
    , if count == 1 then "a Rock Band song" else "Rock Band songs"
    , " to RB3 at 100% speed.\n"
    , "If you don't want to change anything about the "
    , if count == 1 then "song" else "songs"
    , " except console platform,\n"
    , "you should use Quick Convert to preserve the "
    , if count == 1 then "song ID" else "song IDs"
    , " for online compatibility."
    ]
  in stackIO do
    withUTF8 warningText \cmsg -> do
      result <- [C.exp| int { fl_choice("%s", "Cancel", "Start", nullptr, $(char* cmsg)) } |]
      when (result == 1) $ sink $ EventOnyx go

launchBatch
  :: (?preferences :: Preferences)
  => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> [FilePath] -> IO ()
launchBatch sink makeMenuBar startFiles = mdo
  loadedFiles <- newMVar []
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), "Batch Recompile") } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
    $(Fl_Window* window)->size_range(800, 500);
  } |]
  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]

  (tabSongs, filterParts) <- makeTab windowRect "Songs" \rect tab -> do
    setTabColor tab homeTabColor
    let (songsRect, importToggles) = chopBottom 40 rect
    group <- fileLoadWindow songsRect sink "Song" "Songs" (modifyMVar_ loadedFiles) startFiles findSongs \imp -> let
      entry = T.concat
        [ fromMaybe "Untitled" imp.impTitle
        , maybe "" (\art -> " (" <> art <> ")") imp.impArtist
        , if imp.imp2x then " (2x)" else ""
        ]
      sublines = concat
        [ case imp.impAuthor of
          Just author | T.any (not . isSpace) author -> ["Author: " <> author]
          _                                          -> []
        , ["Format: " <> imp.impFormat]
        , ["Path: " <> T.pack imp.impPath <> index]
        ]
      index = case imp.impIndex of
        Nothing -> ""
        Just i  -> T.pack $ " (#" <> show i <> ")"
      in (entry, sublines)
    getter <- do
      let subrects = splitHorizN 7 importToggles
          insts =
            [ ("Guitar", Just Guitar)
            , ("Bass"  , Just Bass  )
            , ("Drums" , Just Drums )
            , ("Keys"  , Just Keys  )
            , ("Vocals", Just Vocal )
            , ("Other" , Nothing    )
            ]
      void $ withRect (head subrects) \bx by bw bh ->
        [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "Import:") } |]
      getters <- forM (zip insts $ drop 1 subrects) \((txt, inst), subrect) -> do
        btn <- withRect subrect \bx by bw bh ->
          [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        withUTF8 (txt :: T.Text) \clbl ->
          [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
        [C.block| void { $(Fl_Check_Button* btn)->value(1); } |]
        return do
          b <- checkValue btn
          return $ guard b >> Just inst
      return \songYaml -> do
        active <- catMaybes <$> sequence getters
        return $ (songYaml :: SongYaml FilePath) & #parts .~ do
          Parts $ flip HM.filterWithKey songYaml.parts.getParts
            \part _ -> case part of
              F.PartGuitar -> elem (Just Guitar) active
              F.PartBass   -> elem (Just Bass  ) active
              F.PartDrums  -> elem (Just Drums ) active
              F.PartKeys   -> elem (Just Keys  ) active
              F.PartVocal  -> elem (Just Vocal ) active
              _            -> elem Nothing       active
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* group)); } |]
    return (tab, getter)
  -- keep track of release fns we should run when canceling batch process or closing the window.
  -- otherwise these folders will not be released until onyx closes
  cleanupOnCancel <- newMVar HM.empty
  let doImport imp fn = do
        proj <- importWithPreferences imp
        stackIO $ forM_ proj.projectRelease \k -> do
          modifyMVar_ cleanupOnCancel $ return . HM.insert proj.projectLocation k
        res <- errorToEither $ fn proj
        stackIO $ forM_ proj.projectRelease \k -> modifyMVar_ cleanupOnCancel \m -> do
          release k
          return $ HM.delete proj.projectLocation m
        either throwNoContext return res
  functionTabs <- sequence
    [ makeTab windowRect "RB3" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageRB3 sink rect tab \qcPossible settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        let go = startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
              let (targets, yaml) = settings proj
              proj' <- stackIO $ filterParts yaml >>= saveProject proj
              fmap concat $ forM targets \(target, creator) -> do
                case creator of
                  RB3CON fout -> do
                    tmp <- buildRB3CON target proj'
                    stackIO $ Dir.copyFile tmp fout
                    return [fout]
                  RB3PKG fout -> do
                    tmp <- buildRB3PKG target proj'
                    stackIO $ Dir.copyFile tmp fout
                    return [fout]
                  RB3Magma dout -> do
                    tmp <- buildMagmaV2 target proj'
                    copyDirRecursive tmp dout
                    return [dout]
                  RB3LoosePS3 dout -> do
                    folder <- buildRB3LoosePS3 target proj'
                    stackIO $ installPS3Folder "BLUS30463" (Just "pkg-contents/rb3") folder dout
        if qcPossible && all (.impFormatRB) files
          then warnQuickConvert (length files) sink go
          else go
      return tab
    , makeTab windowRect "RB2" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageRB2 sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let (targets, yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          fmap concat $ forM targets \(target, creator) -> do
            case creator of
              RB2CON fout -> do
                tmp <- buildRB2CON target proj'
                stackIO $ Dir.copyFile tmp fout
                return [fout]
              RB2PKG fout -> do
                tmp <- buildRB2PKG target proj'
                stackIO $ Dir.copyFile tmp fout
                return [fout]
              RB2LoosePS3 dout -> do
                folder <- buildRB2LoosePS3 target proj'
                stackIO $ installPS3Folder "BLUS30050" (Just "pkg-contents/rb2") folder dout
      return tab
    , makeTab windowRect "Clone Hero" \rect tab -> do
      setTabColor tab functionTabColor
      batchPagePS sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        newPreferences <- readPreferences
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let (target, creator) = settings proj
              target' = (target :: TargetPS FilePath)
                & #audioFormat .~ case newPreferences.prefCHAudioFormat of
                  CHAudioOggVorbis -> "ogg"
                  CHAudioOpus      -> "opus"
          proj' <- stackIO $ filterParts proj.projectSongYaml >>= saveProject proj . applyDownmix newPreferences
          case creator of
            PSDir dout -> do
              tmp <- buildPSDir target' proj'
              copyDirRecursive tmp dout
              return [dout]
            PSZip fout -> do
              tmp <- buildPSZip target' proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            PSSng fout -> do
              tmp <- buildPSSng target' proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
      return tab
    , makeTab windowRect "GH1" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageGH1 sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts proj.projectSongYaml >>= saveProject proj
          case creator of
            GH1ARK fout -> do
              installGH1 target proj' fout
              return [fout]
            GH1DIYPS2 fout -> do
              makeGH1DIY target proj' fout
              return [fout]
      return tab
    , makeTab windowRect "GH2" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageGH2 sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts proj.projectSongYaml >>= saveProject proj
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
    , makeTab windowRect "GH3" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageGH3 sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let (target, creator) = settings proj
          proj' <- stackIO $ filterParts proj.projectSongYaml >>= saveProject proj
          results <- case creator of
            GH3LIVE fout -> do
              tmp <- buildGH3LIVE target proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            GH3PKG fout -> do
              tmp <- buildGH3PKG target proj'
              stackIO $ Dir.copyFile tmp fout
              return [fout]
            GH3LoosePS3 dout -> do
              folder <- buildGH3LoosePS3 target proj'
              stackIO $ installPS3Folder "BLUS30074" (Just "pkg-contents/gh3") folder dout
          warn "Make sure you create a GH3 Song Cache (go to 'Other tools') from all your customs and DLC! This is required to load multiple songs."
          return results
      return tab
    , makeTab windowRect "GH:WoR" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageGHWOR sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
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
            GHWORLoosePS3 dout -> do
              folder <- buildGHWORLoosePS3 target proj'
              stackIO $ installPS3Folder "BLUS30487" (Just "pkg-contents/ghwor") folder dout
      return tab
    , makeTab windowRect "RR" \rect tab -> do
      setTabColor tab functionTabColor
      batchPageRR sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let ((target, creator), yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          let warnRR = warn "All generated Rock Revolution custom songs have the same ID. To load multiple songs, please use \"Quick Convert > RR renumber\" to reassign IDs."
          case creator of
            RRLIVE fout -> do
              tmp <- buildRRLIVE target proj'
              stackIO $ Dir.copyFile tmp fout
              warnRR
              return [fout]
            RRPKG fout -> do
              tmp <- buildRRPKG target proj'
              stackIO $ Dir.copyFile tmp fout
              warnRR
              return [fout]
      return tab
    , makeTab windowRect "Preview" \rect tab -> do
      setTabColor tab functionTabColor
      batchPagePreview sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let dout = settings proj
          proj' <- stackIO $ filterParts proj.projectSongYaml >>= saveProject proj
          tmp <- buildPlayer Nothing proj'
          copyDirRecursive tmp dout
          return [dout]
      return tab
    ]
  let nonTermTabs = tabSongs : functionTabs
  (startTasks, cancelTasks) <- makeTab windowRect "Task" \rect tab -> do
    setTabColor tab taskColor
    [C.block| void { $(Fl_Group* tab)->deactivate(); } |]
    let cbStart = do
          [C.block| void { $(Fl_Group* tab)->activate(); } |]
          forM_ nonTermTabs \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tab)); } |]
          updateTabsColor tabs
        cbEnd = do
          modifyMVar_ cleanupOnCancel \m -> do
            mapM_ release $ HM.elems m
            return HM.empty
          forM_ nonTermTabs \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage rect tab sink cbStart cbEnd

  [C.block| void { $(Fl_Tabs* tabs)->end(); } |]
  updateTabsColor tabs
  cbTabs <- makeCallback \_ _ -> updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->callback($(Fl_Callback cbTabs)); } |]
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* tabSongs)); } |]

  cbWindow <- makeCallback \_ _ -> windowCloser
    (cancelTasks >> modifyMVar_ cleanupOnCancel \m -> mapM_ release (HM.elems m) >> return HM.empty)
    window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->callback($(Fl_Callback cbWindow));
    $(Fl_Window* window)->show();
  } |]

updateTabsColor :: Ptr Fl_Tabs -> IO ()
updateTabsColor tabs = do
  -- Get the currently selected tab's color and apply to tabs widget
  color <- [C.block| Fl_Color {
    Fl_Widget* child = ((Fl_Tabs*)$(Fl_Tabs* tabs))->value();
    if (child) {
      return child->selection_color();
    } else {
      return FL_GRAY;
    }
  } |]
  [C.block| void { $(Fl_Tabs* tabs)->selection_color($(Fl_Color color)); } |]

replaceQueueLog :: ((MessageLevel, Message) -> IO ()) -> Onyx a -> Onyx a
replaceQueueLog q = mapStackTraceT $ QueueLog . local (const q) . (.fromQueueLog)

_localResources :: Onyx a -> Onyx a
_localResources = mapStackTraceT $ QueueLog . mapReaderT (mapReaderT $ liftIO . runResourceT) . (.fromQueueLog)

data TermMessage
  = TermStart String String -- "Task 4" "/path/to/file"
  | TermLog String
  | TermWarning Message
  | TermError Message
  | TermSuccess String

toTermMessage :: (MessageLevel, Message) -> TermMessage
toTermMessage (MessageLog    , msg) = TermLog msg.messageString
toTermMessage (MessageWarning, msg) = TermWarning msg

addTerm :: Ptr Fl_Simple_Terminal -> TermMessage -> IO ()
addTerm term pair = do
  let newtxt = filterUserError $ T.pack case pair of
        TermStart x y   -> "\ESC[46m" <> x <> "\ESC[0m" <> ": " <> y
        TermLog str     -> str
        TermWarning msg -> "\ESC[33mWarning\ESC[0m: " <> Exc.displayException msg
        TermError msg   -> "\ESC[41mERROR!\ESC[0m " <> Exc.displayException msg
        TermSuccess str -> "\ESC[42mSuccess!\ESC[0m " <> str
      -- this comes from the default exception type but it's confusing to users
      filterUserError = T.replace "user error " ""
  B.useAsCString (TE.encodeUtf8 $ T.strip newtxt <> "\n") \cs ->
    [C.block| void {
      $(Fl_Simple_Terminal* term)->append($(char* cs));
    } |]

macOS :: Bool
macOS = os == "darwin"

isNewestRelease :: (Ordering -> IO ()) -> IO ()
isNewestRelease cb = do
  let addr = Req.https "api.github.com" /: "repos" /: "mtolly" /: "onyx" /: "releases" /: "latest"
  rsp <- Req.runReq Req.defaultHttpConfig $ Req.req Req.GET addr Req.NoReqBody Req.jsonResponse $ Req.header "User-Agent" "mtolly/onyx"
  case Req.responseBody rsp of
    A.Object obj -> case KM.lookup "name" obj of
      Just (A.String str) -> cb case (readMaybe $ T.unpack str, readMaybe $ showVersion version) of
        (Just latest, Just this) -> compare (this :: Integer) latest
        _                        -> if T.unpack str == showVersion version then EQ else LT
      _                   -> return ()
    _            -> return ()

launchPreferences :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> Onyx ()
launchPreferences sink makeMenuBar = do
  loadedPrefs <- readPreferences
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
      leaveLeftLabelSpace = snd . chopLeft 180
      lineBox :: (Rectangle -> IO a) -> IO a
      lineBox = padded 5 10 5 10 (Size windowWidth (Height 40))
      bottomHeight = 55
      folderBox :: Rectangle -> T.Text -> T.Text -> IO (IO (Maybe FilePath))
      folderBox rect label initValue = do
        let rectA = leaveLeftLabelSpace rect
            (inputRect, rectB) = chopRight 100 rectA
            (_, rectC) = chopRight 90 rectB
            (browseRect, _) = chopLeft 40 rectC
            (_, rectD) = chopRight 50 rectC
            (_, resetRect) = chopRight 40 rectD
        input <- withRect inputRect \ix iy iw ih ->
          [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih)) } |]
        withUTF8 label \clabel ->
          [C.block| void {
            Fl_Input* inp = $(Fl_Input* input);
            inp->copy_label($(char* clabel));
            inp->labelsize(13);
            inp->align(FL_ALIGN_LEFT);
          } |]
        withUTF8 initValue \cval ->
          [C.block| void { $(Fl_Input* input)->value($(char* cval)); } |]
        browseButton <- withRect browseRect \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@fileopen") } |]
        cbBrowse <- makeCallback \_ _ -> sink $ EventIO $ askFolder Nothing \dir ->
          withUTF8 dir \cdir ->
            [C.block| void { $(Fl_Input* input)->value($(char* cdir)); } |]
        [C.block| void { $(Fl_Button* browseButton)->callback($(Fl_Callback cbBrowse)); } |]
        resetButton <- withRect resetRect \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@undo") } |]
        cbReset <- makeCallback \_ _ -> sink $ EventIO do
          [C.block| void { $(Fl_Input* input)->value(""); } |]
        [C.block| void { $(Fl_Button* resetButton)->callback($(Fl_Callback cbReset)); } |]
        return do
          val <- peekInputValue input
          return $ case val of
            "" -> Nothing
            x  -> Just $ T.unpack x
      combinePrefs :: [IO (a -> a)] -> IO (a -> a)
      combinePrefs = fmap (foldr (.) id) . sequence
  stackIO do
    window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
      [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), "Onyx Preferences") } |]
    [C.block| void { $(Fl_Window* window)->color($(Fl_Color behindTabsColor)); } |]
    menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
    let (tabsRect, bottomRect) = chopBottom bottomHeight $ snd $ chopTop menuHeight $ Rectangle
          (Position (X 0) (Y 0))
          windowSize
    let Width windowW = windowWidth
        Height windowH = windowHeight
    [C.block| void {
      $(Fl_Window* window)->resizable($(Fl_Window* window));
      $(Fl_Window* window)->size_range($(int windowW), $(int windowH));
    } |]
    tabs <- withRect tabsRect \tx ty tw th ->
      [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]
    (tab1, pref1) <- makeTab tabsRect "General" \rect tab -> do
      pack <- withRect rect \px py pw ph ->
        [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
      fn <- combinePrefs <$> sequence
        [ lineBox \box -> do
          let [checkBox, _, counterBox, _] = splitHorizN 4 box
          check <- withRect checkBox \cx cy cw ch ->
            [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
          [C.block| void { $(Fl_Check_Button* check)->label("Unlimited cores"); } |]
          let checkVal = if isNothing loadedPrefs.prefThreads then 1 else 0 :: CInt
          [C.block| void { $(Fl_Check_Button* check)->value($(int checkVal)); } |]
          counter <- withRect counterBox \cx cy cw ch ->
            [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
          let initCores = maybe 1 fromIntegral loadedPrefs.prefThreads :: CDouble
          [C.block| void {
            Fl_Counter* c = $(Fl_Counter* counter);
            c->label("Core limit");
            c->labelsize(13);
            c->align(FL_ALIGN_LEFT);
            c->type(FL_SIMPLE_COUNTER);
            c->step(1);
            c->minimum(1);
            c->maximum(16);
            c->value($(double initCores));
          } |]
          let updateCounter = do
                v <- checkValue check
                if v
                  then [C.block| void { $(Fl_Counter* counter)->deactivate(); } |]
                  else [C.block| void { $(Fl_Counter* counter)->activate(); } |]
          cbCheck <- makeCallback \_ _ -> updateCounter
          [C.block| void { $(Fl_Check_Button* check)->callback($(Fl_Callback cbCheck)); } |]
          updateCounter
          return do
            unlimited <- checkValue check
            cores <- [C.exp| double { $(Fl_Counter* counter)->value() } |]
            return \prefs -> prefs { prefThreads = if unlimited then Nothing else Just $ round cores }
        , do
          check <- lineBox \box -> do
            btn <- withRect box \bx by bw bh ->
              [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
            [C.block| void { $(Fl_Check_Button* btn)->label("For non-open-note games, convert \"muted strums\" to Harmonix style"); } |]
            withUTF8 (T.unwords
              [ "When checked, sequences of the form \"chord, strummed open notes, strummed chord\" are assumed to represent muted strums."
              , "Instead of always becoming green, these open notes will become the lowest note of the following chord,"
              , "or of the preceding chord if it is closer to the open notes."
              ]) \ctooltip ->
              [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
            let v = if loadedPrefs.prefDetectMuted then 1 else 0 :: CInt
            [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
            return btn
          return $ (\b prefs -> prefs { prefDetectMuted = b }) <$> checkValue check
        ]
      [C.block| void { $(Fl_Pack* pack)->end(); } |]
      return (tab, fn)
    restPrefs <- sequence
      [ makeTab tabsRect "Rock Band" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getMagma <- lineBox \box -> horizRadio box
              [ ("Magma required" , MagmaRequire, loadedPrefs.prefMagma == MagmaRequire)
              , ("Magma optional" , MagmaTry    , loadedPrefs.prefMagma == MagmaTry    )
              , ("Don't use Magma", MagmaDisable, loadedPrefs.prefMagma == MagmaDisable)
              ]
            return $ maybe id (\v prefs -> prefs { prefMagma = v }) <$> getMagma
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Always export black VENUE track"); } |]
              let v = if loadedPrefs.prefBlackVenue then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefBlackVenue = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Label 2x kick charts as (2x Bass Pedal) by default"); } |]
              let v = if loadedPrefs.prefLabel2x then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLabel2x = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Modify tempos in RB export to follow Magma rules"); } |]
              withUTF8 (T.unwords
                [ "Magma requires MIDI files to use tempos in the range of 40 to 300 BPM."
                , "This is a more restricted range than the games actually require."
                ]) \ctooltip ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
              let v = if loadedPrefs.prefLegalTempos then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLegalTempos = b }) <$> checkValue check
          , do
            getRB3Encoding <- lineBox \box -> do
              let (boxA, boxB) = chopLeft 230 box
              _ <- withRect boxA \bx by bw bh ->
                [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "RB3 recompile .dta") } |]
              horizRadio boxB
                [ ("Latin-1", Latin1, loadedPrefs.prefRB3Encoding == Latin1)
                , ("UTF-8"  , UTF8  , loadedPrefs.prefRB3Encoding == UTF8  )
                ]
            return $ maybe id (\v prefs -> prefs { prefRB3Encoding = v }) <$> getRB3Encoding
          , do
            getPackEncoding <- lineBox \box -> do
              let (boxA, boxB) = chopLeft 230 box
              _ <- withRect boxA \bx by bw bh ->
                [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "RB3 pack .dta") } |]
              horizRadio boxB
                [ ("Latin-1", Just Latin1, loadedPrefs.prefPackEncoding == Just Latin1)
                , ("UTF-8"  , Just UTF8  , loadedPrefs.prefPackEncoding == Just UTF8  )
                , ("As is"  , Nothing    , loadedPrefs.prefPackEncoding == Nothing    )
                ]
            return $ maybe id (\v prefs -> prefs { prefPackEncoding = v }) <$> getPackEncoding
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "Guitar Hero" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ lineBox \box -> do
            let [_, middleBox, _] = splitHorizN 3 box
            gh2OffsetCounter <- withRect middleBox \cx cy cw ch ->
              [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
            let initOffset = coerce $ loadedPrefs.prefGH2Offset * 1000 :: CDouble
            withUTF8 (T.unwords
              [ "Adjust audio offset only for Guitar Hero II output, intended to compensate for emulator delay."
              , "Positive values mean audio will be pulled earlier, to account for delay."
              , "Negative values mean audio will be pushed later."
              ]) \ctooltip ->
              [C.block| void {
                Fl_Counter* c = $(Fl_Counter* gh2OffsetCounter);
                c->label("GH2 Audio Offset (ms)");
                c->labelsize(13);
                c->align(FL_ALIGN_LEFT);
                c->step(1);
                c->lstep(10);
                c->copy_tooltip($(char* ctooltip));
                c->value($(double initOffset));
              } |]
            return $ (\ms prefs -> prefs { prefGH2Offset = realToFrac ms / 1000 }) <$>
              [C.exp| double { $(Fl_Counter* gh2OffsetCounter)->value() } |]
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Sort GH2 bonus songs when adding to .ARK"); } |]
              let v = if loadedPrefs.prefSortGH2 then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefSortGH2 = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("For GH2 and GH3, sort by artist first (instead of title)"); } |]
              withUTF8 (T.unwords
                [ "This affects GH2 .ARK bonus songs (when the above box is checked)"
                , "and GH3 DLC (as sorted in the Song Cache file)."
                ]) \ctooltip ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
              let v = if loadedPrefs.prefArtistSort then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefArtistSort = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "Clone Hero" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box "Default CH folder" $ T.pack $ fromMaybe "" loadedPrefs.prefDirCH
            return $ (\mdir prefs -> prefs { prefDirCH = mdir }) <$> getDir
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Mix down stems in CH output"); } |]
              let v = if loadedPrefs.prefCHDownmix then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefCHDownmix = b }) <$> checkValue check
          , do
            audioFormat <- lineBox \box -> horizRadio box
              [ ("Output Ogg Vorbis audio", CHAudioOggVorbis, loadedPrefs.prefCHAudioFormat == CHAudioOggVorbis)
              , ("Output Opus audio"      , CHAudioOpus     , loadedPrefs.prefCHAudioFormat == CHAudioOpus     )
              ]
            return $ maybe id (\v prefs -> prefs { prefCHAudioFormat = v }) <$> audioFormat
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "360" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box "Default CON/LIVE folder" $ T.pack $ fromMaybe "" loadedPrefs.prefDirRB
            return $ (\mdir prefs -> prefs { prefDirRB = mdir }) <$> getDir
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Ensure valid filenames for Xbox 360 USB or HDD drives"); } |]
              withUTF8 (T.unwords
                [ "When checked, the filenames of CON or LIVE files will be trimmed to a"
                , "max of 42 characters, and will also have plus and comma characters"
                , "replaced (a restriction for files on a 360 hard drive, but not USB)."
                ]) \ctooltip ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
              let v = if loadedPrefs.prefTrimXbox then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefTrimXbox = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Use true number IDs instead of symbols on Xbox 360 RB files"); } |]
              withUTF8 (T.unwords
                [ "When checked, Rock Band files will use a random integer for song_id."
                , "Otherwise, an 'o' is prefixed to form a symbol, and the game will"
                , "generate a random ID. A true integer is required for some contexts"
                , "such as RGH online play and PS3 conversion, and also maintains scores"
                , "even if the song cache is regenerated."
                ]) \ctooltip ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
              let v = if loadedPrefs.prefRBNumberID then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefRBNumberID = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "PS3" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Encrypt .mid.edat files for PS3"); } |]
              withUTF8 (T.unwords
                [ "When checked, .mid.edat files for PS3 customs will be encrypted."
                , "This is required for a real console, but optional (and can be less"
                , "convenient) when playing on emulator."
                ]) \ctooltip ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_tooltip($(char* ctooltip)); } |]
              let v = if loadedPrefs.prefPS3Encrypt then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefPS3Encrypt = b }) <$> checkValue check
          , do
            getDir <- lineBox \box -> folderBox box "PS3/RPCS3 install folder" $ T.pack $ fromMaybe "" loadedPrefs.prefDirPS3
            return $ (\mdir prefs -> prefs { prefDirPS3 = mdir }) <$> getDir
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "Wii" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box "Default Wii .app folder" $ T.pack $ fromMaybe "" loadedPrefs.prefDirWii
            return $ (\mdir prefs -> prefs { prefDirWii = mdir }) <$> getDir
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "3D Preview" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getMSAA <- lineBox \box -> horizRadio box
              [ ("No MSAA" , Nothing, loadedPrefs.prefMSAA == Nothing)
              , ("MSAA 2x" , Just 2 , loadedPrefs.prefMSAA == Just 2 )
              , ("MSAA 4x" , Just 4 , loadedPrefs.prefMSAA == Just 4 )
              , ("MSAA 8x" , Just 8 , loadedPrefs.prefMSAA == Just 8 )
              , ("MSAA 16x", Just 16, loadedPrefs.prefMSAA == Just 16)
              ]
            return $ maybe id (\v prefs -> prefs { prefMSAA = v }) <$> getMSAA
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("FXAA"); } |]
              let v = if loadedPrefs.prefFXAA then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefFXAA = b }) <$> checkValue check
          , lineBox \box -> do
            let [_, middleBox, _] = splitHorizN 3 box
            fpsCounter <- withRect middleBox \cx cy cw ch ->
              [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
            let initFPS = fromIntegral loadedPrefs.prefPreviewFPS :: CDouble
            [C.block| void {
              Fl_Counter* c = $(Fl_Counter* fpsCounter);
              c->label("Frames per second");
              c->labelsize(13);
              c->align(FL_ALIGN_LEFT);
              c->step(1);
              c->lstep(10);
              c->minimum(1);
              c->value($(double initFPS));
            } |]
            return $ (\n prefs -> prefs { prefPreviewFPS = round n }) <$>
              [C.exp| double { $(Fl_Counter* fpsCounter)->value() } |]
          , lineBox \box -> do
            let [trimClock 0 5 0 0 -> box1, trimClock 0 0 0 5 -> box2] = splitHorizN 2 box
                layoutLeftOpenHand = listToMaybe $ flip mapMaybe loadedPrefs.prefEliteLayout \case
                  EDLeftCrossHand -> Just False
                  EDLeftOpenHand  -> Just True
                  _               -> Nothing
                layoutRightNearCrash = listToMaybe $ flip mapMaybe loadedPrefs.prefEliteLayout \case
                  EDRightFarCrash  -> Just False
                  EDRightNearCrash -> Just True
                  _                -> Nothing
            getLeftSide <- makeDropdown box1
              $ ("Elite drums hihat side: default"   , Nothing   , layoutLeftOpenHand == Nothing   ) NE.:|
              [ ("Elite drums hihat side: cross hand", Just False, layoutLeftOpenHand == Just False)
              , ("Elite drums hihat side: open hand" , Just True , layoutLeftOpenHand == Just True )
              ]
            getRightSide <- makeDropdown box2
              $ ("Elite drums ride side: default"   , Nothing   , layoutRightNearCrash == Nothing   ) NE.:|
              [ ("Elite drums ride side: far crash" , Just False, layoutRightNearCrash == Just False)
              , ("Elite drums ride side: near crash", Just True , layoutRightNearCrash == Just True )
              ]
            return do
              leftSide  <- getLeftSide
              rightSide <- getRightSide
              return \prefs -> prefs
                { prefEliteLayout = concat
                  [ case leftSide of
                    Nothing    -> []
                    Just False -> [EDLeftCrossHand]
                    Just True  -> [EDLeftOpenHand ]
                  , case rightSide of
                    Nothing    -> []
                    Just False -> [EDRightFarCrash ]
                    Just True  -> [EDRightNearCrash]
                  ]
                }
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Audio waveform"); } |]
              let v = if loadedPrefs.prefWaveform then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefWaveform = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Lefty flip"); } |]
              let v = if loadedPrefs.prefLeftyFlip then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLeftyFlip = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect "Audio" \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            sliderQuality <- lineBox \box -> do
              let sliderRect = leaveLeftLabelSpace box
              slider <- withRect sliderRect \sx sy sw sh ->
                [C.exp| Fl_Value_Slider* { new Fl_Value_Slider($(int sx), $(int sy), $(int sw), $(int sh)) } |]
              let initQ = coerce $ loadedPrefs.prefOGGQuality * 10 :: CDouble
              [C.block| void {
                Fl_Value_Slider* s = $(Fl_Value_Slider* slider);
                s->label("OGG Vorbis quality");
                s->labelsize(13);
                s->align(FL_ALIGN_LEFT);
                s->type(FL_HOR_NICE_SLIDER);
                s->minimum(0);
                s->maximum(10);
                s->value($(double initQ));
              } |]
              return slider
            return $ (\v prefs -> prefs { prefOGGQuality = realToFrac v / 10 }) <$>
              [C.exp| double { $(Fl_Value_Slider* sliderQuality)->value() } |]
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              [C.block| void { $(Fl_Check_Button* btn)->label("Treat undecryptable MOGGs as silent instead of an error"); } |]
              let v = if loadedPrefs.prefDecryptSilent then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefDecryptSilent = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      ]
    [C.block| void { $(Fl_Tabs* tabs)->end(); } |]
    [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* tab1)); } |]
    let (bottomLeft, bottomRight) = chopRight 300 bottomRect
        [trimClock 8 4 8 8 -> rectButtonA, trimClock 8 8 8 4 -> rectButtonB] = splitHorizN 2 bottomRight
    bottomArea <- withRect bottomRect \bx by bw bh ->
      [C.exp| Fl_Group* { new Fl_Group($(int bx), $(int by), $(int bw), $(int bh)) } |]
    bottomLeftBox <- withRect bottomLeft \bx by bw bh ->
      [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
    [C.block| void { $(Fl_Group* bottomArea)->resizable($(Fl_Box* bottomLeftBox)); } |]
    saveButton <- withRect rectButtonA \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    [C.block| void {
      $(Fl_Button* saveButton)->label("Save");
      $(Fl_Button* saveButton)->color($(Fl_Color taskColor));
    } |]
    cbSave <- makeCallback \_ _ -> do
      newPrefs <- ($ loadedPrefs) <$> combinePrefs (pref1 : restPrefs)
      let magmaWarning = T.unlines
            [ "Warning! Disabling Magma can result in Rock Band files that crash the game,"
            , "as Onyx does not yet check all of the error cases that Magma does."
            , "Please test any charts thoroughly before distributing to others!"
            ]
          continueSave = do
            savePreferences newPrefs
            [C.block| void { $(Fl_Window* window)->hide(); } |]
      if loadedPrefs.prefMagma == MagmaRequire && newPrefs.prefMagma /= MagmaRequire
        then withUTF8 magmaWarning \cmsg -> do
          result <- [C.exp| int { fl_choice("%s", "Cancel", "OK", nullptr, $(char* cmsg)) } |]
          when (result == 1) continueSave
        else continueSave
    [C.block| void { $(Fl_Button* saveButton)->callback($(Fl_Callback cbSave)); } |]
    cancelButton <- withRect rectButtonB \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Cancel") } |]
    cbCancel <- makeCallback \_ _ ->
      [C.block| void { $(Fl_Window* window)->hide(); } |]
    [C.block| void { $(Fl_Button* cancelButton)->callback($(Fl_Callback cbCancel)); } |]
    [C.block| void { $(Fl_Group* bottomArea)->end(); } |]
    [C.block| void {
      $(Fl_Window* window)->end();
      $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
      $(Fl_Window* window)->show();
    } |]

launchGUI :: [FilePath] -> IO ()
launchGUI initSongs = withAL \hasAudio -> do
  [C.block| void {
    Fl::scheme("gtk+");
    Fl::lock(); // this is required to get threads to work properly
  } |]

  evts <- newTChanIO
  let sink e = do
        atomically $ writeTChan evts e
         -- this makes waitFor finish so we can process the event
        [C.block| void { Fl::awake(); } |]

  -- terminal
  let consoleWidth = 650
      consoleHeight = 480
  termWindow <- [C.exp| Fl_Window* {
    new Fl_Double_Window($(int consoleWidth), $(int consoleHeight), "Onyx Console")
  } |]
  [C.block| void { $(Fl_Window* termWindow)->xclass("Onyx"); } |]
  -- TODO set icon
  [C.block| void {
    $(Fl_Window* termWindow)->size_range($(int consoleWidth), $(int consoleHeight));
    $(Fl_Window* termWindow)->color($(Fl_Color globalLogColor));
  } |]
  let makeMenuBar (Width menuWidth) includeConsole = do
        let menuHeight = if macOS then 0 else 30
            menuX = 0
            menuY = 0
            menuOptions =
              [ ( "File/Open Song"
                , [C.pure| int { FL_COMMAND + 'o' } |]
                , promptLoad sink makeMenuBar hasAudio
                , 0
                )
              , ( "File/Batch Recompile"
                , [C.pure| int { FL_COMMAND + 'b' } |]
                , launchBatch' sink makeMenuBar []
                , 0
                )
              , ( "File/Quick Convert"
                , [C.pure| int { FL_COMMAND + 'u' } |]
                , launchQuickConvert' sink makeMenuBar
                , 0
                )
              , ( "File/Tools"
                , [C.pure| int { FL_COMMAND + 't' } |]
                , launchMisc sink makeMenuBar
                , 0
                )
              , ( "File/Close Window"
                , [C.pure| int { FL_COMMAND + 'w' } |]
                , [C.block| void {
                    // Can't reproduce consistently, but sometimes on Windows, first_window returns one of
                    // the preview windows (a non-root GLWindow). So we go up the chain to be sure
                    Fl_Group* w = Fl::first_window();
                    if (!w) return;
                    while (w->parent()) w = w->parent();
                    w->do_callback();
                  } |]
                , 0
                )
              , ( "Edit/Preferences"
                , 0 -- maybe Cmd+, on Mac
                , sink $ EventOnyx $ launchPreferences sink makeMenuBar
                , 0
                )
              , ( "Help/Readme"
                , 0
                , sink $ EventIO do
                    getResourcesPath "README.html" >>= osOpenFile
                , 0
                )
              , ( "Help/Update history"
                , 0
                , sink $ EventIO do
                    getResourcesPath "CHANGES.txt" >>= osOpenFile
                , 0
                )
              , ( "Help/License"
                , 0
                , sink $ EventIO do
                    getResourcesPath "LICENSE.txt" >>= osOpenFile
                , 0
                )
              , ( "Help/Credits"
                , 0
                , sink $ EventIO do
                    getResourcesPath "CREDITS.txt" >>= osOpenFile
                , 0
                )
              ] ++ do
                guard includeConsole
                return
                  ( "View/Show Console"
                  , [C.pure| int { FL_CTRL + '`' } |]
                  , [C.block| void { $(Fl_Window* termWindow)->show(); } |]
                  , 0
                  )
        if macOS
          then do
            menu <- [C.exp| Fl_Sys_Menu_Bar* { new Fl_Sys_Menu_Bar($(int menuX), $(int menuY), $(int menuWidth), $(int menuHeight)) } |]
            -- TODO need to set window_menu_style to avoid weird slash submenu,
            -- see details in launchWindow
            forM_ menuOptions \(a, b, c, d) -> do
              withUTF8 (a :: String) \str -> do
                cb <- makeCallback \_ _ -> c
                [C.block| void {
                  $(Fl_Sys_Menu_Bar* menu)->add($(char* str), $(int b), $(Fl_Callback cb), nullptr, $(int d));
                } |]
          else do
            menu <- [C.exp| Fl_Menu_Bar* { new Fl_Menu_Bar($(int menuX), $(int menuY), $(int menuWidth), $(int menuHeight)) } |]
            -- TODO need to set window_menu_style to avoid weird slash submenu,
            -- see details in launchWindow
            forM_ menuOptions \(a, b, c, d) -> do
              withUTF8 a \str -> do
                cb <- makeCallback \_ _ -> c
                [C.block| void {
                  $(Fl_Menu_Bar* menu)->add($(char* str), $(int b), $(Fl_Callback cb), nullptr, $(int d));
                } |]
            [C.block| void {
              $(Fl_Menu_Bar* menu)->box(FL_ENGRAVED_BOX);
              $(Fl_Menu_Bar* menu)->down_color(fl_rgb_color(0x32, 0x30, 0x30));
            } |]
        return menuHeight
  menuHeight <- makeMenuBar (Width consoleWidth) macOS
  cbGithub <- makeCallback \_ _ ->
    osOpenFile "https://github.com/mtolly/onyx/releases"
  [C.block| void {
    Fl_Button* buttonGithub = new Fl_Button(10, 10 + $(int menuHeight), 100, 25, "Homepage");
    buttonGithub->callback($(Fl_Callback cbGithub));
  } |]
  labelLatest <- [C.exp| Fl_Box* {
    new Fl_Box(120, 10 + $(int menuHeight), 520, 25, "Checking for updates…")
  } |]
  _ <- forkIO $ isNewestRelease \comp -> sink $ EventIO do
    let compInt = case comp of
          EQ -> 0
          LT -> -1
          GT -> 1
    [C.block| void {
      if ($(int compInt) < 0) {
        $(Fl_Box* labelLatest)->label("New version available!");
        $(Fl_Box* labelLatest)->labelcolor(FL_WHITE);
      } else if ($(int compInt) == 0) {
        $(Fl_Box* labelLatest)->label("You are using the latest version.");
      } else {
        $(Fl_Box* labelLatest)->label("Prerelease version");
        $(Fl_Box* labelLatest)->labelcolor(FL_WHITE);
      }
    } |]
  term <- [C.block| Fl_Simple_Terminal* {
    Fl_Simple_Terminal* term = new Fl_Simple_Terminal
      ( 10
      , 45 + $(int menuHeight)
      , 630
      , 345 - $(int menuHeight)
      );
    term->history_lines(-1);
    term->ansi(true);
    term->stay_at_bottom(true);
    return term;
  } |]

  let bottomBar1 = Rectangle (Position (X 5) (Y 400)) (Size (Width 640) (Height 30))
      bottomBar2 = Rectangle (Position (X 5) (Y 440)) (Size (Width 640) (Height 30))
      [areaOpen, areaBatch] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar1
      [areaQuick, areaMisc] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar2

  cbButtonOpen <- makeCallback \_ _ -> promptLoad sink makeMenuBar hasAudio
  buttonOpen <- withRect areaOpen \x y w h ->
    [C.exp| HsDndButton* { new HsDndButton($(int x), $(int y), $(int w), $(int h), "Load a song") } |]
  [C.block| void {
    $(HsDndButton* buttonOpen)->color($(Fl_Color loadSongColor));
    $(HsDndButton* buttonOpen)->callback($(Fl_Callback cbButtonOpen));
    $(HsDndButton* buttonOpen)->labelsize(13);
  } |]
  setDndButtonCallback buttonOpen (sink . EventOnyx . startLoad makeMenuBar hasAudio)

  cbButtonBatch <- makeCallback \_ _ -> launchBatch' sink makeMenuBar []
  buttonBatch <- withRect areaBatch \x y w h ->
    [C.exp| HsDndButton* { new HsDndButton($(int x), $(int y), $(int w), $(int h), "Batch recompile") } |]
  [C.block| void {
    $(HsDndButton* buttonBatch)->color($(Fl_Color batchProcessColor));
    $(HsDndButton* buttonBatch)->callback($(Fl_Callback cbButtonBatch));
    $(HsDndButton* buttonBatch)->labelsize(13);
  } |]
  setDndButtonCallback buttonBatch (launchBatch' sink makeMenuBar)

  cbButtonQuick <- makeCallback \_ _ -> launchQuickConvert' sink makeMenuBar
  withRect areaQuick \x y w h -> [C.block| void {
    Fl_Button* buttonQuick = new Fl_Button($(int x), $(int y), $(int w), $(int h), "Quick convert/pack");
    buttonQuick->color($(Fl_Color quickConvertColor));
    buttonQuick->callback($(Fl_Callback cbButtonQuick));
    buttonQuick->labelsize(13);
  } |]

  cbButtonMisc <- makeCallback \_ _ -> launchMisc sink makeMenuBar
  withRect areaMisc \x y w h -> [C.block| void {
    Fl_Button* buttonMisc = new Fl_Button($(int x), $(int y), $(int w), $(int h), "Other tools");
    buttonMisc->color($(Fl_Color miscColor));
    buttonMisc->callback($(Fl_Callback cbButtonMisc));
    buttonMisc->labelsize(13);
  } |]

  cbTermWindow <- makeCallback \_ _ -> windowCloser (return ()) termWindow
  [C.block| void {
    $(Fl_Window* termWindow)->end();
    $(Fl_Window* termWindow)->resizable($(Fl_Simple_Terminal* term));
    $(Fl_Window* termWindow)->callback($(Fl_Callback cbTermWindow));
    $(Fl_Window* termWindow)->show();
  } |]

  -- Support drag and drop onto mac app icon
  when macOS do
    fpOpen <- makeOpenCallback \cstr -> do
      str <- peekUTF8 cstr
      sink $ EventOnyx $ startLoad makeMenuBar hasAudio [str]
    [C.block| void {
      fl_open_callback($(void (*fpOpen)(const char*)));
    } |]

  let logChan = logIO $ sink . EventMsg
      wait = if macOS
        then [C.exp| int { Fl::wait(1e20) } |] >> return True
        else (/= 0) <$> [C.exp| int { Fl::wait() } |]

  -- TODO: catch errors that reach top level,
  -- and close the GUI with a nice error message

  let fixLeadingSpace s = case span (== ' ') s of
        -- leading space on a terminal line doesn't work for some reason.
        -- just replace with black characters on black background
        (""    , _   ) -> s
        (spaces, rest) -> "\ESC[30m" <> ('.' <$ spaces) <> "\ESC[0m" <> rest
  mapM_ (addTerm term . TermLog . fixLeadingSpace)
    [ "\ESC[35mOnyx\ESC[0m Music Game Toolkit, version " <> showVersion version
    , "Select an option below to get started."
    , ""
    , "  \ESC[32mLoad a song\ESC[0m     | bring songs to a new game"
    , "                  | change song speed, reassign instruments"
    , "                  | chart preview tool"
    , ""
    , "  \ESC[31mBatch recompile\ESC[0m | \"Load a song\" operations on multiple songs"
    , ""
    , "  \ESC[33mConvert/pack\ESC[0m    | edit songs without changing games"
    , "                  | make packs, or extract from packs"
    , "                  | switch console, e.g. \ESC[32m360\ESC[0m -> \ESC[36mPS3\ESC[0m"
    , ""
    , "  \ESC[36mOther tools\ESC[0m     | midi/audio utilities"
    , "                  | lipsync/dryvox tools"
    , ""
    ]

  case initSongs of
    [] -> return ()
    _  -> sink $ EventOnyx $ startLoad makeMenuBar hasAudio initSongs

  void $ runResourceT $ (`runReaderT` sink) $ logChan let
    -- when we have processed a max number of messages, we want to do a quick
    -- redraw and then continue processing. hopefully awake can be used when
    -- fltk's not in control? documentation is unclear
    process 0               = liftIO [C.block| void { Fl::awake(); } |]
    process messageCapacity = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ addTerm term $ toTermMessage pair
          EventFail   msg  -> liftIO $ addTerm term $ TermError msg
          EventIO     act  -> liftIO act
          EventOnyx   act  -> safeOnyx act
        process $ messageCapacity - 1
    loop = liftIO [C.exp| int { Fl::program_should_quit() } |] >>= \case
      0 -> liftIO wait >>= \case
        False -> return ()
        True  -> process maxMessagesAtOnce >> loop
      _ -> return ()
    maxMessagesAtOnce = 20 :: Int
    in do
      readPreferences >>= stackIO . applyThreads
      unless hasAudio $ warn
        "Couldn't open audio device"
      loop
  [C.block| void { Fl::flush(); } |] -- dunno if required

-- | Get selected (toggled) menu item labels from a menu button
getSelectedMenuItems :: Ptr Fl_Menu_Button -> IO [T.Text]
getSelectedMenuItems menu = do
  n <- [C.exp| int { $(Fl_Menu_Button* menu)->size() } |]
  fmap catMaybes $ forM [0 .. n - 1] \i -> do
    flags <- [C.exp| int { $(Fl_Menu_Button* menu)->menu()[$(int i)].flags } |]
    let flMenuValue = [C.pure| int { FL_MENU_VALUE } |]
    if flags .&. flMenuValue /= 0
      then do
        cstr <- [C.exp| const char* { $(Fl_Menu_Button* menu)->menu()[$(int i)].label() } |]
        if cstr == nullPtr
          then return Nothing
          else Just <$> (T.pack <$> peekUTF8 cstr)
      else return Nothing
