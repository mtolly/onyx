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
import           Data.Bits                         ((.&.))
import qualified Data.ByteString                   as B
import qualified Data.ByteString.Char8             as B8
import qualified Data.ByteString.Lazy              as BL
import           Data.Char                         (isSpace, toLower, toUpper)
import           Data.Coerce                       (coerce)
import           Data.Conduit.Audio                (integralSample, mapSamples)
import           Data.Fixed                        (Milli)
import           Data.Foldable                     (toList)
import           Data.Function                     ((&))
import           Data.Functor                      ((<&>))
import qualified Data.HashMap.Strict               as HM
import           Data.Int                          (Int64)
import           Data.IORef                        (newIORef, readIORef,
                                                    writeIORef)
import           Data.List.Extra                   (elemIndex, intercalate,
                                                    nubOrd, sortOn)
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map                          as Map
import           Data.Maybe                        (catMaybes, fromMaybe,
                                                    isJust, isNothing,
                                                    listToMaybe, mapMaybe)
import           Data.Monoid                       (Endo (..))
import qualified Data.Set                          as Set
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as TE
import           Data.Text.UTF8Proc                (utf8proc_charwidth)
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
import           Onyx.Audio                        (Audio (..), audioChannels,
                                                    audioLength, audioRate,
                                                    buildSource', makeXMAFSB3,
                                                    makeXMAPieces, runAudio)
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
import           Onyx.GUI.Batch
import           Onyx.GUI.Binding
import           Onyx.GUI.Core
import           Onyx.GUI.Fluid                    (adaptWindow, inGroup,
                                                    lockMinimumSize,
                                                    menuBarHeight)
import           Onyx.GUI.Misc
import           Onyx.GUI.Song
import           Onyx.GUI.Util                     (askFolder, checkValue,
                                                    peekUTF8, setCallback,
                                                    setLabelTooltip,
                                                    withMaybeUTF8,
                                                    withNativeFileChooser,
                                                    withUTF8)
import           Onyx.Harmonix.Ark.GH2             (GH2InstallLocation (..))
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Harmonix.MOGG                (oggToMoggFiles)
import           Onyx.Harmonix.RockBand.Score
import           Onyx.Harmonix.RockBand.SongCache  (hardcodeSongCacheIDs)
import           Onyx.Image.DXT                    (readRBImageMaybe)
import           Onyx.Import
import           Onyx.Keys.Ranges                  (closeShiftsFile)
import           Onyx.Localization                 (L (..), Language (..),
                                                    MessageOutput, l10n,
                                                    l10nArgs, l10nFull,
                                                    loadLanguage, vBool,
                                                    vNumber, vString)
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
import           Onyx.Reductions                   (loadFretLimit, simpleReduce)
import           Onyx.Resources                    (getResourcesPath)
import           Onyx.StackTrace
import           Onyx.Util.Files                   (commonDir, copyDirRecursive,
                                                    osOpenFile, osShowFolder)
import           Onyx.Util.Handle                  (fileReadable,
                                                    saveHandleFolder)
import           Onyx.Util.Text.RichText           (RichText (..), Tag (..),
                                                    parseRichText)
import qualified Onyx.Xbox.STFS                    as STFS
import           Optics                            (mapped, (%), (%~), (.~))
import           Paths_onyx_exe_toolkit            (version)
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
import           Text.Read                         (readMaybe)

#ifdef WINDOWS
import           Foreign                           (intPtrToPtr)
import           Graphics.Win32                    (loadIcon)
import           System.Win32                      (HINSTANCE)
#endif

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
C.include "hs_sticky_menu_button.h"
C.include "hs_value_slider.h"
C.include "FL/platform.H"
C.include "FL/fl_string_functions.h"
-- FLUID-generated from the layouts in fluid, see Setup.hs
C.include "multiple_songs.h"
C.include "console.h"
C.include "misc.h"
C.include "quick_convert.h"

promptLoad :: (?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> Bool -> IO ()
promptLoad sink makeMenuBar hasAudio = withNativeFileChooser
  [C.pure| int { Fl_Native_File_Chooser::BROWSE_MULTI_FILE } |]
  \picker -> do
    withUTF8 (l10n "file-picker-load-song") \s ->
      [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* s)); } |]
    result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
    when (result == 0) do
      n <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->count() } |]
      fs <- forM [0 .. n - 1] \i -> do
        cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename($(int i)) } |]
        peekUTF8 cstr
      sink $ EventOnyx $ startLoad makeMenuBar hasAudio fs

startLoad :: (?loc :: L) => (Width -> Bool -> IO CInt) -> Bool -> [FilePath] -> Onyx ()
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
  :: (?loc :: L)
  => (Width -> Bool -> IO CInt)
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
        song <- loadTracks proj.projectSongYaml $ takeDirectory proj.projectLocation </> proj.projectSongYaml.global.fileMidi
        withAudio song Nothing
      withAudio song maybeAudio = stackIO $ sink $ EventOnyx do
        prefs <- readPreferences
        let ?preferences = prefs
        stackIO $ launchWindow sink makeMenuBar proj song maybeAudio albumArt imp.impFormatRB
      selectPlan [] = withNoPlan
      selectPlan [k] = withPlan k
      selectPlan (k : ks) = stackIO $ sink $ EventIO do
        withUTF8 (l10n "audio-plan-select") \cmsg -> withUTF8 (show ks) \cks -> withUTF8 (show k) \ck -> do
          n <- [C.exp| int { fl_choice("%s", $(char* cks), $(char* ck), nullptr, $(char* cmsg)) } |]
          sink $ EventOnyx case n of
            1 -> withPlan k
            _ -> selectPlan ks
  if hasAudio
    then selectPlan $ HM.keys proj.projectSongYaml.plans
    else withNoPlan

multipleSongsWindow
  :: (?loc :: L)
  => (Event -> IO ())
  -> (Width -> Bool -> IO CInt)
  -> Bool
  -> [Importable OnyxInner]
  -> IO ()
multipleSongsWindow sink makeMenuBar hasAudio imps = do
  -- Layout lives in fluid/multiple_songs.fl; see Onyx.GUI.Fluid. Everything
  -- built here is in the layout's own (unstretched, menu-bar-less)
  -- coordinates, and adaptWindow scales and shifts it along with the rest.
  ui <- [C.exp| void* { new MultipleSongsWindow() } |]
  let window       = [C.pure| Fl_Window* { ((MultipleSongsWindow*)$(void* ui))->window } |]
      scroll       = [C.pure| Fl_Scroll* { ((MultipleSongsWindow*)$(void* ui))->scroll } |]
      pack         = [C.pure| Fl_Pack*   { ((MultipleSongsWindow*)$(void* ui))->pack } |]
      selectButton = [C.pure| Fl_Button* { ((MultipleSongsWindow*)$(void* ui))->selectButton } |]
      openButton   = [C.pure| Fl_Button* { ((MultipleSongsWindow*)$(void* ui))->openButton } |]

  -- scrollbar_size is an internal property rather than a widget rectangle, so
  -- adaptWindow can't scale it; it has to match the gap the layout leaves
  let barSizeScaled = stretchX 15
  -- NOTE: skipping custom resize handler for scrollbar width adjustment
  [C.block| void { $(Fl_Scroll* scroll)->scrollbar_size($(int barSizeScaled)); } |]

  [C.block| void { $(Fl_Pack* pack)->begin(); } |]
  checks <- forM imps \imp -> do
    -- TODO ampersands in the label text here turn into "underline next character".
    -- need to figure out how to escape
    let entry = l10nArgs "batch-tab-song-entry"
          [ ("title", vString $ fromMaybe "" imp.impTitle)
          , ("has-title", vBool $ isJust imp.impTitle)
          , ("artist", vString $ fromMaybe "" imp.impArtist)
          , ("has-artist", vBool $ isJust imp.impArtist)
          , ("is-2x", vBool imp.imp2x)
          ]
    check <- [C.exp| Fl_Check_Button* { new Fl_Check_Button(0, 0, 400, 25) } |]
    withUTF8 entry \clabel ->
      [C.block| void {
        $(Fl_Check_Button* check)->copy_label($(char* clabel));
        $(Fl_Check_Button* check)->value(0);
      } |]
    return check
  [C.block| void { $(Fl_Pack* pack)->end(); } |]

  let updateButtons = do
        bools <- mapM checkValue checks
        let newValue = not $ and bools
            checked = length $ filter id bools
            selectLabel, openLabel :: T.Text
            selectLabel = if newValue then l10n "song-list-select-all" else l10n "song-list-select-none"
            openLabel = case checked of
              0 -> l10n "song-list-close"
              _ -> l10nArgs "song-list-open"
                [("count", vNumber $ fromIntegral checked)]
        withUTF8 selectLabel \csel ->
          [C.block| void { $(Fl_Button* selectButton)->copy_label($(char* csel)); } |]
        withUTF8 openLabel \copen ->
          [C.block| void { $(Fl_Button* openButton)->copy_label($(char* copen)); } |]
        let color = if checked == 0 then defaultColor else taskColor
        [C.block| void { $(Fl_Button* openButton)->color($(Fl_Color color)); } |]
        [C.block| void { $(Fl_Button* openButton)->redraw(); } |]
  updateButtons
  setCallback selectButton do
    bools <- mapM checkValue checks
    let newValue = if and bools then 0 else 1 :: CInt
    forM_ checks \check ->
      [C.block| void { $(Fl_Check_Button* check)->value($(int newValue)); } |]
    updateButtons
  setCallback openButton do
    bools <- mapM checkValue checks
    forM_ (zip bools imps) \(b, imp) -> when b do
      sink $ EventOnyx $ void $ forkOnyx do
        continueImport makeMenuBar hasAudio imp
    [C.block| void { $(Fl_Window* window)->hide(); } |]
  forM_ checks \check -> setCallback check updateButtons

  menuHeight <- adaptWindow window
  when (menuHeight /= 0) do
    inGroup (windowGroup window) $ void $ makeMenuBar (Width 500) True
  lockMinimumSize window
  [C.block| void {
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
  :: (?preferences :: Preferences, ?loc :: L)
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
      title = T.unpack $ T.replace "/" "_" $ fromMaybe (l10n "default-song-title") proj.projectSongYaml.metadata.title
  window <- withSize windowSize \w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h)) } |]
  withUTF8 title \ctitle ->
    [C.block| void { $(Fl_Window* window)->copy_label($(char* ctitle)); } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
  } |]
  setSizeRange window windowSize

  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]

  (modifyMeta, _metaTab) <- makeTab windowRect (l10n "tab-metadata") \rect tab -> do
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
        _ <- withRect (Rectangle (Position (X 0) (Y 0)) (Size (Width 800) (Height 5)))
          \bx by bw bh ->
            [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
        return ()
      fullWidth $ simpleTextGet (l10n "label-title") (.title) (#title .~)
      fullWidth \rect' -> do
        let (artistArea, trimClock 0 0 0 50 -> yearArea) = chopRight 120 rect'
        simpleTextGet (l10n "label-artist") (.artist) (#artist .~) artistArea
        simpleTextGetInt (l10n "label-year") (.year) (#year .~) yearArea
      fullWidth \rect' -> do
        let (albumArea, trimClock 0 0 0 30 -> trackNumArea) = chopRight 120 rect'
        simpleTextGet (l10n "label-album") (.album) (#album .~) albumArea
        simpleTextGetInt (l10n "label-track-number") (.trackNumber) (#trackNumber .~) trackNumArea
      fullWidth $ simpleTextGet (l10n "label-author") (.author) (#author .~)
      (genreInput, subgenreInput) <- fullWidth \rect' -> let
        [trimClock 0 50 0 0 -> genreArea, trimClock 0 0 0 50 -> subgenreArea] = splitHorizN 2 rect'
        in do
          genreInput    <- liftIO $ simpleText (l10n "label-genre"   ) flNormalInput (.genre   ) genreArea
          subgenreInput <- liftIO $ simpleText (l10n "label-subgenre") flNormalInput (.subgenre) subgenreArea
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
              -- TODO we probably want to load and scale the image ourselves,
              -- avoid needing to go through gen/cover.png + better downscale through stbir
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
              inside (T.unpack $ l10nArgs "loading-image" [("path", vString $ T.pack f)]) do
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
          setCallback cover $ withNativeFileChooser
            [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
            \picker -> do
              withUTF8 (l10n "file-picker-load-album-art") \s -> [C.block| void {
                $(Fl_Native_File_Chooser* picker)->title($(char* s));
                $(Fl_Native_File_Chooser* picker)->filter("*.{png,jpg,jpeg,png_xbox,png_wii}");
              } |]
              result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
              when (result == 0) do
                cpath <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
                path <- peekUTF8 cpath
                swapImage path
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
        setCallback genreInput calcGenre
        setCallback subgenreInput calcGenre
        [C.block| void {
          $(Fl_Input* genreInput   )->when(FL_WHEN_CHANGED);
          $(Fl_Input* subgenreInput)->when(FL_WHEN_CHANGED);
        } |]
      padded 5 10 5 10 (Size (Width 500) (Height 30)) \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
        simpleCheck (l10n "meta-convert")     (.convert   ) (\b meta -> meta { convert    = b }) r1
        simpleCheck (l10n "meta-rhythm-keys") (.rhythmKeys) (\b meta -> meta { rhythmKeys = b }) r2
        simpleCheck (l10n "meta-rhythm-bass") (.rhythmBass) (\b meta -> meta { rhythmBass = b }) r3
        simpleCheck (l10n "meta-auto-emh")    (.catEMH    ) (\b meta -> meta { catEMH     = b }) r4
        simpleCheck (l10n "meta-expert-only") (.expertOnly) (\b meta -> meta { expertOnly = b }) r5
        simpleCheck (l10n "meta-cover")       (.cover     ) (\b meta -> meta { cover      = b }) r6
      padded 5 10 5 10 (Size (Width 500) (Height 30)) \rect' -> do
        let [r1, r2, r3, r4, r5, r6] = splitHorizN 6 rect'
            editLangs f meta = meta
              { languages = f meta.languages
              }
            lang lbl val = simpleCheck lbl
              (elem val . (.languages))
              (\b -> editLangs if b then (val :) else id)
        lang (l10n "language-english")  "English"  r1
        lang (l10n "language-french")   "French"   r2
        lang (l10n "language-italian")  "Italian"  r3
        lang (l10n "language-spanish")  "Spanish"  r4
        lang (l10n "language-german")   "German"   r5
        lang (l10n "language-japanese") "Japanese" r6
        -- language-clear function at the bottom; this makes it happen first
        tell $ return $ Endo \yaml -> (yaml :: SongYaml FilePath)
          & #metadata .~ editLangs (const []) yaml.metadata
      liftIO [C.block| void { $(Fl_Pack* packBottom)->end(); } |]
      resizeBox <- liftIO $ withRect rectResize \bx by bw bh ->
        [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
      liftIO [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Box* resizeBox)); } |]
    return (yamlModifier, tab)

  (modifyInsts, _instTab) <- makeTab windowRect (l10n "tab-instruments") \rect tab -> do
    setTabColor tab homeTabColor
    let instRect = trimClock 10 10 10 10 rect
    tree <- withRect instRect \tx ty tw th ->
      [C.exp| Fl_Tree* { new Fl_Tree($(int tx), $(int ty), $(int tw), $(int th)) } |]
    [C.block| void {
      Fl_Tree* t = $(Fl_Tree* tree);
      t->end();
      t->selectmode(FL_TREE_SELECT_NONE);
      t->showcollapse(0);
    } |]
    withUTF8 (l10n "tab-instruments") \cs ->
      [C.block| void { $(Fl_Tree* tree)->root_label($(char* cs)); } |]
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
            let label = l10nArgs "tier" [("number", vNumber $ fromIntegral i)]
            withUTF8 label \clabel ->
              [C.block| void { $(Fl_Choice* choice)->add($(char* clabel)); } |]
          withUTF8 (l10n "rank") \crank ->
            [C.block| void { $(Fl_Choice* choice)->add($(char* crank)); } |]
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
          setCallback choice controlRank
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
          mbGRYBO <- forM part.grybo \pg -> addType (l10n "mode-5fret") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #grybo .~ do
                guard isChecked
                Just $ (pg :: ModeFive) & #difficulty .~ diff
          mbGHL <- forM part.ghl \pg -> addType (l10n "mode-6fret") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #ghl .~ do
                guard isChecked
                Just $ (pg :: ModeSix) & #difficulty .~ diff
          mbProKeys <- forM part.proKeys \pk -> addType (l10n "mode-pro-keys") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pk.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #proKeys .~ do
                guard isChecked
                Just $ (pk :: ModeProKeys) & #difficulty .~ diff
          mbProGuitar <- forM part.proGuitar \pg -> addType (l10n "mode-pro-guitar") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pg.difficulty
            return \isChecked curPart -> do
              diff <- getDiff
              return $ curPart & #proGuitar .~ do
                guard isChecked
                Just $ (pg :: ModeProGuitar FilePath) & #difficulty .~ diff
          mbDrums <- forM part.drums \pd -> addType (l10n "mode-drums") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pd.difficulty
            getMode <- makeChoice itemCheck pd.mode \case
              Drums4     -> l10n "drums-mode-4lane"
              Drums5     -> l10n "drums-mode-5lane"
              DrumsPro   -> l10n "drums-mode-pro"
              DrumsReal  -> l10n "drums-mode-real"
              DrumsElite -> l10n "drums-mode-elite"
            getKicks <- makeChoice itemCheck pd.kicks \case
              Kicks1x   -> l10n "bass-pedal-1x"
              Kicks2x   -> l10n "bass-pedal-2x"
              KicksBoth -> l10n "bass-pedal-both-input"
            getKit <- makeChoice itemCheck pd.kit \case
              HardRockKit   -> l10n "drums-rb3-kit-hard-rock"
              ArenaKit      -> l10n "drums-rb3-kit-arena"
              VintageKit    -> l10n "drums-rb3-kit-vintage"
              TrashyKit     -> l10n "drums-rb3-kit-trashy"
              ElectronicKit -> l10n "drums-rb3-kit-electronic"
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
          mbVocal <- forM part.vocal \pv -> addType (l10n "mode-vocals") \itemCheck -> do
            getDiff <- makeDifficulty itemCheck pv.difficulty
            getCount <- makeChoice itemCheck pv.count \case
              Vocal1 -> l10n "vocals-solo"
              Vocal2 -> l10n "vocals-harmonies-2"
              Vocal3 -> l10n "vocals-harmonies-3"
            getGender <- makeChoiceFrom [Nothing, Just Male, Just Female] itemCheck pv.gender \case
              Nothing     -> l10n "gender-unspecified"
              Just Male   -> l10n "gender-male"
              Just Female -> l10n "gender-female"
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
          mbMania <- forM part.mania \pm -> addType (l10n "mode-mania") \_itemCheck -> do
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
  (_previewTab, cleanupGL) <- makeTab windowRect (l10n "tab-preview") \rect tab -> mdo
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
    setCallback volSlider updateVolume
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
    setCallback playButton togglePlay
    [C.block| void { $(Fl_Button* playButton)->shortcut(' '); } |]
    setCallback counter do
      ss <- takeState
      ss' <- case ss.songPlaying of
        Nothing -> redrawGL >> return ss
        Just ps -> stopPlaying ss.songTime ps >>= startPlaying
      putState ss'
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

  _starsTab <- makeTab windowRect (l10n "tab-stars") \rect tab -> do
    setTabColor tab homeTabColor
    pack <- withRect rect \px py pw ph ->
      [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
    [C.block| void { $(Fl_Pack* pack)->end(); } |]
    sink $ EventOnyx $ void $ forkOnyx do
      let input = takeDirectory proj.projectLocation </> proj.projectSongYaml.global.fileMidi
      mid <- F.loadMIDI input
      let fixed = F.onyxToFixed mid.tracks
          foundTracksRB3 = getScoreTracks    fixed
          foundTracksGH2 = getScoreTracksGH2 fixed
      -- TODO this is a hack to not hold onto the whole midi file in memory, should find a better way!
      stackIO $ void $ Exc.evaluate $ length $ show (foundTracksRB3, foundTracksGH2)
      stackIO $ sink $ EventIO $ mdo
        [C.block| void { $(Fl_Pack* pack)->begin(); } |]

        -- this should get real locale-based formatting eventually
        let commafy n = T.pack $ reverse $ go $ reverse $ show n
            go (x : y : z : rest@(_ : _))
              = [x, y, z] <> reverse (T.unpack $ l10n "thousands-separator") <> go rest
            go xs = xs

        -- RB3
        padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          void $ withRect rect' \bx by bw bh ->
            withUTF8 (l10n "stars-rb3-title") \cs -> [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
        getTracksRB3 <- padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          starSelectors rect' foundTracksRB3 updateLabelRB3 scoreTrackName
            [ (l10n "part-guitar", [ScoreGuitar, ScoreProGuitar])
            , (l10n "part-bass"  , [ScoreBass  , ScoreProBass  ])
            , (l10n "part-keys"  , [ScoreKeys  , ScoreProKeys  ])
            , (l10n "part-drums" , [ScoreDrums , ScoreProDrums ])
            , (l10n "part-vocal" , [ScoreVocals, ScoreHarmonies])
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
                  starArgs n = [("score", vString $ commafy n)]
              setLabel l1S $ maybe "" (l10nArgs "stars-1"    . starArgs) stars.stars1
              setLabel l2S $ maybe "" (l10nArgs "stars-2"    . starArgs) stars.stars2
              setLabel l3S $ maybe "" (l10nArgs "stars-3"    . starArgs) stars.stars3
              setLabel l4S $ maybe "" (l10nArgs "stars-4"    . starArgs) stars.stars4
              setLabel l5S $ maybe "" (l10nArgs "stars-5"    . starArgs) stars.stars5
              setLabel lGS $ maybe "" (l10nArgs "stars-gold" . starArgs) stars.starsGold
        updateLabelRB3

        -- GH2
        padded 5 10 5 10 (Size (Width 800) (Height 50)) \rect' ->
          void $ withRect rect' \bx by bw bh ->
            withUTF8 (l10n "stars-gh-title") \cs ->
              [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
        getTracksGH2 <- padded 5 150 5 150 (Size (Width 800) (Height 50)) \rect' ->
          starSelectors rect' foundTracksGH2 updateLabelGH2 scoreTrackNameGH2
            [ (l10n "stars-lead", [ScoreGH2Guitar])
            , (l10n "stars-coop", [ScoreGH2Bass, ScoreGH2Rhythm])
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
              let starArgs n = [("score", vString $ commafy n)]
              if length bases <= 1
                then do
                  setLabel ghBase  $ l10nArgs "stars-gh-base" $ starArgs base
                  setLabel ghFour  $ l10nArgs "stars-gh-4"    $ starArgs $ mult 2
                  setLabel gh2Five $ l10nArgs "stars-gh2-5"   $ starArgs $ mult 2.8
                  setLabel gh1Five $ l10nArgs "stars-gh1-5"   $ starArgs $ mult 3
                else do
                  setLabel ghBase  $ l10nArgs "stars-gh-base" $ starArgs base
                  setLabel ghFour  ("" :: T.Text)
                  setLabel gh2Five $ l10nArgs "stars-gh2-5"   $ starArgs $ mult 2
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

  rb3Tab <- makeTab windowRect (l10n "game-tab-rb3") \rect tab -> do
    setTabColor tab functionTabColor
    songPageRB3 sink rect tab proj \qcPossible tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            RB3CON      _ -> l10n "build-rb3-con"
            RB3PKG      _ -> l10n "build-rb3-pkg"
            RB3Magma    _ -> l10n "build-magma"
            RB3LoosePS3 _ -> l10n "build-rb3-ps3"
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
  rb2Tab <- makeTab windowRect (l10n "game-tab-rb2") \rect tab -> do
    setTabColor tab functionTabColor
    songPageRB2 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            RB2CON      _ -> l10n "build-rb2-con"
            RB2PKG      _ -> l10n "build-rb2-pkg"
            RB2LoosePS3 _ -> l10n "build-rb2-ps3"
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
  psTab <- makeTab windowRect (l10n "song-tab-ch-ps") \rect tab -> do
    setTabColor tab functionTabColor
    songPagePS sink rect tab proj \tgt create -> sink $ EventOnyx do
      newPreferences <- readPreferences
      proj' <- stackIO $ fullProjModify [applyDownmix newPreferences] proj
      let name = T.unpack $ case create of
            PSDir _ -> l10n "build-ch-folder"
            PSZip _ -> l10n "build-ch-zip"
            PSSng _ -> l10n "build-ch-sng"
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
  gh1Tab <- makeTab windowRect (l10n "game-tab-gh1") \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH1 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            GH1ARK{}    -> l10n "build-gh1-ark"
            GH1DIYPS2{} -> l10n "build-gh1-diy"
          task = case create of
            GH1ARK fout -> do
              installGH1 tgt proj' fout
              return [fout]
            GH1DIYPS2 fout -> do
              makeGH1DIY tgt proj' fout
              return [fout]
      sink $ EventOnyx $ startTasks [(name, task)]
    return tab
  gh2Tab <- makeTab windowRect (l10n "game-tab-gh2") \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH2 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            GH2LIVE{}   -> l10n "build-gh2-live"
            GH2ARK{}    -> l10n "build-gh2-ark"
            GH2DIYPS2{} -> l10n "build-gh2-diy"
          task = case create of
            GH2LIVE fout    -> do
              tmp <- buildGH2LIVE tgt proj'
              stackIO $ Dir.copyFile tmp fout
              warn $ T.unpack $ l10n "warning-gh2-packs"
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
  gh3Tab <- makeTab windowRect (l10n "game-tab-gh3") \rect tab -> do
    setTabColor tab functionTabColor
    songPageGH3 sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            GH3LIVE{}     -> l10n "build-gh3-live"
            GH3PKG{}      -> l10n "build-gh3-pkg"
            GH3LoosePS3{} -> l10n "build-gh3-ps3"
          warnCache = warn $ T.unpack $ l10n "warning-gh3-cache"
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
  worTab <- makeTab windowRect (l10n "game-tab-ghwor") \rect tab -> do
    setTabColor tab functionTabColor
    songPageGHWOR sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            GHWORLIVE{}     -> l10n "build-ghwor-live"
            GHWORPKG{}      -> l10n "build-ghwor-pkg"
            GHWORLoosePS3{} -> l10n "build-ghwor-ps3"
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
  rrTab <- makeTab windowRect (l10n "game-tab-rock-revolution") \rect tab -> do
    setTabColor tab functionTabColor
    songPageRR sink rect tab proj \tgt create -> do
      proj' <- fullProjModify [] proj
      let name = T.unpack $ case create of
            RRLIVE{} -> l10n "build-rr-live"
            RRPKG{}  -> l10n "build-rr-pkg"
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
  utilsTab <- makeTab windowRect (l10n "tab-utilities") \rect tab -> do
    setTabColor tab functionTabColor
    pack <- withRect rect \px py pw ph ->
      [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        withUTF8 (l10n "util-button-preview") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      setCallback btn $ sink $ EventIO do
        let presetName = T.pack $ proj.projectTemplate <> "_player"
        saveFileChooser (l10n "file-picker-save-web-preview") presetName (T.pack <$> ?preferences.prefDirPreview) \f -> do
          proj' <- fullProjModify [] proj
          let task = do
                tmp <- buildPlayer Nothing proj'
                copyDirRecursive tmp f
                return [f]
          sink $ EventOnyx $ startTasks [(T.unpack $ l10n "util-button-preview", task)]
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        withUTF8 (l10n "util-button-reduce") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      setCallback btn $ sink $ EventIO $ withNativeFileChooser
        [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |]
        \picker -> do
          withUTF8 (l10n "util-save-midi-dialog") \cs ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* cs)); } |]
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
                  -- start from pre-import midi file if present
                  input = fromMaybe proj.projectSongYaml.global.fileMidi
                    proj.projectSongYaml.global.fileMidiOriginal
              sink $ EventOnyx $ startTasks [(f', do
                fretLimit <- loadFretLimit
                simpleReduce fretLimit (takeDirectory proj.projectLocation </> input) f'
                return [f'])]
    padded 5 10 5 10 (Size (Width 800) (Height 35)) \rect' -> do
      btn <- withRect rect' \bx by bw bh ->
        withUTF8 (l10n "midi-button-prokeys") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
      [C.block| void { $(Fl_Button* btn)->color($(Fl_Color taskColor)); } |]
      setCallback btn $ sink $ EventOnyx do
        startTasks [(T.unpack $ l10n "util-task-prokeys", proKeysHanging Nothing proj >> return [])]
    [C.block| void { $(Fl_Pack* pack)->end(); } |]
    [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Pack* pack)); } |]
    return tab

  let tabsToDisable = [rb3Tab, rb2Tab, psTab, gh1Tab, gh2Tab, gh3Tab, worTab, rrTab, utilsTab]
  (startTasks, cancelTasks) <- makeTab windowRect (l10n "tab-task") \rect tab -> do
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
  setCallback tabs $ updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* rb3Tab)); } |]

  setCallback window $ windowCloser
    (cancelTasks >> cleanupGL >> mapM_ release proj.projectRelease)
    window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
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

makePS3PackPresetDropdown :: (?loc :: L) => (Event -> IO ()) -> Rectangle -> IO (IO QuickPS3Folder)
makePS3PackPresetDropdown _sink rect = do
  let labelOne = l10n "quick-convert-folder-combine-per-pack"
      labelSeparate = l10n "quick-convert-folder-each"
      labelCustomInitial = l10n "quick-convert-folder-custom-initial"
      labelCustom txt = l10nArgs "quick-convert-folder-custom" [("folder", vString txt)]
      customPrompt :: T.Text
      customPrompt = l10n "quick-convert-folder-custom-prompt"
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

makeModeDropdown :: (?loc :: L) => Rectangle -> [(T.Text, a)] -> (a -> IO ()) -> IO ()
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
  :: (Eq track, ?loc :: L)
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
    withUTF8 (l10n "part-none") \cnone ->
      [C.block| void { $(Fl_Choice* choice)->add($(char* cnone)); } |]
    forM_ matchTracks \(strack, diff, _) -> do
      let itemLabel = trackToName strack <> " " <> case diff of
            RB.Easy   -> l10n "diff-easy"
            RB.Medium -> l10n "diff-medium"
            RB.Hard   -> l10n "diff-hard"
            RB.Expert -> l10n "diff-expert"
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
  :: (?loc :: L)
  => Rectangle
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
  cancelButton <- withUTF8 (l10n "task-cancel") \cs -> withRect buttonRect' \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
  showButton <- withUTF8 (l10n "task-show-files") \cs -> withRect buttonRect' \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
  [C.block| void { $(Fl_Button* showButton)->hide(); } |]

  -- functions
  taskStatus <- newMVar (Left Nothing) :: IO (MVar (Either
      (Maybe (FilePath, [FilePath])) -- finished, maybe have files to show the user
      (Int, Int, [[FilePath]], Int, ThreadId) -- tasks in progress
    ))
  let taskMessage :: (?loc :: L) => (MessageLevel, Message) -> IO ()
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
            addTerm termTask $ TermLog $ T.unpack $ l10n "task-cancelled"
            killThread tid
            putFinalResults success
          Left res -> putMVar taskStatus $ Left res
        cbEnd'
      updateStatus fn = takeMVar taskStatus >>= \case
        Left res -> putMVar taskStatus $ Left res -- maybe task was cancelled
        Right oldStatus -> do
          let status@(done, total, success, failure, _) = fn oldStatus
          withUTF8 (l10nArgs "task-status"
            [ ("done", vNumber $ fromIntegral done)
            , ("total", vNumber $ fromIntegral total)
            , ("succeeded", vNumber $ fromIntegral $ length success)
            , ("failed", vNumber $ fromIntegral failure)
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
            liftIO $ sink $ EventIO $ addTerm termTask $ TermStart (T.unpack $ l10nArgs "task-number" [("number", vNumber $ fromIntegral (i :: Int))]) taskName
            errorToEither task >>= \case
              Left e -> liftIO $ sink $ EventIO do
                forM_ e.getMessages $ addTerm termTask . TermError
                updateStatus \case
                  (done, total, success, failure, tid') -> (done + 1, total, success, failure + 1, tid')
              Right fs -> liftIO $ sink $ EventIO do
                addTerm termTask $ TermSuccess case fs of
                  [] -> ""
                  _  -> unlines $ (T.unpack $ l10n "task-created-files") : map ("  " <>) fs
                updateStatus \case
                  (done, total, success, failure, tid') -> (done + 1, total, fs : success, failure, tid')
          liftIO do
            putMVar taskStatus $ Right (0, length tasks, [], 0, tid)
            cbStart'
            updateStatus id

  setCallback cancelButton $ sink $ EventIO cancelTasks
  setCallback showButton $ sink $ EventIO do
    res <- takeMVar taskStatus
    case res of
      Left (Just (dir, files)) -> osShowFolder dir files
      _                        -> return ()
    putMVar taskStatus res

  return (startTasks, cancelTasks)

data AudioSpec = AudioSpec
  { path     :: FilePath
  , format   :: T.Text
  , frames   :: Int
  , rate     :: Int
  , channels :: Int
  }

getAudioSpec :: FilePath -> IO (Maybe AudioSpec)
getAudioSpec f = audioChannels f >>= \case
  Nothing -> return Nothing
  Just ch -> audioRate f >>= \case
    Nothing -> return Nothing
    Just r -> audioLength f >>= \case
      Nothing -> return Nothing
      Just len -> let
        withFormat fmt = return $ Just AudioSpec
          { path     = f
          , format   = fmt
          , frames   = fromIntegral len
          , rate     = r
          , channels = ch
          }
        in case map toLower $ takeExtension f of
          ".wav"  -> withFormat "WAVE"
          ".ogg"  -> withFormat "Ogg Vorbis"
          ".flac" -> withFormat "FLAC"
          ".mp3"  -> withFormat "MP3"
          ".opus" -> withFormat "Opus"
          _       -> return Nothing

-- | Widgets for the GH2 pack creator tab, laid out in fluid/quick_convert.fl.
data PackTabWidgets = PackTabWidgets
  { pkFiles :: Ptr FileLoadPanel
  , pkGame  :: Ptr Fl_Box
  , pkName  :: Ptr Fl_Input
  , pkDesc  :: Ptr Fl_Input
  , pkCON   :: Ptr Fl_Button
  , pkLIVE  :: Ptr Fl_Button
  , pkRaw   :: Ptr Fl_Button
  }

miscPagePacks
  :: (?loc :: L)
  => (Event -> IO ())
  -> PackTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPagePacks sink widgets startTasks = do
  loadedSTFS <- newMVar []
  let btnCON  = widgets.pkCON
      btnLIVE = widgets.pkLIVE
      btnRaw  = widgets.pkRaw
      gameBox = widgets.pkGame
      nameInput = widgets.pkName
      descInput = widgets.pkDesc
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
                  [ l10n "pack-game-from-first"
                  , info.stfsMeta.md_TitleName
                  , "(" <> T.toUpper (T.pack $ showHex info.stfsMeta.md_TitleID "") <> ")"
                  ]
                []       -> ""
          withUTF8 gameLabel \clbl ->
            [C.block| void { $(Fl_Box* gameBox)->copy_label($(char* clbl)); } |]
  void $ fileLoadPanel widgets.pkFiles sink (l10nFull "file-type-con-live" []) modifyButtons [] searchSTFS
    \info -> let
      entry = T.pack $ stfsPath info
      sublines = concat
        [ take 1 info.stfsMeta.md_DisplayName
        , return $ T.unwords
          [ l10n "pack-game"
          , info.stfsMeta.md_TitleName
          , "(" <> T.toUpper (T.pack $ showHex info.stfsMeta.md_TitleID "") <> ")"
          ]
        ]
      in (entry, sublines)

  let doPrompt output = sink $ EventIO do
        stfs <- readMVar loadedSTFS
        withNativeFileChooser [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |] \picker -> do
          withUTF8 (l10n "pack-save-stfs") \cs ->
            [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* cs)); } |]
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
              taskLabel = T.unpack $ case output of
                PackExtracted -> l10nArgs "pack-task-extracted" [("path", vString $ T.pack f)]
                PackCON       -> l10nArgs "pack-task-con" [("path", vString $ T.pack f)]
                PackLIVE      -> l10nArgs "pack-task-live" [("path", vString $ T.pack f)]
              in [(taskLabel, task)]

  setCallback btnCON  $ doPrompt PackCON
  setCallback btnLIVE $ doPrompt PackLIVE
  setCallback btnRaw  $ doPrompt PackExtracted

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
  :: (?preferences :: Preferences, ?loc :: L)
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
            withUTF8 (l10n "quick-convert-invalid-pack-size") \clbl ->
              [C.block| void { $(Fl_Button* go)->copy_label($(char* clbl)); } |]
            [C.block| void { $(Fl_Button* go)->deactivate(); } |]
          Just [] -> do
            withUTF8 (l10n "quick-convert-no-songs-added") \clbl ->
              [C.block| void { $(Fl_Button* go)->copy_label($(char* clbl)); } |]
            [C.block| void { $(Fl_Button* go)->deactivate(); } |]
          Just packs -> do
            let lbl = l10nArgs "quick-convert-make-packs-count"
                  [("count", vNumber $ fromIntegral $ length packs)]
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
              withUTF8 (l10n "button-start") \clbl ->
                [C.block| void { $(Fl_Button* go)->copy_label($(char* clbl)); } |]
              [C.block| void { $(Fl_Button* go)->activate(); } |]
            else do
              withUTF8 (l10n "quick-convert-unsupported-format") \clbl ->
                [C.block| void { $(Fl_Button* go)->copy_label($(char* clbl)); } |]
              [C.block| void { $(Fl_Button* go)->deactivate(); } |]
      updateFiles f = do
        modifyMVar_ loadedFiles f
        sink $ EventIO $ updatePackGo >> updateInPlace
  (filesGroup, clearFiles) <- fileLoadWindow' filesRect sink (l10nFull "file-type-rb-song" []) updateFiles [] searchQuickSongs
    \qinput -> let
      entry = T.pack qinput.quickInputPath
      subline = l10nArgs "song-count"
        [("count", vNumber $ fromIntegral $ length qinput.quickInputSongs)]
      in (entry, [subline])

  -- right side: processors to transform midis and other files
  packProcessors <- withRect (trimClock 5 5 5 0 processorsRect) \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let processorBox = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 40))
      procFolder f qsong = do
        newFiles <- f qsong.quickSongFiles
        return qsong { quickSongFiles = newFiles }
      processors :: [(MessageOutput, QuickSong -> Onyx QuickSong)]
      processors =
        [ (,) (l10nFull "quick-convert-rb-black-venue" [])
          \qsong -> do
            let isRB3 = qsong.quickSongDTA.qdtaRB3
            procFolder (QRB.applyToMIDI $ return . blackVenue isRB3) qsong
        , (,) (l10nFull "quick-convert-rb-no-overdrive" [])
          $ procFolder $ QRB.applyToMIDI $ return . noOverdrive
        , (,) (l10nFull "quick-convert-rb-no-lanes-gbk" [])
          $ procFolder $ QRB.applyToMIDI $ return . noLanesGBK
        , (,) (l10nFull "quick-convert-rb-no-lanes-drums" [])
          $ procFolder $ QRB.applyToMIDI $ return . noLanesDrums
        , (,) (l10nFull "quick-convert-rb-no-drum-fills" [])
          $ procFolder $ QRB.applyToMIDI $ return . noDrumFills
        , (,) (l10nFull "quick-convert-rb-force-22-protar" [])
          $ procFolder $ QRB.applyToMIDI $ return . mustang22
        , (,) (l10nFull "quick-convert-rb-unmute-22-protar" [])
          $ procFolder $ QRB.applyToMIDI $ return . unmuteOver22
        , (,) (l10nFull "quick-convert-rb-decompress-milo" [])
          $ procFolder decompressMilos
        , (,) (l10nFull "quick-convert-rb-author-to-tag" [])
          $ return . transferAuthorToTag
        , (,) (l10nFull "quick-convert-rb-strip-title-tags" [])
          $ return . stripTitleTags
        , (,) (l10nFull "quick-convert-rb-note-shuffle" [])
          \qsong -> do
            shuffled <- noteShuffle qsong
            seed <- randomIO
            return $ addTitleTag "(Shuffle)" $ refreshSong seed shuffled
        , (,) (l10nFull "quick-convert-limit-easy-medium-frets" [])
          QRB.limitEasyMediumFrets
        ]
  processorGetters <- forM processors \(locResult, processor) -> do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip locResult check
    return do
      checked <- checkValue check
      return if checked then processor else return
  let getSongTransform = do
        procs <- sequence processorGetters
        return $ foldr (>=>) return procs
  getDecryptMOGGs <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-rb-decrypt-moggs" []) check
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
    [ (l10n "quick-convert-mode-in-place"  , QCInPlace  )
    , (l10n "quick-convert-mode-one-to-one", QCOneToOne )
    , (l10n "quick-convert-mode-make-packs", QCMakePacks)
    , (l10n "quick-convert-mode-make-songs", QCMakeSongs)
    , (l10n "quick-convert-mode-make-dolphin", QCDolphin  )
    ] withQCMode

  let quickTemplate :: Preferences -> QuickConvertFormat -> FilePath -> T.Text -> T.Text -> Maybe QuickDTA -> FilePath
      quickTemplate prefs fmt fin ext txt qdta = fixXbox $ validFileName NameRulePC $ dropTrailingPathSeparator $ T.unpack $ foldr ($) txt
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
        []        -> l10n "quick-convert-empty"
        qsong : _ -> l10nArgs "quick-convert-songs-incl"
          [ ("count", vNumber $ fromIntegral $ length qsongs)
          , ("title", vString $ artistTitle qsong)
          ]

  -- row3/row4: mode-specific stuff
  let (formatArea, trimClock 0 0 0 10 -> ps3OptionsArea) = chopLeft 150 row3
      (ps3EncryptArea, ps3FolderArea) = chopLeft 225 ps3OptionsArea
      makeCheckEncrypt = do
        btn <- withRect ps3EncryptArea \cx cy cw ch ->
          [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
        withUTF8 (l10n "quick-convert-encrypt-edat") \clbl ->
          [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
        when ?preferences.prefPS3Encrypt
          [C.block| void { $(Fl_Check_Button* btn)->value(1); } |]
        return $ checkValue btn
      makeFormatSelect :: IO () -> IO () -> IO () -> IO () -> IO (IO QuickConvertFormatOutput)
      makeFormatSelect ps3On ps3Off foldersOn foldersOff = do
        ref <- newIORef $ QCOutputTwoWay QCFormatCON
        makeModeDropdown formatArea
          [ (l10n "quick-convert-format-con"    , QCOutputTwoWay QCFormatCON )
          , (l10n "quick-convert-format-live"   , QCOutputTwoWay QCFormatLIVE)
          , (l10n "quick-convert-format-pkg"    , QCOutputTwoWay QCFormatPKG )
          , (l10n "quick-convert-format-folders", QCOutputLoosePS3           )
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
        group <- withRect row34 \gx gy gw gh ->
          [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
        inner
        [C.block| void { $(Fl_Group* group)->end(); } |]
        return group

  -- Transform in place
  inPlaceGoRef <- newIORef Nothing
  inPlace <- modeGroup do
    ps3Box <- withRect formatArea \bx by bw bh ->
      [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "quick-convert-for-ps3-files") \clbl ->
      [C.block| void { $(Fl_Box* ps3Box)->copy_label($(char* clbl)); } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ (l10n "quick-convert-folder-keep", Nothing)
      , (l10n "quick-convert-folder-combine", Just QCOneFolder)
      , (l10n "quick-convert-folder-each", Just QCSeparateFolders)
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
      btn <- withUTF8 (l10n "button-start") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]
      return btn
    writeIORef inPlaceGoRef $ Just btnGo

  -- One to one
  oneToOne <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      [C.block| void { $(Fl_Group* ps3Options)->activate(); } |]
      [C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |]
      ([C.block| void { $(Fl_Group* groupStartFile)->hide(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->show(); } |])
      ([C.block| void { $(Fl_Group* groupStartFile)->show(); } |] >> [C.block| void { $(Fl_Group* groupStartLoosePS3)->hide(); } |])
    ps3Options <- withRect ps3OptionsArea \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePresetDropdown ps3FolderArea
      [ (l10n "quick-convert-folder-keep-any", Nothing)
      , (l10n "quick-convert-folder-combine-per-file", Just QCOneFolder)
      , (l10n "quick-convert-folder-each", Just QCSeparateFolders)
      ]
    [C.block| void { $(Fl_Group* ps3Options)->end(); } |]
    groupStartFile <- withRect row4 \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    void $ makeTemplateRunner'
      sink
      row4
      (l10n "button-start")
      "%input_dir%/%input_base%_convert%ext%"
      \template -> sink $ EventOnyx $ stackIO getFormat >>= \case
        QCOutputLoosePS3   -> return () -- shouldn't happen
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
                  fout = quickTemplate newPreferences fmt fin ext template Nothing
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
      btn <- withUTF8 (l10n "button-start") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
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
      [C.block| void { $(Fl_Group* ps3Options)->activate(); } |]
      [C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |]
      (return ())
      (return ())
    ps3Options <- withRect ps3OptionsArea \gx gy gw gh ->
      [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
    getEncrypt <- makeCheckEncrypt
    getFolderSetting <- makePS3PackPresetDropdown sink ps3FolderArea
    [C.block| void { $(Fl_Group* ps3Options)->end(); } |]
    let (trimClock 0 10 0 150 -> maxSizeArea, goArea) = chopLeft 300 row4
    maxSizeInput <- withRect maxSizeArea \ix iy iw ih -> do
      inp <- withUTF8 (l10n "quick-convert-max-pack-size") \cs -> [C.exp| Fl_Input* { new Fl_Input($(int ix), $(int iy), $(int iw), $(int ih), fl_strdup($(char* cs))) } |]
      [C.block| void {
        $(Fl_Input* inp)->labelsize(13);
        $(Fl_Input* inp)->align(FL_ALIGN_LEFT);
        $(Fl_Input* inp)->value("4000");
      } |]
      return inp
    setCallback maxSizeInput updatePackGo
    [C.block| void { $(Fl_Input* maxSizeInput)->when(FL_WHEN_CHANGED); } |]
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
                    newPreferences <- readPreferences
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
                    saveQuickSongsPS3Folder qsongsReencode ps3Settings dout
                  in (T.unpack $ l10nArgs "quick-convert-pack-number" [("number", vNumber $ fromIntegral (i :: Int))], task)
            QCOutputTwoWay fmt -> stackIO $ withNativeFileChooser
              [C.pure| int { Fl_Native_File_Chooser::BROWSE_SAVE_FILE } |]
              \picker -> do
                let titleStr = case packs of
                      [_] -> l10n "quick-convert-save-pack-file"
                      _   -> l10n "quick-convert-save-pack-template"
                withUTF8 (titleStr :: T.Text) \ctitle ->
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
                          QCFormatPKG  -> saveQuickSongsPKG qsongsReencode ps3Settings fout
                        return [fout]
                      in (T.unpack $ l10nArgs "quick-convert-pack-number" [("number", vNumber $ fromIntegral (i :: Int))], task)
    btnGo <- withRect goArea \bx by bw bh -> do
      btn <- withUTF8 (l10n "pack-save-as") \s ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* s))) } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]
      return btn
    writeIORef packGoRef $ Just btnGo

  -- Make songs
  makeSongs <- modeGroup $ mdo
    getFormat <- makeFormatSelect
      [C.block| void { $(Fl_Group* ps3Options)->activate(); } |]
      [C.block| void { $(Fl_Group* ps3Options)->deactivate(); } |]
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
      (l10n "button-select-folder")
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
                fout = dout </> quickTemplate newPreferences fmt qinput.quickInputPath ext template (Just qsong.quickSongDTA)
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
      btn <- withUTF8 (l10n "button-start") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbLoosePS3));
      } |]
    [C.block| void { $(Fl_Group* groupStartLoosePS3)->end(); } |]

  -- Wii (Dolphin) pack
  makeDolphin <- modeGroup $ mdo
    boxPrev <- withRect row3 \cx cy cw ch ->
      withUTF8 (l10n "quick-convert-dolphin-preview-audio") \cs ->
        [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), fl_strdup($(char* cs))) } |]
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
        in [(T.unpack $ l10n "quick-convert-task-make-dolphin", task)]
    withRect row4 \bx by bw bh -> do
      btn <- withUTF8 (l10n "button-select-folder") \s ->
        [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* s))) } |]
      [C.block| void {
        $(Fl_Button* btn)->color($(Fl_Color taskColor));
        $(Fl_Button* btn)->callback($(Fl_Callback cbGo));
      } |]

  [C.block| void { $(Fl_Group* tab)->resizable($(Fl_Group* filesGroup)); } |]

-- | Widgets for the Rock Revolution renumber tab, laid out in
-- fluid/quick_convert.fl.
data RenumberRRTabWidgets = RenumberRRTabWidgets
  { rrFiles :: Ptr FileLoadPanel
  , rrStart :: Ptr Fl_Button
  }

pageRenumberRR
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ())
  -> RenumberRRTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
pageRenumberRR sink widgets startTasks = do
  loadedFiles <- newMVar []
  let updateFiles = modifyMVar_ loadedFiles
  clearFiles <- fileLoadPanel widgets.rrFiles sink (l10nFull "file-type-rr-song" []) updateFiles [] searchRockRevolution
    \(path, songs) -> let
      entry = T.pack path
      subline = l10nArgs "song-count"
        [("count", vNumber $ fromIntegral $ length songs)]
      in (entry, [subline])

  setCallback widgets.rrStart do
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

data QuickFoFTask
  = QFoFInPlace
  | QFoFKeepOriginal
  | QFoFDeleteOriginal
  deriving (Eq)

pageQuickConvertCH
  :: (?preferences :: Preferences, ?loc :: L)
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
  (filesGroup, clearFiles) <- fileLoadWindow' filesRect sink (l10nFull "file-type-ch-song" []) updateFiles [] searchQuickFoF
    \qfof -> let
      entry = T.pack qfof.location
      subline = case qfof.format of
        FoFFormat FoFFolder -> l10n "quick-convert-fof-folder"
        FoFFormat FoFSNG    -> l10n "quick-convert-fof-sng"
        FoFEncore           -> l10n "quick-convert-fof-encore"
      in (entry, [subline])

  -- right side: processors to transform midis and other files
  packProcessors <- withRect (trimClock 5 5 5 0 processorsRect) \px py pw ph ->
    [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
  let processorBox = Rectangle (Position (X 0) (Y 0)) (Size (Width 500) (Height 45))

  (checkMIDI, processorMIDI) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-ch-chart-to-midi" []) check
    setCallback check do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkPSTapOpen)->value(0); } |]
    return (check, do
      checked <- checkValue check
      return if checked then convertMIDI else return
      )

  (checkPSTapOpen, processorPSTapOpen) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-ch-ps-tap-open" []) check
    setCallback check do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkMIDI)->value(1); } |]
    return (check, do
      checked <- checkValue check
      return if checked then psTapOpenFormats else return
      )

  processorLimitEasyMediumFrets <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-limit-easy-medium-frets" []) check
    setCallback check do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkMIDI)->value(1); } |]
    return do
      checked <- checkValue check
      return if checked then QCH.limitEasyMediumFrets else return

  (checkOgg, processorOgg) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-ch-audio-ogg" []) check
    setCallback check do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkOpus)->value(0); } |]
    return (check, do
      checked <- checkValue check
      return if checked then stackIO . convertOgg else return
      )
  (checkOpus, processorOpus) <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-ch-audio-opus" []) check
    setCallback check do
      checked <- checkValue check
      when checked [C.block| void { $(Fl_Check_Button* checkOgg)->value(0); } |]
    return (check, do
      checked <- checkValue check
      return if checked then stackIO . convertOpus else return
      )

  processorRealToElite <- do
    check <- withRect processorBox \cx cy cw ch ->
      [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
    setLabelTooltip (l10nFull "quick-convert-ch-real-drums-to-elite" []) check
    return do
      checked <- checkValue check
      return if checked then fillEliteFromPSReal else return

  let getTransform = do
        procs <- sequence [processorMIDI, processorPSTapOpen, processorLimitEasyMediumFrets, processorOgg, processorOpus, processorRealToElite]
        return $ foldr (>=>) return procs
  [C.block| void { $(Fl_Pack* packProcessors)->end(); } |]

  -- row2: select mode
  let withQCMode mode = sink $ EventIO do
        if mode == Nothing        then [C.block| void { $(Fl_Group* inPlace)->show(); } |]   else [C.block| void { $(Fl_Group* inPlace)->hide(); } |]
        if mode == Just FoFFolder then [C.block| void { $(Fl_Group* toFolders)->show(); } |] else [C.block| void { $(Fl_Group* toFolders)->hide(); } |]
        if mode == Just FoFSNG    then [C.block| void { $(Fl_Group* toSNGs)->show(); } |]    else [C.block| void { $(Fl_Group* toSNGs)->hide(); } |]
  makeModeDropdown row2
    [ (l10n "quick-convert-mode-in-place"  , Nothing       )
    , (l10n "quick-convert-mode-to-folders", Just FoFFolder)
    , (l10n "quick-convert-mode-to-sng"    , Just FoFSNG   )
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
        group <- withRect row34 \gx gy gw gh ->
          [C.exp| Fl_Group* { new Fl_Group($(int gx), $(int gy), $(int gw), $(int gh)) } |]
        inner
        [C.block| void { $(Fl_Group* group)->end(); } |]
        return group

  -- Transform in place (each input is replaced with a file, package type preserved)
  --   3rd row: go button
  inPlace <- modeGroup do
    cbGo <- makeCallback \_ _ -> do
      qfofs <- readMVar loadedFiles
      transform <- getTransform
      let warningText = T.unlines $ concat
            [ [l10n "quick-convert-ch-warning-base"]
            , do
              guard $ any (\qfof -> qfof.format == FoFEncore) qfofs
              [ "", l10n "quick-convert-ch-warning-encore" ]
            ]
      withUTF8 warningText \cmsg -> withUTF8 (l10n "task-cancel") \cCancel -> withUTF8 (l10n "button-start") \cStart -> do
        result <- [C.exp| int { fl_choice("%s", $(char* cCancel), $(char* cStart), nullptr, $(char* cmsg)) } |]
        when (result == 1) do
          sink $ EventOnyx do
            -- TODO this is a hack to avoid stale .sng references after editing them.
            -- should come up with a better solution!
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
      btn <- withUTF8 (l10n "button-start") \cs -> [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* cs))) } |]
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
        warningText = T.unlines $ concat
          [ [l10n "quick-convert-ch-ops-header"]
          , guard (numInPlace /= 0) >> [l10nArgs "quick-convert-ch-op-overwrite" [("songs", vNumber $ fromIntegral numInPlace)]]
          , guard (numKeepOriginal /= 0) >> [l10nArgs "quick-convert-ch-op-convert" [("songs", vNumber $ fromIntegral numKeepOriginal)]]
          , guard (numDeleteOriginal /= 0) >> [l10nArgs "quick-convert-ch-op-convert-delete" [("songs", vNumber $ fromIntegral numDeleteOriginal)]]
          , do
            guard $ numDeleteEncore /= 0
            ["", l10n "quick-convert-ch-ops-encore"]
          ]
        in withUTF8 warningText \cmsg -> withUTF8 (l10n "task-cancel") \cCancel -> withUTF8 (l10n "button-start") \cStart -> do
          result <- [C.exp| int { fl_choice("%s", $(char* cCancel), $(char* cStart), nullptr, $(char* cmsg)) } |]
          when (result == 1) f

  -- new folders, template "%input_dir%/%input_base%", warn if any name conflicts
  toFolders <- modeGroup do
    checkDeleteOriginals <- withRect row3 \cx cy cw ch ->
      withUTF8 (l10n "quick-convert-delete-originals") \cs -> [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), fl_strdup($(char* cs))) } |]
    void $ makeTemplateRunner'
      sink
      row4
      (l10n "button-start")
      "%input_dir%/%input_base%"
      \template -> do
        deleteOriginals <- checkValue checkDeleteOriginals
        qfofs <- readMVar loadedFiles
        transform <- getTransform
        tasks <- computeTasks qfofs "" template deleteOriginals
        promptTasks tasks $ sink $ EventOnyx do
          stackIO clearFiles -- hack as mentioned above
          startTasks $ tasks <&> \(orig, _, taskType, fout) -> let
            task = do
              qfof <- transform orig
              stackIO $ saveQuickFoFFolder fout qfof
              case taskType of
                QFoFDeleteOriginal -> stackIO $ Dir.removePathForcibly orig.location
                _                  -> return ()
              return [fout]
            in (fout, task)

  -- new sng, template "%input_dir%/%input_base%.sng", warn if any name conflicts
  toSNGs <- modeGroup do
    checkDeleteOriginals <- withRect row3 \cx cy cw ch ->
      withUTF8 (l10n "quick-convert-delete-originals") \cs -> [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch), fl_strdup($(char* cs))) } |]
    void $ makeTemplateRunner'
      sink
      row4
      (l10n "button-start")
      "%input_dir%/%input_base%.sng"
      \template -> do
        deleteOriginals <- checkValue checkDeleteOriginals
        qfofs <- readMVar loadedFiles
        transform <- getTransform
        tasks <- computeTasks qfofs "sng" template deleteOriginals
        promptTasks tasks $ sink $ EventOnyx do
          stackIO clearFiles -- hack as mentioned above
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

-- | Widgets for the MOGG/VGS/FSB tab, laid out in fluid/misc.fl.
data MoggTabWidgets = MoggTabWidgets
  { mgFiles :: Ptr FileLoadPanel
  , mgMogg  :: Ptr Fl_Button
  , mgVgs   :: Ptr Fl_Button
  , mgFsb3  :: Ptr Fl_Button
  , mgFsb4  :: Ptr Fl_Button
  }

miscPageMOGG
  :: (?loc :: L)
  => (Event -> IO ())
  -> MoggTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMOGG sink tab startTasks = do
  loadedAudio <- newMVar []
  let btnMogg = tab.mgMogg
      btnVgs  = tab.mgVgs
      btnFsb3 = tab.mgFsb3
      btnFsb4 = tab.mgFsb4
      isSingleOgg = \case
        [aud] | aud.format == "Ogg Vorbis" -> Just aud
        _                                  -> Nothing
      modifyAudio :: ([AudioSpec] -> IO [AudioSpec]) -> IO ()
      modifyAudio f = do
        newAudio <- modifyMVar loadedAudio \auds -> (\x -> (x, x)) <$> f auds
        let chans = sum $ map (.channels) newAudio
        sink $ EventIO do
          let moggLabel = T.unpack $ case isSingleOgg newAudio of
                Just _  -> l10nArgs "mogg-button-ogg"     [("channels", vNumber $ fromIntegral chans)]
                Nothing -> l10nArgs "mogg-button-combine" [("channels", vNumber $ fromIntegral chans)]
              vgsLabel = T.unpack $ l10nArgs "mogg-button-vgs" [("channels", vNumber $ fromIntegral chans)]
              fsb3Label = T.unpack $ l10nArgs "mogg-button-fsb3"
                [ ("streams", vNumber $ fromIntegral $ length newAudio)
                , ("channels", vNumber $ fromIntegral chans)
                ]
              fsb4Label = T.unpack $ l10nArgs "mogg-button-fsb4" [("channels", vNumber $ fromIntegral chans)]
          withUTF8 moggLabel \cl -> [C.block| void { $(Fl_Button* btnMogg)->copy_label($(char* cl)); } |]
          withUTF8 vgsLabel  \cl -> [C.block| void { $(Fl_Button* btnVgs)->copy_label($(char* cl)); } |]
          withUTF8 fsb3Label \cl -> [C.block| void { $(Fl_Button* btnFsb3)->copy_label($(char* cl)); } |]
          withUTF8 fsb4Label \cl -> [C.block| void { $(Fl_Button* btnFsb4)->copy_label($(char* cl)); } |]
          if chans == 0
            then forM_ [btnMogg, btnVgs, btnFsb3, btnFsb4] \b ->
              [C.block| void { $(Fl_Button* b)->deactivate(); } |]
            else forM_ [btnMogg, btnVgs, btnFsb3, btnFsb4] \b ->
              [C.block| void { $(Fl_Button* b)->activate(); } |]
  _ <- fileLoadPanel tab.mgFiles sink (l10nFull "file-type-audio" []) modifyAudio []
    (\f -> liftIO $ ([],) . toList <$> getAudioSpec f)
    \info -> let
      entry = T.pack info.path
      sublines =
        [ l10nArgs "audio-format-channels"
          [ ("format", vString info.format)
          , ("channels", vNumber $ fromIntegral info.channels)
          ]
        , let
          time = realToFrac info.frames / realToFrac info.rate :: Double
          mins = floor $ time / 60 :: Int
          secs = realToFrac $ time - realToFrac mins * 60 :: Milli
          zero = if secs < 10 then "0" else ""
          stamp = T.pack $ show mins <> ":" <> zero <> show secs
          -- could format timestamp with `time` package now
          in l10nArgs "audio-duration"
            [ ("duration", vString stamp)
            , ("frames", vNumber $ fromIntegral info.frames)
            , ("rate", vNumber $ fromIntegral info.rate)
            ]
        ]
      in (entry, sublines)

  let makeSaveAudioButton
        :: Ptr Fl_Button -> String -> T.Text
        -> ([AudioSpec] -> FilePath -> Onyx ()) -> IO ()
      makeSaveAudioButton btn filterStr defaultExt task' = do
        setCallback btn $ sink $ EventIO do
          audio <- readMVar loadedAudio
          let title = l10nArgs "file-picker-save-multitrack-audio"
                [("extension", vString defaultExt)]
              preset = case audio of
                [aud] -> Just $ T.pack $ takeFileName aud.path -<.> T.unpack defaultExt
                _     -> Just $ "out." <> defaultExt
          saveFileChooser title (T.pack filterStr) preset \f -> let
              ext = map toLower $ takeExtension f
              f' = if ext == "." ++ T.unpack defaultExt then f else f <.> T.unpack defaultExt
              in sink $ EventOnyx $ task' audio f'

  makeSaveAudioButton btnMogg "*.mogg" "mogg" \audio f' ->
    startTasks case isSingleOgg audio of
      Just aud -> [(T.unpack $ l10n "mogg-task-ogg", oggToMoggFiles aud.path f' >> return [f'])]
      Nothing  -> let
        task = tempDir "makemogg" \tmp -> do
          let ogg = tmp </> "temp.ogg"
          case map (.rate) audio of
            []             -> return ()
            rates@(r : rs) -> unless (all (== r) rs) do
              fatal $ "All files must have the same sample rate, but found: " <> show (nubOrd rates)
          ne <- maybe (fatal "No input files") return $ NE.nonEmpty audio
          src <- buildSource' $ Merge $ fmap (Input . (.path)) ne
          runAudio src ogg
          oggToMoggFiles ogg f'
          return [f']
        in [(T.unpack $ l10n "mogg-task-mogg", task)]

  makeSaveAudioButton btnVgs "*.vgs" "vgs" \audio f' ->
    startTasks let
      task = do
        srcs <- mapM (buildSource' . Input . (.path)) audio
        stackIO $ runResourceT $ writeVGSMultiRate f' $ map (mapSamples integralSample) srcs
        return [f']
      in [(T.unpack $ l10n "mogg-task-vgs", task)]

  makeSaveAudioButton btnFsb3 "*.fsb" "fsb" \audio f' ->
    startTasks let
      task = tempDir "makefsb3" \tmp -> do
        wavs <- forM (zip [0..] audio) \(i, aud) -> do
          src <- buildSource' $ Input aud.path
          let wav = tmp </> show (i :: Int) <.> "wav"
          runAudio src wav
          return wav
        makeXMAFSB3 [ (B8.pack $ takeFileName wav, wav) | wav <- wavs ] f'
        return [f']
      in [(T.unpack $ l10n "mogg-task-fsb3", task)]

  makeSaveAudioButton btnFsb4 "*.fsb" "fsb" \audio f' ->
    startTasks let
      task = tempDir "makefsb4" \tmp -> do
        let wav = tmp </> "audio.wav"
        audio' <- maybe (fatal "Panic! No audio files") return $ NE.nonEmpty audio
        src <- buildSource' $ Merge $ fmap (Input . (.path)) audio'
        runAudio src wav
        fsb <- makeXMAPieces (fileReadable wav) >>= ghBandXMAtoFSB4
        stackIO $ BL.writeFile f' $ emitFSB fsb
        return [f']
      in [(T.unpack $ l10n "mogg-task-fsb4", task)]


-- | Widgets for the MIDI stuff tab, laid out in fluid/misc.fl.
data MidiTabWidgets = MidiTabWidgets
  { mpMidiFile    :: Ptr FilePickerRow
  , mpReduce      :: Ptr Fl_Button
  , mpProKeys     :: Ptr Fl_Button
  , mpReaper      :: Ptr Fl_Button
  , mpChartToMidi :: Ptr Fl_Button
  , mpBatch       :: Ptr Fl_Button
  }

miscPageMIDI
  :: (?loc :: L)
  => (Event -> IO ())
  -> MidiTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageMIDI sink tab startTasks = do
  let pathInput = filePickerInput tab.mpMidiFile
  pickedFile <- filePickerRow tab.mpMidiFile (l10n "label-midi-or-chart")
    \useResult -> sink $ EventIO $ withNativeFileChooser
      [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
      \picker -> do
        withUTF8 (l10n "midi-load-dialog") \cs ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* cs)); } |]
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->filter("*.{mid,midi,chart}");
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          peekUTF8 cstr >>= useResult
          -- refreshes which of the buttons below are enabled
          [C.block| void { $(Fl_Input* pathInput)->do_callback(); } |]

  midiButtons <- sequence

    [ do
      let btn = tab.mpReduce
      setCallback btn $ sink $ EventIO do
        input <- pickedFile
        saveFileChooser (l10n "file-picker-save-reduced-midi") "*.mid" (Just $ T.pack $ input -<.> "reduced.mid") \f -> let
          ext = map toLower $ takeExtension f
          f' = if elem ext [".mid", ".midi"] then f else f <.> "mid"
          in sink $ EventOnyx let
            task = do
              fretLimit <- loadFretLimit
              simpleReduce fretLimit input f'
              return [f']
            in startTasks [(T.unpack $ l10nArgs "midi-task-reduce" [("path", vString $ T.pack input)], task)]
      return btn

    , do
      let btn = tab.mpProKeys
      setCallback btn $ sink $ EventIO do
        input <- pickedFile
        sink $ EventOnyx let
          task = do
            mid <- loadMIDIOrChart input
            lg $ T.unpack $ closeShiftsFile mid
            return []
          in startTasks [(T.unpack $ l10nArgs "midi-task-prokeys" [("path", vString $ T.pack input)], task)]
      return btn

    , do
      let btn = tab.mpReaper
      setCallback btn $ sink $ EventIO do
        input <- pickedFile
        saveFileChooser (l10n "file-picker-save-reaper") "*.RPP" (Just $ T.pack $ input -<.> "RPP") \f -> let
          ext = map toUpper $ takeExtension f
          f' = if ext == ".RPP" then f else f <.> "RPP"
          in sink $ EventOnyx let
            task = do
              makeReaper (TuningInfo [] 0) input input [] f'
              return [f']
            in startTasks [(T.unpack $ l10nArgs "midi-task-reaper" [("path", vString $ T.pack input)], task)]
      return btn

    , do
      let btn1 = tab.mpChartToMidi
      setCallback btn1 $ sink $ EventIO do
        input <- pickedFile
        saveFileChooser (l10n "file-picker-save-midi") "*.mid" (Just $ T.pack $ input -<.> ".mid") \f -> let
          ext = map toLower $ takeExtension f
          f' = if elem ext [".mid", ".midi"] then f else f <.> "mid"
          in sink $ EventOnyx let
            task = do
              mid <- loadRawMIDIOrChart input
              stackIO $ Save.toFile f' $ TE.encodeUtf8 <$> mid
              return [f']
            in startTasks [(T.unpack $ l10nArgs "midi-task-chart-to-midi" [("path", vString $ T.pack input)], task)]

      -- note, this button doesn't disable when path box is empty since it doesn't use it
      let btn2 = tab.mpBatch
      setCallback btn2 $ sink $ EventIO $ askFolder Nothing \dir ->
        sink $ EventOnyx let
          task = do
            results <- recursiveChartToMidi dir
            when (null results) $ lg "No .chart files found."
            return results
          in startTasks [(T.unpack $ l10nArgs "midi-task-chart-batch" [("path", vString $ T.pack dir)], task)]

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
  setCallback pathInput $ sink $ EventIO updatePathInput
  [C.block| void { $(Fl_Input* pathInput)->when(FL_WHEN_CHANGED); } |]

-- | Widgets for the RB3 cache tab, laid out in fluid/misc.fl.
data RB3CacheTabWidgets = RB3CacheTabWidgets
  { rcConFolder :: Ptr FilePickerRow
  , rcCacheFile :: Ptr FilePickerRow
  , rcApply     :: Ptr Fl_Button
  }

miscPageHardcodeSongCache
  :: (?loc :: L)
  => (Event -> IO ())
  -> RB3CacheTabWidgets
  -> ([(String, Onyx [FilePath])] -> Onyx ())
  -> IO ()
miscPageHardcodeSongCache sink tab startTasks = do
  pickedFolder <- filePickerRow tab.rcConFolder (l10n "label-con-folder")
    \useResult -> sink $ EventIO $ askFolder Nothing useResult
  pickedCache <- filePickerRow tab.rcCacheFile (l10n "label-song-cache")
    \useResult -> sink $ EventIO $ withNativeFileChooser
      [C.pure| int { Fl_Native_File_Chooser::BROWSE_FILE } |]
      \picker -> do
        withUTF8 (l10n "cache-load-dialog") \cs ->
          [C.block| void { $(Fl_Native_File_Chooser* picker)->title($(char* cs)); } |]
        [C.block| void {
          $(Fl_Native_File_Chooser* picker)->filter("songcache");
        } |]
        result <- [C.exp| int { $(Fl_Native_File_Chooser* picker)->show() } |]
        when (result == 0) do
          cstr <- [C.exp| const char* { $(Fl_Native_File_Chooser* picker)->filename() } |]
          peekUTF8 cstr >>= useResult
  let btn = tab.rcApply
  setCallback btn $ sink $ EventIO do
    dir <- pickedFolder
    cache <- pickedCache
    let cache' = case cache of
          "" -> dir </> "songcache"
          _  -> cache
    sink $ EventOnyx let
      task = do
        hardcodeSongCacheIDs cache' dir
        return []
      in startTasks [(T.unpack $ l10nArgs "cache-task" [("path", vString $ T.pack dir)], task)]

launchQuickConvert' :: (?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchQuickConvert' sink makeMenuBar = sink $ EventOnyx do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchQuickConvert sink makeMenuBar

launchQuickConvert :: (?preferences :: Preferences, ?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchQuickConvert sink makeMenuBar = do
  -- Window, tabs and tab groups come from fluid/quick_convert.fl; the pages
  -- inside each tab are still built here. Those page builders apply the
  -- horizontal stretch themselves via withRect, so adaptWindow has to run
  -- before they do, and they get handed the tab's pre-stretch rectangle.
  ui <- [C.exp| void* { new QuickConvertWindow() } |]
  let window        = [C.pure| Fl_Window* { ((QuickConvertWindow*)$(void* ui))->window } |]
      tabs          = [C.pure| Fl_Tabs*   { ((QuickConvertWindow*)$(void* ui))->tabs } |]
      tabRB         = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabRB } |]
      tabCH         = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabCH } |]
      tabRenumberRR = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabRenumberRR } |]
      renumberRRTab = RenumberRRTabWidgets
        { rrFiles = [C.pure| FileLoadPanel* { ((QuickConvertWindow*)$(void* ui))->rrFiles } |]
        , rrStart = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->rrStart } |]
        }
      tabPacks      = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabPacks } |]
      packTab = PackTabWidgets
        { pkFiles = [C.pure| FileLoadPanel* { ((QuickConvertWindow*)$(void* ui))->pkFiles } |]
        , pkGame  = [C.pure| Fl_Box*        { ((QuickConvertWindow*)$(void* ui))->pkGame } |]
        , pkName  = [C.pure| Fl_Input*      { ((QuickConvertWindow*)$(void* ui))->pkName } |]
        , pkDesc  = [C.pure| Fl_Input*      { ((QuickConvertWindow*)$(void* ui))->pkDesc } |]
        , pkCON   = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->pkCON } |]
        , pkLIVE  = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->pkLIVE } |]
        , pkRaw   = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->pkRaw } |]
        }
      tabCONtoPKG   = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabCONtoPKG } |]
      conToPKGTab = CONtoPKGTabWidgets
        { cpFiles = [C.pure| FileLoadPanel* { ((QuickConvertWindow*)$(void* ui))->cpFiles } |]
        , cpRB2   = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->cpRB2 } |]
        , cpRB3   = [C.pure| Fl_Button*     { ((QuickConvertWindow*)$(void* ui))->cpRB3 } |]
        }
      tabTask       = [C.pure| Fl_Group*  { ((QuickConvertWindow*)$(void* ui))->tabTask } |]
      -- every tab that must be locked out while a task is running
      functionTabs = [tabRB, tabCH, tabRenumberRR, tabPacks, tabCONtoPKG]

  -- read the layout's own coordinates before adaptWindow rewrites them
  windowWidth <- Width <$> [C.exp| int { $(Fl_Window* window)->w() } |]
  Rectangle (Position (X tabX) (Y tabY)) tabSize <- layoutRect tabTask

  menuHeight <- adaptWindow window
  when (menuHeight /= 0) do
    inGroup (windowGroup window) $ void $ makeMenuBar windowWidth True
  lockMinimumSize window
  let tabRect = Rectangle (Position (X tabX) (Y $ tabY + menuHeight)) tabSize

  -- task tab first, since the function pages need startTasks
  (startTasks, cancelTasks) <- inGroup tabTask do
    let cbStart = do
          [C.block| void { $(Fl_Group* tabTask)->activate(); } |]
          forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tabTask)); } |]
          updateTabsColor tabs
        cbEnd = forM_ functionTabs \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage tabRect tabTask sink cbStart cbEnd
  inGroup tabRB $ pageQuickConvertRB sink tabRect tabRB startTasks
  inGroup tabCH $ pageQuickConvertCH sink tabRect tabCH startTasks
  pageRenumberRR   sink renumberRRTab startTasks
  miscPagePacks    sink packTab       startTasks
  miscPageCONtoPKG sink conToPKGTab   startTasks

  updateTabsColor tabs
  setCallback tabs $ updateTabsColor tabs
  setCallback window $ windowCloser cancelTasks window
  [C.block| void { $(Fl_Window* window)->show(); } |]

launchMisc :: (?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchMisc sink makeMenuBar = do
  -- Window, tabs and tab groups come from fluid/misc.fl; the pages inside each
  -- tab are still built here. Those page builders apply the horizontal stretch
  -- themselves via withRect, so adaptWindow has to run before they do, and they
  -- get handed the tab's pre-stretch rectangle.
  ui <- [C.exp| void* { new MiscWindow() } |]
  let window      = [C.pure| Fl_Window* { ((MiscWindow*)$(void* ui))->window } |]
      tabs        = [C.pure| Fl_Tabs*   { ((MiscWindow*)$(void* ui))->tabs } |]
      tabTask     = [C.pure| Fl_Group*  { ((MiscWindow*)$(void* ui))->tabTask } |]
      tabMilo = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabMilo } |]
      miloTab = MiloTabWidgets
        { mtExtract = [C.pure| Fl_Button* { ((MiscWindow*)$(void* ui))->miloExtract } |]
        , mtRepack  = [C.pure| Fl_Button* { ((MiscWindow*)$(void* ui))->miloRepack } |]
        }
      tabMIDI = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabMIDI } |]
      midiTab = MidiTabWidgets
        { mpMidiFile    = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->midiFile } |]
        , mpReduce      = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->midiReduce } |]
        , mpProKeys     = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->midiProKeys } |]
        , mpReaper      = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->midiReaper } |]
        , mpChartToMidi = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->midiChartToMidi } |]
        , mpBatch       = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->midiBatch } |]
        }
      tabMOGG = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabMOGG } |]
      moggTab = MoggTabWidgets
        { mgFiles = [C.pure| FileLoadPanel* { ((MiscWindow*)$(void* ui))->moggFiles } |]
        , mgMogg  = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->moggCombine } |]
        , mgVgs   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->vgsCombine } |]
        , mgFsb3  = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->fsb3Make } |]
        , mgFsb4  = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->fsb4Make } |]
        }
      tabLipsync = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabLipsync } |]
      lipsyncTab = LipsyncTabWidgets
        { lsMidiFile   = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->lsMidiFile } |]
        , lsLanguages  =
          [ [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->lsLang0 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->lsLang1 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->lsLang2 } |]
          ]
        , lsTransition = [C.pure| Fl_Counter*    { ((MiscWindow*)$(void* ui))->lsTransition } |]
        , lsMakeVoc    = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsMakeVoc } |]
        , lsMakeMilo   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsMakeMilo } |]
        , lsUpdateMilo = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsUpdateMilo } |]
        , lsRawLipsync = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsRawLipsync } |]
        , lsTracksRB3  = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsTracksRB3 } |]
        , lsTracksTBRB = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->lsTracksTBRB } |]
        }
      tabDryVox = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabDryVox } |]
      dryVoxTab = DryVoxTabWidgets
        { dvMidiFile    = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->dvMidiFile } |]
        , dvVocalTracks =
          [ [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->dvVox0 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->dvVox1 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->dvVox2 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->dvVox3 } |]
          , [C.pure| Fl_Round_Button* { ((MiscWindow*)$(void* ui))->dvVox4 } |]
          ]
        , dvSine        = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->dvSine } |]
        , dvAudioFile   = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->dvAudioFile } |]
        , dvClipped     = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->dvClipped } |]
        }
      tabRB3Cache = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->tabRB3Cache } |]
      rb3CacheTab = RB3CacheTabWidgets
        { rcConFolder = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->rb3ConFolder } |]
        , rcCacheFile = [C.pure| FilePickerRow* { ((MiscWindow*)$(void* ui))->rb3CacheFile } |]
        , rcApply     = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->rb3Apply } |]
        }
      tabGH3Cache = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->gh3Cache } |]
      gh3CacheTab = GH3CacheTabWidgets
        { gcFiles       = [C.pure| FileLoadPanel* { ((MiscWindow*)$(void* ui))->gh3Files } |]
        , gcButton360   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->gh3Cache360 } |]
        , gcButtonPS3   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->gh3CachePS3 } |]
        , gcButtonRPCS3 = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->gh3CacheRPCS3 } |]
        }
      tabWoRCache = [C.pure| Fl_Group* { ((MiscWindow*)$(void* ui))->worCache } |]
      worCacheTab = WoRCacheTabWidgets
        { wcFiles       = [C.pure| FileLoadPanel* { ((MiscWindow*)$(void* ui))->worFiles } |]
        , wcButton360   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->worCache360 } |]
        , wcButtonPS3   = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->worCachePS3 } |]
        , wcButtonRPCS3 = [C.pure| Fl_Button*     { ((MiscWindow*)$(void* ui))->worCacheRPCS3 } |]
        }
      -- every tab that must be locked out while a task is running
      allFunctionTabs =
        [ tabMIDI, tabMOGG, tabLipsync, tabDryVox, tabMilo
        , tabRB3Cache, tabGH3Cache, tabWoRCache ]

  -- read the layout's own coordinates before adaptWindow rewrites them
  windowWidth <- Width <$> [C.exp| int { $(Fl_Window* window)->w() } |]
  Rectangle (Position (X tabX) (Y tabY)) tabSize <- layoutRect tabTask

  menuHeight <- adaptWindow window
  when (menuHeight /= 0) do
    inGroup (windowGroup window) $ void $ makeMenuBar windowWidth True
  lockMinimumSize window
  let tabRect = Rectangle (Position (X tabX) (Y $ tabY + menuHeight)) tabSize

  -- task tab first, since the function pages need startTasks
  (startTasks, cancelTasks) <- inGroup tabTask do
    let cbStart = do
          [C.block| void { $(Fl_Group* tabTask)->activate(); } |]
          forM_ allFunctionTabs \t -> [C.block| void { $(Fl_Group* t)->deactivate(); } |]
          [C.block| void { ((Fl_Tabs*)$(Fl_Tabs* tabs))->value($(Fl_Group* tabTask)); } |]
          updateTabsColor tabs
        cbEnd = forM_ allFunctionTabs \t -> [C.block| void { $(Fl_Group* t)->activate(); } |]
    taskOutputPage tabRect tabTask sink cbStart cbEnd
  miscPageMIDI              sink midiTab     startTasks
  miscPageMOGG              sink moggTab     startTasks
  miscPageLipsync           sink lipsyncTab  startTasks
  miscPageDryVox            sink dryVoxTab   startTasks
  miscPageMilo              sink miloTab     startTasks
  miscPageHardcodeSongCache sink rb3CacheTab startTasks
  miscPageGH3SongCache      sink gh3CacheTab startTasks
  miscPageWoRSongCache      sink worCacheTab startTasks

  updateTabsColor tabs
  setCallback tabs $ updateTabsColor tabs
  setCallback window $ windowCloser cancelTasks window
  [C.block| void { $(Fl_Window* window)->show(); } |]

-- _watchSong, _launchTimeServer

data GLStatus = GLPreload | GLLoaded RGGraphics.GLStuff | GLFailed

previewGroup
  :: (?preferences :: Preferences, ?loc :: L)
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
    withUTF8 (l10n "preview-view-tracks") \cs -> [C.exp| Fl_Menu_Button* { new_sticky_menu_button($(int mx), $(int my), $(int mw), $(int mh), fl_strdup($(char* cs))) } |]
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
          when doRedraw $ sink $ EventIO [C.block| void { Fl::redraw(); } |]

  initSong <- getSong
  updateParts False $ map (map fst) $ previewTracks initSong

  -- Audio selection menu
  audioMenu <- withRect audioSelectArea \mx my mw mh ->
    withUTF8 (l10n "preview-audio") \cs -> [C.exp| Fl_Menu_Button* { new_sticky_menu_button($(int mx), $(int my), $(int mw), $(int mh), fl_strdup($(char* cs))) } |]
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
    withUTF8 t \ct ->
      [C.block| void {
        $(Fl_Menu_Button* audioMenu)->add($(char* ct), 0, $(Fl_Callback cbAudio), 0, $(int flags));
      } |]

  -- Background selection menu
  bgMenu <- withRect bgSelectArea \mx my mw mh ->
    withUTF8 (l10n "preview-background") \cs -> [C.exp| Fl_Menu_Button* { new Fl_Menu_Button($(int mx), $(int my), $(int mw), $(int mh), fl_strdup($(char* cs))) } |]
  let bgs = [(t, Just bg) | (t, bg) <- previewBG initSong]
      emptyBG = (l10n "preview-bg-none", Nothing)
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
  -- TODO add an option to use Fl::use_high_res_GL(1)
  -- This appears to always be forced true on hidpi Linux. Not sure of Windows.
  -- But on Mac by default I believe the GL resolution is non-retina unless you set this.
  -- Either way pixels_per_unit will handle the UI correctly
  [C.block| void { Fl::use_high_res_GL(0); } |]
  glwindow <- withRect glArea \gx gy gw gh ->
    [C.exp| HsGlWindow* { new HsGlWindow($(int gx), $(int gy), $(int gw), $(int gh)) } |]
  [C.block| void {
    $(HsGlWindow* glwindow)->mode(FL_OPENGL3 | FL_DEPTH | FL_RGB8 | FL_ALPHA | FL_MULTISAMPLE);
    $(HsGlWindow* glwindow)->end();
    // move to front of group so it comes before bottom menus in keyboard focus order
    $(Fl_Group* wholeGroup)->insert(*$(HsGlWindow* glwindow), 0);
  } |]
  drawCb <- makeDrawCallback \_ -> do
    mstuff <- modifyMVar varStuff \case
      GLPreload -> do
        scale <- [C.exp| float { $(HsGlWindow* glwindow)->pixels_per_unit() } |]
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
      w <- [C.exp| int { $(HsGlWindow* glwindow)->pixel_w() } |]
      h <- [C.exp| int { $(HsGlWindow* glwindow)->pixel_h() } |]
      let flatTrks = concat trks
      mAudioHandle <- getAudioHandle
      RGGraphics.drawTracks stuff (RGGraphics.WindowDims (fromIntegral w) (fromIntegral h)) t speed bg ?preferences.prefEliteLayout
        (flip mapMaybe selected \name -> (name,) <$> lookup name flatTrks)
        mAudioHandle
  [C.block| void {
    $(HsGlWindow* glwindow)->set_draw_callback($(void (*drawCb)(void*)), nullptr);
  } |]

  let deleteGL = do
        withMVar varStuff \case
          GLLoaded s -> RGGraphics.stopVideoLoaders s
          _          -> return ()
        -- TODO: delete glwindow widget
      redrawGL = [C.block| void { $(HsGlWindow* glwindow)->redraw(); } |]

  -- redraw directly (sink processing not triggered until menu closes)
  -- + only redraw GL window to not flicker the menu
  setCallback trackMenu redrawGL

  [C.block| void { $(Fl_Group* wholeGroup)->end(); } |]
  [C.block| void { $(Fl_Group* wholeGroup)->resizable((Fl_Gl_Window*)$(HsGlWindow* glwindow)); } |]
  return (wholeGroup, redrawGL, deleteGL)

-- _launchPreview, _promptPreview

applyDownmix :: Preferences -> SongYaml f -> SongYaml f
applyDownmix prefs song = if prefs.prefCHDownmix
  then mixDownStems song
  else song

launchBatch' :: (?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> [FilePath] -> IO ()
launchBatch' sink makeMenuBar startFiles = sink $ EventOnyx do
  prefs <- readPreferences
  let ?preferences = prefs
  stackIO $ launchBatch sink makeMenuBar startFiles

warnQuickConvert :: (?loc :: L) => Int -> (Event -> IO ()) -> Onyx () -> Onyx ()
warnQuickConvert count sink go = let
  warningText = l10nArgs "warning-rb-quick-convert"
    [("count", vNumber $ fromIntegral count)]
  in stackIO do
    withUTF8 warningText \cmsg -> do
      result <- [C.exp| int { fl_choice("%s", "Cancel", "Start", nullptr, $(char* cmsg)) } |]
      when (result == 1) $ sink $ EventOnyx go

launchBatch
  :: (?preferences :: Preferences, ?loc :: L)
  => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> [FilePath] -> IO ()
launchBatch sink makeMenuBar startFiles = mdo
  loadedFiles <- newMVar []
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
  window <- withUTF8 (l10n "function-batch") \s -> withSize windowSize \w h ->
    [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), fl_strdup($(char* s))) } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
  [C.block| void {
    $(Fl_Window* window)->color($(Fl_Color behindTabsColor));
  } |]
  setSizeRange window windowSize
  tabs <- withRect windowRect \tx ty tw th ->
    [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]

  (tabSongs, filterParts) <- makeTab windowRect (l10n "batch-tab-songs") \rect tab -> do
    setTabColor tab homeTabColor
    let (songsRect, importToggles) = chopBottom 40 rect
    group <- fileLoadWindow songsRect sink (l10nFull "file-type-song" []) (modifyMVar_ loadedFiles) startFiles findSongs \imp -> let
      entry = l10nArgs "batch-tab-song-entry"
        [ ("title", vString $ fromMaybe "" imp.impTitle)
        , ("has-title", vBool $ isJust imp.impTitle)
        , ("artist", vString $ fromMaybe "" imp.impArtist)
        , ("has-artist", vBool $ isJust imp.impArtist)
        , ("is-2x", vBool imp.imp2x)
        ]
      sublines = concat
        [ case imp.impAuthor of
          Just author | T.any (not . isSpace) author ->
            [l10nArgs "batch-tab-songs-data-author" [("author", vString author)]]
          _ -> []
        , [l10nArgs "batch-tab-songs-data-format" [("format", vString imp.impFormat)]]
        , [l10nArgs "batch-tab-songs-data-path"
          [ ("path", vString $ T.pack imp.impPath)
          , ("index", case imp.impIndex of
              Nothing -> vString "none"
              Just i  -> vNumber $ fromIntegral i
            )
          ]]
        ]
      in (entry, sublines)
    getter <- do
      let subrects = splitHorizN 7 importToggles
          insts =
            [ (l10n "instrument-guitar", Just Guitar)
            , (l10n "instrument-bass"  , Just Bass  )
            , (l10n "instrument-drums" , Just Drums )
            , (l10n "instrument-keys"  , Just Keys  )
            , (l10n "instrument-vocals", Just Vocal )
            , (l10n "instrument-other" , Nothing    )
            ]
      void $ withUTF8 (l10n "batch-tab-songs-import-label") \s ->
        withRect (head subrects) \bx by bw bh ->
          [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), fl_strdup($(char* s))) } |]
      getters <- forM (zip insts $ drop 1 subrects) \((txt, inst), subrect) -> do
        btn <- withRect subrect \bx by bw bh ->
          [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
        withUTF8 txt \clbl ->
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
    [ makeTab windowRect (l10n "game-tab-rb3") \rect tab -> do
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
    , makeTab windowRect (l10n "game-tab-rb2") \rect tab -> do
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
    , makeTab windowRect (l10n "game-tab-clone-hero") \rect tab -> do
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
    , makeTab windowRect (l10n "game-tab-gh1") \rect tab -> do
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
    , makeTab windowRect (l10n "game-tab-gh2") \rect tab -> do
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
              warn $ T.unpack $ l10n "warning-gh2-packs"
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
    , makeTab windowRect (l10n "game-tab-gh3") \rect tab -> do
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
          warn $ T.unpack $ l10n "warning-gh3-cache"
          return results
      return tab
    , makeTab windowRect (l10n "game-tab-ghwor") \rect tab -> do
      setTabColor tab functionTabColor
      batchPageGHWOR sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let ((target, creator), yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          let warnWoR = warn $ T.unpack $ l10n "warning-ghwor-cache"
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
    , makeTab windowRect (l10n "game-tab-rock-revolution") \rect tab -> do
      setTabColor tab functionTabColor
      batchPageRR sink rect tab \settings -> sink $ EventOnyx do
        files <- stackIO $ readMVar loadedFiles
        startTasks $ zip (map (.impPath) files) $ files <&> \f -> doImport f \proj -> do
          let ((target, creator), yaml) = settings proj
          proj' <- stackIO $ filterParts yaml >>= saveProject proj
          let warnRR = warn $ T.unpack $ l10n "warning-rr-renumber"
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
    , makeTab windowRect (l10n "tab-preview") \rect tab -> do
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
  (startTasks, cancelTasks) <- makeTab windowRect (l10n "tab-task") \rect tab -> do
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
  setCallback tabs $ updateTabsColor tabs
  [C.block| void { $(Fl_Tabs* tabs)->resizable($(Fl_Group* tabSongs)); } |]

  setCallback window $ windowCloser
    (cancelTasks >> modifyMVar_ cleanupOnCancel \m -> mapM_ release (HM.elems m) >> return HM.empty)
    window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->show();
  } |]

-- | A FLUID widget's rectangle in the layout's own coordinates. Only
-- meaningful before 'adaptWindow' runs, which is exactly what hand-written
-- page builders want: they apply the horizontal stretch themselves.
layoutRect :: Ptr Fl_Group -> IO Rectangle
layoutRect g = do
  x <- [C.exp| int { $(Fl_Group* g)->x() } |]
  y <- [C.exp| int { $(Fl_Group* g)->y() } |]
  w <- [C.exp| int { $(Fl_Group* g)->w() } |]
  h <- [C.exp| int { $(Fl_Group* g)->h() } |]
  return $ Rectangle (Position (X x) (Y y)) (Size (Width w) (Height h))

-- | Every window is a group; needed to add widgets back into one.
windowGroup :: Ptr Fl_Window -> Ptr Fl_Group
windowGroup w = [C.pure| Fl_Group* { $(Fl_Window* w) } |]

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

addTerm :: (?loc :: L) => Ptr Fl_Simple_Terminal -> TermMessage -> IO ()
addTerm term pair = do
  let newtxt = snd $ terminalMarkup $ filterUserError case pair of
        TermStart x y   -> l10nArgs "terminal-task-start"   [("task-name", vString $ T.pack x), ("task-path", vString $ T.pack y)]
        TermLog str     -> T.pack str
        TermWarning msg -> l10nArgs "terminal-task-warning" [("message", vString $ T.pack $ Exc.displayException msg)]
        TermError msg   -> l10nArgs "terminal-task-error"   [("message", vString $ T.pack $ Exc.displayException msg)]
        TermSuccess str -> l10nArgs "terminal-task-success" [("message", vString $ T.pack str)]
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

launchPreferences :: (?loc :: L) => (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> Onyx ()
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
        setCallback browseButton $ sink $ EventIO $ askFolder Nothing \dir ->
          withUTF8 dir \cdir ->
            [C.block| void { $(Fl_Input* input)->value($(char* cdir)); } |]
        resetButton <- withRect resetRect \bx by bw bh ->
          [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "@undo") } |]
        setCallback resetButton $ sink $ EventIO do
          [C.block| void { $(Fl_Input* input)->value(""); } |]
        return do
          val <- peekInputValue input
          return $ case val of
            "" -> Nothing
            x  -> Just $ T.unpack x
      combinePrefs :: [IO (a -> a)] -> IO (a -> a)
      combinePrefs = fmap (foldr (.) id) . sequence
  stackIO do
    window <- withSize windowSize \w h ->
      withUTF8 (l10n "window-preferences") \s ->
        [C.exp| Fl_Window* { new Fl_Double_Window($(int w), $(int h), fl_strdup($(char* s))) } |]
    [C.block| void { $(Fl_Window* window)->color($(Fl_Color behindTabsColor)); } |]
    menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
    let (tabsRect, bottomRect) = chopBottom bottomHeight $ snd $ chopTop menuHeight $ Rectangle
          (Position (X 0) (Y 0))
          windowSize
    [C.block| void {
      $(Fl_Window* window)->resizable($(Fl_Window* window));
    } |]
    setSizeRange window windowSize
    tabs <- withRect tabsRect \tx ty tw th ->
      [C.exp| Fl_Tabs* { new Fl_Tabs($(int tx), $(int ty), $(int tw), $(int th)) } |]
    (tab1, pref1) <- makeTab tabsRect (l10n "pref-tab-general") \rect tab -> do
      pack <- withRect rect \px py pw ph ->
        [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
      fn <- combinePrefs <$> sequence
        [ lineBox \box -> do
          let [checkBox, _, counterBox, _] = splitHorizN 4 box
          check <- withRect checkBox \cx cy cw ch ->
            [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int cx), $(int cy), $(int cw), $(int ch)) } |]
          withUTF8 (l10n "pref-unlimited-cores") \clbl ->
            [C.block| void { $(Fl_Check_Button* check)->copy_label($(char* clbl)); } |]
          let checkVal = if isNothing loadedPrefs.prefThreads then 1 else 0 :: CInt
          [C.block| void { $(Fl_Check_Button* check)->value($(int checkVal)); } |]
          counter <- withRect counterBox \cx cy cw ch ->
            [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
          let initCores = maybe 1 fromIntegral loadedPrefs.prefThreads :: CDouble
          [C.block| void {
            Fl_Counter* c = $(Fl_Counter* counter);
            c->labelsize(13);
            c->align(FL_ALIGN_LEFT);
            c->type(FL_SIMPLE_COUNTER);
            c->step(1);
            c->minimum(1);
            c->maximum(16);
            c->value($(double initCores));
          } |]
          withUTF8 (l10n "pref-core-limit") \clbl ->
            [C.block| void { $(Fl_Counter* counter)->copy_label($(char* clbl)); } |]
          let updateCounter = do
                v <- checkValue check
                if v
                  then [C.block| void { $(Fl_Counter* counter)->deactivate(); } |]
                  else [C.block| void { $(Fl_Counter* counter)->activate(); } |]
          setCallback check updateCounter
          updateCounter
          return do
            unlimited <- checkValue check
            cores <- [C.exp| double { $(Fl_Counter* counter)->value() } |]
            return \prefs -> prefs { prefThreads = if unlimited then Nothing else Just $ round cores }
        , do
          check <- lineBox \box -> do
            btn <- withRect box \bx by bw bh ->
              [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
            setLabelTooltip (l10nFull "pref-detect-muted" []) btn
            let v = if loadedPrefs.prefDetectMuted then 1 else 0 :: CInt
            [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
            return btn
          return $ (\b prefs -> prefs { prefDetectMuted = b }) <$> checkValue check
        , do
          check <- lineBox \box -> do
            btn <- withRect box \bx by bw bh ->
              [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
            setLabelTooltip (l10nFull "pref-limit-reduced-frets" []) btn
            let v = if loadedPrefs.prefReduceFrets then 1 else 0 :: CInt
            [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
            return btn
          return $ (\b prefs -> prefs { prefReduceFrets = b }) <$> checkValue check
        ]
      [C.block| void { $(Fl_Pack* pack)->end(); } |]
      return (tab, fn)
    restPrefs <- sequence
      [ makeTab tabsRect (l10n "pref-tab-rock-band") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getMagma <- lineBox \box -> horizRadio box
              [ (l10n "pref-magma-require", MagmaRequire, loadedPrefs.prefMagma == MagmaRequire)
              , (l10n "pref-magma-try"    , MagmaTry    , loadedPrefs.prefMagma == MagmaTry    )
              , (l10n "pref-magma-disable", MagmaDisable, loadedPrefs.prefMagma == MagmaDisable)
              ]
            return $ maybe id (\v prefs -> prefs { prefMagma = v }) <$> getMagma
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-black-venue") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefBlackVenue then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefBlackVenue = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-label-2x") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefLabel2x then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLabel2x = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              setLabelTooltip (l10nFull "pref-legal-tempos" []) btn
              let v = if loadedPrefs.prefLegalTempos then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLegalTempos = b }) <$> checkValue check
          , do
            getRB3Encoding <- lineBox \box -> do
              let (boxA, boxB) = chopLeft 230 box
              boxWidget <- withRect boxA \bx by bw bh ->
                [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-rb3-encoding-label") \clbl ->
                [C.block| void { $(Fl_Box* boxWidget)->copy_label($(char* clbl)); } |]
              horizRadio boxB
                [ (l10n "encoding-latin1", Latin1, loadedPrefs.prefRB3Encoding == Latin1)
                , (l10n "encoding-utf8"  , UTF8  , loadedPrefs.prefRB3Encoding == UTF8  )
                ]
            return $ maybe id (\v prefs -> prefs { prefRB3Encoding = v }) <$> getRB3Encoding
          , do
            getPackEncoding <- lineBox \box -> do
              let (boxA, boxB) = chopLeft 230 box
              boxWidget <- withRect boxA \bx by bw bh ->
                [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-pack-encoding-label") \clbl ->
                [C.block| void { $(Fl_Box* boxWidget)->copy_label($(char* clbl)); } |]
              horizRadio boxB
                [ (l10n "encoding-latin1", Just Latin1, loadedPrefs.prefPackEncoding == Just Latin1)
                , (l10n "encoding-utf8"  , Just UTF8  , loadedPrefs.prefPackEncoding == Just UTF8  )
                , (l10n "encoding-as-is", Nothing    , loadedPrefs.prefPackEncoding == Nothing    )
                ]
            return $ maybe id (\v prefs -> prefs { prefPackEncoding = v }) <$> getPackEncoding
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-guitar-hero") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ lineBox \box -> do
            let [_, middleBox, _] = splitHorizN 3 box
            gh2OffsetCounter <- withRect middleBox \cx cy cw ch ->
              [C.exp| Fl_Counter* { new Fl_Counter($(int cx), $(int cy), $(int cw), $(int ch)) } |]
            let initOffset = coerce $ loadedPrefs.prefGH2Offset * 1000 :: CDouble
            setLabelTooltip (l10nFull "pref-gh2-offset" []) gh2OffsetCounter
            [C.block| void {
              Fl_Counter* c = $(Fl_Counter* gh2OffsetCounter);
              c->labelsize(13);
              c->align(FL_ALIGN_LEFT);
              c->step(1);
              c->lstep(10);
              c->value($(double initOffset));
            } |]
            return $ (\ms prefs -> prefs { prefGH2Offset = realToFrac ms / 1000 }) <$>
              [C.exp| double { $(Fl_Counter* gh2OffsetCounter)->value() } |]
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-sort-gh2") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefSortGH2 then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefSortGH2 = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              setLabelTooltip (l10nFull "pref-artist-sort" []) btn
              let v = if loadedPrefs.prefArtistSort then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefArtistSort = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-clone-hero") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box (l10n "pref-dir-ch") $ T.pack $ fromMaybe "" loadedPrefs.prefDirCH
            return $ (\mdir prefs -> prefs { prefDirCH = mdir }) <$> getDir
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-ch-downmix") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefCHDownmix then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefCHDownmix = b }) <$> checkValue check
          , do
            audioFormat <- lineBox \box -> horizRadio box
              [ (l10n "pref-ch-audio-ogg" , CHAudioOggVorbis, loadedPrefs.prefCHAudioFormat == CHAudioOggVorbis)
              , (l10n "pref-ch-audio-opus", CHAudioOpus     , loadedPrefs.prefCHAudioFormat == CHAudioOpus     )
              ]
            return $ maybe id (\v prefs -> prefs { prefCHAudioFormat = v }) <$> audioFormat
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-360") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box (l10n "pref-dir-rb") $ T.pack $ fromMaybe "" loadedPrefs.prefDirRB
            return $ (\mdir prefs -> prefs { prefDirRB = mdir }) <$> getDir
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              setLabelTooltip (l10nFull "pref-trim-xbox" []) btn
              let v = if loadedPrefs.prefTrimXbox then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefTrimXbox = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              setLabelTooltip (l10nFull "pref-rb-number-id" []) btn
              let v = if loadedPrefs.prefRBNumberID then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefRBNumberID = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-ps3") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              setLabelTooltip (l10nFull "pref-ps3-encrypt" []) btn
              let v = if loadedPrefs.prefPS3Encrypt then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefPS3Encrypt = b }) <$> checkValue check
          , do
            getDir <- lineBox \box -> folderBox box (l10n "pref-dir-ps3") $ T.pack $ fromMaybe "" loadedPrefs.prefDirPS3
            return $ (\mdir prefs -> prefs { prefDirPS3 = mdir }) <$> getDir
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-wii") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getDir <- lineBox \box -> folderBox box (l10n "pref-dir-wii") $ T.pack $ fromMaybe "" loadedPrefs.prefDirWii
            return $ (\mdir prefs -> prefs { prefDirWii = mdir }) <$> getDir
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-3d-preview") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            getMSAA <- lineBox \box -> horizRadio box
              [ (l10n "pref-msaa-none", Nothing, loadedPrefs.prefMSAA == Nothing)
              , (l10n "pref-msaa-2x"  , Just 2 , loadedPrefs.prefMSAA == Just 2 )
              , (l10n "pref-msaa-4x"  , Just 4 , loadedPrefs.prefMSAA == Just 4 )
              , (l10n "pref-msaa-8x"  , Just 8 , loadedPrefs.prefMSAA == Just 8 )
              , (l10n "pref-msaa-16x" , Just 16, loadedPrefs.prefMSAA == Just 16)
              ]
            return $ maybe id (\v prefs -> prefs { prefMSAA = v }) <$> getMSAA
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-fxaa") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
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
              c->labelsize(13);
              c->align(FL_ALIGN_LEFT);
              c->step(1);
              c->lstep(10);
              c->minimum(1);
              c->value($(double initFPS));
            } |]
            withUTF8 (l10n "pref-fps") \clbl ->
              [C.block| void { $(Fl_Counter* fpsCounter)->copy_label($(char* clbl)); } |]
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
              $ (l10n "pref-elite-hihat-default"  , Nothing   , layoutLeftOpenHand == Nothing   ) NE.:|
              [ (l10n "pref-elite-hihat-cross", Just False, layoutLeftOpenHand == Just False)
              , (l10n "pref-elite-hihat-open" , Just True , layoutLeftOpenHand == Just True )
              ]
            getRightSide <- makeDropdown box2
              $ (l10n "pref-elite-ride-default"  , Nothing   , layoutRightNearCrash == Nothing   ) NE.:|
              [ (l10n "pref-elite-ride-far" , Just False, layoutRightNearCrash == Just False)
              , (l10n "pref-elite-ride-near", Just True , layoutRightNearCrash == Just True )
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
              withUTF8 (l10n "pref-waveform") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefWaveform then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefWaveform = b }) <$> checkValue check
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-lefty-flip") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
              let v = if loadedPrefs.prefLeftyFlip then 1 else 0 :: CInt
              [C.block| void { $(Fl_Check_Button* btn)->value($(int v)); } |]
              return btn
            return $ (\b prefs -> prefs { prefLeftyFlip = b }) <$> checkValue check
          ]
        [C.block| void { $(Fl_Pack* pack)->end(); } |]
        return fn
      , makeTab tabsRect (l10n "pref-tab-audio") \rect _tab -> do
        pack <- withRect rect \px py pw ph ->
          [C.exp| Fl_Pack* { new Fl_Pack($(int px), $(int py), $(int pw), $(int ph)) } |]
        fn <- combinePrefs <$> sequence
          [ do
            sliderQuality <- lineBox \box -> do
              let sliderRect = leaveLeftLabelSpace box
              slider <- withRect sliderRect \sx sy sw sh ->
                withUTF8 (l10n "decimal-separator") \csep ->
                  [C.exp| Fl_Value_Slider* { new_value_slider($(int sx), $(int sy), $(int sw), $(int sh), $(char* csep)) } |]
              let initQ = coerce $ loadedPrefs.prefOGGQuality * 10 :: CDouble
              [C.block| void {
                Fl_Value_Slider* s = $(Fl_Value_Slider* slider);
                s->labelsize(13);
                s->value_width(45);
                s->align(FL_ALIGN_LEFT);
                s->type(FL_HOR_NICE_SLIDER);
                s->minimum(0);
                s->maximum(10);
                s->value($(double initQ));
              } |]
              withUTF8 (l10n "pref-ogg-quality") \clbl ->
                [C.block| void { $(Fl_Value_Slider* slider)->copy_label($(char* clbl)); } |]
              return slider
            return $ (\v prefs -> prefs { prefOGGQuality = realToFrac v / 10 }) <$>
              [C.exp| double { $(Fl_Value_Slider* sliderQuality)->value() } |]
          , do
            check <- lineBox \box -> do
              btn <- withRect box \bx by bw bh ->
                [C.exp| Fl_Check_Button* { new Fl_Check_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
              withUTF8 (l10n "pref-decrypt-silent") \clbl ->
                [C.block| void { $(Fl_Check_Button* btn)->copy_label($(char* clbl)); } |]
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
      $(Fl_Button* saveButton)->color($(Fl_Color taskColor));
    } |]
    withUTF8 (l10n "button-save") \clbl ->
      [C.block| void { $(Fl_Button* saveButton)->copy_label($(char* clbl)); } |]
    setCallback saveButton do
      newPrefs <- ($ loadedPrefs) <$> combinePrefs (pref1 : restPrefs)
      let magmaWarning = l10n "pref-magma-warning"
          continueSave = do
            savePreferences newPrefs
            [C.block| void { $(Fl_Window* window)->hide(); } |]
      if loadedPrefs.prefMagma == MagmaRequire && newPrefs.prefMagma /= MagmaRequire
        then withUTF8 magmaWarning \cmsg -> withUTF8 (l10n "task-cancel") \cCancel -> withUTF8 (l10n "warning-confirm") \cOK -> do
          result <- [C.exp| int { fl_choice("%s", $(char* cCancel), $(char* cOK), nullptr, $(char* cmsg)) } |]
          when (result == 1) continueSave
        else continueSave
    cancelButton <- withRect rectButtonB \bx by bw bh ->
      [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh)) } |]
    withUTF8 (l10n "task-cancel") \clbl ->
      [C.block| void { $(Fl_Button* cancelButton)->copy_label($(char* clbl)); } |]
    setCallback cancelButton
      [C.block| void { $(Fl_Window* window)->hide(); } |]
    [C.block| void { $(Fl_Group* bottomArea)->end(); } |]
    [C.block| void {
      $(Fl_Window* window)->end();
      $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
      $(Fl_Window* window)->show();
    } |]

terminalMarkup :: T.Text -> (Int, T.Text)
terminalMarkup txt = let
  rich = parseRichText txt
  -- probably need handling for wide chars here
  len = sum [ T.length t | Plain t <- rich ]
  marked = rich <&> \case
    Plain t -> (t, False)
    Markup tag -> maybe ("", False) (, True) do
      guard $ tag.name == "color"
      if tag.closing
        then Just "\ESC[0m"
        else tag.value >>= \v -> lookup v
          -- supported Fl_Simple_Terminal ansi tags, not exactly unity's list
          [ ("black"  , "\ESC[40m")
          , ("red"    , "\ESC[41m")
          , ("green"  , "\ESC[42m")
          , ("yellow" , "\ESC[43m")
          , ("blue"   , "\ESC[44m")
          , ("magenta", "\ESC[45m")
          , ("cyan"   , "\ESC[46m")
          , ("white"  , "\ESC[47m")
          ]
  closer = if any snd marked
    then "\ESC[0m" -- reset color in case it wasn't
    else ""
  in (len, T.concat (map fst marked) <> closer)

terminalWelcome :: (?loc :: L) => [T.Text]
terminalWelcome = let
  -- leading space on a terminal line doesn't work for some reason.
  -- just replace with black characters on black background
  fixLeadingSpaceGeneral c1 c2 s = case span (== c1) $ T.unpack s of
    (""    , _   ) -> s
    (spaces, rest) -> T.pack $ "\ESC[30m" <> (c2 <$ spaces) <> "\ESC[0m" <> rest
  fixLeadingSpace = fixLeadingSpaceGeneral ' ' '.' . fixLeadingSpaceGeneral '\x3000' '、'
  render = snd . terminalMarkup
  header = T.lines $ render $ l10nArgs "welcome-header"
    [("version", vString $ T.pack $ showVersion version)]
  functions =
    [ (terminalMarkup $ "<color=green>"  <> l10n "function-load"  <> "</color>", map render $ T.lines $ l10n "welcome-load" )
    , (terminalMarkup $ "<color=red>"    <> l10n "function-batch" <> "</color>", map render $ T.lines $ l10n "welcome-batch")
    , (terminalMarkup $ "<color=yellow>" <> l10n "function-quick" <> "</color>", map render $ T.lines $ l10n "welcome-quick")
    , (terminalMarkup $ "<color=cyan>"   <> l10n "function-other" <> "</color>", map render $ T.lines $ l10n "welcome-other")
    ]
  -- hack for CJK language alignment
  detectedWideChars = case T.uncons $ l10n "function-load" of
    Just (c, _) -> utf8proc_charwidth c >= 2
    Nothing     -> False
  (initialTab, paddingSpace, divider) = if detectedWideChars
    then ("\x3000", "\x3000", "\x3000")
    else ("  "    , " "     , " | "   )
  longestNameLength = maximum [ len | ((len, _), _) <- functions ]
  renderedFunctions = functions <&> \((len, name), tasks) -> let
    nameWithPadding = name <> T.replicate (longestNameLength - len) paddingSpace
    leftSides = nameWithPadding : repeat (T.replicate longestNameLength paddingSpace)
    in zipWith (\l r -> initialTab <> l <> divider <> r) leftSides tasks
  in map fixLeadingSpace $ intercalate [""] (header : renderedFunctions) <> [""]

configureLanguageMenu :: (?loc :: L) => (Event -> IO ()) -> Ptr Fl_Menu_Button -> IO ()
configureLanguageMenu sink menu = do
  let sortedLanguages = sortOn (\lang -> T.toCaseFold lang.name) ?loc.allLanguages
  forM_ sortedLanguages \lang -> do
    let label = T.replace "/" "\\/" lang.name -- avoid making submenus
    cb <- makeCallback \_ _ -> sink $ EventOnyx do
      loadedPrefs <- readPreferences
      savePreferences loadedPrefs { prefLanguageCode = Just lang.code }
      newFluent <- stackIO $ loadLanguage ?loc.localizationRoot lang
      let originalLoc = ?loc in let ?loc = originalLoc { loadedFluent = newFluent } in do
        stackIO $ withUTF8 (l10n "language-restart") \msg -> withUTF8 (l10n "warning-confirm") \ok -> do
          [C.block| void {
            fl_choice("%s", $(char* ok), 0, 0, $(char* msg));
          } |]
    let isSelected = if lang.code == ?loc.loadedLanguage.code then 1 else 0
    withUTF8 label \s -> [C.block| void {
      int flags = FL_MENU_RADIO | ($(int isSelected) ? FL_MENU_VALUE : 0);
      $(Fl_Menu_Button* menu)->add($(char* s), 0, $(Fl_Callback cb), nullptr, flags);
    } |]

launchGUI :: (?loc :: L) => [FilePath] -> IO ()
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
  -- Layout lives in fluid/console.fl; see Onyx.GUI.Fluid. That file also owns
  -- the window colour, the button colours and the terminal's box, which FLUID
  -- always re-emits for a subclassed widget and would otherwise clobber.
  let consoleWidth = Width 650
  ui <- [C.exp| void* { new ConsoleWindow() } |]
  let termWindow   = [C.pure| Fl_Window*           { ((ConsoleWindow*)$(void* ui))->window } |]
      term         = [C.pure| Fl_Simple_Terminal*  { ((ConsoleWindow*)$(void* ui))->term } |]
      buttonGithub = [C.pure| Fl_Button*           { ((ConsoleWindow*)$(void* ui))->buttonGithub } |]
      labelLatest  = [C.pure| Fl_Box*              { ((ConsoleWindow*)$(void* ui))->labelLatest } |]
      languageMenu = [C.pure| Fl_Menu_Button*      { ((ConsoleWindow*)$(void* ui))->languageMenu } |]
      buttonOpen   = [C.pure| HsDndButton*         { ((ConsoleWindow*)$(void* ui))->buttonOpen } |]
      buttonBatch  = [C.pure| HsDndButton*         { ((ConsoleWindow*)$(void* ui))->buttonBatch } |]
      buttonQuick  = [C.pure| Fl_Button*           { ((ConsoleWindow*)$(void* ui))->buttonQuick } |]
      buttonMisc   = [C.pure| Fl_Button*           { ((ConsoleWindow*)$(void* ui))->buttonMisc } |]
  -- TODO set icon
  [C.block| void {
    Fl_Simple_Terminal* t = $(Fl_Simple_Terminal* term);
    t->history_lines(-1);
    t->ansi(true);
    t->stay_at_bottom(true);
  } |]
  let makeMenuBar (Width fullWidth) includeConsole = do
        let menuHeight = menuBarHeight
            menuX = 0
            menuY = 0
            menuWidth = stretchX fullWidth
            menuCrumbs = T.intercalate "/" . map (T.replace "/" "\\/")
            menuOptions =
              [ ( menuCrumbs [l10n "menu-file", l10n "menu-file-load"]
                , [C.pure| int { FL_COMMAND + 'o' } |]
                , promptLoad sink makeMenuBar hasAudio
                , 0
                )
              , ( menuCrumbs [l10n "menu-file", l10n "menu-file-batch"]
                , [C.pure| int { FL_COMMAND + 'b' } |]
                , launchBatch' sink makeMenuBar []
                , 0
                )
              , ( menuCrumbs [l10n "menu-file", l10n "menu-file-quick"]
                , [C.pure| int { FL_COMMAND + 'u' } |]
                , launchQuickConvert' sink makeMenuBar
                , 0
                )
              , ( menuCrumbs [l10n "menu-file", l10n "menu-file-other"]
                , [C.pure| int { FL_COMMAND + 't' } |]
                , launchMisc sink makeMenuBar
                , 0
                )
              , ( menuCrumbs [l10n "menu-file", l10n "menu-file-close"]
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
              , ( menuCrumbs [l10n "menu-edit", l10n "menu-edit-preferences"]
                , 0 -- maybe Cmd+, on Mac
                , sink $ EventOnyx $ launchPreferences sink makeMenuBar
                , 0
                )
              , ( menuCrumbs [l10n "menu-help", l10n "menu-help-readme"]
                , 0
                , sink $ EventIO do
                    getResourcesPath "README.html" >>= osOpenFile
                , 0
                )
              , ( menuCrumbs [l10n "menu-help", l10n "menu-help-updates"]
                , 0
                , sink $ EventIO do
                    getResourcesPath "CHANGES.txt" >>= osOpenFile
                , 0
                )
              , ( menuCrumbs [l10n "menu-help", l10n "menu-help-license"]
                , 0
                , sink $ EventIO do
                    getResourcesPath "LICENSE.txt" >>= osOpenFile
                , 0
                )
              , ( menuCrumbs [l10n "menu-help", l10n "menu-help-credits"]
                , 0
                , sink $ EventIO do
                    getResourcesPath "CREDITS.txt" >>= osOpenFile
                , 0
                )
              ] ++ do
                guard includeConsole
                return
                  ( menuCrumbs [l10n "menu-view", l10n "menu-view-console"]
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
              withUTF8 a \str -> do
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
  -- reserve the menu bar space the layout was authored without, then fill it
  _ <- adaptWindow termWindow
  inGroup (windowGroup termWindow) $ void $ makeMenuBar consoleWidth macOS
  lockMinimumSize termWindow

  setCallback buttonGithub $ osOpenFile "https://github.com/mtolly/onyx/releases"
  _ <- forkIO $ isNewestRelease \comp -> sink $ EventIO $ case comp of
    LT -> withUTF8 (l10n "version-new") \s ->
      [C.block| void {
        $(Fl_Box* labelLatest)->copy_label($(char* s));
        $(Fl_Box* labelLatest)->labelcolor(FL_WHITE);
      } |]
    EQ -> withUTF8 (l10n "version-latest") \s ->
      [C.block| void {
        $(Fl_Box* labelLatest)->copy_label($(char* s));
      } |]
    GT -> withUTF8 (l10n "version-prerelease") \s ->
      [C.block| void {
        $(Fl_Box* labelLatest)->copy_label($(char* s));
        $(Fl_Box* labelLatest)->labelcolor(FL_WHITE);
      } |]
  configureLanguageMenu sink languageMenu

  setCallback buttonOpen $ promptLoad sink makeMenuBar hasAudio
  setDndButtonCallback buttonOpen (sink . EventOnyx . startLoad makeMenuBar hasAudio)

  setCallback buttonBatch $ launchBatch' sink makeMenuBar []
  setDndButtonCallback buttonBatch (launchBatch' sink makeMenuBar)

  setCallback buttonQuick $ launchQuickConvert' sink makeMenuBar
  setCallback buttonMisc $ launchMisc sink makeMenuBar

  setCallback termWindow $ windowCloser (return ()) termWindow
  [C.block| void { $(Fl_Window* termWindow)->show(); } |]

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

  mapM_ (addTerm term . TermLog . T.unpack) terminalWelcome

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
    if flags .&. [C.pure| int { FL_MENU_VALUE } |] /= 0
      then do
        cstr <- [C.exp| const char* { $(Fl_Menu_Button* menu)->menu()[$(int i)].label() } |]
        if cstr == nullPtr
          then return Nothing
          else Just . T.pack <$> peekUTF8 cstr
      else return Nothing
