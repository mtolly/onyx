{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
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

-- import           Onyx.GUI.Batch
import           Onyx.GUI.Core
-- import           Onyx.GUI.Misc
import           Onyx.GUI.Song
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
import           Data.Conduit.Audio                (integralSample, mapSamples)
import           Data.Function                     ((&))
import           Data.Functor                      ((<&>))
import qualified Data.Set                          as Set
import           Optics                            (mapped, (%), (%~), (.~))
-- import qualified Data.Connection                           as Conn
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
                                                    freeHaskellFunPtr, peek,
                                                    toBool)
import           Foreign.C                         (CInt, CString)
-- import qualified Graphics.UI.FLTK.LowLevel.Base.GlWindow   as FLGL
-- import qualified Graphics.UI.FLTK.LowLevel.FL              as FLTK
-- import qualified Graphics.UI.FLTK.LowLevel.Fl_Enumerations as FLE
-- import           Graphics.UI.FLTK.LowLevel.FLTKHS          (Height (..),
--                                                             Position (..),
--                                                             Rectangle (..),
--                                                             Size (..),
--                                                             Width (..), X (..),
--                                                             Y (..))
-- import qualified Graphics.UI.FLTK.LowLevel.FLTKHS          as FL
-- import           Graphics.UI.FLTK.LowLevel.X               (openCallback)
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
import           Onyx.GUI.Util                     (askFolder, withUTF8,
                                                    peekUTF8)
import           Onyx.Harmonix.Ark.GH2             (GH2InstallLocation (..))
import qualified Onyx.Harmonix.DTA.Serialize.GH2   as D
import           Onyx.Harmonix.DTA.Serialize.Magma (Gender (..))
import           Onyx.Harmonix.MOGG                (oggToMoggFiles)
import           Onyx.Harmonix.RockBand.Score
import           Onyx.Harmonix.RockBand.SongCache  (hardcodeSongCacheIDs)
import           Onyx.Image.DXT                    (readRBImageMaybe)
import           Onyx.Import
import           Onyx.Import.GuitarHero2           (Setlist (..),
                                                    loadSetlistFull)
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
C.include "FL/Fl_Button.H"
C.include "FL/Fl_Box.H"
C.include "FL/Fl_Terminal.H"
C.include "FL/Fl_Menu_Bar.H"
C.include "FL/Fl_Sys_Menu_Bar.H"
C.include "FL/Fl_Tabs.H"
C.include "FL/Fl_Native_File_Chooser.H"
C.include "FL/fl_ask.H"

-- | Ignores the Escape key, and runs a resource-release function after close.
windowCloser :: IO () -> Ptr Fl_Window -> IO ()
windowCloser finalize window = do
  isEscape <- [C.exp| bool {
    Fl::event() == FL_SHORTCUT && Fl::event_key() == FL_Escape
  } |]
  unless (toBool isEscape) do
    [C.block| void { $(Fl_Window* window)->hide(); } |]
    finalize

data TermMessage
  = TermStart String String -- "Task 4" "/path/to/file"
  | TermLog String
  | TermWarning Message
  | TermError Message
  | TermSuccess String

toTermMessage :: (MessageLevel, Message) -> TermMessage
toTermMessage (MessageLog    , msg) = TermLog msg.messageString
toTermMessage (MessageWarning, msg) = TermWarning msg

addTerm :: Ptr Fl_Terminal -> TermMessage -> IO ()
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
      $(Fl_Terminal* term)->append($(char* cs));
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
  let consoleWidth = Width 650
      consoleHeight = Height 480
  termWindow <- [C.exp| Fl_Window* {
    new Fl_Window(650, 480, "Onyx Console")
  } |]
  [C.block| void { $(Fl_Window* termWindow)->xclass("Onyx"); } |]
  -- TODO set icon
  [C.block| void {
    $(Fl_Window* termWindow)->size_range(650, 480);
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
  menuHeight <- makeMenuBar consoleWidth macOS
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
  term <- [C.block| Fl_Terminal* {
    Fl_Terminal* term = new Fl_Terminal
      ( 10
      , 45 + $(int menuHeight)
      , 630
      , 345 - $(int menuHeight)
      );
    term->history_lines(-1); // unlimited
    term->ansi(true);
    // stay at bottom? can't find
    return term;
  } |]

  let bottomBar1 = Rectangle (Position (X 5) (Y 400)) (Size (Width 640) (Height 30))
      bottomBar2 = Rectangle (Position (X 5) (Y 440)) (Size (Width 640) (Height 30))
      [areaOpen, areaBatch] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar1
      [areaQuick, areaMisc] = map (trimClock 0 5 0 5) $ splitHorizN 2 bottomBar2

  -- TODO drag and drop support for open/batch buttons

  cbButtonOpen <- makeCallback \_ _ -> promptLoad sink makeMenuBar hasAudio
  withRect areaOpen \x y w h -> [C.block| void {
    Fl_Button* buttonOpen = new Fl_Button($(int x), $(int y), $(int w), $(int h), "Load a song");
    buttonOpen->color($(Fl_Color loadSongColor));
    buttonOpen->callback($(Fl_Callback cbButtonOpen));
    buttonOpen->labelsize(13);
  } |]

  cbButtonBatch <- makeCallback \_ _ -> launchBatch' sink makeMenuBar []
  withRect areaBatch \x y w h -> [C.block| void {
    Fl_Button* buttonBatch = new Fl_Button($(int x), $(int y), $(int w), $(int h), "Batch recompile");
    buttonBatch->color($(Fl_Color batchProcessColor));
    buttonBatch->callback($(Fl_Callback cbButtonBatch));
    buttonBatch->labelsize(13);
  } |]

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
    $(Fl_Window* termWindow)->resizable($(Fl_Terminal* term));
    $(Fl_Window* termWindow)->callback($(Fl_Callback cbTermWindow));
    $(Fl_Window* termWindow)->show();
  } |]

  -- TODO support drag and drop onto mac app icon

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
    [ "\ESC[45mOnyx\ESC[0m Music Game Toolkit, version " <> showVersion version
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
        -- TODO: implement plan selection dialog
        -- For now, just pick the first plan
        sink $ EventOnyx $ withPlan k
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
    [C.exp| Fl_Window* { new Fl_Window($(int w), $(int h), "Select Songs") } |]
  menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
  let (_, windowRect) = chopTop menuHeight $ Rectangle
        (Position (X 0) (Y 0))
        windowSize
      (songsArea, trimClock 10 0 0 0 -> buttonsArea) =
        chopBottom 40 $ trimClock 10 10 10 10 windowRect
      [areaSelect, trimClock 0 0 0 10 -> areaOpen] = splitHorizN 2 buttonsArea

  -- TODO: translate scrolled list of songs with checkbuttons
  -- For now just show a simple message and open all songs
  _ <- withRect songsArea \bx by bw bh ->
    [C.exp| Fl_Box* { new Fl_Box($(int bx), $(int by), $(int bw), $(int bh), "Multiple songs found. Click Open to load all.") } |]

  selectButton <- withRect areaSelect \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Close") } |]
  cbClose <- makeCallback \_ _ -> [C.block| void { $(Fl_Window* window)->hide(); } |]
  [C.block| void { $(Fl_Button* selectButton)->callback($(Fl_Callback cbClose)); } |]

  openButton <- withRect areaOpen \bx by bw bh ->
    [C.exp| Fl_Button* { new Fl_Button($(int bx), $(int by), $(int bw), $(int bh), "Open All") } |]
  [C.block| void { $(Fl_Button* openButton)->color($(Fl_Color taskColor)); } |]
  cbOpen <- makeCallback \_ _ -> do
    forM_ imps \imp -> do
      sink $ EventOnyx $ void $ forkOnyx do
        continueImport makeMenuBar hasAudio imp
    [C.block| void { $(Fl_Window* window)->hide(); } |]
  [C.block| void { $(Fl_Button* openButton)->callback($(Fl_Callback cbOpen)); } |]

  [C.block| void {
    $(Fl_Window* window)->end();
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
launchWindow sink makeMenuBar proj _song _maybeAudio _albumArt isRB = mdo
  let windowWidth = Width 800
      windowHeight = Height 500
      windowSize = Size windowWidth windowHeight
      title = T.unpack $ T.replace "/" "_" $ fromMaybe "Song" proj.projectSongYaml.metadata.title
  window <- withRect (Rectangle (Position (X 0) (Y 0)) windowSize) \_ _ w h ->
    withUTF8 title \ctitle ->
      [C.exp| Fl_Window* { new Fl_Window($(int w), $(int h), $(char* ctitle)) } |]
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

  -- TODO: Metadata tab (needs text inputs, album art, checkboxes)
  -- TODO: Instruments tab (needs tree widget)
  -- TODO: Preview tab (needs OpenGL, audio playback)
  -- TODO: Stars tab (needs tier/rank widgets)

  -- For now, project modifications just save without metadata/instrument edits
  let fullProjModify :: [SongYaml FilePath -> SongYaml FilePath] -> Project -> IO Project
      fullProjModify extraMods p = do
        let newYaml = foldr ($) p.projectSongYaml extraMods
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
  -- TODO: Utilities tab (web preview, MIDI reductions, pro keys check)

  let tabsToDisable = [rb3Tab, rb2Tab, psTab, gh1Tab, gh2Tab, gh3Tab, worTab, rrTab]
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
    (cancelTasks >> mapM_ release proj.projectRelease)
    window
  [C.block| void {
    $(Fl_Window* window)->end();
    $(Fl_Window* window)->resizable($(Fl_Tabs* tabs));
    $(Fl_Window* window)->callback($(Fl_Callback cbWindow));
    $(Fl_Window* window)->show();
  } |]

saveProject :: Project -> SongYaml FilePath -> IO Project
saveProject proj song = do
  yamlEncodeFile proj.projectLocation $ toJSON song
  return proj { projectSongYaml = song }

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
    [C.block| Fl_Terminal* {
      Fl_Terminal* t = new Fl_Terminal($(int tx), $(int ty), $(int tw), $(int th));
      t->history_lines(-1);
      t->ansi(true);
      return t;
    } |]
  [C.block| void {
    $(Fl_Group* tab)->resizable($(Fl_Terminal* termTask));
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

applyDownmix :: Preferences -> SongYaml f -> SongYaml f
applyDownmix prefs song = if prefs.prefCHDownmix
  then mixDownStems song
  else song

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
    withUTF8 (T.unpack warningText) \cmsg -> do
      result <- [C.exp| int { fl_choice("%s", "Cancel", "Start", nullptr, $(char* cmsg)) } |]
      when (result == 1) $ sink $ EventOnyx go

launchPreferences :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> Onyx ()
launchPreferences _sink _makeMenuBar = do
  -- TODO: translate preferences window
  return ()

launchBatch' :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> [FilePath] -> IO ()
launchBatch' sink _makeMenuBar _startFiles = do
  -- TODO: translate batch window
  sink $ EventOnyx $ warn "Batch recompile window not yet translated"

launchQuickConvert' :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchQuickConvert' sink _makeMenuBar = do
  -- TODO: translate quick convert window
  sink $ EventOnyx $ warn "Quick convert window not yet translated"

launchMisc :: (Event -> IO ()) -> (Width -> Bool -> IO CInt) -> IO ()
launchMisc sink _makeMenuBar = do
  -- TODO: translate misc tools window
  sink $ EventOnyx $ warn "Other tools window not yet translated"
