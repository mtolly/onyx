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
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Onyx.GUI (launchGUI) where

-- import           Onyx.GUI.Batch
import           Onyx.GUI.Core
-- import           Onyx.GUI.Misc
-- import           Onyx.GUI.Song

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
import           Foreign.C                         (CString)
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
import           Onyx.GUI.Util                     (askFolder, withUTF8)
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
                , return () -- promptLoad sink makeMenuBar hasAudio
                , 0
                )
              , ( "File/Batch Recompile"
                , [C.pure| int { FL_COMMAND + 'b' } |]
                , return () -- launchBatch' sink makeMenuBar []
                , 0
                )
              , ( "File/Quick Convert"
                , [C.pure| int { FL_COMMAND + 'u' } |]
                , return () -- launchQuickConvert' sink makeMenuBar
                , 0
                )
              , ( "File/Tools"
                , [C.pure| int { FL_COMMAND + 't' } |]
                , return () -- launchMisc sink makeMenuBar
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
                , return () -- sink $ EventOnyx $ launchPreferences sink makeMenuBar
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
        return 30 -- TODO
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

      Rectangle (Position (X areaOpenX) (Y areaOpenY)) (Size (Width areaOpenW) (Height areaOpenH)) = areaOpen
      Rectangle (Position (X areaBatchX) (Y areaBatchY)) (Size (Width areaBatchW) (Height areaBatchH)) = areaBatch
      Rectangle (Position (X areaQuickX) (Y areaQuickY)) (Size (Width areaQuickW) (Height areaQuickH)) = areaQuick
      Rectangle (Position (X areaMiscX) (Y areaMiscY)) (Size (Width areaMiscW) (Height areaMiscH)) = areaMisc

  -- TODO drag and drop support for open/batch buttons

  cbButtonOpen <- makeCallback \_ _ -> return () -- TODO: promptLoad sink makeMenuBar hasAudio
  [C.block| void {
    Fl_Button* buttonOpen = new Fl_Button($(int areaOpenX), $(int areaOpenY), $(int areaOpenW), $(int areaOpenH), "Load a song");
    buttonOpen->color($(Fl_Color loadSongColor));
    buttonOpen->callback($(Fl_Callback cbButtonOpen));
    buttonOpen->labelsize(13);
  } |]

  cbButtonBatch <- makeCallback \_ _ -> return () -- TODO: launchBatch' sink makeMenuBar []
  [C.block| void {
    Fl_Button* buttonBatch = new Fl_Button($(int areaBatchX), $(int areaBatchY), $(int areaBatchW), $(int areaBatchH), "Batch recompile");
    buttonBatch->color($(Fl_Color batchProcessColor));
    buttonBatch->callback($(Fl_Callback cbButtonBatch));
    buttonBatch->labelsize(13);
  } |]

  cbButtonQuick <- makeCallback \_ _ -> return () -- TODO: launchQuickConvert' sink makeMenuBar
  [C.block| void {
    Fl_Button* buttonQuick = new Fl_Button($(int areaQuickX), $(int areaQuickY), $(int areaQuickW), $(int areaQuickH), "Quick convert/pack");
    buttonQuick->color($(Fl_Color quickConvertColor));
    buttonQuick->callback($(Fl_Callback cbButtonQuick));
    buttonQuick->labelsize(13);
  } |]

  cbButtonMisc <- makeCallback \_ _ -> return () -- TODO: launchMiscConvert' sink makeMenuBar
  [C.block| void {
    Fl_Button* buttonMisc = new Fl_Button($(int areaMiscX), $(int areaMiscY), $(int areaMiscW), $(int areaMiscH), "Other tools");
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
    _  -> sink $ EventOnyx $ return () -- TODO: startLoad makeMenuBar hasAudio initSongs

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
