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
                                                    freeHaskellFunPtr, peek)
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
import           Onyx.GUI.Util                     (askFolder)
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

data TermMessage
  = TermStart String String -- "Task 4" "/path/to/file"
  | TermLog String
  | TermWarning Message
  | TermError Message
  | TermSuccess String

toTermMessage :: (MessageLevel, Message) -> TermMessage
toTermMessage (MessageLog    , msg) = TermLog msg.messageString
toTermMessage (MessageWarning, msg) = TermWarning msg

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

  let logChan = logIO $ sink . EventMsg
      wait = if macOS
        then [C.exp| int { Fl::wait(1e20) } |] >> return True
        else (/= 0) <$> [C.exp| int { Fl::wait() } |]

  testWindow <- [C.block| Fl_Window* {
    Fl_Window *window = new Fl_Window(340, 180);

    Fl_Button *box = new Fl_Button(20, 40, 300, 100, "Hello, World!");
    box->labelfont(FL_BOLD + FL_ITALIC);
    box->labelsize(36);
    box->labeltype(FL_SHADOW_LABEL);
    window->resizable(box);

    window->end();
    window->show();
    return window;
  } |]

  void $ runResourceT $ (`runReaderT` sink) $ logChan let
    -- when we have processed a max number of messages, we want to do a quick
    -- redraw and then continue processing. hopefully awake can be used when
    -- fltk's not in control? documentation is unclear
    process 0               = liftIO [C.block| void { Fl::awake(); } |]
    process messageCapacity = liftIO (atomically $ tryReadTChan evts) >>= \case
      Nothing -> return ()
      Just e -> do
        case e of
          EventMsg    pair -> liftIO $ return () -- TODO: addTerm term $ toTermMessage pair
          EventFail   msg  -> liftIO $ return () -- TODO: addTerm term $ TermError msg
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
