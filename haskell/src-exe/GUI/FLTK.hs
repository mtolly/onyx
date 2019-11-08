{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module GUI.FLTK (launchGUI) where

import           Audio                                     (Audio (..),
                                                            buildSource',
                                                            runAudio)
import           CommandLine                               (copyDirRecursive,
                                                            runDolphin)
import           Config
import           Control.Arrow                             (first)
import           Control.Concurrent                        (MVar, ThreadId,
                                                            forkIO, killThread,
                                                            modifyMVar,
                                                            modifyMVar_,
                                                            newChan, newMVar,
                                                            putMVar, readChan,
                                                            readMVar, takeMVar,
                                                            writeChan)
import           Control.Concurrent.Async                  (async,
                                                            waitAnyCancel)
import           Control.Concurrent.STM                    (atomically)
import           Control.Concurrent.STM.TChan              (newTChanIO,
                                                            tryReadTChan,
                                                            writeTChan)
import qualified Control.Exception                         as Exc
import           Control.Monad                             (forM, forM_, guard,
                                                            join, unless, void,
                                                            when, (>=>))
import           Control.Monad.Catch                       (catchIOError)
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
import qualified Data.Aeson                                as A
import qualified Data.ByteString                           as B
import qualified Data.ByteString.Char8                     as B8
import           Data.Char                                 (isSpace, toLower,
                                                            toUpper)
import qualified Data.Connection                           as Conn
import           Data.Default.Class                        (def)
import qualified Data.EventList.Absolute.TimeBody          as ATB
import qualified Data.EventList.Relative.TimeBody          as RTB
import           Data.Fixed                                (Milli)
import           Data.Foldable                             (toList)
import qualified Data.HashMap.Strict                       as HM
import           Data.IORef                                (IORef, newIORef,
                                                            readIORef,
                                                            writeIORef)
import qualified Data.Map.Strict                           as Map
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe, isJust,
                                                            listToMaybe)
import qualified Data.Text                                 as T
import qualified Data.Text.Encoding                        as TE
import           Data.Time                                 (getCurrentTime)
import           Data.Version                              (showVersion)
import           DryVox
import           Foreign                                   (Ptr)
import           Foreign.C                                 (CString,
                                                            withCString)
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
import           Reaper.Build                              (makeReaperIO)
import qualified Reaper.Extract                            as RPP
import qualified Reaper.Parse                              as RPP
import qualified Reaper.Scan                               as RPP
import           Reductions                                (simpleReduce)
import qualified RhythmGame.Drums                          as RGDrums
import           RockBand.Codec                            (mapTrack)
import qualified RockBand.Codec.Drums                      as D
import           RockBand.Codec.File                       (FlexPartName (..))
import qualified RockBand.Codec.File                       as RBFile
import           RockBand.Common                           (RB3Instrument (..))
import           RockBand.SongCache                        (fixSongCache)
import           Scripts                                   (loadMIDI)
import qualified Sound.File.Sndfile                        as Snd
import qualified Sound.MIDI.File.Load                      as Load
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
import           Text.Decode                               (decodeGeneral)
import           Text.Read                                 (readMaybe)

#ifdef WINDOWS
import           Foreign                                   (intPtrToPtr, peek)
import           Graphics.Win32                            (loadIcon)
import           System.Win32                              (HINSTANCE)
#endif

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
  void $ forkOnyx $ errorToEither (openProject Nothing f) >>= \case
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
  box <- FL.boxNew rect Nothing
  FL.setResizable group $ Just box
  x <- fn rect
  FL.end group
  return x

makeTab :: Rectangle -> T.Text -> (Rectangle -> FL.Ref FL.Group -> IO a) -> IO a
makeTab rect name fn = do
  let tabHeight = 25
      (_, innerRect) = chopTop tabHeight rect
  group <- FL.groupNew innerRect (Just name)
  res <- fn innerRect group
  FL.end group
  return res

launchWindow :: (Event -> IO ()) -> Project -> IO ()
launchWindow sink proj = mdo
  let windowSize = Size (Width 800) (Height 500)
      windowRect = Rectangle
        (Position (X 0) (Y 0))
        windowSize
  window <- FL.windowNew
    windowSize
    Nothing
    (Just $ fromMaybe "Song" $ _title $ _metadata $ projectSongYaml proj)
  behindTabsColor >>= FL.setColor window
  FL.setResizable window $ Just window -- this is needed after the window is constructed for some reason
  FL.sizeRange window $ Size (Width 800) (Height 500)
  FL.begin window
  tabs <- FL.tabsNew
    (Rectangle (Position (X 0) (Y 0)) windowSize)
    Nothing
  metaTab <- makeTab windowRect "Metadata" $ \rect tab -> do
    homeTabColor >>= setTabColor tab
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
    FL.setResizable tab $ Just pack
    return tab
  {-
  makeTab windowRect "Audio" $ \_ _ -> return ()
  makeTab windowRect "Instruments" $ \_ _ -> return ()
  makeTab windowRect "Rock Band 3" $ \_ _ -> return ()
  makeTab windowRect "Rock Band 2" $ \_ _ -> return ()
  makeTab windowRect "Clone Hero/Phase Shift" $ \_ _ -> return ()
  -}
  utilsTab <- makeTab windowRect "Utilities" $ \rect tab -> do
    functionTabColor >>= setTabColor tab
    pack <- FL.packNew rect Nothing
    padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
      btn <- FL.buttonNew rect' $ Just "Produce MIDI with automatic reductions"
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
      FL.setCallback btn $ \_ -> sink $ EventOnyx $ do
        startTasks [("Pro Keys range check", proKeysHanging Nothing proj >> return [])]
    FL.end pack
    FL.setResizable tab $ Just pack
    return tab
  let nonTermTabs = [metaTab, utilsTab]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" $ \rect tab -> do
    taskTabColor >>= setTabColor tab
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
  FL.setResizable tabs $ Just metaTab
  FL.end window
  FL.setResizable window $ Just tabs
  FL.setCallback window $ windowCloser $ do
    cancelTasks
    mapM_ release $ projectRelease proj
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

saveProject :: Project -> SongYaml -> IO Project
saveProject proj song = do
  yamlEncodeFile (projectLocation proj) $ toJSON song
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

applyGBK2 :: GBKOption -> SongYaml -> TargetRB2 -> TargetRB2
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

horizRadio :: Rectangle -> [(T.Text, a, Bool)] -> IO (IO (Maybe a))
horizRadio _    []   = error "horizRadio: empty option list"
horizRadio rect opts = do
  let rects = splitHorizN (length opts) rect
  btns <- forM (zip opts rects) $ \((label, _, b), rectButton) -> do
    btn <- FL.roundButtonNew rectButton $ Just label
    void $ FL.setValue btn b
    return btn
  forM_ btns $ \opt -> FL.setCallback opt $ \_ -> do
    forM_ btns $ \opt' -> FL.setValue opt' $ opt == opt'
  return $ do
    bools <- mapM FL.getValue btns
    return $ fmap (\(_, (_, x, _)) -> x) $ listToMaybe $ filter fst $ zip bools opts

speedPercent :: Rectangle -> IO (IO Double)
speedPercent rect = do
  speed <- FL.counterNew rect (Just "Speed (%)")
  FL.setLabelsize speed $ FL.FontSize 13
  FL.setLabeltype speed FLE.NormalLabelType FL.ResolveImageLabelDoNothing
  FL.setAlign speed $ FLE.Alignments [FLE.AlignTypeLeft]
  FL.setStep speed 1
  FL.setLstep speed 5
  FL.setMinimum speed 1
  void $ FL.setValue speed 100
  FL.setTooltip speed "Change the speed of the chart and its audio (without changing pitch). If importing from a CON, a non-100% value requires unencrypted audio."
  return $ (/ 100) <$> FL.getValue speed

batchPageRB2
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> ([(TargetRB2, FilePath)], SongYaml)) -> IO ())
  -> IO ()
batchPageRB2 sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 250 5 250 (Size (Width 300) (Height 35)) speedPercent
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
            [ templateApplyInput proj
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
  -> ((Project -> (TargetPS, PSCreate)) -> IO ())
  -> IO ()
batchPagePS sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 250 5 250 (Size (Width 300) (Height 35)) speedPercent
  let getTargetSong usePath template = do
        speed <- getSpeed
        return $ \proj -> let
          tgt = def
            { ps_Common = (ps_Common def)
              { tgt_Speed = Just speed
              }
            }
          fout = T.unpack $ foldr ($) template
            [ templateApplyInput proj
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
    "%input_dir%/%input_base%%modifiers%_ps"
    (getTargetSong PSDir >=> build)
  makeTemplateRunner
    sink
    "Create PS zips"
    "%input_dir%/%input_base%%modifiers%_ps.zip"
    (getTargetSong PSZip >=> build)
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

batchPageRB3
  :: (Event -> IO ())
  -> Rectangle
  -> FL.Ref FL.Group
  -> ((Project -> ([(TargetRB3, RB3Create)], SongYaml)) -> IO ())
  -> IO ()
batchPageRB3 sink rect tab build = do
  pack <- FL.packNew rect Nothing
  getSpeed <- padded 10 250 5 250 (Size (Width 300) (Height 35)) speedPercent
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
          tgt = applyGBK gbk yaml def
            { rb3_Common = (rb3_Common def)
              { tgt_Speed = Just speed
              }
            }
          kicksConfigs = case (kicks, maybe Kicks1x drumsKicks $ getPart FlexDrums yaml >>= partDrums) of
            (_        , Kicks1x) -> [(False, ""   )]
            (Kicks1x  , _      ) -> [(False, "_1x")]
            (Kicks2x  , _      ) -> [(True , "_2x")]
            (KicksBoth, _      ) -> [(False, "_1x"), (True, "_2x")]
          fout kicksLabel = T.unpack $ foldr ($) template
            [ templateApplyInput proj
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

templateApplyInput :: Project -> T.Text -> T.Text
templateApplyInput proj txt = foldr ($) txt
  [ T.intercalate (T.pack $ takeDirectory $ projectTemplate proj) . T.splitOn "%input_dir%"
  , T.intercalate (T.pack $ takeFileName $ projectTemplate proj) . T.splitOn "%input_base%"
  ]

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
    (\template -> build $ \proj -> T.unpack $ templateApplyInput proj template)
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

homeTabColor, functionTabColor, taskTabColor, globalLogColor, loadSongColor, batchProcessColor, behindTabsColor :: IO FLE.Color
homeTabColor      = FLE.rgbColorWithRgb (209,177,224)
functionTabColor  = FLE.rgbColorWithRgb (224,210,177)
taskTabColor      = FLE.rgbColorWithRgb (179,221,187)
globalLogColor    = FLE.rgbColorWithRgb (114,74,124)
loadSongColor     = FLE.rgbColorWithRgb (177,173,244)
batchProcessColor = FLE.rgbColorWithRgb (237,173,193)
behindTabsColor   = FLE.rgbColorWithRgb (94,94,94) -- TODO report incorrect Char binding type for rgbColorWithGrayscale

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
  getVocalTrack <- padded 5 10 5 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    fn <- horizRadio rect'
      [ ("PART VOCALS", Nothing, False)
      , ("HARM1", Just Vocal1, False)
      , ("HARM2", Just Vocal2, False)
      , ("HARM3", Just Vocal3, False)
      ]
    return $ join <$> fn
  let dryvoxButton label fn = padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
        btn <- FL.buttonNew rect' $ Just label
        FL.setCallback btn $ \_ -> sink $ EventIO $ do
          input <- pickedFile
          voc <- getVocalTrack
          picker <- FL.nativeFileChooserNew $ Just FL.BrowseSaveFile
          FL.setTitle picker "Save lipsync audio"
          FL.setFilter picker "*.wav"
          FL.setPresetFile picker $ T.pack $ dropExtension input ++ case voc of
            Nothing     -> "-solovox.wav"
            Just Vocal1 -> "-harm1.wav"
            Just Vocal2 -> "-harm2.wav"
            Just Vocal3 -> "-harm3.wav"
          FL.showWidget picker >>= \case
            FL.NativeFileChooserPicked -> (fmap T.unpack <$> FL.getFilename picker) >>= \case
              Nothing -> return ()
              Just f  -> sink $ EventOnyx $ let
                task = do
                  mid <- stackIO (Load.fromFile input) >>= RBFile.readMIDIFile'
                  let trk = mapTrack (U.applyTempoTrack $ RBFile.s_tempos mid) $ case voc of
                        Nothing     -> RBFile.fixedPartVocals $ RBFile.s_tracks mid
                        Just Vocal1 -> RBFile.fixedHarm1      $ RBFile.s_tracks mid
                        Just Vocal2 -> RBFile.fixedHarm2      $ RBFile.s_tracks mid
                        Just Vocal3 -> RBFile.fixedHarm3      $ RBFile.s_tracks mid
                  src <- toDryVoxFormat <$> fn trk
                  runAudio src f
                  return [f]
                in startTasks [(T.unpack label <> ": " <> input, task)]
            _ -> return ()
  dryvoxButton "Make sine wave dry vox" $ return . sineDryVox
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
  dryvoxButton "Make clipped dry vox" $ \trk -> do
    audio <- stackIO $ pickedAudio
    src <- buildSource' $ Input audio
    return $ clipDryVox (isJust <$> vocalTubes trk) src
  FL.end pack
  FL.setResizable tab $ Just pack
  return ()

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
    FL.setCallback btn $ \_ -> sink $ EventIO $ do
      input <- pickedFile
      sink $ EventOnyx $ let
        task = do
          mid <- stackIO (Load.fromFile input) >>= RBFile.readMIDIFile'
          lg $ closeShiftsFile mid
          return []
        in startTasks [("Pro Keys range check: " <> input, task)]
  padded 5 10 10 10 (Size (Width 800) (Height 35)) $ \rect' -> do
    btn <- FL.buttonNew rect' $ Just "Make REAPER project with RB template"
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
                stackIO $ makeReaperIO [] input input [] f'
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
    , makeTab windowRect "Lipsync dry vox" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageLipsync sink rect tab startTasks
      return tab
    {-
    , makeTab windowRect "RB3 song cache" $ \rect tab -> do
      functionTabColor >>= setTabColor tab
      miscPageSongCache sink rect tab startTasks
      return tab
    -}
    ]
  (startTasks, cancelTasks) <- makeTab windowRect "Task" $ \rect tab -> do
    taskTabColor >>= setTabColor tab
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

watchSong :: IO () -> FilePath -> Onyx (IO (RGDrums.Track Double (D.Gem D.ProType)), IO ())
watchSong notify mid = do
  let loadTrack = do
        song <- case map toLower $ takeExtension mid of
          ".rpp" -> do
            txt <- liftIO $ T.unpack . decodeGeneral <$> B.readFile mid
            RBFile.interpretMIDIFile $ RPP.getMIDI $ RPP.parse $ RPP.scan txt
          _ -> loadMIDI mid
        let tempos = RBFile.s_tempos song
            drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
            drums'
              = Map.fromList
              $ map (first $ realToFrac . U.applyTempoMap tempos)
              $ ATB.toPairList
              $ RTB.toAbsoluteEventList 0
              $ RTB.collectCoincident
              $ fmap RGDrums.Autoplay
              $ D.computePro Nothing drums
        return $ RGDrums.Track drums' Map.empty 0 0.2
  varTrack <- loadTrack >>= liftIO . newIORef
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
            loadTrack >>= liftIO . writeIORef varTrack
            liftIO notify
            go
    go
  return (readIORef varTrack, sendClose)

launchTimeServer
  :: (Event -> IO ())
  -> IORef Double
  -> FL.Ref FL.Input
  -> FL.Ref FL.Button
  -> FL.Ref FL.Box
  -> IO (IO ())
launchTimeServer sink varTime inputPort button label = do
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

data GLStatus = GLPreload | GLLoaded RGDrums.GLStuff | GLClosed

launchPreview :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> FilePath -> Onyx ()
launchPreview sink makeMenuBar mid = do
  (getTrack, stopWatch) <- watchSong (sink $ EventIO FLTK.redraw) mid
  liftIO $ do

    let windowWidth = Width 800
        windowHeight = Height 600
        windowSize = Size windowWidth windowHeight
    window <- FL.windowNew windowSize Nothing $ Just "Onyx Preview"
    menuHeight <- if macOS then return 0 else makeMenuBar windowWidth True
    let (_, windowRect) = chopTop menuHeight $ Rectangle
          (Position (X 0) (Y 0))
          windowSize
        (controlsArea, glArea) = chopTop 50 windowRect
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
    stopServer <- launchTimeServer
      sink
      varTime
      inputPort
      buttonServer
      labelServer

    varStuff <- newMVar GLPreload
    let draw :: FL.Ref FL.GlWindow -> IO ()
        draw wind = do
          mstuff <- modifyMVar varStuff $ \case
            GLPreload -> do
              s <- RGDrums.loadGLStuff
              return (GLLoaded s, Just s)
            loaded@(GLLoaded s) -> return (loaded, Just s)
            GLClosed -> return (GLClosed, Nothing)
          forM_ mstuff $ \stuff -> do
            t <- readIORef varTime
            trk <- getTrack
            Width w <- FL.getW wind
            Height h <- FL.getH wind
            RGDrums.drawDrumsFull stuff (RGDrums.WindowDims w h) trk { RGDrums.trackTime = t }
    glwindow <- FLGL.glWindowCustom
      (rectangleSize glArea)
      (Just $ rectanglePosition glArea)
      Nothing -- label
      (Just draw)
      FL.defaultCustomWidgetFuncs
      FL.defaultCustomWindowFuncs
    FL.end glwindow
    FL.setMode glwindow $ FLE.Modes [FLE.ModeOpenGL3, FLE.ModeDepth, FLE.ModeRGB8, FLE.ModeDouble, FLE.ModeAlpha, FLE.ModeMultisample]

    FL.end window
    FL.setResizable window $ Just glwindow
    FL.setCallback window $ windowCloser $ do
      stopServer
      stopWatch
      modifyMVar_ varStuff $ \x -> do
        case x of
          GLLoaded s -> RGDrums.deleteGLStuff s
          _          -> return ()
        return GLClosed

    FL.showWidget window
  return ()

promptPreview :: (Event -> IO ()) -> (Width -> Bool -> IO Int) -> IO ()
promptPreview sink makeMenuBar = sink $ EventIO $ do
  picker <- FL.nativeFileChooserNew $ Just FL.BrowseFile
  FL.setTitle picker "Load MIDI or REAPER project"
  FL.setFilter picker "*.{mid,midi,RPP}" -- TODO also handle .chart?
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
    taskTabColor >>= setTabColor tab
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

isNewestRelease :: (Bool -> IO ()) -> IO ()
isNewestRelease cb = do
  let addr = Req.https "api.github.com" /: "repos" /: "mtolly" /: "onyxite-customs" /: "releases" /: "latest"
  rsp <- Req.runReq Req.defaultHttpConfig $ Req.req Req.GET addr Req.NoReqBody Req.jsonResponse $ Req.header "User-Agent" "mtolly/onyxite-customs"
  case Req.responseBody rsp of
    A.Object obj -> case HM.lookup "name" obj of
      Just (A.String str) -> cb $ T.unpack str == showVersion version
      _                   -> return ()
    _            -> return ()

#ifdef WINDOWS
foreign import ccall "&fl_display" fl_display :: Ptr HINSTANCE
foreign import ccall "Fl_Window_set_icon"
  setIconRaw :: Ptr () -> Ptr () -> IO ()
#endif

launchGUI :: IO ()
launchGUI = do
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
#ifdef WINDOWS
  peek fl_display >>= \disp -> do
    icon <- loadIcon (Just disp) $ intPtrToPtr 1
    FL.withRef termWindow $ \ptr ->
      setIconRaw ptr icon
#endif
  FL.sizeRange termWindow $ Size consoleWidth consoleHeight
  globalLogColor >>= FL.setColor termWindow
  let makeMenuBar width includeConsole = do
        let menuHeight = if macOS then 0 else 30
            menuRect = Rectangle (Position (X 0) (Y 0)) (Size width (Height menuHeight))
            menuFn :: IO () -> FL.Ref FL.MenuItem -> IO ()
            menuFn = const
            menuOptions =
              [ ( "File/Batch Process"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'b'
                , Just $ launchBatch sink makeMenuBar []
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Tools"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 't'
                , Just $ launchMisc sink makeMenuBar
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Live Preview"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'p'
                , Just $ promptPreview sink makeMenuBar
                , FL.MenuItemFlags [FL.MenuItemNormal]
                )
              , ( "File/Close Window"
                , Just $ FL.KeySequence $ FL.ShortcutKeySequence [FLE.kb_CommandState] $ FL.NormalKeyType 'w'
                , Just $ sink $ EventIO $ FLTK.firstWindow >>= \case
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
            forM_ menuOptions $ \(a, b, c, d) -> do
              FL.add menu a b (fmap menuFn c) d
          else do
            menu <- FL.menuBarNew menuRect Nothing
            forM_ menuOptions $ \(a, b, c, d) -> do
              FL.add menu a b (fmap menuFn c) d
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
  _ <- forkIO $ isNewestRelease $ \b -> sink $ EventIO $ do
    FL.setLabel labelLatest $ if b
      then "You are using the latest version."
      else "New version available!"
    unless b $ FL.setLabelcolor labelLatest FLE.whiteColor
  term <- FL.simpleTerminalNew
    (Rectangle
      (Position (X 10) (Y $ 45 + menuHeight))
      (Size (Width 480) (Height $ 305 - menuHeight))
    )
    Nothing
  FL.setHistoryLines term $ FL.Lines (-1) -- unlimited
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
  buttonBatch <- FL.buttonCustom
    (Rectangle (Position (X 10) (Y 360)) (Size (Width 235) (Height 30)))
    (Just "Batch process")
    Nothing
    $ Just $ FL.defaultCustomWidgetFuncs
      { FL.handleCustom = Just $ dragAndDrop (launchBatch sink makeMenuBar) . FL.handleButtonBase . FL.safeCast
      }
  batchProcessColor >>= FL.setColor buttonBatch
  FL.setLabelsize buttonBatch (FL.FontSize 13)
  FL.setCallback buttonBatch $ \_ -> launchBatch sink makeMenuBar []
  buttonMisc <- FL.buttonNew
    (Rectangle (Position (X 255) (Y 360)) (Size (Width 235) (Height 30)))
    (Just "Other tools")
  loadSongColor >>= FL.setColor buttonMisc
  FL.setLabelsize buttonMisc (FL.FontSize 13)
  FL.setCallback buttonMisc $ \_ -> launchMisc sink makeMenuBar
  FL.end termWindow
  FL.setResizable termWindow $ Just term
  FL.setCallback termWindow $ windowCloser $ return ()
  FL.showWidget termWindow

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
          EventLoaded proj -> liftIO $ launchWindow sink proj
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
