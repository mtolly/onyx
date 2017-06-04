{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}
module GUI (launchGUI) where

import           CommandLine                    (commandLine, useResultFile)
import           Control.Concurrent             (ThreadId, forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, bracket_)
import           Control.Monad.Extra
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace (StackTraceT, runStackTraceT)
import           Control.Monad.Trans.State
import qualified Data.ByteString                as B
import           Data.ByteString.Unsafe         (unsafeUseAsCStringLen)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Word                      (Word8)
import           Foreign                        (Ptr, castPtr)
import           Foreign.C                      (CInt (..), peekCString)
import           Resources                      (pentatonicTTF)
import           SDL                            (($=))
import qualified SDL
import           SDL.Raw                        (Color (..), RWops,
                                                 rwFromConstMem)
import qualified SDL.TTF                        as TTF
import           SDL.TTF.FFI                    (TTFFont)
import           System.Info                    (os)
import           System.IO.Silently             (capture)
import           TinyFileDialogs

foreign import ccall unsafe "TTF_OpenFontRW"
  openFontRW :: Ptr RWops -> CInt -> CInt -> IO TTFFont

withBSFont :: B.ByteString -> Int -> (TTFFont -> IO a) -> IO a
withBSFont bs pts act = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
  rw  <- rwFromConstMem (castPtr ptr) (fromIntegral len)
  bracket (openFontRW rw 1 $ fromIntegral pts) TTF.closeFont act

data Selection
  = NoSelect
  | SelectMenu Int -- SelectMenu 0 means the top option in the menu
  | SelectPage Int -- SelectPage 0 means go back one page, 1 means 2 pages, etc.
  deriving (Eq, Ord, Show, Read)

data Menu
  = Choices [Choice Menu]
  | Files FilePicker [FilePath] ([FilePath] -> Menu)
  | TasksStart [StackTraceT IO [FilePath]]
  | TasksRunning ThreadId TasksStatus
  | TasksDone TasksStatus

data Choice a = Choice
  { choiceTitle       :: T.Text
  , choiceDescription :: T.Text
  , choiceMenu        :: a
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data FilePicker = FilePicker
  { filePatterns    :: [T.Text]
  , fileDescription :: T.Text
  } deriving (Eq, Ord, Show, Read)

data TasksStatus = TasksStatus
  { tasksTotal  :: Int
  , tasksOK     :: Int
  , tasksFailed :: Int
  , tasksFiles  :: [FilePath]
  }

topMenu :: Menu
topMenu = Choices
  [ ( Choice "PS to RB3" "Attempts to convert a Frets on Fire/Phase Shift song to Rock Band 3."
    $ Files (FilePicker ["*.ini"] "Frets on Fire/Phase Shift song.ini") [] $ \fs ->
      TasksStart $ map (\f -> commandLine ["convert", "--game", "rb3", f]) fs
    )
  , ( Choice "RB3 to RB2" "Converts a song from Rock Band 3 to Rock Band 2."
    $ Files (FilePicker ["*_rb3con"] "Rock Band 3 CON file") [] $ \fs -> Choices
      [ ( Choice "No keys" "Drops the Keys part if present."
        $ TasksStart $ map (\f -> commandLine ["convert", "--game", "rb2", f]) fs
        )
      , ( Choice "Keys on guitar" "Drops Guitar if present, and puts Keys on Guitar (like RB3 keytar mode)."
        $ TasksStart $ map (\f -> commandLine ["convert", "--game", "rb2", f, "--keys-on-guitar"]) fs
        )
      , ( Choice "Keys on bass" "Drops Bass if present, and puts Keys on Bass (like RB3 keytar mode)."
        $ TasksStart $ map (\f -> commandLine ["convert", "--game", "rb2", f, "--keys-on-bass"]) fs
        )
      ]
    )
  , ( Choice "Web preview" "Produces a web browser app to preview a song."
    $ Files (FilePicker ["*_rb3con", "*_rb2con", "*.rba", "*.ini"] "Song (RB3/RB2/PS)") [] $ \fs ->
      TasksStart $ map (\f -> commandLine ["player", f]) fs
    )
  , ( Choice "REAPER project" "Converts a MIDI or song (RB3/RB2/PS) to a REAPER project."
    $ Files (FilePicker ["*_rb3con", "*_rb2con", "*.rba", "*.ini", "*.mid"] "Song (RB3/RB2/PS) or MIDI file") [] $ \fs ->
      TasksStart $ map (\f -> commandLine ["reap", f]) fs
    )
  ]

data GUIState = GUIState
  { currentScreen    :: Menu
  , previousScreens  :: [Menu]
  , currentSelection :: Selection
  }

type Onyx = StateT GUIState IO

launchGUI :: IO ()
launchGUI = do

  bracket_ SDL.initializeAll SDL.quit $ do
  TTF.withInit $ do
  withBSFont pentatonicTTF 40 $ \penta -> do
  withBSFont pentatonicTTF 20 $ \pentaSmall -> do
  let windowConf = SDL.defaultWindow
        { SDL.windowResizable = True
        , SDL.windowHighDPI = False
        , SDL.windowInitialSize = SDL.V2 800 600
        }
  bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
  SDL.windowMinimumSize window $= SDL.V2 800 600
  bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do

  let purple :: Double -> SDL.V4 Word8
      purple frac = SDL.V4 (floor $ 0x4B * frac) (floor $ 0x1C * frac) (floor $ 0x4E * frac) 0xFF
      v4ToColor :: SDL.V4 Word8 -> Color
      v4ToColor (SDL.V4 r g b a) = Color r g b a

  varSelectedFile <- newEmptyMVar
  varTaskComplete <- newEmptyMVar

  bracket (TTF.renderUTF8Blended penta "ONYX" $ v4ToColor $ purple 0.5) SDL.freeSurface $ \surfBrand -> do
  bracket (SDL.createTextureFromSurface rend surfBrand) SDL.destroyTexture $ \texBrand -> do
  dimsBrand@(SDL.V2 brandW brandH) <- SDL.surfaceDimensions surfBrand

  let

    initialState = GUIState topMenu [] NoSelect

    simpleText offset txt = liftIO $ do
      bracket (TTF.renderUTF8Blended penta (T.unpack txt) $ Color 0xEE 0xEE 0xEE 255) SDL.freeSurface $ \surf -> do
        dims <- SDL.surfaceDimensions surf
        bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
          SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) 10)) dims

    -- TODO
    getChoices :: GUIState -> [Choice ()]
    getChoices (GUIState menu _ _) = case menu of
      Choices cs -> map void cs
      Files fpick files _ ->
        [ Choice "Select files..." (fileDescription fpick) ()
        , Choice "Clear selection" "" ()
        ] ++ if null files then [] else let
          desc = case length files of
            1 -> "1 file loaded"
            n -> T.pack (show n) <> " files loaded"
            in [Choice "Continue" desc ()]
      TasksStart _ -> [Choice "Go!" "" ()]
      TasksRunning _ _ -> [Choice "Running..." "" ()] -- TODO
      TasksDone (TasksStatus _ ok failed _) ->
        [ let
          desc = T.pack (show ok) <> " succeeded, " <> T.pack (show failed) <> " failed"
          in Choice "Tasks finished" desc ()
        , Choice "View logs" "" ()
        , Choice "Main menu" "" ()
        ]

    draw :: Onyx ()
    draw = do
      guiState <- get
      SDL.V2 windW windH <- SDL.get $ SDL.windowSize window
      SDL.rendererDrawColor rend $= purple 1
      SDL.clear rend
      SDL.copy rend texBrand Nothing $ Just $ SDL.Rectangle
        (SDL.P (SDL.V2 (windW - brandW - 10) (windH - brandH - 10)))
        dimsBrand
      case guiState of
        GUIState menu prevMenus sel -> do
          let offset = fromIntegral $ length prevMenus * 25
          forM_ (zip [0..] $ zip [0.88, 0.76 ..] [offset - 25, offset - 50 .. 0]) $ \(i, (frac, x)) -> do
            SDL.rendererDrawColor rend $= if sel == SelectPage i
              then purple 1.8
              else purple frac
            SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x 0) $ SDL.V2 25 windH
            SDL.rendererDrawColor rend $= SDL.V4 0x11 0x11 0x11 0xFF
            SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x + 24) 0) $ SDL.V2 1 windH
          case menu of
            TasksRunning _ (TasksStatus total good bad _) -> simpleText offset $
              "Running " <> T.pack (show total) <> " tasks... " <> T.pack (show good) <> " ok, " <> T.pack (show bad) <> " failed"
            TasksDone (TasksStatus _ good bad _) -> simpleText offset $ T.pack (show good) <> " ok, " <> T.pack (show bad) <> " failed"
            TasksStart _ -> simpleText offset "Go!"
            Files _ files _ -> simpleText offset $ case length files of
              0 -> "Select files..."
              1 -> "1 file loaded"
              n -> T.pack (show n) <> " files loaded"
            Choices cs -> liftIO $ do
              let choices = zip [ (i, sel == SelectMenu i) | i <- [0..] ] cs
              forM_ choices $ \((index, selected), choice) -> do
                let color = if selected then Color 0xEE 0xEE 0xEE 255 else Color 0x80 0x54 0x82 255
                bracket (TTF.renderUTF8Blended penta (T.unpack $ choiceTitle choice) color) SDL.freeSurface $ \surf -> do
                  dims <- SDL.surfaceDimensions surf
                  bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
                    SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 10))) dims
                bracket (TTF.renderUTF8Blended pentaSmall (T.unpack $ choiceDescription choice) color) SDL.freeSurface $ \surf -> do
                  dims <- SDL.surfaceDimensions surf
                  bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
                    SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 50))) dims

    tick :: Onyx ()
    tick = do
      draw
      SDL.present rend
      liftIO $ threadDelay 10000
      evts <- SDL.pollEvents
      processEvents evts

    newSelect :: Maybe (Int, Int) -> Onyx ()
    newSelect mousePos = modify $ \(GUIState menu prevMenus _) ->
      GUIState menu prevMenus $ case mousePos of
        Nothing -> SelectMenu 0
        Just (x, y) -> let
          offset = length prevMenus * 25
          in if offset > x
            then let
              dropPrev = quot (offset - x) 25
              in SelectPage dropPrev
            else let
              numIndexes = case menu of
                Choices choices -> length choices
                _               -> 1
              i = div (y - 10) 70
              in if 0 <= i && i < numIndexes
                then SelectMenu i
                else NoSelect

    doSelect :: Maybe (Int, Int) -> Onyx ()
    doSelect mousePos = do
      GUIState menu prevMenus sel <- get
      case sel of
        NoSelect -> return ()
        SelectPage i -> case drop i prevMenus of
          pm : pms -> do
            case menu of
              TasksRunning tid _ -> liftIO $ killThread tid
              _                  -> return ()
            put $ GUIState pm pms NoSelect
          [] -> return ()
        SelectMenu i -> case menu of
          TasksDone{} -> do
            put initialState
          TasksRunning{} -> return ()
          Choices choices -> do
            put $ GUIState (choiceMenu $ choices !! i) (menu : prevMenus) NoSelect
          Files fpick loaded useFiles -> if null loaded
            then do
              let pats = if os /= "darwin"
                    then filePatterns fpick
                    else if all ("*." `T.isPrefixOf`) $ filePatterns fpick
                      then filePatterns fpick
                      else []
              liftIO $ void $ forkIO $ openFileDialog "" "" pats (fileDescription fpick) True >>= \case
                Just files -> putMVar varSelectedFile $ map T.unpack files
                _ -> return ()
            else do
              put $ GUIState (useFiles loaded) (menu : prevMenus) NoSelect
          TasksStart tasks -> do
            tid <- liftIO $ forkIO $ forM_ tasks $ \task -> do
              result <- capture $ runStackTraceT task
              putMVar varTaskComplete result
            put $ GUIState (TasksRunning tid $ TasksStatus (length tasks) 0 0 []) (menu : prevMenus) sel
      newSelect mousePos

    processEvents :: [SDL.Event] -> Onyx ()
    processEvents [] = checkVars
    processEvents (e : es) = case SDL.eventPayload e of
      SDL.QuitEvent -> return ()
      SDL.DropEvent (SDL.DropEventData cstr) -> do
        liftIO $ do
          str <- peekCString cstr
          void $ forkIO $ putMVar varSelectedFile [str]
        processEvents es
      SDL.MouseMotionEvent (SDL.MouseMotionEventData
        { SDL.mouseMotionEventPos = SDL.P (SDL.V2 x y)
        }) -> do
          newSelect $ Just (fromIntegral x, fromIntegral y)
          processEvents es
      SDL.MouseButtonEvent (SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Pressed
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y)
        }) -> do
          doSelect $ Just (fromIntegral x, fromIntegral y)
          processEvents es
      SDL.KeyboardEvent (SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = ksym
        , SDL.keyboardEventRepeat = False
        }) -> case SDL.keysymScancode ksym of
          SDL.ScancodeBackspace -> get >>= \case
            GUIState menu (pm : pms) _ -> do
              case menu of
                TasksRunning tid _ -> liftIO $ killThread tid
                _                  -> return ()
              put $ GUIState pm pms $ SelectMenu 0
              processEvents es
            _ -> processEvents es
          SDL.ScancodeReturn -> do
            doSelect Nothing
            processEvents es
          SDL.ScancodeLeft -> do
            modify $ \(GUIState menu prevMenus sel) -> GUIState menu prevMenus $ case sel of
                SelectMenu _ -> if null prevMenus then sel else SelectPage 0
                SelectPage i -> SelectPage $ min (length prevMenus - 1) $ i + 1
                NoSelect     -> SelectMenu 0
            processEvents es
          SDL.ScancodeRight -> do
            modify $ \(GUIState menu prevMenus sel) -> GUIState menu prevMenus $ case sel of
              SelectMenu _ -> sel
              SelectPage 0 -> SelectMenu 0
              SelectPage i -> SelectPage $ i - 1
              NoSelect     -> SelectMenu 0
            processEvents es
          SDL.ScancodeDown -> do
            modify $ \case
              GUIState menu@(Choices choices) prevMenus (SelectMenu i) ->
                GUIState menu prevMenus $ SelectMenu $ min (length choices - 1) $ i + 1
              GUIState menu prevMenus NoSelect -> GUIState menu prevMenus $ SelectMenu 0
              s -> s
            processEvents es
          SDL.ScancodeUp -> do
            modify $ \case
              GUIState menu prevMenus (SelectMenu i) ->
                GUIState menu prevMenus $ SelectMenu $ max 0 $ i - 1
              GUIState menu prevMenus NoSelect -> GUIState menu prevMenus $ SelectMenu 0
              s -> s
            processEvents es
          _ -> processEvents es
      _ -> processEvents es

    checkVars :: Onyx ()
    checkVars = do
      liftIO (tryTakeMVar varSelectedFile) >>= \case
        Nothing -> return ()
        Just fps -> modify $ \case
          GUIState (Files fpick files useFiles) prevMenus sel ->
            GUIState (Files fpick (files ++ fps) useFiles) prevMenus sel
          s -> s
      liftIO (tryTakeMVar varTaskComplete) >>= \case
        Nothing -> return ()
        Just (_strOut, (res, _warns)) -> get >>= \case
          GUIState (TasksRunning tid (TasksStatus total good bad files)) prevMenus sel -> let
            (good', bad', addFiles) = case res of
              Right newFiles -> (good + 1, bad, newFiles)
              Left _err      -> (good, bad + 1, [])
            files' = files ++ addFiles
            in if good' + bad' == total
              then do
                case files' of
                  [f] -> useResultFile f
                  _   -> return ()
                put $ GUIState (TasksDone (TasksStatus total good' bad' files')) prevMenus sel
              else put $ GUIState (TasksRunning tid (TasksStatus total good' bad' files')) prevMenus sel
          _ -> return ()
      tick

  evalStateT tick initialState
