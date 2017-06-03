{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TupleSections            #-}
module GUI where

import           CommandLine                    (commandLine, useResultFile)
import           Control.Concurrent             (ThreadId, forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, bracket_)
import           Control.Monad.Extra
import           Control.Monad.Trans.StackTrace (StackTraceT, runStackTraceT)
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
  = Choices [Choice]
  | Files FilePicker [FilePath] ([FilePath] -> Menu)
  | TasksStart [StackTraceT IO [FilePath]]
  | TasksRunning ThreadId TasksStatus
  | TasksDone TasksStatus

data Choice = Choice
  { choiceTitle       :: T.Text
  , choiceDescription :: T.Text
  , choiceMenu        :: Menu
  }

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

data GUIState = InMenu Menu [Menu] Selection

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

    initialState = InMenu topMenu [] NoSelect

    simpleText offset txt = do
      bracket (TTF.renderUTF8Blended penta (T.unpack txt) $ Color 0xEE 0xEE 0xEE 255) SDL.freeSurface $ \surf -> do
        dims <- SDL.surfaceDimensions surf
        bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
          SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) 10)) dims

    draw guiState = do
      SDL.V2 windW windH <- SDL.get $ SDL.windowSize window
      SDL.rendererDrawColor rend $= purple 1
      SDL.clear rend
      SDL.copy rend texBrand Nothing $ Just $ SDL.Rectangle
        (SDL.P (SDL.V2 (windW - brandW - 10) (windH - brandH - 10)))
        dimsBrand
      case guiState of
        InMenu menu prevMenus sel -> do
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
            Choices cs -> do
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

    tick guiState = do
      draw guiState
      SDL.present rend
      threadDelay 10000
      evts <- SDL.pollEvents
      processEvents evts guiState

    newSelect mousePos (InMenu menu prevMenus _) = InMenu menu prevMenus $ case mousePos of
      Nothing -> SelectMenu 0
      Just (x, y) -> let
        offset = fromIntegral $ length prevMenus * 25
        in if offset > x
          then let
            dropPrev = fromIntegral $ quot (offset - x) 25
            in SelectPage dropPrev
          else case menu of
            Choices choices -> let
              i = max 0 $ min (length choices - 1) $ quot (fromIntegral y - 10) 70
              in SelectMenu i -- TODO noselect
            _ -> SelectMenu 0 -- TODO noselect

    doSelect es guiState mousePos = case guiState of
      InMenu menu prevMenus sel -> case sel of
        NoSelect -> processEvents es $ newSelect mousePos guiState
        SelectPage i -> case drop i prevMenus of
          pm : pms -> do
            case menu of
              TasksRunning tid _ -> killThread tid
              _                  -> return ()
            processEvents es $ newSelect mousePos $ InMenu pm pms $ NoSelect
          [] -> processEvents es $ newSelect mousePos guiState
        SelectMenu i -> case menu of
          TasksDone{} -> processEvents es $ newSelect mousePos initialState
          TasksRunning{} -> processEvents es $ newSelect mousePos guiState
          Choices choices -> processEvents es $ newSelect mousePos $
            InMenu (choiceMenu $ choices !! i) (menu : prevMenus) NoSelect
          Files fpick loaded useFiles -> if null loaded
            then do
              let pats = if os /= "darwin"
                    then filePatterns fpick
                    else if all ("*." `T.isPrefixOf`) $ filePatterns fpick
                      then filePatterns fpick
                      else []
              void $ forkIO $ openFileDialog "" "" pats (fileDescription fpick) True >>= \case
                Just files -> putMVar varSelectedFile $ map T.unpack files
                _ -> return ()
              processEvents es $ newSelect mousePos guiState
            else processEvents es $ newSelect mousePos $ InMenu (useFiles loaded) (menu : prevMenus) NoSelect
          TasksStart tasks -> do
            tid <- forkIO $ forM_ tasks $ \task -> do
              result <- capture $ runStackTraceT task
              putMVar varTaskComplete result
            processEvents es $ newSelect mousePos $ InMenu (TasksRunning tid $ TasksStatus (length tasks) 0 0 []) (menu : prevMenus) sel

    processEvents [] guiState = checkVars guiState
    processEvents (e : es) guiState = case SDL.eventPayload e of
      SDL.QuitEvent -> return ()
      SDL.DropEvent (SDL.DropEventData cstr) -> do
        str <- peekCString cstr
        void $ forkIO $ putMVar varSelectedFile [str]
        processEvents es guiState
      SDL.MouseMotionEvent (SDL.MouseMotionEventData
        { SDL.mouseMotionEventPos = SDL.P (SDL.V2 x y)
        }) -> processEvents es $ newSelect (Just (x, y)) guiState
      SDL.MouseButtonEvent (SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Pressed
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y)
        }) -> doSelect es guiState (Just (x, y))
      SDL.KeyboardEvent (SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = ksym
        , SDL.keyboardEventRepeat = False
        }) -> case SDL.keysymScancode ksym of
          SDL.ScancodeBackspace -> case guiState of
            InMenu menu (pm : pms) _ -> do
              case menu of
                TasksRunning tid _ -> killThread tid
                _                  -> return ()
              processEvents es $ InMenu pm pms $ SelectMenu 0
            _ -> processEvents es guiState
          SDL.ScancodeReturn -> doSelect es guiState Nothing
          SDL.ScancodeLeft -> processEvents es $ case guiState of
            InMenu menu prevMenus sel -> InMenu menu prevMenus $ case sel of
              SelectMenu _ -> if null prevMenus then sel else SelectPage 0
              SelectPage i -> SelectPage $ min (length prevMenus - 1) $ i + 1
              NoSelect     -> SelectMenu 0
          SDL.ScancodeRight -> processEvents es $ case guiState of
            InMenu menu prevMenus sel -> InMenu menu prevMenus $ case sel of
              SelectMenu _ -> sel
              SelectPage 0 -> SelectMenu 0
              SelectPage i -> SelectPage $ i - 1
              NoSelect     -> SelectMenu 0
          SDL.ScancodeDown -> processEvents es $ case guiState of
            InMenu menu@(Choices choices) prevMenus (SelectMenu i) ->
              InMenu menu prevMenus $ SelectMenu $ min (length choices - 1) $ i + 1
            InMenu menu prevMenus NoSelect -> InMenu menu prevMenus $ SelectMenu 0
            _ -> guiState
          SDL.ScancodeUp -> processEvents es $ case guiState of
            InMenu menu prevMenus (SelectMenu i) ->
              InMenu menu prevMenus $ SelectMenu $ max 0 $ i - 1
            InMenu menu prevMenus NoSelect -> InMenu menu prevMenus $ SelectMenu 0
            _ -> guiState
          _ -> processEvents es guiState
      _ -> processEvents es guiState

    checkVars s0 = do
      s1 <- tryTakeMVar varSelectedFile >>= return . \case
        Nothing -> s0
        Just fps -> case s0 of
          InMenu (Files fpick files useFiles) prevMenus sel ->
            InMenu (Files fpick (files ++ fps) useFiles) prevMenus sel
          _ -> s0
      s2 <- tryTakeMVar varTaskComplete >>= \case
        Nothing -> return s1
        Just (_strOut, (res, _warns)) -> case s1 of
          InMenu (TasksRunning tid (TasksStatus total good bad files)) prevMenus sel -> let
            (good', bad', addFiles) = case res of
              Right newFiles -> (good + 1, bad, newFiles)
              Left _err      -> (good, bad + 1, [])
            files' = files ++ addFiles
            in if good' + bad' == total
              then do
                case files' of
                  [f] -> useResultFile f
                  _   -> return ()
                return $ InMenu (TasksDone (TasksStatus total good' bad' files')) prevMenus sel
              else return $ InMenu (TasksRunning tid (TasksStatus total good' bad' files')) prevMenus sel
          _ -> return s1
      tick s2

  tick initialState
