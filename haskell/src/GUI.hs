{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TupleSections            #-}
module GUI (launchGUI) where

import           CommandLine                    (commandLine)
import           Control.Concurrent             (ThreadId, forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, bracket_,
                                                 displayException)
import           Control.Monad.Extra
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.StackTrace (Messages (..), StackTraceT,
                                                 runStackTraceT)
import           Control.Monad.Trans.State
import qualified Data.ByteString                as B
import           Data.ByteString.Unsafe         (unsafeUseAsCStringLen)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Time
import           Data.Word                      (Word8)
import           Foreign                        (Ptr, castPtr)
import           Foreign.C                      (CInt (..), peekCString)
import           Graphics.UI.TinyFileDialogs
import           OSFiles                        (osOpenFile, useResultFiles)
import           Resources                      (pentatonicTTF)
import           SDL                            (($=))
import qualified SDL
import           SDL.Raw                        (Color (..), RWops,
                                                 rwFromConstMem)
import qualified SDL.TTF                        as TTF
import           SDL.TTF.FFI                    (TTFFont)
import           System.Directory               (XdgDirectory (..),
                                                 createDirectoryIfMissing,
                                                 getXdgDirectory)
import           System.Environment             (getEnv)
import           System.FilePath                ((<.>), (</>))
import           System.Info                    (os)
import           System.IO.Silently             (capture)

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
  | TasksDone FilePath TasksStatus

data Choice a = Choice
  { choiceTitle       :: T.Text
  , choiceDescription :: T.Text
  , choiceValue       :: a
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data FilePicker = FilePicker
  { filePatterns    :: [T.Text]
  , fileDescription :: T.Text
  } deriving (Eq, Ord, Show, Read)

data TasksStatus = TasksStatus
  { tasksTotal  :: Int
  , tasksOK     :: Int
  , tasksFailed :: Int
  , tasksOutput :: [(String, Maybe [FilePath])]
  } deriving (Eq, Ord, Show, Read)

commandLine' :: (MonadIO m) => [String] -> StackTraceT m [FilePath]
commandLine' args = do
  let args' = flip map args $ \arg -> if ' ' `elem` arg then "\"" ++ arg ++ "\"" else arg
  liftIO $ mapM_ putStrLn
    [ ""
    , ">>> Command: " ++ unwords args'
    , ""
    ]
  commandLine args

topMenu :: Menu
topMenu = Choices
  [ ( Choice "PS to RB3" "Attempts to convert a Frets on Fire/Phase Shift song to Rock Band 3."
    $ Files (FilePicker ["*.ini"] "Frets on Fire/Phase Shift song.ini") [] $ \fs ->
      TasksStart $ map (\f -> commandLine' ["convert", "--game", "rb3", f]) fs
    )
  , ( Choice "RB3 to RB2" "Converts a song from Rock Band 3 to Rock Band 2."
    $ Files (FilePicker ["*_rb3con"] "Rock Band 3 CON files") [] $ \fs -> Choices
      [ ( Choice "No keys" "Drops the Keys part if present."
        $ TasksStart $ map (\f -> commandLine' ["convert", "--game", "rb2", f]) fs
        )
      , ( Choice "Keys on guitar" "Drops Guitar if present, and puts Keys on Guitar (like RB3 keytar mode)."
        $ TasksStart $ map (\f -> commandLine' ["convert", "--game", "rb2", f, "--keys-on-guitar"]) fs
        )
      , ( Choice "Keys on bass" "Drops Bass if present, and puts Keys on Bass (like RB3 keytar mode)."
        $ TasksStart $ map (\f -> commandLine' ["convert", "--game", "rb2", f, "--keys-on-bass"]) fs
        )
      ]
    )
  , ( Choice "Web preview" "Produces a web browser app to preview a song."
    $ Files (FilePicker ["*_rb3con", "*_rb2con", "*.rba", "*.ini"] "Songs (RB3/RB2/PS)") [] $ \fs ->
      TasksStart $ map (\f -> commandLine' ["player", f]) fs
    )
  , ( Choice "REAPER project" "Converts a MIDI or song (RB3/RB2/PS) to a REAPER project."
    $ Files (FilePicker ["*_rb3con", "*_rb2con", "*.rba", "*.ini", "*.mid"] "Songs (RB3/RB2/PS) or MIDI files") [] $ \fs ->
      TasksStart $ map (\f -> commandLine' ["reap", f]) fs
    )
  , ( Choice "Auto reductions" "Fills empty difficulties in a MIDI file with CAT-quality reductions."
    $ Files (FilePicker ["*.mid"] "MIDI files") [] $ \fs ->
      TasksStart $ map (\f -> commandLine' ["reduce", f]) fs
    )
  ]

data GUIState = GUIState
  { currentScreen    :: Menu
  , previousScreens  :: [Menu] -- TODO should add Selection
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
  bracket (SDL.createWindow "Onyx Music Game Toolkit" windowConf) SDL.destroyWindow $ \window -> do
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

    setMenu :: Menu -> Onyx ()
    setMenu menu = modify $ \GUIState{..} -> GUIState{ currentScreen = menu, .. }

    pushMenu :: Menu -> Onyx ()
    pushMenu menu = modify $ \(GUIState m pms sel) -> GUIState menu (m : pms) sel

    popMenu :: Onyx ()
    popMenu = modify $ \case
      GUIState _ (m : pms) sel -> GUIState m pms sel
      s                        -> s

    modifySelect :: (Selection -> Selection) -> Onyx ()
    modifySelect f = modify $ \(GUIState m pms sel) -> GUIState m pms $ f sel

    getChoices :: Onyx [Choice (Onyx ())]
    getChoices = gets $ \(GUIState menu _ _) -> case menu of
      Choices cs -> flip map cs $ fmap pushMenu
      Files fpick files useFiles ->
        [ Choice "Select files... (or drag and drop)" (fileDescription fpick) $ do
          let pats = if os /= "darwin"
                then filePatterns fpick
                else if all ("*." `T.isPrefixOf`) $ filePatterns fpick
                  then filePatterns fpick
                  else []
          liftIO $ void $ forkIO $ openFileDialog "" "" pats (fileDescription fpick) True >>= \case
            Just chosen -> putMVar varSelectedFile $ map T.unpack chosen
            _           -> return ()
        ] ++ if null files then [] else let
          desc = case length files of
            1 -> "1 file loaded"
            n -> T.pack (show n) <> " files loaded"
          in  [ Choice "Continue" desc $ pushMenu $ useFiles files
              , Choice "Clear selection" "" $ setMenu $ Files fpick [] useFiles
              ]
      TasksStart tasks -> let
        go = do
          tid <- liftIO $ forkIO $ forM_ tasks $ \task -> do
            result <- capture $ runStackTraceT task
            putMVar varTaskComplete result
          setMenu $ TasksRunning tid TasksStatus
            { tasksTotal = length tasks
            , tasksOK = 0
            , tasksFailed = 0
            , tasksOutput = []
            }
        in [Choice "Go!" "" go]
      TasksRunning tid TasksStatus{..} ->
        [ let
          desc = T.pack (show tasksOK) <> " succeeded, " <> T.pack (show tasksFailed) <> " failed"
          in Choice "Running..." desc $ return ()
        , Choice "Cancel" "" $ do
          liftIO $ killThread tid
          popMenu
        ]
      TasksDone logFile TasksStatus{..} ->
        [ let
          desc = T.pack (show tasksOK) <> " succeeded, " <> T.pack (show tasksFailed) <> " failed"
          in Choice "Tasks finished" desc $ return ()
        , Choice "View log" "" $ osOpenFile logFile
        , Choice "Main menu" "" $ put initialState
        ]

    draw :: Onyx ()
    draw = do
      SDL.V2 windW windH <- SDL.get $ SDL.windowSize window
      SDL.rendererDrawColor rend $= purple 1
      SDL.clear rend
      SDL.copy rend texBrand Nothing $ Just $ SDL.Rectangle
        (SDL.P (SDL.V2 (windW - brandW - 10) (windH - brandH - 10)))
        dimsBrand
      GUIState{..} <- get
      let offset = fromIntegral $ length previousScreens * 25
      forM_ (zip [0..] $ zip [0.88, 0.76 ..] [offset - 25, offset - 50 .. 0]) $ \(i, (frac, x)) -> do
        SDL.rendererDrawColor rend $= if currentSelection == SelectPage i
          then purple 1.8
          else purple frac
        SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x 0) $ SDL.V2 25 windH
        SDL.rendererDrawColor rend $= SDL.V4 0x11 0x11 0x11 0xFF
        SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 (x + 24) 0) $ SDL.V2 1 windH
      choices <- getChoices
      forM_ (zip [0..] choices) $ \(index, choice) -> liftIO $ do
        let selected = currentSelection == SelectMenu index
            color = if selected then Color 0xEE 0xEE 0xEE 255 else Color 0x80 0x54 0x82 255
        bracket (TTF.renderUTF8Blended penta (T.unpack $ choiceTitle choice) color) SDL.freeSurface $ \surf -> do
          dims <- SDL.surfaceDimensions surf
          bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
            SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 10))) dims
        case T.unpack $ choiceDescription choice of
          ""  -> return () -- otherwise sdl2_ttf returns null surface
          str -> bracket (TTF.renderUTF8Blended pentaSmall str color) SDL.freeSurface $ \surf -> do
            dims <- SDL.surfaceDimensions surf
            bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
              SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 50))) dims
      case currentScreen of
        TasksRunning{} -> do
          -- square spinner animation
          t <- (`rem` 2000) <$> SDL.ticks
          let smallSide = 50
              bigX = quot windW 2 - smallSide
              bigY = quot windH 2 - smallSide
              bigSide = smallSide * 2
              smallRect = SDL.V2 smallSide smallSide
              smallDraw x y = SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x y) smallRect
          SDL.rendererDrawColor rend $= purple 0.5
          SDL.fillRect rend $ Just $ SDL.Rectangle
            (SDL.P $ SDL.V2 bigX bigY)
            (SDL.V2 bigSide bigSide)
          SDL.rendererDrawColor rend $= SDL.V4 0xCC 0x8E 0xD1 0xFF
          if  | t < 250 -> do
                smallDraw bigX bigY
                smallDraw (bigX + smallSide) (bigY + smallSide)
              | t < 1000 -> do
                let moved = floor ((fromIntegral (t - 250) / 750) * fromIntegral smallSide :: Double)
                smallDraw (bigX + moved) bigY
                smallDraw (bigX + smallSide - moved) (bigY + smallSide)
              | t < 1250 -> do
                smallDraw (bigX + smallSide) bigY
                smallDraw bigX (bigY + smallSide)
              | otherwise -> do
                let moved = floor ((fromIntegral (t - 1250) / 750) * fromIntegral smallSide :: Double)
                smallDraw bigX (bigY + smallSide - moved)
                smallDraw (bigX + smallSide) (bigY + moved)
        _ -> return ()

    tick :: Onyx ()
    tick = do
      draw
      SDL.present rend
      liftIO $ threadDelay 10000
      evts <- SDL.pollEvents
      processEvents evts

    newSelect :: Maybe (Int, Int) -> Onyx ()
    newSelect mousePos = do
      choices <- getChoices
      GUIState{..} <- get
      modifySelect $ \_ -> case mousePos of
        Nothing -> SelectMenu 0
        Just (x, y) -> let
          offset = length previousScreens * 25
          in if offset > x
            then let
              dropPrev = quot (offset - x) 25
              in SelectPage dropPrev
            else let
              i = div (y - 10) 70
              in if 0 <= i && i < length choices
                then SelectMenu i
                else NoSelect

    doSelect :: Maybe (Int, Int) -> Onyx ()
    doSelect mousePos = do
      GUIState{..} <- get
      case currentSelection of
        NoSelect -> return ()
        SelectPage i -> case drop i previousScreens of
          pm : pms -> do
            case currentScreen of
              TasksRunning tid _ -> liftIO $ killThread tid
              _                  -> return ()
            put $ GUIState pm pms NoSelect
          [] -> return ()
        SelectMenu i -> do
          choices <- getChoices
          choiceValue $ choices !! i
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
      SDL.MouseMotionEvent SDL.MouseMotionEventData
        { SDL.mouseMotionEventPos = SDL.P (SDL.V2 x y)
        } -> do
          newSelect $ Just (fromIntegral x, fromIntegral y)
          processEvents es
      SDL.MouseButtonEvent SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Pressed
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (SDL.V2 x y)
        } -> do
          doSelect $ Just (fromIntegral x, fromIntegral y)
          processEvents es
      SDL.KeyboardEvent SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = ksym
        , SDL.keyboardEventRepeat = False
        } -> case SDL.keysymScancode ksym of
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
            GUIState{..} <- get
            modifySelect $ \case
              SelectMenu i -> if null previousScreens then SelectMenu i else SelectPage 0
              SelectPage i -> SelectPage $ min (length previousScreens - 1) $ i + 1
              NoSelect     -> SelectMenu 0
            processEvents es
          SDL.ScancodeRight -> do
            modifySelect $ \case
              sel@(SelectMenu _) -> sel
              SelectPage 0 -> SelectMenu 0
              SelectPage i -> SelectPage $ i - 1
              NoSelect     -> SelectMenu 0
            processEvents es
          SDL.ScancodeDown -> do
            choices <- getChoices
            modifySelect $ \case
              SelectMenu i       -> SelectMenu $ min (length choices - 1) $ i + 1
              NoSelect           -> SelectMenu 0
              sel@(SelectPage _) -> sel
            processEvents es
          SDL.ScancodeUp -> do
            modifySelect $ \case
              SelectMenu i       -> SelectMenu $ max 0 $ i - 1
              NoSelect           -> SelectMenu 0
              sel@(SelectPage _) -> sel
            processEvents es
          _ -> processEvents es
      _ -> processEvents es

    checkVars :: Onyx ()
    checkVars = do
      liftIO (tryTakeMVar varSelectedFile) >>= \case
        Nothing -> return ()
        Just fps -> modify $ \case
          GUIState{ currentScreen = Files fpick files useFiles, .. } ->
            GUIState{ currentScreen = Files fpick (files ++ fps) useFiles, .. }
          s -> s
      liftIO (tryTakeMVar varTaskComplete) >>= \case
        Nothing -> return ()
        Just (strOut, (res, Messages warns)) -> get >>= \case
          GUIState (TasksRunning tid oldStatus) prevMenus sel -> let
            newStatus = let
              TasksStatus{..} = oldStatus
              strOut' = unlines $ concat
                [ [strOut]
                , map (\msg -> "Warning: " ++ displayException msg) warns
                , case res of
                  Right newFiles -> "Success! Output files:" : map ("  " ++) newFiles
                  Left err -> ["ERROR!", displayException err]
                ]
              in case res of
                Right newFiles -> TasksStatus
                  { tasksOK = tasksOK + 1
                  , tasksOutput = tasksOutput ++ [(strOut', Just newFiles)]
                  , ..
                  }
                Left _err -> TasksStatus
                  { tasksFailed = tasksFailed + 1
                  , tasksOutput = tasksOutput ++ [(strOut', Nothing)]
                  , ..
                  }
            in if tasksOK newStatus + tasksFailed newStatus == tasksTotal newStatus
              then do
                useResultFiles $ tasksOutput newStatus >>= concatMap concat
                logFile <- liftIO $ do
                  logDir <- getXdgDirectory XdgCache "onyx-log"
                  createDirectoryIfMissing False logDir
                  time <- getCurrentTime
                  let logFile = logDir </> fmt time <.> "txt"
                      fmt = formatTime defaultTimeLocale $ iso8601DateFormat $ Just "%H%M%S"
                  path <- getEnv "PATH"
                  writeFile logFile $ unlines $ (["PATH:", path] ++) $ map fst $ tasksOutput newStatus
                  return logFile
                put $ GUIState (TasksDone logFile newStatus) prevMenus sel
              else put $ GUIState (TasksRunning tid newStatus) prevMenus sel
          _ -> return ()
      tick

  evalStateT tick initialState
