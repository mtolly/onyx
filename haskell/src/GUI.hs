{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
module GUI where

import           CommandLine                    (commandLine)
import           Control.Concurrent             (ThreadId, forkIO, killThread,
                                                 threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception              (bracket, bracket_)
import           Control.Monad.Extra
import           Control.Monad.Trans.StackTrace (StackTraceT, runStackTraceT)
import qualified Data.ByteString                as B
import           Data.ByteString.Unsafe         (unsafeUseAsCStringLen)
import qualified Data.Text                      as T
import           Foreign                        (Ptr, castPtr)
import           Foreign.C                      (CInt (..), peekCString)
import           Resources                      (pentatonicTTF)
import           SDL                            (($=))
import qualified SDL
import           SDL.Raw                        (Color (..), RWops,
                                                 rwFromConstMem)
import qualified SDL.TTF                        as TTF
import           SDL.TTF.FFI                    (TTFFont)
import           TinyFileDialogs

foreign import ccall unsafe "TTF_OpenFontRW"
  openFontRW :: Ptr RWops -> CInt -> CInt -> IO TTFFont

withBSFont :: B.ByteString -> Int -> (TTFFont -> IO a) -> IO a
withBSFont bs pts act = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
  rw  <- rwFromConstMem (castPtr ptr) (fromIntegral len)
  bracket (openFontRW rw 1 $ fromIntegral pts) TTF.closeFont act

data Menu
  = Choice [(T.Text, Menu)]
  | File T.Text (FilePath -> Menu)
  | Go (StackTraceT IO ())

topMenu :: Menu
topMenu = Choice
  [ ( "PS to RB3"
    , File "Select song.ini" $ \f ->
      Go $ commandLine ["convert", "--game", "rb3", f]
    )
  , ( "RB3 to RB2"
    , File "Select rb3con" $ \f -> Choice
      [ ( "No keys"
        , Go $ commandLine ["convert", "--game", "rb2", f]
        )
      , ( "Keys on guitar"
        , Go $ commandLine ["convert", "--game", "rb2", f, "--keys-on-guitar"]
        )
      , ( "Keys on bass"
        , Go $ commandLine ["convert", "--game", "rb2", f, "--keys-on-bass"]
        )
      ]
    )
  , ( "Song preview"
    , File "Select rb3con" $ \f ->
      Go $ commandLine ["player", f]
    )
  ]

data GUIState
  = InMenu (Menu, Int) [(Menu, Int)]
  | TaskRunning ThreadId
  | TaskComplete
  | TaskFailed

launchGUI :: IO ()
launchGUI = do

  bracket_ SDL.initializeAll SDL.quit $ do
  TTF.withInit $ do
  withBSFont pentatonicTTF 50 $ \penta -> do
  let windowConf = SDL.defaultWindow { SDL.windowResizable = True, SDL.windowHighDPI = False }
  bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
  bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do

  let purple = SDL.V4 0x4B 0x1C 0x4E 0xFF

  varSelectedFile <- newEmptyMVar
  varTaskComplete <- newEmptyMVar

  let

    initialState = InMenu (topMenu, 0) []

    simpleText txt = do
      bracket (TTF.renderUTF8Blended penta (T.unpack txt) $ Color 0xEE 0xEE 0xEE 255) SDL.freeSurface $ \surf -> do
        dims <- SDL.surfaceDimensions surf
        bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
          SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 0 0)) dims

    draw guiState = do
      SDL.rendererDrawColor rend $= purple
      SDL.clear rend
      case guiState of
        TaskRunning _ -> simpleText "Running..."
        TaskComplete -> simpleText "Complete!"
        TaskFailed -> simpleText "Failed..."
        InMenu (menu, selected) _ -> case menu of
          Go _ -> simpleText "Go!"
          File label _ -> simpleText label
          Choice choices -> do
            forM_ (zip [0..] $ map fst choices) $ \(index, label) -> do
              let color = if index == selected then Color 0xEE 0xEE 0xEE 255 else Color 0xD0 0xB4 0xD2 255
              bracket (TTF.renderUTF8Blended penta (T.unpack label) color) SDL.freeSurface $ \surf -> do
                dims <- SDL.surfaceDimensions surf
                bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
                  SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 0 (fromIntegral index * 50))) dims

    tick guiState = do
      draw guiState
      SDL.present rend
      threadDelay 5000
      evts <- SDL.pollEvents
      processEvents evts guiState

    processEvents [] guiState = checkVars guiState
    processEvents (e : es) guiState = case SDL.eventPayload e of
      SDL.QuitEvent -> return ()
      SDL.DropEvent (SDL.DropEventData cstr) -> do
        peekCString cstr >>= putMVar varSelectedFile
        processEvents es guiState
      SDL.KeyboardEvent (SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = ksym
        }) -> case guiState of
          TaskComplete -> processEvents es $ case SDL.keysymScancode ksym of
            SDL.ScancodeReturn -> initialState
            _                  -> guiState
          TaskFailed -> processEvents es $ case SDL.keysymScancode ksym of
            SDL.ScancodeReturn -> initialState
            _                  -> guiState
          TaskRunning tid -> case SDL.keysymScancode ksym of
            SDL.ScancodeBackspace -> do
              killThread tid
              processEvents es initialState
            _ -> processEvents es guiState
          InMenu m@(menu, selected) prevMenus -> case SDL.keysymScancode ksym of
            SDL.ScancodeDown -> let
              maxIndex = case menu of
                Choice xs -> length xs - 1
                _         -> 0
              in processEvents es $ InMenu (menu, min maxIndex $ selected + 1) prevMenus
            SDL.ScancodeUp -> processEvents es $ InMenu (menu, max 0 $ selected - 1) prevMenus
            SDL.ScancodeBackspace -> case prevMenus of
              pm : pms -> processEvents es $ InMenu pm pms
              []       -> processEvents es guiState
            SDL.ScancodeReturn -> case menu of
              Choice choices -> processEvents es $ InMenu (snd $ choices !! selected, 0) (m : prevMenus)
              File _ _ -> do
                void $ forkIO $ openFileDialog "" "" [] "" False >>= \case
                  Just (file : _) -> putMVar varSelectedFile $ T.unpack file
                  _ -> return ()
                processEvents es guiState
              Go task -> do
                tid <- forkIO $ do
                  (res, _warns) <- runStackTraceT task
                  putMVar varTaskComplete $ case res of Left _ -> False; Right _ -> True
                processEvents es $ TaskRunning tid
            _ -> processEvents es guiState
      _ -> processEvents es guiState

    checkVars s0 = do
      s1 <- tryTakeMVar varSelectedFile >>= return . \case
        Nothing -> s0
        Just fp -> case s0 of
          InMenu m@(File _ useFile, _) prevMenus ->
            InMenu (useFile fp, 0) (m : prevMenus)
          _ -> s0
      s2 <- tryTakeMVar varTaskComplete >>= return . \case
        Nothing -> s1
        Just b -> case s1 of
          TaskRunning _ -> if b then TaskComplete else TaskFailed
          _             -> s1
      tick s2

  tick initialState
