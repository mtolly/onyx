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
import           System.IO.Silently             (capture)
import           TinyFileDialogs

foreign import ccall unsafe "TTF_OpenFontRW"
  openFontRW :: Ptr RWops -> CInt -> CInt -> IO TTFFont

withBSFont :: B.ByteString -> Int -> (TTFFont -> IO a) -> IO a
withBSFont bs pts act = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
  rw  <- rwFromConstMem (castPtr ptr) (fromIntegral len)
  bracket (openFontRW rw 1 $ fromIntegral pts) TTF.closeFont act

data Menu
  = Choices [Choice] Choice [Choice]
  | File T.Text (FilePath -> Menu)
  | Go (StackTraceT IO [FilePath])

data Choice = Choice
  { choiceTitle       :: T.Text
  , choiceDescription :: T.Text
  , choiceMenu        :: Menu
  }

topMenu :: Menu
topMenu = Choices []
  ( Choice "PS to RB3" "Attempts to convert a Frets on Fire/Phase Shift song to Rock Band 3."
  $ File "Select song.ini" $ \f ->
    Go $ commandLine ["convert", "--game", "rb3", f]
  )
  [ ( Choice "RB3 to RB2" "Converts a song from Rock Band 3 to Rock Band 2."
    $ File "Select rb3con" $ \f -> Choices []
      ( Choice "No keys" "Drops the Keys part if present."
      $ Go $ commandLine ["convert", "--game", "rb2", f]
      )
      [ ( Choice "Keys on guitar" "Drops Guitar if present, and puts Keys on Guitar (like RB3 keytar mode)."
        $ Go $ commandLine ["convert", "--game", "rb2", f, "--keys-on-guitar"]
        )
      , ( Choice "Keys on bass" "Drops Bass if present, and puts Keys on Bass (like RB3 keytar mode)."
        $ Go $ commandLine ["convert", "--game", "rb2", f, "--keys-on-bass"]
        )
      ]
    )
  , ( Choice "Web preview" "Produces a web browser app to preview a song."
    $ File "Select rb3con" $ \f ->
      Go $ commandLine ["player", f]
    )
  , ( Choice "REAPER project" "Converts a MIDI or song (RB3/RB2/PS) to a REAPER project."
    $ File "Select _rb3con or .mid" $ \f ->
      Go $ commandLine ["reap", f]
    )
  ]

data GUIState
  = InMenu Menu [Menu]
  | TaskRunning ThreadId
  | TaskComplete
  | TaskFailed

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

    initialState = InMenu topMenu []

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
        TaskRunning _ -> simpleText 0 "Running..."
        TaskComplete -> simpleText 0 "Complete!"
        TaskFailed -> simpleText 0 "Failed..."
        InMenu menu prevMenus -> do
          let offset = fromIntegral $ length prevMenus * 20
          forM_ (zip [0.88, 0.76 ..] [offset - 20, offset - 40 .. 0]) $ \(frac, x) -> do
            SDL.rendererDrawColor rend $= purple frac
            SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 x 0) $ SDL.V2 20 windH
          case menu of
            Go _ -> simpleText offset "Go!"
            File label _ -> simpleText offset label
            Choices prev this next -> do
              let choices = map (False,) (reverse prev) ++ [(True, this)] ++ map (False,) next
              forM_ (zip [0..] choices) $ \(index, (selected, choice)) -> do
                let color = if selected then Color 0xEE 0xEE 0xEE 255 else Color 0x80 0x54 0x82 255
                bracket (TTF.renderUTF8Blended penta (T.unpack $ choiceTitle choice) color) SDL.freeSurface $ \surf -> do
                  dims <- SDL.surfaceDimensions surf
                  bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
                    SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (index * 70 + 10))) dims
                bracket (TTF.renderUTF8Blended pentaSmall (T.unpack $ choiceDescription choice) color) SDL.freeSurface $ \surf -> do
                  dims <- SDL.surfaceDimensions surf
                  bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
                    SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (index * 70 + 50))) dims

    tick guiState = do
      draw guiState
      SDL.present rend
      threadDelay 10000
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
        , SDL.keyboardEventRepeat = False
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
          InMenu menu prevMenus -> case SDL.keysymScancode ksym of
            SDL.ScancodeDown -> processEvents es $ case menu of
              Choices prev this (n : next) -> InMenu (Choices (this : prev) n next) prevMenus
              _                            -> guiState
            SDL.ScancodeUp -> processEvents es $ case menu of
              Choices (p : prev) this next -> InMenu (Choices prev p (this : next)) prevMenus
              _                            -> guiState
            SDL.ScancodeBackspace -> case prevMenus of
              pm : pms -> processEvents es $ InMenu pm pms
              []       -> processEvents es guiState
            SDL.ScancodeReturn -> case menu of
              Choices _ choice _ -> processEvents es $ InMenu (choiceMenu choice) (menu : prevMenus)
              File _ _ -> do
                void $ forkIO $ openFileDialog "" "" [] "" False >>= \case
                  Just (file : _) -> putMVar varSelectedFile $ T.unpack file
                  _ -> return ()
                processEvents es guiState
              Go task -> do
                tid <- forkIO $ do
                  results <- capture $ runStackTraceT task
                  putMVar varTaskComplete results
                processEvents es $ TaskRunning tid
            _ -> processEvents es guiState
      _ -> processEvents es guiState

    checkVars s0 = do
      s1 <- tryTakeMVar varSelectedFile >>= return . \case
        Nothing -> s0
        Just fp -> case s0 of
          InMenu m@(File _ useFile) prevMenus ->
            InMenu (useFile fp) (m : prevMenus)
          _ -> s0
      s2 <- tryTakeMVar varTaskComplete >>= \case
        Nothing -> return s1
        Just (_strOut, (res, _warns)) -> case s1 of
          TaskRunning _ -> case res of
            Right files -> do
              case files of
                [f] -> useResultFile f
                _   -> return ()
              return TaskComplete
            Left _err -> return TaskFailed
          _ -> return s1
      tick s2

  tick initialState
