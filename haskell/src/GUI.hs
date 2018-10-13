{-# LANGUAGE DeriveFoldable           #-}
{-# LANGUAGE DeriveFunctor            #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE MultiWayIf               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TupleSections            #-}
module GUI (launchGUI) where

import           Build                            (loadYaml)
import           CommandLine                      (FileType (..), commandLine,
                                                   identifyFile')
import           Config
import           Control.Arrow                    (first)
import           Control.Concurrent               (ThreadId, forkIO, killThread,
                                                   threadDelay)
import           Control.Concurrent.STM
import           Control.Exception                (bracket, bracket_,
                                                   displayException, throwIO)
import           Control.Monad.Extra
import           Control.Monad.IO.Class           (MonadIO (..))
import           Control.Monad.State
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                       as A
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isPrint)
import           Data.Default.Class               (def)
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromJust, fromMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Data.Time
import           Data.Version                     (showVersion)
import           Data.Word                        (Word8)
import qualified FeedBack.Load                    as FB
import           Foreign                          (castPtr)
import           Foreign.C                        (CInt (..))
import qualified FretsOnFire                      as FoF
import           Graphics.UI.TinyFileDialogs
import           Import                           (importSTFS)
import           Magma                            (getRBAFileBS)
import           Network.HTTP.Req                 ((/:))
import qualified Network.HTTP.Req                 as Req
import           OpenProject                      (Project (..), openProject)
import           OSFiles                          (osOpenFile, useResultFiles)
import           Paths_onyxite_customs_tool       (version)
import           PrettyDTA                        (DTASingle (..),
                                                   readDTASingles)
import           Resources                        (pentatonicTTF, veraMonoTTF)
import qualified RhythmGame.Audio                 as RGAudio
import qualified RhythmGame.Drums                 as RGDrums
import           RockBand.Codec.Drums
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.ProGuitar         (standardGuitar)
import           RockBand.Common                  (Difficulty (..))
import           RockBand.ProGuitar.Keyboard      (GtrSettings (..), runApp)
import           Scripts                          (loadMIDI)
import           SDL                              (($=))
import qualified SDL
import qualified SDL.Font                         as TTF
import qualified SDL.Raw                          as Raw
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (STFSContents (..), withSTFS)
import qualified System.Directory                 as Dir
import           System.Environment               (getEnv)
import           System.FilePath                  ((<.>), (</>))
import           System.Info                      (os)
import qualified System.IO.Temp                   as Temp
import qualified System.MIDI                      as MIDI

withBSFont :: B.ByteString -> Int -> (TTF.Font -> IO a) -> IO a
withBSFont bs pts = bracket (TTF.decode bs pts) TTF.free

data Selection
  = NoSelect
  | SelectMenu Int -- SelectMenu 0 means the top option in the menu
  | SelectPage Int -- SelectPage 0 means go back one page, 1 means 2 pages, etc.
  | SelectLogo
  deriving (Eq, Ord, Show, Read)

data Menu
  = Choices [Choice (Onyx ())]
  | Files FilePicker ([FilePath] -> Onyx ())
  | SaveFile SavePicker (FilePath -> Onyx ())
  | TasksStart [StackTraceT (QueueLog IO) [FilePath]]
  | TasksRunning ThreadId TasksStatus
  | TasksDone FilePath TasksStatus
  | EnterInt T.Text Int (Int -> Onyx ())
  | Song Project Menu
  | Game FilePath

data Choice a = Choice
  { choiceTitle       :: T.Text
  , choiceDescription :: T.Text
  , choiceValue       :: a
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

data FilePicker = FilePicker
  { filePatterns    :: [T.Text]
  , fileDescription :: T.Text
  , fileFilter      :: FilePath -> Onyx T.Text
  , fileLoaded      :: [FilePath]
  }

data SavePicker = SavePicker
  { savePatterns    :: [T.Text]
  , saveDescription :: T.Text
  , saveCurrent     :: T.Text
  }

pickFiles :: [T.Text] -> T.Text -> (FilePath -> Onyx T.Text) -> ([FilePath] -> Onyx ()) -> Menu
pickFiles pats desc filt fn = let
  picker = FilePicker
    { filePatterns = pats
    , fileDescription = desc
    , fileFilter = filt
    , fileLoaded = []
    }
  in Files picker fn

data TasksStatus = TasksStatus
  { tasksTotal  :: Int
  , tasksOK     :: Int
  , tasksFailed :: Int
  , tasksFiles  :: [FilePath]
  } deriving (Eq, Ord, Show, Read)

data Terminal a = Terminal
  { terminalOutput :: [a]
  -- TODO readd scroll but this time do it right
  } deriving (Eq, Ord, Show, Read)

setMenu :: Menu -> Onyx ()
setMenu menu = modify $ \GUIState{..} -> GUIState{ currentScreen = menu, .. }

pushMenu :: Menu -> Onyx ()
pushMenu menu = modify $ \gs -> gs
  { currentScreen   = menu
  , previousScreens = currentScreen gs : previousScreens gs
  }

popMenu :: Onyx ()
popMenu = modify $ \gs -> case previousScreens gs of
  m : pms -> gs { currentScreen = m, previousScreens = pms }
  []      -> gs

modifySelect :: (Selection -> Selection) -> Onyx ()
modifySelect f = modify $ \gs -> gs { currentSelection = f $ currentSelection gs }

commandLine' :: (MonadUnliftIO m) => [String] -> StackTraceT (QueueLog m) [FilePath]
commandLine' args = do
  let args' = flip map args $ \arg -> if ' ' `elem` arg then "\"" ++ arg ++ "\"" else arg
  lg $ unlines
    [ ""
    , ">>> Command: " ++ unwords args'
    , ""
    ]
  commandLine args

data KeysRB2 = NoKeys | KeysGuitar | KeysBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data ConvertOptions
  = ConvertRB3
    { crb3Speed         :: Int -- ^ in percent
    , crb3Project       :: Bool -- ^ make a Magma v2 + REAPER project instead of CON
    , crb3DropOpenHOPOs :: Bool -- ^ remove open HOPO or tap notes
    , crb3AutoToms      :: Bool -- ^ tom markers over whole song if no pro authored
    , crb3CopyGuitar    :: Bool -- ^ copy guitar to keys
    }
  | ConvertRB2
    { crb2Speed         :: Int -- ^ in percent
    , crb2DropOpenHOPOs :: Bool -- ^ remove open HOPO or tap notes
    , crb2Label         :: Bool -- ^ should (RB2 version) be added
    , crb2Keys          :: KeysRB2 -- ^ if keys should be dropped or moved to gtr or bass
    }
  deriving (Eq, Ord, Show, Read)

convertRB3 :: ConvertOptions
convertRB3 = ConvertRB3
  { crb3Speed = 100
  , crb3Project = False
  , crb3DropOpenHOPOs = False
  , crb3AutoToms = False
  , crb3CopyGuitar = False
  }

convertRB2 :: ConvertOptions
convertRB2 = ConvertRB2
  { crb2Speed = 100
  , crb2DropOpenHOPOs = False
  , crb2Label = True
  , crb2Keys = NoKeys
  }

data OptionInput a
  = OptionEnum [Choice a]
  | OptionInt T.Text Int (Int -> a)

optionsMenu :: a -> (a -> ([Choice (OptionInput a)], Menu)) -> Menu
optionsMenu current getOptions = let
  (topChoices, continue) = getOptions current
  topChoices' = flip map topChoices $ fmap $ \case
    OptionInt label start cont -> pushMenu $ EnterInt label start $ \int -> do
      popMenu
      setMenu $ optionsMenu (cont int) getOptions
    OptionEnum optionValues -> pushMenu $ Choices $ do
      optionValue <- optionValues
      return $ flip fmap optionValue $ \new -> do
        popMenu
        setMenu $ optionsMenu new getOptions
  continueChoice = Choice
    { choiceValue = pushMenu continue
    , choiceTitle = "Continue..."
    , choiceDescription = ""
    }
  in Choices $ topChoices' ++ [continueChoice]

topMenu :: Menu
topMenu = Choices
  [ ( Choice "Convert" "Modifies a song or converts between games."
    $ pushMenu $ pickFiles ["*_rb3con", "*_rb2con", "*.rba", "*.ini"] "Songs (RB3/RB2/PS)" filterSong
    $ \fs -> pushMenu $ optionsMenu convertRB3
    $ \case
      ConvertRB3{..} -> let
        continue = TasksStart $ flip map fs $ \f -> commandLine' $ concat
          [ [if crb3Project then "magma" else "convert", f, "--game", "rb3"]
          , ["--force-pro-drums" | not crb3AutoToms]
          , ["--drop-open-hopos" | crb3DropOpenHOPOs]
          , case crb3Speed of
            100 -> []
            _   -> ["--speed", show (fromIntegral crb3Speed / 100 :: Double)]
          , ["--guitar-on-keys" | crb3CopyGuitar]
          ]
        opts =
          [ Choice
            { choiceTitle = "Target game"
            , choiceDescription = "Rock Band 3"
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Rock Band 3"
                , choiceDescription = ""
                , choiceValue = ConvertRB3{..}
                }
              , Choice
                { choiceTitle = "Rock Band 2"
                , choiceDescription = ""
                , choiceValue = convertRB2
                  { crb2Speed = crb3Speed
                  , crb2DropOpenHOPOs = crb3DropOpenHOPOs
                  }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Make project: " <> if crb3Project then "Yes" else "No"
            , choiceDescription = "Make a Magma v2 + REAPER project instead of a CON file."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "Produce a Magma project. (Unencrypted audio only)"
                , choiceValue = ConvertRB3 { crb3Project = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "Produce a CON file."
                , choiceValue = ConvertRB3 { crb3Project = False, .. }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Speed: " <> T.pack (show crb3Speed) <> "%"
            , choiceDescription = "Speed up or slow down the song. (Unencrypted audio only)"
            , choiceValue = OptionInt "Song speed (%)" crb3Speed
              $ \newSpeed -> ConvertRB3 { crb3Speed = newSpeed, .. }
            }
          , Choice
            { choiceTitle = "[option] Automatic tom markers: " <> if crb3AutoToms then "Yes" else "No"
            , choiceDescription = "For FoF/PS songs with no Pro Drums, mark everything as toms."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "If no Pro Drums are found, tom markers will be added over the whole song."
                , choiceValue = ConvertRB3 { crb3AutoToms = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "No tom markers will be added."
                , choiceValue = ConvertRB3 { crb3AutoToms = False, .. }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Copy guitar to keys: " <> if crb3CopyGuitar then "Yes" else "No"
            , choiceDescription = "Copy the guitar chart to keys so two people can play it."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "Copy the guitar chart to keys"
                , choiceValue = ConvertRB3 { crb3CopyGuitar = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "No change"
                , choiceValue = ConvertRB3 { crb3CopyGuitar = False, .. }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Drop open HOPOs: " <> if crb3DropOpenHOPOs then "Yes" else "No"
            , choiceDescription = "Remove open notes that are HOPOs or tap notes."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "Remove open notes if HOPO/tap; translate to green if strum"
                , choiceValue = ConvertRB3 { crb3DropOpenHOPOs = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "All open notes will translate to green"
                , choiceValue = ConvertRB3 { crb3DropOpenHOPOs = False, .. }
                }
              ]
            }
          ]
        in (opts, continue)
      ConvertRB2{..} -> let
        continue = TasksStart $ flip map fs $ \f -> commandLine' $ concat
          [ ["convert", f, "--game", "rb2"]
          , case crb2Keys of NoKeys -> []; KeysGuitar -> ["--keys-on-guitar"]; KeysBass -> ["--keys-on-bass"]
          , case crb2Speed of
            100 -> []
            _   -> ["--speed", show (fromIntegral crb2Speed / 100 :: Double)]
          , ["--rb2-version" | crb2Label]
          , ["--drop-open-hopos" | crb2DropOpenHOPOs]
          ]
        opts =
          [ Choice
            { choiceTitle = "Target game"
            , choiceDescription = "Rock Band 2"
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Rock Band 3"
                , choiceDescription = ""
                , choiceValue = convertRB3
                  { crb3Speed = crb2Speed
                  , crb3DropOpenHOPOs = crb2DropOpenHOPOs
                  }
                }
              , Choice
                { choiceTitle = "Rock Band 2"
                , choiceDescription = ""
                , choiceValue = ConvertRB2{..}
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Speed: " <> T.pack (show crb2Speed) <> "%"
            , choiceDescription = "Speed up or slow down the song. (Unencrypted audio only)"
            , choiceValue = OptionInt "Song speed (%)" crb2Speed
              $ \newSpeed -> ConvertRB2 { crb2Speed = newSpeed, .. }
            }
          , Choice
            { choiceTitle = "[option] Keys: " <> case crb2Keys of NoKeys -> "No keys"; KeysGuitar -> "Keys on guitar"; KeysBass -> "Keys on bass"
            , choiceDescription = "Should Keys replace Guitar or Bass, or be removed?"
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "No keys"
                , choiceDescription = "Drops the Keys part if present."
                , choiceValue = ConvertRB2 { crb2Keys = NoKeys, .. }
                }
              , Choice
                { choiceTitle = "Keys on guitar"
                , choiceDescription = "Drops Guitar if present, and puts Keys on Guitar (like RB3 keytar mode)."
                , choiceValue = ConvertRB2 { crb2Keys = KeysGuitar, .. }
                }
              , Choice
                { choiceTitle = "Keys on bass"
                , choiceDescription = "Drops Bass if present, and puts Keys on Bass (like RB3 keytar mode)."
                , choiceValue = ConvertRB2 { crb2Keys = KeysBass, .. }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] RB2 label: " <> if crb2Label then "Yes" else "No"
            , choiceDescription = "Add (RB2 version) to the title of the song."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "Add (RB2 version)"
                , choiceValue = ConvertRB2 { crb2Label = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "Title will be unchanged"
                , choiceValue = ConvertRB2 { crb2Label = False, .. }
                }
              ]
            }
          , Choice
            { choiceTitle = "[option] Drop open HOPOs: " <> if crb2DropOpenHOPOs then "Yes" else "No"
            , choiceDescription = "Remove open notes that are HOPOs or tap notes."
            , choiceValue = OptionEnum
              [ Choice
                { choiceTitle = "Yes"
                , choiceDescription = "Remove open notes if HOPO/tap; translate to green if strum"
                , choiceValue = ConvertRB2 { crb2DropOpenHOPOs = True, .. }
                }
              , Choice
                { choiceTitle = "No"
                , choiceDescription = "All open notes will translate to green"
                , choiceValue = ConvertRB2 { crb2DropOpenHOPOs = False, .. }
                }
              ]
            }
          ]
        in (opts, continue)
    )
  , ( Choice "Preview" "Produces a web browser app to preview a song."
    $ pushMenu $ pickFiles ["*_rb3con", "*_rb2con", "*.rba", "*.ini"] "Songs (RB3/RB2/PS)" filterSong $ \fs ->
      pushMenu $ TasksStart $ map (\f -> commandLine' ["player", f]) fs
    )
  , ( Choice "Reduce" "Fills empty difficulties in a MIDI file with automatic reductions."
    $ pushMenu $ pickFiles ["*.mid"] "MIDI files" (const $ return "") $ \fs ->
      pushMenu $ TasksStart $ map (\f -> commandLine' ["reduce", f]) fs
    )
  ]

hiddenOptions :: [Choice (Onyx ())]
hiddenOptions =
  [ ( Choice "Game" "(WIP) Building a RB clone game."
    $ pushMenu $ pickFiles ["*_rb3con", "*_rb2con"] "Songs (RB3/RB2)" filterSong $ \fs ->
      pushMenu $ case fs of
        [f] -> Game f
        _   -> Choices []
    )
  , ( Choice "Keytar" "(WIP) Play Pro Guitar with a keyboard."
    $ liftIO MIDI.enumerateSources >>= \srcs -> do
      srcNames <- liftIO $ mapM MIDI.getName srcs
      pushMenu $ Choices $ let
        withSrc (srcName, src) = Choice (T.pack srcName) "MIDI source" $ liftIO MIDI.enumerateDestinations >>= \dests -> do
          destNames <- liftIO $ mapM MIDI.getName dests
          pushMenu $ Choices $ let
            -- TODO menu for settings
            settings = GtrSettings $ \str -> Just $ standardGuitar !! fromEnum str
            withDest (destName, dest) = Choice (T.pack destName) "MIDI destination"
              $ pushMenu $ TasksStart [runApp src dest settings >> return []]
            in case srcs of
              []    -> [Choice "No MIDI destinations found" "" $ return ()]
              _ : _ -> map withDest $ zip destNames dests
        in case srcs of
          []    -> [Choice "No MIDI sources found" "" $ return ()]
          _ : _ -> map withSrc $ zip srcNames srcs
    )
  , ( Choice "Load song" "Imports a song and shows all available functions."
    $ pushMenu $ pickFiles ["*_rb3con", "*_rb2con", "*.rba", "*.ini"] "Songs (RB3/RB2/PS)" (return . T.pack)
    $ \case
      [] -> return ()
      f : _ -> void $ forkOnyx $ do
        res <- mapStackTraceT (mapQueueLog lift) $ errorToEither $ openProject f
        case res of
          Left msgs -> getEventSink >>= sendErrors msgs
          Right proj -> do
            sendEvent $ LogMessage (GUISuccess, Message "Imported song successfully." [])
            sendEvent $ SongImport proj
    )
  ]

data GUIEvent
  = LoadFile FilePath
  | SongImport Project
  | LogStart
  | LogMessage (GUIMessageLevel, Message)
  | TaskProgress TaskProgress

data GUIMessageLevel
  = GUISuccess
  | GUIFailure
  | GUIWarning
  | GUILog
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data GUIState = GUIState
  { currentScreen    :: Menu
  , previousScreens  :: [Menu] -- TODO should add Selection
  , currentSelection :: Selection
  , globalTerminal   :: Terminal (GUIMessageLevel, Message)
  , currentLog       :: [(GUIMessageLevel, Message)]
  , eventChannel     :: TChan GUIEvent
  }

initialState :: TChan GUIEvent -> GUIState
initialState evs = GUIState
  { currentScreen    = topMenu
  , previousScreens  = []
  , currentSelection = NoSelect
  , globalTerminal   = Terminal []
  , currentLog       = []
  , eventChannel     = evs
  }

type Onyx = StackTraceT (QueueLog (StateT GUIState (ResourceT IO)))

getEventSink :: Onyx (GUIEvent -> IO ())
getEventSink = do
  gs <- get
  return $ atomically . writeTChan (eventChannel gs)

logToSink
  :: (MonadIO m)
  => (GUIEvent -> IO ())
  -> StackTraceT (QueueLog m) a
  -> m (Either Messages a)
logToSink sink = logIO $ sink . LogMessage . \case
  (MessageWarning, x) -> (GUIWarning, x)
  (MessageLog    , x) -> (GUILog    , x)

sendEvent :: GUIEvent -> Onyx ()
sendEvent e = getEventSink >>= liftIO . ($ e)

sendErrors :: (MonadIO m) => Messages -> (GUIEvent -> IO ()) -> m ()
sendErrors (Messages msgs) sink = liftIO $ forM_ msgs $ \msg -> sink $ LogMessage (GUIFailure, msg)

forkOnyx :: Onyx () -> Onyx ThreadId
forkOnyx act = do
  sink <- getEventSink
  gs <- get
  lift $ lift $ lift $ resourceForkIO $ do
    -- the child thread gets the same GUIState,
    -- but really it should only access the eventChannel.
    -- we could make a different type I guess but doesn't seem too important
    res <- evalStateT (logToSink sink act) gs
    case res of
      Right ()             -> return ()
      Left (Messages msgs) -> forM_ msgs $ \msg -> do
        liftIO $ sink $ LogMessage (GUIFailure, msg)

data TaskProgress
  = TaskOK [FilePath]
  | TaskFailed Messages
  deriving (Eq, Ord, Show, Read)

launchGUI :: IO ()
launchGUI = do

  bracket_ SDL.initializeAll SDL.quit $ do
  bracket_ TTF.initialize TTF.quit $ do
  withBSFont pentatonicTTF 40 $ \penta -> do
  withBSFont pentatonicTTF 20 $ \pentaSmall -> do
  withBSFont veraMonoTTF 15 $ \mono -> do
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

  varEvents <- atomically newTChan
  varNewestRelease <- atomically newTChan

  _ <- forkIO $ do
    let addr = Req.https "api.github.com" /: "repos" /: "mtolly" /: "onyxite-customs" /: "releases" /: "latest"
    rsp <- Req.runReq def $ Req.req Req.GET addr Req.NoReqBody Req.jsonResponse $ Req.header "User-Agent" "mtolly/onyxite-customs"
    case Req.responseBody rsp of
      A.Object obj -> case HM.lookup "name" obj of
        Just (A.String str) -> atomically $ writeTChan varNewestRelease $ T.unpack str
        _                   -> return ()
      _            -> return ()

  bracket (TTF.blended penta (purple 0.4) "ONYX") SDL.freeSurface $ \surfBrand -> do
  bracket (SDL.createTextureFromSurface rend surfBrand) SDL.destroyTexture $ \texBrand -> do
  dimsBrand@(SDL.V2 brandW brandH) <- SDL.surfaceDimensions surfBrand

  bracket (TTF.blended penta (SDL.V4 0xEE 0xEE 0xEE 0xFF) "ONYX") SDL.freeSurface $ \surfBrandSel -> do
  bracket (SDL.createTextureFromSurface rend surfBrandSel) SDL.destroyTexture $ \texBrandSel -> do

  bracket (TTF.blended pentaSmall (purple 0.4) $ T.pack $ showVersion version) SDL.freeSurface $ \surfVersion -> do
  bracket (SDL.createTextureFromSurface rend surfVersion) SDL.destroyTexture $ \texVersion -> do
  dimsVersion@(SDL.V2 versionW versionH) <- SDL.surfaceDimensions surfVersion

  bracket (TTF.blended pentaSmall (purple 0.4) "latest") SDL.freeSurface $ \surfLatest -> do
  bracket (SDL.createTextureFromSurface rend surfLatest) SDL.destroyTexture $ \texLatest -> do
  dimsLatest@(SDL.V2 latestW latestH) <- SDL.surfaceDimensions surfLatest

  bracket (TTF.blended pentaSmall (SDL.V4 0x80 0x54 0x82 255) "update available!") SDL.freeSurface $ \surfUpdate -> do
  bracket (SDL.createTextureFromSurface rend surfUpdate) SDL.destroyTexture $ \texUpdate -> do
  dimsUpdate@(SDL.V2 updateW updateH) <- SDL.surfaceDimensions surfUpdate

  let monoChar c = TTF.blendedGlyph mono (SDL.V4 0xEE 0xEE 0xEE 0xFF) c
      printChars = filter isPrint ['\0' .. '\255']
  bracket (mapM monoChar printChars) (mapM_ SDL.freeSurface) $ \surfsMono -> do
  bracket (mapM (SDL.createTextureFromSurface rend) surfsMono) (mapM_ SDL.destroyTexture) $ \texsMono -> do
  dimsMono@(SDL.V2 monoW monoH) <- SDL.surfaceDimensions $ head surfsMono
  let monoMap = HM.fromList $ zip printChars texsMono
      monoGlyph c = fromMaybe (fromJust $ HM.lookup '?' monoMap) $ HM.lookup c monoMap

  let

    fixPatterns :: [T.Text] -> [T.Text]
    fixPatterns pats = if os /= "darwin" || all ("*." `T.isPrefixOf`) pats
      then pats
      else []

    getChoices :: Onyx [Choice (Onyx ())]
    getChoices = gets $ \gs -> getChoices' $ currentScreen gs

    getChoices' :: Menu -> [Choice (Onyx ())]
    getChoices' = \case
      Game{} -> []
      Song proj menu -> Choice
        (getTitle $ _metadata $ projectSongYaml proj)
        (getArtist $ _metadata $ projectSongYaml proj)
        (return ())
        : getChoices' menu
      Choices cs -> cs
      SaveFile spick useFile ->
        [ Choice "Select output path..." (saveDescription spick) $ do
          let pats = fixPatterns $ savePatterns spick
          liftIO $ void $ forkIO $ saveFileDialog "" (saveCurrent spick) pats (saveDescription spick) >>= \case
            Just chosen -> atomically $ writeTChan varEvents $ LoadFile $ T.unpack chosen
            Nothing     -> return ()
        ] ++ if T.null $ saveCurrent spick then [] else
          [ Choice "Continue" (saveCurrent spick) $ useFile $ T.unpack $ saveCurrent spick
          ]
      Files fpick useFiles ->
        [ Choice "Select files... (or drag and drop)" (fileDescription fpick) $ do
          let pats = fixPatterns $ filePatterns fpick
          liftIO $ void $ forkIO $ openFileDialog "" "" pats (fileDescription fpick) True >>= \case
            Just chosen -> atomically $ forM_ chosen $ \f -> writeTChan varEvents $ LoadFile $ T.unpack f
            Nothing     -> return ()
        ] ++ if null $ fileLoaded fpick then [] else let
          desc = case length $ fileLoaded fpick of
            1 -> "1 file loaded"
            n -> T.pack (show n) <> " files loaded"
          in  [ Choice "Continue" desc $ useFiles $ fileLoaded fpick
              , Choice "Clear selection" "" $ do
                lg "Selection cleared."
                setMenu $ Files fpick{ fileLoaded = [] } useFiles
              ]
      TasksStart tasks -> let
        go = do
          liftIO $ atomically $ writeTChan varEvents LogStart
          tid <- forkOnyx $ forM_ tasks $ \task -> do
            res <- mapStackTraceT (mapQueueLog $ lift . lift) $ errorToEither task
            case res of
              Left msgs -> do
                getEventSink >>= sendErrors msgs
                sendEvent $ TaskProgress $ TaskFailed msgs
              Right files -> do
                sendEvent $ LogMessage (GUISuccess, Message (show files) []) -- TODO show nicer
                sendEvent $ TaskProgress $ TaskOK files
          setMenu $ TasksRunning tid TasksStatus
            { tasksTotal = length tasks
            , tasksOK = 0
            , tasksFailed = 0
            , tasksFiles = []
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
        , Choice "Main menu" "" $ do
          gs <- get
          mapM_ cleanupPage $ currentScreen gs : previousScreens gs
          put (initialState varEvents){ globalTerminal = globalTerminal gs }
        ]
      EnterInt label int useInt ->
        [ Choice "+ 5" "" $ setMenu $ EnterInt label (int + 5) useInt
        , Choice "+ 1" "" $ setMenu $ EnterInt label (int + 1) useInt
        , Choice (T.pack $ show int) label $ return ()
        , Choice "- 1" "" $ setMenu $ EnterInt label (max 1 $ int - 1) useInt
        , Choice "- 5" "" $ setMenu $ EnterInt label (max 1 $ int - 5) useInt
        , Choice "Save" "" $ useInt int
        ]

    draw :: Onyx ()
    draw = do
      SDL.V2 windW windH <- SDL.get $ SDL.windowSize window
      SDL.rendererDrawColor rend $= purple 1
      SDL.clear rend
      GUIState{..} <- get
      when (null previousScreens) $ do
        let brand = case currentSelection of
              SelectLogo -> texBrandSel
              _          -> texBrand
        SDL.copy rend brand Nothing $ Just $ SDL.Rectangle
          (SDL.P (SDL.V2 (windW - brandW - 10) (windH - brandH - 10)))
          dimsBrand
        SDL.copy rend texVersion Nothing $ Just $ SDL.Rectangle
          (SDL.P (SDL.V2 (windW - brandW - 10 - versionW - 10) (windH - versionH - 13)))
          dimsVersion
        liftIO (atomically $ tryPeekTChan varNewestRelease) >>= \case
          Nothing -> return ()
          Just s -> if s == showVersion version
            then SDL.copy rend texLatest Nothing $ Just $ SDL.Rectangle
              (SDL.P (SDL.V2 (windW - brandW - 10 - latestW - 10) (windH - latestH - 3 - versionH - 13)))
              dimsLatest
            else SDL.copy rend texUpdate Nothing $ Just $ SDL.Rectangle
              (SDL.P (SDL.V2 (windW - brandW - 10 - updateW - 10) (windH - updateH - 3 - versionH - 13)))
              dimsUpdate
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
            color = if selected then SDL.V4 0xEE 0xEE 0xEE 255 else SDL.V4 0x80 0x54 0x82 255
        bracket (TTF.blended penta color $ choiceTitle choice) SDL.freeSurface $ \surf -> do
          dims <- SDL.surfaceDimensions surf
          bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
            SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 10))) dims
        case choiceDescription choice of
          ""  -> return () -- otherwise sdl2_ttf returns null surface
          str -> bracket (TTF.blended pentaSmall color str) SDL.freeSurface $ \surf -> do
            dims <- SDL.surfaceDimensions surf
            bracket (SDL.createTextureFromSurface rend surf) SDL.destroyTexture $ \tex -> do
              SDL.copy rend tex Nothing $ Just $ SDL.Rectangle (SDL.P (SDL.V2 (offset + 10) (fromIntegral index * 70 + 50))) dims
      let drawTerminalFor term = do
            let termBoxX = offset
                termBoxY = fromIntegral $ length choices * 70
                termBoxW = windW - termBoxX
                termBoxH = windH - termBoxY
            drawTerminal termBoxX termBoxY termBoxW termBoxH term
      case currentScreen of
        Files{} -> drawTerminalFor globalTerminal
        TasksRunning{} -> do
          drawTerminalFor globalTerminal
          let bigX = windW - 120
              bigY = 20
              smallSide = 50
          spinner bigX bigY smallSide
        TasksDone{} -> drawTerminalFor globalTerminal
        _ -> return ()

    drawTerminal :: (ShowTerminal a) => CInt -> CInt -> CInt -> CInt -> Terminal a -> Onyx ()
    drawTerminal termBoxX termBoxY termBoxW termBoxH term = do
      let termCols = max 0 $ quot termBoxW monoW - 2
          termRows = max 0 $ quot termBoxH monoH - 2
          termW = termCols * monoW
          termH = termRows * monoH
          termX = termBoxX + quot (termBoxW - termW) 2
          termY = termBoxY + quot (termBoxH - termH) 2
          drawChar row col c = SDL.copy rend (monoGlyph c) Nothing $ Just $ SDL.Rectangle
            (SDL.P (SDL.V2 (termX + col * monoW) (termY + row * monoH)))
            dimsMono
      SDL.rendererDrawColor rend $= SDL.V4 0x11 0x11 0x11 0xFF
      SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 termX termY) $ SDL.V2 termW termH
      let makeLines _    []       = []
          makeLines tone (m : ms) = let
            color = case colorTerminal m of
              Just c  -> c
              Nothing -> if tone then SDL.V4 0x22 0x22 0x22 0xFF else SDL.V4 0x33 0x33 0x33 0xFF
            wrapLines s = if length s <= fromIntegral termCols || termCols == 0
              then [s]
              else case splitAt (fromIntegral termCols) s of
                (l, ls) -> l : wrapLines ls
            msgLines = reverse $ concatMap wrapLines $ lines $ showTerminal m
            in map (color, ) msgLines ++ makeLines (not tone) ms
          drawLines _ [] = return ()
          drawLines n ((color, line) : clns) = if n < 0
            then return ()
            else do
              SDL.rendererDrawColor rend $= color
              SDL.fillRect rend $ Just $ SDL.Rectangle (SDL.P $ SDL.V2 termX $ termY + n * monoH) $ SDL.V2 termW monoH
              forM_ (zip [0 .. termCols - 1] line) $ uncurry $ drawChar n
              drawLines (n - 1) clns
      -- TODO readd scroll
      drawLines (termRows - 1) $ makeLines False $ terminalOutput term

    spinner :: CInt -> CInt -> CInt -> Onyx ()
    spinner bigX bigY smallSide = do
      -- square spinner animation
      t <- (`rem` 2000) <$> SDL.ticks
      let bigSide = smallSide * 2
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

    tick :: Onyx ()
    tick = gets currentScreen >>= \case
      Game f -> liftIO $ Temp.withSystemTempDirectory "onyx_game" $ \dir -> do
        res <- logStdout $ do
          _ <- importSTFS f Nothing dir
          song <- loadMIDI $ dir </> "notes.mid"
          let tempos = RBFile.s_tempos song
              drums = RBFile.fixedPartDrums $ RBFile.s_tracks song
              drums'
                = Map.fromList
                $ map (first $ realToFrac . U.applyTempoMap tempos)
                $ ATB.toPairList
                $ RTB.toAbsoluteEventList 0
                $ RTB.collectCoincident
                $ fmap RGDrums.Upcoming
                $ drumGems
                $ fromMaybe mempty
                $ Map.lookup Expert
                $ drumDifficulties drums
          yml <- loadYaml $ dir </> "song.yml"
          (pans, vols) <- case HM.toList $ _plans yml of
            [(_, MoggPlan{..})] -> return (map realToFrac _pans, map realToFrac _vols)
            _                   -> fatal "Couldn't find pans and vols after importing STFS"
          return (RGDrums.Track drums' Map.empty 0 0.2, pans, vols)
        case res of
          Left err  -> throwIO err
          Right (trk, pans, vols) -> do
            RGAudio.playMOGG pans vols (dir </> "audio.mogg") $ do
              RGDrums.playDrums window rend trk
      _ -> do
        draw
        SDL.present rend
        liftIO $ threadDelay 20000
        checkVars
        evts <- SDL.pollEvents
        processEvents evts >>= \b -> when b tick

    newSelect :: Maybe (Int, Int) -> Onyx ()
    newSelect mousePos = do
      choices <- getChoices
      GUIState{..} <- get
      SDL.V2 windW windH <- SDL.get $ SDL.windowSize window
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
              fromMenu = if 0 <= i && i < length choices
                then SelectMenu i
                else NoSelect
              brandX = windW - brandW - 10 - updateW - 10
              brandY = windH - updateH - 3 - versionH - 13
              in case previousScreens of
                [] -> if fromIntegral x >= brandX && fromIntegral y >= brandY
                  then SelectLogo
                  else fromMenu
                _ -> fromMenu

    cleanupPage :: Menu -> Onyx ()
    cleanupPage = \case
      Song proj _         -> mapM_ release $ projectRelease proj
      TasksRunning tid _  -> liftIO $ killThread tid
      _                   -> return ()

    doSelect :: Maybe (Int, Int) -> Onyx ()
    doSelect mousePos = do
      wasInt <- (\case EnterInt{} -> True; _ -> False) <$> gets currentScreen
      gs <- get
      case currentSelection gs of
        NoSelect -> return ()
        SelectPage i -> case drop i $ previousScreens gs of
          pm : pms -> do
            mapM_ cleanupPage $ currentScreen gs : take i (previousScreens gs)
            put gs
              { currentScreen = pm
              , previousScreens = pms
              , currentSelection = NoSelect
              }
          [] -> return ()
        SelectMenu i -> do
          choices <- getChoices
          choiceValue $ choices !! i
        SelectLogo -> osOpenFile "https://github.com/mtolly/onyxite-customs/releases"
      isInt <- (\case EnterInt{} -> True; _ -> False) <$> gets currentScreen
      unless (wasInt && isInt) $ newSelect mousePos

    goBack :: Onyx ()
    goBack = get >>= \gs -> case previousScreens gs of
      pm : pms -> do
        cleanupPage $ currentScreen gs
        put gs
          { currentScreen = pm
          , previousScreens = pms
          , currentSelection = SelectMenu 0
          }
      [] -> return ()

    -- | Returns 'False' if an exit event was processed.
    processEvents :: [SDL.Event] -> Onyx Bool
    processEvents [] = return True
    processEvents (e : es) = case SDL.eventPayload e of
      SDL.QuitEvent -> get >>= \gs -> do
        case currentScreen gs of
          TasksRunning tid _ -> liftIO $ killThread tid
          _                  -> return ()
        return False
      SDL.DropEvent (SDL.DropEventData cstr) -> do
        liftIO $ do
          -- IIUC, SDL2 guarantees the char* is utf-8 on all platforms
          str <- T.unpack . decodeUtf8 <$> B.packCString cstr
          Raw.free $ castPtr cstr
          void $ forkIO $ atomically $ writeTChan varEvents $ LoadFile str
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
      SDL.MouseButtonEvent SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Pressed
        , SDL.mouseButtonEventButton = SDL.ButtonX1 -- back button at least for me
        } -> do
          goBack
          processEvents es
      SDL.MouseWheelEvent SDL.MouseWheelEventData
        { SDL.mouseWheelEventPos = SDL.V2 _ dy
        , SDL.mouseWheelEventDirection = dir
        } -> do
          -- TODO readd scrolling
          processEvents es
      SDL.KeyboardEvent SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = ksym
        , SDL.keyboardEventRepeat = False
        } -> case SDL.keysymScancode ksym of
          SDL.ScancodeBackspace -> do
            goBack
            processEvents es
          SDL.ScancodeReturn -> do
            doSelect Nothing
            processEvents es
          SDL.ScancodeLeft -> do
            GUIState{..} <- get
            modifySelect $ \case
              SelectMenu i -> if null previousScreens then SelectMenu i else SelectPage 0
              SelectPage i -> SelectPage $ min (length previousScreens - 1) $ i + 1
              NoSelect     -> SelectMenu 0
              SelectLogo   -> SelectMenu 0
            processEvents es
          SDL.ScancodeRight -> do
            GUIState{..} <- get
            modifySelect $ \case
              sel@(SelectMenu _) -> case previousScreens of
                [] -> SelectLogo
                _  -> sel
              SelectPage 0 -> SelectMenu 0
              SelectPage i -> SelectPage $ i - 1
              NoSelect     -> SelectMenu 0
              sel@SelectLogo -> sel
            processEvents es
          SDL.ScancodeDown -> do
            choices <- getChoices
            GUIState{..} <- get
            modifySelect $ \case
              SelectMenu i       -> case previousScreens of
                [] -> if i == length choices - 1
                  then SelectLogo
                  else SelectMenu $ i + 1
                _ -> SelectMenu $ min (length choices - 1) $ i + 1
              NoSelect           -> SelectMenu 0
              sel@(SelectPage _) -> sel
              sel@SelectLogo     -> sel
            processEvents es
          SDL.ScancodeUp -> do
            choices <- getChoices
            modifySelect $ \case
              SelectMenu i       -> SelectMenu $ max 0 $ i - 1
              NoSelect           -> SelectMenu 0
              sel@(SelectPage _) -> sel
              SelectLogo         -> SelectMenu $ length choices - 1
            processEvents es
          _ -> processEvents es
      _ -> processEvents es

    addFile :: FilePath -> Onyx ()
    addFile fp = get >>= \case
      GUIState{ currentScreen = Files fpick useFiles, .. } -> do
        res <- errorToEither $ fileFilter fpick fp
        let fpick' = fpick
              { fileLoaded = case res of
                Left  _ -> fileLoaded fpick
                Right _ -> fileLoaded fpick ++ [fp]
              }
        stackIO $ case res of
          Left (Messages errs) -> forM_ errs $ \msg -> atomically $ writeTChan varEvents $ LogMessage (GUIFailure, msg)
          Right label -> atomically $ writeTChan varEvents
            $ LogMessage (GUILog, Message (unlines [fp, "  " <> T.unpack label]) [])
        setMenu $ Files fpick' useFiles
      GUIState{ currentScreen = SaveFile spick useFile } -> do
        setMenu $ SaveFile spick{ saveCurrent = T.pack fp } useFile
      _ -> return ()

    checkVars :: Onyx ()
    checkVars = do
      gs <- get
      liftIO (atomically $ tryReadTChan varEvents) >>= \case
        Nothing -> return ()
        Just e -> case e of
          LoadFile f -> addFile f
          SongImport proj -> pushMenu $ Song proj $ Choices
            [ ( Choice "Preview" "Produce a web browser app to preview the song."
              $ pushMenu $ SaveFile (SavePicker
                { savePatterns    = []
                , saveDescription = "Web app folder"
                , saveCurrent     = "" -- TODO fill with default _player dir
                }) $ \out -> pushMenu $ TasksStart $ (:[]) $ do
                  _ <- commandLine' ["player", projectLocation proj, "--to", out]
                  return [out </> "index.html"]
              )
            , ( Choice "Build" "Create a song file for a game."
              $ pushMenu $ Choices [(Choice "TODO" "Not built yet!" $ return ())]
              )
            , ( Choice "Utilities" "Extra authoring tools."
              $ pushMenu $ Choices
                [ ( Choice "Automatic difficulties" "Fill in a chart's missing lower difficulties." $ return () )
                , ( Choice "Hanging pro keys" "Find pro keys notes whose ranges are too close."
                  $ pushMenu $ TasksStart $ (:[]) $ do
                    commandLine' ["hanging", projectLocation proj]
                  )
                ]
              )
            ]
          LogStart -> put gs { currentLog = [] }
          LogMessage msg -> do
            let term = globalTerminal gs
            put gs
              { globalTerminal = term { terminalOutput = msg : terminalOutput term }
              , currentLog = msg : currentLog gs
              }
          TaskProgress progress -> case currentScreen gs of
            TasksRunning tid oldStatus -> let
              newStatus = oldStatus
                { tasksOK = case progress of
                  TaskOK{} -> tasksOK oldStatus + 1
                  _        -> tasksOK oldStatus
                , tasksFailed = case progress of
                  TaskFailed{} -> tasksFailed oldStatus + 1
                  _            -> tasksFailed oldStatus
                , tasksFiles = case progress of
                  TaskOK fs -> fs ++ tasksFiles oldStatus
                  _         -> tasksFiles oldStatus
                }
              in if tasksOK newStatus + tasksFailed newStatus == tasksTotal newStatus
                then do
                  useResultFiles $ tasksFiles newStatus
                  logFile <- liftIO $ do
                    logDir <- Dir.getXdgDirectory Dir.XdgCache "onyx-log"
                    Dir.createDirectoryIfMissing False logDir
                    time <- getCurrentTime
                    let logFile = logDir </> fmt time <.> "txt"
                        fmt = formatTime defaultTimeLocale $ iso8601DateFormat $ Just "%H%M%S"
                    path <- getEnv "PATH"
                    writeFile logFile $ unlines
                      $ "PATH:"
                      : path
                      : map showTerminal (reverse $ currentLog gs)
                    return logFile
                  put gs { currentScreen = TasksDone logFile newStatus }
                else put gs { currentScreen = TasksRunning tid newStatus }
            _ -> return ()

  void $ runResourceT $ evalStateT (logToSink (atomically . writeTChan varEvents) tick) $ initialState varEvents

class ShowTerminal a where
  showTerminal :: a -> String
  colorTerminal :: a -> Maybe (SDL.V4 Word8)
  colorTerminal _ = Nothing

instance ShowTerminal (GUIMessageLevel, Message) where
  showTerminal (GUILog, msg) = filter (/= '\r') $ messageString    msg
  showTerminal (_     , msg) = filter (/= '\r') $ displayException msg
  colorTerminal (lvl, _) = case lvl of
    GUISuccess -> Just $ SDL.V4 0 0x50 0 0xFF
    GUIFailure -> Just $ SDL.V4 0x50 0 0 0xFF
    GUIWarning -> Just $ SDL.V4 0x50 0x50 0 0xFF
    GUILog     -> Nothing

filterSong :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m T.Text
filterSong fp = identifyFile' fp >>= \(typ, fp') -> case typ of
  FileSTFS -> liftBracketLog (withSTFS fp') $ \contents -> do
    case [ getDTA | ("songs/songs.dta", getDTA) <- stfsFiles contents ] of
      getDTA : _ -> stackIO getDTA >>= useDTA "STFS"
      _          -> fatal "Could not locate songs/songs.dta"
  FileRBA -> getRBAFileBS 0 fp' >>= useDTA "RBA"
  FileChart -> do
    ini <- FB.chartToIni <$> FB.loadChartFile fp'
    return $ foundFile "dB" (fromMaybe "(no title)" $ FoF.name ini) (fromMaybe "(no artist)" $ FoF.artist ini)
  FilePS -> do
    ini <- FoF.loadSong fp'
    return $ foundFile "PS" (fromMaybe "(no title)" $ FoF.name ini) (fromMaybe "(no artist)" $ FoF.artist ini)
  _ -> fatal "Not a recognized song file"
  where useDTA filetype bs = readDTASingles (BL.toStrict bs) >>= \case
          [(DTASingle _ pkg _, _)] -> return $
            foundFile filetype (D.name pkg) (D.artist pkg)
          _ -> fatal "RB song packs not supported yet, coming soon!"
        foundFile typ title artist = "[" <> typ <> "] " <> title <> " (" <> artist <> ")"
