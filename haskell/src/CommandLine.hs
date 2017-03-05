{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CommandLine (commandLine) where

import           Build                          (loadYaml, shakeBuild)
import           Config
import           Control.Monad                  (forM_, guard, unless)
import           Control.Monad.Extra            (filterM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Lazy.Char8     ()
import           Data.Char                      (isAscii, isPrint, isSpace)
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.DTA.Lex                   (scanEither)
import           Data.DTA.Parse                 (parseEither)
import qualified Data.DTA.Serialize.Magma       as RBProj
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.DTA.Serialize2            as D
import           Data.Functor                   (void)
import           Data.Hashable                  (hash)
import qualified Data.HashMap.Strict            as Map
import           Data.List.HT                   (partitionMaybe)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Word                      (Word32)
import qualified Data.Yaml                      as Y
import           Import                         (importFoF, importRBA,
                                                 importSTFS, simpleRBAtoCON)
import           Magma                          (getRBAFile, oggToMogg,
                                                 runMagma, runMagmaV1, runMagmaMIDI)
import           MoggDecrypt                    (moggToOgg)
import           PrettyDTA                      (readRB3DTA)
import           ProKeysRanges                  (closeShiftsFile, completeFile)
import           Reaper.Build                   (makeReaperIO)
import           Reductions                     (simpleReduce)
import qualified RockBand.File                  as RBFile
import qualified Sound.Jammit.Base              as J
import qualified Sound.MIDI.File.Load           as Load
import qualified Sound.MIDI.File.Save           as Save
import qualified Sound.MIDI.Script.Base         as MS
import qualified Sound.MIDI.Script.Parse        as MS
import qualified Sound.MIDI.Script.Read         as MS
import qualified Sound.MIDI.Script.Scan         as MS
import           STFS.Extract                   (extractSTFS)
import           System.Console.GetOpt
import qualified System.Directory               as Dir
import           System.FilePath                (dropTrailingPathSeparator,
                                                 takeDirectory, takeExtension,
                                                 takeFileName, (-<.>), (</>))
import           System.Info                    (os)
import qualified System.IO                      as IO
import           System.IO.Error                (tryIOError)
import           System.Process                 (callProcess, spawnCommand)
import           Text.Printf                    (printf)
import           X360                           (rb2pkg, rb3pkg, stfsFolder)

#ifdef WINDOWS
import           Data.Bits                      (testBit)
import           System.Win32.File              (getLogicalDrives)
#else
import           Data.List                      (isPrefixOf)
import           System.MountPoints
#endif

loadDTA :: (D.DTASerialize a, MonadIO m) => FilePath -> StackTraceT m a
loadDTA f = inside f $ liftIO (tryIOError $ T.readFile f) >>= \case
  Left err -> fatal $ show err
  Right txt -> do
    toks <- either fatal return $ scanEither txt
    dta <- either fatal return $ parseEither toks
    D.unserialize D.format dta

firstPresentTarget :: (MonadIO m) => FilePath -> [T.Text] -> StackTraceT m T.Text
firstPresentTarget yamlPath targets = do
  songYaml <- loadYaml yamlPath
  case filter (`elem` Map.keys (_targets songYaml)) targets of
    []    -> fail $ "panic! couldn't find any of these targets: " ++ show targets
    t : _ -> return t

getInfoForSTFS :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m (T.Text, T.Text)
getInfoForSTFS dir stfs = do
  let dta = dir </> "songs/songs.dta"
  ex <- liftIO $ Dir.doesFileExist dta
  fromMaybe (T.pack $ takeFileName stfs, T.pack stfs) <$> if ex
    then errorToWarning $ do
      (_, pkg, _) <- readRB3DTA dta
      return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
    else return Nothing

installSTFS :: (MonadIO m) => FilePath -> FilePath -> m ()
installSTFS stfs usb = liftIO $ do
  (titleID, sign) <- stfsFolder stfs
  stfsHash <- take 10 . show . MD5.md5 <$> BL.readFile stfs
  let folder = "Content/0000000000000000" </> w32 titleID </> w32 sign
      w32 :: Word32 -> FilePath
      w32 = printf "%08x"
      file = "onyx_" ++ stfsHash
  Dir.copyFile stfs $ usb </> folder </> file

findXbox360USB :: (MonadIO m) => m [FilePath]
findXbox360USB = liftIO $ do
#ifdef WINDOWS
  dword <- getLogicalDrives
  let drives = [ letter : ":\\" | (letter, i) <- zip ['A'..'Z'] [0..], dword `testBit` i ]
#else
  mnts <- getMounts
  let drives = map mnt_dir $ flip filter mnts $ \mnt ->
        ("/dev/" `isPrefixOf` mnt_fsname mnt) && (mnt_dir mnt /= "/")
#endif
  filterM (\drive -> Dir.doesDirectoryExist $ drive </> "Content") drives

makeFilename :: T.Text -> T.Text -> FilePath
makeFilename title artist = let
  hashed = hash (title, artist) `mod` 10000000
  safeInfo = T.filter (\c -> isPrint c && isAscii c && not (isSpace c)) $ title <> artist
  in take 32 $ "o" <> show hashed <> "_" <> T.unpack safeInfo

copyDirRecursive :: (MonadIO m) => FilePath -> FilePath -> m ()
copyDirRecursive src dst = liftIO $ do
  Dir.createDirectoryIfMissing False dst
  ents <- Dir.listDirectory src
  forM_ ents $ \ent -> do
    let pathFrom = src </> ent
        pathTo = dst </> ent
    isDir <- Dir.doesDirectoryExist pathFrom
    if isDir
      then copyDirRecursive pathFrom pathTo
      else Dir.copyFile     pathFrom pathTo

readConfig :: (MonadIO m) => StackTraceT m (Map.HashMap T.Text Y.Value)
readConfig = do
  cfg <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  liftIO (Dir.doesFileExist cfg) >>= \case
    False -> return Map.empty
    True  -> liftIO (Y.decodeFileEither cfg) >>= \case
      Left err -> fatal $ show err
      Right x  -> return x

osOpenFile :: (MonadIO m) => FilePath -> m ()
osOpenFile f = liftIO $ case os of
  "mingw32" -> void $ spawnCommand f
  "darwin"  -> callProcess "open" [f]
  "linux"   -> callProcess "exo-open" [f]
  _         -> return ()

data Command = Command
  { commandWord  :: T.Text
  , commandRun   :: forall m. (MonadIO m) => [FilePath] -> [OnyxOption] -> StackTraceT m ()
  , commandDesc  :: T.Text
  , commandUsage :: T.Text
  }

identifyFile :: (MonadIO m) => FilePath -> m FileResult
identifyFile fp = liftIO $ Dir.doesFileExist fp >>= \case
  True -> case takeExtension fp of
    ".yml" -> return $ FileType FileSongYaml fp
    ".rbproj" -> return $ FileType FileRBProj fp
    ".midtxt" -> return $ FileType FileMidiText fp
    ".mogg" -> return $ FileType FileMOGG fp
    ".zip" -> return $ FileType FileZip fp
    _ -> case takeFileName fp of
      "song.ini" -> return $ FileType FilePS fp
      _ -> do
        magic <- IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
        case magic of
          "RBSF" -> return $ FileType FileRBA fp
          "CON " -> return $ FileType FileSTFS fp
          "LIVE" -> return $ FileType FileSTFS fp
          "MThd" -> return $ FileType FileMidi fp
          "fLaC" -> return $ FileType FileFLAC fp
          "OggS" -> return $ FileType FileOGG fp
          "RIFF" -> return $ FileType FileWAV fp
          _      -> return FileUnrecognized
  False -> Dir.doesDirectoryExist fp >>= \case
    True -> Dir.doesFileExist (fp </> "song.yml") >>= \case
      True -> return $ FileType FileSongYaml $ fp </> "song.yml"
      False -> Dir.doesFileExist (fp </> "song.ini") >>= \case
        True -> return $ FileType FilePS $ fp </> "song.ini"
        False -> do
          ents <- Dir.listDirectory fp
          case filter (\ent -> takeExtension ent == ".rbproj") ents of
            [ent] -> return $ FileType FileRBProj $ fp </> ent
            _     -> return FileUnrecognized
    False -> return FileDoesNotExist

data FileResult
  = FileType FileType FilePath
  | FileDoesNotExist
  | FileUnrecognized
  deriving (Eq, Ord, Show, Read)

data FileType
  = FileSongYaml
  | FileSTFS
  | FileRBA
  | FilePS
  | FileMidi
  | FileMidiText
  | FileRBProj
  | FileOGG
  | FileMOGG
  | FileFLAC
  | FileWAV
  | FileZip
  deriving (Eq, Ord, Show, Read)

identifyFile' :: (MonadIO m) => FilePath -> StackTraceT m (FileType, FilePath)
identifyFile' file = identifyFile file >>= \case
  FileDoesNotExist     -> fatal $ "Path does not exist: " <> file
  FileUnrecognized     -> fatal $ "File/directory type not recognized: " <> file
  FileType ftype fpath -> return (ftype, fpath)

optionalFile :: (MonadIO m) => [FilePath] -> StackTraceT m (FileType, FilePath)
optionalFile files = case files of
  []     -> identifyFile' "."
  [file] -> identifyFile' file
  _      -> fatal $ "Command expected 0 or 1 arguments, given " <> show (length files)

unrecognized :: (Monad m) => FileType -> FilePath -> StackTraceT m a
unrecognized ftype fpath = fatal $ "Unsupported file for command (" <> show ftype <> "): " <> fpath

getAudioDirs :: (MonadIO m) => FilePath -> StackTraceT m [FilePath]
getAudioDirs yamlPath = do
  config <- readConfig
  jmt <- liftIO J.findJammitDir
  let addons = maybe id (:) jmt [takeDirectory yamlPath]
  case A.parseEither (.: "audio-dirs") config of
    Left _ -> return $ addons ++ []
    Right obj -> do
      dirs <- either fatal return $ A.parseEither A.parseJSON obj
      mapM (liftIO . Dir.canonicalizePath) $ addons ++ dirs

outputFile :: (Monad m) => [OnyxOption] -> m FilePath -> m FilePath
outputFile opts dft = case [ to | OptTo to <- opts ] of
  []     -> dft
  to : _ -> return to

buildTarget :: (MonadIO m) => FilePath -> [OnyxOption] -> StackTraceT m (Target, FilePath)
buildTarget yamlPath opts = do
  songYaml <- loadYaml yamlPath
  targetName <- case [ t | OptTarget t <- opts ] of
    []    -> fatal "command requires --target, none given"
    t : _ -> return t
  audioDirs <- getAudioDirs yamlPath
  target <- case Map.lookup targetName $ _targets songYaml of
    Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
    Just target -> return target
  let built = case target of
        RB3{} -> "gen/target" </> T.unpack targetName </> "rb3con"
        RB2{} -> "gen/target" </> T.unpack targetName </> "rb2con"
        PS {} -> "gen/target" </> T.unpack targetName </> "ps.zip"
  shakeBuild audioDirs yamlPath [built]
  return (target, takeDirectory yamlPath </> built)

getKeysRB2 :: [OnyxOption] -> KeysRB2
getKeysRB2 opts
  | elem OptKeysOnGuitar opts = KeysGuitar
  | elem OptKeysOnBass   opts = KeysBass
  | otherwise                 = NoKeys

getPlanName :: (MonadIO m) => FilePath -> [OnyxOption] -> StackTraceT m T.Text
getPlanName yamlPath opts = case [ p | OptPlan p <- opts ] of
  p : _ -> return p
  []    -> do
    songYaml <- loadYaml yamlPath
    case Map.keys $ _plans songYaml of
      [p]   -> return p
      plans -> fatal $ "No --plan given, and YAML file doesn't have exactly 1 plan: " <> show plans

getInputMIDI :: (MonadIO m) => [FilePath] -> StackTraceT m FilePath
getInputMIDI files = optionalFile files >>= \case
  (FileSongYaml, yamlPath) -> return $ takeDirectory yamlPath </> "notes.mid"
  (FileRBProj, rbprojPath) -> do
    rbproj <- loadDTA rbprojPath
    return $ T.unpack $ RBProj.midiFile $ RBProj.midi $ RBProj.project rbproj
  (FileMidi, mid) -> return mid
  (FilePS, ini) -> return $ takeDirectory ini </> "notes.mid"
  (ftype, fpath) -> unrecognized ftype fpath

undone :: (Monad m) => StackTraceT m ()
undone = fatal "Feature not built yet..."

commands :: [Command]
commands =

  [ Command
    { commandWord = "build"
    , commandDesc = "Compile a onyx/Magma project."
    , commandUsage = T.unlines
      [ "onyx build --target rb3 --to new_rb3con"
      , "onyx build --target ps --to new_ps.zip"
      , "onyx build mycoolsong.yml --target rb3 --to new_rb3con"
      , "onyx build song.rbproj"
      , "onyx build song.rbproj --to new.rba"
      -- , "onyx build song.rbproj --to new_rb3con"
      ]
    , commandRun = \files opts -> optionalFile files >>= \case
      (FileSongYaml, yamlPath) -> do
        out <- outputFile opts $ fatal "onyx build (yaml) requires --to, none given"
        (_, built) <- buildTarget yamlPath opts
        liftIO $ Dir.copyFile built out
      (FileRBProj, rbprojPath) -> do
        rbproj <- loadDTA rbprojPath
        out <- outputFile opts $ return $ T.unpack $ RBProj.destinationFile $ RBProj.project rbproj
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        if isMagmaV2
          then runMagma   rbprojPath out >>= liftIO . putStrLn
          else runMagmaV1 rbprojPath out >>= liftIO . putStrLn
        -- TODO: handle CON conversion
      (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "shake"
    , commandDesc = "For debug use only."
    , commandUsage = "onyx file mysong.yml file1 [file2...]"
    , commandRun = \files _opts -> case files of
      [] -> return ()
      yml : builds -> identifyFile' yml >>= \case
        (FileSongYaml, yml') -> do
          audioDirs <- getAudioDirs yml
          shakeBuild audioDirs yml' builds
        (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "install"
    , commandDesc = "Install a song to a USB drive or game folder."
    , commandUsage = ""
    , commandRun = \files opts -> let
      doInstall ftype fpath = case ftype of
        FileSongYaml -> do
          (target, built) <- buildTarget fpath opts
          let ftype' = case target of
                PS {} -> FileZip
                RB3{} -> FileSTFS
                RB2{} -> FileSTFS
          doInstall ftype' built
        FileRBProj -> undone -- install con to usb drive
        FileSTFS -> do
          drive <- outputFile opts $ findXbox360USB >>= \case
            [d] -> return d
            [ ] -> fatal "onyx install (stfs): no Xbox 360 USB drives found"
            _   -> fatal "onyx install (stfs): more than 1 Xbox 360 USB drive found"
          liftIO $ installSTFS fpath drive
        FileRBA -> undone -- convert to con, install to usb drive
        FilePS -> undone -- install to PS music dir
        FileZip -> undone -- install to PS music dir
        _ -> unrecognized ftype fpath
      in optionalFile files >>= uncurry doInstall
    }

  , Command
    { commandWord = "reap"
    , commandDesc = "Open a MIDI file (with audio) in REAPER."
    , commandUsage = ""
    , commandRun = \files opts -> do
      files' <- mapM identifyFile' $ case files of
        [] -> ["."]
        _  -> files
      let isType types (ftype, fpath) = guard (elem ftype types) >> Just fpath
      case files' of
        [(FileSongYaml, yamlPath)] -> do
          audioDirs <- getAudioDirs yamlPath
          planName <- getPlanName yamlPath opts
          let rpp = "notes-" <> T.unpack planName <> ".RPP"
              yamlDir = takeDirectory yamlPath
          shakeBuild audioDirs yamlPath [rpp]
          let rppFull = yamlDir </> "notes.RPP"
          liftIO $ Dir.renameFile (yamlDir </> rpp) rppFull
          unless (elem OptNoOpen opts) $ osOpenFile rppFull
        _ -> case partitionMaybe (isType [FileMidi]) files' of
          ([mid], notMid) -> case partitionMaybe (isType [FileOGG, FileWAV, FileFLAC]) notMid of
            (audio, []      ) -> do
              rpp <- outputFile opts $ return $ mid -<.> "RPP"
              makeReaperIO mid mid audio rpp
            (_    , notAudio) -> fatal $ "onyx reap given non-MIDI, non-audio files: " <> show notAudio
          (mids, _) -> fatal $ "onyx reap expected 1 MIDI file, given " <> show (length mids)
    }

  , Command
    { commandWord = "new"
    , commandDesc = "Start a new project from an audio file."
    , commandUsage = "onyx new song.flac"
    , commandRun = \_ _ -> undone
    }

  , Command
    { commandWord = "player"
    , commandDesc = "Create a web browser chart playback app."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileSongYaml -> do
        audioDirs <- getAudioDirs fpath
        planName <- getPlanName fpath opts
        let player = "gen/plan" </> T.unpack planName </> "web"
        shakeBuild audioDirs fpath [player]
        player' <- case [ to | OptTo to <- opts ] of
          []      -> return player
          out : _ -> liftIO $ do
            Dir.createDirectoryIfMissing False out
            copyDirRecursive player out
            return out
        unless (elem OptNoOpen opts) $ osOpenFile $ player' </> "index.html"
      FileRBProj -> undone
      FileSTFS -> tempDir "onyx_player" $ \tmp -> do
        out <- outputFile opts $ return $ fpath ++ "_player"
        importSTFS NoKeys fpath tmp
        let player = "gen/plan/mogg/web"
        shakeBuild [tmp] (tmp </> "song.yml") [player]
        liftIO $ Dir.createDirectoryIfMissing False out
        copyDirRecursive (tmp </> player) out
        unless (elem OptNoOpen opts) $ osOpenFile $ out </> "index.html"
      FileRBA -> undone
      FilePS -> undone
      FileMidi -> undone
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "check"
    , commandDesc = "Check for any authoring errors in a project."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \case
      (FileSongYaml, yamlPath) -> do
        targetName <- case [ t | OptTarget t <- opts ] of
          []    -> fatal "command requires --target, none given"
          t : _ -> return t
        -- TODO: handle non-RB3 targets
        let built = "gen/target" </> T.unpack targetName </> "notes-magma-export.mid"
        shakeBuild [] yamlPath [built]
      (FileRBProj, rbprojPath) -> do
        rbproj <- loadDTA rbprojPath
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        tempDir "onyx_check" $ \tmp -> if isMagmaV2
          then runMagmaMIDI rbprojPath (tmp </> "out.mid") >>= liftIO . putStrLn
          else undone
      (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "readme"
    , commandDesc = "Generate a GitHub README file for a song."
    , commandUsage = ""
    , commandRun = \files _opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileSongYaml -> shakeBuild [] fpath ["update-readme"]
      _            -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "import"
    , commandDesc = "Import a file into onyx's project format."
    , commandUsage = ""
    -- TODO: support --2x
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileRBProj -> undone
      FileSTFS -> do
        out <- outputFile opts $ return $ fpath ++ "_import"
        liftIO $ Dir.createDirectoryIfMissing False out
        importSTFS (getKeysRB2 opts) fpath out
      FileRBA -> do
        out <- outputFile opts $ return $ fpath ++ "_import"
        liftIO $ Dir.createDirectoryIfMissing False out
        importRBA (getKeysRB2 opts) fpath out
      FilePS -> do
        out <- outputFile opts $ return $ takeDirectory fpath ++ "_import"
        liftIO $ Dir.createDirectoryIfMissing False out
        importFoF (getKeysRB2 opts) (takeDirectory fpath) out
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "convert"
    , commandDesc = "Convert a song file to a different game."
    , commandUsage = T.unlines
      [ "onyx convert in.rba --to out_rb3con --game rb3"
      , "onyx convert in_rb3con --to out_rb2con --game rb2"
      , "onyx convert in_rb3con --to out_rb2con --game rb2 --keys-on-guitar"
      , "onyx convert in_rb3con --to out_rb2con --game rb2 --keys-on-bass"
      ]
    , commandRun = \files opts -> tempDir "onyx_convert" $ \tmp -> do
      (ftype, fpath) <- optionalFile files
      let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
          conSuffix = case game of GameRB3 -> "rb3con"; GameRB2 -> "rb2con"
      if game == GameRB3 && ftype == FileRBA
        then do
          -- TODO make sure that the RBA is actually RB3 not RB2
          out <- outputFile opts $ return $ fpath ++ "_rb3con"
          simpleRBAtoCON fpath out >>= liftIO . putStrLn
        else do
          case ftype of
            FileSTFS -> importSTFS (getKeysRB2 opts) fpath tmp
            FileRBA  -> importRBA (getKeysRB2 opts) fpath tmp
            FilePS   -> importFoF (getKeysRB2 opts) (takeDirectory fpath) tmp
            FileZip  -> undone
            _        -> unrecognized ftype fpath
          targetName <- firstPresentTarget (tmp </> "song.yml") $ case game of
            GameRB3 -> ["rb3-2x", "rb3"]
            GameRB2 -> ["rb2-2x", "rb2"]
          let con = "gen/target" </> T.unpack targetName </> conSuffix
          out <- outputFile opts $ do
            fpathAbs <- liftIO $ Dir.makeAbsolute fpath
            return $ dropTrailingPathSeparator fpathAbs <> "_" <> conSuffix
          shakeBuild [tmp] (tmp </> "song.yml") [con]
          liftIO $ Dir.copyFile (tmp </> con) out
    }

  , Command
    { commandWord = "hanging"
    , commandDesc = "Find pro keys range shifts with hanging notes."
    , commandUsage = T.unlines
      [ "onyx hanging mysong.yml"
      , "onyx hanging notes.mid"
      ]
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> let
      withMIDI mid = liftIO (Load.fromFile mid) >>= RBFile.readMIDIFile >>= liftIO . putStrLn . closeShiftsFile
      in case ftype of
        FileSongYaml -> do
          audioDirs <- getAudioDirs fpath
          planName <- getPlanName fpath opts
          shakeBuild audioDirs fpath ["gen/plan" </> T.unpack planName </> "hanging"]
        FileRBProj   -> do
          rbproj <- loadDTA fpath
          let midPath = T.unpack $ RBProj.midiFile $ RBProj.midi $ RBProj.project rbproj
          withMIDI (takeDirectory fpath </> midPath)
        FileSTFS     -> undone
        FileRBA      -> tempDir "onyx_hanging_rba" $ \tmp -> do
          let midPath = tmp </> "notes.mid"
          getRBAFile 1 fpath midPath
          withMIDI midPath
        FilePS       -> withMIDI $ takeDirectory fpath </> "notes.mid"
        FileMidi     -> withMIDI fpath
        _            -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "reduce"
    , commandDesc = "Fill in missing difficulties in a MIDI file."
    , commandUsage = T.unlines
      [ "onyx reduce [notes.mid|song.yml|magma.rbproj|song.ini]"
      , "# or run on current directory"
      , "onyx reduce"
      , "# by default, modifies the midi in-place. or specify --out"
      , "onyx reduce notes.mid --to reduced.mid"
      ]
    , commandRun = \files opts -> do
      mid <- getInputMIDI files
      out <- outputFile opts $ return mid
      simpleReduce mid out
    }

  , Command
    { commandWord = "ranges"
    , commandDesc = "Generate an automatic set of pro keys range shifts."
    , commandUsage = T.unlines
      [ "onyx ranges [notes.mid|song.yml|magma.rbproj]"
      , "# or run on current directory"
      , "onyx ranges"
      , "# by default, modifies the midi in-place. or specify --out"
      , "onyx ranges notes.mid --to ranges.mid"
      ]
    , commandRun = \files opts -> do
      mid <- getInputMIDI files
      out <- outputFile opts $ return mid
      completeFile mid out
    }

  , Command
    { commandWord = "stfs"
    , commandDesc = "Compile a folder's contents into an Xbox 360 STFS file."
    , commandUsage = T.unlines
      [ "onyx stfs my_folder"
      , "onyx stfs my_folder --to new_rb3con"
      , "onyx stfs my_folder --to new_rb3con --game rb3"
      , "onyx stfs my_folder --to new_rb3con --game rb2"
      ]
    , commandRun = \files opts -> case files of
      [dir] -> liftIO (Dir.doesFileExist dir) >>= \case
        True -> do
          let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
              suffix = case game of GameRB3 -> "_rb3con"; GameRB2 -> "_rb2con"
              pkg    = case game of GameRB3 -> rb3pkg   ; GameRB2 -> rb2pkg
          stfs <- outputFile opts $ (++ suffix) . dropTrailingPathSeparator <$> liftIO (Dir.makeAbsolute dir)
          (title, desc) <- getInfoForSTFS dir stfs
          pkg title desc dir stfs >>= liftIO . putStrLn
        False -> fatal $ "onyx stfs expected directory; given: " <> dir
      _ -> fatal $ "onyx stfs expected 1 argument, given " <> show (length files)
    }

  , Command
    { commandWord = "unstfs"
    , commandDesc = "Extract the contents of an Xbox 360 STFS file."
    , commandUsage = T.unlines
      [ "onyx stfs song_rb3con"
      , "onyx stfs song_rb3con --to a_folder"
      ]
    , commandRun = \files opts -> mapM identifyFile' files >>= \case
      [(FileSTFS, stfs)] -> do
        out <- outputFile opts $ return $ stfs ++ "_extract"
        liftIO $ extractSTFS stfs out
      _ -> fatal $ "onyx unstfs expected a single STFS argument, given: " <> show files
    }

  , Command
    { commandWord = "midi-text"
    , commandDesc = "Convert a MIDI file to/from a plaintext format."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileMidi -> do
        out <- outputFile opts $ return $ fpath -<.> "midtxt"
        res <- liftIO $ MS.toStandardMIDI <$> Load.fromFile fpath
        case res of
          Left  err -> fatal err
          Right sm  -> liftIO $ writeFile out $ MS.showStandardMIDI (midiOptions opts) sm
      FileMidiText -> do
        out <- outputFile opts $ return $ fpath -<.> "mid"
        sf <- liftIO $ MS.readStandardFile . MS.parse . MS.scan <$> readFile fpath
        let (mid, warnings) = MS.fromStandardMIDI (midiOptions opts) sf
        mapM_ warn warnings
        liftIO $ Save.toFile out mid
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "midi-text-git"
    , commandDesc = "Convert a MIDI file to a plaintext format. (for git use)"
    , commandUsage = "onyx midi-text-git in.mid > out.midtxt"
    , commandRun = \files opts -> case files of
      [mid] -> do
        res <- liftIO $ MS.toStandardMIDI <$> Load.fromFile mid
        case res of
          Left  err -> fatal err
          Right sm  -> liftIO $ putStrLn $ MS.showStandardMIDI (midiOptions opts) sm
      _ -> fatal $ "onyx midi-text-git expected 1 argument, given " <> show (length files)
    }

  , Command
    { commandWord = "mogg"
    , commandDesc = "Add or remove a MOGG header to an OGG Vorbis file."
    , commandUsage = T.unlines
      [ "onyx mogg in.ogg --to out.mogg"
      , "onyx mogg in.mogg --to out.ogg"
      ]
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileOGG -> do
        mogg <- outputFile opts $ return $ fpath -<.> "mogg"
        oggToMogg fpath mogg
      FileMOGG -> do
        ogg <- outputFile opts $ return $ fpath -<.> "ogg"
        moggToOgg fpath ogg
      _ -> unrecognized ftype fpath
    }

  ]

commandLine :: (MonadIO m) => [String] -> StackTraceT m ()
commandLine args = let
  (opts, nonopts, errors) = getOpt Permute optDescrs args
  printIntro = liftIO $ mapM_ T.putStrLn
    [ "Onyxite's Rock Band Custom Song Toolkit"
    , ""
    , "Usage: onyx [command] [files] [options]"
    , "Commands: " <> T.unwords (map commandWord commands)
    , "Run `onyx [command] --help` for further instructions."
    , ""
    ]
  in case errors of
    [] -> case nonopts of
      [] -> if elem OptHelp opts
        then printIntro
        else printIntro >> fatal "No command given."
      cmd : nonopts' -> case [ c | c <- commands, commandWord c == T.pack cmd ] of
        [c] -> inside ("command: onyx " <> cmd) $ if elem OptHelp opts
          then liftIO $ mapM_ T.putStrLn
            [ "onyx " <> commandWord c
            , commandDesc c
            , ""
            , "Usage:"
            , commandUsage c
            ]
          else commandRun c nonopts' opts
        _ -> printIntro >> fatal ("Unrecognized command: " ++ show cmd)
    _ -> inside "command line parsing" $ do
      printIntro
      throwError $ Messages [ Message e [] | e <- errors ]

optDescrs :: [OptDescr OnyxOption]
optDescrs =
  [ Option []   ["target"        ] (ReqArg (OptTarget . T.pack)   "TARGET"   ) ""
  , Option []   ["plan"          ] (ReqArg (OptPlan   . T.pack)   "PLAN"     ) ""
  , Option []   ["to"            ] (ReqArg OptTo                  "PATH"     ) ""
  , Option []   ["no-open"       ] (NoArg  OptNoOpen                         ) ""
  , Option []   ["2x"            ] (ReqArg Opt2x                  "PATH"     ) ""
  , Option []   ["game"          ] (ReqArg (OptGame . readGame)   "{rb3,rb2}") ""
  , Option []   ["separate-lines"] (NoArg  OptSeparateLines                  ) ""
  , Option []   ["in-beats"      ] (NoArg  OptInBeats                        ) ""
  , Option []   ["in-seconds"    ] (NoArg  OptInSeconds                      ) ""
  , Option []   ["in-measures"   ] (NoArg  OptInMeasures                     ) ""
  , Option []   ["match-notes"   ] (NoArg  OptMatchNotes                     ) ""
  , Option []   ["resolution"    ] (ReqArg (OptResolution . read) "int"      ) ""
  , Option []   ["keys-on-guitar"] (NoArg  OptKeysOnGuitar                   ) ""
  , Option []   ["keys-on-bass"  ] (NoArg  OptKeysOnBass                     ) ""
  , Option "h?" ["help"          ] (NoArg  OptHelp                           ) ""
  ] where
    readGame = \case
      "rb3" -> GameRB3
      "rb2" -> GameRB2
      g     -> error $ "Unrecognized --game value: " ++ show g

data OnyxOption
  = OptTarget T.Text
  | OptPlan T.Text
  | OptTo FilePath
  | OptNoOpen
  | Opt2x FilePath
  | OptGame Game
  | OptSeparateLines
  | OptInBeats
  | OptInSeconds
  | OptInMeasures
  | OptResolution Integer
  | OptMatchNotes
  | OptKeysOnGuitar
  | OptKeysOnBass
  | OptHelp
  deriving (Eq, Ord, Show, Read)

data Game
  = GameRB3
  | GameRB2
  deriving (Eq, Ord, Show, Read)

midiOptions :: [OnyxOption] -> MS.Options
midiOptions opts = MS.Options
  { MS.showFormat = if
    | elem OptInBeats opts -> MS.ShowBeats
    | elem OptInSeconds opts -> MS.ShowSeconds
    | elem OptInMeasures opts -> MS.ShowMeasures
    | otherwise -> MS.ShowBeats
  , MS.resolution = Just $ fromMaybe 480 $ listToMaybe [ r | OptResolution r <- opts ]
  , MS.separateLines = elem OptSeparateLines opts
  , MS.matchNoteOff = elem OptMatchNotes opts
  }
