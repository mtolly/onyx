{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CommandLine (commandLine) where

import           Build                          (loadYaml, shakeBuildFiles,
                                                 shakeBuildTarget)
import           Config
import           Control.Applicative            (liftA2)
import           Control.Monad                  (forM_, guard)
import           Control.Monad.Extra            (filterM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Lazy.Char8     ()
import           Data.Char                      (isAlphaNum, isAscii)
import           Data.Default.Class             (def)
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.DTA.Lex                   (scanStack)
import           Data.DTA.Parse                 (parseStack)
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.Magma       as RBProj
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Functor                   (void)
import qualified Data.HashMap.Strict            as Map
import           Data.List.HT                   (partitionMaybe)
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeUtf16BE)
import qualified Data.Text.IO                   as T
import           Data.Word                      (Word32)
import qualified Data.Yaml                      as Y
import           Import                         (HasKicks (..), importFoF,
                                                 importRBA, importSTFS,
                                                 simpleRBAtoCON)
import           Magma                          (getRBAFile, oggToMogg,
                                                 runMagma, runMagmaMIDI,
                                                 runMagmaV1)
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
                                                 splitFileName, takeDirectory,
                                                 takeExtension, takeFileName,
                                                 (-<.>), (</>))
import qualified System.IO                      as IO
import           Text.Printf                    (printf)
import           Text.Read                      (readMaybe)
import           X360DotNet                     (rb2pkg, rb3pkg, stfsFolder)

#ifdef WINDOWS
import           Data.Bits                      (testBit)
import           System.Win32.File              (getLogicalDrives)
#else
import           Data.List                      (isPrefixOf)
import           System.MountPoints
#endif

loadDTA :: (D.StackChunks a, MonadIO m) => FilePath -> StackTraceT m a
loadDTA f = inside f $
  stackIO (T.readFile f) >>= scanStack >>= parseStack >>= D.unserialize D.stackChunks
  -- TODO I don't think this handles utf8/latin1 properly

getInfoForSTFS :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m (T.Text, T.Text)
getInfoForSTFS dir stfs = do
  let dta = dir </> "songs/songs.dta"
  ex <- stackIO $ Dir.doesFileExist dta
  fromMaybe (T.pack $ takeFileName stfs, T.pack stfs) <$> if ex
    then errorToWarning $ do
      (_, pkg, _) <- readRB3DTA dta
      return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
    else return Nothing

installSTFS :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
installSTFS stfs usb = do
  (titleID, sign) <- stfsFolder stfs
  packageTitle <- stackIO $ IO.withBinaryFile stfs IO.ReadMode $ \h -> do
    IO.hSeek h IO.AbsoluteSeek 0x411
    bs <- B.hGet h 0x80
    return $ T.takeWhile (/= '\0') $ decodeUtf16BE bs
  stfsHash <- stackIO $ show . MD5.md5 <$> BL.readFile stfs
  let folder = "Content/0000000000000000" </> w32 titleID </> w32 sign
      w32 :: Word32 -> FilePath
      w32 = printf "%08x"
      file = take 32 $ "o" ++ take 7 stfsHash ++ T.unpack
        (T.filter (\c -> isAscii c && isAlphaNum c) packageTitle)
  stackIO $ Dir.createDirectoryIfMissing True $ usb </> folder
  stackIO $ Dir.copyFile stfs $ usb </> folder </> file

findXbox360USB :: (MonadIO m) => StackTraceT m [FilePath]
findXbox360USB = stackIO $ do
#ifdef WINDOWS
  dword <- getLogicalDrives
  let drives = [ letter : ":\\" | (letter, i) <- zip ['A'..'Z'] [0..], dword `testBit` i ]
#else
  mnts <- getMounts
  let drives = map mnt_dir $ flip filter mnts $ \mnt ->
        ("/dev/" `isPrefixOf` mnt_fsname mnt) && (mnt_dir mnt /= "/")
#endif
  filterM (\drive -> Dir.doesDirectoryExist $ drive </> "Content") drives

copyDirRecursive :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
copyDirRecursive src dst = do
  stackIO $ Dir.createDirectoryIfMissing False dst
  ents <- stackIO $ Dir.listDirectory src
  forM_ ents $ \ent -> do
    let pathFrom = src </> ent
        pathTo = dst </> ent
    isDir <- stackIO $ Dir.doesDirectoryExist pathFrom
    if isDir
      then copyDirRecursive       pathFrom pathTo
      else stackIO $ Dir.copyFile pathFrom pathTo

readConfig :: (MonadIO m) => StackTraceT m (Map.HashMap T.Text Y.Value)
readConfig = do
  cfg <- stackIO $ Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  stackIO (Dir.doesFileExist cfg) >>= \case
    False -> return Map.empty
    True  -> stackIO (Y.decodeFileEither cfg) >>= \case
      Left err -> fatal $ show err
      Right x  -> return x

data Command = Command
  { commandWord  :: T.Text
  , commandRun   :: forall m. (MonadIO m) => [FilePath] -> [OnyxOption] -> StackTraceT m [FilePath]
  , commandDesc  :: T.Text
  , commandUsage :: T.Text
  }

identifyFile :: (MonadIO m) => FilePath -> StackTraceT m FileResult
identifyFile fp = stackIO $ Dir.doesFileExist fp >>= \case
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
  jmt <- stackIO J.findJammitDir
  let addons = maybe id (:) jmt [takeDirectory yamlPath]
  case A.parseEither (.: "audio-dirs") config of
    Left _ -> return $ addons ++ []
    Right obj -> do
      dirs <- either fatal return $ A.parseEither A.parseJSON obj
      mapM (stackIO . Dir.canonicalizePath) $ addons ++ dirs

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
  shakeBuildFiles audioDirs yamlPath [built]
  return (target, takeDirectory yamlPath </> built)

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

undone :: (Monad m) => StackTraceT m a
undone = fatal "Feature not built yet..."

-- | Ensure that a \"dir/original-file_suffix\" filename stays within a length limit
-- by trimming \"original-file\".
trimFileName :: FilePath -> Int -> String -> FilePath
trimFileName fp len sfx = let
  (dir, file) = splitFileName fp
  in dir </> take (len - length sfx) file ++ sfx

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
        stackIO $ Dir.copyFile built out
        return [out]
      (FileRBProj, rbprojPath) -> do
        rbproj <- loadDTA rbprojPath
        out <- outputFile opts $ return $ T.unpack $ RBProj.destinationFile $ RBProj.project rbproj
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        if isMagmaV2
          then runMagma   rbprojPath out >>= stackIO . putStrLn
          else runMagmaV1 rbprojPath out >>= stackIO . putStrLn
        -- TODO: handle CON conversion
        return [out]
      (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "shake"
    , commandDesc = "For debug use only."
    , commandUsage = "onyx file mysong.yml file1 [file2...]"
    , commandRun = \files _opts -> case files of
      [] -> return []
      yml : builds -> identifyFile' yml >>= \case
        (FileSongYaml, yml') -> do
          audioDirs <- getAudioDirs yml
          shakeBuildFiles audioDirs yml' builds
          return $ map (takeDirectory yml' </>) builds
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
          installSTFS fpath drive
        FileRBA -> undone -- convert to con, install to usb drive
        FilePS -> undone -- install to PS music dir
        FileZip -> undone -- install to PS music dir
        _ -> unrecognized ftype fpath
      in optionalFile files >>= uncurry doInstall >> return []
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
          withSongYaml yamlPath = do
            audioDirs <- getAudioDirs yamlPath
            planName <- getPlanName yamlPath opts
            let rpp = "notes-" <> T.unpack planName <> ".RPP"
                yamlDir = takeDirectory yamlPath
            shakeBuildFiles audioDirs yamlPath [rpp]
            let rppFull = yamlDir </> "notes.RPP"
            stackIO $ Dir.renameFile (yamlDir </> rpp) rppFull
            return [rppFull]
          doImport fn inputPath = do
            let out = inputPath ++ "_reaper"
            stackIO $ Dir.createDirectoryIfMissing False out
            let f2x = listToMaybe [ f | Opt2x f <- opts ]
            void $ fn inputPath f2x out
            withSongYaml $ out </> "song.yml"
      case files' of
        [(FileSTFS, stfsPath)] -> doImport importSTFS stfsPath
        [(FileRBA, rbaPath)] -> doImport importRBA rbaPath
        [(FilePS, iniPath)] -> do
          let out = takeDirectory iniPath ++ "_reaper"
          stackIO $ Dir.createDirectoryIfMissing False out
          void $ importFoF (OptForceProDrums `notElem` opts) (takeDirectory iniPath) out
          withSongYaml $ out </> "song.yml"
        [(FileSongYaml, yamlPath)] -> withSongYaml yamlPath
        _ -> case partitionMaybe (isType [FileMidi]) files' of
          ([mid], notMid) -> case partitionMaybe (isType [FileOGG, FileWAV, FileFLAC]) notMid of
            (audio, []      ) -> do
              rpp <- outputFile opts $ return $ mid -<.> "RPP"
              makeReaperIO mid mid audio rpp
              return [rpp]
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
        shakeBuildFiles audioDirs fpath [player]
        player' <- case [ to | OptTo to <- opts ] of
          []      -> return player
          out : _ -> do
            stackIO $ Dir.createDirectoryIfMissing False out
            copyDirRecursive player out
            return out
        return [player' </> "index.html"]
      FileRBProj -> undone
      FileSTFS -> tempDir "onyx_player" $ \tmp -> do
        out <- outputFile opts $ return $ fpath ++ "_player"
        void $ importSTFS fpath Nothing tmp
        let player = "gen/plan/mogg/web"
        shakeBuildFiles [tmp] (tmp </> "song.yml") [player]
        stackIO $ Dir.createDirectoryIfMissing False out
        copyDirRecursive (tmp </> player) out
        return [out </> "index.html"]
      FileRBA -> tempDir "onyx_player" $ \tmp -> do
        out <- outputFile opts $ return $ fpath ++ "_player"
        void $ importRBA fpath Nothing tmp
        let player = "gen/plan/mogg/web"
        shakeBuildFiles [tmp] (tmp </> "song.yml") [player]
        stackIO $ Dir.createDirectoryIfMissing False out
        copyDirRecursive (tmp </> player) out
        return [out </> "index.html"]
      FilePS -> tempDir "onyx_player" $ \tmp -> do
        out <- outputFile opts $ return $ takeDirectory fpath ++ "_player"
        void $ importFoF (OptForceProDrums `notElem` opts) (takeDirectory fpath) tmp
        let player = "gen/plan/fof/web"
        shakeBuildFiles [tmp] (tmp </> "song.yml") [player]
        stackIO $ Dir.createDirectoryIfMissing False out
        copyDirRecursive (tmp </> player) out
        return [out </> "index.html"]
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
        audioDirs <- getAudioDirs yamlPath
        shakeBuildFiles audioDirs yamlPath [built]
        return []
      (FileRBProj, rbprojPath) -> do
        rbproj <- loadDTA rbprojPath
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        tempDir "onyx_check" $ \tmp -> if isMagmaV2
          then runMagmaMIDI rbprojPath (tmp </> "out.mid") >>= stackIO . putStrLn
          else undone
        return []
      (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "import"
    , commandDesc = "Import a file into onyx's project format."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileRBProj -> undone
      FileSTFS -> do
        out <- outputFile opts $ return $ fpath ++ "_import"
        stackIO $ Dir.createDirectoryIfMissing False out
        let f2x = listToMaybe [ f | Opt2x f <- opts ]
        void $ importSTFS fpath f2x out
        return [out]
      FileRBA -> do
        out <- outputFile opts $ return $ fpath ++ "_import"
        stackIO $ Dir.createDirectoryIfMissing False out
        let f2x = listToMaybe [ f | Opt2x f <- opts ]
        void $ importRBA fpath f2x out
        return [out]
      FilePS -> do
        out <- outputFile opts $ return $ takeDirectory fpath ++ "_import"
        stackIO $ Dir.createDirectoryIfMissing False out
        void $ importFoF (OptForceProDrums `notElem` opts) (takeDirectory fpath) out
        return [out]
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
      , "onyx convert in_ps/song.ini --to out_1x_rb3con --2x out_2x_rb3con"
      -- TODO , "onyx convert in_1x_rb3con --2x in_2x_rb3con --to out/ --game ps"
      ]
    , commandRun = \files opts -> tempDir "onyx_convert" $ \tmp -> do
      (ftype, fpath) <- optionalFile files
      let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
      if game == GameRB3 && ftype == FileRBA
        then do
          -- TODO make sure that the RBA is actually RB3 not RB2
          out <- outputFile opts $ return $ trimFileName fpath 42 "_rb3con"
          simpleRBAtoCON fpath out
          return [out]
        else do
          hasKicks <- case ftype of
            FileSTFS -> importSTFS fpath Nothing tmp
            FileRBA  -> importRBA fpath Nothing tmp
            FilePS   -> importFoF (OptForceProDrums `notElem` opts) (takeDirectory fpath) tmp
            FileZip  -> undone
            _        -> unrecognized ftype fpath
          let (gtr, bass)
                | elem OptKeysOnGuitar opts = (RBFile.FlexKeys  , RBFile.FlexBass)
                | elem OptKeysOnBass   opts = (RBFile.FlexGuitar, RBFile.FlexKeys)
                | otherwise                 = (RBFile.FlexGuitar, RBFile.FlexBass)
              specified1x = listToMaybe [ f | OptTo f <- opts ]
              specified2x = listToMaybe [ f | Opt2x f <- opts ]
              target1x = makeTarget False
              target2x = makeTarget True
              makeTarget is2x = case game of
                GameRB3 -> RB3 def
                  { rb3_2xBassPedal = is2x
                  }
                GameRB2 -> RB2 def
                  { rb2_2xBassPedal = is2x
                  , rb2_Guitar = gtr
                  , rb2_Bass = bass
                  }
              suffix = case game of GameRB3 -> "rb3con"; GameRB2 -> "rb2con"
          prefix <- fmap dropTrailingPathSeparator $ stackIO $ Dir.makeAbsolute $ case ftype of
            FilePS -> takeDirectory fpath
            _      -> fpath
          let out1x = flip fromMaybe specified1x $ case hasKicks of
                Has1x -> trimFileName prefix 42 $ "_" <> suffix
                _     -> trimFileName prefix 42 $ "_1x_" <> suffix
              out2x = flip fromMaybe specified2x $ case hasKicks of
                Has2x -> trimFileName prefix 42 $ "_" <> suffix
                _     -> trimFileName prefix 42 $ "_2x_" <> suffix
              go target out = do
                con <- shakeBuildTarget [tmp] (tmp </> "song.yml") target
                stackIO $ Dir.copyFile con out
                return [out]
              go1x = go target1x out1x
              go2x = go target2x out2x
              goBoth = liftA2 (++) go1x go2x
          case (specified1x, specified2x) of
            (Nothing, Nothing) -> case hasKicks of
              Has1x   -> go1x
              Has2x   -> go2x
              HasBoth -> goBoth
            (Just _ , Nothing) -> go1x
            (Nothing, Just _ ) -> go2x
            (Just _ , Just _ ) -> goBoth
    }

  , Command
    { commandWord = "hanging"
    , commandDesc = "Find pro keys range shifts with hanging notes."
    , commandUsage = T.unlines
      [ "onyx hanging mysong.yml"
      , "onyx hanging notes.mid"
      ]
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> do
      let withMIDI mid = stackIO (Load.fromFile mid) >>= RBFile.readMIDIFile' >>= stackIO . putStrLn . closeShiftsFile
      case ftype of
        FileSongYaml -> do
          audioDirs <- getAudioDirs fpath
          planName <- getPlanName fpath opts
          shakeBuildFiles audioDirs fpath ["gen/plan" </> T.unpack planName </> "hanging"]
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
      return []
    }

  , Command
    { commandWord = "reduce"
    , commandDesc = "Fill in missing difficulties in a MIDI file."
    , commandUsage = T.unlines
      [ "onyx reduce [notes.mid|song.yml|magma.rbproj|song.ini]"
      , "# or run on current directory"
      , "onyx reduce"
      , "# by default, creates .reduced.mid. or specify --to"
      , "onyx reduce notes.mid --to new.mid"
      ]
    , commandRun = \files opts -> do
      mid <- getInputMIDI files
      out <- outputFile opts $ return $ mid -<.> "reduced.mid"
      simpleReduce mid out
      return [out]
    }

  , Command
    { commandWord = "ranges"
    , commandDesc = "Generate an automatic set of pro keys range shifts."
    , commandUsage = T.unlines
      [ "onyx ranges [notes.mid|song.yml|magma.rbproj]"
      , "# or run on current directory"
      , "onyx ranges"
      , "# by default, creates .ranges.mid. or specify --to"
      , "onyx ranges notes.mid --to new.mid"
      ]
    , commandRun = \files opts -> do
      mid <- getInputMIDI files
      out <- outputFile opts $ return $ mid -<.> "ranges.mid"
      completeFile mid out
      return [out]
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
      [dir] -> stackIO (Dir.doesDirectoryExist dir) >>= \case
        True -> do
          let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
              suffix = case game of GameRB3 -> "_rb3con"; GameRB2 -> "_rb2con"
              pkg    = case game of GameRB3 -> rb3pkg   ; GameRB2 -> rb2pkg
          stfs <- outputFile opts
            $   (\f -> trimFileName f 42 suffix) . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          (title, desc) <- getInfoForSTFS dir stfs
          pkg title desc dir stfs
          return [stfs]
        False -> fatal $ "onyx stfs expected directory; given: " <> dir
      _ -> fatal $ "onyx stfs expected 1 argument, given " <> show (length files)
    }

  , Command
    { commandWord = "unstfs"
    , commandDesc = "Extract the contents of an Xbox 360 STFS file."
    , commandUsage = T.unlines
      [ "onyx unstfs song_rb3con"
      , "onyx unstfs song_rb3con --to a_folder"
      ]
    , commandRun = \files opts -> mapM identifyFile' files >>= \case
      [(FileSTFS, stfs)] -> do
        out <- outputFile opts $ return $ stfs ++ "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        stackIO $ extractSTFS stfs out
        return [out]
      _ -> fatal $ "onyx unstfs expected a single STFS argument, given: " <> show files
    }

  , Command
    { commandWord = "midi-text"
    , commandDesc = "Convert a MIDI file to/from a plaintext format."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileMidi -> do
        out <- outputFile opts $ return $ fpath -<.> "midtxt"
        res <- stackIO $ MS.toStandardMIDI <$> Load.fromFile fpath
        case res of
          Left  err -> fatal err
          Right sm  -> stackIO $ writeFile out $ MS.showStandardMIDI (midiOptions opts) sm
        return [out]
      FileMidiText -> do
        out <- outputFile opts $ return $ fpath -<.> "mid"
        sf <- stackIO $ MS.readStandardFile . MS.parse . MS.scan <$> readFile fpath
        let (mid, warnings) = MS.fromStandardMIDI (midiOptions opts) sf
        mapM_ warn warnings
        stackIO $ Save.toFile out mid
        return [out]
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "midi-text-git"
    , commandDesc = "Convert a MIDI file to a plaintext format. (for git use)"
    , commandUsage = "onyx midi-text-git in.mid > out.midtxt"
    , commandRun = \files opts -> case files of
      [mid] -> do
        res <- stackIO $ MS.toStandardMIDI <$> Load.fromFile mid
        case res of
          Left  err -> fatal err
          Right sm  -> stackIO $ putStrLn $ MS.showStandardMIDI (midiOptions opts) sm
        return []
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
        return [mogg]
      FileMOGG -> do
        ogg <- outputFile opts $ return $ fpath -<.> "ogg"
        moggToOgg fpath ogg
        return [ogg]
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "merge"
    , commandDesc = "Take tracks from one MIDI file, and convert them to a different file's tempo map."
    , commandUsage = T.unlines
      [ "onyx merge base.mid tracks.mid pad  n --to out.mid"
      , "onyx merge base.mid tracks.mid drop n --to out.mid"
      ]
    , commandRun = \args opts -> case args of
      [base, tracks] -> do
        baseMid <- stackIO (Load.fromFile base) >>= RBFile.readMIDIFile'
        tracksMid <- stackIO (Load.fromFile tracks) >>= RBFile.readMIDIFile'
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        let newMid = RBFile.mergeCharts (RBFile.TrackPad 0) baseMid tracksMid
        stackIO $ Save.toFile out $ RBFile.showMIDIFile' newMid
        return [out]
      [base, tracks, "pad", n] -> do
        baseMid <- stackIO (Load.fromFile base) >>= RBFile.readMIDIFile'
        tracksMid <- stackIO (Load.fromFile tracks) >>= RBFile.readMIDIFile'
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        n' <- case readMaybe n of
          Nothing -> fatal "Invalid merge pad amount"
          Just d  -> return $ realToFrac (d :: Double)
        let newMid = RBFile.mergeCharts (RBFile.TrackPad n') baseMid tracksMid
        stackIO $ Save.toFile out $ RBFile.showMIDIFile' newMid
        return [out]
      [base, tracks, "drop", n] -> do
        baseMid <- stackIO (Load.fromFile base) >>= RBFile.readMIDIFile'
        tracksMid <- stackIO (Load.fromFile tracks) >>= RBFile.readMIDIFile'
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        n' <- case readMaybe n of
          Nothing -> fatal "Invalid merge drop amount"
          Just d  -> return $ realToFrac (d :: Double)
        let newMid = RBFile.mergeCharts (RBFile.TrackDrop n') baseMid tracksMid
        stackIO $ Save.toFile out $ RBFile.showMIDIFile' newMid
        return [out]
      _ -> fatal "Invalid merge syntax"
    }

  ]

commandLine :: (MonadIO m) => [String] -> StackTraceT m [FilePath]
commandLine args = let
  (opts, nonopts, errors) = getOpt Permute optDescrs args
  printIntro = stackIO $ mapM_ T.putStrLn
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
        then printIntro >> return []
        else printIntro >> fatal "No command given."
      cmd : nonopts' -> case [ c | c <- commands, commandWord c == T.pack cmd ] of
        [c] -> inside ("command: onyx " <> cmd) $ if elem OptHelp opts
          then do
            stackIO $ mapM_ T.putStrLn
              [ "onyx " <> commandWord c
              , commandDesc c
              , ""
              , "Usage:"
              , commandUsage c
              ]
            return []
          else commandRun c nonopts' opts
        _ -> printIntro >> fatal ("Unrecognized command: " ++ show cmd)
    _ -> inside "command line parsing" $ do
      printIntro
      throwError $ Messages [ Message e [] | e <- errors ]

optDescrs :: [OptDescr OnyxOption]
optDescrs =
  [ Option []   ["target"         ] (ReqArg (OptTarget . T.pack)   "TARGET"   ) ""
  , Option []   ["plan"           ] (ReqArg (OptPlan   . T.pack)   "PLAN"     ) ""
  , Option []   ["to"             ] (ReqArg OptTo                  "PATH"     ) ""
  , Option []   ["2x"             ] (ReqArg Opt2x                  "PATH"     ) ""
  , Option []   ["game"           ] (ReqArg (OptGame . readGame)   "{rb3,rb2}") ""
  , Option []   ["separate-lines" ] (NoArg  OptSeparateLines                  ) ""
  , Option []   ["in-beats"       ] (NoArg  OptInBeats                        ) ""
  , Option []   ["in-seconds"     ] (NoArg  OptInSeconds                      ) ""
  , Option []   ["in-measures"    ] (NoArg  OptInMeasures                     ) ""
  , Option []   ["match-notes"    ] (NoArg  OptMatchNotes                     ) ""
  , Option []   ["resolution"     ] (ReqArg (OptResolution . read) "int"      ) ""
  , Option []   ["keys-on-guitar" ] (NoArg  OptKeysOnGuitar                   ) ""
  , Option []   ["keys-on-bass"   ] (NoArg  OptKeysOnBass                     ) ""
  , Option []   ["force-pro-drums"] (NoArg  OptForceProDrums                  ) ""
  , Option "h?" ["help"           ] (NoArg  OptHelp                           ) ""
  ] where
    readGame = \case
      "rb3" -> GameRB3
      "rb2" -> GameRB2
      g     -> error $ "Unrecognized --game value: " ++ show g

data OnyxOption
  = OptTarget T.Text
  | OptPlan T.Text
  | OptTo FilePath
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
  | OptForceProDrums
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
