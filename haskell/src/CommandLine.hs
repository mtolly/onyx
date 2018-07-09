{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module CommandLine (commandLine, identifyFile', FileType(..)) where

import           Audio                            (applyPansVols, fadeEnd,
                                                   fadeStart)
import           Build                            (loadYaml, shakeBuildFiles)
import           Config
import           Control.Applicative              (liftA2)
import           Control.Monad.Extra              (filterM, forM, forM_, guard)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (runResourceT)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.ByteString.Lazy.Char8       ()
import           Data.Char                        (isAlphaNum, isAscii)
import qualified Data.Conduit.Audio               as CA
import           Data.Conduit.Audio.Sndfile       (sinkSnd, sourceSndFrom)
import           Data.Default.Class               (def)
import qualified Data.Digest.Pure.MD5             as MD5
import           Data.DTA.Lex                     (scanStack)
import           Data.DTA.Parse                   (parseStack)
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Magma         as RBProj
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     (void)
import qualified Data.HashMap.Strict              as Map
import           Data.List.Extra                  (intercalate, stripSuffix)
import           Data.List.HT                     (partitionMaybe)
import           Data.Maybe                       (fromMaybe, listToMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Text.Encoding.Error         (lenientDecode)
import           Data.Word                        (Word32)
import qualified Image
import           Import
import           Magma                            (getRBAFile, oggToMogg,
                                                   runMagma, runMagmaMIDI,
                                                   runMagmaV1)
import           MoggDecrypt                      (moggToOgg)
import           OpenProject
import           PrettyDTA                        (DTASingle (..),
                                                   readFileSongsDTA, readRB3DTA,
                                                   writeDTASingle)
import           ProKeysRanges                    (closeShiftsFile,
                                                   completeFile)
import           Reaper.Build                     (makeReaperIO)
import           Reductions                       (simpleReduce)
import qualified RockBand.Codec.File              as RBFile
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Script.Base           as MS
import qualified Sound.MIDI.Script.Parse          as MS
import qualified Sound.MIDI.Script.Read           as MS
import qualified Sound.MIDI.Script.Scan           as MS
import qualified Sound.MIDI.Util                  as U
import           STFS.Extract                     (extractSTFS)
import           System.Console.GetOpt
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropTrailingPathSeparator,
                                                   splitFileName, takeDirectory,
                                                   takeExtension, takeFileName,
                                                   (-<.>), (<.>), (</>))
import qualified System.IO                        as IO
import           Text.Printf                      (printf)
import           Text.Read                        (readMaybe)
import           U8                               (packU8)
import           X360DotNet                       (rb2pkg, rb3pkg, stfsFolder)

#ifdef WINDOWS
import           Data.Bits                        (testBit)
import           System.Win32.File                (getLogicalDrives)
#else
import           Data.List                        (isPrefixOf)
import           System.MountPoints
#endif

loadRBProj :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m RBProj.RBProj
loadRBProj f = inside f $ do
  stackIO (TE.decodeUtf8With lenientDecode <$> B.readFile f)
    >>= scanStack >>= parseStack >>= D.unserialize D.stackChunks

getInfoForSTFS :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m (T.Text, T.Text)
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
    return $ T.takeWhile (/= '\0') $ TE.decodeUtf16BE bs
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
      then copyDirRecursive pathFrom pathTo
      else stackIO $ do
        Dir.copyFile pathFrom pathTo
        -- force r/w permissions due to X360 requiring it
        perm <- Dir.getPermissions pathTo
        Dir.setPermissions pathTo
          $ Dir.setOwnerReadable True
          $ Dir.setOwnerWritable True perm

data Command = Command
  { commandWord  :: T.Text
  , commandRun   :: forall m. (MonadIO m) => [FilePath] -> [OnyxOption] -> StackTraceT (QueueLog m) [FilePath]
  , commandDesc  :: T.Text
  , commandUsage :: T.Text
  }

identifyFile :: FilePath -> IO FileResult
identifyFile fp = Dir.doesFileExist fp >>= \case
  True -> case takeExtension fp of
    ".yml" -> return $ FileType FileSongYaml fp
    ".rbproj" -> return $ FileType FileRBProj fp
    ".midtxt" -> return $ FileType FileMidiText fp
    ".mogg" -> return $ FileType FileMOGG fp
    ".zip" -> return $ FileType FileZip fp
    ".chart" -> return $ FileType FileChart fp
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
        False -> Dir.doesFileExist (fp </> "notes.chart") >>= \case
          True -> return $ FileType FileChart $ fp </> "notes.chart"
          False -> do
            ents <- Dir.listDirectory fp
            case filter (\ent -> takeExtension ent == ".rbproj") ents of
              [ent] -> return $ FileType FileRBProj $ fp </> ent
              _     -> Dir.doesFileExist (fp </> "songs/songs.dta") >>= \case
                True -> return $ FileType FileDTA $ fp </> "songs/songs.dta"
                False -> return FileUnrecognized
    False -> return FileDoesNotExist

data FileResult
  = FileType FileType FilePath
  | FileDoesNotExist
  | FileUnrecognized
  deriving (Eq, Ord, Show, Read)

data FileType
  = FileSongYaml
  | FileSTFS
  | FileDTA
  | FileRBA
  | FilePS
  | FileChart
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
identifyFile' file = stackIO (identifyFile file) >>= \case
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

outputFile :: (Monad m) => [OnyxOption] -> m FilePath -> m FilePath
outputFile opts dft = case [ to | OptTo to <- opts ] of
  []     -> dft
  to : _ -> return to

buildTarget :: (MonadIO m) => FilePath -> [OnyxOption] -> StackTraceT (QueueLog m) (Target, FilePath)
buildTarget yamlPath opts = do
  songYaml <- loadYaml yamlPath
  targetName <- case [ t | OptTarget t <- opts ] of
    []    -> fatal "command requires --target, none given"
    t : _ -> return t
  audioDirs <- withProject yamlPath getAudioDirs
  target <- case Map.lookup targetName $ _targets songYaml of
    Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
    Just target -> return target
  let built = case target of
        RB3{} -> "gen/target" </> T.unpack targetName </> "rb3con"
        RB2{} -> "gen/target" </> T.unpack targetName </> "rb2con"
        PS {} -> "gen/target" </> T.unpack targetName </> "ps.zip"
        GH2{} -> "gen/target" </> T.unpack targetName </> "gh2.zip"
  shakeBuildFiles audioDirs yamlPath [built]
  return (target, takeDirectory yamlPath </> built)

getMaybePlan :: [OnyxOption] -> Maybe T.Text
getMaybePlan opts = listToMaybe [ p | OptPlan p <- opts ]

getPlanName :: (SendMessage m, MonadIO m) => Maybe T.Text -> FilePath -> [OnyxOption] -> StackTraceT m T.Text
getPlanName defaultPlan yamlPath opts = case getMaybePlan opts of
  Just p  -> return p
  Nothing -> do
    songYaml <- loadYaml yamlPath
    case Map.keys $ _plans songYaml of
      [p]   -> return p
      plans -> let
        err = fatal $ "No --plan given, and YAML file doesn't have exactly 1 plan: " <> show plans
        in case defaultPlan of
          Nothing -> err
          Just p  -> if elem p plans then return p else err

getInputMIDI :: (SendMessage m, MonadIO m) => [FilePath] -> StackTraceT m FilePath
getInputMIDI files = optionalFile files >>= \case
  (FileSongYaml, yamlPath) -> return $ takeDirectory yamlPath </> "notes.mid"
  (FileRBProj, rbprojPath) -> do
    rbproj <- loadRBProj rbprojPath
    return $ T.unpack $ RBProj.midiFile $ RBProj.midi $ RBProj.project rbproj
  (FileMidi, mid) -> return mid
  (FilePS, ini) -> return $ takeDirectory ini </> "notes.mid"
  (ftype, fpath) -> unrecognized ftype fpath

undone :: (Monad m) => StackTraceT m a
undone = fatal "Feature not built yet..."

-- | Ensure that a \"dir/original-file_suffix\" filename stays within a length limit
-- by trimming \"original-file\".
trimFileName :: FilePath -> Int -> String -> String -> FilePath
trimFileName fp len sfxOld sfx = let
  fp' = case stripSuffix sfxOld fp of
    Nothing       -> fp
    Just stripped -> stripped
  (dir, file) = splitFileName fp'
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
        rbproj <- loadRBProj rbprojPath
        out <- outputFile opts $ return $ T.unpack $ RBProj.destinationFile $ RBProj.project rbproj
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        if isMagmaV2
          then runMagma   rbprojPath out >>= lg
          else runMagmaV1 rbprojPath out >>= lg
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
          audioDirs <- withProject yml getAudioDirs
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
                GH2{} -> undefined -- TODO
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
            audioDirs <- withProject yamlPath getAudioDirs
            planName <- getPlanName (Just "author") yamlPath opts
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
          pschart dir = do
            let out = dir ++ "_reaper"
            stackIO $ Dir.createDirectoryIfMissing False out
            void $ importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) dir out
            withSongYaml $ out </> "song.yml"
      case files' of
        [(FileSTFS, stfsPath)] -> doImport importSTFS stfsPath
        [(FileDTA, dtaPath)] -> doImport importSTFSDir $ takeDirectory $ takeDirectory dtaPath
        [(FileRBA, rbaPath)] -> doImport importRBA rbaPath
        [(FilePS, iniPath)] -> pschart $ takeDirectory iniPath
        [(FileChart, chartPath)] -> pschart $ takeDirectory chartPath
        [(FileSongYaml, yamlPath)] -> withSongYaml yamlPath
        _ -> case partitionMaybe (isType [FileMidi]) files' of
          ([mid], notMid) -> case partitionMaybe (isType [FileOGG, FileWAV, FileFLAC]) notMid of
            (audio, []      ) -> do
              rpp <- outputFile opts $ return $ mid -<.> "RPP"
              makeReaperIO [] mid mid audio rpp
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
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> let
      pschart = tempDir "onyx_player" $ \tmp -> do
        out <- outputFile opts $ return $ takeDirectory fpath ++ "_player"
        void $ importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) tmp
        let player = "gen/plan/fof/web"
        shakeBuildFiles [tmp] (tmp </> "song.yml") [player]
        stackIO $ Dir.createDirectoryIfMissing False out
        copyDirRecursive (tmp </> player) out
        return [out </> "index.html"]
      in case ftype of
        FileSongYaml -> do
          audioDirs <- withProject fpath getAudioDirs
          planName <- getPlanName (Just "player") fpath opts
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
        FilePS -> pschart
        FileChart -> pschart
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
        audioDirs <- withProject yamlPath getAudioDirs
        shakeBuildFiles audioDirs yamlPath [built]
        return []
      (FileRBProj, rbprojPath) -> do
        rbproj <- loadRBProj rbprojPath
        let isMagmaV2 = case RBProj.projectVersion $ RBProj.project rbproj of
              24 -> True
              5  -> False -- v1
              _  -> True -- need to do more testing
        tempDir "onyx_check" $ \tmp -> if isMagmaV2
          then runMagmaMIDI rbprojPath (tmp </> "out.mid") >>= lg
          else undone
        return []
      (ftype, fpath) -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "import"
    , commandDesc = "Import a file into onyx's project format."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> let
      pschart = do
        out <- outputFile opts $ return $ takeDirectory fpath ++ "_import"
        stackIO $ Dir.createDirectoryIfMissing False out
        void $ importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) out
        return [out]
      in case ftype of
        FileSTFS -> do
          out <- outputFile opts $ return $ fpath ++ "_import"
          stackIO $ Dir.createDirectoryIfMissing False out
          let f2x = listToMaybe [ f | Opt2x f <- opts ]
          void $ importSTFS fpath f2x out
          return [out]
        FileDTA -> do
          let topDir = takeDirectory $ takeDirectory fpath
          out <- outputFile opts $ return $ topDir ++ "_import"
          stackIO $ Dir.createDirectoryIfMissing False out
          let f2x = listToMaybe [ f | Opt2x f <- opts ]
          void $ importSTFSDir topDir f2x out
          return [out]
        FileRBA -> do
          out <- outputFile opts $ return $ fpath ++ "_import"
          stackIO $ Dir.createDirectoryIfMissing False out
          let f2x = listToMaybe [ f | Opt2x f <- opts ]
          void $ importRBA fpath f2x out
          return [out]
        FilePS -> pschart
        FileChart -> pschart
        FileRBProj -> do
          out <- outputFile opts $ return $ takeDirectory fpath ++ "_import"
          stackIO $ Dir.createDirectoryIfMissing False out
          void $ importMagma fpath out
          return [out]
        _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "magma"
    , commandDesc = "Import a file into a Magma project."
    , commandUsage = T.unlines
      [ "onyx magma in_rb3con"
      , "onyx magma in_rb3con --to project/"
      , "onyx magma in_ps/song.ini"
      , "onyx magma in_ps/song.ini --to project_1x/ --2x project_2x/"
      ]
    , commandRun = \files opts -> tempDir "onyx_magma" $ \tmp -> do
      (ftype, fpath) <- optionalFile files
      hasKicks <- case ftype of
        FileSTFS   -> importSTFS fpath Nothing tmp
        FileRBA    -> importRBA fpath Nothing tmp
        FilePS     -> importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) tmp
        FileChart  -> importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) tmp
        FileRBProj -> importMagma (takeDirectory fpath) tmp -- why would you do this
        _          -> unrecognized ftype fpath
      let specified1x = listToMaybe [ f | OptTo f <- opts ]
          specified2x = listToMaybe [ f | Opt2x f <- opts ]
          target1x = makeTarget False
          target2x = makeTarget True
          speed = listToMaybe [ d | OptSpeed d <- opts ]
          speedPercent = round $ fromMaybe 1 speed * 100 :: Int
          makeTarget is2x = def
            { rb3_2xBassPedal = is2x
            , rb3_Speed = speed
            , rb3_Keys = if elem OptGuitarOnKeys opts
              then RBFile.FlexGuitar
              else RBFile.FlexKeys
            }
      prefix <- fmap dropTrailingPathSeparator $ stackIO $ Dir.makeAbsolute $ case ftype of
        FilePS    -> takeDirectory fpath
        FileChart -> takeDirectory fpath
        _         -> fpath
      let out1x = flip fromMaybe specified1x $ case hasKicks of
            Has1x -> prefix <> "_" <> suffix
            _     -> prefix <> "_1x_" <> suffix
          out2x = flip fromMaybe specified2x $ case hasKicks of
            Has2x -> prefix <> "_" <> suffix
            _     -> prefix <> "_2x_" <> suffix
          suffix = intercalate "_" $ concat
            [ ["gk" | elem OptGuitarOnKeys opts]
            , case speedPercent of 100 -> []; _ -> [show speedPercent]
            , ["project"]
            ]
          go target out = do
            magma <- withProject (tmp </> "song.yml") $ buildMagmaV2 target
            copyDirRecursive magma out
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
        && all (\case OptSpeed{} -> False; OptGuitarOnKeys -> False; _ -> True) opts
        then do
          -- TODO make sure that the RBA is actually RB3 not RB2
          out <- outputFile opts $ return $ trimFileName fpath 42 ".rba" "_rb3con"
          simpleRBAtoCON fpath out
          return [out]
        else do
          hasKicks <- case ftype of
            FileSTFS   -> importSTFS fpath Nothing tmp
            FileRBA    -> importRBA fpath Nothing tmp
            FilePS     -> importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) tmp
            FileChart  -> importFoF (OptForceProDrums `notElem` opts) (OptDropOpenHOPOs `elem` opts) (takeDirectory fpath) tmp
            FileRBProj -> importMagma (takeDirectory fpath) tmp
            _          -> unrecognized ftype fpath
          let (gtr, bass)
                | elem OptKeysOnGuitar opts = (RBFile.FlexKeys  , RBFile.FlexBass)
                | elem OptKeysOnBass   opts = (RBFile.FlexGuitar, RBFile.FlexKeys)
                | otherwise                 = (RBFile.FlexGuitar, RBFile.FlexBass)
              keys
                | elem OptGuitarOnKeys opts = RBFile.FlexGuitar
                | otherwise                 = RBFile.FlexKeys
              specified1x = listToMaybe [ f | OptTo f <- opts ]
              specified2x = listToMaybe [ f | Opt2x f <- opts ]
              target1x = makeTarget False
              target2x = makeTarget True
              speed = listToMaybe [ d | OptSpeed d <- opts ]
              speedPercent = round $ fromMaybe 1 speed * 100 :: Int
              makeTarget is2x = case game of
                GameRB3 -> buildRB3CON def
                  { rb3_2xBassPedal = is2x
                  , rb3_Speed = speed
                  , rb3_Keys = keys
                  }
                GameRB2 -> buildRB2CON def
                  { rb2_2xBassPedal = is2x
                  , rb2_Speed = speed
                  , rb2_Guitar = gtr
                  , rb2_Bass = bass
                  , rb2_LabelRB2 = elem OptRB2Version opts
                  }
              suffix = intercalate "_" $ concat
                [ ["gk" | keys == RBFile.FlexGuitar]
                , case speedPercent of 100 -> []; _ -> [show speedPercent]
                , [case game of GameRB3 -> "rb3con"; GameRB2 -> "rb2con"]
                ]
          prefix <- fmap dropTrailingPathSeparator $ stackIO $ Dir.makeAbsolute $ case ftype of
            FilePS    -> takeDirectory fpath
            FileChart -> takeDirectory fpath
            _         -> fpath
          let out1x = flip fromMaybe specified1x $ case hasKicks of
                Has1x -> trimFileName prefix 42 "_rb3con" $ "_" <> suffix
                _     -> trimFileName prefix 42 "_rb3con" $ "_1x_" <> suffix
              out2x = flip fromMaybe specified2x $ case hasKicks of
                Has2x -> trimFileName prefix 42 "_rb3con" $ "_" <> suffix
                _     -> trimFileName prefix 42 "_rb3con" $ "_2x_" <> suffix
              go buildCON out = do
                con <- withProject (tmp </> "song.yml") buildCON
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
      let withMIDI mid = stackIO (Load.fromFile mid) >>= RBFile.readMIDIFile' >>= lg . closeShiftsFile
      case ftype of
        FileSongYaml -> withProject fpath $ proKeysHanging $ getMaybePlan opts
        FileRBProj   -> do
          rbproj <- loadRBProj fpath
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
            $   (\f -> trimFileName f 42 "" suffix) . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          (title, desc) <- getInfoForSTFS dir stfs
          tempDir "onyx_stfs" $ \tmp -> do
            -- because X360 fails if the files aren't writable,
            -- as a hack we copy everything to a new dir,
            -- and force r/w permissions (see copyDirRecursive)
            copyDirRecursive dir tmp
            pkg title desc tmp stfs
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
          Right sm  -> lg $ MS.showStandardMIDI (midiOptions opts) sm
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

  , Command
    { commandWord = "add-track"
    , commandDesc = "Add an empty track to a MIDI file with a given name."
    , commandUsage = "onyx add-track \"TRACK NAME\" --to [notes.mid|song.yml]"
    , commandRun = \args opts -> case args of
      [name] -> do
        f <- outputFile opts $ return "."
        (ftype, fpath) <- identifyFile' f
        pathMid <- case ftype of
          FileMidi -> return fpath
          FileSongYaml -> return $ takeDirectory fpath </> "notes.mid"
          FileRBProj -> undone
          _ -> fatal "Unrecognized --to argument, expected .mid/.yml/.rbproj"
        mid <- stackIO (Load.fromFile pathMid) >>= RBFile.readMIDIFile'
        let trks = RBFile.rawTracks $ RBFile.s_tracks mid
        if any ((== Just name) . U.trackName) trks
          then return ()
          else stackIO $ Save.toFile pathMid $ RBFile.showMIDIFile' $ mid
            { RBFile.s_tracks = RBFile.RawFile $ trks ++ [U.setTrackName name RTB.empty]
            }
        return [pathMid]
      _ -> fatal "Expected 1 argument (track name)"
    }

  , Command
    { commandWord = "u8"
    , commandDesc = "Generate U8 packages for the Wii."
    , commandUsage = "onyx u8 dir --to out.app"
    , commandRun = \args opts -> case args of
      [dir] -> stackIO (Dir.doesDirectoryExist dir) >>= \case
        True -> do
          fout <- outputFile opts
            $ (<> ".app") . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          stackIO $ packU8 dir fout
          return [fout]
        False -> fatal $ "onyx u8 expected directory; given: " <> dir
      _ -> fatal "Expected 1 argument (folder to pack)"
    }

  , Command
    { commandWord = "dolphin"
    , commandDesc = "Combine CON files into two U8 packages intended for RB3 in Dolphin."
    , commandUsage = "onyx dolphin 1_rb3con 2_rb3con --to dir/"
    , commandRun = \args opts -> do
      out <- outputFile opts $ fatal "need output folder for .app files"
      stackIO $ Dir.createDirectoryIfMissing False out
      let out_meta = out </> "00000001.app"
          out_song = out </> "00000002.app"
      tempDir "onyx_dolphin" $ \temp -> do
        let extract  = temp </> "extract"
            dir_meta = temp </> "meta"
            dir_song = temp </> "song"
        allSongs <- fmap concat $ forM args $ \stfs -> do
          stackIO $ Dir.createDirectory extract
          stackIO $ extractSTFS stfs extract
          songs <- readFileSongsDTA $ extract </> "songs/songs.dta"
          prevEnds <- forM songs $ \(song, _) -> do
            let DTASingle _ pkg _ = song
                songsXX = T.unpack $ D.songName $ D.song pkg
                songsXgen = takeDirectory songsXX </> "gen"
                songsXgenX = songsXgen </> takeFileName songsXX
            stackIO $ Dir.createDirectoryIfMissing True $ dir_meta </> "content" </> songsXgen
            stackIO $ Dir.createDirectoryIfMissing True $ dir_song </> "content" </> songsXgen
            -- .mid (copy; later, option to remove drum fills (but not BRE))
            stackIO $ Dir.renameFile (extract </> songsXX <.> "mid") (dir_song </> "content" </> songsXX <.> "mid")
            -- .mogg (copy)
            let mogg = dir_song </> "content" </> songsXX <.> "mogg"
            stackIO $ Dir.renameFile (extract </> songsXX <.> "mogg") mogg
            -- _prev.mogg (generate)
            let ogg = extract </> "temp.ogg"
                prevOgg = extract </> "temp_prev.ogg"
                (prevStart, prevEnd) = D.preview pkg
                prevLength = min 15000 $ prevEnd - prevStart
            moggToOgg mogg ogg
            src <- stackIO $ sourceSndFrom (CA.Seconds $ realToFrac prevStart / 1000) ogg
            stackIO
              $ runResourceT
              $ sinkSnd prevOgg (Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile)
              $ fadeStart (CA.Seconds 0.75)
              $ fadeEnd (CA.Seconds 0.75)
              $ CA.takeStart (CA.Seconds $ realToFrac prevLength / 1000)
              $ applyPansVols (D.pans $ D.song pkg) (D.vols $ D.song pkg)
              $ src
            oggToMogg prevOgg $ dir_meta </> "content" </> (songsXX ++ "_prev.mogg")
            -- .png_wii (convert from .png_xbox)
            img <- stackIO $ Image.readRBImage . BL.fromStrict <$> B.readFile (extract </> (songsXgenX ++ "_keep.png_xbox"))
            stackIO $ BL.writeFile (dir_meta </> "content" </> (songsXgenX ++ "_keep.png_wii")) $ Image.toDXT1File Image.PNGWii img
            -- .milo_wii (copy)
            stackIO $ Dir.renameFile (extract </> songsXgenX <.> "milo_xbox") (dir_song </> "content" </> songsXgenX <.> "milo_wii")
            -- return new preview end time to put in .dta
            return $ prevStart + prevLength
          stackIO $ Dir.removeDirectoryRecursive extract
          return $ zip songs prevEnds
        -- write new combined dta file
        let dta = dir_meta </> "content/songs/songs.dta"
        stackIO $ Dir.createDirectoryIfMissing True $ takeDirectory dta
        stackIO $ B.writeFile dta $ TE.encodeUtf8 $ T.unlines $ do
          ((DTASingle key pkg c3, _), prevEnd) <- allSongs
          let pkg' = pkg
                { D.song = (D.song pkg)
                  { D.songName = "dlc/sZAE/001/content/" <> D.songName (D.song pkg)
                  }
                , D.encoding = Just "utf8"
                , D.preview = (fst $ D.preview pkg, prevEnd)
                }
          return $ writeDTASingle $ DTASingle key pkg' c3
        -- pack it all up
        stackIO $ packU8 dir_meta out_meta
        stackIO $ packU8 dir_song out_song
      return [out_meta, out_song]
    }

  ]

commandLine :: (MonadIO m) => [String] -> StackTraceT (QueueLog m) [FilePath]
commandLine args = let
  (opts, nonopts, errors) = getOpt Permute optDescrs args
  printIntro = lg $ T.unpack $ T.unlines
    [ "Onyx Music Game Toolkit"
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
            lg $ T.unpack $ T.unlines
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
  , Option []   ["drop-open-hopos"] (NoArg  OptDropOpenHOPOs                  ) ""
  , Option []   ["speed"          ] (ReqArg (OptSpeed . read)      "real"     ) ""
  , Option []   ["guitar-on-keys" ] (NoArg  OptGuitarOnKeys                   ) ""
  , Option []   ["rb2-version"    ] (NoArg  OptRB2Version                     ) ""
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
  | OptDropOpenHOPOs
  | OptSpeed Double
  | OptGuitarOnKeys
  | OptRB2Version
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
