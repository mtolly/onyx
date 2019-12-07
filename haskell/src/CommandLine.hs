{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
module CommandLine (commandLine, identifyFile', FileType(..), copyDirRecursive, runDolphin) where

import           Audio                            (applyPansVols, fadeEnd,
                                                   fadeStart, runAudio)
import           Build                            (loadYaml, shakeBuildFiles)
import           Config
import           Control.Monad.Extra              (filterM, forM, forM_, guard,
                                                   when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource, ResourceT,
                                                   runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get                  (runGetOrFail)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.ByteString.Lazy.Char8       ()
import           Data.Char                        (isAlphaNum, isAscii)
import qualified Data.Conduit.Audio               as CA
import           Data.Conduit.Audio.Sndfile       (sinkSnd, sourceSndFrom)
import qualified Data.Digest.Pure.MD5             as MD5
import           Data.DTA.Lex                     (scanStack)
import           Data.DTA.Parse                   (parseStack)
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Magma         as RBProj
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Functor                     (void)
import qualified Data.HashMap.Strict              as Map
import           Data.Int                         (Int16)
import           Data.List.Extra                  (stripSuffix, unsnoc)
import           Data.List.HT                     (partitionMaybe)
import           Data.Maybe                       (fromMaybe, listToMaybe,
                                                   mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word                        (Word32)
import           GuitarHeroII.Audio               (readVGS)
import qualified Image
import           Import
import           Magma                            (getRBAFile, runMagma,
                                                   runMagmaMIDI, runMagmaV1)
import           MoggDecrypt                      (moggToOgg, oggToMogg)
import           OpenProject
import           OSFiles                          (copyDirRecursive)
import           PrettyDTA                        (DTASingle (..),
                                                   readFileSongsDTA, readRB3DTA,
                                                   writeDTASingle)
import           ProKeysRanges                    (closeShiftsFile)
import           Reaper.Build                     (makeReaperIO)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Milo                    (MiloDir (..), addMiloHeader,
                                                   breakMilo, decompressMilo,
                                                   parseMiloFile,
                                                   testConvertLipsync)
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Script.Base           as MS
import qualified Sound.MIDI.Script.Parse          as MS
import qualified Sound.MIDI.Script.Read           as MS
import qualified Sound.MIDI.Script.Scan           as MS
import qualified Sound.MIDI.Util                  as U
import           STFS.Package                     (extractSTFS, rb2pkg, rb3pkg,
                                                   stfsFolder)
import           System.Console.GetOpt
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropExtension,
                                                   dropTrailingPathSeparator,
                                                   splitFileName, takeDirectory,
                                                   takeExtension, takeFileName,
                                                   (-<.>), (<.>), (</>))
import qualified System.IO                        as IO
import           Text.Decode                      (decodeGeneral)
import           Text.Printf                      (printf)
import           Text.Read                        (readMaybe)
import           U8                               (packU8)

#ifdef WINDOWS
import           Data.Bits                        (testBit)
import           System.Win32.File                (getLogicalDrives)
#else
import           Data.List                        (isPrefixOf)
import           System.MountPoints
#endif

loadRBProj :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m RBProj.RBProj
loadRBProj f = inside f $ do
  stackIO (decodeGeneral <$> B.readFile f)
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

data Command = Command
  { commandWord  :: T.Text
  , commandRun   :: forall m. (MonadResource m) => [FilePath] -> [OnyxOption] -> StackTraceT (QueueLog m) [FilePath]
  , commandDesc  :: T.Text
  , commandUsage :: T.Text
  }

identifyFile :: FilePath -> IO FileResult
identifyFile fp = Dir.doesFileExist fp >>= \case
  True -> case takeExtension fp of
    ".yml" -> return $ FileType FileSongYaml fp
    ".rbproj" -> return $ FileType FileRBProj fp
    ".moggsong" -> return $ FileType FileMOGGSong fp
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
  | FileMOGGSong
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

optIndex :: [OnyxOption] -> Maybe Int
optIndex opts = listToMaybe [ i | OptIndex i <- opts ]

buildTarget :: (MonadResource m) => FilePath -> [OnyxOption] -> StackTraceT (QueueLog m) (Target, FilePath)
buildTarget yamlPath opts = do
  songYaml <- loadYaml yamlPath
  targetName <- case [ t | OptTarget t <- opts ] of
    []    -> fatal "command requires --target, none given"
    t : _ -> return t
  audioDirs <- withProject (optIndex opts) yamlPath getAudioDirs
  target <- case Map.lookup targetName $ _targets songYaml of
    Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
    Just target -> return target
  let built = case target of
        RB3   {} -> "gen/target" </> T.unpack targetName </> "rb3con"
        RB2   {} -> "gen/target" </> T.unpack targetName </> "rb2con"
        PS    {} -> "gen/target" </> T.unpack targetName </> "ps.zip"
        GH2   {} -> "gen/target" </> T.unpack targetName </> "gh2.zip"
        Melody{} -> undefined -- TODO
        Konga {} -> undefined -- TODO
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
  fp' = fromMaybe fp $ stripSuffix sfxOld fp
  (dir, file) = splitFileName fp'
  in dir </> take (len - length sfx) file ++ sfx

changeToVenueGen :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m ()
changeToVenueGen dir = do
  let midPath = dir </> "notes.mid"
  mid <- stackIO (Load.fromFile midPath) >>= RBFile.readMIDIFile'
  stackIO $ Save.toFile midPath $ RBFile.showMIDIFile' $ RBFile.convertToVenueGen mid

commands :: [Command]
commands =

  [ Command
    { commandWord = "build"
    , commandDesc = "Compile an onyx or Magma project."
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
        out <- fmap (takeDirectory rbprojPath </>)
          $ outputFile opts $ return $ T.unpack $ RBProj.destinationFile $ RBProj.project rbproj
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
    , commandRun = \files opts -> case files of
      [] -> return []
      yml : builds -> identifyFile' yml >>= \case
        (FileSongYaml, yml') -> do
          audioDirs <- withProject (optIndex opts) yml getAudioDirs
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
                PS    {} -> FileZip
                RB3   {} -> FileSTFS
                RB2   {} -> FileSTFS
                GH2   {} -> undefined -- TODO
                Melody{} -> undefined -- TODO
                Konga {} -> undefined -- TODO
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
            audioDirs <- withProject (optIndex opts) yamlPath getAudioDirs
            planName <- getPlanName (Just "author") yamlPath opts
            let rpp = "notes-" <> T.unpack planName <> ".RPP"
                yamlDir = takeDirectory yamlPath
            when (OptVenueGen `elem` opts) $ changeToVenueGen yamlDir
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
            void $ importFoF dir out
            withSongYaml $ out </> "song.yml"
      case files' of
        [(FileSTFS, stfsPath)] -> doImport (importSTFS 0) stfsPath
        [(FileDTA, dtaPath)] -> doImport (importSTFSDir 0) $ takeDirectory $ takeDirectory dtaPath
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
    { commandWord = "player"
    , commandDesc = "Create a web browser chart playback app."
    , commandUsage = ""
    , commandRun = \files opts -> do
      fpath <- case files of
        []  -> return "."
        [x] -> return x
        _   -> fatal "Expected 0 or 1 files/folders"
      proj <- openProject (optIndex opts) fpath
      out <- outputFile opts $ return $ projectTemplate proj ++ "_player"
      player <- buildPlayer (getMaybePlan opts) proj
      case projectRelease proj of
        Nothing -> return [player </> "index.html"] -- onyx project, return folder directly
        Just _ -> do
          stackIO $ Dir.createDirectoryIfMissing False out
          copyDirRecursive player out
          return [out </> "index.html"]
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
        audioDirs <- withProject (optIndex opts) yamlPath getAudioDirs
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
    , commandRun = \files opts -> do
      fpath <- case files of
        []  -> return "."
        [x] -> return x
        _   -> fatal "Expected 0 or 1 files/folders"
      proj <- openProject (optIndex opts) fpath
      out <- outputFile opts $ return $ projectTemplate proj ++ "_import"
      stackIO $ Dir.createDirectoryIfMissing False out
      copyDirRecursive (takeDirectory $ projectLocation proj) out
      when (OptVenueGen `elem` opts) $ changeToVenueGen out
      return [out]
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
        FileSongYaml -> withProject (optIndex opts) fpath $ proKeysHanging $ getMaybePlan opts
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
    , commandRun = \args opts -> do
      f <- outputFile opts $ return "."
      (ftype, fpath) <- identifyFile' f
      pathMid <- case ftype of
        FileMidi -> return fpath
        FileSongYaml -> return $ takeDirectory fpath </> "notes.mid"
        FileRBProj -> undone
        _ -> fatal "Unrecognized --to argument, expected .mid/.yml/.rbproj"
      mid <- stackIO (Load.fromFile pathMid) >>= RBFile.readMIDIFile'
      let existingTracks = RBFile.rawTracks $ RBFile.s_tracks mid
          existingNames = mapMaybe U.trackName existingTracks
      case filter (`notElem` existingNames) args of
        []      -> return ()
        newTrks -> stackIO $ Save.toFile pathMid $ RBFile.showMIDIFile' mid
          { RBFile.s_tracks = RBFile.RawFile $ existingTracks ++ map (`U.setTrackName` RTB.empty) newTrks
          }
      return [pathMid]
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
    { commandWord = "dolphin-midi"
    , commandDesc = "Apply useful changes to MIDI files for Dolphin preview recording."
    , commandUsage = T.unlines
      [ "onyx dolphin-midi [notes.mid|song.yml|magma.rbproj|song.ini] [--wii-no-fills] [--wii-mustang-22]"
      , "# or run on current directory"
      , "onyx dolphin-midi [--wii-no-fills] [--wii-mustang-22]"
      , "# by default, creates .dolphin.mid. or specify --to"
      , "onyx dolphin-midi notes.mid --to new.mid [--wii-no-fills] [--wii-mustang-22]"
      ]
    , commandRun = \files opts -> do
      fin <- getInputMIDI files
      fout <- outputFile opts $ return $ fin -<.> "dolphin.mid"
      applyMidiFunction (getDolphinFunction opts) fin fout
      return [fout]
    }

  , Command
    { commandWord = "dolphin"
    , commandDesc = "Combine CON files into two U8 packages intended for RB3 in Dolphin."
    , commandUsage = "onyx dolphin 1_rb3con 2_rb3con --to dir/"
    , commandRun = \args opts -> do
      out <- outputFile opts $ fatal "need output folder for .app files"
      runDolphin args (getDolphinFunction opts) False out
    }

  , Command
    { commandWord = "vgs"
    , commandDesc = "Convert VGS audio to a set of WAV files."
    , commandUsage = "onyx vgs in.vgs"
    , commandRun = \args _opts -> case args of
      [fin] -> do
        srcs <- liftIO $ readVGS fin
        forM (zip [0..] srcs) $ \(i, src) -> do
          let fout = dropExtension fin ++ "_" ++ show (i :: Int) ++ ".wav"
          runAudio (CA.mapSamples CA.fractionalSample src) fout
          return fout
      _ -> fatal "Expected 1 argument (.vgs file)"
    }

  , Command
    { commandWord = "install-ark"
    , commandDesc = "Install a song into GH2 (PS2)."
    , commandUsage = "onyx install-ark song GEN/ songtoreplace --target gh2"
    , commandRun = \args opts -> case args of
      [song, gen, replace] -> withProject (optIndex opts) song $ \proj -> do
        targetName <- case [ t | OptTarget t <- opts ] of
          []    -> fatal "command requires --target, none given"
          t : _ -> return t
        target <- case Map.lookup targetName $ _targets $ projectSongYaml proj of
          Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
          Just target -> return target
        gh2 <- case target of
          GH2 gh2 -> return gh2
          _       -> fatal $ "Target is not for GH2: " <> show targetName
        installGH2 gh2 proj (B8.pack replace) gen
        return [gen]
      _ -> fatal "Expected 3 arguments (input song, GEN, song to replace)"
    }

  , Command
    { commandWord = "lipsync-midi"
    , commandDesc = "Transfer data from a lipsync file to a MIDI file."
    , commandUsage = "onyx lipsync-midi in.mid in.lipsync [in2.lipsync [in3.lipsync]] out.mid"
    , commandRun = \args _opts -> case args of
      fmid : (unsnoc -> Just (fvocs, fout)) -> do
        stackIO $ testConvertLipsync fmid fvocs fout
        return [fout]
      _ -> fatal "Expected at least 2-5 arguments (input midi, 0-3 input lipsyncs, output midi)"
    }

  , Command
    { commandWord = "milo"
    , commandDesc = "Combine a list of pieces into a .milo_xxx file."
    , commandUsage = "onyx milo piece1.bin piece2.bin ... --to out.milo_xbox"
    , commandRun = \args opts -> do
      out <- case [ to | OptTo to <- opts ] of
        []    -> fatal "milo command requires --to, none given"
        f : _ -> return f
      parts <- stackIO $ mapM BL.readFile args
      stackIO $ BL.writeFile out $ addMiloHeader $
        BL.intercalate (BL.pack [0xAD, 0xDE, 0xAD, 0xDE]) parts
      return [out]
    }

  , Command
    { commandWord = "unmilo"
    , commandDesc = "Decompress and split the data inside a .milo_xxx file."
    , commandUsage = "onyx unmilo in.milo"
    , commandRun = \args _opts -> case args of
      [fin] -> do
        bs <- stackIO $ BL.readFile fin
        dec <- case runGetOrFail decompressMilo bs of
          Left (_, pos, err) -> fatal $ "Failed to decompress the milo. Error at position " <> show pos <> ": " <> err
          Right (_, _, x)    -> return x
        let dout = fin ++ "_extract"
        stackIO $ Dir.createDirectoryIfMissing False dout
        stackIO $ BL.writeFile (dout </> "decompressed.bin") dec
        case runGetOrFail parseMiloFile dec of
          Left (_, pos, err) -> do
            stackIO $ forM_ (zip [0..] $ breakMilo $ BL.toStrict dec) $ \(i, piece) -> do
              let num = case show (i :: Int) of
                    [c] -> ['0', c]
                    s   -> s
              B.writeFile (dout </> "piece" <> num <.> "bin") piece
            inside "Parsing .milo structure" $ inside ("position " <> show pos) $ warn err
            lg "Couldn't parse milo structure; split into simple pieces."
          Right (_, _, topdir) -> do
            let go parent milodir = do
                  let thisout = parent </> B8.unpack (miloName milodir)
                  Dir.createDirectoryIfMissing False thisout
                  forM_ (zip (miloEntryNames milodir) (miloFiles milodir)) $ \((_typ, name), fileBytes) -> do
                    BL.writeFile (thisout </> B8.unpack name) fileBytes
                  mapM_ (go thisout) $ miloSubdirs milodir
                removeFiles dir = dir
                  { miloSubdirs = map removeFiles $ miloSubdirs dir
                  , miloFiles = map (const "(file contents)") $ miloFiles dir
                  }
            stackIO $ go dout topdir
            stackIO $ writeFile (dout </> "parsed.txt") $ show $ removeFiles topdir
            lg "Recognized and extracted milo contents."
        return [dout]
      _ -> fatal "Expected 1 argument (input milo)"
    }

  ]

runDolphin
  :: (MonadResource m, SendMessage m)
  => [FilePath] -- ^ CONs
  -> Maybe (RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)) -- ^ MIDI transform
  -> Bool -- ^ Try to make preview audio?
  -> FilePath -- ^ output dir
  -> StackTraceT m [FilePath] -- ^ 2 .app files
runDolphin cons midfn makePreview out = do
  stackIO $ Dir.createDirectoryIfMissing False out
  let out_meta = out </> "00000001.app"
      out_song = out </> "00000002.app"
  tempDir "onyx_dolphin" $ \temp -> do
    let extract  = temp </> "extract"
        dir_meta = temp </> "meta"
        dir_song = temp </> "song"
    allSongs <- fmap concat $ forM cons $ \stfs -> do
      lg $ "STFS: " <> stfs
      stackIO $ Dir.createDirectory extract
      stackIO $ extractSTFS stfs extract
      songs <- readFileSongsDTA $ extract </> "songs/songs.dta"
      prevEnds <- forM songs $ \(song, _) -> do
        let DTASingle _ pkg _ = song
            songsXX = T.unpack $ D.songName $ D.song pkg
            songsXgen = takeDirectory songsXX </> "gen"
            songsXgenX = songsXgen </> takeFileName songsXX
        lg $ "Song: " <> T.unpack (D.name pkg) <> " (" <> T.unpack (D.artist pkg) <> ")"
        stackIO $ Dir.createDirectoryIfMissing True $ dir_meta </> "content" </> songsXgen
        stackIO $ Dir.createDirectoryIfMissing True $ dir_song </> "content" </> songsXgen
        -- .mid (use unchanged, or edit to remove fills and/or mustang PG)
        let midin  = extract </> songsXX <.> "mid"
            midout = dir_song </> "content" </> songsXX <.> "mid"
        case midfn of
          Nothing -> stackIO $ Dir.copyFile midin midout
          Just f  -> do
            mid <- stackIO (Load.fromFile midin) >>= RBFile.readMIDIFile'
            stackIO $ Save.toFile midout $ RBFile.showMIDIFile' $ f mid
        -- .mogg (use unchanged)
        let mogg = dir_song </> "content" </> songsXX <.> "mogg"
        stackIO $ Dir.renameFile (extract </> songsXX <.> "mogg") mogg
        -- _prev.mogg (generate if possible)
        let ogg = extract </> "temp.ogg"
            prevOgg = extract </> "temp_prev.ogg"
            (prevStart, prevEnd) = D.preview pkg
            prevLength = min 15000 $ prevEnd - prevStart
            silentPreview = stackIO
              $ runResourceT
              $ sinkSnd prevOgg (Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile)
              $ (CA.silent (CA.Seconds $ realToFrac prevLength / 1000) 44100 2 :: CA.AudioSource (ResourceT IO) Int16)
        if makePreview
          then errorToEither (moggToOgg mogg ogg) >>= \case
            Right () -> do
              lg "Creating preview file"
              src <- stackIO $ sourceSndFrom (CA.Seconds $ realToFrac prevStart / 1000) ogg
              stackIO
                $ runResourceT
                $ sinkSnd prevOgg (Snd.Format Snd.HeaderFormatOgg Snd.SampleFormatVorbis Snd.EndianFile)
                $ fadeStart (CA.Seconds 0.75)
                $ fadeEnd (CA.Seconds 0.75)
                $ CA.takeStart (CA.Seconds $ realToFrac prevLength / 1000)
                $ applyPansVols (D.pans $ D.song pkg) (D.vols $ D.song pkg)
                $ src
            Left _msgs -> do
              lg "Encrypted audio, skipping preview"
              silentPreview
          else silentPreview
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
    lg "Writing combined songs.dta"
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
    lg "Packing into U8 files"
    stackIO $ packU8 dir_meta out_meta
    stackIO $ packU8 dir_song out_song
  return [out_meta, out_song]

commandLine :: (MonadResource m) => [String] -> StackTraceT (QueueLog m) [FilePath]
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
  , Option []   ["speed"          ] (ReqArg (OptSpeed . read)      "real"     ) ""
  , Option []   ["rb2-version"    ] (NoArg  OptRB2Version                     ) ""
  , Option []   ["wii-no-fills"   ] (NoArg  OptWiiNoFills                     ) ""
  , Option []   ["wii-mustang-22" ] (NoArg  OptWiiMustang22                   ) ""
  , Option []   ["wii-unmute-22"  ] (NoArg  OptWiiUnmute22                    ) ""
  , Option []   ["venuegen"       ] (NoArg  OptVenueGen                       ) ""
  , Option []   ["index"          ] (ReqArg (OptIndex . read)      "int"      ) ""
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
  | OptSpeed Double
  | OptRB2Version
  | OptWiiNoFills
  | OptWiiMustang22
  | OptWiiUnmute22
  | OptVenueGen
  | OptIndex Int
  | OptHelp
  deriving (Eq, Ord, Show, Read)

applyMidiFunction
  :: (MonadIO m, SendMessage m)
  => Maybe (RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)) -- ^ MIDI transform
  -> FilePath
  -> FilePath
  -> StackTraceT m ()
applyMidiFunction Nothing fin fout = stackIO $ Dir.copyFile fin fout
applyMidiFunction (Just fn) fin fout = do
  mid <- stackIO (Load.fromFile fin) >>= RBFile.readMIDIFile'
  stackIO $ Save.toFile fout $ RBFile.showMIDIFile' $ fn mid

getDolphinFunction
  :: [OnyxOption]
  -> Maybe (RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats))
getDolphinFunction opts = let
  nofills   = elem OptWiiNoFills   opts
  mustang22 = elem OptWiiMustang22 opts
  unmute22  = elem OptWiiUnmute22  opts
  in do
    guard $ or [nofills, mustang22, unmute22]
    Just
      $ (if nofills   then RBFile.wiiNoFills   else id)
      . (if mustang22 then RBFile.wiiMustang22 else id)
      . (if unmute22  then RBFile.wiiUnmute22  else id)

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
