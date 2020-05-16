{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
module CommandLine
( commandLine
, identifyFile'
, FileType(..)
, copyDirRecursive
, runDolphin
, blackVenue
) where

import           Audio                            (applyPansVols, fadeEnd,
                                                   fadeStart, runAudio)
import           Build                            (loadYaml, shakeBuildFiles)
import           Config
import           Control.Monad.Codec              (codecIn)
import           Control.Monad.Extra              (filterM, forM, forM_, guard,
                                                   when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource, ResourceT,
                                                   runResourceT)
import           Control.Monad.Trans.StackTrace
import           Data.Binary.Get                  (runGetOrFail)
import           Data.Binary.Put                  (runPut)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.ByteString.Lazy.Char8       ()
import           Data.Char                        (isAlphaNum, isAscii, isDigit,
                                                   isUpper)
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
import qualified Data.HashMap.Strict              as HM
import           Data.Int                         (Int16)
import           Data.List.Extra                  (nubOrd, stripSuffix, unsnoc)
import           Data.List.HT                     (partitionMaybe)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe, mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Data.Word                        (Word32)
import           GuitarHeroII.Audio               (readVGS)
import           GuitarHeroIOS                    (extractIGA)
import qualified Image
import           Import
import           Magma                            (getRBAFile, runMagma,
                                                   runMagmaMIDI, runMagmaV1)
import           MoggDecrypt                      (moggToOgg, oggToMogg)
import           OpenProject
import           OSFiles                          (copyDirRecursive)
import           PrettyDTA                        (DTASingle (..),
                                                   readDTASingles,
                                                   readFileSongsDTA, readRB3DTA,
                                                   writeDTASingle)
import           ProKeysRanges                    (closeShiftsFile)
import           Reaper.Build                     (makeReaper)
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Codec.Vocal             (nullVox)
import           RockBand.Common                  (Difficulty (..),
                                                   pattern RNil, pattern Wait,
                                                   makeEdgeCPV)
import qualified RockBand.IOS                     as IOS
import           RockBand.Milo                    (SongPref, autoLipsync,
                                                   beatlesLipsync,
                                                   englishVowels,
                                                   lipsyncFromMIDITrack,
                                                   packMilo, putLipsync,
                                                   setBeatles,
                                                   testConvertLipsync,
                                                   unpackMilo)
import           RockBand.Score
import           Rocksmith.Sng2014                (bin)
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Parser.Report         as Report
import qualified Sound.MIDI.Script.Base           as MS
import qualified Sound.MIDI.Script.Parse          as MS
import qualified Sound.MIDI.Script.Read           as MS
import qualified Sound.MIDI.Script.Scan           as MS
import qualified Sound.MIDI.Util                  as U
import           STFS.Package
import           System.Console.GetOpt
import qualified System.Directory                 as Dir
import           System.FilePath                  (dropExtension,
                                                   dropTrailingPathSeparator,
                                                   splitDirectories,
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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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

buildTarget :: (MonadResource m) => FilePath -> [OnyxOption] -> StackTraceT (QueueLog m) (Target FilePath, FilePath)
buildTarget yamlPath opts = do
  songYaml <- loadYaml yamlPath
  targetName <- case [ t | OptTarget t <- opts ] of
    []    -> fatal "command requires --target, none given"
    t : _ -> return t
  audioDirs <- withProject (optIndex opts) yamlPath getAudioDirs
  target <- case HM.lookup targetName $ _targets songYaml of
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
    case HM.keys $ _plans (songYaml :: SongYaml FilePath) of
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
  mid <- RBFile.loadMIDI midPath
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
              makeReaper [] mid mid audio rpp
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
      let withMIDI mid = RBFile.loadMIDI mid >>= lg . closeShiftsFile
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
              suffix = case game of GameRB3 -> "_rb3con"; GameRB2 -> "_rb2con"; GameTBRB -> "_tbrbcon"
          pkg <- case game of
            GameRB3  -> return rb3pkg
            GameRB2  -> return rb2pkg
            GameTBRB -> fatal "TBRB unsupported as game for stfs command"
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
        res <- MS.toStandardMIDI <$> RBFile.loadRawMIDI fpath
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
        res <- MS.toStandardMIDI <$> RBFile.loadRawMIDI mid
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
        baseMid <- RBFile.loadMIDI base
        tracksMid <- RBFile.loadMIDI tracks
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        let newMid = RBFile.mergeCharts (RBFile.TrackPad 0) baseMid tracksMid
        stackIO $ Save.toFile out $ RBFile.showMIDIFile' newMid
        return [out]
      [base, tracks, "pad", n] -> do
        baseMid <- RBFile.loadMIDI base
        tracksMid <- RBFile.loadMIDI tracks
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        n' <- case readMaybe n of
          Nothing -> fatal "Invalid merge pad amount"
          Just d  -> return $ realToFrac (d :: Double)
        let newMid = RBFile.mergeCharts (RBFile.TrackPad n') baseMid tracksMid
        stackIO $ Save.toFile out $ RBFile.showMIDIFile' newMid
        return [out]
      [base, tracks, "drop", n] -> do
        baseMid <- RBFile.loadMIDI base
        tracksMid <- RBFile.loadMIDI tracks
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
      mid <- RBFile.loadMIDI pathMid
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
        target <- case HM.lookup targetName $ _targets $ projectSongYaml proj of
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
    { commandWord = "lipsync-gen"
    , commandDesc = "Make lipsync files from the vocal tracks in a MIDI file."
    , commandUsage = "onyx lipsync-gen in.mid"
    , commandRun = \args opts -> case args of
      [fmid] -> do
        mid <- RBFile.loadMIDI fmid
        let template = dropExtension fmid
            tracks =
              [ (RBFile.fixedPartVocals, "_solovox.lipsync", False)
              , (RBFile.fixedHarm1, "_harm1.lipsync", False)
              , (RBFile.fixedHarm2, "_harm2.lipsync", False)
              , (RBFile.fixedHarm3, "_harm3.lipsync", False)
              , (const mempty, "_empty.lipsync", True)
              ]
            game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
            voxToLip = case game of
              GameRB3  -> autoLipsync englishVowels
              GameRB2  -> autoLipsync englishVowels
              GameTBRB -> beatlesLipsync englishVowels
        fmap catMaybes $ forM tracks $ \(getter, suffix, alwaysWrite) -> do
          let trk = getter $ RBFile.s_tracks mid
              fout = template <> suffix
          if nullVox trk && not alwaysWrite
            then return Nothing
            else do
              let lip = voxToLip $ mapTrack (U.applyTempoTrack $ RBFile.s_tempos mid) trk
              stackIO $ BL.writeFile fout $ runPut $ putLipsync lip
              return $ Just fout
      _ -> fatal "Expected 1 argument (input midi)"
    }

  , Command
    { commandWord = "lipsync-track-gen"
    , commandDesc = "Make lipsync files from LIPSYNC{1,2,3} in a MIDI file."
    , commandUsage = "onyx lipsync-gen in.mid"
    , commandRun = \args opts -> case args of
      [fmid] -> do
        mid <- RBFile.loadMIDI fmid
        let template = dropExtension fmid
            tracks =
              [ (RBFile.onyxLipsync1, "_1.lipsync")
              , (RBFile.onyxLipsync2, "_2.lipsync")
              , (RBFile.onyxLipsync3, "_3.lipsync")
              ]
            game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
            voxToLip
              = (if game == GameTBRB then setBeatles else id)
              . lipsyncFromMIDITrack
        fmap concat $ forM (Map.toList $ RBFile.onyxParts $ RBFile.s_tracks mid) $ \(fpart, opart) -> do
          fmap catMaybes $ forM tracks $ \(getter, suffix) -> do
            let trk = getter opart
                fout = template <> "_" <> T.unpack (RBFile.getPartName fpart) <> suffix
            if trk == mempty
              then return Nothing
              else do
                let lip = voxToLip $ mapTrack (U.applyTempoTrack $ RBFile.s_tempos mid) trk
                stackIO $ BL.writeFile fout $ runPut $ putLipsync lip
                return $ Just fout
      _ -> fatal "Expected 1 argument (input midi)"
    }

  , Command
    { commandWord = "unpref"
    , commandDesc = "Parse a BandSongPref file"
    , commandUsage = "onyx unpref BandSongPref [--to out.txt]"
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <.> "txt"
        bs <- stackIO $ BL.readFile fin
        case runGetOrFail (codecIn bin) bs of
          Right (_, _, pref) -> stackIO $ writeFile fout $ show (pref :: SongPref)
          Left (_, pos, err) -> fatal $ "Binary Get at position " <> show pos <> ": " <> err
        return [fout]
      _ -> fatal "Expected 1 argument (BandSongPref file)"
    }

  , Command
    { commandWord = "unmilo"
    , commandDesc = "Decompress and split the data inside a .milo_xxx file."
    , commandUsage = "onyx unmilo in.milo_xxx [--to dir]"
    , commandRun = \args opts -> case args of
      [fin] -> do
        dout <- outputFile opts $ return $ fin ++ "_extract"
        unpackMilo fin dout
        return [dout]
      _ -> fatal "Expected 1 argument (input milo)"
    }

  , Command
    { commandWord = "milo"
    , commandDesc = "Recombine a split .milo_xxx file."
    , commandUsage = "onyx milo dir [--to out.milo_xxx]"
    , commandRun = \args opts -> case args of
      [din] -> do
        fout <- outputFile opts $ return $ dropTrailingPathSeparator din <.> "milo_xbox"
        packMilo din fout
        return [fout]
      _ -> fatal "Expected 1 argument (input dir)"
    }

  , Command
    { commandWord = "benchmark-midi"
    , commandDesc = ""
    , commandUsage = "onyx benchmark-midi [raw|fixed|onyx] in.mid [--to out.txt]"
    , commandRun = \args opts -> case args of
      ["raw", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- RBFile.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: RBFile.Song (RBFile.RawFile U.Beats))
        return [fout]
      ["raw-len", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- RBFile.loadMIDI fin
        stackIO $ writeFile fout $ show $ length $ show (mid :: RBFile.Song (RBFile.RawFile U.Beats))
        return [fout]
      ["fixed", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- RBFile.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: RBFile.Song (RBFile.FixedFile U.Beats))
        return [fout]
      ["onyx", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- RBFile.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: RBFile.Song (RBFile.OnyxFile U.Beats))
        return [fout]
      _ -> fatal "Expected 2 arguments (raw/fixed/onyx, then a midi file)"
    }

  , Command
    { commandWord = "scoring"
    , commandDesc = ""
    , commandUsage = T.unlines
      [ "onyx scoring in.mid [players]"
      , "players is a sequence that alternates:"
      , "  part: G B D K V PG PB PD PK H"
      , "  difficulty: e m h x"
      , "for example: onyx scoring in.mid G x B x PD x V x"
      ]
    , commandRun = \args _opts -> case args of
      fin : players -> do
        mid <- RBFile.loadMIDI fin
        case players of
          [] -> stackIO $ forM_ [minBound .. maxBound] $ \scoreTrack -> do
            forM_ [minBound .. maxBound] $ \diff -> do
              let stars = starCutoffs (RBFile.s_tracks mid) [(scoreTrack, diff)]
              when (any (maybe False (> 0)) stars) $ do
                putStrLn $ unwords [show scoreTrack, show diff, show stars]
          _ -> let
            parsePlayers [] = Right []
            parsePlayers [_] = Left "Odd number of player arguments"
            parsePlayers (p : d : rest) = do
              part <- maybe (Left $ "Unrecognized track name: " <> p) Right $ lookup p
                [ (filter (\c -> isUpper c || isDigit c) $ drop 5 $ show strack, strack) | strack <- [minBound .. maxBound] ]
              diff <- maybe (Left $ "Unrecognized difficulty: " <> d) Right $ lookup d
                [("e", Easy), ("m", Medium), ("h", Hard), ("x", Expert)]
              ((part, diff) :) <$> parsePlayers rest
            in case parsePlayers players of
              Left  err   -> fatal err
              Right pairs -> stackIO $ do
                forM_ pairs $ \pair -> let
                  (base, solo) = baseAndSolo (RBFile.s_tracks mid) pair
                  in putStrLn $ show pair <> ": base " <> show base <> ", solo bonuses " <> show solo
                print $ starCutoffs (RBFile.s_tracks mid) pairs
        return []
      _ -> fatal "Expected 1 argument (input midi)"
    }

  , Command
    { commandWord = "black"
    , commandDesc = "Replace the VENUE track in CON MIDIs with a black background."
    , commandUsage = "onyx black con_1 con_2 ..."
    , commandRun = \args _opts -> do
      mapM_ blackVenue args
      return args
    }

  , Command
    { commandWord = "ios"
    , commandDesc = "Extract the contents of a Rock Band iOS, Rock Band Reloaded, or Guitar Hero iOS song."
    , commandUsage = "onyx ios [song.blob|song.iga] ..."
    , commandRun = \args _opts -> fmap concat $ forM args $ \arg -> case takeExtension arg of
      ".blob" -> stackIO $ do
        dec <- IOS.decodeBlob arg
        (blob, dats) <- IOS.loadBlob arg
        let blobDec = arg <.> "dec"
            blobTxt = arg <.> "txt"
        B.writeFile blobDec dec
        writeFile blobTxt $ show blob
        forM_ dats $ \(fout, bs) -> B.writeFile fout bs
        return $ blobDec : blobTxt : fmap fst dats
      ".iga" -> stackIO $ extractIGA arg
      _ -> fatal $ "Unrecognized iOS game file extension: " <> arg
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
            mid <- RBFile.loadMIDI midin
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
      "rb3"  -> GameRB3
      "rb2"  -> GameRB2
      "tbrb" -> GameTBRB
      g      -> error $ "Unrecognized --game value: " ++ show g

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
  deriving (Eq, Ord, Show)

applyMidiFunction
  :: (MonadIO m, SendMessage m)
  => Maybe (RBFile.Song (RBFile.FixedFile U.Beats) -> RBFile.Song (RBFile.FixedFile U.Beats)) -- ^ MIDI transform
  -> FilePath
  -> FilePath
  -> StackTraceT m ()
applyMidiFunction Nothing fin fout = stackIO $ Dir.copyFile fin fout
applyMidiFunction (Just fn) fin fout = do
  mid <- RBFile.loadMIDI fin
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
  | GameTBRB
  deriving (Eq, Ord, Show)

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

blackVenue :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m ()
blackVenue fcon = inside ("Inserting black VENUE in: " <> fcon) $ do
  (hdr, meta) <- stackIO $ openSTFS fcon $ \pkg -> return (stfsHeader pkg, stfsMetadata pkg)
  case hdr of
    CON _ -> do
      filePairs <- stackIO $ withSTFS fcon $ mapM sequence . stfsFiles
      let isMid = (== ".mid") . takeExtension
      if any (isMid . fst) filePairs
        then do
          filePairs' <- flip mapM filePairs $ \(f, bs) -> if isMid f
            then do
              F.Cons typ dvn trks <- inside ("loading " <> f) $ either fatal return
                $ Report.result $ Load.maybeFromByteString bs
              isRB3 <- case lookup ("songs" </> "songs.dta") filePairs of
                Nothing -> fatal "Couldn't find songs.dta in package"
                Just dtaBS -> do
                  songs <- readDTASingles $ BL.toStrict dtaBS
                  case map (D.songFormat . dtaSongPackage . fst) songs of
                    [] -> fatal "No songs found in songs.dta"
                    fmts  | all (>= 10) fmts -> return True
                          | all (< 10) fmts -> return False
                          | otherwise -> fatal
                            "Mix of RB3 and pre-RB3 songs found in pack (???)"
              let black = U.setTrackName "VENUE" $ if isRB3
                    then RTB.fromPairList $ map (\s -> (0, E.MetaEvent $ Meta.TextEvent s))
                      ["[lighting (blackout_fast)]", "[film_b+w.pp]", "[coop_all_far]"]
                    else Wait 0 (E.MetaEvent $ Meta.TextEvent "[verse]")
                      $ Wait 0 (E.MetaEvent $ Meta.TextEvent "[lighting (blackout_fast)]")
                      $ Wait 0 (makeEdgeCPV 0 60 $ Just 96) -- camera cut
                      $ Wait 0 (makeEdgeCPV 0 61 $ Just 96) -- focus bass
                      $ Wait 0 (makeEdgeCPV 0 62 $ Just 96) -- focus drums
                      $ Wait 0 (makeEdgeCPV 0 63 $ Just 96) -- focus guitar
                      $ Wait 0 (makeEdgeCPV 0 64 $ Just 96) -- focus vocal
                      $ Wait 0 (makeEdgeCPV 0 71 $ Just 96) -- only far
                      $ Wait 0 (makeEdgeCPV 0 108 $ Just 96) -- video_bw
                      $ Wait 120 (makeEdgeCPV 0 60 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 61 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 62 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 63 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 64 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 71 Nothing)
                      $ Wait 0 (makeEdgeCPV 0 108 Nothing) RNil
                  isVenue = (== Just "VENUE") . U.trackName
                  mid = F.Cons typ dvn $ filter (not . isVenue) trks ++ [black]
              return (splitDirectories f, Save.toByteString mid)
            else return (splitDirectories f, bs)
          let opts = CreateOptions
                { createName          = head $ md_DisplayName meta
                , createDescription   = head $ md_DisplayDescription meta
                , createTitleID       = md_TitleID meta
                , createTitleName     = md_TitleName meta
                , createThumb         = md_ThumbnailImage meta
                , createTitleThumb    = md_TitleThumbnailImage meta
                , createLicense       = LicenseEntry (-1) 1 0 -- unlocked
                , createMediaID       = 0
                , createVersion       = 0
                , createBaseVersion   = 0
                , createTransferFlags = 0xC0
                }
              mem = makeFolder filePairs'
              makeFolder pairs = let
                files = [ (concat parts, bs) | (parts, bs) <- pairs, null $ drop 1 parts ]
                folders = nubOrd [ dir | (dir : _ : _, _) <- pairs ]
                in [ MemoryFile (T.pack f) bs | (f, bs) <- files ] ++ do
                  dir <- folders
                  let inDir = [ (rest, bs) | (fileDir : rest, bs) <- pairs, fileDir == dir ]
                  return $ MemoryFolder (T.pack dir) $ makeFolder inDir
              ftemp = fcon <> ".tmp"
          stackIO $ makeCONMemory opts mem ftemp
          stackIO $ Dir.renameFile ftemp fcon
        else fatal "Couldn't find MIDI file in package"
    _ -> fatal "Package is LIVE/PIRS, can't edit"
