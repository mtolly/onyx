{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ViewPatterns          #-}
module Onyx.CommandLine
( commandLine
, identifyFile'
, FileType(..)
) where

import           Codec.Picture                        (writePng)
import           Control.Applicative                  ((<|>))
import           Control.Monad.Extra                  (filterM, forM, forM_,
                                                       guard, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource         (MonadResource)
import           Data.Bifunctor                       (bimap, first)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.ByteString.Lazy.Char8           ()
import           Data.Char                            (isAlphaNum, isAscii,
                                                       isDigit, isUpper,
                                                       toLower)
import qualified Data.Conduit.Audio                   as CA
import           Data.Default.Class                   (def)
import qualified Data.Digest.Pure.MD5                 as MD5
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra                      (find, stripSuffix,
                                                       unsnoc)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Text.IO                         as T
import qualified Data.Yaml                            as Y
import           Onyx.Amplitude.PS2.TxtBin            (getTxtBin, txtBinToDTA)
import           Onyx.Audio                           (Audio (Input),
                                                       audioLength, audioMD5,
                                                       makeFSB4, makeFSB4',
                                                       makeXMAFSB3, runAudio)
import           Onyx.Audio.FSB                       (parseXMA,
                                                       splitFSBStreamsToDir,
                                                       writeXMA2, xma1To2)
import           Onyx.Audio.VGS                       (readVGS)
import           Onyx.Build                           (shakeBuildFiles)
import           Onyx.Build.Neversoft                 (makeMetadataLIVE,
                                                       shareMetadata)
import           Onyx.Codec.Binary
import           Onyx.Codec.JSON                      (loadYaml, toJSON,
                                                       yamlEncodeFile)
import           Onyx.GuitarHero.IOS                  (extractIGA)
import qualified Onyx.Harmonix.Ark                    as Ark
import           Onyx.Harmonix.Ark.Amplitude          as AmpArk
import qualified Onyx.Harmonix.Ark.GH2                as GHArk
import           Onyx.Harmonix.DTA.C3                 (readRB3DTA)
import           Onyx.Harmonix.DTA.Parse              (parseStack)
import           Onyx.Harmonix.DTA.Print              (showDTA)
import           Onyx.Harmonix.DTA.Scan               (scanStack)
import qualified Onyx.Harmonix.DTA.Serialize          as D
import qualified Onyx.Harmonix.DTA.Serialize.Magma    as RBProj
import qualified Onyx.Harmonix.DTA.Serialize.RockBand as D
import           Onyx.Harmonix.GH2.File               (GH2File (..))
import           Onyx.Harmonix.GH2.PartGuitar         (nullPart)
import           Onyx.Harmonix.Magma                  (getRBAFile, runMagma,
                                                       runMagmaMIDI, runMagmaV1)
import           Onyx.Harmonix.MOGG                   (encryptRB1, moggToOgg,
                                                       oggToMogg)
import qualified Onyx.Harmonix.RockBand.IOS           as IOS
import           Onyx.Harmonix.RockBand.Milo          (SongPref, autoLipsync,
                                                       beatlesLipsync,
                                                       defaultTransition,
                                                       englishSyllables,
                                                       lipsyncFromMIDITrack,
                                                       loadVisemesRB3,
                                                       loadVisemesTBRB,
                                                       packMilo, parseVenue,
                                                       putLipsync, setBeatles,
                                                       testConvertLipsync,
                                                       testConvertVenue,
                                                       unpackMilo)
import           Onyx.Harmonix.RockBand.Score
import           Onyx.Image.DXT                       (readRBImageMaybe)
import           Onyx.Import
import           Onyx.ISO                             (folderISO)
import           Onyx.Keys.Ranges                     (closeShiftsFile)
import           Onyx.MIDI.Common                     (Difficulty (..))
import           Onyx.MIDI.Read                       (mapTrack)
import qualified Onyx.MIDI.Script.Base                as MS
import qualified Onyx.MIDI.Script.Parse               as MS
import qualified Onyx.MIDI.Script.Read                as MS
import qualified Onyx.MIDI.Script.Scan                as MS
import qualified Onyx.MIDI.Track.File                 as F
import           Onyx.MIDI.Track.Vocal                (nullVox)
import           Onyx.Neversoft.CRC                   (knownKeys, qbKeyCRC)
import           Onyx.Neversoft.Crypt                 (decryptFSB, gh3Encrypt,
                                                       ghworEncrypt)
import           Onyx.Neversoft.Note                  (loadNoteFile)
import           Onyx.Neversoft.Pak                   (Node (..), buildPak,
                                                       nodeFileType, qsBank,
                                                       splitPakNodes)
import           Onyx.Neversoft.QB                    (discardStrings, lookupQB,
                                                       lookupQS, parseQB, putQB)
import           Onyx.Nintendo.GCM                    (loadGCM)
import           Onyx.Nintendo.U8                     (packU8, readU8)
import           Onyx.Nintendo.WAD                    (getWAD, hackSplitU8s)
import           Onyx.PlayStation.PKG                 (PKG (..), loadPKG,
                                                       makePKG, tryDecryptEDATs)
import qualified Onyx.PowerGig.Crypt                  as PG
import           Onyx.Project
import           Onyx.Reaper.Build                    (TuningInfo (..),
                                                       makeReaper)
import           Onyx.Rocksmith.PSARC                 (extractPSARC)
import           Onyx.StackTrace
import           Onyx.Util.Files                      (copyDirRecursive,
                                                       fixFileCase,
                                                       shortWindowsPath)
import           Onyx.Util.Handle                     (byteStringSimpleHandle,
                                                       crawlFolder,
                                                       fileReadable, makeHandle,
                                                       saveHandleFolder)
import           Onyx.Util.Text.Decode                (decodeGeneral)
import           Onyx.Xbox.ISO                        (loadXboxISO)
import           Onyx.Xbox.STFS
import qualified Sound.MIDI.File.Save                 as Save
import qualified Sound.MIDI.Util                      as U
import           System.Console.GetOpt
import qualified System.Directory                     as Dir
import           System.FilePath                      (dropExtension,
                                                       dropTrailingPathSeparator,
                                                       splitExtension,
                                                       takeDirectory,
                                                       takeExtension,
                                                       takeFileName, (-<.>),
                                                       (<.>), (</>))
import qualified System.IO                            as IO
import           Text.Printf                          (printf)
import           Text.Read                            (readMaybe)

#ifdef WINDOWS
import           Data.Bits                            (testBit)
import           System.Win32.File                    (getLogicalDrives)
#else
import           Data.List                            (isPrefixOf)
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
      return (D.name pkg, D.name pkg <> maybe "" (\artist -> " (" <> artist <> ")") (D.artist pkg))
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
  True -> case map toLower $ takeExtension fp of
    ".yml" -> return $ FileType FileSongYaml fp
    ".rbproj" -> return $ FileType FileRBProj fp
    ".moggsong" -> return $ FileType FileMOGGSong fp
    ".midtxt" -> return $ FileType FileMidiText fp
    ".mogg" -> return $ FileType FileMOGG fp
    ".zip" -> return $ FileType FileZip fp
    ".chart" -> return $ FileType FileChart fp
    ".psarc" -> return $ FileType FilePSARC fp
    ".iso" -> return $ FileType FileISO fp
    ".2" | ".hdr.e.2" `T.isSuffixOf` T.toLower (T.pack $ takeFileName fp) -> return $ FileType FileHdrE2 fp
    _ -> case takeFileName fp of
      "song.ini" -> return $ FileType FilePS fp
      _ -> do
        magic <- IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 4
        return $ case magic of
          "RBSF" -> FileType FileRBA fp
          "CON " -> FileType FileSTFS fp
          "LIVE" -> FileType FileSTFS fp
          "MThd" -> FileType FileMidi fp
          "fLaC" -> FileType FileFLAC fp
          "OggS" -> FileType FileOGG fp
          "RIFF" -> FileType FileWAV fp
          _      -> case BL.unpack magic of
            [0xAF, 0xDE, 0xBE, 0xCA] -> FileType FileMilo fp
            [0xAF, 0xDE, 0xBE, 0xCB] -> FileType FileMilo fp
            [0xAF, 0xDE, 0xBE, 0xCC] -> FileType FileMilo fp
            [0xAF, 0xDE, 0xBE, 0xCD] -> FileType FileMilo fp
            [0x7F, 0x50, 0x4B, 0x47] -> FileType FilePKG fp
            _                        -> FileUnrecognized
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
                True  -> return $ FileType FileDTA $ fp </> "songs/songs.dta"
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
  | FilePSARC
  | FileMilo
  | FilePKG
  | FileHdrE2
  | FileISO
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

buildTarget :: (MonadResource m) => FilePath -> [OnyxOption] -> StackTraceT (QueueLog m) (Target, FilePath)
buildTarget yamlPath opts = do
  songYaml <- loadYaml yamlPath
  let _ = songYaml :: SongYaml FilePath
  targetName <- case [ t | OptTarget t <- opts ] of
    []    -> fatal "command requires --target, none given"
    t : _ -> return t
  audioDirs <- withProject (optIndex opts) yamlPath getAudioDirs
  target <- case HM.lookup targetName songYaml.targets of
    Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
    Just target -> return target
  let built = case target of
        RB3   {} -> "gen/target" </> T.unpack targetName </> "rb3con"
        RB2   {} -> "gen/target" </> T.unpack targetName </> "rb2con"
        PS    {} -> "gen/target" </> T.unpack targetName </> "ps.zip"
        GH2   {} -> "gen/target" </> T.unpack targetName </> "gh2.zip"
        GH1   {} -> undefined -- TODO
        GH3   {} -> undefined -- TODO
        GH5   {} -> undefined -- TODO
        RS    {} -> undefined -- TODO
        DTX   {} -> undefined -- TODO
        PG    {} -> undefined -- TODO
  shakeBuildFiles audioDirs yamlPath [built]
  return (target, takeDirectory yamlPath </> built)

getMaybePlan :: [OnyxOption] -> Maybe T.Text
getMaybePlan opts = listToMaybe [ p | OptPlan p <- opts ]

getPlanName :: (SendMessage m, MonadIO m) => Maybe T.Text -> FilePath -> [OnyxOption] -> StackTraceT m T.Text
getPlanName defaultPlan yamlPath opts = case getMaybePlan opts of
  Just p  -> return p
  Nothing -> do
    songYaml <- loadYaml yamlPath
    case HM.keys (songYaml :: SongYaml FilePath).plans of
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

changeToVenueGen :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m ()
changeToVenueGen dir = do
  let midPath = dir </> "notes.mid"
  mid <- F.loadMIDI midPath
  stackIO $ Save.toFile midPath $ F.showMIDIFile' $ F.convertToVenueGen mid

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
                GH1   {} -> undefined -- TODO
                GH2   {} -> undefined -- TODO
                GH3   {} -> undefined -- TODO
                GH5   {} -> undefined -- TODO
                RS    {} -> undefined -- TODO
                DTX   {} -> undefined -- TODO
                PG    {} -> undefined -- TODO
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
      fpath <- case files of
        []  -> return "."
        [x] -> return x
        _   -> fatal "Expected 0 or 1 files/folders"
      if takeExtension fpath == ".mid"
        then do
          rpp <- outputFile opts $ return $ fpath -<.> "RPP"
          makeReaper (TuningInfo [] 0) fpath fpath [] rpp
          return [rpp]
        else do
          proj <- openProject (optIndex opts) fpath
          yamlDir <- case projectRelease proj of
            Nothing -> return $ takeDirectory $ projectLocation proj
            Just _  -> do
              out <- outputFile opts $ return $ projectTemplate proj ++ "_reaper"
              stackIO $ Dir.createDirectoryIfMissing False out
              copyDirRecursive (takeDirectory $ projectLocation proj) out
              return out
          let yamlPath = yamlDir </> "song.yml"
          planName <- getPlanName (Just "author") yamlPath opts
          let rpp = "notes-" <> T.unpack planName <> ".RPP"
          when (OptVenueGen `elem` opts) $ changeToVenueGen yamlDir
          audioDirs <- withProject Nothing yamlPath getAudioDirs
          shakeBuildFiles audioDirs yamlPath [rpp]
          let rppFull = yamlDir </> "notes.RPP"
          stackIO $ Dir.renameFile (yamlDir </> rpp) rppFull
          return [rppFull]
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
      let withMIDI mid = F.loadMIDI mid >>= lg . T.unpack . closeShiftsFile
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
          (pkg, suffix) <- case game of
            GameRB3 -> return (rb3pkg, "_rb3con")
            GameRB2 -> return (rb2pkg, "_rb2con")
            GameGH2 -> return (gh2pkg, "_gh2live")
            _       -> fatal "Unsupported as game for stfs command"
          stfs <- outputFile opts
            $ (<> suffix) . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          (title, desc) <- getInfoForSTFS dir stfs
          pkg title desc dir stfs
          return [stfs]
        False -> fatal $ "onyx stfs expected directory; given: " <> dir
      _ -> fatal $ "onyx stfs expected 1 argument, given " <> show (length files)
    }

  , Command
    { commandWord = "extract"
    , commandDesc = "Extract various archive formats to a folder."
    , commandUsage = T.unlines
      [ "onyx extract file_in"
      , "onyx extract file_in --to folder_out"
      ]
    , commandRun = \files opts -> forM files $ \f -> identifyFile' f >>= \case
      (FileSTFS, stfs) -> do
        out <- outputFile opts $ return $ stfs ++ "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        stackIO $ extractSTFS stfs out
        repack <- stackIO $ withSTFSPackage stfs $ return . repackOptions
        stackIO $ saveHandleFolder (saveCreateOptions repack) $ out </> "onyx-repack"
        return out
      (FilePSARC, psarc) -> do
        out <- outputFile opts $ return $ psarc ++ "_extract"
        stackIO $ extractPSARC psarc out
        return out
      (FileRBA, rba) -> do
        out <- outputFile opts $ return $ rba ++ "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        getRBAFile 0 rba $ out </> "songs.dta"
        getRBAFile 1 rba $ out </> "notes.mid"
        getRBAFile 2 rba $ out </> "audio.mogg"
        getRBAFile 3 rba $ out </> "song.milo"
        getRBAFile 4 rba $ out </> "cover.bmp"
        getRBAFile 5 rba $ out </> "weights.bin"
        getRBAFile 6 rba $ out </> "extra.dta"
        return out
      (FileMilo, milo) -> do
        out <- outputFile opts $ return $ milo ++ "_extract"
        unpackMilo milo out
        return out
      (FilePKG, pkg) -> do
        out <- outputFile opts $ return $ pkg <> "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        folder <- stackIO (loadPKG pkg) >>= tryDecryptEDATs . pkgFolder
        stackIO $ saveHandleFolder (first TE.decodeLatin1 folder) out
        return out
      (FileHdrE2, hdrE2) -> do
        let base = takeWhile (/= '.') $ takeFileName hdrE2
        out <- outputFile opts $ return $ takeDirectory hdrE2 </> (base <> "_extract")
        hdr <- stackIO (B.readFile hdrE2) >>= PG.decryptE2 >>= PG.readHeader
        stackIO $ do
          tree <- crawlFolder $ takeDirectory hdrE2
          let connected = (if elem OptCrypt opts then PG.decryptPKContents else id)
                $ PG.connectPKFiles tree base $ PG.getFolder $ PG.fh_Contents hdr
          saveHandleFolder connected out
        return out
      (FileISO, iso) -> do
        out <- outputFile opts $ return $ iso <> "_extract"
        dir <- stackIO $ do
          magic <- IO.withBinaryFile iso IO.ReadMode $ \h -> BL.hGet h 1
          if magic == "G" -- maybe not foolproof
            then loadGCM $ fileReadable iso
            else loadXboxISO (fileReadable iso) >>= \case
              Just xiso -> return xiso
              Nothing   -> folderISO $ fileReadable iso
        stackIO $ saveHandleFolder (first TE.decodeLatin1 dir) out
        return out
      p -> fatal $ "Unexpected file type given to extractor: " <> show p
    }

  , Command
    { commandWord = "pack-powergig"
    , commandDesc = "Pack a folder into Data.hdr.e.2 and Data.pk0."
    , commandUsage = ""
    , commandRun = \args opts -> case args of
      -- make new hdr and single pk
      [dir] -> do
        src <- (if elem OptCrypt opts then PG.encryptPKContents else id)
          <$> stackIO (crawlFolder dir)
        (folder, pk) <- stackIO $ PG.makeNewPK 0 src
        hdrE2 <- PG.encryptE2 $ BL.toStrict $ PG.buildHeader $ PG.rebuildFullHeader folder
        let pathHdr = takeDirectory dir </> "Data.hdr.e.2"
            pathPk = takeDirectory dir </> "Data.pk0"
        stackIO $ BL.writeFile pathHdr hdrE2
        stackIO $ BL.writeFile pathPk pk
        return [pathHdr, pathPk]
      -- add a new pk to an existing hdr
      -- TODO this is untested!
      [pathHdr, dir] -> do
        hdrOrig <- stackIO (B.readFile pathHdr) >>= PG.decryptE2 >>= PG.readHeader
        let origFolder = PG.getFolder $ PG.fh_Contents hdrOrig
            newPKNum = fromIntegral $
              maximum (map PG.oe_PKNum $ toList origFolder >>= toList . PG.pkOffsetEntries) + 1
        src <- (if elem OptCrypt opts then PG.encryptPKContents else id)
          <$> stackIO (crawlFolder dir)
        (newFolder, newPK) <- stackIO $ PG.makeNewPK newPKNum src
        let combinedFolder = origFolder <> newFolder -- right arg prefered (new contents supercede old ones)
        hdrE2 <- PG.encryptE2 $ BL.toStrict $ PG.buildHeader $ PG.rebuildFullHeader combinedFolder
        let pathPk = takeDirectory pathHdr </> ("Data.pk" <> show newPKNum)
        stackIO $ BL.writeFile pathHdr hdrE2
        stackIO $ BL.writeFile pathPk newPK
        return [pathHdr, pathPk]
      _ -> fatal "Expected 1 arg (folder to pack)"
    }

  , Command
    { commandWord = "decrypt-powergig"
    , commandDesc = "Decrypt .e.2, producing crypt header and file"
    , commandUsage = ""
    , commandRun = \files _opts -> fmap concat $ forM files $ \f -> do
      let out = case T.stripSuffix ".e.2" $ T.pack f of
            Nothing -> f <> ".dec"
            Just f' -> T.unpack f'
          outCryptHeader = f <> ".cryptheader"
      enc <- stackIO $ B.readFile f
      cryptHeader <- PG.getE2Header enc
      dec <- PG.decryptE2 enc
      stackIO $ B.writeFile outCryptHeader cryptHeader
      stackIO $ BL.writeFile out dec
      when (".hdr" `T.isSuffixOf` T.pack out) $ do
        PG.readHeader dec >>= stackIO . writeFile (out <> ".txt") . show
      return [outCryptHeader, out]
    }

  , Command
    { commandWord = "encrypt-powergig"
    , commandDesc = "Encrypt to .e.2"
    , commandUsage = ""
    , commandRun = \files _opts -> forM files $ \f -> do
      let out = f <> ".e.2"
      stackIO (B.readFile f) >>= PG.encryptE2 >>= stackIO . BL.writeFile out
      return out
    }

  , Command
    { commandWord = "midi-text"
    , commandDesc = "Convert a MIDI file to/from a plaintext format."
    , commandUsage = ""
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileMidi -> do
        out <- outputFile opts $ return $ fpath -<.> "midtxt"
        res <- MS.toStandardMIDI <$> F.loadRawMIDI fpath
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
        res <- MS.toStandardMIDI <$> F.loadRawMIDI mid
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
        baseMid <- F.loadMIDI base
        tracksMid <- F.loadMIDI tracks
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        let newMid = F.mergeCharts (F.TrackPad 0) baseMid tracksMid
        stackIO $ Save.toFile out $ F.showMIDIFile' newMid
        return [out]
      [base, tracks, "pad", n] -> do
        baseMid <- F.loadMIDI base
        tracksMid <- F.loadMIDI tracks
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        n' <- case readMaybe n of
          Nothing -> fatal "Invalid merge pad amount"
          Just d  -> return $ realToFrac (d :: Double)
        let newMid = F.mergeCharts (F.TrackPad n') baseMid tracksMid
        stackIO $ Save.toFile out $ F.showMIDIFile' newMid
        return [out]
      [base, tracks, "drop", n] -> do
        baseMid <- F.loadMIDI base
        tracksMid <- F.loadMIDI tracks
        out <- outputFile opts $ return $ tracks ++ ".merged.mid"
        n' <- case readMaybe n of
          Nothing -> fatal "Invalid merge drop amount"
          Just d  -> return $ realToFrac (d :: Double)
        let newMid = F.mergeCharts (F.TrackDrop n') baseMid tracksMid
        stackIO $ Save.toFile out $ F.showMIDIFile' newMid
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
      mid <- F.loadMIDI pathMid
      let existingTracks = F.rawTracks $ F.s_tracks mid
          existingNames = mapMaybe U.trackName existingTracks
      case filter (`notElem` existingNames) args of
        []      -> return ()
        newTrks -> stackIO $ Save.toFile pathMid $ F.showMIDIFile' mid
          { F.s_tracks = F.RawFile $ existingTracks ++ map (`U.setTrackName` RTB.empty) newTrks
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
    , commandUsage = "onyx install-ark song GEN/ --target gh2"
    , commandRun = \args opts -> case args of
      [song, gen] -> withProject (optIndex opts) song $ \proj -> do
        targetName <- case [ t | OptTarget t <- opts ] of
          []    -> fatal "command requires --target, none given"
          t : _ -> return t
        target <- case HM.lookup targetName (projectSongYaml proj).targets of
          Nothing     -> fatal $ "Target not found in YAML file: " <> show targetName
          Just target -> return target
        gh2 <- case target of
          GH2 gh2 -> return gh2
          _       -> fatal $ "Target is not for GH2: " <> show targetName
        installGH2 gh2 proj gen
        return [gen]
      _ -> fatal "Expected 2 arguments (input song, GEN)"
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
        mid <- F.loadMIDI fmid
        let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
        voxToLip <- case game of
          GameRB3  -> (\vmap -> autoLipsync    defaultTransition vmap englishSyllables) <$> loadVisemesRB3
          GameRB2  -> (\vmap -> autoLipsync    defaultTransition vmap englishSyllables) <$> loadVisemesRB3
          GameTBRB -> (\vmap -> beatlesLipsync defaultTransition vmap englishSyllables) <$> loadVisemesTBRB
          _        -> fatal "Unsupported game for lipsync-gen"
        let template = dropExtension fmid
            tracks =
              [ (F.fixedPartVocals, "_solovox.lipsync", False)
              , (F.fixedHarm1, "_harm1.lipsync", False)
              , (F.fixedHarm2, "_harm2.lipsync", False)
              , (F.fixedHarm3, "_harm3.lipsync", False)
              , (const mempty, "_empty.lipsync", True)
              ]
        fmap catMaybes $ forM tracks $ \(getter, suffix, alwaysWrite) -> do
          let trk = getter $ F.s_tracks mid
              fout = template <> suffix
          if nullVox trk && not alwaysWrite
            then return Nothing
            else do
              let lip = voxToLip $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
              stackIO $ BL.writeFile fout $ runPut $ putLipsync lip
              return $ Just fout
      _ -> fatal "Expected 1 argument (input midi)"
    }

  , Command
    { commandWord = "lipsync-track-gen"
    , commandDesc = "Make lipsync files from LIPSYNC{1,2,3,4} in a MIDI file."
    , commandUsage = "onyx lipsync-track-gen in.mid"
    , commandRun = \args opts -> case args of
      [fmid] -> do
        mid <- F.loadMIDI fmid
        let game = fromMaybe GameRB3 $ listToMaybe [ g | OptGame g <- opts ]
        vmap <- case game of
          GameRB3  -> loadVisemesRB3
          GameRB2  -> loadVisemesRB3
          GameTBRB -> loadVisemesTBRB
          _        -> fatal "Unsupported game for lipsync-track-gen"
        let template = dropExtension fmid
            tracks =
              [ (F.onyxLipsync1, "_1.lipsync")
              , (F.onyxLipsync2, "_2.lipsync")
              , (F.onyxLipsync3, "_3.lipsync")
              , (F.onyxLipsync4, "_4.lipsync")
              ]
            voxToLip
              = (if game == GameTBRB then setBeatles else id)
              . lipsyncFromMIDITrack vmap
        fmap concat $ forM (Map.toList $ F.onyxParts $ F.s_tracks mid) $ \(fpart, opart) -> do
          fmap catMaybes $ forM tracks $ \(getter, suffix) -> do
            let trk = getter opart
                fout = template <> "_" <> T.unpack (F.getPartName fpart) <> suffix
            if trk == mempty
              then return Nothing
              else do
                let lip = voxToLip $ mapTrack (U.applyTempoTrack $ F.s_tempos mid) trk
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
        pref <- runGetM (codecIn bin) bs
        stackIO $ writeFile fout $ show (pref :: SongPref)
        return [fout]
      _ -> fatal "Expected 1 argument (BandSongPref file)"
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
        mid <- F.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: F.Song (F.RawFile U.Beats))
        return [fout]
      ["raw-len", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- F.loadMIDI fin
        stackIO $ writeFile fout $ show $ length $ show (mid :: F.Song (F.RawFile U.Beats))
        return [fout]
      ["fixed", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- F.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: F.Song (F.FixedFile U.Beats))
        return [fout]
      ["onyx", fin] -> do
        fout <- outputFile opts $ return $ fin <> ".txt"
        mid <- F.loadMIDI fin
        stackIO $ writeFile fout $ show (mid :: F.Song (F.OnyxFile U.Beats))
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
        mid <- F.loadMIDI fin
        case players of
          [] -> stackIO $ forM_ [minBound .. maxBound] $ \scoreTrack -> do
            forM_ [minBound .. maxBound] $ \diff -> do
              let stars = starCutoffs (F.s_tracks mid) [(scoreTrack, diff)]
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
                  (base, solo) = baseAndSolo (F.s_tracks mid) pair
                  in putStrLn $ show pair <> ": base " <> show base <> ", solo bonuses " <> show solo
                print $ starCutoffs (F.s_tracks mid) pairs
        return []
      _ -> fatal "Expected 1 argument (input midi)"
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

  , Command
    { commandWord = "ark-contents"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <> ".contents.txt"
        stackIO $ BL.readFile fin >>= Ark.readHdr >>= writeFile fout . unlines . map show . (.files)
        return [fout]
      _ -> fatal "Expected 1 arg (.hdr, or .ark if none)"
    }

  , Command
    { commandWord = "ark-extract"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args opts -> case args of
      [hdrPath] -> do
        dout <- outputFile opts $ return $ hdrPath <> "_extract"
        (hdr, arks) <- stackIO $ crawlFolder (takeDirectory hdrPath) >>= Ark.loadGEN
        Ark.extractArk hdr arks dout
        return [dout]
      _ -> fatal "Expected 1 arg (.hdr, or .ark if none)"
    }

  , Command
    { commandWord = "amp-pack"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args opts -> case args of
      [dir] -> do
        ark <- outputFile opts $ return $ dir <.> "ark"
        stackIO $ AmpArk.createArk dir ark
        return [ark]
      _ -> fatal "Expected 1 arg (folder to make into .ark)"
    }

  , Command
    { commandWord = "gh-pack"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args _opts -> case args of
      [dir, hdr] -> do
        let ark = dropExtension hdr <> "_0.ARK"
        stackIO $ GHArk.createHdrArk dir hdr ark
        return [hdr, ark]
      _ -> fatal "Expected 2 args (folder to pack, .hdr)"
    }

  , Command
    { commandWord = "decode-image"
    , commandDesc = "Converts from HMX image format to PNG."
    , commandUsage = "onyx decode-image input.ext_platform {--to output.png}"
    , commandRun = \args opts -> case args of
      [fin] -> do
        let isPS3 = takeExtension fin == ".png_ps3"
        bs <- stackIO $ fmap BL.fromStrict $ B.readFile fin
        case readRBImageMaybe isPS3 bs of
          Just img -> do
            fout <- outputFile opts $ return $ fin <> ".png"
            stackIO $ writePng fout img
            return [fout]
          Nothing -> fatal "Couldn't decode image file"
      _ -> fatal "Expected 1 arg (HMX image file)"
    }

  , Command
    { commandWord = "coop-max-scores"
    , commandDesc = ""
    , commandUsage = "onyx coop-max-scores notes.mid"
    , commandRun = \args _opts -> case args of
      [fin] -> do
        mid <- F.loadMIDI fin
        let p1 = if nullPart $ gh2PartGuitarCoop $ F.s_tracks mid
              then gh2PartGuitar     $ F.s_tracks mid
              else gh2PartGuitarCoop $ F.s_tracks mid
            p2 = if nullPart $ gh2PartBass $ F.s_tracks mid
              then gh2PartRhythm $ F.s_tracks mid
              else gh2PartBass   $ F.s_tracks mid
            scores1 = map (\diff -> gh2Base diff p1) [Easy .. Expert]
            scores2 = map (\diff -> gh2Base diff p2) [Easy .. Expert]
            scoresCoop = map (\diff -> gh2Base diff p1 + gh2Base diff p2) [Easy .. Expert]
            dta scores = "(" <> unwords (map show scores) <> ")"
        lg $ "p1: " <> dta scores1
        lg $ "p2: " <> dta scores2
        lg $ "coop: " <> dta scoresCoop
        return []
      _ -> fatal "Expected 1 arg (midi)"
    }

  , Command
    { commandWord = "import-audio"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args opts -> case args of
      audios@(_ : _) -> do
        pairs <- forM audios $ \audio -> inside ("audio file: " <> audio) $ do
          md5 <- audioMD5 audio >>= maybe (fatal "Couldn't compute audio hash") return
          len <- audioLength audio >>= maybe (fatal "Couldn't compute audio length") return
          return (md5, len)
        dir <- outputFile opts $ return "."
        stackIO $ Dir.createDirectoryIfMissing False dir
        stackIO $ yamlEncodeFile (dir </> "song.yml") $ toJSON (SongYaml
          { metadata = def
          , global   = def
          , audio    = HM.fromList $ flip map (zip [1..] pairs) $ \(i, (md5, len)) -> let
            ainfo = AudioFile AudioInfo
              { md5      = Just $ T.pack md5
              , frames   = Just len
              , filePath = Nothing
              , commands = []
              , rate     = Nothing
              , channels = 2 -- TODO real channel count from audio
              }
            in (T.pack $ "audio-" <> show (i :: Int), ainfo)
          , jammit   = HM.empty
          , plans    = HM.singleton "plan" $ StandardPlan StandardPlanInfo
            { song        = Just PlanAudio
              { expr = Input $ Named "audio-1"
              , pans = []
              , vols = []
              }
            , parts       = Parts HM.empty
            , crowd       = Nothing
            , comments    = []
            , tuningCents = 0
            , fileTempo   = Nothing
            }
          , targets  = HM.empty
          , parts    = Parts HM.empty
          } :: SongYaml FilePath)
        return [dir]
      _ -> fatal "Expected at least 1 arg (flac or wav files)"
    }

  , Command
    { commandWord = "split-fsb"
    , commandDesc = "Split an .fsb into streams, and optionally first decrypt an .fsb.* from a Neversoft GH game."
    , commandUsage = T.unlines
      [ "onyx split-fsb in.fsb"
      , "onyx split-fsb in.fsb.xen"
      , "onyx split-fsb in.fsb.xen --to out.fsb"
      ]
    , commandRun = \args opts -> fmap concat $ forM args $ \f -> case map toLower $ takeExtension f of
      ".fsb" -> do
        -- not encrypted
        let contentsDir = f <> "_contents"
        stackIO $ Dir.createDirectoryIfMissing False contentsDir
        stackIO $ splitFSBStreamsToDir f contentsDir
        return [contentsDir]
      _ -> do
        -- assume GH encrypted
        dec <- stackIO (decryptFSB f) >>= maybe (fatal "Couldn't decrypt GH .fsb.(xen/ps3) audio") return
        out <- outputFile opts $ return $ case stripSuffix ".fsb.xen" f <|> stripSuffix ".fsb.ps3" f <|> stripSuffix ".FSB.PS3" f of
          Just root -> root <.> "fsb"
          Nothing   -> f <.> "fsb"
        stackIO $ BL.writeFile out dec
        let contentsDir = out <> "_contents"
        stackIO $ Dir.createDirectoryIfMissing False contentsDir
        stackIO $ splitFSBStreamsToDir out contentsDir
        return [out, contentsDir]
    }

  , Command
    { commandWord = "encrypt-wor"
    , commandDesc = "Produce an encrypted .fsb.xen for GH:WoR."
    , commandUsage = "onyx encrypt-wor audio.fsb [--to audio.fsb.xen]"
    , commandRun = \args opts -> case args of
      [fsb] -> do
        fsb' <- stackIO $ B.readFile fsb
        case ghworEncrypt fsb' of
          Nothing -> fatal "Couldn't encrypt into .fsb.xen"
          Just xen -> do
            out <- outputFile opts $ return $ fsb <.> "xen"
            stackIO $ B.writeFile out xen
            return [out]
      _ -> fatal "Expected 2 arguments (original.fsb.xen and new.fsb)"
    }

  , Command
    { commandWord = "encrypt-gh3"
    , commandDesc = "Produce an encrypted .fsb.xen for GH3."
    , commandUsage = "onyx encrypt-gh3 audio.fsb [--to audio.fsb.xen]"
    , commandRun = \args opts -> case args of
      [fsb] -> do
        fsb' <- stackIO $ BL.readFile fsb
        out <- outputFile opts $ return $ fsb <.> "xen"
        stackIO $ BL.writeFile out $ gh3Encrypt fsb'
        return [out]
      _ -> fatal "Expected 2 arguments (original.fsb.xen and new.fsb)"
    }

  , Command
    { commandWord = "make-fsb"
    , commandDesc = ""
    , commandUsage = T.unlines
      [ "onyx make-fsb fmod in.wav --to out.fsb"
      , "onyx make-fsb xdk  in.wav --to out.fsb"
      ]
    , commandRun = \args opts -> case args of
      ["fmod", fin] -> do
        fout <- outputFile opts $ return $ fin <.> "fsb"
        makeFSB4 fin fout
        let xmaDir = fout <> "_xma"
        stackIO $ Dir.createDirectoryIfMissing False xmaDir
        stackIO $ splitFSBStreamsToDir fout xmaDir
        return [fout, xmaDir]
      ["xdk", fin] -> do
        fout <- outputFile opts $ return $ fin <.> "fsb"
        makeFSB4' fin fout
        return [fout]
      "gh3" : streams -> do
        fout <- outputFile opts $ fatal "Need --to"
        makeXMAFSB3 (zipWith
          (\i stream -> (B8.pack $ show (i :: Int) <> ".xma", stream))
          [0..] streams) fout
        return [fout]
      _ -> fatal "Invalid format"
    }

  , Command
    { commandWord = "extract-pak"
    , commandDesc = ""
    , commandUsage = "onyx extract-pak in.pak.xen [--to out-folder]"
    , commandRun = \args opts -> case args of
      [pak] -> inside ("extracting pak " <> pak) $ do
        dout <- outputFile opts $ return $ pak <> "_extract"
        stackIO $ Dir.createDirectoryIfMissing False dout
        (pabData, endian) <- do
          let (noPlatform, platform) = splitExtension pak
          pabData <- case splitExtension noPlatform of
            (noPak, ext) | map toLower ext == ".pak" -> do
              pabPath <- fixFileCase $ noPak <> ".pab" <> platform
              stackIO (Dir.doesFileExist pabPath) >>= \case
                False -> return Nothing
                True  -> Just <$> stackIO (BL.readFile pabPath)
            _ -> return Nothing
          let endian = case platform of
                ".ps2" -> LittleEndian
                _      -> BigEndian
          return (pabData, endian)
        nodes <- stackIO (BL.readFile pak) >>= \bs -> splitPakNodes endian bs pabData
        stackIO $ writeFile (dout </> "pak-contents.txt") $ unlines $ map (show . fst) nodes
        let knownExts =
              [ ".cam", ".clt", ".col", ".dbg", ".empty", ".fam", ".fnc", ".fnt", ".fnv"
              , ".gap", ".hkc", ".img", ".imv", ".jam", ".last", ".mcol", ".mdl", ".mdv"
              , ".mqb", ".nav", ".note", ".nqb", ".oba", ".perf", ".pfx", ".pimg", ".pimv"
              , ".qb", ".qd", ".qs", ".qs.br", ".qs.de", ".qs.en", ".qs.es", ".qs.fr", ".qs.it"
              , ".rag", ".raw", ".rgn", ".rnb", ".scn", ".scv", ".shd", ".ska", ".ske", ".skin"
              , ".skiv", ".snp", ".sqb", ".stat", ".stex", ".table", ".tex", ".trkobj", ".tvx", ".wav", ".xml"
              ]
            digits = length $ show $ length nodes - 1
        forM_ (zip [0..] nodes) $ \(i, (node, contents)) -> do
          let ext = fromMaybe ("." <> show (nodeFileType node))
                $ find ((== nodeFileType node) . qbKeyCRC . B8.pack) knownExts
              name = reverse (take digits $ reverse (show (i :: Int)) <> repeat '0') <> ext
          stackIO $ BL.writeFile (dout </> name) contents
          when (nodeFileType node == qbKeyCRC ".note") $ do
            -- TODO handle failure
            note <- loadNoteFile contents
            stackIO $ writeFile (dout </> name <.> "parsed.txt") $ show note
          when (nodeFileType node == qbKeyCRC ".qb") $ do
            let matchingQS = flip filter nodes $ \(otherNode, _) ->
                  nodeFilenameCRC node == nodeFilenameCRC otherNode
                    && nodeFileType otherNode == qbKeyCRC ".qs.en"
                mappingQS = qsBank matchingQS
            inside ("Parsing QB file: " <> show name) $ do
              let ?endian = endian
              mqb <- errorToWarning $ runGetM parseQB contents
              forM_ mqb $ \qb -> do
                let filled = map (lookupQB knownKeys . lookupQS mappingQS) qb
                stackIO $ Y.encodeFile (dout </> name <.> "parsed.yaml") filled
        return [dout]
      _ -> fatal "Expected 1 argument (_song.pak.xen)"
    }

  , Command
    { commandWord = "rebuild-pak"
    , commandDesc = ""
    , commandUsage = "onyx rebuild-pak in-folder [--to out.pak.xen]"
    , commandRun = \args opts -> case args of
      [dir] -> do
        fout <- outputFile opts $ return $ dir <.> "pak"
        contents <- stackIO $ readFile $ dir </> "pak-contents.txt"
        let nodes = map read $ lines contents
        files <- stackIO $ Dir.listDirectory dir
        let fileMap = Map.fromList $ do
              f <- files
              n <- toList $ readMaybe $ takeWhile isDigit f
              guard $ not $ ".parsed." `T.isInfixOf` T.pack f
              return (n, f)
        newContents <- forM (zip [0..] nodes) $ \(i, node) -> case Map.lookup (i :: Int) fileMap of
          Just f -> do
            bs <- stackIO $ BL.fromStrict <$> B.readFile (dir </> f)
            return (node, bs)
          Nothing -> fatal $ "Couldn't find pak content #" <> show i
        stackIO $ BL.writeFile fout $ buildPak newContents
        return [fout]
      _ -> fatal "Expected 1 argument (extracted .pak folder)"
    }

  , Command
    { commandWord = "make-qb"
    , commandDesc = "Compile a .yaml file back into a .qb"
    , commandUsage = "onyx make-qb in.qb.parsed.yaml [--to out.qb]"
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <.> "qb"
        qb <- stackIO (Y.decodeFileEither fin) >>= either (\e -> fatal $ show e) return
        stackIO $ BL.writeFile fout $ putQB $ discardStrings qb
        return [fout]
      _ -> fatal "Expected 1 argument (.yaml)"
    }

  , Command
    { commandWord = "share-wor"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args _opts -> do
      shareMetadata args
      return args
    }

  , Command
    { commandWord = "make-wor-database"
    , commandDesc = ""
    , commandUsage = ""
    , commandRun = \args opts -> do
      fout <- outputFile opts $ fatal "make-wor-database requires --to argument"
      makeMetadataLIVE args fout
      return [fout]
    }

  , Command
    { commandWord = "rb1-moggcrypt"
    , commandDesc = ""
    , commandUsage = "onyx rb1-moggcrypt file-in --to file-out"
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ fatal "Requires --to argument"
        fin'  <- shortWindowsPath False fin
        fout' <- shortWindowsPath True  fout
        stackIO $ encryptRB1 fin' fout'
        return [fout]
      _ -> fatal "Expected 1 argument"
    }

  , Command
    { commandWord = "test-rb3-venue"
    , commandDesc = ""
    , commandUsage = T.unlines
      [ "onyx test-rb3-venue song.anim --to out.txt"
      , "onyx test-rb3-venue song.anim in.mid --to out.mid"
      ]
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <.> "txt"
        venue <- stackIO (BL.readFile fin) >>= runGetM parseVenue
        stackIO $ writeFile fout $ show venue
        return [fout]
      [fin, mid] -> do
        fout <- outputFile opts $ return $ mid <> ".venue.mid"
        stackIO $ testConvertVenue mid fin fout
        return [fout]
      _ -> fatal "Expected 1 or 2 arguments"
    }

  , Command
    { commandWord = "pkg"
    , commandDesc = "Compile a folder's contents into a PS3 .pkg file."
    , commandUsage = T.unlines
      [ "onyx pkg CONTENT_ID my_folder --to new.pkg"
      ]
    , commandRun = \args opts -> case args of
      [contentID, dir] -> stackIO (Dir.doesDirectoryExist dir) >>= \case
        True -> do
          pkg <- outputFile opts
            $ (<.> "pkg") . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          folder <- stackIO $ first TE.encodeUtf8 <$> crawlFolder dir
          stackIO $ makePKG (B8.pack contentID) folder pkg
          return [pkg]
        False -> fatal $ "onyx pkg expected directory; given: " <> dir
      _ -> fatal $ "onyx pkg expected 2 argument, given " <> show (length args)
    }

  , Command
    { commandWord = "extract-wad"
    , commandDesc = "Tries to extract U8 files from a Wii .wad file."
    , commandUsage = T.unlines
      [ "onyx extract-wad in.wad [--to out_folder]"
      ]
    , commandRun = \args opts -> case args of
      [fin] -> do
        dout <- outputFile opts $ return $ fin <> "_extract"
        if takeExtension fin == ".app"
          then do
            (u8, _) <- stackIO (B.readFile fin) >>= readU8 . BL.fromStrict
            stackIO $ flip saveHandleFolder dout
              $ bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle) u8
          else do
            stackIO $ Dir.createDirectoryIfMissing False dout
            wad <- stackIO (BL.readFile fin) >>= runGetM getWAD
            (initialData, u8s) <- hackSplitU8s wad
            stackIO $ B.writeFile (dout </> "initial-data.bin") initialData
            forM_ (zip [0..] u8s) $ \(i, u8) -> do
              let u8' = bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle) u8
              stackIO $ saveHandleFolder u8' $ dout </> ("u8-" <> show (i :: Int))
        return [dout]
      _ -> fatal $ "onyx extract-wad expected 1 argument, given " <> show (length args)
    }

  , Command
    { commandWord = "txt-bin-dta"
    , commandDesc = "Converts Amplitude (PS2) .txt.bin to .dta"
    , commandUsage = T.unlines
      [ "onyx txt-bin-yml in.txt.bin [--to out.dta]"
      ]
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <> ".dta"
        bs <- stackIO $ B.readFile fin
        txtBin <- runGetM getTxtBin $ BL.fromStrict bs
        stackIO $ T.writeFile fout $ showDTA $ fmap TE.decodeLatin1 $ txtBinToDTA txtBin
        return [fout]
      _ -> fatal "Expected 1 argument"
    }

  , Command
    { commandWord = "to-xma2"
    , commandDesc = "Converts a (single-stream) XMA1 file to XMA2."
    , commandUsage = T.unlines
      [ "onyx to-xma2 in.xma [--to out.xma]"
      ]
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin -<.> "xma2.xma"
        xma <- stackIO (B.readFile fin) >>= parseXMA . BL.fromStrict
        xma2 <- case xma of
          Left  xma1 -> xma1To2 xma1
          Right xma2 -> do
            lg "File is already XMA2."
            return xma2
        stackIO $ writeXMA2 fout xma2
        return [fout]
      _ -> fatal "Expected 1 argument"
    }

  ]

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
  , Option []   ["crypt"          ] (NoArg  OptCrypt                          ) ""
  ] where
    readGame = \case
      "rb3"   -> GameRB3
      "rb2"   -> GameRB2
      "tbrb"  -> GameTBRB
      "gh2"   -> GameGH2
      "ghwor" -> GameGHWOR
      g       -> error $ "Unrecognized --game value: " ++ show g

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
  | OptCrypt
  deriving (Eq, Ord, Show)

applyMidiFunction
  :: (MonadIO m, SendMessage m)
  => Maybe (F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats)) -- ^ MIDI transform
  -> FilePath
  -> FilePath
  -> StackTraceT m ()
applyMidiFunction Nothing fin fout = stackIO $ Dir.copyFile fin fout
applyMidiFunction (Just fn) fin fout = do
  mid <- F.loadMIDI fin
  stackIO $ Save.toFile fout $ F.showMIDIFile' $ fn mid

getDolphinFunction
  :: [OnyxOption]
  -> Maybe (F.Song (F.FixedFile U.Beats) -> F.Song (F.FixedFile U.Beats))
getDolphinFunction opts = let
  nofills   = elem OptWiiNoFills   opts
  mustang22 = elem OptWiiMustang22 opts
  unmute22  = elem OptWiiUnmute22  opts
  in do
    guard $ or [nofills, mustang22, unmute22]
    Just
      $ (if nofills   then F.wiiNoFills   else id)
      . (if mustang22 then F.wiiMustang22 else id)
      . (if unmute22  then F.wiiUnmute22  else id)

data Game
  = GameRB3
  | GameRB2
  | GameTBRB
  | GameGH2
  | GameGHWOR
  deriving (Eq, Ord, Show)

midiOptions :: [OnyxOption] -> MS.Options
midiOptions opts = MS.Options
  { MS.showFormat = if
    | elem OptInBeats opts    -> MS.ShowBeats
    | elem OptInSeconds opts  -> MS.ShowSeconds
    | elem OptInMeasures opts -> MS.ShowMeasures
    | otherwise               -> MS.ShowBeats
  , MS.resolution = Just $ fromMaybe 480 $ listToMaybe [ r | OptResolution r <- opts ]
  , MS.separateLines = elem OptSeparateLines opts
  , MS.matchNoteOff = elem OptMatchNotes opts
  }
