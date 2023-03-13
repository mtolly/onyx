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
import           Control.Monad.Extra                  (concatMapM, filterM,
                                                       forM, forM_, guard, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource         (MonadResource,
                                                       runResourceT)
import           Data.Bifunctor                       (bimap, first)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.ByteString.Lazy.Char8           ()
import           Data.Char                            (isAlphaNum, isAscii,
                                                       isDigit, isUpper,
                                                       toLower)
import qualified Data.Conduit.Audio                   as CA
import           Data.Conduit.Audio.LAME              (sinkMP3WithHandle)
import qualified Data.Conduit.Audio.LAME.Binding      as L
import           Data.Conduit.Audio.SampleRate
import           Data.Default.Class                   (def)
import qualified Data.Digest.Pure.MD5                 as MD5
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra                      (find, isPrefixOf,
                                                       stripPrefix, stripSuffix,
                                                       unsnoc)
import           Data.List.Split                      (chunksOf)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       listToMaybe, mapMaybe)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as TE
import qualified Data.Yaml                            as Y
import           Onyx.Amplitude.PS2.TxtBin            (TxtBin (..), dtaToTxtBin,
                                                       getTxtBin, putTxtBin,
                                                       txtBinToDTA,
                                                       txtBinToTracedDTA)
import           Onyx.Audio                           (Audio (Input),
                                                       audioLength, audioMD5,
                                                       buildSource', makeFSB4,
                                                       makeFSB4', makeXMAFSB3,
                                                       runAudio)
import           Onyx.Audio.FSB                       (emitFSB,
                                                       ghBandMP3sToFSB4,
                                                       parseXMA,
                                                       splitFSBStreamsToDir,
                                                       writeXMA2, xma1To2)
import           Onyx.Audio.VGS                       (readVGS)
import           Onyx.Build                           (shakeBuildFiles)
import           Onyx.Build.Neversoft                 (makeMetadataLIVE,
                                                       packageNameHashFormat,
                                                       worFilePS3EmptyVRAMPak,
                                                       worFilePS3SongVRAMPak)
import           Onyx.Codec.Binary
import           Onyx.Codec.JSON                      (loadYaml, toJSON,
                                                       yamlEncodeFile)
import           Onyx.GuitarHero.IOS                  (loadIGA)
import qualified Onyx.Harmonix.Ark                    as Ark
import           Onyx.Harmonix.Ark.Amplitude          as AmpArk
import qualified Onyx.Harmonix.Ark.GH2                as GHArk
import           Onyx.Harmonix.DTA                    (readDTABytes)
import           Onyx.Harmonix.DTA.C3                 (readRB3DTA)
import           Onyx.Harmonix.DTA.Parse              (parseStack)
import           Onyx.Harmonix.DTA.Print              (showDTA)
import           Onyx.Harmonix.DTA.Scan               (scanStack)
import qualified Onyx.Harmonix.DTA.Serialize          as D
import qualified Onyx.Harmonix.DTA.Serialize.Magma    as RBProj
import qualified Onyx.Harmonix.DTA.Serialize.RockBand as D
import           Onyx.Harmonix.GH2.File               (GH2File (..))
import           Onyx.Harmonix.GH2.PartGuitar         (nullPart)
import           Onyx.Harmonix.Magma                  (getRBAFile, runMagmaMIDI)
import           Onyx.Harmonix.MOGG                   (encryptRB1, moggToOgg,
                                                       oggToMogg)
import qualified Onyx.Harmonix.RockBand.IOS           as IOS
import           Onyx.Harmonix.RockBand.Milo          (autoLipsync,
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
                                                       ghworEncrypt,
                                                       ghwtEncrypt)
import           Onyx.Neversoft.Note                  (loadNoteFile)
import           Onyx.Neversoft.Pak                   (Node (..), buildPak,
                                                       nodeFileType, qsBank,
                                                       splitPakNodes)
import           Onyx.Neversoft.QB                    (discardStrings, lookupQB,
                                                       lookupQS, parseQB, putQB)
import           Onyx.Nintendo.GCM                    (loadGCM)
import           Onyx.Nintendo.U8                     (packU8, readU8)
import           Onyx.Nintendo.WAD                    (getWAD, hackSplitU8s)
import           Onyx.PlayStation.NPData
import           Onyx.PlayStation.PKG                 (PKG (..), loadPKG,
                                                       makePKG, tryDecryptEDATs)
import qualified Onyx.PowerGig.Crypt                  as PG
import           Onyx.Project
import           Onyx.Reaper.Build                    (TuningInfo (..),
                                                       makeReaper)
import           Onyx.Resources                       (getResourcesPath)
import           Onyx.Rocksmith.PSARC                 (extractPSARC)
import           Onyx.StackTrace
import           Onyx.Util.Files                      (copyDirRecursive,
                                                       fixFileCase,
                                                       shortWindowsPath)
import           Onyx.Util.Handle                     (Folder (..),
                                                       byteStringSimpleHandle,
                                                       crawlFolder,
                                                       fileReadable, makeHandle,
                                                       saveHandleFolder,
                                                       saveReadable)
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

_installSTFS :: (MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
_installSTFS stfs usb = do
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

_findXbox360USB :: (MonadIO m) => StackTraceT m [FilePath]
_findXbox360USB = stackIO $ do
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
  , commandList  :: Bool
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
    ".blob" -> return $ FileType FileBlob fp
    ".iga" -> return $ FileType FileIGA fp
    ".hdr" -> return $ FileType FileHdrOrArk fp
    ".ark" -> return $ FileType FileHdrOrArk fp
    ".pak" -> return $ FileType FilePak fp
    ".fsb" -> return $ FileType FileFSB fp
    ".wad" -> return $ FileType FileWAD fp
    ".app" -> return $ FileType FileU8 fp -- note, we check both extension and "U.8-" magic since official rb u8 don't actually start with that
    ".2" | ".hdr.e.2" `T.isSuffixOf` T.toLower (T.pack $ takeFileName fp) -> return $ FileType FileHdrE2 fp
    ext | any (`isPrefixOf` ext) [".png_", ".bmp_"] -> return $ FileType FileHarmonixImage fp -- assume this is .png_xbox, .bmp_ps2, etc.
    ".bmp" -> return $ FileType FileHarmonixImage fp -- assume this is actually bmp from gzip from amplitude ark
    _ -> case map toLower $ takeExtension $ dropExtension fp of
      ".pak" -> return $ FileType FilePak fp -- handle the usual .pak.xen, .pak.ps3, etc.
      ".fsb" -> return $ FileType FileFSBEncrypted fp -- assume .fsb.xen, .fsb.ps3, etc. are game-encrypted
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
            "VgS!" -> FileType FileVGS fp
            "FSB3" -> FileType FileFSB fp
            "FSB4" -> FileType FileFSB fp
            _      -> case BL.unpack magic of
              [0xAF, 0xDE, 0xBE, 0xCA] -> FileType FileMilo fp
              [0xAF, 0xDE, 0xBE, 0xCB] -> FileType FileMilo fp
              [0xAF, 0xDE, 0xBE, 0xCC] -> FileType FileMilo fp
              [0xAF, 0xDE, 0xBE, 0xCD] -> FileType FileMilo fp
              [0x7F, 0x50, 0x4B, 0x47] -> FileType FilePKG fp
              [0x55, 0xAA, 0x38, 0x2D] -> FileType FileU8 fp
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
  | FileFSB
  | FileFSBEncrypted
  | FileU8
  | FileVGS
  | FileBlob
  | FileIGA
  | FileHdrOrArk
  | FilePak
  | FileWAD
  | FileHarmonixImage
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
    { commandWord = "import"
    , commandDesc = "Import a file into Onyx's project format. The project format is not documented yet and is subject to change."
    , commandUsage = ""
    , commandList = True
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
    { commandWord = "build"
    , commandDesc = "Compile an Onyx project for a given target. This is not documented yet, but after importing a song to a project, you will need to edit song.yml in order to add or edit a target."
    , commandUsage = T.unlines
      [ "onyx build --target rb3 --to new_rb3con"
      , "onyx build --target ps --to new_ps.zip"
      , "onyx build song.yml --target rb3 --to new_rb3con"
      ]
    , commandList = True
    , commandRun = \files opts -> optionalFile files >>= \case
      (FileSongYaml, yamlPath) -> do
        out <- outputFile opts $ fatal "onyx build (yaml) requires --to, none given"
        (_, built) <- buildTarget yamlPath opts
        stackIO $ Dir.copyFile built out
        return [out]
      (ftype, fpath) -> unrecognized ftype fpath
    }

  -- TODO "convert" command

  , Command
    { commandWord = "debug"
    , commandDesc = "Internal command to build intermediate files in an Onyx project."
    , commandUsage = "onyx debug mysong.yml file1 [file2 ...]"
    , commandList = False
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
    { commandWord = "web-player"
    , commandDesc = "Create a web browser chart playback app."
    , commandUsage = "onyx web-player (song) [--to folder]"
    , commandList = True
    , commandRun = \files opts -> do
      fpath <- case files of
        []  -> return "."
        [x] -> return x
        _   -> fatal "Expected 0 or 1 files/folders"
      proj <- openProject (optIndex opts) fpath
      player <- buildPlayer (getMaybePlan opts) proj
      case projectRelease proj of
        Nothing -> return [player </> "index.html"] -- onyx project, return folder directly
        Just _ -> do
          out <- outputFile opts $ return $ projectTemplate proj ++ "_player"
          stackIO $ Dir.createDirectoryIfMissing False out
          copyDirRecursive player out
          return [out </> "index.html"]
    }

  , Command
    { commandWord = "reaper"
    , commandDesc = "Generate a REAPER project for a song."
    , commandUsage = "onyx reaper (song) [--to folder]"
    , commandList = True
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
    { commandWord = "pro-keys-hanging"
    , commandDesc = "Find pro keys range shifts with hanging notes."
    , commandUsage = T.unlines
      [ "onyx hanging mysong.yml"
      , "onyx hanging notes.mid"
      ]
    , commandList = True
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
      [ "onyx stfs (folder) [--to (output)] [--game (rb3/rb2/gh2)]"
      , "# Other data can be overridden by making an `onyx-repack` folder."
      , "# Extract an existing STFS file to see an example of the format."
      ]
    , commandList = True
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
    { commandWord = "mogg"
    , commandDesc = "Turn an Ogg Vorbis file into an unencrypted MOGG file."
    , commandUsage = T.unlines
      [ "onyx mogg in.ogg [--to out.mogg]"
      ]
    , commandList = True
    , commandRun = \files opts -> optionalFile files >>= \(ftype, fpath) -> case ftype of
      FileOGG -> do
        mogg <- outputFile opts $ return $ fpath -<.> "mogg"
        oggToMogg fpath mogg
        return [mogg]
      _ -> unrecognized ftype fpath
    }

  , Command
    { commandWord = "encrypt-mogg-rb1"
    , commandDesc = "Encrypt a MOGG file as 0x0B-type (Rock Band 1)."
    , commandUsage = "onyx encrypt-mogg-rb1 in.mogg --to out.mogg"
    , commandList = True
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
    { commandWord = "u8"
    , commandDesc = "Package a folder into a U8 file for Wii."
    , commandUsage = "onyx u8 dir --to out.app"
    , commandList = True
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
    { commandWord = "milo"
    , commandDesc = "Recombine an extracted .milo_xxx file. This requires the format output by running `onyx extract` on an existing milo file."
    , commandUsage = "onyx milo dir [--to out.milo_xxx]"
    , commandList = True
    , commandRun = \args opts -> case args of
      [din] -> do
        fout <- outputFile opts $ return $ dropTrailingPathSeparator din <.> "milo_xbox"
        packMilo din fout
        return [fout]
      _ -> fatal "Expected 1 argument (input dir)"
    }

  , Command
    { commandWord = "encrypt-gh-fsb"
    , commandDesc = "Apply Neversoft GH encryption to a .fsb file."
    , commandUsage = T.unlines
      [ "onyx encrypt-gh-fsb gh3   in.fsb [--to out.fsb.xen]"
      , "onyx encrypt-gh-fsb ghwt  in.fsb [--to out.fsb.xen]"
      , "onyx encrypt-gh-fsb ghwor in.fsb [--to out.fsb.xen]"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [game, fsb] -> do
        out <- outputFile opts $ return $ fsb <.> "xen"
        case game of
          "gh3"   -> do
            dec <- stackIO $ BL.readFile fsb
            stackIO $ BL.writeFile out $ gh3Encrypt dec
          "ghwt"  -> do
            dec <- stackIO $ BL.readFile fsb
            stackIO $ BL.writeFile out $ ghwtEncrypt fsb dec
          "ghwor" -> do
            dec <- stackIO $ B.readFile fsb
            enc <- maybe (fatal "Couldn't apply WoR encryption") return $ ghworEncrypt dec
            stackIO $ B.writeFile out enc
          _       -> fatal $ "Unrecognized game type: " <> show game
        return [out]
      _ -> fatal "Expected 2 arguments (gh game, input fsb)"
    }

  -- TODO clean this up, maybe only output MP3 and just have the two types (gh3 and ghwt+)
  , Command
    { commandWord = "fsb"
    , commandDesc = "Encode a WAV file to an XMA-based FSB file. This command will be modified in the future to primarily support MP3-based FSB files."
    , commandUsage = T.unlines
      [ "onyx fsb fmod    in.wav --to out.fsb"
      , "onyx fsb xdk     in.wav --to out.fsb"
      , "onyx fsb gh4-mp3 in1.wav [in2.wav ...] --to out.fsb # each input should be stereo"
      ]
    , commandList = True
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
      "gh4-mp3" : streams -> do
        fout <- outputFile opts $ fatal "Need --to"
        tempDir "onyx-fsb" $ \tmp -> do
          let tmpWav = tmp </> "temp.wav"
          mp3s <- forM streams $ \fin -> case map toLower $ takeExtension tmp of
            ".mp3" -> stackIO $ BL.fromStrict <$> B.readFile fin
            ".wav" -> do
              let setup lame = liftIO $ do
                    L.check $ L.setBrate lame 128
                    L.check $ L.setQuality lame 5
                    L.check $ L.setOutSamplerate lame 48000
              src <- buildSource' $ Input fin
              let resampled = resampleTo 48000 SincMediumQuality src
              stackIO $ runResourceT $ sinkMP3WithHandle tmpWav setup resampled
              stackIO $ BL.fromStrict <$> B.readFile tmpWav
            _ -> fatal $ "Expected mp3 or wav file extension for audio: " <> show fin
          stackIO $ ghBandMP3sToFSB4 mp3s >>= BL.writeFile fout . emitFSB
        return [fout]
      _ -> fatal "Invalid format"
    }

  , Command
    { commandWord = "pak"
    , commandDesc = "Compile an extracted Neversoft GH .pak file back into a folder. This requires the format output by running `onyx extract` on an existing .pak file."
    , commandUsage = "onyx pak in-folder [--to out.pak.xen]"
    , commandList = True
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
    { commandWord = "pkg"
    , commandDesc = "Compile a folder's contents into a PS3 .pkg file."
    , commandUsage = T.unlines
      [ "onyx pkg (content-id) my_folder --to new.pkg"
      , "* content-id should be something like UP0006-BLUS30050_00-RBSTILLALCCF005D"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [argContentID, dir] -> stackIO (Dir.doesDirectoryExist dir) >>= \case
        True -> do
          let contentID = B8.pack argContentID
          pkg <- outputFile opts
            $ (<.> "pkg") . dropTrailingPathSeparator
            <$> stackIO (Dir.makeAbsolute dir)
          folderOrig <- stackIO $ crawlFolder dir
          folderEDATs <- tempDir "onyx-pkg" $ \tmp -> let
            tmpIn  = tmp </> "input"
            tmpOut = tmp </> "output.edat"
            modifyFolder folder = do
              newFiles <- forM (folderFiles folder) $ \(name, r) ->
                if ".PS3" `T.isSuffixOf` name
                  then do
                    let edatName = name <> ".EDAT"
                    stackIO $ saveReadable r tmpIn
                    tmpIn'  <- shortWindowsPath False tmpIn
                    tmpOut' <- shortWindowsPath True  tmpOut
                    stackIO $ packNPData NPDataConfig
                      { npdContentID = contentID
                      , npdKLIC      = ghworKLIC
                      , npdRAP       = Nothing
                      , npdVersion   = 2
                      , npdLicense   = 3
                      , npdType      = 0
                      , npdBlock     = 16
                      , npdEDAT      = True
                      } tmpIn' tmpOut' (TE.encodeUtf8 edatName)
                    edat <- stackIO $ BL.fromStrict <$> B.readFile tmpOut
                    return (edatName, makeHandle "(new edat)" $ byteStringSimpleHandle edat)
                  else return (name, r)
              newFolders <- forM (folderSubfolders folder) $ \(name, sub) -> do
                sub' <- modifyFolder sub
                return (name, sub')
              return Folder { folderFiles = newFiles, folderSubfolders = newFolders }
            in modifyFolder folderOrig
          stackIO $ makePKG contentID (first TE.encodeUtf8 folderEDATs) pkg
          return [pkg]
        False -> fatal $ "onyx pkg expected directory; given: " <> dir
      _ -> fatal $ "onyx pkg expected 2 arguments, given " <> show (length args)
    }

  , Command
    { commandWord = "edat"
    , commandDesc = "Encrypt a file into a PS3 .edat file."
    , commandUsage = T.unlines
      [ "onyx edat (content-id) (klic) file-in [--to file-out.edat]"
      , "* content-id should be like: UP0002-BLUS30487_00-MYPACKAGELABEL"
      , "* klic should be a 16-byte hex string (32 chars) like: d7f3f90a1f012d844ca557e08ee42391"
      , "  * or, it can be `neversoft` to use the common NS GH klic"
      , "  * or, it can be `rb-FOLDERNAME` to use the RB klic for the given folder name"
      , "* the output filename affects encryption, so it should be named as it will be on the console"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [argContentID, argKlic, fin] -> do
        let contentID = TE.encodeUtf8 $ T.pack argContentID
        klic <- case argKlic of
          "neversoft" -> return ghworKLIC
          _ -> case stripPrefix "rb-" argKlic of
            Just folder -> return $ rockBandKLIC $ TE.encodeUtf8 $ T.pack folder
            Nothing -> let
              lookupHex c = lookup (toLower c) $ zip "0123456789abcdef" [0..]
              hexes = mapMaybe lookupHex argKlic
              in case length hexes of
                32 -> return $ B.pack $ map (sum . zipWith (*) [16, 1]) $ chunksOf 2 hexes
                n  -> fatal $ "Expected KLIC to have 32 hex digits, but it has " <> show n
        fout <- outputFile opts $ return $ fin <.> "edat"
        fin' <- shortWindowsPath False fin
        fout' <- shortWindowsPath True fout
        stackIO $ packNPData NPDataConfig
          { npdContentID = contentID
          , npdKLIC      = klic
          , npdRAP       = Nothing
          , npdVersion   = 2
          , npdLicense   = 3
          , npdType      = 0
          , npdBlock     = 16
          , npdEDAT      = True
          } fin' fout' (TE.encodeUtf8 $ T.pack $ takeFileName fout)
        return [fout]
      _ -> fatal $ "onyx edat expected 3 arguments, given " <> show (length args)
    }

  , Command
    { commandWord = "port-gh-ps3"
    , commandDesc = "Extract a Neversoft GH Xbox 360 STFS file, and get the contents ready to repackage for PS3."
    , commandUsage = T.unlines
      [ "1. onyx port-gh-ps3 (live-file) [--to dir]"
      , "2. for any .REPLACEME audio files, replace with a valid .FSB.PS3"
      , "  - can use `onyx encrypt-gh-fsb ghwt WHATEVER.FSB` if you need to GH-encrypt"
      , "3. pick a content ID like UP0002-BLUS30487_00-MYPACKAGELABEL"
      , "4. onyx pkg (CONTENT-ID) dir [--to out.pkg]"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [stfs] -> do
        out <- outputFile opts $ return $ stfs <> "_ps3"
        displayName <- stackIO $ withSTFSPackage stfs $ return . head . md_DisplayName . stfsMetadata
        let displayName' = TE.decodeLatin1 $ packageNameHashFormat True displayName
        contents <- stackIO $ getSTFSFolder stfs
        contents' <- let
          modifyFolder folder = do
            newFiles <- flip concatMapM (folderFiles folder) $ \(name, r) -> let
              upper = T.toUpper name
              in case T.stripSuffix ".FSB.XEN" upper of
                Just audioName -> return $ return
                  ( audioName <> ".FSB.PS3.REPLACEME"
                  , makeHandle "(empty file)" $ byteStringSimpleHandle BL.empty
                  )
                Nothing -> case T.stripSuffix ".XEN" upper of
                  Just baseName -> case T.stripSuffix ".PAK" baseName of
                    Just pakName -> case T.stripPrefix "B" pakName of
                      Just dlcN_song -> let
                        dlcID = qbKeyCRC $ TE.encodeUtf8 $ T.takeWhile (/= '_') dlcN_song
                        in return
                          [ (baseName <> ".PS3", r)
                          , ( pakName <> "_VRAM.PAK.PS3"
                            , makeHandle "(vram pak)" $ byteStringSimpleHandle $ worFilePS3SongVRAMPak dlcID
                            )
                          ]
                      Nothing -> return
                        [ (baseName <> ".PS3", r)
                        , ( pakName <> "_VRAM.PAK.PS3"
                          , makeHandle "(vram pak)" $ byteStringSimpleHandle worFilePS3EmptyVRAMPak
                          )
                        ]
                    Nothing -> return [(baseName <> ".PS3", r)]
                  Nothing       -> return [(name, r)] -- shouldn't happen
            newFolders <- forM (folderSubfolders folder) $ \(name, sub) -> do
              sub' <- modifyFolder sub
              return (name, sub')
            return Folder { folderFiles = newFiles, folderSubfolders = newFolders }
          in modifyFolder contents
        additions <- stackIO $ getResourcesPath "pkg-contents/ghwor" >>= crawlFolder
        let pkgContents = additions <> Folder
              { folderSubfolders = [("USRDIR", Folder
                { folderSubfolders = [(displayName', contents')]
                , folderFiles = []
                })]
              , folderFiles = []
              }
        stackIO $ saveHandleFolder pkgContents out
        return [out]
      _ -> fatal $ "onyx port-gh-ps3 expected 1 argument, given " <> show (length args)

    }

  , Command
    { commandWord = "extract"
    , commandDesc = T.unlines
      [ "Extract various archive/container formats to a folder. These formats are currently (somewhat) supported:"
      , "- Xbox 360 STFS (CON/LIVE)"
      , "- Rocksmith .psarc"
      , "- Magma (v1/v2) .rba"
      , "- Rock Band .milo_*"
      , "- Power Gig .hdr.e.2"
      , "- Standard (PS2) and Xbox 360 .iso"
      , "- FMOD Sample Bank v3/v4 (extracts to individual streams)"
      , "- Wii U8 archive"
      , "- Harmonix PS2 .vgs (extracts each channel to WAV)"
      , "- Rock Band iOS .blob"
      , "- Guitar Hero iOS .iga"
      , "- Harmonix .hdr/.ark (versions 2-6)"
      , "- Neversoft .pak.*"
      , "- Wii .wad"
      ]
    , commandUsage = T.unlines
      [ "onyx extract file_in [--to folder_out]"
      ]
    , commandList = True
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
      (FileFSB, fin) -> do
        -- not encrypted
        let out = fin <> "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        stackIO $ splitFSBStreamsToDir fin out
        return out
      (FileU8, fin) -> do
        out <- outputFile opts $ return $ fin <> "_extract"
        (u8, _) <- stackIO (B.readFile fin) >>= readU8 . BL.fromStrict
        stackIO $ flip saveHandleFolder out
          $ bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle) u8
        return out
      (FileVGS, vgs) -> do
        out <- outputFile opts $ return $ vgs <> "_extract"
        srcs <- liftIO $ readVGS vgs
        forM_ (zip [0..] srcs) $ \(i, src) -> do
          let fout = out </> show (i :: Int) <.> "wav"
          runAudio (CA.mapSamples CA.fractionalSample src) fout
        return out
      (FileBlob, fin) -> do
        out <- outputFile opts $ return $ fin <> "_extract"
        stackIO $ do
          Dir.createDirectoryIfMissing False out
          dec <- IOS.decodeBlob fin
          (blob, dats) <- IOS.loadBlob fin
          B.writeFile (out </> "decrypted.bin") dec
          writeFile (out </> "decrypted.txt") $ show blob
          forM_ dats $ \(fout, bs) -> B.writeFile (out </> fout) bs
        return out
      (FileIGA, fin) -> do
        out <- outputFile opts $ return $ fin <> "_extract"
        (_hdr, contents) <- stackIO $ loadIGA fin
        stackIO $ Dir.createDirectoryIfMissing False out
        stackIO $ forM_ contents $ \(name, content) -> do
          let outName = out </> B8.unpack name
          Dir.createDirectoryIfMissing True $ takeDirectory outName
          BL.writeFile outName content
        return out
      (FileHdrOrArk, fin) -> do
        out <- outputFile opts $ return $ fin <> "_extract"
        hdr <- stackIO (B.readFile fin) >>= Ark.readHdr . BL.fromStrict
        let arks = do
              ark <- Ark.getFileArks hdr $ T.pack $ takeFileName fin
              return $ takeDirectory fin </> T.unpack ark
        Ark.extractArk hdr (map fileReadable arks) out
        return out
      (FilePak, pak) -> inside ("extracting pak " <> pak) $ do
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
        return dout
      (FileWAD, fin) -> do
        out <- outputFile opts $ return $ fin <> "_extract"
        stackIO $ Dir.createDirectoryIfMissing False out
        wad <- stackIO (BL.readFile fin) >>= runGetM getWAD
        (initialData, u8s) <- hackSplitU8s wad
        stackIO $ B.writeFile (out </> "initial-data.bin") initialData
        forM_ (zip [0..] u8s) $ \(i, (u8Bytes, u8)) -> do
          stackIO $ B.writeFile (out </> ("u8-" <> show (i :: Int) <> ".app")) u8Bytes
          let u8' = bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle) u8
          stackIO $ saveHandleFolder u8' $ out </> ("u8-" <> show (i :: Int))
        return out
      p -> fatal $ "Unexpected file type given to extractor: " <> show p
    }

  , Command
    { commandWord = "unwrap"
    , commandDesc = "Decodes/decrypts a single file inside certain formats, see usage."
    , commandUsage = T.unlines
      [ "onyx unwrap in.fsb.xen --to out.fsb  # decrypt Neversoft GH FSB"
      , "onyx unwrap in.mogg --to out.ogg     # unwrap unencrypted MOGG"
      , "onyx unwrap in.png_xbox --to out.png # decode Harmonix image format"
      ]
    , commandList = True
    , commandRun = \files opts -> forM files $ \f -> identifyFile' f >>= \case
      (FileFSBEncrypted, fin) -> do
        dec <- stackIO (decryptFSB fin) >>= maybe (fatal "Couldn't decrypt GH .fsb.(xen/ps3) audio") return
        out <- outputFile opts $ return $ case stripSuffix ".fsb.xen" fin <|> stripSuffix ".fsb.ps3" fin <|> stripSuffix ".FSB.PS3" fin of
          Just root -> root <.> "fsb"
          Nothing   -> f <.> "fsb"
        stackIO $ BL.writeFile out dec
        return out
      (FileMOGG, fin) -> do
        ogg <- outputFile opts $ return $ fin -<.> "ogg"
        moggToOgg fin ogg
        return ogg
      (FileHarmonixImage, fin) -> do
        let isPS3 = map toLower (takeExtension fin) == ".png_ps3"
        bs <- stackIO $ fmap BL.fromStrict $ B.readFile fin
        case readRBImageMaybe isPS3 bs of
          Just img -> do
            fout <- outputFile opts $ return $ fin -<.> ".png"
            stackIO $ writePng fout img
            return fout
          Nothing -> fatal "Couldn't decode image file"
      p -> fatal $ "Unexpected file type given to unwrap: " <> show p
    }

  , Command
    { commandWord = "midi-text"
    , commandDesc = "Convert a MIDI file to/from a plaintext format. Some information at https://github.com/mtolly/midiscript"
    , commandUsage = ""
    , commandList = True
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
    , commandDesc = "Convert a MIDI file to a plaintext format. (modified for git use)"
    , commandUsage = "onyx midi-text-git in.mid > out.midtxt"
    , commandList = False
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
    { commandWord = "midi-merge"
    , commandDesc = "Take tracks from one MIDI file, and convert them to a different file's tempo map."
    , commandUsage = T.unlines
      [ "onyx midi-merge base.mid tracks.mid pad  n --to out.mid"
      , "onyx midi-merge base.mid tracks.mid drop n --to out.mid"
      ]
    , commandList = True
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
    { commandWord = "bin-to-dta"
    , commandDesc = "Converts Amplitude (PS2) .bin data to .dta"
    , commandUsage = T.unlines
      [ "onyx bin-to-dta in.bin [--to out.dta]"
      , "# to preserve source file and line number info:"
      , "onyx bin-to-dta in.bin [--to out.dta] --bin-sources"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin -<.> "dta"
        let binSources = elem OptBinSources opts
        bs <- stackIO $ B.readFile fin
        txtBin <- runGetM getTxtBin $ BL.fromStrict bs
        let dta = showDTA $ fmap TE.decodeLatin1
              $ (if binSources then txtBinToTracedDTA else txtBinToDTA) txtBin
            comments = case filter (\src -> T.take 1 src /= "(") $ map TE.decodeLatin1 txtBin.sources of
              []      -> ""
              sources -> T.unlines ("; .bin sources:" : map (";   " <>) sources) <> "\n"
        stackIO $ B.writeFile fout $ B8.pack $ T.unpack $ if binSources
          then dta
          else comments <> dta
        return [fout]
      _ -> fatal "Expected 1 argument"
    }

  , Command
    { commandWord = "dta-to-bin"
    , commandDesc = T.unlines
      [ "Converts .dta to Amplitude (PS2) .bin."
      , "Note, only specific constructs are supported, matching the output of bin-to-dta."
      ]
    , commandUsage = T.unlines
      [ "onyx dta-to-bin in.dta [--to out.bin]"
      ]
    , commandList = True
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin -<.> "bin"
        txtBin <- stackIO (B.readFile fin) >>= readDTABytes >>= dtaToTxtBin
        stackIO $ BL.writeFile fout $ putTxtBin txtBin
        return [fout]
      _ -> fatal "Expected 1 argument"
    }

  , Command
    { commandWord = "add-track"
    , commandDesc = "Add an empty track to a MIDI file with a given name."
    , commandUsage = "onyx add-track \"TRACK NAME\" --to [notes.mid|song.yml]"
    , commandList = False
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
    { commandWord = "import-audio"
    , commandDesc = ""
    , commandUsage = ""
    , commandList = False
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

  ]

_oldCommands :: [Command]
_oldCommands =

  [ Command
    { commandWord = "magma-check"
    , commandDesc = "Run Magma compilation for an RB3 target in a project, to check for errors."
    , commandUsage = ""
    , commandList = False
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
    { commandWord = "pack-powergig"
    , commandDesc = "Pack a folder into Data.hdr.e.2 and Data.pk0."
    , commandUsage = ""
    , commandList = False
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
    , commandList = False
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
    , commandList = False
    , commandRun = \files _opts -> forM files $ \f -> do
      let out = f <> ".e.2"
      stackIO (B.readFile f) >>= PG.encryptE2 >>= stackIO . BL.writeFile out
      return out
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
    , commandList = False
    , commandRun = \files opts -> do
      fin <- getInputMIDI files
      fout <- outputFile opts $ return $ fin -<.> "dolphin.mid"
      applyMidiFunction (getDolphinFunction opts) fin fout
      return [fout]
    }

  , Command
    { commandWord = "install-ark"
    , commandDesc = "Install a song into GH2 (PS2)."
    , commandUsage = "onyx install-ark song GEN/ --target gh2"
    , commandList = False
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
    , commandList = False
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
    , commandList = False
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
    , commandList = False
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
    { commandWord = "scoring"
    , commandDesc = ""
    , commandUsage = T.unlines
      [ "onyx scoring in.mid [players]"
      , "players is a sequence that alternates:"
      , "  part: G B D K V PG PB PD PK H"
      , "  difficulty: e m h x"
      , "for example: onyx scoring in.mid G x B x PD x V x"
      ]
    , commandList = False
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
    { commandWord = "amp-pack"
    , commandDesc = ""
    , commandUsage = ""
    , commandList = False
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
    , commandList = False
    , commandRun = \args _opts -> case args of
      [dir, hdr] -> do
        let ark = dropExtension hdr <> "_0.ARK"
        stackIO $ GHArk.createHdrArk dir hdr ark
        return [hdr, ark]
      _ -> fatal "Expected 2 args (folder to pack, .hdr)"
    }

  , Command
    { commandWord = "coop-max-scores"
    , commandDesc = ""
    , commandUsage = "onyx coop-max-scores notes.mid"
    , commandList = False
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
    { commandWord = "make-qb"
    , commandDesc = "Compile a .yaml file back into a .qb"
    , commandUsage = "onyx make-qb in.qb.parsed.yaml [--to out.qb]"
    , commandList = False
    , commandRun = \args opts -> case args of
      [fin] -> do
        fout <- outputFile opts $ return $ fin <.> "qb"
        qb <- stackIO (Y.decodeFileEither fin) >>= either (\e -> fatal $ show e) return
        stackIO $ BL.writeFile fout $ putQB $ discardStrings qb
        return [fout]
      _ -> fatal "Expected 1 argument (.yaml)"
    }

  , Command
    { commandWord = "make-wor-database"
    , commandDesc = ""
    , commandUsage = ""
    , commandList = False
    , commandRun = \args opts -> do
      fout <- outputFile opts $ fatal "make-wor-database requires --to argument"
      makeMetadataLIVE args fout
      return [fout]
    }

  , Command
    { commandWord = "test-rb3-venue"
    , commandDesc = ""
    , commandUsage = T.unlines
      [ "onyx test-rb3-venue song.anim --to out.txt"
      , "onyx test-rb3-venue song.anim in.mid --to out.mid"
      ]
    , commandList = False
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
    { commandWord = "to-xma2"
    , commandDesc = "Converts a (single-stream) XMA1 file to XMA2."
    , commandUsage = T.unlines
      [ "onyx to-xma2 in.xma [--to out.xma]"
      ]
    , commandList = False
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
    , "Commands: " <> T.unwords (map commandWord $ filter commandList commands)
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
  , Option []   ["bin-sources"    ] (NoArg  OptBinSources                     ) ""
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
  | OptBinSources
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
