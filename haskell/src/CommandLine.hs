{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module CommandLine where

import           Build                          (shakeBuild, targetTitle)
import           Config
import qualified Control.Exception              as Exc
import           Control.Monad                  (forM, forM_)
import           Control.Monad.Extra            (filterM)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader     (runReaderT)
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import           Data.Aeson                     ((.:))
import qualified Data.Aeson.Types               as A
import           Data.Binary                    (Binary, decodeFileOrFail)
import qualified Data.ByteString.Lazy           as BL
import           Data.ByteString.Lazy.Char8     ()
import           Data.Char                      (isAscii, isPrint, isSpace)
import qualified Data.Digest.Pure.MD5           as MD5
import           Data.DTA.Lex                   (scanEither)
import           Data.DTA.Parse                 (parseEither)
import qualified Data.DTA.Serialize.Magma       as RBProj
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.DTA.Serialize2            as D
import           Data.Either                    (rights)
import           Data.Functor                   (void)
import           Data.Hashable                  (hash)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (fromMaybe, mapMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           Data.Word                      (Word32)
import qualified Data.Yaml                      as Y
import           Import                         (importFoF, importRBA,
                                                 importSTFS)
import           JSONData                       (traceJSON)
import           Magma                          (oggToMogg, runMagma)
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
import qualified System.Directory               as Dir
import           System.FilePath                (takeDirectory, takeExtension,
                                                 takeFileName, (-<.>), (</>))
import           System.Info                    (os)
import           System.IO.Error                (ioeGetErrorString, tryIOError)
import           System.IO.Temp                 (withSystemTempDirectory)
import           System.Process                 (callProcess, spawnCommand)
import           Text.Printf                    (printf)
import           X360                           (rb2pkg, rb3pkg, stfsFolder)
import           YAMLTree                       (readYAMLTree,
                                                 readYAMLTreeStack)

#ifdef WINDOWS
import           Data.Bits                      (testBit)
import           System.Win32.File              (getLogicalDrives)
#else
import           Data.List                      (isPrefixOf)
import           System.MountPoints
#endif

type Match = WriterT [Matcher] (StackTraceT IO)

data Matcher = Matcher
  { matcherTest  :: T.Text -> StackTraceT IO (Match ())
  , matcherClick :: Click
  , matcherDesc  :: T.Text
  }

-- | What happens when clicking a button in the GUI?
data Click
  = ClickText T.Text -- ^ Add the given text to the end of the command
  | ClickPick [T.Text] -- ^ Launch an Open File dialog, with patterns
  | ClickPickDir -- ^ Launch an Open Folder dialog
  | ClickSave -- ^ Launch a Save File dialog
  | ClickEnd (StackTraceT IO T.Text) -- ^ Do a thing

inputFile :: (FilePath -> Match ()) -> Match ()
inputFile cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (Dir.doesFileExist f) >>= \b -> if b
      then return $ cont f
      else fatal "Expected an existing file"
  , matcherClick = ClickPick ["*"]
  , matcherDesc = "File"
  }

matchSongYml :: (FilePath -> SongYaml -> Match ()) -> Match ()
matchSongYml cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    isDir <- liftIO $ Dir.doesDirectoryExist f
    let path = if isDir then f </> "song.yml" else f
    yaml <- readYAMLTreeStack path
    cont path <$> mapStackTraceT (`runReaderT` yaml) traceJSON
  , matcherClick = ClickPick ["*.yml", "*.yaml"]
  , matcherDesc = "An Onyx song.yml file"
  }

matchTarget :: SongYaml -> (T.Text -> Target -> Match ()) -> Match ()
matchTarget songYaml cont
  = forM_ (Map.toList $ _targets songYaml) $ \(targetName, target) -> let
    desc = case target of
      RB3{} -> "Rock Band 3 target"
      RB2{} -> "Rock Band 2 target"
      PS {} -> "Phase Shift target"
    in word targetName desc $ cont targetName target

matchPlan :: SongYaml -> (T.Text -> Plan -> Match ()) -> Match ()
matchPlan songYaml cont
  = forM_ (Map.toList $ _plans songYaml) $ \(planName, plan)
  -> word planName "Plan" $ cont planName plan

word :: T.Text -> T.Text -> Match () -> Match ()
word t desc cont = tell $ (: []) $ Matcher
  { matcherTest = \t' -> if t == t'
    then return cont
    else fatal $ "Expected " <> T.unpack t
  , matcherClick = ClickText t
  , matcherDesc = desc
  }

matchBinary :: (Binary a) => T.Text -> (FilePath -> a -> Match ()) -> Match ()
matchBinary desc cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (Dir.doesFileExist f) >>= \case
      True -> return ()
      False -> fatal "File does not exist"
    liftIO (decodeFileOrFail f) >>= \case
      Left err -> inside f $ fatal (show err)
      Right x -> return $ cont f x
  , matcherClick = ClickPick []
  , matcherDesc = desc
  }

matchMagma :: (FilePath -> RBProj.RBProj -> Match ()) -> Match ()
matchMagma cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (tryIOError $ T.readFile f) >>= \case
      Left err -> fatal $ show err
      Right txt -> inside f $ do
        toks <- either fatal return $ scanEither txt
        dta <- either fatal return $ parseEither toks
        rbproj <- D.unserialize D.format dta
        return $ cont f rbproj
  , matcherClick = ClickPick ["*.rbproj"]
  , matcherDesc = "A Magma .rbproj file"
  }

outputFile :: T.Text -> (FilePath -> Match ()) -> Match ()
outputFile desc cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> if "--" `T.isPrefixOf` t
    then fatal "Parsed as an option (starts with --)"
    else return $ cont $ T.unpack t
  , matcherClick = ClickSave
  , matcherDesc = desc
  }

-- | Can be an input directory, or directory to save things into
matchDir :: T.Text -> (FilePath -> Match ()) -> Match ()
matchDir desc cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (Dir.doesDirectoryExist $ T.unpack fp) >>= \case
    True  -> return $ cont $ T.unpack fp
    False -> fatal "Not a directory"
  , matcherClick = ClickPickDir
  , matcherDesc = desc
  }

end :: T.Text -> StackTraceT IO T.Text -> Match ()
end desc act = tell $ (: []) $ Matcher
  { matcherTest = const $ fatal "Expected end of input"
  , matcherClick = ClickEnd act
  , matcherDesc = desc
  }

matchSTFS :: (FilePath -> Match ()) -> Match ()
matchSTFS cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (tryIOError $ BL.readFile $ T.unpack fp) >>= \case
    Left ioe -> fatal $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs `elem` ["CON ", "LIVE"]
      then return $ cont $ T.unpack fp
      else fatal "Not an STFS (CON/LIVE) file"
  , matcherClick = ClickPick ["*_rb3con"]
  , matcherDesc = "An STFS (CON/LIVE) Xbox 360 package"
  }

matchMIDI :: (FilePath -> Match ()) -> Match ()
matchMIDI cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (tryIOError $ BL.readFile $ T.unpack fp) >>= \case
    Left ioe -> fatal $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs == "MThd"
      then return $ cont $ T.unpack fp
      else fatal "Not a MIDI file"
  , matcherClick = ClickPick ["*.mid", "*.midi"]
  , matcherDesc = "A Standard MIDI File (SMF)"
  }

matchFoF :: (FilePath -> Match ()) -> Match ()
matchFoF cont = tell $ (: []) $ Matcher
  { matcherTest = \t -> do
    let f = T.unpack t
    liftIO (Dir.doesDirectoryExist f) >>= \case
      True -> do
        let path = f </> "song.ini"
        hasFile <- liftIO $ Dir.doesFileExist path
        if hasFile
          then return $ cont path
          else fatal "Directory does not have a song.ini"
      False -> if takeFileName f == "song.ini"
        then return $ cont f
        else fatal "Not a song.ini file"
  , matcherClick = ClickPick ["*.ini"]
  , matcherDesc = "A song.ini for Frets on Fire or Phase Shift"
  }

matchMOGG :: (FilePath -> Match ()) -> Match ()
matchMOGG cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeExtension (T.unpack $ T.toLower fp) == ".mogg"
    then return $ cont $ T.unpack fp
    else fatal "Not a MOGG file"
  , matcherClick = ClickPick ["*.mogg"]
  , matcherDesc = "A Rock Band MOGG file"
  }

matchOGG :: (FilePath -> Match ()) -> Match ()
matchOGG cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> if takeExtension (T.unpack $ T.toLower fp) == ".ogg"
    then return $ cont $ T.unpack fp
    else fatal "Not an OGG file"
  , matcherClick = ClickPick ["*.ogg"]
  , matcherDesc = "An OGG Vorbis file"
  }

matchRBA :: (FilePath -> Match ()) -> Match ()
matchRBA cont = tell $ (: []) $ Matcher
  { matcherTest = \fp -> liftIO (tryIOError $ BL.readFile $ T.unpack fp) >>= \case
    Left ioe -> fatal $ ioeGetErrorString ioe
    Right lbs -> if BL.take 4 lbs == "RBSF"
      then return $ cont $ T.unpack fp
      else fatal "Not an RBA file"
  , matcherClick = ClickPick ["*.rba"]
  , matcherDesc = "A Magma RBA file"
  }

getInfoForSTFS :: FilePath -> FilePath -> IO (T.Text, T.Text)
getInfoForSTFS dir stfs = do
  let getDTAInfo = do
        (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
        return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
      handler1 :: Exc.IOException -> IO (T.Text, T.Text)
      handler1 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
      handler2 :: Exc.ErrorCall -> IO (T.Text, T.Text)
      handler2 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
  getDTAInfo `Exc.catch` handler1 `Exc.catch` handler2

withDefaultFilename :: FilePath -> T.Text -> (FilePath -> StackTraceT IO T.Text) -> Match ()
withDefaultFilename f desc cont = do
  end (T.pack f) $ cont f
  outputFile desc $ \out -> end "" $ cont out

findXbox360USB :: IO [FilePath]
findXbox360USB = do
#ifdef WINDOWS
  dword <- getLogicalDrives
  let drives = [ letter : ":\\" | (letter, i) <- zip ['A'..'Z'] [0..], dword `testBit` i ]
#else
  mnts <- getMounts
  let drives = map mnt_dir $ flip filter mnts $ \mnt ->
        ("/dev/" `isPrefixOf` mnt_fsname mnt) && (mnt_dir mnt /= "/")
#endif
  filterM (\drive -> Dir.doesDirectoryExist $ drive </> "Content") drives

installSTFS :: FilePath -> FilePath -> IO ()
installSTFS stfs usb = do
  (titleID, sign) <- stfsFolder stfs
  stfsHash <- take 10 . show . MD5.md5 <$> BL.readFile stfs
  let folder = "Content/0000000000000000" </> w32 titleID </> w32 sign
      w32 :: Word32 -> FilePath
      w32 = printf "%08x"
      file = "onyx_" ++ stfsHash
  Dir.copyFile stfs $ usb </> folder </> file

slurpArgs :: ([FilePath] -> StackTraceT IO T.Text) -> Match ()
slurpArgs cont = go [] where
  go args = do
    end "" $ cont args
    tell $ (: []) $ Matcher
      { matcherTest = \t -> return $ go $ args ++ [T.unpack t]
      , matcherClick = ClickSave -- ???
      , matcherDesc = ""
      }

selectUSB :: (FilePath -> StackTraceT IO T.Text) -> Match ()
selectUSB cont = do
  liftIO findXbox360USB >>= \case
    [usb] -> end (T.pack usb) $ cont usb
    _     -> let
      err = "0 or multiple Xbox 360 USB drives detected"
      in end err $ fatal $ T.unpack err
  matchDir "USB drive to install to" $ \usb -> do
    end "" $ cont usb

readConfig :: Match (Map.HashMap T.Text Y.Value)
readConfig = do
  cfg <- liftIO $ Dir.getXdgDirectory Dir.XdgConfig "onyx.yml"
  liftIO (Dir.doesFileExist cfg) >>= \case
    False -> return Map.empty
    True  -> liftIO (Y.decodeFileEither cfg) >>= \case
      Left err -> lift $ fatal $ show err
      Right x  -> return x

firstPresentTarget :: FilePath -> [T.Text] -> IO T.Text
firstPresentTarget yamlPath targets = do
  songYaml
    <-  readYAMLTree yamlPath
    >>= runReaderT (printStackTraceIO traceJSON)
  let present = Map.keys $ _targets songYaml
  case filter (`elem` present) targets of
    []    -> fail $ "panic! couldn't find any of these targets: " ++ show targets
    t : _ -> return t

makeFilename :: T.Text -> T.Text -> FilePath
makeFilename title artist = let
  hashed = hash (title, artist) `mod` 10000000
  safeInfo = T.filter (\c -> isPrint c && isAscii c && not (isSpace c)) $ title <> artist
  in take 32 $ "o" <> show hashed <> "_" <> T.unpack safeInfo

lookupArgs :: [T.Text] -> Match () -> StackTraceT IO (StackTraceT IO T.Text)
lookupArgs []       m = do
  matchers <- execWriterT m
  let ends = flip mapMaybe matchers $ \matcher -> case matcherClick matcher of
        ClickEnd act -> Just act
        _            -> Nothing
  case ends of
    []  -> fatal "Unexpected end of arguments"
    [x] -> return x
    n   -> fatal $ "Panic! Command line was ambiguous, matched " <> show (length n) <> " patterns"
lookupArgs (t : ts) m = do
  matchers <- execWriterT m
  results <- liftIO $ forM matchers $ \matcher ->
    runStackTraceT $ matcherTest matcher t
  case rights $ map fst results of
    []   -> fatal $ "Unrecognized argument: " ++ show t
    good -> inside ("Argument " ++ show t) $ lookupArgs ts $ sequence_ good

copyDirRecursive :: FilePath -> FilePath -> IO ()
copyDirRecursive src dst = do
  Dir.createDirectoryIfMissing False dst
  ents <- Dir.listDirectory src
  forM_ ents $ \ent -> do
    let pathFrom = src </> ent
        pathTo = dst </> ent
    isDir <- Dir.doesDirectoryExist pathFrom
    if isDir
      then copyDirRecursive pathFrom pathTo
      else Dir.copyFile     pathFrom pathTo

osOpenFile :: FilePath -> IO ()
osOpenFile f = case os of
  "mingw32" -> void $ spawnCommand f
  "darwin"  -> callProcess "open" [f]
  "linux"   -> callProcess "exo-open" [f]
  _         -> return ()

commandLine :: Match ()
commandLine = do

  config <- readConfig
  addJammitDir <- maybe id (:) <$> liftIO J.findJammitDir
  audioDirs <- lift $ case A.parseEither (.: "audio-dirs") config of
    Left _ -> return []
    Right obj -> do
      dirs <- either fatal return $ A.parseEither A.parseJSON obj
      mapM (liftIO . Dir.canonicalizePath) $ addJammitDir dirs

  matchSongYml $ \yamlPath songYaml -> do
    yamlDir <- liftIO $ Dir.canonicalizePath $ takeDirectory yamlPath
    let shakeIt = liftIO . shakeBuild [] (yamlDir : audioDirs) yamlPath
    word "file" "Don't click this" $ do
      slurpArgs $ \args -> do
        shakeIt args
        return ""
    matchPlan songYaml $ \planName _plan -> do
      word "reap" "Open plan in REAPER" $ do
        end "" $ do
          let rpp = "notes-" <> T.unpack planName <> ".RPP"
          shakeIt [rpp]
          liftIO $ do
            let rppFull = yamlDir </> "notes.RPP"
            Dir.renameFile (yamlDir </> rpp) rppFull
            osOpenFile rppFull
          return ""
      word "player" "Open web player" $ do
        end "" $ do
          let player = "gen/plan" </> T.unpack planName </> "web"
          shakeIt [player]
          liftIO $ osOpenFile $ player </> "index.html"
          return ""
    matchTarget songYaml $ \targetName target -> do
      case target of
        RB3 rb3 -> do
          let built = "gen/target" </> T.unpack targetName </> "rb3con"
              filename = makeFilename (targetTitle songYaml $ RB3 rb3) (fromMaybe "" $ _artist $ _metadata songYaml)
          word "con" "Create an Xbox 360 CON-STFS package" $ do
            outputFile "CON file to save to" $ \con -> do
              end "" $ do
                shakeIt [built]
                liftIO $ Dir.copyFile (yamlDir </> built) con
                return ""
          word "install" "Install to an Xbox 360 USB drive" $ do
            selectUSB $ \usb -> do
              shakeIt [built]
              liftIO $ Dir.copyFile (yamlDir </> built) $ usb </> "Content/0000000000000000/45410914/00000001" </> filename
              return ""
        RB2 rb2 -> do
          let built = "gen/target" </> T.unpack targetName </> "rb2con"
              filename = makeFilename (targetTitle songYaml $ RB2 rb2) (fromMaybe "" $ _artist $ _metadata songYaml)
          word "con" "Create an Xbox 360 CON-STFS package" $ do
            outputFile "CON file to save to" $ \con -> do
              end "" $ do
                shakeIt [built]
                liftIO $ Dir.copyFile (yamlDir </> built) con
                return ""
          word "install" "Install to an Xbox 360 USB drive" $ do
            selectUSB $ \usb -> do
              shakeIt [built]
              liftIO $ Dir.copyFile (yamlDir </> built) $ usb </> "Content/0000000000000000/45410869/00000001" </> filename
              return ""
        PS  ps  -> do
          word "zip" "Create a .zip file for distribution" $ do
            outputFile "ZIP file to create" $ \zipPath -> do
              undefined ps zipPath
          word "install" "Install to a Phase Shift installation" $ do
            undefined ps
  matchMagma $ \rbprojPath rbproj -> do
    word "rba" "Create an RBA file for Audition mode" $ do
      withDefaultFilename (T.unpack $ RBProj.destinationFile $ RBProj.project rbproj) "RBA file to build to" $ \rba -> do
        liftIO $ T.pack <$> runMagma rbprojPath rba
    word "con" "Create an Xbox 360 CON-STFS package" $ do
      end "Use the CON path listed in the .rbproj" $ do
        undefined
      outputFile "CON file to save to" $ \con -> do
        end "" $ do
          undefined con
    word "import" "Convert a Magma project to an Onyx project" $ do
      withDefaultFilename (rbprojPath ++ "_import") "Folder to import into" $ \dir -> do
        undefined rbprojPath rbproj dir
  matchSTFS $ \stfs -> do
    word "install" "Install the CON file to an Xbox 360 USB drive" $ do
      selectUSB $ \usb -> liftIO (installSTFS stfs usb) >> return ""
    word "player" "Generates a browser chart preview app" $ do
      withDefaultFilename (stfs ++ "_player") "Folder to build a web player in" $ \dir -> do
        liftIO $ withSystemTempDirectory "onyx_player" $ \tmp -> do
          importSTFS NoKeys stfs tmp
          let player = "gen/plan/mogg/web"
          shakeBuild [] [tmp] (tmp </> "song.yml") [player]
          Dir.createDirectoryIfMissing False dir
          copyDirRecursive (tmp </> player) dir
        return ""
    word "unstfs" "Extract the contents of the STFS package" $ do
      withDefaultFilename (stfs ++ "_extract") "New folder to extract to" $ \dir -> do
        liftIO $ extractSTFS stfs dir
        return ""
    word "import" "Convert an STFS package to an Onyx project" $ do
      withDefaultFilename (stfs ++ "_import") "Folder to import into" $ \dir -> do
        liftIO $ Dir.createDirectoryIfMissing False dir
        liftIO $ importSTFS NoKeys stfs dir
        return ""
    word "convert-rb2" "Converts an RB3 song to RB2 format" $ do
      let go keysMode = withDefaultFilename (stfs ++ "_rb2con") "New RB2 song to create" $ \rb2 -> do
            liftIO $ withSystemTempDirectory "onyx_convert" $ \dir -> do
              importSTFS keysMode stfs dir
              target <- firstPresentTarget (dir </> "song.yml") ["rb2-2x", "rb2"]
              let planCon = "gen/target" </> T.unpack target </> "rb2con"
              shakeBuild [] [dir] (dir </> "song.yml") [planCon]
              Dir.copyFile (dir </> planCon) rb2
            return ""
      word "no-keys" "Drops the Keys part (if any)" $ go NoKeys
      word "keys-guitar" "Assigns RB3 Keys to RB2 Guitar" $ go KeysGuitar
      word "keys-bass" "Assigns RB3 Keys to RB2 Bass" $ go KeysBass
  matchRBA $ \rba -> do
    word "import" "Convert an RBA to an Onyx project" $ do
      withDefaultFilename (rba ++ "_import") "Folder to import into" $ \dir -> do
        liftIO $ importRBA NoKeys rba dir
        return ""
    word "convert" "Convert an RBA to a CON file" $ do
      withDefaultFilename (rba ++ "_rb3con") "New RB3 CON file to create" $ \con -> do
        liftIO $ withSystemTempDirectory "onyx_convert" $ \dir -> do
          importRBA NoKeys rba dir
          target <- firstPresentTarget (dir </> "song.yml") ["rb3-2x", "rb3"]
          let planCon = "gen/target" </> T.unpack target </> "rb3con"
          shakeBuild [] [dir] (dir </> "song.yml") [planCon]
          Dir.copyFile (dir </> planCon) con
        return ""
  matchMIDI $ \mid -> do
    word "reduce" "Fill in missing difficulties in the MIDI" $ do
      withDefaultFilename (mid -<.> "reduced.mid") "New MIDI location" $ \new -> do
        liftIO $ simpleReduce mid new
        return ""
    word "ranges" "Fill in automatic Pro Keys ranges" $ do
      withDefaultFilename (mid -<.> "ranges.mid") "New MIDI location" $ \new -> do
        liftIO $ completeFile mid new
        return ""
    word "rpp" "Convert the MIDI to a Reaper project (.RPP)" $ do
      withDefaultFilename (mid -<.> "RPP") "New RPP location" $ \rpp -> do
        liftIO $ makeReaperIO mid mid [] rpp
        return ""
    word "hanging" "List pro keys range shifts with hanging notes" $ do
      end "" $ do
        song <- liftIO (Load.fromFile mid) >>= RBFile.readMIDIFile
        return $ T.pack $ closeShiftsFile song
    word "text" "Convert MIDI file to an editable plaintext format" $ do
      let go opts = do
            word "--in-measures" "Write MIDI event positions using measure + beats format" $ do
              go opts { MS.showFormat = MS.ShowMeasures }
            word "--in-seconds" "Write MIDI event positions using real time in seconds" $ do
              go opts { MS.showFormat = MS.ShowSeconds }
            word "--separate-lines" "Write each MIDI event on its own line" $ do
              go opts { MS.separateLines = True }
            word "--match-note-off" "Join note on/off events into a single event" $ do
              go opts { MS.matchNoteOff = True }
            withDefaultFilename (mid -<.> "txt") "New text file location" $ \fout -> do
              res <- liftIO $ MS.toStandardMIDI <$> Load.fromFile mid
              case res of
                Left  err -> fatal err
                Right sm  -> liftIO $ writeFile fout $ MS.showStandardMIDI opts sm
              return ""
      go MS.Options
        { MS.showFormat = MS.ShowBeats
        , MS.resolution = Nothing
        , MS.separateLines = False
        , MS.matchNoteOff = False
        }
  word "git-midi-text" "Convert next arg (MIDI file) to plaintext on stdout" $ do
    let opts = MS.Options
          { MS.showFormat = MS.ShowMeasures
          , MS.resolution = Nothing
          , MS.separateLines = False
          , MS.matchNoteOff = True
          }
    matchMIDI $ \mid -> do
      end "" $ do
        res <- liftIO $ MS.toStandardMIDI <$> Load.fromFile mid
        case res of
          Left  err -> fatal err
          Right sm  -> liftIO $ putStrLn $ MS.showStandardMIDI opts sm
        return ""
  matchDir "A folder" $ \dir -> do
    word "stfs" "Make an RB3 CON package from the folder" $ do
      withDefaultFilename (dir ++ "_rb3con") "New CON location" $ \stfs -> liftIO $ do
        (title, desc) <- getInfoForSTFS dir stfs
        T.pack <$> rb3pkg title desc dir stfs
    word "stfs-rb2" "Make an RB2 CON package from the folder" $ do
      withDefaultFilename (dir ++ "_rb2con") "New CON location" $ \stfs -> liftIO $ do
        (title, desc) <- getInfoForSTFS dir stfs
        T.pack <$> rb2pkg title desc dir stfs
  matchMOGG $ \mogg -> do
    word "unmogg" "Unwrap an unencrypted MOGG file into an OGG Vorbis file" $ do
      withDefaultFilename (mogg -<.> "ogg") "New OGG location" $ \ogg -> do
        liftIO $ moggToOgg mogg ogg
        return ""
  matchOGG $ \ogg -> do
    word "mogg" "Wrap an OGG Vorbis file into an unencrypted MOGG file" $ do
      withDefaultFilename (ogg -<.> "mogg") "New OGG location" $ \mogg -> do
        liftIO $ oggToMogg ogg mogg
        return ""
  matchFoF $ \ini -> do
    word "import" "Convert an FoF/PS song to an Onyx project" $ do
      let fof = takeDirectory ini
      withDefaultFilename (fof ++ "_import") "Folder to import into" $ \dir -> do
        liftIO $ importFoF NoKeys fof dir
        return ""
  inputFile $ \fin -> do
    word "midi" "Convert MIDI plaintext format back to a binary MIDI file" $ do
      withDefaultFilename (fin -<.> "mid") "New MIDI file location" $ \fout -> do
        liftIO $ do
          sf <- MS.readStandardFile . MS.parse . MS.scan <$> readFile fin
          let (mid, warnings) = MS.fromStandardMIDI opts sf
              opts = MS.Options
                { MS.showFormat = MS.ShowBeats
                , MS.resolution = Just 480 -- TODO allow overriding this
                , MS.separateLines = False
                , MS.matchNoteOff = False
                }
          Save.toFile fout mid
          case warnings of
            Nothing -> return ""
            Just s  -> return $ T.pack s
