{-# LANGUAGE CPP                       #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
module Main (main) where

import           Build                          (shakeBuild)
import           Config                         hiding (Difficulty)
import           Control.Exception              as Exc
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.DTA.Serialize.RB3         as D
import qualified Data.HashMap.Strict            as HM
import           Data.Maybe                     (fromMaybe, listToMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Development.Shake              hiding (phony)
import           Development.Shake.FilePath
import           Import
import           JSONData                       (traceJSON)
import qualified Magma
import           MoggDecrypt
import           PrettyDTA
import           ProKeysRanges
import           Reaper.Build                   (makeReaper)
import           Reductions
import qualified RockBand.File                  as RBFile
import qualified Sound.Jammit.Base              as J
import qualified Sound.MIDI.File.Load           as Load
import qualified Sound.MIDI.File.Save           as Save
import qualified Sound.MIDI.Script.Base         as MS
import qualified Sound.MIDI.Script.Parse        as MS
import qualified Sound.MIDI.Script.Read         as MS
import qualified Sound.MIDI.Script.Scan         as MS
import           STFS.Extract
import           System.Console.GetOpt
import qualified System.Directory               as Dir
import           System.Environment             (getArgs)
import qualified System.Info                    as Info
import           System.IO                      (hPutStrLn, stderr)
import           System.IO.Temp                 (withSystemTempDirectory)
import           System.Process                 (callProcess, spawnCommand)
import           X360
import           YAMLTree

import qualified SDL
import SDL (($=))
import Control.Exception (bracket, bracket_)
import Control.Concurrent (threadDelay, forkIO)
import System.Exit (exitSuccess)
import Foreign.C (peekCString)
import TinyFileDialogs

main :: IO ()
main = do
  argv <- getArgs
  defaultJammitDir <- J.findJammitDir
  let
    (opts, nonopts, _) = getOpt Permute optDescrs argv
    shakeBuild' buildables yml = do

      yamlPath <- Dir.canonicalizePath $ case yml of
        Just y  -> y
        Nothing -> fromMaybe "song.yml" $ listToMaybe [ f | SongFile f <- opts ]
      audioDirs <- mapM Dir.canonicalizePath
        $ takeDirectory yamlPath
        : maybe id (:) defaultJammitDir [ d | AudioDir d <- opts ]

      shakeBuild (map void optDescrs) audioDirs yamlPath buildables

    makePlayer :: FilePath -> FilePath -> IO ()
    makePlayer fin dout = withSystemTempDirectory "onyx_player" $ \dir -> do
      importAny NoKeys fin dir
      isFoF <- Dir.doesDirectoryExist fin
      let planWeb = if isFoF then "gen/plan/fof/web" else "gen/plan/mogg/web"
      shakeBuild' [planWeb] $ Just $ dir </> "song.yml"
      let copyDir :: FilePath -> FilePath -> IO ()
          copyDir src dst = do
            Dir.createDirectory dst
            content <- Dir.getDirectoryContents src
            let xs = filter (`notElem` [".", ".."]) content
            forM_ xs $ \name -> do
              let srcPath = src </> name
              let dstPath = dst </> name
              isDirectory <- Dir.doesDirectoryExist srcPath
              if isDirectory
                then copyDir  srcPath dstPath
                else Dir.copyFile srcPath dstPath
      b <- Dir.doesDirectoryExist dout
      when b $ Dir.removeDirectoryRecursive dout
      copyDir (dir </> planWeb) dout

    midiTextOptions :: MS.Options
    midiTextOptions = MS.Options
      { showFormat = if
        | PositionSeconds `elem` opts -> MS.ShowSeconds
        | PositionMeasure `elem` opts -> MS.ShowMeasures
        | otherwise                   -> MS.ShowBeats
      , resolution = listToMaybe [ i | Resolution i <- opts ]
      , separateLines = SeparateLines `elem` opts
      , matchNoteOff = MatchNotes `elem` opts
      }

  case nonopts of
    [] -> launchGUI
    ["help"] -> do
      let p = hPutStrLn stderr
      p "Onyxite's Rock Band Custom Song Toolkit"
      p "By Michael Tolly, licensed under the GPL"
      p ""
      -- TODO: print version number or compile date
#ifdef MOGGDECRYPT
      p "Compiled with MOGG decryption."
      p ""
#endif
      p "Usage: onyx [command] [args]"
      p "Commands:"
      p "  build - create files in a Make-like fashion"
      p "  mogg - convert OGG to unencrypted MOGG"
#ifdef MOGGDECRYPT
      p "  unmogg - convert MOGG to OGG (supports some encrypted MOGGs)"
#else
      p "  unmogg - convert unencrypted MOGG to OGG"
#endif
      p "  stfs - pack a directory into a US RB3 CON STFS package"
      p "  stfs-rb2 - pack a directory into a US RB2 CON STFS package"
      p "  unstfs - unpack an STFS package to a directory"
      p "  import - import CON/RBA/FoF to onyx's project format"
      p "  convert - convert CON/RBA/FoF to RB3 CON"
      p "  convert-rb2    - convert RB3 CON to RB2 CON (drop keys)"
      p "  convert-rb2-kg - convert RB3 CON to RB2 CON (guitar is keys)"
      p "  convert-rb2-kb - convert RB3 CON to RB2 CON (bass is keys)"
      p "  reduce - fill in blank difficulties in a MIDI"
      p "  player - create web browser song playback app"
      p "  rpp - convert MIDI to Reaper project"
      p "  ranges - add automatic Pro Keys ranges"
      p "  hanging - find Pro Keys range shifts with hanging notes"
      p "  reap - from onyx project, create and launch Reaper project"
      p "  mt - convert MIDI to a plain text format"
      p "  tm - convert plain text format back to MIDI"
    "build" : buildables -> shakeBuild' buildables Nothing
    "mogg" : args -> case inputOutput ".mogg" args of
      Nothing -> error "Usage: onyx mogg in.ogg [out.mogg]"
      Just (ogg, mogg) -> shake shakeOptions $ action $ Magma.oggToMogg ogg mogg
    "unmogg" : args -> case inputOutput ".ogg" args of
      Nothing          -> error "Usage: onyx unmogg in.mogg [out.ogg]"
      Just (mogg, ogg) -> moggToOgg mogg ogg
    "stfs" : args -> case inputOutput "_rb3con" args of
      Nothing -> error "Usage: onyx stfs in_dir/ [out_rb3con]"
      Just (dir, stfs) -> do
        let getDTAInfo = do
              (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
              return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
            handler1 :: Exc.IOException -> IO (T.Text, T.Text)
            handler1 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
            handler2 :: Exc.ErrorCall -> IO (T.Text, T.Text)
            handler2 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
        (title, desc) <- getDTAInfo `Exc.catch` handler1 `Exc.catch` handler2
        shake shakeOptions $ action $ rb3pkg title desc dir stfs
    "stfs-rb2" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx stfs-rb2 in_dir/ [out_rb2con]"
      Just (dir, stfs) -> do
        let getDTAInfo = do
              (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
              return (D.name pkg, D.name pkg <> " (" <> D.artist pkg <> ")")
            handler1 :: Exc.IOException -> IO (T.Text, T.Text)
            handler1 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
            handler2 :: Exc.ErrorCall -> IO (T.Text, T.Text)
            handler2 _ = return (T.pack $ takeFileName stfs, T.pack stfs)
        (title, desc) <- getDTAInfo `Exc.catch` handler1 `Exc.catch` handler2
        shake shakeOptions $ action $ rb2pkg title desc dir stfs
    "unstfs" : args -> case inputOutput "_extract" args of
      Nothing          -> error "Usage: onyx unstfs in_rb3con [outdir/]"
      Just (stfs, dir) -> extractSTFS stfs dir
    "import" : args -> case inputOutput "_import" args of
      Nothing -> error "Usage: onyx import in{_rb3con|.rba} [outdir/]"
      Just (file, dir) -> importAny NoKeys file dir
    "convert" : args -> case inputOutput "_rb3con" args of
      Nothing -> error "Usage: onyx convert in.rba [out_rb3con]"
      Just (rba, con) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny NoKeys rba dir
        target <- firstPresentTarget (dir </> "song.yml") ["rb3-2x", "rb3"]
        let planCon = "gen/target" </> T.unpack target </> "rb3con"
        shakeBuild' [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) con
    "convert-rb2" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx convert-rb2 in_rb3con [out_rb2con]"
      Just (fin, fout) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny NoKeys fin dir
        target <- firstPresentTarget (dir </> "song.yml") ["rb2-2x", "rb2"]
        let planCon = "gen/target" </> T.unpack target </> "rb2con"
        shakeBuild' [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) fout
    "convert-rb2-kg" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx convert-rb2-kg in_rb3con [out_rb2con]"
      Just (fin, fout) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny KeysGuitar fin dir
        target <- firstPresentTarget (dir </> "song.yml") ["rb2-2x", "rb2"]
        let planCon = "gen/target" </> T.unpack target </> "rb2con"
        shakeBuild' [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) fout
    "convert-rb2-kb" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx convert-rb2-kb in_rb3con [out_rb2con]"
      Just (fin, fout) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny KeysBass fin dir
        target <- firstPresentTarget (dir </> "song.yml") ["rb2-2x", "rb2"]
        let planCon = "gen/target" </> T.unpack target </> "rb2con"
        shakeBuild' [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) fout
    "reduce" : args -> case inputOutput ".reduced.mid" args of
      Nothing          -> error "Usage: onyx reduce in.mid [out.mid]"
      Just (fin, fout) -> simpleReduce fin fout
    "player" : args -> case inputOutput "_player" args of
      Nothing -> error "Usage: onyx player in{_rb3con|.rba} [outdir/]"
      Just (fin, dout) -> makePlayer fin dout
    "rpp" : args -> case inputOutput ".RPP" args of
      Nothing -> error "Usage: onyx rpp in.mid [out.RPP]"
      Just (mid, rpp) -> shake shakeOptions $ action $ makeReaper mid mid [] rpp
    "ranges" : args -> case inputOutput ".ranges.mid" args of
      Nothing          -> error "Usage: onyx ranges in.mid [out.mid]"
      Just (fin, fout) -> completeFile fin fout
    "hanging" : args -> case inputOutput ".hanging.txt" args of
      Nothing -> error "Usage: onyx hanging in.mid [out.txt]"
      Just (fin, fout) -> do
        song <- Load.fromFile fin >>= printStackTraceIO . RBFile.readMIDIFile
        writeFile fout $ closeShiftsFile song
    "reap" : args -> case args of
      [plan] -> do
        let rpp = "notes-" <> plan <> ".RPP"
        shakeBuild' [rpp] Nothing
        Dir.renameFile rpp "notes.RPP"
        case Info.os of
          "mingw32" -> void $ spawnCommand "notes.RPP"
          "darwin"  -> callProcess "open" ["notes.RPP"]
          "linux"   -> callProcess "exo-open" ["notes.RPP"]
          _         -> return ()
      _ -> error "Usage: onyx reap plan"
    ["mt", fin] -> fmap MS.toStandardMIDI (Load.fromFile fin) >>= \case
      Left  err -> error err
      Right mid -> putStr $ MS.showStandardMIDI midiTextOptions mid
    "mt" : args -> case inputOutput ".txt" args of
      Nothing -> error "Usage: onyx mt in.mid [out.txt]"
      Just (fin, fout) -> fmap MS.toStandardMIDI (Load.fromFile fin) >>= \case
        Left  err -> error err
        Right mid -> writeFile fout $ MS.showStandardMIDI midiTextOptions mid
    "tm" : args -> case inputOutput ".mid" args of
      Nothing -> error "Usage: onyx tm in.txt [out.mid]"
      Just (fin, fout) -> do
        sf <- MS.readStandardFile . MS.parse . MS.scan <$> readFile fin
        let (mid, warnings) = MS.fromStandardMIDI midiTextOptions sf
        mapM_ (hPutStrLn stderr) warnings
        Save.toFile fout mid
    _ -> error "Invalid command"

launchGUI :: IO ()
launchGUI = bracket_ SDL.initializeAll SDL.quit $ do
  let windowConf = SDL.defaultWindow { SDL.windowResizable = True, SDL.windowHighDPI = True }
  bracket (SDL.createWindow "Onyx" windowConf) SDL.destroyWindow $ \window -> do
    bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \rend -> do
      SDL.rendererDrawColor rend $= SDL.V4 255 0 0 255
      forever $ do
        SDL.clear rend
        SDL.present rend
        threadDelay 5000
        evts <- SDL.pollEvents
        forM_ evts $ \e -> case SDL.eventPayload e of
          SDL.QuitEvent -> exitSuccess
          SDL.DropEvent (SDL.DropEventData cstr) -> do
            peekCString cstr >>= putStrLn
            SDL.rendererDrawColor rend $= SDL.V4 0 0 255 255
          SDL.MouseButtonEvent SDL.MouseButtonEventData{ SDL.mouseButtonEventMotion = SDL.Pressed } -> void $ forkIO $ do
            colorChooser "Pick a window color." (255, 255, 255) >>= \case
              Nothing -> return ()
              Just (r, g, b) -> SDL.rendererDrawColor rend $= SDL.V4 r g b 255
          _ -> return ()

firstPresentTarget :: FilePath -> [T.Text] -> IO T.Text
firstPresentTarget yamlPath targets = do
  songYaml
    <-  readYAMLTree yamlPath
    >>= runReaderT (printStackTraceIO traceJSON)
  let present = HM.keys $ _targets songYaml
  case filter (`elem` present) targets of
    []    -> fail $ "panic! couldn't find any of these targets: " ++ show targets
    t : _ -> return t

inputOutput :: String -> [String] -> Maybe (FilePath, FilePath)
inputOutput suffix args = case args of
  [fin] -> let
    dropSlash = reverse . dropWhile (`elem` ("/\\" :: String)) . reverse
    in Just (fin, dropSlash fin <> suffix)
  [fin, fout] -> Just (fin, fout)
  _ -> Nothing

data Argument
  = AudioDir FilePath
  | SongFile FilePath
  -- midi<->text options:
  | MatchNotes
  | PositionSeconds
  | PositionMeasure
  | SeparateLines
  | Resolution Integer
  deriving (Eq, Ord, Show, Read)

optDescrs :: [OptDescr Argument]
optDescrs =
  [ Option [] ["audio"] (ReqArg AudioDir "DIR" ) "a directory with audio"
  , Option [] ["song" ] (ReqArg SongFile "FILE") "the song YAML file"
  , Option [] ["match-notes"] (NoArg MatchNotes) "midi to text: combine note on/off events"
  , Option [] ["seconds"] (NoArg PositionSeconds) "midi to text: position non-tempo-track events in seconds"
  , Option [] ["measure"] (NoArg PositionMeasure) "midi to text: position non-tempo-track events in measures + beats"
  , Option [] ["separate-lines"] (NoArg SeparateLines) "midi to text: give every event on its own line"
  , Option ['r'] ["resolution"] (ReqArg (Resolution . read) "int") "text to midi: how many ticks per beat"
  ]
