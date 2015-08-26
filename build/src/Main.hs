{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Config
import Audio
import Control.Monad.Trans.Reader
import StackTrace
import System.Environment (getArgs)
import YAMLTree
import System.Console.GetOpt
import qualified Sound.Jammit.Base as J
import qualified Sound.Jammit.Export as J
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import System.Directory (canonicalizePath, setCurrentDirectory)
import qualified Data.Text as T
import Data.Char (isSpace)
import Control.Monad.Extra (filterM, concatMapM, forM_, guard)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Conduit.Audio
import Data.Conduit.Audio.Sndfile
import qualified Sound.File.Sndfile as Snd
import Control.Monad.Trans.Resource
import Data.Int (Int16)
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List (nub)

data Argument
  = AudioDir  FilePath
  | JammitDir FilePath
  | SongFile  FilePath
  deriving (Eq, Ord, Show, Read)

optDescrs :: [OptDescr Argument]
optDescrs =
  [ Option ['a'] ["audio" ] (ReqArg AudioDir "DIR" ) "a directory with audio"
  , Option ['j'] ["jammit"] (ReqArg AudioDir "DIR" ) "a directory with Jammit data"
  , Option ['s'] ["song"  ] (ReqArg SongFile "FILE") "the song YAML file"
  ]

-- | Oracle for an audio file search.
-- The String is the 'show' of a value of type 'AudioFile'.
newtype AudioSearch = AudioSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for a Jammit track search.
-- The Strings are the title and artist.
newtype JammitSearch = JammitSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

allFiles :: FilePath -> Action [FilePath]
allFiles absolute = do
  entries <- getDirectoryContents absolute
  flip concatMapM entries $ \entry -> do
    let full = absolute </> entry
    isDir <- doesDirectoryExist full
    if  | entry `elem` [".", ".."] -> return []
        | isDir                    -> allFiles full
        | otherwise                -> return [full]

main :: IO ()
main = do
  argv <- getArgs
  defaultJammitDir <- J.findJammitDir
  let (opts, nonopts, _) = getOpt Permute optDescrs argv
  yamlPath <- canonicalizePath $
    fromMaybe "song.yml" $ listToMaybe [ f | SongFile f <- opts ]
  audioDirs <- mapM canonicalizePath $
    "." : takeDirectory yamlPath : [ d | AudioDir d <- opts ]
  jammitDirs <- mapM canonicalizePath $
    maybe id (:) defaultJammitDir [ d | JammitDir d <- opts ]
  songYaml
    <-  readYAMLTree yamlPath
    >>= runReaderT (printStackTraceIO traceJSON)

  let audioSearch :: AudioFile -> Action (Maybe FilePath)
      audioSearch aud = do
        files <- concatMapM allFiles audioDirs
        let md5Result = case _md5 aud of
              Nothing -> return Nothing
              Just md5search -> fmap listToMaybe $ flip filterM files $ \f -> do
                case takeExtension f of
                  ".flac" -> do
                    Stdout result <- cmd "metaflac --show-md5sum" [f]
                    return $
                      T.takeWhile (not . isSpace) (T.pack result) == md5search
                  ".wav" -> return False -- TODO: manually compute MD5 of data chunk
                  _ -> return False
        md5Result

      jammitSearch :: JammitTrack -> Action [(J.AudioPart, FilePath)]
      jammitSearch jmt = do
        let title  = fromMaybe (_title  $ _metadata songYaml) $ _jammitTitle  jmt
            artist = fromMaybe (_artist $ _metadata songYaml) $ _jammitArtist jmt
        lib <- liftIO $ concatMapM J.loadLibrary jammitDirs
        return $ J.getAudioParts
          $ J.exactSearchBy J.title  (T.unpack title )
          $ J.exactSearchBy J.artist (T.unpack artist) lib

  setCurrentDirectory $ takeDirectory yamlPath
  shake shakeOptions $ do

    _ <- addOracle $ \(AudioSearch  s) -> audioSearch $ read s
    _ <- addOracle $ \(JammitSearch s) -> fmap show $ jammitSearch $ read s

    phony "yaml"   $ liftIO $ print songYaml
    phony "audio"  $ liftIO $ print audioDirs
    phony "jammit" $ liftIO $ print jammitDirs
    phony "clean"  $ cmd "rm -rf gen"

    let audioPath :: T.Text -> FilePath
        audioPath name = "gen/audio" </> T.unpack name <.> "wav"
        jammitPath :: T.Text -> J.AudioPart -> FilePath
        jammitPath name (J.Only part)
          = "gen/jammit" </> T.unpack name </> "only" </> show part <.> "wav"
        jammitPath name (J.Without inst)
          = "gen/jammit" </> T.unpack name </> "without" </> show inst <.> "wav"

    -- Find and convert all audio files into the work directory
    forM_ (HM.toList $ _audio songYaml) $ \(audioName, audioQuery) -> do
      audioPath audioName %> \out -> do
        result <- askOracle $ AudioSearch $ show audioQuery
        case result of
          Nothing -> fail "Couldn't find a necessary audio file"
          Just fp -> do
            let wavfmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
            src <- liftIO $ sourceSnd fp
            let _ = src :: AudioSource (ResourceT IO) Int16
            liftIO $ runResourceT $ sinkSnd out wavfmt src

    -- Find and convert all Jammit audio into the work directory
    let jammitAudioParts = map J.Only    [minBound .. maxBound]
                        ++ map J.Without [minBound .. maxBound]
    forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) -> do
      forM_ jammitAudioParts $ \audpart -> do
        jammitPath jammitName audpart %> \out -> do
          result <- fmap read $ askOracle $ JammitSearch $ show jammitQuery
          case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
            jcfx : _ -> liftIO $ J.runAudio [jcfx] [] out
            []       -> fail "Couldn't find a necessary Jammit track"

    -- Looking up single audio files and Jammit parts in the work directory
    let manualLeaf :: AudioInput -> Action (Audio Duration FilePath)
        manualLeaf (Named name) = case HM.lookup name $ _audio songYaml of
          Just audioQuery -> return $ let
            maybeResample = case _rate audioQuery of
              Nothing -> Resample
              Just _  -> id -- if rate is specified, don't auto-resample
            in maybeResample $ Input $ audioPath name
          Nothing -> fail $ "Couldn't find an audio file named " ++ show name
        manualLeaf (JammitSelect audpart name) = case HM.lookup name $ _jammit songYaml of
          Just _  -> return $ Input $ jammitPath name audpart
          Nothing -> fail $ "Couldn't find a Jammit file named " ++ show name

    -- The "auto" mode of Jammit audio assignment, using EachPlan
    let autoLeaf :: Maybe J.Instrument -> T.Text -> Action (Audio Duration FilePath)
        autoLeaf minst name = do
          case HM.lookup name $ _audio songYaml of
            Just _ -> return $ Input $ audioPath name
            Nothing -> case HM.lookup name $ _jammit songYaml of
              Nothing -> fail $ "Couldn't find a Jammit file named " ++ show name
              Just jmtQuery -> do
                result <- fmap read $ askOracle $ JammitSearch $ show jmtQuery
                let _ = result :: [(J.AudioPart, FilePath)]
                let backs = concat
                      [ [J.Drums    | _hasDrums  $ _instruments songYaml]
                      , [J.Bass     | _hasBass   $ _instruments songYaml]
                      , [J.Guitar   | _hasGuitar $ _instruments songYaml]
                      , [J.Keyboard | _hasKeys   $ _instruments songYaml]
                      , [J.Vocal    | _hasVocal   (_instruments songYaml) /= Vocal0]
                      ]
                    -- audio that is used in the song and bought by the user
                    boughtInstrumentParts :: J.Instrument -> [FilePath]
                    boughtInstrumentParts inst = do
                      guard $ inst `elem` backs
                      J.Only part <- nub $ map fst result
                      guard $ J.partToInstrument part == inst
                      return $ jammitPath name $ J.Only part
                    mixOrStereo []    = Silence 2 $ Seconds 1
                    mixOrStereo files = Mix $ map Input files
                case minst of
                  Just inst -> return $ mixOrStereo $ boughtInstrumentParts inst
                  Nothing -> case filter (\inst -> J.Without inst `elem` map fst result) backs of
                    []       -> fail "No charted instruments with Jammit tracks found"
                    back : _ -> return $ let
                      negative = mixOrStereo $ do
                        otherInstrument <- filter (/= back) backs
                        boughtInstrumentParts otherInstrument
                      in Mix [Input $ jammitPath name $ J.Without back, Gain (-1) negative]

    forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> case plan of
      Plan{..} -> return ()
      EachPlan{..} -> return ()

    want nonopts
