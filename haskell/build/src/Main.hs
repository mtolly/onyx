{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RecordWildCards            #-}
module Main (main) where

import           Audio
import qualified C3
import           Config                           hiding (Difficulty)
import           Genre                            (genreDisplay, magmaV1Genre)
import           Image
import           Magma                            hiding
                                                   (withSystemTempDirectory)
import qualified Magma
import           MoggDecrypt
import           OneFoot
import qualified OnyxiteDisplay.Process           as Proc
import           PrettyDTA
import           Reaper.Base                      (writeRPP)
import qualified Reaper.Build                     as RPP
import           Reductions
import           Resources                        (emptyMilo, webDisplay, emptyMiloRB2, emptyWeightsRB2)
import           Scripts
import           STFS.Extract
import           X360
import           YAMLTree
import           ProKeysRanges
import           Import
import           Difficulty
import qualified FretsOnFire                      as FoF
import qualified Sound.MIDI.Script.Base           as MS
import qualified Sound.MIDI.Script.Parse          as MS
import qualified Sound.MIDI.Script.Read           as MS
import qualified Sound.MIDI.Script.Scan           as MS
import qualified MelodysEscape
import qualified RockBand2                        as RB2

import           Codec.Picture
import           Control.Exception as Exc
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                       as A
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower, isSpace)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Sndfile
import qualified Data.Digest.Pure.MD5             as MD5
import qualified Data.DTA                         as D
import qualified Data.DTA.Serialize               as D
import qualified Data.DTA.Serialize.Magma         as Magma
import qualified Data.DTA.Serialize.RB3           as D
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Milli, Centi)
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (runIdentity)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (intercalate, isPrefixOf, nub, stripPrefix)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing,
                                                   listToMaybe, mapMaybe)
import           Data.Monoid                      ((<>))
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Development.Shake                hiding (phony, (%>), (&%>))
import qualified Development.Shake                as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import qualified Numeric.NonNegative.Class        as NNC
import           RockBand.Common                  (Difficulty (..), readCommand', showCommand')
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as RBFive
import           RockBand.Parse                   (unparseCommand, unparseBlip)
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as RBVox
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U
import           System.Console.GetOpt
import qualified System.Directory                 as Dir
import           System.Environment               (getArgs)
import           System.Environment.Executable    (getExecutablePath)
import           System.IO                        (hPutStrLn, stderr, withBinaryFile, IOMode(ReadMode), hFileSize)
import           System.IO.Temp                   (withSystemTempDirectory)
import qualified System.Info                      as Info
import           System.Process                   (callProcess)

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

-- | Oracle for an audio file search.
-- The String is the 'show' of a value of type 'AudioFile'.
newtype AudioSearch = AudioSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for a Jammit track search.
-- The String is the 'show' of a value of type 'JammitTrack'.
newtype JammitSearch = JammitSearch String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

-- | Oracle for an existing MOGG file search.
-- The Text is an MD5 hash of the complete MOGG file.
newtype MoggSearch = MoggSearch T.Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype GetSongYaml = GetSongYaml ()
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype CompileTime = CompileTime ()
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

computeChannels :: Audio Duration Int -> Int
computeChannels = \case
  Silence n _ -> n
  Input n -> n
  Mix auds -> foldr max 0 $ map computeChannels auds
  Merge auds -> sum $ map computeChannels auds
  Concatenate auds -> foldr max 0 $ map computeChannels auds
  Gain _ aud -> computeChannels aud
  Take _ _ aud -> computeChannels aud
  Drop _ _ aud -> computeChannels aud
  Fade _ _ aud -> computeChannels aud
  Pad _ _ aud -> computeChannels aud
  Resample aud -> computeChannels aud
  Channels chans _ -> length chans

audioSearch :: AudioFile -> [FilePath] -> Action (Maybe FilePath)
audioSearch AudioSnippet{} _     = fail "panic! called audioSearch on a snippet. report this bug"
audioSearch AudioFile{..}  files = do
  files1 <- case _filePath of
    Nothing   -> return files
    Just path -> do
      need [path]
      liftIO $ fmap (:[]) $ Dir.canonicalizePath path
  files2 <- liftIO $ case _md5 of
    Nothing  -> return files1
    Just md5 -> fmap toList $ findM (fmap (== Just (T.unpack md5)) . audioMD5) files1
  files3 <- liftIO $ case _frames of
    Nothing  -> return files2
    Just len -> filterM (fmap (== Just len) . audioLength) files2
  files4 <- if isNothing _filePath && isNothing _md5
    then fail "audioSearch: you must specify either file-path or md5"
    else return files3
  files5 <- liftIO $ filterM (fmap (== Just _channels) . audioChannels) files4
  files6 <- liftIO $ case _rate of
    Nothing   -> return files5
    Just rate -> filterM (fmap (== Just rate) . audioRate) files5
  need files6
  return $ listToMaybe files6

moggSearch :: T.Text -> [FilePath] -> Action (Maybe FilePath)
moggSearch md5search files = do
  flip findM files $ \f -> case takeExtension f of
    ".mogg" -> do
      md5 <- liftIO $ show . MD5.md5 <$> BL.readFile f
      return $ T.unpack md5search == md5
    _ -> return False

main :: IO ()
main = do
  argv <- getArgs
  defaultJammitDir <- J.findJammitDir
  let
    (opts, nonopts, _) = getOpt Permute optDescrs argv
    shakeBuild buildables yml = do

      yamlPath <- Dir.canonicalizePath $ case yml of
        Just y  -> y
        Nothing -> fromMaybe "song.yml" $ listToMaybe [ f | SongFile f <- opts ]
      audioDirs <- mapM Dir.canonicalizePath
        $ takeDirectory yamlPath
        : maybe id (:) defaultJammitDir [ d | AudioDir d <- opts ]
      songYaml
        <-  readYAMLTree yamlPath
        >>= runReaderT (printStackTraceIO traceJSON)

      -- make sure all audio leaves are defined, catch typos
      let definedLeaves = HM.keys (_audio songYaml) ++ HM.keys (_jammit songYaml)
      forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
        let leaves = case plan of
              EachPlan{..} -> toList _each
              MoggPlan{} -> []
              Plan{..} -> let
                getLeaves = \case
                  Named t -> t
                  JammitSelect _ t -> t
                in map getLeaves
                  $ concatMap (maybe [] toList) [_song, _guitar, _bass, _keys, _drums, _vocal, _crowd]
                  ++ case _countin of Countin xs -> concatMap (toList . snd) xs
        case filter (not . (`elem` definedLeaves)) leaves of
          [] -> return ()
          undefinedLeaves -> fail $
            "Undefined leaves in plan " ++ show planName ++ " audio expression: " ++ show undefinedLeaves

      let computeChannelsPlan :: Audio Duration AudioInput -> Int
          computeChannelsPlan = let
            toChannels ai = case ai of
              Named name -> case HM.lookup name $ _audio songYaml of
                Nothing               -> error
                  "panic! audio leaf not found, after it should've been checked"
                Just AudioFile   {..} -> _channels
                Just AudioSnippet{..} -> computeChannelsPlan _expr
              JammitSelect _ _ -> 2
            in computeChannels . fmap toChannels

          computeChannelsEachPlan :: Audio Duration T.Text -> Int
          computeChannelsEachPlan = let
            toChannels name = case HM.lookup name $ _jammit songYaml of
              Just _ -> 2
              Nothing -> case HM.lookup name $ _audio songYaml of
                Nothing      -> error "panic! audio leaf not found, after it should've been checked"
                Just AudioFile{..} -> _channels
                Just AudioSnippet{..} -> computeChannelsPlan _expr
            in computeChannels . fmap toChannels

          jammitSearch :: JammitTrack -> J.Library -> Action [(J.AudioPart, FilePath)]
          jammitSearch jmt lib = do
            let title  = fromMaybe (_title  $ _metadata songYaml) $ _jammitTitle  jmt
                artist = fromMaybe (_artist $ _metadata songYaml) $ _jammitArtist jmt
            return $ J.getAudioParts
              $ J.exactSearchBy J.title  (T.unpack title )
              $ J.exactSearchBy J.artist (T.unpack artist) lib

      origDirectory <- Dir.getCurrentDirectory
      Dir.setCurrentDirectory $ takeDirectory yamlPath
      shakeArgsWith shakeOptions{ shakeThreads = 0, shakeFiles = "gen" } (map (fmap $ const (Right ())) optDescrs) $ \_ _ -> return $ Just $ do

        allFilesInAudioDirs <- newCache $ \() -> do
          genAbsolute <- liftIO $ Dir.canonicalizePath "gen/"
          filter (\f -> not $ genAbsolute `isPrefixOf` f)
            <$> concatMapM allFiles audioDirs
        allJammitInAudioDirs <- newCache $ \() -> liftIO $ concatMapM J.loadLibrary audioDirs

        audioOracle  <- addOracle $ \(AudioSearch  s) -> allFilesInAudioDirs () >>= audioSearch (read s)
        jammitOracle <- addOracle $ \(JammitSearch s) -> fmap show $ allJammitInAudioDirs () >>= jammitSearch (read s)
        moggOracle   <- addOracle $ \(MoggSearch   s) -> allFilesInAudioDirs () >>= moggSearch s

        -- Make all rules depend on the parsed song.yml contents and onyx compile time
        strSongYaml    <- addOracle $ \(GetSongYaml ()) -> return $ show songYaml
        ctime'         <- newCache $ \(CompileTime ()) -> liftIO $ fmap show $ getExecutablePath >>= Dir.getModificationTime
        ctime          <- addOracle ctime'
        let onyxDeps act = strSongYaml (GetSongYaml ()) >> ctime (CompileTime ()) >> act
            (%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
            pat %> f = pat Shake.%> onyxDeps . f
            (&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
            pats &%> f = pats Shake.&%> onyxDeps . f
            phony :: String -> Action () -> Rules ()
            phony s f = do
              Shake.phony s $ onyxDeps f
              Shake.phony (s ++ "/") $ onyxDeps f
            infix 1 %>, &%>

        forM_ (HM.elems $ _audio songYaml) $ \case
          AudioFile{ _filePath = Just fp, _commands = cmds } | not $ null cmds -> do
            fp %> \_ -> mapM_ (Shake.unit . Shake.cmd) cmds
          _ -> return ()

        phony "yaml"  $ liftIO $ print songYaml
        phony "audio" $ liftIO $ print audioDirs
        phony "clean" $ cmd "rm -rf gen"

        let jammitPath :: T.Text -> J.AudioPart -> FilePath
            jammitPath name (J.Only part)
              = "gen/jammit" </> T.unpack name </> "only" </> map toLower (drop 4 $ show part) <.> "wav"
            jammitPath name (J.Without inst)
              = "gen/jammit" </> T.unpack name </> "without" </> map toLower (show inst) <.> "wav"

        let getRank has diff dmap = if has $ _instruments songYaml
              then case diff $ _difficulty $ _metadata songYaml of
                Nothing       -> 1
                Just (Rank r) -> r
                Just (Tier t) -> tierToRank dmap t
              else 0

            drumsRank   = getRank _hasDrums    _difficultyDrums   drumsDiffMap
            bassRank    = getRank _hasBass     _difficultyBass    bassDiffMap
            guitarRank  = getRank _hasGuitar   _difficultyGuitar  guitarDiffMap
            vocalRank   = getRank hasAnyVocal  _difficultyVocal   vocalDiffMap
            keysRank    = getRank hasAnyKeys   _difficultyKeys    keysDiffMap
            proKeysRank = getRank hasAnyKeys   _difficultyProKeys keysDiffMap
            bandRank    = getRank (const True) _difficultyBand    bandDiffMap

            drumsTier   = rankToTier drumsDiffMap  drumsRank
            bassTier    = rankToTier bassDiffMap   bassRank
            guitarTier  = rankToTier guitarDiffMap guitarRank
            vocalTier   = rankToTier vocalDiffMap  vocalRank
            keysTier    = rankToTier keysDiffMap   keysRank
            proKeysTier = rankToTier keysDiffMap   proKeysRank
            bandTier    = rankToTier bandDiffMap   bandRank

        -- Looking up single audio files and Jammit parts in the work directory
        let manualLeaf :: AudioInput -> Action (Audio Duration FilePath)
            manualLeaf (Named name) = case HM.lookup name $ _audio songYaml of
              Just audioQuery -> case audioQuery of
                AudioFile{..} -> do
                  putNormal $ "Looking for the audio file named " ++ show name
                  result <- audioOracle $ AudioSearch $ show audioQuery
                  case result of
                    Nothing -> fail $ "Couldn't find a necessary audio file for query: " ++ show audioQuery
                    Just fp -> do
                      putNormal $ "Found " ++ show name ++ " located at: " ++ fp
                      return $ case _rate of
                        Nothing -> Resample $ Input fp
                        Just _  -> Input fp -- if rate is specified, don't auto-resample
                AudioSnippet expr -> fmap join $ mapM manualLeaf expr
              Nothing -> fail $ "Couldn't find an audio source named " ++ show name
            manualLeaf (JammitSelect audpart name) = case HM.lookup name $ _jammit songYaml of
              Just _  -> return $ Input $ jammitPath name audpart
              Nothing -> fail $ "Couldn't find a Jammit source named " ++ show name

        -- The "auto" mode of Jammit audio assignment, using EachPlan
        let autoLeaf :: Maybe J.Instrument -> T.Text -> Action (Audio Duration FilePath)
            autoLeaf minst name = case HM.lookup name $ _jammit songYaml of
              Nothing -> manualLeaf $ Named name
              Just jmtQuery -> do
                result <- fmap read $ jammitOracle $ JammitSearch $ show jmtQuery
                let _ = result :: [(J.AudioPart, FilePath)]
                let backs = concat
                      [ [J.Drums    | _hasDrums   $ _instruments songYaml]
                      , [J.Bass     | _hasBass    $ _instruments songYaml]
                      , [J.Guitar   | _hasGuitar  $ _instruments songYaml]
                      , [J.Keyboard | hasAnyKeys  $ _instruments songYaml]
                      , [J.Vocal    | hasAnyVocal $ _instruments songYaml]
                      ]
                    -- audio that is used in the song and bought by the user
                    boughtInstrumentParts :: J.Instrument -> [FilePath]
                    boughtInstrumentParts inst = do
                      guard $ inst `elem` backs
                      J.Only part <- nub $ map fst result
                      guard $ J.partToInstrument part == inst
                      return $ jammitPath name $ J.Only part
                    mixOrStereo []    = Silence 2 $ Frames 0
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

        -- Find and convert all Jammit audio into the work directory
        let jammitAudioParts = map J.Only    [minBound .. maxBound]
                            ++ map J.Without [minBound .. maxBound]
        forM_ (HM.toList $ _jammit songYaml) $ \(jammitName, jammitQuery) ->
          forM_ jammitAudioParts $ \audpart ->
            jammitPath jammitName audpart %> \out -> do
              putNormal $ "Looking for the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
              result <- fmap read $ jammitOracle $ JammitSearch $ show jammitQuery
              case [ jcfx | (audpart', jcfx) <- result, audpart == audpart' ] of
                jcfx : _ -> do
                  putNormal $ "Found the Jammit track named " ++ show jammitName ++ ", part " ++ show audpart
                  liftIO $ J.runAudio [jcfx] [] out
                []       -> fail "Couldn't find a necessary Jammit track"

        -- Cover art
        let loadRGB8 = case _fileAlbumArt $ _metadata songYaml of
              Just img -> do
                need [img]
                liftIO $ if takeExtension img == ".png_xbox"
                  then readPNGXbox <$> BL.readFile img
                  else readImage img >>= \case
                    Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
                    Right dyn -> return $ convertRGB8 dyn
              Nothing -> return $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
        "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleBilinear 256 256
        "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleBilinear 256 256
        "gen/cover.dds" %> \out -> loadRGB8 >>= liftIO . writeDDS    out . scaleBilinear 256 256
        "gen/cover.png_xbox" %> \out -> case _fileAlbumArt $ _metadata songYaml of
          Just f | takeExtension f == ".png_xbox" -> copyFile' f out
          _ -> do
            let dds = out -<.> "dds"
            need [dds]
            liftIO $ do
              b <- B.readFile dds
              B.writeFile out $ pngXboxDXT1Signature <> flipWord16sStrict (B.drop 0x80 b)

        -- The Markdown README file, for GitHub purposes
        "README.md" %> \out -> liftIO $ writeFile out $ execWriter $ do
          let escape = concatMap $ \c -> if c `elem` "\\`*_{}[]()#+-.!"
                then ['\\', c]
                else [c]
              line str = tell $ str ++ "\n"
          line $ "# " ++ escape (T.unpack $ _title $ _metadata songYaml)
          line ""
          line $ "## " ++ escape (T.unpack $ _artist $ _metadata songYaml)
          line ""
          case T.unpack $ _author $ _metadata songYaml of
            "Onyxite" -> return ()
            auth      -> line $ "Author: " ++ auth
          line ""
          let titleDir  = takeFileName $ takeDirectory yamlPath
              artistDir = takeFileName $ takeDirectory $ takeDirectory yamlPath
              link = "http://pages.cs.wisc.edu/~tolly/customs/?title=" ++ titleDir ++ "&artist=" ++ artistDir
          when (HM.member (T.pack "album") $ _plans songYaml) $ line $ "[Play in browser](" ++ link ++ ")"
          line ""
          line "Instruments:"
          line ""
          let diffString f dm = case f $ _difficulty $ _metadata songYaml of
                Just (Rank rank) -> g $ rankToTier dm rank
                Just (Tier tier) -> g tier
                Nothing -> ""
                where g = \case
                        1 -> " ⚫️⚫️⚫️⚫️⚫️"
                        2 -> " ⚪️⚫️⚫️⚫️⚫️"
                        3 -> " ⚪️⚪️⚫️⚫️⚫️"
                        4 -> " ⚪️⚪️⚪️⚫️⚫️"
                        5 -> " ⚪️⚪️⚪️⚪️⚫️"
                        6 -> " ⚪️⚪️⚪️⚪️⚪️"
                        7 -> " 😈😈😈😈😈"
                        _ -> ""
          when (_hasDrums   $ _instruments songYaml) $ line $ "  * (Pro) Drums" ++ diffString _difficultyDrums   drumsDiffMap
          when (_hasBass    $ _instruments songYaml) $ line $ "  * Bass"        ++ diffString _difficultyBass    bassDiffMap
          when (_hasGuitar  $ _instruments songYaml) $ line $ "  * Guitar"      ++ diffString _difficultyGuitar  guitarDiffMap
          when (_hasKeys    $ _instruments songYaml) $ line $ "  * Keys"        ++ diffString _difficultyKeys    keysDiffMap
          when (_hasProKeys $ _instruments songYaml) $ line $ "  * Pro Keys"    ++ diffString _difficultyProKeys keysDiffMap
          case _hasVocal $ _instruments songYaml of
            Vocal0 -> return ()
            Vocal1 -> line $ "  * Vocals (1)" ++ diffString _difficultyVocal vocalDiffMap
            Vocal2 -> line $ "  * Vocals (2)" ++ diffString _difficultyVocal vocalDiffMap
            Vocal3 -> line $ "  * Vocals (3)" ++ diffString _difficultyVocal vocalDiffMap
          line ""
          line "Supported audio:"
          line ""
          forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do
            line $ "  * `" ++ T.unpack planName ++ "`" ++ if planName == T.pack "album"
              then " (" ++ escape (T.unpack $ _album $ _metadata songYaml) ++ ")"
              else ""
            line ""
            forM_ (_planComments plan) $ \cmt -> do
              line $ "    * " ++ T.unpack cmt
              line ""
          unless (null $ _comments $ _metadata songYaml) $ do
            line "Notes:"
            line ""
            forM_ (_comments $ _metadata songYaml) $ \cmt -> do
              line $ "  * " ++ T.unpack cmt
              line ""

        forM_ (HM.toList $ _plans songYaml) $ \(planName, plan) -> do

          let dir = "gen/plan" </> T.unpack planName

              planPV :: Maybe (PlanAudio Duration AudioInput) -> [(Double, Double)]
              planPV Nothing = [(-1, 0), (1, 0)]
              planPV (Just paud) = let
                chans = computeChannelsPlan $ _planExpr paud
                pans = case _planPans paud of
                  [] -> case chans of
                    0 -> []
                    1 -> [0]
                    2 -> [-1, 1]
                    c -> error $ "Error: I don't know what pans to use for " ++ show c ++ "-channel audio"
                  ps -> ps
                vols = case _planVols paud of
                  [] -> replicate chans 0
                  vs -> vs
                in zip pans vols
              eachPlanPV :: PlanAudio Duration T.Text -> [(Double, Double)]
              eachPlanPV paud = let
                chans = computeChannelsEachPlan $ _planExpr paud
                pans = case _planPans paud of
                  [] -> case chans of
                    0 -> []
                    1 -> [0]
                    2 -> [-1, 1]
                    c -> error $ "Error: I don't know what pans to use for " ++ show c ++ "-channel audio"
                  ps -> ps
                vols = case _planVols paud of
                  [] -> replicate chans 0
                  vs -> vs
                in zip pans vols
              bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, crowdPV, songPV :: [(Double, Double)]
              mixMode :: RBDrums.Audio
              bassPV = guard (_hasBass $ _instruments songYaml) >> case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggBass
                Plan{..} -> planPV _bass
                EachPlan{..} -> eachPlanPV _each
              guitarPV = guard (_hasGuitar $ _instruments songYaml) >> case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggGuitar
                Plan{..} -> planPV _guitar
                EachPlan{..} -> eachPlanPV _each
              keysPV = guard (hasAnyKeys $ _instruments songYaml) >> case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggKeys
                Plan{..} -> planPV _keys
                EachPlan{..} -> eachPlanPV _each
              vocalPV = guard (hasAnyVocal $ _instruments songYaml) >> case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggVocal
                Plan{..} -> planPV _vocal
                EachPlan{..} -> eachPlanPV _each
              crowdPV = case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) _moggCrowd
                Plan{..} -> guard (isJust _crowd) >> planPV _crowd
                EachPlan{..} -> []
              (kickPV, snarePV, drumsPV, mixMode) = if _hasDrums $ _instruments songYaml
                then case plan of
                  MoggPlan{..} -> let
                    getChannel i = (_pans !! i, _vols !! i)
                    kickChannels = case _drumMix of
                      RBDrums.D0 -> []
                      RBDrums.D1 -> take 1 _moggDrums
                      RBDrums.D2 -> take 1 _moggDrums
                      RBDrums.D3 -> take 2 _moggDrums
                      RBDrums.D4 -> take 1 _moggDrums
                    snareChannels = case _drumMix of
                      RBDrums.D0 -> []
                      RBDrums.D1 -> take 1 $ drop 1 _moggDrums
                      RBDrums.D2 -> take 2 $ drop 1 _moggDrums
                      RBDrums.D3 -> take 2 $ drop 2 _moggDrums
                      RBDrums.D4 -> []
                    drumsChannels = case _drumMix of
                      RBDrums.D0 -> _moggDrums
                      RBDrums.D1 -> drop 2 _moggDrums
                      RBDrums.D2 -> drop 3 _moggDrums
                      RBDrums.D3 -> drop 4 _moggDrums
                      RBDrums.D4 -> drop 1 _moggDrums
                    in (map getChannel kickChannels, map getChannel snareChannels, map getChannel drumsChannels, _drumMix)
                  Plan{..} -> let
                    count = maybe 0 (computeChannelsPlan . _planExpr)
                    matchingMix = case (count _kick, count _snare) of
                      (0, 0) -> RBDrums.D0
                      (1, 1) -> RBDrums.D1
                      (1, 2) -> RBDrums.D2
                      (2, 2) -> RBDrums.D3
                      (1, 0) -> RBDrums.D4
                      (k, s) -> error $ "No matching drum mix mode for (kick,snare) == " ++ show (k, s)
                    in  ( guard (matchingMix /= RBDrums.D0) >> planPV _kick
                        , guard (matchingMix `elem` [RBDrums.D1, RBDrums.D2, RBDrums.D3]) >> planPV _snare
                        , planPV _drums
                        , matchingMix
                        )
                  EachPlan{..} -> ([], [], eachPlanPV _each, RBDrums.D0)
                else ([], [], [], RBDrums.D0)
              songPV = case plan of
                MoggPlan{..} -> map (\i -> (_pans !! i, _vols !! i)) $ let
                  notSong = concat [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal, _moggCrowd]
                  in filter (`notElem` notSong) [0 .. length _pans - 1]
                Plan{..} -> planPV _song
                EachPlan{..} -> eachPlanPV _each

          -- REAPER project
          "notes-" ++ T.unpack planName ++ ".RPP" %> \out -> do
            let audios = map (\x -> "gen/plan" </> T.unpack planName </> x <.> "wav")
                  $ ["guitar", "bass", "drums", "kick", "snare", "keys", "vocal", "crowd"] ++ case plan of
                    MoggPlan{} -> ["song-countin"]
                    _          -> ["song"]
                    -- Previously this relied on countin,
                    -- but it's better to not have to generate gen/plan/foo/xp/notes.mid
                extraTempo = "tempo-" ++ T.unpack planName ++ ".mid"
            b <- doesFileExist extraTempo
            let tempo = if b then extraTempo else "notes.mid"
            makeReaper "notes.mid" tempo audios out

          -- Audio files
          case plan of
            Plan{..} -> do
              let locate :: Audio Duration AudioInput -> Action (Audio Duration FilePath)
                  locate = fmap join . mapM manualLeaf
                  buildPart planPart fout = let
                    expr = maybe (Silence 2 $ Frames 0) _planExpr planPart
                    in locate expr >>= \aud -> buildAudio aud fout
              dir </> "song.wav"   %> buildPart _song
              dir </> "guitar.wav" %> buildPart _guitar
              dir </> "bass.wav"   %> buildPart _bass
              dir </> "keys.wav"   %> buildPart _keys
              dir </> "kick.wav"   %> buildPart _kick
              dir </> "snare.wav"  %> buildPart _snare
              dir </> "drums.wav"  %> buildPart _drums
              dir </> "vocal.wav"  %> buildPart _vocal
              dir </> "crowd.wav"  %> buildPart _crowd
            EachPlan{..} -> do
              dir </> "kick.wav"   %> buildAudio (Silence 1 $ Frames 0)
              dir </> "snare.wav"  %> buildAudio (Silence 1 $ Frames 0)
              dir </> "crowd.wav"  %> buildAudio (Silence 1 $ Frames 0)
              let locate :: Maybe J.Instrument -> Action (Audio Duration FilePath)
                  locate inst = fmap join $ mapM (autoLeaf inst) $ _planExpr _each
                  buildPart maybeInst fout = locate maybeInst >>= \aud -> buildAudio aud fout
              forM_ (Nothing : map Just [minBound .. maxBound]) $ \maybeInst -> let
                planAudioPath :: Maybe Instrument -> FilePath
                planAudioPath (Just inst) = dir </> map toLower (show inst) <.> "wav"
                planAudioPath Nothing     = dir </> "song.wav"
                in planAudioPath maybeInst %> buildPart (fmap jammitInstrument maybeInst)
            MoggPlan{..} -> do
              let oggChannels []    = buildAudio $ Silence 2 $ Frames 0
                  oggChannels chans = buildAudio $ Channels chans $ Input $ dir </> "audio.ogg"
              dir </> "guitar.wav" %> oggChannels _moggGuitar
              dir </> "bass.wav" %> oggChannels _moggBass
              dir </> "keys.wav" %> oggChannels _moggKeys
              dir </> "vocal.wav" %> oggChannels _moggVocal
              dir </> "crowd.wav" %> oggChannels _moggCrowd
              dir </> "kick.wav" %> do
                oggChannels $ case mixMode of
                  RBDrums.D0 -> []
                  RBDrums.D1 -> take 1 _moggDrums
                  RBDrums.D2 -> take 1 _moggDrums
                  RBDrums.D3 -> take 2 _moggDrums
                  RBDrums.D4 -> take 1 _moggDrums
              dir </> "snare.wav" %> do
                oggChannels $ case mixMode of
                  RBDrums.D0 -> []
                  RBDrums.D1 -> take 1 $ drop 1 _moggDrums
                  RBDrums.D2 -> take 2 $ drop 1 _moggDrums
                  RBDrums.D3 -> take 2 $ drop 2 _moggDrums
                  RBDrums.D4 -> []
              dir </> "drums.wav" %> do
                oggChannels $ case mixMode of
                  RBDrums.D0 -> _moggDrums
                  RBDrums.D1 -> drop 2 _moggDrums
                  RBDrums.D2 -> drop 3 _moggDrums
                  RBDrums.D3 -> drop 4 _moggDrums
                  RBDrums.D4 -> drop 1 _moggDrums
              dir </> "song-countin.wav" %> \out -> do
                need [dir </> "audio.ogg"]
                chanCount <- liftIO $ Snd.channels <$> Snd.getFileInfo (dir </> "audio.ogg")
                let songChannels = do
                      i <- [0 .. chanCount - 1]
                      guard $ notElem i $ concat
                        [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal, _moggCrowd]
                      return i
                oggChannels songChannels out

          dir </> "web/song.js" %> \out -> do
            let json = dir </> "display.json"
            s <- readFile' json
            let s' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
                js = "window.onyxSong = " ++ s' ++ ";\n"
            liftIO $ writeFile out js
          phony (dir </> "web") $ do
            liftIO $ forM_ webDisplay $ \(f, bs) -> do
              Dir.createDirectoryIfMissing True $ dir </> "web" </> takeDirectory f
              B.writeFile (dir </> "web" </> f) bs
            need
              [ dir </> "web/preview-audio.mp3"
              , dir </> "web/preview-audio.ogg"
              , dir </> "web/song.js"
              ]

          let allAudioWithPV =
                [ (kickPV, dir </> "kick.wav")
                , (snarePV, dir </> "snare.wav")
                , (drumsPV, dir </> "drums.wav")
                , (guitarPV, dir </> "guitar.wav")
                , (bassPV, dir </> "bass.wav")
                , (keysPV, dir </> "keys.wav")
                , (vocalPV, dir </> "vocal.wav")
                , (crowdPV, dir </> "crowd.wav")
                , (songPV, dir </> "song-countin.wav")
                ]

          dir </> "everything.wav" %> \out -> case plan of
            MoggPlan{..} -> do
              let ogg = dir </> "audio.ogg"
              need [ogg]
              src <- liftIO $ sourceSnd ogg
              runAudio (applyPansVols (map realToFrac _pans) (map realToFrac _vols) src) out
            _ -> do
              need $ map snd allAudioWithPV
              srcs <- fmap concat $ forM allAudioWithPV $ \(pv, wav) -> case pv of
                  [] -> return []
                  _  -> do
                    src <- liftIO $ sourceSnd wav
                    return [applyPansVols (map (realToFrac . fst) pv) (map (realToFrac . snd) pv) src]
              let mixed = case srcs of
                    []     -> silent (Frames 0) 44100 2
                    s : ss -> foldr mix s ss
              runAudio mixed out
          dir </> "everything.ogg" %> buildAudio (Input $ dir </> "everything.wav")

          dir </> "everything-mono.wav" %> \out -> case plan of
            MoggPlan{..} -> do
              let ogg = dir </> "audio.ogg"
              need [ogg]
              src <- liftIO $ sourceSnd ogg
              runAudio (applyVolsMono (map realToFrac _vols) src) out
            _ -> do
              need $ map snd allAudioWithPV
              srcs <- fmap concat $ forM allAudioWithPV $ \(pv, wav) -> case pv of
                  [] -> return []
                  _  -> do
                    src <- liftIO $ sourceSnd wav
                    return [applyVolsMono (map (realToFrac . snd) pv) src]
              let mixed = case srcs of
                    []     -> silent (Frames 0) 44100 1
                    s : ss -> foldr mix s ss
              runAudio mixed out

          -- MIDI files
          let midPS = dir </> "ps/notes.mid"
              mid2p = dir </> "2p/notes.mid"
              mid1p = dir </> "1p/notes.mid"
              midraw = dir </> "raw.mid"
              has2p = dir </> "has2p.txt"
              display = dir </> "display.json"
          [midPS, midraw, mid2p, mid1p, has2p] &%> \_ -> do
            putNormal "Loading the MIDI file..."
            input <- loadMIDI "notes.mid"
            let extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
                showPosition = RBFile.showPosition . U.applyMeasureMap (RBFile.s_signatures input)
            tempos <- fmap RBFile.s_tempos $ doesFileExist extraTempo >>= \b -> if b
              then loadMIDI extraTempo
              else return input
            let trks = RBFile.s_tracks input
                mergeTracks = foldr RTB.merge RTB.empty
                eventsRaw = mergeTracks [ t | RBFile.Events t <- trks ]
                eventsList = ATB.toPairList $ RTB.toAbsoluteEventList 0 eventsRaw
            -- If [music_start] is before 2 beats,
            -- Magma will add auto [idle] events there in instrument tracks, and then error...
            musicStartPosn <- case [ t | (t, Events.MusicStart) <- eventsList ] of
              t : _ -> if t < 2
                then do
                  putNormal $ "[music_start] is too early. Moving to " ++ showPosition 2
                  return 2
                else return t
              []    -> do
                putNormal $ "[music_start] is missing. Placing at " ++ showPosition 2
                return 2
            -- A better default end position would take into account the audio length.
            -- But, it's nice to not have notes.mid depend on any audio files.
            endPosn <- case [ t | (t, Events.End) <- eventsList ] of
              t : _ -> return t
              [] -> do
                let absTimes = ATB.getTimes . RTB.toAbsoluteEventList 0
                    lastMIDIEvent = foldr max 0 $ concatMap (absTimes . RBFile.showTrack) trks ++ absTimes (U.tempoMapToBPS tempos)
                putNormal $ unwords
                  [ "[end] is missing. The last MIDI event is at"
                  , showPosition lastMIDIEvent
                  , "so [end] will be at"
                  , showPosition $ 4 + lastMIDIEvent
                  ]
                return $ 4 + lastMIDIEvent
            musicEndPosn <- case [ t | (t, Events.MusicEnd) <- eventsList ] of
              t : _ -> return t
              []    -> do
                putNormal $ unwords
                  [ "[music_end] is missing. [end] is at"
                  , showPosition endPosn
                  , "so [music_end] will be at"
                  , showPosition $ endPosn - 2
                  ]  
                return $ endPosn - 2
            let eventsTrack = RBFile.Events eventsRaw'
                eventsRaw'
                  = RTB.insert musicStartPosn Events.MusicStart
                  $ RTB.insert musicEndPosn Events.MusicEnd
                  $ RTB.insert endPosn Events.End
                  $ RTB.filter (`notElem` [Events.MusicStart, Events.MusicEnd, Events.End]) eventsRaw
                venueTracks = let
                  trk = mergeTracks [ t | RBFile.Venue t <- trks ]
                  in guard (not $ RTB.null trk) >> [RBFile.Venue trk]
                (drumsPS, drums1p, drums2p, has2xNotes) = if not $ _hasDrums $ _instruments songYaml
                  then ([], [], [], False)
                  else let
                    trk = mergeTracks [ t | RBFile.PartDrums t <- trks ]
                    psKicks = if _auto2xBass $ _metadata songYaml
                      then U.unapplyTempoTrack tempos . phaseShiftKicks 0.18 0.11 . U.applyTempoTrack tempos
                      else id
                    sections = flip RTB.mapMaybe eventsRaw $ \case
                      Events.PracticeSection s -> Just s
                      _                        -> Nothing
                    ps = psKicks $ drumMix mixMode $ drumsComplete (RBFile.s_signatures input) sections trk
                    -- Note: drumMix must be applied *after* drumsComplete.
                    -- Otherwise the automatic EMH mix events could prevent lower difficulty generation.
                    in  ( [RBFile.PartDrums ps]
                        , [RBFile.PartDrums $ rockBand1x ps]
                        , [RBFile.PartDrums $ rockBand2x ps]
                        , elem RBDrums.Kick2x ps
                        )
                guitarTracks = if not $ _hasGuitar $ _instruments songYaml
                  then []
                  else (: []) $ RBFile.PartGuitar $ gryboComplete (Just $ _hopoThreshold $ _metadata songYaml) (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartGuitar t <- trks ]
                bassTracks = if not $ _hasBass $ _instruments songYaml
                  then []
                  else (: []) $ RBFile.PartBass $ gryboComplete (Just $ _hopoThreshold $ _metadata songYaml) (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartBass t <- trks ]
                keysTracks = if not $ hasAnyKeys $ _instruments songYaml
                  then []
                  else let
                    basicKeys = gryboComplete Nothing (RBFile.s_signatures input) $ if _hasKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartKeys t <- trks ]
                      else expertProKeysToKeys keysExpert
                    keysDiff diff = if _hasProKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartRealKeys diff' t <- trks, diff == diff' ]
                      else keysToProKeys diff basicKeys
                    rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
                    keysExpert = completeRanges $ keysDiff Expert
                    keysHard   = completeRanges $ keysDiff Hard   `orIfNull` pkReduce Hard   (RBFile.s_signatures input) keysOD keysExpert
                    keysMedium = completeRanges $ keysDiff Medium `orIfNull` pkReduce Medium (RBFile.s_signatures input) keysOD keysHard
                    keysEasy   = completeRanges $ keysDiff Easy   `orIfNull` pkReduce Easy   (RBFile.s_signatures input) keysOD keysMedium
                    keysOD = flip RTB.mapMaybe keysExpert $ \case
                      ProKeys.Overdrive b -> Just b
                      _                   -> Nothing
                    keysAnim = flip RTB.filter keysExpert $ \case
                      ProKeys.Note _ _ -> True
                      _                -> False
                    in  [ RBFile.PartKeys            basicKeys
                        , RBFile.PartKeysAnimRH      keysAnim
                        , RBFile.PartKeysAnimLH      RTB.empty
                        , RBFile.PartRealKeys Expert keysExpert
                        , RBFile.PartRealKeys Hard   keysHard
                        , RBFile.PartRealKeys Medium keysMedium
                        , RBFile.PartRealKeys Easy   keysEasy
                        ]
                vocalTracks = case _hasVocal $ _instruments songYaml of
                  Vocal0 -> []
                  Vocal1 ->
                    [ RBFile.PartVocals partVox'
                    ]
                  Vocal2 ->
                    [ RBFile.PartVocals partVox'
                    , RBFile.Harm1 harm1
                    , RBFile.Harm2 harm2
                    ]
                  Vocal3 ->
                    [ RBFile.PartVocals partVox'
                    , RBFile.Harm1 harm1
                    , RBFile.Harm2 harm2
                    , RBFile.Harm3 harm3
                    ]
                  where partVox = mergeTracks [ t | RBFile.PartVocals t <- trks ]
                        partVox' = windLyrics $ if RTB.null partVox then harm1ToPartVocals harm1 else partVox
                        harm1   = windLyrics $ mergeTracks [ t | RBFile.Harm1      t <- trks ]
                        harm2   = windLyrics $ mergeTracks [ t | RBFile.Harm2      t <- trks ]
                        harm3   = windLyrics $ mergeTracks [ t | RBFile.Harm3      t <- trks ]
            beatTrack <- let
              trk = mergeTracks [ t | RBFile.Beat t <- trks ]
              in if RTB.null trk
                then do
                  putNormal "Generating a BEAT track..."
                  return $ RBFile.Beat $ U.trackTake endPosn $ makeBeatTrack $ RBFile.s_signatures input
                else return $ RBFile.Beat trk
            forM_ [(midPS, drumsPS), (mid1p, drums1p), (mid2p, drums2p)] $ \(midout, drumsTracks) ->
              saveMIDI midout RBFile.Song
                { RBFile.s_tempos = tempos
                , RBFile.s_signatures = RBFile.s_signatures input
                , RBFile.s_tracks = map fixRolls $ concat
                  [ [beatTrack]
                  , [eventsTrack]
                  , venueTracks
                  , drumsTracks
                  , guitarTracks
                  , bassTracks
                  , keysTracks
                  , vocalTracks
                  ]
                }
            saveMIDI midraw RBFile.Song
              { RBFile.s_tempos = tempos
              , RBFile.s_signatures = RBFile.s_signatures input
              , RBFile.s_tracks = RBFile.s_tracks input
              }
            liftIO $ writeFile has2p $ show has2xNotes

          display %> \out -> do
            song <- loadMIDI mid2p
            let ht = fromIntegral (_hopoThreshold $ _metadata songYaml) / 480
                gtr = justIf (_hasGuitar $ _instruments songYaml) $ Proc.processFive (Just ht) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartGuitar t <- RBFile.s_tracks song ]
                bass = justIf (_hasBass $ _instruments songYaml) $ Proc.processFive (Just ht) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartBass t <- RBFile.s_tracks song ]
                keys = justIf (_hasKeys $ _instruments songYaml) $ Proc.processFive Nothing (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartKeys t <- RBFile.s_tracks song ]
                drums = justIf (_hasDrums $ _instruments songYaml) $ Proc.processDrums (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                prokeys = justIf (_hasProKeys $ _instruments songYaml) $ Proc.processProKeys (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartRealKeys Expert t <- RBFile.s_tracks song ]
                vox = case _hasVocal $ _instruments songYaml of
                  Vocal0 -> Nothing
                  Vocal1 -> makeVox
                    (foldr RTB.merge RTB.empty [ t | RBFile.PartVocals t <- RBFile.s_tracks song ])
                    RTB.empty
                    RTB.empty
                  Vocal2 -> makeVox
                    (foldr RTB.merge RTB.empty [ t | RBFile.Harm1 t <- RBFile.s_tracks song ])
                    (foldr RTB.merge RTB.empty [ t | RBFile.Harm2 t <- RBFile.s_tracks song ])
                    RTB.empty
                  Vocal3 -> makeVox
                    (foldr RTB.merge RTB.empty [ t | RBFile.Harm1 t <- RBFile.s_tracks song ])
                    (foldr RTB.merge RTB.empty [ t | RBFile.Harm2 t <- RBFile.s_tracks song ])
                    (foldr RTB.merge RTB.empty [ t | RBFile.Harm3 t <- RBFile.s_tracks song ])
                makeVox h1 h2 h3 = Just $ Proc.processVocal (RBFile.s_tempos song) h1 h2 h3 (fmap fromEnum $ _key $ _metadata songYaml)
                beat = Proc.processBeat (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.Beat t <- RBFile.s_tracks song ]
                end = U.applyTempoMap (RBFile.s_tempos song) $ songLengthBeats song
                justIf b x = guard b >> Just x
            liftIO $ BL.writeFile out $ A.encode $ Proc.mapTime (realToFrac :: U.Seconds -> Milli)
              $ Proc.Processed gtr bass keys drums prokeys vox beat end

          -- -- Guitar rules
          -- dir </> "guitar.mid" %> \out -> do
          --   input <- loadMIDI mid2p
          --   saveMIDI out $ RBFile.playGuitarFile [0, 0, 0, 0, 0, 0] [0, 0, 0, 0] input
          --   -- TODO: support different tunings again

          -- Countin audio, and song+countin files
          let useCountin (Countin hits) = do
                dir </> "countin.wav" %> \out -> case hits of
                  [] -> buildAudio (Silence 1 $ Frames 0) out
                  _  -> do
                    mid <- loadMIDI $ dir </> "2p/notes.mid"
                    hits' <- forM hits $ \(posn, aud) -> do
                      let time = case posn of
                            Left  mb   -> Seconds $ realToFrac $ U.applyTempoMap (RBFile.s_tempos mid) $ U.unapplyMeasureMap (RBFile.s_signatures mid) mb
                            Right secs -> Seconds $ realToFrac secs
                      aud' <- fmap join $ mapM manualLeaf aud
                      return $ Pad Start time aud'
                    buildAudio (Mix hits') out
                dir </> "song-countin.wav" %> \out -> do
                  let song = Input $ dir </> "song.wav"
                      countin = Input $ dir </> "countin.wav"
                  buildAudio (Mix [song, countin]) out
          case plan of
            MoggPlan{}   -> return () -- handled above
            Plan{..}     -> useCountin _countin
            EachPlan{..} -> useCountin _countin
          dir </> "song-countin.ogg" %> \out ->
            buildAudio (Input $ out -<.> "wav") out

          -- Rock Band OGG and MOGG
          let ogg  = dir </> "audio.ogg"
              mogg = dir </> "audio.mogg"
          ogg %> \out -> case plan of
            MoggPlan{} -> do
              need [mogg]
              liftIO $ moggToOgg mogg out
            _ -> let
              hasCrowd = case plan of
                Plan{..} -> isJust _crowd
                _        -> False
              parts = map Input $ concat
                [ [dir </> "kick.wav"   | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
                , [dir </> "snare.wav"  | _hasDrums    (_instruments songYaml) && elem mixMode [RBDrums.D1, RBDrums.D2, RBDrums.D3]]
                , [dir </> "drums.wav"  | _hasDrums   $ _instruments songYaml]
                , [dir </> "bass.wav"   | _hasBass    $ _instruments songYaml]
                , [dir </> "guitar.wav" | _hasGuitar  $ _instruments songYaml]
                , [dir </> "keys.wav"   | hasAnyKeys  $ _instruments songYaml]
                , [dir </> "vocal.wav"  | hasAnyVocal $ _instruments songYaml]
                , [dir </> "crowd.wav"  | hasCrowd                           ]
                , [dir </> "song-countin.wav"]
                ]
              in buildAudio (Merge parts) out
          mogg %> \out -> case plan of
            MoggPlan{..} -> moggOracle (MoggSearch _moggMD5) >>= \case
              Nothing -> fail "Couldn't find the MOGG file"
              Just f -> do
                putNormal $ "Found the MOGG file: " ++ f
                copyFile' f out
            _ -> do
              need [ogg]
              oggToMogg ogg out

          -- Low-quality audio files for the online preview app
          forM_ [("mp3", crapMP3), ("ogg", crapVorbis)] $ \(ext, crap) -> do
            dir </> "web/preview-audio" <.> ext %> \out -> do
              need [dir </> "everything-mono.wav"]
              src <- liftIO $ sourceSnd $ dir </> "everything-mono.wav"
              putNormal $ "Writing a crappy audio file to " ++ out
              liftIO $ runResourceT $ crap out src
              putNormal $ "Finished writing a crappy audio file to " ++ out

          dir </> "ps/song.ini" %> \out -> do
            song <- loadMIDI midPS
            let (pstart, _) = previewBounds songYaml song
                len = songLengthMS song
            liftIO $ FoF.saveSong out FoF.Song
              { FoF.artist           = Just $ _artist $ _metadata songYaml
              , FoF.name             = Just $ _title $ _metadata songYaml
              , FoF.album            = Just $ _album $ _metadata songYaml
              , FoF.charter          = Just $ _author $ _metadata songYaml
              , FoF.year             = Just $ _year $ _metadata songYaml
              , FoF.genre            = Just $ genreDisplay $ _genre $ _metadata songYaml
              , FoF.proDrums         = guard (_hasDrums $ _instruments songYaml) >> Just True
              , FoF.songLength       = Just len
              , FoF.previewStartTime = Just pstart
              -- difficulty tiers go from 0 to 6, or -1 for no part
              , FoF.diffBand         = Just $ fromIntegral $ bandTier    - 1
              , FoF.diffGuitar       = Just $ fromIntegral $ guitarTier  - 1
              , FoF.diffBass         = Just $ fromIntegral $ bassTier    - 1
              , FoF.diffDrums        = Just $ fromIntegral $ drumsTier   - 1
              , FoF.diffDrumsReal    = Just $ fromIntegral $ drumsTier   - 1
              , FoF.diffKeys         = Just $ fromIntegral $ keysTier    - 1
              , FoF.diffKeysReal     = Just $ fromIntegral $ proKeysTier - 1
              , FoF.diffVocals       = Just $ fromIntegral $ vocalTier   - 1
              , FoF.diffVocalsHarm   = Just $ fromIntegral $ vocalTier   - 1
              , FoF.diffDance        = Just (-1)
              , FoF.diffBassReal     = Just (-1)
              , FoF.diffGuitarReal   = Just (-1)
              , FoF.diffBassReal22   = Just (-1)
              , FoF.diffGuitarReal22 = Just (-1)
              , FoF.diffGuitarCoop   = Just (-1)
              , FoF.diffRhythm       = Just (-1)
              , FoF.diffDrumsRealPS  = Just (-1)
              , FoF.diffKeysRealPS   = Just (-1)
              , FoF.delay            = Nothing
              , FoF.starPowerNote    = Just 116
              , FoF.track            = Just $ _trackNumber $ _metadata songYaml
              }
          dir </> "ps/drums.ogg"   %> buildAudio (Input $ dir </> "drums.wav"       )
          dir </> "ps/drums_1.ogg" %> buildAudio (Input $ dir </> "kick.wav"        )
          dir </> "ps/drums_2.ogg" %> buildAudio (Input $ dir </> "snare.wav"       )
          dir </> "ps/drums_3.ogg" %> buildAudio (Input $ dir </> "drums.wav"       )
          dir </> "ps/guitar.ogg"  %> buildAudio (Input $ dir </> "guitar.wav"      )
          dir </> "ps/keys.ogg"    %> buildAudio (Input $ dir </> "keys.wav"        )
          dir </> "ps/rhythm.ogg"  %> buildAudio (Input $ dir </> "bass.wav"        )
          dir </> "ps/vocal.ogg"   %> buildAudio (Input $ dir </> "vocal.wav"       )
          dir </> "ps/crowd.ogg"   %> buildAudio (Input $ dir </> "crowd.wav"       )
          dir </> "ps/song.ogg"    %> buildAudio (Input $ dir </> "song-countin.wav")
          dir </> "ps/album.png"   %> copyFile' "gen/cover.png"
          phony (dir </> "ps") $ need $ map (\f -> dir </> "ps" </> f) $ concat
            [ ["song.ini", "notes.mid", "song.ogg", "album.png"]
            , ["drums.ogg"   | _hasDrums    (_instruments songYaml) && mixMode == RBDrums.D0]
            , ["drums_1.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["drums_2.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["drums_3.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["guitar.ogg"  | _hasGuitar  $ _instruments songYaml]
            , ["keys.ogg"    | hasAnyKeys  $ _instruments songYaml]
            , ["rhythm.ogg"  | _hasBass    $ _instruments songYaml]
            , ["vocal.ogg"   | hasAnyVocal $ _instruments songYaml]
            , ["crowd.ogg"   | case plan of Plan{..} -> isJust _crowd; _ -> False]
            ]

          -- Rock Band 3 DTA file
          let makeDTA :: String -> FilePath -> String -> Maybe Int -> Action D.SongPackage
              makeDTA pkg mid title piracy = do
                song <- loadMIDI mid
                let (pstart, pend) = previewBounds songYaml song
                    len = songLengthMS song
                    perctype = getPercType song

                let channels = concat [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV, crowdPV, songPV]
                    pans = piratePans $ case plan of
                      MoggPlan{..} -> _pans
                      _ -> map fst channels
                    vols = pirateVols $ case plan of
                      MoggPlan{..} -> _vols
                      _ -> map snd channels
                    piratePans = case piracy of
                      Nothing -> id
                      Just i  -> zipWith const $ map (\j -> if i == j then -1 else 1) [0..]
                    pirateVols = case piracy of
                      Nothing -> id
                      Just _  -> map $ const 0
                    -- I still don't know what cores are...
                    -- All I know is guitar channels are usually (not always) 1 and all others are -1
                    cores = case plan of
                      MoggPlan{..} -> map (\i -> if elem i _moggGuitar then 1 else -1) $ zipWith const [0..] _pans
                      _ -> concat
                        [ map (const (-1)) $ concat [kickPV, snarePV, drumsPV, bassPV]
                        , map (const   1)    guitarPV
                        , map (const (-1)) $ concat [keysPV, vocalPV, crowdPV, songPV]
                        ]
                    -- TODO: clean this up
                    crowdChannels = case plan of
                      MoggPlan{..} -> _moggCrowd
                      EachPlan{} -> []
                      Plan{..} -> take (length crowdPV) $ drop (sum $ map length [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV]) $ [0..]
                    tracksAssocList = Map.fromList $ case plan of
                      MoggPlan{..} -> let
                        maybeChannelPair _   []    = []
                        maybeChannelPair str chans = [(str, Right $ D.InParens $ map fromIntegral chans)]
                        in concat
                          [ maybeChannelPair "drum" _moggDrums
                          , maybeChannelPair "guitar" _moggGuitar
                          , maybeChannelPair "bass" _moggBass
                          , maybeChannelPair "keys" _moggKeys
                          , maybeChannelPair "vocals" _moggVocal
                          ]
                      _ -> let
                        counts =
                          [ ("drum", concat [kickPV, snarePV, drumsPV])
                          , ("bass", bassPV)
                          , ("guitar", guitarPV)
                          , ("keys", keysPV)
                          , ("vocals", vocalPV)
                          ]
                        go _ [] = []
                        go n ((inst, chans) : rest) = case length chans of
                          0 -> go n rest
                          c -> (inst, Right $ D.InParens $ map fromIntegral $ take c [n..]) : go (n + c) rest
                        in go 0 counts

                return D.SongPackage
                  { D.name = title
                  , D.artist = T.unpack $ _artist $ _metadata songYaml
                  , D.master = True
                  , D.songId = case _songID $ _metadata songYaml of
                    Nothing  -> Right $ D.Keyword pkg
                    Just sid -> either Left (Right . D.Keyword . T.unpack) sid
                  , D.song = D.Song
                    { D.songName = "songs/" ++ pkg ++ "/" ++ pkg
                    , D.tracksCount = Nothing
                    , D.tracks = D.InParens $ D.Dict tracksAssocList
                    , D.vocalParts = Just $ case _hasVocal $ _instruments songYaml of
                      Vocal0 -> 0
                      Vocal1 -> 1
                      Vocal2 -> 2
                      Vocal3 -> 3
                    , D.pans = D.InParens $ map realToFrac pans
                    , D.vols = D.InParens $ map realToFrac vols
                    , D.cores = D.InParens cores
                    -- TODO: different drum kit sounds
                    , D.drumSolo = D.DrumSounds $ D.InParens $ map D.Keyword $ words
                      "kick.cue snare.cue tom1.cue tom2.cue crash.cue"
                    , D.drumFreestyle = D.DrumSounds $ D.InParens $ map D.Keyword $ words
                      "kick.cue snare.cue hat.cue ride.cue crash.cue"
                    , D.crowdChannels = guard (not $ null crowdChannels) >> Just (map fromIntegral crowdChannels)
                    , D.hopoThreshold = Just $ fromIntegral $ _hopoThreshold $ _metadata songYaml
                    , D.muteVolume = Nothing
                    , D.muteVolumeVocals = Nothing
                    , D.midiFile = Nothing
                    }
                  , D.bank = Just $ Left $ case perctype of
                    Nothing               -> "sfx/tambourine_bank.milo"
                    Just RBVox.Tambourine -> "sfx/tambourine_bank.milo"
                    Just RBVox.Cowbell    -> "sfx/cowbell_bank.milo"
                    Just RBVox.Clap       -> "sfx/handclap_bank.milo"
                  , D.drumBank = Just $ Right $ D.Keyword $ case _drumKit $ _metadata songYaml of
                    HardRockKit   -> "sfx/kit01_bank.milo"
                    ArenaKit      -> "sfx/kit02_bank.milo"
                    VintageKit    -> "sfx/kit03_bank.milo"
                    TrashyKit     -> "sfx/kit04_bank.milo"
                    ElectronicKit -> "sfx/kit05_bank.milo"
                  , D.animTempo = Left D.KTempoMedium
                  , D.bandFailCue = Nothing
                  , D.songScrollSpeed = 2300
                  , D.preview = (fromIntegral pstart, fromIntegral pend)
                  , D.songLength = fromIntegral len
                  , D.rank = D.Dict $ Map.fromList
                    [ ("drum"     , drumsRank  )
                    , ("bass"     , bassRank   )
                    , ("guitar"   , guitarRank )
                    , ("vocals"   , vocalRank  )
                    , ("keys"     , keysRank   )
                    , ("real_keys", proKeysRank)
                    , ("band"     , bandRank   )
                    ]
                  , D.solo = let
                    kwds = concat
                      [ [D.Keyword "guitar" | hasSolo Guitar song]
                      , [D.Keyword "bass" | hasSolo Bass song]
                      , [D.Keyword "drum" | hasSolo Drums song]
                      , [D.Keyword "keys" | hasSolo Keys song]
                      , [D.Keyword "vocal_percussion" | hasSolo Vocal song]
                      ]
                    in guard (not $ null kwds) >> Just (D.InParens kwds)
                  , D.format = 10
                  , D.version = 30
                  , D.gameOrigin = D.Keyword "ugc_plus"
                  , D.rating = fromIntegral $ fromEnum (_rating $ _metadata songYaml) + 1
                  , D.genre = D.Keyword $ T.unpack $ _genre $ _metadata songYaml
                  , D.subGenre = Just $ D.Keyword $ "subgenre_" ++ T.unpack (_subgenre $ _metadata songYaml)
                  , D.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
                  , D.shortVersion = Nothing
                  , D.yearReleased = fromIntegral $ _year $ _metadata songYaml
                  , D.albumArt = Just True
                  , D.albumName = Just $ T.unpack $ _album $ _metadata songYaml
                  , D.albumTrackNumber = Just $ fromIntegral $ _trackNumber $ _metadata songYaml
                  , D.vocalTonicNote = toEnum . fromEnum <$> _key (_metadata songYaml)
                  , D.songTonality = Nothing
                  , D.tuningOffsetCents = Just 0
                  , D.realGuitarTuning = Nothing
                  , D.realBassTuning = Nothing
                  , D.guidePitchVolume = Just (-3)
                  , D.encoding = Just $ D.Keyword "utf8"
                  , D.context = Nothing
                  , D.decade = Nothing
                  , D.downloaded = Nothing
                  , D.basePoints = Nothing
                  }

          -- CONs for recording MOGG channels
          -- (pan one channel to left, all others to right)
          case plan of
            MoggPlan{..} -> forM_ (zipWith const [0..] $ _pans) $ \i -> do
              dir </> ("mogg" ++ show (i :: Int) ++ ".con") %> \out -> do
                Magma.withSystemTempDirectory "moggrecord" $ \tmp -> do
                  let pkg = "mogg_" ++ T.unpack (T.take 6 _moggMD5) ++ "_" ++ show i
                  liftIO $ Dir.createDirectoryIfMissing True (tmp </> "songs" </> pkg </> "gen")
                  copyFile' (dir </> "audio.mogg"              ) $ tmp </> "songs" </> pkg </> pkg <.> "mogg"
                  copyFile' (dir </> "2p/notes-magma-added.mid") $ tmp </> "songs" </> pkg </> pkg <.> "mid"
                  copyFile' "gen/cover.png_xbox"                 $ tmp </> "songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                  songPkg <- makeDTA pkg (dir </> "2p/notes-magma-added.mid")
                    (T.unpack (_title $ _metadata songYaml) ++ " - Channel " ++ show i)
                    (Just i)
                  liftIO $ do
                    flip B.writeFile emptyMilo                   $ tmp </> "songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                    D.writeFileDTA_utf8                           (tmp </> "songs/songs.dta")
                      $ D.serialize $ D.Dict $ Map.fromList [(pkg, D.toChunks songPkg)]
                  rb3pkg (T.unpack (_title $ _metadata songYaml) ++ " MOGG " ++ show i) "" tmp out
            _ -> return ()

          -- Warn about notes that might hang off before a pro keys range shift
          phony (dir </> "ranges") $ do
            song <- loadMIDI $ dir </> "2p/notes.mid"
            putNormal $ closeShiftsFile song

          -- Print out a summary of (non-vocal) overdrive and unison phrases
          phony (dir </> "overdrive") $ do
            song <- loadMIDI $ dir </> "2p/notes.mid"
            let trackTimes = Set.fromList . ATB.getTimes . RTB.toAbsoluteEventList 0
                getTrack f = foldr RTB.merge RTB.empty $ mapMaybe f $ RBFile.s_tracks song
                fiveOverdrive t = trackTimes $ RTB.filter (== RBFive.Overdrive True) t
                drumOverdrive t = trackTimes $ RTB.filter (== RBDrums.Overdrive True) t
                gtr = fiveOverdrive $ getTrack $ \case RBFile.PartGuitar t -> Just t; _ -> Nothing
                bass = fiveOverdrive $ getTrack $ \case RBFile.PartBass t -> Just t; _ -> Nothing
                keys = fiveOverdrive $ getTrack $ \case RBFile.PartKeys t -> Just t; _ -> Nothing
                drums = drumOverdrive $ getTrack $ \case RBFile.PartDrums t -> Just t; _ -> Nothing
            forM_ (Set.toAscList $ Set.unions [gtr, bass, keys, drums]) $ \t -> let
              insts = intercalate "," $ concat
                [ ["guitar" | Set.member t gtr]
                , ["bass" | Set.member t bass]
                , ["keys" | Set.member t keys]
                , ["drums" | Set.member t drums]
                ]
              posn = RBFile.showPosition $ U.applyMeasureMap (RBFile.s_signatures song) t
              in putNormal $ posn ++ ": " ++ insts
            return ()

          -- Melody's Escape customs
          let melodyAudio = dir </> "melody/audio.ogg"
              melodyChart = dir </> "melody/song.track"
          melodyAudio %> copyFile' (dir </> "everything.ogg")
          melodyChart %> \out -> do
            need [midraw, melodyAudio]
            mid <- loadMIDI midraw
            melody <- liftIO $ MelodysEscape.randomNotes $ U.applyTempoTrack (RBFile.s_tempos mid)
              $ foldr RTB.merge RTB.empty [ trk | RBFile.MelodysEscape trk <- RBFile.s_tracks mid ]
            info <- liftIO $ Snd.getFileInfo melodyAudio
            let secs = realToFrac (Snd.frames info) / realToFrac (Snd.samplerate info) :: U.Seconds
                str = unlines
                  [ "1.02"
                  , intercalate ";"
                    [ show (MelodysEscape.secondsToTicks secs)
                    , show (realToFrac secs :: Centi)
                    , "420"
                    , "4"
                    ]
                  , MelodysEscape.writeTransitions melody
                  , MelodysEscape.writeNotes melody
                  ]
            liftIO $ writeFile out str
          phony (dir </> "melody") $ need [melodyAudio, melodyChart]

          let get1xTitle, get2xTitle :: Action String
              get1xTitle = return $ T.unpack $ _title $ _metadata songYaml
              get2xTitle = flip fmap get2xBass $ \b -> if b
                  then T.unpack (_title $ _metadata songYaml) ++ " (2x Bass Pedal)"
                  else T.unpack (_title $ _metadata songYaml)
              get2xBass :: Action Bool
              get2xBass = read <$> readFile' has2p

          let pedalVersions =
                [ (dir </> "1p", get1xTitle, return False)
                , (dir </> "2p", get2xTitle, get2xBass   )
                ]
          forM_ pedalVersions $ \(pedalDir, getTitle, is2xBass) -> do

            let pkg = "onyx" ++ show (hash (pedalDir, _title $ _metadata songYaml, _artist $ _metadata songYaml) `mod` 1000000000)

            -- Check for some extra problems that Magma doesn't catch.
            phony (pedalDir </> "problems") $ do
              song <- loadMIDI $ pedalDir </> "notes.mid"
              -- Don't have a kick at the start of a drum roll.
              -- It screws up the roll somehow and causes spontaneous misses.
              let drums = foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                  kickSwells = flip RTB.mapMaybe (RTB.collectCoincident drums) $ \evts -> do
                    let kick = RBDrums.DiffEvent Expert $ RBDrums.Note RBDrums.Kick
                        swell1 = RBDrums.SingleRoll True
                        swell2 = RBDrums.DoubleRoll True
                    guard $ elem kick evts && (elem swell1 evts || elem swell2 evts)
                    return ()
              -- Every discobeat mix event should be simultaneous with,
              -- or immediately followed by, a set of notes not including red or yellow.
              let discos = flip RTB.mapMaybe drums $ \case
                    RBDrums.DiffEvent d (RBDrums.Mix _ RBDrums.Disco) -> Just d
                    _ -> Nothing
                  badDiscos = fmap (const ()) $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ filter isBadDisco $ ATB.toPairList $ RTB.toAbsoluteEventList 0 discos
                  drumsDiff d = flip RTB.mapMaybe drums $ \case
                    RBDrums.DiffEvent d' (RBDrums.Note gem) | d == d' -> Just gem
                    _ -> Nothing
                  isBadDisco (t, diff) = case RTB.viewL $ RTB.collectCoincident $ U.trackDrop t $ drumsDiff diff of
                    Just ((_, evts), _) | any isDiscoGem evts -> True
                    _ -> False
                  isDiscoGem = \case
                    RBDrums.Red -> True
                    RBDrums.Pro RBDrums.Yellow _ -> True
                    _ -> False
              -- Don't have a vocal phrase that ends simultaneous with a lyric event.
              -- In static vocals, this puts the lyric in the wrong phrase.
              let vox = foldr RTB.merge RTB.empty [ t | RBFile.PartVocals t <- RBFile.s_tracks song ]
                  harm1 = foldr RTB.merge RTB.empty [ t | RBFile.Harm1 t <- RBFile.s_tracks song ]
                  harm2 = foldr RTB.merge RTB.empty [ t | RBFile.Harm2 t <- RBFile.s_tracks song ]
                  harm3 = foldr RTB.merge RTB.empty [ t | RBFile.Harm3 t <- RBFile.s_tracks song ]
                  phraseOff = RBVox.Phrase False
                  isLyric = \case RBVox.Lyric _ -> True; _ -> False
                  voxBugs = flip RTB.mapMaybe (RTB.collectCoincident vox) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
                  harm1Bugs = flip RTB.mapMaybe (RTB.collectCoincident harm1) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
                  harm2Bugs = flip RTB.mapMaybe (RTB.collectCoincident $ RTB.merge harm2 harm3) $ \evts -> do
                    guard $ elem phraseOff evts && any isLyric evts
                    return ()
              -- Put it all together and show the error positions.
              let showPositions :: RTB.T U.Beats () -> [String]
                  showPositions
                    = map (RBFile.showPosition . U.applyMeasureMap (RBFile.s_signatures song))
                    . ATB.getTimes
                    . RTB.toAbsoluteEventList 0
                  message rtb msg = forM_ (showPositions rtb) $ \pos ->
                    putNormal $ pos ++ ": " ++ msg
              message kickSwells "kick note is simultaneous with start of drum roll"
              message badDiscos "discobeat drum event is followed immediately by red or yellow gem"
              message voxBugs "PART VOCALS vocal phrase ends simultaneous with a lyric"
              message harm1Bugs "HARM1 vocal phrase ends simultaneous with a lyric"
              message harm2Bugs "HARM2 vocal phrase ends simultaneous with a (HARM2 or HARM3) lyric"
              unless (all RTB.null [kickSwells, badDiscos, voxBugs, harm1Bugs, harm2Bugs]) $
                fail "At least 1 problem was found in the MIDI."

            -- Rock Band 3 CON package
            let pathDta  = pedalDir </> "rb3/songs/songs.dta"
                pathMid  = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mid")
                pathMogg = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mogg")
                pathPng  = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                pathMilo = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg <.> ".milo_xbox")
                pathCon  = pedalDir </> "rb3.con"
            pathDta %> \out -> do
              title <- getTitle
              songPkg <- makeDTA pkg (pedalDir </> "notes.mid") title Nothing
              liftIO $ writeUtf8CRLF out $ prettyDTA pkg songPkg
            pathMid  %> copyFile' (pedalDir </> "notes-magma-added.mid")
            pathMogg %> copyFile' (dir </> "audio.mogg")
            pathPng  %> copyFile' "gen/cover.png_xbox"
            pathMilo %> \out -> liftIO $ B.writeFile out emptyMilo
            pathCon  %> \out -> do
              need [pathDta, pathMid, pathMogg, pathPng, pathMilo]
              rb3pkg
                (T.unpack (_artist $ _metadata songYaml) ++ ": " ++ T.unpack (_title $ _metadata songYaml))
                ("Version: " ++ pedalDir)
                (pedalDir </> "rb3")
                out

            -- Magma RBProj rules
            let makeMagmaProj :: Action Magma.RBProj
                makeMagmaProj = do
                  song <- loadMIDI $ pedalDir </> "magma/notes.mid"
                  let (pstart, _) = previewBounds songYaml song
                      perctype = getPercType song
                      silentDryVox = Magma.DryVoxPart
                        { Magma.dryVoxFile = "dryvox.wav"
                        , Magma.dryVoxEnabled = True
                        }
                      emptyDryVox = Magma.DryVoxPart
                        { Magma.dryVoxFile = ""
                        , Magma.dryVoxEnabled = False
                        }
                      disabledFile = Magma.AudioFile
                        { Magma.audioEnabled = False
                        , Magma.channels = 0
                        , Magma.pan = []
                        , Magma.vol = []
                        , Magma.audioFile = ""
                        }
                      pvFile [] _ = disabledFile
                      pvFile pv f = Magma.AudioFile
                        { Magma.audioEnabled = True
                        , Magma.channels = fromIntegral $ length pv
                        , Magma.pan = map (realToFrac . fst) pv
                        , Magma.vol = map (realToFrac . snd) pv
                        , Magma.audioFile = f
                        }
                  title <- getTitle
                  return Magma.RBProj
                    { Magma.project = Magma.Project
                      { Magma.toolVersion = "110411_A"
                      , Magma.projectVersion = 24
                      , Magma.metadata = Magma.Metadata
                        { Magma.songName = title
                        , Magma.artistName = T.unpack $ _artist $ _metadata songYaml
                        , Magma.genre = D.Keyword $ T.unpack $ _genre $ _metadata songYaml
                        , Magma.subGenre = D.Keyword $ "subgenre_" ++ T.unpack (_subgenre $ _metadata songYaml)
                        , Magma.yearReleased = fromIntegral $ _year $ _metadata songYaml
                        , Magma.albumName = T.unpack $ _album $ _metadata songYaml
                        , Magma.author = T.unpack $ _author $ _metadata songYaml
                        , Magma.releaseLabel = "Onyxite Customs"
                        , Magma.country = D.Keyword "ugc_country_us"
                        , Magma.price = 160
                        , Magma.trackNumber = fromIntegral $ _trackNumber $ _metadata songYaml
                        , Magma.hasAlbum = True
                        }
                      , Magma.gamedata = Magma.Gamedata
                        { Magma.previewStartMs = fromIntegral pstart
                        , Magma.rankDrum    = max 1 drumsTier
                        , Magma.rankBass    = max 1 bassTier
                        , Magma.rankGuitar  = max 1 guitarTier
                        , Magma.rankVocals  = max 1 vocalTier
                        , Magma.rankKeys    = max 1 keysTier
                        , Magma.rankProKeys = max 1 proKeysTier
                        , Magma.rankBand    = max 1 bandTier
                        , Magma.vocalScrollSpeed = 2300
                        , Magma.animTempo = 32
                        , Magma.vocalGender = fromMaybe Magma.Female $ _vocalGender $ _metadata songYaml
                        , Magma.vocalPercussion = case perctype of
                          Nothing               -> Magma.Tambourine
                          Just RBVox.Tambourine -> Magma.Tambourine
                          Just RBVox.Cowbell    -> Magma.Cowbell
                          Just RBVox.Clap       -> Magma.Handclap
                        , Magma.vocalParts = case _hasVocal $ _instruments songYaml of
                          Vocal0 -> 0
                          Vocal1 -> 1
                          Vocal2 -> 2
                          Vocal3 -> 3
                        , Magma.guidePitchVolume = -3
                        }
                      , Magma.languages = Magma.Languages
                        { Magma.english  = Just True
                        , Magma.french   = Just False
                        , Magma.italian  = Just False
                        , Magma.spanish  = Just False
                        , Magma.german   = Just False
                        , Magma.japanese = Just False
                        }
                      , Magma.destinationFile = pkg <.> "rba"
                      , Magma.midi = Magma.Midi
                        { Magma.midiFile = "notes.mid"
                        , Magma.autogenTheme = Right $ case _autogenTheme $ _metadata songYaml of
                          AutogenDefault -> "Default.rbtheme"
                          theme -> show theme ++ ".rbtheme"
                        }
                      , Magma.dryVox = Magma.DryVox
                        { Magma.part0 = if _hasVocal (_instruments songYaml) >= Vocal1
                          then silentDryVox
                          else emptyDryVox
                        , Magma.part1 = if _hasVocal (_instruments songYaml) >= Vocal2
                          then silentDryVox
                          else emptyDryVox
                        , Magma.part2 = if _hasVocal (_instruments songYaml) >= Vocal3
                          then silentDryVox
                          else emptyDryVox
                        , Magma.dryVoxFileRB2 = Nothing
                        , Magma.tuningOffsetCents = 0
                        }
                      , Magma.albumArt = Magma.AlbumArt "cover.bmp"
                      , Magma.tracks = Magma.Tracks
                        { Magma.drumLayout = case mixMode of
                          RBDrums.D0 -> Magma.Kit
                          RBDrums.D1 -> Magma.KitKickSnare
                          RBDrums.D2 -> Magma.KitKickSnare
                          RBDrums.D3 -> Magma.KitKickSnare
                          RBDrums.D4 -> Magma.KitKick
                        , Magma.drumKit = pvFile drumsPV "drums.wav"
                        , Magma.drumKick = pvFile kickPV "kick.wav"
                        , Magma.drumSnare = pvFile snarePV "snare.wav"
                        , Magma.bass = pvFile bassPV "bass.wav"
                        , Magma.guitar = pvFile guitarPV "guitar.wav"
                        , Magma.vocals = pvFile vocalPV "vocal.wav"
                        , Magma.keys = pvFile keysPV "keys.wav"
                        , Magma.backing = pvFile songPV "song-countin.wav"
                        }
                      }
                    }

            -- Magma rules
            do
              let kick   = pedalDir </> "magma/kick.wav"
                  snare  = pedalDir </> "magma/snare.wav"
                  drums  = pedalDir </> "magma/drums.wav"
                  bass   = pedalDir </> "magma/bass.wav"
                  guitar = pedalDir </> "magma/guitar.wav"
                  keys   = pedalDir </> "magma/keys.wav"
                  vocal  = pedalDir </> "magma/vocal.wav"
                  crowd  = pedalDir </> "magma/crowd.wav"
                  dryvox = pedalDir </> "magma/dryvox.wav"
                  dryvoxSine = pedalDir </> "magma/dryvox-sine.wav"
                  song   = pedalDir </> "magma/song-countin.wav"
                  cover  = pedalDir </> "magma/cover.bmp"
                  coverV1 = pedalDir </> "magma/cover-v1.bmp"
                  mid    = pedalDir </> "magma/notes.mid"
                  midV1  = pedalDir </> "magma/notes-v1.mid"
                  proj   = pedalDir </> "magma/magma.rbproj"
                  projV1 = pedalDir </> "magma/magma-v1.rbproj"
                  c3     = pedalDir </> "magma/magma.c3"
                  setup  = pedalDir </> "magma"
                  rba    = pedalDir </> "magma.rba"
                  rbaV1  = pedalDir </> "magma-v1.rba"
                  export = pedalDir </> "notes-magma-export.mid"
                  export2 = pedalDir </> "notes-magma-added.mid"
                  dummyMono = pedalDir </> "magma/dummy-mono.wav"
                  dummyStereo = pedalDir </> "magma/dummy-stereo.wav"
              kick   %> copyFile' (dir </> "kick.wav"  )
              snare  %> copyFile' (dir </> "snare.wav" )
              drums  %> copyFile' (dir </> "drums.wav" )
              bass   %> copyFile' (dir </> "bass.wav"  )
              guitar %> copyFile' (dir </> "guitar.wav")
              keys   %> copyFile' (dir </> "keys.wav"  )
              vocal  %> copyFile' (dir </> "vocal.wav" )
              crowd  %> copyFile' (dir </> "crowd.wav" )
              dryvox %> \out -> do
                len <- songLengthMS <$> loadMIDI mid
                let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                    audsrc :: (Monad m) => AudioSource m Float
                    audsrc = silent (Seconds $ fromIntegral len / 1000) 16000 1
                liftIO $ runResourceT $ sinkSnd out fmt audsrc
              dryvoxSine %> \out -> do
                m <- loadMIDI mid
                let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                liftIO $ runResourceT $ sinkSnd out fmt $ RB2.dryVoxAudio m
              dummyMono %> \out -> do
                len <- songLengthMS <$> loadMIDI mid
                buildAudio (Silence 1 $ Seconds $ fromIntegral len / 1000) out
              dummyStereo %> \out -> do
                len <- songLengthMS <$> loadMIDI mid
                buildAudio (Silence 2 $ Seconds $ fromIntegral len / 1000) out
              song %> copyFile' (dir </> "song-countin.wav")
              cover %> copyFile' "gen/cover.bmp"
              coverV1 %> \out -> liftIO $ writeBitmap out $ generateImage (\_ _ -> PixelRGB8 0 0 255) 256 256
              mid %> copyFile' (pedalDir </> "notes.mid")
              midV1 %> \out -> loadMIDI mid >>= saveMIDI out . RB2.convertMIDI
              proj %> \out -> do
                p <- makeMagmaProj
                liftIO $ D.writeFileDTA_latin1 out $ D.serialize p
              projV1 %> \out -> do
                p <- makeMagmaProj
                let makeDummy (Magma.Tracks dl dkt dk ds b g v k bck) = Magma.Tracks
                      dl
                      (makeDummyKeep dkt)
                      (makeDummyKeep dk)
                      (makeDummyKeep ds)
                      (makeDummyMono b)
                      (makeDummyMono g)
                      (makeDummyMono v)
                      (makeDummyMono k) -- doesn't matter
                      (makeDummyMono bck)
                    makeDummyMono af = af
                      { Magma.audioFile = "dummy-mono.wav"
                      , Magma.channels = 1
                      , Magma.pan = [0]
                      , Magma.vol = [0]
                      }
                    makeDummyKeep af = case Magma.channels af of
                      1 -> af
                        { Magma.audioFile = "dummy-mono.wav"
                        }
                      _ -> af
                        { Magma.audioFile = "dummy-stereo.wav"
                        , Magma.channels = 2
                        , Magma.pan = [-1, 1]
                        , Magma.vol = [0, 0]
                        }
                    (newGenre, newSubgenre) = case stripPrefix "subgenre_" $ D.fromKeyword $ Magma.subGenre $ Magma.metadata $ Magma.project p of
                      Just sub -> magmaV1Genre
                        ( T.pack $ D.fromKeyword $ Magma.genre $ Magma.metadata $ Magma.project p
                        , T.pack sub
                        )
                      Nothing -> (T.pack "other", T.pack "other")
                liftIO $ D.writeFileDTA_latin1 out $ D.serialize p
                  { Magma.project = (Magma.project p)
                    { Magma.albumArt = Magma.AlbumArt "cover-v1.bmp"
                    , Magma.midi = (Magma.midi $ Magma.project p)
                      { Magma.midiFile = "notes-v1.mid"
                      }
                    , Magma.projectVersion = 5
                    , Magma.languages = Magma.Languages
                      { Magma.english  = Just True
                      , Magma.french   = Just False
                      , Magma.spanish  = Just False
                      , Magma.italian  = Just False
                      , Magma.german   = Nothing
                      , Magma.japanese = Nothing
                      }
                    , Magma.dryVox = (Magma.dryVox $ Magma.project p)
                      { Magma.dryVoxFileRB2 = Just "dryvox-sine.wav"
                      }
                    , Magma.tracks = makeDummy $ Magma.tracks $ Magma.project p
                    , Magma.metadata = (Magma.metadata $ Magma.project p)
                      { Magma.genre = D.Keyword $ T.unpack newGenre
                      , Magma.subGenre = D.Keyword $ "subgenre_" ++ T.unpack newSubgenre
                      }
                    }
                  }
              c3 %> \out -> do
                midi <- loadMIDI mid
                let (pstart, _) = previewBounds songYaml midi
                title <- getTitle
                is2x <- is2xBass
                let crowdVol = case map snd crowdPV of
                      [] -> Nothing
                      v : vs -> if all (== v) vs
                        then Just v
                        else error $ "C3 doesn't support separate crowd volumes: " ++ show (v : vs)
                liftIO $ writeFile out $ C3.showC3 C3.C3
                  { C3.song = T.unpack $ _title $ _metadata songYaml
                  , C3.artist = T.unpack $ _artist $ _metadata songYaml
                  , C3.album = T.unpack $ _album $ _metadata songYaml
                  , C3.customID = pkg
                  , C3.version = 1
                  , C3.isMaster = True
                  , C3.encodingQuality = 5
                  , C3.crowdAudio = guard (isJust crowdVol) >> Just "crowd.wav"
                  , C3.crowdVol = crowdVol
                  , C3.is2xBass = is2x
                  , C3.rhythmKeys = False
                  , C3.rhythmBass = False
                  , C3.karaoke = case plan of
                    Plan{..} -> isJust _vocal && all isNothing [_guitar, _bass, _keys, _drums]
                    _ -> False
                  , C3.multitrack = case plan of
                    Plan{..} -> any isJust [_guitar, _bass, _keys, _drums]
                    _ -> True
                  , C3.convert = False
                  , C3.expertOnly = False
                  , C3.proBassDiff = Nothing
                  , C3.proBassTuning = Nothing
                  , C3.proGuitarDiff = Nothing
                  , C3.proGuitarTuning = Nothing
                  , C3.disableProKeys =
                      _hasKeys (_instruments songYaml) && not (_hasProKeys $ _instruments songYaml)
                  , C3.tonicNote = _key $ _metadata songYaml
                  , C3.tuningCents = 0
                  , C3.songRating = fromEnum (_rating $ _metadata songYaml) + 1
                  , C3.drumKitSFX = fromEnum $ _drumKit $ _metadata songYaml
                  , C3.hopoThresholdIndex = case _hopoThreshold $ _metadata songYaml of
                    90  -> 0
                    130 -> 1
                    170 -> 2
                    250 -> 3
                    ht  -> error $ "C3 Magma does not support the HOPO threshold " ++ show ht
                  , C3.muteVol = -96
                  , C3.vocalMuteVol = -12
                  , C3.soloDrums = hasSolo Drums midi
                  , C3.soloGuitar = hasSolo Guitar midi
                  , C3.soloBass = hasSolo Bass midi
                  , C3.soloKeys = hasSolo Keys midi
                  , C3.soloVocals = hasSolo Vocal midi
                  , C3.songPreview = fromIntegral pstart
                  , C3.checkTempoMap = True
                  , C3.wiiMode = False
                  , C3.doDrumMixEvents = True -- is this a good idea?
                  , C3.packageDisplay = T.unpack (_artist $ _metadata songYaml) ++ " - " ++ title
                  , C3.packageDescription = "Created with Magma: C3 Roks Edition (forums.customscreators.com) and Onyxite's Build Tool."
                  , C3.songAlbumArt = "cover.bmp"
                  , C3.packageThumb = ""
                  , C3.encodeANSI = True  -- is this right?
                  , C3.encodeUTF8 = False -- is this right?
                  , C3.useNumericID = False
                  , C3.uniqueNumericID = ""
                  , C3.uniqueNumericID2X = ""
                  , C3.toDoList = C3.defaultToDo
                  }
              phony setup $ need $ concat
                -- Just make all the Magma prereqs, but don't actually run Magma
                [ guard (_hasDrums   $ _instruments songYaml) >> [drums, kick, snare]
                , guard (_hasBass    $ _instruments songYaml) >> [bass              ]
                , guard (_hasGuitar  $ _instruments songYaml) >> [guitar            ]
                , guard (hasAnyKeys  $ _instruments songYaml) >> [keys              ]
                , guard (hasAnyVocal $ _instruments songYaml) >> [vocal, dryvox     ]
                , [song, crowd, cover, mid, proj, c3]
                ]
              rba %> \out -> do
                need [setup]
                runMagma proj out
              rbaV1 %> \out -> do
                need [dummyMono, dummyStereo, dryvoxSine, coverV1, midV1, projV1]
                good <- runMagmaV1 projV1 out
                unless good $ do
                  putNormal "Magma v1 failed; optimistically bypassing."
                  liftIO $ B.writeFile out B.empty
              export %> \out -> do
                need [mid, proj]
                runMagmaMIDI proj out
              export2 %> \out -> do
                -- Using Magma's "export MIDI" option overwrites all animations/venue
                -- with autogenerated ones, even if they were actually authored.
                -- So, we now need to readd them back from the user MIDI (if they exist).
                userMid <- loadMIDI mid
                magmaMid <- loadMIDI export
                let reauthor getTrack eventPredicates magmaTrack = let
                      authoredTrack = foldr RTB.merge RTB.empty $ mapMaybe getTrack $ RBFile.s_tracks userMid
                      applyEventFn isEvent t = let
                        authoredEvents = RTB.filter isEvent authoredTrack
                        magmaNoEvents = RTB.filter (not . isEvent) t
                        in if RTB.null authoredEvents then t else RTB.merge authoredEvents magmaNoEvents
                      in foldr applyEventFn magmaTrack eventPredicates
                    fivePredicates =
                      [ \case RBFive.Mood{} -> True; _ -> False
                      , \case RBFive.HandMap{} -> True; _ -> False
                      , \case RBFive.StrumMap{} -> True; _ -> False
                      , \case RBFive.FretPosition{} -> True; _ -> False
                      ]
                saveMIDI out $ magmaMid
                  { RBFile.s_tracks = flip map (RBFile.s_tracks magmaMid) $ \case
                    RBFile.PartDrums t -> RBFile.PartDrums $ let
                      getTrack = \case RBFile.PartDrums trk -> Just trk; _ -> Nothing
                      isMood = \case RBDrums.Mood{} -> True; _ -> False
                      isAnim = \case RBDrums.Animation{} -> True; _ -> False
                      in reauthor getTrack [isMood, isAnim] t
                    RBFile.PartGuitar t -> RBFile.PartGuitar $ let
                      getTrack = \case RBFile.PartGuitar trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartBass t -> RBFile.PartBass $ let
                      getTrack = \case RBFile.PartBass trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartKeys       t -> RBFile.PartKeys $ let
                      getTrack = \case RBFile.PartKeys trk -> Just trk; _ -> Nothing
                      in reauthor getTrack fivePredicates t
                    RBFile.PartVocals t -> RBFile.PartVocals $ let
                      getTrack = \case RBFile.PartVocals trk -> Just trk; _ -> Nothing
                      isMood = \case RBVox.Mood{} -> True; _ -> False
                      in reauthor getTrack [isMood] t
                    RBFile.Venue t -> RBFile.Venue $ let
                      getTrack = \case RBFile.Venue trk -> Just trk; _ -> Nothing
                      -- TODO: split up camera and lighting so you can author just one
                      in reauthor getTrack [const True] t
                    -- Stuff "export midi" doesn't overwrite:
                    -- PART KEYS_ANIM_LH/RH
                    -- Crowd stuff in EVENTS
                    t -> t
                  }

            -- Magma v1 rba to con
            do
              let doesRBAExist = do
                    need [rb2RBA]
                    liftIO $ (/= 0) <$> withBinaryFile (pedalDir </> "magma-v1.rba") ReadMode hFileSize 
                  rb2RBA = pedalDir </> "magma-v1.rba"
                  rb2CON = pedalDir </> "rb2.con"
                  rb2OriginalDTA = pedalDir </> "rb2-original.dta"
                  rb2DTA = pedalDir </> "rb2/songs/songs.dta"
                  rb2Mogg = pedalDir </> "rb2/songs" </> pkg </> pkg <.> "mogg"
                  rb2Mid = pedalDir </> "rb2/songs" </> pkg </> pkg <.> "mid"
                  rb2Art = pedalDir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                  rb2Weights = pedalDir </> "rb2/songs" </> pkg </> "gen" </> (pkg ++ "_weights.bin")
                  rb2Milo = pedalDir </> "rb2/songs" </> pkg </> "gen" </> pkg <.> "milo_xbox"
                  rb2Pan = pedalDir </> "rb2/songs" </> pkg </> pkg <.> "pan"
              rb2OriginalDTA %> \out -> do
                ex <- doesRBAExist
                if ex
                  then liftIO $ getRBAFile 0 rb2RBA out
                  else do
                    need [pathDta]
                    (_, rb3DTA, _) <- liftIO $ readRB3DTA pathDta
                    let (newGenre, newSubgenre) = case D.subGenre rb3DTA >>= stripPrefix "subgenre_" . D.fromKeyword of
                          Just sub -> magmaV1Genre
                            ( T.pack $ D.fromKeyword $ D.genre rb3DTA
                            , T.pack sub
                            )
                          Nothing -> (T.pack "other", T.pack "other")
                        newDTA :: D.SongPackage
                        newDTA = D.SongPackage
                          { D.name = D.name rb3DTA
                          , D.artist = D.artist rb3DTA
                          , D.master = True
                          , D.song = D.Song
                            -- most of this gets rewritten later anyway
                            { D.songName = D.songName $ D.song rb3DTA
                            , D.tracksCount = Nothing
                            , D.tracks = D.tracks $ D.song rb3DTA
                            , D.pans = D.pans $ D.song rb3DTA
                            , D.vols = D.vols $ D.song rb3DTA
                            , D.cores = D.cores $ D.song rb3DTA
                            , D.drumSolo = D.drumSolo $ D.song rb3DTA -- needed
                            , D.drumFreestyle = D.drumFreestyle $ D.song rb3DTA -- needed
                            , D.midiFile = D.midiFile $ D.song rb3DTA
                            -- not used
                            , D.vocalParts = Nothing
                            , D.crowdChannels = Nothing
                            , D.hopoThreshold = Nothing
                            , D.muteVolume = Nothing
                            , D.muteVolumeVocals = Nothing
                            }
                          , D.songScrollSpeed = D.songScrollSpeed rb3DTA
                          , D.bank = D.bank rb3DTA
                          , D.animTempo = D.animTempo rb3DTA
                          , D.songLength = D.songLength rb3DTA
                          , D.preview = D.preview rb3DTA
                          , D.rank
                            = D.Dict
                            . Map.filterWithKey (\k _ -> k `elem` words "guitar bass drum vocals band")
                            . D.fromDict
                            $ D.rank rb3DTA
                          , D.genre = D.Keyword $ T.unpack newGenre
                          , D.decade = Just $ D.Keyword $ let y = D.yearReleased rb3DTA in if
                            | 1960 <= y && y < 1970 -> "the60s"
                            | 1970 <= y && y < 1980 -> "the70s"
                            | 1980 <= y && y < 1990 -> "the80s"
                            | 1990 <= y && y < 2000 -> "the90s"
                            | 2000 <= y && y < 2010 -> "the00s"
                            | 2010 <= y && y < 2020 -> "the10s"
                            | otherwise -> "the10s"
                          , D.vocalGender = D.vocalGender rb3DTA
                          , D.version = 0
                          , D.downloaded = Just True
                          , D.format = 4
                          , D.albumArt = Just True
                          , D.yearReleased = D.yearReleased rb3DTA
                          , D.basePoints = Just 0
                          , D.rating = D.rating rb3DTA
                          , D.subGenre = Just $ D.Keyword $ T.unpack newSubgenre
                          , D.songId = D.songId rb3DTA
                          , D.tuningOffsetCents = D.tuningOffsetCents rb3DTA
                          , D.context = Just 2000
                          , D.gameOrigin = D.Keyword "rb2"
                          , D.albumName = D.albumName rb3DTA
                          , D.albumTrackNumber = D.albumTrackNumber rb3DTA
                          -- not present
                          , D.drumBank = Nothing
                          , D.bandFailCue = Nothing
                          , D.solo = Nothing
                          , D.shortVersion = Nothing
                          , D.vocalTonicNote = Nothing
                          , D.songTonality = Nothing
                          , D.realGuitarTuning = Nothing
                          , D.realBassTuning = Nothing
                          , D.guidePitchVolume = Nothing
                          , D.encoding = Nothing
                          }
                    liftIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Key pkg : D.toChunks newDTA))]
              rb2DTA %> \out -> do
                need [rb2OriginalDTA, pathDta]
                (_, magmaDTA, _) <- liftIO $ readRB3DTA rb2OriginalDTA
                (_, rb3DTA, _) <- liftIO $ readRB3DTA pathDta
                let newDTA :: D.SongPackage
                    newDTA = magmaDTA
                      { D.song = (D.song magmaDTA)
                        { D.tracksCount = Nothing
                        , D.tracks = fmap (D.Dict . Map.filterWithKey (\k _ -> k /= "keys") . D.fromDict) $ D.tracks $ D.song rb3DTA
                        , D.midiFile = Just $ "songs/" ++ pkg ++ "/" ++ pkg ++ ".mid"
                        , D.songName = "songs/" ++ pkg ++ "/" ++ pkg
                        , D.pans = D.pans $ D.song rb3DTA
                        , D.vols = D.vols $ D.song rb3DTA
                        , D.cores = D.cores $ D.song rb3DTA
                        , D.crowdChannels = D.crowdChannels $ D.song rb3DTA
                        }
                      , D.songId = case _songID $ _metadata songYaml of
                        Nothing  -> Right $ D.Keyword pkg
                        Just eis -> either Left (Right . D.Keyword . T.unpack) eis
                      }
                liftIO $ D.writeFileDTA_latin1 out $ D.DTA 0 $ D.Tree 0 [D.Parens (D.Tree 0 (D.Key pkg : D.toChunks newDTA))]
              rb2Mid %> \out -> do
                ex <- doesRBAExist
                need [pedalDir </> "notes.mid"]
                mid <- liftIO $ if ex
                  then do
                    getRBAFile 1 rb2RBA out
                    Load.fromFile out
                  else Load.fromFile (pedalDir </> "magma/notes-v1.mid")
                let Left beatTracks = U.decodeFile mid
                -- add back practice sections
                sectsMid <- loadMIDI $ pedalDir </> "notes.mid"
                let sects = foldr RTB.merge RTB.empty $ flip mapMaybe (RBFile.s_tracks sectsMid) $ \case
                      RBFile.Events t -> Just $ flip RTB.mapMaybe t $ \case
                        Events.PracticeSection s -> Just $ showCommand' ["section", s]
                        _                        -> Nothing
                      _               -> Nothing
                    modifyTrack t = if U.trackName t == Just "EVENTS"
                      then RTB.merge sects $ flip RTB.filter t $ \e -> case readCommand' e of
                        Just ["section", _] -> False
                        _                   -> True
                      else t
                    defaultVenue = U.setTrackName "VENUE" $ U.trackJoin $ RTB.flatten $ RTB.singleton 0
                      [ unparseCommand ["lighting", "()"]
                      , unparseCommand ["verse"]
                      , unparseBlip 60
                      , unparseBlip 61
                      , unparseBlip 62
                      , unparseBlip 63
                      , unparseBlip 64
                      , unparseBlip 70
                      , unparseBlip 71
                      , unparseBlip 73
                      , unparseBlip 109
                      ]
                    addVenue = if any ((== Just "VENUE") . U.trackName) beatTracks
                      then id
                      else (++ [defaultVenue])
                liftIO $ Save.toFile out $ U.encodeFileBeats F.Parallel 480 $
                  addVenue $ if RTB.null sects
                    then beatTracks
                    else map modifyTrack beatTracks
              rb2Mogg %> copyFile' (dir </> "audio.mogg")
              rb2Milo %> \out -> do
                ex <- doesRBAExist
                liftIO $ if ex
                  then getRBAFile 3 rb2RBA out
                  else B.writeFile out emptyMiloRB2
              rb2Weights %> \out -> do
                ex <- doesRBAExist
                liftIO $ if ex
                  then getRBAFile 5 rb2RBA out
                  else B.writeFile out emptyWeightsRB2
              rb2Art %> copyFile' "gen/cover.png_xbox"
              rb2Pan %> \out -> liftIO $ B.writeFile out B.empty
              rb2CON %> \out -> do
                need [rb2DTA, rb2Mogg, rb2Mid, rb2Art, rb2Weights, rb2Milo, rb2Pan]
                rb2pkg "title" "desc" (pedalDir </> "rb2") out

        want buildables

      Dir.setCurrentDirectory origDirectory

    makePlayer :: FilePath -> FilePath -> IO ()
    makePlayer fin dout = withSystemTempDirectory "onyx_player" $ \dir -> do
      importAny fin dir
      isFoF <- Dir.doesDirectoryExist fin
      let planWeb = if isFoF then "gen/plan/fof/web" else "gen/plan/mogg/web"
      shakeBuild [planWeb] $ Just $ dir </> "song.yml"
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
    [] -> do
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
      p "  convert-rb2 - convert RB3 CON to RB2 CON"
      p "  reduce - fill in blank difficulties in a MIDI"
      p "  player - create web browser song playback app"
      p "  rpp - convert MIDI to Reaper project"
      p "  ranges - add automatic Pro Keys ranges"
      p "  hanging - find Pro Keys range shifts with hanging notes"
      p "  reap - from onyx project, create and launch Reaper project"
      p "  mt - convert MIDI to a plain text format"
      p "  tm - convert plain text format back to MIDI"
    "build" : buildables -> shakeBuild buildables Nothing
    "mogg" : args -> case inputOutput ".mogg" args of
      Nothing -> error "Usage: onyx mogg in.ogg [out.mogg]"
      Just (ogg, mogg) -> shake shakeOptions $ action $ oggToMogg ogg mogg
    "unmogg" : args -> case inputOutput ".ogg" args of
      Nothing -> error "Usage: onyx unmogg in.mogg [out.ogg]"
      Just (mogg, ogg) -> moggToOgg mogg ogg
    "stfs" : args -> case inputOutput "_rb3con" args of
      Nothing -> error "Usage: onyx stfs in_dir/ [out_rb3con]"
      Just (dir, stfs) -> do
        let getDTAInfo = do
              (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
              return (D.name pkg, D.name pkg ++ " (" ++ D.artist pkg ++ ")")
            handler :: Exc.ErrorCall -> IO (String, String)
            handler _ = return (takeFileName stfs, stfs)
        (title, desc) <- getDTAInfo `Exc.catch` handler
        shake shakeOptions $ action $ rb3pkg title desc dir stfs
    "stfs-rb2" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx stfs-rb2 in_dir/ [out_rb2con]"
      Just (dir, stfs) -> do
        let getDTAInfo = do
              (_, pkg, _) <- readRB3DTA $ dir </> "songs/songs.dta"
              return (D.name pkg, D.name pkg ++ " (" ++ D.artist pkg ++ ")")
            handler :: Exc.ErrorCall -> IO (String, String)
            handler _ = return (takeFileName stfs, stfs)
        (title, desc) <- getDTAInfo `Exc.catch` handler
        shake shakeOptions $ action $ rb2pkg title desc dir stfs
    "unstfs" : args -> case inputOutput "_extract" args of
      Nothing -> error "Usage: onyx unstfs in_rb3con [outdir/]"
      Just (stfs, dir) -> extractSTFS stfs dir
    "import" : args -> case inputOutput "_import" args of
      Nothing -> error "Usage: onyx import in{_rb3con|.rba} [outdir/]"
      Just (file, dir) -> importAny file dir
    "convert" : args -> case inputOutput "_rb3con" args of
      Nothing -> error "Usage: onyx convert in.rba [out_rb3con]"
      Just (rba, con) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny rba dir
        isFoF <- Dir.doesDirectoryExist rba
        let planCon = if isFoF then "gen/plan/fof/2p/rb3.con" else "gen/plan/mogg/2p/rb3.con"
        shakeBuild [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) con
    "convert-rb2" : args -> case inputOutput "_rb2con" args of
      Nothing -> error "Usage: onyx convert-rb2 in_rb3con [out_rb2con]"
      Just (fin, fout) -> withSystemTempDirectory "onyx_convert" $ \dir -> do
        importAny fin dir
        isFoF <- Dir.doesDirectoryExist fin
        let planCon = if isFoF then "gen/plan/fof/2p/rb2.con" else "gen/plan/mogg/2p/rb2.con"
        shakeBuild [planCon] $ Just $ dir </> "song.yml"
        Dir.copyFile (dir </> planCon) fout
    "reduce" : args -> case inputOutput ".reduced.mid" args of
      Nothing -> error "Usage: onyx reduce in.mid [out.mid]"
      Just (fin, fout) -> simpleReduce fin fout
    "player" : args -> case inputOutput "_player" args of
      Nothing -> error "Usage: onyx player in{_rb3con|.rba} [outdir/]"
      Just (fin, dout) -> makePlayer fin dout
    "rpp" : args -> case inputOutput ".RPP" args of
      Nothing -> error "Usage: onyx rpp in.mid [out.RPP]"
      Just (mid, rpp) -> shake shakeOptions $ action $ makeReaper mid mid [] rpp
    "ranges" : args -> case inputOutput ".ranges.mid" args of
      Nothing -> error "Usage: onyx ranges in.mid [out.mid]"
      Just (fin, fout) -> completeFile fin fout
    "hanging" : args -> case inputOutput ".hanging.txt" args of
      Nothing -> error "Usage: onyx hanging in.mid [out.txt]"
      Just (fin, fout) -> do
        song <- Load.fromFile fin >>= printStackTraceIO . RBFile.readMIDIFile
        writeFile fout $ closeShiftsFile song
    "reap" : args -> case args of
      [plan] -> do
        let rpp = "notes-" ++ plan ++ ".RPP"
        shakeBuild [rpp] Nothing
        Dir.renameFile rpp "notes.RPP"
        case Info.os of
          "darwin" -> callProcess "open" ["notes.RPP"]
          _        -> return ()
      _ -> error "Usage: onyx reap plan"
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

inputOutput :: String -> [String] -> Maybe (FilePath, FilePath)
inputOutput suffix args = case args of
  [fin] -> let
    dropSlash = reverse . dropWhile (`elem` "/\\") . reverse
    in Just (fin, dropSlash fin ++ suffix)
  [fin, fout] -> Just (fin, fout)
  _ -> Nothing

makeReaper :: FilePath -> FilePath -> [FilePath] -> FilePath -> Action ()
makeReaper evts tempo audios out = do
  need $ evts : tempo : audios
  lenAudios <- flip mapMaybeM audios $ \aud -> do
    info <- liftIO $ Snd.getFileInfo aud
    return $ case Snd.frames info of
      0 -> Nothing
      f -> Just (fromIntegral f / fromIntegral (Snd.samplerate info), aud)
  mid <- liftIO $ Load.fromFile evts
  tmap <- loadTempos tempo
  tempoMid <- liftIO $ Load.fromFile tempo
  let getLastTime :: (NNC.C t, Num t) => [RTB.T t a] -> t
      getLastTime = foldr max NNC.zero . map getTrackLastTime
      getTrackLastTime trk = case reverse $ ATB.getTimes $ RTB.toAbsoluteEventList NNC.zero trk of
        []    -> NNC.zero
        t : _ -> t
      lastEventSecs = case U.decodeFile mid of
        Left beatTracks -> U.applyTempoMap tmap $ getLastTime beatTracks
        Right secTracks -> getLastTime secTracks
      midiLenSecs = 5 + foldr max lastEventSecs (map fst lenAudios)
      midiLenTicks resn = floor $ U.unapplyTempoMap tmap midiLenSecs * fromIntegral resn
      writeTempoTrack = case tempoMid of
        F.Cons F.Parallel (F.Ticks resn) (tempoTrack : _) -> let
          t_ticks = RPP.processTempoTrack tempoTrack
          t_beats = RTB.mapTime (\tks -> fromIntegral tks / fromIntegral resn) t_ticks
          t_secs = U.applyTempoTrack tmap t_beats
          in RPP.tempoTrack $ RTB.toAbsoluteEventList 0 t_secs
        F.Cons F.Mixed (F.Ticks resn) tracks -> let
          track = foldr RTB.merge RTB.empty tracks
          t_ticks = RPP.processTempoTrack track
          t_beats = RTB.mapTime (\tks -> fromIntegral tks / fromIntegral resn) t_ticks
          t_secs = U.applyTempoTrack tmap t_beats
          in RPP.tempoTrack $ RTB.toAbsoluteEventList 0 t_secs
        _ -> error "Unsupported MIDI format for Reaper project generation"
  liftIO $ writeRPP out $ runIdentity $
    RPP.rpp "REAPER_PROJECT" ["0.1", "5.0/OSX64", "1449358215"] $ do
      RPP.line "VZOOMEX" ["0"]
      RPP.line "SAMPLERATE" ["44100", "0", "0"]
      writeTempoTrack
      case mid of
        F.Cons F.Parallel (F.Ticks resn) (_ : trks) -> do
          forM_ (RPP.sortTracks trks) $ RPP.track (midiLenTicks resn) midiLenSecs resn
        F.Cons F.Mixed (F.Ticks resn) tracks -> let
          track = foldr RTB.merge RTB.empty tracks
          in RPP.track (midiLenTicks resn) midiLenSecs resn track
        _ -> error "Unsupported MIDI format for Reaper project generation"
      forM_ lenAudios $ \(len, aud) -> do
        RPP.audio len $ makeRelative (takeDirectory out) aud
