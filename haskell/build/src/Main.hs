{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE RecordWildCards            #-}
module Main (main) where

import           Audio
import qualified C3
import           Config                           hiding (Difficulty)
import           Image
import           Magma                            hiding
                                                   (withSystemTempDirectory)
import           MoggDecrypt
import           OneFoot
import qualified OnyxiteDisplay.Process           as Proc
import           Reaper.Base                      (writeRPP)
import qualified Reaper.Build                     as RPP
import           Reductions
import           Resources                        (emptyMilo, webDisplay)
import           Scripts
import           STFS.Extract
import           X360
import           YAMLTree

import           Codec.Picture
import           Control.Monad.Extra
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                       as A
import           Data.Binary.Get                  (getWord32le, runGet)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
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
import           Data.Fixed                       (Milli)
import           Data.Foldable                    (toList)
import           Data.Functor.Identity            (runIdentity)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (isPrefixOf, nub, sortOn,
                                                   stripPrefix)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust, isNothing,
                                                   listToMaybe, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Yaml                        as Y
import           Development.Shake                hiding (phony, (%>), (&%>))
import qualified Development.Shake                as Shake
import           Development.Shake.Classes
import           Development.Shake.FilePath
import           RockBand.Common                  (Difficulty (..), Key (..))
import qualified RockBand.Drums                   as RBDrums
import qualified RockBand.Events                  as Events
import qualified RockBand.File                    as RBFile
import qualified RockBand.FiveButton              as RBFive
import qualified RockBand.ProKeys                 as ProKeys
import qualified RockBand.Vocals                  as RBVox
import qualified Sound.File.Sndfile               as Snd
import qualified Sound.Jammit.Base                as J
import qualified Sound.Jammit.Export              as J
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Load             as Load
import qualified Sound.MIDI.Util                  as U
import           System.Console.GetOpt
import qualified System.Directory                 as Dir
import           System.Environment               (getArgs)
import           System.IO                        (IOMode (..), SeekMode (..),
                                                   hSeek, withBinaryFile)
import           System.IO.Extra                  (latin1, readFileEncoding',
                                                   utf8)
import           System.IO.Temp                   (withSystemTempDirectory)

data Argument
  = AudioDir  FilePath
  | SongFile  FilePath
  deriving (Eq, Ord, Show, Read)

optDescrs :: [OptDescr Argument]
optDescrs =
  [ Option [] ["audio"] (ReqArg AudioDir  "DIR" ) "a directory with audio"
  , Option [] ["song" ] (ReqArg SongFile  "FILE") "the song YAML file"
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
-- The String is an MD5 hash of the complete MOGG file.
newtype MoggSearch = MoggSearch T.Text
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

newtype GetSongYaml = GetSongYaml ()
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
                in map getLeaves $ concatMap (maybe [] toList) [_song, _guitar, _bass, _keys, _drums, _vocal]
        case filter (not . (`elem` definedLeaves)) leaves of
          [] -> return ()
          undefinedLeaves -> fail $
            "Undefined leaves in plan " ++ show planName ++ " audio expression: " ++ show undefinedLeaves

      let computeChannels :: Audio Duration Int -> Int
          computeChannels = \case
            Silence n _ -> n
            Input n -> n
            Mix auds -> maximum $ map computeChannels auds
            Merge auds -> sum $ map computeChannels auds
            Concatenate auds -> maximum $ map computeChannels auds
            Gain _ aud -> computeChannels aud
            Take _ _ aud -> computeChannels aud
            Drop _ _ aud -> computeChannels aud
            Fade _ _ aud -> computeChannels aud
            Pad _ _ aud -> computeChannels aud
            Resample aud -> computeChannels aud
            Channels chans _ -> length chans

          computeChannelsPlan :: Audio Duration AudioInput -> Int
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

          audioSearch :: AudioFile -> Action (Maybe FilePath)
          audioSearch AudioSnippet{} = fail "panic! called audioSearch on a snippet. report this bug"
          audioSearch AudioFile{..} = do
            genAbsolute <- liftIO $ Dir.canonicalizePath "gen/"
            files <- filter (\f -> not $ genAbsolute `isPrefixOf` f)
              <$> concatMapM allFiles audioDirs
            let md5Result = liftIO $ case _md5 of
                  Nothing -> return Nothing
                  Just md5search -> flip findM files $ \f ->
                    (== Just (T.unpack md5search)) <$> audioMD5 f
                lenResult = liftIO $ case _frames of
                  Nothing -> return Nothing
                  Just len -> flip findM files $ \f ->
                    (== Just len) <$> audioLength f
                nameResult = do
                  name <- _name
                  listToMaybe $ flip filter files $ \f -> takeFileName f == name
                nameResultNoExt = do
                  name <- dropExtension <$> _name
                  listToMaybe $ flip filter files $ \f ->
                    dropExtension (takeFileName f) == name
            firstJustM id [md5Result, lenResult, return nameResult, return nameResultNoExt]

          jammitSearch :: JammitTrack -> Action [(J.AudioPart, FilePath)]
          jammitSearch jmt = do
            let title  = fromMaybe (_title  $ _metadata songYaml) $ _jammitTitle  jmt
                artist = fromMaybe (_artist $ _metadata songYaml) $ _jammitArtist jmt
            lib <- liftIO $ concatMapM J.loadLibrary audioDirs
            return $ J.getAudioParts
              $ J.exactSearchBy J.title  (T.unpack title )
              $ J.exactSearchBy J.artist (T.unpack artist) lib

          moggSearch :: T.Text -> Action (Maybe FilePath)
          moggSearch md5search = do
            genAbsolute <- liftIO $ Dir.canonicalizePath "gen/"
            files <- filter (\f -> not $ genAbsolute `isPrefixOf` f)
              <$> concatMapM allFiles audioDirs
            flip findM files $ \f -> case takeExtension f of
              ".mogg" -> do
                md5 <- liftIO $ show . MD5.md5 <$> BL.readFile f
                return $ T.unpack md5search == md5
              _ -> return False

      origDirectory <- Dir.getCurrentDirectory
      Dir.setCurrentDirectory $ takeDirectory yamlPath
      shakeArgsWith shakeOptions{ shakeThreads = 0 } (map (fmap $ const (Right ())) optDescrs) $ \_ _ -> return $ Just $ do

        audioOracle  <- addOracle $ \(AudioSearch  s) -> audioSearch $ read s
        jammitOracle <- addOracle $ \(JammitSearch s) -> fmap show $ jammitSearch $ read s
        moggOracle   <- addOracle $ \(MoggSearch   s) -> moggSearch s

        -- Make all rules depend on the parsed song.yml contents
        strSongYaml  <- addOracle $ \(GetSongYaml ()) -> return $ show songYaml
        let (%>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
            pat %> f = pat Shake.%> \out -> strSongYaml (GetSongYaml ()) >> f out
            (&%>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
            pats &%> f = pats Shake.&%> \outs -> strSongYaml (GetSongYaml ()) >> f outs
            phony :: String -> Action () -> Rules ()
            phony s f = Shake.phony s $ strSongYaml (GetSongYaml ())  >> f
            infix 1 %>, &%>

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
                    Just fp -> return $ case _rate of
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
        let loadRGB8 = do
              let img = _fileAlbumArt $ _metadata songYaml
              need [img]
              res <- liftIO $ readImage img
              case res of
                Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
                Right dyn -> return $ anyToRGB8 dyn
        "gen/cover.bmp" %> \out -> loadRGB8 >>= liftIO . writeBitmap out . scaleBilinear 256 256
        "gen/cover.png" %> \out -> loadRGB8 >>= liftIO . writePng    out . scaleBilinear 256 256
        "gen/cover.dds" %> \out -> loadRGB8 >>= liftIO . writeDDS    out . scaleBilinear 256 256
        "gen/cover.png_xbox" %> \out -> do
          let f = _fileAlbumArt $ _metadata songYaml
          if takeExtension f == ".png_xbox"
            then copyFile' f out
            else do
              let dds = out -<.> "dds"
              need [dds]
              b <- liftIO $ B.readFile dds
              let header =
                    [ 0x01, 0x04, 0x08, 0x00, 0x00, 0x00, 0x04, 0x00
                    , 0x01, 0x00, 0x01, 0x80, 0x00, 0x00, 0x00, 0x00
                    , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                    , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
                    ]
                  bytes = B.unpack $ B.drop 0x80 b
                  flipPairs (x : y : xs) = y : x : flipPairs xs
                  flipPairs _ = []
                  b' = B.pack $ header ++ flipPairs bytes
              liftIO $ B.writeFile out b'

        -- Build a REAPER project
        "notes.RPP" %> \out -> do
          let countin = "gen/plan/album/countin.wav"
              audio = "gen/plan/album/song.wav"
          need [countin, audio, "notes.mid"]
          countinLen <- do
            info <- liftIO $ Snd.getFileInfo countin
            return $ fromIntegral (Snd.frames info) / fromIntegral (Snd.samplerate info)
          audioLen <- do
            info <- liftIO $ Snd.getFileInfo audio
            return $ fromIntegral (Snd.frames info) / fromIntegral (Snd.samplerate info)
          song <- loadMIDI "notes.mid"
          mid <- liftIO $ Load.fromFile "notes.mid"
          let ends = map (U.applyTempoMap $ RBFile.s_tempos song) $ do
                RBFile.Events trk <- RBFile.s_tracks song
                (bts, Events.End) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 trk
                return bts
              midiLen = 5 + case ends of
                [] -> audioLen
                secs : _ -> secs
          liftIO $ writeRPP out $ runIdentity $
            RPP.rpp "REAPER_PROJECT" ["0.1", "5.0/OSX64", "1449358215"] $ do
              RPP.line "VZOOMEX" ["0"]
              RPP.line "SAMPLERATE" ["44100", "0", "0"]
              case mid of
                F.Cons F.Parallel (F.Ticks resn) (tempoTrack : trks) -> do
                  let t_ticks = RPP.processTempoTrack tempoTrack
                      t_beats = RTB.mapTime (\tks -> fromIntegral tks / fromIntegral resn) t_ticks
                      t_secs = U.applyTempoTrack (RBFile.s_tempos song) t_beats
                  RPP.tempoTrack $ RTB.toAbsoluteEventList 0 t_secs
                  let trackOrder :: [(String, Int)]
                      trackOrder = zip
                        [ "PART DRUMS"
                        , "PART BASS"
                        , "PART GUITAR"
                        , "PART VOCALS"
                        , "HARM1"
                        , "HARM2"
                        , "HARM3"
                        , "PART KEYS"
                        , "PART REAL_KEYS_X"
                        , "PART REAL_KEYS_H"
                        , "PART REAL_KEYS_M"
                        , "PART REAL_KEYS_E"
                        , "PART KEYS_ANIM_RH"
                        , "PART KEYS_ANIM_LH"
                        , "EVENTS"
                        , "VENUE"
                        , "BEAT"
                        , "countin"
                        ] [0..]
                  forM_ (sortOn (U.trackName >=> flip lookup trackOrder) trks) $ RPP.track midiLen resn
                  RPP.audio countinLen countin
                  RPP.audio audioLen audio
                _ -> error "Unsupported MIDI format for Reaper project generation"

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
          line $ "[Play in browser](" ++ link ++ ")"
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

          let dir = "gen/plan/" ++ T.unpack planName
              -- NOTE: the above doesn't use </> because of
              -- https://github.com/ndmitchell/shake/issues/405

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
              bassPV, guitarPV, keysPV, vocalPV, drumsPV, kickPV, snarePV, songPV :: [(Double, Double)]
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
                  notSong = concat [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal]
                  in filter (`notElem` notSong) [0 .. length _pans - 1]
                Plan{..} -> planPV _song
                EachPlan{..} -> eachPlanPV _each

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
            EachPlan{..} -> do
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
                        [_moggGuitar, _moggBass, _moggKeys, _moggDrums, _moggVocal]
                      return i
                oggChannels songChannels out

          dir </> "song.js" %> \out -> do
            let json = dir </> "display.json"
            s <- readFile' json
            let s' = reverse $ dropWhile isSpace $ reverse $ dropWhile isSpace s
                js = "window.onyxSong = " ++ s' ++ ";\n"
            liftIO $ writeFile out js
          -- NOTE: the below phony command doesn't use </> because of
          -- https://github.com/ndmitchell/shake/issues/405
          phony (dir ++ "/web") $ do
            liftIO $ forM_ webDisplay $ \(f, bs) -> do
              Dir.createDirectoryIfMissing True $ dir </> "web" </> takeDirectory f
              B.writeFile (dir </> "web" </> f) bs
            let songFiles = ["preview-audio.mp3", "preview-audio.ogg", "song.js"]
            need $ map (dir </>) songFiles
            forM_ songFiles $ \f -> do
              copyFile' (dir </> f) (dir </> "web" </> f)

          let allAudioWithPV =
                [ (kickPV, dir </> "kick.wav")
                , (snarePV, dir </> "snare.wav")
                , (drumsPV, dir </> "drums.wav")
                , (guitarPV, dir </> "guitar.wav")
                , (bassPV, dir </> "bass.wav")
                , (keysPV, dir </> "keys.wav")
                , (vocalPV, dir </> "vocal.wav")
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
              midcountin = dir </> "countin.mid"
              has2p = dir </> "has2p.txt"
              display = dir </> "display.json"
          [midPS, midcountin, mid2p, mid1p, has2p] &%> \_ -> do
            input <- loadMIDI "notes.mid"
            let extraTempo  = "tempo-" ++ T.unpack planName ++ ".mid"
            tempos <- fmap RBFile.s_tempos $ doesFileExist extraTempo >>= \b -> if b
              then loadMIDI extraTempo
              else return input
            let trks = RBFile.s_tracks input
                mergeTracks = foldr RTB.merge RTB.empty
                beatTrack = let
                  trk = mergeTracks [ t | RBFile.Beat t <- trks ]
                  in RBFile.Beat $ if RTB.null trk
                    then U.trackTake (songLength' input) $ makeBeatTrack $ RBFile.s_signatures input
                    else trk
                eventsTrack = RBFile.Events eventsRaw
                eventsRaw   = mergeTracks [ t | RBFile.Events t <- trks ]
                countinTrack = RBFile.Countin $ mergeTracks [ t | RBFile.Countin t <- trks ]
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
                  else (: []) $ RBFile.PartGuitar $ gryboComplete False (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartGuitar t <- trks ]
                bassTracks = if not $ _hasBass $ _instruments songYaml
                  then []
                  else (: []) $ RBFile.PartBass $ gryboComplete False (RBFile.s_signatures input)
                    $ mergeTracks [ t | RBFile.PartBass t <- trks ]
                keysTracks = if not $ hasAnyKeys $ _instruments songYaml
                  then []
                  else let
                    basicKeys = gryboComplete True (RBFile.s_signatures input) $ if _hasKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartKeys t <- trks ]
                      else expertProKeysToKeys keysExpert
                    keysDiff diff = if _hasProKeys $ _instruments songYaml
                      then mergeTracks [ t | RBFile.PartRealKeys diff' t <- trks, diff == diff' ]
                      else keysToProKeys diff basicKeys
                    rtb1 `orIfNull` rtb2 = if length rtb1 < 5 then rtb2 else rtb1
                    keysExpert = keysDiff Expert
                    keysHard   = keysDiff Hard   `orIfNull` pkReduce Hard   (RBFile.s_signatures input) keysOD keysExpert
                    keysMedium = keysDiff Medium `orIfNull` pkReduce Medium (RBFile.s_signatures input) keysOD keysHard
                    keysEasy   = keysDiff Easy   `orIfNull` pkReduce Easy   (RBFile.s_signatures input) keysOD keysMedium
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
                        partVox' = if RTB.null partVox then harm1ToPartVocals harm1 else partVox
                        harm1   = mergeTracks [ t | RBFile.Harm1      t <- trks ]
                        harm2   = mergeTracks [ t | RBFile.Harm2      t <- trks ]
                        harm3   = mergeTracks [ t | RBFile.Harm3      t <- trks ]
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
            saveMIDI midcountin RBFile.Song
              { RBFile.s_tempos = tempos
              , RBFile.s_signatures = RBFile.s_signatures input
              , RBFile.s_tracks = [countinTrack]
              }
            liftIO $ writeFile has2p $ show has2xNotes

          display %> \out -> do
            song <- loadMIDI mid2p
            let gtr = justIf (_hasGuitar $ _instruments songYaml) $ Proc.processFive (Just $ 170 / 480) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartGuitar t <- RBFile.s_tracks song ]
                bass = justIf (_hasBass $ _instruments songYaml) $ Proc.processFive (Just $ 170 / 480) (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartBass t <- RBFile.s_tracks song ]
                keys = justIf (_hasKeys $ _instruments songYaml) $ Proc.processFive Nothing (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartKeys t <- RBFile.s_tracks song ]
                drums = justIf (_hasDrums $ _instruments songYaml) $ Proc.processDrums (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                prokeys = justIf (_hasProKeys $ _instruments songYaml) $ Proc.processProKeys (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.PartRealKeys Expert t <- RBFile.s_tracks song ]
                beat = Proc.processBeat (RBFile.s_tempos song)
                  $ foldr RTB.merge RTB.empty [ t | RBFile.Beat t <- RBFile.s_tracks song ]
                end = U.applyTempoMap (RBFile.s_tempos song) $ songLength' song
                justIf b x = guard b >> Just x
            liftIO $ BL.writeFile out $ A.encode $ Proc.mapTime (realToFrac :: U.Seconds -> Milli)
              $ Proc.Processed gtr bass keys drums prokeys beat end

          -- -- Guitar rules
          -- dir </> "guitar.mid" %> \out -> do
          --   input <- loadMIDI mid2p
          --   saveMIDI out $ RBFile.playGuitarFile [0, 0, 0, 0, 0, 0] [0, 0, 0, 0] input
          --   -- TODO: support different tunings again

          -- Countin audio, and song+countin files
          dir </> "countin.wav" %> \out -> case _fileCountin $ _metadata songYaml of
            Nothing -> buildAudio (Silence 1 $ Frames 0) out
            Just hit -> makeCountin midcountin hit out
          case plan of
            MoggPlan{} -> return () -- handled above
            _          -> do
              dir </> "song-countin.wav" %> \out -> do
                let song = Input $ dir </> "song.wav"
                    countin = Input $ dir </> "countin.wav"
                buildAudio (Mix [song, countin]) out
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
              parts = map Input $ concat
                [ [dir </> "kick.wav"   | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
                , [dir </> "snare.wav"  | _hasDrums    (_instruments songYaml) && elem mixMode [RBDrums.D1, RBDrums.D2, RBDrums.D3]]
                , [dir </> "drums.wav"  | _hasDrums   $ _instruments songYaml]
                , [dir </> "bass.wav"   | _hasBass    $ _instruments songYaml]
                , [dir </> "guitar.wav" | _hasGuitar  $ _instruments songYaml]
                , [dir </> "keys.wav"   | hasAnyKeys  $ _instruments songYaml]
                , [dir </> "vocal.wav"  | hasAnyVocal $ _instruments songYaml]
                , [dir </> "song-countin.wav"]
                ]
              in buildAudio (Merge parts) out
          mogg %> \out -> case plan of
            MoggPlan{..} -> moggOracle (MoggSearch _moggMD5) >>= \case
              Nothing -> fail "Couldn't find the MOGG file"
              Just f -> copyFile' f out
            _ -> do
              need [ogg]
              oggToMogg ogg out

          -- Low-quality audio files for the online preview app
          forM_ [("mp3", crapMP3), ("ogg", crapVorbis)] $ \(ext, crap) -> do
            dir </> "preview-audio" <.> ext %> \out -> do
              need [dir </> "everything-mono.wav"]
              src <- liftIO $ sourceSnd $ dir </> "everything-mono.wav"
              putNormal $ "Writing a crappy audio file to " ++ out
              liftIO $ runResourceT $ crap out src
              putNormal $ "Finished writing a crappy audio file to " ++ out

          dir </> "ps/song.ini" %> \out -> do
            (pstart, _) <- previewBounds midPS
            len <- songLength midPS
            liftIO $ writeFile out $ unlines
              [ "[song]"
              , "artist = " ++ T.unpack (_artist $ _metadata songYaml)
              , "name = " ++ T.unpack (_title $ _metadata songYaml)
              , "album = " ++ T.unpack (_album $ _metadata songYaml)
              , "frets = " ++ T.unpack (_author $ _metadata songYaml)
              , "charter = " ++ T.unpack (_author $ _metadata songYaml)
              , "year = " ++ show (_year $ _metadata songYaml)
              , "genre = " ++ T.unpack (_genre $ _metadata songYaml) -- TODO: capitalize
              , if _hasDrums $ _instruments songYaml then "pro_drums = True" else ""
              , "song_length = " ++ show len
              , "preview_start_time = " ++ show pstart
              -- difficulty tiers go from 0 to 6, or -1 for no part
              , "diff_band = "        ++ show (bandTier    - 1)
              , "diff_guitar = "      ++ show (guitarTier  - 1)
              , "diff_bass = "        ++ show (bassTier    - 1)
              , "diff_drums = "       ++ show (drumsTier   - 1)
              , "diff_drums_real = "  ++ show (drumsTier   - 1)
              , "diff_keys = "        ++ show (keysTier    - 1)
              , "diff_keys_real = "   ++ show (proKeysTier - 1)
              , "diff_vocals = "      ++ show (vocalTier   - 1)
              , "diff_vocals_harm = " ++ show (vocalTier   - 1)
              , "diff_dance = -1"
              , "diff_bass_real = -1"
              , "diff_guitar_real = -1"
              , "diff_bass_real_22 = -1"
              , "diff_guitar_real_22 = -1"
              , "diff_guitar_coop = -1"
              , "diff_rhythm = -1"
              , "diff_drums_real_ps = -1"
              , "diff_keys_real_ps = -1"
              ]
          dir </> "ps/drums.ogg"   %> buildAudio (Input $ dir </> "drums.wav"       )
          dir </> "ps/drums_1.ogg" %> buildAudio (Input $ dir </> "kick.wav"        )
          dir </> "ps/drums_2.ogg" %> buildAudio (Input $ dir </> "snare.wav"       )
          dir </> "ps/drums_3.ogg" %> buildAudio (Input $ dir </> "drums.wav"       )
          dir </> "ps/guitar.ogg"  %> buildAudio (Input $ dir </> "guitar.wav"      )
          dir </> "ps/keys.ogg"    %> buildAudio (Input $ dir </> "keys.wav"        )
          dir </> "ps/rhythm.ogg"  %> buildAudio (Input $ dir </> "bass.wav"        )
          dir </> "ps/vocal.ogg"   %> buildAudio (Input $ dir </> "vocal.wav"       )
          dir </> "ps/song.ogg"    %> buildAudio (Input $ dir </> "song-countin.wav")
          dir </> "ps/album.png"   %> copyFile' "gen/cover.png"
          -- NOTE: the below phony command doesn't use </> because of
          -- https://github.com/ndmitchell/shake/issues/405
          phony (dir ++ "/ps") $ need $ map (\f -> dir </> "ps" </> f) $ concat
            [ ["song.ini", "notes.mid", "song.ogg", "album.png"]
            , ["drums.ogg"   | _hasDrums    (_instruments songYaml) && mixMode == RBDrums.D0]
            , ["drums_1.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["drums_2.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["drums_3.ogg" | _hasDrums    (_instruments songYaml) && mixMode /= RBDrums.D0]
            , ["guitar.ogg"  | _hasGuitar  $ _instruments songYaml]
            , ["keys.ogg"    | hasAnyKeys  $ _instruments songYaml]
            , ["rhythm.ogg"  | _hasBass    $ _instruments songYaml]
            , ["vocal.ogg"   | hasAnyVocal $ _instruments songYaml]
            ]

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
            -- NOTE: the below phony command doesn't use </> because of
            -- https://github.com/ndmitchell/shake/issues/405
            phony (pedalDir ++ "/problems") $ do
              song <- loadMIDI (pedalDir </> "notes.mid")
              -- Don't have a kick at the start of a drum roll.
              -- It screws up the roll somehow and causes spontaneous misses.
              let drums = foldr RTB.merge RTB.empty [ t | RBFile.PartDrums t <- RBFile.s_tracks song ]
                  kickSwells = flip RTB.mapMaybe (RTB.collectCoincident drums) $ \evts -> do
                    let kick = RBDrums.DiffEvent Expert $ RBDrums.Note RBDrums.Kick
                        swell1 = RBDrums.SingleRoll True
                        swell2 = RBDrums.DoubleRoll True
                    guard $ elem kick evts && (elem swell1 evts || elem swell2 evts)
                    return ()
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
              message kickSwells "kick note can't be simultaneous with start of drum roll"
              message voxBugs "PART VOCALS vocal phrase can't end simultaneous with a lyric"
              message harm1Bugs "HARM1 vocal phrase can't end simultaneous with a lyric"
              message harm2Bugs "HARM2 vocal phrase can't end simultaneous with a (HARM2 or HARM3) lyric"
              unless (all RTB.null [kickSwells, voxBugs, harm1Bugs, harm2Bugs]) $
                fail "At least 1 problem was found in the MIDI."

            -- Rock Band 3 DTA file
            let makeDTA :: Action D.SongPackage
                makeDTA = do
                  let mid = pedalDir </> "notes.mid"
                  song <- loadMIDI mid
                  (pstart, pend) <- previewBounds mid
                  len <- songLength mid
                  perctype <- getPercType mid

                  let channels = concat [kickPV, snarePV, drumsPV, bassPV, guitarPV, keysPV, vocalPV, songPV]
                      pans = case plan of
                        MoggPlan{..} -> _pans
                        _ -> map fst channels
                      vols = case plan of
                        MoggPlan{..} -> _vols
                        _ -> map snd channels
                      -- I still don't know what cores are...
                      -- All I know is guitar channels are usually (not always) 1 and all others are -1
                      cores = case plan of
                        MoggPlan{..} -> map (\i -> if elem i _moggGuitar then 1 else -1) $ zipWith const [0..] _pans
                        _ -> concat
                          [ map (const (-1)) $ concat [kickPV, snarePV, drumsPV, bassPV]
                          , map (const   1)    guitarPV
                          , map (const (-1)) $ concat [keysPV, vocalPV, songPV]
                          ]
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

                  title <- getTitle
                  return D.SongPackage
                    { D.name = title
                    , D.artist = T.unpack $ _artist $ _metadata songYaml
                    , D.master = True
                    , D.songId = Right $ D.Keyword pkg
                    , D.song = D.Song
                      { D.songName = "songs/" ++ pkg ++ "/" ++ pkg
                      , D.tracksCount = Nothing
                      , D.tracks = D.InParens $ D.Dict tracksAssocList
                      , D.vocalParts = case _hasVocal $ _instruments songYaml of
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
                      , D.crowdChannels = Nothing
                      }
                    , D.bank = Just $ Left $ case perctype of
                      Nothing               -> "sfx/tambourine_bank.milo"
                      Just RBVox.Tambourine -> "sfx/tambourine_bank.milo"
                      Just RBVox.Cowbell    -> "sfx/cowbell_bank.milo"
                      Just RBVox.Clap       -> "sfx/handclap_bank.milo"
                    , D.drumBank = Nothing
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
                    , D.solo = Just $ D.InParens $ concat
                      [ [D.Keyword "guitar" | hasSolo Guitar song]
                      , [D.Keyword "bass" | hasSolo Bass song]
                      , [D.Keyword "drum" | hasSolo Drums song]
                      , [D.Keyword "keys" | hasSolo Keys song]
                      , [D.Keyword "vocal_percussion" | hasSolo Vocal song]
                      ]
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
                    }

            -- Rock Band 3 CON package
            let pathDta  = pedalDir </> "rb3/songs/songs.dta"
                pathMid  = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mid")
                pathMogg = pedalDir </> "rb3/songs" </> pkg </> (pkg <.> "mogg")
                pathPng  = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg ++ "_keep.png_xbox")
                pathMilo = pedalDir </> "rb3/songs" </> pkg </> "gen" </> (pkg <.> ".milo_xbox")
                pathCon  = pedalDir </> "rb3.con"
            pathDta %> \out -> do
              songPkg <- makeDTA
              liftIO $ D.writeFileDTA_utf8 out $ D.serialize $
                D.Dict $ Map.fromList [(pkg, D.toChunks songPkg)]
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
                  (pstart, _) <- previewBounds $ pedalDir </> "magma/notes.mid"
                  perctype    <- getPercType   $ pedalDir </> "magma/notes.mid"
                  let silentDryVox = Magma.DryVoxPart
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
                        { Magma.english  = True
                        , Magma.french   = False
                        , Magma.italian  = False
                        , Magma.spanish  = False
                        , Magma.german   = False
                        , Magma.japanese = False
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
              let drums  = pedalDir </> "magma/drums.wav"
                  bass   = pedalDir </> "magma/bass.wav"
                  guitar = pedalDir </> "magma/guitar.wav"
                  keys   = pedalDir </> "magma/keys.wav"
                  vocal  = pedalDir </> "magma/vocal.wav"
                  dryvox = pedalDir </> "magma/dryvox.wav"
                  song   = pedalDir </> "magma/song-countin.wav"
                  cover  = pedalDir </> "magma/cover.bmp"
                  mid    = pedalDir </> "magma/notes.mid"
                  proj   = pedalDir </> "magma/magma.rbproj"
                  c3     = pedalDir </> "magma/magma.c3"
                  setup  = pedalDir </> "magma"
                  rba    = pedalDir </> "magma.rba"
                  export = pedalDir </> "notes-magma-export.mid"
                  export2 = pedalDir </> "notes-magma-added.mid"
              drums  %> copyFile' (dir </> "drums.wav" )
              bass   %> copyFile' (dir </> "bass.wav"  )
              guitar %> copyFile' (dir </> "guitar.wav")
              keys   %> copyFile' (dir </> "keys.wav"  )
              vocal  %> copyFile' (dir </> "vocal.wav" )
              dryvox %> \out -> do
                need [mid]
                len <- songLength mid
                let fmt = Snd.Format Snd.HeaderFormatWav Snd.SampleFormatPcm16 Snd.EndianFile
                    audsrc :: (Monad m) => AudioSource m Float
                    audsrc = silent (Seconds $ fromIntegral len / 1000) 16000 1
                liftIO $ runResourceT $ sinkSnd out fmt audsrc
              song %> copyFile' (dir </> "song-countin.wav")
              cover %> copyFile' "gen/cover.bmp"
              mid %> copyFile' (pedalDir </> "notes.mid")
              proj %> \out -> do
                p <- makeMagmaProj
                liftIO $ D.writeFileDTA_latin1 out $ D.serialize p
              c3 %> \out -> do
                (pstart, _) <- previewBounds mid
                title <- getTitle
                midi <- loadMIDI mid
                is2x <- is2xBass
                liftIO $ writeFile out $ C3.showC3 C3.C3
                  { C3.song = T.unpack $ _title $ _metadata songYaml
                  , C3.artist = T.unpack $ _artist $ _metadata songYaml
                  , C3.album = T.unpack $ _album $ _metadata songYaml
                  , C3.customID = pkg
                  , C3.version = 1
                  , C3.isMaster = True
                  , C3.encodingQuality = 5
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
                  , C3.hopoThresholdIndex = 2 -- default 170
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
                [ guard (_hasDrums   $ _instruments songYaml) >> [drums        ]
                , guard (_hasBass    $ _instruments songYaml) >> [bass         ]
                , guard (_hasGuitar  $ _instruments songYaml) >> [guitar       ]
                , guard (hasAnyKeys  $ _instruments songYaml) >> [keys         ]
                , guard (hasAnyVocal $ _instruments songYaml) >> [vocal, dryvox]
                , [song, cover, mid, proj, c3]
                ]
              rba %> \out -> do
                need [setup]
                runMagma proj out
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

        want buildables

      Dir.setCurrentDirectory origDirectory

  case nonopts of
    [] -> return ()
    "build" : buildables -> shakeBuild buildables Nothing
    ["unstfs", stfs, dir] -> extractSTFS stfs dir
    "unstfs" : _ -> error "Usage: onyx unstfs input_rb3con outdir/"
    ["import", file, dir] -> importFile file dir
    "import" : _ -> error "Usage: onyx import [input_rb3con|input.rba] outdir/"
    [file] -> withSystemTempDirectory "onyx_preview" $ \dir -> do
      let out = file ++ "_preview"
      importFile file dir
      shakeBuild ["gen/plan/mogg/web"] $ Just $ dir </> "song.yml"
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
      b <- Dir.doesDirectoryExist out
      when b $ Dir.removeDirectoryRecursive out
      copyDir (dir </> "gen/plan/mogg/web") out
    _ -> error "Invalid command"

importFile :: FilePath -> FilePath -> IO ()
importFile file dir = do
  magic <- withBinaryFile file ReadMode $ \h -> B.hGet h 4
  if magic `elem` [B8.pack "CON ", B8.pack "STFS"]
    then withSystemTempDirectory "onyx_con" $ \temp -> do
      extractSTFS file temp
      (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs/songs.dta"
      -- C3 puts extra info in DTA comments
      dtaLines <- fmap (lines . filter (/= '\r')) $ readFileEncoding' (if isUTF8 then utf8 else latin1) $ temp </> "songs/songs.dta"
      let author = listToMaybe $ mapMaybe (stripPrefix ";Song authored by ") dtaLines
          base = D.songName $ D.song pkg
          -- Note: the base path does NOT necessarily have to be songs/foo/foo
          -- where foo is the top key of songs.dta. foo can be different!
          -- e.g. C3's "City Escape" has a top key 'SonicAdvCityEscape2x'
          -- and a 'name' of "songs/sonicadv2cityescape2x/sonicadv2cityescape2x"
      importRB3 pkg (fmap T.pack author)
        (temp </> base <.> "mid")
        (temp </> base <.> "mogg")
        (temp </> takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox"))
        "cover.png_xbox"
        dir
    else if magic == B8.pack "RBSF"
      then do
        withSystemTempDirectory "onyx_rba" $ \temp -> do
          withBinaryFile file ReadMode $ \h -> do
            hSeek h AbsoluteSeek 0x08
            let read7words = runGet (replicateM 7 getWord32le) <$> BL.hGet h (7 * 4)
            offsets <- read7words
            sizes <- read7words
            let getFile i = do
                  hSeek h AbsoluteSeek $ fromIntegral $ offsets !! i
                  BL.hGet h $ fromIntegral $ sizes !! i
            getFile 0 >>= BL.writeFile (temp </> "songs.dta")
            getFile 1 >>= BL.writeFile (temp </> "notes.mid")
            getFile 2 >>= BL.writeFile (temp </> "audio.mogg")
            getFile 4 >>= BL.writeFile (temp </> "cover.bmp")
            getFile 6 >>= BL.writeFile (temp </> "extra.dta")
            (_, pkg, isUTF8) <- readRB3DTA $ temp </> "songs.dta"
            extra <- (if isUTF8 then D.readFileDTA_utf8 else D.readFileDTA_latin1) $ temp </> "extra.dta"
            let author = case extra of
                  D.DTA _ (D.Tree _ [D.Parens (D.Tree _
                    ( D.String "backend"
                    : D.Parens (D.Tree _ [D.Key "author", D.String s])
                    : _
                    ))])
                    -> Just s
                  _ -> Nothing
            importRB3 pkg (fmap T.pack author)
              (temp </> "notes.mid")
              (temp </> "audio.mogg")
              (temp </> "cover.bmp")
              "cover.bmp"
              dir
      else error $ file ++ " is not a CON or RBA file; can't import"

-- | CONs put out by C3 Magma sometimes bizarrely have the @tracks_count@ key
-- completely removed from @songs.dta@, but the list of track counts is still
-- there. So, we have to put it back before parsing @song@ as a key-value map.
fixTracksCount :: [D.Chunk String] -> [D.Chunk String]
fixTracksCount = map findSong where
  findSong = \case
    D.Parens (D.Tree w (D.Key "song" : rest)) ->
      D.Parens (D.Tree w (D.Key "song" : map findTracksCount rest))
    x -> x
  findTracksCount = \case
    D.Parens (D.Tree w [D.Parens (D.Tree w2 nums)]) ->
      D.Parens $ D.Tree w [D.Key "tracks_count", D.Parens $ D.Tree w2 nums]
    x -> x

readRB3DTA :: FilePath -> IO (String, D.SongPackage, Bool)
readRB3DTA dtaPath = do
  -- Not sure what encoding it is, try both.
  let readSongWith :: (FilePath -> IO (D.DTA String)) -> IO (String, D.SongPackage)
      readSongWith rdr = do
        dta <- rdr dtaPath
        (k, chunks) <- case D.treeChunks $ D.topTree dta of
          [D.Parens (D.Tree _ (D.Key k : chunks))] -> return (k, chunks)
          _ -> error $ dtaPath ++ " is not a valid songs.dta with exactly one song"
        case D.fromChunks $ fixTracksCount chunks of
          Left e -> error $ dtaPath ++ " couldn't be unserialized: " ++ e
          Right pkg -> return (k, pkg)
  (k_l1, l1) <- readSongWith D.readFileDTA_latin1
  case D.fromKeyword <$> D.encoding l1 of
    Just "utf8" -> (\(k, pkg) -> (k, pkg, True)) <$> readSongWith D.readFileDTA_utf8
    Just "latin1" -> return (k_l1, l1, False)
    Nothing -> return (k_l1, l1, False)
    Just enc -> error $ dtaPath ++ " specifies an unrecognized encoding: " ++ enc

importRB3 :: D.SongPackage -> Maybe T.Text -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
importRB3 pkg author mid mogg cover coverName dir = do
  Dir.copyFile mogg $ dir </> "audio.mogg"
  Dir.copyFile mid $ dir </> "notes.mid"
  Dir.copyFile cover $ dir </> coverName
  md5 <- show . MD5.md5 <$> BL.readFile (dir </> "audio.mogg")
  rb3mid <- Load.fromFile (dir </> "notes.mid") >>= printStackTraceIO . RBFile.readMIDIFile
  Y.encodeFile (dir </> "song.yml") SongYaml
    { _metadata = Metadata
      { _title        = T.pack $ D.name pkg
      , _artist       = T.pack $ D.artist pkg
      , _album        = T.pack $ fromMaybe "" $ D.albumName pkg
      , _genre        = T.pack $ D.fromKeyword $ D.genre pkg
      , _subgenre     = T.pack $ case D.subGenre pkg of
        Nothing -> error $ "When importing a CON file: no subgenre specified"
        Just subk -> case stripPrefix "subgenre_" $ D.fromKeyword subk of
          Nothing -> error $ "When importing a CON file: can't read subgenre: " ++ D.fromKeyword subk
          Just sub -> sub
      , _year         = fromIntegral $ D.yearReleased pkg
      , _fileAlbumArt = coverName
      , _trackNumber  = maybe 0 fromIntegral $ D.albumTrackNumber pkg
      , _fileCountin  = Nothing
      , _comments     = []
      , _vocalGender  = Just $ D.vocalGender pkg
      , _difficulty   = let
        diffMap :: Map.Map String Integer
        diffMap = D.fromDict $ D.rank pkg
        in Difficulties
          { _difficultyDrums   = Rank <$> Map.lookup "drum" diffMap
          , _difficultyGuitar  = Rank <$> Map.lookup "guitar" diffMap
          , _difficultyBass    = Rank <$> Map.lookup "bass" diffMap
          , _difficultyKeys    = Rank <$> Map.lookup "keys" diffMap
          , _difficultyProKeys = Rank <$> Map.lookup "real_keys" diffMap
          , _difficultyVocal   = Rank <$> Map.lookup "vocals" diffMap
          , _difficultyBand    = Rank <$> Map.lookup "band" diffMap
          }
      , _key          = toEnum . fromEnum <$> D.vocalTonicNote pkg
      , _autogenTheme = AutogenDefault
      , _author       = fromMaybe (T.pack "Unknown") author
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _drumKit      = HardRockKit -- TODO
      , _auto2xBass   = False
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton (T.pack "mogg") $ let
      instChans :: Map.Map String [Integer]
      instChans = fmap chanList $ D.fromDict $ D.fromInParens $ D.tracks $ D.song pkg
      chanList :: Either Integer (D.InParens [Integer]) -> [Integer]
      chanList (Left n)                = [n]
      chanList (Right (D.InParens ns)) = ns
      in MoggPlan
        { _moggMD5 = T.pack md5
        , _moggGuitar = maybe [] (map fromIntegral) $ Map.lookup "guitar" instChans
        , _moggBass   = maybe [] (map fromIntegral) $ Map.lookup "bass" instChans
        , _moggKeys   = maybe [] (map fromIntegral) $ Map.lookup "keys" instChans
        , _moggDrums  = maybe [] (map fromIntegral) $ Map.lookup "drum" instChans
        , _moggVocal  = maybe [] (map fromIntegral) $ Map.lookup "vocals" instChans
        , _pans = map realToFrac $ D.fromInParens $ D.pans $ D.song pkg
        , _vols = map realToFrac $ D.fromInParens $ D.vols $ D.song pkg
        , _planComments = []
        , _drumMix = let
          drumEvents = concat [ toList t | RBFile.PartDrums t <- RBFile.s_tracks rb3mid ]
          drumMixes = [ aud | RBDrums.DiffEvent _ (RBDrums.Mix aud _) <- drumEvents ]
          in case drumMixes of
            [] -> RBDrums.D0
            aud : auds -> if all (== aud) auds
              then aud
              else error $ "When importing a CON file: inconsistent drum mixes: " ++ show (nub drumMixes)
        }
    , _instruments = let
      diffMap :: Map.Map String Integer
      diffMap = D.fromDict $ D.rank pkg
      in Instruments
        { _hasDrums   = maybe False (/= 0) $ Map.lookup "drum" diffMap
        , _hasGuitar  = maybe False (/= 0) $ Map.lookup "guitar" diffMap
        , _hasBass    = maybe False (/= 0) $ Map.lookup "bass" diffMap
        , _hasKeys    = maybe False (/= 0) $ Map.lookup "keys" diffMap
        , _hasProKeys = maybe False (/= 0) $ Map.lookup "real_keys" diffMap
        , _hasVocal   = if maybe False (/= 0) $ Map.lookup "vocals" diffMap
          then case D.vocalParts $ D.song pkg of
            0 -> Vocal0
            1 -> Vocal1
            2 -> Vocal2
            3 -> Vocal3
            n -> error $ "When importing a CON file: invalid vocal count of " ++ show n
          else Vocal0
        }
    , _published = True
    }

getPercType :: FilePath -> Action (Maybe RBVox.PercussionType)
getPercType mid = do
  song <- loadMIDI mid
  let vox = foldr RTB.merge RTB.empty $ mapMaybe getVox $ RBFile.s_tracks song
      getVox (RBFile.PartVocals t) = Just t
      getVox (RBFile.Harm1      t) = Just t
      getVox (RBFile.Harm2      t) = Just t
      getVox (RBFile.Harm3      t) = Just t
      getVox _                     = Nothing
      isPercType (RBVox.PercussionAnimation ptype _) = Just ptype
      isPercType _                                   = Nothing
  return $ listToMaybe $ mapMaybe isPercType $ RTB.getBodies vox

rankToTier :: DiffMap -> Integer -> Integer
rankToTier dm rank = fromIntegral $ length $ takeWhile (<= rank) (1 : dm)

tierToRank :: DiffMap -> Integer -> Integer
tierToRank dm tier = (0 : 1 : dm) !! fromIntegral tier

type DiffMap = [Integer]

drumsDiffMap, vocalDiffMap, bassDiffMap, guitarDiffMap, keysDiffMap, bandDiffMap :: DiffMap
drumsDiffMap     = [124, 151, 178, 242, 345, 448]
vocalDiffMap     = [132, 175, 218, 279, 353, 427]
bassDiffMap      = [135, 181, 228, 293, 364, 436]
guitarDiffMap    = [139, 176, 221, 267, 333, 409]
keysDiffMap      = [153, 211, 269, 327, 385, 443]
bandDiffMap      = [163, 215, 243, 267, 292, 345]
-- proGuitarDiffMap = [150, 205, 264, 323, 382, 442]
-- proBassDiffMap   = [150, 208, 267, 325, 384, 442]

-- | Makes a dummy Basic Keys track, for songs with only Pro Keys charted.
expertProKeysToKeys :: RTB.T U.Beats ProKeys.Event -> RTB.T U.Beats RBFive.Event
expertProKeysToKeys = let
  pkToBasic :: [ProKeys.Event] -> RTB.T U.Beats RBFive.Event
  pkToBasic pk = let
    hasNote     = any (\case ProKeys.Note      True _ -> True; _ -> False) pk
    hasODTrue   = elem (ProKeys.Overdrive True ) pk
    hasODFalse  = elem (ProKeys.Overdrive False) pk
    hasBRETrue  = elem (ProKeys.BRE       True ) pk
    hasBREFalse = elem (ProKeys.BRE       False) pk
    blip diff = RTB.fromPairList
      [ (0     , RBFive.DiffEvent diff $ RBFive.Note True  RBFive.Green)
      , (1 / 32, RBFive.DiffEvent diff $ RBFive.Note False RBFive.Green)
      ]
    in foldr RTB.merge RTB.empty $ concat
      [ [ blip d | d <- [minBound .. maxBound], hasNote ]
      , [ RTB.singleton 0 $ RBFive.Overdrive True  | hasODTrue   ]
      , [ RTB.singleton 0 $ RBFive.Overdrive False | hasODFalse  ]
      , [ RTB.singleton 0 $ RBFive.BRE       True  | hasBRETrue  ]
      , [ RTB.singleton 0 $ RBFive.BRE       False | hasBREFalse ]
      ]
  in U.trackJoin . fmap pkToBasic . RTB.collectCoincident

-- | Makes a Pro Keys track, for songs with only Basic Keys charted.
keysToProKeys :: Difficulty -> RTB.T U.Beats RBFive.Event -> RTB.T U.Beats ProKeys.Event
keysToProKeys d = let
  basicToPK = \case
    RBFive.DiffEvent d' (RBFive.Note b c) | d == d' ->
      Just $ ProKeys.Note b $ ProKeys.BlueGreen $ case c of
        RBFive.Green  -> C
        RBFive.Red    -> D
        RBFive.Yellow -> E
        RBFive.Blue   -> F
        RBFive.Orange -> G
    RBFive.Overdrive b | d == Expert -> Just $ ProKeys.Overdrive b
    RBFive.Solo      b | d == Expert -> Just $ ProKeys.Solo      b
    RBFive.BRE       b | d == Expert -> Just $ ProKeys.BRE       b
    RBFive.Trill     b               -> Just $ ProKeys.Trill     b
    _                                -> Nothing
  in RTB.cons 0 (ProKeys.LaneShift ProKeys.RangeA) . RTB.mapMaybe basicToPK

hasSolo :: Instrument -> RBFile.Song t -> Bool
hasSolo Guitar song = not $ null $ do
  RBFile.PartGuitar t <- RBFile.s_tracks song
  RBFive.Solo _ <- RTB.getBodies t
  return ()
hasSolo Bass song = not $ null $ do
  RBFile.PartBass t <- RBFile.s_tracks song
  RBFive.Solo _ <- RTB.getBodies t
  return ()
hasSolo Drums song = not $ null $ do
  RBFile.PartDrums t <- RBFile.s_tracks song
  RBDrums.Solo _ <- RTB.getBodies t
  return ()
hasSolo Keys song = not $ null
  $ do
    RBFile.PartKeys t <- RBFile.s_tracks song
    RBFive.Solo _ <- RTB.getBodies t
    return ()
  ++ do
    RBFile.PartRealKeys Expert t <- RBFile.s_tracks song
    ProKeys.Solo _ <- RTB.getBodies t
    return ()
hasSolo Vocal song = not $ null
  $ do
    RBFile.PartVocals t <- RBFile.s_tracks song
    RBVox.Percussion <- RTB.getBodies t
    return ()
  ++ do
    RBFile.Harm1 t <- RBFile.s_tracks song
    RBVox.Percussion <- RTB.getBodies t
    return ()
