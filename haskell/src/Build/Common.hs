{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Build.Common where

import           Audio
import           AudioSearch
import           Codec.Picture
import           Codec.Picture.Types              (dropTransparency)
import           Config                           hiding (Difficulty)
import           Control.Applicative              (liftA2)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.StackTrace
import           Data.Bifunctor                   (first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum, isAscii,
                                                   isControl, isDigit, isSpace)
import           Data.Conduit                     (runConduit)
import           Data.Conduit.Audio
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Hashable                    (Hashable, hash)
import qualified Data.HashMap.Strict              as HM
import qualified Data.List.NonEmpty               as NE
import           Data.Maybe                       (fromMaybe)
import           Data.SimpleHandle                (Folder (..), Readable,
                                                   crawlFolder)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Development.Shake                hiding (phony, (%>), (&%>))
import           Development.Shake.FilePath
import           Genre
import           Image
import qualified Numeric.NonNegative.Class        as NNC
import           RenderAudio
import           Resources                        (onyxAlbum)
import           RockBand.Codec                   (mapTrack)
import qualified RockBand.Codec.Drums             as RBDrums
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Common
import qualified Sound.MIDI.Util                  as U

data BuildInfo = BuildInfo
  { biSongYaml        :: SongYaml FilePath
  , biYamlDir         :: FilePath
  , biRelative        :: FilePath -> FilePath
  , biAudioLib        :: AudioLibrary
  , biAudioDependPath :: T.Text -> FilePath
  , biOggWavForPlan   :: T.Text -> FilePath
  }

shk :: Action a -> StackTraceT (QueueLog Action) a
shk = lift . lift

makePS3Name :: Int -> SongYaml f -> B.ByteString
makePS3Name num songYaml
  = TE.encodeUtf8
  $ T.take 0x1B -- 0x1C is probably fine, but leaving a null char so make_npdata doesn't get confused when making edat
  $ T.toUpper
  $ T.filter (\c -> isAscii c && isAlphaNum c)
  $ "O" <> T.pack (show num)
    <> getTitle  (_metadata songYaml)
    <> getArtist (_metadata songYaml)

targetTitle :: SongYaml f -> Target -> T.Text
targetTitle songYaml target = let
  base = fromMaybe (getTitle $ _metadata songYaml) $ tgt_Title $ targetCommon target
  in addTitleSuffix target base

targetTitleJP :: SongYaml f -> Target -> Maybe T.Text
targetTitleJP songYaml target = case tgt_Title $ targetCommon target of
  Just _  -> Nothing -- TODO do we need JP title on targets also
  Nothing -> case _titleJP $ _metadata songYaml of
    Nothing   -> Nothing
    Just base -> Just $ addTitleSuffix target base

addTitleSuffix :: Target -> T.Text -> T.Text
addTitleSuffix target base = let
  common = targetCommon target
  segments = base : case target of
    RB3 TargetRB3{..} -> makeLabel []                               rb3_2xBassPedal
    RB2 TargetRB2{..} -> makeLabel ["(RB2 version)" | rb2_LabelRB2] rb2_2xBassPedal
    _                 -> makeLabel []                               False
  makeLabel sfxs is2x = case tgt_Label common of
    Just lbl -> [lbl]
    Nothing  -> concat
      [ case tgt_Speed common of
        Nothing  -> []
        Just 1   -> []
        Just spd -> let
          intSpeed :: Int
          intSpeed = round $ spd * 100
          in ["(" <> T.pack (show intSpeed) <> "% Speed)"]
      , ["(2x Bass Pedal)" | is2x && tgt_Label2x common]
      , sfxs
      ]
  in T.intercalate " " segments

hashRB3 :: (Hashable f) => SongYaml f -> TargetRB3 -> Int
hashRB3 songYaml rb3 = let
  hashed =
    ( rb3
    , _title $ _metadata songYaml
    , _artist $ _metadata songYaml
    -- TODO this should use more info, or find a better way to come up with hashes.
    )
  -- want these to be higher than real DLC, but lower than C3 IDs
  n = hash hashed `mod` 1000000000
  minID = 10000000
  in if n < minID then n + minID else n

crawlFolderBytes :: (MonadIO m) => FilePath -> StackTraceT m (Folder B.ByteString Readable)
crawlFolderBytes p = stackIO $ fmap (first TE.encodeUtf8) $ crawlFolder p

applyTargetMIDI :: TargetCommon -> RBFile.Song (RBFile.OnyxFile U.Beats) -> RBFile.Song (RBFile.OnyxFile U.Beats)
applyTargetMIDI tgt mid = let
  eval = fmap (U.unapplyTempoMap $ RBFile.s_tempos mid) . evalPreviewTime False (Just RBFile.getEventsTrack) mid 0 False
  applyEnd = case tgt_End tgt >>= eval . seg_Notes of
    Nothing -> id
    Just notesEnd -> \m -> m
      { RBFile.s_tracks
        = chopTake notesEnd
        $ RBFile.s_tracks m
      -- the RockBand3 module process functions will remove tempos and sigs after [end]
      }
  applyStart = case tgt_Start tgt >>= \seg -> liftA2 (,) (eval $ seg_FadeStart seg) (eval $ seg_Notes seg) of
    Nothing -> id
    Just (audioStart, notesStart) -> \m -> m
      { RBFile.s_tracks
        = mapTrack (RTB.delay $ notesStart - audioStart)
        $ chopDrop notesStart
        $ RBFile.s_tracks m
      , RBFile.s_tempos = case U.trackSplit audioStart $ U.tempoMapToBPS $ RBFile.s_tempos m of
        -- cut time off the front of the tempo map, and copy the last tempo
        -- from before the cut point to the cut point if needed
        (cut, keep) -> U.tempoMapFromBPS $ case U.trackTakeZero keep of
          [] -> U.trackGlueZero (toList $ snd . snd <$> RTB.viewR cut) keep
          _  -> keep
      , RBFile.s_signatures = case U.trackSplit audioStart $ U.measureMapToTimeSigs $ RBFile.s_signatures m of
        (cut, keep) -> U.measureMapFromTimeSigs U.Error $ case U.trackTakeZero keep of
          _ : _ -> keep -- already a time signature at the cut point
          []    -> case lastEvent cut of
            Nothing -> keep
            Just (t, sig) -> let
              len = U.timeSigLength sig
              afterSig = audioStart - t
              (_, barRemainder) = properFraction $ afterSig / len :: (Int, U.Beats)
              in if barRemainder == 0
                then U.trackGlueZero [sig] keep -- cut point is on an existing barline
                else let
                  partial = barRemainder * len
                  afterPartial = U.trackDrop partial keep
                  in U.trackGlueZero [U.measureLengthToTimeSig partial] $
                    case U.trackTakeZero afterPartial of
                      _ : _ -> keep -- after the partial bar there's an existing signature
                      []    -> Wait partial sig afterPartial -- continue with the pre-cut signature
      }
  applySpeed = case fromMaybe 1 $ tgt_Speed tgt of
    1     -> id
    speed -> \m -> m
      { RBFile.s_tempos
        = U.tempoMapFromBPS
        $ fmap (* realToFrac speed)
        $ U.tempoMapToBPS
        $ RBFile.s_tempos m
      }
  in applySpeed . applyStart . applyEnd $ mid

lastEvent :: (NNC.C t) => RTB.T t a -> Maybe (t, a)
lastEvent (Wait !t x RNil) = Just (t, x)
lastEvent (Wait !t _ xs  ) = lastEvent $ RTB.delay t xs
lastEvent RNil             = Nothing

applyTargetLength :: TargetCommon -> RBFile.Song (f U.Beats) -> U.Seconds -> U.Seconds
applyTargetLength tgt mid = let
  -- TODO get Events track to support sections as segment boundaries
  applyEnd = case tgt_End tgt >>= evalPreviewTime False Nothing mid 0 False . seg_FadeEnd of
    Nothing   -> id
    Just secs -> min secs
  applyStart = case tgt_Start tgt >>= evalPreviewTime False Nothing mid 0 False . seg_FadeStart of
    Nothing   -> id
    Just secs -> subtract secs
  applySpeed t = t / realToFrac (fromMaybe 1 $ tgt_Speed tgt)
  in applySpeed . applyStart . applyEnd

getAudioLength :: BuildInfo -> T.Text -> Plan f -> Staction U.Seconds
getAudioLength buildInfo planName = \case
  MoggPlan{} -> do
    let ogg = biRelative buildInfo $ "gen/plan" </> T.unpack planName </> "audio.ogg"
    shk $ need [ogg]
    liftIO $ audioSeconds ogg
  Plan{..} -> let
    parts = fmap _planExpr $ concat
      [ toList _song
      , toList _crowd
      , toList _planParts >>= toList
      ]
    in case NE.nonEmpty parts of
      Nothing -> return 0
      Just parts' -> do
        src <- mapM (manualLeaf (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo)) (Mix parts') >>= lift . lift . buildSource . join
        let _ = src :: AudioSource (ResourceT IO) Float
        return $ realToFrac $ fromIntegral (frames src) / rate src

audioDepend :: BuildInfo -> T.Text -> Staction FilePath
audioDepend buildInfo name = do
  let path = biAudioDependPath buildInfo name
  shk $ need [path]
  return path

sourceKick, sourceSnare, sourceKit, sourceSimplePart
  :: (MonadResource m)
  => BuildInfo -> [RBFile.FlexPartName] -> TargetCommon -> RBFile.Song f -> Int -> Bool -> T.Text -> Plan FilePath -> RBFile.FlexPartName -> Integer
  -> Staction (AudioSource m Float)

sourceKick buildInfo gameParts tgt mid pad supportsOffMono planName plan fpart rank = do
  ((spec', _, _), _) <- computeDrumsPart fpart plan $ biSongYaml buildInfo
  let spec = adjustSpec supportsOffMono spec'
  src <- case plan of
    MoggPlan{..} -> channelsToSpec spec (biOggWavForPlan buildInfo planName) (zip _pans _vols) $ do
      guard $ rank /= 0
      case HM.lookup fpart $ getParts _moggParts of
        Just (PartDrumKit kick _ _) -> fromMaybe [] kick
        _                           -> []
    Plan{..}     -> buildAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec $ do
      guard $ rank /= 0
      case HM.lookup fpart $ getParts _planParts of
        Just (PartDrumKit kick _ _) -> kick
        _                           -> Nothing
  return $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src

sourceSnare buildInfo gameParts tgt mid pad supportsOffMono planName plan fpart rank = do
  ((_, spec', _), _) <- computeDrumsPart fpart plan $ biSongYaml buildInfo
  let spec = adjustSpec supportsOffMono spec'
  src <- case plan of
    MoggPlan{..} -> channelsToSpec spec (biOggWavForPlan buildInfo planName) (zip _pans _vols) $ do
      guard $ rank /= 0
      case HM.lookup fpart $ getParts _moggParts of
        Just (PartDrumKit _ snare _) -> fromMaybe [] snare
        _                            -> []
    Plan{..}     -> buildAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec $ do
      guard $ rank /= 0
      case HM.lookup fpart $ getParts _planParts of
        Just (PartDrumKit _ snare _) -> snare
        _                            -> Nothing
  return $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src

sourceKit buildInfo gameParts tgt mid pad supportsOffMono planName plan fpart rank = do
  ((_, _, spec'), mixMode) <- computeDrumsPart fpart plan $ biSongYaml buildInfo
  let spec = adjustSpec supportsOffMono spec'
  src <- case plan of
    MoggPlan{..} -> let
      build = channelsToSpec spec (biOggWavForPlan buildInfo planName) (zip _pans _vols)
      indexSets = do
        guard $ rank /= 0
        case HM.lookup fpart $ getParts _moggParts of
          Just (PartDrumKit kick snare kit) -> case mixMode of
            RBDrums.D0 -> toList kick <> toList snare <> [kit]
            _          -> [kit]
          Just (PartSingle             kit) -> [kit]
          _                                 -> []
      in mapM build indexSets >>= \case
        []     -> build []
        s : ss -> return $ foldr mix s ss
    Plan{..}     -> let
      build = buildAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec
      exprs = do
        guard $ rank /= 0
        case HM.lookup fpart $ getParts _planParts of
          Just (PartDrumKit kick snare kit) -> case mixMode of
            RBDrums.D0 -> toList kick <> toList snare <> [kit]
            _          -> [kit]
          Just (PartSingle             kit) -> [kit]
          _                                 -> []
      in mapM (build . Just) exprs >>= \case
        []     -> build Nothing
        s : ss -> return $ foldr mix s ss
  return $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src

getPartSource
  :: (MonadResource m)
  => BuildInfo -> [(Double, Double)] -> T.Text -> Plan FilePath -> RBFile.FlexPartName -> Integer
  -> Staction (AudioSource m Float)
getPartSource buildInfo spec planName plan fpart rank = case plan of
  MoggPlan{..} -> channelsToSpec spec (biOggWavForPlan buildInfo planName) (zip _pans _vols) $ do
    guard $ rank /= 0
    toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
  Plan{..} -> buildPartAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec $ do
    guard $ rank /= 0
    HM.lookup fpart $ getParts _planParts

sourceStereoParts
  :: (MonadResource m)
  => BuildInfo -> [RBFile.FlexPartName] -> TargetCommon -> RBFile.Song f -> Int -> T.Text -> Plan FilePath -> [(RBFile.FlexPartName, Integer)]
  -> Staction (AudioSource m Float)
sourceStereoParts buildInfo gameParts tgt mid pad planName plan fpartranks = do
  let spec = [(-1, 0), (1, 0)]
  srcs <- forM fpartranks $ \(fpart, rank)
    -> zeroIfMultiple gameParts fpart
    <$> getPartSource buildInfo spec planName plan fpart rank
  src <- case srcs of
    []     -> buildAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec Nothing
    s : ss -> return $ foldr mix s ss
  return $ padAudio pad $ applyTargetAudio tgt mid src

sourceSimplePart buildInfo gameParts tgt mid pad supportsOffMono planName plan fpart rank = do
  let spec = adjustSpec supportsOffMono $ computeSimplePart fpart plan $ biSongYaml buildInfo
  src <- getPartSource buildInfo spec planName plan fpart rank
  return $ zeroIfMultiple gameParts fpart $ padAudio pad $ applyTargetAudio tgt mid src

sourceCrowd
  :: (MonadResource m)
  => BuildInfo -> TargetCommon -> RBFile.Song f -> Int -> T.Text -> Plan FilePath
  -> Staction (AudioSource m Float)
sourceCrowd buildInfo tgt mid pad planName plan = do
  src <- case plan of
    MoggPlan{..} -> channelsToSpec [(-1, 0), (1, 0)] (biOggWavForPlan buildInfo planName) (zip _pans _vols) _moggCrowd
    Plan{..}     -> buildAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) [(-1, 0), (1, 0)] _crowd
  return $ padAudio pad $ applyTargetAudio tgt mid src

sourceSongCountin
  :: (MonadResource m)
  => BuildInfo -> TargetCommon -> RBFile.Song f -> Int -> Bool -> T.Text -> Plan FilePath -> [(RBFile.FlexPartName, Integer)]
  -> Staction (AudioSource m Float)
sourceSongCountin buildInfo tgt mid pad includeCountin planName plan fparts = do
  let usedParts' = [ fpart | (fpart, rank) <- fparts, rank /= 0 ]
      usedParts =
        [ fpart
        | fpart <- usedParts'
        , case filter (== fpart) usedParts' of
          -- if more than 1 game part maps to this flex part,
          -- the flex part's audio should go in backing track
          _ : _ : _ -> False
          _         -> True
        ]
      spec = [(-1, 0), (1, 0)]
  src <- case plan of
    MoggPlan{..} -> channelsToSpec spec (biOggWavForPlan buildInfo planName) (zip _pans _vols) $ let
      channelsFor fpart = toList (HM.lookup fpart $ getParts _moggParts) >>= toList >>= toList
      usedChannels = concatMap channelsFor usedParts ++ _moggCrowd
      in filter (`notElem` usedChannels) [0 .. length _pans - 1]
    Plan{..} -> let
      unusedParts = do
        (fpart, pa) <- HM.toList $ getParts _planParts
        guard $ notElem fpart usedParts
        return pa
      partAudios = maybe id (\pa -> (PartSingle pa :)) _song unusedParts
      countinPath = biRelative buildInfo $ "gen/plan" </> T.unpack planName </> "countin.wav"
      in do
        unusedSrcs <- mapM (buildPartAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec . Just) partAudios
        if includeCountin
          then do
            countinSrc <- shk $ buildSource $ Input countinPath
            return $ foldr mix countinSrc unusedSrcs
          else case unusedSrcs of
            []     -> buildPartAudioToSpec (biYamlDir buildInfo) (biAudioLib buildInfo) (audioDepend buildInfo) (biSongYaml buildInfo) spec Nothing
            s : ss -> return $ foldr mix s ss
  return $ padAudio pad $ applyTargetAudio tgt mid src

adjustSpec :: Bool -> [(Double, Double)] -> [(Double, Double)]
adjustSpec True  spec     = spec
adjustSpec False [(0, 0)] = [(0, 0)]
adjustSpec False _        = [(-1, 0), (1, 0)]

padAudio :: (Monad m) => Int -> AudioSource m Float -> AudioSource m Float
padAudio pad src = if frames src == 0
  then src
  else padStart (Seconds $ realToFrac pad) src

setAudioLength :: (Monad m) => U.Seconds -> AudioSource m Float -> AudioSource m Float
setAudioLength len src = let
  currentLength = fromIntegral (frames src) / rate src
  requiredLength = realToFrac len
  in case compare currentLength requiredLength of
    EQ -> src
    LT -> padEnd (Seconds $ requiredLength - currentLength) src
    GT -> takeStart (Seconds requiredLength) src

setAudioLengthOrEmpty :: (Monad m) => U.Seconds -> AudioSource m Float -> m (AudioSource m Float)
setAudioLengthOrEmpty secs src = do
  chans <- runConduit $ emptyChannels src
  return $ if length chans == channels src
    then src { frames = 0, source = return () }
    else setAudioLength secs src

-- Silences out an audio stream if more than 1 game part maps to the same flex part
zeroIfMultiple :: (Monad m) => [RBFile.FlexPartName] -> RBFile.FlexPartName -> AudioSource m Float -> AudioSource m Float
zeroIfMultiple fparts fpart src = case filter (== fpart) fparts of
  _ : _ : _ -> takeStart (Frames 0) src
  _         -> src

fullGenre :: SongYaml f -> FullGenre
fullGenre songYaml = interpretGenre
  (_genre    $ _metadata songYaml)
  (_subgenre $ _metadata songYaml)

loadRGB8 :: SongYaml FilePath -> Staction (Image PixelRGB8)
loadRGB8 songYaml = case _fileAlbumArt $ _metadata songYaml of
  Just img -> do
    shk $ need [img]
    stackIO $ if takeExtension img `elem` [".png_xbox", ".png_wii"]
      then pixelMap dropTransparency . readRBImage False <$> BL.readFile img
      else if takeExtension img == ".png_ps3"
        then pixelMap dropTransparency . readRBImage True <$> BL.readFile img
        else readImage img >>= \case
          Left  err -> fail $ "Failed to load cover art (" ++ img ++ "): " ++ err
          Right dyn -> return $ convertRGB8 dyn
  Nothing -> stackIO onyxAlbum

applyTargetAudio :: (MonadResource m) => TargetCommon -> RBFile.Song f -> AudioSource m Float -> AudioSource m Float
applyTargetAudio tgt mid = let
  eval = evalPreviewTime False Nothing mid 0 False -- TODO get Events track to support sections as segment boundaries
  bounds seg = liftA2 (,) (eval $ seg_FadeStart seg) (eval $ seg_FadeEnd seg)
  toDuration :: U.Seconds -> Duration
  toDuration = Seconds . realToFrac
  applyEnd = case tgt_End tgt >>= bounds of
    Nothing           -> id
    Just (start, end) -> fadeEnd (toDuration $ end - start) . takeStart (toDuration end)
  applyStart = case tgt_Start tgt >>= bounds of
    Nothing           -> id
    Just (start, end) -> fadeStart (toDuration $ end - start) . dropStart (toDuration start)
  applySpeed = applySpeedAudio tgt
  in applySpeed . applyStart . applyEnd

applySpeedAudio :: (MonadResource m) => TargetCommon -> AudioSource m Float -> AudioSource m Float
applySpeedAudio tgt = case fromMaybe 1 $ tgt_Speed tgt of
  1 -> id
  n -> stretchFull (1 / n) 1

data NameRule
  = NameRulePC -- mostly windows but also mac/linux
  | NameRuleXbox -- stfs files on hard drive. includes pc rules too

-- Smarter length trim that keeps 1x, 2x, 125, rb3con, etc. at end of name
makeLength :: Int -> T.Text -> T.Text
makeLength n t = if n >= T.length t
  then t
  else case reverse $ T.splitOn "_" t of
    lastPiece : rest@(_ : _) -> let
      (modifiers, notModifiers) = flip span rest $ \x ->
        x == "1x" || x == "2x" || T.all isDigit x || case T.uncons x of
          Just ('v', v) -> T.all isDigit v
          _             -> False
      base = T.intercalate "_" $ reverse notModifiers
      suffix = T.intercalate "_" $ reverse $ lastPiece : modifiers
      base' = T.dropWhileEnd (== '_') $ T.take (max 1 $ n - (T.length suffix + 1)) base
      in T.take n $ base' <> "_" <> suffix
    _ -> T.take n t

validFileNamePiece :: NameRule -> T.Text -> T.Text
validFileNamePiece rule s = let
  trimLength = case rule of
    NameRulePC   -> id
    NameRuleXbox -> makeLength 42
  invalidChars :: String
  invalidChars = "<>:\"/\\|?*" <> case rule of
    NameRulePC   -> ""
    NameRuleXbox -> "+," -- these are only invalid on hard drives? not usb drives apparently
  eachChar c = if isAscii c && not (isControl c) && notElem c invalidChars
    then c
    else '_'
  fixEnds = T.dropWhile isSpace . T.dropWhileEnd (\c -> isSpace c || c == '.')
  reserved =
    [ ""
    -- rest are invalid names on Windows
    , "CON", "PRN", "AUX", "NUL"
    , "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "COM0"
    , "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9", "LPT0"
    ]
  s' = fixEnds $ trimLength $ T.map eachChar s
  in if elem (T.toUpper s') reserved
    then s' <> "_"
    else s'

validFileName :: NameRule -> FilePath -> FilePath
validFileName rule f = let
  (dir, file) = splitFileName f
  in dir </> T.unpack (validFileNamePiece rule $ T.pack file)

makeShortName :: Int -> SongYaml f -> T.Text
makeShortName num songYaml
  = T.dropWhileEnd (== '_')
  -- Short name doesn't have to be name used in paths but it makes things simple.
  -- Max path name is 40 chars (stfs limit) - 14 chars ("_keep.png_xbox") = 26 chars.
  -- Also now used for GH3, which has a 27 char max (40 - 13 for "_song.pak.xen")
  $ T.take 26
  $ "o" <> T.pack (show num)
    <> "_" <> makePart (getTitle  $ _metadata songYaml)
    <> "_" <> makePart (getArtist $ _metadata songYaml)
  where makePart = T.toLower . T.filter (\c -> isAscii c && isAlphaNum c)

getPlan :: Maybe T.Text -> SongYaml f -> Maybe (T.Text, Plan f)
getPlan Nothing songYaml = case HM.toList $ _plans songYaml of
  [pair] -> Just pair
  _      -> Nothing
getPlan (Just p) songYaml = case HM.lookup p $ _plans songYaml of
  Just found -> Just (p, found)
  Nothing    -> Nothing
