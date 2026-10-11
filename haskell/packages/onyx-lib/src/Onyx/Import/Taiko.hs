{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- Import from PS2 Taiko games
module Onyx.Import.Taiko where

import           Control.Monad                    (guard, unless, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Binary.Get                  (getWord32le)
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower, toUpper)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Function                    ((&))
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sortOn)
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe, isJust,
                                                   listToMaybe, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           Onyx.Audio
import           Onyx.Import.Base
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.Namco.Taiko
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Taiko.Base
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import           Optics                           ((.~))
import qualified Sound.MIDI.Util                  as U

-- * Chart conversion

-- flatten measures into absolute times, merging runs of single-slot
-- roll notes that the older chart format uses into one roll each
taikoChartEvents :: [TaikoMeasure] -> [(U.Seconds, TaikoEvent U.Seconds)]
taikoChartEvents measures = mergeRolls $ sortOn fst do
  m <- measures
  n <- m.notes
  let posn = m.start + m.len * fromIntegral n.position / 48
  return (realToFrac posn, n)

mergeRolls :: [(U.Seconds, SheetNote)] -> [(U.Seconds, TaikoEvent U.Seconds)]
mergeRolls = go where
  go = \case
    [] -> []
    (t, n) : rest -> case n.noteType of
      NoteDon     -> (t, TaikoSingle TaikoCenter     ) : go rest
      NoteKa      -> (t, TaikoSingle TaikoRim        ) : go rest
      NoteBigDon  -> (t, TaikoSingle TaikoCenterLarge) : go rest
      NoteBigKa   -> (t, TaikoSingle TaikoRimLarge   ) : go rest
      NoteBalloon -> (t, TaikoDenden $ roll n) : go rest
      NoteRoll    -> extend TaikoRoll      t n rest
      NoteBigRoll -> extend TaikoRollLarge t n rest
  roll :: SheetNote -> U.Seconds
  roll n = realToFrac n.len
  -- older format writes roll as a run of one-slot notes, so join them back up
  -- if they carry straight on from the end of the previous one
  extend con t n rest = let
    absorb end = \case
      (t2, n2) : more
        | n2.noteType == n.noteType && t2 <= end + roll n2
        -> absorb (max end $ t2 + roll n2) more
      after -> (end, after)
    (end', after') = absorb (t + roll n) rest
    in (t, con $ end' - t) : go after'

{-
* Each MIDI bar starts where a chart bar starts
* Shorten a bar's time signature if the next bar starts early
* Anything not a clean number of half beats is stretched to 4 beats
* Bars that start before one already placed are left out
-}
taikoTiming :: [TaikoMeasure] -> (U.TempoMap, U.MeasureMap)
taikoTiming measures = let
  increasing :: Double -> [TaikoMeasure] -> [TaikoMeasure]
  increasing prev = \case
    m : ms | m.start > prev + 0.001 -> m : increasing m.start ms
           | otherwise              -> increasing prev ms
    []                              -> []
  bars = increasing (-1) measures
  -- (seconds, beats) for each bar of the MIDI grid
  segments :: [(Double, Double)]
  segments = leadIn <> zipWith barSegment bars (map (Just . (.start)) (drop 1 bars) <> [Nothing])
  leadIn = case bars of
    m : _ | m.start > 0.001 -> [(m.start, 4)]
    _                       -> []
  barSegment :: TaikoMeasure -> Maybe Double -> (Double, Double)
  barSegment m next = let
    secs = maybe m.len (subtract m.start) next
    beats = secs / m.len * 4
    halves = fromIntegral (round $ beats * 2 :: Integer) / 2 :: Double
    in if halves > 0 && abs (beats - halves) <= 0.08
      then (secs, halves)
      else (secs, 4)
  lengths = [ realToFrac beats :: U.Beats | (_, beats) <- segments ]
  tempos = U.tempoMapFromBPS $ RTB.fromPairList $ zip (0 : lengths)
    [ U.makeTempo (realToFrac beats) (realToFrac secs) | (secs, beats) <- segments ]
  timesigs = U.measureMapFromLengths U.Error $ RTB.fromPairList $ zip (0 : lengths) lengths
  in if null segments
    then (U.tempoMapFromBPS $ RTB.singleton 0 2, U.measureMapFromLengths U.Error $ RTB.singleton 0 4)
    else (tempos, timesigs)

-- * Audio

taikoADPCMtoVGS
  :: Int -- sample rate
  -> Int -- number of channels
  -> Int -- how many bytes is one channel chunk
  -> BL.ByteString
  -> BL.ByteString
taikoADPCMtoVGS rate numChannels interleave bs = let
  groupSize = fromIntegral $ numChannels * interleave
  -- round up so a final partial group isn't dropped
  totalGroups = (BL.length bs + groupSize - 1) `div` groupSize
  deinterleaved = [0 .. numChannels - 1] <&> \i -> BL.concat
    [ BL.take (fromIntegral interleave)
      $ BL.drop (group * groupSize + fromIntegral (i * interleave)) bs
    | group <- [0 .. totalGroups - 1]
    ]
  -- trim all channels to common length in case last group is short
  blocks = case deinterleaved of
    [] -> 0
    cs -> minimum (map BL.length cs) `div` 0x10
  chans = map (BL.take $ blocks * 0x10) deinterleaved
  in runPut do
    putByteString "VgS!"
    putWord32le 2
    mapM_
      (\_ -> putWord32le (fromIntegral rate) >> putWord32le (fromIntegral blocks))
      chans
    putByteString $ B.replicate (0x80 - 8 - 8 * numChannels) 0
    -- interleave 16-byte blocks, tag with channel
    let go left = if any BL.null left
          then return ()
          else do
            mapM_
              (\(i, chan) -> do
                putWord8 $ BL.head chan
                putWord8 i
                putLazyByteString $ BL.take 14 $ BL.drop 2 chan
              )
              (zip [0 ..] left)
            go $ map (BL.drop 0x10) left
    go chans

{-
.vag streams from newer games

- 16-byte header (group size, data size, channel count, sample rate) padded to one whole group
- note: group size is for a frame of chunks (one per channel)
-}
taikoStreamToVGS :: (MonadFail m) => BL.ByteString -> m BL.ByteString
taikoStreamToVGS bs = do
  (groupSize, dataSize, channels, rate) <- flip runGetM bs
    $ (,,,) <$> getWord32le <*> getWord32le <*> getWord32le <*> getWord32le
  unless (groupSize > 0 && channels > 0 && channels <= 8 && rate > 0 && groupSize `rem` (channels * 0x10) == 0)
    $ fail "unrecognized Taiko audio stream header"
  return $ taikoADPCMtoVGS
    (fromIntegral rate)
    (fromIntegral channels)
    (fromIntegral $ groupSize `quot` channels)
    (BL.take (fromIntegral dataSize) $ BL.drop (fromIntegral groupSize) bs)

-- * Importing

taikoDifficulties :: [(B.ByteString, T.Text, Int)]
taikoDifficulties =
  [ ("m", "Oni"   , 4)
  , ("h", "Hard"  , 3)
  , ("n", "Normal", 2)
  , ("e", "Easy"  , 1)
  ]

-- songs from disc using LIST.BIN + DATA.000
importTaikoListBin
  :: (SendMessage m, MonadIO m)
  => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importTaikoListBin _src folder = do
  list <- maybe (fatal "Couldn't find LIST.BIN") return $ findFileCI (pure "LIST.BIN") folder
  dat  <- maybe (fatal "Couldn't find DATA.000") return $ findFileCI (pure "DATA.000") folder
  tree <- stackIO $ listBinFolder list dat
  infos <- case findFolder ["gamedata"] tree of
    Just gd -> case lookup "musicinfo.bin" $ folderFiles gd of
      Just r -> do
        bs <- stackIO $ useHandle r handleToByteString
        errorToWarning (readMusicInfo bs) <&> fromMaybe []
      Nothing -> return []
    Nothing -> return []
  let charts = Map.fromList do
        (name, sub) <- folderSubfolders tree
        key <- toList $ B.stripPrefix "fumen." name >>= parseFumenName
        sht <- toList $ lookup "sht" $ folderFiles sub
        return (key, sht)
      hasAudio sid = isJust $ findFolder ["sound.stream.music_" <> sid] tree
      listed sid = any (\info -> info.songID == sid) infos
  -- skip charts that aren't songs, like Yondaime's relay-mode filler charts
  -- (rela1-3), which have no audio and aren't in the song list
  return $ flip map (filter (\sid -> hasAudio sid || listed sid) $ taikoSongIDs $ Map.keys charts) \sid ->
    importTaikoSong tree infos (\suffix -> Map.lookup (sid, suffix) charts) sid

-- songs with at least one normal chart
-- (skip tutorial which only has practice chart)
taikoSongIDs :: [(B.ByteString, B.ByteString)] -> [B.ByteString]
taikoSongIDs keys = nubOrd
  [ sid
  | (sid, suffix) <- keys
  , suffix `elem` [ s | (s, _, _) <- taikoDifficulties ]
  ]

-- split chart group name (after "fumen.") into song ID and difficulty letter,
-- keeping only 1P charts. most games name them <song>1p_<diff>
-- except Yondaime is <song>_<diff>
parseFumenName :: B.ByteString -> Maybe (B.ByteString, B.ByteString)
parseFumenName name = do
  let (beforeUnderscore, afterUnderscore) = B.breakEnd (== 0x5F) name
  suffix <- guard (not $ B.null afterUnderscore) >> return afterUnderscore
  withSide <- B.stripSuffix "_" beforeUnderscore
  sid <- case B.stripSuffix "1p" withSide of
    Just sid -> return sid
    Nothing  -> do
      guard $ not $ "2p" `B.isSuffixOf` withSide
      return withSide
  guard $ not $ B.null sid
  return (sid, suffix)

importTaikoSong
  :: (SendMessage m, MonadIO m)
  => Folder B.ByteString Readable
  -> [TaikoSongInfo]
  -> (B.ByteString -> Maybe Readable) -- ^ 1P chart by difficulty letter
  -> B.ByteString
  -> Import m
importTaikoSong tree infos chartFor sid level = do
  let info = listToMaybe [ i | i <- infos, i.songID == sid ]
      title = maybe (TE.decodeLatin1 sid) (.title) info
      audio = do
        sub <- findFolder ["sound.stream.music_" <> sid] tree
        lookup "vag" $ folderFiles sub
  (diff, chartFile) <- case mapMaybe
    (\(suffix, name, tier) -> (,) (name, tier) <$> chartFor suffix) taikoDifficulties of
    x : _ -> return x
    []    -> fatal $ "No charts found for song " <> show sid
  chart <- case level of
    ImportQuick -> return $ TaikoChart Nothing []
    ImportFull  -> do
      bs <- stackIO $ useHandle chartFile handleToByteString
      readFumen bs
  audioVGS <- case (level, audio) of
    (ImportFull, Just r) -> errorToWarning do
      bs <- stackIO $ useHandle r handleToByteString
      vgs <- taikoStreamToVGS bs
      return $ makeHandle "taiko audio" $ byteStringSimpleHandle vgs
    _ -> return Nothing
  samples <- case level of
    ImportQuick -> return []
    ImportFull  -> loadTaikoSamples
  return $ taikoSongYaml title diff (info >>= (.preview)) samples chart audioVGS

-- first game is 48000 kHz, rest of the early games are 44100 kHz
taikoImageAudioRate :: Folder T.Text Readable -> IO Int
taikoImageAudioRate folder = case findFileCI (pure "SYSTEM.CNF") folder of
  Nothing  -> return 44100
  Just cnf -> do
    bs <- BL.toStrict <$> useHandle cnf handleToByteString
    -- "BOOT2 = cdrom0:\SLPS_202.21;1"
    return $ if any (`B.isInfixOf` bs) taikoDiscs48kHz then 48000 else 44100

taikoDiscs48kHz :: [B.ByteString]
taikoDiscs48kHz = ["SLPS_202.21"] -- Tatacon de Dodon ga Don

-- songs from disc using IMAGE00.TBL + IMAGE00.DAT
importTaikoImage
  :: (SendMessage m, MonadIO m)
  => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importTaikoImage _src folder = do
  tbl <- maybe (fatal "Couldn't find IMAGE00.TBL") return
    $ findFileCI (pure "IMAGE00.TBL") folder
  dat <- maybe (fatal "Couldn't find IMAGE00.DAT") return
    $ findFileCI (pure "IMAGE00.DAT") folder
  tree <- stackIO $ imageFolder tbl dat
  -- names are like "SHEET/aba2_n.bin", but some discs use upper case
  let charts = Map.fromList do
        (dir, sub) <- folderSubfolders tree
        guard $ B8.map toUpper dir == "SHEET"
        (name, r) <- folderFiles sub
        base <- toList $ B.stripSuffix ".BIN" $ B8.map toUpper name
        let (beforeUnderscore, suffix) = B.breakEnd (== 0x5F) base
        sid <- toList $ B.stripSuffix "_" beforeUnderscore
        guard $ not (B.null sid) && not (B.null suffix)
        return ((sid, B8.map toLower suffix), r)
      -- song audio is in one of the BGM folders (different per game)
      audios = Map.fromList do
        (dir, sub) <- folderSubfolders folder
        guard $ "BGM" `T.isPrefixOf` T.toUpper dir
        (name, r) <- folderFiles sub
        base <- toList $ T.stripSuffix ".VAG" $ T.toUpper name
        return (TE.encodeUtf8 base, r)
  when (Map.null charts) $ fatal "Couldn't find any charts in IMAGE00.DAT"
  rate <- stackIO $ taikoImageAudioRate folder
  return $ flip map (taikoSongIDs $ Map.keys charts) \sid ->
    importTaikoImageSong
      rate
      (\suffix -> Map.lookup (sid, suffix) charts)
      (Map.lookup sid audios)
      sid

importTaikoImageSong
  :: (SendMessage m, MonadIO m)
  => Int -- ^ audio sample rate
  -> (B.ByteString -> Maybe Readable) -- ^ chart by difficulty letter
  -> Maybe Readable -- ^ song audio
  -> B.ByteString
  -> Import m
importTaikoImageSong rate chartFor audio sid level = do
  (diff, chartFile) <- case mapMaybe
    (\(suffix, name, tier) -> (,) (name, tier) <$> chartFor suffix) taikoDifficulties of
    x : _ -> return x
    []    -> fatal $ "No charts found for song " <> show sid
  chart <- case level of
    ImportQuick -> return $ TaikoChart Nothing []
    ImportFull  -> do
      bs <- stackIO $ useHandle chartFile handleToByteString
      readSheetBin bs
  audioVGS <- case (level, audio) of
    (ImportFull, Just r) -> errorToWarning do
      bs <- stackIO $ useHandle r handleToByteString
      -- headerless, always same interleave
      return $ makeHandle "taiko audio"
        $ byteStringSimpleHandle $ taikoADPCMtoVGS rate 2 0x8000 bs
    _ -> return Nothing
  samples <- case level of
    ImportQuick -> return []
    ImportFull  -> loadTaikoSamples
  -- no song title table on disc; use hardcoded table, fall back on lookup key
  let title = fromMaybe (T.toLower $ TE.decodeLatin1 sid) $ Map.lookup sid taikoImageSongTitles
  return $ taikoSongYaml title diff Nothing samples chart audioVGS

taikoSongYaml
  :: T.Text
  -> (T.Text, Int) -- ^ difficulty name + rough tier
  -> Maybe Double -- ^ preview start in seconds
  -> [(T.Text, FilePath, Readable)] -- ^ drum hit samples
  -> TaikoChart
  -> Maybe Readable
  -> SongYaml SoftFile
taikoSongYaml title (diffName, tier) preview samples chart audio = let
  (tempos, timesigs) = taikoTiming chart.measures
  events = RTB.fromAbsoluteEventList $ ATB.fromPairList $ taikoChartEvents chart.measures
  drums = taikoEventsToTrack tempos events
  branchName = \case
    BranchNormal   -> "normal"
    BranchAdvanced -> "advanced"
    BranchMaster   -> "master"
  drumSampleTrack = taikoKeySamples F.PartDrums
  in SongYaml
    { metadata = def'
      { title = Just title
      , comments = ["Chart difficulty: " <> diffName <> case chart.branch of
        Nothing -> ""
        Just b  -> " (" <> branchName b <> " branch)"
        ]
      , previewStart = PreviewSeconds . realToFrac <$> preview
      , fileAlbumArt = Nothing
      }
    , global = def'
      { fileMidi = SoftFile "notes.mid" $ SoftChart F.Song
        { F.tempos = tempos
        , F.timesigs = timesigs
        , F.tracks = mempty
          { F.onyxParts = Map.singleton F.PartDrums mempty
            { F.onyxPartDrums = drums
            }
          , F.onyxSamples = if null samples
            then Map.empty
            else Map.singleton drumSampleTrack $ taikoSampleTrack drums
          }
        }
      , fileMidiOriginal = Nothing
      , fileSongAnim = Nothing
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList let
      songAudio = toList audio <&> \r ->
        ("song", AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , commands = []
          , filePath = Just $ SoftFile "song.vgs" $ SoftReadable r
          , rate = Nothing
          , channels = 2
          })
      sampleAudio = samples <&> \(key, fileName, r) ->
        (key, AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , commands = []
          , filePath = Just $ SoftFile fileName $ SoftReadable r
          , rate = Nothing
          , channels = 2
          })
      sampleGroup = [ (drumSampleTrack, AudioSamples SamplesInfo
        { groupPolyphony = Nothing
        , groupCrossfade = 0
        }) | not $ null samples ]
      in songAudio <> sampleAudio <> sampleGroup
    , jammit = HM.empty
    , plans = case (audio, samples) of
      (Nothing, []) -> HM.empty
      _ -> HM.singleton "plan" $ StandardPlan StandardPlanInfo
        { song        = audio <&> \_ -> PansVols [-1, 1] [0, 0] $ Input $ Named "song"
        , parts       = Parts $ HM.fromList
          [ (F.PartDrums, PartSingle $ Input $ Named drumSampleTrack)
          | not $ null samples
          ]
        , crowd       = Nothing
        , comments    = []
        , tuningCents = 0
        , fileTempo   = Nothing
        }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.PartDrums
      $ (emptyPart :: Part SoftFile)
      & #drums .~ Just (emptyPartDrums Drums4 Kicks1x & #difficulty .~ Tier (fromIntegral tier))
    }
