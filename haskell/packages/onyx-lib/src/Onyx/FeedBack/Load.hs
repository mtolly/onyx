{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.FeedBack.Load where

import           Control.Monad                    (guard, void)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bifunctor                   (first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (toLower)
import           Data.Default.Class               (def)
import           Data.Either                      (isLeft)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Fixed (..), Milli)
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd, partition, sort)
import qualified Data.Map                         as Map
import           Data.Maybe                       (fromMaybe)
import qualified Data.Text                        as T
import           Data.Traversable                 (forM)
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           Onyx.FeedBack.Base
import           Onyx.FeedBack.Parse              (parseStack)
import           Onyx.FeedBack.Scan               (scanStack)
import qualified Onyx.FretsOnFire                 as FoF
import qualified Onyx.Guitar                      as G
import           Onyx.MIDI.Common
import qualified Onyx.MIDI.Track.Drums            as D
import           Onyx.MIDI.Track.Events
import           Onyx.MIDI.Track.File
import           Onyx.MIDI.Track.FiveFret
import           Onyx.MIDI.Track.SixFret
import           Onyx.MIDI.Track.Vocal
import           Onyx.Sections                    (simpleSection)
import           Onyx.StackTrace                  (SendMessage, StackTraceT,
                                                   fatal, inside, stackIO, warn)
import           Onyx.Util.Handle                 (Readable, handleToByteString,
                                                   useHandle)
import           Onyx.Util.Text.Decode            (decodeGeneral)
import qualified Sound.MIDI.File                  as F
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeExtension)
import           Text.Read                        (readMaybe)

atomStr :: Atom -> T.Text
atomStr (Str  s) = s
atomStr (Int  i) = T.pack $ show i
atomStr (Real r) = T.pack $ show (realToFrac r :: Milli)

parseSong :: (Monad m) => RawLines -> StackTraceT m (HM.HashMap T.Text Atom)
parseSong lns = fmap HM.fromList $ forM lns $ \(k, vs) -> do
  let key = atomStr k
      val = case vs of
        [atom] -> atom
        _      -> Str $ T.unwords $ map atomStr vs
  return (key, val)

readTicks :: (Monad m) => Integer -> StackTraceT m Ticks
readTicks i = if i < 0
  then fatal $ "Invalid negative number: " <> show i
  else return $ NN.fromNumber i

parseTrack :: (Monad m) => RawLines -> StackTraceT m (RTB.T Ticks (Event Ticks))
parseTrack lns = do
  parsed <- forM lns $ \(k, vs) -> do
    time <- case k of
      Int i -> readTicks i
      Str s -> fatal $ "Track event has non-number timestamp: " <> show s
      Real r -> fatal $ "Track event has fractional timestamp: " <> show (realToFrac r :: Milli)
    evt <- inside ("ticks: " <> show time) $ parseEvent vs
    return (time, evt)
  return $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort parsed

parseEvent :: (Monad m) => [Atom] -> StackTraceT m (Event Ticks)
parseEvent = \case
  [Str "TS", Int i] -> return $ TimeSig i 2
  -- I haven't seen this form in published charts, but it is written
  -- for non-*/4 signatures by https://github.com/EFHIII/midi-ch
  -- For example 7/8 becomes "TS 3.5"
  [Str "TS", Real r] -> case U.showSignature $ realToFrac r of
    Just (E.MetaEvent (Meta.TimeSig n d _ _)) -> return $ TimeSig (fromIntegral n) (fromIntegral d)
    _                                         -> fatal $
      "Unsupported fractional time signature (non-base-2 denominator): " <> show r
  [Str "TS", Int i, Int d] -> return $ TimeSig i d
  [Str "B", Int i] -> return $ BPM $ MkFixed i
  [Str "A", Int i] -> return $ Anchor $ MkFixed i
  Str "E" : rest -> return $ Event $ T.unwords $ map atomStr rest
  [Str "N", Int fret, Int len] -> Note fret <$> readTicks len
  [Str "S", Int stype, Int len] -> Special stype <$> readTicks len
  atoms -> fatal $ "Unrecognized track event: " <> show atoms

parseChart :: (Monad m) => [RawSection] -> StackTraceT m (Chart Ticks)
parseChart sects = do
  let (songs, notSongs) = partition (("Song" ==) . fst) sects
  song <- mconcat <$> mapM (parseSong . snd) songs
  trks <- fmap HM.fromList $ forM notSongs $ \(name, lns) -> do
    inside (".chart track [" <> T.unpack name <> "]") $ do
      trk <- parseTrack lns
      return (name, trk)
  return $ Chart song trks

chartResolution :: Chart t -> Integer
chartResolution chart = case HM.lookup "Resolution" $ chartSong chart of
  Just (Int n) -> n
  _            -> 192

chartToBeats :: Chart Ticks -> Chart U.Beats
chartToBeats chart = let
  res = fromIntegral $ chartResolution chart
  in fmap (\tks -> fromIntegral tks / res) chart

loadChartFile :: (MonadIO m) => FilePath -> StackTraceT m (Chart Ticks)
loadChartFile fp = inside ("Loading .chart file: " <> fp) $ do
  str <- stackIO $ decodeGeneral <$> B.readFile fp
  scanStack str >>= parseStack >>= parseChart

loadChartReadable :: (MonadIO m) => Readable -> StackTraceT m (Chart Ticks)
loadChartReadable r = inside "Loading .chart file" $ do
  str <- stackIO $ decodeGeneral . BL.toStrict <$> useHandle r handleToByteString
  scanStack str >>= parseStack >>= parseChart

getTempos :: Chart U.Beats -> U.TempoMap
getTempos
  = U.tempoMapFromBPS
  . RTB.mapMaybe (\case BPM bpm -> Just (realToFrac bpm / 60 :: U.BPS); _ -> Nothing)
  . fromMaybe RTB.empty
  . HM.lookup "SyncTrack"
  . chartTracks

getSignatures :: Chart U.Beats -> U.MeasureMap
getSignatures
  = U.measureMapFromTimeSigs U.Truncate
  . RTB.mapMaybe (\case
      TimeSig n d -> let
        unit = 4 / (2 ^ d)
        len = fromIntegral n * unit
        in Just $ U.TimeSig (U.Beats len) (U.Beats unit)
      _ -> Nothing
    )
  . fromMaybe RTB.empty
  . HM.lookup "SyncTrack"
  . chartTracks

chartToIni :: Chart t -> FoF.Song
chartToIni chart = def
  { FoF.name = HM.lookup "Name" song >>= atomStr'
  , FoF.artist = HM.lookup "Artist" song >>= atomStr'
  , FoF.album = HM.lookup "Album" song >>= atomStr'
  , FoF.genre = HM.lookup "Genre" song >>= atomStr'
  , FoF.charter = HM.lookup "Charter" song >>= atomStr'
  , FoF.year = HM.lookup "Year" song >>= \case
    Int i  -> Just $ fromIntegral i
    Str s  -> T.stripPrefix ", " s >>= readMaybe . T.unpack
    Real r -> Just $ floor r
  , FoF.delay = fmap (floor . (* 1000)) $ HM.lookup "Offset" song >>= atomReal
  -- could also get PreviewStart, PreviewEnd, Genre
  } where song = chartSong chart
          atomStr' x = case atomStr x of
            "" -> Nothing
            s  -> Just s
          atomReal :: Atom -> Maybe Rational
          atomReal (Int  i) = Just $ fromIntegral i
          atomReal (Real r) = Just r
          atomReal _        = Nothing

traverseWithAbsTime :: (Applicative m, Num t, NNC.C t) => (t -> a -> m b) -> RTB.T t a -> m (RTB.T t b)
traverseWithAbsTime f
  = fmap (RTB.fromAbsoluteEventList . ATB.fromPairList)
  . traverse (\(t, x) -> (,) t <$> f t x)
  . ATB.toPairList
  . RTB.toAbsoluteEventList NNC.zero

-- | Apply force and tap notes to modify a note stream
applyChartSwitch :: (NNC.C t, Ord a) => RTB.T t t -> RTB.T t a -> RTB.T t (Bool, a)
applyChartSwitch switch rtb = let
  (zero, nonzero) = RTB.partition (== NNC.zero) switch
  nonzero' = U.trackJoin $ flip fmap nonzero $ \len -> RTB.fromPairList
    [ (NNC.zero, ((), True ))
    , (len     , ((), False))
    ]
  appliedNonzero = first (not . null) <$> G.applyStatus nonzero' rtb
  appliedZero
    = RTB.flatten
    $ fmap applyZeroInstant
    $ RTB.collectCoincident
    $ RTB.merge (fmap Left zero) (fmap Right appliedNonzero)
  applyZeroInstant evts = do
    Right (b, x) <- evts
    return (b || any isLeft evts, x)
  in appliedZero

data TrackEvent t a
  = TrackNote a t
  | TrackForce t
  | TrackTap t
  | TrackCymbal D.ProColor
  | TrackKick2x
  | TrackP1 t
  | TrackP2 t
  | TrackOD t
  | TrackActivation t
  | TrackSolo Bool
  | TrackAccent a
  | TrackGhost a
  | TrackSingleLane t
  | TrackDoubleLane t
  deriving (Eq, Ord, Show, Functor, Foldable)

emitTrack :: (NNC.C t, Ord a) => t -> RTB.T t (TrackEvent t a) -> RTB.T t ((a, StrumHOPOTap), Maybe t)
emitTrack hopoThreshold trk = let
  -- we call nub for each note group in case of two copies of a note (seen in the wild)
  gnotes = RTB.flatten $ fmap nubOrd $ RTB.collectCoincident $ flip RTB.mapMaybe trk $ \case
    TrackNote x len -> Just (x, guard (len /= NNC.zero) >> Just len)
    _               -> Nothing
  gh = G.strumHOPOTap G.HOPOsGH3 hopoThreshold gnotes
  forces = RTB.mapMaybe (\case TrackForce t -> Just t; _ -> Nothing) trk
  taps   = RTB.mapMaybe (\case TrackTap   t -> Just t; _ -> Nothing) trk
  applied = applyChartSwitch forces $ applyChartSwitch taps gh
  flipSH Strum = HOPO
  flipSH HOPO  = Strum
  flipSH Tap   = Tap
  in flip fmap applied $ \(forced, (tap, ((color, sht), len))) ->
    ((color, if tap then Tap else if forced then flipSH sht else sht), len)

chartToMIDI :: (SendMessage m) => Chart U.Beats -> StackTraceT m (Song (FixedFile U.Beats))
chartToMIDI chart = Song (getTempos chart) (getSignatures chart) <$> do
  let insideTrack name fn = inside (".chart track [" <> T.unpack name <> "]") $ do
        fn $ fromMaybe RTB.empty $ HM.lookup name $ chartTracks chart
      res = fromIntegral $ chartResolution chart
      insideEvents trk f = flip traverseWithAbsTime trk $ \t x -> do
        inside ("ticks: " <> show (round $ t * res :: Integer)) $ do
          f x
      hopoThreshold = 65/192 -- default threshold according to moonscraper
      eachEvent evt parseNote = case evt of
        Note n len -> parseNote n len
        Special n len -> let
          len' = if len == 0 then 1/4 else len -- I guess S 2 0 means "instant SP phrase"?
          in case n of
            0 -> return $ Just $ TrackP1 len'
            1 -> return $ Just $ TrackP2 len'
            2 -> return $ Just $ TrackOD len'
            64 -> return $ Just $ TrackActivation len'
            65 -> return $ Just $ TrackSingleLane len'
            66 -> return $ Just $ TrackDoubleLane len'
            _ -> do
              warn $ "Unrecognized special type: S " <> show n <> " " <> show len
              return Nothing
        Event "solo"    -> return $ Just $ TrackSolo True
        Event "soloend" -> return $ Just $ TrackSolo False
        _ -> return Nothing
      -- some songs start a new solo without ending the previous one
      fixBackToBackSolos :: (NNC.C t, Ord a) => RTB.T t (TrackEvent t a) -> RTB.T t (TrackEvent t a)
      fixBackToBackSolos = go False . RTB.normalize where
        go False RNil             = RNil -- normal end
        go True  RNil             = Wait NNC.zero (TrackSolo False) RNil -- solo goes to end of song without ending
        go b     (Wait dt x rest) = case x of
          TrackSolo False -> if b
            then Wait dt x $ go False rest
            else RTB.delay dt $ go False rest -- ended a solo without starting one...?
          TrackSolo True  -> if b
            then Wait dt (TrackSolo False) $ Wait NNC.zero x $ go True rest
            else                             Wait dt       x $ go True rest
          _               -> Wait dt x $ go b rest
      lengthToBools t = RTB.fromPairList [(0, True), (t, False)]
      parseGRYBO label = do
        diffs <- fmap Map.fromList $ forM [Easy, Medium, Hard, Expert] $ \diff -> do
          parsed <- insideTrack (T.pack (show diff) <> label) $ \trk -> do
            fmap RTB.catMaybes $ insideEvents trk $ \evt -> do
              eachEvent evt $ \n len -> case n of
                0 -> return $ Just $ TrackNote (Just Green ) len
                1 -> return $ Just $ TrackNote (Just Red   ) len
                2 -> return $ Just $ TrackNote (Just Yellow) len
                3 -> return $ Just $ TrackNote (Just Blue  ) len
                4 -> return $ Just $ TrackNote (Just Orange) len
                5 -> return $ Just $ TrackForce len
                6 -> return $ Just $ TrackTap len
                7 -> return $ Just $ TrackNote Nothing len
                _ -> do
                  warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                  return Nothing
          return (diff, parsed)
        let expert = fixBackToBackSolos $ fromMaybe RTB.empty $ Map.lookup Expert diffs
        return mempty
          { fiveOverdrive = U.trackJoin $ flip fmap expert
            $ \case TrackOD t -> lengthToBools t; _ -> RTB.empty
          , fiveSolo = flip RTB.mapMaybe expert
            $ \case TrackSolo b -> Just b; _ -> Nothing
          , fivePlayer1 = U.trackJoin $ flip fmap expert
            $ \case TrackP1 t -> lengthToBools t; _ -> RTB.empty
          , fivePlayer2 = U.trackJoin $ flip fmap expert
            $ \case TrackP2 t -> lengthToBools t; _ -> RTB.empty
          , fiveDifficulties
            = G.emit5'
            . G.noOpenExtSustains G.standardBlipThreshold G.standardSustainGap
            . emitTrack hopoThreshold
            <$> diffs
          -- these aren't currently supported by CH (only drum lanes) but could be in the future
          , fiveTremolo = fmap (\b -> guard b >> Just LaneExpert) $ U.trackJoin $ flip fmap expert
            $ \case TrackSingleLane t -> lengthToBools t; _ -> RTB.empty
          , fiveTrill = fmap (\b -> guard b >> Just LaneExpert) $ U.trackJoin $ flip fmap expert
            $ \case TrackDoubleLane t -> lengthToBools t; _ -> RTB.empty
          }
      parseGHL label = do
        diffs <- fmap Map.fromList $ forM [Easy, Medium, Hard, Expert] $ \diff -> do
          parsed <- insideTrack (T.pack (show diff) <> label) $ \trk -> do
            fmap RTB.catMaybes $ insideEvents trk $ \evt -> do
              eachEvent evt $ \n len -> case n of
                0 -> return $ Just $ TrackNote (Just White1) len
                1 -> return $ Just $ TrackNote (Just White2) len
                2 -> return $ Just $ TrackNote (Just White3) len
                3 -> return $ Just $ TrackNote (Just Black1) len
                4 -> return $ Just $ TrackNote (Just Black2) len
                5 -> return $ Just $ TrackForce len
                6 -> return $ Just $ TrackTap len
                7 -> return $ Just $ TrackNote Nothing len
                8 -> return $ Just $ TrackNote (Just Black3) len
                _ -> do
                  warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                  return Nothing
          return (diff, parsed)
        let expert = fixBackToBackSolos $ fromMaybe RTB.empty $ Map.lookup Expert diffs
        return (mempty :: SixTrack U.Beats)
          { sixOverdrive = U.trackJoin $ flip fmap expert
            $ \case TrackOD t -> lengthToBools t; _ -> RTB.empty
          , sixSolo = flip RTB.mapMaybe expert
            $ \case TrackSolo b -> Just b; _ -> Nothing
          , sixDifficulties = G.emit6' . emitTrack hopoThreshold <$> diffs
          }
      parseDrums label = do
        diffs <- fmap Map.fromList $ forM [Easy, Medium, Hard, Expert] $ \diff -> do
          parsed <- insideTrack (T.pack (show diff) <> label) $ \trk -> do
            fmap RTB.catMaybes $ insideEvents trk $ \evt -> do
              eachEvent evt $ \n len -> case n of

                -- first we assign green/orange assuming this is a 4-lane chart.
                -- then later if there are any orange notes, flip it so the lower numbers are orange and higher are green
                0 -> return $ Just $ TrackNote D.Kick              len
                1 -> return $ Just $ TrackNote D.Red               len
                2 -> return $ Just $ TrackNote (D.Pro D.Yellow ()) len
                3 -> return $ Just $ TrackNote (D.Pro D.Blue   ()) len
                4 -> return $ Just $ TrackNote (D.Pro D.Green  ()) len
                5 -> return $ Just $ TrackNote D.Orange            len
                32 -> return $ Just TrackKick2x

                33 -> return $ Just $ TrackAccent D.Kick
                34 -> return $ Just $ TrackAccent D.Red
                35 -> return $ Just $ TrackAccent (D.Pro D.Yellow ())
                36 -> return $ Just $ TrackAccent (D.Pro D.Blue   ())
                37 -> return $ Just $ TrackAccent (D.Pro D.Green  ())
                38 -> return $ Just $ TrackAccent D.Orange

                39 -> return $ Just $ TrackGhost D.Kick
                40 -> return $ Just $ TrackGhost D.Red
                41 -> return $ Just $ TrackGhost (D.Pro D.Yellow ())
                42 -> return $ Just $ TrackGhost (D.Pro D.Blue   ())
                43 -> return $ Just $ TrackGhost (D.Pro D.Green  ())
                44 -> return $ Just $ TrackGhost D.Orange

                -- From Moonscraper it appears cymbal modifier length should always be zero
                -- (can't stretch out a marker to cover multiple notes)
                -- Otherwise they work like .mid tom markers but in reverse (put next to note to make it a cymbal)
                66 -> return $ Just $ TrackCymbal D.Yellow
                67 -> return $ Just $ TrackCymbal D.Blue
                68 -> return $ Just $ TrackCymbal D.Green

                _ -> do
                  warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                  return Nothing
          let flipped5 = if any (\case TrackNote D.Orange _ -> True; _ -> False) parsed
                then flip fmap parsed $ \case
                  TrackNote D.Orange           len -> TrackNote (D.Pro D.Green ()) len
                  TrackNote (D.Pro D.Green ()) len -> TrackNote D.Orange           len
                  TrackAccent D.Orange             -> TrackAccent (D.Pro D.Green ())
                  TrackAccent (D.Pro D.Green ())   -> TrackAccent D.Orange
                  TrackGhost D.Orange              -> TrackGhost (D.Pro D.Green ())
                  TrackGhost (D.Pro D.Green ())    -> TrackGhost D.Orange
                  x                                -> x
                else parsed
          return (diff, flipped5)
        let expert = fixBackToBackSolos $ fromMaybe RTB.empty $ Map.lookup Expert diffs
        return (mempty :: D.DrumTrack U.Beats)
          { D.drumOverdrive = U.trackJoin $ flip fmap expert
            $ \case TrackOD t -> lengthToBools t; _ -> RTB.empty
          , D.drumActivation = U.trackJoin $ flip fmap expert
            $ \case TrackActivation t -> lengthToBools t; _ -> RTB.empty
          , D.drumSolo = flip RTB.mapMaybe expert
            $ \case TrackSolo b -> Just b; _ -> Nothing
          , D.drumPlayer1 = U.trackJoin $ flip fmap expert
            $ \case TrackP1 t -> lengthToBools t; _ -> RTB.empty
          , D.drumPlayer2 = U.trackJoin $ flip fmap expert
            $ \case TrackP2 t -> lengthToBools t; _ -> RTB.empty
          -- doesn't check kick accent (DrumTrack doesn't support yet)
          , D.drumKick2x = void $ RTB.filter (\case TrackKick2x -> True; _ -> False) expert
          , D.drumDifficulties = flip fmap diffs $ \parsed -> D.DrumDifficulty
            { drumMix         = RTB.empty
            , drumPSModifiers = RTB.empty
            , drumGems        = RTB.flatten $ flip fmap (RTB.collectCoincident parsed) $ \instant -> let
              notes   = [ n | TrackNote   n _ <- instant ]
              ghosts  = [ n | TrackGhost  n   <- instant ]
              accents = [ n | TrackAccent n   <- instant ]
              in flip map notes $ \note -> let
                vel | elem note ghosts  = D.VelocityGhost
                    | elem note accents = D.VelocityAccent
                    | otherwise         = D.VelocityNormal
                in (note, vel)
            }
          -- TODO this doesn't handle cymbal markers on E/M/H since .mid can't separate them by difficulty
          , D.drumToms = if any (\case TrackCymbal _ -> True; _ -> False) expert
            then let
              tomInstants = RTB.flatten $ flip fmap (RTB.collectCoincident expert) $ \evts -> let
                -- emit tom markers for each YBG note that doesn't have a cymbal marker
                ybg = [ c | TrackNote (D.Pro c ()) _ <- evts ]
                cymbals = [ c | TrackCymbal c <- evts ]
                in filter (`notElem` cymbals) ybg
              in U.trackJoin $ fmap (\c -> Wait 0 (c, D.Tom) $ Wait (1/32) (c, D.Cymbal) RNil) tomInstants
            else RTB.empty -- probably not pro authored
          -- ignoring lanes on hard for now
          , D.drumSingleRoll = fmap (\b -> guard b >> Just LaneExpert) $ U.trackJoin $ flip fmap expert
            $ \case TrackSingleLane t -> lengthToBools t; _ -> RTB.empty
          , D.drumDoubleRoll = fmap (\b -> guard b >> Just LaneExpert) $ U.trackJoin $ flip fmap expert
            $ \case TrackDoubleLane t -> lengthToBools t; _ -> RTB.empty
          , D.drumEnableDynamics = RTB.singleton 0 () -- so dynamics don't get removed by import process
          }
  fixedPartGuitar       <- parseGRYBO "Single" -- ExpertSingle etc.
  fixedPartGuitarGHL    <- parseGHL "GHLGuitar" -- ExpertGHLGuitar etc.
  fixedPartBass         <- parseGRYBO "DoubleBass" -- ExpertDoubleBass etc.
  fixedPartBassGHL      <- parseGHL "GHLBass" -- ExpertGHLBass etc.
  fixedPartKeys         <- parseGRYBO "Keyboard" -- ExpertKeyboard etc.
  fixedPartRhythm       <- parseGRYBO "DoubleRhythm" -- ExpertDoubleRhythm etc.
  fixedPartGuitarCoop   <- parseGRYBO "DoubleGuitar" -- ExpertDoubleGuitar etc.
  fixedPartDrums        <- parseDrums "Drums" -- ExpertDrums etc.
  fixedEvents           <- insideTrack "Events" $ \trk -> return mempty
    { eventsSections = fmap simpleSection $ flip RTB.mapMaybe trk $ \case
      Event t -> T.stripPrefix "section " t
      _       -> Nothing
    -- newly supported in CH 1.1 public test build. but maybe this will cause
    -- issues if people don't put activation (S 64) on non-drums instruments?
    , eventsCoda = flip RTB.mapMaybe trk $ \case
      Event "coda" -> Just ()
      _            -> Nothing
    }
  -- CH-format lyrics
  fixedPartVocals <- insideTrack "Events" $ \trk -> let
    lyrics = flip RTB.mapMaybe trk $ \case
      Event t -> flip fmap (T.stripPrefix "lyric " t) $ T.replace "’" "'" . T.strip
      -- TODO probably more char fixes, also maybe it should be moved to song compile time
      _       -> Nothing
    phrases = flip RTB.mapMaybe trk $ \case
      Event "phrase_start" -> Just True
      Event "phrase_end"   -> Just False
      _                    -> Nothing
    endPhrases True  (Wait dt (Left  True) rest) = Wait dt False $ Wait NNC.zero True $ endPhrases True rest
    endPhrases _     (Wait dt (Left  b   ) rest) = Wait dt b $ endPhrases b rest
    endPhrases b     (Wait dt (Right _   ) rest) = RTB.delay dt $ endPhrases b rest
    endPhrases True  RNil                        = Wait (0.5 :: U.Beats) False RNil
    endPhrases False RNil                        = RNil
    in return mempty
      { vocalLyrics = lyrics
      , vocalPhrase1 = endPhrases False $ RTB.normalize $ RTB.merge (fmap Left phrases) (fmap Right lyrics)
      }
  let fixedPartDrums2x      = mempty
      fixedPartRealDrumsPS  = mempty
      fixedPartTrueDrums    = mempty
      fixedPartRealGuitar   = mempty
      fixedPartRealGuitar22 = mempty
      fixedPartRealBass     = mempty
      fixedPartRealBass22   = mempty
      fixedPartRealKeysE    = mempty
      fixedPartRealKeysM    = mempty
      fixedPartRealKeysH    = mempty
      fixedPartRealKeysX    = mempty
      fixedPartKeysAnimLH   = mempty
      fixedPartKeysAnimRH   = mempty
      fixedPartDance        = mempty
      fixedHarm1            = mempty
      fixedHarm2            = mempty
      fixedHarm3            = mempty
      fixedLipsync1         = mempty
      fixedLipsync2         = mempty
      fixedLipsync3         = mempty
      fixedLipsync4         = mempty
      fixedLipsyncJohn      = mempty
      fixedLipsyncPaul      = mempty
      fixedLipsyncGeorge    = mempty
      fixedLipsyncRingo     = mempty
      fixedBeat             = mempty
      fixedVenue            = mempty
  return FixedFile{..}

loadChartAsRawMIDI :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (F.T T.Text)
loadChartAsRawMIDI f = do
  chart <- chartToBeats <$> loadChartFile f
  mid   <- chartToMIDI chart
  return $ showMIDIFile' mid

loadMIDIOrChart :: (SendMessage m, MonadIO m, ParseFile f) => FilePath -> StackTraceT m (Song (f U.Beats))
loadMIDIOrChart f = case map toLower $ takeExtension f of
  ".chart" -> loadChartAsRawMIDI f >>= readMIDIFile'
  _        -> loadMIDI f

loadRawMIDIOrChart :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m (F.T T.Text)
loadRawMIDIOrChart f = case map toLower $ takeExtension f of
  ".chart" -> loadChartAsRawMIDI f
  _        -> loadRawMIDI f
