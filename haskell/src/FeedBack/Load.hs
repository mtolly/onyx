{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FeedBack.Load where

import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT,
                                                   fatal, inside, stackIO, warn)
import           Data.Bifunctor                   (first)
import           Data.Default.Class               (def)
import           Data.Either                      (isLeft)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Fixed (..), Milli)
import qualified Data.HashMap.Strict              as Map
import           Data.List                        (partition, sort)
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      (mconcat, (<>))
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as TIO
import           Data.Traversable                 (forM)
import           FeedBack.Base
import           FeedBack.Parse                   (parseStack)
import           FeedBack.Scan                    (scanStack)
import qualified FretsOnFire                      as FoF
import qualified Guitars                          as G
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..), splitEdges)
import qualified RockBand.Events                  as Ev
import           RockBand.File
import qualified RockBand.FiveButton              as Five
import qualified RockBand.GHL                     as GHL
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

atomStr :: Atom -> T.Text
atomStr (Str  s) = s
atomStr (Int  i) = T.pack $ show i
atomStr (Real r) = T.pack $ show (realToFrac r :: Milli)

parseSong :: (Monad m) => RawLines -> StackTraceT m (Map.HashMap T.Text Atom)
parseSong lns = fmap Map.fromList $ forM lns $ \(k, vs) -> do
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
  [Str "TS", Int i, Int d] -> return $ TimeSig i d
  [Str "B", Int i] -> return $ BPM $ MkFixed i
  [Str "A", Int i] -> return $ Anchor $ MkFixed i
  Str "E" : rest -> return $ Event $ T.unwords $ map atomStr rest
  [Str "N", Int fret, Int len] -> Note fret <$> readTicks len
  [Str "S", Int stype, Int len] -> Stream stype <$> readTicks len
  atoms -> fatal $ "Unrecognized track event: " <> show atoms

parseChart :: (Monad m) => [RawSection] -> StackTraceT m (Chart Ticks)
parseChart sects = do
  let (songs, notSongs) = partition (("Song" ==) . fst) sects
  song <- mconcat <$> mapM (parseSong . snd) songs
  trks <- fmap Map.fromList $ forM notSongs $ \(name, lns) -> do
    inside (".chart track [" <> T.unpack name <> "]") $ do
      trk <- parseTrack lns
      return (name, trk)
  return $ Chart song trks

chartResolution :: Chart t -> Integer
chartResolution chart = case Map.lookup "Resolution" $ chartSong chart of
  Just (Int n) -> n
  _            -> 192

chartToBeats :: Chart Ticks -> Chart U.Beats
chartToBeats chart = let
  res = fromIntegral $ chartResolution chart
  in fmap (\tks -> fromIntegral tks / res) chart

loadChartFile :: (MonadIO m) => FilePath -> StackTraceT m (Chart Ticks)
loadChartFile fp = inside ("Loading .chart file: " <> fp) $ do
  str <- stackIO $ TIO.readFile fp
  scanStack str >>= parseStack >>= parseChart

getTempos :: Chart U.Beats -> U.TempoMap
getTempos
  = U.tempoMapFromBPS
  . RTB.mapMaybe (\case BPM bpm -> Just (realToFrac bpm / 60 :: U.BPS); _ -> Nothing)
  . fromMaybe RTB.empty
  . Map.lookup "SyncTrack"
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
  . Map.lookup "SyncTrack"
  . chartTracks

chartToIni :: Chart t -> FoF.Song
chartToIni chart = def
  { FoF.name = Map.lookup "Name" song >>= atomStr'
  , FoF.artist = Map.lookup "Artist" song >>= atomStr'
  , FoF.charter = Map.lookup "Charter" song >>= atomStr'
  , FoF.year = Map.lookup "Year" song >>= \case
    Int i -> Just $ fromIntegral i
    Str s -> T.stripPrefix ", " s >>= readMaybe . T.unpack
    Real r -> Just $ floor r
  , FoF.delay = fmap (floor . (* 1000)) $ Map.lookup "Offset" song >>= atomReal
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
  | TrackP1 t
  | TrackP2 t
  | TrackOD t
  | TrackSolo Bool
  deriving (Eq, Ord, Show, Read)

emitTrack :: (NNC.C t, Ord a) => t -> RTB.T t (TrackEvent t a) -> RTB.T t (LongNote (Five.StrumHOPO, Bool) a)
emitTrack hopoThreshold trk = let
  gnotes = fmap G.Note $ splitEdges $ flip RTB.mapMaybe trk $ \case
    TrackNote x len -> Just ((), x, guard (len /= NNC.zero) >> Just len)
    _               -> Nothing
  gh = G.strumHOPOTap G.HOPOsGH3 hopoThreshold gnotes
  forces = RTB.mapMaybe (\case TrackForce t -> Just t; _ -> Nothing) trk
  taps   = RTB.mapMaybe (\case TrackTap   t -> Just t; _ -> Nothing) trk
  applied = applyChartSwitch forces $ applyChartSwitch taps gh
  flipSH Five.Strum = Five.HOPO
  flipSH Five.HOPO  = Five.Strum
  in flip fmap applied $ \(forced, (tap, ln)) ->
    first (\(sh, _) -> (if forced then flipSH sh else sh, tap)) ln

chartToMIDI :: (SendMessage m) => Chart U.Beats -> StackTraceT m (Song (PSFile U.Beats))
chartToMIDI chart = Song (getTempos chart) (getSignatures chart) <$> do
  let insideTrack name fn = inside (".chart track [" <> T.unpack name <> "]") $ do
        fn $ fromMaybe RTB.empty $ Map.lookup name $ chartTracks chart
      res = fromIntegral $ chartResolution chart
      insideEvents trk f = flip traverseWithAbsTime trk $ \t x -> do
        inside ("ticks: " <> show (round $ t * res :: Integer)) $ do
          f x
      hopoThreshold = 1/3 -- default threshold according to moonscraper
      eachEvent evt parseNote = case evt of
        Note n len -> parseNote n len
        Stream n len -> case n of
          0 -> return $ Just $ TrackP1 len
          1 -> return $ Just $ TrackP2 len
          2 -> return $ Just $ TrackOD len
          _ -> do
            warn $ "Unrecognized stream type: S " <> show n <> " " <> show len
            return Nothing
        Event "solo"    -> return $ Just $ TrackSolo True
        Event "soloend" -> return $ Just $ TrackSolo False
        _ -> return Nothing
      parseGRYBO label = foldr RTB.merge RTB.empty <$> do
        forM [Easy, Medium, Hard, Expert] $ \diff -> do
          parsed <- insideTrack (T.pack (show diff) <> label) $ \trk -> do
            fmap RTB.catMaybes $ insideEvents trk $ \evt -> do
              eachEvent evt $ \n len -> case n of
                0 -> return $ Just $ TrackNote (Just Five.Green ) len
                1 -> return $ Just $ TrackNote (Just Five.Red   ) len
                2 -> return $ Just $ TrackNote (Just Five.Yellow) len
                3 -> return $ Just $ TrackNote (Just Five.Blue  ) len
                4 -> return $ Just $ TrackNote (Just Five.Orange) len
                5 -> return $ Just $ TrackForce len
                6 -> return $ Just $ TrackTap len
                7 -> return $ Just $ TrackNote Nothing len
                _ -> do
                  warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                  return Nothing
          return $ RTB.merge
            (fmap (Five.DiffEvent diff) $ G.emit5 $ emitTrack hopoThreshold parsed)
            $ case diff of
              Expert -> U.trackJoin $ flip fmap parsed $ \case
                TrackP1 t -> RTB.fromPairList [(0, Five.Player1   True), (t, Five.Player1   False)]
                TrackP2 t -> RTB.fromPairList [(0, Five.Player2   True), (t, Five.Player2   False)]
                TrackOD t -> RTB.fromPairList [(0, Five.Overdrive True), (t, Five.Overdrive False)]
                TrackSolo b -> RTB.singleton 0 $ Five.Solo b
                _ -> RTB.empty
              _ -> RTB.empty
      parseGHL label = foldr RTB.merge RTB.empty <$> do
        forM [Easy, Medium, Hard, Expert] $ \diff -> do
          parsed <- insideTrack (T.pack (show diff) <> label) $ \trk -> do
            fmap RTB.catMaybes $ insideEvents trk $ \evt -> do
              eachEvent evt $ \n len -> case n of
                0 -> return $ Just $ TrackNote (Just GHL.White1) len
                1 -> return $ Just $ TrackNote (Just GHL.White2) len
                2 -> return $ Just $ TrackNote (Just GHL.White3) len
                3 -> return $ Just $ TrackNote (Just GHL.Black1) len
                4 -> return $ Just $ TrackNote (Just GHL.Black2) len
                5 -> return $ Just $ TrackForce len
                6 -> return $ Just $ TrackTap len
                7 -> return $ Just $ TrackNote Nothing len
                8 -> return $ Just $ TrackNote (Just GHL.Black3) len
                _ -> do
                  warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                  return Nothing
          return $ RTB.merge
            (fmap (GHL.DiffEvent diff) $ G.emit6 $ emitTrack hopoThreshold parsed)
            $ case diff of
              Expert -> U.trackJoin $ flip fmap parsed $ \case
                TrackP1 _ -> RTB.empty
                TrackP2 _ -> RTB.empty
                TrackOD t -> RTB.fromPairList [(0, GHL.Overdrive True), (t, GHL.Overdrive False)]
                TrackSolo b -> RTB.singleton 0 $ GHL.Solo b
                _ -> RTB.empty
              _ -> RTB.empty
  psPartGuitar       <- parseGRYBO "Single" -- ExpertSingle etc.
  psPartGuitarGHL    <- parseGHL "GHLGuitar" -- ExpertGHLGuitar etc.
  psPartBass         <- parseGRYBO "DoubleBass" -- ExpertDoubleBass etc.
  psPartBassGHL      <- return RTB.empty -- don't know!
  psPartKeys         <- parseGRYBO "Keyboard" -- ExpertKeyboard etc.
  psPartRhythm       <- return RTB.empty -- ExpertDoubleBass when Player2 = rhythm ???
  psPartGuitarCoop   <- return RTB.empty -- ExpertDoubleGuitar ???
  psEvents           <- insideTrack "Events" $ \trk -> do
    return $ fmap Ev.SectionRB2 $ flip RTB.mapMaybe trk $ \case
      Event t -> T.stripPrefix "section " t
      _       -> Nothing
  let psPartDrums        = RTB.empty -- ExpertDrums etc.
      psPartDrums2x      = RTB.empty
      psPartRealDrumsPS  = RTB.empty
      psPartRealGuitar   = RTB.empty
      psPartRealGuitar22 = RTB.empty
      psPartRealBass     = RTB.empty
      psPartRealBass22   = RTB.empty
      psPartRealKeysE    = RTB.empty
      psPartRealKeysM    = RTB.empty
      psPartRealKeysH    = RTB.empty
      psPartRealKeysX    = RTB.empty
      psPartRealKeysPS_E = RTB.empty
      psPartRealKeysPS_M = RTB.empty
      psPartRealKeysPS_H = RTB.empty
      psPartRealKeysPS_X = RTB.empty
      psPartKeysAnimLH   = RTB.empty
      psPartKeysAnimRH   = RTB.empty
      psPartVocals       = RTB.empty
      psHarm1            = RTB.empty
      psHarm2            = RTB.empty
      psHarm3            = RTB.empty
      psBeat             = RTB.empty
      psVenue            = RTB.empty
  return PSFile{..}
