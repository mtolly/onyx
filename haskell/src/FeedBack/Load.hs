{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module FeedBack.Load where

import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (SendMessage, StackTraceT,
                                                   fatal, inside, stackIO, warn)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Fixed                       (Fixed (..))
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
import qualified Numeric.NonNegative.Class        as NNC
import qualified Numeric.NonNegative.Wrapper      as NN
import           RockBand.Common                  (Difficulty (..),
                                                   LongNote (..))
import qualified RockBand.Events                  as Ev
import           RockBand.File
import qualified RockBand.FiveButton              as Five
import qualified RockBand.GHL                     as GHL
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

atomStr :: Atom -> T.Text
atomStr (Str s) = s
atomStr (Int i) = T.pack $ show i

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
  , FoF.delay = Map.lookup "Offset" song >>= atomInt
  -- could also get PreviewStart, PreviewEnd, Genre
  } where song = chartSong chart
          atomStr' x = case atomStr x of
            "" -> Nothing
            s  -> Just s
          atomInt (Int i) = Just $ fromIntegral i
          atomInt _       = Nothing

traverseWithAbsTime :: (Applicative m, Num t, NNC.C t) => (t -> a -> m b) -> RTB.T t a -> m (RTB.T t b)
traverseWithAbsTime f
  = fmap (RTB.fromAbsoluteEventList . ATB.fromPairList)
  . traverse (\(t, x) -> (,) t <$> f t x)
  . ATB.toPairList
  . RTB.toAbsoluteEventList NNC.zero

chartToMIDI :: (SendMessage m) => Chart U.Beats -> StackTraceT m (Song (PSFile U.Beats))
chartToMIDI chart = Song (getTempos chart) (getSignatures chart) <$> do
  let insideTrack name fn = inside (".chart track [" <> T.unpack name <> "]") $ do
        fn $ fromMaybe RTB.empty $ Map.lookup name $ chartTracks chart
      res = fromIntegral $ chartResolution chart
      insideEvents trk f = flip traverseWithAbsTime trk $ \t x -> do
        inside ("ticks: " <> show (round $ t * res :: Integer)) $ do
          f x
      -- TODO replace/finish with the functions/types from Guitars module
      parseGRYBO label = do
        let parseDiff diff = insideTrack (T.pack (show diff) <> label) $ \trk -> do
              fmap U.trackJoin $ insideEvents trk $ \case
                Note n len -> case n of
                  0 -> emitNote Five.Green
                  1 -> emitNote Five.Red
                  2 -> emitNote Five.Yellow
                  3 -> emitNote Five.Blue
                  4 -> emitNote Five.Orange
                  5 -> return RTB.empty -- TODO: force (flip from GH3 algorithm)
                  6 -> return RTB.empty -- TODO: tap
                  7 -> do
                    green <- emitNote Five.Green
                    let open = RTB.fromPairList
                          [ (0             , Five.DiffEvent diff $ Five.OpenNotes True )
                          , (max len (1/32), Five.DiffEvent diff $ Five.OpenNotes False)
                          ]
                    return $ RTB.merge green open
                  _ -> do
                    warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                    return RTB.empty
                  where emitNote col = case len of
                          0 -> return $ RTB.singleton 0 $ Five.DiffEvent diff $ Five.Note $ Blip () col
                          _ -> return $ RTB.fromPairList
                            [ (0  , Five.DiffEvent diff $ Five.Note $ NoteOn () col)
                            , (len, Five.DiffEvent diff $ Five.Note $ NoteOff   col)
                            ]
                Stream n len -> case diff of
                  Expert -> case n of
                    0 -> return $ RTB.fromPairList [(0, Five.Player1 True), (len, Five.Player1 False)]
                    1 -> return $ RTB.fromPairList [(0, Five.Player2 True), (len, Five.Player2 False)]
                    2 -> return $ RTB.fromPairList [(0, Five.Overdrive True), (len, Five.Overdrive False)]
                    _ -> do
                      warn $ "Unrecognized stream type: S " <> show n <> " " <> show len
                      return RTB.empty
                  _ -> return RTB.empty
                Event "solo" -> case diff of
                  Expert -> return $ RTB.singleton 0 $ Five.Solo True
                  _      -> return RTB.empty
                Event "soloend" -> case diff of
                  Expert -> return $ RTB.singleton 0 $ Five.Solo False
                  _      -> return RTB.empty
                _ -> return RTB.empty
        foldr RTB.merge RTB.empty <$> mapM parseDiff [Easy, Medium, Hard, Expert]
      parseGHL label = do
        let parseDiff diff = insideTrack (T.pack (show diff) <> label) $ \trk -> do
              fmap U.trackJoin $ insideEvents trk $ \case
                Note n len -> case n of
                  0 -> emitNote $ Just GHL.White1
                  1 -> emitNote $ Just GHL.White2
                  2 -> emitNote $ Just GHL.White3
                  3 -> emitNote $ Just GHL.Black1
                  4 -> emitNote $ Just GHL.Black2
                  5 -> return RTB.empty -- TODO: force (flip from GH3 algorithm)
                  6 -> return RTB.empty -- TODO: tap
                  7 -> emitNote Nothing
                  8 -> emitNote $ Just GHL.Black3
                  _ -> do
                    warn $ "Unrecognized note type: N " <> show n <> " " <> show len
                    return RTB.empty
                  where emitNote col = case len of
                          0 -> return $ RTB.singleton 0 $ GHL.DiffEvent diff $ GHL.Note $ Blip () col
                          _ -> return $ RTB.fromPairList
                            [ (0  , GHL.DiffEvent diff $ GHL.Note $ NoteOn () col)
                            , (len, GHL.DiffEvent diff $ GHL.Note $ NoteOff   col)
                            ]
                Stream n len -> case diff of
                  Expert -> case n of
                    0 -> return RTB.empty
                    1 -> return RTB.empty
                    2 -> return $ RTB.fromPairList [(0, GHL.Overdrive True), (len, GHL.Overdrive False)]
                    _ -> do
                      warn $ "Unrecognized stream type: S " <> show n <> " " <> show len
                      return RTB.empty
                  _ -> return RTB.empty
                Event "solo" -> case diff of
                  Expert -> return $ RTB.singleton 0 $ GHL.Solo True
                  _      -> return RTB.empty
                Event "soloend" -> case diff of
                  Expert -> return $ RTB.singleton 0 $ GHL.Solo False
                  _      -> return RTB.empty
                _ -> return RTB.empty
        foldr RTB.merge RTB.empty <$> mapM parseDiff [Easy, Medium, Hard, Expert]
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
