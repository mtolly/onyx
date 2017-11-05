{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module FeedBack.Load where

import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.StackTrace   (StackTraceT, fatal, inside,
                                                   stackIO)
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
import qualified Numeric.NonNegative.Wrapper      as NN
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
  [Str "TS", Int i] -> return $ TimeSig i
  [Str "B", Int i] -> return $ BPM $ MkFixed i
  [Str "A", Int i] -> return $ Anchor $ MkFixed i
  [Str "E", Str s] -> return $ Event s
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

chartToBeats :: Chart Ticks -> Chart U.Beats
chartToBeats chart = let
  res = case Map.lookup "Resolution" $ chartSong chart of
    Just (Int n) -> fromIntegral n
    _            -> 192
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
  . RTB.mapMaybe (\case TimeSig n -> Just $ U.TimeSig (fromIntegral n) 1; _ -> Nothing)
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
