{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.DTX where

import           Audio                            (applyPansVols)
import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Resource     (MonadResource)
import qualified Data.ByteString                  as B
import           Data.Char                        (isDigit, toLower)
import           Data.Conduit
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Mpg123        (sourceMpg)
import           Data.Conduit.Audio.SampleRate    (ConverterType (SincMediumQuality),
                                                   resampleTo)
import           Data.Conduit.Audio.Sndfile       (sourceSnd)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Data.Text                        as T
import qualified Data.Vector.Storable             as V
import           DTXMania.ShiftJIS                (decodeShiftJIS)
import           DTXMania.XA                      (sourceXA)
import           Numeric
import qualified Numeric.NonNegative.Wrapper      as NN
import           RockBand.Common                  (pattern RNil, pattern Wait)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath
import           Text.Read                        (readMaybe)

loadDTXLines :: FilePath -> IO [(T.Text, T.Text)]
loadDTXLines f = do
  lns <- lines . decodeShiftJIS <$> B.readFile f
  return $ flip mapMaybe lns $ \ln -> case ln of
    '#' : rest -> case T.splitOn ":" $ T.pack rest of
      x : xs@(_ : _) -> Just (T.strip x, T.strip $ T.intercalate ":" xs)
      _              -> Nothing
    _ -> Nothing
  -- TODO ignore from ; to end of line
  -- TODO : is not actually required! can just be "#KEY VALUE"

type BarNumber = Int
type Channel = T.Text
type Chip = T.Text

data DTX = DTX
  { dtx_TITLE      :: Maybe T.Text
  , dtx_ARTIST     :: Maybe T.Text
  , dtx_WAV        :: HM.HashMap Chip FilePath
  , dtx_VOLUME     :: HM.HashMap Chip Int
  , dtx_PAN        :: HM.HashMap Chip Int
  , dtx_MeasureMap :: U.MeasureMap
  , dtx_TempoMap   :: U.TempoMap
  , dtx_Drums      :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_DrumsDummy :: RTB.T U.Beats (DrumLane, Chip)
  , dtx_Guitar     :: RTB.T U.Beats (GuitarNote, Chip)
  , dtx_Bass       :: RTB.T U.Beats (GuitarNote, Chip)
  , dtx_BGM        :: RTB.T U.Beats Chip
  } deriving (Show)

textFloat :: (RealFrac a) => T.Text -> Maybe a
textFloat = fmap fst . listToMaybe . readFloat . T.unpack

textHex :: (Eq a, Num a) => T.Text -> Maybe a
textHex = fmap fst . listToMaybe . readHex . T.unpack

splitChips :: T.Text -> [T.Text]
splitChips t = if T.length t >= 2
  then case T.splitAt 2 t of
    (x, y) -> x : splitChips y
  else []

data DrumLane
  = HihatClose
  | Snare
  | BassDrum
  | HighTom
  | LowTom
  | Cymbal
  | FloorTom
  | HihatOpen
  | RideCymbal
  | LeftCymbal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

drumChar :: DrumLane -> Char
drumChar = \case
  HihatClose -> '1'
  Snare      -> '2'
  BassDrum   -> '3'
  HighTom    -> '4'
  LowTom     -> '5'
  Cymbal     -> '6'
  FloorTom   -> '7'
  HihatOpen  -> '8'
  RideCymbal -> '9'
  LeftCymbal -> 'A'
  -- TODO The Enemy Inside has 1B notes

data GuitarNote = Open | B | G | GB | R | RB | RG | RGB
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

guitarChar :: GuitarNote -> Char
guitarChar = head . show . fromEnum

readDTXLines :: [(T.Text, T.Text)] -> DTX
readDTXLines lns = DTX
  { dtx_TITLE      = lookup "TITLE" lns
  , dtx_ARTIST     = lookup "ARTIST" lns
  , dtx_WAV        = fmap T.unpack $ HM.fromList $ getReferences "WAV"
  , dtx_VOLUME     = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $ getReferences "VOLUME"
  , dtx_PAN        = HM.mapMaybe (readMaybe . T.unpack) $ HM.fromList $ getReferences "PAN"
  , dtx_MeasureMap = mmap
  , dtx_TempoMap   = tmap
  , dtx_Drums      = readDrums '1'
  , dtx_DrumsDummy = readDrums '3'
  , dtx_Guitar     = readGuitar '2'
  , dtx_Bass       = readGuitar 'A'
  , dtx_BGM        = foldr RTB.merge RTB.empty
    $ map (getChannel . T.pack . (\s -> case s of [_] -> '0' : s; _ -> s) . show)
    $ [1 :: Int] ++ [61..69] ++ [70..79] ++ [80..89] ++ [90..92]
  } where

    getReferences :: T.Text -> [(Chip, T.Text)]
    getReferences typ = flip mapMaybe lns $ \(k, v) -> (, v) <$> T.stripPrefix typ k

    isObjects :: T.Text -> Maybe (BarNumber, Channel)
    isObjects k = if T.length k == 5 && T.all isDigit (T.take 3 k)
      then Just (read $ T.unpack $ T.take 3 k, T.drop 3 k)
      else Nothing
    objects :: Map.Map BarNumber (HM.HashMap Channel [T.Text])
    objects
      = foldr (Map.unionWith $ HM.unionWith (++)) Map.empty
      $ flip mapMaybe lns
      $ \(k, v) -> flip fmap (isObjects k)
        $ \(bar, chan) -> Map.singleton bar $ HM.singleton chan [v]
    maxBar = maybe 0 fst $ Map.lookupMax objects

    mmap
      = U.measureMapFromLengths U.Error
      $ assembleMMap
      $ getBarLengths 4 0
    getBarLengths len n = if n > maxBar
      then []
      else let
        len' = maybe len (* 4) $
          Map.lookup n objects >>= HM.lookup "02" >>= listToMaybe >>= textFloat
        in len' : getBarLengths len' (n + 1)
    assembleMMap lens = foldr ($) RNil $ zipWith Wait (0 : lens) lens

    getChannel :: Channel -> RTB.T U.Beats Chip
    getChannel chans = foldr RTB.merge RTB.empty $ map (`readBar` chans) [0 .. maxBar]

    readBar :: BarNumber -> Channel -> RTB.T U.Beats Chip
    readBar bar chan = let
      barStart = U.unapplyMeasureMap mmap (bar, 0)
      barEnd = U.unapplyMeasureMap mmap (bar + 1, 0)
      barLen = barEnd - barStart
      placeLine xs = let
        chips = splitChips xs
        subdiv = barLen / fromIntegral (length chips)
        in  RTB.filter (/= "00")
          $ foldr ($) RNil
          $ zipWith Wait (barStart : repeat subdiv) chips
      in foldr RTB.merge RTB.empty
        $ map placeLine
        $ fromMaybe []
        $ Map.lookup bar objects >>= HM.lookup chan

    tmap
      = U.tempoMapFromBPS
      $ fmap (\bpm -> U.BPS $ bpm / 60)
      $ RTB.merge startBPM
      $ fmap (+ baseBPM)
      $ RTB.merge bpmHexes bpmRefs
    startBPM = maybe RTB.empty (RTB.singleton 0) $ lookup "BPM" lns >>= textFloat
    bpmHexes = RTB.mapMaybe textHex $ getChannel "03"
    bpmRefs = RTB.mapMaybe (`HM.lookup` refBPMs) $ getChannel "08"
    refBPMs = HM.mapMaybe textFloat $ HM.fromList $ getReferences "BPM"
    baseBPM = fromMaybe 0 $ lookup "BASEBPM" lns >>= textFloat

    readDrums col = foldr RTB.merge RTB.empty $ do
      lane <- [minBound .. maxBound]
      return $ fmap (lane,) $ getChannel $ T.pack [col, drumChar lane]

    readGuitar col = foldr RTB.merge RTB.empty $ do
      lane <- [minBound .. maxBound]
      return $ fmap (lane,) $ getChannel $ T.pack [col, guitarChar lane]

unvoid :: (Monad m) => ConduitT i Void m r -> ConduitT i o m r
unvoid = mapOutput $ \case {}

getChunk
  :: (Monad m)
  => Int
  -> SealedConduitT () (V.Vector Float) m ()
  -> ConduitT () o m (Maybe (SealedConduitT () (V.Vector Float) m ()), V.Vector Float)
getChunk n sc = do
  (sc', mv) <- unvoid $ sc =$$++ await
  case mv of
    Nothing -> return (Nothing, V.replicate n 0)
    Just v -> case compare (V.length v) n of
      EQ -> return (Just sc', v)
      LT -> do
        (msc, v') <- getChunk (n - V.length v) sc'
        return (msc, v <> v')
      GT -> do
        let (this, after) = V.splitAt n v
        (sc'', ()) <- unvoid $ sc' =$$++ leftover after
        return (Just sc'', this)

mixMany :: (Monad m) => Rate -> Channels -> RTB.T U.Seconds (AudioSource m Float) -> AudioSource m Float
mixMany r c srcs = let
  srcs' = RTB.discretize $ RTB.mapTime (* realToFrac r) srcs
  in AudioSource
    { rate = r
    , channels = c
    , frames = foldr max 0
      $ map (\(t, src) -> NN.toNumber t + frames src)
      $ ATB.toPairList
      $ RTB.toAbsoluteEventList 0 srcs'
    , source = let
      getFrames n sources = do
        results <- mapM (getChunk $ n * c) sources
        return $ (mapMaybe fst results ,) $ case map snd results of
          []     -> V.replicate (n * c) 0
          v : vs -> foldr (V.zipWith (+)) v vs
      go currentSources future = case future of
        RNil -> case currentSources of
          [] -> return ()
          _ -> do
            (nextSources, v) <- getFrames chunkSize currentSources
            yield v
            go nextSources RNil
        Wait 0 src rest -> do
          let (now, later) = U.trackSplitZero rest
          opened <- unvoid $ map fst <$> mapM (=$$+ return ()) (src : now)
          go (currentSources ++ opened) later
        Wait dt src next -> do
          let sizeToGet = min chunkSize $ fromIntegral dt
          (nextSources, v) <- getFrames sizeToGet currentSources
          yield v
          go nextSources $ Wait (dt - fromIntegral sizeToGet) src next
      in go [] $ fmap source srcs'
    }

getAudio :: (MonadResource m, MonadIO f) =>
  RTB.T U.Beats Chip -> FilePath -> DTX -> f (AudioSource m Float)
getAudio chips dtxPath dtx = liftIO $ do
  let usedChips = nubOrd $ RTB.getBodies chips
      wavs = HM.filterWithKey (\k _ -> elem k usedChips) $ dtx_WAV dtx
  srcs <- forM wavs $ \fp -> let
    fp' = takeDirectory dtxPath </> fp
    in case map toLower $ takeExtension fp' of
      ".mp3" -> sourceMpg fp'
      ".xa"  -> mapSamples fractionalSample <$> sourceXA fp'
      _      -> sourceSnd fp'
  let outOf100 n = realToFrac n / 100
      r = 44100
      lookupChip chip = flip fmap (HM.lookup chip srcs) $ \src -> let
        stereo = applyPansVols
          (case channels src of
            2 -> [-1, 1]
            1 -> [maybe 0 outOf100 $ HM.lookup chip $ dtx_PAN dtx]
            n -> replicate n 0
          )
          (replicate (channels src) $ maybe 0 outOf100 $ HM.lookup chip $ dtx_VOLUME dtx)
          src
        in if rate stereo == r
          then stereo
          else resampleTo r SincMediumQuality stereo
  return
    $ mixMany r 2
    $ U.applyTempoTrack (dtx_TempoMap dtx)
    $ RTB.mapMaybe lookupChip chips
