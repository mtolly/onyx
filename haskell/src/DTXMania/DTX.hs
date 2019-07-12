{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module DTXMania.DTX where

import           Audio                            (applyPansVols, mixMany)
import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Control.Monad.Trans.Resource     (MonadResource, ResourceT,
                                                   runResourceT)
import           Control.Monad.Trans.StackTrace   (tempDir)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isAlphaNum, isDigit, toLower)
import           Data.Conduit.Audio
import           Data.Conduit.Audio.Mpg123        (sourceMpg)
import           Data.Conduit.Audio.SampleRate    (ConverterType (SincMediumQuality),
                                                   resampleTo)
import           Data.Conduit.Audio.Sndfile       (sourceSnd)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Data.Text                        as T
import           DTXMania.ShiftJIS                (decodeShiftJIS)
import           DTXMania.XA                      (sourceXA)
import           Numeric
import           RockBand.Common                  (pattern RNil, pattern Wait)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath
import           System.IO
import           Text.Read                        (readMaybe)

loadDTXLines :: FilePath -> IO [(T.Text, T.Text)]
loadDTXLines f = do
  lns <- lines . decodeShiftJIS <$> B.readFile f
  return $ flip mapMaybe lns $ \ln -> case ln of
    '#' : rest -> case T.span isAlphaNum $ T.pack rest of
      (x, y) -> Just (T.strip x, T.strip $ T.takeWhile (/= ';') $ T.dropWhile (== ':') y)
    _ -> Nothing

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
  -- TODO The Enemy Inside has 1B notes, is this left foot

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
      ".wav" -> sourceWAVMaybeOGG fp'
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

dropBytes :: (MonadResource m) => Integer -> FilePath -> FilePath -> (FilePath -> m a) -> m a
dropBytes n template f cb = tempDir "onyx-dtx-import" $ \dir -> do
  temp <- liftIO $ withBinaryFile f ReadMode $ \h -> do
    let temp = dir </> template
    hSeek h AbsoluteSeek n
    BL.hGetContents h >>= BL.writeFile temp
    return temp
  cb temp

sourceWAVMaybeOGG :: (MonadIO f, MonadResource m) => FilePath -> f (AudioSource m Float)
sourceWAVMaybeOGG f = liftIO $ do
  maybeOGGStart <- withBinaryFile f ReadMode $ \h -> do
    let findChunk magic = hSeek h AbsoluteSeek 12 >> findChunk' magic
        findChunk' magic = do
          thisChunk <- BL.hGet h 4
          size <- runGet getWord32le <$> BL.hGet h 4
          if magic == thisChunk
            then return ()
            else do
              hSeek h RelativeSeek $ fromIntegral size
              findChunk' magic
    findChunk "fmt "
    audioType <- BL.hGet h 2
    if audioType == "Og"
      then do
        findChunk "data"
        Just <$> hTell h
      else return Nothing
  case maybeOGGStart of
    Nothing  -> sourceSnd f
    Just ogg -> do
      src <- runResourceT $ dropBytes ogg "dtx-audio.ogg" f $ \temp -> do
        src <- liftIO $ sourceSnd temp
        return (src :: AudioSource (ResourceT IO) Float)
      return AudioSource
        { frames = frames src
        , rate = rate src
        , channels = channels src
        , source = dropBytes ogg "dtx-audio.ogg" f $ \temp -> do
          src' <- liftIO $ sourceSnd temp
          source src'
        }
