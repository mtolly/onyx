{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Beatmania.BMS where

import           Audio                            (applyPansVols, mixMany')
import           Control.Monad                    (forM)
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import           Data.Conduit.Audio
import           Data.Conduit.Audio.SampleRate    (ConverterType (SincMediumQuality),
                                                   resampleTo)
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import           Data.List.Extra                  (nubOrd)
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Data.Text                        as T
import           DTXMania.DTX
import           RockBand.Common                  (pattern RNil, pattern Wait)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath
import           Text.Read                        (readMaybe)

data BMS = BMS
  { bms_PLAYER      :: Maybe Int
  , bms_GENRE       :: Maybe T.Text
  , bms_TITLE       :: Maybe T.Text
  , bms_ARTIST      :: Maybe T.Text
  , bms_PLAYLEVEL   :: Maybe Int
  , bms_RANK        :: Maybe Int
  , bms_STAGEFILE   :: Maybe FilePath
  , bms_WAV         :: HM.HashMap Chip FilePath
  , bms_VOLWAV      :: HM.HashMap Chip Int
  , bms_MeasureMap  :: U.MeasureMap
  , bms_TempoMap    :: U.TempoMap
  , bms_Player1     :: RTB.T U.Beats (BMKey, Chip)
  , bms_Player2     :: RTB.T U.Beats (BMKey, Chip)
  , bms_Player1Long :: RTB.T U.Beats (BMKey, Chip, Bool)
  , bms_Player2Long :: RTB.T U.Beats (BMKey, Chip, Bool)
  , bms_BGM         :: RTB.T U.Beats Chip
  } deriving (Show)

data BMKey
  = BMScratch
  | BMKey1
  | BMKey2
  | BMKey3
  | BMKey4
  | BMKey5
  | BMKey6
  | BMKey7
  deriving (Eq, Ord, Show, Enum, Bounded)

bmeMapping :: [(Char, BMKey)]
bmeMapping =
  [ ('1', BMKey1)
  , ('2', BMKey2)
  , ('3', BMKey3)
  , ('4', BMKey4)
  , ('5', BMKey5)
  , ('6', BMScratch)
  , ('8', BMKey6)
  , ('9', BMKey7)
  ]

readBMSLines :: [(T.Text, T.Text)] -> BMS
readBMSLines lns = BMS
  { bms_PLAYER = lookup "PLAYER" lns >>= readMaybe . T.unpack
  , bms_GENRE = lookup "GENRE" lns
  , bms_TITLE = lookup "TITLE" lns
  , bms_ARTIST = lookup "ARTIST" lns
  , bms_PLAYLEVEL = lookup "PLAYLEVEL" lns >>= readMaybe . T.unpack
  , bms_RANK = lookup "RANK" lns >>= readMaybe . T.unpack
  , bms_STAGEFILE = T.unpack <$> lookup "STAGEFILE" lns
  , bms_WAV = fmap T.unpack $ HM.fromList $ getReferences "WAV" lns
  , bms_VOLWAV
    = HM.mapMaybe (readMaybe . T.unpack)
    $ HM.fromList
    $ getReferences "VOLWAV" lns
  , bms_MeasureMap = mmap
  , bms_TempoMap = tmap
  , bms_Player1 = readPlayer '1'
  , bms_Player2 = readPlayer '2'
  , bms_Player1Long = readPlayerLong '5'
  , bms_Player2Long = readPlayerLong '6'
  , bms_BGM = getChannel "01"
  } where

    objects :: Map.Map BarNumber (HM.HashMap Channel [T.Text])
    objects = getObjects lns

    maxBar = maybe 0 fst $ Map.lookupMax objects

    mmap
      = U.measureMapFromLengths U.Error
      $ assembleMMap
      $ map getBarLength [0 .. maxBar]
    getBarLength n = maybe 4 (* 4) $ do
      obj <- Map.lookup n objects
      lens <- HM.lookup "02" obj
      listToMaybe lens >>= textFloat
    assembleMMap lens = foldr ($) RNil $ zipWith Wait (0 : lens) lens

    getChannel :: Channel -> RTB.T U.Beats Chip
    getChannel chan = getChannelGeneral chan objects mmap

    tmap
      = U.tempoMapFromBPS
      $ fmap (\bpm -> U.BPS $ bpm / 60)
      $ addStartBPM
      $ fmap (+ baseBPM)
      $ RTB.merge bpmHexes bpmRefs
    addStartBPM bpms = case bpms of
      Wait 0 _ _ -> bpms -- already a tempo set at time 0
      _          -> case lookup "BPM" lns >>= textFloat of
        Just n | n /= 0 -> Wait 0 n   bpms
        _               -> Wait 0 120 bpms
    bpmHexes = RTB.mapMaybe textHex $ getChannel "03"
    bpmRefs = RTB.mapMaybe (`HM.lookup` refBPMs) $ getChannel "08"
    refBPMs = HM.mapMaybe textFloat $ HM.fromList $ getReferences "BPM" lns
    baseBPM = fromMaybe 0 $ lookup "BASEBPM" lns >>= textFloat

    readPlayer c = foldr RTB.merge RTB.empty $ do
      (col, key) <- bmeMapping
      return $ (key,) <$> getChannel (T.pack [c, col])

    readPlayerLong c = foldr RTB.merge RTB.empty $ do
      (col, key) <- bmeMapping
      let chan = getChannel $ T.pack [c, col]
          edges = zip (cycle [True, False]) $ RTB.toPairList chan
      return $ RTB.fromPairList $ flip fmap edges
        $ \(edge, (dt, chip)) -> (dt, (key, chip, edge))

getBMSAudio :: (MonadResource m, MonadIO f, SendMessage f) =>
  RTB.T U.Beats Chip -> FilePath -> BMS -> StackTraceT f (AudioSource m Float)
getBMSAudio chips bmsPath bms = do
  let usedChips = nubOrd $ RTB.getBodies chips
      wavs = HM.filterWithKey (\k _ -> elem k usedChips) $ bms_WAV bms
  srcs <- fmap (HM.mapMaybe id) $ forM wavs $ \fp -> do
    dtxAudioSource
      $ takeDirectory bmsPath
      </> map (\case '¥' -> '/'; '\\' -> '/'; c -> c) fp
      -- ¥ is the backslash when Shift-JIS decoded
  cachedSrcs <- forM srcs $ \src -> if fromIntegral (frames src) < rate src * 5
    then stackIO $ cacheAudio src
    else return src
  let outOf100 n = realToFrac n / 100
      r = 44100
      lookupChip chip = flip fmap (HM.lookup chip cachedSrcs) $ \src -> let
        stereo = applyPansVols
          (case channels src of
            2 -> [-1, 1]
            n -> replicate n 0
          )
          (replicate (channels src) $ maybe 0 outOf100 $ HM.lookup chip $ bms_VOLWAV bms)
          src
        resampled = if rate stereo == r
          then stereo
          else resampleTo r SincMediumQuality stereo
        in (resampled, chip)
  return
    $ mixMany' r 2 (const $ Just (1, 0.002)) -- TODO adjust cutoff time?
    $ U.applyTempoTrack (bms_TempoMap bms)
    $ RTB.mapMaybe lookupChip chips
