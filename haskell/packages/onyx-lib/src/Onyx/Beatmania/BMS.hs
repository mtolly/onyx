{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Beatmania.BMS where

import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as Map
import           Data.Maybe
import qualified Data.Text                        as T
import           Onyx.DTXMania.DTX
import           Onyx.MIDI.Common                 (pattern RNil, pattern Wait)
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data BMS = BMS
  { bms_PLAYER      :: Maybe Int
  , bms_GENRE       :: Maybe T.Text
  , bms_TITLE       :: Maybe T.Text
  , bms_SUBTITLE    :: Maybe T.Text
  , bms_ARTIST      :: Maybe T.Text
  , bms_SUBARTIST   :: Maybe T.Text
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
  , bms_BMP         :: HM.HashMap Chip FilePath
  , bms_BGA         :: RTB.T U.Beats Chip
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
  , bms_SUBTITLE = lookup "SUBTITLE" lns
  , bms_ARTIST = lookup "ARTIST" lns
  , bms_SUBARTIST = lookup "SUBARTIST" lns
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
  , bms_BMP = fmap T.unpack $ HM.fromList $ getReferences "BMP" lns
  , bms_BGA = getChannel "04"
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
