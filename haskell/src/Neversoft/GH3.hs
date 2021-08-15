{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.GH3 where

import           Control.Monad                  (forM)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import qualified Data.ByteString.Lazy as BL
import STFS.Package (runGetM)
import           Data.Word
import           Neversoft.Checksum             (qbKeyCRC)
import           Neversoft.QB
import qualified RockBand.Codec.File as RBFile
import qualified  Sound.MIDI.Util as U
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.EventList.Relative.TimeBody as RTB
import qualified Data.EventList.Absolute.TimeBody as ATB
import Data.List (sort)
import qualified RockBand.Codec.Five as F
import RockBand.Common (Difficulty(..), StrumHOPOTap(..))
import Guitars (emit5')
import Data.Bits
import System.FilePath ((<.>))

data GH3Track = GH3Track
  { gh3Notes       :: [(Word32, Word32, Word32)]
  , gh3StarPower   :: [(Word32, Word32, Word32)]
  , gh3BattleStars :: [(Word32, Word32, Word32)]
  } deriving (Show)

data GH3Part = GH3Part
  { gh3Easy   :: GH3Track
  , gh3Medium :: GH3Track
  , gh3Hard   :: GH3Track
  , gh3Expert :: GH3Track
  } deriving (Show)

data GH3Background a = GH3Background
  { gh3Scripts     :: [a]
  , gh3Anim        :: [a]
  , gh3Triggers    :: [a]
  , gh3Cameras     :: [a]
  , gh3LightShow   :: [a]
  , gh3Crowd       :: [a]
  , gh3Drums       :: [a]
  , gh3Performance :: [a]
  } deriving (Show)

data GH3AnimEvent = GH3AnimEvent
  { gh3AnimTime   :: Word32
  , gh3AnimScr    :: Word32
  , gh3AnimParams :: [QBStructItem Word32 Word32]
  } deriving (Show)

data GH3MidQB = GH3MidQB
  { gh3Guitar          :: GH3Part
  , gh3Rhythm          :: GH3Part
  , gh3CoopGuitar      :: GH3Part
  , gh3CoopRhythm      :: GH3Part

  , gh3P1FaceOff       :: [(Word32, Word32)]
  , gh3P2FaceOff       :: [(Word32, Word32)]
  , gh3P1BossBattle    :: [(Word32, Word32)] -- is this right? TODO check
  , gh3P2BossBattle    :: [(Word32, Word32)]

  , gh3TimeSignatures  :: [(Word32, Word32, Word32)]
  , gh3FretBars        :: [Word32]
  , gh3Markers         :: [(Word32, Word32)] -- time, marker

  , gh3BackgroundNotes :: GH3Background (Word32, Word32, Word32)
  , gh3Background      :: GH3Background GH3AnimEvent
  } deriving (Show)

findSection
  :: (Monad m)
  => [QBSection Word32 Word32]
  -> B.ByteString
  -> (QBArray Word32 Word32 -> StackTraceT m a)
  -> StackTraceT m a
findSection qb key go = inside ("QB section: " <> show key) $ do
  case [ary | QBSectionArray k _ ary <- qb, k == qbKeyCRC key] of
    [ary] -> go ary
    []    -> fatal "Section not found"
    _     -> fatal "Multiple sections with same key?"

groupBy3 :: (Monad m) => [w] -> StackTraceT m [(w, w, w)]
groupBy3 = go [] where
  go trips []                 = return $ reverse trips
  go trips (x : y : z : rest) = go ((x, y, z) : trips) rest
  go _     _                  = fatal "Expected a list whose length is a multiple of 3"

listOfPairs :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32)]
listOfPairs = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfArray arys -> forM arys $ \case
    QBArrayOfInteger [x, y] -> return (x, y)
    _                       -> fatal "Expected an array of 2 integers"
  _                   -> fatal "Expected array of arrays"

listOfTriples :: (Monad m) => QBArray Word32 Word32 -> StackTraceT m [(Word32, Word32, Word32)]
listOfTriples = \case
  QBArrayOfFloatRaw [] -> return []
  QBArrayOfArray arys -> forM arys $ \case
    QBArrayOfInteger [x, y, z] -> return (x, y, z)
    _                          -> fatal "Expected an array of 3 integers"
  _                   -> fatal "Expected array of arrays"

parsePart :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> B.ByteString -> StackTraceT m GH3Part
parsePart dlc qb part = let
  parseTrack diff = do
    gh3Notes       <- findSection qb (dlc <> "_song_" <> part <> diff) $ \case
      QBArrayOfFloatRaw [] -> return []
      QBArrayOfInteger ns -> groupBy3 ns
      _                   -> fatal "Expected array of integers for notes"
    gh3StarPower   <- findSection qb (dlc <> "_" <> part <> diff <> "_star") listOfTriples
    gh3BattleStars <- findSection qb (dlc <> "_" <> part <> diff <> "_starbattlemode") listOfTriples
    return GH3Track{..}
  in do
    gh3Easy   <- parseTrack "easy"
    gh3Medium <- parseTrack "medium"
    gh3Hard   <- parseTrack "hard"
    gh3Expert <- parseTrack "expert"
    return GH3Part{..}

parseAnimEvent :: (Monad m) => [QBStructItem Word32 Word32] -> StackTraceT m GH3AnimEvent
parseAnimEvent = \case
  QBStructHeader : struct -> do
    gh3AnimTime <- case [v | QBStructItemInteger810000 k v <- struct, k == qbKeyCRC "time"] of
      [t] -> return t
      _   -> fatal "Couldn't get time of anim event"
    gh3AnimScr <- case [v | QBStructItemQbKey8D0000 k v <- struct, k == qbKeyCRC "scr"] of
      [scr] -> return scr
      _     -> fatal "Couldn't get scr of anim event"
    gh3AnimParams <- case [v | QBStructItemStruct8A0000 k v <- struct, k == qbKeyCRC "params"] of
      [QBStructHeader : params] -> return params
      [] -> return []
      _ -> fatal "Couldn't get params of anim event"
    return GH3AnimEvent{..}
  _ -> fatal "Expected struct header in anim event"

parseBackground
  :: (Monad m)
  => B.ByteString
  -> [QBSection Word32 Word32]
  -> B.ByteString
  -> (QBArray Word32 Word32 -> StackTraceT m [a])
  -> StackTraceT m (GH3Background a)
parseBackground dlc qb sfx inner = do
  gh3Scripts     <- findSection qb (dlc <> "_scripts" <> sfx) inner
  gh3Anim        <- findSection qb (dlc <> "_anim" <> sfx) inner
  gh3Triggers    <- findSection qb (dlc <> "_triggers" <> sfx) inner
  gh3Cameras     <- findSection qb (dlc <> "_cameras" <> sfx) inner
  gh3LightShow   <- findSection qb (dlc <> "_lightshow" <> sfx) inner
  gh3Crowd       <- findSection qb (dlc <> "_crowd" <> sfx) inner
  gh3Drums       <- findSection qb (dlc <> "_drums" <> sfx) inner
  gh3Performance <- findSection qb (dlc <> "_performance" <> sfx) inner
  return GH3Background{..}

parseMidQB :: (Monad m) => B.ByteString -> [QBSection Word32 Word32] -> StackTraceT m GH3MidQB
parseMidQB dlc qb = do

  gh3Guitar     <- parsePart dlc qb ""
  gh3Rhythm     <- parsePart dlc qb "rhythm_"
  gh3CoopGuitar <- parsePart dlc qb "guitarcoop_"
  gh3CoopRhythm <- parsePart dlc qb "rhythmcoop_"

  gh3P1FaceOff    <- findSection qb (dlc <> "_faceoffp1") listOfPairs
  gh3P2FaceOff    <- findSection qb (dlc <> "_faceoffp2") listOfPairs
  gh3P1BossBattle <- findSection qb (dlc <> "_bossbattlep1") listOfPairs -- TODO check
  gh3P2BossBattle <- findSection qb (dlc <> "_bossbattlep2") listOfPairs -- TODO check

  gh3TimeSignatures <- findSection qb (dlc <> "_timesig") listOfTriples
  gh3FretBars       <- findSection qb (dlc <> "_fretbars") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfInteger ns -> return ns
    _                   -> fatal "Expected array of integers for fretbars"
  gh3Markers        <- findSection qb (dlc <> "_markers") $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfStruct marks -> forM marks $ \case
      QBStructHeader : items -> let
        time   = [v | QBStructItemInteger810000     k v <- items, k == qbKeyCRC "time"  ]
        marker = [v | QBStructItemQbKeyString9A0000 k v <- items, k == qbKeyCRC "marker"]
        in case (time, marker) of
          ([t], [m]) -> return (t, m)
          _          -> fatal "Unexpected contents of marker"
      _ -> fatal "No struct header in marker"
    _ -> fatal "Expected array of structs for markers"


  gh3BackgroundNotes <- parseBackground dlc qb "_notes" listOfTriples
  gh3Background      <- parseBackground dlc qb "" $ \case
    QBArrayOfFloatRaw [] -> return []
    QBArrayOfStruct entries -> mapM parseAnimEvent entries
    _ -> fatal "Expected array of structs for background events"

  return GH3MidQB{..}

testParse :: B.ByteString -> FilePath -> IO GH3MidQB
testParse k f = (either (fail . show) return =<<) $ logStdout $ do
  bs <- stackIO $ BL.readFile f
  qb <- runGetM parseQB bs
  parseMidQB k qb

testConvert :: B.ByteString -> FilePath -> IO ()
testConvert k fin = do
  midqb <- testParse k fin
  writeFile (fin <.> "txt") $ show midqb
  RBFile.saveMIDI (fin <.> "mid") $ gh3ToMidi mempty midqb

gh3ToMidi :: HM.HashMap Word32 T.Text -> GH3MidQB -> RBFile.Song (RBFile.FixedFile U.Beats)
gh3ToMidi bank gh3 = let
  toSeconds :: Word32 -> U.Seconds
  toSeconds = (/ 1000) . fromIntegral
  sigMap = Map.fromList [ (time, (num, den)) | (time, num, den) <- gh3TimeSignatures gh3 ]
  barSigs = [ (t, maybe 4 (snd . snd) $ Map.lookupLE t sigMap) | t <- gh3FretBars gh3 ]
  tempos = U.tempoMapFromBPS $ let
    makeTempo (t1, denom) (t2, _) = let
      secs = toSeconds t2 - toSeconds t1
      beats = 4 / fromIntegral denom
      in (U.makeTempo beats secs, beats)
    temposGaps = zipWith makeTempo barSigs (drop 1 barSigs)
    in RTB.fromPairList $ zip (0 : map snd temposGaps) (map fst temposGaps)
  toBeats :: Word32 -> U.Beats
  toBeats = U.unapplyTempoMap tempos . toSeconds
  fromPairs ps = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort ps
  getTrack trk = emit5' $ fromPairs $ gh3Notes trk >>= \(time, len, bits) -> do
    fret <- concat
      [ [Just F.Green  | bits `testBit` 0]
      , [Just F.Red    | bits `testBit` 1]
      , [Just F.Yellow | bits `testBit` 2]
      , [Just F.Blue   | bits `testBit` 3]
      , [Just F.Orange | bits `testBit` 4]
      ]
    let pos = toBeats time
        sht = if bits `testBit` 5 then HOPO else Strum -- TODO actual force algorithm
        len' = toBeats (time + len) - pos
    return (pos, ((fret, sht), Just len'))
  getPart part = mempty
    { F.fiveDifficulties = Map.fromList
      [ (Expert, getTrack $ gh3Expert part)
      , (Hard  , getTrack $ gh3Hard   part)
      , (Medium, getTrack $ gh3Medium part)
      , (Easy  , getTrack $ gh3Easy   part)
      ]
    , F.fiveOverdrive = RTB.fromAbsoluteEventList $ ATB.fromPairList $ sort $ do
      (time, len, _noteCount) <- gh3StarPower $ gh3Expert part
      [(toBeats time, True), (toBeats $ time + len, False)]
    }
  fixed = mempty
    { RBFile.fixedPartGuitar = getPart $ gh3Guitar gh3
    , RBFile.fixedPartRhythm = getPart $ gh3Rhythm gh3 -- TODO find when to put it on bass
    , RBFile.fixedPartGuitarCoop = getPart $ gh3CoopGuitar gh3
    -- , RBFile.fixedEvents = mempty
    --   { eventsSections = RTB.mapMaybe (\case Right sect -> Just sect; _ -> Nothing) markers
    --   , eventsEnd      = RTB.mapMaybe (\case Left  ()   -> Just ()  ; _ -> Nothing) markers
    --   }
    }
  in RBFile.Song
    { RBFile.s_tempos = tempos
    , RBFile.s_signatures = U.measureMapFromTimeSigs U.Truncate $ RTB.fromAbsoluteEventList $ ATB.fromPairList $ do
      (time, num, den) <- gh3TimeSignatures gh3
      let unit = 4 / fromIntegral den
          len = fromIntegral num * unit
      return (toBeats time, U.TimeSig len unit)
    , RBFile.s_tracks = fixed
    }
