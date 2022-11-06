{-
Preprocessed version of MIDI file
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.PowerGig.GEV where

import           Control.Monad                    (forM_, guard)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits                        ((.&.))
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Int
import qualified Data.Map                         as Map
import qualified Data.Vector                      as V
import           Data.Word
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U

data GEV = GEV
  { gevPCMC :: PCMC
  , gevGELH :: GELH
  , gevSTRH :: STRH
  , gevSTRS :: STRS
  , gevTMPO :: TMPO
  } deriving (Show)

data PCMC = PCMC
  { pcmcUnk1 :: Word32 -- always 1
  , pcmcUnk2 :: Word32 -- think this is a Unix timestamp
  , pcmcUnk3 :: Word32 -- always 0
  , pcmcMIDI :: B.ByteString
  } deriving (Show)

data GELH = GELH
  { gelhGELS :: V.Vector GELS
  } deriving (Show)

data GELS = GELS
  { gelsUnk1      :: Word32 -- just counts up from 2?
  , gelsTrackName :: Word32 -- index into STRH
  , gelsUnk3      :: Word32 -- number of non-power-chord notes (chord = 1 note). does not include pc-only non-pc notes
  , gelsUnk4      :: Word32
  , gelsUnk5      :: Float -- not totally sure this and next are floats
  , gelsUnk6      :: Float
  , gelsUnk7      :: Word32 -- number of power chords, 0 on non-guitar tracks. does not include pc-only non-pc notes
  , gelsGEVT      :: V.Vector GEVT
  } deriving (Show)

data GEVT = GEVT
  { gevtTime     :: Float -- seconds
  , gevtTime2    :: Float -- always same as gevtTime?
  , gevtSustain  :: Float -- note sustain length in seconds (0 if below sustain threshold). drum sustains may be shortened? see cherub rock
  , gevtGameBits :: Int32 -- -1 unless this is a group of notes on guitar/drums/vocals track. see below
  , gevtUnk5     :: Word32 -- this matches the gelsUnk1 for this track
  , gevtType     :: Word32 -- event type: 2 = note, 4 = controller, 5 = program change, 9 = text event, 11 = track name, 13 = lyric, 17 = end of track, 18 and 20 seen in _cue.gev but probably just tempo/timesig
  , gevtData1    :: Word32 -- pitch of (lowest) note, STRH index for track name / lyric / text event, controller index, program change index. for end of track uses whatever it was last event
  , gevtData2    :: Word32 -- velocity of note, controller value. if unused (anything else) uses whatever it was last event. inconsistent for first event (track name), seen 0x26afef4, 0x27afef4
  } deriving (Show)

-- Bits used for note data on guitar (incl. power chord) and drums
data GuitarDrumBit
  = Bit_StandardNote -- guitar notes outside of PC sections, non-PC-only notes in PC sections, all drums
  | Bit_Green
  | Bit_Red
  | Bit_Yellow
  | Bit_Blue
  | Bit_Orange
  | Bit_FreestyleOpen -- not seen, guessing
  | Bit_FreestyleGreen
  | Bit_FreestyleRed
  | Bit_FreestyleYellow
  | Bit_FreestyleBlue
  | Bit_FreestyleOrange
  | Bit_PowerChordOnly -- by itself, power-chord-only open note. but also present with GY chords in crack the skye
  | Bit_PowerChordOnlyGreen
  | Bit_PowerChordOnlyRed
  | Bit_PowerChordOnlyYellow
  | Bit_PowerChordOnlyBlue
  | Bit_PowerChordOnlyOrange
  | Bit_PowerChordE0
  | Bit_PowerChordE2
  | Bit_PowerChordE3
  | Bit_PowerChordE4
  | Bit_PowerChordE5
  | Bit_PowerChordE6
  | Bit_PowerChordA0
  | Bit_PowerChordA2
  | Bit_PowerChordA3
  | Bit_PowerChordA4
  | Bit_PowerChordA5
  | Bit_PowerChordA6
  deriving (Eq, Show, Enum, Bounded)

guitarDrumBit :: GuitarDrumBit -> Int32
guitarDrumBit = \case
  Bit_StandardNote         ->        0x1
  Bit_Green                ->        0x2
  Bit_Red                  ->        0x4
  Bit_Yellow               ->        0x8
  Bit_Blue                 ->       0x10
  Bit_Orange               ->       0x20
  Bit_FreestyleOpen        ->       0x40
  Bit_FreestyleGreen       ->       0x80
  Bit_FreestyleRed         ->      0x100
  Bit_FreestyleYellow      ->      0x200
  Bit_FreestyleBlue        ->      0x400
  Bit_FreestyleOrange      ->      0x800
  Bit_PowerChordOnly       ->     0x1000
  Bit_PowerChordOnlyGreen  ->     0x2000
  Bit_PowerChordOnlyRed    ->     0x4000
  Bit_PowerChordOnlyYellow ->     0x8000
  Bit_PowerChordOnlyBlue   ->    0x10000
  Bit_PowerChordOnlyOrange ->    0x20000
  Bit_PowerChordE0         ->    0x40000
  Bit_PowerChordE2         ->    0x80000
  Bit_PowerChordE3         ->   0x100000
  Bit_PowerChordE4         ->   0x200000
  Bit_PowerChordE5         ->   0x400000
  Bit_PowerChordE6         ->   0x800000
  Bit_PowerChordA0         ->  0x1000000
  Bit_PowerChordA2         ->  0x2000000
  Bit_PowerChordA3         ->  0x4000000
  Bit_PowerChordA4         ->  0x8000000
  Bit_PowerChordA5         -> 0x10000000
  Bit_PowerChordA6         -> 0x20000000

findBits :: (Enum a, Bounded a) => (a -> Int32) -> Int32 -> Maybe [a]
findBits _ (-1) = Nothing
findBits f n    = Just $ do
  x <- [minBound .. maxBound]
  guard $ n .&. f x /= 0
  return x

{-
gevtGameBits explanation

bits (guitar/drums)
  see mapping above

for power chord notes there are two separate GEVT for the PC and non PC events

vocals:
for normal notes, gevtGameBits = the midi pitch
freestyle:
e.g. 11094 = 0x2b56 = 0b10101101010110 = 52 54 56 57 59 61 62 64 (E mixolydian in cherub rock)
bits:
       1   C
       2 * C#
       4 * D
       8   D#
      10 * E
      20   F
      40 * F#
      80   G
     100 * G#
     200 * A
     400   A#
     800 * B
    1000   B#
    2000 * freestyle

in _cue.gev all evtGameBits are -1

-}

data STRH = STRH
  { strhData :: V.Vector Word32
  } deriving (Show)

data STRS = STRS
  { strsData :: B.ByteString -- null term'd strings, but some have two nulls so every string starts on an even offset
  } deriving (Show)

data TMPO = TMPO
  { tmpoResolution :: Word32
  , tmpoFraction   :: Float -- appears to just be "1 / tmpoResolution"
  , tmpoEvents     :: V.Vector TMPOEvent
  } deriving (Show)

data TMPOEvent = TMPOEvent
  { tmpoEventTicks :: Word32 -- absolute midi ticks
  , tmpoEventUnk2  :: Word32 -- always 0?
  , tmpoEventUSPQN :: Word32
  , tmpoEventTime  :: Float -- seconds
  } deriving (Show)

readGEV :: Get GEV
readGEV = do
  gevPCMC <- do
    "PCMC" <- getByteString 4
    pcmcSize <- getWord32le
    pcmcUnk1 <- getWord32le
    pcmcUnk2 <- getWord32le
    pcmcUnk3 <- getWord32le
    pcmcMIDI <- getByteString $ fromIntegral pcmcSize - 13
    0 <- getWord8
    return PCMC{..}
  gevGELH <- do
    "GELH" <- getByteString 4
    4 <- getWord32le
    gelhCount <- getWord32le
    gelhGELS <- V.replicateM (fromIntegral gelhCount) $ do
      "GELS" <- getByteString 4
      0x20 <- getWord32le
      gelsCount <- getWord32le
      gelsUnk1 <- getWord32le
      gelsTrackName <- getWord32le
      gelsUnk3 <- getWord32le
      gelsUnk4 <- getWord32le
      gelsUnk5 <- getFloatle
      gelsUnk6 <- getFloatle
      gelsUnk7 <- getWord32le
      gelsGEVT <- V.replicateM (fromIntegral gelsCount) $ do
        "GEVT" <- getByteString 4
        0x20 <- getWord32le
        gevtTime <- getFloatle
        gevtTime2 <- getFloatle
        gevtSustain <- getFloatle
        gevtGameBits <- getInt32le
        gevtUnk5 <- getWord32le
        gevtType <- getWord32le
        gevtData1 <- getWord32le
        gevtData2 <- getWord32le
        return GEVT{..}
      return GELS{..}
    return GELH{..}
  gevSTRH <- do
    "STRH" <- getByteString 4
    strhSize <- getWord32le
    strhCount <- case quotRem strhSize 4 of
      (n, 0) -> return n
      _      -> fail $ "STRH data size not divisible by 4: " <> show strhSize
    strhData <- V.replicateM (fromIntegral strhCount - 1) getWord32le
    return STRH{..}
  gevSTRS <- do
    "STRS" <- getByteString 4
    strsSize <- getWord32le
    strsData <- getByteString $ fromIntegral strsSize
    return STRS{..}
  gevTMPO <- do
    "TMPO" <- getByteString 4
    tmpoSize <- getWord32le
    tmpoResolution <- getWord32le
    tmpoFraction <- getFloatle
    tmpoCount <- case quotRem (tmpoSize - 8) 16 of
      (n, 0) -> return n
      _      -> fail $ "TMPO data size not divisible by 16: " <> show (tmpoSize - 8)
    tmpoEvents <- V.replicateM (fromIntegral tmpoCount) $ do
      tmpoEventTicks <- getWord32le
      tmpoEventUnk2 <- getWord32le
      tmpoEventUSPQN <- getWord32le
      tmpoEventTime <- getFloatle
      return TMPOEvent{..}
    return TMPO{..}
  return GEV{..}

showGEV :: GEV -> Put
showGEV GEV{..} = do
  let PCMC{..} = gevPCMC in do
    putByteString "PCMC"
    putWord32le $ fromIntegral $ B.length pcmcMIDI + 13
    putWord32le pcmcUnk1
    putWord32le pcmcUnk2
    putWord32le pcmcUnk3
    putByteString pcmcMIDI
    putWord8 0
  let GELH{..} = gevGELH in do
    putByteString "GELH"
    putWord32le 4
    putWord32le $ fromIntegral $ V.length gelhGELS
    forM_ gelhGELS $ \GELS{..} -> do
      putByteString "GELS"
      putWord32le 0x20
      putWord32le $ fromIntegral $ V.length gelsGEVT
      putWord32le gelsUnk1
      putWord32le gelsTrackName
      putWord32le gelsUnk3
      putWord32le gelsUnk4
      putFloatle gelsUnk5
      putFloatle gelsUnk6
      putWord32le gelsUnk7
      forM_ gelsGEVT $ \GEVT{..} -> do
        putByteString "GEVT"
        putWord32le 0x20
        putFloatle gevtTime
        putFloatle gevtTime2
        putFloatle gevtSustain
        putInt32le gevtGameBits
        putWord32le gevtUnk5
        putWord32le gevtType
        putWord32le gevtData1
        putWord32le gevtData2
  let STRH{..} = gevSTRH in do
    putByteString "STRH"
    putWord32le $ fromIntegral $ V.length strhData * 4 + 4
    mapM_ putWord32le strhData
  let STRS{..} = gevSTRS in do
    putByteString "STRS"
    putWord32le $ fromIntegral $ B.length strsData
    putByteString strsData
  let TMPO{..} = gevTMPO in do
    putByteString "TMPO"
    putWord32le $ fromIntegral $ V.length tmpoEvents * 16 + 8
    putWord32le tmpoResolution
    putFloatle tmpoFraction
    forM_ tmpoEvents $ \TMPOEvent{..} -> do
      putWord32le tmpoEventTicks
      putWord32le tmpoEventUnk2
      putWord32le tmpoEventUSPQN
      putFloatle tmpoEventTime

debugPrintGEV :: GEV -> IO ()
debugPrintGEV GEV{..} = do
  let printVector typ v each = forM_ (zip [0..] $ V.toList v) $ \(i, x) -> do
        putStrLn ""
        putStrLn $ typ <> " #" <> show (i :: Int) <> ":"
        each x
  print gevPCMC
  printVector "GELS" (gelhGELS gevGELH) $ \gels -> do
    print gels { gelsGEVT = V.empty }
    printVector "GEVT" (gelsGEVT gels) $ \gevt -> do
      print gevt
      print $ findBits guitarDrumBit $ gevtGameBits gevt
  printVector "String" (strhData gevSTRH) $ \offset -> do
    print $ B.takeWhile (/= 0) $ B.drop (fromIntegral offset) $ strsData gevSTRS
  print gevTMPO { tmpoEvents = V.empty }
  printVector "TMPOEvent" (tmpoEvents gevTMPO) print

getMIDITempos :: GEV -> U.TempoMap
getMIDITempos gev
  = U.makeTempoMap
  $ RTB.fromAbsoluteEventList
  $ ATB.fromPairList
  $ flip map (V.toList $ tmpoEvents $ gevTMPO gev) $ \e -> let
    posn = fromIntegral (tmpoEventTicks e) / fromIntegral (tmpoResolution $ gevTMPO gev)
    event = E.MetaEvent $ Meta.SetTempo $ fromIntegral $ tmpoEventUSPQN e
    in (posn, event)

getString :: Word32 -> GEV -> Maybe B.ByteString
getString i gev = do
  offset <- strhData (gevSTRH gev) V.!? fromIntegral i
  return $ B.takeWhile (/= 0) $ B.drop (fromIntegral offset) $ strsData $ gevSTRS gev

makeStringBank :: [B.ByteString] -> (STRH, STRS, Map.Map B.ByteString Word32)
makeStringBank = go BL.empty [] Map.empty where
  go bank byteOffsets mapping [] = let
    strh = STRH
      { strhData = V.fromList $ reverse byteOffsets
      }
    strs = STRS
      { strsData = BL.toStrict bank
      }
    in (strh, strs, mapping)
  go bank byteOffsets mapping (x : xs) = let
    nulls = BL.replicate (if rem (B.length x) 2 == 0 then 2 else 1) 0
    bank' = bank <> BL.fromStrict x <> nulls
    byteOffsets' = fromIntegral (BL.length bank) : byteOffsets
    mapping' = Map.insert x (fromIntegral $ length byteOffsets) mapping
    in go bank' byteOffsets' mapping' xs

makeTempos :: U.TempoMap -> TMPO
makeTempos tmap = TMPO
  { tmpoResolution = 480
  , tmpoFraction = 1 / 480
  , tmpoEvents = V.fromList $ do
    (absBeats, bps) <- ATB.toPairList $ RTB.toAbsoluteEventList 0 $ U.tempoMapToBPS tmap
    return TMPOEvent
      { tmpoEventTicks = round $ absBeats * 480
      , tmpoEventUnk2  = 0
      , tmpoEventUSPQN = round $ 1000000 / bps
      , tmpoEventTime  = realToFrac $ U.applyTempoMap tmap absBeats
      }
  }
