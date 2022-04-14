{-
Preprocessed version of MIDI file
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module PowerGig.GEV where

import           Control.Monad   (forM_)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as B
import           Data.Int
import qualified Data.Vector     as V
import           Data.Word

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

{-
gevtGameBits explanation

bits (guitar/drums)
       1 on if non-power-chord note
       2 green
       4 red
       8 yellow
      10 blue
      20 orange
      40 (freestyle open?)
      80 freestyle green
     100 freestyle red
     200 freestyle yellow
     400 freestyle blue
     800 freestyle orange
    1000 power chord only open
    2000 power chord only green
    4000 power chord only red
    8000 power chord only yellow
   10000 power chord only blue
   20000 power chord only orange
   40000 E 0 power chord
   80000 E 2 power chord
  100000 E 3 power chord
  200000 E 4 power chord
  400000 E 5 power chord
  800000 E 6 power chord
 1000000 A 0 power chord
 2000000 A 2 power chord
 4000000 A 3 power chord
 8000000 A 4 power chord
10000000 A 5 power chord
20000000 A 6 power chord

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
    printVector "GEVT" (gelsGEVT gels) print
  printVector "String" (strhData gevSTRH) $ \offset -> do
    print $ B.takeWhile (/= 0) $ B.drop (fromIntegral offset) $ strsData gevSTRS
  print gevTMPO { tmpoEvents = V.empty }
  printVector "TMPOEvent" (tmpoEvents gevTMPO) print
