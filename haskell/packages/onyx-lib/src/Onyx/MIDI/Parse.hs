{-
This is a faster + way less memory hungry parser for the MIDI file format
than the one that comes with the `midi` package.
-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Onyx.MIDI.Parse (getMIDI) where

import           Control.Applicative                   (liftA2, (<|>))
import           Control.Monad                         (forM)
import           Data.Binary.Get
import           Data.Bits                             (shiftR, testBit, (.&.))
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Lazy                  as BL
import qualified Data.EventList.Relative.TimeBody      as RTB
import           Data.Foldable                         (toList)
import           Data.Int                              (Int64)
import           Data.Word                             (Word32, Word8)
import qualified Numeric.NonNegative.Wrapper           as NN
import           Onyx.MIDI.Common                      (pattern RNil,
                                                        pattern Wait)
import           Onyx.Xbox.STFS                        (runGetMOffset)
import qualified Sound.MIDI.File                       as F
import qualified Sound.MIDI.File.Event                 as E
import qualified Sound.MIDI.File.Event.Meta            as Meta
import qualified Sound.MIDI.File.Event.SystemExclusive as SysEx
import qualified Sound.MIDI.KeySignature               as Key
import qualified Sound.MIDI.Message.Channel            as C
import qualified Sound.MIDI.Message.Channel.Mode       as Mode
import qualified Sound.MIDI.Message.Channel.Voice      as V

getMIDI :: Get (F.T B.ByteString, [String])
getMIDI = do
  chunks <- getChunks
  case chunks of
    ("MThd", headerDataStart, header, _) : rest -> do
      (fmt, ntrks, dvn) <- runGetMOffset headerDataStart getHeader header
      tracks <- forM [(n, chunk) | ("MTrk", n, chunk, _) <- rest] $ \(chunkDataStart, chunk) -> do
        runGetMOffset chunkDataStart getTrack chunk
      let warnings = do
            (cid, chunkDataStart, _, maybeWarning) <- rest
            toList maybeWarning <> case cid of
              "MTrk" -> []
              _      -> ["Chunk starting at byte " <> show (chunkDataStart - 8)
                <> " has unrecognized type: " <> show cid]
      return (F.Cons fmt dvn $ take ntrks tracks, warnings)
    _ -> fail "Couldn't find MIDI header chunk at start"

getChunks :: Get [(B.ByteString, Int64, BL.ByteString, Maybe String)]
getChunks = isEmpty >>= \case
  True -> return []
  False -> let
    getChunk = do
      rootPosn <- bytesRead
      magic <- getByteString 4
      size <- getWord32be
      posn <- bytesRead
      chunk <- fmap Right (getLazyByteString $ fromIntegral size)
        <|> fmap Left getRemainingLazyByteString
      let (chunk', warning) = case chunk of
            Right x -> (x, Nothing)
            Left  x -> (x, Just $ "Chunk starting at byte " <> show rootPosn <>
              " should be size " <> show size <> ", but there aren't enough bytes")
      return (magic, posn, chunk', warning)
    in liftA2 (:) getChunk getChunks

getHeader :: Get (F.Type, Int, F.Division)
getHeader = do
  fmt <- getWord16be >>= \case
    0 -> return F.Mixed
    1 -> return F.Parallel
    2 -> return F.Serial
    n -> fail $ "Unknown MIDI file type: " <> show n
  ntrks <- fromIntegral <$> getWord16be
  dvn <- getInt16be
  let dvn' = if dvn >= 0
        then F.Ticks $ fromIntegral dvn
        else F.SMPTE
          (negate $ fromIntegral $ dvn `shiftR` 8)
          (fromIntegral $ dvn .&. 0xFF)
  return (fmt, ntrks, dvn')

getVariableNum :: Get Int
getVariableNum = readVariableBytes <$> getVariableBytes where
  getVariableBytes = do
    b <- getWord8
    if b > 0x7F
      then ((b .&. 0x7F) :) <$> getVariableBytes
      else return [b]
  bytePlaces = 1 : map (* 0x80) bytePlaces
  readVariableBytes = sum . zipWith (*) bytePlaces . reverse . map fromIntegral

getWord24be :: Get Word32
getWord24be = do
  x <- getWord8
  y <- getWord16be
  return $ fromIntegral x * 0x10000 + fromIntegral y

data RunningStatus
  = Status  Word8 -- normal running status from the previous event
  | Cleared Word8 -- status was technically cleared by a meta event. however we ignore it because other programs do the same

getTrack :: Get (RTB.T NN.Integer (E.T B.ByteString))
getTrack = removeEnd <$> go Nothing where
  removeEnd (Wait _ (E.MetaEvent Meta.EndOfTrack) RNil) = RNil
  removeEnd (Wait dt x rest) = Wait dt x $ removeEnd rest
  removeEnd RNil = RNil
  go running = isEmpty >>= \case
    True -> return RTB.empty
    False -> (fmap Just (getEvent running) <|> return Nothing) >>= \case
      Just (tks, e, running') -> RTB.cons tks e <$> go running'
      Nothing                 -> return RTB.empty -- TODO should warn here, probably ran out of bytes

getEvent :: Maybe RunningStatus -> Get (NN.Integer, E.T B.ByteString, Maybe RunningStatus)
getEvent running = do
  tks <- fromIntegral <$> getVariableNum
  let markCleared = case running of
        Nothing          -> Nothing
        Just (Status  w) -> Just (Cleared w)
        Just (Cleared _) -> running
  (e, running') <- getWord8 >>= \case
    0xFF -> do
      metaType <- getWord8
      metaLen <- getVariableNum
      let str = getByteString metaLen
      e <- E.MetaEvent <$> case (metaType, metaLen) of
        (0x00, 2) -> Meta.SequenceNum . fromIntegral <$> getWord16be
        (0x01, _) -> Meta.TextEvent <$> str
        (0x02, _) -> Meta.Copyright <$> str
        (0x03, _) -> Meta.TrackName <$> str
        (0x04, _) -> Meta.InstrumentName <$> str
        (0x05, _) -> Meta.Lyric <$> str
        (0x06, _) -> Meta.Marker <$> str
        (0x07, _) -> Meta.CuePoint <$> str
        (0x20, 1) -> Meta.MIDIPrefix . C.toChannel . fromIntegral <$> getWord8
        (0x2F, 0) -> return Meta.EndOfTrack
        (0x51, 3) -> Meta.SetTempo . fromIntegral <$> getWord24be
        (0x54, 5) -> do
          [hr, mn, se, fr, ff] <- map fromIntegral . B.unpack <$> getByteString 5
          return $ Meta.SMPTEOffset hr mn se fr ff
        (0x58, 4) -> do
          [nn, dd, cc, bb] <- map fromIntegral . B.unpack <$> getByteString 4
          return $ Meta.TimeSig nn dd cc bb
        (0x59, 2) -> do
          sf <- getWord8
          mi <- getWord8
          return $ Meta.KeySig $ Key.Cons
            (case mi of 0 -> Key.Major; _ -> Key.Minor) -- technically only 1 should be minor
            (Key.Accidentals $ fromIntegral sf)
        (0x7F, _) -> Meta.SequencerSpecific . B.unpack <$> getByteString metaLen
        _ -> Meta.Unknown (fromIntegral metaType) . B.unpack <$> getByteString metaLen
      return (e, markCleared)
    0xF0 -> do
      sysexLen <- getVariableNum
      e <- E.SystemExclusive . SysEx.Regular . B.unpack <$> getByteString sysexLen
      return (e, markCleared)
    0xF7 -> do
      sysexLen <- getVariableNum
      e <- E.SystemExclusive . SysEx.Escape . B.unpack <$> getByteString sysexLen
      return (e, markCleared)
    n -> do
      (statusByte, getFirstByte) <- if n `testBit` 7
        then return (n, getWord8)
        else case running of
          Just (Status  runningByte) -> return (runningByte, return n)
          Just (Cleared runningByte) -> return (runningByte, return n) -- TODO maybe warn about this, technically not legal
          Nothing                    -> fail "Event needs running status but none is set"
      let chan = C.Cons $ C.toChannel $ fromIntegral $ statusByte .&. 0xF
          int7Bits b = fromIntegral $ (b :: Word8) .&. 0x7F :: Int
      e <- E.MIDIEvent . chan <$> case statusByte `shiftR` 4 of
        0x8 -> (\k v -> C.Voice $ V.NoteOff k v)
          <$> (V.toPitch . int7Bits <$> getFirstByte)
          <*> (V.toVelocity . int7Bits <$> getWord8)
        0x9 -> (\k v -> C.Voice $ V.NoteOn k v)
          <$> (V.toPitch . int7Bits <$> getFirstByte)
          <*> (V.toVelocity . int7Bits <$> getWord8)
        0xA -> (\k v -> C.Voice $ V.PolyAftertouch k v)
          <$> (V.toPitch . int7Bits <$> getFirstByte)
          <*> (int7Bits <$> getWord8)
        0xB -> do
          c <- int7Bits <$> getFirstByte
          v <- int7Bits <$> getWord8
          case (c, v) of
            (120, 0)   -> return $ C.Mode Mode.AllSoundOff
            (121, 0)   -> return $ C.Mode Mode.ResetAllControllers
            (122, 0)   -> return $ C.Mode $ Mode.LocalControl False
            (122, 127) -> return $ C.Mode $ Mode.LocalControl True
            (123, 0)   -> return $ C.Mode Mode.AllNotesOff
            (124, 0)   -> return $ C.Mode $ Mode.OmniMode False
            (125, 0)   -> return $ C.Mode $ Mode.OmniMode True
            (126, _)   -> return $ C.Mode $ Mode.MonoMode v
            (127, 0)   -> return $ C.Mode Mode.PolyMode
            _          -> return $ C.Voice $ V.Control (V.toController c) v
        0xC -> C.Voice . V.ProgramChange
          <$> (V.toProgram . int7Bits <$> getFirstByte)
        0xD -> C.Voice . V.MonoAftertouch
          <$> (int7Bits <$> getFirstByte)
        0xE -> do
          x <- getFirstByte
          y <- getWord8
          return $ C.Voice $ V.PitchBend $
            int7Bits x + int7Bits y * 0x80
        -- 0xF is meta/sysex, handled above
        _ -> fail $ "Unknown event byte: " <> show statusByte
      return (e, Just $ Status statusByte)
  return (tks, e, running')
