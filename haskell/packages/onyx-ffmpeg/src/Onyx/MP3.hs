{-
Claude wrote this
-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Onyx.MP3
( MP3GaplessInfo(..)
, stripMP3
) where

import           Control.Applicative   ((<|>))
import           Control.Monad         (guard)
import           Data.Bits             (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Word             (Word32, Word64)
import           Onyx.Util.Handle      (Readable (..), subHandle, useHandle)
import           System.IO             (Handle, SeekMode (..), hFileSize, hSeek,
                                        hTell)

data MP3GaplessInfo = MP3GaplessInfo
  { startDelay      :: Word64
  , endPadding      :: Word64
  , originalSamples :: Maybe Word64
  } deriving (Show, Eq)

-- | Strip ID3v2 tags, Xing\/Info\/VBRI metadata frames from the front of an
-- MP3 'Readable', returning a 'Readable' pointing at the first real audio
-- frame and any gapless playback info that was found in the metadata.
stripMP3 :: Readable -> IO (Readable, Maybe MP3GaplessInfo)
stripMP3 readable = useHandle readable \h -> do
  fileSize <- hFileSize h
  hSeek h AbsoluteSeek 0

  -- 1. Skip ID3v2 tags, collecting iTunSMPB gapless info
  itunesGapless <- skipID3v2Tags h

  -- 2. Scan for first real audio frame, extracting LAME gapless from Xing frames
  result <- findFirstAudioFrame h fileSize

  case result of
    Nothing -> do
      -- No audio found; return the original readable unchanged
      return (readable, Nothing)
    Just (audioOffset, lameGapless) -> do
      let gapless = lameGapless <|> itunesGapless
          len     = fileSize - audioOffset
          r'      = subHandle (<> " [MP3 stripped]") audioOffset (Just len) readable
      return (r', gapless)

--------------------------------------------------------------------------------
-- ID3v2 parsing
--------------------------------------------------------------------------------

-- | Skip all ID3v2 tags at the current position. Returns iTunSMPB info if found.
skipID3v2Tags :: Handle -> IO (Maybe MP3GaplessInfo)
skipID3v2Tags h = go Nothing
  where
    go acc = do
      pos <- hTell h
      header <- B.hGet h 10
      if B.length header < 10 || B.take 3 header /= "ID3"
        then do
          -- Not an ID3v2 tag; seek back
          hSeek h AbsoluteSeek pos
          return acc
        else do
          let tagSize   = syncsafeSize (B.drop 6 header)
              flags     = B.index header 5
              hasFooter = testBit flags 4
              total     = fromIntegral tagSize + if hasFooter then 10 else 0
          -- Read the tag body to look for iTunSMPB
          tagBody <- B.hGet h total
          let iTunes = case acc of
                Just _  -> acc  -- already found
                Nothing -> extractITunesSMPB tagBody
          -- Position is now past this tag; check for another
          go iTunes

-- | Decode a 4-byte synchsafe integer (7 bits per byte).
syncsafeSize :: B.ByteString -> Word32
syncsafeSize bs
  | B.length bs < 4 = 0
  | otherwise =
      (fromIntegral (B.index bs 0) `shiftL` 21) .|.
      (fromIntegral (B.index bs 1) `shiftL` 14) .|.
      (fromIntegral (B.index bs 2) `shiftL`  7) .|.
       fromIntegral (B.index bs 3)

-- | Search for "iTunSMPB" in a bytestring and parse the hex fields.
-- Format: " 00000000 00000AAC 00000210 00000000003F4000"
extractITunesSMPB :: B.ByteString -> Maybe MP3GaplessInfo
extractITunesSMPB bs = do
  idx <- findSubstring "iTunSMPB" bs
  let after = B.drop (idx + 8) bs
  -- Find start of hex data (skip nulls, whitespace, frame overhead)
  let hexPart = B8.dropWhile (\c -> not (isHexChar c)) after
  -- Parse 4 hex tokens: unknown, start_delay, end_padding, total_samples
  (_, rest1) <- parseHexToken hexPart
  (delay, rest2) <- parseHexToken rest1
  (pad, rest3) <- parseHexToken rest2
  (total, _) <- parseHexToken rest3
  return MP3GaplessInfo
    { startDelay      = delay
    , endPadding      = pad
    , originalSamples = Just total
    }

findSubstring :: B.ByteString -> B.ByteString -> Maybe Int
findSubstring needle haystack = go 0
  where
    nlen = B.length needle
    hlen = B.length haystack
    go i
      | i + nlen > hlen = Nothing
      | B.take nlen (B.drop i haystack) == needle = Just i
      | otherwise = go (i + 1)

parseHexToken :: B.ByteString -> Maybe (Word64, B.ByteString)
parseHexToken bs0 = let
  bs = B8.dropWhile (== ' ') bs0
  (digits, rest) = B8.span isHexChar bs
  in if B.null digits
     then Nothing
     else Just (parseHex digits, rest)

parseHex :: B.ByteString -> Word64
parseHex = B.foldl' step 0
  where
    step acc b
      | b >= 0x30 && b <= 0x39 = acc * 16 + fromIntegral (b - 0x30)
      | b >= 0x61 && b <= 0x66 = acc * 16 + fromIntegral (b - 0x61 + 10)
      | b >= 0x41 && b <= 0x46 = acc * 16 + fromIntegral (b - 0x41 + 10)
      | otherwise              = acc

isHexChar :: Char -> Bool
isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

--------------------------------------------------------------------------------
-- MPEG frame parsing
--------------------------------------------------------------------------------

data FrameHeader = FrameHeader
  { fhVersion     :: !Int  -- 0=2.5, 1=reserved, 2=MPEG2, 3=MPEG1
  , fhLayer       :: !Int  -- 0=reserved, 1=III, 2=II, 3=I
  , fhBitrate     :: !Int  -- kbps
  , fhSampleRate  :: !Int  -- Hz
  , fhPadding     :: !Bool
  , fhChannelMode :: !Int  -- 0=stereo,1=joint,2=dual,3=mono
  , fhFrameSize   :: !Int  -- bytes
  }

-- Bitrate tables: [version_is_1][layer_index - 1][bitrate_index]
bitrateTable :: Bool -> Int -> Int -> Int
bitrateTable ver1 layer brIdx = case (ver1, layer, brIdx) of
  -- MPEG 2/2.5, Layer III & II
  (False, 1, i) -> [0,8,16,24,32,40,48,56,64,80,96,112,128,144,160,-1] !! i
  (False, 2, i) -> [0,8,16,24,32,40,48,56,64,80,96,112,128,144,160,-1] !! i
  -- MPEG 2/2.5, Layer I
  (False, 3, i) -> [0,32,48,56,64,80,96,112,128,144,160,176,192,224,256,-1] !! i
  -- MPEG 1, Layer III
  (True,  1, i) -> [0,32,40,48,56,64,80,96,112,128,160,192,224,256,320,-1] !! i
  -- MPEG 1, Layer II
  (True,  2, i) -> [0,32,48,56,64,80,96,112,128,160,192,224,256,320,384,-1] !! i
  -- MPEG 1, Layer I
  (True,  3, i) -> [0,32,64,96,128,160,192,224,256,288,320,352,384,416,448,-1] !! i
  _ -> -1

sampleRateTable :: Int -> Int -> Int
sampleRateTable ver srIdx = case (ver, srIdx) of
  (0, 0) -> 11025; (0, 1) -> 12000; (0, 2) ->  8000
  (2, 0) -> 22050; (2, 1) -> 24000; (2, 2) -> 16000
  (3, 0) -> 44100; (3, 1) -> 48000; (3, 2) -> 32000
  _      -> -1

samplesPerFrame :: Bool -> Int -> Int
samplesPerFrame ver1 layer = case (ver1, layer) of
  (False, 1) ->  576; (False, 2) -> 1152; (False, 3) -> 384
  (True,  1) -> 1152; (True,  2) -> 1152; (True,  3) -> 384
  _          -> 0

-- | Try to parse 4 bytes as an MPEG audio frame header.
parseFrameHeader :: B.ByteString -> Maybe FrameHeader
parseFrameHeader bs
  | B.length bs < 4 = Nothing
  | otherwise = do
      let b0 = B.index bs 0
          b1 = B.index bs 1
          b2 = B.index bs 2
          b3 = B.index bs 3
      -- Sync: 11 set bits
      guard $ b0 == 0xFF && (b1 .&. 0xE0) == 0xE0
      let version = fromIntegral $ (b1 `shiftR` 3) .&. 0x03
          layer   = fromIntegral $ (b1 `shiftR` 1) .&. 0x03
          brIdx   = fromIntegral $ (b2 `shiftR` 4) .&. 0x0F
          srIdx   = fromIntegral $ (b2 `shiftR` 2) .&. 0x03
          pad     = testBit b2 1
          chMode  = fromIntegral $ (b3 `shiftR` 6) .&. 0x03
      guard $ version /= 1 && layer /= 0 -- not reserved
      guard $ brIdx /= 0 && brIdx /= 15  -- not free / bad
      guard $ srIdx /= 3                 -- not reserved
      let ver1    = version == 3
          bitrate = bitrateTable ver1 layer brIdx * 1000
          sr      = sampleRateTable version srIdx
          spf     = samplesPerFrame ver1 layer
      guard $ bitrate > 0 && sr > 0 && spf > 0
      let padBytes = if pad then (if layer == 3 then 4 else 1) else 0
          frameSize = if layer == 3
                      then (12 * bitrate `div` sr + padBytes) * 4
                      else spf `div` 8 * bitrate `div` sr + padBytes
      guard $ frameSize >= 4
      return FrameHeader
        { fhVersion     = version
        , fhLayer       = layer
        , fhBitrate     = bitrate `div` 1000
        , fhSampleRate  = sr
        , fhPadding     = pad
        , fhChannelMode = chMode
        , fhFrameSize   = frameSize
        }

-- | Byte offset within a frame where "Xing"/"Info" tag appears.
xingOffset :: FrameHeader -> Int
xingOffset fh
  | fh.fhVersion == 3 = if fh.fhChannelMode == 3 then 21 else 36
  | otherwise         = if fh.fhChannelMode == 3 then 13 else 21

-- | Check if a frame's payload contains a Xing or Info tag.
isXingFrame :: FrameHeader -> B.ByteString -> Bool
isXingFrame fh frameData =
  let off = xingOffset fh
  in  B.length frameData > off + 4 &&
      (B.take 4 (B.drop off frameData) == "Xing" ||
       B.take 4 (B.drop off frameData) == "Info")

-- | Check if a frame's payload contains a VBRI tag (always at offset 36).
isVBRIFrame :: B.ByteString -> Bool
isVBRIFrame frameData =
  B.length frameData > 40 && B.take 4 (B.drop 36 frameData) == "VBRI"

--------------------------------------------------------------------------------
-- LAME gapless extraction
--------------------------------------------------------------------------------

-- | Extract LAME encoder delay/padding from a Xing/Info frame.
extractLAMEGapless :: FrameHeader -> B.ByteString -> Maybe MP3GaplessInfo
extractLAMEGapless fh frameData = do
  let off = xingOffset fh
  guard $ B.length frameData >= off + 8
  let tag = B.drop off frameData
  -- Parse Xing flags to find where LAME tag starts
  guard $ B.length tag >= 8
  let flags = (fromIntegral (B.index tag 4) :: Word32) `shiftL` 24
          .|. (fromIntegral (B.index tag 5) :: Word32) `shiftL` 16
          .|. (fromIntegral (B.index tag 6) :: Word32) `shiftL`  8
          .|.  fromIntegral (B.index tag 7) :: Word32
      -- Count variable-size fields
      skip = 8
           + (if flags .&. 0x01 /= 0 then 4   else 0) -- frames
           + (if flags .&. 0x02 /= 0 then 4   else 0) -- bytes
           + (if flags .&. 0x04 /= 0 then 100 else 0) -- TOC
           + (if flags .&. 0x08 /= 0 then 4   else 0) -- quality
      lame = B.drop skip tag
  -- Need at least 24 bytes for encoder string (9) + fields up to delay (12 more)
  guard $ B.length lame >= 24
  -- Verify encoder string is printable ASCII
  let encoderStr = B.take 9 lame
  guard $ B.all (\b -> b >= 0x20 && b <= 0x7E) encoderStr
  -- Delay/padding at offset 21: 3 bytes, two 12-bit fields
  let b21 = fromIntegral (B.index lame 21) :: Word32
      b22 = fromIntegral (B.index lame 22) :: Word32
      b23 = fromIntegral (B.index lame 23) :: Word32
      combined = (b21 `shiftL` 16) .|. (b22 `shiftL` 8) .|. b23
      delay = fromIntegral $ combined `shiftR` 12
      padding = fromIntegral $ combined .&. 0xFFF
  return MP3GaplessInfo
    { startDelay      = delay
    , endPadding      = padding
    , originalSamples = Nothing
    }

--------------------------------------------------------------------------------
-- Frame scanning
--------------------------------------------------------------------------------

-- | Find the first real audio frame, skipping Xing/Info/VBRI metadata frames.
-- Returns the absolute file offset and any LAME gapless info found.
findFirstAudioFrame :: Handle -> Integer -> IO (Maybe (Integer, Maybe MP3GaplessInfo))
findFirstAudioFrame h fileSize = go Nothing
  where
    go lameInfo = do
      pos <- hTell h
      if pos >= fileSize
        then return Nothing
        else do
          -- Read 4 bytes to check for a sync word
          headerBytes <- B.hGet h 4
          if B.length headerBytes < 4
            then return Nothing
            else case parseFrameHeader headerBytes of
              Nothing -> do
                -- Not a valid header at this position; advance by 1
                hSeek h AbsoluteSeek (pos + 1)
                go lameInfo
              Just fh -> do
                let fsize = fh.fhFrameSize
                if pos + fromIntegral fsize > fileSize
                  then do
                    -- Frame extends past EOF, skip this byte
                    hSeek h AbsoluteSeek (pos + 1)
                    go lameInfo
                  else do
                    -- Read the rest of the frame
                    restBytes <- B.hGet h (fsize - 4)
                    let frameData = headerBytes <> restBytes
                    if isXingFrame fh frameData || isVBRIFrame frameData
                      then do
                        -- Extract LAME info before skipping
                        let newLame = case lameInfo of
                              Just _  -> lameInfo
                              Nothing -> extractLAMEGapless fh frameData
                        -- Skip past this metadata frame
                        hSeek h AbsoluteSeek (pos + fromIntegral fsize)
                        go newLame
                      else do
                        -- Validate: check that a second frame header follows
                        let nextPos = pos + fromIntegral fsize
                        validated <- if nextPos + 4 <= fileSize
                              then do
                                hSeek h AbsoluteSeek nextPos
                                nextHeader <- B.hGet h 4
                                return $ B.length nextHeader == 4 &&
                                         case parseFrameHeader nextHeader of
                                           Just _  -> True
                                           Nothing -> False
                              else
                                -- Last frame in file; accept it
                                return True
                        if validated
                          then return $ Just (pos, lameInfo)
                          else do
                            -- False sync; advance by 1
                            hSeek h AbsoluteSeek (pos + 1)
                            go lameInfo
