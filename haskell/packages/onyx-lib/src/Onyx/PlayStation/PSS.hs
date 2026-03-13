{-
MPEG structures decoded with the help of
https://unix4lyfe.org/mpeg1/
http://dvd.sourceforge.net/dvdinfo/mpeghdrs.html
http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
module Onyx.PlayStation.PSS where

import           Control.Monad        (forM)
import           Data.Binary.Get
import           Data.Bits            (Bits, bit, testBit, zeroBits, (.&.),
                                       (.|.))
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int64)
import           Data.Word            (Word16, Word32, Word8)
import           Numeric
import           Onyx.Util.Binary     (runGetM)
import           System.IO

data Packet a
  = PackStart PackHeader
  | Packet Word8 a
  deriving (Show)

data PackHeader = PackHeader
  { scr              :: Word32
  , scr_ext          :: Word16
  , program_mux_rate :: Word32
  } deriving (Show)

scanPackets :: Handle -> IO [(Int, Packet (Int, Int))]
scanPackets h = go [] 0 where
  go packets !posn = hIsEOF h >>= \case
    True -> return $ reverse packets -- seems to happen in VOB, but RB PSS should use B9 explicit end
    False -> do
      pktID <- B.hGet h 4
      let unknownID = fail $ "Unrecognized packet ID " <> show pktID <> " at position " <> showHex posn ""
      case B.unpack <$> B.stripPrefix "\x00\x00\x01" pktID of
        Just [byte] -> case byte of
          0xB9 -> return $ reverse packets -- end indicator
          0xBA -> readPackHeader h >>= \case
            Left  err    -> fail err
            Right header -> go ((posn, PackStart header) : packets) (posn + 14)
          _ -> do
            dataLen <- fmap fromIntegral $ BL.hGet h 2 >>= runGetM getWord16be
            let dataPosn = posn + 6
            hSeek h RelativeSeek $ fromIntegral dataLen
            go ((posn, Packet byte (dataPosn, dataLen)) : packets) (posn + 6 + dataLen)
        _ -> unknownID

fromBitsBE :: (Bits a) => [Bool] -> a
fromBitsBE bs = foldr (.|.) zeroBits [ bit i | (i, True) <- zip [0..] $ reverse bs ]

-- https://dvd.sourceforge.net/dvdinfo/packhdr.html
readPackHeader :: Handle -> IO (Either String PackHeader)
readPackHeader h = do
  mainHeader <- B.hGet h 10
  case B.unpack mainHeader of
    [b4, b5, b6, b7, b8, b9, b10, b11, b12, b13] -> let
      expectedBits =
        [ not $ b4 `testBit` 7
        ,       b4 `testBit` 6
        ,       b4 `testBit` 2
        ,       b6 `testBit` 2
        ,       b8 `testBit` 2
        ,       b9 `testBit` 0
        ,      b12 `testBit` 1
        ,      b12 `testBit` 0
        ]
      pack_stuffing_length = fromIntegral $ b13 .&. 0x03
      in if not $ and expectedBits
        then return $ Left "Unexpected control bit in pack header"
        else do
          _ <- B.hGet h pack_stuffing_length
          return $ Right PackHeader
            { scr = fromBitsBE $ concat
              [ map (b4 `testBit`) [5, 4, 3, 1, 0]
              , map (b5 `testBit`) [7, 6 .. 0]
              , map (b6 `testBit`) [7, 6, 5, 4, 3, 1, 0]
              , map (b7 `testBit`) [7, 6 .. 0]
              , map (b8 `testBit`) [7, 6, 5, 4, 3]
              ]
            , scr_ext = fromBitsBE $ concat
              [ map (b8 `testBit`) [1, 0]
              , map (b9 `testBit`) [7, 6, 5, 4, 3, 2, 1]
              ]
            , program_mux_rate = fromBitsBE $ concat
              [ map (b10 `testBit`) [7, 6 .. 0]
              , map (b11 `testBit`) [7, 6 .. 0]
              , map (b12 `testBit`) [7, 6, 5, 4, 3, 2]
              ]
            }
    _ -> return $ Left "Not enough bytes for pack header"

data PESExtension = PESExtension
  { pesScramblingControl   :: Word8
  , pesPriority            :: Bool
  , dataAlignmentIndicator :: Bool
  , copyright              :: Bool
  , original               :: Bool
  , timeStamps             :: PESTimestamps
  , escr                   :: Bool
  , esRate                 :: Bool
  , dsmTrickMode           :: Bool
  , additionalCopyInfo     :: Bool
  , pesCRC                 :: Bool
  , pesExtension           :: Bool
  , headerData             :: B.ByteString
  , totalExtensionLength   :: Int
  } deriving (Show)

data PESTimestamps
  = TimeNone
  | TimeForbidden
  | TimePTS Word32
  | TimePTSDTS Word32 Word32
  deriving (Show)

readPTSDTS :: B.ByteString -> Maybe (Word32, B.ByteString)
readPTSDTS bs = case B.splitAt 5 bs of
  (xs, ys) -> case B.unpack xs of
    [b0, b1, b2, b3, b4] -> Just
      ( fromBitsBE $ concat
        [ map (b0 `testBit`) [3, 2, 1]
        , map (b1 `testBit`) [7, 6 .. 0]
        , map (b2 `testBit`) [7, 6 .. 1]
        , map (b3 `testBit`) [7, 6 .. 0]
        , map (b4 `testBit`) [7, 6 .. 1]
        ]
      , ys
      )
    _ -> Nothing

readPESExtension :: Handle -> IO (Either String PESExtension)
readPESExtension h = do
  three <- B.hGet h 3
  case B.unpack three of
    [b6, b7, b8] -> do
      header <- B.hGet h $ fromIntegral b8
      (timestamps, header') <- case (b7 `testBit` 7, b7 `testBit` 6) of
        (False, False) -> return (TimeNone, header)
        (False, True ) -> return (TimeForbidden, header)
        (True , False) -> case readPTSDTS header of
          Nothing             -> return (TimeForbidden, header) -- should error
          Just (pts, header') -> return (TimePTS pts, header')
        (True , True ) -> case readPTSDTS header of
          Nothing -> return (TimeForbidden, header) -- should error
          Just (pts, header') -> case readPTSDTS header' of
            Nothing              -> return (TimeForbidden, header) -- should error
            Just (dts, header'') -> return (TimePTSDTS pts dts, header'')
      return $ Right PESExtension
        { pesScramblingControl   = fromBitsBE [b6 `testBit` 5, b6 `testBit` 4]
        , pesPriority            = b6 `testBit` 3
        , dataAlignmentIndicator = b6 `testBit` 2
        , copyright              = b6 `testBit` 1
        , original               = b6 `testBit` 0
        , timeStamps             = timestamps
        , escr                   = b7 `testBit` 5
        , esRate                 = b7 `testBit` 4
        , dsmTrickMode           = b7 `testBit` 3
        , additionalCopyInfo     = b7 `testBit` 2
        , pesCRC                 = b7 `testBit` 1
        , pesExtension           = b7 `testBit` 0
        , headerData             = header'
        , totalExtensionLength   = 3 + fromIntegral b8
        }
    _ -> return $ Left "Not enough bytes for PES extension"

extractVideoStream :: Word8 -> [(Int, Packet (Int, Int))] -> Handle -> IO BL.ByteString
extractVideoStream videoID pkts h = do
  let matchPackets = [(pos, len) | (_, Packet pktID (pos, len)) <- pkts, videoID == pktID]
  fmap BL.concat $ forM matchPackets \(pos, len) -> do
    -- Packet starts with extension described at http://dvd.sourceforge.net/dvdinfo/pes-hdr.html
    hSeek h AbsoluteSeek $ fromIntegral pos
    readPESExtension h >>= \case
      Left  err -> fail err
      Right ext -> BL.hGet h $ len - ext.totalExtensionLength

extractVGSStream :: Word8 -> [(Int, Packet (Int, Int))] -> Handle -> IO BL.ByteString
extractVGSStream vgsID pkts h = do
  let matchPackets = [(pos, len) | (_, Packet pktID (pos, len)) <- pkts, vgsID == pktID]
  fmap BL.concat $ forM matchPackets \(pos, len) -> do
    -- Packet starts with PES extension and then some extra private audio stuff
    hSeek h AbsoluteSeek $ fromIntegral pos
    readPESExtension h >>= \case
      Left  err -> fail err
      Right ext -> do
        _substreamID <- B.hGet h 1 -- 0xFF
        _unknown     <- B.hGet h 3 -- 0x90 0x00 0x01
        BL.hGet h $ len - ext.totalExtensionLength - 4

findSubstring :: BL.ByteString -> BL.ByteString -> Maybe Int64
findSubstring needle haystack = go 0
  where
    nlen = BL.length needle
    hlen = BL.length haystack
    go i
      | i + nlen > hlen = Nothing
      | BL.take nlen (BL.drop i haystack) == needle = Just i
      | otherwise = go (i + 1)

splitVideoStream :: BL.ByteString -> [(Word8, BL.ByteString)]
splitVideoStream orig = let
  find001 = findSubstring $ BL.pack [0, 0, 1]
  after001 bs = case BL.uncons bs of
    Nothing               -> []
    Just (packetID, rest) -> case find001 rest of
      Nothing -> [(packetID, rest)]
      Just i  -> let
        (xs, ys) = BL.splitAt i rest
        in (packetID, xs) : after001 (BL.drop 3 ys)
  in case find001 orig of
    Nothing -> []
    Just i  -> after001 $ BL.drop (i + 3) orig
