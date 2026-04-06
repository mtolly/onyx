{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Amplitude.PS2.Bnk
( bnkChunks
, SAMPEntry(..)
, SDESEntry(..)
, INSTEntry(..)
, BANKEntry(..)
, Chunk(..)
) where

import           Control.Monad   (replicateM)
import           Data.Binary.Get
import qualified Data.ByteString as B
import           Data.Word

data SAMPEntry = SAMPEntry
  { sampChannels     :: Int -- always 1, just a guess
  , sampRate         :: Int
  , sampFilePosition :: Int
  } deriving (Eq, Show)

data SDESEntry = SDESEntry
  { sdesMinPitch   :: Int
  , sdesMaxPitch   :: Int
  , sdesBasePitch  :: Int
  , sdesTranspose  :: Int -- semitones to move. seen in supersprode bass
  , sdesPan        :: Int -- 0x00 is left, 0x40 is center, 0x7F is right
  , sdesSAMPNumber :: Int
  -- lots of other parts not deciphered yet
  , sdesBytes      :: B.ByteString
  } deriving (Eq, Show)

data INSTEntry = INSTEntry
  { instProgNumber :: Int
  , instSDESCount  :: Int
  -- other parts not deciphered yet
  , instBytes      :: B.ByteString
  } deriving (Eq, Show)

data BANKEntry = BANKEntry
  { bankNumber    :: Int
  , bankINSTCount :: Int
  -- other parts not deciphered yet
  , bankBytes     :: B.ByteString
  } deriving (Eq, Show)

data Chunk
  = SAMP [SAMPEntry] -- samples
  | SANM [B.ByteString] -- sample names
  | SAFN [B.ByteString] -- sample filenames
  | BANK BANKEntry -- bank(s?)
  | BKNM B.ByteString -- bank name
  | INST [INSTEntry] -- instruments
  | INNM [B.ByteString] -- instrument names
  | SDES [SDESEntry] -- sample... directives?
  | SDNM [B.ByteString] -- SDES names
  deriving (Eq, Show)

sizedArea :: (B.ByteString -> Get a) -> Get a
sizedArea g = do
  size <- getWord32le
  bs <- lookAhead $ getByteString $ fromIntegral size
  startPosn <- bytesRead
  x <- g bs
  endPosn <- bytesRead
  if startPosn + fromIntegral size == endPosn
    then return x
    else fail $ unwords
      [ "Struct with size at", show startPosn
      , "should've been", show size
      , "bytes but was", show (endPosn - startPosn)
      ]

getSAMPEntry :: Get SAMPEntry
getSAMPEntry = sizedArea \_ -> do
  chans <- getWord32le -- guessing this is channels, all 1 in files i've seen
  rate <- getWord32le
  0 <- getWord32le -- dunno what this space is for
  0 <- getWord16le
  posn <- getWord32le
  return $ SAMPEntry (fromIntegral chans) (fromIntegral rate) (fromIntegral posn)

getSDESEntry :: Get SDESEntry
getSDESEntry = sizedArea \bs -> do
  endbytes <- getWord32le -- size of some unknown variable field at end
  minPitch <- getWord8
  maxPitch <- getWord8
  basePitch <- getWord8
  transpose <- getInt8 -- usually 0. used in supersprode bass
  _ <- getByteString 12 -- lots of unknown stuff
  _vol <- getWord8 -- I previously thought this was volume but don't think so now
  pan <- getWord8
  samp <- getWord8
  _ <- getByteString $ 3 + fromIntegral endbytes -- all 0
  return $ SDESEntry
    (fromIntegral minPitch)
    (fromIntegral maxPitch)
    (fromIntegral basePitch)
    (fromIntegral transpose)
    (fromIntegral pan)
    (fromIntegral samp)
    bs

getINSTEntry :: Get INSTEntry
getINSTEntry = sizedArea \bs -> do
  _ <- getWord32le -- observed 1
  prog <- getWord16le
  _ <- getWord16le -- unknown
  _ <- getWord16le -- observed 0
  sdes <- getWord16le -- number of SDES entries for this instrument
  return $ INSTEntry (fromIntegral prog) (fromIntegral sdes) bs

getBANK :: Word32 -> Get BANKEntry
getBANK n = do
  bs <- getByteString $ fromIntegral n
  let bankNum = B.index bs 8
      instCount = B.index bs 11
  -- I assume you'd use the inst count with multiple BANK entries in a .bnk,
  -- but I haven't seen that yet.
  return $ BANKEntry (fromIntegral bankNum) (fromIntegral instCount) bs

getSAMP :: Word32 -> Get [SAMPEntry]
getSAMP n = case quotRem n 22 of
  (samps, 0) -> replicateM (fromIntegral samps) getSAMPEntry
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 22"

getINST :: Word32 -> Get [INSTEntry]
getINST n = case quotRem n 16 of
  (insts, 0) -> replicateM (fromIntegral insts) getINSTEntry
  _          -> fail $ "SAMP length of " <> show n <> " not divisible by 16"

getSDES :: Word32 -> Get [SDESEntry]
getSDES n = do
  endPosn <- (+ fromIntegral n) <$> bytesRead
  let go = bytesRead >>= \br -> case compare br endPosn of
        EQ -> return []
        LT -> do
          ent <- getSDESEntry
          (ent :) <$> go
        GT -> fail "SDES went over its chunk length"
  go

getString :: Get B.ByteString
getString = do
  len <- getWord32le
  getByteString $ fromIntegral len

getStringList :: Word32 -> Get [B.ByteString]
getStringList n = do
  endPosn <- (+ fromIntegral n) <$> bytesRead
  1 <- getWord32le -- dunno what this is
  let go = bytesRead >>= \br -> case compare br endPosn of
        EQ -> return []
        LT -> do
          str <- getString
          (str :) <$> go
        GT -> fail "SANM/SAFN went over its chunk length"
  go

bnkChunks :: Get [Chunk]
bnkChunks = isEmpty >>= \case
  True -> return []
  False -> do
    ctype <- getByteString 4
    size <- getWord32le
    chunk <- case ctype of
      "SAMP" -> SAMP <$> getSAMP size
      "SANM" -> SANM <$> getStringList size
      "SAFN" -> SAFN <$> getStringList size
      "BANK" -> BANK <$> getBANK size
      "BKNM" -> do
        1 <- getWord32le -- dunno
        BKNM <$> getString
      "INST" -> INST <$> getINST size
      "INNM" -> INNM <$> getStringList size
      "SDES" -> SDES <$> getSDES size
      "SDNM" -> SDNM <$> getStringList size
      _ -> fail $ "Unknown chunk type " <> show ctype
    (chunk :) <$> bnkChunks
