{-# LANGUAGE RecordWildCards #-}
module RockBand.Save where

import           Control.Monad        (guard, replicateM)
import           Data.Binary.Get
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Word

data RB3Save = RB3Save
  { sUnknown1       :: Word32
  , sAwards         :: [Award]
  , sUnknown2       :: B.ByteString -- TODO mostly zeroes but some stuff at the end of this
  , sCharacterCount :: Word32
  , sCharacters     :: [B.ByteString]
  -- now we're at 0x194743 in save.dat
  , sUnknown3       :: B.ByteString
  -- now 0x2C7123
  , sUnknown4       :: B.ByteString -- looks like garbage/encrypted
  -- now 0x42245F
  , sUnknown5       :: B.ByteString
  -- now 0x43A76F
  , sBandName       :: B.ByteString
  , sUnknown6       :: BL.ByteString
  } deriving (Show)

data Award = Award
  { aKey   :: B.ByteString
  , aValue :: Word32
  } deriving (Show)

readFixedString :: Int -> Get B.ByteString
readFixedString maxLen = do
  len <- getWord32le
  bytes <- getByteString maxLen
  let (x, zeroes) = B.break (== 0) bytes
  guard $ B.length x == fromIntegral len
  guard $ B.all (== 0) zeroes
  return x

readRB3SaveDat :: Get RB3Save
readRB3SaveDat = do
  sUnknown1 <- getWord32le
  awardLen <- getWord32le
  sAwards <- replicateM (fromIntegral awardLen) $ do
    aKey <- readFixedString 46
    aValue <- getWord32le
    return Award{..}
  sUnknown2 <- getByteString 290871 -- TODO
  sCharacterCount <- getWord32le
  sCharacters <- replicateM 10 $ do
    getByteString 134902
  sUnknown3 <- getByteString $ 0x2C7123 - 0x194743
  sUnknown4 <- getByteString $ 0x42245F - 0x2C7123
  sUnknown5 <- getByteString $ 0x43A76F - 0x42245F
  sBandName <- readFixedString 46 -- don't know actual max length
  sUnknown6 <- getRemainingLazyByteString
  return RB3Save{..}
