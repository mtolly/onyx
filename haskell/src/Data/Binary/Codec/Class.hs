{-# LANGUAGE ImplicitParams #-}
module Data.Binary.Codec.Class
( module Control.Monad.Codec
, module Data.Binary.Codec
, module Data.Binary.Get
, module Data.Binary.Put
, module Data.Int
, module Data.Word
, module GHC.ByteOrder
, Bin(..)
, BinEndian(..)
, fixedArray
) where

import           Control.Monad       (forM_, replicateM)
import           Control.Monad.Codec
import           Data.Binary.Codec
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Int
import           Data.Word
import           GHC.ByteOrder

class Bin a where
  bin :: BinaryCodec a

class BinEndian a where
  binEndian :: (?endian :: ByteOrder) => BinaryCodec a

instance Bin Int8 where
  bin = int8

instance BinEndian Int16 where
  binEndian = case ?endian of
    BigEndian    -> int16be
    LittleEndian -> int16le

instance BinEndian Int32 where
  binEndian = case ?endian of
    BigEndian    -> int32be
    LittleEndian -> int32le

instance BinEndian Int64 where
  binEndian = case ?endian of
    BigEndian    -> int64be
    LittleEndian -> int64le

instance Bin Word8 where
  bin = word8

instance BinEndian Word16 where
  binEndian = case ?endian of
    BigEndian    -> word16be
    LittleEndian -> word16le

instance BinEndian Word32 where
  binEndian = case ?endian of
    BigEndian    -> word32be
    LittleEndian -> word32le

instance BinEndian Word64 where
  binEndian = case ?endian of
    BigEndian    -> word64be
    LittleEndian -> word64le

instance BinEndian Float where
  binEndian = case ?endian of
    BigEndian    -> Codec getFloatbe (fmapArg putFloatbe)
    LittleEndian -> Codec getFloatle (fmapArg putFloatle)

instance BinEndian Double where
  binEndian = case ?endian of
    BigEndian    -> Codec getDoublebe (fmapArg putDoublebe)
    LittleEndian -> Codec getDoublele (fmapArg putDoublele)

fixedArray :: Int -> BinaryCodec a -> BinaryCodec [a]
fixedArray n c = Codec
  { codecIn = replicateM n $ codecIn c
  , codecOut = fmapArg $ \xs -> if length xs == n
    then forM_ xs $ codecOut c
    else error $ "Expected an array of size " <> show n
  }
