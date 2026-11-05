{- |
LZSS variant by Haruhiko Okumura used by Neversoft games,
for compiled scripts in .qb files and for whole files
on the GH3/GHA PS3 discs (under DATA/COMPRESSED/PS3)
-}
{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE BlockArguments #-}
module Onyx.Neversoft.LZSS (decompressLZSS) where

import           Control.Monad                (forM_)
import           Control.Monad.ST             (runST)
import           Data.Bits                    (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString              as B
import           Data.ByteString.Internal     (fromForeignPtr0)
import qualified Data.ByteString.Unsafe       as BU
import           Data.STRef
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Data.Vector.Unboxed.Mutable  as MVU
import           Data.Word                    (Word8)

ringSize, maxMatch :: Int
ringSize = 4096
maxMatch = 18

decompressLZSS :: B.ByteString -> B.ByteString
decompressLZSS input = runST do
  ring <- MVU.replicate ringSize (0x20 :: Word8)
  outRef <- MVS.new (max 64 $ B.length input * 4) >>= newSTRef
  let n = B.length input
      byteAt i = BU.unsafeIndex input i

      emit len r c = do
        out <- readSTRef outRef
        out' <- if len < MVS.length out
          then return out
          else do
            bigger <- MVS.grow out $ MVS.length out
            writeSTRef outRef bigger
            return bigger
        MVS.write out' len c
        MVU.write ring r c

      -- shift to the next flag bit, reading a new flag byte every 8 units
      next !i !r !flags !len = let flags' = flags `shiftR` 1 :: Int in
        if flags' .&. 0x100 == 0
          then if i >= n
            then return len
            else unit (i + 1) r (fromIntegral (byteAt i) .|. 0xFF00) len
          else unit i r flags' len

      unit !i !r !flags !len
        | flags .&. 1 /= 0 = if i >= n
          then return len
          else do
            emit len r $ byteAt i
            next (i + 1) ((r + 1) .&. (ringSize - 1)) flags (len + 1)
        | i + 1 >= n = return len
        | otherwise = do
          let a = fromIntegral (byteAt i) :: Int
              b = fromIntegral (byteAt $ i + 1) :: Int
              pos = a .|. ((b .&. 0xF0) `shiftL` 4)
              count = (b .&. 0x0F) + 3
          forM_ [0 .. count - 1] \k -> do
            c <- MVU.read ring $ (pos + k) .&. (ringSize - 1)
            emit (len + k) ((r + k) .&. (ringSize - 1)) c
          next (i + 2) ((r + count) .&. (ringSize - 1)) flags (len + count)

  len <- next 0 (ringSize - maxMatch) 0 0
  out <- readSTRef outRef
  frozen <- VS.freeze $ MVS.take len out
  let (fptr, size) = VS.unsafeToForeignPtr0 frozen
  return $ fromForeignPtr0 fptr size
