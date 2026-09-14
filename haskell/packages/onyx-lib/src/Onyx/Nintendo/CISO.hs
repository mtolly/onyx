{-
CISO (compact ISO) disc images, as used by some GameCube/Wii loaders.
Reference: wiimms-iso-tools, src/lib-ciso.c

Header is 0x8000 bytes:
  0x0: magic "CISO"
  0x4: block size (u32, little endian)
  0x8: map of 0x7FF8 bytes, 1 = block is stored in the file, 0 = block is all zeroes
Stored blocks follow the header in order, so the Nth used block is at 0x8000 + N * block size.
-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Nintendo.CISO (isCISO, cisoReadable) where

import           Control.Monad        (unless, when)
import           Data.Binary.Get      (getWord32le, runGet)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.IORef
import qualified Data.Vector.Unboxed  as U
import           Data.Word            (Word8)
import           Onyx.Util.Handle
import           System.IO

cisoHeaderSize :: Integer
cisoHeaderSize = 0x8000

isCISO :: Readable -> IO Bool
isCISO r = useHandle r \h -> (== "CISO") <$> B.hGet h 4

-- | Presents the uncompressed disc image contained in a .ciso file.
cisoReadable :: Readable -> Readable
cisoReadable r = Readable
  { rFilePath = Nothing
  , rOpen = do
    h <- rOpen r
    header <- B.hGet h $ fromIntegral cisoHeaderSize
    unless (B.length header == fromIntegral cisoHeaderSize && B.take 4 header == "CISO") do
      hClose h
      fail "Not a valid CISO file"
    let blockSize = fromIntegral $ runGet getWord32le $ BL.fromStrict $ B.take 4 $ B.drop 4 header :: Integer
        mapBytes = B.unpack $ B.drop 8 header
        neededBlocks = length $ dropWhile (== 0) $ reverse mapBytes
    when (blockSize < 0x8000 || blockSize `mod` 0x8000 /= 0 || any (> 1) mapBytes) do
      hClose h
      fail "Invalid CISO header"
    -- for each virtual block, index of the stored block or -1 if not stored
    let blockMap = U.fromList $ take neededBlocks $ go 0 mapBytes :: U.Vector Int
        go :: Int -> [Word8] -> [Int]
        go _ []       = []
        go n (b : bs) = if b == 1 then n : go (n + 1) bs else (-1) : go n bs
    posn <- newIORef 0
    let virtSize = blockSize * fromIntegral (U.length blockMap)
        readChunk pos len = let
          (block, beg) = pos `quotRem` blockSize
          size = min len $ blockSize - beg
          in case blockMap U.!? fromIntegral block of
            Just stored | stored >= 0 -> do
              hSeek h AbsoluteSeek $ cisoHeaderSize + fromIntegral stored * blockSize + beg
              bs <- B.hGet h $ fromIntegral size
              -- final stored block may be truncated in the file
              return $ bs <> B.replicate (fromIntegral size - B.length bs) 0
            _ -> return $ B.replicate (fromIntegral size) 0
        readRange pos len = if len <= 0
          then return []
          else do
            chunk <- readChunk pos len
            let n = fromIntegral $ B.length chunk
            (chunk :) <$> readRange (pos + n) (len - n)
    openSimpleHandle (handleLabel h <> " | ciso") SimpleHandle
      { shSize  = virtSize
      , shSeek  = writeIORef posn
      , shTell  = readIORef posn
      , shClose = hClose h
      , shRead  = \n -> do
        p <- readIORef posn
        let n' = max 0 $ min n $ virtSize - p
        chunks <- readRange p n'
        writeIORef posn $ p + n'
        return $ B.concat chunks
      }
  }
