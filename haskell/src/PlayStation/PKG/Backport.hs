{-

Backports of packZipWith functions from newer bytestring versions

-}
{-# LANGUAGE BangPatterns #-}
module PlayStation.PKG.Backport (lazyPackZipWith) where

import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy.Internal as BLI
import           Foreign
import           System.IO.Unsafe              (unsafePerformIO)

strictPackZipWith :: (Word8 -> Word8 -> Word8) -> B.ByteString -> B.ByteString -> B.ByteString
strictPackZipWith f (BI.PS fp loff l) (BI.PS fq moff m) = unsafePerformIO $
    withForeignPtr fp $ \a ->
    withForeignPtr fq $ \b ->
    BI.create len $ go (plusPtr a loff) (plusPtr b moff)
  where
    go p1 p2 = zipWith_ 0
      where
        zipWith_ :: Int -> Ptr Word8 -> IO ()
        zipWith_ !n !r
           | n >= len = return ()
           | otherwise = do
                x <- peekByteOff p1 n
                y <- peekByteOff p2 n
                pokeByteOff r n (f x y)
                zipWith_ (n+1) r
    len = min l m

lazyPackZipWith :: (Word8 -> Word8 -> Word8) -> BLI.ByteString -> BLI.ByteString -> BLI.ByteString
lazyPackZipWith _ BLI.Empty _ = BLI.Empty
lazyPackZipWith _ _ BLI.Empty = BLI.Empty
lazyPackZipWith f (BLI.Chunk a@(BI.PS _ _ al) as) (BLI.Chunk b@(BI.PS _ _ bl) bs) = BLI.Chunk (strictPackZipWith f a b) $
    case compare al bl of
        LT -> lazyPackZipWith f as $ BLI.Chunk (B.drop al b) bs
        EQ -> lazyPackZipWith f as bs
        GT -> lazyPackZipWith f (BLI.Chunk (B.drop bl a) as) bs
