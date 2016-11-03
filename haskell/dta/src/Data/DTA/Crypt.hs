-- | The encryption scheme used for DTB files found on game discs.
module Data.DTA.Crypt
( oldCrypt, newCrypt
, decrypt, encrypt
, decryptFile, encryptFile
, decryptHandle, encryptHandle
, Key, Crypt
) where

import           Control.Monad         (forM_, liftM2, liftM3)
import           Control.Monad.ST.Lazy (ST, runST)
import           Data.Array.ST         (STArray, newArray, readArray,
                                        writeArray)
import           Data.Bits             (shiftR, xor, (.&.), (.|.))
import           Data.STRef.Lazy       (STRef, newSTRef, readSTRef, writeSTRef)
import           Data.Word             (Word32, Word8)
import           System.IO             (Handle)

import           Data.Binary.Get       (getRemainingLazyByteString, getWord32le,
                                        runGet)
import           Data.Binary.Put       (putLazyByteString, putWord32le, runPut)
import qualified Data.ByteString.Lazy  as BL

-- | An encryption/decryption key.
type Key = Word32

-- | Using a key to generate an infinite stream of crypt bytes.
type Crypt = Key -> [Word8]

{- |
The way both the new and old DTB encryption algorithms work is by using the key
to generate a stream of bytes. Each of these bytes is then XOR'd with the
corresponding bytes in the source file. The same algorithm is both the
decryption and encryption; this is because @(A xor B) xor B == A@.
-}
crypt :: Crypt -> Key -> BL.ByteString -> BL.ByteString
crypt cry key = BL.pack . zipWith xor (cry key) . BL.unpack

-- | Take the first four bytes of the string as the key, and decrypt the rest of
-- the file.
decrypt :: Crypt -> BL.ByteString -> BL.ByteString
decrypt cry = runGet $ liftM2 (crypt cry) getWord32le getRemainingLazyByteString

-- | Encrypt a string with a key, and append the key to the encrypted string.
encrypt :: Crypt -> Key -> BL.ByteString -> BL.ByteString
encrypt cry key input
  = runPut $ putWord32le key >> putLazyByteString (crypt cry key input)

-- | Decrypt an encrypted DTB file using the given crypt method.
decryptFile :: Crypt -> FilePath -> FilePath -> IO ()
decryptFile cry fi fo = BL.readFile fi >>= BL.writeFile fo . decrypt cry

-- | Encrypt an unencrypted DTB file using the given crypt method and key.
encryptFile :: Crypt -> Key -> FilePath -> FilePath -> IO ()
encryptFile cry key fi fo = BL.readFile fi >>= BL.writeFile fo . encrypt cry key

-- | Decrypt an encrypted DTB file across two handles.
decryptHandle :: Crypt -> Handle -> Handle -> IO ()
decryptHandle cry hi ho = BL.hGetContents hi >>= BL.hPutStr ho . decrypt cry

-- | Encrypt an unencrypted DTB files across two handles.
encryptHandle :: Crypt -> Key -> Handle -> Handle -> IO ()
encryptHandle cry key hi ho = BL.hGetContents hi >>= BL.hPutStr ho . encrypt cry key

-- New (Rock Band) encryption

{-
From xorloser's dtbcrypt:
unsigned int dtb_xor_x360(unsigned int data)
{
  int val1 = (data / 0x1F31D) * 0xB14;
  int val2 = (data - ((data / 0x1F31D) * 0x1F31D)) * 0x41A7;
  val2 = val2 - val1;
  if(val2 <= 0)
    val2 += 0x7FFFFFFF;
  return val2;
}
-}

-- | The key iteration function for new DTB encryption/decryption.
dtbXor360 :: Word32 -> Word32
dtbXor360 d = let
  q = quot d 0x1F31D
  v = (d - (q * 0x1F31D)) * 0x41A7 - q * 0xB14 in
    if v > 0x7FFFFFFF then v + 0x7FFFFFFF else v

-- | The lazy infinite list of crypt bytes for new-style encryption.
newCrypt :: Crypt
newCrypt key = fmap fromIntegral $ tail $ iterate dtbXor360 key

-- Old (PS2 Guitar Hero) encryption
-- This algorithm uses a large table to produce the XOR bytes, which is
-- implemented using an STArray in the lazy ST monad.

data CryptTable s = CryptTable
  { idx1  :: STRef s Word8
  , idx2  :: STRef s Word8
  , table :: STArray s Word8 Word32 }

cryptTable :: Key -> ST s (CryptTable s)
cryptTable key = do
  v1ref <- newSTRef key
  tbl <- newArray (0, 0xF8) 0
  forM_ [0..0xF8] $ \i -> do
    v1 <- readSTRef v1ref
    let v2 = (v1 * 0x41C64E6D) + 0x3039
    let v1' = (v2 * 0x41C64E6D) + 0x3039
    writeSTRef v1ref v1'
    writeArray tbl i $ (v1' .&. 0x7FFF0000) .|. (v2 `shiftR` 16)
  liftM3 CryptTable (newSTRef 0) (newSTRef 0x67) (return tbl)

oldNext :: CryptTable s -> ST s Word8
oldNext CryptTable{ idx1 = i1ref, idx2 = i2ref, table = tbl } = do
  i1 <- readSTRef i1ref
  i2 <- readSTRef i2ref
  next <- liftM2 xor (readArray tbl i1) (readArray tbl i2)
  writeArray tbl i1 next
  writeSTRef i1ref $ if i1 == 0xF8 then 0 else i1 + 1
  writeSTRef i2ref $ if i2 == 0xF8 then 0 else i2 + 1
  return $ fromIntegral next
  -- The fromIntegral chops each Word32 down to the least significant byte.

-- | The lazy infinite list of crypt bytes for old-style encryption.
oldCrypt :: Crypt
oldCrypt key = runST $ cryptTable key >>= sequence . repeat . oldNext
