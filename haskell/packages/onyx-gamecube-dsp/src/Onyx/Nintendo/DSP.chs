module Onyx.Nintendo.DSP (decodeStereoCstr) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Foreign
import Data.Binary.Get
import Control.Monad
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

#include "dsptool.h"

{#pointer *ADPCMINFO as ADPCMINFO newtype #}

{#fun decode
  { castPtr `Ptr Word8'
  , castPtr `Ptr Int16'
  , `ADPCMINFO'
  , `Word32'
  } -> `()'
#}

splitEvery :: Int -> B.ByteString -> [B.ByteString]
splitEvery n b = if B.null b
  then []
  else let
    (x, y) = B.splitAt n b
    in x : splitEvery n y

decodeStereoCstr :: B.ByteString -> IO (Word16, V.Vector Int16, V.Vector Int16)
decodeStereoCstr bs = let
  sampleRate = runGet getWord16le $ BL.fromStrict $ B.drop 0x18 bs
  afterHeaders = B.drop (0x20 + 0x60 + 0x60) bs
  dspHeader1 = B.take 0x60 $ B.drop 0x20 bs
  dspHeader2 = B.take 0x60 $ B.drop (0x20 + 0x60) bs
  interleave = runGet getWord16be $ BL.fromStrict $ B.drop 6 bs
  chunks = splitEvery (fromIntegral interleave) afterHeaders
  data1 = B.concat $ map snd $ filter fst $ zip (cycle [True, False]) chunks
  data2 = B.concat $ map snd $ filter fst $ zip (cycle [False, True]) chunks
  runDecode dspHeader dat = do
    let numSamples = runGet getWord32be $ BL.fromStrict dspHeader
    B.useAsCString dat $ \src -> do
      vec <- VM.unsafeNew $ fromIntegral numSamples
      VM.unsafeWith vec $ \dest -> do
        allocaBytes {#sizeof ADPCMINFO #} $ \raw -> do
          let info = ADPCMINFO $ castPtr raw
              coefs = runGet (replicateM 16 getInt16be) $ BL.fromStrict $ B.drop 0x1C dspHeader
          withArray coefs $ {#set ADPCMINFO.coef #} info . castPtr
          {#set ADPCMINFO.yn1 #} info 0
          {#set ADPCMINFO.yn2 #} info 0
          decode (castPtr src) dest info numSamples
      V.unsafeFreeze vec
  in do
    result1 <- runDecode dspHeader1 data1
    result2 <- runDecode dspHeader2 data2
    return (sampleRate, result1, result2)
