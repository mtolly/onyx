{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
module Onyx.Nintendo.DSP (decodeStereoCstr) where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Int
import qualified Data.Map                     as Map
import qualified Data.Vector.Storable         as V
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word
import           Foreign
import qualified Language.C.Inline            as C
import qualified Language.C.Inline.Context    as C
import qualified Language.C.Types             as C

data ADPCMINFO

C.context $ C.baseCtx <> mempty
  { C.ctxTypesTable = Map.fromList
    [ (C.TypeName "ADPCMINFO", [t| ADPCMINFO |])
    ]
  }

C.include "dsptool.h"

decode :: Ptr Word8 -> Ptr Int16 -> Ptr ADPCMINFO -> Word32 -> IO ()
decode src dst cxt samples =
  [C.block| void {
    decode($(uint8_t* src), $(int16_t* dst), $(ADPCMINFO* cxt), $(uint32_t samples));
  } |]

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
    B.useAsCString dat \src -> do
      vec <- VM.unsafeNew $ fromIntegral numSamples
      VM.unsafeWith vec \dest -> do
        allocaBytes (fromIntegral [C.pure| int { sizeof(ADPCMINFO) } |]) \info -> do
          let coefs = runGet (replicateM 16 getInt16be) $ BL.fromStrict $ B.drop 0x1C dspHeader
          forM_ (zip [0..] coefs) \(i, coef) ->
            [C.block| void { $(ADPCMINFO* info)->coef[$(int i)] = $(int16_t coef); } |]
          [C.block| void {
            $(ADPCMINFO* info)->yn1 = 0;
            $(ADPCMINFO* info)->yn2 = 0;
          } |]
          decode (castPtr src) dest info numSamples
      V.unsafeFreeze vec
  in do
    result1 <- runDecode dspHeader1 data1
    result2 <- runDecode dspHeader2 data2
    return (sampleRate, result1, result2)
