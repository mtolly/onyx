{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings #-}
module GuitarHeroII.Audio (writeVGS, readVGS) where

import           Control.Monad                (liftM4, replicateM_, forM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.Class (lift)
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import Data.Conduit ((.|))
import qualified Data.Conduit.Audio           as A
import           Data.Int                     (Int16)
import qualified Data.Vector.Storable         as V
import           Foreign
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Binary.Get (Get, getByteString, runGet, getWord32le)

#include "encode_vag.h"

{#fun encodeVAGBlock
  { castPtr `Ptr Int16'
  , castPtr `Ptr Word8'
  , `Word8'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  , castPtr `Ptr Double'
  } -> `()'
#}

xaBlockSamples :: Int
xaBlockSamples = 28

makeXABlock
  :: (Int, V.Vector Int16, (Double, Double, Double, Double))
  -> (B.ByteString, (Double, Double, Double, Double))
makeXABlock (channel, v, (a, b, c, d)) = unsafePerformIO $ do
  V.unsafeWith (V.take xaBlockSamples $ v V.++ V.replicate xaBlockSamples 0) $ \pin -> do
  allocaBytes 16 $ \pout -> do
  with a $ \pa -> do
  with b $ \pb -> do
  with c $ \pc -> do
  with d $ \pd -> do
    encodeVAGBlock pin pout (fromIntegral channel) pa pb pc pd
    bs <- B.packCStringLen (castPtr pout, 16)
    newState <- liftM4 (,,,) (peek pa) (peek pb) (peek pc) (peek pd)
    return (bs, newState)

writeVGS :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeVGS fp src = let
  A.AudioSource s r c _ = A.reorganize xaBlockSamples src
  header blocksPerChannel = BL.take 0x80 $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    replicateM_ c $ do
      putWord32le $ round r
      putWord32le blocksPerChannel
    putByteString $ B.replicate 0x80 0
  writeBlock h input = do
    let (bs, newState) = makeXABlock input
    liftIO $ B.hPut h bs
    return newState
  writeBlocks h xaStates !soFar = C.await >>= \case
    Nothing  -> return soFar
    Just blk -> do
      xaStates' <- mapM (writeBlock h) $ zip3 [0..] (A.deinterleave c blk) xaStates
      writeBlocks h xaStates' $ soFar + 1
  go h = do
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0x80
    blocksPerChannel <- writeBlocks h (repeat (0, 0, 0, 0)) 0
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0
    liftIO $ BL.hPut h $ header blocksPerChannel
  in C.runConduit $ s .| C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose go

decodeVAGBlock :: Word8 -> StateT (Double, Double) Get [Int16]
decodeVAGBlock targetChannel = do
  bytes <- lift $ getByteString 16
  let predictor = B.index bytes 0 `shiftR` 4 -- high nibble, shouldn't be more than 4
      shift'    = B.index bytes 0 .&. 0xF    -- low  nibble
      channel   = B.index bytes 1
      samples = do
        byte <- B.unpack $ B.drop 2 bytes
        let signExtend :: Word32 -> Int32
            signExtend x = fromIntegral $ if (x .&. 0x8000) /= 0 then x .|. 0xFFFF0000 else x
            ss0 = signExtend $ (fromIntegral byte .&. 0xF ) `shiftL` 12
            ss1 = signExtend $ (fromIntegral byte .&. 0xF0) `shiftL` 8
        [   realToFrac $ ss0 `shiftR` fromIntegral shift'
          , realToFrac $ ss1 `shiftR` fromIntegral shift'
          ]
  if channel /= targetChannel
    then return []
    else forM samples $ \sample -> do
      (s0, s1) <- get
      let newSample = sample
            + s0 * fst (vagFilter !! fromIntegral predictor)
            + s1 * snd (vagFilter !! fromIntegral predictor)
      put (newSample, s0)
      -- TODO do we need to clamp this
      return $ round newSample

vagFilter :: [(Double, Double)]
vagFilter =
  [ (0.0, 0.0)
  , (60.0 / 64.0,  0.0)
  , (115.0 / 64.0, -52.0 / 64.0)
  , (98.0 / 64.0, -55.0 / 64.0)
  , (122.0 / 64.0, -60.0 / 64.0)
  ]

readVGS :: (MonadResource m) => FilePath -> IO [A.AudioSource m Int16]
readVGS fp = do
  header <- IO.withBinaryFile fp IO.ReadMode $ \h -> BL.hGet h 0x80
  let readHeader = do
        "VgS!" <- getByteString 4
        2 <- getWord32le
        readChannels
      readChannels = do
        rate <- getWord32le
        blocks <- fromIntegral <$> getWord32le
        if blocks == 0
          then return []
          else ((rate, blocks) :) <$> readChannels
  let chans = runGet readHeader header
      totalBlocks = sum $ map snd chans
  return $ flip map (zip [0..] chans) $ \(i, (rate, blocks)) -> let
    src = C.bracketP (IO.openBinaryFile fp IO.ReadMode) IO.hClose $ \h -> do
      _ <- liftIO $ BL.hGet h 0x80
      let go _    0          = return ()
          go dbls blocksLeft = do
            chunk <- liftIO $ BL.hGet h 16
            case runGet (runStateT (decodeVAGBlock i) dbls) chunk of
              ([], _)    -> go dbls $ blocksLeft - 1
              (samps, dbls') -> do
                C.yield $ V.fromList samps
                go dbls' $ blocksLeft - 1
      go (0, 0) totalBlocks
    in A.AudioSource src (realToFrac rate) 1 $ blocks * xaBlockSamples
