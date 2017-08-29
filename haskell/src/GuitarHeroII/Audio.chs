{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
module GuitarHeroII.Audio (writeVGS) where

import           Control.Monad                (liftM4, replicateM_)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import           Data.Int                     (Int16)
import qualified Data.Vector.Storable         as V
import           Foreign
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafePerformIO)

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
  in s C.$$ C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose go
