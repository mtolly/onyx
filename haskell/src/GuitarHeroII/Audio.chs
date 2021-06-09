{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
module GuitarHeroII.Audio (writeVGS, writeVGSMultiRate, readVGS, readVGSReadable) where

import           Control.Monad                (forM, forM_, forever, liftM4,
                                               replicateM_, zipWithM)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State    (StateT, get, put, runStateT)
import           Data.Binary.Get              (Get, getByteString, getWord32le,
                                               runGet)
import           Data.Binary.Put
import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 ((.|))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import           Data.Maybe                   (isNothing)
import           Data.SimpleHandle            (Readable (..), fileReadable,
                                               useHandle)
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

data VGSInputState = VGSInputState
  { vgsXAStates      :: [(Double, Double, Double, Double)]
  , vgsBlocksWritten :: !Word32
  , vgsFirstChannel  :: !Int
  , vgsChannels      :: !Int
  }

writeVGSMultiRate :: (MonadResource m) => FilePath -> [A.AudioSource m Int16] -> m ()
writeVGSMultiRate fp srcs = let
  extendSrc src = do
    C.mapOutput Just $ A.source $ A.reorganize xaBlockSamples src
    forever $ C.yield Nothing
  header blocksPerSource = BL.take 0x80 $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    forM_ (zip srcs blocksPerSource) $ \(src, numBlocks) -> do
      replicateM_ (A.channels src) $ do
        putWord32le $ round $ A.rate src
        putWord32le numBlocks
    putByteString $ B.replicate 0x80 0
  writeBlock _ inputState Nothing = return inputState
  writeBlock h inputState (Just blk) = do
    let chans = A.deinterleave (vgsChannels inputState) blk
    newStates <- forM (zip3 [vgsFirstChannel inputState ..] chans $ vgsXAStates inputState) $ \input -> do
      let (bs, newState) = makeXABlock input
      liftIO $ B.hPut h bs
      return newState
    return inputState
      { vgsXAStates      = newStates
      , vgsBlocksWritten = vgsBlocksWritten inputState + 1
      }
  writeBlocks h inputStates = C.await >>= \case
    Nothing -> return inputStates -- shouldn't happen
    Just blks -> if all isNothing blks
      then return inputStates -- all channels are done
      else zipWithM (writeBlock h) inputStates blks >>= writeBlocks h
  initialStates _              []       = []
  initialStates currentChannel (s : ss) = VGSInputState
    { vgsXAStates      = repeat (0, 0, 0, 0)
    , vgsBlocksWritten = 0
    , vgsFirstChannel  = currentChannel
    , vgsChannels      = A.channels s
    } : initialStates (currentChannel + A.channels s) ss
  go h = do
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0x80
    finalStates <- writeBlocks h $ initialStates 0 srcs
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0
    liftIO $ BL.hPut h $ header $ map vgsBlocksWritten finalStates
  in C.runConduit $ C.sequenceSources (map extendSrc srcs)
    .| C.bracketP (IO.openBinaryFile fp IO.WriteMode) IO.hClose go

writeVGS :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeVGS fp src = writeVGSMultiRate fp [src]

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

readVGSReadable :: (MonadResource m) => Readable -> IO [A.AudioSource m Int16]
readVGSReadable r = do
  header <- useHandle r $ \h -> BL.hGet h 0x80
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
    src = C.bracketP (rOpen r) IO.hClose $ \h -> do
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

readVGS :: (MonadResource m) => FilePath -> IO [A.AudioSource m Int16]
readVGS = readVGSReadable . fileReadable
