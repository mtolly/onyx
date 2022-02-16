{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedStrings        #-}
module GuitarHeroII.Audio (writeVGS, writeVGSMultiRate, readVGS, readVGSReadable, readSingleRateVGS, splitOutVGSChannels, vgsChannelCount) where

import           Control.Monad                (forM, forM_, liftM4, replicateM_, void)
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
import           Data.Conduit                 (($$++))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as A
import           Data.List.Extra              (minimumOn, elemIndex)
import           Data.SimpleHandle            (Readable (..), fileReadable,
                                               useHandle)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Vector.Storable         as V
import           Foreign hiding (void)
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

data VGSInputState m = VGSInputState
  { vgsXAStates      :: [(Double, Double, Double, Double)]
  , vgsBlocksWritten :: !Word32
  , vgsFirstChannel  :: !Int
  , vgsChannels      :: !Int
  , vgsRate          :: !Double
  , vgsSealedInput   :: !(C.SealedConduitT () (V.Vector Int16) m ())
  }

writeVGSMultiRate :: (MonadResource m) => FilePath -> [A.AudioSource m Int16] -> m ()
writeVGSMultiRate fp srcs = let
  maxSeconds = maximum $ map (\src -> A.framesToSeconds (A.frames src) (A.rate src)) srcs
  header blocksPerSource = BL.take 0x80 $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    forM_ (zip srcs blocksPerSource) $ \(src, numBlocks) -> do
      replicateM_ (A.channels src) $ do
        putWord32le $ round $ A.rate src
        putWord32le numBlocks
    putByteString $ B.replicate 0x80 0
  writeBlock h inputState = do
    mblk <- C.await
    let chans = case mblk of
          Just blk -> A.deinterleave (vgsChannels inputState) blk
          Nothing  -> replicate (vgsChannels inputState) V.empty
    newStates <- forM (zip3 [vgsFirstChannel inputState ..] chans $ vgsXAStates inputState) $ \input -> do
      let (bs, newState) = makeXABlock input
      liftIO $ B.hPut h bs
      return newState
    return inputState
      { vgsXAStates      = newStates
      , vgsBlocksWritten = vgsBlocksWritten inputState + 1
      }
  secondsWritten st = fromIntegral (vgsBlocksWritten st * 28) / vgsRate st
  writeBlocks h inputStates = do
    let (nextIndex, nextInput) = minimumOn (\(_, st) -> secondsWritten st) $ zip [0..] inputStates
    if secondsWritten nextInput < maxSeconds
      then do
        (newSealed, newInput) <- vgsSealedInput nextInput $$++ writeBlock h nextInput
        let inputStates' = flip map (zip [0..] inputStates) $ \(i, st) ->
              if i == (nextIndex :: Int) then newInput { vgsSealedInput = newSealed } else st
        writeBlocks h inputStates'
      else return inputStates
  initialStates _              []       = []
  initialStates currentChannel (s : ss) = VGSInputState
    { vgsXAStates      = repeat (0, 0, 0, 0)
    , vgsBlocksWritten = 0
    , vgsFirstChannel  = currentChannel
    , vgsChannels      = A.channels s
    , vgsRate          = A.rate s
    , vgsSealedInput   = C.sealConduitT $ A.source s
    } : initialStates (currentChannel + A.channels s) ss
  go h = do
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0x80
    finalStates <- writeBlocks h $ initialStates 0 $ map (A.reorganize xaBlockSamples) srcs
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0
    liftIO $ BL.hPut h $ header $ map vgsBlocksWritten finalStates
  in do
    (key, h) <- allocate (IO.openBinaryFile fp IO.WriteMode) IO.hClose
    go h
    release key

writeVGS :: (MonadResource m) => FilePath -> A.AudioSource m Int16 -> m ()
writeVGS fp src = writeVGSMultiRate fp [src]

decodeVAGBlock :: Maybe Word8 -> StateT (Double, Double) Get [Int16]
decodeVAGBlock targetChannel = do
  bytes <- lift $ getByteString 16
  let predictor = B.index bytes 0 `shiftR` 4 -- high nibble, shouldn't be more than 4
      shift'    = B.index bytes 0 .&. 0xF    -- low  nibble
      channel   = B.index bytes 1 .&. 0xF    -- low  nibble, high is usually 0, but 8 seen at end of RB PS2 VGS
      samples = do
        byte <- B.unpack $ B.drop 2 bytes
        let signExtend :: Word32 -> Int32
            signExtend x = fromIntegral $ if (x .&. 0x8000) /= 0 then x .|. 0xFFFF0000 else x
            ss0 = signExtend $ (fromIntegral byte .&. 0xF ) `shiftL` 12
            ss1 = signExtend $ (fromIntegral byte .&. 0xF0) `shiftL` 8
        [   realToFrac $ ss0 `shiftR` fromIntegral shift'
          , realToFrac $ ss1 `shiftR` fromIntegral shift'
          ]
  if maybe False (channel /=) targetChannel
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

readVGSHeader :: BL.ByteString -> [(Word32, Word32)]
readVGSHeader = let
  start = do
    "VgS!" <- getByteString 4
    2 <- getWord32le
    readChannels
  readChannels = do
    rate <- getWord32le
    blocks <- getWord32le
    if blocks == 0
      then return []
      else ((rate, blocks) :) <$> readChannels
  in runGet start

vgsChannelCount :: Readable -> IO Int
vgsChannelCount r = do
  header <- useHandle r $ \h -> BL.hGet h 0x80
  return $ length $ readVGSHeader header

readVGSReadable :: (MonadResource m) => Readable -> IO [A.AudioSource m Int16]
readVGSReadable r = do
  header <- useHandle r $ \h -> BL.hGet h 0x80
  let chans = readVGSHeader header
      totalBlocks = sum $ map snd chans
  return $ flip map (zip [0..] chans) $ \(i, (rate, blocks)) -> let
    src = C.bracketP (rOpen r) IO.hClose $ \h -> do
      _ <- liftIO $ BL.hGet h 0x80
      let go _    0          = return ()
          go dbls blocksLeft = do
            chunk <- liftIO $ BL.hGet h 16
            case runGet (runStateT (decodeVAGBlock $ Just i) dbls) chunk of
              ([], _)    -> go dbls $ blocksLeft - 1
              (samps, dbls') -> do
                C.yield $ V.fromList samps
                go dbls' $ blocksLeft - 1
      go (0, 0) totalBlocks
    in A.AudioSource src (realToFrac rate) 1 $ fromIntegral blocks * xaBlockSamples

readVGS :: (MonadResource m) => FilePath -> IO [A.AudioSource m Int16]
readVGS = readVGSReadable . fileReadable

-- Allows seeking. Only supports VGS where all channels are the same sample rate.
readSingleRateVGS :: (MonadResource m) => A.Duration -> Readable -> IO (A.AudioSource m Int16)
readSingleRateVGS dur r = do
  header <- useHandle r $ \h -> BL.hGet h 0x80
  let chans = readVGSHeader header
      numChans = length chans
      totalBlocks = sum $ map snd chans
  (rate, blockSets) <- case chans of
    pair@(rate, _) : rest -> if all ((== rate) . fst) rest
      then return pair
      else fail $ "readSingleRateVGS: VGS does not have consistent sample rate, channel rates are: " <> show (map fst chans)
    [] -> fail "readSingleRateVGS: VGS has no channels"
  let startFrame = case dur of
        A.Frames  f -> f
        A.Seconds s -> floor $ s * fromIntegral rate
      startBlockSet = quot startFrame xaBlockSamples
      initialSkip = startFrame - startBlockSet * xaBlockSamples
  return $ A.dropStart (A.Frames initialSkip) $ let
    src = C.bracketP (rOpen r) IO.hClose $ \h -> do
      liftIO $ IO.hSeek h IO.AbsoluteSeek $ fromIntegral $ 0x80 + startBlockSet * 0x10 * numChans
      let go _      0             = return ()
          go states blockSetsLeft = do
            results <- forM states $ \channelState -> do
              chunk <- liftIO $ BL.hGet h 16
              let (samps, newState) = runGet (runStateT (decodeVAGBlock Nothing) channelState) chunk
              return (V.fromList samps, newState)
            C.yield $ A.interleave $ map fst results
            go (map snd results) $ blockSetsLeft - 1
      go (replicate numChans (0, 0)) $ fromIntegral blockSets - startBlockSet
    in A.AudioSource src (realToFrac rate) numChans $ fromIntegral totalBlocks * xaBlockSamples - startFrame

splitOutVGSChannels :: [Int] -> Readable -> IO BL.ByteString
splitOutVGSChannels wantChannels r = useHandle r $ \hin -> do
  withSystemTempDirectory "onyx-vgs-split" $ \dout -> do
    let fout = dout </> "out.vgs"
    IO.withBinaryFile fout IO.WriteMode $ \hout -> do
      header <- BL.hGet hin 0x80
      let chans = readVGSHeader header
          totalBlocks = sum $ map snd chans
      -- first, write out new header
      BL.hPut hout $ runPut $ do
        putLazyByteString $ BL.take 8 header
        forM_ wantChannels $ \i -> do
          let (rate, blocks) = chans !! i
          putWord32le rate
          putWord32le blocks
        putByteString $ B.replicate (0x80 - (length wantChannels + 1) * 8) 0
      -- then, filter the vag blocks by channel and change their index
      replicateM_ (fromIntegral totalBlocks) $ do
        byte0 <- B.hGet hin 1
        channelByte <- B.head <$> B.hGet hin 1
        case elemIndex (fromIntegral $ channelByte .&. 0xF) wantChannels of
          Nothing -> void $ B.hGet hin 14 -- skip this block (hSeek seems to be much worse performance?)
          Just newIndex -> do
            block <- B.hGet hin 14
            mapM_ (B.hPut hout) [byte0, B.singleton $ fromIntegral newIndex, block]
    BL.fromStrict <$> B.readFile fout
