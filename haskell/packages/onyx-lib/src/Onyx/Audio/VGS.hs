{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NoFieldSelectors         #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecordWildCards          #-}
module Onyx.Audio.VGS
( writeVGS
, writeVGSMultiRate
, readVGS
, readVGSReadable
, readSingleRateVGS
, splitOutVGSChannels
, vgsChannelCount
, readVGSHeader
, decryptVGS
, VGSHeader(..)
) where

import           Control.Monad                (forM, forM_, liftM4, replicateM_,
                                               void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State    (StateT, get, put, runStateT)
import           Data.Binary.Get              (Get, getByteString, getWord32le,
                                               getWord64le, isEmpty, runGet)
import           Data.Binary.Put
import           Data.Bits                    (shiftL, shiftR, xor, (.&.),
                                               (.|.))
import qualified Data.ByteString              as B
import qualified Data.ByteString.Builder      as BB
import qualified Data.ByteString.Char8        as B8
import qualified Data.ByteString.Lazy         as BL
import           Data.Conduit                 (($$++))
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Audio           as CA
import           Data.Functor                 ((<&>))
import           Data.Int                     (Int16, Int32)
import           Data.List.Extra              (elemIndex, minimumOn)
import qualified Data.Vector.Storable         as V
import           Data.Word                    (Word32, Word64, Word8)
import           Foreign                      (Ptr, allocaBytes, castPtr, peek,
                                               with)
import           Onyx.Util.Binary             (runGetM)
import           Onyx.Util.Handle             (Readable (..), fileReadable,
                                               useHandle)
import           System.FilePath              ((</>))
import qualified System.IO                    as IO
import           System.IO.Temp               (withSystemTempDirectory)
import           System.IO.Unsafe             (unsafePerformIO)

foreign import ccall unsafe "encodeVAGBlock"
  encodeVAGBlock
    :: Ptr Int16
    -> Ptr Word8
    -> Word8
    -> Ptr Double
    -> Ptr Double
    -> Ptr Double
    -> Ptr Double
    -> IO ()

xaBlockSamples :: Int
xaBlockSamples = 28

makeXABlock
  :: (Int, V.Vector Int16, (Double, Double, Double, Double))
  -> (B.ByteString, (Double, Double, Double, Double))
makeXABlock (channel, v, (a, b, c, d)) = unsafePerformIO do
  V.unsafeWith (V.take xaBlockSamples $ v V.++ V.replicate xaBlockSamples 0) \pin -> do
  allocaBytes 16 \pout -> do
  with a \pa -> do
  with b \pb -> do
  with c \pc -> do
  with d \pd -> do
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

writeVGSMultiRate :: (MonadResource m) => FilePath -> [CA.AudioSource m Int16] -> m ()
writeVGSMultiRate fp srcs = let
  maxSeconds = maximum $ map (\src -> CA.framesToSeconds (CA.frames src) (CA.rate src)) srcs
  header blocksPerSource = BL.take 0x80 $ runPut do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    forM_ (zip srcs blocksPerSource) \(src, numBlocks) -> do
      replicateM_ (CA.channels src) do
        putWord32le $ round $ CA.rate src
        putWord32le numBlocks
    putByteString $ B.replicate 0x80 0
  writeBlock h inputState = do
    mblk <- C.await
    let chans = case mblk of
          Just blk -> CA.deinterleave inputState.vgsChannels blk
          Nothing  -> replicate inputState.vgsChannels V.empty
    newStates <- forM (zip3 [inputState.vgsFirstChannel ..] chans inputState.vgsXAStates) \input -> do
      let (bs, newState) = makeXABlock input
      liftIO $ B.hPut h bs
      return newState
    return inputState
      { vgsXAStates      = newStates
      , vgsBlocksWritten = inputState.vgsBlocksWritten + 1
      }
  secondsWritten :: VGSInputState m -> Double
  secondsWritten st = fromIntegral (st.vgsBlocksWritten * 28) / st.vgsRate
  writeBlocks h inputStates = do
    let (nextIndex, nextInput) = minimumOn (\(_, st) -> secondsWritten st) $ zip [0..] inputStates
    if secondsWritten nextInput < maxSeconds
      then do
        (newSealed, newInput) <- nextInput.vgsSealedInput $$++ writeBlock h nextInput
        let inputStates' = zip [0..] inputStates <&> \(i, st) ->
              if i == (nextIndex :: Int) then newInput { vgsSealedInput = newSealed } else st
        writeBlocks h inputStates'
      else return inputStates
  initialStates _              []       = []
  initialStates currentChannel (s : ss) = VGSInputState
    { vgsXAStates      = repeat (0, 0, 0, 0)
    , vgsBlocksWritten = 0
    , vgsFirstChannel  = currentChannel
    , vgsChannels      = CA.channels s
    , vgsRate          = CA.rate s
    , vgsSealedInput   = C.sealConduitT $ CA.source s
    } : initialStates (currentChannel + CA.channels s) ss
  go h = do
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0x80
    finalStates <- writeBlocks h $ initialStates 0 $ map (CA.reorganize xaBlockSamples) srcs
    liftIO $ IO.hSeek h IO.AbsoluteSeek 0
    liftIO $ BL.hPut h $ header $ map (.vgsBlocksWritten) finalStates
  in do
    (key, h) <- allocate (IO.openBinaryFile fp IO.WriteMode) IO.hClose
    go h
    release key

writeVGS :: (MonadResource m) => FilePath -> CA.AudioSource m Int16 -> m ()
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
            signExtend x = fromIntegral if (x .&. 0x8000) /= 0 then x .|. 0xFFFF0000 else x
            ss0 = signExtend $ (fromIntegral byte .&. 0xF ) `shiftL` 12
            ss1 = signExtend $ (fromIntegral byte .&. 0xF0) `shiftL` 8
        [   realToFrac $ ss0 `shiftR` fromIntegral shift'
          , realToFrac $ ss1 `shiftR` fromIntegral shift'
          ]
  if maybe False (channel /=) targetChannel
    then return []
    else forM samples \sample -> do
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

data VGSHeader = VGSHeader
  { version  :: Word32
  , channels :: [(Word32, Word32)]
  } deriving (Show)

readVGSHeader :: Get VGSHeader
readVGSHeader = let
  start = do
    "VgS!" <- getByteString 4
    version <- getWord32le
    channels <- readChannels
    return VGSHeader{..}
  readChannels = isEmpty >>= \case
    True  -> return []
    False -> do
      rate   <- getWord32le
      blocks <- getWord32le
      if blocks == 0
        then return []
        else ((rate, blocks) :) <$> readChannels
  in start

vgsChannelCount :: Readable -> IO Int
vgsChannelCount r = do
  header <- useHandle r (\h -> BL.hGet h 0x80) >>= runGetM readVGSHeader
  return $ length header.channels

readVGSReadable :: (MonadResource m) => Readable -> IO [CA.AudioSource m Int16]
readVGSReadable r = do
  header <- useHandle r (\h -> BL.hGet h 0x80) >>= runGetM readVGSHeader
  let totalBlocks = sum $ map snd header.channels
  return $ zip [0..] header.channels <&> \(i, (rate, blocks)) -> let
    src = C.bracketP (rOpen r) IO.hClose \h -> do
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
    in CA.AudioSource src (realToFrac rate) 1 $ fromIntegral blocks * xaBlockSamples

readVGS :: (MonadResource m) => FilePath -> IO [CA.AudioSource m Int16]
readVGS = readVGSReadable . fileReadable

-- Allows seeking. Only supports VGS where all channels are the same sample rate.
readSingleRateVGS :: (MonadResource m) => CA.Duration -> Readable -> IO (CA.AudioSource m Int16)
readSingleRateVGS dur r = do
  header <- useHandle r (\h -> BL.hGet h 0x80) >>= runGetM readVGSHeader
  let numChans = length header.channels
      totalBlocks = sum $ map snd header.channels
  (rate, blockSets) <- case header.channels of
    pair@(rate, _) : rest -> if all ((== rate) . fst) rest
      then return pair
      else fail $ "readSingleRateVGS: VGS does not have consistent sample rate, channel rates are: " <> show (map fst header.channels)
    [] -> fail "readSingleRateVGS: VGS has no channels"
  let startFrame = case dur of
        CA.Frames  f -> f
        CA.Seconds s -> floor $ s * fromIntegral rate
      startBlockSet = quot startFrame xaBlockSamples
      initialSkip = startFrame - startBlockSet * xaBlockSamples
  return $ CA.dropStart (CA.Frames initialSkip) let
    src = C.bracketP (rOpen r) IO.hClose \h -> do
      liftIO $ IO.hSeek h IO.AbsoluteSeek $ fromIntegral $ 0x80 + startBlockSet * 0x10 * numChans
      let go _      0             = return ()
          go states blockSetsLeft = do
            results <- forM states \channelState -> do
              chunk <- liftIO $ BL.hGet h 16
              let (samps, newState) = runGet (runStateT (decodeVAGBlock Nothing) channelState) chunk
              return (V.fromList samps, newState)
            C.yield $ CA.interleave $ map fst results
            go (map snd results) $ blockSetsLeft - 1
      go (replicate numChans (0, 0)) $ fromIntegral blockSets - startBlockSet
    in CA.AudioSource src (realToFrac rate) numChans $ fromIntegral totalBlocks * xaBlockSamples - startFrame

splitOutVGSChannels :: [Int] -> Readable -> IO BL.ByteString
splitOutVGSChannels wantChannels r = useHandle r \hin -> do
  withSystemTempDirectory "onyx-vgs-split" \dout -> do
    let fout = dout </> "out.vgs"
    IO.withBinaryFile fout IO.WriteMode \hout -> do
      header <- BL.hGet hin 0x80 >>= runGetM readVGSHeader
      let totalBlocks = sum $ map snd header.channels
      -- first, write out new header
      BL.hPut hout $ runPut do
        putByteString "VgS!"
        putWord32le header.version
        forM_ wantChannels \i -> do
          let (rate, blocks) = header.channels !! i
          putWord32le rate
          putWord32le blocks
        putByteString $ B.replicate (0x80 - (length wantChannels + 1) * 8) 0
      -- then, filter the vag blocks by channel and change their index
      replicateM_ (fromIntegral totalBlocks) do
        byte0 <- B.hGet hin 1
        channelByte <- B.head <$> B.hGet hin 1
        case elemIndex (fromIntegral $ channelByte .&. 0xF) wantChannels of
          Nothing -> void $ B.hGet hin 14 -- skip this block (hSeek seems to be much worse performance?)
          Just newIndex -> do
            block <- B.hGet hin 14
            mapM_ (B.hPut hout) [byte0, B.singleton $ fromIntegral newIndex, block]
    BL.fromStrict <$> B.readFile fout

-------------------------------------
-- decrypt translated from arktool --
-------------------------------------

ps2CryptTable :: [Word32]
ps2CryptTable = [0x208B5A54, 0xF367B37F, 0xBCF7AFF0, 0xDD3BA5E0]

-- TEA-like transform for 64-bit values
transform64Bits :: Word64 -> [Word32] -> Word64
transform64Bits input table = let
  hi0 = fromIntegral (input `shiftR` 32) :: Word32
  lo0 = fromIntegral input :: Word32

  step (hi, lo, num) = let
    tmp1 = ((hi `shiftL` 4) `xor` (hi `shiftR` 5))
    lo' = lo + ((tmp1 + hi) `xor` (num + (table !! (fromIntegral (num .&. 3)))))
    num' = num + 0x9E3779B9
    tmp2 = ((lo' `shiftL` 4) `xor` (lo' `shiftR` 5))
    hi' = hi + ((tmp2 + lo') `xor` (num' + (table !! (fromIntegral ((num' `shiftR` 11) .&. 3)))))
    in (hi', lo', num')

  (hiF, loF, _) = foldr ($) (hi0, lo0, 0) $ replicate 4 step
  in (fromIntegral hiF `shiftL` 32) .|. fromIntegral loF

-- infinite lazy bytestring for xoring
vgsCryptStream :: (Word64, Word64) -> BL.ByteString
vgsCryptStream keys = BB.toLazyByteString let
  go (data0, data1) = mconcat
    [ BB.word64LE $ transform64Bits data0 ps2CryptTable
    , BB.word64LE $ transform64Bits data1 ps2CryptTable
    , go (data0 + 1, data1 + 1)
    ]
  in go keys

decryptV4Data :: (Word64, Word64) -> BL.ByteString -> BL.ByteString
decryptV4Data = BL.packZipWith xor . vgsCryptStream

decryptVGS :: (MonadFail m) => BL.ByteString -> m (BL.ByteString)
decryptVGS input = do
  header <- runGetM readVGSHeader $ BL.take 0x80 input
  case header.version of
    1 -> return input -- already unencrypted
    2 -> return input -- already unencrypted
    -- 3 apparently AES like mogg? don't know where these are seen, not in ps2 rb games.
    -- leaving undone for now
    4 -> do
      let restBytes = BL.drop 0x80 input
      keys <- runGetM (liftA2 (,) getWord64le getWord64le) restBytes
      let decryptedData = decryptV4Data keys $ BL.drop 16 restBytes
          -- Create new header with version 2 (unencrypted)
          newHeaderBytes = runPut do
            putByteString "VgS!"
            putWord32le 2
            mapM_ (\(rate, blocks) -> putWord32le rate >> putWord32le blocks) header.channels
            let paddingSize = 0x80 - 8 - (length header.channels * 8)
            putByteString (B.replicate paddingSize 0)
      return $ newHeaderBytes <> decryptedData
    n -> fail $ "Unsupported VGS version " ++ show n ++ " (only versions 1, 2, 4 supported)"
