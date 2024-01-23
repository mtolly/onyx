{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE StrictData          #-}
module Onyx.Neversoft.PS2 where

import           Control.Applicative   (liftA2)
import           Control.Monad         (forM_)
import           Data.Bifunctor        (bimap)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as BL
import           Data.Int
import qualified Data.List.NonEmpty    as NE
import           Data.List.Split       (chunksOf)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import           Data.Word
import           Onyx.Neversoft.CRC
import           Onyx.Nintendo.WAD     (roundUpToMultiple, skipToMultiple)
import           Onyx.Util.Binary      (runGetM)
import           Onyx.Util.Handle

-- DATAP.HED + DATAP.WAD, contains most smaller files

-- Entry in DATAP.HED, found in GH3 PS2 and possibly others
data HedEntry = HedEntry
  { hedID   :: Word32
  , hedSize :: Word32
  , hedName :: B.ByteString
  } deriving (Show)

parseHed :: Get [HedEntry]
parseHed = go [] where
  go entries = do
    hedID <- getWord32le
    if hedID == 0xFFFFFFFF
      then return $ reverse entries
      else do
        hedSize <- getWord32le
        hedName <- getNullTerm
        skipToMultiple 4
        go $ HedEntry{..} : entries

getNullTerm :: Get B.ByteString
getNullTerm = let
  loop !n = do
    b <- getWord8
    if b == 0
      then return n
      else loop $ n + 1
  in do
    len <- lookAhead $ loop 0
    bs <- getByteString len
    skip 1 -- skip null byte
    return bs

data HedFormat
  = HedFormatGH3
  | HedFormatGH4
  | HedFormatGH5

identifyHedFormat :: [HedEntry] -> Maybe HedFormat
identifyHedFormat entries = let
  hasRootNamed x entry = let
    name = B.dropWhile (\c -> c == 0x5C || c == 0x2F) entry.hedName
    in x `B.isPrefixOf` name
  in if any (hasRootNamed "gh3c") entries
    then Just HedFormatGH3
    else if any (hasRootNamed "gh4c") entries
      then Just HedFormatGH4
      else if any (hasRootNamed "gh5c") entries
        then Just HedFormatGH5
        else Nothing

applyHed :: HedFormat -> [HedEntry] -> Folder B.ByteString (Word32, Word32)
applyHed fmt entries = let
  findPositions _   []       = []
  findPositions !pos (e : es) = let
    -- 0x5C is backslash (seen in GH3), 0x2F is forward slash (seen in GH5).
    -- paths also start with a slash/backslash so we remove it before split
    isSlash c = c == 0x5C || c == 0x2F
    splitName = NE.fromList $ B.splitWith isSlash $ B.dropWhile isSlash e.hedName
    jumpNext = case es of
      next : _ -> case fmt of
        HedFormatGH3 -> if ".pak.ps2" `B8.isSuffixOf` next.hedName
          then 0x8000
          else 0x800
        _ -> if any (`B8.isPrefixOf` next.hedName)
            [ "\\pak\\anims\\songs\\", "\\songs\\"
            , "/pak/anims/songs/"    , "/songs/"
            ]
          then 0x800
          else 0x8000
      [] -> 0x800 -- doesn't matter
    nextPosition = roundUpToMultiple jumpNext $ pos + e.hedSize
    in (splitName, (pos, e.hedSize)) : findPositions nextPosition es
  in fromFiles $ findPositions 0 entries

hookUpWAD :: Readable -> Folder B.ByteString (Word32, Word32) -> Folder T.Text Readable
hookUpWAD wad = bimap TE.decodeLatin1
  -- TODO add label to handle
  $ \(pos, len) -> subHandle id (fromIntegral pos) (Just $ fromIntegral len) wad

-- GH3 audio

-- .IMF + .ISF, main audio files (XA, like VGS but different interleaving)
-- note: SOUNDS/SFX.WAD also contains XA but looks like some differences

splitEvery :: Int64 -> BL.ByteString -> [BL.ByteString]
splitEvery n bs = if BL.null bs
  then []
  else let (x, y) = BL.splitAt n bs in x : splitEvery n y

detectChannels :: BL.ByteString -> Int
detectChannels = length . takeWhile ("MSVp" `BL.isPrefixOf`) . splitEvery 0x20000

splitChannels :: BL.ByteString -> [BL.ByteString]
splitChannels bs = do
  let blocks = splitEvery 0x20000 bs
      chans = detectChannels bs
      groups = chunksOf chans blocks
  i <- [0 .. chans - 1]
  return $ BL.concat $ groups >>= \group -> case drop i group of
    []      -> []
    blk : _ -> [blk]

convertChannelToVGS :: (MonadFail m) => BL.ByteString -> m BL.ByteString
convertChannelToVGS chan = do
  (len, rate) <- flip runGetM (BL.drop 12 chan) $ liftA2 (,) getWord32be getWord32be
  return $ runPut $ do
    putByteString $ B8.pack "VgS!"
    putWord32le 2
    let audio = BL.take (fromIntegral len) $ BL.drop 0x40 chan
    putWord32le $ fromIntegral rate
    putWord32le $ fromIntegral $ len `quot` 0x10
    putByteString $ B.replicate (0x80 - 16) 0
    putLazyByteString audio

-- GHWT audio

-- GameZelda called this "NeoIMF".
-- https://web.archive.org/web/20090327110815/https://usuarios.lycos.es/gamezelda/doc/neoimf.html

data NeoHeader = NeoHeader
  { neoKey         :: QBKey
  , totalBlockSize :: Word32
  , streams        :: [NeoStream]
  } deriving (Show)

data NeoStream = NeoStream
  { streamKey        :: QBKey
  , firstBlockOffset :: Word32
  , channelBlockSize :: Word32
  , streamSize       :: Word32
  , frequency        :: Word32
  , nsUnk1           :: Float
  , channels         :: Word8
  , nsUnk2           :: B.ByteString
  } deriving (Show)

getNeoHeader :: Get NeoHeader
getNeoHeader = do
  "494D463\x06"  <- getByteString 8
  neoKey         <- QBKey <$> getWord32le
  totalBlockSize <- getWord32le
  let getStreams prev = do
        streamKey <- QBKey <$> getWord32le
        if streamKey == 0
          then return $ reverse prev
          else do
            firstBlockOffset <- getWord32le
            channelBlockSize <- getWord32le
            streamSize       <- getWord32le
            frequency        <- getWord32le
            nsUnk1           <- getFloatle
            channels         <- getWord8
            nsUnk2           <- getByteString 3
            getStreams $ NeoStream{..} : prev
  streams <- getStreams []
  return NeoHeader{..}

splitNeoStreams :: NeoHeader -> BL.ByteString -> [(NeoStream, [BL.ByteString])]
splitNeoStreams hdr bs = let
  blocks = splitEvery (fromIntegral hdr.totalBlockSize) $ BL.drop 0x10000 bs
  in flip map hdr.streams $ \stream -> let
    blockTake = fromIntegral stream.channelBlockSize
    chans = flip map [0 .. stream.channels - 1] $ \i -> let
      blockDrop = fromIntegral (stream.firstBlockOffset - 0x10000)
        + fromIntegral i * blockTake
      in BL.concat $ map
        (BL.take blockTake . BL.drop blockDrop)
        blocks
    in (stream, chans)

neoStreamToVGS :: (NeoStream, [BL.ByteString]) -> BL.ByteString
neoStreamToVGS (stream, chans) = runPut $ do
  putByteString "VgS!"
  putWord32le 2
  forM_ chans $ \chan -> do
    putWord32le $ fromIntegral stream.frequency
    putWord32le $ fromIntegral $ quot (BL.length chan) 0x10
  putByteString $ B.replicate (0x80 - 8 - 8 * length chans) 0
  let go chansLeft = if any BL.null chansLeft
        then return ()
        else do
          forM_ (zip [0..] chansLeft) $ \(i, chanLeft) -> do
            putWord8 $ BL.head chanLeft
            putWord8 i
            putLazyByteString $ BL.take 14 $ BL.drop 2 chanLeft
          go $ map (BL.drop 0x10) chansLeft
  go chans

testSplitNeoIMF :: FilePath -> IO ()
testSplitNeoIMF f = do
  bs <- BL.readFile f
  hdr <- runGetM getNeoHeader bs
  forM_ (zip [0..] $ splitNeoStreams hdr bs) $ \(i, pair) -> do
    BL.writeFile (f <> "." <> show (i :: Int) <> ".vgs") $ neoStreamToVGS pair
