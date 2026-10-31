{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-
MT: mostly unmodified Claude. This could maybe be improved to not bytestring the
whole file and just use the edited + added parts to make a Readable view layer

--------------------------------------------------------------------------------

Appends silence to the end of an Ogg Vorbis file without re-encoding.

A Vorbis audio packet can mark every channel's floor as "unused", which decodes
to all zeros and takes only a few bits. We parse the setup header to learn how
to write such packets (mode numbers, block sizes, and how many bits each
channel's floor needs), append them in new Ogg pages, and fix up the old last
page (clear EOS, recompute its granule position since it is no longer final).
-}
module Onyx.Audio.Extend
( extendOggVorbis
, extendOggVorbisBytes
) where

import           Control.Monad           (forM, forM_, replicateM, replicateM_,
                                          unless, when)
import           Data.Bits
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy    as BL
import           Data.Int                (Int64)
import           Data.List               (findIndex, foldl')
import           Data.Maybe              (catMaybes)
import qualified Data.Vector.Unboxed     as VU
import           Data.Word               (Word32, Word8)
import           Onyx.Util.Handle        (Readable (..), byteStringSimpleHandle,
                                          handleLabel, openSimpleHandle,
                                          useHandle)
import           System.IO               (hFileSize)

-- | Adds the given number of seconds of silence to the end of an Ogg Vorbis
-- file. The work happens when the result is opened; errors are thrown then.
extendOggVorbis :: Double -> Readable -> Readable
extendOggVorbis secs r = if secs <= 0 then r else Readable
  { rFilePath = Nothing
  , rOpen = do
    (label, input) <- useHandle r \h -> do
      bs <- hFileSize h >>= B.hGet h . fromIntegral
      return (handleLabel h, bs)
    case extendOggVorbisBytes secs input of
      Left  err -> fail $ "extendOggVorbis (" <> label <> "): " <> err
      Right out -> byteStringSimpleHandle out >>= openSimpleHandle (label <> " | extended")
  }

-- | Pure version of 'extendOggVorbis' operating on the whole file.
extendOggVorbisBytes :: Double -> B.ByteString -> Either String BL.ByteString
extendOggVorbisBytes secs input = do
  pages <- parsePages input
  firstPage <- case pages of
    p : _ -> Right p
    []    -> Left "No Ogg pages found"
  unless (all (\pg -> pg.serial == firstPage.serial) pages) $
    Left "Multiplexed or chained Ogg streams are not supported"
  let packets = pagePackets pages
  (idPacket, setupPacket, audioPackets) <- case packets of
    (_, i) : _ : (_, s) : rest -> Right (i, s, rest)
    _                          -> Left "Missing Vorbis header packets"
  idh <- parseIdHeader idPacket
  modes <- runBitGet (setupParser idh.channels) setupPacket
  let modeBits = ilog $ length modes - 1
      blockSize isLong = if isLong then idh.blocksize1 else idh.blocksize0
      -- packets that don't parse as audio are ignored, same as libvorbis
      blocks = catMaybes $ flip map audioPackets \(pageIndex, pkt) ->
        case runBitGet (packetBlockInfo modeBits modes) pkt of
          Right (Just (isLong, nextLong)) -> Just (pageIndex, isLong, nextLong)
          _                               -> Nothing
      -- samples output by each packet; the first audio packet outputs none
      blockSamples = case blocks of
        [] -> []
        (p0, _, _) : _ -> (p0, 0) : zipWith
          (\(_, prev, _) (p, cur, _) -> (p, quot (blockSize prev) 4 + quot (blockSize cur) 4))
          blocks
          (drop 1 blocks)
      lastIndex = length pages - 1
      lastPage = last pages
  (lastBlockPage, lastLong, lastNextLong) <- case blocks of
    [] -> Left "No audio packets found"
    _  -> Right $ last blocks
  unless (lastBlockPage == lastIndex) $
    Left "Last Ogg page does not end an audio packet"
  when (lastPage.granule == -1) $
    Left "Last Ogg page has no granule position"

  let prevGranule = case filter (/= -1) $ map (.granule) $ init pages of
        [] -> 0
        gs -> last gs
      -- the untrimmed decoded length, which the last page's granule
      -- needs to be once it's no longer the final page
      trueEnd = prevGranule + sum [ fromIntegral n | (p, n) <- blockSamples, p == lastIndex ]
      addSamples = round $ secs * fromIntegral idh.rate :: Int64
      target = lastPage.granule + addSamples
      prefix = B.take lastPage.offset input

  if addSamples <= 0
    then Right $ BL.fromStrict input
    else if target <= trueEnd
      -- The encoder's trimmed padding already covers the new length,
      -- so just move the final granule (decoders trim the last packet).
      then Right $ BL.fromChunks
        [ prefix
        , buildPage lastPage.flags target lastPage.serial lastPage.sequence
            lastPage.segments lastPage.body
        ]
      else do
        let longMode  = findIndex (.long) modes
            shortMode = findIndex (not . (.long)) modes
            anyMode = case longMode of
              Just m  -> Right m
              Nothing -> maybe (Left "No Vorbis modes") Right shortMode
        restMode <- anyMode
        -- If the last real block expected a short block next, give it one so
        -- the windows overlap correctly; afterwards prefer long blocks.
        let firstMode = case shortMode of
              Just m | lastLong && not lastNextLong -> m
              _                                     -> restMode
            isLongMode m = (modes !! m).long
            -- every packet after the first uses restMode, so that's the next window
            silentPacket prevLong m = packBits $ concat
              [ [(1, 0), (modeBits, m)]
              , if isLongMode m
                then [(1, fromEnum prevLong), (1, fromEnum $ isLongMode restMode)]
                else []
              , [((modes !! m).silentBits, 0)]
              ]
            -- (packet, granule after packet), until we reach the target
            generate :: Int64 -> Bool -> Bool -> [(B.ByteString, Int64)]
            generate cur prevLong isFirst = let
              m = if isFirst then firstMode else restMode
              thisLong = isLongMode m
              cur' = cur + fromIntegral (quot (blockSize prevLong) 4 + quot (blockSize thisLong) 4)
              in (silentPacket prevLong m, cur') : if cur' >= target then [] else generate cur' thisLong False
            newPackets = generate trueEnd lastLong True
            groups = groupPackets newPackets
            newPages = flip map (zip3 [1 ..] groups $ map (== length groups) [1 ..])
              \(i, grp, isLast) -> let
                segs = concatMap (lacing . B.length . fst) grp
                granule = if isLast then target else snd $ last grp
                flags = if isLast then 4 else 0
                in buildPage flags granule lastPage.serial (lastPage.sequence + i)
                  segs (B.concat $ map fst grp)
        Right $ BL.fromChunks
          $ prefix
          : buildPage (lastPage.flags .&. complement 4) trueEnd lastPage.serial
              lastPage.sequence lastPage.segments lastPage.body
          : newPages

-- Ogg layer

data Page = Page
  { offset   :: Int
  , flags    :: Word8
  , granule  :: Int64
  , serial   :: Word32
  , sequence :: Word32
  , segments :: [Int] -- ^ lacing values
  , body     :: B.ByteString
  }

readLE :: (Integral a, Bits a) => B.ByteString -> Int -> Int -> a
readLE bs off n = foldr
  (\i acc -> fromIntegral (B.index bs $ off + i) .|. shiftL acc 8)
  0
  [0 .. n - 1]

parsePages :: B.ByteString -> Either String [Page]
parsePages input = go 0 where
  go off = if off >= B.length input then Right [] else do
    let rest = B.drop off input
        bad msg = Left $ msg <> " at offset " <> show off
    when (B.length rest < 27 || B.take 4 rest /= "OggS") $ bad "Invalid Ogg page"
    when (B.index rest 4 /= 0) $ bad "Unsupported Ogg version"
    let nsegs = fromIntegral $ B.index rest 26
    when (B.length rest < 27 + nsegs) $ bad "Truncated Ogg page"
    let segs = map fromIntegral $ B.unpack $ B.take nsegs $ B.drop 27 rest
        bodyStart = 27 + nsegs
        total = bodyStart + sum segs
    when (B.length rest < total) $ bad "Truncated Ogg page"
    let page = Page
          { offset   = off
          , flags    = B.index rest 5
          , granule  = readLE rest 6 8
          , serial   = readLE rest 14 4
          , sequence = readLE rest 18 4
          , segments = segs
          , body     = B.take (total - bodyStart) $ B.drop bodyStart rest
          }
    (page :) <$> go (off + total)

-- | Complete packets from a page body, plus a trailing unfinished piece if any.
splitLacing :: [Int] -> B.ByteString -> ([B.ByteString], Maybe B.ByteString)
splitLacing = go 0 where
  go acc [] b = ([], if acc == 0 then Nothing else Just $ B.take acc b)
  go acc (s : ss) b = let acc' = acc + s in if s < 255
    then let
      (pkt, b') = B.splitAt acc' b
      (more, partial) = go 0 ss b'
      in (pkt : more, partial)
    else go acc' ss b

-- | All complete packets, each tagged with the index of the page it ends on.
pagePackets :: [Page] -> [(Int, B.ByteString)]
pagePackets = go Nothing . zip [0 ..] where
  go :: Maybe B.ByteString -> [(Int, Page)] -> [(Int, B.ByteString)]
  go _ [] = []
  go carry ((i, pg) : rest) = let
    (complete, partial) = splitLacing pg.segments pg.body
    (complete', carry') = case (carry, complete) of
      (Just c, p : ps) -> ((c <> p) : ps, partial)
      (Just c, [])     -> ([], Just $ maybe c (c <>) partial)
      (Nothing, _)     -> (complete, partial)
    in map (i,) complete' <> go carry' rest

lacing :: Int -> [Int]
lacing n = replicate (quot n 255) 255 <> [rem n 255]

-- | Splits packets into page-sized groups (at most 255 lacing values each).
groupPackets :: [(B.ByteString, a)] -> [[(B.ByteString, a)]]
groupPackets = go [] 0 where
  go cur _ [] = [reverse cur | not $ null cur]
  go cur n (p : ps) = let
    k = length $ lacing $ B.length $ fst p
    in if n + k > 255 && not (null cur)
      then reverse cur : go [p] k ps
      else go (p : cur) (n + k) ps

buildPage :: Word8 -> Int64 -> Word32 -> Word32 -> [Int] -> B.ByteString -> B.ByteString
buildPage flags granule serial seqNum segs body = let
  noCRC = BL.toStrict $ BB.toLazyByteString $ mconcat
    [ BB.byteString "OggS"
    , BB.word8 0
    , BB.word8 flags
    , BB.int64LE granule
    , BB.word32LE serial
    , BB.word32LE seqNum
    , BB.word32LE 0
    , BB.word8 $ fromIntegral $ length segs
    , foldMap (BB.word8 . fromIntegral) segs
    , BB.byteString body
    ]
  crc = BL.toStrict $ BB.toLazyByteString $ BB.word32LE $ oggCRC noCRC
  in B.take 22 noCRC <> crc <> B.drop 26 noCRC

crcTable :: VU.Vector Word32
crcTable = VU.generate 256 \i -> foldl'
  (\r _ -> if testBit r 31 then shiftL r 1 `xor` 0x04C11DB7 else shiftL r 1)
  (shiftL (fromIntegral i) 24)
  [1 .. 8 :: Int]

oggCRC :: B.ByteString -> Word32
oggCRC = B.foldl'
  (\crc b -> shiftL crc 8 `xor` (crcTable VU.! fromIntegral (shiftR crc 24 `xor` fromIntegral b)))
  0

-- Vorbis bit packing (LSB first)

newtype BitGet a = BitGet (B.ByteString -> Int -> Either String (a, Int))

instance Functor BitGet where
  fmap f (BitGet g) = BitGet \bs pos -> fmap (\(x, pos') -> (f x, pos')) $ g bs pos

instance Applicative BitGet where
  pure x = BitGet \_ pos -> Right (x, pos)
  BitGet f <*> BitGet x = BitGet \bs pos -> do
    (f', pos' ) <- f bs pos
    (x', pos'') <- x bs pos'
    Right (f' x', pos'')

instance Monad BitGet where
  BitGet x >>= f = BitGet \bs pos -> do
    (x', pos') <- x bs pos
    let BitGet g = f x'
    g bs pos'

runBitGet :: BitGet a -> B.ByteString -> Either String a
runBitGet (BitGet f) bs = fst <$> f bs 0

failB :: String -> BitGet a
failB err = BitGet \_ _ -> Left err

skipBits :: Int -> BitGet ()
skipBits n = BitGet \bs pos -> if pos + n > 8 * B.length bs
  then Left "Unexpected end of Vorbis packet"
  else Right ((), pos + n)

getBits :: Int -> BitGet Int
getBits n = BitGet \bs pos -> if pos + n > 8 * B.length bs
  then Left "Unexpected end of Vorbis packet"
  else let
    bitAt p = testBit (B.index bs $ shiftR p 3) (p .&. 7)
    value = foldl' (\acc i -> if bitAt (pos + i) then setBit acc i else acc) 0 [0 .. n - 1]
    in Right (value, pos + n)

getFlag :: BitGet Bool
getFlag = (/= 0) <$> getBits 1

-- | (bit count, value) fields, packed LSB first.
packBits :: [(Int, Int)] -> B.ByteString
packBits fields = B.pack $ map toByte $ chunksOf8 bits where
  bits = concat [ [ testBit v i | i <- [0 .. n - 1] ] | (n, v) <- fields ]
  toByte bs = foldl' (\acc (i, b) -> if b then setBit acc i else acc) 0 $ zip [0 ..] bs
  chunksOf8 [] = []
  chunksOf8 xs = let (a, b) = splitAt 8 xs in a : chunksOf8 b

ilog :: Int -> Int
ilog n = if n <= 0 then 0 else finiteBitSize n - countLeadingZeros n

-- Vorbis headers

data IdHeader = IdHeader
  { channels   :: Int
  , rate       :: Int
  , blocksize0 :: Int
  , blocksize1 :: Int
  }

parseIdHeader :: B.ByteString -> Either String IdHeader
parseIdHeader pkt = do
  unless (B.length pkt >= 30 && B.take 7 pkt == "\x01vorbis") $
    Left "Not a Vorbis stream (bad identification header)"
  let blocksizes = B.index pkt 28
  Right IdHeader
    { channels   = fromIntegral $ B.index pkt 11
    , rate       = readLE pkt 12 4
    , blocksize0 = shiftL 1 $ fromIntegral $ blocksizes .&. 0xF
    , blocksize1 = shiftL 1 $ fromIntegral $ shiftR blocksizes 4
    }

data Mode = Mode
  { long       :: Bool
  , silentBits :: Int -- ^ total floor bits for an all-channels-unused packet
  }

-- | Parses the setup header just enough to get the modes. Codebooks and
-- residues are skipped, but must be parsed since they're variable-length.
setupParser :: Int -> BitGet [Mode]
setupParser chans = do
  typ <- getBits 8
  magic <- replicateM 6 $ getBits 8
  unless (typ == 5 && magic == map fromEnum ("vorbis" :: String)) $
    failB "Invalid Vorbis setup header"
  codebookCount <- (+ 1) <$> getBits 8
  replicateM_ codebookCount skipCodebook
  timeCount <- (+ 1) <$> getBits 6
  replicateM_ timeCount do
    t <- getBits 16
    when (t /= 0) $ failB "Invalid Vorbis time domain transform"
  floorCount <- (+ 1) <$> getBits 6
  floorBits <- replicateM floorCount parseFloor
  residueCount <- (+ 1) <$> getBits 6
  replicateM_ residueCount skipResidue
  mappingCount <- (+ 1) <$> getBits 6
  mappings <- replicateM mappingCount $ parseMapping chans
  forM_ (concat mappings) \f -> when (f >= floorCount) $
    failB "Invalid Vorbis floor number in mapping"
  modeCount <- (+ 1) <$> getBits 6
  modes <- replicateM modeCount do
    blockflag <- getFlag
    windowType <- getBits 16
    transformType <- getBits 16
    mapping <- getBits 8
    when (windowType /= 0 || transformType /= 0 || mapping >= mappingCount) $
      failB "Invalid Vorbis mode"
    return Mode
      { long       = blockflag
      , silentBits = sum $ map (floorBits !!) $ mappings !! mapping
      }
  framing <- getFlag
  unless framing $ failB "Missing Vorbis setup header framing bit"
  return modes

skipCodebook :: BitGet ()
skipCodebook = do
  sync <- getBits 24
  when (sync /= 0x564342) $ failB "Invalid Vorbis codebook sync pattern"
  dims <- getBits 16
  entries <- getBits 24
  ordered <- getFlag
  if ordered
    then do
      skipBits 5
      let go cur = when (cur < entries) do
            n <- getBits $ ilog $ entries - cur
            go $ cur + n
      go 0
    else do
      sparse <- getFlag
      replicateM_ entries do
        used <- if sparse then getFlag else return True
        when used $ skipBits 5
  getBits 4 >>= \case
    0 -> return ()
    lookupType | lookupType <= 2 -> do
      skipBits 64 -- minimum value, delta value
      valueBits <- (+ 1) <$> getBits 4
      skipBits 1 -- sequence_p
      when (dims == 0) $ failB "Invalid Vorbis codebook dimensions"
      let values = if lookupType == 1 then lookup1Values entries dims else entries * dims
      skipBits $ values * valueBits
    _ -> failB "Invalid Vorbis codebook lookup type"

-- | Greatest r such that r^dims <= entries.
lookup1Values :: Int -> Int -> Int
lookup1Values entries dims = let
  fits r = toInteger r ^ dims <= toInteger entries
  guess = floor $ (fromIntegral entries :: Double) ** (1 / fromIntegral dims) :: Int
  up r = if fits (r + 1) then up (r + 1) else r
  down r = if r > 0 && not (fits r) then down (r - 1) else r
  in up $ down guess

-- | Returns the number of bits an "unused" floor takes in an audio packet.
parseFloor :: BitGet Int
parseFloor = getBits 16 >>= \case
  0 -> do
    skipBits $ 8 + 16 + 16 -- order, rate, bark map size
    ampBits <- getBits 6
    skipBits 8 -- amplitude offset
    books <- (+ 1) <$> getBits 4
    skipBits $ 8 * books
    return ampBits -- amplitude of 0 means unused
  1 -> do
    partitions <- getBits 5
    classList <- replicateM partitions $ getBits 4
    let maxClass = if null classList then -1 else maximum classList
    classDims <- forM [0 .. maxClass] \_ -> do
      dims <- (+ 1) <$> getBits 3
      subclasses <- getBits 2
      when (subclasses /= 0) $ skipBits 8 -- masterbook
      skipBits $ 8 * shiftL 1 subclasses
      return dims
    skipBits 2 -- multiplier
    rangeBits <- getBits 4
    forM_ classList \c -> skipBits $ rangeBits * (classDims !! c)
    return 1 -- "nonzero" flag
  _ -> failB "Invalid Vorbis floor type"

skipResidue :: BitGet ()
skipResidue = do
  typ <- getBits 16
  when (typ > 2) $ failB "Invalid Vorbis residue type"
  skipBits $ 24 * 3 -- begin, end, partition size
  classifications <- (+ 1) <$> getBits 6
  skipBits 8 -- classbook
  cascades <- replicateM classifications do
    low <- getBits 3
    high <- getFlag >>= \b -> if b then getBits 5 else return 0
    return $ high * 8 + low
  forM_ cascades \c -> skipBits $ 8 * popCount c

-- | Returns the floor number used by each channel.
parseMapping :: Int -> BitGet [Int]
parseMapping chans = do
  typ <- getBits 16
  when (typ /= 0) $ failB "Invalid Vorbis mapping type"
  submaps <- getFlag >>= \b -> if b then (+ 1) <$> getBits 4 else return 1
  getFlag >>= \b -> when b do
    steps <- (+ 1) <$> getBits 8
    skipBits $ steps * 2 * ilog (chans - 1)
  reserved <- getBits 2
  when (reserved /= 0) $ failB "Invalid Vorbis mapping reserved field"
  mux <- if submaps > 1
    then replicateM chans $ getBits 4
    else return $ replicate chans 0
  submapFloors <- replicateM submaps do
    skipBits 8 -- unused time config
    floorNum <- getBits 8
    skipBits 8 -- residue
    return floorNum
  forM mux \m -> if m >= submaps
    then failB "Invalid Vorbis mapping submap number"
    else return $ submapFloors !! m

-- | For an audio packet: (is long block, next window flag).
packetBlockInfo :: Int -> [Mode] -> BitGet (Maybe (Bool, Bool))
packetBlockInfo modeBits modes = getBits 1 >>= \case
  0 -> do
    m <- getBits modeBits
    if m >= length modes
      then return Nothing
      else if (modes !! m).long
        then do
          _prevLong <- getFlag
          nextLong <- getFlag
          return $ Just (True, nextLong)
        else return $ Just (False, False)
  _ -> return Nothing
