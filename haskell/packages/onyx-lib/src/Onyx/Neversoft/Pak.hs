{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
module Onyx.Neversoft.Pak where

import qualified Codec.Compression.Zlib.Internal as Z
import           Control.Monad                   (forM, forM_, guard,
                                                  replicateM)
import           Control.Monad.ST.Lazy           (runST)
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits                       ((.&.))
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Char                       (isAscii, isSpace)
import qualified Data.HashMap.Strict             as HM
import           Data.Int                        (Int64)
import           Data.List                       (sortOn)
import           Data.Maybe                      (fromMaybe, isJust,
                                                  listToMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Data.Word
import           GHC.ByteOrder
import           Numeric                         (readHex, showHex)
import           Onyx.Neversoft.CRC              (QBKey (..), putQBKeyBE)
import           Onyx.Neversoft.LZSS             (decompressLZSS)
import           Onyx.Util.Binary                (runGetM)
import           Witch                           (from)

data Node = Node
  { nodeFileType       :: QBKey
  , nodeOffset         :: Word32
  , nodeSize           :: Word32
  , nodeFilenamePakKey :: QBKey
  , nodeFilenameKey    :: QBKey
  , nodeFilenameCRC    :: QBKey
  , nodeUnknown        :: Word32
  , nodeFlags          :: Word32
  , nodeName           :: Maybe B.ByteString -- snippet of filename seen in PS2 GH3 when flags (LE) is 0x20
  } deriving (Show, Read)

-- | Chunked deflate starting with "CHNK" magic, used on 360/PS3 GHWT+
-- Credit to unpak.pl by Tarragon Allen (tma)
decompressCHNK :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressCHNK bs = do
  -- conveniently, start and next are relative to this CHNK, not the whole file
  [magic, start, len, next, _flags, _size] <- runGetM (replicateM 6 getWord32be) bs
  if magic /= 0x43484e4b -- CHNK
    then return BL.empty
    else do
      let buf = BL.take (from len) $ BL.drop (from start) bs
      dec <- tryDecompress Z.rawFormat False buf
      case next of
        0xffffffff -> return dec
        _          -> (dec <>) <$> decompressCHNK (BL.drop (from next) bs)

-- | Raw deflate of whole file, used on GH3 360 (compress.toc method 3)
decompressDeflate :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressDeflate = tryDecompress Z.rawFormat True

-- decompresses stream, optionally ignores "input ended prematurely" error
tryDecompress :: (MonadFail m) => Z.Format -> Bool -> BL.ByteString -> m BL.ByteString
tryDecompress format ignoreTruncate bs = either fail return $ runST let
  go input output = \case
    Z.DecompressInputRequired f                               -> case input of
      []     -> f B.empty >>= go [] output
      x : xs -> f x       >>= go xs output
    Z.DecompressOutputAvailable out getNext                   -> do
      next <- getNext
      go input (out : output) next
    Z.DecompressStreamEnd _unread                             -> return $ Right $ BL.fromChunks $ reverse output
    Z.DecompressStreamError Z.TruncatedInput | ignoreTruncate -> return $ Right $ BL.fromChunks $ reverse output
    Z.DecompressStreamError err                               -> return $
      Left $ "Decompression error: " <> show err
  in go (BL.toChunks bs) [] $ Z.decompressST format Z.defaultDecompressParams

data PakCompression
  = PakUncompressed
  | PakChunked -- ^ "CHNK" chunked deflate (GHWT and later, 360/PS3)
  | PakDeflate -- ^ raw deflate of the whole file (GH3/GH:A 360)
  | PakLZSS    -- ^ LZSS of the whole file (GH3/GH:A PS3, under DATA/COMPRESSED/PS3)
  deriving (Eq, Show)

-- | What was detected about a .pak (and .pab) when reading it
data PakInfo = PakInfo
  { byteOrder      :: ByteOrder -- ^ little endian on PS2, big elsewhere
  , compression    :: PakCompression
  , pabCompression :: Maybe PakCompression -- ^ Nothing if there is no .pab
  , pabRelative    :: Bool
    -- ^ True (WoR) if offsets are relative to the start of the .pab.
    -- False (GH3, and GHWT on at least PS2) if the .pab is treated as if it
    -- were appended to the .pak, starting at the first entry's offset.
  } deriving (Show)

-- | Reads the entry table in the given byte order. Returns each entry's position
-- in the table, and the node with its offset still relative to that position.
-- Fails if the table doesn't end in a "last" entry.
readPakEntries :: ByteOrder -> B.ByteString -> Maybe [(Word32, Node)]
readPakEntries endian pak = go 0 where
  w32 = wordAt endian pak
  go p = if p + 32 > B.length pak
    then Nothing
    else let
      nodeFileType       = QBKey $ w32 p
      nodeOffset         = w32 $ p + 4
      nodeSize           = w32 $ p + 8
      nodeFilenamePakKey = QBKey $ w32 $ p + 12
      nodeFilenameKey    = QBKey $ w32 $ p + 16
      nodeFilenameCRC    = QBKey $ w32 $ p + 20
      nodeUnknown        = w32 $ p + 24
      nodeFlags          = w32 $ p + 28
      -- PS2 paks have a 0xA0-byte filename after entries with flag 0x20.
      -- WoR sets flag 0x20 too but without a filename, so check that one is actually there.
      nameBlock = B.take 0xA0 $ B.drop (p + 32) pak
      nodeName = if nodeFlags .&. 0x20 /= 0 && looksLikeName nameBlock
        then Just $ B.takeWhile (/= 0) nameBlock
        else Nothing
      next = p + 32 + maybe 0 (const 0xA0) nodeName
      in ((fromIntegral p, Node{..}) :) <$> if elem nodeFileType ["last", ".last"]
        then Just []
        else go next
  looksLikeName block = let
    (str, rest) = B.break (== 0) block
    in B.length block == 0xA0
      && not (B.null str)
      && B.all (\c -> 0x20 <= c && c < 0x7F) str
      && B.all (== 0) rest

-- | Reads a 32-bit word at a position (which should be in range).
wordAt :: ByteOrder -> B.ByteString -> Int -> Word32
wordAt endian bs i = let
  bytes = B.unpack $ B.take 4 $ B.drop i bs
  ordered = case endian of
    BigEndian    -> bytes
    LittleEndian -> reverse bytes
  in foldl (\acc b -> acc * 0x100 + fromIntegral b) 0 ordered

-- | Finds the compression and byte order of a .pak by checking which one gives
-- a valid entry table. Returns the decompressed .pak and its entries.
detectPak :: BL.ByteString -> Maybe BL.ByteString -> Maybe (BL.ByteString, PakCompression, ByteOrder, [(Word32, Node)])
detectPak pak maybePab = listToMaybe do
  (comp, pak') <- if BL.take 4 pak == "CHNK"
    then [(PakChunked, dec) | Just dec <- [decompressCHNK pak]]
    else concat
      [ [(PakUncompressed, pak)]
      , [(PakDeflate, dec) | Just dec <- [decompressDeflate pak]]
      , [(PakLZSS, BL.fromStrict $ decompressLZSS $ BL.toStrict pak)]
      ]
  let strict = BL.toStrict pak'
      totalLen = fromIntegral (B.length strict) + maybe 0 BL.length maybePab
      -- try first the byte order where the first entry's offset and size look sensible
      plausible endian = B.length strict >= 12 && all
        (\i -> fromIntegral (wordAt endian strict i) <= totalLen)
        [4, 8]
      orders = sortOn (not . plausible) [BigEndian, LittleEndian]
  endian <- orders
  Just entries <- [readPakEntries endian strict]
  -- Compressed bytes can contain the "last" key verbatim (LZSS copies literal bytes through),
  -- so also check two things true of every official pak: the first entry's offset is
  -- within the .pak (a .pab is appended at or before the .pak's end, or uses offset 0),
  -- and the final "last" entry is 0 or 4 bytes.
  case (entries, reverse entries) of
    ((_, firstNode) : _, (_, lastNode) : _) -> guard
      $  fromIntegral firstNode.nodeOffset <= B.length strict
      && lastNode.nodeSize <= 4
    _ -> []
  return (pak', comp, endian, entries)

-- | Splits a .pak (and optional .pab) into its files, detecting the format.
splitPak :: (MonadFail m) => BL.ByteString -> Maybe BL.ByteString -> m ([(Node, BL.ByteString)], PakInfo)
splitPak pak maybePab
  | "\\\\Dummy file" `BL.isPrefixOf` pak
  -- these are seen in GH3 PS2, songs/*_{gfx,sfx}.pak.ps2
  = return ([], PakInfo
    { byteOrder = LittleEndian
    , compression = PakUncompressed
    , pabCompression = Nothing
    , pabRelative = False
    })
  | otherwise = case detectPak pak maybePab of
    Nothing -> fail ".pak format not recognized (no valid entry table found in any byte order or compression)"
    Just (pak', comp, endian, entries) -> let
      firstOffset = case entries of
        (_, node) : _ -> node.nodeOffset
        []            -> 0
      pabRelative = isJust maybePab && firstOffset == 0
      nodes = flip map entries \(posn, node) -> if pabRelative
        then node
        else node { nodeOffset = posn + node.nodeOffset }
      -- the data that the final node offsets point into
      (dataSection, pabComp) = case maybePab of
        Nothing -> (pak', Nothing)
        Just pab -> let
          -- in GH3, the .pab acts as if it were appended to the .pak at the first entry's offset.
          -- (this also makes us ignore the useless pab of GH3 PS2 zones/global/global_net.pak.ps2)
          prefix = if pabRelative then BL.empty else BL.take (fromIntegral firstOffset) pak'
          -- The "last" entry's data is at the end of the data section. (Don't use other entries;
          -- some point past the end, like VRAM entries whose data is in the _vram.pak.)
          needed = case reverse nodes of
            lastNode : _ -> fromIntegral lastNode.nodeOffset + fromIntegral lastNode.nodeSize - BL.length prefix
            []           -> 0
          (pab', comp') = decodePab needed pab
          in (prefix <> pab', Just comp')
      attachData :: Node -> (Node, BL.ByteString)
      attachData node = (node, BL.take (fromIntegral node.nodeSize) $ BL.drop (fromIntegral node.nodeOffset) dataSection)
      in return
        ( map attachData nodes
        , PakInfo
          { byteOrder = endian
          , compression = comp
          , pabCompression = pabComp
          , pabRelative = pabRelative
          }
        )

-- | Undoes .pab compression; without CHNK magic there's no marker,
-- so pick the first candidate that is long enough to hold all the entries' data
decodePab :: Int64 -> BL.ByteString -> (BL.ByteString, PakCompression)
decodePab needed pab = if BL.take 4 pab == "CHNK"
  then (fromMaybe pab $ decompressCHNK pab, PakChunked)
  else let
    candidates = concat
      [ [(pab, PakUncompressed)]
      , [(dec, PakDeflate) | Just dec <- [decompressDeflate pab]]
      , [(BL.fromStrict $ decompressLZSS $ BL.toStrict pab, PakLZSS)]
      ]
    in fromMaybe (pab, PakUncompressed) $ listToMaybe
      [ cand | cand@(bs, _) <- candidates, BL.length bs >= needed ]

splitPakNodes :: (MonadFail m) => BL.ByteString -> Maybe BL.ByteString -> m [(Node, BL.ByteString)]
splitPakNodes pak maybePab = fst <$> splitPak pak maybePab

buildPak :: [(Node, BL.ByteString)] -> BL.ByteString
buildPak nodes = let
  -- previously sorted according to original position, but don't think it's necessary
  -- nodes' = sortOn (nodeOffset . fst) nodes
  fixNodes _    []                  = []
  fixNodes posn ((node, bs) : rest) = let
    len = fromIntegral $ BL.length bs
    in node
      { nodeOffset = posn
      , nodeSize = len
      } : fixNodes (padLength $ posn + len) rest
  dataStart = 0x1000 -- TODO support if this needs to be higher
  padLength n = 0x10 + case quotRem n 0x10 of
    (_, 0) -> n
    (q, _) -> (q + 1) * 0x10
  padData bs = bs <> let
    len = BL.length bs
    in BL.replicate (padLength len - len) 0
  putHeader (i, Node{..}) = do
    putQBKeyBE nodeFileType
    putWord32be $ nodeOffset - 32 * i
    putWord32be nodeSize
    putQBKeyBE nodeFilenamePakKey
    putQBKeyBE nodeFilenameKey
    putQBKeyBE nodeFilenameCRC
    putWord32be nodeUnknown
    putWord32be nodeFlags
    forM_ nodeName \bs -> do
      putByteString bs
      putByteString $ B.replicate (0xA0 - B.length bs) 0
  header = runPut $ mapM_ putHeader $ zip [0..] $ fixNodes dataStart nodes
  header' = BL.take (from dataStart) $ header <> BL.replicate (from dataStart) 0
  in BL.concat $ [header'] <> map (padData . snd) nodes <> [BL.replicate 0xCF0 0xAB]

qsBank :: [(Node, BL.ByteString)] -> HM.HashMap Word32 T.Text
qsBank nodes = HM.fromList do
  (node, nodeData) <- nodes
  guard $ elem node.nodeFileType [".qs.en", ".qs"]
  fromMaybe [] $ parseQS nodeData

parseQS :: BL.ByteString -> Maybe [(Word32, T.Text)]
parseQS bs = do
  t <- if "\xFF\xFE" `BL.isPrefixOf` bs
    then T.stripPrefix "\xFEFF" $ TE.decodeUtf16LE $ BL.toStrict bs
    else return $ TE.decodeLatin1 $ BL.toStrict bs -- ghwt ps2
  fmap concat $ forM (T.lines t) \ln ->
    if T.all isSpace ln
      then return []
      else do
        (hex, "") <- listToMaybe $ readHex $ T.unpack $ T.take 8 ln
        str <- T.stripPrefix "\"" (T.strip $ T.drop 8 ln) >>= T.stripSuffix "\""
        return [(hex, str)]

makeQS :: [(Word32, T.Text)] -> BL.ByteString
makeQS entries = BL.fromStrict $ TE.encodeUtf16LE let
  lns = T.unlines do
    (w, t) <- sortOn snd entries -- Sort not necessary, but official ones are
    let hex = T.pack $ showHex w ""
        hex' = T.replicate (8 - T.length hex) "0" <> hex
    return $ hex' <> " \"" <> t <> "\""
  in "\xFEFF" <> lns <> "\n\n" -- Extra newlines probably not necessary

worMetadataString :: T.Text -> T.Text
worMetadataString = noSortCrash . noBrackets . fancyQuotes where
  -- I don't yet know how to do simple double quotes inside a qs string (if it's possible).
  -- \q \Q \" don't work
  -- So for now, we replace with left/right quotes, which are supported by WoR, in a simple alternating pattern.
  fancyQuotes t
    = T.concat
    $ concat
    $ zipWith (\x y -> [x, y]) ("" : cycle ["“", "”"]) (T.splitOn "\"" t)
  -- These aren't in the fonts used for title/artist
  noBrackets = T.replace "[" "(" . T.replace "]" ")"
  -- The first character (after ignored words) of title/artist must be ASCII.
  -- Otherwise WoR crashes when you sort by title/artist in the song list.
  -- Æ and “ were observed to crash; ASCII punctuation appears to be fine.
  -- Likely it's in the code that is supposed to put it under a category header.
  noSortCrash t = case T.stripPrefix "\\L" t of
    Nothing -> noSortCrash' t
    Just t' -> "\\L" <> noSortCrash' t'
  noSortCrash' t = case dropIgnored $ T.unpack $ T.toLower t of
    c : _ -> if isAscii c
      then t
      else ". " <> t
    "" -> "."
  dropIgnored = \case
    -- dunno if any other leading words are ignored
    't' : 'h' : 'e' : ' ' : xs -> dropIgnored xs
    'a' :             ' ' : xs -> dropIgnored xs
    'a' : 'n' :       ' ' : xs -> dropIgnored xs
    xs                         -> xs
