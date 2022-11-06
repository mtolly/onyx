{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
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
import           Data.List                       (sortOn)
import           Data.Maybe                      (fromMaybe, listToMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Data.Word
import           GHC.ByteOrder
import           Numeric                         (readHex, showHex)
import           Onyx.Neversoft.CRC              (qbKeyCRC)
import           Onyx.Xbox.STFS                  (runGetM)

data Node = Node
  { nodeFileType       :: Word32
  , nodeOffset         :: Word32
  , nodeSize           :: Word32
  , nodeFilenamePakKey :: Word32
  , nodeFilenameKey    :: Word32
  , nodeFilenameCRC    :: Word32
  , nodeUnknown        :: Word32
  , nodeFlags          :: Word32
  , nodeName           :: Maybe B.ByteString -- snippet of filename seen in PS2 GH3 when flags (LE) is 0x20
  } deriving (Show, Read)

-- Credit to unpak.pl by Tarragon Allen (tma).
-- Used in GHWT and onward.
-- TODO this seems to not handle some WoR disc files right, like qb.pab.xen
decompressPakGH4 :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressPakGH4 bs = let
  -- conveniently, start and next are relative to this CHNK, not the whole file
  [magic, start, len, next, _flags, _size] = runGet (replicateM 6 getWord32be) bs
  in if magic /= 0x43484e4b -- CHNK
    then return BL.empty
    else do
      let buf = BL.take (fromIntegral len) $ BL.drop (fromIntegral start) bs
      dec <- tryDecompress Z.rawFormat False buf
      case next of
        0xffffffff -> return dec
        _          -> (dec <>) <$> decompressPakGH4 (BL.drop (fromIntegral next) bs)

-- TODO this apparently doesn't work for PS3 ("invalid block type") at least qb.pab.ps3
decompressPakGH3 :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressPakGH3 bs = tryDecompress Z.zlibFormat True $ BL.pack [0x58, 0x85] <> bs

-- decompresses stream, optionally ignores "input ended prematurely" error
tryDecompress :: (MonadFail m) => Z.Format -> Bool -> BL.ByteString -> m BL.ByteString
tryDecompress format ignoreTruncate bs = either fail return $ runST $ let
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

decompressPak :: (MonadFail m) => BL.ByteString -> m BL.ByteString
decompressPak bs = if BL.take 4 bs == "CHNK"
  then decompressPakGH4 bs
  else return $ fromMaybe bs $ decompressPakGH3 bs -- if gh3 decompression fails, assume it's uncompressed

-- .pak header values are big endian on 360/PS3, but little on PS2

getPakNodes :: (MonadFail m) => ByteOrder -> BL.ByteString -> m [Node]
getPakNodes endian = runGetM $ let
  end = qbKeyCRC "last"
  end2 = qbKeyCRC ".last"
  go = do
    posn <- fromIntegral <$> bytesRead
    let getW32 = case endian of
          BigEndian    -> getWord32be
          LittleEndian -> getWord32le
    nodeFileType       <- getW32
    nodeOffset         <- (+ posn) <$> getW32
    nodeSize           <- getW32
    nodeFilenamePakKey <- getW32
    nodeFilenameKey    <- getW32
    nodeFilenameCRC    <- getW32
    nodeUnknown        <- getW32
    nodeFlags          <- getW32
    nodeName           <- if nodeFlags .&. 0x20 == 0x20
      then Just . B.takeWhile (/= 0) <$> getByteString 0xA0
      else return Nothing
    (Node{..} :) <$> if elem nodeFileType [end, end2]
      then return []
      else go
  in go

splitPakNodes :: (MonadFail m) => ByteOrder -> BL.ByteString -> Maybe BL.ByteString -> m [(Node, BL.ByteString)]
splitPakNodes _ pak _
  | "\\\\Dummy file" `BL.isPrefixOf` pak
  -- these are seen in GH3 PS2, songs/*_{gfx,sfx}.pak.ps2
  = return []
splitPakNodes endian pak maybePab = do
  pak'      <- decompressPak pak
  maybePab' <- mapM decompressPak maybePab
  let bs' = pak' <> fromMaybe BL.empty maybePab'
      attachData node = let
        goToData
          = BL.take (fromIntegral $ nodeSize node)
          . BL.drop (fromIntegral $ nodeOffset node)
        in (node, goToData bs')
  map attachData <$> getPakNodes endian bs'

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
    putWord32be nodeFileType
    putWord32be $ nodeOffset - 32 * i
    putWord32be nodeSize
    putWord32be nodeFilenamePakKey
    putWord32be nodeFilenameKey
    putWord32be nodeFilenameCRC
    putWord32be nodeUnknown
    putWord32be nodeFlags
    forM_ nodeName $ \bs -> do
      putByteString bs
      putByteString $ B.replicate (0xA0 - B.length bs) 0
  header = runPut $ mapM_ putHeader $ zip [0..] $ fixNodes dataStart nodes
  header' = BL.take (fromIntegral dataStart) $ header <> BL.replicate (fromIntegral dataStart) 0
  in BL.concat $ [header'] <> map (padData . snd) nodes <> [BL.replicate 0xCF0 0xAB]

qsBank :: [(Node, BL.ByteString)] -> HM.HashMap Word32 T.Text
qsBank nodes = HM.fromList $ do
  (node, nodeData) <- nodes
  guard $ elem (nodeFileType node) [qbKeyCRC ".qs.en", qbKeyCRC ".qs"]
  fromMaybe [] $ parseQS nodeData

parseQS :: BL.ByteString -> Maybe [(Word32, T.Text)]
parseQS bs = do
  t <- T.stripPrefix "\xFEFF" $ TE.decodeUtf16LE $ BL.toStrict bs
  fmap concat $ forM (T.lines t) $ \ln ->
    if T.all isSpace ln
      then return []
      else do
        (hex, "") <- listToMaybe $ readHex $ T.unpack $ T.take 8 ln
        str <- T.stripPrefix "\"" (T.strip $ T.drop 8 ln) >>= T.stripSuffix "\""
        return [(hex, str)]

makeQS :: [(Word32, T.Text)] -> BL.ByteString
makeQS entries = BL.fromStrict $ TE.encodeUtf16LE $ let
  lns = T.unlines $ do
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
    $ map (\(x, y) -> [x, y])
    $ zip ("" : cycle ["“", "”"]) (T.splitOn "\"" t)
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
