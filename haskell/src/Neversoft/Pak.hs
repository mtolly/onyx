{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Pak where

import qualified Codec.Compression.Zlib.Raw as Raw
import           Control.Monad              (forM, guard, replicateM)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy       as BL
import           Data.Char                  (isAlphaNum, isAscii, isSpace)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (sortOn)
import           Data.Maybe                 (fromMaybe, listToMaybe)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Data.Word
import           Neversoft.Checksum         (qbKeyCRC)
import           Numeric                    (readHex, showHex)

data Node = Node
  { nodeFileType       :: Word32
  , nodeOffset         :: Word32
  , nodeSize           :: Word32
  , nodeFilenamePakKey :: Word32
  , nodeFilenameKey    :: Word32
  , nodeFilenameCRC    :: Word32
  , nodeUnknown        :: Word32
  , nodeFlags          :: Word32
  } deriving (Show, Read)

-- Credit to unpak.pl by Tarragon Allen (tma)
decompressPak :: BL.ByteString -> BL.ByteString
decompressPak bs = let
  -- conveniently, start and next are relative to this CHNK, not the whole file
  [magic, start, len, next, _flags, _size] = runGet (replicateM 6 getWord32be) bs
  in if magic /= 0x43484e4b -- CHNK
    then BL.empty
    else let
      buf = BL.take (fromIntegral len) $ BL.drop (fromIntegral start) bs
      in Raw.decompress buf <> case next of
        0xffffffff -> BL.empty
        _          -> decompressPak $ BL.drop (fromIntegral next) bs

-- TODO handle runGet failure
splitPakNodes :: BL.ByteString -> [(Node, BL.ByteString)]
splitPakNodes bs = let
  bs' = if BL.take 4 bs == "CHNK"
    then decompressPak bs
    else bs
  end = qbKeyCRC "last"
  end2 = qbKeyCRC ".last"
  getNodes = do
    posn <- fromIntegral <$> bytesRead
    nodeFileType       <- getWord32be
    nodeOffset         <- (+ posn) <$> getWord32be
    nodeSize           <- getWord32be
    nodeFilenamePakKey <- getWord32be
    nodeFilenameKey    <- getWord32be
    nodeFilenameCRC    <- getWord32be
    nodeUnknown        <- getWord32be
    nodeFlags          <- getWord32be
    (Node{..} :) <$> if elem nodeFileType [end, end2]
      then return []
      else getNodes
  attachData node = let
    goToData
      = BL.take (fromIntegral $ nodeSize node)
      . BL.drop (fromIntegral $ nodeOffset node)
    in (node, goToData bs')
  in map attachData $ runGet getNodes bs'

buildPak :: [(Node, BL.ByteString)] -> BL.ByteString
buildPak nodes = let
  nodes' = sortOn (nodeOffset . fst) nodes -- sort according to original position, maybe not necessary
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
  header = runPut $ mapM_ putHeader $ zip [0..] $ fixNodes dataStart nodes'
  header' = BL.take (fromIntegral dataStart) $ header <> BL.replicate (fromIntegral dataStart) 0
  in BL.concat $ [header'] <> map (padData . snd) nodes' <> [BL.replicate 0xCF0 0xAB]

qsBank :: [(Node, BL.ByteString)] -> HM.HashMap Word32 T.Text
qsBank nodes = HM.fromList $ do
  (node, nodeData) <- nodes
  guard $ nodeFileType node == qbKeyCRC ".qs.en"
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
