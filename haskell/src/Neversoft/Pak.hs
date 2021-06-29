{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Pak where

import           Control.Monad        (forM, guard)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (isSpace)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (sortOn)
import           Data.Maybe           (listToMaybe)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word
import           Neversoft.Checksum   (qbKeyCRC)
import           Numeric              (readHex, showHex)

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

splitPakNodes :: BL.ByteString -> [(Node, BL.ByteString)]
splitPakNodes bs = let
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
    in (node, goToData bs)
  in map attachData $ runGet getNodes bs

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
  -- first 2 chars should be "\xFF\xFE"
  line <- T.lines $ TE.decodeUtf16LE $ BL.toStrict $ BL.drop 2 nodeData
  guard $ T.length line > 8
  (x, s) <- readHex $ T.unpack $ T.take 8 line
  guard $ all isSpace s
  return (x, T.drop 9 line) -- TODO strip quotes and weird escapes

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
    (w, t) <- sortOn snd entries -- Need to sort, otherwise game crashes!
    let hex = T.pack $ showHex w ""
        hex' = T.replicate (8 - T.length hex) "0" <> hex
    return $ hex' <> " \"" <> t <> "\""
  in "\xFEFF" <> lns <> "\n\n" -- Extra newlines probably not necessary
