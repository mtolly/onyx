{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Pak where

import           Control.Monad        (guard)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (isSpace)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Word
import           Neversoft.Checksum   (qbKeyCRC)
import           Numeric              (readHex)

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
  header = runPut $ mapM_ putHeader $ zip [0..] $ fixNodes dataStart nodes
  header' = BL.take (fromIntegral dataStart) $ header <> BL.replicate (fromIntegral dataStart) 0
  in BL.concat $ [header'] <> map (padData . snd) nodes <> [BL.replicate 0xCF0 0xAB]

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
