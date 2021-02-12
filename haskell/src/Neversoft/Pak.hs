{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Neversoft.Pak where

import           Control.Monad        (guard)
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.Char            (isSpace)
import qualified Data.Map             as Map
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
  } deriving (Show)

splitPakNodes :: BL.ByteString -> [(Node, BL.ByteString)]
splitPakNodes bs = let
  end = qbKeyCRC "last"
  end2 = qbKeyCRC ".last"
  getNodes = do
    posn <- fromIntegral <$> bytesRead
    nodeFileType <- getWord32be
    if elem nodeFileType [end, end2]
      then return []
      else do
        nodeOffset         <- (+ posn) <$> getWord32be
        nodeSize           <- getWord32be
        nodeFilenamePakKey <- getWord32be
        nodeFilenameKey    <- getWord32be
        nodeFilenameCRC    <- getWord32be
        nodeUnknown        <- getWord32be
        nodeFlags          <- getWord32be
        (Node{..} :) <$> getNodes
  attachData node = let
    goToData
      = BL.take (fromIntegral $ nodeSize node)
      . BL.drop (fromIntegral $ nodeOffset node)
    in (node, goToData bs)
  in map attachData $ runGet getNodes bs

qsBank :: [(Node, BL.ByteString)] -> Map.Map Word32 T.Text
qsBank nodes = Map.fromList $ do
  (node, nodeData) <- nodes
  guard $ nodeFileType node == qbKeyCRC ".qs.en"
  -- first 2 chars should be "\xFF\xFE"
  line <- T.lines $ TE.decodeUtf16LE $ BL.toStrict $ BL.drop 2 nodeData
  guard $ T.length line > 8
  (x, s) <- readHex $ T.unpack $ T.take 8 line
  guard $ all isSpace s
  return (x, T.drop 9 line) -- TODO strip quotes and weird escapes
