-- gen/*.txt.bin files (and other *.bin)
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Amplitude.PS2.TxtBin where

import           Control.Monad   (forM, replicateM)
import           Data.Binary.Get
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.DTA        as D
import           Data.Int
import           Data.Word

data TxtBin = TxtBin
  { tb_sources :: [B.ByteString]
  , tb_node    :: Node
  } deriving (Show)

data Node = Node Word32 Word32 [NodeContents]
  deriving (Show)

data NodeContents
  = I Int32
  | S B.ByteString
  | F Float
  | N Node
  deriving (Show)

getLenStr :: Get B.ByteString
getLenStr = do
  len <- getWord32le
  getByteString $ fromIntegral len

getTxtBin :: Get TxtBin
getTxtBin = do
  2 <- getWord8
  tb_sources <- getWord32le >>= \n -> replicateM (fromIntegral n) getLenStr
  tb_node <- getNode
  return TxtBin{..}

getNode :: Get Node
getNode = do
  len <- fromIntegral <$> getWord16le
  id1 <- getWord32le
  id2 <- getWord32le
  let numTypeWords = case quotRem len 16 of
        (n, 0) -> n
        (n, _) -> n + 1
  typeWords <- replicateM numTypeWords getWord32le
  contents <- forM [0 .. len - 1] $ \i -> do
    let typeTag = case quotRem i 16 of
          (x, y) -> ((typeWords !! x) `shiftR` (y * 2)) .&. 0x3
    case typeTag of
      0 -> I <$> getInt32le
      1 -> S <$> getLenStr
      2 -> F <$> getFloatle
      3 -> N <$> getNode
      _ -> fail "shouldn't happen"
  return $ Node id1 id2 contents

txtBinToDTA :: TxtBin -> D.DTA B.ByteString
txtBinToDTA tb = D.DTA 0 $ D.Tree 0
  [ D.Parens $ D.Tree 0 $ D.Sym "sources" : map D.String (tb_sources tb)
  , D.Parens $ D.Tree 0 $ encodeNode $ tb_node tb
  ] where
    encodeNode (Node _ _ vals) = flip map vals $ \case
      I int  -> D.Int int
      S str  -> D.Sym str
      F flt  -> D.Float flt
      N node -> D.Parens $ D.Tree 0 $ encodeNode node
