{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
module Onyx.Harmonix.DTA.Base
( DTA(..), Tree(..), Chunk(..)
, renumberFrom
) where

import           Control.Applicative       (liftA2)
import           Control.Monad             (replicateM)
import qualified Data.ByteString           as B
import           Data.Int                  (Int32)
import           Data.Word                 (Word32, Word8)

import qualified Control.Monad.Trans.State as S
import           Data.Binary               (Binary (..), Get, Put)
import           Data.Binary.Get           (getByteString, getFloatle,
                                            getWord16le, getWord32le, skip)
import           Data.Binary.Put           (putByteString, putFloatle,
                                            putWord16le, putWord32le)
import           Data.Hashable             (Hashable (..))
import           GHC.Generics              (Generic (..))

--
-- Type definitions
--

-- | A top-level file.
data DTA s = DTA { byteZero :: Word8, topTree :: Tree s }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable)

-- | A list of chunks, for either the top-level tree or a subtree.
data Tree s = Tree { nodeID :: Word32, treeChunks :: [Chunk s] }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable)

-- | A data value, which may be a subtree. The constructors are ordered by their
-- chunk identification tag in the binary format.
data Chunk s
  = Int Int32
  | Float Float
  | Var s
  | Sym s
  | Unhandled
  | IfDef s
  | Else
  | EndIf
  | Parens (Tree s)
  | Braces (Tree s)
  | String s
  | Brackets (Tree s)
  | Define s
  | Include s
  | Merge s
  | IfNDef s
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic, Hashable)

--
-- Binary (DTB) instances
--

-- Single byte, then a tree.
instance Binary (DTA B.ByteString) where
  put (DTA b t) = put b >> put t
  get = liftA2 DTA get get

-- 2-byte length, 4-byte node ID, then each element in sequence.
instance Binary (Tree B.ByteString) where
  put (Tree nid chks) = do
    putWord16le $ fromIntegral $ length chks
    putWord32le nid
    mapM_ put chks
  get = do
    len <- getWord16le
    liftA2 Tree getWord32le $ replicateM (fromIntegral len) get

-- 4-byte chunk type ID, then at least 4 bytes of chunk data.
instance Binary (Chunk B.ByteString) where
  put c = case c of
    Int i       -> putWord32le 0x0  >> putWord32le (fromIntegral i)
    Float f     -> putWord32le 0x1  >> putFloatle f
    Var b       -> putWord32le 0x2  >> putLenStr b
    Sym b       -> putWord32le 0x5  >> putLenStr b
    Unhandled   -> putWord32le 0x6  >> putWord32le 0
    IfDef b     -> putWord32le 0x7  >> putLenStr b
    Else        -> putWord32le 0x8  >> putWord32le 0
    EndIf       -> putWord32le 0x9  >> putWord32le 0
    Parens tr   -> putWord32le 0x10 >> put tr
    Braces tr   -> putWord32le 0x11 >> put tr
    String b    -> putWord32le 0x12 >> putLenStr b
    Brackets tr -> putWord32le 0x13 >> put tr
    Define b    -> putWord32le 0x20 >> putLenStr b
    Include b   -> putWord32le 0x21 >> putLenStr b
    Merge b     -> putWord32le 0x22 >> putLenStr b
    IfNDef b    -> putWord32le 0x23 >> putLenStr b
  get = getWord32le >>= \cid -> case cid of
    0x0  -> Int . fromIntegral <$> getWord32le
    0x1  -> Float <$> getFloatle
    0x2  -> Var <$> getLenStr
    0x5  -> Sym <$> getLenStr
    0x6  -> skip 4 >> return Unhandled
    0x7  -> IfDef <$> getLenStr
    0x8  -> skip 4 >> return Else
    0x9  -> skip 4 >> return EndIf
    0x10 -> Parens <$> get
    0x11 -> Braces <$> get
    0x12 -> String <$> getLenStr
    0x13 -> Brackets <$> get
    0x20 -> Define <$> getLenStr
    0x21 -> Include <$> getLenStr
    0x22 -> Merge <$> getLenStr
    0x23 -> IfNDef <$> getLenStr
    -- TODO: 0x24 seen in Beatles config/gen/process_clips_func.dtb
    _    -> fail $ "Unidentified DTB chunk with ID " ++ show cid

-- | DTB string format: 4-byte length, then a string in latin-1.
putLenStr :: B.ByteString -> Put
putLenStr b = putWord32le (fromIntegral $ B.length b) >> putByteString b

-- | DTB string format: 4-byte length, then a string in latin-1.
getLenStr :: Get B.ByteString
getLenStr = getWord32le >>= getByteString . fromIntegral

-- | Assign new sequential node IDs to each tree in a DTA, starting with the
-- top-level tree.
renumberFrom :: Word32 -> DTA s -> DTA s
renumberFrom w (DTA b t) = DTA b $ S.evalState (renumberTree t) w where
  renumberTree :: Tree s -> S.State Word32 (Tree s)
  renumberTree (Tree _ sub) = liftA2 Tree S.get $
    S.modify (+ 1) >> mapM renumberChunk sub
  renumberChunk :: Chunk s -> S.State Word32 (Chunk s)
  renumberChunk c = case c of
    Parens tr   -> Parens <$> renumberTree tr
    Braces tr   -> Braces <$> renumberTree tr
    Brackets tr -> Brackets <$> renumberTree tr
    _           -> return c
  -- alternately, with uniplate: renumberChunk = descendBiM renumberTree
