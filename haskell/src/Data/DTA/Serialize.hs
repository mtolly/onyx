{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Data.DTA.Serialize where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Codec
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State      (evalStateT)
import           Data.DTA.Base
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import           JSONData

type ChunksCodec m a = ValueCodec m [Chunk T.Text] a
type ChunkCodec  m a = ValueCodec m (Chunk T.Text) a

single :: (Monad m) => ChunkCodec m a -> ChunksCodec m a
single cdc = Codec
  { codecOut = makeOut $ \x -> [makeValue' cdc x]
  , codecIn = lift ask >>= \case
    [x] -> inside "single chunk" $ parseFrom x $ codecIn cdc
    _   -> expected "a single chunk"
  }

unserialize :: (Monad m) => ChunksCodec m a -> DTA T.Text -> StackTraceT m a
unserialize cf (DTA _ (Tree _ cs)) = mapStackTraceT (`runReaderT` cs) (codecIn cf)

serialize :: (Monad m) => ChunksCodec m a -> a -> DTA T.Text
serialize cf = DTA 0 . Tree 0 . makeValue' cf

class StackChunks a where
  stackChunks :: (SendMessage m) => ChunksCodec m a
  default stackChunks :: (StackChunk a, SendMessage m) => ChunksCodec m a
  stackChunks = single stackChunk

class StackChunk a where
  stackChunk :: (SendMessage m) => ChunkCodec m a

chunksList :: (Monad m) => ChunkCodec m a -> ChunksCodec m [a]
chunksList cf = Codec
  { codecOut = makeOut $ map $ makeValue' cf
  , codecIn = do
    chunks <- lift ask
    forM (zip [0..] chunks) $ \(i, chunk) ->
      inside ("list element " ++ show (i :: Int)) $ parseFrom chunk $ codecIn cf
  }

instance (StackChunk a) => StackChunks [a] where
  stackChunks = chunksList stackChunk

chunksDTA :: (Monad m) => ChunksCodec m (DTA T.Text)
chunksDTA = Codec
  { codecOut  = makeOut $ treeChunks . topTree
  , codecIn = DTA 0 . Tree 0 <$> lift ask
  }

chunkInt :: (Monad m, Integral a) => ChunkCodec m a
chunkInt = Codec
  { codecOut = makeOut $ Int . fromIntegral
  , codecIn = lift ask >>= \case
    Int i -> return $ fromIntegral i
    _     -> expected "integer"
  }

instance StackChunk  Int     where stackChunk = chunkInt
instance StackChunks Int
instance StackChunk  Integer where stackChunk = chunkInt
instance StackChunks Integer

instance StackChunk Float where
  stackChunk = Codec
    { codecOut = makeOut Float
    , codecIn = lift ask >>= \case
      Float f -> return f
      Int   i -> return $ fromIntegral i
      _       -> expected "float"
    }
instance StackChunks Float

instance StackChunk Bool where
  stackChunk = Codec
    { codecOut = makeOut $ \b -> Int $ if b then 1 else 0
    , codecIn = lift ask >>= \case
      Int 1       -> return True
      Int 0       -> return False
      Key "TRUE"  -> return True
      Key "FALSE" -> return False
      Braces (Tree _ [Key "==", Var "SONG_VERSION", Int 0]) -> return False
      -- so we can parse (fake {== $SONG_VERSION 0})
      Braces (Tree _ [Key ">", Var "SONG_VERSION", Int 0]) -> return True
      -- so we can parse (downloaded {> $SONG_VERSION 0})
      _ -> expected "bool"
    }
instance StackChunks Bool

-- | Allows a string or key as input, and outputs to a string.
chunkString :: (Monad m) => ChunkCodec m T.Text
chunkString = Codec
  { codecOut = makeOut String
  , codecIn = lift ask >>= \case
    Key    s -> return s
    String s -> return s
    _        -> expected "string"
  }

-- | Allows a string or key as input, and outputs to a key.
chunkKey :: (Monad m) => ChunkCodec m T.Text
chunkKey = Codec
  { codecOut = makeOut Key
  , codecIn = lift ask >>= \case
    Key    s -> return s
    String s -> return s
    _     -> expected "keyword"
  }

instance StackChunk T.Text where
  stackChunk = chunkString
instance StackChunks T.Text

chunksMaybe :: (Monad m) => ChunksCodec m a -> ChunksCodec m (Maybe a)
chunksMaybe cf = Codec
  { codecOut = makeOut $ \case
    Nothing -> []
    Just x  -> makeValue' cf x
  , codecIn = lift ask >>= \case
    [] -> return Nothing
    _  -> Just <$> codecIn cf
  }

instance (StackChunks a) => StackChunks (Maybe a) where
  stackChunks = chunksMaybe stackChunks

newtype DictList k a = DictList { fromDictList :: [(k, a)] }
  deriving (Eq, Show, Read)

chunksDictList :: (Monad m, Eq k) => ChunkCodec m k -> ChunksCodec m a -> ChunksCodec m (DictList k a)
chunksDictList ck cv = Codec
  { codecOut = makeOut $ \mp ->
    [ Parens $ Tree 0 $ makeValue' ck k : makeValue' cv v | (k, v) <- fromDictList mp ]
  , codecIn = lift ask >>= \chunks -> fmap DictList $ forM chunks $ \case
    Parens (Tree _ (k : chunks')) -> do
      k' <- inside "parsing dict key" $ parseFrom k $ codecIn ck
      v' <- inside ("dict key " ++ show k) $ parseFrom chunks' $ codecIn cv
      return (k', v')
    _ -> expected "a key-value pair (parenthesized list starting with a key)"
  }

instance (Eq k, StackChunk k, StackChunks a) => StackChunks (DictList k a) where
  stackChunks = chunksDictList stackChunk stackChunks

chunksDict :: (Monad m, Eq k, Hashable k) => ChunkCodec m k -> ChunksCodec m a -> ChunksCodec m (Map.HashMap k a)
chunksDict ck cv = Codec
  { codecOut = fmapArg $ void . codecOut dl . DictList . Map.toList
  , codecIn = Map.fromList . fromDictList <$> codecIn dl
  } where dl = chunksDictList ck cv

instance (Eq k, Hashable k, StackChunk k, StackChunks a) => StackChunks (Map.HashMap k a) where
  stackChunks = chunksDict stackChunk stackChunks

chunksParens :: (Monad m) => ChunksCodec m a -> ChunksCodec m a
chunksParens = single . chunkParens

chunkParens :: (Monad m) => ChunksCodec m a -> ChunkCodec m a
chunkParens cf = Codec
  { codecOut = makeOut $ \x -> Parens $ Tree 0 $ makeValue' cf x
  , codecIn = lift ask >>= \case
    Parens (Tree _ chunks) -> parseFrom chunks $ codecIn cf
    _ -> expected "a set of parentheses"
  }

chunksPair :: (Monad m) =>  ChunkCodec m a -> ChunkCodec m b -> ChunksCodec m (a, b)
chunksPair xf yf = Codec
  { codecOut = makeOut $ \(x, y) -> [makeValue' xf x, makeValue' yf y]
  , codecIn = lift ask >>= \case
    [x, y] -> liftA2 (,)
      (inside "first item of a pair"  $ parseFrom x $ codecIn xf)
      (inside "second item of a pair" $ parseFrom y $ codecIn yf)
    _ -> expected "exactly 2 chunks"
  }

instance (StackChunk a, StackChunk b) => StackChunks (a, b) where
  stackChunks = chunksPair stackChunk stackChunk

instance (StackChunks a, StackChunks b) => StackChunks (Either a b) where
  stackChunks = eitherCodec stackChunks stackChunks

dtaEnum :: (Monad m, Enum a, Bounded a) => String -> (a -> Chunk T.Text) -> ChunkCodec m a
dtaEnum err f = let
  kv = Map.fromList [ (f x, x) | x <- [minBound .. maxBound] ]
  in Codec
    { codecOut  = makeOut f
    , codecIn = lift ask >>= \v -> case Map.lookup v kv of
      Nothing -> expected $ err ++ " enumeration value"
      Just x  -> return x
    }

asAssoc :: (Monad m) => T.Text -> ObjectCodec m [Chunk T.Text] a -> ChunksCodec m a
asAssoc err codec = Codec
  { codecIn = inside ("parsing " ++ T.unpack err) $ do
    obj <- codecIn cdc
    let f = withReaderT (const $ Map.fromList $ fromDictList obj) . mapReaderT (`evalStateT` Set.empty)
    mapStackTraceT f $ codecIn codec
  , codecOut = fmapArg $ void . codecOut cdc . DictList . makeObject codec
  } where cdc = chunksDictList chunkKey identityCodec

asStrictAssoc :: (Monad m) => T.Text -> ObjectCodec m [Chunk T.Text] a -> ChunksCodec m a
asStrictAssoc err codec = asAssoc err Codec
  { codecOut = codecOut codec
  , codecIn = codecIn codec <* strictKeys
  }
