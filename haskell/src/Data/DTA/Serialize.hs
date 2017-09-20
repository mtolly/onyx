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
import           Data.Functor.Identity          (Identity)
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import qualified Data.Text                      as T
import           JSONData

type ChunksCodec = StackCodec [Chunk T.Text]
type ChunkCodec  = StackCodec (Chunk T.Text)

single :: ChunkCodec a -> ChunksCodec a
single cdc = StackCodec
  { stackShow = \x -> [stackShow cdc x]
  , stackParse = lift ask >>= \case
    [x] -> inside "single chunk" $ parseFrom x $ stackParse cdc
    _   -> expected "a single chunk"
  }

unserialize :: (Monad m) => ChunksCodec a -> DTA T.Text -> StackTraceT m a
unserialize cf (DTA _ (Tree _ cs)) = mapStackTraceT (`runReaderT` cs) (stackParse cf)

serialize :: ChunksCodec a -> a -> DTA T.Text
serialize cf = DTA 0 . Tree 0 . stackShow cf

class StackChunks a where
  stackChunks :: ChunksCodec a
  default stackChunks :: (StackChunk a) => ChunksCodec a
  stackChunks = single stackChunk

class StackChunk a where
  stackChunk :: ChunkCodec a

chunksList :: ChunkCodec a -> ChunksCodec [a]
chunksList cf = StackCodec
  { stackShow = map $ stackShow cf
  , stackParse = do
    chunks <- lift ask
    forM (zip [0..] chunks) $ \(i, chunk) ->
      inside ("list element " ++ show (i :: Int)) $ parseFrom chunk $ stackParse cf
  }

instance (StackChunk a) => StackChunks [a] where
  stackChunks = chunksList stackChunk

chunksDTA :: ChunksCodec (DTA T.Text)
chunksDTA = StackCodec
  { stackShow  = treeChunks . topTree
  , stackParse = DTA 0 . Tree 0 <$> lift ask
  }

chunkInt :: (Integral a) => ChunkCodec a
chunkInt = StackCodec
  { stackShow = Int . fromIntegral
  , stackParse = lift ask >>= \case
    Int i -> return $ fromIntegral i
    _     -> expected "integer"
  }

instance StackChunk  Int     where stackChunk = chunkInt
instance StackChunks Int
instance StackChunk  Integer where stackChunk = chunkInt
instance StackChunks Integer

instance StackChunk Float where
  stackChunk = StackCodec
    { stackShow = Float
    , stackParse = lift ask >>= \case
      Float f -> return f
      Int   i -> return $ fromIntegral i
      _       -> expected "float"
    }
instance StackChunks Float

instance StackChunk Bool where
  stackChunk = StackCodec
    { stackShow = \b -> Int $ if b then 1 else 0
    , stackParse = lift ask >>= \case
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

chunkString :: ChunkCodec T.Text
chunkString = StackCodec
  { stackShow = String
  , stackParse = lift ask >>= \case
    String s -> return s
    _        -> expected "string"
  }

chunkKey :: ChunkCodec T.Text
chunkKey = StackCodec
  { stackShow = Key
  , stackParse = lift ask >>= \case
    Key s -> return s
    _     -> expected "keyword"
  }

chunkStringOrKey :: ChunkCodec T.Text
chunkStringOrKey = StackCodec
  { stackShow  = stackShow  chunkString
  , stackParse = stackParse chunkString <|> stackParse chunkKey <|> expected "a string or keyword"
  }

instance StackChunk T.Text where
  stackChunk = chunkStringOrKey
instance StackChunks T.Text

chunksMaybe :: ChunksCodec a -> ChunksCodec (Maybe a)
chunksMaybe cf = StackCodec
  { stackShow = \case
    Nothing -> []
    Just x  -> stackShow cf x
  , stackParse = lift ask >>= \case
    [] -> return Nothing
    _  -> Just <$> stackParse cf
  }

instance (StackChunks a) => StackChunks (Maybe a) where
  stackChunks = chunksMaybe stackChunks

chunksDict :: (Eq k, Hashable k) => ChunkCodec k -> ChunksCodec a -> ChunksCodec (Map.HashMap k a)
chunksDict ck cv = StackCodec
  { stackShow = \mp ->
    [ Parens $ Tree 0 $ stackShow ck k : stackShow cv v | (k, v) <- Map.toList mp ]
  , stackParse = lift ask >>= \chunks -> fmap Map.fromList $ forM chunks $ \case
    Parens (Tree _ (k : chunks')) -> do
      k' <- inside "parsing dict key" $ parseFrom k $ stackParse ck
      v' <- inside ("dict key " ++ show k) $ parseFrom chunks' $ stackParse cv
      return (k', v')
    _ -> expected "a key-value pair (parenthesized list starting with a key)"
  }

instance (Eq k, Hashable k, StackChunk k, StackChunks a) => StackChunks (Map.HashMap k a) where
  stackChunks = chunksDict stackChunk stackChunks

chunksParens :: ChunksCodec a -> ChunksCodec a
chunksParens cf = StackCodec
  { stackShow = \x -> [Parens $ Tree 0 $ stackShow cf x]
  , stackParse = lift ask >>= \case
    [Parens (Tree _ chunks)] -> parseFrom chunks $ stackParse cf
    _ -> expected "a set of parentheses"
  }

chunksPair :: ChunkCodec a -> ChunkCodec b -> ChunksCodec (a, b)
chunksPair xf yf = StackCodec
  { stackShow = \(x, y) -> [stackShow xf x, stackShow yf y]
  , stackParse = lift ask >>= \case
    [x, y] -> liftA2 (,)
      (inside "first item of a pair"  $ parseFrom x $ stackParse xf)
      (inside "second item of a pair" $ parseFrom y $ stackParse yf)
    _ -> expected "exactly 2 chunks"
  }

instance (StackChunk a, StackChunk b) => StackChunks (a, b) where
  stackChunks = chunksPair stackChunk stackChunk

instance (StackChunks a, StackChunks b) => StackChunks (Either a b) where
  stackChunks = eitherCodec stackChunks stackChunks

dtaEnum :: (Enum a, Bounded a) => String -> (a -> Chunk T.Text) -> ChunkCodec a
dtaEnum err f = let
  kv = Map.fromList [ (f x, x) | x <- [minBound .. maxBound] ]
  in StackCodec
    { stackShow  = f
    , stackParse = lift ask >>= \v -> case Map.lookup v kv of
      Nothing -> expected $ err ++ " enumeration value"
      Just x  -> return x
    }

asAssoc :: T.Text -> (forall m. (Monad m) => ObjectCodec m [Chunk T.Text] a) -> ChunksCodec a
asAssoc err codec = StackCodec
  { stackParse = inside ("parsing " ++ T.unpack err) $ do
    obj <- stackParse cdc
    let f = withReaderT (const obj) . mapReaderT (`evalStateT` Set.empty)
    mapStackTraceT f $ codecIn codec
  , stackShow = stackShow cdc . makeObject codec
  } where cdc = chunksDict chunkKey identityCodec

asStrictAssoc :: T.Text -> (forall m. (Monad m) => ObjectCodec m [Chunk T.Text] a) -> ChunksCodec a
asStrictAssoc err codec = asAssoc err Codec
  { codecOut = codecOut ((id :: ObjectCodec Identity v a -> ObjectCodec Identity v a) codec)
  , codecIn = codecIn codec <* strictKeys
  }
