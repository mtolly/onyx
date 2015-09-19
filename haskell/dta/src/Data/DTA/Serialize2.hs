{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Data.DTA.Serialize where

import Data.DTA.Base
import Control.Monad.Trans.StackTrace
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad
import Control.Applicative
import qualified Data.Map as Map

type Parser m context = StackTraceT (ReaderT context m)

expected :: (Monad m, Show context) => String -> Parser m context a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ show v

data ChunkFormat a = ChunkFormat
  { toChunks   :: a -> [Chunk String]
  , fromChunks :: forall m. (Monad m) => Parser m [Chunk String] a
  }

unserialize :: (Monad m) => ChunkFormat a -> DTA String -> StackTraceT m a
unserialize cf (DTA _ (Tree _ cs)) = mapStackTraceT (`runReaderT` cs) (fromChunks cf)

serialize :: ChunkFormat a -> a -> DTA String
serialize cf = DTA 0 . Tree 0 . toChunks cf

chunksDTA :: ChunkFormat (DTA String)
chunksDTA = ChunkFormat
  { toChunks   = treeChunks . topTree
  , fromChunks = DTA 0 . Tree 0 <$> lift ask
  }

chunksChunk :: ChunkFormat (Chunk String)
chunksChunk = ChunkFormat
  { toChunks = \x -> [x]
  , fromChunks = lift ask >>= \case
    [x] -> return x
    _ -> expected "exactly 1 chunk"
  }

chunksInt :: (Integral a) => ChunkFormat a
chunksInt = ChunkFormat
  { toChunks = \i -> [Int $ fromIntegral i]
  , fromChunks = lift ask >>= \case
    [Int i] -> return $ fromIntegral i
    _ -> expected "integer"
  }

chunksFloat :: ChunkFormat Float
chunksFloat = ChunkFormat
  { toChunks = \f -> [Float f]
  , fromChunks = lift ask >>= \case
    [Float f] -> return f
    _ -> expected "float"
  }

chunksBool :: ChunkFormat Bool
chunksBool = ChunkFormat
  { toChunks = \b -> [Int $ if b then 1 else 0]
  , fromChunks = lift ask >>= \case
    [Int 1] -> return True
    [Int 0] -> return False
    [Key "TRUE"] -> return True
    [Key "FALSE"] -> return False
    _ -> expected "bool"
  }

chunksString :: ChunkFormat String
chunksString = ChunkFormat
  { toChunks = \s -> [String s]
  , fromChunks = lift ask >>= \case
    [String s] -> return s
    _ -> expected "string"
  }

chunksKey :: ChunkFormat String
chunksKey = ChunkFormat
  { toChunks = \s -> [Key s]
  , fromChunks = lift ask >>= \case
    [Key s] -> return s
    _ -> expected "keyword"
  }

readFrom :: (Monad m) => c -> Parser m c a -> Parser m d a
readFrom ctxt = mapStackTraceT $ withReaderT $ const ctxt

chunksList :: ChunkFormat a -> ChunkFormat [a]
chunksList cf = ChunkFormat
  { toChunks = concatMap $ toChunks cf
  , fromChunks = do
    chunks <- lift ask
    forM chunks $ \chunk -> readFrom [chunk] $ fromChunks cf
  }

chunksMaybe :: ChunkFormat a -> ChunkFormat (Maybe a)
chunksMaybe cf = ChunkFormat
  { toChunks = \case
    Nothing -> []
    Just x  -> toChunks cf x
  , fromChunks = undefined
  }

chunksDict :: ChunkFormat a -> ChunkFormat (Map.Map String a)
chunksDict cf = ChunkFormat
  { toChunks = \mp ->
    [ Parens $ Tree 0 $ Key k : toChunks cf v | (k, v) <- Map.toList mp ]
  , fromChunks = lift ask >>= \chunks -> fmap Map.fromList $ forM chunks $ \case
    Parens (Tree _ (Key k : chunks')) -> (\v -> (k, v)) <$> readFrom chunks' (fromChunks cf)
    _ -> expected "a parenthesized list starting with a keyword"
  }

chunksParens :: ChunkFormat a -> ChunkFormat a
chunksParens cf = ChunkFormat
  { toChunks = \x -> [Parens $ Tree 0 $ toChunks cf x]
  , fromChunks = lift ask >>= \case
    [Parens (Tree _ chunks)] -> readFrom chunks $ fromChunks cf
    _ -> expected "a parenthesized list"
  }

chunksPair :: ChunkFormat a -> ChunkFormat b -> ChunkFormat (a, b)
chunksPair xf yf = ChunkFormat
  { toChunks = \(x, y) -> toChunks xf x ++ toChunks yf y
  , fromChunks = lift ask >>= \case
    [x, y] -> liftA2 (,) (readFrom [x] $ fromChunks xf) (readFrom [y] $ fromChunks yf)
    _ -> expected "exactly 2 chunks"
  }

chunksEither :: ChunkFormat a -> ChunkFormat b -> ChunkFormat (Either a b)
chunksEither xf yf = ChunkFormat
  { toChunks = \case
    Left  x -> toChunks xf x
    Right y -> toChunks yf y
  , fromChunks = fmap Left (fromChunks xf) <|> fmap Right (fromChunks yf)
  }
