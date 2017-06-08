{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
module Data.DTA.Serialize2 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import           Data.DTA.Base
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as Map
import qualified Data.Text                      as T
import           JSONData                       (StackCodec (..), StackParser,
                                                 eitherCodec, identityCodec,
                                                 parseFrom)
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax     as TH

expected :: (Monad m, Show context) => String -> StackParser m context a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ show v

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
      Int i -> return $ fromIntegral i
      _ -> expected "float"
    }
instance StackChunks Float

instance StackChunk Bool where
  stackChunk = StackCodec
    { stackShow = \b -> Int $ if b then 1 else 0
    , stackParse = lift ask >>= \case
      Int 1 -> return True
      Int 0 -> return False
      Key "TRUE" -> return True
      Key "FALSE" -> return False
      _ -> expected "bool"
    }
instance StackChunks Bool

chunkString :: ChunkCodec T.Text
chunkString = StackCodec
  { stackShow = \s -> String s
  , stackParse = lift ask >>= \case
    String s -> return s
    _ -> expected "string"
  }

chunkKey :: ChunkCodec T.Text
chunkKey = StackCodec
  { stackShow = \s -> Key s
  , stackParse = lift ask >>= \case
    Key s -> return s
    _ -> expected "keyword"
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

data DTAField = DTAField
  { hsField      :: String
  , dtaKey       :: String
  , fieldType    :: TypeQ
  , defaultValue :: Maybe ExpQ
  , warnMissing  :: Bool
  , writeDefault :: Bool
  , fieldFormat  :: ExpQ
  }

req :: String -> String -> TypeQ -> ExpQ -> Writer [DTAField] ()
req hs dta t fmt = tell [DTAField hs dta t Nothing False False fmt]

warning, fill, opt :: String -> String -> TypeQ -> ExpQ -> ExpQ -> Writer [DTAField] ()
warning hs dta t dft fmt = tell [DTAField hs dta t (Just dft) True  False fmt]
fill    hs dta t dft fmt = tell [DTAField hs dta t (Just dft) False True  fmt]
opt     hs dta t dft fmt = tell [DTAField hs dta t (Just dft) False False fmt]

esr :: CxtQ
esr = cxt [[t| Eq |], [t| Show |], [t| Read |]]

eosr :: CxtQ
eosr = cxt [[t| Eq |], [t| Ord |], [t| Show |], [t| Read |]]

eosreb :: CxtQ
eosreb = cxt [[t| Eq |], [t| Ord |], [t| Show |], [t| Read |], [t| Enum |], [t| Bounded |]]

dtaRecord :: String -> CxtQ -> Writer [DTAField] () -> DecsQ
dtaRecord rec derivs writ = do
  let fields = execWriter writ
      apply x y = [e| $x <*> $y |]
  dataDecl <- dataD (cxt []) (mkName rec) [] Nothing
    [ recC (mkName rec) $ flip map fields $ \field ->
      varBangType (mkName $ hsField field)
      $ bangType (bang sourceNoUnpack noSourceStrictness) (fieldType field)
    ]
    derivs
  recName <- newName "rec"
  formatDecls <- [d|
    instance StackChunks $(conT (mkName rec)) where
      stackChunks = StackCodec
        { stackShow = \($(varP recName)) -> stackShow (chunksDict chunkString $ chunksList identityCodec) $ Map.fromList $ concat $(
            listE $ flip map fields $ \field -> [e|
              let v = $(varE (mkName (hsField field))) $(varE recName)
              in $(
                case defaultValue field of
                  Just dft | not $ writeDefault field -> [e|
                    if v == $dft
                      then []
                      else [(T.pack $(TH.lift $ dtaKey field), stackShow $(fieldFormat field) v)]
                    |]
                  _ -> [e| [(T.pack $(TH.lift $ dtaKey field), stackShow $(fieldFormat field) v)] |]
              )
            |]
          )
        , stackParse = do
          mapping <- stackParse (chunksDict chunkString $ chunksList identityCodec)
          $( foldl apply [e| pure $(conE (mkName rec)) |] $ flip map fields $ \field ->
            let key = "key " ++ show (dtaKey field)
            in case defaultValue field of
              Just dft -> [e|
                case Map.lookup (T.pack $(TH.lift (dtaKey field))) mapping of
                  Nothing -> do
                    when $(TH.lift (warnMissing field)) (warn $ "missing " ++ $(TH.lift key))
                    return $dft
                  Just x -> inside $(TH.lift key) $ parseFrom x $ stackParse $(fieldFormat field)
                |]
              Nothing -> [e|
                case Map.lookup (T.pack $(TH.lift (dtaKey field))) mapping of
                  Nothing -> fatal $ "missing " ++ $(TH.lift key)
                  Just x -> inside $(TH.lift key) $ parseFrom x $ stackParse $(fieldFormat field)
                |]
            )
        }
    |]
  return $ dataDecl : formatDecls

dtaEnum :: (Enum a, Bounded a) => String -> (a -> Chunk T.Text) -> ChunkCodec a
dtaEnum err f = let
  mapping = Map.fromList [ (f x, x) | x <- [minBound .. maxBound] ]
  in StackCodec
    { stackShow  = f
    , stackParse = lift ask >>= \v -> case Map.lookup v mapping of
      Nothing -> expected $ err ++ " enumeration value"
      Just x  -> return x
    }
