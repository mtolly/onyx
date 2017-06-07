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
import qualified Data.Map                       as Map
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

class DTASerialize a where
  format :: ChunksCodec a

chunksDTA :: ChunksCodec (DTA T.Text)
chunksDTA = StackCodec
  { stackShow  = treeChunks . topTree
  , stackParse = DTA 0 . Tree 0 <$> lift ask
  }

chunksChunk :: ChunksCodec (Chunk T.Text)
chunksChunk = single identityCodec

chunkInt :: (Integral a) => ChunkCodec a
chunkInt = StackCodec
  { stackShow = Int . fromIntegral
  , stackParse = lift ask >>= \case
    Int i -> return $ fromIntegral i
    _     -> expected "integer"
  }

instance DTASerialize Int     where format = single chunkInt
instance DTASerialize Integer where format = single chunkInt

instance DTASerialize Float where
  format = StackCodec
    { stackShow = \f -> [Float f]
    , stackParse = lift ask >>= \case
      [Float f] -> return f
      [Int i] -> return $ fromIntegral i
      _ -> expected "float"
    }

instance DTASerialize Bool where
  format = StackCodec
    { stackShow = \b -> [Int $ if b then 1 else 0]
    , stackParse = lift ask >>= \case
      [Int 1] -> return True
      [Int 0] -> return False
      [Key "TRUE"] -> return True
      [Key "FALSE"] -> return False
      _ -> expected "bool"
    }

chunksString :: ChunksCodec T.Text
chunksString = StackCodec
  { stackShow = \s -> [String s]
  , stackParse = lift ask >>= \case
    [String s] -> return s
    _ -> expected "string"
  }

chunksKey :: ChunksCodec T.Text
chunksKey = StackCodec
  { stackShow = \s -> [Key s]
  , stackParse = lift ask >>= \case
    [Key s] -> return s
    _ -> expected "keyword"
  }

chunksList :: ChunksCodec a -> ChunksCodec [a]
chunksList cf = StackCodec
  { stackShow = concatMap $ stackShow cf
  , stackParse = do
    chunks <- lift ask
    forM (zip [0..] chunks) $ \(i, chunk) ->
      inside ("list element " ++ show (i :: Int)) $ parseFrom [chunk] $ stackParse cf
  }

instance (DTASerialize a) => DTASerialize [a] where
  format = chunksList format

chunksMaybe :: ChunksCodec a -> ChunksCodec (Maybe a)
chunksMaybe cf = StackCodec
  { stackShow = \case
    Nothing -> []
    Just x  -> stackShow cf x
  , stackParse = lift ask >>= \case
    [] -> return Nothing
    _  -> Just <$> stackParse cf
  }

instance (DTASerialize a) => DTASerialize (Maybe a) where
  format = chunksMaybe format

-- | A key-value structure which is stored as a sequence of @(tag rest...)@
-- chunks.
newtype Dict a = Dict { fromDict :: Map.Map T.Text a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

chunksDict :: ChunksCodec a -> ChunksCodec (Dict a)
chunksDict cf = StackCodec
  { stackShow = \mp ->
    [ Parens $ Tree 0 $ Key k : stackShow cf v | (k, v) <- Map.toList $ fromDict mp ]
  , stackParse = lift ask >>= \chunks -> fmap (Dict . Map.fromList) $ forM chunks $ \case
    Parens (Tree _ (Key k : chunks')) -> inside ("dict key " ++ show k) $
      (\v -> (k, v)) <$> parseFrom chunks' (stackParse cf)
    _ -> expected "a key-value pair (parenthesized list starting with a keyword)"
  }

instance (DTASerialize a) => DTASerialize (Dict a) where
  format = chunksDict format

chunksParens :: ChunksCodec a -> ChunksCodec a
chunksParens cf = StackCodec
  { stackShow = \x -> [Parens $ Tree 0 $ stackShow cf x]
  , stackParse = lift ask >>= \case
    [Parens (Tree _ chunks)] -> parseFrom chunks $ stackParse cf
    _ -> expected "a set of parentheses"
  }

chunksPair :: ChunksCodec a -> ChunksCodec b -> ChunksCodec (a, b)
chunksPair xf yf = StackCodec
  { stackShow = \(x, y) -> stackShow xf x ++ stackShow yf y
  , stackParse = lift ask >>= \case
    [x, y] -> liftA2 (,)
      (inside "first item of a pair" $ parseFrom [x] $ stackParse xf)
      (inside "second item of a pair" $ parseFrom [y] $ stackParse yf)
    _ -> expected "exactly 2 chunks"
  }

instance (DTASerialize a, DTASerialize b) => DTASerialize (a, b) where
  format = chunksPair format format

instance (DTASerialize a, DTASerialize b) => DTASerialize (Either a b) where
  format = eitherCodec format format

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
    instance DTASerialize $(conT (mkName rec)) where
      format = StackCodec
        { stackShow = \($(varP recName)) -> stackShow (chunksDict $ chunksList chunksChunk) $ Dict $ Map.fromList $ concat $(
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
          Dict mapping <- stackParse (chunksDict $ chunksList chunksChunk)
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

data DTAValue = DTAValue
  { hsCon    :: String
  , dtaValue :: ExpQ
  }

val :: String -> ExpQ -> Writer [DTAValue] ()
val hs dta = tell [DTAValue hs dta]

dtaEnum :: String -> CxtQ -> Writer [DTAValue] () -> DecsQ
dtaEnum typ derivs writ = do
  let vals = execWriter writ
  dataDecl <- dataD (cxt []) (mkName typ) [] Nothing
    [ normalC (mkName $ hsCon v) []
    | v <- vals
    ]
    derivs
  formatDecls <- [d|
    instance DTASerialize $(conT (mkName typ)) where
      format = StackCodec
        { stackShow = $(lamCaseE $ do
          v <- vals
          return $ match (conP (mkName (hsCon v)) []) (normalB (dtaValue v)) []
          )
        , stackParse = do
          x <- lift ask
          let mapping = $(listE $ do
                v <- vals
                return $ tupE [dtaValue v, conE $ mkName $ hsCon v]
                )
          case lookup x mapping of
            Nothing -> fatal $ "unhandled enum value: " ++ show x
            Just v  -> return v
        }
    |]
  return $ dataDecl : formatDecls
