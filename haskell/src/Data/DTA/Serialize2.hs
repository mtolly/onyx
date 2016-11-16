{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE OverloadedStrings        #-}
module Data.DTA.Serialize2 where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Data.DTA.Base
import qualified Data.Map                       as Map
import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax     as TH
import Control.Monad.Trans.Writer
import qualified Data.Text as T

type Parser m context = StackTraceT (ReaderT context m)

expected :: (Monad m, Show context) => String -> Parser m context a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ show v

data ChunkFormat a = ChunkFormat
  { toChunks   :: a -> [Chunk T.Text]
  , fromChunks :: forall m. (Monad m) => Parser m [Chunk T.Text] a
  }

unserialize :: (Monad m) => ChunkFormat a -> DTA T.Text -> StackTraceT m a
unserialize cf (DTA _ (Tree _ cs)) = mapStackTraceT (`runReaderT` cs) (fromChunks cf)

serialize :: ChunkFormat a -> a -> DTA T.Text
serialize cf = DTA 0 . Tree 0 . toChunks cf

class DTASerialize a where
  format :: ChunkFormat a

chunksDTA :: ChunkFormat (DTA T.Text)
chunksDTA = ChunkFormat
  { toChunks   = treeChunks . topTree
  , fromChunks = DTA 0 . Tree 0 <$> lift ask
  }

chunksChunk :: ChunkFormat (Chunk T.Text)
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

instance DTASerialize Int     where format = chunksInt
instance DTASerialize Integer where format = chunksInt

instance DTASerialize Float where
  format = ChunkFormat
    { toChunks = \f -> [Float f]
    , fromChunks = lift ask >>= \case
      [Float f] -> return f
      [Int i] -> return $ fromIntegral i
      _ -> expected "float"
    }

instance DTASerialize Bool where
  format = ChunkFormat
    { toChunks = \b -> [Int $ if b then 1 else 0]
    , fromChunks = lift ask >>= \case
      [Int 1] -> return True
      [Int 0] -> return False
      [Key "TRUE"] -> return True
      [Key "FALSE"] -> return False
      _ -> expected "bool"
    }

chunksString :: ChunkFormat T.Text
chunksString = ChunkFormat
  { toChunks = \s -> [String s]
  , fromChunks = lift ask >>= \case
    [String s] -> return s
    _ -> expected "string"
  }

chunksKey :: ChunkFormat T.Text
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
    forM (zip [0..] chunks) $ \(i, chunk) ->
      inside ("list element " ++ show (i :: Int)) $ readFrom [chunk] $ fromChunks cf
  }

instance (DTASerialize a) => DTASerialize [a] where
  format = chunksList format

chunksMaybe :: ChunkFormat a -> ChunkFormat (Maybe a)
chunksMaybe cf = ChunkFormat
  { toChunks = \case
    Nothing -> []
    Just x  -> toChunks cf x
  , fromChunks = lift ask >>= \case
    [] -> return Nothing
    _  -> Just <$> fromChunks cf
  }

instance (DTASerialize a) => DTASerialize (Maybe a) where
  format = chunksMaybe format

-- | A key-value structure which is stored as a sequence of @(tag rest...)@
-- chunks.
newtype Dict a = Dict { fromDict :: Map.Map T.Text a }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

chunksDict :: ChunkFormat a -> ChunkFormat (Dict a)
chunksDict cf = ChunkFormat
  { toChunks = \mp ->
    [ Parens $ Tree 0 $ Key k : toChunks cf v | (k, v) <- Map.toList $ fromDict mp ]
  , fromChunks = lift ask >>= \chunks -> fmap (Dict . Map.fromList) $ forM chunks $ \case
    Parens (Tree _ (Key k : chunks')) -> inside "a key-value structure" $
      (\v -> (k, v)) <$> readFrom chunks' (fromChunks cf)
    _ -> expected "a key-value pair (parenthesized list starting with a keyword)"
  }

instance (DTASerialize a) => DTASerialize (Dict a) where
  format = chunksDict format

chunksParens :: ChunkFormat a -> ChunkFormat a
chunksParens cf = ChunkFormat
  { toChunks = \x -> [Parens $ Tree 0 $ toChunks cf x]
  , fromChunks = lift ask >>= \case
    [Parens (Tree _ chunks)] -> readFrom chunks $ fromChunks cf
    _ -> expected "a set of parentheses"
  }

chunksPair :: ChunkFormat a -> ChunkFormat b -> ChunkFormat (a, b)
chunksPair xf yf = ChunkFormat
  { toChunks = \(x, y) -> toChunks xf x ++ toChunks yf y
  , fromChunks = lift ask >>= \case
    [x, y] -> liftA2 (,)
      (inside "first item of a pair" $ readFrom [x] $ fromChunks xf)
      (inside "second item of a pair" $ readFrom [y] $ fromChunks yf)
    _ -> expected "exactly 2 chunks"
  }

instance (DTASerialize a, DTASerialize b) => DTASerialize (a, b) where
  format = chunksPair format format

chunksEither :: ChunkFormat a -> ChunkFormat b -> ChunkFormat (Either a b)
chunksEither xf yf = ChunkFormat
  { toChunks = \case
    Left  x -> toChunks xf x
    Right y -> toChunks yf y
  , fromChunks = fmap Left (fromChunks xf) <|> fmap Right (fromChunks yf)
  }

instance (DTASerialize a, DTASerialize b) => DTASerialize (Either a b) where
  format = chunksEither format format

data DTAField = DTAField
  { hsField :: String
  , dtaKey :: String
  , fieldType :: TypeQ
  , defaultValue :: Maybe ExpQ
  , warnMissing  :: Bool
  , writeDefault :: Bool
  , fieldFormat :: ExpQ
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
      format = ChunkFormat
        { toChunks = \($(varP recName)) -> toChunks (chunksDict $ chunksList chunksChunk) $ Dict $ Map.fromList $ concat $(
            listE $ flip map fields $ \field -> [e|
              let v = $(varE (mkName (hsField field))) $(varE recName)
              in $(
                case defaultValue field of
                  Just dft | not $ writeDefault field -> [e|
                    if v == $dft
                      then []
                      else [(T.pack $(TH.lift $ dtaKey field), toChunks $(fieldFormat field) v)]
                    |]
                  _ -> [e| [(T.pack $(TH.lift $ dtaKey field), toChunks $(fieldFormat field) v)] |]
              )
            |]
          )
        , fromChunks = do
          Dict mapping <- fromChunks (chunksDict $ chunksList chunksChunk)
          $( foldl apply [e| pure $(conE (mkName rec)) |] $ flip map fields $ \field ->
            let key = "key " ++ show (dtaKey field)
            in case defaultValue field of
              Just dft -> [e|
                case Map.lookup (T.pack $(TH.lift (dtaKey field))) mapping of
                  Nothing -> do
                    when $(TH.lift (warnMissing field)) (warn $ "missing " ++ $(TH.lift key))
                    return $dft
                  Just x -> inside $(TH.lift key) $ readFrom x $ fromChunks $(fieldFormat field)
                |]
              Nothing -> [e|
                case Map.lookup (T.pack $(TH.lift (dtaKey field))) mapping of
                  Nothing -> fatal $ "missing " ++ $(TH.lift key)
                  Just x -> inside $(TH.lift key) $ readFrom x $ fromChunks $(fieldFormat field)
                |]
            )
        }
    |]
  return $ dataDecl : formatDecls

data DTAValue = DTAValue
  { hsCon :: String
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
      format = ChunkFormat
        { toChunks = $(lamCaseE $ do
          v <- vals
          return $ match (conP (mkName (hsCon v)) []) (normalB (dtaValue v)) []
          )
        , fromChunks = do
          x <- lift ask
          let mapping = $(listE $ do
                v <- vals
                return $ tupE [dtaValue v, conE $ mkName $ hsCon v]
                )
          case lookup x mapping of
            Nothing -> fatal $ "unhandled enum value: " ++ show x
            Just v -> return v
        }
    |]
  return $ dataDecl : formatDecls
