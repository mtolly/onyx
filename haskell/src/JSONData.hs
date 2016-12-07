{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module JSONData where

import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace hiding (optional)
import           Control.Monad.Trans.Writer
import           Data.Aeson                     ((.=))
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.Default.Class
import qualified Data.HashMap.Strict            as Map
import           Data.List                      ((\\))
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax     as TH

type Parser m context = StackTraceT (ReaderT context m)

expected :: (Monad m) => String -> Parser m A.Value a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ BL8.unpack (A.encode v)

class TraceJSON a where
  traceJSON :: (Monad m) => Parser m A.Value a
  traceJSONList :: (Monad m) => Parser m A.Value [a]
  traceJSONList = lift ask >>= \case
    A.Array v -> forM (zip [0..] $ V.toList v) $ \(i, x) ->
      inside ("array element " ++ show (i :: Int)) $ parseFrom x traceJSON
    _         -> expected "an array"

instance (TraceJSON a) => TraceJSON [a] where
  traceJSON = traceJSONList

instance TraceJSON A.Value where
  traceJSON = lift ask

instance TraceJSON Bool where
  traceJSON = lift ask >>= \case
    A.Bool b -> return b
    _        -> expected "a boolean"

instance TraceJSON T.Text where
  traceJSON = lift ask >>= \case
    A.String s -> return s
    _          -> expected "a string"

instance TraceJSON Char where
  traceJSON = traceJSON >>= \case
    [c] -> return c
    _   -> expected "a string of a single character"
  traceJSONList = fmap T.unpack traceJSON

instance TraceJSON Scientific where
  traceJSON = lift ask >>= \case
    A.Number n -> return n
    _          -> expected "a number"

instance TraceJSON Integer where
  traceJSON = fmap (round :: Scientific -> Integer) traceJSON

instance TraceJSON Double where
  traceJSON = fmap toRealFloat traceJSON

instance TraceJSON Int where
  traceJSON = traceJSON >>= \n -> case toBoundedInteger n of
    Nothing -> expected "a number in Int range"
    Just i  -> return i

newtype JSONEither a b = JSONEither (Either a b)
  deriving (Eq, Ord, Show, Read)

instance (TraceJSON a, TraceJSON b) => TraceJSON (JSONEither a b) where
  traceJSON = fmap JSONEither $ fmap Left traceJSON <|> fmap Right traceJSON

instance (A.ToJSON a, A.ToJSON b) => A.ToJSON (JSONEither a b) where
  toJSON (JSONEither (Left  x)) = A.toJSON x
  toJSON (JSONEither (Right y)) = A.toJSON y

instance (TraceJSON a, TraceJSON b) => TraceJSON (Either a b) where
  traceJSON = fmap Left traceJSON <|> fmap Right traceJSON

instance (TraceJSON a) => TraceJSON (Maybe a) where
  traceJSON = lift ask >>= \case
    A.Null -> return Nothing
    _      -> fmap Just traceJSON

parseFrom :: v -> Parser m v a -> Parser m v' a
parseFrom = mapStackTraceT . withReaderT . const

object :: (Monad m) => Parser m (Map.HashMap T.Text A.Value) a -> Parser m A.Value a
object p = lift ask >>= \case
  A.Object o -> parseFrom o p
  _          -> expected "an object"

required :: (Monad m) => T.Text -> Parser m A.Value a -> Parser m (Map.HashMap T.Text A.Value) a
required k p = lift ask >>= \hm -> case Map.lookup k hm of
  Nothing -> parseFrom (A.Object hm) $
    expected $ "to find required key " ++ show k ++ " in object"
  Just v  -> inside ("required key " ++ show k) $ parseFrom v p

optional :: (Monad m) => T.Text -> Parser m A.Value a -> Parser m (Map.HashMap T.Text A.Value) (Maybe a)
optional k p = lift ask >>= \hm -> case Map.lookup k hm of
  Nothing -> return Nothing
  Just v  -> fmap Just $ inside ("optional key " ++ show k) $ parseFrom v p

theKey :: (Monad m) => T.Text -> Parser m A.Value a -> Parser m (Map.HashMap T.Text A.Value) a
theKey k p = lift ask >>= \hm -> case Map.toList hm of
  [(k', v)] | k == k' -> inside ("only key " ++ show k) $ parseFrom v p
  _ -> parseFrom (A.Object hm) $
    expected $ "to find only key " ++ show k ++ " in object"

expectedKeys :: (Monad m) => [T.Text] -> Parser m (Map.HashMap T.Text A.Value) ()
expectedKeys keys = do
  hm <- lift ask
  case Map.keys hm \\ keys of
    []    -> return ()
    unrec -> fatal $ "Unrecognized object keys: " ++ show unrec

mapping :: (Monad m) => Parser m A.Value a -> Parser m A.Value (Map.HashMap T.Text a)
mapping p = lift ask >>= \case
  A.Object o -> Map.traverseWithKey (\k x -> inside ("mapping key " ++ show k) $ parseFrom x p) o
  _          -> expected "an object"

pattern OneKey :: T.Text -> A.Value -> A.Value
pattern OneKey k v <- A.Object (Map.toList -> [(k, v)]) where
  OneKey k v = A.Object $ Map.fromList [(k, v)]

data JSONField = JSONField
  { hsField      :: String
  , jsonKey      :: String
  , fieldType    :: TypeQ
  , defaultValue :: Maybe ExpQ
  , warnMissing  :: Bool
  , writeDefault :: Bool
  }

req :: String -> String -> TypeQ -> Writer [JSONField] ()
req hs js t = tell [JSONField hs js t Nothing False False]

warning, fill, opt :: String -> String -> TypeQ -> ExpQ -> Writer [JSONField] ()
warning hs js t dft = tell [JSONField hs js t (Just dft) True  False]
fill    hs js t dft = tell [JSONField hs js t (Just dft) False True ]
opt     hs js t dft = tell [JSONField hs js t (Just dft) False False]

eos :: CxtQ
eos = cxt [[t| Eq |], [t| Ord |], [t| Show |]]

eosr :: CxtQ
eosr = cxt [[t| Eq |], [t| Ord |], [t| Show |], [t| Read |]]

jsonRecord :: String -> CxtQ -> Writer [JSONField] () -> DecsQ
jsonRecord rec derivs writ = do
  let fields = execWriter writ
      apply x y = [e| $x <*> $y |]
      fn x y = [e| $x $y |]
  dec1 <- dataD (cxt []) (mkName rec) [] Nothing
    [ recC (mkName rec) $ flip map fields $ \field ->
      varBangType (mkName $ hsField field)
      $ bangType (bang sourceNoUnpack noSourceStrictness) (fieldType field)
    ]
    derivs
  decs2 <- let
    keys = TH.lift $ map jsonKey fields
    tracer = foldl apply [e| pure $(conE (mkName rec)) |] $ flip map fields $ \field ->
      case defaultValue field of
        Just dft -> [e|
          optional $(TH.lift (jsonKey field)) traceJSON >>= \case
            Nothing -> do
              when $(TH.lift (warnMissing field))
                (warn ("missing key " ++ show ($(TH.lift (jsonKey field)) :: String)))
              return $dft
            Just x -> return x
          |]
        Nothing -> [e| required $(TH.lift (jsonKey field)) traceJSON |]
    in [d|
      instance TraceJSON $(conT (mkName rec)) where
        traceJSON = object (expectedKeys $keys >> $tracer)
    |]
  recName <- newName "rec"
  decs3 <- let
    parts = listE $ flip map fields $ \field -> let
      getField = varE (mkName (hsField field))
      in case defaultValue field of
        Just dft | not $ writeDefault field -> [e|
          if $getField $(varE recName) == $dft
            then []
            else [$(TH.lift (jsonKey field)) .= $getField $(varE recName)]
          |]
        _ -> [e| [$(TH.lift (jsonKey field)) .= $getField $(varE recName)] |]
    in [d|
      instance A.ToJSON $(conT (mkName rec)) where
        toJSON $(varP recName) = A.object (concat $parts)
    |]
  decs4 <- case mapM defaultValue fields of
    Nothing   -> return []
    Just defs -> [d|
      instance Default $(conT (mkName rec)) where
        def = $(foldl fn (conE (mkName rec)) defs)
      |]
  return $ [dec1] ++ decs2 ++ decs3 ++ decs4
