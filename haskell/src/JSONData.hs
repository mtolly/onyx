{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ViewPatterns               #-}
module JSONData where

import           Control.Applicative            ((<|>))
import qualified Control.Exception              as Exc
import           Control.Monad                  (forM, unless, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS        as RWS
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.Writer
import qualified Data.Aeson                     as A
import qualified Data.Aeson.Types               as A
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.Functor.Identity          (Identity (..))
import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

type StackParser m context = StackTraceT (ReaderT context m)

type ObjectParserT m = StackTraceT (RWS.RWST A.Object () (Set.HashSet T.Text) m)
type ObjectBuilder = Writer [A.Pair]
type ObjectCodec m a = Codec (ObjectParserT m) ObjectBuilder a

data JSONCodec m a = JSONCodec
  { stackParse :: StackParser m A.Value a
  , stackShow  :: a -> A.Value
  }

expected :: (Monad m) => String -> StackParser m A.Value a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ BL8.unpack (A.encode v)

expectedObj :: (Monad m) => String -> ObjectParserT m a
expectedObj x = lift RWS.ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ BL8.unpack (A.encode $ A.Object v)

class TraceJSON a where
  traceCodec :: (Monad m) => JSONCodec m a
  default traceCodec :: (A.ToJSON a, A.FromJSON a, Monad m) => JSONCodec m a
  traceCodec = aesonCodec

  traceCodecList :: (Monad m) => JSONCodec m [a]
  traceCodecList = listCodec traceCodec

aesonCodec :: (A.ToJSON a, A.FromJSON a, Monad m) => JSONCodec m a
aesonCodec = JSONCodec
  { stackShow = A.toJSON
  , stackParse = lift ask >>= \v -> case A.fromJSON v of
    A.Success x -> return x
    A.Error err -> fatal err
  }

listCodec :: (Monad m) => JSONCodec m a -> JSONCodec m [a]
listCodec elt = JSONCodec
  { stackShow = A.Array . V.fromList . map (stackShow elt)
  , stackParse = lift ask >>= \case
    A.Array vect -> forM (zip [0..] $ V.toList vect) $ \(i, x) ->
      inside ("array element " ++ show (i :: Int)) $
        parseFrom x $ stackParse elt
    _ -> expected "array"
  }

parseFrom :: (Monad m) => v1 -> StackParser m v1 a -> StackParser m v2 a
parseFrom v = mapStackTraceT $ withReaderT $ const v

-- | Should be run as the last action in an object parser.
-- Raises an error if the object has a key that wasn't parsed.
strictKeys :: (Monad m) => ObjectParserT m ()
strictKeys = do
  obj <- lift RWS.ask
  known <- lift RWS.get
  let unknown = Set.fromList (Map.keys obj) `Set.difference` known
  unless (Set.null unknown) $ fatal $ "Unrecognized object keys: " ++ show (Set.toList unknown)

asObject :: (Monad m) => T.Text -> ObjectCodec m a -> JSONCodec m a
asObject err codec = JSONCodec
  { stackParse = lift ask >>= \case
    A.Object obj -> let
      f rws = lift $ fmap fst $ RWS.evalRWST rws obj Set.empty
      in mapStackTraceT f $ codecIn codec
    _ -> expected $ T.unpack err ++ " object"
  , stackShow = A.object . execWriter . codecOut codec
  }

asStrictObject :: (Monad m) => T.Text -> Codec (ObjectParserT m) ObjectBuilder a -> JSONCodec m a
asStrictObject err codec = asObject err codec{ codecIn = codecIn codec <* strictKeys }

object :: (Monad m) => StackParser m (Map.HashMap T.Text A.Value) a -> StackParser m A.Value a
object p = lift ask >>= \case
  A.Object o -> parseFrom o p
  _          -> expected "an object"

objectKey :: (Monad m, Eq a) => Maybe a -> Bool -> Bool -> T.Text -> JSONCodec m a -> ObjectCodec m a
objectKey dflt shouldWarn shouldFill key valCodec = Codec
  { codecIn = do
    obj <- lift RWS.ask
    case Map.lookup key obj of
      Nothing -> case dflt of
        Nothing -> expectedObj $ "to find required key " ++ show key ++ " in object"
        Just x  -> do
          when shouldWarn $ warn $ "missing key " ++ show key
          return x
      Just v  -> inside ("required key " ++ show key) $ do
        lift $ RWS.modify $ Set.insert key
        let f rdr = lift $ runReaderT rdr v
        mapStackTraceT f $ stackParse valCodec
  , codecOut = \val -> writer
    ( val
    , [ (key, stackShow valCodec val) | shouldFill || dflt == Just val ]
    )
  }

req :: (Monad m, Eq a) => T.Text -> JSONCodec m a -> ObjectCodec m a
req = objectKey Nothing False False

warning, fill, opt :: (Monad m, Eq a) => a -> T.Text -> JSONCodec m a -> ObjectCodec m a
warning x = objectKey (Just x) True  True
fill    x = objectKey (Just x) False True
opt     x = objectKey (Just x) False False

-- TODO cleanup
requiredKey :: (Monad m) => T.Text -> StackParser m A.Value a -> StackParser m (Map.HashMap T.Text A.Value) a
requiredKey k p = lift ask >>= \hm -> case Map.lookup k hm of
  Nothing -> parseFrom (A.Object hm) $
    expected $ "to find required key " ++ show k ++ " in object"
  Just v  -> inside ("required key " ++ show k) $ parseFrom v p

-- TODO cleanup
optionalKey :: (Monad m) => T.Text -> StackParser m A.Value a -> StackParser m (Map.HashMap T.Text A.Value) (Maybe a)
optionalKey k p = lift ask >>= \hm -> case Map.lookup k hm of
  Nothing -> return Nothing
  Just v  -> fmap Just $ inside ("optional key " ++ show k) $ parseFrom v p

-- TODO cleanup
expectedKeys :: (Monad m) => [T.Text] -> StackParser m (Map.HashMap T.Text A.Value) ()
expectedKeys keys = do
  hm <- lift ask
  let unknown = Set.fromList (Map.keys hm) `Set.difference` Set.fromList keys
  unless (Set.null unknown) $ fatal $ "Unrecognized object keys: " ++ show (Set.toList unknown)

instance TraceJSON Int
instance TraceJSON Integer
instance TraceJSON Scientific
instance TraceJSON Double
instance TraceJSON T.Text
instance TraceJSON Bool
instance TraceJSON A.Value

instance (TraceJSON a) => TraceJSON [a] where
  traceCodec = traceCodecList

instance TraceJSON Char where
  traceCodecList = aesonCodec

eitherCodec :: (Monad m) => JSONCodec m a -> JSONCodec m b -> JSONCodec m (Either a b)
eitherCodec ca cb = JSONCodec
  { stackShow  = either (stackShow ca) (stackShow cb)
  , stackParse = fmap Left (stackParse ca) <|> fmap Right (stackParse cb)
  }

instance (TraceJSON a, TraceJSON b) => TraceJSON (Either a b) where
  traceCodec = eitherCodec traceCodec traceCodec

maybeCodec :: (Monad m) => JSONCodec m a -> JSONCodec m (Maybe a)
maybeCodec c = JSONCodec
  { stackShow = maybe A.Null $ stackShow c
  , stackParse = lift ask >>= \case
    A.Null -> return Nothing
    _      -> fmap Just $ stackParse c
  }

instance (TraceJSON a) => TraceJSON (Maybe a) where
  traceCodec = maybeCodec traceCodec

onlyKey :: (Monad m) => T.Text -> StackParser m A.Value a -> StackParser m (Map.HashMap T.Text A.Value) a
onlyKey k p = lift ask >>= \hm -> case Map.toList hm of
  [(k', v)] | k == k' -> inside ("only key " ++ show k) $ parseFrom v p
  _ -> parseFrom (A.Object hm) $
    expected $ "to find only key " ++ show k ++ " in object"

mapping :: (Monad m) => StackParser m A.Value a -> StackParser m A.Value (Map.HashMap T.Text a)
mapping p = lift ask >>= \case
  A.Object o -> Map.traverseWithKey (\k x -> inside ("mapping key " ++ show k) $ parseFrom x p) o
  _          -> expected "an object"

mappingToJSON :: (TraceJSON a) => Map.HashMap T.Text a -> A.Value
mappingToJSON = A.toJSON . fmap toJSON

dict :: (Monad m) => JSONCodec m a -> JSONCodec m (Map.HashMap T.Text a)
dict c = JSONCodec
  { stackShow = A.toJSON . fmap (stackShow c)
  , stackParse = mapping $ stackParse c
  }

pattern OneKey :: T.Text -> A.Value -> A.Value
pattern OneKey k v <- A.Object (Map.toList -> [(k, v)]) where
  OneKey k v = A.Object $ Map.fromList [(k, v)]

-- TODO find a safer way to do this
fromEmptyObject :: (TraceJSON a) => a
fromEmptyObject = case runReader (runStackTraceT $ stackParse traceCodec) $ A.object [] of
  (Right x , _) -> x
  (Left err, _) -> error $ Exc.displayException err

toJSON :: (TraceJSON a) => a -> A.Value
toJSON x = stackShow (traceCodec :: (TraceJSON b) => JSONCodec Identity b) x

fromJSON :: (Monad m, TraceJSON a) => StackParser m A.Value a
fromJSON = stackParse traceCodec
