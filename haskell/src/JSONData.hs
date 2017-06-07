{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}
module JSONData where

import           Control.Applicative            ((<|>))
import qualified Control.Exception              as Exc
import           Control.Monad                  (forM, unless, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
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

type StackParser m v = StackTraceT (ReaderT v m)

type ObjectParserT m = StackParser (StateT (Set.HashSet T.Text) m) A.Object
type ObjectBuilder = Writer [A.Pair]
type ObjectCodec m a = Codec (ObjectParserT m) ObjectBuilder a

data StackCodec v a = StackCodec
  { stackParse :: forall m. (Monad m) => StackParser m v a
  , stackShow  :: a -> v
  }

identityCodec :: StackCodec a a
identityCodec = StackCodec
  { stackParse = lift ask
  , stackShow  = id
  }

type JSONCodec = StackCodec A.Value

class StackJSON a where
  stackJSON :: JSONCodec a
  default stackJSON :: (A.ToJSON a, A.FromJSON a) => JSONCodec a
  stackJSON = aesonCodec

  stackJSONList :: JSONCodec [a]
  stackJSONList = listCodec stackJSON

expected :: (Monad m) => String -> StackParser m A.Value a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ BL8.unpack (A.encode v)

expectedObj :: (Monad m) => String -> ObjectParserT m a
expectedObj x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ BL8.unpack (A.encode $ A.Object v)

aesonCodec :: (A.ToJSON a, A.FromJSON a) => JSONCodec a
aesonCodec = StackCodec
  { stackShow = A.toJSON
  , stackParse = lift ask >>= \v -> case A.fromJSON v of
    A.Success x -> return x
    A.Error err -> fatal err
  }

listCodec :: JSONCodec a -> JSONCodec [a]
listCodec elt = StackCodec
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
  obj <- lift ask
  known <- lift $ lift get
  let unknown = Set.fromList (Map.keys obj) `Set.difference` known
  unless (Set.null unknown) $ fatal $ "Unrecognized object keys: " ++ show (Set.toList unknown)

makeObject :: (forall m. (Monad m) => ObjectCodec m a) -> a -> A.Object
makeObject codec x = let
  forceIdentity :: (forall m. (Monad m) => ObjectCodec m a) -> ObjectCodec Identity a
  forceIdentity = id
  in Map.fromList $ execWriter $ codecOut (forceIdentity codec) x

asObject :: T.Text -> (forall m. (Monad m) => ObjectCodec m a) -> JSONCodec a
asObject err codec = StackCodec
  { stackParse = lift ask >>= \case
    A.Object obj -> let
      f = withReaderT (const obj) . mapReaderT (`evalStateT` Set.empty)
      in mapStackTraceT f $ codecIn codec
    _ -> expected $ T.unpack err ++ " object"
  , stackShow = A.Object . makeObject codec
  }

asStrictObject :: T.Text -> (forall m. (Monad m) => ObjectCodec m a) -> JSONCodec a
asStrictObject err codec = asObject err Codec
  { codecOut = codecOut ((id :: ObjectCodec Identity a -> ObjectCodec Identity a) codec)
  , codecIn = codecIn codec <* strictKeys
  }

object :: (Monad m) => StackParser m (Map.HashMap T.Text A.Value) a -> StackParser m A.Value a
object p = lift ask >>= \case
  A.Object o -> parseFrom o p
  _          -> expected "an object"

objectKey :: (Monad m, Eq a) => Maybe a -> Bool -> Bool -> T.Text -> JSONCodec a -> ObjectCodec m a
objectKey dflt shouldWarn shouldFill key valCodec = Codec
  { codecIn = do
    obj <- lift ask
    case Map.lookup key obj of
      Nothing -> case dflt of
        Nothing -> expectedObj $ "to find required key " ++ show key ++ " in object"
        Just x  -> do
          when shouldWarn $ warn $ "missing key " ++ show key
          return x
      Just v  -> inside ("required key " ++ show key) $ do
        lift $ lift $ modify $ Set.insert key
        let f = withReaderT (const v) . mapReaderT lift
        mapStackTraceT f $ stackParse valCodec
  , codecOut = \val -> writer
    ( val
    , [ (key, stackShow valCodec val) | shouldFill || dflt == Just val ]
    )
  }

req :: (Monad m, Eq a) => T.Text -> JSONCodec a -> ObjectCodec m a
req = objectKey Nothing False False

warning, fill, opt :: (Monad m, Eq a) => a -> T.Text -> JSONCodec a -> ObjectCodec m a
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

instance StackJSON Int
instance StackJSON Integer
instance StackJSON Scientific
instance StackJSON Double
instance StackJSON T.Text
instance StackJSON Bool
instance StackJSON A.Value

instance (StackJSON a) => StackJSON [a] where
  stackJSON = stackJSONList

instance StackJSON Char where
  stackJSONList = aesonCodec

eitherCodec :: StackCodec v a -> StackCodec v b -> StackCodec v (Either a b)
eitherCodec ca cb = StackCodec
  { stackShow  = either (stackShow ca) (stackShow cb)
  , stackParse = fmap Left (stackParse ca) <|> fmap Right (stackParse cb)
  }

instance (StackJSON a, StackJSON b) => StackJSON (Either a b) where
  stackJSON = eitherCodec stackJSON stackJSON

maybeCodec :: JSONCodec a -> JSONCodec (Maybe a)
maybeCodec c = StackCodec
  { stackShow = maybe A.Null $ stackShow c
  , stackParse = lift ask >>= \case
    A.Null -> return Nothing
    _      -> fmap Just $ stackParse c
  }

instance (StackJSON a) => StackJSON (Maybe a) where
  stackJSON = maybeCodec stackJSON

onlyKey :: (Monad m) => T.Text -> StackParser m A.Value a -> StackParser m (Map.HashMap T.Text A.Value) a
onlyKey k p = lift ask >>= \hm -> case Map.toList hm of
  [(k', v)] | k == k' -> inside ("only key " ++ show k) $ parseFrom v p
  _ -> parseFrom (A.Object hm) $
    expected $ "to find only key " ++ show k ++ " in object"

mapping :: (Monad m) => StackParser m A.Value a -> StackParser m A.Value (Map.HashMap T.Text a)
mapping p = lift ask >>= \case
  A.Object o -> Map.traverseWithKey (\k x -> inside ("mapping key " ++ show k) $ parseFrom x p) o
  _          -> expected "an object"

mappingToJSON :: (StackJSON a) => Map.HashMap T.Text a -> A.Value
mappingToJSON = A.toJSON . fmap toJSON

dict :: JSONCodec a -> JSONCodec (Map.HashMap T.Text a)
dict c = StackCodec
  { stackShow = A.toJSON . fmap (stackShow c)
  , stackParse = mapping $ stackParse c
  }

pattern OneKey :: T.Text -> A.Value -> A.Value
pattern OneKey k v <- A.Object (Map.toList -> [(k, v)]) where
  OneKey k v = A.Object $ Map.fromList [(k, v)]

-- TODO find a safer way to do this
fromEmptyObject :: (StackJSON a) => a
fromEmptyObject = case runReader (runStackTraceT $ stackParse stackJSON) $ A.object [] of
  (Right x , _) -> x
  (Left err, _) -> error $ Exc.displayException err

toJSON :: (StackJSON a) => a -> A.Value
toJSON = stackShow stackJSON

fromJSON :: (Monad m, StackJSON a) => StackParser m A.Value a
fromJSON = stackParse stackJSON
