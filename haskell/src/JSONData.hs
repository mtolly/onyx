{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
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
import           Data.Functor.Identity          (Identity (..))
import qualified Data.HashMap.Strict            as Map
import qualified Data.HashSet                   as Set
import           Data.Maybe                     (isJust)
import           Data.Scientific
import qualified Data.Text                      as T
import qualified Data.Vector                    as V

type StackParser m v = StackTraceT (ReaderT v m)

type ObjectParser m v = StackParser (StateT (Set.HashSet T.Text) m) (Map.HashMap T.Text v)
type ObjectBuilder v = Writer [(T.Text, v)]
type ObjectCodec m v a = Codec (ObjectParser m v) (ObjectBuilder v) a

-- note, v will probably not be a Monoid! intended to just tell a single value
type ValueCodec m v a = Codec (StackParser m v) (Writer v) a

makeOut :: (a -> v) -> (a -> Writer v a)
makeOut f = fmapArg $ tell . f

makeValue' :: ValueCodec m v a -> a -> v
makeValue' cdc = execWriter . codecOut cdc

makeValue :: ValueCodec (PureLog Identity) v a -> a -> v
makeValue = makeValue'

identityCodec :: (Monad m) => ValueCodec m a a
identityCodec = Codec
  { codecIn = lift ask
  , codecOut = makeOut id
  }

type JSONCodec m a = ValueCodec m A.Value a

class StackJSON a where
  stackJSON :: (SendMessage m) => JSONCodec m a
  default stackJSON :: (A.ToJSON a, A.FromJSON a, SendMessage m) => JSONCodec m a
  stackJSON = aesonCodec

  stackJSONList :: (SendMessage m) => JSONCodec m [a]
  stackJSONList = listCodec stackJSON

expected :: (Monad m, Show v) => String -> StackParser m v a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ show v

aesonCodec :: (A.ToJSON a, A.FromJSON a, Monad m) => JSONCodec m a
aesonCodec = Codec
  { codecOut = makeOut A.toJSON
  , codecIn = lift ask >>= \v -> case A.fromJSON v of
    A.Success x -> return x
    A.Error err -> fatal err
  }

listCodec :: (Monad m) => JSONCodec m a -> JSONCodec m [a]
listCodec elt = Codec
  { codecOut = makeOut $ A.Array . V.fromList . map (makeValue' elt)
  , codecIn = lift ask >>= \case
    A.Array vect -> forM (zip [0..] $ V.toList vect) $ \(i, x) ->
      inside ("array element " ++ show (i :: Int)) $
        parseFrom x $ codecIn elt
    _ -> expected "array"
  }

parseFrom :: (Monad m) => v1 -> StackParser m v1 a -> StackParser m v2 a
parseFrom v = mapStackTraceT $ withReaderT $ const v

-- | Should be run as the last action in an object parser.
-- Raises an error if the object has a key that wasn't parsed.
strictKeys :: (Monad m) => ObjectParser m v ()
strictKeys = do
  obj <- lift ask
  known <- lift $ lift get
  let unknown = Set.fromList (Map.keys obj) `Set.difference` known
  unless (Set.null unknown) $ fatal $ "Unrecognized object keys: " ++ show (Set.toList unknown)

makeObject :: (Monad m) => ObjectCodec m v a -> a -> [(T.Text, v)]
makeObject codec x = execWriter $ codecOut codec x

asObject :: (Monad m) => T.Text -> ObjectCodec m A.Value a -> JSONCodec m a
asObject err codec = Codec
  { codecIn = inside ("parsing " ++ T.unpack err) $ lift ask >>= \case
    A.Object obj -> let
      f = withReaderT (const obj) . mapReaderT (`evalStateT` Set.empty)
      in mapStackTraceT f $ codecIn codec
    _ -> expected "object"
  , codecOut = makeOut $ A.Object . Map.fromList . makeObject codec
  }

objectId :: ObjectCodec (PureLog Identity) v a -> ObjectCodec (PureLog Identity) v a
objectId = id

valueId :: ValueCodec (PureLog Identity) v a -> ValueCodec (PureLog Identity) v a
valueId = id

asStrictObject :: (Monad m) => T.Text -> ObjectCodec m A.Value a -> JSONCodec m a
asStrictObject err codec = asObject err Codec
  { codecOut = codecOut codec
  , codecIn = codecIn codec <* strictKeys
  }

object :: (Monad m) => StackParser m (Map.HashMap T.Text A.Value) a -> StackParser m A.Value a
object p = lift ask >>= \case
  A.Object o -> parseFrom o p
  _          -> expected "an object"

objectKey :: (SendMessage m, Eq a, Show v) => Maybe a -> Bool -> Bool -> T.Text -> ValueCodec m v a -> ObjectCodec m v a
objectKey dflt shouldWarn shouldFill key valCodec = Codec
  { codecIn = do
    obj <- lift ask
    case Map.lookup key obj of
      Nothing -> case dflt of
        Nothing -> expected $ "to find required key " ++ show key ++ " in object"
        Just x  -> do
          when shouldWarn $ warn $ "missing key " ++ show key
          return x
      Just v  -> let
        keyLayer = (if isJust dflt then "optional" else "required") <> " key " <> show key
        in inside keyLayer $ do
          lift $ lift $ modify $ Set.insert key
          let f = withReaderT (const v) . mapReaderT lift
          mapStackTraceT f $ codecIn valCodec
  , codecOut = fmapArg $ \val -> tell
    [(key, makeValue' valCodec val) | shouldFill || dflt /= Just val]
  }

-- TODO req/fill/opt shouldn't need SendMessage constraint

req :: (SendMessage m, Show v, Eq a) => T.Text -> ValueCodec m v a -> ObjectCodec m v a
req = objectKey Nothing False False

warning, fill, opt :: (SendMessage m, Show v, Eq a) => a -> T.Text -> ValueCodec m v a -> ObjectCodec m v a
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

eitherCodec :: (Monad m) => ValueCodec m v a -> ValueCodec m v b -> ValueCodec m v (Either a b)
eitherCodec ca cb = Codec
  { codecOut  = makeOut $ either (makeValue' ca) (makeValue' cb)
  , codecIn = fmap Left (codecIn ca) <|> fmap Right (codecIn cb)
  }

instance (StackJSON a, StackJSON b) => StackJSON (Either a b) where
  stackJSON = eitherCodec stackJSON stackJSON

maybeCodec :: (Monad m) => JSONCodec m a -> JSONCodec m (Maybe a)
maybeCodec c = Codec
  { codecOut = makeOut $ maybe A.Null $ makeValue' c
  , codecIn = lift ask >>= \case
    A.Null -> return Nothing
    _      -> Just <$> codecIn c
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

dict :: (Monad m) => JSONCodec m a -> JSONCodec m (Map.HashMap T.Text a)
dict c = Codec
  { codecOut = makeOut $ A.toJSON . fmap (makeValue' c)
  , codecIn = mapping $ codecIn c
  }

pattern OneKey :: T.Text -> A.Value -> A.Value
pattern OneKey k v <- A.Object (Map.toList -> [(k, v)]) where
  OneKey k v = A.Object $ Map.fromList [(k, v)]

-- TODO find a safer way to do this
fromEmptyObject :: (StackJSON a) => a
fromEmptyObject = case runPureLog $ runReaderT (runStackTraceT $ codecIn stackJSON) $ A.object [] of
  (Right x , _) -> x
  (Left err, _) -> error $ Exc.displayException err

toJSON :: (StackJSON a) => a -> A.Value
toJSON = makeValue stackJSON

fromJSON :: (SendMessage m, StackJSON a) => StackParser m A.Value a
fromJSON = codecIn stackJSON
