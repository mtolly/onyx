module Onyx.Codec.Common where

import           Control.Applicative        ((<|>))
import           Control.Monad              (unless, when)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Functor.Identity      (Identity (..))
import qualified Data.HashMap.Strict        as HM
import qualified Data.HashSet               as HS
import           Data.Maybe                 (isJust)
import qualified Data.Text                  as T
import           Onyx.StackTrace

type StackParser m v = StackTraceT (ReaderT v m)

type ObjectParser m v = StackParser (StateT (HS.HashSet T.Text) m) (HM.HashMap T.Text v)
type ObjectBuilder v = Writer [(T.Text, v)]
type ObjectCodec m v a = Codec (ObjectParser m v) (ObjectBuilder v) a

-- note, v will probably not be a Monoid! intended to just tell a single value
type ValueCodec m v a = Codec (StackParser m v) (Writer v) a

makeOut :: (Monad m) => (a -> v) -> (a -> WriterT v m a)
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

expected :: (Monad m, Show v) => String -> StackParser m v a
expected x = lift ask >>= \v -> fatal $ "Expected " ++ x ++ ", but found: " ++ show v

enumCodec :: (Enum a, Bounded a, Eq v, Show v, Monad m) =>
  String -> (a -> v) -> ValueCodec m v a
enumCodec s f = enumCodecFull s $ is . f

enumCodecFull :: (Enum a, Bounded a, Show v, Monad m) =>
  String -> (a -> ValueCodec m v ()) -> ValueCodec m v a
enumCodecFull s f = Codec
  { codecOut = fmapArg $ \x -> codecOut (f x) ()
  , codecIn = foldr (<|>) (expected s)
    [ x <$ codecIn (f x) | x <- [minBound .. maxBound] ]
  }

(|?>) :: (Monad m) => ValueCodec m v () -> ValueCodec m v () -> ValueCodec m v ()
x |?> y = Codec
  { codecOut = codecOut x
  , codecIn = codecIn x <|> codecIn y
  }

is :: (Eq v, Show v, Monad m) => v -> ValueCodec m v ()
is x = Codec
  { codecOut = makeOut $ \() -> x
  , codecIn  = lift ask >>= \v -> if v == x
    then return ()
    else expected $ show x
  }

parseFrom :: (Monad m) => v1 -> StackParser m v1 a -> StackParser m v2 a
parseFrom v = mapStackTraceT $ withReaderT $ const v

-- | Should be run as the last action in an object parser.
-- Raises an error if the object has a key that wasn't parsed.
strictKeys :: (Monad m) => ObjectParser m v ()
strictKeys = do
  obj <- lift ask
  known <- lift $ lift get
  let unknown = HS.fromList (HM.keys obj) `HS.difference` known
  unless (HS.null unknown) $ fatal $ "Unrecognized object keys: " ++ show (HS.toList unknown)

warnKeys :: (SendMessage m) => ObjectParser m v ()
warnKeys = do
  obj <- lift ask
  known <- lift $ lift get
  let unknown = HS.fromList (HM.keys obj) `HS.difference` known
  unless (HS.null unknown) $ warn $ "Unrecognized object keys: " ++ show (HS.toList unknown)

makeObject :: (Monad m) => ObjectCodec m v a -> a -> [(T.Text, v)]
makeObject codec x = execWriter $ codecOut codec x

objectId :: ObjectCodec (PureLog Identity) v a -> ObjectCodec (PureLog Identity) v a
objectId = id

valueId :: ValueCodec (PureLog Identity) v a -> ValueCodec (PureLog Identity) v a
valueId = id

objectKey :: (SendMessage m, Show v) => Maybe (a, a -> Bool) -> Bool -> Bool -> T.Text -> ValueCodec m v a -> ObjectCodec m v a
objectKey dflt shouldWarn shouldFill key valCodec = Codec
  { codecIn = do
    obj <- lift ask
    case HM.lookup key obj of
      Nothing -> case dflt of
        Nothing     -> expected $ "to find required key " ++ show key ++ " in object"
        Just (x, _) -> do
          when shouldWarn $ warn $ "missing key " ++ show key
          return x
      Just v  -> let
        keyLayer = (if isJust dflt then "optional" else "required") <> " key " <> show key
        in inside keyLayer $ do
          lift $ lift $ modify $ HS.insert key
          let f = withReaderT (const v) . mapReaderT lift
          mapStackTraceT f $ codecIn valCodec
  , codecOut = fmapArg $ \val -> tell
    [ (key, makeValue' valCodec val)
    | shouldFill || case dflt of
      Just (_, fn) -> not $ fn val
      _            -> True
    ]
  }

-- TODO req/fill/opt shouldn't need SendMessage constraint

req :: (SendMessage m, Show v) => T.Text -> ValueCodec m v a -> ObjectCodec m v a
req = objectKey Nothing False False

warning, fill, opt :: (SendMessage m, Show v, Eq a) => a -> T.Text -> ValueCodec m v a -> ObjectCodec m v a
warning x = objectKey (Just (x, (== x))) True  True
fill    x = objectKey (Just (x, (== x))) False True
opt     x = objectKey (Just (x, (== x))) False False

eitherCodec :: (Monad m) => ValueCodec m v a -> ValueCodec m v b -> ValueCodec m v (Either a b)
eitherCodec ca cb = Codec
  { codecOut  = makeOut $ either (makeValue' ca) (makeValue' cb)
  , codecIn = fmap Left (codecIn ca) <|> fmap Right (codecIn cb)
  }

emptyCodec :: (Monad r, Monad w, Monoid a) => Codec r w a
emptyCodec = Codec
  { codecIn  = return mempty
  , codecOut = return
  }
