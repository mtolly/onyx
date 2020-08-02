{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Control.Monad.Codec.Onyx.XML where

import           Control.Arrow                  (second)
import           Control.Monad                  (forM, forM_, guard, unless,
                                                 void, when)
import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                      (isSpace)
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.List.NonEmpty             as NE
import           Data.Maybe                     (fromMaybe, isJust, isNothing)
import           Data.Profunctor                (dimap)
import           Data.String                    (IsString (..))
import qualified Data.Text                      as T
import           Data.Tuple                     (swap)
import qualified Data.Vector                    as V
import qualified Sound.MIDI.Util                as U
import           Text.Read                      (readMaybe)
import           Text.XML.Light

data Inside = Inside
  { insideAttrs   :: V.Vector Attr
  , insideContent :: V.Vector Content
  }

instance Semigroup Inside where
  Inside a b <> Inside c d = Inside (a <> c) (b <> d)

instance Monoid Inside where
  mempty = Inside V.empty V.empty

data Namespace = Namespace
  { nsPrefix :: String
  , nsURI    :: String
  }

type InsideParser m = StackTraceT (StateT Inside m)
type InsideBuilder = WriterT Inside (State (NE.NonEmpty [Namespace]))
type InsideCodec m a = Codec (InsideParser m) InsideBuilder a

type ValueCodec' m v a = Codec (StackParser m v) (WriterT v (State (NE.NonEmpty [Namespace]))) a

class IsInside a where
  insideCodec :: (SendMessage m) => InsideCodec m a

data ParseName = ParseName
  { pURI  :: Maybe String
  , pName :: String
  } deriving (Eq, Show)

instance IsString ParseName where
  fromString = ParseName Nothing

inSpace :: String -> String -> ParseName
inSpace = ParseName . Just

getNamespace :: Maybe String -> String -> InsideBuilder String
getNamespace defaultPrefix uri = do
  existing <- lift get
  case filter (\ns -> nsURI ns == uri) $ concat $ NE.toList existing of
    ns : _ -> return $ nsPrefix ns
    []     -> do
      let prefix = case defaultPrefix of
            Just p -> p -- TODO maybe check if it's being used already
            Nothing -> let
              depth        = NE.length existing
              prefixNumber = length (NE.head existing) + 1
              in "d" <> show depth <> "p" <> show prefixNumber
          newNamespace = Namespace
            { nsPrefix = prefix
            , nsURI = uri
            }
      lift $ put $ case existing of
        h :| t -> (newNamespace : h) :| t
      tell Inside
        { insideAttrs = V.singleton $ case prefix of
          "" -> Attr (QName "xmlns" Nothing Nothing) uri
          _  -> Attr (QName prefix Nothing $ Just "xmlns") uri
        , insideContent = V.empty
        }
      return prefix

useNamespace :: (Monad m) => Maybe String -> String -> CodecFor m InsideBuilder c ()
useNamespace dp uri = Codec
  { codecIn = return ()
  , codecOut = \_ -> void $ getNamespace dp uri
  }

matchName :: ParseName -> QName -> Bool
matchName pn qn = qURI qn == pURI pn && qName qn == pName pn

matchTag :: ParseName -> Element -> Maybe (StackTraceT m a -> StackTraceT m a)
matchTag pn elt = do
  guard $ matchName pn $ elName elt
  return $ inside $ unwords $ concat
    [ toList (elLine elt) >>= \ln -> ["line", show ln]
    , ["tag " <> show pn]
    ]

makeDoc :: ParseName -> InsideBuilder () -> Element
makeDoc = makeTag []

makeTag :: [[Namespace]] -> ParseName -> InsideBuilder () -> Element
makeTag namespaces pn inner = let
  (prefix, result) = flip evalState ([] :| namespaces) $ runWriterT $ do
    inner
    mapM (getNamespace Nothing) $ pURI pn
  in Element
    { elName = QName
      { qName = pName pn
      , qURI = pURI pn
      , qPrefix = case prefix of
        Just "" -> Nothing
        _       -> prefix
      }
    , elAttribs = V.toList $ insideAttrs result
    , elContent = V.toList $ insideContent result
    , elLine = Nothing
    }

makeTag' :: (Monoid w) => ParseName -> InsideBuilder () -> WriterT w (State (NE.NonEmpty [Namespace])) Element
makeTag' pn inner = do
  namespaces <- lift get
  return $ makeTag (NE.toList namespaces) pn inner

isTag :: (Monad m) => ParseName -> ValueCodec' m Inside a -> ValueCodec' m Element a
isTag t cdc = Codec
  { codecIn = do
    elt <- lift ask
    case matchTag t elt of
      Nothing -> fatal $ "Expected a tag " <> show t <> " but got: " <> show (elName elt)
      Just layer -> layer $ parseFrom (Inside (V.fromList $ elAttribs elt) (V.fromList $ elContent elt)) $ codecIn cdc
  , codecOut = fmapArg $ mapWriterT (fmap swap) . makeTag' t . void . codecOut cdc
  }

childTagOpt :: (Monad m) => ParseName -> ValueCodec' m Inside a -> InsideCodec m (Maybe a)
childTagOpt t cdc = Codec
  { codecIn = do
    ins <- lift get
    let isElement = \case Elem e -> Just e; _ -> Nothing
    case V.break (isJust . snd) $ fmap (\x -> (x, isElement x >>= matchTag t)) $ insideContent ins of
      (before, V.toList -> (Elem elt, Just layer) : after) -> do
        let ins' = Inside (V.fromList $ elAttribs elt) (V.fromList $ elContent elt)
        x <- layer $ mapStackTraceT (lift . (`runReaderT` ins')) $ codecIn cdc
        lift $ put $ ins { insideContent = fmap fst $ before <> V.fromList after }
        return $ Just x
      _ -> return Nothing
  , codecOut = fmapArg $ \case
    Nothing -> return ()
    Just x -> do
      elt <- makeTag' t $ void $ codecOut cdc x
      tell Inside
        { insideAttrs = V.empty
        , insideContent = V.singleton $ Elem elt
        }
  }

childTag :: (Monad m) => ParseName -> ValueCodec' m Inside a -> InsideCodec m a
childTag t cdc = let
  o = childTagOpt t cdc
  in Codec
    { codecIn = codecIn o >>= \case
      Just x  -> return x
      Nothing -> fatal $ "Missing required child tag " <> show t
    , codecOut = fmapArg $ void . codecOut o . Just
    }

parseInside :: (Monad m) => InsideCodec m a -> ValueCodec' m Inside a
parseInside cdc = Codec
  { codecIn = do
    ins <- lift ask
    mapStackTraceT (lift . (`evalStateT` ins)) $ codecIn cdc
  , codecOut = codecOut cdc
  }

warnUnused :: (SendMessage m) => InsideCodec m a -> InsideCodec m a
warnUnused cdc = cdc
  { codecIn = codecIn cdc <* do
    ins <- lift get
    forM_ (insideAttrs ins) $ \attr -> warn $ "Unused attribute " <> show (qName $ attrKey attr)
    forM_ (insideContent ins) $ \case
      Elem elt -> warn $ unwords $ concat
        [ ["Unused element <" <> qName (elName elt) <> ">"]
        , toList (elLine elt) >>= \ln -> ["at line", show ln]
        ]
      Text cd -> maybe id (inside . ("line " <>) . show) (cdLine cd) $ do
        unless (all isSpace $ cdData cd)
          $ warn $ "Found text: " <> show (cdData cd)
      CRef _ -> warn "Found CRef"
  }

parseInside' :: (SendMessage m) => InsideCodec m a -> ValueCodec' m Inside a
parseInside' = parseInside . warnUnused

optAttr :: (Monad m) => ParseName -> InsideCodec m (Maybe T.Text)
optAttr a = Codec
  { codecIn = do
    ins <- lift get
    case V.break (matchName a . attrKey) $ insideAttrs ins of
      (before, V.toList -> attr : after) -> do
        lift $ put $ ins { insideAttrs = before <> V.fromList after }
        return $ Just $ T.pack $ attrVal attr
      _ -> return Nothing
  , codecOut = fmapArg $ \case
    Nothing -> return ()
    Just x -> do
      prefix <- mapM (getNamespace Nothing) $ pURI a
      let qn = QName
            { qName = pName a
            , qURI = pURI a
            , qPrefix = case prefix of
              Just "" -> Nothing
              _       -> prefix
            }
      tell Inside
        { insideContent = V.empty
        , insideAttrs = V.singleton $ Attr qn $ T.unpack x
        }
  }

reqAttr :: (Monad m) => ParseName -> InsideCodec m T.Text
reqAttr a = Codec
  { codecIn = codecIn o >>= \case
    Just x  -> return x
    Nothing -> fatal $ "Missing required attribute " <> show a
  , codecOut = fmapArg $ void . codecOut o . Just
  } where o = optAttr a

childElements :: (SendMessage m) => InsideCodec m (V.Vector Element)
childElements = Codec
  { codecIn = do
    ins <- lift get
    elts <- fmap (V.mapMaybe id) $ forM (insideContent ins) $ \case
      Elem elt -> return $ Just elt
      Text cd -> do
        unless (all isSpace $ cdData cd) $ do
          warn $ "Unexpected text in list element: " ++ show (cdData cd)
        return Nothing
      CRef _ -> warn "Unexpected CRef" >> return Nothing
    lift $ put ins{ insideContent = V.empty }
    return elts
  , codecOut = makeOut $ \elts -> Inside
    { insideAttrs   = V.empty
    , insideContent = fmap Elem elts
    }
  }

bareList :: (SendMessage m) => ValueCodec' m Element a -> InsideCodec m (V.Vector a)
bareList cdc = Codec
  { codecIn = do
    elts <- codecIn children
    forM (V.fromList $ zip [0..] $ V.toList elts) $ \(i, e) -> do
      let layer = unwords $ concat
            [ toList (elLine e) >>= \ln -> ["line", show ln]
            , ["list child #" <> show (i :: Int)]
            ]
      inside layer $ mapStackTraceT (lift . (`runReaderT` e)) $ codecIn cdc
  , codecOut = fmapArg $ \xs -> void $ lift (traverse (execWriterT . codecOut cdc) xs) >>= codecOut children
  } where children = childElements

countList :: (SendMessage m) => ValueCodec' m Element a -> InsideCodec m (V.Vector a)
countList cdc = Codec
  { codecIn = do
    mlen <- codecIn $ zoomValue (maybeValue intValue) $ optAttr "count"
    xs <- codecIn bare
    forM_ mlen $ \len -> when (length xs /= len) $ warn $ unwords
      [ "List has count attribute of"
      , show len
      , "but contains"
      , show $ length xs
      , "children"
      ]
    return xs
  , codecOut = fmapArg $ \xs -> do
    tell Inside
      { insideAttrs = V.singleton $ Attr (QName "count" Nothing Nothing) (show $ length xs)
      , insideContent = V.empty
      }
    void $ codecOut bare xs
  } where bare = bareList cdc

childTextRaw :: (Monad m) => InsideCodec m T.Text
childTextRaw = Codec
  { codecIn = do
    ins <- lift get
    let getText = \case Text cd -> Just cd; _ -> Nothing
    lift $ put $ ins { insideContent = V.filter (isNothing . getText) $ insideContent ins }
    return
      $ T.unwords
      $ V.toList
      $ V.filter (not . T.null)
      $ fmap (T.pack . cdData)
      $ V.mapMaybe getText
      $ insideContent ins
  , codecOut = makeOut $ \t -> Inside
    { insideAttrs = V.empty
    , insideContent = do
      guard $ not $ T.null t
      V.singleton $ Text $ CData CDataText (T.unpack t) Nothing
    }
  }

childText :: (Monad m) => InsideCodec m T.Text
childText = dimap T.strip T.strip childTextRaw

zoomValue :: (Monad m) => ValueCodec' m v a -> InsideCodec m v -> InsideCodec m a
zoomValue z cdc = Codec
  { codecIn = codecIn cdc >>= \x -> mapStackTraceT (lift . (`runReaderT` x)) $ codecIn z
  , codecOut = fmapArg $ \x -> void $ lift (execWriterT $ codecOut z x) >>= codecOut cdc
  }

intValue :: (Monad m, Integral a) => ValueCodec' m T.Text a
intValue = Codec
  { codecIn = lift ask >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just i  -> return $ fromInteger i
  , codecOut = fmapArg $ tell . T.pack . show . toInteger
  }

intText :: (Monad m, Integral a) => InsideCodec m T.Text -> InsideCodec m a
intText = zoomValue intValue

maybeValue :: (Monad m) => ValueCodec' m v a -> ValueCodec' m (Maybe v) (Maybe a)
maybeValue cdc = Codec
  { codecIn = lift ask >>= \case
    Nothing -> return Nothing
    Just v  -> fmap Just $ parseFrom v $ codecIn cdc
  , codecOut = fmapArg $ \case
    Nothing -> tell Nothing
    Just a  -> void $ mapWriterT (fmap $ second Just) $ codecOut cdc a
  }

maybeInside :: (Monad m) => InsideCodec m a -> InsideCodec m (Maybe a)
maybeInside cdc = Codec
  { codecIn = lift get >>= \case
    Inside (V.null -> True) (V.null -> True) -> return Nothing
    _                                        -> fmap Just $ codecIn cdc
  , codecOut = fmapArg $ maybe (return ()) (void . codecOut cdc)
  }

boolValue :: (Monad m) => ValueCodec' m T.Text Bool
boolValue = Codec
  { codecIn = lift ask >>= \case
    "0" -> return False
    "1" -> return True
    str -> fatal $ "Expected 0 or 1 (bool), got: " <> show str
  , codecOut = fmapArg $ tell . \b -> if b then "1" else "0"
  }

boolText :: (Monad m) => InsideCodec m T.Text -> InsideCodec m Bool
boolText = zoomValue boolValue

milliValue :: (Monad m, RealFrac a) => ValueCodec' m T.Text a
milliValue = Codec
  { codecIn = lift ask >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected a number to 3 decimal places, got: " <> show str
    Just m  -> return $ realToFrac (m :: Milli)
  , codecOut = fmapArg $ tell . T.pack . show . \n -> realToFrac n :: Milli
  }

milliText :: (Monad m, RealFrac a) => InsideCodec m T.Text -> InsideCodec m a
milliText = zoomValue milliValue

secondsValue :: (Monad m) => ValueCodec' m T.Text U.Seconds
secondsValue = dimap U.fromSeconds U.Seconds milliValue

seconds :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.Seconds
seconds = zoomValue secondsValue

bpm :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.BPS
bpm = dimap ((* 60) . U.fromBPS) (U.BPS . (/ 60)) . milliText

plural' :: (SendMessage m, IsInside a) => ParseName -> ParseName -> InsideCodec m (V.Vector a)
plural' many one = dimap (\xs -> guard (not $ null xs) >> Just xs) (fromMaybe V.empty)
  $ childTagOpt many $ parseInside' $ countList $ isTag one $ parseInside' insideCodec

plural :: (SendMessage m, IsInside a) => ParseName -> InsideCodec m (V.Vector a)
plural one = let
  many = one { pName = pName one <> "s" }
  in plural' many one

flag :: (Monad m) => ParseName -> InsideCodec m Bool
flag s = zoomValue flagValue $ optAttr s

flagValue :: (Monad m) => ValueCodec' m (Maybe T.Text) Bool
flagValue = dimap (\b -> guard b >> Just b) (fromMaybe False) $ maybeValue boolValue

boolWordValue :: (Monad m) => ValueCodec' m T.Text Bool
boolWordValue = Codec
  { codecIn = lift ask >>= \case
    "false" -> return False
    "true"  -> return True
    str     -> fatal $ "Expected true or false, got " <> show str
  , codecOut = fmapArg $ tell . \b -> if b then "true" else "false"
  }

boolWordText :: (Monad m) => InsideCodec m T.Text -> InsideCodec m Bool
boolWordText = zoomValue boolWordValue

pluralBare' :: (SendMessage m, IsInside a) => ParseName -> ParseName -> InsideCodec m (V.Vector a)
pluralBare' many one = dimap (\xs -> guard (not $ null xs) >> Just xs) (fromMaybe V.empty) $ childTagOpt many $ do
  mapM_ (useNamespace Nothing) $ pURI one
  parseInside' $ bareList $ isTag one $ parseInside' insideCodec

pluralBare :: (SendMessage m, IsInside a) => ParseName -> InsideCodec m (V.Vector a)
pluralBare one = let
  many = one { pName = pName one <> "s" }
  in pluralBare' many one
