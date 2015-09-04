{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Config where

import           Audio
import           Control.Applicative        ((<|>))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                  (isDigit)
import           Data.Conduit.Audio         (Duration (..))
import qualified Data.HashMap.Strict        as Map
import           Data.List                  ((\\))
import           Data.Maybe                 (fromMaybe)
import           Data.Scientific            (Scientific, toBoundedInteger,
                                             toRealFloat)
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Data.Vector                as V
import qualified Sound.Jammit.Base          as J
import           StackTrace                 hiding (optional)
import           Text.Read                  (readMaybe)

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

parseFrom :: (Monad m) => v -> Parser m v a -> Parser m v' a
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

data Instrument = Guitar | Bass | Drums | Keys | Vocal
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

jammitInstrument :: Instrument -> J.Instrument
jammitInstrument = \case
  Guitar -> J.Guitar
  Bass   -> J.Bass
  Drums  -> J.Drums
  Keys   -> J.Keyboard
  Vocal  -> J.Vocal

instance TraceJSON Instrument where
  traceJSON = do
    s <- traceJSON
    let _ = s :: String
    case s of
      "guitar" -> return Guitar
      "bass"   -> return Bass
      "drums"  -> return Drums
      "keys"   -> return Keys
      "vocal"  -> return Vocal
      _        -> expected "an instrument name"

data SongYaml = SongYaml
  { _metadata    :: Metadata
  , _audio       :: Map.HashMap T.Text AudioFile
  , _jammit      :: Map.HashMap T.Text JammitTrack
  , _plans       :: Map.HashMap T.Text Plan
  , _instruments :: Instruments
  , _published   :: Bool
  } deriving (Eq, Show, Read)

mapping :: (Monad m) => Parser m A.Value a -> Parser m A.Value (Map.HashMap T.Text a)
mapping p = lift ask >>= \case
  A.Object o -> Map.traverseWithKey (\k x -> inside ("mapping key " ++ show k) $ parseFrom x p) o
  _          -> expected "an object"

instance TraceJSON SongYaml where
  traceJSON = object $ do
    let defaultEmptyMap = fmap $ fromMaybe Map.empty
    _metadata    <- required "metadata" traceJSON
    _audio       <- defaultEmptyMap $ optional "audio"  $ mapping traceJSON
    _jammit      <- defaultEmptyMap $ optional "jammit" $ mapping traceJSON
    _plans       <- defaultEmptyMap $ optional "plans"  $ mapping traceJSON
    _instruments <- required "instruments" traceJSON
    _published   <- fromMaybe True <$> optional "published" traceJSON
    expectedKeys ["metadata", "audio", "jammit", "plans", "instruments", "published"]
    return SongYaml{..}

data Metadata = Metadata
  { _title        :: T.Text
  , _artist       :: T.Text
  , _album        :: T.Text
  , _genre        :: T.Text
  , _subgenre     :: T.Text
  , _year         :: Int
  , _fileAlbumArt :: FilePath
  , _trackNumber  :: Int
  , _fileCountin  :: Maybe FilePath
  , _comments     :: [T.Text]
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON Metadata where
  traceJSON = object $ do
    _title        <- required "title"          traceJSON
    _artist       <- required "artist"         traceJSON
    _album        <- required "album"          traceJSON
    _genre        <- required "genre"          traceJSON
    _subgenre     <- required "subgenre"       traceJSON
    _year         <- required "year"           traceJSON
    _fileAlbumArt <- required "file-album-art" traceJSON
    _trackNumber  <- required "track-number"   traceJSON
    _fileCountin  <- optional "file-countin"   traceJSON
    _comments     <- fromMaybe [] <$> optional "comments" traceJSON
    expectedKeys ["title", "artist", "album", "genre", "subgenre", "year", "file-album-art", "track-number", "file-countin", "comments"]
    return Metadata{..}

data AudioFile = AudioFile
  { _md5    :: Maybe T.Text
  , _frames :: Maybe Integer
  , _name   :: Maybe FilePath
  , _rate   :: Maybe Int
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioFile where
  traceJSON = object $ do
    _md5    <- optional "md5"    traceJSON
    _frames <- optional "frames" traceJSON
    _name   <- optional "name"   traceJSON
    _rate   <- optional "rate"   traceJSON
    expectedKeys ["md5", "frames", "name", "rate"]
    return AudioFile{..}

data JammitTrack = JammitTrack
  { _jammitTitle  :: Maybe T.Text
  , _jammitArtist :: Maybe T.Text
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON JammitTrack where
  traceJSON = object $ do
    _jammitTitle  <- optional "title"  traceJSON
    _jammitArtist <- optional "artist" traceJSON
    expectedKeys ["title", "artist"]
    return JammitTrack{..}

data Plan
  = Plan
    { _song         :: Audio Duration AudioInput
    , _guitar       :: Audio Duration AudioInput
    , _bass         :: Audio Duration AudioInput
    , _keys         :: Audio Duration AudioInput
    , _drums        :: Audio Duration AudioInput
    , _vocal        :: Audio Duration AudioInput
    , _planComments :: [T.Text]
    }
  | EachPlan
    { _each         :: Audio Duration T.Text
    , _planComments :: [T.Text]
    }
  | MoggPlan
    { _moggMD5      :: T.Text
    , _moggGuitar   :: [Int]
    , _moggBass     :: [Int]
    , _moggKeys     :: [Int]
    , _moggDrums    :: [Int]
    , _moggVocal    :: [Int]
    , _pans         :: [Double]
    , _vols         :: [Double]
    , _planComments :: [T.Text]
    , _drumMix      :: Int
    }
  deriving (Eq, Ord, Show, Read)

instance TraceJSON Plan where
  traceJSON = object
    $   do
      _each <- required "each" traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["each", "comments"]
      return EachPlan{..}
    <|> do
      let defaultSilence = fromMaybe $ Silence 2 $ Seconds 1
      _song   <-                    required "song"   traceJSON
      _guitar <- defaultSilence <$> optional "guitar" traceJSON
      _bass   <- defaultSilence <$> optional "bass"   traceJSON
      _keys   <- defaultSilence <$> optional "keys"   traceJSON
      _drums  <- defaultSilence <$> optional "drums"  traceJSON
      _vocal  <- defaultSilence <$> optional "vocal"  traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["song", "guitar", "bass", "keys", "drums", "vocal", "comments"]
      return Plan{..}
    <|> do
      _moggMD5 <- required "mogg-md5" traceJSON
      _moggGuitar <- fromMaybe [] <$> optional "guitar" traceJSON
      _moggBass   <- fromMaybe [] <$> optional "bass" traceJSON
      _moggKeys   <- fromMaybe [] <$> optional "keys" traceJSON
      _moggDrums  <- fromMaybe [] <$> optional "drums" traceJSON
      _moggVocal  <- fromMaybe [] <$> optional "vocal" traceJSON
      _pans <- required "pans" traceJSON
      _vols <- required "vols" traceJSON
      _drumMix <- required "drum-mix" traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["mogg-md5", "guitar", "bass", "keys", "drums", "vocal", "pans", "vols", "drum-mix", "comments"]
      return MoggPlan{..}

data AudioInput
  = Named T.Text
  | JammitSelect J.AudioPart T.Text
  deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioInput where
  traceJSON
    =   do
      Named <$> traceJSON
    <|> do
      algebraic2 "only"
        (\part str -> JammitSelect (J.Only part) str)
        (traceJSON >>= \title -> case J.titleToPart title of
          Just part -> return part
          Nothing   -> expected "a Jammit part name"
          )
        traceJSON
    <|> do
      algebraic2 "without"
        (\inst str -> JammitSelect (J.Without $ jammitInstrument inst) str)
        traceJSON
        traceJSON

instance TraceJSON Edge where
  traceJSON = do
    s <- traceJSON
    let _ = s :: String
    case s of
      "start" -> return Start
      "begin" -> return Start
      "end"   -> return End
      _       -> expected "an audio edge"

algebraic1 :: (Monad m) => T.Text -> (a -> b) -> Parser m A.Value a -> Parser m A.Value b
algebraic1 k f p1 = object $ theKey k $ do
  x <- lift ask
  fmap f $ inside "ADT field 1 of 1" $ parseFrom x p1

algebraic2 :: (Monad m) => T.Text -> (a -> b -> c) ->
  Parser m A.Value a -> Parser m A.Value b -> Parser m A.Value c
algebraic2 k f p1 p2 = object $ theKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y] -> f
      <$> do inside "ADT field 1 of 2" $ parseFrom x p1
      <*> do inside "ADT field 2 of 2" $ parseFrom y p2
    _ -> expected "an array of 2 ADT fields"
  _ -> expected "an array of 2 ADT fields"

algebraic3 :: (Monad m) => T.Text -> (a -> b -> c -> d) ->
  Parser m A.Value a -> Parser m A.Value b -> Parser m A.Value c -> Parser m A.Value d
algebraic3 k f p1 p2 p3 = object $ theKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y, z] -> f
      <$> do inside "ADT field 1 of 3" $ parseFrom x p1
      <*> do inside "ADT field 2 of 3" $ parseFrom y p2
      <*> do inside "ADT field 3 of 3" $ parseFrom z p3
    _ -> expected "an array of 3 ADT fields"
  _ -> expected "an array of 3 ADT fields"

instance (TraceJSON t, TraceJSON a) => TraceJSON (Audio t a) where
  traceJSON
    =   do algebraic2 "silence" Silence traceJSON traceJSON
    <|> do object $ theKey "mix"         $ Mix         <$> traceJSON
    <|> do object $ theKey "merge"       $ Merge       <$> traceJSON
    <|> do object $ theKey "concatenate" $ Concatenate <$> traceJSON
    <|> do algebraic2 "gain" Gain traceJSON traceJSON
    <|> do supplyEdge "take" Take
    <|> do supplyEdge "drop" Drop
    <|> do supplyEdge "trim" Drop
    <|> do supplyEdge "fade" Fade
    <|> do supplyEdge "pad"  Pad
    <|> do algebraic1 "resample" Resample traceJSON
    <|> do algebraic2 "channels" Channels traceJSON traceJSON
    <|> do Input <$> traceJSON
    where supplyEdge s f
            =   do algebraic2 s (f Start   ) traceJSON traceJSON
            <|> do algebraic3 s  f traceJSON traceJSON traceJSON

instance TraceJSON Duration where
  traceJSON
    =   do
      object $ theKey "frames" $ Frames <$> traceJSON
    <|> do
      object $ theKey "seconds" $ Seconds <$> parseMinutes
    <|> do
      Seconds <$> parseMinutes
    where parseMinutes = do
            v <- lift ask
            case v of
              A.String minstr
                | (minutes@(_:_), ':' : secstr) <- span isDigit $ T.unpack minstr
                , Just seconds <- readMaybe secstr
                -> return $ read minutes * 60 + seconds
              A.String secstr
                | Just seconds <- readMaybe $ T.unpack secstr
                -> return seconds
              _ -> traceJSON -- will succeed if JSON number

data Instruments = Instruments
  { _hasDrums  :: Bool
  , _hasGuitar :: Bool
  , _hasBass   :: Bool
  , _hasKeys   :: Bool
  , _hasVocal  :: VocalCount
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON Instruments where
  traceJSON = object $ do
    _hasDrums  <- fromMaybe False  <$> optional "drums"  traceJSON
    _hasGuitar <- fromMaybe False  <$> optional "guitar" traceJSON
    _hasBass   <- fromMaybe False  <$> optional "bass"   traceJSON
    _hasKeys   <- fromMaybe False  <$> optional "keys"   traceJSON
    _hasVocal  <- fromMaybe Vocal0 <$> optional "vocal"  traceJSON
    return Instruments{..}

data VocalCount = Vocal0 | Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON VocalCount where
  traceJSON = lift ask >>= \case
    A.Bool False -> return Vocal0
    A.Bool True -> return Vocal1
    A.Number 0 -> return Vocal0
    A.Number 1 -> return Vocal1
    A.Number 2 -> return Vocal2
    A.Number 3 -> return Vocal3
    _          -> expected "a vocal part count (0 to 3)"
