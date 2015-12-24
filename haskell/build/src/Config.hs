{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE ViewPatterns      #-}
module Config where

import           Audio
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Char                  (isDigit)
import           Data.Conduit.Audio         (Duration (..))
import qualified Data.DTA.Serialize.Magma   as Magma
import qualified Data.HashMap.Strict        as Map
import           Data.List                  ((\\))
import Data.Monoid ((<>))
import           Data.Maybe                 (fromMaybe)
import           Data.Scientific            (Scientific, toBoundedInteger,
                                             toRealFloat)
import qualified Data.Text                  as T
import           Data.Traversable
import qualified Data.Vector                as V
import qualified Sound.Jammit.Base          as J
import           Control.Monad.Trans.StackTrace hiding (optional)
import           Text.Read                  (readMaybe)
import           RockBand.Common            (Key(..))

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

keyNames :: [(T.Text, Key)]
keyNames = let
  keys = [C,D,E,F,G,A,B]
  letter = T.toLower . T.pack . show
  numKeys = fromEnum (maxBound :: Key) + 1
  in   [(letter k, k) | k <- keys]
    ++ [(letter k <> " flat" , toEnum $ (fromEnum k - 1) `mod` numKeys) | k <- keys]
    ++ [(letter k <> " sharp", toEnum $ (fromEnum k + 1) `mod` numKeys) | k <- keys]

instance TraceJSON Key where
  traceJSON = traceJSON >>= \t -> case lookup (T.toLower t) keyNames of
    Just k -> return k
    Nothing -> expected "the name of a pitch"

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
  traceJSON = traceJSON >>= \s -> case s :: String of
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
  , _vocalGender  :: Maybe Magma.Gender
  , _difficulty   :: Difficulties
  , _key          :: Maybe Key
  , _autogenTheme :: AutogenTheme
  , _author       :: T.Text
  , _rating       :: Rating
  , _drumKit      :: DrumKit
  , _auto2xBass   :: Bool
  } deriving (Eq, Ord, Show, Read)

data Difficulties = Difficulties
  { _difficultyDrums   :: Maybe Difficulty
  , _difficultyGuitar  :: Maybe Difficulty
  , _difficultyBass    :: Maybe Difficulty
  , _difficultyKeys    :: Maybe Difficulty
  , _difficultyProKeys :: Maybe Difficulty
  , _difficultyVocal   :: Maybe Difficulty
  , _difficultyBand    :: Maybe Difficulty
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON Difficulties where
  traceJSON = object $ do
    _difficultyDrums   <- optional "drums"    traceJSON
    _difficultyGuitar  <- optional "guitar"   traceJSON
    _difficultyBass    <- optional "bass"     traceJSON
    _difficultyKeys    <- optional "keys"     traceJSON
    _difficultyProKeys <- optional "pro-keys" traceJSON
    _difficultyVocal   <- optional "vocal"    traceJSON
    _difficultyBand    <- optional "band"     traceJSON
    expectedKeys ["drums", "guitar", "bass", "keys", "pro-keys", "vocal", "band"]
    return Difficulties{..}

data Difficulty
  = Tier Integer -- ^ [1..7]: 1 = no dots, 7 = devil dots
  | Rank Integer -- ^ [1..]
  deriving (Eq, Ord, Show, Read)

pattern OneKey k v <- A.Object (Map.toList -> [(k, v)])

instance TraceJSON Difficulty where
  traceJSON = lift ask >>= \case
    OneKey "tier" (A.Number n) -> return $ Tier $ round n
    OneKey "rank" (A.Number n) -> return $ Rank $ round n
    A.Number n -> return $ Tier $ round n
    _ -> expected "a difficulty value (tier or rank)"

instance TraceJSON Magma.Gender where
  traceJSON = lift ask >>= \case
    A.String "female" -> return Magma.Female
    A.String "male"   -> return Magma.Male
    _                 -> expected "a gender (male or female)"

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
    _vocalGender  <- optional "vocal-gender"   traceJSON
    let emptyDiffs = Difficulties Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    _difficulty   <- fromMaybe emptyDiffs <$> optional "difficulty" traceJSON
    _key          <- optional "key" traceJSON
    _comments     <- fromMaybe [] <$> optional "comments" traceJSON
    _autogenTheme <- fromMaybe AutogenDefault <$> optional "autogen-theme" traceJSON
    _author       <- fromMaybe "Onyxite" <$> optional "author" traceJSON
    _rating       <- fromMaybe Unrated <$> optional "rating" traceJSON
    _drumKit      <- fromMaybe HardRockKit <$> optional "drum-kit" traceJSON
    _auto2xBass   <- fromMaybe True <$> optional "auto-2x-bass" traceJSON
    expectedKeys
      [ "title", "artist", "album", "genre", "subgenre", "year"
      , "file-album-art", "track-number", "file-countin", "comments", "vocal-gender"
      , "difficulty", "key", "autogen-theme", "author", "rating", "drum-kit", "auto-2x-bass"
      ]
    return Metadata{..}

data AudioFile = AudioFile
  { _md5      :: Maybe T.Text
  , _frames   :: Maybe Integer
  , _name     :: Maybe FilePath
  , _rate     :: Maybe Int
  , _channels :: Int
  } deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioFile where
  traceJSON = object $ do
    _md5      <- optional "md5"    traceJSON
    _frames   <- optional "frames" traceJSON
    _name     <- optional "name"   traceJSON
    _rate     <- optional "rate"   traceJSON
    _channels <- fromMaybe 2 <$> optional "channels" traceJSON
    expectedKeys ["md5", "frames", "name", "rate", "channels"]
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
  traceJSON = decideKey
    [ ("each", object $ do
      _each <- required "each" traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["each", "comments"]
      return EachPlan{..}
      )
    , ("song", object $ do
      let defaultSilence = fromMaybe $ Silence 2 $ Frames 0
      _song   <-                    required "song"   traceJSON
      _guitar <- defaultSilence <$> optional "guitar" traceJSON
      _bass   <- defaultSilence <$> optional "bass"   traceJSON
      _keys   <- defaultSilence <$> optional "keys"   traceJSON
      _drums  <- defaultSilence <$> optional "drums"  traceJSON
      _vocal  <- defaultSilence <$> optional "vocal"  traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["song", "guitar", "bass", "keys", "drums", "vocal", "comments"]
      return Plan{..}
      )
    , ("mogg-md5", object $ do
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
      )
    ] (expected "an object with one of the keys \"each\", \"song\", or \"mogg-md5\"")

data AudioInput
  = Named T.Text
  | JammitSelect J.AudioPart T.Text
  deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioInput where
  traceJSON = decideKey
    [ ("only", do
      algebraic2 "only"
        (\part str -> JammitSelect (J.Only part) str)
        (traceJSON >>= \title -> case J.titleToPart title of
          Just part -> return part
          Nothing   -> expected "a Jammit part name"
          )
        traceJSON
      )
    , ("without", do
      algebraic2 "without"
        (\inst str -> JammitSelect (J.Without $ jammitInstrument inst) str)
        traceJSON
        traceJSON
      )
    ] (Named <$> traceJSON)

instance TraceJSON Edge where
  traceJSON = lift ask >>= \case
    A.String "start" -> return Start
    A.String "begin" -> return Start
    A.String "end"   -> return End
    _                -> expected "an audio edge (start or end)"

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

decideKey :: (Monad m) => [(T.Text, Parser m A.Value a)] -> Parser m A.Value a -> Parser m A.Value a
decideKey opts def = lift ask >>= \case
  A.Object hm -> case [ p | (k, p) <- opts, Map.member k hm ] of
    p : _ -> p
    [] -> def
  _ -> def

instance (TraceJSON t, TraceJSON a) => TraceJSON (Audio t a) where
  traceJSON = decideKey
    [ ("silence", algebraic2 "silence" Silence traceJSON traceJSON)
    , ("mix"        , object $ theKey "mix"         $ Mix         <$> traceJSON)
    , ("merge"      , object $ theKey "merge"       $ Merge       <$> traceJSON)
    , ("concatenate", object $ theKey "concatenate" $ Concatenate <$> traceJSON)
    , ("gain", algebraic2 "gain" Gain traceJSON traceJSON)
    , ("take", supplyEdge "take" Take)
    , ("drop", supplyEdge "drop" Drop)
    , ("trim", supplyEdge "trim" Drop)
    , ("fade", supplyEdge "fade" Fade)
    , ("pad" , supplyEdge "pad"  Pad )
    , ("resample", algebraic1 "resample" Resample traceJSON)
    , ("channels", algebraic2 "channels" Channels traceJSON traceJSON)
    ] (fmap Input traceJSON `catch` \_ -> expected "an audio expression")
    where supplyEdge s f = lift ask >>= \case
            OneKey _ (A.Array v)
              | V.length v == 2 -> algebraic2 s (f Start   ) traceJSON traceJSON
              | V.length v == 3 -> algebraic3 s  f traceJSON traceJSON traceJSON
            _ -> expected $ "2 or 3 fields in the " ++ show s ++ " ADT"

instance TraceJSON Duration where
  traceJSON = lift ask >>= \case
    OneKey "frames" v -> inside "frames duration" $ Frames <$> parseFrom v traceJSON
    OneKey "seconds" v -> inside "seconds duration" $ Seconds <$> parseFrom v parseMinutes
    _ -> (inside "unitless (seconds) duration" $ Seconds <$> parseMinutes)
      `catch` \_ -> expected "a duration in frames or seconds"
    where parseMinutes = lift ask >>= \case
            A.String minstr
              | (minutes@(_:_), ':' : secstr) <- span isDigit $ T.unpack minstr
              , Just seconds <- readMaybe secstr
              -> return $ read minutes * 60 + seconds
            A.String secstr
              | Just seconds <- readMaybe $ T.unpack secstr
              -> return seconds
            _ -> traceJSON -- will succeed if JSON number

data Instruments = Instruments
  { _hasDrums   :: Bool
  , _hasGuitar  :: Bool
  , _hasBass    :: Bool
  , _hasKeys    :: Bool
  , _hasProKeys :: Bool
  , _hasVocal   :: VocalCount
  } deriving (Eq, Ord, Show, Read)

hasAnyKeys :: Instruments -> Bool
hasAnyKeys insts = _hasKeys insts || _hasProKeys insts

instance TraceJSON Instruments where
  traceJSON = object $ do
    _hasDrums   <- fromMaybe False  <$> optional "drums"    traceJSON
    _hasGuitar  <- fromMaybe False  <$> optional "guitar"   traceJSON
    _hasBass    <- fromMaybe False  <$> optional "bass"     traceJSON
    _hasKeys    <- fromMaybe False  <$> optional "keys"     traceJSON
    _hasProKeys <- fromMaybe False  <$> optional "pro-keys" traceJSON
    _hasVocal   <- fromMaybe Vocal0 <$> optional "vocal"    traceJSON
    expectedKeys ["drums", "guitar", "bass", "keys", "pro-keys", "vocal"]
    return Instruments{..}

data VocalCount = Vocal0 | Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON VocalCount where
  traceJSON = lift ask >>= \case
    A.Bool False -> return Vocal0
    A.Bool True  -> return Vocal1
    A.Number 0   -> return Vocal0
    A.Number 1   -> return Vocal1
    A.Number 2   -> return Vocal2
    A.Number 3   -> return Vocal3
    _            -> expected "a vocal part count (0 to 3)"

data AutogenTheme
  = AutogenDefault
  | AggressiveMetal
  | ArenaRock
  | DarkHeavyRock
  | DustyVintage
  | EdgyProgRock
  | FeelGoodPopRock
  | GaragePunkRock
  | PsychJamRock
  | SlowJam
  | SynthPop
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON AutogenTheme where
  traceJSON = lift ask >>= \case
    A.Null             -> return AutogenDefault
    A.String "Default" -> return AutogenDefault
    A.String t         -> case readMaybe $ filter (/= ' ') $ T.unpack t of
      Just theme -> return theme
      Nothing    -> expected "the name of an autogen theme or null"
    _                  -> expected "the name of an autogen theme or null"

data Rating
  = FamilyFriendly
  | SupervisionRecommended
  | Mature
  | Unrated
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON Rating where
  traceJSON = lift ask >>= \case
    A.Null     -> return Unrated
    A.String t -> case T.filter (/= ' ') t of
      "FF"                     -> return FamilyFriendly
      "SR"                     -> return SupervisionRecommended
      "M"                      -> return Mature
      "UR"                     -> return Unrated
      "FamilyFriendly"         -> return FamilyFriendly
      "SupervisionRecommended" -> return SupervisionRecommended
      "Mature"                 -> return Mature
      "Unrated"                -> return Unrated
      _ -> expected "a valid content rating or null"
    _          -> expected "a valid content rating or null"

data DrumKit
  = HardRockKit
  | ArenaKit
  | VintageKit
  | TrashyKit
  | ElectronicKit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON DrumKit where
  traceJSON = lift ask >>= \case
    A.Null     -> return HardRockKit
    A.String t         -> case readMaybe $ filter (/= ' ') $ T.unpack t of
      Just kit -> return kit
      Nothing    -> expected "the name of a drum kit or null"
    _                  -> expected "the name of a drum kit or null"
