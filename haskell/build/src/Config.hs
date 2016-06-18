{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
module Config where

import           Audio
import           Control.Monad                  (when)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace hiding (optional)
import qualified Data.Aeson                     as A
import           Data.Aeson                     ((.=))
import qualified Data.ByteString.Lazy.Char8     as BL8
import           Data.Char                      (isDigit, isSpace)
import           Data.Conduit.Audio             (Duration (..))
import qualified Data.DTA.Serialize.Magma       as Magma
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as Map
import           Data.String                    (IsString)
import           Data.List                      ((\\))
import           Data.Maybe                     (fromMaybe, isNothing)
import           Data.Monoid                    ((<>))
import           Data.Scientific                (Scientific, toBoundedInteger,
                                                 toRealFloat)
import qualified Data.Text                      as T
import           Data.Traversable
import qualified Data.Vector                    as V
import           Genre                          (defaultSubgenre)
import           RockBand.Common                (Key (..))
import qualified RockBand.Drums                 as Drums
import qualified Sound.Jammit.Base              as J
import qualified Sound.MIDI.Util                as U
import           Text.Read                      (readMaybe)

import qualified Text.ParserCombinators.ReadP   as ReadP
import qualified Text.Read.Lex                  as Lex

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

instance A.ToJSON Key where
  toJSON = \case
    C  -> "C"
    Cs -> "C sharp"
    D  -> "D"
    Ds -> "D sharp"
    E  -> "E"
    F  -> "F"
    Fs -> "F sharp"
    G  -> "G"
    Gs -> "G sharp"
    A  -> "A"
    As -> "A sharp"
    B  -> "B"

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

fromJammitInstrument :: J.Instrument -> Instrument
fromJammitInstrument = \case
  J.Guitar   -> Guitar
  J.Bass     -> Bass
  J.Drums    -> Drums
  J.Keyboard -> Keys
  J.Vocal    -> Vocal

instance TraceJSON Instrument where
  traceJSON = traceJSON >>= \s -> case s :: String of
    "guitar" -> return Guitar
    "bass"   -> return Bass
    "drums"  -> return Drums
    "keys"   -> return Keys
    "vocal"  -> return Vocal
    _        -> expected "an instrument name"

instance A.ToJSON Instrument where
  toJSON = \case
    Guitar -> "guitar"
    Bass   -> "bass"
    Drums  -> "drums"
    Keys   -> "keys"
    Vocal  -> "vocal"

data SongYaml = SongYaml
  { _metadata    :: Metadata
  , _audio       :: Map.HashMap T.Text AudioFile
  , _jammit      :: Map.HashMap T.Text JammitTrack
  , _plans       :: Map.HashMap T.Text Plan
  , _instruments :: Instruments
  , _published   :: Bool
  } deriving (Eq, Show)

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

instance A.ToJSON SongYaml where
  toJSON SongYaml{..} = A.object
    [ "metadata" .= _metadata
    , "audio" .= A.Object (fmap A.toJSON _audio)
    , "jammit" .= A.Object (fmap A.toJSON _jammit)
    , "plans" .= A.Object (fmap A.toJSON _plans)
    , "instruments" .= _instruments
    , "published" .= _published
    ]

data Metadata = Metadata
  { _title        :: T.Text
  , _artist       :: T.Text
  , _album        :: T.Text
  , _genre        :: T.Text
  , _subgenre     :: T.Text
  , _year         :: Int
  , _fileAlbumArt :: Maybe FilePath
  , _trackNumber  :: Int
  , _comments     :: [T.Text]
  , _vocalGender  :: Maybe Magma.Gender
  , _difficulty   :: Difficulties
  , _key          :: Maybe Key
  , _autogenTheme :: AutogenTheme
  , _author       :: T.Text
  , _rating       :: Rating
  , _drumKit      :: DrumKit
  , _auto2xBass   :: Bool
  , _hopoThreshold :: Int
  , _previewStart :: Maybe Double
  , _previewEnd   :: Maybe Double
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

instance A.ToJSON Difficulties where
  toJSON Difficulties{..} = A.object $ concat
    [ map ("drums"    .=) $ toList _difficultyDrums
    , map ("guitar"   .=) $ toList _difficultyGuitar
    , map ("bass"     .=) $ toList _difficultyBass
    , map ("keys"     .=) $ toList _difficultyKeys
    , map ("pro-keys" .=) $ toList _difficultyProKeys
    , map ("vocal"    .=) $ toList _difficultyVocal
    , map ("band"     .=) $ toList _difficultyBand
    ]

data Difficulty
  = Tier Integer -- ^ [1..7]: 1 = no dots, 7 = devil dots
  | Rank Integer -- ^ [1..]
  deriving (Eq, Ord, Show, Read)

pattern OneKey :: T.Text -> A.Value -> A.Value
pattern OneKey k v <- A.Object (Map.toList -> [(k, v)]) where
  OneKey k v = A.Object $ Map.fromList [(k, v)]

instance TraceJSON Difficulty where
  traceJSON = lift ask >>= \case
    OneKey "tier" (A.Number n) -> return $ Tier $ round n
    OneKey "rank" (A.Number n) -> return $ Rank $ round n
    A.Number n -> return $ Tier $ round n
    _ -> expected "a difficulty value (tier or rank)"

instance A.ToJSON Difficulty where
  toJSON = \case
    Tier i -> A.object ["tier" .= i]
    Rank i -> A.object ["rank" .= i]

instance TraceJSON Magma.Gender where
  traceJSON = lift ask >>= \case
    A.String "female" -> return Magma.Female
    A.String "male"   -> return Magma.Male
    _                 -> expected "a gender (male or female)"

instance A.ToJSON Magma.Gender where
  toJSON = \case
    Magma.Female -> "female"
    Magma.Male   -> "male"

fromMaybe' :: (Eq s, IsString s) => s -> Maybe s -> s
fromMaybe' s ms = case ms of
  Nothing -> s
  Just "" -> s
  Just s' -> s'

instance TraceJSON Metadata where
  traceJSON = object $ do
    let s `orWarn` (w, v) = optional s traceJSON >>= \case
          Nothing -> warn w >> return v
          Just x  -> return x
    _title        <- "title" `orWarn` ("Song has no title", "Untitled")
    _artist       <- "artist" `orWarn` ("Song has no artist", "Unknown Artist")
    _album        <- "album" `orWarn` ("Song has no album", "Unknown Album")
    _genre        <- "genre" `orWarn` ("Song has no genre", "other")
    _subgenre     <- "subgenre" `orWarn` ("Song has no subgenre", defaultSubgenre _genre)
    _year         <- "year" `orWarn` ("Song has no release year", 1900)
    _fileAlbumArt <- optional "file-album-art" traceJSON
    when (isNothing _fileAlbumArt) $ warn "Song has no album art"
    _trackNumber  <- "track-number" `orWarn` ("Song has no track number", 0)
    _vocalGender  <- optional "vocal-gender"   traceJSON
    let emptyDiffs = Difficulties Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    _difficulty   <- fromMaybe emptyDiffs <$> optional "difficulty" traceJSON
    _key          <- optional "key" traceJSON
    _comments     <- fromMaybe [] <$> optional "comments" traceJSON
    _autogenTheme <- fromMaybe AutogenDefault <$> optional "autogen-theme" traceJSON
    _author       <- "author" `orWarn` ("Song has no author", "Unknown Author")
    _rating       <- fromMaybe Unrated <$> optional "rating" traceJSON
    _drumKit      <- fromMaybe HardRockKit <$> optional "drum-kit" traceJSON
    _auto2xBass   <- fromMaybe True <$> optional "auto-2x-bass" traceJSON
    _hopoThreshold <- fromMaybe 170 <$> optional "hopo-threshold" traceJSON
    let traceDurationSecs = flip fmap traceJSON $ \case
          Frames  f -> fromIntegral f / 44100
          Seconds s -> s
    _previewStart <- optional "preview-start" traceDurationSecs
    _previewEnd   <- optional "preview-end" traceDurationSecs
    expectedKeys
      [ "title", "artist", "album", "genre", "subgenre", "year"
      , "file-album-art", "track-number", "comments", "vocal-gender"
      , "difficulty", "key", "autogen-theme", "author", "rating", "drum-kit", "auto-2x-bass", "hopo-threshold"
      , "preview-start", "preview-end"
      ]
    return Metadata{..}

instance A.ToJSON Metadata where
  toJSON Metadata{..} = A.object $ concat
    [ ["title" .= _title]
    , ["artist" .= _artist]
    , ["album" .= _album]
    , ["genre" .= _genre]
    , ["subgenre" .= _subgenre]
    , ["year" .= _year]
    , map ("file-album-art" .=) $ toList _fileAlbumArt
    , ["track-number" .= _trackNumber]
    , case _comments of
      [] -> []
      _  -> ["comments" .= _comments]
    , map ("vocal-gender" .=) $ toList _vocalGender
    , ["difficulty" .= _difficulty]
    , map ("key" .=) $ toList _key
    , ["autogen-theme" .= _autogenTheme]
    , ["author" .= _author]
    , ["rating" .= _rating]
    , ["drum-kit" .= _drumKit]
    , ["auto-2x-bass" .= _auto2xBass]
    , ["hopo-threshold" .= _hopoThreshold]
    , map ("preview-start" .=) $ toList _previewStart
    , map ("preview-end" .=) $ toList _previewEnd
    ]

data AudioFile
  = AudioFile
    { _md5      :: Maybe T.Text
    , _frames   :: Maybe Integer
    , _filePath :: Maybe FilePath
    , _commands :: [String]
    , _rate     :: Maybe Int
    , _channels :: Int
    }
  | AudioSnippet
    { _expr     :: Audio Duration AudioInput
    }
  deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioFile where
  traceJSON = decideKey
    [ ("expr", object $ do
      _expr <- required "expr" traceJSON
      expectedKeys ["expr"]
      return AudioSnippet{..}
      )
    ] $ object $ do
      _md5      <- optional "md5"       traceJSON
      _frames   <- optional "frames"    traceJSON
      _filePath <- optional "file-path" traceJSON
      _commands <- fromMaybe [] <$> optional "commands" traceJSON
      _rate     <- optional "rate"      traceJSON
      _channels <- fromMaybe 2 <$> optional "channels" traceJSON
      expectedKeys ["md5", "frames", "file-path", "commands", "rate", "channels"]
      return AudioFile{..}

instance A.ToJSON AudioFile where
  toJSON AudioFile{..} = A.object $ concat
    [ map ("md5"       .=) $ toList _md5
    , map ("frames"    .=) $ toList _frames
    , map ("file-path" .=) $ toList _filePath
    , ["commands" .= _commands | not $ null _commands]
    , map ("rate"      .=) $ toList _rate
    , ["channels"      .= _channels]
    ]
  toJSON AudioSnippet{..} = A.object
    [ "expr" .= A.toJSON _expr
    ]

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

instance A.ToJSON JammitTrack where
  toJSON JammitTrack{..} = A.object $ concat
    [ map ("title" .=) $ toList _jammitTitle
    , map ("artist" .=) $ toList _jammitArtist
    ]

data PlanAudio t a = PlanAudio
  { _planExpr :: Audio t a
  , _planPans :: [Double]
  , _planVols :: [Double]
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (TraceJSON t, TraceJSON a) => TraceJSON (PlanAudio t a) where
  traceJSON = decideKey
    [ ("expr", object $ do
      _planExpr <- required "expr" traceJSON
      _planPans <- fromMaybe [] <$> optional "pans" traceJSON
      _planVols <- fromMaybe [] <$> optional "vols" traceJSON
      expectedKeys ["expr", "pans", "vols"]
      return PlanAudio{..}
      )
    ] $ (\expr -> PlanAudio expr [] []) <$> traceJSON

instance (A.ToJSON t, A.ToJSON a) => A.ToJSON (PlanAudio t a) where
  toJSON (PlanAudio expr [] []) = A.toJSON expr
  toJSON PlanAudio{..} = A.object
    [ "expr" .= _planExpr
    , "pans" .= _planPans
    , "vols" .= _planVols
    ]

newtype Countin = Countin [(Either U.MeasureBeats U.Seconds, Audio Duration AudioInput)]
  deriving (Eq, Ord, Show)

data Plan
  = Plan
    { _song         :: Maybe (PlanAudio Duration AudioInput)
    , _countin      :: Countin
    , _guitar       :: Maybe (PlanAudio Duration AudioInput)
    , _bass         :: Maybe (PlanAudio Duration AudioInput)
    , _keys         :: Maybe (PlanAudio Duration AudioInput)
    , _kick         :: Maybe (PlanAudio Duration AudioInput)
    , _snare        :: Maybe (PlanAudio Duration AudioInput)
    , _drums        :: Maybe (PlanAudio Duration AudioInput)
    , _vocal        :: Maybe (PlanAudio Duration AudioInput)
    , _crowd        :: Maybe (PlanAudio Duration AudioInput)
    , _planComments :: [T.Text]
    }
  | EachPlan
    { _each         :: PlanAudio Duration T.Text
    , _countin      :: Countin
    , _planComments :: [T.Text]
    }
  | MoggPlan
    { _moggMD5      :: T.Text
    , _moggGuitar   :: [Int]
    , _moggBass     :: [Int]
    , _moggKeys     :: [Int]
    , _moggDrums    :: [Int]
    , _moggVocal    :: [Int]
    , _moggCrowd    :: [Int]
    , _pans         :: [Double]
    , _vols         :: [Double]
    , _planComments :: [T.Text]
    , _drumMix      :: Drums.Audio
    }
  deriving (Eq, Ord, Show)

instance TraceJSON Countin where
  traceJSON = do
    hm <- mapping traceJSON
    fmap Countin $ forM (Map.toList hm) $ \(k, v) -> (, v) <$> parseFrom k parseCountinTime

-- | Parses any of \"measure|beats\", \"seconds\", or \"minutes:seconds\".
parseCountinTime :: (Monad m) => Parser m T.Text (Either U.MeasureBeats U.Seconds)
parseCountinTime = do
  t <- lift ask
  inside ("Countin timestamp " ++ show t)
    $             fmap Left                 parseMeasureBeats
    `catch` \_ -> fmap (Right . realToFrac) (parseFrom (A.String t) parseMinutes)
    `catch` \_ -> parseFrom (A.String t) $ expected "a timestamp in measure|beats, seconds, or minutes:seconds"

parseMeasureBeats :: (Monad m) => Parser m T.Text U.MeasureBeats
parseMeasureBeats = lift ask >>= \t -> let
  parser :: ReadP.ReadP U.MeasureBeats
  parser = do
    measures <- Lex.readDecP
    _ <- ReadP.char '|'
    beats <- parseDecimal ReadP.+++ parsePlusFrac
    return (measures, beats)
  parseDecimal, parsePlusFrac :: ReadP.ReadP U.Beats
  parseDecimal = Lex.lex >>= \case
    Lex.Number n -> return $ realToFrac $ Lex.numberToRational n
    _ -> ReadP.pfail
  parsePlusFrac = do
    a <- Lex.readDecP
    _ <- ReadP.char '+'
    _ <- ReadP.char '('
    b <- Lex.readDecP
    _ <- ReadP.char '/'
    c <- Lex.readDecP
    _ <- ReadP.char ')'
    return $ a + (b / c)
  in case map fst $ filter (all isSpace . snd) $ ReadP.readP_to_S parser $ T.unpack t of
    mb : _ -> return mb
    []     -> fatal "Couldn't parse as measure|beats"

instance A.ToJSON Countin where
  toJSON (Countin pairs) = A.Object $ Map.fromList $ flip map pairs $ \(t, v) -> let
    k = case t of
      Left (msr, bts) -> T.pack $ show msr ++ "|" ++ show (realToFrac bts :: Scientific)
      Right secs -> T.pack $ show (realToFrac secs :: Milli)
    in (k, A.toJSON v)

instance TraceJSON Plan where
  traceJSON = decideKey
    [ ("each", object $ do
      _each <- required "each" traceJSON
      _countin <- fromMaybe (Countin []) <$> optional "countin" traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["each", "countin", "comments"]
      return EachPlan{..}
      )
    , ("mogg-md5", object $ do
      _moggMD5 <- required "mogg-md5" traceJSON
      _moggGuitar <- fromMaybe [] <$> optional "guitar" traceJSON
      _moggBass   <- fromMaybe [] <$> optional "bass" traceJSON
      _moggKeys   <- fromMaybe [] <$> optional "keys" traceJSON
      _moggDrums  <- fromMaybe [] <$> optional "drums" traceJSON
      _moggVocal  <- fromMaybe [] <$> optional "vocal" traceJSON
      _moggCrowd  <- fromMaybe [] <$> optional "crowd" traceJSON
      _pans <- required "pans" traceJSON
      _vols <- required "vols" traceJSON
      _drumMix <- required "drum-mix" traceJSON
      _planComments <- fromMaybe [] <$> optional "comments" traceJSON
      expectedKeys ["mogg-md5", "guitar", "bass", "keys", "drums", "vocal", "crowd", "pans", "vols", "drum-mix", "comments"]
      return MoggPlan{..}
      )
    , ("song", normalPlan)
    , ("guitar", normalPlan)
    , ("bass", normalPlan)
    , ("keys", normalPlan)
    , ("kick", normalPlan)
    , ("snare", normalPlan)
    , ("drums", normalPlan)
    , ("vocal", normalPlan)
    , ("crowd", normalPlan)
    ] (expected "an instrument to audio plan (standard, each, or mogg)")
    where normalPlan = object $ do
            _song    <- optional "song"    traceJSON
            _countin <- fromMaybe (Countin []) <$> optional "countin" traceJSON
            _guitar  <- optional "guitar"  traceJSON
            _bass    <- optional "bass"    traceJSON
            _keys    <- optional "keys"    traceJSON
            _kick    <- optional "kick"    traceJSON
            _snare   <- optional "snare"   traceJSON
            _drums   <- optional "drums"   traceJSON
            _vocal   <- optional "vocal"   traceJSON
            _crowd   <- optional "crowd"   traceJSON
            _planComments <- fromMaybe [] <$> optional "comments" traceJSON
            expectedKeys ["song", "countin", "guitar", "bass", "keys", "kick", "snare", "drums", "vocal", "crowd", "comments"]
            return Plan{..}

instance A.ToJSON Plan where
  toJSON = \case
    Plan{..} -> A.object $ concat
      [ map ("song" .=) $ toList _song
      , ["countin" .= _countin | _countin /= Countin []]
      , map ("guitar" .=) $ toList _guitar
      , map ("bass" .=) $ toList _bass
      , map ("keys" .=) $ toList _keys
      , map ("kick" .=) $ toList _kick
      , map ("snare" .=) $ toList _snare
      , map ("drums" .=) $ toList _drums
      , map ("vocal" .=) $ toList _vocal
      , map ("crowd" .=) $ toList _crowd
      , ["comments" .= _planComments | not $ null _planComments]
      ]
    EachPlan{..} -> A.object $ concat
      [ map ("each" .=) $ toList _each
      , ["countin" .= _countin | _countin /= Countin []]
      , ["comments" .= _planComments | not $ null _planComments]
      ]
    MoggPlan{..} -> A.object $ concat
      [ ["mogg-md5" .= _moggMD5]
      , ["guitar" .= _moggGuitar | not $ null _moggGuitar]
      , ["bass" .= _moggBass | not $ null _moggBass]
      , ["keys" .= _moggKeys | not $ null _moggKeys]
      , ["drums" .= _moggDrums | not $ null _moggDrums]
      , ["vocal" .= _moggVocal | not $ null _moggVocal]
      , ["crowd" .= _moggCrowd | not $ null _moggCrowd]
      , ["pans" .= _pans]
      , ["vols" .= _vols]
      , ["comments" .= _planComments | not $ null _planComments]
      , ["drum-mix" .= _drumMix]
      ]

instance TraceJSON Drums.Audio where
  traceJSON = traceJSON >>= \n -> case n :: Int of
    0 -> return Drums.D0
    1 -> return Drums.D1
    2 -> return Drums.D2
    3 -> return Drums.D3
    4 -> return Drums.D4
    _ -> expected "a valid drum mix mode (a number 0 through 4)"

instance A.ToJSON Drums.Audio where
  toJSON = A.toJSON . fromEnum

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

instance A.ToJSON AudioInput where
  toJSON = \case
    Named t -> A.toJSON t
    JammitSelect (J.Only p) t -> A.object ["only" .= [A.toJSON $ jammitPartToTitle p, A.toJSON t]]
    JammitSelect (J.Without i) t -> A.object ["without" .= [A.toJSON $ fromJammitInstrument i, A.toJSON t]]

jammitPartToTitle :: J.Part -> String
jammitPartToTitle = \case
  J.PartGuitar1 -> "Guitar 1"
  J.PartGuitar2 -> "Guitar 2"
  J.PartBass -> "Bass"
  J.PartDrums1 -> "Drums 1"
  J.PartDrums2 -> "Drums 2"
  J.PartKeys1 -> "Keys 1"
  J.PartKeys2 -> "Keys 2"
  J.PartPiano -> "Piano"
  J.PartSynth -> "Synth"
  J.PartOrgan -> "Organ"
  J.PartVocal -> "Vocal"
  J.PartBVocals -> "B Vocals"

instance TraceJSON Edge where
  traceJSON = lift ask >>= \case
    A.String "start" -> return Start
    A.String "begin" -> return Start
    A.String "end"   -> return End
    _                -> expected "an audio edge (start or end)"

instance A.ToJSON Edge where
  toJSON = \case
    Start -> "start"
    End -> "end"

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

instance (A.ToJSON t, A.ToJSON a) => A.ToJSON (Audio t a) where
  toJSON = \case
    Silence chans t -> A.object ["silence" .= [A.toJSON chans, A.toJSON t]]
    Input x -> A.toJSON x
    Mix auds -> A.object ["mix" .= auds]
    Merge auds -> A.object ["merge" .= auds]
    Concatenate auds -> A.object ["concatenate" .= auds]
    Gain d aud -> A.object ["gain" .= [A.toJSON d, A.toJSON aud]]
    Take e t aud -> A.object ["take" .= [A.toJSON e, A.toJSON t, A.toJSON aud]]
    Drop e t aud -> A.object ["drop" .= [A.toJSON e, A.toJSON t, A.toJSON aud]]
    Fade e t aud -> A.object ["fade" .= [A.toJSON e, A.toJSON t, A.toJSON aud]]
    Pad e t aud -> A.object ["pad" .= [A.toJSON e, A.toJSON t, A.toJSON aud]]
    Resample aud -> A.object ["resample" .= aud]
    Channels ns aud -> A.object ["channels" .= [A.toJSON ns, A.toJSON aud]]

instance TraceJSON Duration where
  traceJSON = lift ask >>= \case
    OneKey "frames" v -> inside "frames duration" $ Frames <$> parseFrom v traceJSON
    OneKey "seconds" v -> inside "seconds duration" $ Seconds . toRealFloat <$> parseFrom v parseMinutes
    _ -> (inside "unitless (seconds) duration" $ Seconds . toRealFloat <$> parseMinutes)
      `catch` \_ -> expected "a duration in frames or seconds"

parseMinutes :: (Monad m) => Parser m A.Value Scientific
parseMinutes = lift ask >>= \case
  A.String minstr
    | (minutes@(_:_), ':' : secstr) <- span isDigit $ T.unpack minstr
    , Just seconds <- readMaybe secstr
    -> return $ read minutes * 60 + seconds
  A.String secstr
    | Just seconds <- readMaybe $ T.unpack secstr
    -> return seconds
  _ -> traceJSON -- will succeed if JSON number

instance A.ToJSON Duration where
  toJSON = \case
    Frames f -> A.object ["frames" .= f]
    Seconds s -> let
      mins = floor $ s / 60 :: Int
      secs = s - fromIntegral mins * 60
      in case mins of
        0 -> A.toJSON s
        _ -> A.toJSON $ show mins ++ ":" ++ (if secs < 10 then "0" else "") ++ show secs

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

hasAnyVocal :: Instruments -> Bool
hasAnyVocal insts = _hasVocal insts /= Vocal0

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

instance A.ToJSON Instruments where
  toJSON Instruments{..} = A.object $ concat
    [ ["drums" .= True | _hasDrums]
    , ["guitar" .= True | _hasGuitar]
    , ["bass" .= True | _hasBass]
    , ["keys" .= True | _hasKeys]
    , ["pro-keys" .= True | _hasProKeys]
    , case _hasVocal of
      Vocal0 -> []
      _ -> ["vocal" .= fromEnum _hasVocal]
    ]

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

instance A.ToJSON AutogenTheme where
  toJSON = A.toJSON . show -- maybe put spaces in here later

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

instance A.ToJSON Rating where
  toJSON = A.toJSON . show -- maybe put spaces in here later

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

instance A.ToJSON DrumKit where
  toJSON = \case
    HardRockKit -> "Hard Rock Kit"
    ArenaKit -> "Arena Kit"
    VintageKit -> "Vintage Kit"
    TrashyKit -> "Trashy Kit"
    ElectronicKit -> "Electronic Kit"
