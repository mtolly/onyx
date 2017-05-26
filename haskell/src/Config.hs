{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Config where

import           Audio
import           Control.Arrow                  (first)
import           Control.Monad                  (when)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Data.Aeson                     ((.=))
import qualified Data.Aeson                     as A
import           Data.Char                      (isDigit, isSpace)
import           Data.Conduit.Audio             (Duration (..))
import           Data.Default.Class
import qualified Data.DTA.Serialize.Magma       as Magma
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           Data.Scientific                (Scientific, toRealFloat)
import qualified Data.Text                      as T
import           Data.Traversable
import qualified Data.Vector                    as V
import           JSONData
import           RockBand.Common                (Key (..))
import qualified RockBand.Drums                 as Drums
import           RockBand.File                  (FlexPartName (..), getPartName,
                                                 readPartName)
import qualified Sound.Jammit.Base              as J
import qualified Sound.MIDI.Util                as U
import qualified Text.ParserCombinators.ReadP   as ReadP
import           Text.Read                      (readMaybe)
import qualified Text.Read.Lex                  as Lex

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
    Just k  -> return k
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
  traceJSON = traceJSON >>= \s -> case s :: T.Text of
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

instance TraceJSON Magma.Gender where
  traceJSON = lift ask >>= \case
    A.String "female" -> return Magma.Female
    A.String "male"   -> return Magma.Male
    _                 -> expected "a gender (male or female)"

instance A.ToJSON Magma.Gender where
  toJSON = \case
    Magma.Female -> "female"
    Magma.Male   -> "male"

data AudioFile
  = AudioFile
    { _md5      :: Maybe T.Text
    , _frames   :: Maybe Integer
    , _filePath :: Maybe FilePath
    , _commands :: [T.Text]
    , _rate     :: Maybe Int
    , _channels :: Int
    }
  | AudioSnippet
    { _expr :: Audio Duration AudioInput
    }
  deriving (Eq, Ord, Show, Read)

instance TraceJSON AudioFile where
  traceJSON = decideKey
    [ ("expr", object $ do
      _expr <- requiredKey "expr" traceJSON
      expectedKeys ["expr"]
      return AudioSnippet{..}
      )
    ] $ object $ do
      _md5      <- optionalKey "md5"       traceJSON
      _frames   <- optionalKey "frames"    traceJSON
      _filePath <- optionalKey "file-path" traceJSON
      _commands <- fromMaybe [] <$> optionalKey "commands" traceJSON
      _rate     <- optionalKey "rate"      traceJSON
      _channels <- fromMaybe 2 <$> optionalKey "channels" traceJSON
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
    _jammitTitle  <- optionalKey "title"  traceJSON
    _jammitArtist <- optionalKey "artist" traceJSON
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
      _planExpr <- requiredKey "expr" traceJSON
      _planPans <- fromMaybe [] <$> optionalKey "pans" traceJSON
      _planVols <- fromMaybe [] <$> optionalKey "vols" traceJSON
      expectedKeys ["expr", "pans", "vols"]
      return PlanAudio{..}
      )
    ] $ (\expr -> PlanAudio expr [] []) <$> traceJSON

instance (A.ToJSON t, A.ToJSON a) => A.ToJSON (PlanAudio t a) where
  toJSON (PlanAudio expr [] []) = A.toJSON expr
  toJSON PlanAudio{..} = A.object $ concat
    [ ["expr" .= _planExpr]
    , ["pans" .= _planPans]
    , ["vols" .= _planVols]
    ]

data PartAudio a
  = PartSingle a
  | PartDrumKit
    { drumsSplitKick  :: Maybe a
    , drumsSplitSnare :: Maybe a
    , drumsSplitKit   :: a
    }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (TraceJSON a) => TraceJSON (PartAudio a) where
  traceJSON = decideKey
    [ ("kit", object $ do
      drumsSplitKick <- optionalKey "kick" traceJSON
      drumsSplitSnare <- optionalKey "snare" traceJSON
      drumsSplitKit <- requiredKey "kit" traceJSON
      expectedKeys ["kick", "snare", "kit"]
      return PartDrumKit{..}
      )
    ] $ PartSingle <$> traceJSON

instance (A.ToJSON a) => A.ToJSON (PartAudio a) where
  toJSON (PartSingle x) = A.toJSON x
  toJSON PartDrumKit{..} = A.object $ concat
    [ map ("kick"  .=) $ toList drumsSplitKick
    , map ("snare" .=) $ toList drumsSplitSnare
    , ["kit" .= drumsSplitKit]
    ]

newtype Countin = Countin [(Either U.MeasureBeats U.Seconds, Audio Duration AudioInput)]
  deriving (Eq, Ord, Show)

newtype Parts a = Parts { getParts :: Map.HashMap FlexPartName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (TraceJSON a) => TraceJSON (Parts a) where
  traceJSON = Parts . Map.fromList . map (first readPartName) . Map.toList <$> mapping traceJSON

instance (A.ToJSON a) => A.ToJSON (Parts a) where
  toJSON (Parts hm) = A.toJSON $ Map.fromList $ map (first getPartName) $ Map.toList hm

data Plan
  = Plan
    { _song         :: Maybe (PlanAudio Duration AudioInput)
    , _countin      :: Countin
    , _planParts    :: Parts (PartAudio (PlanAudio Duration AudioInput))
    , _crowd        :: Maybe (PlanAudio Duration AudioInput)
    , _planComments :: [T.Text]
    }
  | MoggPlan
    { _moggMD5      :: T.Text
    , _moggParts    :: Parts (PartAudio [Int])
    , _moggCrowd    :: [Int]
    , _pans         :: [Double]
    , _vols         :: [Double]
    , _planComments :: [T.Text]
    , _karaoke      :: Bool
    , _multitrack   :: Bool
    }
  deriving (Eq, Show)

getKaraoke, getMultitrack :: Plan -> Bool
getKaraoke = \case
  Plan{..} -> Map.keys (getParts _planParts) == [FlexVocal]
  MoggPlan{..} -> _karaoke
getMultitrack = \case
  Plan{..} -> not $ Map.null $ Map.delete FlexVocal $ getParts _planParts
  MoggPlan{..} -> _multitrack

instance TraceJSON Countin where
  traceJSON = do
    hm <- mapping traceJSON
    fmap Countin $ forM (Map.toList hm) $ \(k, v) -> (, v) <$> parseFrom k parseCountinTime

-- | Parses any of \"measure|beats\", \"seconds\", or \"minutes:seconds\".
parseCountinTime :: (Monad m) => Parser m T.Text (Either U.MeasureBeats U.Seconds)
parseCountinTime = do
  t <- lift ask
  inside ("Countin timestamp " ++ show t)
    $                  fmap Left                 parseMeasureBeats
    `catchError` \_ -> fmap (Right . realToFrac) (parseFrom (A.String t) parseMinutes)
    `catchError` \_ -> parseFrom (A.String t) $ expected "a timestamp in measure|beats, seconds, or minutes:seconds"

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

showMeasureBeats :: U.MeasureBeats -> T.Text
showMeasureBeats (msr, bts) = T.pack $ show msr ++ "|" ++ show (realToFrac bts :: Scientific)

instance A.ToJSON Countin where
  toJSON (Countin pairs) = A.Object $ Map.fromList $ flip map pairs $ \(t, v) -> let
    k = case t of
      Left mb    -> showMeasureBeats mb
      Right secs -> T.pack $ show (realToFrac secs :: Milli)
    in (k, A.toJSON v)

instance TraceJSON Plan where
  traceJSON = decideKey
    [ ("mogg-md5", object $ do
      _moggMD5 <- requiredKey "mogg-md5" traceJSON
      _moggParts <- requiredKey "parts" traceJSON
      _moggCrowd <- fromMaybe [] <$> optionalKey "crowd" traceJSON
      _pans <- requiredKey "pans" traceJSON
      _vols <- requiredKey "vols" traceJSON
      _planComments <- fromMaybe [] <$> optionalKey "comments" traceJSON
      _karaoke    <- fromMaybe False          <$> optionalKey "karaoke"    traceJSON
      _multitrack <- fromMaybe (not _karaoke) <$> optionalKey "multitrack" traceJSON
      expectedKeys ["mogg-md5", "parts", "crowd", "pans", "vols", "comments", "karaoke", "multitrack"]
      return MoggPlan{..}
      )
    ] $ object $ do
      _song <- optionalKey "song" traceJSON
      _countin <- fromMaybe (Countin []) <$> optionalKey "countin" traceJSON
      _planParts <- fromMaybe (Parts Map.empty) <$> optionalKey "parts" traceJSON
      _crowd <- optionalKey "crowd" traceJSON
      _planComments <- fromMaybe [] <$> optionalKey "comments" traceJSON
      expectedKeys ["song", "countin", "parts", "crowd", "comments"]
      return Plan{..}

instance A.ToJSON Plan where
  toJSON = \case
    Plan{..} -> A.object $ concat
      [ map ("song" .=) $ toList _song
      , ["countin" .= _countin | _countin /= Countin []]
      , ["parts" .= _planParts]
      , map ("crowd" .=) $ toList _crowd
      , ["comments" .= _planComments | not $ null _planComments]
      ]
    MoggPlan{..} -> A.object $ concat
      [ ["mogg-md5" .= _moggMD5]
      , ["parts" .= _moggParts]
      , ["crowd" .= _moggCrowd | not $ null _moggCrowd]
      , ["pans" .= _pans]
      , ["vols" .= _vols]
      , ["comments" .= _planComments | not $ null _planComments]
      , ["karaoke" .= _karaoke]
      , ["multitrack" .= _multitrack]
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

jammitPartToTitle :: J.Part -> T.Text
jammitPartToTitle = \case
  J.PartGuitar1 -> "Guitar 1"
  J.PartGuitar2 -> "Guitar 2"
  J.PartBass1 -> "Bass 1"
  J.PartBass2 -> "Bass 2"
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
algebraic1 k f p1 = object $ onlyKey k $ do
  x <- lift ask
  fmap f $ inside "ADT field 1 of 1" $ parseFrom x p1

algebraic2 :: (Monad m) => T.Text -> (a -> b -> c) ->
  Parser m A.Value a -> Parser m A.Value b -> Parser m A.Value c
algebraic2 k f p1 p2 = object $ onlyKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y] -> f
      <$> do inside "ADT field 1 of 2" $ parseFrom x p1
      <*> do inside "ADT field 2 of 2" $ parseFrom y p2
    _ -> expected "an array of 2 ADT fields"
  _ -> expected "an array of 2 ADT fields"

algebraic3 :: (Monad m) => T.Text -> (a -> b -> c -> d) ->
  Parser m A.Value a -> Parser m A.Value b -> Parser m A.Value c -> Parser m A.Value d
algebraic3 k f p1 p2 p3 = object $ onlyKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y, z] -> f
      <$> do inside "ADT field 1 of 3" $ parseFrom x p1
      <*> do inside "ADT field 2 of 3" $ parseFrom y p2
      <*> do inside "ADT field 3 of 3" $ parseFrom z p3
    _ -> expected "an array of 3 ADT fields"
  _ -> expected "an array of 3 ADT fields"

decideKey :: (Monad m) => [(T.Text, Parser m A.Value a)] -> Parser m A.Value a -> Parser m A.Value a
decideKey opts dft = lift ask >>= \case
  A.Object hm -> case [ p | (k, p) <- opts, Map.member k hm ] of
    p : _ -> p
    []    -> dft
  _ -> dft

instance (TraceJSON t, TraceJSON a) => TraceJSON (Audio t a) where
  traceJSON = decideKey
    [ ("silence", algebraic2 "silence" Silence traceJSON traceJSON)
    , ("mix"        , object $ onlyKey "mix"         $ Mix         <$> traceJSON)
    , ("merge"      , object $ onlyKey "merge"       $ Merge       <$> traceJSON)
    , ("concatenate", object $ onlyKey "concatenate" $ Concatenate <$> traceJSON)
    , ("gain", algebraic2 "gain" Gain traceJSON traceJSON)
    , ("take", supplyEdge "take" Take)
    , ("drop", supplyEdge "drop" Drop)
    , ("trim", supplyEdge "trim" Drop)
    , ("fade", supplyEdge "fade" Fade)
    , ("pad" , supplyEdge "pad"  Pad )
    , ("resample", algebraic1 "resample" Resample traceJSON)
    , ("channels", algebraic2 "channels" Channels traceJSON traceJSON)
    , ("stretch", algebraic2 "stretch" StretchSimple traceJSON traceJSON)
    , ("stretch", algebraic3 "stretch" StretchFull traceJSON traceJSON traceJSON)
    , ("mask", algebraic3 "mask" Mask traceJSON traceJSON traceJSON)
    ] (fmap Input traceJSON `catchError` \_ -> expected "an audio expression")
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
    StretchSimple d aud -> A.object ["stretch" .= [A.toJSON d, A.toJSON aud]]
    StretchFull t p aud -> A.object ["stretch" .= [A.toJSON t, A.toJSON p, A.toJSON aud]]
    Mask tags seams aud -> A.object ["mask" .= [A.toJSON tags, A.toJSON seams, A.toJSON aud]]

instance (TraceJSON t) => TraceJSON (Seam t) where
  traceJSON = object $ do
    seamCenter <- requiredKey "center" traceJSON
    valueFade <- fromMaybe (A.Number 0) <$> optionalKey "fade" traceJSON
    seamFade <- parseFrom valueFade traceJSON
    seamTag <- requiredKey "tag" traceJSON
    expectedKeys ["center", "fade", "tag"]
    return Seam{..}

instance (A.ToJSON t) => A.ToJSON (Seam t) where
  toJSON Seam{..} = A.object
    [ "center" .= seamCenter
    , "fade" .= seamFade
    , "tag" .= seamTag
    ]

instance TraceJSON Duration where
  traceJSON = lift ask >>= \case
    OneKey "frames" v -> inside "frames duration" $ Frames <$> parseFrom v traceJSON
    OneKey "seconds" v -> inside "seconds duration" $ Seconds . toRealFloat <$> parseFrom v parseMinutes
    _ -> inside "unitless (seconds) duration" (Seconds . toRealFloat <$> parseMinutes)
      `catchError` \_ -> expected "a duration in frames or seconds"

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

showTimestamp :: Milli -> A.Value
showTimestamp s = let
  mins = floor $ s / 60 :: Int
  secs = s - fromIntegral mins * 60
  in case mins of
    0 -> A.toJSON s
    _ -> A.toJSON $ show mins ++ ":" ++ (if secs < 10 then "0" else "") ++ show secs

instance A.ToJSON Duration where
  toJSON = \case
    Frames f -> A.object ["frames" .= f]
    Seconds s -> showTimestamp $ realToFrac s

instance TraceJSON FlexPartName where
  traceJSON = fmap readPartName traceJSON

instance A.ToJSON FlexPartName where
  toJSON = A.toJSON . getPartName

data Difficulty
  = Tier Integer -- ^ [1..7]: 1 = no dots, 7 = devil dots
  | Rank Integer -- ^ [1..]
  deriving (Eq, Ord, Show, Read)

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

jsonRecord "PartGRYBO" eosr $ do
  fill "gryboDifficulty" "difficulty" [t| Difficulty |] [e| Tier 1 |]
  opt "gryboHopoThreshold" "hopo-threshold" [t| Int |] [e| 170 |]
  opt "gryboFixFreeform" "fix-freeform" [t| Bool |] [e| True |]

jsonRecord "PartProKeys" eosr $ do
  fill "pkDifficulty" "difficulty" [t| Difficulty |] [e| Tier 1 |]
  opt "pkFixFreeform" "fix-freeform" [t| Bool |] [e| True |]

jsonRecord "PartProGuitar" eosr $ do
  fill "pgDifficulty" "difficulty" [t| Difficulty |] [e| Tier 1 |]
  opt "pgHopoThreshold" "hopo-threshold" [t| Int |] [e| 170 |]
  opt "pgTuning" "tuning" [t| [Int] |] [e| [] |]
  opt "pgFixFreeform" "fix-freeform" [t| Bool |] [e| True |]

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
      Nothing  -> expected "the name of a drum kit or null"
    _                  -> expected "the name of a drum kit or null"

instance A.ToJSON DrumKit where
  toJSON = \case
    HardRockKit -> "Hard Rock Kit"
    ArenaKit -> "Arena Kit"
    VintageKit -> "Vintage Kit"
    TrashyKit -> "Trashy Kit"
    ElectronicKit -> "Electronic Kit"

data DrumLayout
  = StandardLayout
  | FlipYBToms
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON DrumLayout where
  traceJSON = lift ask >>= \case
    A.Null                     -> return StandardLayout
    A.String "standard-layout" -> return StandardLayout
    A.String "flip-yb-toms"    -> return FlipYBToms
    _                          -> expected "the name of a drum kit layout or null"

instance A.ToJSON DrumLayout where
  toJSON = \case
    StandardLayout -> "standard-layout"
    FlipYBToms     -> "flip-yb-toms"

jsonRecord "PartDrums" eosr $ do
  fill "drumsDifficulty" "difficulty" [t| Difficulty |] [e| Tier 1 |]
  opt "drumsPro" "pro" [t| Bool |] [e| True |]
  opt "drumsAuto2xBass" "auto-2x-bass" [t| Bool |] [e| False |]
  opt "drumsFixFreeform" "fix-freeform" [t| Bool |] [e| True |]
  opt "drumsKit" "kit" [t| DrumKit |] [e| HardRockKit |]
  opt "drumsLayout" "layout" [t| DrumLayout |] [e| StandardLayout |]

data VocalCount = Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON VocalCount where
  traceJSON = lift ask >>= \case
    A.Number 1   -> return Vocal1
    A.Number 2   -> return Vocal2
    A.Number 3   -> return Vocal3
    _            -> expected "a vocal part count (1 to 3)"

instance A.ToJSON VocalCount where
  toJSON c = A.toJSON $ fromEnum c + 1

jsonRecord "PartVocal" eosr $ do
  fill "vocalDifficulty" "difficulty" [t| Difficulty |] [e| Tier 1 |]
  fill "vocalCount" "count" [t| VocalCount |] [e| Vocal1 |]
  opt "vocalGender" "gender" [t| Maybe Magma.Gender |] [e| Nothing |]

jsonRecord "Part" eosr $ do
  opt "partGRYBO"     "grybo"      [t| Maybe PartGRYBO     |] [e| Nothing |]
  opt "partProKeys"   "pro-keys"   [t| Maybe PartProKeys   |] [e| Nothing |]
  opt "partProGuitar" "pro-guitar" [t| Maybe PartProGuitar |] [e| Nothing |]
  opt "partDrums"     "drums"      [t| Maybe PartDrums     |] [e| Nothing |]
  opt "partVocal"     "vocal"      [t| Maybe PartVocal     |] [e| Nothing |]

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
      _                        -> expected "a valid content rating or null"
    _          -> expected "a valid content rating or null"

instance A.ToJSON Rating where
  toJSON = A.toJSON . show -- maybe put spaces in here later

data PreviewTime
  = PreviewSection T.Text
  | PreviewMIDI    U.MeasureBeats
  | PreviewSeconds U.Seconds
  deriving (Eq, Ord, Show)

instance TraceJSON PreviewTime where
  traceJSON = let
    traceNum = do
      d <- traceJSON
      return $ PreviewSeconds $ realToFrac (d :: Double)
    traceStr = do
      str <- traceJSON
      case T.stripPrefix "prc_" str of
        Just prc -> return $ PreviewSection prc
        Nothing -> let
          p = parseFrom str $ either PreviewMIDI PreviewSeconds <$> parseCountinTime
          in p `catchError` \_ -> expected "a preview time: prc_something, timestamp, or measure|beats"
    in traceNum `catchError` \_ -> traceStr

instance A.ToJSON PreviewTime where
  toJSON = \case
    PreviewSection str -> A.toJSON $ "prc_" <> str
    PreviewMIDI mb -> A.toJSON $ showMeasureBeats mb
    PreviewSeconds secs -> showTimestamp $ realToFrac secs

-- | Extra information with no gameplay affect.
jsonRecord "Metadata" eqshow $ do
  warning "_title" "title" [t| Maybe T.Text |] [e| Nothing |]
  warning "_artist" "artist" [t| Maybe T.Text |] [e| Nothing |]
  warning "_album" "album" [t| Maybe T.Text |] [e| Nothing |]
  warning "_genre" "genre" [t| Maybe T.Text |] [e| Nothing |]
  warning "_subgenre" "subgenre" [t| Maybe T.Text |] [e| Nothing |]
  warning "_year" "year" [t| Maybe Int |] [e| Nothing |]
  warning "_fileAlbumArt" "file-album-art" [t| Maybe FilePath |] [e| Nothing |]
  warning "_trackNumber" "track-number" [t| Maybe Int |] [e| Nothing |]
  opt "_comments" "comments" [t| [T.Text] |] [e| [] |]
  opt "_key" "key" [t| Maybe Key |] [e| Nothing |]
  opt "_autogenTheme" "autogen-theme" [t| AutogenTheme |] [e| AutogenDefault |]
  warning "_author" "author" [t| Maybe T.Text |] [e| Nothing |]
  opt "_rating" "rating" [t| Rating |] [e| Unrated |]
  opt "_previewStart" "preview-start" [t| Maybe PreviewTime |] [e| Nothing |]
  opt "_previewEnd" "preview-end" [t| Maybe PreviewTime |] [e| Nothing |]
  opt "_languages" "languages" [t| [T.Text] |] [e| [] |]
  opt "_convert"    "convert"     [t| Bool |] [e| False |]
  opt "_rhythmKeys" "rhythm-keys" [t| Bool |] [e| False |]
  opt "_rhythmBass" "rhythm-bass" [t| Bool |] [e| False |]
  opt "_catEMH"     "cat-emh"     [t| Bool |] [e| False |]
  opt "_expertOnly" "expert-only" [t| Bool |] [e| False |]
  opt "_cover"      "cover"       [t| Bool |] [e| False |]
  fill "_difficulty" "difficulty"  [t| Difficulty |] [e| Tier 1 |]

getTitle, getArtist, getAlbum, getAuthor :: Metadata -> T.Text
getTitle = fromMaybe "Untitled" . _title
getArtist = fromMaybe "Unknown Artist" . _artist
getAlbum = fromMaybe "Unknown Album" . _album
getAuthor = fromMaybe "Unknown Author" . _author

getYear, getTrackNumber :: Metadata -> Int
getYear = fromMaybe 1960 . _year
getTrackNumber = fromMaybe 1 . _trackNumber

jsonRecord "TargetRB3" eosr $ do
  opt "rb3_Speed" "speed" [t| Maybe Double |] [e| Nothing |]
  opt "rb3_Plan" "plan" [t| Maybe T.Text |] [e| Nothing |]
  opt "rb3_2xBassPedal" "2x-bass-pedal" [t| Bool |] [e| False |]
  opt "rb3_SongID" "song-id" [t| Maybe (JSONEither Integer T.Text) |] [e| Nothing |]
  opt "rb3_Label" "label" [t| Maybe T.Text |] [e| Nothing |]
  opt "rb3_Version" "version" [t| Maybe Integer |] [e| Nothing |]
  opt "rb3_Guitar" "guitar" [t| FlexPartName |] [e| FlexGuitar |]
  opt "rb3_Bass" "bass" [t| FlexPartName |] [e| FlexBass |]
  opt "rb3_Drums" "drums" [t| FlexPartName |] [e| FlexDrums |]
  opt "rb3_Keys" "keys" [t| FlexPartName |] [e| FlexKeys |]
  opt "rb3_Vocal" "vocal" [t| FlexPartName |] [e| FlexVocal |]

jsonRecord "TargetRB2" eosr $ do
  opt "rb2_Speed" "speed" [t| Maybe Double |] [e| Nothing |]
  opt "rb2_Plan" "plan" [t| Maybe T.Text |] [e| Nothing |]
  opt "rb2_2xBassPedal" "2x-bass-pedal" [t| Bool |] [e| False |]
  opt "rb2_SongID" "song-id" [t| Maybe (JSONEither Integer T.Text) |] [e| Nothing |]
  opt "rb2_Label" "label" [t| Maybe T.Text |] [e| Nothing |]
  opt "rb2_Version" "version" [t| Maybe Integer |] [e| Nothing |]
  opt "rb2_Guitar" "guitar" [t| FlexPartName |] [e| FlexGuitar |]
  opt "rb2_Bass" "bass" [t| FlexPartName |] [e| FlexBass |]
  opt "rb2_Drums" "drums" [t| FlexPartName |] [e| FlexDrums |]
  opt "rb2_Vocal" "vocal" [t| FlexPartName |] [e| FlexVocal |]

jsonRecord "TargetPS" eosr $ do
  opt "ps_Speed" "speed" [t| Maybe Double |] [e| Nothing |]
  opt "ps_Plan" "plan" [t| Maybe T.Text |] [e| Nothing |]
  opt "ps_Label" "label" [t| Maybe T.Text |] [e| Nothing |]
  opt "ps_FileVideo" "file-video" [t| Maybe FilePath |] [e| Nothing |]
  opt "ps_Guitar" "guitar" [t| FlexPartName |] [e| FlexGuitar |]
  opt "ps_Bass" "bass" [t| FlexPartName |] [e| FlexBass |]
  opt "ps_Drums" "drums" [t| FlexPartName |] [e| FlexDrums |]
  opt "ps_Keys" "keys" [t| FlexPartName |] [e| FlexKeys |]
  opt "ps_Vocal" "vocal" [t| FlexPartName |] [e| FlexVocal |]
  opt "ps_Rhythm" "rhythm" [t| FlexPartName |] [e| FlexExtra "rhythm" |]
  opt "ps_GuitarCoop" "guitar-coop" [t| FlexPartName |] [e| FlexExtra "guitar-coop" |]

data Target
  = RB3    TargetRB3
  | RB2    TargetRB2
  | PS     TargetPS
  deriving (Eq, Ord, Show, Read)

addKey :: (A.ToJSON a) => T.Text -> A.Value -> a -> A.Value
addKey k v t = case A.toJSON t of
  A.Object o -> A.Object $ Map.insert k v o
  x          -> error $ "panic! expected JSON object, but got: " ++ show x

instance A.ToJSON Target where
  toJSON (RB3 rb3) = addKey "game" "rb3" rb3
  toJSON (RB2 rb2) = addKey "game" "rb2" rb2
  toJSON (PS  ps ) = addKey "game" "ps"  ps

instance TraceJSON Target where
  traceJSON = object $ do
    target <- requiredKey "game" traceJSON
    hm <- lift ask
    parseFrom (A.Object $ Map.delete "game" hm) $ case target :: T.Text of
      "rb3" -> fmap RB3 traceJSON
      "rb2" -> fmap RB2 traceJSON
      "ps"  -> fmap PS  traceJSON
      _     -> fatal $ "Unrecognized target game: " ++ show target

data SongYaml = SongYaml
  { _metadata :: Metadata
  , _audio    :: Map.HashMap T.Text AudioFile
  , _jammit   :: Map.HashMap T.Text JammitTrack
  , _plans    :: Map.HashMap T.Text Plan
  , _targets  :: Map.HashMap T.Text Target
  , _parts    :: Parts Part
  } deriving (Eq, Show)

instance TraceJSON SongYaml where
  traceJSON = object $ do
    let defaultEmptyMap = fmap $ fromMaybe Map.empty
    _metadata    <- fromMaybe def <$> optionalKey "metadata" traceJSON
    _audio       <- defaultEmptyMap $ optionalKey "audio"  $ mapping traceJSON
    _jammit      <- defaultEmptyMap $ optionalKey "jammit" $ mapping traceJSON
    _plans       <- defaultEmptyMap $ optionalKey "plans"  $ mapping traceJSON
    _targets     <- defaultEmptyMap $ optionalKey "targets"  $ mapping traceJSON
    _parts       <- requiredKey "parts" traceJSON
    expectedKeys ["metadata", "audio", "jammit", "plans", "targets", "parts"]
    return SongYaml{..}

instance A.ToJSON SongYaml where
  toJSON SongYaml{..} = A.object
    [ "metadata" .= _metadata
    , "audio" .= A.Object (fmap A.toJSON _audio)
    , "jammit" .= A.Object (fmap A.toJSON _jammit)
    , "plans" .= A.Object (fmap A.toJSON _plans)
    , "targets" .= A.Object (fmap A.toJSON _targets)
    , "parts" .= _parts
    ]

getPart :: FlexPartName -> SongYaml -> Maybe Part
getPart fpart = Map.lookup fpart . getParts . _parts
