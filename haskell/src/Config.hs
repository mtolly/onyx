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
import           Data.Hashable                  (Hashable (..))
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (fromMaybe, isJust, isNothing)
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

data KeysRB2
  = NoKeys
  | KeysGuitar
  | KeysBass
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- | Options that affect gameplay.
jsonRecord "Options" eosr $ do
  opt "_hopoThreshold"   "hopo-threshold"    [t| Int |]     [e| 170 |]
  opt "_auto2xBass"      "auto-2x-bass"      [t| Bool |]    [e| False |]
  opt "_proGuitarTuning" "pro-guitar-tuning" [t| [Int] |]   [e| [] |]
  opt "_proBassTuning"   "pro-bass-tuning"   [t| [Int] |]   [e| [] |]
  opt "_proDrums"        "pro-drums"         [t| Bool |]    [e| True |]
  opt "_fixFreeform"     "fix-freeform"      [t| Bool |]    [e| True |]

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
    , _karaoke      :: Bool
    , _multitrack   :: Bool
    }
  deriving (Eq, Ord, Show)

getKaraoke, getMultitrack :: Plan -> Bool
getKaraoke = \case
  Plan{..} -> isJust _vocal && all isNothing [_guitar, _bass, _keys, _drums]
  MoggPlan{..} -> _karaoke
  EachPlan{} -> False
getMultitrack = \case
  Plan{..} -> any isJust [_guitar, _bass, _keys, _drums]
  MoggPlan{..} -> _multitrack
  EachPlan{} -> True

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
    [ ("each", object $ do
      _each <- requiredKey "each" traceJSON
      _countin <- fromMaybe (Countin []) <$> optionalKey "countin" traceJSON
      _planComments <- fromMaybe [] <$> optionalKey "comments" traceJSON
      expectedKeys ["each", "countin", "comments"]
      return EachPlan{..}
      )
    , ("mogg-md5", object $ do
      _moggMD5 <- requiredKey "mogg-md5" traceJSON
      _moggGuitar <- fromMaybe [] <$> optionalKey "guitar" traceJSON
      _moggBass   <- fromMaybe [] <$> optionalKey "bass" traceJSON
      _moggKeys   <- fromMaybe [] <$> optionalKey "keys" traceJSON
      _moggDrums  <- fromMaybe [] <$> optionalKey "drums" traceJSON
      _moggVocal  <- fromMaybe [] <$> optionalKey "vocal" traceJSON
      _moggCrowd  <- fromMaybe [] <$> optionalKey "crowd" traceJSON
      _pans <- requiredKey "pans" traceJSON
      _vols <- requiredKey "vols" traceJSON
      _drumMix <- requiredKey "drum-mix" traceJSON
      _planComments <- fromMaybe [] <$> optionalKey "comments" traceJSON
      _karaoke    <- fromMaybe False          <$> optionalKey "karaoke"    traceJSON
      _multitrack <- fromMaybe (not _karaoke) <$> optionalKey "multitrack" traceJSON
      expectedKeys ["mogg-md5", "guitar", "bass", "keys", "drums", "vocal", "crowd", "pans", "vols", "drum-mix", "comments", "karaoke", "multitrack"]
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
            _song    <- optionalKey "song"    traceJSON
            _countin <- fromMaybe (Countin []) <$> optionalKey "countin" traceJSON
            _guitar  <- optionalKey "guitar"  traceJSON
            _bass    <- optionalKey "bass"    traceJSON
            _keys    <- optionalKey "keys"    traceJSON
            _kick    <- optionalKey "kick"    traceJSON
            _snare   <- optionalKey "snare"   traceJSON
            _drums   <- optionalKey "drums"   traceJSON
            _vocal   <- optionalKey "vocal"   traceJSON
            _crowd   <- optionalKey "crowd"   traceJSON
            _planComments <- fromMaybe [] <$> optionalKey "comments" traceJSON
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
    , ("stretch", algebraic2 "stretch" Stretch traceJSON traceJSON)
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
    Stretch d aud -> A.object ["stretch" .= [A.toJSON d, A.toJSON aud]]
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

instance A.ToJSON VocalCount where
  toJSON = A.toJSON . fromEnum

instance TraceJSON FlexPartName where
  traceJSON = fmap readPartName traceJSON

instance A.ToJSON FlexPartName where
  toJSON = A.toJSON . getPartName

data PlayMode = PlayGRYBO | PlayProKeys | PlayProGuitar | PlayDrums | PlayVocal1 | PlayVocal2 | PlayVocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance TraceJSON PlayMode where
  traceJSON = lift ask >>= \case
    "grybo"      -> return PlayGRYBO
    "pro-keys"   -> return PlayProKeys
    "pro-guitar" -> return PlayProGuitar
    "drums"      -> return PlayDrums
    "vocal"      -> return PlayVocal1
    "vocal-1"    -> return PlayVocal1
    "vocal-2"    -> return PlayVocal2
    "vocal-3"    -> return PlayVocal3
    _            -> expected "a play mode (grybo, pro-keys, pro-guitar, drums, vocal-1, vocal-2, vocal-3)"

instance A.ToJSON PlayMode where
  toJSON = \case
    PlayGRYBO     -> "grybo"
    PlayProKeys   -> "pro-keys"
    PlayProGuitar -> "pro-guitar"
    PlayDrums     -> "drums"
    PlayVocal1    -> "vocal-1"
    PlayVocal2    -> "vocal-2"
    PlayVocal3    -> "vocal-3"

instance Hashable PlayMode where
  hashWithSalt salt = hashWithSalt salt . fromEnum

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

newtype Difficulties = Difficulties (Map.HashMap (Maybe (FlexPartName, PlayMode)) Difficulty)
  deriving (Eq, Show)

instance TraceJSON Difficulties where
  traceJSON = object $ do
    bandDiff <- optionalKey "band" traceJSON
    hm <- lift ask
    parseFrom (A.Object $ Map.delete "band" hm) $ do
      diffs <- mapping $ mapping traceJSON
      let paired = do
            (fpart, modes) <- Map.toList diffs
            (mode, diff) <- Map.toList modes
            return ((fpart, mode), diff)
      diffs' <- forM paired $ \((fpart, mode), diff) -> do
        fpart' <- parseFrom (A.String fpart) traceJSON
        mode' <- parseFrom (A.String mode) traceJSON
        return (Just (fpart', mode'), diff)
      return $ Difficulties $ Map.fromList $ case bandDiff of
        Nothing -> diffs'
        Just bd -> (Nothing, bd) : diffs'

instance A.ToJSON Difficulties where
  toJSON = undefined

instance Default Difficulties where
  def = Difficulties Map.empty

newtype Instruments = Instruments (Map.HashMap FlexPartName [PlayMode])
  deriving (Eq, Show)

instance TraceJSON Instruments where
  traceJSON = Instruments . Map.fromList . map (first readPartName) . Map.toList <$> mapping traceJSON

instance A.ToJSON Instruments where
  toJSON (Instruments hm) = A.toJSON $ Map.fromList $ map (first getPartName) $ Map.toList hm

playModes :: FlexPartName -> Instruments -> [PlayMode]
playModes fpart (Instruments hm) = Map.lookupDefault [] fpart hm

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
  opt "_vocalGender" "vocal-gender" [t| Maybe Magma.Gender |] [e| Nothing |]
  opt "_difficulty" "difficulty" [t| Difficulties |] [e| def |]
  opt "_key" "key" [t| Maybe Key |] [e| Nothing |]
  opt "_autogenTheme" "autogen-theme" [t| AutogenTheme |] [e| AutogenDefault |]
  warning "_author" "author" [t| Maybe T.Text |] [e| Nothing |]
  opt "_rating" "rating" [t| Rating |] [e| Unrated |]
  opt "_drumKit" "drum-kit" [t| DrumKit |] [e| HardRockKit |]
  opt "_drumLayout" "drum-layout" [t| DrumLayout |] [e| StandardLayout |]
  opt "_previewStart" "preview-start" [t| Maybe PreviewTime |] [e| Nothing |]
  opt "_previewEnd" "preview-end" [t| Maybe PreviewTime |] [e| Nothing |]
  opt "_languages" "languages" [t| [T.Text] |] [e| [] |]
  opt "_convert"    "convert"     [t| Bool |] [e| False |]
  opt "_rhythmKeys" "rhythm-keys" [t| Bool |] [e| False |]
  opt "_rhythmBass" "rhythm-bass" [t| Bool |] [e| False |]
  opt "_catEMH"     "cat-emh"     [t| Bool |] [e| False |]
  opt "_expertOnly" "expert-only" [t| Bool |] [e| False |]
  opt "_cover"      "cover"       [t| Bool |] [e| False |]

getTitle, getArtist, getAlbum, getAuthor :: Metadata -> T.Text
getTitle = fromMaybe "Untitled" . _title
getArtist = fromMaybe "Unknown Artist" . _artist
getAlbum = fromMaybe "Unknown Album" . _album
getAuthor = fromMaybe "Unknown Author" . _author

getYear, getTrackNumber :: Metadata -> Int
getYear = fromMaybe 1960 . _year
getTrackNumber = fromMaybe 1 . _trackNumber

jsonRecord "TargetRB3" eosr $ do
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
  opt "ps_Plan" "plan" [t| Maybe T.Text |] [e| Nothing |]
  opt "ps_Label" "label" [t| Maybe T.Text |] [e| Nothing |]
  opt "ps_FileVideo" "file-video" [t| Maybe FilePath |] [e| Nothing |]
  opt "ps_Guitar" "guitar" [t| FlexPartName |] [e| FlexGuitar |]
  opt "ps_Bass" "bass" [t| FlexPartName |] [e| FlexBass |]
  opt "ps_Drums" "drums" [t| FlexPartName |] [e| FlexDrums |]
  opt "ps_Keys" "keys" [t| FlexPartName |] [e| FlexKeys |]
  opt "ps_Vocal" "vocal" [t| FlexPartName |] [e| FlexVocal |]
  -- TODO non-rb3 parts: rhythm, coop guitar

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
  { _metadata    :: Metadata
  , _options     :: Options
  , _audio       :: Map.HashMap T.Text AudioFile
  , _jammit      :: Map.HashMap T.Text JammitTrack
  , _plans       :: Map.HashMap T.Text Plan
  , _targets     :: Map.HashMap T.Text Target
  , _instruments :: Instruments
  , _published   :: Bool
  } deriving (Eq, Show)

instance TraceJSON SongYaml where
  traceJSON = object $ do
    let defaultEmptyMap = fmap $ fromMaybe Map.empty
    _metadata    <- fromMaybe def <$> optionalKey "metadata" traceJSON
    _options     <- fromMaybe def <$> optionalKey "options" traceJSON
    _audio       <- defaultEmptyMap $ optionalKey "audio"  $ mapping traceJSON
    _jammit      <- defaultEmptyMap $ optionalKey "jammit" $ mapping traceJSON
    _plans       <- defaultEmptyMap $ optionalKey "plans"  $ mapping traceJSON
    _targets     <- defaultEmptyMap $ optionalKey "targets"  $ mapping traceJSON
    _instruments <- requiredKey "instruments" traceJSON
    _published   <- fromMaybe True <$> optionalKey "published" traceJSON
    expectedKeys ["metadata", "options", "audio", "jammit", "plans", "targets", "instruments", "published"]
    return SongYaml{..}

instance A.ToJSON SongYaml where
  toJSON SongYaml{..} = A.object
    [ "metadata" .= _metadata
    , "options" .= _options
    , "audio" .= A.Object (fmap A.toJSON _audio)
    , "jammit" .= A.Object (fmap A.toJSON _jammit)
    , "plans" .= A.Object (fmap A.toJSON _plans)
    , "targets" .= A.Object (fmap A.toJSON _targets)
    , "instruments" .= _instruments
    , "published" .= _published
    ]
