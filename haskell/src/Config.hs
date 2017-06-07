{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Config where

import           Audio
import           Control.Arrow                  (first)
import           Control.Monad.Codec            ((=.))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.Aeson                     as A
import           Data.Char                      (isDigit, isSpace)
import           Data.Conduit.Audio             (Duration (..))
import           Data.Default.Class
import qualified Data.DTA.Serialize.Magma       as Magma
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.Hashable                  (Hashable (..))
import qualified Data.HashMap.Strict            as Map
import           Data.Maybe                     (fromMaybe)
import           Data.Monoid                    ((<>))
import           Data.Scientific                (Scientific, toRealFloat)
import qualified Data.Text                      as T
import           Data.Traversable
import qualified Data.Vector                    as V
import           GHC.Generics                   (Generic (..))
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

instance StackJSON Key where
  stackJSON = StackCodec
    { stackShow = \case
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
    , stackParse = stackParse stackJSON >>= \t -> case lookup (T.toLower t) keyNames of
      Just k  -> return k
      Nothing -> expected "the name of a pitch"
    }

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

instance StackJSON Instrument where
  stackJSON = StackCodec
    { stackParse = stackParse stackJSON >>= \s -> case s :: T.Text of
      "guitar" -> return Guitar
      "bass"   -> return Bass
      "drums"  -> return Drums
      "keys"   -> return Keys
      "vocal"  -> return Vocal
      _        -> expected "an instrument name"
    , stackShow = \case
      Guitar -> "guitar"
      Bass   -> "bass"
      Drums  -> "drums"
      Keys   -> "keys"
      Vocal  -> "vocal"
    }

instance StackJSON Magma.Gender where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.String "female" -> return Magma.Female
      A.String "male"   -> return Magma.Male
      _                 -> expected "a gender (male or female)"
    , stackShow = \case
      Magma.Female -> "female"
      Magma.Male   -> "male"
    }

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

instance StackJSON AudioFile where
  stackJSON = StackCodec
    { stackParse = decideKey
      [ ("expr", object $ do
        _expr <- requiredKey "expr" fromJSON
        expectedKeys ["expr"]
        return AudioSnippet{..}
        )
      ] $ object $ do
        _md5      <- optionalKey "md5"       fromJSON
        _frames   <- optionalKey "frames"    fromJSON
        _filePath <- optionalKey "file-path" fromJSON
        _commands <- fromMaybe [] <$> optionalKey "commands" fromJSON
        _rate     <- optionalKey "rate"      fromJSON
        _channels <- fromMaybe 2 <$> optionalKey "channels" fromJSON
        expectedKeys ["md5", "frames", "file-path", "commands", "rate", "channels"]
        return AudioFile{..}
    , stackShow = \case
      AudioFile{..} -> A.object $ concat
        [ map ("md5"       .=) $ toList _md5
        , map ("frames"    .=) $ toList _frames
        , map ("file-path" .=) $ toList _filePath
        , ["commands" .= _commands | not $ null _commands]
        , map ("rate"      .=) $ toList _rate
        , ["channels"      .= _channels]
        ]
      AudioSnippet{..} -> A.object
        [ "expr" .= toJSON _expr
        ]
    }

data JammitTrack = JammitTrack
  { _jammitTitle  :: Maybe T.Text
  , _jammitArtist :: Maybe T.Text
  } deriving (Eq, Ord, Show, Read)

instance StackJSON JammitTrack where
  stackJSON = asStrictObject "JammitTrack" $ do
    _jammitTitle  <- _jammitTitle  =. opt Nothing "title"  stackJSON
    _jammitArtist <- _jammitArtist =. opt Nothing "artist" stackJSON
    return JammitTrack{..}

data PlanAudio t a = PlanAudio
  { _planExpr :: Audio t a
  , _planPans :: [Double]
  , _planVols :: [Double]
  } deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (StackJSON t, StackJSON a) => StackJSON (PlanAudio t a) where
  stackJSON = StackCodec
    { stackParse = decideKey
      [ ("expr", object $ do
        _planExpr <- requiredKey "expr" fromJSON
        _planPans <- fromMaybe [] <$> optionalKey "pans" fromJSON
        _planVols <- fromMaybe [] <$> optionalKey "vols" fromJSON
        expectedKeys ["expr", "pans", "vols"]
        return PlanAudio{..}
        )
      ] $ (\expr -> PlanAudio expr [] []) <$> fromJSON
    , stackShow = \case
      PlanAudio expr [] [] -> toJSON expr
      PlanAudio{..} -> A.object $ concat
        [ ["expr" .= _planExpr]
        , ["pans" .= _planPans]
        , ["vols" .= _planVols]
        ]
    }

data PartAudio a
  = PartSingle a
  | PartDrumKit
    { drumsSplitKick  :: Maybe a
    , drumsSplitSnare :: Maybe a
    , drumsSplitKit   :: a
    }
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (PartAudio a) where
  stackJSON = StackCodec
    { stackParse = decideKey
      [ ("kit", object $ do
        drumsSplitKick <- optionalKey "kick" fromJSON
        drumsSplitSnare <- optionalKey "snare" fromJSON
        drumsSplitKit <- requiredKey "kit" fromJSON
        expectedKeys ["kick", "snare", "kit"]
        return PartDrumKit{..}
        )
      ] $ PartSingle <$> fromJSON
    , stackShow = \case
      PartSingle x -> toJSON x
      PartDrumKit{..} -> A.object $ concat
        [ map ("kick"  .=) $ toList drumsSplitKick
        , map ("snare" .=) $ toList drumsSplitSnare
        , ["kit" .= drumsSplitKit]
        ]
    }

newtype Parts a = Parts { getParts :: Map.HashMap FlexPartName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (Parts a) where
  stackJSON = StackCodec
    { stackParse = Parts . Map.fromList . map (first readPartName) . Map.toList <$> mapping fromJSON
    , stackShow = \(Parts hm) -> mappingToJSON $ Map.fromList $ map (first getPartName) $ Map.toList hm
    }

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

newtype Countin = Countin [(Either U.MeasureBeats U.Seconds, Audio Duration AudioInput)]
  deriving (Eq, Ord, Show)

instance StackJSON Countin where
  stackJSON = StackCodec
    { stackParse = do
      hm <- mapping fromJSON
      fmap Countin $ forM (Map.toList hm) $ \(k, v) -> (, v) <$> parseFrom k parseCountinTime
    , stackShow = \(Countin pairs) -> A.Object $ Map.fromList $ flip map pairs $ \(t, v) -> let
      k = case t of
        Left mb    -> showMeasureBeats mb
        Right secs -> T.pack $ show (realToFrac secs :: Milli)
      in (k, toJSON v)
    }

-- | Parses any of \"measure|beats\", \"seconds\", or \"minutes:seconds\".
parseCountinTime :: (Monad m) => StackParser m T.Text (Either U.MeasureBeats U.Seconds)
parseCountinTime = do
  t <- lift ask
  inside ("Countin timestamp " ++ show t)
    $                  fmap Left                 parseMeasureBeats
    `catchError` \_ -> fmap (Right . realToFrac) (parseFrom (A.String t) parseMinutes)
    `catchError` \_ -> parseFrom (A.String t) $ expected "a timestamp in measure|beats, seconds, or minutes:seconds"

parseMeasureBeats :: (Monad m) => StackParser m T.Text U.MeasureBeats
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

instance StackJSON Plan where
  stackJSON = StackCodec
    { stackParse = decideKey
      [ ("mogg-md5", object $ do
        _moggMD5 <- requiredKey "mogg-md5" fromJSON
        _moggParts <- requiredKey "parts" fromJSON
        _moggCrowd <- fromMaybe [] <$> optionalKey "crowd" fromJSON
        _pans <- requiredKey "pans" fromJSON
        _vols <- requiredKey "vols" fromJSON
        _planComments <- fromMaybe [] <$> optionalKey "comments" fromJSON
        _karaoke    <- fromMaybe False          <$> optionalKey "karaoke"    fromJSON
        _multitrack <- fromMaybe (not _karaoke) <$> optionalKey "multitrack" fromJSON
        expectedKeys ["mogg-md5", "parts", "crowd", "pans", "vols", "comments", "karaoke", "multitrack"]
        return MoggPlan{..}
        )
      ] $ object $ do
        _song <- optionalKey "song" fromJSON
        _countin <- fromMaybe (Countin []) <$> optionalKey "countin" fromJSON
        _planParts <- fromMaybe (Parts Map.empty) <$> optionalKey "parts" fromJSON
        _crowd <- optionalKey "crowd" fromJSON
        _planComments <- fromMaybe [] <$> optionalKey "comments" fromJSON
        expectedKeys ["song", "countin", "parts", "crowd", "comments"]
        return Plan{..}
    , stackShow = \case
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
    }

instance StackJSON Drums.Audio where
  stackJSON = StackCodec
    { stackParse = fromJSON >>= \n -> case n :: Int of
      0 -> return Drums.D0
      1 -> return Drums.D1
      2 -> return Drums.D2
      3 -> return Drums.D3
      4 -> return Drums.D4
      _ -> expected "a valid drum mix mode (a number 0 through 4)"
    , stackShow = A.toJSON . fromEnum
    }

data AudioInput
  = Named T.Text
  | JammitSelect J.AudioPart T.Text
  deriving (Eq, Ord, Show, Read)

instance StackJSON AudioInput where
  stackJSON = StackCodec
    { stackParse = decideKey
      [ ("only", do
        algebraic2 "only"
          (\part str -> JammitSelect (J.Only part) str)
          (fromJSON >>= \title -> case J.titleToPart title of
            Just part -> return part
            Nothing   -> expected "a Jammit part name"
            )
          fromJSON
        )
      , ("without", do
        algebraic2 "without"
          (\inst str -> JammitSelect (J.Without $ jammitInstrument inst) str)
          fromJSON
          fromJSON
        )
      ] (Named <$> fromJSON)
    , stackShow = \case
      Named t -> toJSON t
      JammitSelect (J.Only p) t -> A.object ["only" .= [toJSON $ jammitPartToTitle p, toJSON t]]
      JammitSelect (J.Without i) t -> A.object ["without" .= [toJSON $ fromJammitInstrument i, toJSON t]]
    }

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

instance StackJSON Edge where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.String "start" -> return Start
      A.String "begin" -> return Start
      A.String "end"   -> return End
      _                -> expected "an audio edge (start or end)"
    , stackShow = \case
      Start -> "start"
      End -> "end"
    }

algebraic1 :: (Monad m) => T.Text -> (a -> b) -> StackParser m A.Value a -> StackParser m A.Value b
algebraic1 k f p1 = object $ onlyKey k $ do
  x <- lift ask
  fmap f $ inside "ADT field 1 of 1" $ parseFrom x p1

algebraic2 :: (Monad m) => T.Text -> (a -> b -> c) ->
  StackParser m A.Value a -> StackParser m A.Value b -> StackParser m A.Value c
algebraic2 k f p1 p2 = object $ onlyKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y] -> f
      <$> do inside "ADT field 1 of 2" $ parseFrom x p1
      <*> do inside "ADT field 2 of 2" $ parseFrom y p2
    _ -> expected "an array of 2 ADT fields"
  _ -> expected "an array of 2 ADT fields"

algebraic3 :: (Monad m) => T.Text -> (a -> b -> c -> d) ->
  StackParser m A.Value a -> StackParser m A.Value b -> StackParser m A.Value c -> StackParser m A.Value d
algebraic3 k f p1 p2 p3 = object $ onlyKey k $ lift ask >>= \case
  A.Array v -> case V.toList v of
    [x, y, z] -> f
      <$> do inside "ADT field 1 of 3" $ parseFrom x p1
      <*> do inside "ADT field 2 of 3" $ parseFrom y p2
      <*> do inside "ADT field 3 of 3" $ parseFrom z p3
    _ -> expected "an array of 3 ADT fields"
  _ -> expected "an array of 3 ADT fields"

decideKey :: (Monad m) => [(T.Text, StackParser m A.Value a)] -> StackParser m A.Value a -> StackParser m A.Value a
decideKey opts dft = lift ask >>= \case
  A.Object hm -> case [ p | (k, p) <- opts, Map.member k hm ] of
    p : _ -> p
    []    -> dft
  _ -> dft

instance (StackJSON t, StackJSON a) => StackJSON (Audio t a) where
  stackJSON = StackCodec
    { stackParse = let
      supplyEdge s f = lift ask >>= \case
        OneKey _ (A.Array v)
          | V.length v == 2 -> algebraic2 s (f Start)  fromJSON fromJSON
          | V.length v == 3 -> algebraic3 s f fromJSON fromJSON fromJSON
        _ -> expected $ "2 or 3 fields in the " ++ show s ++ " ADT"
      in decideKey
        [ ("silence", algebraic2 "silence" Silence fromJSON fromJSON)
        , ("mix"        , object $ onlyKey "mix"         $ Mix         <$> fromJSON)
        , ("merge"      , object $ onlyKey "merge"       $ Merge       <$> fromJSON)
        , ("concatenate", object $ onlyKey "concatenate" $ Concatenate <$> fromJSON)
        , ("gain", algebraic2 "gain" Gain fromJSON fromJSON)
        , ("take", supplyEdge "take" Take)
        , ("drop", supplyEdge "drop" Drop)
        , ("trim", supplyEdge "trim" Drop)
        , ("fade", supplyEdge "fade" Fade)
        , ("pad" , supplyEdge "pad"  Pad )
        , ("resample", algebraic1 "resample" Resample fromJSON)
        , ("channels", algebraic2 "channels" Channels fromJSON fromJSON)
        , ("stretch", algebraic2 "stretch" StretchSimple fromJSON fromJSON)
        , ("stretch", algebraic3 "stretch" StretchFull fromJSON fromJSON fromJSON)
        , ("mask", algebraic3 "mask" Mask fromJSON fromJSON fromJSON)
        ] (fmap Input fromJSON `catchError` \_ -> expected "an audio expression")
    , stackShow = \case
      Silence chans t -> A.object ["silence" .= [toJSON chans, toJSON t]]
      Input x -> toJSON x
      Mix auds -> A.object ["mix" .= auds]
      Merge auds -> A.object ["merge" .= auds]
      Concatenate auds -> A.object ["concatenate" .= auds]
      Gain d aud -> A.object ["gain" .= [toJSON d, toJSON aud]]
      Take e t aud -> A.object ["take" .= [toJSON e, toJSON t, toJSON aud]]
      Drop e t aud -> A.object ["drop" .= [toJSON e, toJSON t, toJSON aud]]
      Fade e t aud -> A.object ["fade" .= [toJSON e, toJSON t, toJSON aud]]
      Pad e t aud -> A.object ["pad" .= [toJSON e, toJSON t, toJSON aud]]
      Resample aud -> A.object ["resample" .= aud]
      Channels ns aud -> A.object ["channels" .= [toJSON ns, toJSON aud]]
      StretchSimple d aud -> A.object ["stretch" .= [toJSON d, toJSON aud]]
      StretchFull t p aud -> A.object ["stretch" .= [toJSON t, toJSON p, toJSON aud]]
      Mask tags seams aud -> A.object ["mask" .= [toJSON tags, toJSON seams, toJSON aud]]
    }

(.=) :: (StackJSON a) => T.Text -> a -> (T.Text, A.Value)
k .= x = (k, toJSON x)

instance (StackJSON t) => StackJSON (Seam t) where
  stackJSON = StackCodec
    { stackParse = object $ do
      seamCenter <- requiredKey "center" (stackParse stackJSON)
      valueFade <- fromMaybe (A.Number 0) <$> optionalKey "fade" (stackParse stackJSON)
      seamFade <- parseFrom valueFade (stackParse stackJSON)
      seamTag <- requiredKey "tag" (stackParse stackJSON)
      expectedKeys ["center", "fade", "tag"]
      return Seam{..}
    , stackShow = \Seam{..} -> A.object
      [ "center" .= seamCenter
      , "fade" .= seamFade
      , "tag" .= seamTag
      ]
    }

parseMinutes :: (Monad m) => StackParser m A.Value Scientific
parseMinutes = lift ask >>= \case
  A.String minstr
    | (minutes@(_:_), ':' : secstr) <- span isDigit $ T.unpack minstr
    , Just seconds <- readMaybe secstr
    -> return $ read minutes * 60 + seconds
  A.String secstr
    | Just seconds <- readMaybe $ T.unpack secstr
    -> return seconds
  _ -> stackParse stackJSON -- will succeed if JSON number

showTimestamp :: Milli -> A.Value
showTimestamp s = let
  mins = floor $ s / 60 :: Int
  secs = s - fromIntegral mins * 60
  in case mins of
    0 -> A.toJSON s
    _ -> A.toJSON $ show mins ++ ":" ++ (if secs < 10 then "0" else "") ++ show secs

instance StackJSON Duration where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      OneKey "frames" v -> inside "frames duration" $ Frames <$> parseFrom v fromJSON
      OneKey "seconds" v -> inside "seconds duration" $ Seconds . toRealFloat <$> parseFrom v parseMinutes
      _ -> inside "unitless (seconds) duration" (Seconds . toRealFloat <$> parseMinutes)
        `catchError` \_ -> expected "a duration in frames or seconds"
    , stackShow = \case
      Frames f -> A.object ["frames" .= f]
      Seconds s -> showTimestamp $ realToFrac s
    }

instance StackJSON FlexPartName where
  stackJSON = StackCodec
    { stackParse = fmap readPartName fromJSON
    , stackShow = A.toJSON . getPartName
    }

data Difficulty
  = Tier Integer -- ^ [1..7]: 1 = no dots, 7 = devil dots
  | Rank Integer -- ^ [1..]
  deriving (Eq, Ord, Show, Read)

instance StackJSON Difficulty where
  stackJSON = StackCodec
    { stackShow = \case
      Tier i -> A.object ["tier" .= i]
      Rank i -> A.object ["rank" .= i]
    , stackParse = lift ask >>= \case
      OneKey "tier" (A.Number n) -> return $ Tier $ round n
      OneKey "rank" (A.Number n) -> return $ Rank $ round n
      A.Number n -> return $ Tier $ round n
      _ -> expected "a difficulty value (tier or rank)"
    }

data PartGRYBO = PartGRYBO
  { gryboDifficulty    :: Difficulty
  , gryboHopoThreshold :: Int
  , gryboFixFreeform   :: Bool
  } deriving (Eq, Ord, Show, Read)

instance StackJSON PartGRYBO where
  stackJSON = asStrictObject "PartGRYBO" $ do
    gryboDifficulty    <- gryboDifficulty    =. fill (Tier 1) "difficulty"     stackJSON
    gryboHopoThreshold <- gryboHopoThreshold =. opt  170      "hopo-threshold" stackJSON
    gryboFixFreeform   <- gryboFixFreeform   =. opt  True     "fix-freeform"   stackJSON
    return PartGRYBO{..}

data PartProKeys = PartProKeys
  { pkDifficulty  :: Difficulty
  , pkFixFreeform :: Bool
  } deriving (Eq, Ord, Show, Read)

instance StackJSON PartProKeys where
  stackJSON = asStrictObject "PartProKeys" $ do
    pkDifficulty  <- pkDifficulty  =. fill (Tier 1) "difficulty"   stackJSON
    pkFixFreeform <- pkFixFreeform =. opt  True     "fix-freeform" stackJSON
    return PartProKeys{..}

data PartProGuitar = PartProGuitar
  { pgDifficulty    :: Difficulty
  , pgHopoThreshold :: Int
  , pgTuning        :: [Int]
  , pgFixFreeform   :: Bool
  } deriving (Eq, Ord, Show, Read)

instance StackJSON PartProGuitar where
  stackJSON = asStrictObject "PartProGuitar" $ do
    pgDifficulty    <- pgDifficulty    =. fill (Tier 1) "difficulty"     stackJSON
    pgHopoThreshold <- pgHopoThreshold =. opt  170      "hopo-threshold" stackJSON
    pgTuning        <- pgTuning        =. opt  []       "tuning"         stackJSON
    pgFixFreeform   <- pgFixFreeform   =. opt  True     "fix-freeform"   stackJSON
    return PartProGuitar{..}

data DrumKit
  = HardRockKit
  | ArenaKit
  | VintageKit
  | TrashyKit
  | ElectronicKit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON DrumKit where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.Null     -> return HardRockKit
      A.String t         -> case readMaybe $ filter (/= ' ') $ T.unpack t of
        Just kit -> return kit
        Nothing  -> expected "the name of a drum kit or null"
      _                  -> expected "the name of a drum kit or null"
    , stackShow = \case
      HardRockKit -> "Hard Rock Kit"
      ArenaKit -> "Arena Kit"
      VintageKit -> "Vintage Kit"
      TrashyKit -> "Trashy Kit"
      ElectronicKit -> "Electronic Kit"
    }

data DrumLayout
  = StandardLayout
  | FlipYBToms
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON DrumLayout where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.Null                     -> return StandardLayout
      A.String "standard-layout" -> return StandardLayout
      A.String "flip-yb-toms"    -> return FlipYBToms
      _                          -> expected "the name of a drum kit layout or null"
    , stackShow = \case
      StandardLayout -> "standard-layout"
      FlipYBToms     -> "flip-yb-toms"
    }

data PartDrums = PartDrums
  { drumsDifficulty  :: Difficulty
  , drumsPro         :: Bool
  , drumsAuto2xBass  :: Bool
  , drumsFixFreeform :: Bool
  , drumsKit         :: DrumKit
  , drumsLayout      :: DrumLayout
  } deriving (Eq, Ord, Show, Read)

instance StackJSON PartDrums where
  stackJSON = asStrictObject "PartDrums" $ do
    drumsDifficulty  <- drumsDifficulty  =. fill (Tier 1)       "difficulty"   stackJSON
    drumsPro         <- drumsPro         =. opt  True           "pro"          stackJSON
    drumsAuto2xBass  <- drumsAuto2xBass  =. opt  False          "auto-2x-bass" stackJSON
    drumsFixFreeform <- drumsFixFreeform =. opt  True           "fix-freeform" stackJSON
    drumsKit         <- drumsKit         =. opt  HardRockKit    "kit"          stackJSON
    drumsLayout      <- drumsLayout      =. opt  StandardLayout "layout"       stackJSON
    return PartDrums{..}

data VocalCount = Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON VocalCount where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.Number 1   -> return Vocal1
      A.Number 2   -> return Vocal2
      A.Number 3   -> return Vocal3
      _            -> expected "a vocal part count (1 to 3)"
    , stackShow = \c -> A.toJSON $ fromEnum c + 1
    }

data PartVocal = PartVocal
  { vocalDifficulty :: Difficulty
  , vocalCount      :: VocalCount
  , vocalGender     :: Maybe Magma.Gender
  } deriving (Eq, Ord, Show, Read)

instance StackJSON PartVocal where
  stackJSON = asStrictObject "PartVocal" $ do
    vocalDifficulty <- vocalDifficulty =. fill (Tier 1) "difficulty" stackJSON
    vocalCount      <- vocalCount      =. opt  Vocal1   "count"      stackJSON
    vocalGender     <- vocalGender     =. opt  Nothing  "gender"     stackJSON
    return PartVocal{..}

data Part = Part
  { partGRYBO     :: Maybe PartGRYBO
  , partProKeys   :: Maybe PartProKeys
  , partProGuitar :: Maybe PartProGuitar
  , partDrums     :: Maybe PartDrums
  , partVocal     :: Maybe PartVocal
  } deriving (Eq, Ord, Show, Read)

instance StackJSON Part where
  stackJSON = asStrictObject "Part" $ do
    partGRYBO     <- partGRYBO     =. opt Nothing "grybo"      stackJSON
    partProKeys   <- partProKeys   =. opt Nothing "pro-keys"   stackJSON
    partProGuitar <- partProGuitar =. opt Nothing "pro-guitar" stackJSON
    partDrums     <- partDrums     =. opt Nothing "drums"      stackJSON
    partVocal     <- partVocal     =. opt Nothing "vocal"      stackJSON
    return Part{..}

instance Default Part where
  def = fromEmptyObject

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

instance StackJSON AutogenTheme where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
      A.Null             -> return AutogenDefault
      A.String "Default" -> return AutogenDefault
      A.String t         -> case readMaybe $ filter (/= ' ') $ T.unpack t of
        Just theme -> return theme
        Nothing    -> expected "the name of an autogen theme or null"
      _                  -> expected "the name of an autogen theme or null"
    , stackShow = A.toJSON . show -- maybe put spaces in here later
    }

data Rating
  = FamilyFriendly
  | SupervisionRecommended
  | Mature
  | Unrated
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON Rating where
  stackJSON = StackCodec
    { stackParse = lift ask >>= \case
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
    , stackShow = A.toJSON . show -- maybe put spaces in here later
    }

data PreviewTime
  = PreviewSection T.Text
  | PreviewMIDI    U.MeasureBeats
  | PreviewSeconds U.Seconds
  deriving (Eq, Ord, Show)

instance StackJSON PreviewTime where
  stackJSON = StackCodec
    { stackParse = let
      traceNum = do
        d <- fromJSON
        return $ PreviewSeconds $ realToFrac (d :: Double)
      traceStr = do
        str <- fromJSON
        case T.stripPrefix "prc_" str of
          Just prc -> return $ PreviewSection prc
          Nothing -> let
            p = parseFrom str $ either PreviewMIDI PreviewSeconds <$> parseCountinTime
            in p `catchError` \_ -> expected "a preview time: prc_something, timestamp, or measure|beats"
      in traceNum `catchError` \_ -> traceStr
    , stackShow = \case
      PreviewSection str -> A.toJSON $ "prc_" <> str
      PreviewMIDI mb -> A.toJSON $ showMeasureBeats mb
      PreviewSeconds secs -> showTimestamp $ realToFrac secs
    }

-- | Extra information with no gameplay effect.
data Metadata = Metadata
  { _title        :: Maybe T.Text
  , _artist       :: Maybe T.Text
  , _album        :: Maybe T.Text
  , _genre        :: Maybe T.Text
  , _subgenre     :: Maybe T.Text
  , _year         :: Maybe Int
  , _fileAlbumArt :: Maybe FilePath
  , _trackNumber  :: Maybe Int
  , _comments     :: [T.Text]
  , _key          :: Maybe Key
  , _autogenTheme :: AutogenTheme
  , _author       :: Maybe T.Text
  , _rating       :: Rating
  , _previewStart :: Maybe PreviewTime
  , _previewEnd   :: Maybe PreviewTime
  , _languages    :: [T.Text]
  , _convert      :: Bool
  , _rhythmKeys   :: Bool
  , _rhythmBass   :: Bool
  , _catEMH       :: Bool
  , _expertOnly   :: Bool
  , _cover        :: Bool
  , _difficulty   :: Difficulty
  } deriving (Eq, Show)

instance StackJSON Metadata where
  stackJSON = asStrictObject "Metadata" $ do
    _title        <- _title        =. warning Nothing        "title"          stackJSON
    _artist       <- _artist       =. warning Nothing        "artist"         stackJSON
    _album        <- _album        =. warning Nothing        "album"          stackJSON
    _genre        <- _genre        =. warning Nothing        "genre"          stackJSON
    _subgenre     <- _subgenre     =. warning Nothing        "subgenre"       stackJSON
    _year         <- _year         =. warning Nothing        "year"           stackJSON
    _fileAlbumArt <- _fileAlbumArt =. warning Nothing        "file-album-art" stackJSON
    _trackNumber  <- _trackNumber  =. warning Nothing        "track-number"   stackJSON
    _comments     <- _comments     =. opt     []             "comments"       stackJSON
    _key          <- _key          =. opt     Nothing        "key"            stackJSON
    _autogenTheme <- _autogenTheme =. opt     AutogenDefault "autogen-theme"  stackJSON
    _author       <- _author       =. warning Nothing        "author"         stackJSON
    _rating       <- _rating       =. opt     Unrated        "rating"         stackJSON
    _previewStart <- _previewStart =. opt     Nothing        "preview-start"  stackJSON
    _previewEnd   <- _previewEnd   =. opt     Nothing        "preview-end"    stackJSON
    _languages    <- _languages    =. opt     []             "languages"      stackJSON
    _convert      <- _convert      =. opt     False          "convert"        stackJSON
    _rhythmKeys   <- _rhythmKeys   =. opt     False          "rhythm-keys"    stackJSON
    _rhythmBass   <- _rhythmBass   =. opt     False          "rhythm-bass"    stackJSON
    _catEMH       <- _catEMH       =. opt     False          "cat-emh"        stackJSON
    _expertOnly   <- _expertOnly   =. opt     False          "expert-only"    stackJSON
    _cover        <- _cover        =. opt     False          "cover"          stackJSON
    _difficulty   <- _difficulty   =. fill    (Tier 1)       "difficulty"     stackJSON
    return Metadata{..}

instance Default Metadata where
  def = fromEmptyObject

getTitle, getArtist, getAlbum, getAuthor :: Metadata -> T.Text
getTitle = fromMaybe "Untitled" . _title
getArtist = fromMaybe "Unknown Artist" . _artist
getAlbum = fromMaybe "Unknown Album" . _album
getAuthor = fromMaybe "Unknown Author" . _author

getYear, getTrackNumber :: Metadata -> Int
getYear = fromMaybe 1960 . _year
getTrackNumber = fromMaybe 1 . _trackNumber

data TargetRB3 = TargetRB3
  { rb3_Speed       :: Maybe Double
  , rb3_Plan        :: Maybe T.Text
  , rb3_2xBassPedal :: Bool
  , rb3_SongID      :: Maybe (Either Integer T.Text)
  , rb3_Label       :: Maybe T.Text
  , rb3_Version     :: Maybe Integer
  , rb3_Guitar      :: FlexPartName
  , rb3_Bass        :: FlexPartName
  , rb3_Drums       :: FlexPartName
  , rb3_Keys        :: FlexPartName
  , rb3_Vocal       :: FlexPartName
  } deriving (Eq, Ord, Show, Read, Generic, Hashable)

parseTargetRB3 :: (Monad m) => ObjectCodec m TargetRB3
parseTargetRB3 = do
  rb3_Speed       <- rb3_Speed       =. opt Nothing    "speed"         stackJSON
  rb3_Plan        <- rb3_Plan        =. opt Nothing    "plan"          stackJSON
  rb3_2xBassPedal <- rb3_2xBassPedal =. opt False      "2x-bass-pedal" stackJSON
  rb3_SongID      <- rb3_SongID      =. opt Nothing    "song-id"       stackJSON
  rb3_Label       <- rb3_Label       =. opt Nothing    "label"         stackJSON
  rb3_Version     <- rb3_Version     =. opt Nothing    "version"       stackJSON
  rb3_Guitar      <- rb3_Guitar      =. opt FlexGuitar "guitar"        stackJSON
  rb3_Bass        <- rb3_Bass        =. opt FlexBass   "bass"          stackJSON
  rb3_Drums       <- rb3_Drums       =. opt FlexDrums  "drums"         stackJSON
  rb3_Keys        <- rb3_Keys        =. opt FlexKeys   "keys"          stackJSON
  rb3_Vocal       <- rb3_Vocal       =. opt FlexVocal  "vocal"         stackJSON
  return TargetRB3{..}

instance StackJSON TargetRB3 where
  stackJSON = asStrictObject "TargetRB3" parseTargetRB3

instance Default TargetRB3 where
  def = fromEmptyObject

data TargetRB2 = TargetRB2
  { rb2_Speed       :: Maybe Double
  , rb2_Plan        :: Maybe T.Text
  , rb2_2xBassPedal :: Bool
  , rb2_SongID      :: Maybe (Either Integer T.Text)
  , rb2_Label       :: Maybe T.Text
  , rb2_Version     :: Maybe Integer
  , rb2_Guitar      :: FlexPartName
  , rb2_Bass        :: FlexPartName
  , rb2_Drums       :: FlexPartName
  , rb2_Vocal       :: FlexPartName
  } deriving (Eq, Ord, Show, Read, Generic, Hashable)

parseTargetRB2 :: (Monad m) => ObjectCodec m TargetRB2
parseTargetRB2 = do
  rb2_Speed       <- rb2_Speed       =. opt Nothing    "speed"         stackJSON
  rb2_Plan        <- rb2_Plan        =. opt Nothing    "plan"          stackJSON
  rb2_2xBassPedal <- rb2_2xBassPedal =. opt False      "2x-bass-pedal" stackJSON
  rb2_SongID      <- rb2_SongID      =. opt Nothing    "song-id"       stackJSON
  rb2_Label       <- rb2_Label       =. opt Nothing    "label"         stackJSON
  rb2_Version     <- rb2_Version     =. opt Nothing    "version"       stackJSON
  rb2_Guitar      <- rb2_Guitar      =. opt FlexGuitar "guitar"        stackJSON
  rb2_Bass        <- rb2_Bass        =. opt FlexBass   "bass"          stackJSON
  rb2_Drums       <- rb2_Drums       =. opt FlexDrums  "drums"         stackJSON
  rb2_Vocal       <- rb2_Vocal       =. opt FlexVocal  "vocal"         stackJSON
  return TargetRB2{..}

instance StackJSON TargetRB2 where
  stackJSON = asStrictObject "TargetRB2" parseTargetRB2

instance Default TargetRB2 where
  def = fromEmptyObject

data TargetPS = TargetPS
  { ps_Speed      :: Maybe Double
  , ps_Plan       :: Maybe T.Text
  , ps_Label      :: Maybe T.Text
  , ps_FileVideo  :: Maybe FilePath
  , ps_Guitar     :: FlexPartName
  , ps_Bass       :: FlexPartName
  , ps_Drums      :: FlexPartName
  , ps_Keys       :: FlexPartName
  , ps_Vocal      :: FlexPartName
  , ps_Rhythm     :: FlexPartName
  , ps_GuitarCoop :: FlexPartName
  } deriving (Eq, Ord, Show, Read, Generic, Hashable)

parseTargetPS :: (Monad m) => ObjectCodec m TargetPS
parseTargetPS = do
  ps_Speed      <- ps_Speed      =. opt Nothing                   "speed"       stackJSON
  ps_Plan       <- ps_Plan       =. opt Nothing                   "plan"        stackJSON
  ps_Label      <- ps_Label      =. opt Nothing                   "label"       stackJSON
  ps_FileVideo  <- ps_FileVideo  =. opt Nothing                   "file-video"  stackJSON
  ps_Guitar     <- ps_Guitar     =. opt FlexGuitar                "guitar"      stackJSON
  ps_Bass       <- ps_Bass       =. opt FlexBass                  "bass"        stackJSON
  ps_Drums      <- ps_Drums      =. opt FlexDrums                 "drums"       stackJSON
  ps_Keys       <- ps_Keys       =. opt FlexKeys                  "keys"        stackJSON
  ps_Vocal      <- ps_Vocal      =. opt FlexVocal                 "vocal"       stackJSON
  ps_Rhythm     <- ps_Rhythm     =. opt (FlexExtra "rhythm"     ) "rhythm"      stackJSON
  ps_GuitarCoop <- ps_GuitarCoop =. opt (FlexExtra "guitar-coop") "guitar-coop" stackJSON
  return TargetPS{..}

instance StackJSON TargetPS where
  stackJSON = asStrictObject "TargetPS" parseTargetPS

instance Default TargetPS where
  def = fromEmptyObject

data Target
  = RB3 TargetRB3
  | RB2 TargetRB2
  | PS  TargetPS
  deriving (Eq, Ord, Show, Read, Generic, Hashable)

addKey :: (forall m. (Monad m) => ObjectCodec m a) -> T.Text -> A.Value -> a -> A.Value
addKey codec k v x = A.Object $ Map.insert k v $ makeObject codec x

instance StackJSON Target where
  stackJSON = StackCodec
    { stackParse = object $ do
      target <- requiredKey "game" fromJSON
      hm <- lift ask
      parseFrom (A.Object $ Map.delete "game" hm) $ case target :: T.Text of
        "rb3" -> fmap RB3 fromJSON
        "rb2" -> fmap RB2 fromJSON
        "ps"  -> fmap PS  fromJSON
        _     -> fatal $ "Unrecognized target game: " ++ show target
    , stackShow = \case
      RB3 rb3 -> addKey parseTargetRB3 "game" "rb3" rb3
      RB2 rb2 -> addKey parseTargetRB2 "game" "rb2" rb2
      PS  ps  -> addKey parseTargetPS  "game" "ps"  ps
    }

data SongYaml = SongYaml
  { _metadata :: Metadata
  , _audio    :: Map.HashMap T.Text AudioFile
  , _jammit   :: Map.HashMap T.Text JammitTrack
  , _plans    :: Map.HashMap T.Text Plan
  , _targets  :: Map.HashMap T.Text Target
  , _parts    :: Parts Part
  } deriving (Eq, Show)

instance StackJSON SongYaml where
  stackJSON = asStrictObject "SongYaml" $ do
    _metadata <- _metadata =. opt def       "metadata" stackJSON
    _audio    <- _audio    =. opt Map.empty "audio"    (dict stackJSON)
    _jammit   <- _jammit   =. opt Map.empty "jammit"   (dict stackJSON)
    _plans    <- _plans    =. opt Map.empty "plans"    (dict stackJSON)
    _targets  <- _targets  =. opt Map.empty "targets"  (dict stackJSON)
    _parts    <- _parts    =. req           "parts"    stackJSON
    return SongYaml{..}

getPart :: FlexPartName -> SongYaml -> Maybe Part
getPart fpart = Map.lookup fpart . getParts . _parts
