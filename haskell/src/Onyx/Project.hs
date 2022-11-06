{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
module Onyx.Project where

import           Control.Applicative                  (liftA2)
import           Control.Arrow                        (first)
import           Control.Monad.Codec                  (CodecFor (..), (=.))
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                           as A
import qualified Data.Aeson.KeyMap                    as KM
import           Data.Char                            (isDigit, isSpace)
import           Data.Conduit.Audio                   (Duration (..))
import           Data.Default.Class
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Fixed                           (Milli)
import           Data.Foldable                        (find, toList)
import           Data.Hashable                        (Hashable (..))
import qualified Data.HashMap.Strict                  as HM
import           Data.Int                             (Int32)
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Scientific                      (Scientific, toRealFloat)
import           Data.String                          (IsString (..))
import qualified Data.Text                            as T
import           Data.Traversable
import qualified Data.Vector                          as V
import           GHC.Generics                         (Generic (..))
import qualified Numeric.NonNegative.Class            as NNC
import qualified Onyx.Amplitude.File                  as Amp
import           Onyx.Audio
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.DeriveHelpers
import           Onyx.Drums.OneFoot                   (phaseShiftKicks,
                                                       rockBand1x, rockBand2x)
import qualified Onyx.Harmonix.DTA.Serialize.Magma    as Magma
import           Onyx.Harmonix.DTA.Serialize.RockBand (AnimTempo (..))
import           Onyx.MIDI.Common                     (Key (..), SongKey (..),
                                                       Tonality (..), readpKey,
                                                       showKey,
                                                       songKeyUsesFlats)
import qualified Onyx.MIDI.Common                     as RB
import           Onyx.MIDI.Read                       (mapTrack)
import qualified Onyx.MIDI.Track.Drums                as D
import qualified Onyx.MIDI.Track.Drums.Full           as FD
import           Onyx.MIDI.Track.Events
import           Onyx.MIDI.Track.File                 (FlexPartName (..))
import qualified Onyx.MIDI.Track.File                 as RBFile
import           Onyx.MIDI.Track.ProGuitar            (GtrBase (..),
                                                       GtrTuning (..))
import           Onyx.Preferences                     (MagmaSetting (..))
import           Onyx.StackTrace
import qualified Sound.Jammit.Base                    as J
import qualified Sound.MIDI.Util                      as U
import qualified Text.ParserCombinators.ReadP         as ReadP
import           Text.Read                            (readMaybe)
import qualified Text.Read.Lex                        as Lex

parsePitch :: (SendMessage m) => ValueCodec m A.Value Key
parsePitch = Codec
  { codecOut = makeOut $ A.toJSON . showKey False -- no way of getting accidental
  , codecIn = do
    t <- codecIn stackJSON
    case ReadP.readP_to_S (readpKey <* ReadP.eof) $ T.unpack t of
      (sk, _) : _ -> return sk
      []          -> expected "a key"
  }

parseSongKey :: (SendMessage m) => ValueCodec m A.Value SongKey
parseSongKey = Codec
  { codecOut = makeOut $ \sk@(SongKey k t) -> A.toJSON $ T.concat
    [ showKey (songKeyUsesFlats sk) k
    , case t of Major -> " major"; Minor -> " minor"
    ]
  , codecIn = codecIn stackJSON >>= \t -> let
    parse = do
      key <- readpKey
      tone <- ReadP.choice
        [ ReadP.string " major" >> return Major
        , ReadP.string " minor" >> return Minor
        ,                          return Major
        ]
      ReadP.eof
      return $ SongKey key tone
    in case ReadP.readP_to_S parse $ T.unpack t of
      (sk, _) : _ -> return sk
      []          -> expected "a key and optional tonality"
  }

parseJammitInstrument :: (Monad m) => ValueCodec m A.Value J.Instrument
parseJammitInstrument = enumCodec "a jammit instrument name" $ \case
  J.Guitar   -> "guitar"
  J.Bass     -> "bass"
  J.Drums    -> "drums"
  J.Keyboard -> "keys"
  J.Vocal    -> "vocal"

parseGender :: (Monad m) => ValueCodec m A.Value Magma.Gender
parseGender = enumCodec "a gender (male or female)" $ \case
  Magma.Female -> "female"
  Magma.Male   -> "male"

data AudioInfo f = AudioInfo
  { _md5      :: Maybe T.Text
  , _frames   :: Maybe Integer
  , _filePath :: Maybe f
  , _commands :: [T.Text]
  , _rate     :: Maybe Int
  , _channels :: Int
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AudioFile f
  = AudioFile (AudioInfo f)
  | AudioSnippet
    { _expr :: Audio Duration AudioInput
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (AudioInfo f) where
  stackJSON = asStrictObject "AudioInfo" $ do
    _md5      <- _md5      =. opt Nothing "md5"       stackJSON
    _frames   <- _frames   =. opt Nothing "frames"    stackJSON
    _filePath <- _filePath =. opt Nothing "file-path" stackJSON
    _commands <- _commands =. opt []      "commands"  stackJSON
    _rate     <- _rate     =. opt Nothing "rate"      stackJSON
    _channels <- _channels =. opt 2       "channels"  stackJSON
    return AudioInfo{..}

instance (Eq f, StackJSON f) => StackJSON (AudioFile f) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("expr", object $ do
        _expr <- requiredKey "expr" fromJSON
        expectedKeys ["expr"]
        return AudioSnippet{..}
        )
      ] $ AudioFile <$> codecIn stackJSON
    , codecOut = makeOut $ \case
      AudioFile info   -> makeValue stackJSON info
      AudioSnippet{..} -> A.object
        [ "expr" .= toJSON _expr
        ]
    }

data JammitTrack = JammitTrack
  { _jammitTitle  :: Maybe T.Text
  , _jammitArtist :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance StackJSON JammitTrack where
  stackJSON = asStrictObject "JammitTrack" $ do
    _jammitTitle  <- _jammitTitle  =. opt Nothing "title"  stackJSON
    _jammitArtist <- _jammitArtist =. opt Nothing "artist" stackJSON
    return JammitTrack{..}

data PlanAudio t a = PlanAudio
  { _planExpr :: Audio t a
  , _planPans :: [Double]
  , _planVols :: [Double]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON t, StackJSON a) => StackJSON (PlanAudio t a) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("expr", object $ do
        _planExpr <- requiredKey "expr" fromJSON
        _planPans <- fromMaybe [] <$> optionalKey "pans" fromJSON
        _planVols <- fromMaybe [] <$> optionalKey "vols" fromJSON
        expectedKeys ["expr", "pans", "vols"]
        return PlanAudio{..}
        )
      ] $ (\expr -> PlanAudio expr [] []) <$> fromJSON
    , codecOut = makeOut $ \case
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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (PartAudio a) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("kit", object $ do
        drumsSplitKick <- optionalKey "kick" fromJSON
        drumsSplitSnare <- optionalKey "snare" fromJSON
        drumsSplitKit <- requiredKey "kit" fromJSON
        expectedKeys ["kick", "snare", "kit"]
        return PartDrumKit{..}
        )
      ] $ PartSingle <$> fromJSON
    , codecOut = makeOut $ \case
      PartSingle x -> toJSON x
      PartDrumKit{..} -> A.object $ concat
        [ map ("kick"  .=) $ toList drumsSplitKick
        , map ("snare" .=) $ toList drumsSplitSnare
        , ["kit" .= drumsSplitKit]
        ]
    }

newtype Parts a = Parts { getParts :: HM.HashMap FlexPartName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (Parts a) where
  stackJSON = Codec
    { codecIn = Parts . HM.fromList . map (first RBFile.readPartName) . HM.toList
      <$> mapping fromJSON
    , codecOut = makeOut $ \(Parts hm) -> mappingToJSON $ HM.fromList
      $ map (first RBFile.getPartName) $ HM.toList hm
    }

data Plan f
  = Plan
    { _song         :: Maybe (PlanAudio Duration AudioInput)
    , _countin      :: Countin
    , _planParts    :: Parts (PartAudio (PlanAudio Duration AudioInput))
    , _crowd        :: Maybe (PlanAudio Duration AudioInput)
    , _planComments :: [T.Text]
    , _tuningCents  :: Int
    , _fileTempo    :: Maybe f
    }
  | MoggPlan
    { _fileMOGG      :: Maybe f
    , _moggMD5       :: Maybe T.Text
    , _moggParts     :: Parts (PartAudio [Int])
    , _moggCrowd     :: [Int]
    , _pans          :: [Double]
    , _vols          :: [Double]
    , _planComments  :: [T.Text]
    , _karaoke       :: Bool
    , _multitrack    :: Bool
    , _tuningCents   :: Int
    , _fileTempo     :: Maybe f
    , _decryptSilent :: Bool -- should encrypted audio just be treated as silent
    }
  deriving (Eq, Show, Functor, Foldable, Traversable)

getKaraoke, getMultitrack :: Plan f -> Bool
getKaraoke = \case
  Plan{..}     -> HM.keys (getParts _planParts) == [FlexVocal]
  MoggPlan{..} -> _karaoke
getMultitrack = \case
  Plan{..}     -> not $ HM.null $ HM.delete FlexVocal $ getParts _planParts
  MoggPlan{..} -> _multitrack

newtype Countin = Countin [(Either U.MeasureBeats U.Seconds, Audio Duration AudioInput)]
  deriving (Eq, Ord, Show)

instance StackJSON Countin where
  stackJSON = Codec
    { codecIn = do
      hm <- mapping fromJSON
      fmap Countin $ forM (HM.toList hm) $ \(k, v) -> (, v) <$> parseFrom k parseCountinTime
    , codecOut = makeOut $ \(Countin pairs) -> A.Object $ KM.fromHashMapText $ HM.fromList $ flip map pairs $ \(t, v) -> let
      k = case t of
        Left mb    -> showMeasureBeats mb
        Right secs -> T.pack $ show (realToFrac secs :: Milli)
      in (k, toJSON v)
    }

-- | Parses any of \"measure|beats\", \"seconds\", or \"minutes:seconds\".
parseCountinTime :: (SendMessage m) => StackParser m T.Text (Either U.MeasureBeats U.Seconds)
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
    _            -> ReadP.pfail
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

instance (StackJSON f) => StackJSON (Plan f) where
  stackJSON = Codec
    { codecIn = let
      parseMOGG = object $ do
        _fileMOGG      <- optionalKey "file-mogg" fromJSON
        _moggMD5       <- optionalKey "mogg-md5" fromJSON
        _moggParts     <- requiredKey "parts" fromJSON
        _moggCrowd     <- fromMaybe [] <$> optionalKey "crowd" fromJSON
        _pans          <- requiredKey "pans" fromJSON
        _vols          <- requiredKey "vols" fromJSON
        _planComments  <- fromMaybe [] <$> optionalKey "comments" fromJSON
        _karaoke       <- fromMaybe False          <$> optionalKey "karaoke"    fromJSON
        _multitrack    <- fromMaybe (not _karaoke) <$> optionalKey "multitrack" fromJSON
        _tuningCents   <- fromMaybe 0 <$> optionalKey "tuning-cents" fromJSON
        _fileTempo     <- fromMaybe Nothing <$> optionalKey "file-tempo" fromJSON
        _decryptSilent <- fromMaybe False <$> optionalKey "decrypt-silent" fromJSON
        expectedKeys
          [ "file-mogg", "mogg-md5", "parts", "crowd", "pans", "vols"
          , "comments", "karaoke", "multitrack", "silent", "tuning-cents"
          , "file-tempo", "decrypt-silent"
          ]
        return MoggPlan{..}
      parseNormal = object $ do
        _song         <- optionalKey "song" fromJSON
        _countin      <- fromMaybe (Countin []) <$> optionalKey "countin" fromJSON
        _planParts    <- fromMaybe (Parts HM.empty) <$> optionalKey "parts" fromJSON
        _crowd        <- optionalKey "crowd" fromJSON
        _planComments <- fromMaybe [] <$> optionalKey "comments" fromJSON
        _tuningCents  <- fromMaybe 0 <$> optionalKey "tuning-cents" fromJSON
        _fileTempo    <- fromMaybe Nothing <$> optionalKey "file-tempo" fromJSON
        expectedKeys ["song", "countin", "parts", "crowd", "comments", "tuning-cents", "file-tempo"]
        return Plan{..}
      in decideKey [("mogg-md5", parseMOGG), ("file-mogg", parseMOGG)] parseNormal
    , codecOut = makeOut $ \case
      Plan{..} -> A.object $ concat
        [ map ("song" .=) $ toList _song
        , ["countin" .= _countin | _countin /= Countin []]
        , ["parts" .= _planParts]
        , map ("crowd" .=) $ toList _crowd
        , ["comments" .= _planComments | not $ null _planComments]
        , ["tuning-cents" .= _tuningCents | _tuningCents /= 0]
        , map ("file-tempo" .=) $ toList _fileTempo
        ]
      MoggPlan{..} -> A.object $ concat
        [ map ("file-mogg" .=) $ toList _fileMOGG
        , map ("mogg-md5" .=) $ toList _moggMD5
        , ["parts" .= _moggParts]
        , ["crowd" .= _moggCrowd | not $ null _moggCrowd]
        , ["pans" .= _pans]
        , ["vols" .= _vols]
        , ["comments" .= _planComments | not $ null _planComments]
        , ["karaoke" .= _karaoke]
        , ["multitrack" .= _multitrack]
        , ["tuning-cents" .= _tuningCents | _tuningCents /= 0]
        , map ("file-tempo" .=) $ toList _fileTempo
        , ["decrypt-silent" .= _decryptSilent | _decryptSilent]
        ]
    }

data AudioInput
  = Named T.Text
  | JammitSelect J.AudioPart T.Text
  deriving (Eq, Ord, Show)

instance StackJSON AudioInput where
  stackJSON = Codec
    { codecIn = decideKey
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
          (\inst str -> JammitSelect (J.Without inst) str)
          (codecIn parseJammitInstrument)
          fromJSON
        )
      ] (Named <$> fromJSON)
    , codecOut = makeOut $ \case
      Named t -> toJSON t
      JammitSelect (J.Only p) t -> A.object
        [ "only" .= [toJSON $ jammitPartToTitle p, toJSON t]
        ]
      JammitSelect (J.Without i) t -> A.object
        [ "without" .= [makeValue parseJammitInstrument i, toJSON t]
        ]
    }

jammitPartToTitle :: J.Part -> T.Text
jammitPartToTitle = \case
  J.PartGuitar1 -> "Guitar 1"
  J.PartGuitar2 -> "Guitar 2"
  J.PartBass1   -> "Bass 1"
  J.PartBass2   -> "Bass 2"
  J.PartDrums1  -> "Drums 1"
  J.PartDrums2  -> "Drums 2"
  J.PartKeys1   -> "Keys 1"
  J.PartKeys2   -> "Keys 2"
  J.PartPiano   -> "Piano"
  J.PartSynth   -> "Synth"
  J.PartOrgan   -> "Organ"
  J.PartVocal   -> "Vocal"
  J.PartBVocals -> "B Vocals"

instance StackJSON Edge where
  stackJSON = enumCodecFull "an audio edge (start or end)" $ \case
    Start -> is "start" |?> is "begin"
    End   -> is "end"

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
  A.Object (KM.toHashMapText -> hm) -> case [ p | (k, p) <- opts, HM.member k hm ] of
    p : _ -> p
    []    -> dft
  _ -> dft

instance (StackJSON t, StackJSON a) => StackJSON (Audio t a) where
  stackJSON = Codec
    { codecIn = let
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
        , ("stretch-full", algebraic3 "stretch-full" StretchFull fromJSON fromJSON fromJSON)
        , ("mask", algebraic3 "mask" Mask fromJSON fromJSON fromJSON)
        ] (fmap Input fromJSON `catchError` \_ -> expected "an audio expression")
    , codecOut = makeOut $ \case
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
      StretchFull t p aud -> A.object ["stretch-full" .= [toJSON t, toJSON p, toJSON aud]]
      Mask tags seams aud -> A.object ["mask" .= [toJSON tags, toJSON seams, toJSON aud]]
    }

(.=) :: (StackJSON a) => A.Key -> a -> (A.Key, A.Value)
k .= x = (k, toJSON x)

instance (StackJSON t) => StackJSON (Seam t) where
  stackJSON = Codec
    { codecIn = object $ do
      seamCenter <- requiredKey "center" (codecIn stackJSON)
      valueFade <- fromMaybe (A.Number 0) <$> optionalKey "fade" (codecIn stackJSON)
      seamFade <- parseFrom valueFade (codecIn stackJSON)
      seamTag <- requiredKey "tag" (codecIn stackJSON)
      expectedKeys ["center", "fade", "tag"]
      return Seam{..}
    , codecOut = makeOut $ \Seam{..} -> A.object
      [ "center" .= seamCenter
      , "fade" .= seamFade
      , "tag" .= seamTag
      ]
    }

parseMinutes :: (SendMessage m) => StackParser m A.Value Scientific
parseMinutes = lift ask >>= \case
  A.String minstr
    | (minutes@(_:_), ':' : secstr) <- span isDigit $ T.unpack minstr
    , Just seconds <- readMaybe secstr
    -> return $ read minutes * 60 + seconds
  A.String secstr
    | Just seconds <- readMaybe $ T.unpack secstr
    -> return seconds
  _ -> codecIn stackJSON -- will succeed if JSON number

showTimestamp :: Milli -> A.Value
showTimestamp s = let
  mins = floor $ s / 60 :: Int
  secs = s - fromIntegral mins * 60
  in case mins of
    0 -> A.toJSON s
    _ -> A.toJSON $ show mins ++ ":" ++ (if secs < 10 then "0" else "") ++ show secs

instance StackJSON Duration where
  stackJSON = Codec
    { codecIn = lift ask >>= \case
      OneKey "frames" v -> inside "frames duration" $ Frames <$> parseFrom v fromJSON
      OneKey "seconds" v -> inside "seconds duration" $ Seconds . toRealFloat <$> parseFrom v parseMinutes
      _ -> inside "unitless (seconds) duration" (Seconds . toRealFloat <$> parseMinutes)
        `catchError` \_ -> expected "a duration in frames or seconds"
    , codecOut = makeOut $ \case
      Frames f  -> A.object ["frames" .= f]
      Seconds s -> showTimestamp $ realToFrac s
    }

instance StackJSON FlexPartName where
  stackJSON = Codec
    { codecIn = RBFile.readPartName <$> fromJSON
    , codecOut = makeOut $ A.toJSON . RBFile.getPartName
    }

data Difficulty
  = Tier Integer -- ^ [1..7]: 1 = no dots, 7 = devil dots
  | Rank Integer -- ^ [1..]
  deriving (Eq, Ord, Show)

instance StackJSON Difficulty where
  stackJSON = Codec
    { codecOut = makeOut $ \case
      Tier i -> A.object ["tier" .= i]
      Rank i -> A.object ["rank" .= i]
    , codecIn = lift ask >>= \case
      OneKey "tier" (A.Number n) -> return $ Tier $ round n
      OneKey "rank" (A.Number n) -> return $ Rank $ round n
      A.Number n                 -> return $ Tier $ round n
      _                          -> expected "a difficulty value (tier or rank)"
    }

data PartGRYBO = PartGRYBO
  { gryboDifficulty    :: Difficulty
  , gryboHopoThreshold :: Int
  , gryboFixFreeform   :: Bool
  , gryboSustainGap    :: Int -- ticks, 480 per beat
  , gryboSmoothFrets   :: Bool -- should animation fret positions be smoothed out like pre-RB3
  } deriving (Eq, Ord, Show)

instance StackJSON PartGRYBO where
  stackJSON = asStrictObject "PartGRYBO" $ do
    gryboDifficulty    <- gryboDifficulty    =. fill (Tier 1) "difficulty"      stackJSON
    gryboHopoThreshold <- gryboHopoThreshold =. opt  170      "hopo-threshold"  stackJSON
    gryboFixFreeform   <- gryboFixFreeform   =. opt  True     "fix-freeform"    stackJSON
    gryboSustainGap    <- gryboSustainGap    =. opt  60       "sustain-gap"     stackJSON
    gryboSmoothFrets   <- gryboSmoothFrets   =. opt  False    "smooth-frets"    stackJSON
    return PartGRYBO{..}

instance Default PartGRYBO where
  def = fromEmptyObject

data PartProKeys = PartProKeys
  { pkDifficulty  :: Difficulty
  , pkFixFreeform :: Bool
  } deriving (Eq, Ord, Show)

instance StackJSON PartProKeys where
  stackJSON = asStrictObject "PartProKeys" $ do
    pkDifficulty  <- pkDifficulty  =. fill (Tier 1) "difficulty"   stackJSON
    pkFixFreeform <- pkFixFreeform =. opt  True     "fix-freeform" stackJSON
    return PartProKeys{..}

data PartProGuitar f = PartProGuitar
  { pgDifficulty    :: Difficulty
  , pgHopoThreshold :: Int
  , pgTuning        :: GtrTuning
  , pgTuningRSBass  :: Maybe GtrTuning -- for 5/6-string bass which also has a 4-string arrangement
  , pgFixFreeform   :: Bool
  , pgTones         :: Maybe (RSTones f)
  , pgPickedBass    :: Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

tuningBaseFormat :: (SendMessage m) => ValueCodec m A.Value GtrBase
tuningBaseFormat = Codec
  { codecIn = lift ask >>= \case
    A.Null              -> return Guitar6
    A.String "guitar-6" -> return Guitar6
    A.String "guitar-7" -> return Guitar7
    A.String "guitar-8" -> return Guitar8
    A.String "bass-4"   -> return Bass4
    A.String "bass-5"   -> return Bass5
    A.String "bass-6"   -> return Bass6
    A.Array _           -> GtrCustom <$> codecIn (listCodec stackJSON)
    _                   -> expected "a guitar/bass tuning base"
  , codecOut = makeOut $ \case
    Guitar6      -> "guitar-6"
    Guitar7      -> "guitar-7"
    Guitar8      -> "guitar-8"
    Bass4        -> "bass-4"
    Bass5        -> "bass-5"
    Bass6        -> "bass-6"
    GtrCustom ps -> A.toJSON ps
  }

tuningFormat :: (SendMessage m) => ValueCodec m A.Value GtrTuning
tuningFormat = asStrictObject "GtrTuning" $ do
  gtrBase    <- gtrBase    =. opt Guitar6 "base"    tuningBaseFormat
  gtrOffsets <- gtrOffsets =. opt []      "offsets" stackJSON
  gtrGlobal  <- gtrGlobal  =. opt 0       "global"  stackJSON
  gtrCapo    <- gtrCapo    =. opt 0       "capo"    stackJSON
  return GtrTuning{..}

instance (Eq f, StackJSON f) => StackJSON (PartProGuitar f) where
  stackJSON = asStrictObject "PartProGuitar" $ do
    pgDifficulty    <- pgDifficulty    =. fill (Tier 1) "difficulty"     stackJSON
    pgHopoThreshold <- pgHopoThreshold =. opt  170      "hopo-threshold" stackJSON
    pgTuning        <- pgTuning        =. opt  def      "tuning"         tuningFormat
    pgTuningRSBass  <- pgTuningRSBass  =. opt  Nothing  "tuning-rs-bass" (maybeCodec tuningFormat)
    pgFixFreeform   <- pgFixFreeform   =. opt  True     "fix-freeform"   stackJSON
    pgTones         <- pgTones         =. opt  Nothing  "tones"          stackJSON
    pgPickedBass    <- pgPickedBass    =. opt  False    "picked-bass"    stackJSON
    return PartProGuitar{..}

instance (Eq f, StackJSON f) => Default (PartProGuitar f) where
  def = fromEmptyObject

data RSTones f = RSTones
  { rsFileToneBase :: f
  , rsFileToneA    :: Maybe f
  , rsFileToneB    :: Maybe f
  , rsFileToneC    :: Maybe f
  , rsFileToneD    :: Maybe f
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (RSTones f) where
  stackJSON = asStrictObject "RSTones" $ do
    rsFileToneBase <- rsFileToneBase =. req         "file-tone-base" stackJSON
    rsFileToneA    <- rsFileToneA    =. opt Nothing "file-tone-a"    stackJSON
    rsFileToneB    <- rsFileToneB    =. opt Nothing "file-tone-b"    stackJSON
    rsFileToneC    <- rsFileToneC    =. opt Nothing "file-tone-c"    stackJSON
    rsFileToneD    <- rsFileToneD    =. opt Nothing "file-tone-d"    stackJSON
    return RSTones{..}

data PartGHL = PartGHL
  { ghlDifficulty    :: Difficulty
  , ghlHopoThreshold :: Int
  } deriving (Eq, Ord, Show)

instance StackJSON PartGHL where
  stackJSON = asStrictObject "PartGHL" $ do
    ghlDifficulty    <- ghlDifficulty    =. fill (Tier 1) "difficulty"     stackJSON
    ghlHopoThreshold <- ghlHopoThreshold =. opt  170      "hopo-threshold" stackJSON
    return PartGHL{..}

data DrumKit
  = HardRockKit
  | ArenaKit
  | VintageKit
  | TrashyKit
  | ElectronicKit
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON DrumKit where
  stackJSON = enumCodecFull "the name of a drum kit or null" $ \case
    HardRockKit   -> is A.Null |?> fuzzy "Hard Rock Kit"
    ArenaKit      -> fuzzy "Arena Kit"
    VintageKit    -> fuzzy "Vintage Kit"
    TrashyKit     -> fuzzy "Trashy Kit"
    ElectronicKit -> fuzzy "Electronic Kit"

data DrumLayout
  = StandardLayout
  | FlipYBToms
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON DrumLayout where
  stackJSON = enumCodecFull "the name of a drum kit layout or null" $ \case
    StandardLayout -> is A.Null |?> is "standard-layout"
    FlipYBToms     -> is "flip-yb-toms"

data FullDrumLayout
  = FDStandard -- snare, hihat, left crash
  | FDOpenHand -- left crash, hihat, snare
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON FullDrumLayout where
  stackJSON = enumCodec "a full drums layout" $ \case
    FDStandard -> "standard"
    FDOpenHand -> "open-hand"

data DrumMode
  = Drums4
  | Drums5
  | DrumsPro
  | DrumsReal
  | DrumsFull
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON DrumMode where
  stackJSON = enumCodec "a drum mode (4, 5, pro, real, full)" $ \case
    Drums4    -> A.Number 4
    Drums5    -> A.Number 5
    DrumsPro  -> "pro"
    DrumsReal -> "real"
    DrumsFull -> "full"

data OrangeFallback = FallbackBlue | FallbackGreen
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON OrangeFallback where
  stackJSON = enumCodec "an orange drum note fallback color (blue, green)" $ \case
    FallbackBlue  -> "blue"
    FallbackGreen -> "green"

data Kicks = Kicks1x | Kicks2x | KicksBoth
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON Kicks where
  stackJSON = enumCodecFull "number of bass pedals (1, 2, both)" $ \case
    Kicks1x   -> is (A.Number 1) |?> is "1" |?> is A.Null
    Kicks2x   -> is (A.Number 2) |?> is "2"
    KicksBoth -> is "both"

data PartDrums f = PartDrums
  { drumsDifficulty  :: Difficulty
  , drumsMode        :: DrumMode
  , drumsKicks       :: Kicks
  , drumsFixFreeform :: Bool
  , drumsKit         :: DrumKit
  , drumsLayout      :: DrumLayout
  , drumsFallback    :: OrangeFallback
  , drumsFileDTXKit  :: Maybe f
  , drumsFullLayout  :: FullDrumLayout
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (PartDrums f) where
  stackJSON = asStrictObject "PartDrums" $ do
    drumsDifficulty  <- drumsDifficulty  =. fill    (Tier 1)       "difficulty"   stackJSON
    drumsMode        <- drumsMode        =. opt     DrumsPro       "mode"         stackJSON
    drumsKicks       <- drumsKicks       =. warning Kicks1x        "kicks"        stackJSON
    drumsFixFreeform <- drumsFixFreeform =. opt     True           "fix-freeform" stackJSON
    drumsKit         <- drumsKit         =. opt     HardRockKit    "kit"          stackJSON
    drumsLayout      <- drumsLayout      =. opt     StandardLayout "layout"       stackJSON
    drumsFallback    <- drumsFallback    =. opt     FallbackGreen  "fallback"     stackJSON
    drumsFileDTXKit  <- drumsFileDTXKit  =. opt     Nothing        "file-dtx-kit" stackJSON
    drumsFullLayout  <- drumsFullLayout  =. opt     FDStandard     "full-layout"  stackJSON
    return PartDrums{..}

data VocalCount = Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON VocalCount where
  stackJSON = enumCodec "a vocal part count (1 to 3)" $ \case
    Vocal1 -> A.Number 1
    Vocal2 -> A.Number 2
    Vocal3 -> A.Number 3

data PartVocal f = PartVocal
  { vocalDifficulty :: Difficulty
  , vocalCount      :: VocalCount
  , vocalGender     :: Maybe Magma.Gender
  , vocalKey        :: Maybe Key
  , vocalLipsyncRB3 :: Maybe (LipsyncRB3 f)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (PartVocal f) where
  stackJSON = asStrictObject "PartVocal" $ do
    vocalDifficulty <- vocalDifficulty =. fill (Tier 1) "difficulty"  stackJSON
    vocalCount      <- vocalCount      =. opt  Vocal1   "count"       stackJSON
    vocalGender     <- vocalGender     =. opt  Nothing  "gender"      (maybeCodec parseGender)
    vocalKey        <- vocalKey        =. opt  Nothing  "key"         (maybeCodec parsePitch)
    vocalLipsyncRB3 <- vocalLipsyncRB3 =. opt  Nothing  "lipsync-rb3" stackJSON
    return PartVocal{..}

data PartAmplitude = PartAmplitude
  { ampInstrument :: Amp.Instrument
  } deriving (Eq, Ord, Show)

instance StackJSON PartAmplitude where
  stackJSON = asStrictObject "PartAmplitude" $ do
    ampInstrument <- ampInstrument =. req "instrument" stackJSON
    return PartAmplitude{..}

instance StackJSON Amp.Instrument where
  stackJSON = enumCodec "amplitude instrument type" $ \case
    Amp.Drums  -> "drums"
    Amp.Bass   -> "bass"
    Amp.Synth  -> "synth"
    Amp.Vocal  -> "vocal"
    Amp.Guitar -> "guitar"

data PartMelody = PartMelody
  deriving (Eq, Ord, Show)

instance StackJSON PartMelody where
  stackJSON = asStrictObject "PartMelody" $ do
    return PartMelody

data PartKonga = PartKonga
  { dkMode1E :: Maybe Int
  , dkMode1H :: Maybe Int
  , dkMode1X :: Maybe Int
  , dkMode2E :: Maybe Int
  , dkMode2H :: Maybe Int
  , dkMode2X :: Maybe Int
  , dkMode4  :: Maybe Int
  , dkModeB  :: Maybe Int
  , dkModeC  :: Maybe Int
  } deriving (Eq, Ord, Show)

instance StackJSON PartKonga where
  stackJSON = asStrictObject "PartKonga" $ do
    dkMode1E <- dkMode1E =. opt Nothing "mode-1E" stackJSON
    dkMode1H <- dkMode1H =. opt Nothing "mode-1H" stackJSON
    dkMode1X <- dkMode1X =. opt Nothing "mode-1X" stackJSON
    dkMode2E <- dkMode2E =. opt Nothing "mode-2E" stackJSON
    dkMode2H <- dkMode2H =. opt Nothing "mode-2H" stackJSON
    dkMode2X <- dkMode2X =. opt Nothing "mode-2X" stackJSON
    dkMode4  <- dkMode4  =. opt Nothing "mode-4"  stackJSON
    dkModeB  <- dkModeB  =. opt Nothing "mode-B"  stackJSON
    dkModeC  <- dkModeC  =. opt Nothing "mode-C"  stackJSON
    return PartKonga{..}

data PartDance = PartDance
  { danceDifficulty :: Difficulty
  } deriving (Eq, Ord, Show)

instance StackJSON PartDance where
  stackJSON = asStrictObject "PartDance" $ do
    danceDifficulty <- danceDifficulty =. fill (Tier 1) "difficulty" stackJSON
    return PartDance{..}

data Part f = Part
  { partGRYBO     :: Maybe PartGRYBO
  , partGHL       :: Maybe PartGHL
  , partProKeys   :: Maybe PartProKeys
  , partProGuitar :: Maybe (PartProGuitar f)
  , partDrums     :: Maybe (PartDrums f)
  , partVocal     :: Maybe (PartVocal f)
  , partAmplitude :: Maybe PartAmplitude
  , partMelody    :: Maybe PartMelody
  , partKonga     :: Maybe PartKonga
  , partDance     :: Maybe PartDance
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (Part f) where
  stackJSON = asStrictObject "Part" $ do
    partGRYBO     <- partGRYBO     =. opt Nothing "grybo"      stackJSON
    partGHL       <- partGHL       =. opt Nothing "ghl"        stackJSON
    partProKeys   <- partProKeys   =. opt Nothing "pro-keys"   stackJSON
    partProGuitar <- partProGuitar =. opt Nothing "pro-guitar" stackJSON
    partDrums     <- partDrums     =. opt Nothing "drums"      stackJSON
    partVocal     <- partVocal     =. opt Nothing "vocal"      stackJSON
    partAmplitude <- partAmplitude =. opt Nothing "amplitude"  stackJSON
    partMelody    <- partMelody    =. opt Nothing "melody"     stackJSON
    partKonga     <- partKonga     =. opt Nothing "konga"      stackJSON
    partDance     <- partDance     =. opt Nothing "dance"      stackJSON
    return Part{..}

instance Default (Part f) where
  def = Part Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

instance StackJSON Magma.AutogenTheme where
  stackJSON = enumCodecFull "the name of an autogen theme or null" $ \case
    Magma.DefaultTheme -> is A.Null |?> fuzzy "Default"
    theme              -> fuzzy $ T.pack $ show theme

data Rating
  = FamilyFriendly
  | SupervisionRecommended
  | Mature
  | Unrated
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON Rating where
  stackJSON = enumCodecFull "a valid content rating or null" $ \case
    FamilyFriendly         -> fuzzy "Family Friendly"         |?> fuzzy "FF"
    SupervisionRecommended -> fuzzy "Supervision Recommended" |?> fuzzy "SR"
    Mature                 -> fuzzy "Mature"                  |?> fuzzy "M"
    Unrated                -> fuzzy "Unrated"                 |?> fuzzy "UR"

data PreviewTime
  = PreviewSection T.Text
  | PreviewMIDI    U.MeasureBeats
  | PreviewSeconds U.Seconds
  deriving (Eq, Ord, Show, Generic)

instance Hashable PreviewTime where
  hashWithSalt s = hashWithSalt s . \case
    PreviewSection x -> '1' : show x
    PreviewMIDI    x -> '2' : show x
    PreviewSeconds x -> '3' : show x

instance StackJSON PreviewTime where
  stackJSON = Codec
    { codecIn = let
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
    , codecOut = makeOut $ \case
      PreviewSection str  -> A.toJSON $ "prc_" <> str
      PreviewMIDI mb      -> A.toJSON $ showMeasureBeats mb
      PreviewSeconds secs -> showTimestamp $ realToFrac secs
    }

-- | Extra information with no gameplay effect.
data Metadata f = Metadata
  { _title        :: Maybe T.Text
  , _titleJP      :: Maybe T.Text
  , _artist       :: Maybe T.Text
  , _artistJP     :: Maybe T.Text
  , _album        :: Maybe T.Text
  , _genre        :: Maybe T.Text
  , _subgenre     :: Maybe T.Text
  , _year         :: Maybe Int
  , _fileAlbumArt :: Maybe f
  , _trackNumber  :: Maybe Int
  , _comments     :: [T.Text]
  , _key          :: Maybe SongKey
  , _author       :: Maybe T.Text
  , _rating       :: Rating
  , _previewStart :: Maybe PreviewTime
  , _previewEnd   :: Maybe PreviewTime
  , _languages    :: [T.Text]
  , _convert      :: Bool
  , _rhythmKeys   :: Bool -- should be in target!
  , _rhythmBass   :: Bool -- should be in target!
  , _catEMH       :: Bool
  , _expertOnly   :: Bool
  , _cover        :: Bool
  , _difficulty   :: Difficulty
  } deriving (Eq, Show, Functor, Foldable, Traversable)

parseAnimTempo :: (SendMessage m) => ValueCodec m A.Value (Either AnimTempo Integer)
parseAnimTempo = eitherCodec
  (enumCodecFull "an animation speed" $ \case
    KTempoSlow   -> is "slow" |?> is A.Null
    KTempoMedium -> is "medium"
    KTempoFast   -> is "fast"
  )
  stackJSON

instance (Eq f, StackJSON f) => StackJSON (Metadata f) where
  stackJSON = asStrictObject "Metadata" $ do
    let stripped = fmap (fmap T.strip) stackJSON
    _title        <- _title        =. warning Nothing        "title"          stripped
    _titleJP      <- _titleJP      =. opt     Nothing        "title-jp"       stripped
    _artist       <- _artist       =. warning Nothing        "artist"         stripped
    _artistJP     <- _artistJP     =. opt     Nothing        "artist-jp"      stripped
    _album        <- _album        =. opt     Nothing        "album"          stripped
    _genre        <- _genre        =. warning Nothing        "genre"          stripped
    _subgenre     <- _subgenre     =. opt     Nothing        "subgenre"       stripped
    _year         <- _year         =. warning Nothing        "year"           stackJSON
    _fileAlbumArt <- _fileAlbumArt =. opt     Nothing        "file-album-art" stackJSON
    _trackNumber  <- _trackNumber  =. opt     Nothing        "track-number"   stackJSON
    _comments     <- _comments     =. opt     []             "comments"       stackJSON
    _key          <- _key          =. opt     Nothing        "key"            (maybeCodec parseSongKey)
    _author       <- _author       =. warning Nothing        "author"         stripped
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

instance (Eq f, StackJSON f) => Default (Metadata f) where
  def = fromEmptyObject

data Global f = Global
  { _fileMidi            :: f
  , _fileSongAnim        :: Maybe f -- ^ venue format found in RB3 milos
  , _autogenTheme        :: Maybe Magma.AutogenTheme -- ^ 'Nothing' means black venue
  , _animTempo           :: Either AnimTempo Integer
  , _backgroundVideo     :: Maybe (VideoInfo f)
  , _fileBackgroundImage :: Maybe f
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, IsString f, StackJSON f) => StackJSON (Global f) where
  stackJSON = asStrictObject "Global" $ do
    _fileMidi            <- _fileMidi            =. opt "notes.mid"         "file-midi"             stackJSON
    _fileSongAnim        <- _fileSongAnim        =. opt Nothing             "file-song-anim"        stackJSON
    _autogenTheme        <- _autogenTheme        =. opt (Just Magma.DefaultTheme) "autogen-theme"         stackJSON
    _animTempo           <- _animTempo           =. opt (Left KTempoMedium) "anim-tempo"            parseAnimTempo
    _backgroundVideo     <- _backgroundVideo     =. opt Nothing             "background-video"      stackJSON
    _fileBackgroundImage <- _fileBackgroundImage =. opt Nothing             "file-background-image" stackJSON
    return Global{..}

instance (Eq f, IsString f, StackJSON f) => Default (Global f) where
  def = fromEmptyObject

data VideoInfo f = VideoInfo
  { _fileVideo      :: f
  , _videoStartTime :: Maybe Milli -- seconds, can be negative
  , _videoEndTime   :: Maybe Milli
  , _videoLoop      :: Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON f) => StackJSON (VideoInfo f) where
  stackJSON = asStrictObject "VideoInfo" $ do
    _fileVideo      <- _fileVideo      =. req         "file-video" stackJSON
    _videoStartTime <- _videoStartTime =. opt Nothing "start-time" stackJSON
    _videoEndTime   <- _videoEndTime   =. opt Nothing "end-time"   stackJSON
    _videoLoop      <- _videoLoop      =. opt False   "loop"       stackJSON
    return VideoInfo{..}

getTitle, getArtist, getAlbum, getAuthor :: Metadata f -> T.Text
getTitle  m = case _title  m of Just x | not $ T.null x -> x; _ -> "Untitled"
getArtist m = case _artist m of Just x | not $ T.null x -> x; _ -> "Unknown Artist"
getAlbum  m = case _album  m of Just x | not $ T.null x -> x; _ -> "Unknown Album"
getAuthor m = case _author m of Just x | not $ T.null x -> x; _ -> "Unknown Author"

getYear, getTrackNumber :: Metadata f -> Int
getYear        = fromMaybe 1960 . _year
getTrackNumber = fromMaybe 1    . _trackNumber

data TargetCommon = TargetCommon
  { tgt_Speed   :: Maybe Double
  , tgt_Plan    :: Maybe T.Text
  , tgt_Title   :: Maybe T.Text -- override base song title
  , tgt_Label   :: Maybe T.Text -- suffix after title
  , tgt_Label2x :: Bool -- if automatic label and 2x drums, should we add (2x Bass Pedal)
  , tgt_Start   :: Maybe SegmentEdge
  , tgt_End     :: Maybe SegmentEdge
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetCommon :: (SendMessage m) => ObjectCodec m A.Value TargetCommon
parseTargetCommon = do
  tgt_Speed   <- tgt_Speed   =. opt Nothing "speed"    stackJSON
  tgt_Plan    <- tgt_Plan    =. opt Nothing "plan"     stackJSON
  tgt_Title   <- tgt_Title   =. opt Nothing "title"    stackJSON
  tgt_Label   <- tgt_Label   =. opt Nothing "label"    stackJSON
  tgt_Label2x <- tgt_Label2x =. opt True    "label-2x" stackJSON
  tgt_Start   <- tgt_Start   =. opt Nothing "start"    stackJSON
  tgt_End     <- tgt_End     =. opt Nothing "end"      stackJSON
  return TargetCommon{..}

instance Default TargetCommon where
  def = TargetCommon Nothing Nothing Nothing Nothing True Nothing Nothing

data SegmentEdge = SegmentEdge
  { seg_FadeStart :: PreviewTime
  , seg_FadeEnd   :: PreviewTime
  , seg_Notes     :: PreviewTime
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseSegmentEdge :: (SendMessage m) => ObjectCodec m A.Value SegmentEdge
parseSegmentEdge = do
  seg_FadeStart <- seg_FadeStart =. req "fade-start" stackJSON
  seg_FadeEnd   <- seg_FadeEnd   =. req "fade-end"   stackJSON
  seg_Notes     <- seg_Notes     =. req "notes"      stackJSON
  return SegmentEdge{..}

instance StackJSON SegmentEdge where
  stackJSON = asStrictObject "SegmentEdge" parseSegmentEdge

data RBSongID
  = SongIDAutoSymbol
  | SongIDAutoInt
  | SongIDSymbol T.Text
  | SongIDInt Int32
  deriving (Eq, Ord, Show, Generic, Hashable)

instance StackJSON RBSongID where
  stackJSON = Codec
    { codecIn = lift ask >>= \case
      A.Null                 -> return SongIDAutoSymbol
      OneKey "auto" "symbol" -> return SongIDAutoSymbol
      OneKey "auto" "int"    -> return SongIDAutoInt
      A.String s             -> return $ SongIDSymbol s
      A.Number n             -> return $ SongIDInt $ round n
      _                      -> expected "a RB song ID, or {auto: symbol/int}"
    , codecOut = makeOut $ \case
      SongIDAutoSymbol -> OneKey "auto" "symbol"
      SongIDAutoInt    -> OneKey "auto" "int"
      SongIDSymbol s   -> A.String s
      SongIDInt n      -> A.Number $ fromIntegral n
    }

data TargetRB3 = TargetRB3
  { rb3_Common      :: TargetCommon
  , rb3_2xBassPedal :: Bool
  , rb3_SongID      :: RBSongID
  , rb3_Version     :: Maybe Integer
  , rb3_Harmonix    :: Bool
  , rb3_Magma       :: MagmaSetting
  , rb3_Guitar      :: FlexPartName
  , rb3_Bass        :: FlexPartName
  , rb3_Drums       :: FlexPartName
  , rb3_Keys        :: FlexPartName
  , rb3_Vocal       :: FlexPartName
  , rb3_PS3Encrypt  :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetRB3 :: (SendMessage m) => ObjectCodec m A.Value TargetRB3
parseTargetRB3 = do
  rb3_Common      <- rb3_Common      =. parseTargetCommon
  rb3_2xBassPedal <- rb3_2xBassPedal =. opt False        "2x-bass-pedal" stackJSON
  rb3_SongID      <- rb3_SongID      =. fill SongIDAutoSymbol "song-id"  stackJSON
  rb3_Version     <- rb3_Version     =. opt Nothing      "version"       stackJSON
  rb3_Harmonix    <- rb3_Harmonix    =. opt False        "harmonix"      stackJSON
  rb3_Magma       <- rb3_Magma       =. opt MagmaRequire "magma"         stackJSON
  rb3_Guitar      <- rb3_Guitar      =. opt FlexGuitar   "guitar"        stackJSON
  rb3_Bass        <- rb3_Bass        =. opt FlexBass     "bass"          stackJSON
  rb3_Drums       <- rb3_Drums       =. opt FlexDrums    "drums"         stackJSON
  rb3_Keys        <- rb3_Keys        =. opt FlexKeys     "keys"          stackJSON
  rb3_Vocal       <- rb3_Vocal       =. opt FlexVocal    "vocal"         stackJSON
  rb3_PS3Encrypt  <- rb3_PS3Encrypt  =. opt True         "ps3-encrypt"   stackJSON
  return TargetRB3{..}

instance StackJSON TargetRB3 where
  stackJSON = asStrictObject "TargetRB3" parseTargetRB3

instance Default TargetRB3 where
  def = fromEmptyObject

data LipsyncMember = LipsyncGuitar | LipsyncBass | LipsyncDrums
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON LipsyncMember where
  stackJSON = enumCodecFull "guitar, bass, or drums" $ \case
    LipsyncGuitar -> is "guitar"
    LipsyncBass   -> is "bass"
    LipsyncDrums  -> is "drums"

data LipsyncRB3 f = LipsyncRB3
  { lipsyncSources :: [LipsyncSource f]
  , lipsyncMember2 :: LipsyncMember
  , lipsyncMember3 :: LipsyncMember
  , lipsyncMember4 :: LipsyncMember
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (LipsyncRB3 f) where
  stackJSON = asStrictObject "LipsyncRB3" $ do
    lipsyncSources <- lipsyncSources =. req "sources" stackJSON
    lipsyncMember2 <- lipsyncMember2 =. opt LipsyncGuitar "member-2" stackJSON
    lipsyncMember3 <- lipsyncMember3 =. opt LipsyncBass   "member-3" stackJSON
    lipsyncMember4 <- lipsyncMember4 =. opt LipsyncDrums  "member-4" stackJSON
    return LipsyncRB3{..}

data LipsyncSource f
  = LipsyncTrack1
  | LipsyncTrack2
  | LipsyncTrack3
  | LipsyncTrack4
  | LipsyncVocal (Maybe VocalCount)
  | LipsyncFile f
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (LipsyncSource f) where
  stackJSON = Codec
    { codecIn = lift ask >>= \case
      "track1"                -> return LipsyncTrack1
      "track2"                -> return LipsyncTrack2
      "track3"                -> return LipsyncTrack3
      "track4"                -> return LipsyncTrack4
      "solo"                  -> return $ LipsyncVocal Nothing
      "harm1"                 -> return $ LipsyncVocal $ Just Vocal1
      "harm2"                 -> return $ LipsyncVocal $ Just Vocal2
      "harm3"                 -> return $ LipsyncVocal $ Just Vocal3
      OneKey "file-lipsync" x -> LipsyncFile <$> parseFrom x (codecIn stackJSON)
      _                       -> expected "a lipsync source"
    , codecOut = makeOut $ \case
      LipsyncTrack1 -> "track1"
      LipsyncTrack2 -> "track2"
      LipsyncTrack3 -> "track3"
      LipsyncTrack4 -> "track4"
      LipsyncVocal Nothing -> "solo"
      LipsyncVocal (Just Vocal1) -> "harm1"
      LipsyncVocal (Just Vocal2) -> "harm2"
      LipsyncVocal (Just Vocal3) -> "harm3"
      LipsyncFile f -> OneKey "file-lipsync" $ makeValue stackJSON f
    }

data TargetRB2 = TargetRB2
  { rb2_Common      :: TargetCommon
  , rb2_2xBassPedal :: Bool
  , rb2_SongID      :: RBSongID
  , rb2_LabelRB2    :: Bool
  , rb2_Version     :: Maybe Integer
  , rb2_Magma       :: MagmaSetting -- this currently only affects Magma v2; v1 is always tried but optional
  , rb2_Guitar      :: FlexPartName
  , rb2_Bass        :: FlexPartName
  , rb2_Drums       :: FlexPartName
  , rb2_Vocal       :: FlexPartName
  , rb2_PS3Encrypt  :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetRB2 :: (SendMessage m) => ObjectCodec m A.Value TargetRB2
parseTargetRB2 = do
  rb2_Common      <- rb2_Common      =. parseTargetCommon
  rb2_2xBassPedal <- rb2_2xBassPedal =. opt False        "2x-bass-pedal" stackJSON
  rb2_SongID      <- rb2_SongID      =. fill SongIDAutoSymbol "song-id"  stackJSON
  rb2_LabelRB2    <- rb2_LabelRB2    =. opt False        "label-rb2"     stackJSON
  rb2_Version     <- rb2_Version     =. opt Nothing      "version"       stackJSON
  rb2_Magma       <- rb2_Magma       =. opt MagmaRequire "magma"         stackJSON
  rb2_Guitar      <- rb2_Guitar      =. opt FlexGuitar   "guitar"        stackJSON
  rb2_Bass        <- rb2_Bass        =. opt FlexBass     "bass"          stackJSON
  rb2_Drums       <- rb2_Drums       =. opt FlexDrums    "drums"         stackJSON
  rb2_Vocal       <- rb2_Vocal       =. opt FlexVocal    "vocal"         stackJSON
  rb2_PS3Encrypt  <- rb2_PS3Encrypt  =. opt True         "ps3-encrypt"   stackJSON
  return TargetRB2{..}

instance StackJSON TargetRB2 where
  stackJSON = asStrictObject "TargetRB2" parseTargetRB2

instance Default TargetRB2 where
  def = fromEmptyObject

data TargetPS = TargetPS
  { ps_Common        :: TargetCommon
  , ps_Guitar        :: FlexPartName
  , ps_Bass          :: FlexPartName
  , ps_Drums         :: FlexPartName
  , ps_Keys          :: FlexPartName
  , ps_Vocal         :: FlexPartName
  , ps_Rhythm        :: FlexPartName
  , ps_GuitarCoop    :: FlexPartName
  , ps_Dance         :: FlexPartName
  , ps_LoadingPhrase :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPS :: (SendMessage m) => ObjectCodec m A.Value TargetPS
parseTargetPS = do
  ps_Common        <- ps_Common        =. parseTargetCommon
  ps_Guitar        <- ps_Guitar        =. opt FlexGuitar                "guitar"         stackJSON
  ps_Bass          <- ps_Bass          =. opt FlexBass                  "bass"           stackJSON
  ps_Drums         <- ps_Drums         =. opt FlexDrums                 "drums"          stackJSON
  ps_Keys          <- ps_Keys          =. opt FlexKeys                  "keys"           stackJSON
  ps_Vocal         <- ps_Vocal         =. opt FlexVocal                 "vocal"          stackJSON
  ps_Rhythm        <- ps_Rhythm        =. opt (FlexExtra "rhythm"     ) "rhythm"         stackJSON
  ps_GuitarCoop    <- ps_GuitarCoop    =. opt (FlexExtra "guitar-coop") "guitar-coop"    stackJSON
  ps_Dance         <- ps_Dance         =. opt (FlexExtra "global"     ) "dance"          stackJSON
  ps_LoadingPhrase <- ps_LoadingPhrase =. opt Nothing                   "loading-phrase" stackJSON
  return TargetPS{..}

instance StackJSON TargetPS where
  stackJSON = asStrictObject "TargetPS" parseTargetPS

instance Default TargetPS where
  def = fromEmptyObject

data TargetGH1 = TargetGH1
  { gh1_Common        :: TargetCommon
  , gh1_Guitar        :: FlexPartName
  , gh1_Bass          :: FlexPartName
  , gh1_Drums         :: FlexPartName
  , gh1_Vocal         :: FlexPartName
  , gh1_Keys          :: FlexPartName
  , gh1_Key           :: Maybe T.Text -- top symbol
  , gh1_LoadingPhrase :: Maybe T.Text -- these go in ghui/eng/gen/locale.dtb, loading_tip_thesongkey
  , gh1_Offset        :: Double -- in seconds, positive means pull audio earlier, negative means push later
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH1 :: (SendMessage m) => ObjectCodec m A.Value TargetGH1
parseTargetGH1 = do
  gh1_Common        <- gh1_Common        =. parseTargetCommon
  gh1_Guitar        <- gh1_Guitar        =. opt FlexGuitar "guitar"         stackJSON
  gh1_Bass          <- gh1_Bass          =. opt FlexBass   "bass"           stackJSON
  gh1_Drums         <- gh1_Drums         =. opt FlexDrums  "drums"          stackJSON
  gh1_Keys          <- gh1_Keys          =. opt FlexKeys   "keys"           stackJSON
  gh1_Vocal         <- gh1_Vocal         =. opt FlexVocal  "vocal"          stackJSON
  gh1_Key           <- gh1_Key           =. opt Nothing    "key"            stackJSON
  gh1_LoadingPhrase <- gh1_LoadingPhrase =. opt Nothing    "loading-phrase" stackJSON
  gh1_Offset        <- gh1_Offset        =. opt 0          "offset"         stackJSON
  return TargetGH1{..}

instance StackJSON TargetGH1 where
  stackJSON = asStrictObject "TargetGH1" parseTargetGH1

instance Default TargetGH1 where
  def = fromEmptyObject

data GH2Coop = GH2Bass | GH2Rhythm
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

instance StackJSON GH2Coop where
  stackJSON = enumCodecFull "bass or rhythm" $ \case
    GH2Bass   -> is "bass" |?> is A.Null
    GH2Rhythm -> is "rhythm"

data TargetGH2 = TargetGH2
  { gh2_Common        :: TargetCommon
  , gh2_Guitar        :: FlexPartName
  , gh2_Bass          :: FlexPartName
  , gh2_Rhythm        :: FlexPartName
  , gh2_Drums         :: FlexPartName
  , gh2_Vocal         :: FlexPartName
  , gh2_Keys          :: FlexPartName
  , gh2_Coop          :: GH2Coop
  , gh2_Key           :: Maybe T.Text -- top symbol for 360 DLC
  , gh2_Context       :: Maybe Int -- contexts.dta for 360 DLC
  , gh2_Leaderboard   :: Maybe (Int, Int) -- leaderboards.dta for 360 DLC
  , gh2_PracticeAudio :: Bool -- should we make slow audio for PS2
  , gh2_LoadingPhrase :: Maybe T.Text
  , gh2_Offset        :: Double -- in seconds, positive means pull audio earlier, negative means push later
  , gh2_DrumChart     :: Bool
  , gh2_2xBassPedal   :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH2 :: (SendMessage m) => ObjectCodec m A.Value TargetGH2
parseTargetGH2 = do
  gh2_Common        <- gh2_Common        =. parseTargetCommon
  gh2_Guitar        <- gh2_Guitar        =. opt FlexGuitar           "guitar"         stackJSON
  gh2_Bass          <- gh2_Bass          =. opt FlexBass             "bass"           stackJSON
  gh2_Rhythm        <- gh2_Rhythm        =. opt (FlexExtra "rhythm") "rhythm"         stackJSON
  gh2_Drums         <- gh2_Drums         =. opt FlexDrums            "drums"          stackJSON
  gh2_Keys          <- gh2_Keys          =. opt FlexKeys             "keys"           stackJSON
  gh2_Vocal         <- gh2_Vocal         =. opt FlexVocal            "vocal"          stackJSON
  gh2_Coop          <- gh2_Coop          =. opt GH2Bass              "coop"           stackJSON
  gh2_Key           <- gh2_Key           =. opt Nothing              "key"            stackJSON
  gh2_Context       <- gh2_Context       =. opt Nothing              "context"        stackJSON
  gh2_Leaderboard   <- gh2_Leaderboard   =. opt Nothing              "leaderboard"    stackJSON
  gh2_PracticeAudio <- gh2_PracticeAudio =. opt True                 "practice-audio" stackJSON
  gh2_LoadingPhrase <- gh2_LoadingPhrase =. opt Nothing              "loading-phrase" stackJSON
  gh2_Offset        <- gh2_Offset        =. opt 0                    "offset"         stackJSON
  gh2_DrumChart     <- gh2_DrumChart     =. opt False                "drum-chart"     stackJSON
  gh2_2xBassPedal   <- gh2_2xBassPedal   =. opt False                "2x-bass-pedal"  stackJSON
  return TargetGH2{..}

instance StackJSON TargetGH2 where
  stackJSON = asStrictObject "TargetGH2" parseTargetGH2

instance Default TargetGH2 where
  def = fromEmptyObject

-- gh5 or ghwor, mostly same formats
data TargetGH5 = TargetGH5
  { gh5_Common :: TargetCommon
  , gh5_Guitar :: FlexPartName
  , gh5_Bass   :: FlexPartName
  , gh5_Drums  :: FlexPartName
  , gh5_Vocal  :: FlexPartName
  , gh5_SongID :: Maybe Int -- like 783 in "adlc783_1.fsb.xen"
  , gh5_CDL    :: Maybe Int -- like 511 in "cdl511"
  , gh5_ProTo4 :: Bool -- true if pro drums should just use RYBG
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH5 :: (SendMessage m) => ObjectCodec m A.Value TargetGH5
parseTargetGH5 = do
  gh5_Common        <- gh5_Common        =. parseTargetCommon
  gh5_Guitar        <- gh5_Guitar        =. opt FlexGuitar           "guitar"         stackJSON
  gh5_Bass          <- gh5_Bass          =. opt FlexBass             "bass"           stackJSON
  gh5_Drums         <- gh5_Drums         =. opt FlexDrums            "drums"          stackJSON
  gh5_Vocal         <- gh5_Vocal         =. opt FlexVocal            "vocal"          stackJSON
  gh5_SongID        <- gh5_SongID        =. opt Nothing              "song-id"        stackJSON
  gh5_CDL           <- gh5_CDL           =. opt Nothing              "cdl"            stackJSON
  gh5_ProTo4        <- gh5_ProTo4        =. opt False                "pro-to-4"       stackJSON
  return TargetGH5{..}

instance StackJSON TargetGH5 where
  stackJSON = asStrictObject "TargetGH5" parseTargetGH5

instance Default TargetGH5 where
  def = fromEmptyObject

data TargetGH3 = TargetGH3
  { gh3_Common :: TargetCommon
  , gh3_Guitar :: FlexPartName
  , gh3_Bass   :: FlexPartName
  , gh3_Rhythm :: FlexPartName
  , gh3_Coop   :: GH2Coop
  , gh3_Drums  :: FlexPartName
  , gh3_Vocal  :: FlexPartName
  , gh3_Keys   :: FlexPartName
  , gh3_SongID :: Maybe (Either Int T.Text) -- Int gets title/artist added, Text is exact
  , gh3_DL     :: Maybe Int -- like 15 in "dl15"
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH3 :: (SendMessage m) => ObjectCodec m A.Value TargetGH3
parseTargetGH3 = do
  gh3_Common        <- gh3_Common        =. parseTargetCommon
  gh3_Guitar        <- gh3_Guitar        =. opt FlexGuitar           "guitar"         stackJSON
  gh3_Bass          <- gh3_Bass          =. opt FlexBass             "bass"           stackJSON
  gh3_Rhythm        <- gh3_Rhythm        =. opt (FlexExtra "rhythm") "rhythm"         stackJSON
  gh3_Coop          <- gh3_Coop          =. opt GH2Bass              "coop"           stackJSON
  gh3_Drums         <- gh3_Drums         =. opt FlexDrums            "drums"          stackJSON
  gh3_Vocal         <- gh3_Vocal         =. opt FlexVocal            "vocal"          stackJSON
  gh3_Keys          <- gh3_Keys          =. opt FlexVocal            "keys"           stackJSON
  gh3_SongID        <- gh3_SongID        =. opt Nothing              "song-id"        stackJSON
  gh3_DL            <- gh3_DL            =. opt Nothing              "dl"             stackJSON
  return TargetGH3{..}

instance StackJSON TargetGH3 where
  stackJSON = asStrictObject "TargetGH3" parseTargetGH3

instance Default TargetGH3 where
  def = fromEmptyObject

data TargetDTX = TargetDTX
  { dtx_Common :: TargetCommon
  , dtx_Drums  :: FlexPartName
  , dtx_Guitar :: FlexPartName
  , dtx_Bass   :: FlexPartName
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetDTX :: (SendMessage m) => ObjectCodec m A.Value TargetDTX
parseTargetDTX = do
  dtx_Common  <- dtx_Common  =. parseTargetCommon
  dtx_Guitar  <- dtx_Guitar  =. opt FlexGuitar    "guitar"       stackJSON
  dtx_Bass    <- dtx_Bass    =. opt FlexBass      "bass"         stackJSON
  dtx_Drums   <- dtx_Drums   =. opt FlexDrums     "drums"        stackJSON
  return TargetDTX{..}

instance StackJSON TargetDTX where
  stackJSON = asStrictObject "TargetDTX" parseTargetDTX

instance Default TargetDTX where
  def = fromEmptyObject

data RSArrModifier
  = RSDefault
  | RSBonus
  | RSAlternate
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

data RSArrType
  = RSLead
  | RSRhythm
  | RSComboLead -- ^ combo, set to be on the lead path
  | RSComboRhythm -- ^ combo, set to be on the rhythm path
  | RSBass
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Hashable)

data RSArrSlot = RSArrSlot RSArrModifier RSArrType
  deriving (Eq, Ord, Show, Generic, Hashable)
  deriving (Enum, Bounded) via GenericFullEnum RSArrSlot

data TargetRS = TargetRS
  { rs_Common       :: TargetCommon
  , rs_Arrangements :: [(RSArrSlot, FlexPartName)]
  , rs_Vocal        :: FlexPartName
  , rs_SongKey      :: Maybe T.Text
  , rs_Version      :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

rsArrSlot :: RSArrSlot -> T.Text
rsArrSlot (RSArrSlot arrmod arrtype) = let
  base = case arrtype of
    RSLead        -> "lead"
    RSRhythm      -> "rhythm"
    RSComboLead   -> "combo-lead"
    RSComboRhythm -> "combo-rhythm"
    RSBass        -> "bass"
  in case arrmod of
    RSDefault   -> base
    RSBonus     -> "bonus-" <> base
    RSAlternate -> "alt-" <> base

parseRSArr :: (SendMessage m) => ValueCodec m A.Value (RSArrSlot, FlexPartName)
parseRSArr = let
  slotCodec = enumCodec
    "arrangement slot like lead, bonus-rhythm, alt-bass, etc."
    (A.String . rsArrSlot)
  partCodec = stackJSON
  in Codec
    { codecIn = lift ask >>= \case
      A.Array (V.toList -> [x, y]) -> liftA2 (,)
        (inside "RS arrangement slot" $ parseFrom x $ codecIn slotCodec)
        (inside "RS arrangement part" $ parseFrom y $ codecIn partCodec)
      _ -> expected "2-element array for RS arrangement"
    , codecOut = makeOut $ \case
      (slot, fpart) -> A.toJSON [makeValue' slotCodec slot, makeValue' partCodec fpart]
    }

parseTargetRS :: (SendMessage m) => ObjectCodec m A.Value TargetRS
parseTargetRS = do
  rs_Common       <- rs_Common       =. parseTargetCommon
  rs_Arrangements <- rs_Arrangements =. opt [] "arrangements" (listCodec parseRSArr)
  rs_Vocal        <- rs_Vocal        =. opt FlexVocal "vocal" stackJSON
  rs_SongKey      <- rs_SongKey      =. opt Nothing "song-key" stackJSON
  rs_Version      <- rs_Version      =. opt Nothing "version" stackJSON
  return TargetRS{..}

instance StackJSON TargetRS where
  stackJSON = asStrictObject "TargetRS" parseTargetRS

instance Default TargetRS where
  def = fromEmptyObject

data TargetPG = TargetPG
  { pg_Common      :: TargetCommon
  , pg_2xBassPedal :: Bool
  , pg_Guitar      :: FlexPartName
  , pg_Drums       :: FlexPartName
  , pg_Vocal       :: FlexPartName
  , pg_Key         :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPG :: (SendMessage m) => ObjectCodec m A.Value TargetPG
parseTargetPG = do
  pg_Common      <- pg_Common      =. parseTargetCommon
  pg_2xBassPedal <- pg_2xBassPedal =. opt False        "2x-bass-pedal" stackJSON
  pg_Guitar      <- pg_Guitar      =. opt FlexGuitar   "guitar"        stackJSON
  pg_Drums       <- pg_Drums       =. opt FlexDrums    "drums"         stackJSON
  pg_Vocal       <- pg_Vocal       =. opt FlexVocal    "vocal"         stackJSON
  pg_Key         <- pg_Key         =. opt Nothing      "key"           stackJSON
  return TargetPG{..}

instance StackJSON TargetPG where
  stackJSON = asStrictObject "TargetPG" parseTargetPG

instance Default TargetPG where
  def = fromEmptyObject

data TargetPart = TargetPart
  { tgt_Common :: TargetCommon
  , tgt_Part   :: FlexPartName
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPart :: (SendMessage m) => ObjectCodec m A.Value TargetPart
parseTargetPart = do
  tgt_Common <- tgt_Common =. parseTargetCommon
  tgt_Part   <- tgt_Part   =. opt (FlexExtra "global") "part" stackJSON
  return TargetPart{..}

instance StackJSON TargetPart where
  stackJSON = asStrictObject "TargetPart" parseTargetPart

instance Default TargetPart where
  def = fromEmptyObject

data Target
  = RB3    TargetRB3
  | RB2    TargetRB2
  | PS     TargetPS
  | GH1    TargetGH1
  | GH2    TargetGH2
  | GH3    TargetGH3
  | GH5    TargetGH5
  | RS     TargetRS
  | DTX    TargetDTX
  | PG     TargetPG
  | Melody TargetPart
  | Konga  TargetPart
  deriving (Eq, Ord, Show, Generic, Hashable)

targetCommon :: Target -> TargetCommon
targetCommon = \case
  RB3    TargetRB3 {..} -> rb3_Common
  RB2    TargetRB2 {..} -> rb2_Common
  PS     TargetPS  {..} -> ps_Common
  GH1    TargetGH1 {..} -> gh1_Common
  GH2    TargetGH2 {..} -> gh2_Common
  GH3    TargetGH3 {..} -> gh3_Common
  GH5    TargetGH5 {..} -> gh5_Common
  RS     TargetRS  {..} -> rs_Common
  DTX    TargetDTX {..} -> dtx_Common
  PG     TargetPG  {..} -> pg_Common
  Melody TargetPart{..} -> tgt_Common
  Konga  TargetPart{..} -> tgt_Common

addKey :: (forall m. (SendMessage m) => ObjectCodec m A.Value a) -> T.Text -> A.Value -> a -> A.Value
addKey codec k v x = A.Object $ KM.fromHashMapText $ HM.insert k v $ HM.fromList $ makeObject (objectId codec) x

instance StackJSON Target where
  stackJSON = Codec
    { codecIn = object $ do
      target <- requiredKey "game" fromJSON
      hm <- lift ask
      parseFrom (A.Object $ KM.fromHashMapText $ HM.delete "game" hm) $ case target :: T.Text of
        "rb3"    -> fmap RB3    fromJSON
        "rb2"    -> fmap RB2    fromJSON
        "ps"     -> fmap PS     fromJSON
        "gh2"    -> fmap GH2    fromJSON
        "gh3"    -> fmap GH3    fromJSON
        "gh5"    -> fmap GH5    fromJSON
        "rs"     -> fmap RS     fromJSON
        "dtx"    -> fmap DTX    fromJSON
        "pg"     -> fmap PG     fromJSON
        "melody" -> fmap Melody fromJSON
        "konga"  -> fmap Konga  fromJSON
        _        -> fatal $ "Unrecognized target game: " ++ show target
    , codecOut = makeOut $ \case
      RB3    rb3 -> addKey parseTargetRB3  "game" "rb3"    rb3
      RB2    rb2 -> addKey parseTargetRB2  "game" "rb2"    rb2
      PS     ps  -> addKey parseTargetPS   "game" "ps"     ps
      GH1    gh1 -> addKey parseTargetGH1  "game" "gh1"    gh1
      GH2    gh2 -> addKey parseTargetGH2  "game" "gh2"    gh2
      GH3    gh3 -> addKey parseTargetGH3  "game" "gh3"    gh3
      GH5    gh5 -> addKey parseTargetGH5  "game" "gh5"    gh5
      RS     rs  -> addKey parseTargetRS   "game" "rs"     rs
      DTX    dtx -> addKey parseTargetDTX  "game" "dtx"    dtx
      PG     pg  -> addKey parseTargetPG   "game" "pg"     pg
      Melody tgt -> addKey parseTargetPart "game" "melody" tgt
      Konga  tgt -> addKey parseTargetPart "game" "konga"  tgt
    }

data SongYaml f = SongYaml
  { _metadata :: Metadata f
  , _global   :: Global f
  , _audio    :: HM.HashMap T.Text (AudioFile f)
  , _jammit   :: HM.HashMap T.Text JammitTrack
  , _plans    :: HM.HashMap T.Text (Plan f)
  , _targets  :: HM.HashMap T.Text Target
  , _parts    :: Parts (Part f)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, IsString f, StackJSON f) => StackJSON (SongYaml f) where
  stackJSON = asStrictObject "SongYaml" $ do
    _metadata <- _metadata =. opt def       "metadata" stackJSON
    _global   <- _global   =. opt def       "global"   stackJSON
    _audio    <- _audio    =. opt HM.empty "audio"    (dict stackJSON)
    _jammit   <- _jammit   =. opt HM.empty "jammit"   (dict stackJSON)
    _plans    <- _plans    =. opt HM.empty "plans"    (dict stackJSON)
    _targets  <- _targets  =. opt HM.empty "targets"  (dict stackJSON)
    _parts    <- _parts    =. req           "parts"    stackJSON
    return SongYaml{..}

getPart :: FlexPartName -> SongYaml f -> Maybe (Part f)
getPart fpart = HM.lookup fpart . getParts . _parts

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds
  :: (RBFile.HasEvents f)
  => SongYaml file
  -> RBFile.Song (f U.Beats) -- Midi used to evaluate midi timestamps and section boundaries
  -> U.Seconds -- Padding added to the project midi to produce the game midi
  -> Bool -- Is the given midi file already padded?
  -> (Int, Int)
previewBounds syaml song padding prepadded = let
  len = RBFile.songLengthMS song
  secsToMS s = floor $ s * 1000
  evalTime t = secsToMS <$> evalPreviewTime True (Just RBFile.getEventsTrack) song padding prepadded t
  evalTime' pt = fromMaybe (error $ "Couldn't evaluate preview bound: " ++ show pt) $ evalTime pt
  defStartTime = max 0 $ case mapMaybe (evalTime . PreviewSection) ["chorus", "chorus_1", "chorus_1a", "verse", "verse_1"] of
    []    -> quot len 2 - 15000
    t : _ -> min (len - 30000) t
  in case (_previewStart $ _metadata syaml, _previewEnd $ _metadata syaml) of
    (Nothing, Nothing) -> (defStartTime, defStartTime + 30000)
    (Just ps, Just pe) -> (evalTime' ps, evalTime' pe)
    (Just ps, Nothing) -> let start = evalTime' ps in (start, start + 30000)
    (Nothing, Just pe) -> let end = evalTime' pe in (max 0 $ end - 30000, end)

evalPreviewTime
  :: Bool -- Should midi and section timestamps be pulled back slightly to account for fade-in?
  -> Maybe (f -> EventsTrack U.Beats)
  -> RBFile.Song f
  -> U.Seconds -- Padding added to the project midi to produce the game midi
  -> Bool -- Are the given midi file and events track already padded?
  -> PreviewTime
  -> Maybe U.Seconds
evalPreviewTime leadin getEvents song padding prepadded = \case
  PreviewSeconds secs -> Just $ secs + padding
  PreviewMIDI mb -> Just $ addLeadin
    $ addMIDIPadding
    $ U.applyTempoMap (RBFile.s_tempos song)
    $ U.unapplyMeasureMap (RBFile.s_signatures song) mb
  PreviewSection str -> addLeadin . addMIDIPadding . U.applyTempoMap (RBFile.s_tempos song)
    <$> findSection str
  where addLeadin = if leadin then (NNC.-| 0.6) else id
        addMIDIPadding = if prepadded then id else (+ padding)
        -- TODO make this more reliable in matching section formatting
        findSection sect = getEvents >>= \f ->
          fmap (fst . fst) $ RTB.viewL $ RTB.filter ((== sect) . snd)
            $ eventsSections $ f $ RBFile.s_tracks song

data DrumTarget
  = DrumTargetRB1x -- pro, 1x
  | DrumTargetRB2x -- pro, 2x
  | DrumTargetCH -- pro, x+
  | DrumTargetGH -- 5-lane, x+

buildDrumTarget
  :: DrumTarget
  -> PartDrums f
  -> U.Beats
  -> U.TempoMap
  -> RBFile.OnyxPart U.Beats
  -> D.DrumTrack U.Beats
buildDrumTarget tgt pd timingEnd tmap opart = let

  src1x   =                             RBFile.onyxPartDrums       opart
  src2x   =                             RBFile.onyxPartDrums2x     opart
  srcReal = D.psRealToPro             $ RBFile.onyxPartRealDrumsPS opart
  srcFull = FD.convertFullDrums False $ RBFile.onyxPartFullDrums   opart
  srcsRB = case tgt of
    DrumTargetRB1x -> [src1x, src2x]
    _              -> [src2x, src1x]
  srcList = case drumsMode pd of
    DrumsReal -> srcReal : srcsRB
    DrumsFull -> srcFull : srcsRB
    _         -> srcsRB
  src = fromMaybe mempty $ find (not . D.nullDrums) srcList

  stepAddKicks = case drumsKicks pd of
    Kicks2x -> mapTrack (U.unapplyTempoTrack tmap) . phaseShiftKicks 0.18 0.11 . mapTrack (U.applyTempoTrack tmap)
    _       -> id

  isRBTarget = case tgt of
    DrumTargetRB1x -> True
    DrumTargetRB2x -> True
    _              -> False

  stepRBKicks = case tgt of
    DrumTargetRB1x -> rockBand1x
    DrumTargetRB2x -> rockBand2x
    _              -> id

  drumEachDiff f dt = dt { D.drumDifficulties = fmap f $ D.drumDifficulties dt }
  step5to4 = if drumsMode pd == Drums5 && isRBTarget
    then drumEachDiff $ \dd -> dd
      { D.drumGems = D.fiveToFour
        (case drumsFallback pd of
          FallbackBlue  -> D.Blue
          FallbackGreen -> D.Green
        )
        (D.drumGems dd)
      }
    else id

  isBasicSource = case drumsMode pd of
    Drums4 -> True
    Drums5 -> True
    _      -> False

  noToms dt = dt { D.drumToms = RTB.empty }
  allToms dt = dt
    { D.drumToms = RTB.fromPairList
      [ (0        , (D.Yellow, D.Tom   ))
      , (0        , (D.Blue  , D.Tom   ))
      , (0        , (D.Green , D.Tom   ))
      , (timingEnd, (D.Yellow, D.Cymbal))
      , (0        , (D.Blue  , D.Cymbal))
      , (0        , (D.Green , D.Cymbal))
      ]
    }
  stepToms = if isBasicSource
    then if isRBTarget
      then allToms
      else noToms
    else id

  -- TODO pro to 5 conversion (for GH target)
  -- Move logic from Neversoft.Export to here

  in stepToms $ step5to4 $ stepRBKicks $ stepAddKicks src

buildDrumAnimation
  :: PartDrums f
  -> U.TempoMap
  -> RBFile.OnyxPart U.Beats
  -> RTB.T U.Beats D.Animation
buildDrumAnimation pd tmap opart = let
  rbTracks = map ($ opart) [RBFile.onyxPartRealDrumsPS, RBFile.onyxPartDrums2x, RBFile.onyxPartDrums]
  inRealTime f = U.unapplyTempoTrack tmap . f . U.applyTempoTrack tmap
  closeTime = 0.25 :: U.Seconds
  in case filter (not . RTB.null) $ map D.drumAnimation rbTracks of
    anims : _ -> anims
    []        -> case drumsMode pd of
      DrumsFull -> inRealTime (FD.autoFDAnimation closeTime)
        $ FD.getDifficulty (Just RB.Expert) $ RBFile.onyxPartFullDrums opart
      -- TODO this could be made better for modes other than pro
      _ -> inRealTime (D.autoDrumAnimation closeTime)
        $ fmap fst $ D.computePro (Just RB.Expert)
        $ case filter (not . D.nullDrums) rbTracks of
          trk : _ -> trk
          []      -> mempty
