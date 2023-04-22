{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE ViewPatterns          #-}
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
import           Data.Fixed                           (Centi, Milli)
import           Data.Foldable                        (toList)
import           Data.Hashable                        (Hashable (..))
import qualified Data.HashMap.Strict                  as HM
import           Data.Int                             (Int32)
import           Data.Maybe                           (fromMaybe, mapMaybe)
import           Data.Scientific                      (Scientific, toRealFloat)
import           Data.String                          (IsString (..))
import qualified Data.Text                            as T
import qualified Data.Vector                          as V
import           GHC.Generics                         (Generic (..))
import qualified Numeric.NonNegative.Class            as NNC
import qualified Onyx.Amplitude.File                  as Amp
import           Onyx.Audio                           hiding (fadeEnd,
                                                       fadeStart)
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import           Onyx.DeriveHelpers
import qualified Onyx.Harmonix.DTA.Serialize.Magma    as Magma
import           Onyx.Harmonix.DTA.Serialize.RockBand (AnimTempo (..))
import           Onyx.MIDI.Common                     (Key (..), SongKey (..),
                                                       Tonality (..), readpKey,
                                                       showKey,
                                                       songKeyUsesFlats)
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File                 as F
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
  { md5      :: Maybe T.Text
  , frames   :: Maybe Integer
  , filePath :: Maybe f
  , commands :: [T.Text]
  , rate     :: Maybe Int
  , channels :: Int
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data AudioFile f
  = AudioFile    (AudioInfo f)
  | AudioSnippet (Audio Duration AudioInput)
  | AudioSamples SamplesInfo
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (AudioInfo f) where
  stackJSON = asStrictObject "AudioInfo" $ do
    md5      <- (.md5     ) =. opt Nothing "md5"       stackJSON
    frames   <- (.frames  ) =. opt Nothing "frames"    stackJSON
    filePath <- (.filePath) =. opt Nothing "file-path" stackJSON
    commands <- (.commands) =. opt []      "commands"  stackJSON
    rate     <- (.rate    ) =. opt Nothing "rate"      stackJSON
    channels <- (.channels) =. opt 2       "channels"  stackJSON
    return AudioInfo{..}

instance (Eq f, StackJSON f) => StackJSON (AudioFile f) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("expr", object $ do
        expr <- requiredKey "expr" fromJSON
        expectedKeys ["expr"]
        return $ AudioSnippet expr
        )
      , ("samples", object $ do
        info <- requiredKey "samples" fromJSON
        expectedKeys ["samples"]
        return $ AudioSamples info
        )
      ] $ AudioFile <$> codecIn stackJSON
    , codecOut = makeOut $ \case
      AudioFile    info -> makeValue stackJSON info
      AudioSnippet expr -> A.object
        [ "expr" .= toJSON expr
        ]
      AudioSamples info -> OneKey "samples" $ makeValue stackJSON info
    }

data SamplesInfo = SamplesInfo
  { groupPolyphony :: Maybe Int
  , groupCrossfade :: Double
  } deriving (Eq, Ord, Show)

instance StackJSON SamplesInfo where
  stackJSON = asStrictObject "SamplesInfo" $ do
    groupPolyphony <- (.groupPolyphony) =. opt Nothing "group-polyphony" stackJSON
    groupCrossfade <- (.groupCrossfade) =. opt 0.002   "group-crossfade" stackJSON
    return SamplesInfo{..}

data JammitTrack = JammitTrack
  { title  :: Maybe T.Text
  , artist :: Maybe T.Text
  } deriving (Eq, Ord, Show)

instance StackJSON JammitTrack where
  stackJSON = asStrictObject "JammitTrack" $ do
    title  <- (.title ) =. opt Nothing "title"  stackJSON
    artist <- (.artist) =. opt Nothing "artist" stackJSON
    return JammitTrack{..}

data PlanAudio t a = PlanAudio
  { expr :: Audio t a
  , pans :: [Double]
  , vols :: [Double]
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON t, StackJSON a) => StackJSON (PlanAudio t a) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("expr", object $ do
        expr <- requiredKey "expr" fromJSON
        pans <- fromMaybe [] <$> optionalKey "pans" fromJSON
        vols <- fromMaybe [] <$> optionalKey "vols" fromJSON
        expectedKeys ["expr", "pans", "vols"]
        return PlanAudio{..}
        )
      ] $ (\expr -> PlanAudio expr [] []) <$> fromJSON
    , codecOut = makeOut $ \case
      PlanAudio expr [] [] -> toJSON expr
      PlanAudio{..} -> A.object $ concat
        [ ["expr" .= expr]
        , ["pans" .= pans]
        , ["vols" .= vols]
        ]
    }

data PartAudio a
  = PartSingle a
  | PartDrumKit
    { kick  :: Maybe a
    , snare :: Maybe a
    , kit   :: a
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (PartAudio a) where
  stackJSON = Codec
    { codecIn = decideKey
      [ ("kit", object $ do
        kick  <- optionalKey "kick"  fromJSON
        snare <- optionalKey "snare" fromJSON
        kit   <- requiredKey "kit"   fromJSON
        expectedKeys ["kick", "snare", "kit"]
        return PartDrumKit{..}
        )
      ] $ PartSingle <$> fromJSON
    , codecOut = makeOut $ \case
      PartSingle x -> toJSON x
      PartDrumKit{..} -> A.object $ concat
        [ map ("kick"  .=) $ toList kick
        , map ("snare" .=) $ toList snare
        , ["kit" .= kit]
        ]
    }

newtype Parts a = Parts { getParts :: HM.HashMap F.FlexPartName a }
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance (StackJSON a) => StackJSON (Parts a) where
  stackJSON = Codec
    { codecIn = Parts . HM.fromList . map (first F.readPartName) . HM.toList
      <$> mapping fromJSON
    , codecOut = makeOut $ \(Parts hm) -> mappingToJSON $ HM.fromList
      $ map (first F.getPartName) $ HM.toList hm
    }

data Plan f
  = StandardPlan (StandardPlanInfo f)
  | MoggPlan     (MoggPlanInfo     f)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data StandardPlanInfo f = StandardPlanInfo
  { song        :: Maybe (PlanAudio Duration AudioInput)
  , parts       :: Parts (PartAudio (PlanAudio Duration AudioInput))
  , crowd       :: Maybe (PlanAudio Duration AudioInput)
  , comments    :: [T.Text]
  , tuningCents :: Int
  , fileTempo   :: Maybe f
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (StandardPlanInfo f) where
  stackJSON = asStrictObject "StandardPlanInfo" $ do
    song        <- (.song       ) =. opt Nothing          "song"         stackJSON
    parts       <- (.parts      ) =. opt (Parts HM.empty) "parts"        stackJSON
    crowd       <- (.crowd      ) =. opt Nothing          "crowd"        stackJSON
    comments    <- (.comments   ) =. opt []               "comments"     stackJSON
    tuningCents <- (.tuningCents) =. opt 0                "tuning-cents" stackJSON
    fileTempo   <- (.fileTempo  ) =. opt Nothing          "file-tempo"   stackJSON
    return StandardPlanInfo{..}

data MoggPlanInfo f = MoggPlanInfo
  { fileMOGG      :: Maybe f
  , moggMD5       :: Maybe T.Text
  , parts         :: Parts (PartAudio [Int])
  , crowd         :: [Int]
  , pans          :: [Double]
  , vols          :: [Double]
  , comments      :: [T.Text]
  , karaoke       :: Bool
  , multitrack    :: Bool
  , tuningCents   :: Int
  , fileTempo     :: Maybe f
  , decryptSilent :: Bool -- should encrypted audio just be treated as silent
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (MoggPlanInfo f) where
  stackJSON = asStrictObject "MoggPlanInfo" $ do
    fileMOGG      <- (.fileMOGG     ) =. opt  Nothing       "file-mogg"      stackJSON
    moggMD5       <- (.moggMD5      ) =. opt  Nothing       "mogg-md5"       stackJSON
    parts         <- (.parts        ) =. req                "parts"          stackJSON
    crowd         <- (.crowd        ) =. opt  []            "crowd"          stackJSON
    pans          <- (.pans         ) =. req                "pans"           stackJSON
    vols          <- (.vols         ) =. req                "vols"           stackJSON
    comments      <- (.comments     ) =. opt  []            "comments"       stackJSON
    karaoke       <- (.karaoke      ) =. fill False         "karaoke"        stackJSON
    multitrack    <- (.multitrack   ) =. fill (not karaoke) "multitrack"     stackJSON
    tuningCents   <- (.tuningCents  ) =. opt  0             "tuning-cents"   stackJSON
    fileTempo     <- (.fileTempo    ) =. opt  Nothing       "file-tempo"     stackJSON
    decryptSilent <- (.decryptSilent) =. opt  False         "decrypt-silent" stackJSON
    return MoggPlanInfo{..}

getTuningCents :: Plan f -> Int
getTuningCents = \case
  StandardPlan x -> x.tuningCents
  MoggPlan     x -> x.tuningCents

getFileTempo :: Plan f -> Maybe f
getFileTempo = \case
  StandardPlan x -> x.fileTempo
  MoggPlan     x -> x.fileTempo

getKaraoke, getMultitrack :: Plan f -> Bool
getKaraoke = \case
  StandardPlan x -> HM.keys x.parts.getParts == [F.FlexVocal]
  MoggPlan     x -> x.karaoke
getMultitrack = \case
  StandardPlan x -> not $ HM.null $ HM.delete F.FlexVocal x.parts.getParts
  MoggPlan     x -> x.multitrack

-- | Parses any of \"measure|beats\", \"seconds\", or \"minutes:seconds\".
parseTimestamp :: (SendMessage m) => StackParser m T.Text (Either U.MeasureBeats U.Seconds)
parseTimestamp = do
  t <- lift ask
  inside ("Parsing timestamp " ++ show t)
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

instance (Eq f, StackJSON f) => StackJSON (Plan f) where
  stackJSON = Codec
    { codecIn = let
      parseMogg     = MoggPlan     <$> fromJSON
      parseStandard = StandardPlan <$> fromJSON
      in decideKey [("mogg-md5", parseMogg), ("file-mogg", parseMogg)] parseStandard
    , codecOut = makeOut $ \case
      StandardPlan x -> toJSON x
      MoggPlan     x -> toJSON x
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
        -- TODO samples? probably don't need
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
      Samples _ _ -> undefined -- TODO but probably don't need
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

instance StackJSON F.FlexPartName where
  stackJSON = Codec
    { codecIn = F.readPartName <$> fromJSON
    , codecOut = makeOut $ A.toJSON . F.getPartName
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
  { difficulty       :: Difficulty
  , hopoThreshold    :: Int
  , fixFreeform      :: Bool
  , sustainGap       :: Int -- ticks, 480 per beat
  , smoothFrets      :: Bool -- should animation fret positions be smoothed out like pre-RB3
  , detectMutedOpens :: Bool -- if open notes are auto-removed, should we detect when opens are used as muted strums in between chords
  } deriving (Eq, Ord, Show)

instance StackJSON PartGRYBO where
  stackJSON = asStrictObject "PartGRYBO" $ do
    difficulty       <- (.difficulty      ) =. fill (Tier 1) "difficulty"         stackJSON
    hopoThreshold    <- (.hopoThreshold   ) =. opt  170      "hopo-threshold"     stackJSON
    fixFreeform      <- (.fixFreeform     ) =. opt  True     "fix-freeform"       stackJSON
    sustainGap       <- (.sustainGap      ) =. opt  60       "sustain-gap"        stackJSON
    smoothFrets      <- (.smoothFrets     ) =. opt  False    "smooth-frets"       stackJSON
    detectMutedOpens <- (.detectMutedOpens) =. opt  True     "detect-muted-opens" stackJSON
    return PartGRYBO{..}

instance Default PartGRYBO where
  def = fromEmptyObject

data PartProKeys = PartProKeys
  { difficulty  :: Difficulty
  , fixFreeform :: Bool
  } deriving (Eq, Ord, Show)

instance StackJSON PartProKeys where
  stackJSON = asStrictObject "PartProKeys" $ do
    difficulty  <- (.difficulty ) =. fill (Tier 1) "difficulty"   stackJSON
    fixFreeform <- (.fixFreeform) =. opt  True     "fix-freeform" stackJSON
    return PartProKeys{..}

data PartProGuitar f = PartProGuitar
  { difficulty    :: Difficulty
  , hopoThreshold :: Int
  , tuning        :: GtrTuning
  , tuningRSBass  :: Maybe GtrTuning -- for 5/6-string bass which also has a 4-string arrangement
  , fixFreeform   :: Bool
  , tones         :: Maybe (RSTones f)
  , pickedBass    :: Bool
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
    difficulty    <- (.difficulty   ) =. fill (Tier 1) "difficulty"     stackJSON
    hopoThreshold <- (.hopoThreshold) =. opt  170      "hopo-threshold" stackJSON
    tuning        <- (.tuning       ) =. opt  def      "tuning"         tuningFormat
    tuningRSBass  <- (.tuningRSBass ) =. opt  Nothing  "tuning-rs-bass" (maybeCodec tuningFormat)
    fixFreeform   <- (.fixFreeform  ) =. opt  True     "fix-freeform"   stackJSON
    tones         <- (.tones        ) =. opt  Nothing  "tones"          stackJSON
    pickedBass    <- (.pickedBass   ) =. opt  False    "picked-bass"    stackJSON
    return PartProGuitar{..}

instance (Eq f, StackJSON f) => Default (PartProGuitar f) where
  def = fromEmptyObject

data RSTones f = RSTones
  { fileToneBase :: f
  , fileToneA    :: Maybe f
  , fileToneB    :: Maybe f
  , fileToneC    :: Maybe f
  , fileToneD    :: Maybe f
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (RSTones f) where
  stackJSON = asStrictObject "RSTones" $ do
    fileToneBase <- (.fileToneBase) =. req         "file-tone-base" stackJSON
    fileToneA    <- (.fileToneA   ) =. opt Nothing "file-tone-a"    stackJSON
    fileToneB    <- (.fileToneB   ) =. opt Nothing "file-tone-b"    stackJSON
    fileToneC    <- (.fileToneC   ) =. opt Nothing "file-tone-c"    stackJSON
    fileToneD    <- (.fileToneD   ) =. opt Nothing "file-tone-d"    stackJSON
    return RSTones{..}

data PartGHL = PartGHL
  { difficulty    :: Difficulty
  , hopoThreshold :: Int
  } deriving (Eq, Ord, Show)

instance StackJSON PartGHL where
  stackJSON = asStrictObject "PartGHL" $ do
    difficulty    <- (.difficulty   ) =. fill (Tier 1) "difficulty"     stackJSON
    hopoThreshold <- (.hopoThreshold) =. opt  170      "hopo-threshold" stackJSON
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

data TrueDrumLayout
  = TDStandard -- snare, hihat, left crash
  | TDOpenHand -- left crash, hihat, snare
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON TrueDrumLayout where
  stackJSON = enumCodec "a true drums layout" $ \case
    TDStandard -> "standard"
    TDOpenHand -> "open-hand"

data DrumMode
  = Drums4
  | Drums5
  | DrumsPro
  | DrumsReal
  | DrumsTrue
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON DrumMode where
  stackJSON = enumCodec "a drum mode (4, 5, pro, real, true)" $ \case
    Drums4    -> A.Number 4
    Drums5    -> A.Number 5
    DrumsPro  -> "pro"
    DrumsReal -> "real"
    DrumsTrue -> "true"

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
  { difficulty    :: Difficulty
  , mode          :: DrumMode
  , kicks         :: Kicks
  , fixFreeform   :: Bool
  , kit           :: DrumKit
  , layout        :: DrumLayout
  , fallback      :: OrangeFallback
  , fileDTXKit    :: Maybe f
  , trueLayout    :: TrueDrumLayout
  , difficultyDTX :: Maybe Centi
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (PartDrums f) where
  stackJSON = asStrictObject "PartDrums" $ do
    difficulty    <- (.difficulty   ) =. fill    (Tier 1)       "difficulty"     stackJSON
    mode          <- (.mode         ) =. opt     DrumsPro       "mode"           stackJSON
    kicks         <- (.kicks        ) =. warning Kicks1x        "kicks"          stackJSON
    fixFreeform   <- (.fixFreeform  ) =. opt     True           "fix-freeform"   stackJSON
    kit           <- (.kit          ) =. opt     HardRockKit    "kit"            stackJSON
    layout        <- (.layout       ) =. opt     StandardLayout "layout"         stackJSON
    fallback      <- (.fallback     ) =. opt     FallbackGreen  "fallback"       stackJSON
    fileDTXKit    <- (.fileDTXKit   ) =. opt     Nothing        "file-dtx-kit"   stackJSON
    trueLayout    <- (.trueLayout   ) =. opt     TDStandard     "true-layout"    stackJSON
    difficultyDTX <- (.difficultyDTX) =. opt     Nothing        "difficulty-dtx" stackJSON
    return PartDrums{..}

emptyPartDrums :: DrumMode -> Kicks -> PartDrums f
emptyPartDrums mode kicks = PartDrums
  { difficulty    = Tier 1
  , mode          = mode
  , kicks         = kicks
  , fixFreeform   = False
  , kit           = HardRockKit
  , layout        = StandardLayout
  , fallback      = FallbackGreen
  , fileDTXKit    = Nothing
  , trueLayout    = TDStandard
  , difficultyDTX = Nothing
  }

data VocalCount = Vocal1 | Vocal2 | Vocal3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance StackJSON VocalCount where
  stackJSON = enumCodec "a vocal part count (1 to 3)" $ \case
    Vocal1 -> A.Number 1
    Vocal2 -> A.Number 2
    Vocal3 -> A.Number 3

data PartVocal f = PartVocal
  { difficulty :: Difficulty
  , count      :: VocalCount
  , gender     :: Maybe Magma.Gender
  , key        :: Maybe Key
  , lipsyncRB3 :: Maybe (LipsyncRB3 f)
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (PartVocal f) where
  stackJSON = asStrictObject "PartVocal" $ do
    difficulty <- (.difficulty) =. fill (Tier 1) "difficulty"  stackJSON
    count      <- (.count     ) =. opt  Vocal1   "count"       stackJSON
    gender     <- (.gender    ) =. opt  Nothing  "gender"      (maybeCodec parseGender)
    key        <- (.key       ) =. opt  Nothing  "key"         (maybeCodec parsePitch)
    lipsyncRB3 <- (.lipsyncRB3) =. opt  Nothing  "lipsync-rb3" stackJSON
    return PartVocal{..}

data PartAmplitude = PartAmplitude
  { instrument :: Amp.Instrument
  } deriving (Eq, Ord, Show)

instance StackJSON PartAmplitude where
  stackJSON = asStrictObject "PartAmplitude" $ do
    instrument <- (.instrument) =. req "instrument" stackJSON
    return PartAmplitude{..}

instance StackJSON Amp.Instrument where
  stackJSON = enumCodec "amplitude instrument type" $ \case
    Amp.Drums  -> "drums"
    Amp.Bass   -> "bass"
    Amp.Synth  -> "synth"
    Amp.Vocal  -> "vocal"
    Amp.Guitar -> "guitar"

data PartDance = PartDance
  { difficulty :: Difficulty
  } deriving (Eq, Ord, Show)

instance StackJSON PartDance where
  stackJSON = asStrictObject "PartDance" $ do
    difficulty <- (.difficulty) =. fill (Tier 1) "difficulty" stackJSON
    return PartDance{..}

data PartMania = PartMania
  { keys      :: Int
  , turntable :: Bool
  } deriving (Eq, Ord, Show)

instance StackJSON PartMania where
  stackJSON = asStrictObject "PartMania" $ do
    keys      <- (.keys)      =. req       "keys"      stackJSON
    turntable <- (.turntable) =. opt False "turntable" stackJSON
    return PartMania{..}

data Part f = Part
  { grybo     :: Maybe PartGRYBO
  , ghl       :: Maybe PartGHL
  , proKeys   :: Maybe PartProKeys
  , proGuitar :: Maybe (PartProGuitar f)
  , drums     :: Maybe (PartDrums f)
  , vocal     :: Maybe (PartVocal f)
  , amplitude :: Maybe PartAmplitude
  , dance     :: Maybe PartDance
  , mania     :: Maybe PartMania
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (Part f) where
  stackJSON = asStrictObject "Part" $ do
    grybo     <- (.grybo    ) =. opt Nothing "grybo"      stackJSON
    ghl       <- (.ghl      ) =. opt Nothing "ghl"        stackJSON
    proKeys   <- (.proKeys  ) =. opt Nothing "pro-keys"   stackJSON
    proGuitar <- (.proGuitar) =. opt Nothing "pro-guitar" stackJSON
    drums     <- (.drums    ) =. opt Nothing "drums"      stackJSON
    vocal     <- (.vocal    ) =. opt Nothing "vocal"      stackJSON
    amplitude <- (.amplitude) =. opt Nothing "amplitude"  stackJSON
    dance     <- (.dance    ) =. opt Nothing "dance"      stackJSON
    mania     <- (.mania    ) =. opt Nothing "mania"      stackJSON
    return Part{..}

emptyPart :: Part f
emptyPart = Part Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
            p = parseFrom str $ either PreviewMIDI PreviewSeconds <$> parseTimestamp
            in p `catchError` \_ -> expected "a preview time: prc_something, timestamp, or measure|beats"
      in traceNum `catchError` \_ -> traceStr
    , codecOut = makeOut $ \case
      PreviewSection str  -> A.toJSON $ "prc_" <> str
      PreviewMIDI mb      -> A.toJSON $ showMeasureBeats mb
      PreviewSeconds secs -> showTimestamp $ realToFrac secs
    }

-- | Extra information with no gameplay effect.
data Metadata f = Metadata
  { title        :: Maybe T.Text
  , titleJP      :: Maybe T.Text
  , artist       :: Maybe T.Text
  , artistJP     :: Maybe T.Text
  , album        :: Maybe T.Text
  , genre        :: Maybe T.Text
  , subgenre     :: Maybe T.Text
  , year         :: Maybe Int
  , fileAlbumArt :: Maybe f
  , trackNumber  :: Maybe Int
  , comments     :: [T.Text]
  , key          :: Maybe SongKey
  , author       :: Maybe T.Text
  , rating       :: Rating
  , previewStart :: Maybe PreviewTime
  , previewEnd   :: Maybe PreviewTime
  , languages    :: [T.Text]
  , convert      :: Bool
  , rhythmKeys   :: Bool -- should be in target!
  , rhythmBass   :: Bool -- should be in target!
  , catEMH       :: Bool
  , expertOnly   :: Bool
  , cover        :: Bool
  , difficulty   :: Difficulty
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
    title        <- (.title       ) =. warning Nothing  "title"          stripped
    titleJP      <- (.titleJP     ) =. opt     Nothing  "title-jp"       stripped
    artist       <- (.artist      ) =. warning Nothing  "artist"         stripped
    artistJP     <- (.artistJP    ) =. opt     Nothing  "artist-jp"      stripped
    album        <- (.album       ) =. opt     Nothing  "album"          stripped
    genre        <- (.genre       ) =. warning Nothing  "genre"          stripped
    subgenre     <- (.subgenre    ) =. opt     Nothing  "subgenre"       stripped
    year         <- (.year        ) =. warning Nothing  "year"           stackJSON
    fileAlbumArt <- (.fileAlbumArt) =. opt     Nothing  "file-album-art" stackJSON
    trackNumber  <- (.trackNumber ) =. opt     Nothing  "track-number"   stackJSON
    comments     <- (.comments    ) =. opt     []       "comments"       stackJSON
    key          <- (.key         ) =. opt     Nothing  "key"            (maybeCodec parseSongKey)
    author       <- (.author      ) =. warning Nothing  "author"         stripped
    rating       <- (.rating      ) =. opt     Unrated  "rating"         stackJSON
    previewStart <- (.previewStart) =. opt     Nothing  "preview-start"  stackJSON
    previewEnd   <- (.previewEnd  ) =. opt     Nothing  "preview-end"    stackJSON
    languages    <- (.languages   ) =. opt     []       "languages"      stackJSON
    convert      <- (.convert     ) =. opt     False    "convert"        stackJSON
    rhythmKeys   <- (.rhythmKeys  ) =. opt     False    "rhythm-keys"    stackJSON
    rhythmBass   <- (.rhythmBass  ) =. opt     False    "rhythm-bass"    stackJSON
    catEMH       <- (.catEMH      ) =. opt     False    "cat-emh"        stackJSON
    expertOnly   <- (.expertOnly  ) =. opt     False    "expert-only"    stackJSON
    cover        <- (.cover       ) =. opt     False    "cover"          stackJSON
    difficulty   <- (.difficulty  ) =. fill    (Tier 1) "difficulty"     stackJSON
    return Metadata{..}

instance (Eq f, StackJSON f) => Default (Metadata f) where
  def = fromEmptyObject

data Global f = Global
  { fileMidi            :: f
  , fileSongAnim        :: Maybe f -- ^ venue format found in RB3 milos
  , autogenTheme        :: Maybe Magma.AutogenTheme -- ^ 'Nothing' means black venue
  , animTempo           :: Either AnimTempo Integer
  , backgroundVideo     :: Maybe (VideoInfo f)
  , fileBackgroundImage :: Maybe f
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, IsString f, StackJSON f) => StackJSON (Global f) where
  stackJSON = asStrictObject "Global" $ do
    fileMidi            <- (.fileMidi           ) =. opt "notes.mid"         "file-midi"             stackJSON
    fileSongAnim        <- (.fileSongAnim       ) =. opt Nothing             "file-song-anim"        stackJSON
    autogenTheme        <- (.autogenTheme       ) =. opt (Just Magma.DefaultTheme) "autogen-theme"   stackJSON
    animTempo           <- (.animTempo          ) =. opt (Left KTempoMedium) "anim-tempo"            parseAnimTempo
    backgroundVideo     <- (.backgroundVideo    ) =. opt Nothing             "background-video"      stackJSON
    fileBackgroundImage <- (.fileBackgroundImage) =. opt Nothing             "file-background-image" stackJSON
    return Global{..}

instance (Eq f, IsString f, StackJSON f) => Default (Global f) where
  def = fromEmptyObject

data VideoInfo f = VideoInfo
  { fileVideo      :: f
  , videoStartTime :: Maybe Milli -- seconds, can be negative
  , videoEndTime   :: Maybe Milli
  , videoLoop      :: Bool
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (StackJSON f) => StackJSON (VideoInfo f) where
  stackJSON = asStrictObject "VideoInfo" $ do
    fileVideo      <- (.fileVideo     ) =. req         "file-video" stackJSON
    videoStartTime <- (.videoStartTime) =. opt Nothing "start-time" stackJSON
    videoEndTime   <- (.videoEndTime  ) =. opt Nothing "end-time"   stackJSON
    videoLoop      <- (.videoLoop     ) =. opt False   "loop"       stackJSON
    return VideoInfo{..}

getTitle, getArtist, getAlbum, getAuthor :: Metadata f -> T.Text
getTitle  m = case m.title  of Just x | not $ T.null x -> x; _ -> "Untitled"
getArtist m = case m.artist of Just x | not $ T.null x -> x; _ -> "Unknown Artist"
getAlbum  m = case m.album  of Just x | not $ T.null x -> x; _ -> "Unknown Album"
getAuthor m = case m.author of Just x | not $ T.null x -> x; _ -> "Unknown Author"

getYear, getTrackNumber :: Metadata f -> Int
getYear        = fromMaybe 1960 . (.year)
getTrackNumber = fromMaybe 1    . (.trackNumber)

data TargetCommon = TargetCommon
  { speed   :: Maybe Double
  , plan    :: Maybe T.Text
  , title   :: Maybe T.Text -- override base song title
  , label_  :: Maybe T.Text -- suffix after title
  , label2x :: Bool -- if automatic label and 2x drums, should we add (2x Bass Pedal)
  , start   :: Maybe SegmentEdge
  , end     :: Maybe SegmentEdge
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetCommon :: (SendMessage m) => ObjectCodec m A.Value TargetCommon
parseTargetCommon = do
  speed   <- (.speed  ) =. opt Nothing "speed"    stackJSON
  plan    <- (.plan   ) =. opt Nothing "plan"     stackJSON
  title   <- (.title  ) =. opt Nothing "title"    stackJSON
  label_  <- (.label_ ) =. opt Nothing "label"    stackJSON
  label2x <- (.label2x) =. opt True    "label-2x" stackJSON
  start   <- (.start  ) =. opt Nothing "start"    stackJSON
  end     <- (.end    ) =. opt Nothing "end"      stackJSON
  return TargetCommon{..}

instance Default TargetCommon where
  def = TargetCommon Nothing Nothing Nothing Nothing True Nothing Nothing

data SegmentEdge = SegmentEdge
  { fadeStart :: PreviewTime
  , fadeEnd   :: PreviewTime
  , notes     :: PreviewTime
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseSegmentEdge :: (SendMessage m) => ObjectCodec m A.Value SegmentEdge
parseSegmentEdge = do
  fadeStart <- (.fadeStart) =. req "fade-start" stackJSON
  fadeEnd   <- (.fadeEnd  ) =. req "fade-end"   stackJSON
  notes     <- (.notes    ) =. req "notes"      stackJSON
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
  { common        :: TargetCommon
  , is2xBassPedal :: Bool
  , songID        :: RBSongID
  , version       :: Maybe Integer
  , harmonix      :: Bool
  , magma         :: MagmaSetting
  , guitar        :: F.FlexPartName
  , bass          :: F.FlexPartName
  , drums         :: F.FlexPartName
  , keys          :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , ps3Encrypt    :: Bool
  , legalTempos   :: Bool -- should tempos be kept in Magma-legal range of 40-300 BPM
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetRB3 :: (SendMessage m) => ObjectCodec m A.Value TargetRB3
parseTargetRB3 = do
  common        <- (.common       ) =. parseTargetCommon
  is2xBassPedal <- (.is2xBassPedal) =. opt  False            "2x-bass-pedal" stackJSON
  songID        <- (.songID       ) =. fill SongIDAutoSymbol "song-id"       stackJSON
  version       <- (.version      ) =. opt  Nothing          "version"       stackJSON
  harmonix      <- (.harmonix     ) =. opt  False            "harmonix"      stackJSON
  magma         <- (.magma        ) =. opt  MagmaRequire     "magma"         stackJSON
  guitar        <- (.guitar       ) =. opt  F.FlexGuitar     "guitar"        stackJSON
  bass          <- (.bass         ) =. opt  F.FlexBass       "bass"          stackJSON
  drums         <- (.drums        ) =. opt  F.FlexDrums      "drums"         stackJSON
  keys          <- (.keys         ) =. opt  F.FlexKeys       "keys"          stackJSON
  vocal         <- (.vocal        ) =. opt  F.FlexVocal      "vocal"         stackJSON
  ps3Encrypt    <- (.ps3Encrypt   ) =. opt  True             "ps3-encrypt"   stackJSON
  legalTempos   <- (.legalTempos  ) =. opt  True             "legal-tempos"  stackJSON
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
  { sources :: [LipsyncSource f]
  , member2 :: LipsyncMember
  , member3 :: LipsyncMember
  , member4 :: LipsyncMember
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance (Eq f, StackJSON f) => StackJSON (LipsyncRB3 f) where
  stackJSON = asStrictObject "LipsyncRB3" $ do
    sources <- (.sources) =. req "sources" stackJSON
    member2 <- (.member2) =. opt LipsyncGuitar "member-2" stackJSON
    member3 <- (.member3) =. opt LipsyncBass   "member-3" stackJSON
    member4 <- (.member4) =. opt LipsyncDrums  "member-4" stackJSON
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
  { common        :: TargetCommon
  , is2xBassPedal :: Bool
  , songID        :: RBSongID
  , labelRB2      :: Bool
  , version       :: Maybe Integer
  , magma         :: MagmaSetting -- this currently only affects Magma v2; v1 is always tried but optional
  , guitar        :: F.FlexPartName
  , bass          :: F.FlexPartName
  , drums         :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , ps3Encrypt    :: Bool
  , legalTempos   :: Bool -- should tempos be kept in Magma-legal range of 40-300 BPM
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetRB2 :: (SendMessage m) => ObjectCodec m A.Value TargetRB2
parseTargetRB2 = do
  common        <- (.common       ) =. parseTargetCommon
  is2xBassPedal <- (.is2xBassPedal) =. opt  False            "2x-bass-pedal" stackJSON
  songID        <- (.songID       ) =. fill SongIDAutoSymbol "song-id"       stackJSON
  labelRB2      <- (.labelRB2     ) =. opt  False            "label-rb2"     stackJSON
  version       <- (.version      ) =. opt  Nothing          "version"       stackJSON
  magma         <- (.magma        ) =. opt  MagmaRequire     "magma"         stackJSON
  guitar        <- (.guitar       ) =. opt  F.FlexGuitar     "guitar"        stackJSON
  bass          <- (.bass         ) =. opt  F.FlexBass       "bass"          stackJSON
  drums         <- (.drums        ) =. opt  F.FlexDrums      "drums"         stackJSON
  vocal         <- (.vocal        ) =. opt  F.FlexVocal      "vocal"         stackJSON
  ps3Encrypt    <- (.ps3Encrypt   ) =. opt  True             "ps3-encrypt"   stackJSON
  legalTempos   <- (.legalTempos  ) =. opt  True             "legal-tempos"  stackJSON
  return TargetRB2{..}

instance StackJSON TargetRB2 where
  stackJSON = asStrictObject "TargetRB2" parseTargetRB2

instance Default TargetRB2 where
  def = fromEmptyObject

data TargetPS = TargetPS
  { common        :: TargetCommon
  , guitar        :: F.FlexPartName
  , bass          :: F.FlexPartName
  , drums         :: F.FlexPartName
  , keys          :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , rhythm        :: F.FlexPartName
  , guitarCoop    :: F.FlexPartName
  , dance         :: F.FlexPartName
  , loadingPhrase :: Maybe T.Text
  , bigRockEnding :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPS :: (SendMessage m) => ObjectCodec m A.Value TargetPS
parseTargetPS = do
  common        <- (.common       ) =. parseTargetCommon
  guitar        <- (.guitar       ) =. opt F.FlexGuitar                "guitar"          stackJSON
  bass          <- (.bass         ) =. opt F.FlexBass                  "bass"            stackJSON
  drums         <- (.drums        ) =. opt F.FlexDrums                 "drums"           stackJSON
  keys          <- (.keys         ) =. opt F.FlexKeys                  "keys"            stackJSON
  vocal         <- (.vocal        ) =. opt F.FlexVocal                 "vocal"           stackJSON
  rhythm        <- (.rhythm       ) =. opt (F.FlexExtra "rhythm"     ) "rhythm"          stackJSON
  guitarCoop    <- (.guitarCoop   ) =. opt (F.FlexExtra "guitar-coop") "guitar-coop"     stackJSON
  dance         <- (.dance        ) =. opt (F.FlexExtra "dance"      ) "dance"           stackJSON
  loadingPhrase <- (.loadingPhrase) =. opt Nothing                     "loading-phrase"  stackJSON
  bigRockEnding <- (.bigRockEnding) =. opt True                        "big-rock-ending" stackJSON
  return TargetPS{..}

instance StackJSON TargetPS where
  stackJSON = asStrictObject "TargetPS" parseTargetPS

instance Default TargetPS where
  def = fromEmptyObject

data TargetGH1 = TargetGH1
  { common        :: TargetCommon
  , guitar        :: F.FlexPartName
  , bass          :: F.FlexPartName
  , drums         :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , keys          :: F.FlexPartName
  , key           :: Maybe T.Text -- top symbol
  , loadingPhrase :: Maybe T.Text -- these go in ghui/eng/gen/locale.dtb, loading_tip_thesongkey
  , offset        :: Double -- in seconds, positive means pull audio earlier, negative means push later
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH1 :: (SendMessage m) => ObjectCodec m A.Value TargetGH1
parseTargetGH1 = do
  common        <- (.common       ) =. parseTargetCommon
  guitar        <- (.guitar       ) =. opt F.FlexGuitar "guitar"         stackJSON
  bass          <- (.bass         ) =. opt F.FlexBass   "bass"           stackJSON
  drums         <- (.drums        ) =. opt F.FlexDrums  "drums"          stackJSON
  keys          <- (.keys         ) =. opt F.FlexKeys   "keys"           stackJSON
  vocal         <- (.vocal        ) =. opt F.FlexVocal  "vocal"          stackJSON
  key           <- (.key          ) =. opt Nothing    "key"            stackJSON
  loadingPhrase <- (.loadingPhrase) =. opt Nothing    "loading-phrase" stackJSON
  offset        <- (.offset       ) =. opt 0          "offset"         stackJSON
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
  { common        :: TargetCommon
  , guitar        :: F.FlexPartName
  , bass          :: F.FlexPartName
  , rhythm        :: F.FlexPartName
  , drums         :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , keys          :: F.FlexPartName
  , coop          :: GH2Coop
  , key           :: Maybe T.Text -- top symbol for 360 DLC
  , context       :: Maybe Int -- contexts.dta for 360 DLC
  , leaderboard   :: Maybe (Int, Int) -- leaderboards.dta for 360 DLC
  , practiceAudio :: Bool -- should we make slow audio for PS2
  , loadingPhrase :: Maybe T.Text
  , offset        :: Double -- in seconds, positive means pull audio earlier, negative means push later
  , gh2Deluxe     :: Bool -- enables gh2dx features: drum chart, extra metadata sets, album art, future hopo/strum/tap markers
  , is2xBassPedal :: Bool
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH2 :: (SendMessage m) => ObjectCodec m A.Value TargetGH2
parseTargetGH2 = do
  common        <- (.common       ) =. parseTargetCommon
  guitar        <- (.guitar       ) =. opt F.FlexGuitar           "guitar"         stackJSON
  bass          <- (.bass         ) =. opt F.FlexBass             "bass"           stackJSON
  rhythm        <- (.rhythm       ) =. opt (F.FlexExtra "rhythm") "rhythm"         stackJSON
  drums         <- (.drums        ) =. opt F.FlexDrums            "drums"          stackJSON
  keys          <- (.keys         ) =. opt F.FlexKeys             "keys"           stackJSON
  vocal         <- (.vocal        ) =. opt F.FlexVocal            "vocal"          stackJSON
  coop          <- (.coop         ) =. opt GH2Bass              "coop"           stackJSON
  key           <- (.key          ) =. opt Nothing              "key"            stackJSON
  context       <- (.context      ) =. opt Nothing              "context"        stackJSON
  leaderboard   <- (.leaderboard  ) =. opt Nothing              "leaderboard"    stackJSON
  practiceAudio <- (.practiceAudio) =. opt True                 "practice-audio" stackJSON
  loadingPhrase <- (.loadingPhrase) =. opt Nothing              "loading-phrase" stackJSON
  offset        <- (.offset       ) =. opt 0                    "offset"         stackJSON
  gh2Deluxe     <- (.gh2Deluxe    ) =. opt False                "gh2-deluxe"     stackJSON
  is2xBassPedal <- (.is2xBassPedal) =. opt False                "2x-bass-pedal"  stackJSON
  return TargetGH2{..}

instance StackJSON TargetGH2 where
  stackJSON = asStrictObject "TargetGH2" parseTargetGH2

instance Default TargetGH2 where
  def = fromEmptyObject

-- gh5 or ghwor, mostly same formats
data TargetGH5 = TargetGH5
  { common :: TargetCommon
  , guitar :: F.FlexPartName
  , bass   :: F.FlexPartName
  , drums  :: F.FlexPartName
  , vocal  :: F.FlexPartName
  , songID :: Maybe Int -- like 783 in "adlc783_1.fsb.xen"
  , cdl    :: Maybe Int -- like 511 in "cdl511"
  , proTo4 :: Bool -- true if pro drums should just use RYBG
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH5 :: (SendMessage m) => ObjectCodec m A.Value TargetGH5
parseTargetGH5 = do
  common <- (.common) =. parseTargetCommon
  guitar <- (.guitar) =. opt F.FlexGuitar "guitar"   stackJSON
  bass   <- (.bass  ) =. opt F.FlexBass   "bass"     stackJSON
  drums  <- (.drums ) =. opt F.FlexDrums  "drums"    stackJSON
  vocal  <- (.vocal ) =. opt F.FlexVocal  "vocal"    stackJSON
  songID <- (.songID) =. opt Nothing    "song-id"  stackJSON
  cdl    <- (.cdl   ) =. opt Nothing    "cdl"      stackJSON
  proTo4 <- (.proTo4) =. opt False      "pro-to-4" stackJSON
  return TargetGH5{..}

instance StackJSON TargetGH5 where
  stackJSON = asStrictObject "TargetGH5" parseTargetGH5

instance Default TargetGH5 where
  def = fromEmptyObject

data TargetGH3 = TargetGH3
  { common :: TargetCommon
  , guitar :: F.FlexPartName
  , bass   :: F.FlexPartName
  , rhythm :: F.FlexPartName
  , coop   :: GH2Coop
  , drums  :: F.FlexPartName
  , vocal  :: F.FlexPartName
  , keys   :: F.FlexPartName
  , songID :: Maybe (Either Int T.Text) -- Int gets title/artist added, Text is exact
  , dl     :: Maybe Int -- like 15 in "dl15"
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetGH3 :: (SendMessage m) => ObjectCodec m A.Value TargetGH3
parseTargetGH3 = do
  common <- (.common) =. parseTargetCommon
  guitar <- (.guitar) =. opt F.FlexGuitar           "guitar"  stackJSON
  bass   <- (.bass  ) =. opt F.FlexBass             "bass"    stackJSON
  rhythm <- (.rhythm) =. opt (F.FlexExtra "rhythm") "rhythm"  stackJSON
  coop   <- (.coop  ) =. opt GH2Bass              "coop"    stackJSON
  drums  <- (.drums ) =. opt F.FlexDrums            "drums"   stackJSON
  vocal  <- (.vocal ) =. opt F.FlexVocal            "vocal"   stackJSON
  keys   <- (.keys  ) =. opt F.FlexVocal            "keys"    stackJSON
  songID <- (.songID) =. opt Nothing              "song-id" stackJSON
  dl     <- (.dl    ) =. opt Nothing              "dl"      stackJSON
  return TargetGH3{..}

instance StackJSON TargetGH3 where
  stackJSON = asStrictObject "TargetGH3" parseTargetGH3

instance Default TargetGH3 where
  def = fromEmptyObject

data TargetDTX = TargetDTX
  { common      :: TargetCommon
  , drums       :: F.FlexPartName
  , guitar      :: F.FlexPartName
  , bass        :: F.FlexPartName
  , planPreview :: Maybe T.Text -- used for preview snippet, since .plan will usually be drumless
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetDTX :: (SendMessage m) => ObjectCodec m A.Value TargetDTX
parseTargetDTX = do
  common      <- (.common     ) =. parseTargetCommon
  guitar      <- (.guitar     ) =. opt F.FlexGuitar "guitar"       stackJSON
  bass        <- (.bass       ) =. opt F.FlexBass   "bass"         stackJSON
  drums       <- (.drums      ) =. opt F.FlexDrums  "drums"        stackJSON
  planPreview <- (.planPreview) =. opt Nothing      "plan-preview" stackJSON
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
  { common       :: TargetCommon
  , arrangements :: [(RSArrSlot, F.FlexPartName)]
  , vocal        :: F.FlexPartName
  , songKey      :: Maybe T.Text
  , version      :: Maybe T.Text
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

parseRSArr :: (SendMessage m) => ValueCodec m A.Value (RSArrSlot, F.FlexPartName)
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
  common       <- (.common      ) =. parseTargetCommon
  arrangements <- (.arrangements) =. opt [] "arrangements" (listCodec parseRSArr)
  vocal        <- (.vocal       ) =. opt F.FlexVocal "vocal" stackJSON
  songKey      <- (.songKey     ) =. opt Nothing "song-key" stackJSON
  version      <- (.version     ) =. opt Nothing "version" stackJSON
  return TargetRS{..}

instance StackJSON TargetRS where
  stackJSON = asStrictObject "TargetRS" parseTargetRS

instance Default TargetRS where
  def = fromEmptyObject

data TargetPG = TargetPG
  { common        :: TargetCommon
  , is2xBassPedal :: Bool
  , guitar        :: F.FlexPartName
  , drums         :: F.FlexPartName
  , vocal         :: F.FlexPartName
  , key           :: Maybe T.Text
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPG :: (SendMessage m) => ObjectCodec m A.Value TargetPG
parseTargetPG = do
  common        <- (.common       ) =. parseTargetCommon
  is2xBassPedal <- (.is2xBassPedal) =. opt False        "2x-bass-pedal" stackJSON
  guitar        <- (.guitar       ) =. opt F.FlexGuitar   "guitar"        stackJSON
  drums         <- (.drums        ) =. opt F.FlexDrums    "drums"         stackJSON
  vocal         <- (.vocal        ) =. opt F.FlexVocal    "vocal"         stackJSON
  key           <- (.key          ) =. opt Nothing      "key"           stackJSON
  return TargetPG{..}

instance StackJSON TargetPG where
  stackJSON = asStrictObject "TargetPG" parseTargetPG

instance Default TargetPG where
  def = fromEmptyObject

data TargetPart = TargetPart
  { common :: TargetCommon
  , part   :: F.FlexPartName
  } deriving (Eq, Ord, Show, Generic, Hashable)

parseTargetPart :: (SendMessage m) => ObjectCodec m A.Value TargetPart
parseTargetPart = do
  common <- (.common) =. parseTargetCommon
  part   <- (.part  ) =. opt (F.FlexExtra "global") "part" stackJSON
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
  deriving (Eq, Ord, Show, Generic, Hashable)

targetCommon :: Target -> TargetCommon
targetCommon = \case
  RB3    x -> x.common
  RB2    x -> x.common
  PS     x -> x.common
  GH1    x -> x.common
  GH2    x -> x.common
  GH3    x -> x.common
  GH5    x -> x.common
  RS     x -> x.common
  DTX    x -> x.common
  PG     x -> x.common

addKey :: (forall m. (SendMessage m) => ObjectCodec m A.Value a) -> T.Text -> A.Value -> a -> A.Value
addKey codec k v x = A.Object $ KM.fromHashMapText $ HM.insert k v $ HM.fromList $ makeObject (objectId codec) x

instance StackJSON Target where
  stackJSON = Codec
    { codecIn = object $ do
      target <- requiredKey "game" fromJSON
      hm <- lift ask
      parseFrom (A.Object $ KM.fromHashMapText $ HM.delete "game" hm) $ case target :: T.Text of
        "rb3" -> fmap RB3    fromJSON
        "rb2" -> fmap RB2    fromJSON
        "ps"  -> fmap PS     fromJSON
        "gh2" -> fmap GH2    fromJSON
        "gh3" -> fmap GH3    fromJSON
        "gh5" -> fmap GH5    fromJSON
        "rs"  -> fmap RS     fromJSON
        "dtx" -> fmap DTX    fromJSON
        "pg"  -> fmap PG     fromJSON
        _     -> fatal $ "Unrecognized target game: " ++ show target
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
    }

data SongYaml f = SongYaml
  { metadata :: Metadata f
  , global   :: Global f
  , audio    :: HM.HashMap T.Text (AudioFile f)
  , jammit   :: HM.HashMap T.Text JammitTrack
  , plans    :: HM.HashMap T.Text (Plan f)
  , targets  :: HM.HashMap T.Text Target
  , parts    :: Parts (Part f)
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance (Eq f, IsString f, StackJSON f) => StackJSON (SongYaml f) where
  stackJSON = asStrictObject "SongYaml" $ do
    metadata <- (.metadata) =. opt def      "metadata" stackJSON
    global   <- (.global  ) =. opt def      "global"   stackJSON
    audio    <- (.audio   ) =. opt HM.empty "audio"    (dict stackJSON)
    jammit   <- (.jammit  ) =. opt HM.empty "jammit"   (dict stackJSON)
    plans    <- (.plans   ) =. opt HM.empty "plans"    (dict stackJSON)
    targets  <- (.targets ) =. opt HM.empty "targets"  (dict stackJSON)
    parts    <- (.parts   ) =. req          "parts"    stackJSON
    return SongYaml{..}

getPart :: F.FlexPartName -> SongYaml f -> Maybe (Part f)
getPart fpart = HM.lookup fpart . (.getParts) . (.parts)

-- | Returns the start and end of the preview audio in milliseconds.
previewBounds
  :: (F.HasEvents f)
  => SongYaml file
  -> F.Song (f U.Beats) -- Midi used to evaluate midi timestamps and section boundaries
  -> U.Seconds -- Padding added to the project midi to produce the game midi
  -> Bool -- Is the given midi file already padded?
  -> (Int, Int)
previewBounds syaml song padding prepadded = let
  len = F.songLengthMS song
  secsToMS s = floor $ s * 1000
  evalTime t = secsToMS <$> evalPreviewTime True (Just F.getEventsTrack) song padding prepadded t
  evalTime' pt = fromMaybe (error $ "Couldn't evaluate preview bound: " ++ show pt) $ evalTime pt
  defStartTime = max 0 $ case mapMaybe (evalTime . PreviewSection) ["chorus", "chorus_1", "chorus_1a", "verse", "verse_1"] of
    []    -> quot len 2 - 15000
    t : _ -> min (len - 30000) t
  in case (syaml.metadata.previewStart, syaml.metadata.previewEnd) of
    (Nothing, Nothing) -> (defStartTime, defStartTime + 30000)
    (Just ps, Just pe) -> (evalTime' ps, evalTime' pe)
    (Just ps, Nothing) -> let start = evalTime' ps in (start, start + 30000)
    (Nothing, Just pe) -> let end = evalTime' pe in (max 0 $ end - 30000, end)

evalPreviewTime
  :: Bool -- Should midi and section timestamps be pulled back slightly to account for fade-in?
  -> Maybe (f -> EventsTrack U.Beats)
  -> F.Song f
  -> U.Seconds -- Padding added to the project midi to produce the game midi
  -> Bool -- Are the given midi file and events track already padded?
  -> PreviewTime
  -> Maybe U.Seconds
evalPreviewTime leadin getEvents song padding prepadded = \case
  PreviewSeconds secs -> Just $ secs + padding
  PreviewMIDI mb -> Just $ addLeadin
    $ addMIDIPadding
    $ U.applyTempoMap (F.s_tempos song)
    $ U.unapplyMeasureMap (F.s_signatures song) mb
  PreviewSection str -> addLeadin . addMIDIPadding . U.applyTempoMap (F.s_tempos song)
    <$> findSection str
  where addLeadin = if leadin then (NNC.-| 0.6) else id
        addMIDIPadding = if prepadded then id else (+ padding)
        -- TODO make this more reliable in matching section formatting
        findSection sect = getEvents >>= \f ->
          fmap (fst . fst) $ RTB.viewL $ RTB.filter ((== sect) . snd)
            $ eventsSections $ f $ F.s_tracks song
