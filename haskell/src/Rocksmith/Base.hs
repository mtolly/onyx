{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.Base where

import           Control.Arrow                  (second)
import           Control.Monad                  (forM_, guard, unless, void)
import           Control.Monad.Codec
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                      (isSpace)
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.Maybe                     (isJust, isNothing, mapMaybe)
import           Data.Profunctor                (dimap)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import           JSONData
import qualified Sound.MIDI.Util                as U
import           Text.Read                      (readMaybe)
import           Text.XML.Light

data Inside = Inside
  { insideAttrs   :: [Attr]
  , insideContent :: [Content]
  }

instance Semigroup Inside where
  Inside a b <> Inside c d = Inside (a ++ c) (b ++ d)

instance Monoid Inside where
  mempty = Inside [] []

type InsideParser m = StackTraceT (StateT Inside m)
type InsideBuilder = Writer Inside
type InsideCodec m a = Codec (InsideParser m) InsideBuilder a

class IsInside a where
  insideCodec :: (SendMessage m) => InsideCodec m a

matchTag :: T.Text -> Element -> Maybe (StackTraceT m a -> StackTraceT m a)
matchTag t elt = do
  guard $ elName elt == QName (T.unpack t) Nothing Nothing
  return $ inside $ unwords $ concat
    [ toList (elLine elt) >>= \ln -> ["line", show ln]
    , ["tag <" <> T.unpack t <> ">"]
    ]

makeTag :: T.Text -> Inside -> Element
makeTag t ins = Element
  { elName = QName (T.unpack t) Nothing Nothing
  , elAttribs = insideAttrs ins
  , elContent = insideContent ins
  , elLine = Nothing
  }

isTag :: (Monad m) => T.Text -> ValueCodec m Inside a -> ValueCodec m Element a
isTag t cdc = Codec
  { codecIn = do
    elt <- lift ask
    case matchTag t elt of
      Nothing -> fatal $ "Expected a tag <" <> T.unpack t <> "> but got: " <> show (elName elt)
      Just layer -> layer $ parseFrom (Inside (elAttribs elt) (elContent elt)) $ codecIn cdc
  , codecOut = mapWriter (second $ makeTag t) . codecOut cdc
  }

childTag :: (Monad m) => T.Text -> ValueCodec m Inside a -> InsideCodec m a
childTag t cdc = Codec
  { codecIn = do
    ins <- lift get
    let isElement = \case Elem e -> Just e; _ -> Nothing
    case break (isJust . snd) [ (x, isElement x >>= matchTag t) | x <- insideContent ins ] of
      (before, (Elem elt, Just layer) : after) -> do
        let ins' = Inside (elAttribs elt) (elContent elt)
        x <- layer $ mapStackTraceT (lift . (`runReaderT` ins')) $ codecIn cdc
        lift $ put $ ins { insideContent = map fst $ before ++ after }
        return x
      _ -> fatal $ "Missing required child tag <" <> T.unpack t <> ">"
  , codecOut = let
    makeChild ins = Inside
      { insideAttrs = []
      , insideContent = [Elem $ makeTag t ins]
      }
    in mapWriter (second makeChild) . codecOut cdc
  }

parseInside :: (Monad m) => InsideCodec m a -> ValueCodec m Inside a
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

parseInside' :: (SendMessage m) => InsideCodec m a -> ValueCodec m Inside a
parseInside' = parseInside . warnUnused

reqAttr :: (Monad m) => T.Text -> InsideCodec m T.Text
reqAttr a = Codec
  { codecIn = do
    ins <- lift get
    case break (\attr -> attrKey attr == QName (T.unpack a) Nothing Nothing) $ insideAttrs ins of
      (before, attr : after) -> do
        lift $ put $ ins { insideAttrs = before ++ after }
        return $ T.pack $ attrVal attr
      _ -> fatal $ "Missing required attribute " <> show a
  , codecOut = makeOut $ \x -> Inside
    { insideAttrs = [Attr (QName (T.unpack a) Nothing Nothing) (T.unpack x)]
    , insideContent = []
    }
  }

countList :: (SendMessage m) => ValueCodec m Element a -> InsideCodec m [a]
countList cdc = Codec
  { codecIn = do
    len <- codecIn $ intText $ reqAttr "count"
    undefined len
  , codecOut = let
    makeList elts = Inside
      { insideAttrs = [Attr (QName "count" Nothing Nothing) (show $ length elts)]
      , insideContent = map Elem elts
      }
    in makeOut $ makeList . map (makeValue' cdc)
  }

childText :: (Monad m) => InsideCodec m T.Text
childText = Codec
  { codecIn = do
    ins <- lift get
    let getText = \case Text cd -> Just cd; _ -> Nothing
    lift $ put $ ins { insideContent = filter (isNothing . getText) $ insideContent ins }
    return
      $ T.unwords
      $ filter (not . T.null)
      $ map (T.strip . T.pack . cdData)
      $ mapMaybe getText
      $ insideContent ins
  , codecOut = makeOut $ \t -> Inside
    { insideAttrs = []
    , insideContent = [Text $ CData CDataText (T.unpack t) Nothing | not $ T.null t]
    }
  }

intText :: (Monad m, Integral a) => InsideCodec m T.Text -> InsideCodec m a
intText cdc = Codec
  { codecIn = codecIn cdc >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just i  -> return $ fromInteger i
  , codecOut = \i -> do
    void $ codecOut cdc $ T.pack $ show $ toInteger i
    return i
  }

boolText :: (Monad m) => InsideCodec m T.Text -> InsideCodec m Bool
boolText cdc = Codec
  { codecIn = codecIn cdc >>= \case
    "0" -> return False
    "1" -> return True
    str -> fatal $ "Expected 0 or 1 (bool), got: " <> show str
  , codecOut = \b -> do
    void $ codecOut cdc $ if b then "1" else "0"
    return b
  }

milliText :: (Monad m, RealFrac a) => InsideCodec m T.Text -> InsideCodec m a
milliText cdc = Codec
  { codecIn = codecIn cdc >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just m  -> return $ realToFrac (m :: Milli)
  , codecOut = \n -> do
    void $ codecOut cdc $ T.pack $ show (realToFrac n :: Milli)
    return n
  }

seconds :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.Seconds
seconds = dimap U.fromSeconds U.Seconds . milliText

bpm :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.BPS
bpm = dimap ((* 60) . U.fromBPS) (U.BPS . (/ 60)) . milliText

data Arrangement = Arrangement
  { arr_version                :: Int
  , arr_title                  :: T.Text
  , arr_arrangement            :: T.Text
  , arr_part                   :: Int -- what is this?
  , arr_offset                 :: U.Seconds
  , arr_centOffset             :: Int
  , arr_songLength             :: U.Seconds
  , arr_lastConversionDateTime :: T.Text -- whatever
  , arr_startBeat              :: U.Seconds
  , arr_averageTempo           :: U.BPS
  , arr_tuning                 :: Tuning
  , arr_capo                   :: Int
  , arr_artistName             :: T.Text
  , arr_artistNameSort         :: T.Text
  , arr_albumName              :: T.Text
  , arr_albumYear              :: Int -- should verify
  , arr_crowdSpeed             :: Int
  , arr_arrangementProperties  :: ArrangementProperties
  , arr_phrases                :: [Phrase]
  } deriving (Eq, Show)

instance IsInside Arrangement where
  insideCodec = do
    arr_version                <- arr_version                =. intText (reqAttr "version")
    arr_title                  <- arr_title                  =. childTag "title"                  (parseInside' childText)
    arr_arrangement            <- arr_arrangement            =. childTag "arrangement"            (parseInside' childText)
    arr_part                   <- arr_part                   =. childTag "part"                   (parseInside' $ intText childText)
    arr_offset                 <- arr_offset                 =. childTag "offset"                 (parseInside' $ seconds childText)
    arr_centOffset             <- arr_centOffset             =. childTag "centOffset"             (parseInside' $ intText childText)
    arr_songLength             <- arr_songLength             =. childTag "songLength"             (parseInside' $ seconds childText)
    arr_lastConversionDateTime <- arr_lastConversionDateTime =. childTag "lastConversionDateTime" (parseInside' childText)
    arr_startBeat              <- arr_startBeat              =. childTag "startBeat"              (parseInside' $ seconds childText)
    arr_averageTempo           <- arr_averageTempo           =. childTag "averageTempo"           (parseInside' $ bpm childText)
    arr_tuning                 <- arr_tuning                 =. childTag "tuning"                 (parseInside' insideCodec)
    arr_capo                   <- arr_capo                   =. childTag "capo"                   (parseInside' $ intText childText)
    arr_artistName             <- arr_artistName             =. childTag "artistName"             (parseInside' childText)
    arr_artistNameSort         <- arr_artistNameSort         =. childTag "artistNameSort"         (parseInside' childText)
    arr_albumName              <- arr_albumName              =. childTag "albumName"              (parseInside' childText)
    arr_albumYear              <- arr_albumYear              =. childTag "albumYear"              (parseInside' $ intText childText)
    arr_crowdSpeed             <- arr_crowdSpeed             =. childTag "crowdSpeed"             (parseInside' $ intText childText)
    arr_arrangementProperties  <- arr_arrangementProperties  =. childTag "arrangementProperties"  (parseInside' insideCodec)
    arr_phrases                <- arr_phrases                =. childTag "phrases"
      (parseInside' $ countList $ isTag "phrase" $ parseInside' insideCodec)
    return Arrangement{..}

data Phrase = Phrase
  { ph_maxDifficulty :: Int
  , ph_name          :: T.Text
  } deriving (Eq, Show)

instance IsInside Phrase where
  insideCodec = do
    ph_maxDifficulty <- ph_maxDifficulty =. intText (reqAttr "maxDifficulty")
    ph_name          <- ph_name          =. reqAttr "name"
    return Phrase{..}

data ArrangementProperties = ArrangementProperties
  { ap_represent         :: Bool
  , ap_bonusArr          :: Bool
  , ap_standardTuning    :: Bool
  , ap_nonStandardChords :: Bool
  , ap_barreChords       :: Bool
  , ap_powerChords       :: Bool
  , ap_dropDPower        :: Bool
  , ap_openChords        :: Bool
  , ap_fingerPicking     :: Bool
  , ap_pickDirection     :: Bool
  , ap_doubleStops       :: Bool
  , ap_palmMutes         :: Bool
  , ap_harmonics         :: Bool
  , ap_pinchHarmonics    :: Bool
  , ap_hopo              :: Bool
  , ap_tremolo           :: Bool
  , ap_slides            :: Bool
  , ap_unpitchedSlides   :: Bool
  , ap_bends             :: Bool
  , ap_tapping           :: Bool
  , ap_vibrato           :: Bool
  , ap_fretHandMutes     :: Bool
  , ap_slapPop           :: Bool
  , ap_twoFingerPicking  :: Bool
  , ap_fifthsAndOctaves  :: Bool
  , ap_syncopation       :: Bool
  , ap_bassPick          :: Bool
  , ap_sustain           :: Bool
  , ap_pathLead          :: Bool
  , ap_pathRhythm        :: Bool
  , ap_pathBass          :: Bool
  } deriving (Eq, Show)

instance IsInside ArrangementProperties where
  insideCodec = do
    ap_represent         <- ap_represent         =. boolText (reqAttr "represent")
    ap_bonusArr          <- ap_bonusArr          =. boolText (reqAttr "bonusArr")
    ap_standardTuning    <- ap_standardTuning    =. boolText (reqAttr "standardTuning")
    ap_nonStandardChords <- ap_nonStandardChords =. boolText (reqAttr "nonStandardChords")
    ap_barreChords       <- ap_barreChords       =. boolText (reqAttr "barreChords")
    ap_powerChords       <- ap_powerChords       =. boolText (reqAttr "powerChords")
    ap_dropDPower        <- ap_dropDPower        =. boolText (reqAttr "dropDPower")
    ap_openChords        <- ap_openChords        =. boolText (reqAttr "openChords")
    ap_fingerPicking     <- ap_fingerPicking     =. boolText (reqAttr "fingerPicking")
    ap_pickDirection     <- ap_pickDirection     =. boolText (reqAttr "pickDirection")
    ap_doubleStops       <- ap_doubleStops       =. boolText (reqAttr "doubleStops")
    ap_palmMutes         <- ap_palmMutes         =. boolText (reqAttr "palmMutes")
    ap_harmonics         <- ap_harmonics         =. boolText (reqAttr "harmonics")
    ap_pinchHarmonics    <- ap_pinchHarmonics    =. boolText (reqAttr "pinchHarmonics")
    ap_hopo              <- ap_hopo              =. boolText (reqAttr "hopo")
    ap_tremolo           <- ap_tremolo           =. boolText (reqAttr "tremolo")
    ap_slides            <- ap_slides            =. boolText (reqAttr "slides")
    ap_unpitchedSlides   <- ap_unpitchedSlides   =. boolText (reqAttr "unpitchedSlides")
    ap_bends             <- ap_bends             =. boolText (reqAttr "bends")
    ap_tapping           <- ap_tapping           =. boolText (reqAttr "tapping")
    ap_vibrato           <- ap_vibrato           =. boolText (reqAttr "vibrato")
    ap_fretHandMutes     <- ap_fretHandMutes     =. boolText (reqAttr "fretHandMutes")
    ap_slapPop           <- ap_slapPop           =. boolText (reqAttr "slapPop")
    ap_twoFingerPicking  <- ap_twoFingerPicking  =. boolText (reqAttr "twoFingerPicking")
    ap_fifthsAndOctaves  <- ap_fifthsAndOctaves  =. boolText (reqAttr "fifthsAndOctaves")
    ap_syncopation       <- ap_syncopation       =. boolText (reqAttr "syncopation")
    ap_bassPick          <- ap_bassPick          =. boolText (reqAttr "bassPick")
    ap_sustain           <- ap_sustain           =. boolText (reqAttr "sustain")
    ap_pathLead          <- ap_pathLead          =. boolText (reqAttr "pathLead")
    ap_pathRhythm        <- ap_pathRhythm        =. boolText (reqAttr "pathRhythm")
    ap_pathBass          <- ap_pathBass          =. boolText (reqAttr "pathBass")
    return ArrangementProperties{..}

data Tuning = Tuning
  { tuning_string0 :: Int
  , tuning_string1 :: Int
  , tuning_string2 :: Int
  , tuning_string3 :: Int
  , tuning_string4 :: Int
  , tuning_string5 :: Int
  } deriving (Eq, Show)

instance IsInside Tuning where
  insideCodec = do
    tuning_string0 <- tuning_string0 =. intText (reqAttr "string0")
    tuning_string1 <- tuning_string1 =. intText (reqAttr "string1")
    tuning_string2 <- tuning_string2 =. intText (reqAttr "string2")
    tuning_string3 <- tuning_string3 =. intText (reqAttr "string3")
    tuning_string4 <- tuning_string4 =. intText (reqAttr "string4")
    tuning_string5 <- tuning_string5 =. intText (reqAttr "string5")
    return Tuning{..}

testParse :: FilePath -> IO Arrangement
testParse f = do
  let cdc = isTag "song" $ parseInside' insideCodec
  T.readFile f >>= \t -> case parseXMLDoc t of
    Nothing -> error "couldn't parse xml"
    Just elt -> logStdout (mapStackTraceT (`runReaderT` elt) $ codecIn cdc) >>= \case
      Left msgs -> error $ show msgs
      Right arr -> return arr
