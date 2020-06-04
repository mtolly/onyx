{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.Base where

import           Control.Arrow                  (second)
import           Control.Monad                  (forM, forM_, guard, unless,
                                                 void, when)
import           Control.Monad.Codec
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Char                      (isSpace)
import           Data.Fixed                     (Milli)
import           Data.Foldable                  (toList)
import           Data.Maybe                     (catMaybes, fromMaybe, isJust,
                                                 isNothing, mapMaybe)
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

childTagOpt :: (Monad m) => T.Text -> ValueCodec m Inside a -> InsideCodec m (Maybe a)
childTagOpt t cdc = Codec
  { codecIn = do
    ins <- lift get
    let isElement = \case Elem e -> Just e; _ -> Nothing
    case break (isJust . snd) [ (x, isElement x >>= matchTag t) | x <- insideContent ins ] of
      (before, (Elem elt, Just layer) : after) -> do
        let ins' = Inside (elAttribs elt) (elContent elt)
        x <- layer $ mapStackTraceT (lift . (`runReaderT` ins')) $ codecIn cdc
        lift $ put $ ins { insideContent = map fst $ before ++ after }
        return $ Just x
      _ -> return Nothing
  , codecOut = fmapArg $ \case
    Nothing -> return ()
    Just x -> let
      makeChild ins = Inside
        { insideAttrs = []
        , insideContent = [Elem $ makeTag t ins]
        }
      in void $ mapWriter (second makeChild) $ codecOut cdc x
  }

childTag :: (Monad m) => T.Text -> ValueCodec m Inside a -> InsideCodec m a
childTag t cdc = let
  o = childTagOpt t cdc
  in Codec
    { codecIn = codecIn o >>= \case
      Just x  -> return x
      Nothing -> fatal $ "Missing required child tag <" <> T.unpack t <> ">"
    , codecOut = fmapArg $ void . codecOut o . Just
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

optAttr :: (Monad m) => T.Text -> InsideCodec m (Maybe T.Text)
optAttr a = Codec
  { codecIn = do
    ins <- lift get
    case break (\attr -> attrKey attr == QName (T.unpack a) Nothing Nothing) $ insideAttrs ins of
      (before, attr : after) -> do
        lift $ put $ ins { insideAttrs = before ++ after }
        return $ Just $ T.pack $ attrVal attr
      _ -> return Nothing
  , codecOut = makeOut $ \case
    Nothing -> mempty
    Just x -> Inside
      { insideAttrs = [Attr (QName (T.unpack a) Nothing Nothing) (T.unpack x)]
      , insideContent = []
      }
  }

reqAttr :: (Monad m) => T.Text -> InsideCodec m T.Text
reqAttr a = Codec
  { codecIn = codecIn o >>= \case
    Just x  -> return x
    Nothing -> fatal $ "Missing required attribute " <> show a
  , codecOut = fmapArg $ void . codecOut o . Just
  } where o = optAttr a

childElements :: (SendMessage m) => InsideCodec m [Element]
childElements = Codec
  { codecIn = do
    ins <- lift get
    elts <- fmap catMaybes $ forM (insideContent ins) $ \case
      Elem elt -> return $ Just elt
      Text cd -> do
        unless (all isSpace $ cdData cd) $ do
          warn $ "Unexpected text in list element: " ++ show (cdData cd)
        return Nothing
      CRef _ -> warn "Unexpected CRef" >> return Nothing
    lift $ put ins{ insideContent = [] }
    return elts
  , codecOut = makeOut $ \elts -> Inside
    { insideAttrs   = []
    , insideContent = map Elem elts
    }
  }

bareList :: (SendMessage m) => ValueCodec m Element a -> InsideCodec m [a]
bareList cdc = Codec
  { codecIn = do
    elts <- codecIn children
    forM (zip [0..] elts) $ \(i, e) -> do
      let layer = unwords $ concat
            [ toList (elLine e) >>= \ln -> ["line", show ln]
            , ["list child #" <> show (i :: Int)]
            ]
      inside layer $ mapStackTraceT (lift . (`runReaderT` e)) $ codecIn cdc
  , codecOut = fmapArg $ void . codecOut children . map (makeValue' cdc)
  } where children = childElements

countList :: (SendMessage m) => ValueCodec m Element a -> InsideCodec m [a]
countList cdc = Codec
  { codecIn = do
    len <- codecIn $ intText $ reqAttr "count"
    xs <- codecIn bare
    when (length xs /= len) $ warn $ unwords
      [ "List has count attribute of"
      , show len
      , "but contains"
      , show $ length xs
      , "children"
      ]
    return xs
  , codecOut = fmapArg $ \xs -> do
    tell Inside
      { insideAttrs = [Attr (QName "count" Nothing Nothing) (show $ length xs)]
      , insideContent = []
      }
    void $ codecOut bare xs
  } where bare = bareList cdc

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

zoomValue :: (Monad m) => ValueCodec m v a -> InsideCodec m v -> InsideCodec m a
zoomValue z cdc = Codec
  { codecIn = codecIn cdc >>= \x -> mapStackTraceT (lift . (`runReaderT` x)) $ codecIn z
  , codecOut = fmapArg $ void . codecOut cdc . execWriter . codecOut z
  }

intValue :: (Monad m, Integral a) => ValueCodec m T.Text a
intValue = Codec
  { codecIn = lift ask >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected an integer, got: " <> show str
    Just i  -> return $ fromInteger i
  , codecOut = fmapArg $ tell . T.pack . show . toInteger
  }

intText :: (Monad m, Integral a) => InsideCodec m T.Text -> InsideCodec m a
intText = zoomValue intValue

maybeValue :: (Monad m) => ValueCodec m v a -> ValueCodec m (Maybe v) (Maybe a)
maybeValue cdc = Codec
  { codecIn = lift ask >>= \case
    Nothing -> return Nothing
    Just v  -> fmap Just $ parseFrom v $ codecIn cdc
  , codecOut = fmapArg $ \case
    Nothing -> tell Nothing
    Just a  -> void $ mapWriter (second Just) $ codecOut cdc a
  }

maybeInside :: (Monad m) => InsideCodec m a -> InsideCodec m (Maybe a)
maybeInside cdc = Codec
  { codecIn = lift get >>= \case
    Inside [] [] -> return Nothing
    _            -> fmap Just $ codecIn cdc
  , codecOut = fmapArg $ maybe (return ()) (void . codecOut cdc)
  }

boolValue :: (Monad m) => ValueCodec m T.Text Bool
boolValue = Codec
  { codecIn = lift ask >>= \case
    "0" -> return False
    "1" -> return True
    str -> fatal $ "Expected 0 or 1 (bool), got: " <> show str
  , codecOut = fmapArg $ tell . \b -> if b then "1" else "0"
  }

boolText :: (Monad m) => InsideCodec m T.Text -> InsideCodec m Bool
boolText = zoomValue boolValue

milliValue :: (Monad m, RealFrac a) => ValueCodec m T.Text a
milliValue = Codec
  { codecIn = lift ask >>= \str -> case readMaybe $ T.unpack str of
    Nothing -> fatal $ "Expected a number to 3 decimal places, got: " <> show str
    Just m  -> return $ realToFrac (m :: Milli)
  , codecOut = fmapArg $ tell . T.pack . show . \n -> realToFrac n :: Milli
  }

milliText :: (Monad m, RealFrac a) => InsideCodec m T.Text -> InsideCodec m a
milliText = zoomValue milliValue

secondsValue :: (Monad m) => ValueCodec m T.Text U.Seconds
secondsValue = dimap U.fromSeconds U.Seconds milliValue

seconds :: (Monad m) => InsideCodec m T.Text -> InsideCodec m U.Seconds
seconds = zoomValue secondsValue

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
  , arr_albumYear              :: Maybe Int -- should verify
  , arr_crowdSpeed             :: Int
  , arr_arrangementProperties  :: ArrangementProperties
  , arr_phrases                :: [Phrase]
  , arr_phraseIterations       :: [PhraseIteration]
  , arr_chordTemplates         :: [ChordTemplate]
  , arr_ebeats                 :: [Ebeat]
  , arr_sections               :: [Section]
  , arr_events                 :: [Event]
  , arr_transcriptionTrack     :: Level
  , arr_levels                 :: [Level]
  } deriving (Eq, Show)

plural' :: (SendMessage m, IsInside a) => T.Text -> T.Text -> InsideCodec m [a]
plural' many one = dimap (\xs -> guard (not $ null xs) >> Just xs) (fromMaybe [])
  $ childTagOpt many $ parseInside' $ countList $ isTag one $ parseInside' insideCodec

plural :: (SendMessage m, IsInside a) => T.Text -> InsideCodec m [a]
plural one = plural' (one <> "s") one

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
    arr_albumYear              <- arr_albumYear              =. childTag "albumYear"              (parseInside' $ maybeInside $ intText childText)
    arr_crowdSpeed             <- arr_crowdSpeed             =. childTag "crowdSpeed"             (parseInside' $ intText childText)
    arr_arrangementProperties  <- arr_arrangementProperties  =. childTag "arrangementProperties"  (parseInside' insideCodec)
    arr_phrases                <- arr_phrases                =. plural "phrase"
    arr_phraseIterations       <- arr_phraseIterations       =. plural "phraseIteration"
    arr_chordTemplates         <- arr_chordTemplates         =. plural "chordTemplate"
    arr_ebeats                 <- arr_ebeats                 =. plural "ebeat"
    arr_sections               <- arr_sections               =. plural "section"
    arr_events                 <- arr_events                 =. plural "event"
    arr_transcriptionTrack     <- arr_transcriptionTrack     =. childTag "transcriptionTrack"     (parseInside' insideCodec)
    arr_levels                 <- arr_levels                 =. plural "level"
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

data PhraseIteration = PhraseIteration
  { pi_time       :: U.Seconds
  , pi_phraseId   :: Int
  , pi_variation  :: T.Text -- only seen empty, dunno what this is
  , pi_heroLevels :: [HeroLevel]
  } deriving (Eq, Show)

instance IsInside PhraseIteration where
  insideCodec = do
    pi_time       <- pi_time       =. seconds (reqAttr "time")
    pi_phraseId   <- pi_phraseId   =. intText (reqAttr "phraseId")
    pi_variation  <- pi_variation  =. reqAttr "variation"
    pi_heroLevels <- pi_heroLevels =. plural "heroLevel"
    return PhraseIteration{..}

data HeroLevel = HeroLevel
  { hl_difficulty :: Int
  , hl_hero       :: Int
  } deriving (Eq, Show)

instance IsInside HeroLevel where
  insideCodec = do
    hl_difficulty <- hl_difficulty =. intText (reqAttr "difficulty")
    hl_hero       <- hl_hero       =. intText (reqAttr "hero")
    return HeroLevel{..}

data ChordTemplate = ChordTemplate
  { ct_chordName   :: T.Text
  , ct_displayName :: T.Text
  -- fingers are 1,2,3,4 = I,M,R,P I think
  , ct_finger0     :: Maybe Int
  , ct_finger1     :: Maybe Int
  , ct_finger2     :: Maybe Int
  , ct_finger3     :: Maybe Int
  , ct_finger4     :: Maybe Int
  , ct_finger5     :: Maybe Int
  , ct_fret0       :: Maybe Int
  , ct_fret1       :: Maybe Int
  , ct_fret2       :: Maybe Int
  , ct_fret3       :: Maybe Int
  , ct_fret4       :: Maybe Int
  , ct_fret5       :: Maybe Int
  } deriving (Eq, Show)

instance IsInside ChordTemplate where
  insideCodec = do
    ct_chordName   <- ct_chordName   =. reqAttr "chordName"
    ct_displayName <- ct_displayName =. reqAttr "displayName"
    ct_finger0     <- ct_finger0     =. zoomValue (maybeValue intValue) (optAttr "finger0")
    ct_finger1     <- ct_finger1     =. zoomValue (maybeValue intValue) (optAttr "finger1")
    ct_finger2     <- ct_finger2     =. zoomValue (maybeValue intValue) (optAttr "finger2")
    ct_finger3     <- ct_finger3     =. zoomValue (maybeValue intValue) (optAttr "finger3")
    ct_finger4     <- ct_finger4     =. zoomValue (maybeValue intValue) (optAttr "finger4")
    ct_finger5     <- ct_finger5     =. zoomValue (maybeValue intValue) (optAttr "finger5")
    ct_fret0       <- ct_fret0       =. zoomValue (maybeValue intValue) (optAttr "fret0")
    ct_fret1       <- ct_fret1       =. zoomValue (maybeValue intValue) (optAttr "fret1")
    ct_fret2       <- ct_fret2       =. zoomValue (maybeValue intValue) (optAttr "fret2")
    ct_fret3       <- ct_fret3       =. zoomValue (maybeValue intValue) (optAttr "fret3")
    ct_fret4       <- ct_fret4       =. zoomValue (maybeValue intValue) (optAttr "fret4")
    ct_fret5       <- ct_fret5       =. zoomValue (maybeValue intValue) (optAttr "fret5")
    return ChordTemplate{..}

data Ebeat = Ebeat
  { eb_time    :: U.Seconds
  , eb_measure :: Maybe Int
  } deriving (Eq, Show)

instance IsInside Ebeat where
  insideCodec = do
    eb_time    <- eb_time    =. seconds (reqAttr "time")
    eb_measure <- eb_measure =. zoomValue (maybeValue intValue) (optAttr "measure")
    return Ebeat{..}

data Section = Section
  { sect_name      :: T.Text
  , sect_number    :: Int
  , sect_startTime :: U.Seconds
  } deriving (Eq, Show)

instance IsInside Section where
  insideCodec = do
    sect_name      <- sect_name      =. reqAttr "name"
    sect_number    <- sect_number    =. intText (reqAttr "number")
    sect_startTime <- sect_startTime =. seconds (reqAttr "startTime")
    return Section{..}

data Level = Level
  { lvl_difficulty    :: Int
  , lvl_notes         :: [Note]
  , lvl_chords        :: [Chord]
  , lvl_fretHandMutes :: [FretHandMute]
  , lvl_anchors       :: [Anchor]
  , lvl_handShapes    :: [HandShape]
  } deriving (Eq, Show)

instance IsInside Level where
  insideCodec = do
    lvl_difficulty    <- lvl_difficulty    =. intText (reqAttr "difficulty")
    lvl_notes         <- lvl_notes         =. plural "note"
    lvl_chords        <- lvl_chords        =. plural "chord"
    lvl_fretHandMutes <- lvl_fretHandMutes =. plural "fretHandMute"
    lvl_anchors       <- lvl_anchors       =. plural "anchor"
    lvl_handShapes    <- lvl_handShapes    =. plural "handShape"
    return Level{..}

data Note = Note
  { n_time           :: U.Seconds
  , n_string         :: Int
  , n_fret           :: Int
  , n_sustain        :: Maybe U.Seconds
  , n_vibrato        :: Maybe Int -- don't know what format this is
  , n_hopo           :: Bool
  , n_hammerOn       :: Bool
  , n_pullOff        :: Bool
  , n_slideTo        :: Maybe Int
  , n_slideUnpitchTo :: Maybe Int
  , n_mute           :: Bool
  , n_palmMute       :: Bool
  , n_accent         :: Bool
  , n_linkNext       :: Bool
  , n_bend           :: Maybe Int
  , n_bendValues     :: [BendValue]
  , n_harmonicPinch  :: Bool
  , n_leftHand       :: Maybe Int -- only in chordNote
  } deriving (Eq, Show)

data Chord = Chord
  { chd_time         :: U.Seconds
  , chd_chordId      :: Int
  , chd_accent       :: Bool
  , chd_highDensity  :: Bool
  , chd_palmMute     :: Bool
  , chd_fretHandMute :: Bool
  , chd_linkNext     :: Bool
  , chd_chordNotes   :: [Note]
  } deriving (Eq, Show)

-- TODO
data FretHandMute = FretHandMute
  deriving (Eq, Show)

data Anchor = Anchor
  { an_time  :: U.Seconds
  , an_fret  :: Int
  , an_width :: Milli
  } deriving (Eq, Show)

data HandShape = HandShape
  { hs_chordId   :: Int
  , hs_startTime :: U.Seconds
  , hs_endTime   :: U.Seconds
  } deriving (Eq, Show)

flag :: (Monad m) => T.Text -> InsideCodec m Bool
flag s = zoomValue flagValue $ optAttr s

flagValue :: (Monad m) => ValueCodec m (Maybe T.Text) Bool
flagValue = dimap (\b -> guard b >> Just b) (fromMaybe False) $ maybeValue boolValue

instance IsInside Note where
  insideCodec = do
    n_time           <- n_time           =. seconds (reqAttr "time")
    n_string         <- n_string         =. intText (reqAttr "string")
    n_fret           <- n_fret           =. intText (reqAttr "fret")
    n_sustain        <- n_sustain        =. zoomValue (maybeValue secondsValue) (optAttr "sustain")
    n_vibrato        <- n_vibrato        =. zoomValue (maybeValue intValue) (optAttr "vibrato")
    n_hopo           <- n_hopo           =. flag "hopo"
    n_hammerOn       <- n_hammerOn       =. flag "hammerOn"
    n_pullOff        <- n_pullOff        =. flag "pullOff"
    n_slideTo        <- n_slideTo        =. zoomValue (maybeValue intValue) (optAttr "slideTo")
    n_slideUnpitchTo <- n_slideUnpitchTo =. zoomValue (maybeValue intValue) (optAttr "slideUnpitchTo")
    n_mute           <- n_mute           =. flag "mute"
    n_palmMute       <- n_palmMute       =. flag "palmMute"
    n_accent         <- n_accent         =. flag "accent"
    n_linkNext       <- n_linkNext       =. flag "linkNext"
    n_bend           <- n_bend           =. zoomValue (maybeValue intValue) (optAttr "bend")
    n_bendValues     <- n_bendValues     =. plural "bendValue"
    n_harmonicPinch  <- n_harmonicPinch  =. flag "harmonicPinch"
    n_leftHand       <- n_leftHand       =. zoomValue (maybeValue intValue) (optAttr "leftHand")
    return Note{..}

instance IsInside Chord where
  insideCodec = do
    chd_time         <- chd_time         =. seconds (reqAttr "time")
    chd_chordId      <- chd_chordId      =. intText (reqAttr "chordId")
    chd_accent       <- chd_accent       =. flag "accent"
    chd_highDensity  <- chd_highDensity  =. flag "highDensity"
    chd_palmMute     <- chd_palmMute     =. flag "palmMute"
    chd_fretHandMute <- chd_fretHandMute =. flag "fretHandMute"
    chd_linkNext     <- chd_linkNext     =. flag "linkNext"
    chd_chordNotes   <- chd_chordNotes   =. bareList (isTag "chordNote" $ parseInside' insideCodec)
    return Chord{..}

-- TODO
instance IsInside FretHandMute where
  insideCodec = return FretHandMute{}

instance IsInside Anchor where
  insideCodec = do
    an_time  <- an_time  =. seconds (reqAttr "time")
    an_fret  <- an_fret  =. intText (reqAttr "fret")
    an_width <- an_width =. milliText (reqAttr "width")
    return Anchor{..}

instance IsInside HandShape where
  insideCodec = do
    hs_chordId   <- hs_chordId   =. intText (reqAttr "chordId")
    hs_startTime <- hs_startTime =. seconds (reqAttr "startTime")
    hs_endTime   <- hs_endTime   =. seconds (reqAttr "endTime")
    return HandShape{..}

data BendValue = BendValue
  { bv_time :: U.Seconds
  , bv_step :: Maybe Milli
  } deriving (Eq, Show)

instance IsInside BendValue where
  insideCodec = do
    bv_time <- bv_time =. seconds (reqAttr "time")
    bv_step <- bv_step =. zoomValue (maybeValue milliValue) (optAttr "step")
    return BendValue{..}

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

data Event = Event
  { ev_time :: U.Seconds
  , ev_code :: T.Text
  } deriving (Eq, Show)

instance IsInside Event where
  insideCodec = do
    ev_time <- ev_time =. seconds (reqAttr "time")
    ev_code <- ev_code =. reqAttr "code"
    return Event{..}

data PartContents
  = PartArrangement Arrangement
  | PartVocals -- TODO parse the vocals
  deriving (Eq, Show)

parseFile :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m PartContents
parseFile f = inside ("Loading: " <> f) $ do
  stackIO (T.readFile f) >>= \t -> case parseXMLDoc t of
    Nothing  -> fatal "Couldn't parse XML"
    Just elt -> mapStackTraceT (`runReaderT` elt) $ case matchTag "vocals" elt of
      Just _  -> return PartVocals
      Nothing -> fmap PartArrangement $ codecIn $ isTag "song" $ parseInside' insideCodec
