{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rocksmith.ArrangementXML where

import           Control.Monad                  (void)
import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx.XML
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString                as B
import           Data.Char                      (isSpace)
import           Data.Fixed                     (Milli)
import           Data.Functor.Identity          (Identity)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Text.IO                   as T
import qualified Data.Vector                    as V
import           Data.Version                   (showVersion)
import           Paths_onyxite_customs_lib      (version)
import qualified Sound.MIDI.Util                as U
import           Text.XML.Light

data Arrangement = Arrangement
  { arr_version                :: Int
  , arr_title                  :: T.Text
  , arr_arrangement            :: T.Text
  , arr_part                   :: Int -- what is this?
  , arr_offset                 :: Milli -- time, but can be negative
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
  , arr_phrases                :: V.Vector Phrase
  , arr_phraseIterations       :: V.Vector PhraseIteration
  , arr_chordTemplates         :: V.Vector ChordTemplate
  , arr_ebeats                 :: V.Vector Ebeat
  , arr_sections               :: V.Vector Section
  , arr_events                 :: V.Vector Event
  , arr_transcriptionTrack     :: Level
  , arr_levels                 :: V.Vector Level
  } deriving (Eq, Show)

instance IsInside Arrangement where
  insideCodec = do
    arr_version                <- arr_version                =. intText (reqAttr "version")
    arr_title                  <- arr_title                  =. childTag "title"                  (parseInside' childText)
    arr_arrangement            <- arr_arrangement            =. childTag "arrangement"            (parseInside' childText)
    arr_part                   <- arr_part                   =. childTag "part"                   (parseInside' $ intText childText)
    arr_offset                 <- arr_offset                 =. childTag "offset"                 (parseInside' $ milliText childText)
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
  , ph_disparity     :: Maybe Int
  , ph_ignore        :: Bool
  , ph_solo          :: Bool
  } deriving (Eq, Show)

instance IsInside Phrase where
  insideCodec = do
    ph_maxDifficulty <- ph_maxDifficulty =. intText (reqAttr "maxDifficulty")
    ph_name          <- ph_name          =. reqAttr "name"
    ph_disparity     <- ph_disparity     =. zoomValue (maybeValue intValue) (optAttr "disparity")
    ph_ignore        <- ph_ignore        =. flag "ignore"
    ph_solo          <- ph_solo          =. flag "solo"
    return Phrase{..}

data PhraseIteration = PhraseIteration
  { pi_time       :: U.Seconds
  , pi_phraseId   :: Int
  , pi_variation  :: Maybe T.Text -- only seen empty, dunno what this is
  , pi_heroLevels :: V.Vector HeroLevel
  } deriving (Eq, Show)

instance IsInside PhraseIteration where
  insideCodec = do
    pi_time       <- pi_time       =. seconds (reqAttr "time")
    pi_phraseId   <- pi_phraseId   =. intText (reqAttr "phraseId")
    pi_variation  <- pi_variation  =. optAttr "variation"
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
  , lvl_notes         :: V.Vector Note
  , lvl_chords        :: V.Vector Chord
  , lvl_fretHandMutes :: V.Vector FretHandMute
  , lvl_anchors       :: V.Vector Anchor
  , lvl_handShapes    :: V.Vector HandShape
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
  , n_bend           :: Maybe Milli
  , n_bendValues     :: V.Vector BendValue
  -- TODO did I miss non-pinch harmonics?
  , n_harmonicPinch  :: Bool
  , n_leftHand       :: Maybe Int -- only in chordNote
  , n_tap            :: Bool
  , n_slap           :: Maybe Int
  , n_pluck          :: Maybe Int
  , n_tremolo        :: Bool
  , n_pickDirection  :: Maybe Int
  , n_ignore         :: Bool
  } deriving (Eq, Show)

data Chord = Chord
  { chd_time         :: U.Seconds
  , chd_chordId      :: Int
  , chd_accent       :: Bool
  , chd_highDensity  :: Bool
  , chd_palmMute     :: Bool
  , chd_fretHandMute :: Bool
  , chd_linkNext     :: Bool
  , chd_chordNotes   :: V.Vector Note
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
    n_bend           <- n_bend           =. zoomValue (maybeValue milliValue) (optAttr "bend")
    n_bendValues     <- n_bendValues     =. plural "bendValue"
    n_harmonicPinch  <- n_harmonicPinch  =. flag "harmonicPinch"
    n_leftHand       <- n_leftHand       =. zoomValue (maybeValue intValue) (optAttr "leftHand")
    n_tap            <- n_tap            =. flag "tap"
    n_slap           <- n_slap           =. zoomValue (maybeValue intValue) (optAttr "slap")
    n_pluck          <- n_pluck          =. zoomValue (maybeValue intValue) (optAttr "pluck")
    n_tremolo        <- n_tremolo        =. flag "tremolo"
    n_pickDirection  <- n_pickDirection  =. zoomValue (maybeValue intValue) (optAttr "pickDirection")
    n_ignore         <- n_ignore         =. flag "ignore"
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
  -- seen in CST output: unk5="<number>"
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
  , ap_routeMask         :: Maybe Int
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
    ap_routeMask         <- ap_routeMask         =. zoomValue (maybeValue intValue) (optAttr "routeMask")
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

data Vocal = Vocal
  { voc_time   :: U.Seconds
  -- TODO are any of these optional
  , voc_note   :: Int
  , voc_length :: U.Seconds
  , voc_lyric  :: T.Text
  } deriving (Eq, Show)

instance IsInside Vocal where
  insideCodec = do
    voc_time <- voc_time =. seconds (reqAttr "time")
    voc_note <- voc_note =. intText (reqAttr "note")
    voc_length <- voc_length =. seconds (reqAttr "length")
    voc_lyric <- voc_lyric =. reqAttr "lyric"
    return Vocal{..}

data Vocals = Vocals
  { vocs_vocals :: V.Vector Vocal
  } deriving (Eq, Show)

instance IsInside Vocals where
  insideCodec = do
    vocs_vocals <- vocs_vocals =. countList (isTag "vocal" $ parseInside' insideCodec)
    return Vocals{..}

data PartContents
  = PartArrangement Arrangement
  | PartVocals Vocals
  deriving (Eq, Show)

parseFile :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m PartContents
parseFile f = inside ("Loading: " <> f) $ do
  stackIO (T.readFile f) >>= \t -> case parseXMLDoc t of
    Nothing  -> fatal "Couldn't parse XML"
    Just elt -> mapStackTraceT (`runReaderT` elt) $ if matchName "vocals" $ elName elt
      then fmap PartVocals      $ codecIn $ isTag "vocals" $ parseInside' insideCodec
      else fmap PartArrangement $ codecIn $ isTag "song"   $ parseInside' insideCodec

writePart :: (MonadIO m) => FilePath -> PartContents -> m ()
writePart f pc = liftIO $ do
  let xml = case pc of
        PartArrangement arr -> makeDoc "song" $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) Arrangement) arr
        PartVocals vocs -> makeDoc "vocals" $ void $ codecOut (insideCodec :: InsideCodec (PureLog Identity) Vocals) vocs
      -- this is a hack, could find better way to insert comment
      xmlLines = ppTopElement xml
      xmlLines' = case break (any isSpace . take 1) $ lines xmlLines of
        (before, after) -> before <> ["<!-- Onyx v" <> showVersion version <> " -->"] <> after
  B.writeFile f $ TE.encodeUtf8 $ T.pack $ unlines xmlLines'
