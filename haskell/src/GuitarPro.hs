{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module GuitarPro where

import qualified Codec.Archive.Zip              as Zip
import           Control.Monad                  (forM, guard, void)
import           Control.Monad.Codec
import           Control.Monad.Codec.Onyx.XML
import           Control.Monad.IO.Class         (MonadIO (..))
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.StackTrace
import           Control.Monad.Trans.State      (get, put)
import           Control.Monad.Trans.Writer
import           Data.Functor.Identity          (Identity)
import           Data.Maybe                     (isJust)
import           Data.Profunctor                (dimap)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import qualified Data.Vector                    as V
import           Text.Read                      (readMaybe)
import           Text.XML.Light

-- .gp (GP7) format, a zip file containing an XML file

data GPIF = GPIF
  { gp_GPVersion   :: Int
  -- GPRevision
  -- Encoding
  , gp_Score       :: Score
  , gp_MasterTrack :: MasterTrack
  -- AudioTracks
  , gp_Tracks      :: V.Vector Track
  , gp_MasterBars  :: V.Vector MasterBar
  , gp_Bars        :: V.Vector Bar
  , gp_Voices      :: V.Vector Voice
  , gp_Beats       :: V.Vector Beat
  , gp_Notes       :: V.Vector Note
  , gp_Rhythms     :: V.Vector Rhythm
  } deriving (Show)

instance IsInside GPIF where
  insideCodec = do
    gp_GPVersion   <- gp_GPVersion =. childTag "GPVersion" (parseInside' $ intText childText)
    ignoreChildTag "GPRevision"
    ignoreChildTag "Encoding"
    gp_Score       <- gp_Score =. childTag "Score" (parseInside' insideCodec)
    gp_MasterTrack <- gp_MasterTrack =. childTag "MasterTrack" (parseInside' insideCodec)
    ignoreChildTag "AudioTracks"
    gp_Tracks      <- gp_Tracks =. childTag "Tracks"
      (parseInside' $ bareList $ isTag "Track" $ parseInside' insideCodec)
    gp_MasterBars  <- gp_MasterBars =. childTag "MasterBars"
      (parseInside' $ bareList $ isTag "MasterBar" $ parseInside' insideCodec)
    gp_Bars        <- gp_Bars       =. childTag "Bars"
      (parseInside' $ bareList $ isTag "Bar" $ parseInside' insideCodec)
    gp_Voices      <- gp_Voices     =. childTag "Voices"
      (parseInside' $ bareList $ isTag "Voice" $ parseInside' insideCodec)
    gp_Beats       <- gp_Beats      =. childTag "Beats"
      (parseInside' $ bareList $ isTag "Beat" $ parseInside' insideCodec)
    gp_Notes       <- gp_Notes      =. childTag "Notes"
      (parseInside' $ bareList $ isTag "Note" $ parseInside' insideCodec)
    gp_Rhythms     <- gp_Rhythms    =. childTag "Rhythms"
      (parseInside' $ bareList $ isTag "Rhythm" $ parseInside' insideCodec)
    return GPIF{..}

data Score = Score
  -- probably some or all of these should be optional
  { score_Title                     :: T.Text
  , score_SubTitle                  :: T.Text
  , score_Artist                    :: T.Text
  , score_Album                     :: T.Text
  , score_Words                     :: T.Text
  , score_Music                     :: T.Text
  , score_WordsAndMusic             :: T.Text
  , score_Copyright                 :: T.Text
  , score_Tabber                    :: T.Text
  , score_Instructions              :: T.Text
  , score_Notices                   :: T.Text
  , score_FirstPageHeader           :: T.Text
  , score_FirstPageFooter           :: T.Text
  , score_PageHeader                :: T.Text
  , score_PageFooter                :: T.Text
  , score_ScoreSystemsDefaultLayout :: T.Text
  , score_ScoreSystemsLayout        :: [Int]
  , score_ScoreZoomPolicy           :: T.Text
  , score_ScoreZoom                 :: T.Text
  , score_MultiVoice                :: T.Text
  } deriving (Show)

instance IsInside Score where
  insideCodec = do
    score_Title                     <- score_Title                     =. childTag "Title"                     (parseInside' childText)
    score_SubTitle                  <- score_SubTitle                  =. childTag "SubTitle"                  (parseInside' childText)
    score_Artist                    <- score_Artist                    =. childTag "Artist"                    (parseInside' childText)
    score_Album                     <- score_Album                     =. childTag "Album"                     (parseInside' childText)
    score_Words                     <- score_Words                     =. childTag "Words"                     (parseInside' childText)
    score_Music                     <- score_Music                     =. childTag "Music"                     (parseInside' childText)
    score_WordsAndMusic             <- score_WordsAndMusic             =. childTag "WordsAndMusic"             (parseInside' childText)
    score_Copyright                 <- score_Copyright                 =. childTag "Copyright"                 (parseInside' childText)
    score_Tabber                    <- score_Tabber                    =. childTag "Tabber"                    (parseInside' childText)
    score_Instructions              <- score_Instructions              =. childTag "Instructions"              (parseInside' childText)
    score_Notices                   <- score_Notices                   =. childTag "Notices"                   (parseInside' childText)
    score_FirstPageHeader           <- score_FirstPageHeader           =. childTag "FirstPageHeader"           (parseInside' childText)
    score_FirstPageFooter           <- score_FirstPageFooter           =. childTag "FirstPageFooter"           (parseInside' childText)
    score_PageHeader                <- score_PageHeader                =. childTag "PageHeader"                (parseInside' childText)
    score_PageFooter                <- score_PageFooter                =. childTag "PageFooter"                (parseInside' childText)
    score_ScoreSystemsDefaultLayout <- score_ScoreSystemsDefaultLayout =. childTag "ScoreSystemsDefaultLayout" (parseInside' childText)
    score_ScoreSystemsLayout        <- score_ScoreSystemsLayout        =. childTag "ScoreSystemsLayout"        (parseInside' listOfInts)
    score_ScoreZoomPolicy           <- score_ScoreZoomPolicy           =. childTag "ScoreZoomPolicy"           (parseInside' childText)
    score_ScoreZoom                 <- score_ScoreZoom                 =. childTag "ScoreZoom"                 (parseInside' childText)
    score_MultiVoice                <- score_MultiVoice                =. childTag "MultiVoice"                (parseInside' childText)
    return Score{..}

data MasterTrack = MasterTrack
  { mt_Tracks      :: [Int]
  , mt_Automations :: V.Vector Automation
  -- RSE
  } deriving (Show)

instance IsInside MasterTrack where
  insideCodec = do
    mt_Tracks      <- mt_Tracks      =. childTag "Tracks" (parseInside' listOfInts)
    mt_Automations <- mt_Automations =. childTag "Automations"
      (parseInside' $ bareList $ isTag "Automation" $ parseInside' insideCodec)
    ignoreChildTag "RSE"
    return MasterTrack{..}

data Automation = Automation
  { auto_Type     :: T.Text
  , auto_Linear   :: Bool
  , auto_Bar      :: Int -- this appears to count from 0.
  , auto_Position :: Double -- this appears to count from 0. (beats?) not sure of num type
  , auto_Visible  :: Bool
  , auto_Value    :: T.Text
  } deriving (Show)

instance IsInside Automation where
  insideCodec = do
    auto_Type     <- auto_Type     =. childTag "Type"     (parseInside' childText)
    auto_Linear   <- auto_Linear   =. childTag "Linear"   (parseInside' $ boolWordText childText)
    auto_Bar      <- auto_Bar      =. childTag "Bar"      (parseInside' $ intText childText)
    auto_Position <- auto_Position =. childTag "Position" (parseInside' $ milliText childText)
    auto_Visible  <- auto_Visible  =. childTag "Visible"  (parseInside' $ boolWordText childText)
    auto_Value    <- auto_Value    =. childTag "Value"    (parseInside' childText)
    return Automation{..}

data Track = Track
  { trk_id          :: Int
  , trk_Name        :: T.Text
  , trk_ShortName   :: T.Text
  , trk_Color       :: [Int]
  -- SystemsDefautLayout, SystemsLayout, PalmMute, AutoAccentuation, PlayingStyle, UseOneChannelPerString, IconId, InstrumentSet
  , trk_Transpose   :: Transpose
  -- RSE, ForcedSound, Sounds, MidiConnection, PlaybackState, AudioEngineState, Lyrics
  , trk_Staves      :: V.Vector Staff
  , trk_Automations :: V.Vector Automation
  } deriving (Show)

instance IsInside Track where
  insideCodec = do
    trk_id          <- trk_id          =. intText (reqAttr "id")
    trk_Name        <- trk_Name        =. childTag "Name" (parseInside' childText)
    trk_ShortName   <- trk_ShortName   =. childTag "ShortName" (parseInside' childText)
    trk_Color       <- trk_Color       =. childTag "Color" (parseInside' listOfInts)
    ignoreChildTag "SystemsDefautLayout"
    ignoreChildTag "SystemsLayout"
    ignoreChildTag "PalmMute"
    ignoreChildTag "AutoAccentuation"
    ignoreChildTag "PlayingStyle"
    ignoreChildTag "UseOneChannelPerString"
    ignoreChildTag "IconId"
    ignoreChildTag "InstrumentSet"
    trk_Transpose   <- trk_Transpose   =. childTag "Transpose" (parseInside' insideCodec)
    ignoreChildTag "RSE"
    ignoreChildTag "ForcedSound"
    ignoreChildTag "Sounds"
    ignoreChildTag "MidiConnection"
    ignoreChildTag "PlaybackState"
    ignoreChildTag "AudioEngineState"
    ignoreChildTag "Lyrics"
    trk_Staves      <- trk_Staves      =. childTag "Staves"
      (parseInside' $ bareList $ isTag "Staff" $ parseInside' insideCodec)
    trk_Automations <- trk_Automations =. childTag "Automations"
      (parseInside' $ bareList $ isTag "Automation" $ parseInside' insideCodec)
    return Track{..}

data Transpose = Transpose
  { tp_Chromatic :: Int
  , tp_Octave    :: Int
  } deriving (Show)

instance IsInside Transpose where
  insideCodec = do
    tp_Chromatic <- tp_Chromatic =. childTag "Chromatic" (parseInside' $ intText childText)
    tp_Octave    <- tp_Octave    =. childTag "Octave"    (parseInside' $ intText childText)
    return Transpose{..}

data Staff = Staff
  { staff_Properties :: Properties
  } deriving (Show)

instance IsInside Staff where
  insideCodec = do
    staff_Properties <- staff_Properties =. childTag "Properties" (parseInside' insideCodec)
    return Staff{..}

data Properties = Properties
  { props_Properties :: V.Vector Property
  , props_Name       :: Maybe T.Text
  } deriving (Show)

instance IsInside Properties where
  insideCodec = do
    -- first parse the Name, which removes it, so all that's left are Property children
    props_Name       <- props_Name       =. childTagOpt "Name" (parseInside' childText)
    props_Properties <- props_Properties =. bareList (isTag "Property" $ parseInside' insideCodec)
    return Properties{..}

data Property = Property
  { prop_name  :: T.Text
  , prop_value :: PropertyValue
  } deriving (Show)

data PropertyValue
  = PropertyFret      Int
  | PropertyString    Int
  | PropertyFlags     Int
  | PropertyNumber    Int
  | PropertyBitset    T.Text
  | PropertyTuning    Tuning
  | PropertyItems     (V.Vector Element) -- unknown
  | PropertyFloat     Double
  | PropertyEnable
  | PropertyDirection T.Text
  | PropertyPitch     Pitch
  | PropertyHFret     Double
  | PropertyHType     T.Text
  | PropertyUnknown   (V.Vector Element)
  deriving (Show)

instance IsInside PropertyValue where
  insideCodec = Codec
    { codecIn = do
      ins <- lift get
      children <- codecIn childElements
      case V.toList children of
        []    -> return $ PropertyUnknown children
        [x]
          | matchName "Fret" $ elName x -> fmap PropertyFret
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Fret" $ parseInside' $ intText childText
          | matchName "String" $ elName x -> fmap PropertyString
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "String" $ parseInside' $ intText childText
          | matchName "Flags" $ elName x -> fmap PropertyFlags
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Flags" $ parseInside' $ intText childText
          | matchName "Number" $ elName x -> fmap PropertyNumber
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Number" $ parseInside' $ intText childText
          | matchName "Bitset" $ elName x -> fmap PropertyBitset
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Bitset" $ parseInside' childText
          | matchName "Items" $ elName x -> return $ PropertyItems children
          | matchName "Float" $ elName x -> fmap PropertyFloat
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Float" $ parseInside' $ milliText childText
          | matchName "Enable" $ elName x -> return PropertyEnable
          | matchName "Direction" $ elName x -> fmap PropertyDirection
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Direction" $ parseInside' childText
          | matchName "Pitch" $ elName x -> fmap PropertyPitch
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "Pitch" $ parseInside' insideCodec
          | matchName "HFret" $ elName x -> fmap PropertyHFret
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "HFret" $ parseInside' $ milliText childText
          | matchName "HType" $ elName x -> fmap PropertyHType
            $ mapStackTraceT (`runReaderT` x) $ codecIn $ isTag "HType" $ parseInside' childText
        x : _
          | matchName "Pitches" $ elName x -> do
            lift $ put ins -- restore the children so they can be parsed
            fmap PropertyTuning $ codecIn insideCodec
        _ -> return $ PropertyUnknown children
    , codecOut = fmapArg $ \case
      PropertyFret   n -> void $ codecOut (childTag' "Fret"   $ parseInside' $ intText childText) n
      PropertyString n -> void $ codecOut (childTag' "String" $ parseInside' $ intText childText) n
      PropertyFlags  n -> void $ codecOut (childTag' "Flags"  $ parseInside' $ intText childText) n
      PropertyNumber n -> void $ codecOut (childTag' "Number" $ parseInside' $ intText childText) n
      PropertyBitset s -> void $ codecOut (childTag' "Bitset" $ parseInside' childText) s
      PropertyTuning t -> void $ codecOut insideCodec' t
      PropertyItems _xs -> return () -- TODO
      PropertyFloat  n -> void $ codecOut (childTag' "Float"  $ parseInside' $ milliText childText) n
      PropertyEnable -> void $ codecOut (childTag' "Enable" $ return ()) ()
      PropertyDirection s -> void $ codecOut (childTag' "Direction" $ parseInside' childText) s
      PropertyPitch p -> void $ codecOut (childTag' "Pitch" $ parseInside' insideCodec) p
      PropertyHFret  n -> void $ codecOut (childTag' "HFret"  $ parseInside' $ milliText childText) n
      PropertyHType s -> void $ codecOut (childTag' "HType" $ parseInside' childText) s
      PropertyUnknown elts -> tell $ Inside { insideAttrs = V.empty, insideContent = V.map Elem elts }
    } where
      childTag' :: ParseName -> ValueCodec' (PureLog Identity) Inside a -> InsideCodec (PureLog Identity) a
      childTag' = childTag
      insideCodec' :: (IsInside a) => InsideCodec (PureLog Identity) a
      insideCodec' = insideCodec

data Tuning = Tuning
  { tuning_Pitches      :: [Int]
  , tuning_Instrument   :: T.Text
  , tuning_Label        :: T.Text
  , tuning_LabelVisible :: Bool
  } deriving (Show)

instance IsInside Tuning where
  insideCodec = do
    tuning_Pitches      <- tuning_Pitches      =. childTag "Pitches"      (parseInside' listOfInts)
    tuning_Instrument   <- tuning_Instrument   =. childTag "Instrument"   (parseInside' childText)
    tuning_Label        <- tuning_Label        =. childTag "Label"        (parseInside' childText)
    tuning_LabelVisible <- tuning_LabelVisible =. childTag "LabelVisible" (parseInside' $ boolWordText childText)
    return Tuning{..}

data Pitch = Pitch
  { pitch_Step       :: T.Text -- capital letter
  , pitch_Accidental :: T.Text -- "#" or "b"
  , pitch_Octave     :: Int
  } deriving (Show)

instance IsInside Pitch where
  insideCodec = do
    pitch_Step       <- pitch_Step       =. childTag "Step"       (parseInside' childText)
    pitch_Accidental <- pitch_Accidental =. childTag "Accidental" (parseInside' childText)
    pitch_Octave     <- pitch_Octave     =. childTag "Octave"     (parseInside' $ intText childText)
    return Pitch{..}

instance IsInside Property where
  insideCodec = do
    prop_name  <- prop_name  =. reqAttr "name"
    prop_value <- prop_value =. insideCodec
    return Property{..}

data MasterBar = MasterBar
  { mb_Key       :: Key
  , mb_Time      :: T.Text
  -- Repeat, Section, Fermatas
  , mb_DoubleBar :: Bool -- true if empty DoubleBar tag is present
  , mb_Bars      :: [Int]
  -- XProperties
  } deriving (Show)

instance IsInside MasterBar where
  insideCodec = do
    mb_Key       <- mb_Key       =. childTag "Key"  (parseInside' insideCodec)
    mb_Time      <- mb_Time      =. childTag "Time" (parseInside' childText)
    ignoreChildTag "Repeat"
    ignoreChildTag "Section"
    ignoreChildTag "Fermatas"
    mb_DoubleBar <- mb_DoubleBar =. dimap
      (\b -> guard b >> Just ())
      isJust
      (childTagOpt "DoubleBar" $ return ())
    mb_Bars      <- mb_Bars      =. childTag "Bars" (parseInside' listOfInts)
    ignoreChildTag "XProperties"
    return MasterBar{..}

data Key = Key
  { key_AccidentalCount :: Int
  , key_Mode            :: T.Text
  , key_TransposeAs     :: T.Text
  } deriving (Show)

instance IsInside Key where
  insideCodec = do
    key_AccidentalCount <- key_AccidentalCount =. childTag "AccidentalCount" (parseInside' $ intText childText)
    key_Mode            <- key_Mode            =. childTag "Mode"            (parseInside' childText)
    key_TransposeAs     <- key_TransposeAs     =. childTag "TransposeAs"     (parseInside' childText)
    return Key{..}

data Bar = Bar
  { bar_id     :: Int
  , bar_Clef   :: T.Text
  , bar_Voices :: [Int]
  -- XProperties (optional)
  } deriving (Show)

instance IsInside Bar where
  insideCodec = do
    bar_id     <- bar_id     =. intText (reqAttr "id")
    bar_Clef   <- bar_Clef   =. childTag "Clef"   (parseInside' childText)
    bar_Voices <- bar_Voices =. childTag "Voices" (parseInside' listOfInts)
    ignoreChildTag "XProperties"
    return Bar{..}

data Voice = Voice
  { voice_id    :: Int
  , voice_Beats :: [Int]
  } deriving (Show)

instance IsInside Voice where
  insideCodec = do
    voice_id    <- voice_id    =. intText (reqAttr "id")
    voice_Beats <- voice_Beats =. childTag "Beats" (parseInside' listOfInts)
    return Voice{..}

data Beat = Beat
  { beat_id         :: Int
  , beat_GraceNotes :: Maybe T.Text
  , beat_Dynamic    :: T.Text
  , beat_Rhythm     :: RhythmRef
  -- TransposedPitchStemOrientation, ConcertPitchStemOrientation
  , beat_Arpeggio   :: Maybe T.Text
  , beat_Variation  :: Maybe Int
  , beat_FreeText   :: Maybe T.Text
  , beat_Notes      :: Maybe [Int]
  , beat_Properties :: Properties
  -- XProperties
  } deriving (Show)

instance IsInside Beat where
  insideCodec = do
    beat_id         <- beat_id         =. intText (reqAttr "id")
    beat_GraceNotes <- beat_GraceNotes =. childTagOpt "GraceNotes" (parseInside' childText)
    beat_Dynamic    <- beat_Dynamic    =. childTag "Dynamic" (parseInside' childText)
    beat_Rhythm     <- beat_Rhythm     =. childTag "Rhythm" (parseInside' insideCodec)
    ignoreChildTag "TransposedPitchStemOrientation"
    ignoreChildTag "ConcertPitchStemOrientation"
    beat_Arpeggio   <- beat_Arpeggio   =. childTagOpt "Arpeggio" (parseInside' childText)
    beat_Variation  <- beat_Variation  =. childTagOpt "Variation" (parseInside' $ intText childText)
    beat_FreeText   <- beat_FreeText   =. childTagOpt "FreeText" (parseInside' childText)
    beat_Notes      <- beat_Notes      =. childTagOpt "Notes" (parseInside' listOfInts)
    beat_Properties <- beat_Properties =. childTag "Properties" (parseInside' insideCodec)
    ignoreChildTag "XProperties"
    return Beat{..}

newtype RhythmRef = RhythmRef
  { rhythm_ref :: Int
  } deriving (Show)

instance IsInside RhythmRef where
  insideCodec = do
    rhythm_ref <- rhythm_ref =. intText (reqAttr "ref")
    return RhythmRef{..}

data Note = Note
  { note_id                     :: Int
  , note_InstrumentArticulation :: Int
  , note_Properties             :: Properties
  , note_RightFingering         :: Maybe T.Text
  , note_Accent                 :: Maybe Int
  , note_Tie                    :: Maybe Tie
  , note_Vibrato                :: Maybe T.Text
  , note_AntiAccent             :: Maybe T.Text
  } deriving (Show)

instance IsInside Note where
  insideCodec = do
    note_id                     <- note_id                     =. intText (reqAttr "id")
    note_InstrumentArticulation <- note_InstrumentArticulation =. childTag "InstrumentArticulation" (parseInside' $ intText childText)
    note_Properties             <- note_Properties             =. childTag "Properties" (parseInside' insideCodec)
    note_RightFingering         <- note_RightFingering         =. childTagOpt "RightFingering" (parseInside' childText)
    note_Accent                 <- note_Accent                 =. childTagOpt "Accent" (parseInside' $ intText childText)
    note_Tie                    <- note_Tie                    =. childTagOpt "Tie" (parseInside' insideCodec)
    note_Vibrato                <- note_Vibrato                =. childTagOpt "Vibrato" (parseInside' childText)
    note_AntiAccent             <- note_AntiAccent             =. childTagOpt "AntiAccent" (parseInside' childText)
    return Note{..}

data Tie = Tie
  { tie_origin      :: Bool
  , tie_destination :: Bool
  } deriving (Show)

instance IsInside Tie where
  insideCodec = do
    tie_origin      <- tie_origin      =. boolWordText (reqAttr "origin")
    tie_destination <- tie_destination =. boolWordText (reqAttr "destination")
    return Tie{..}

data Rhythm = Rhythm
  { rhythm_id              :: Int
  , rhythm_NoteValue       :: T.Text
  , rhythm_AugmentationDot :: Maybe AugmentationDot
  , rhythm_PrimaryTuplet   :: Maybe PrimaryTuplet
  } deriving (Show)

instance IsInside Rhythm where
  insideCodec = do
    rhythm_id              <- rhythm_id              =. intText (reqAttr "id")
    rhythm_NoteValue       <- rhythm_NoteValue       =. childTag "NoteValue" (parseInside' childText)
    rhythm_AugmentationDot <- rhythm_AugmentationDot =. childTagOpt "AugmentationDot" (parseInside' insideCodec)
    rhythm_PrimaryTuplet   <- rhythm_PrimaryTuplet   =. childTagOpt "PrimaryTuplet" (parseInside' insideCodec)
    return Rhythm{..}

newtype AugmentationDot = AugmentationDot
  { aug_count :: Int
  } deriving (Show)

instance IsInside AugmentationDot where
  insideCodec = do
    aug_count <- aug_count =. intText (reqAttr "count")
    return AugmentationDot{..}

data PrimaryTuplet = PrimaryTuplet
  { tup_num :: Int
  , tup_den :: Int
  } deriving (Show)

instance IsInside PrimaryTuplet where
  insideCodec = do
    tup_num <- tup_num =. intText (reqAttr "num")
    tup_den <- tup_den =. intText (reqAttr "den")
    return PrimaryTuplet{..}

listOfInts :: (Monad m) => InsideCodec m [Int]
listOfInts = Codec
  { codecIn = do
    t <- codecIn childText
    forM (T.words t) $ \word -> case readMaybe $ T.unpack word of
      Nothing -> fatal $ "Failed to parse list of ints: " <> show t
      Just n  -> return n
  , codecOut = undefined
  }

-- `zip-archive` package fails to parse .gp zip for some reason. `zip` is fine.
-- does not handle edit-locked files (score.gpif is encrypted somehow, via 'editLocked' file)
parseGP :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m GPIF
parseGP f = inside ("Loading: " <> f) $ do
  gpif <- Zip.withArchive f $ Zip.mkEntrySelector "Content/score.gpif" >>= Zip.getEntry
  elt <- maybe (fatal "Couldn't parse XML") return
    $ parseXMLDoc $ TE.decodeUtf8 gpif
  mapStackTraceT (`runReaderT` elt) $ codecIn $ isTag "GPIF" $ parseInside' insideCodec
