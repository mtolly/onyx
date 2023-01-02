{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.GuitarPro where

import qualified Codec.Archive.Zip          as Zip
import           Control.Monad              (forM, guard, void)
import           Control.Monad.Codec
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State  (get, put)
import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import           Data.Functor.Identity      (Identity)
import           Data.Maybe                 (isJust)
import           Data.Profunctor            (dimap)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Vector                as V
import           Onyx.Codec.XML
import           Onyx.GuitarPro.GPX         (gpxFiles)
import           Onyx.StackTrace
import           Text.Read                  (readMaybe)
import           Text.XML.Light

-- .gpif is an XML format, shared by .gpx and .gp (maybe earlier as well)
-- .gp (GP7) is a zip file
-- .gpx (GP6) is a custom file structure + compression format

data GPIF = GPIF
  { gpVersion   :: Maybe Int -- not in .gpx
  -- GPRevision
  -- Encoding
  , score       :: Score
  , masterTrack :: MasterTrack
  -- AudioTracks
  , tracks      :: V.Vector Track
  , masterBars  :: V.Vector MasterBar
  , bars        :: V.Vector Bar
  , voices      :: V.Vector Voice
  , beats       :: V.Vector Beat
  , notes       :: V.Vector Note
  , rhythms     :: V.Vector Rhythm
  } deriving (Show)

instance IsInside GPIF where
  insideCodec = do
    gpVersion   <- (.gpVersion) =. childTagOpt "GPVersion" (parseInside' $ intText childText)
    ignoreChildTag "GPRevision"
    ignoreChildTag "Encoding"
    score       <- (.score) =. childTag "Score" (parseInside' insideCodec)
    masterTrack <- (.masterTrack) =. childTag "MasterTrack" (parseInside' insideCodec)
    ignoreChildTag "AudioTracks"
    tracks      <- (.tracks) =. childTag "Tracks"
      (parseInside' $ bareList $ isTag "Track" $ parseInside' insideCodec)
    masterBars  <- (.masterBars) =. childTag "MasterBars"
      (parseInside' $ bareList $ isTag "MasterBar" $ parseInside' insideCodec)
    bars        <- (.bars)       =. childTag "Bars"
      (parseInside' $ bareList $ isTag "Bar" $ parseInside' insideCodec)
    voices      <- (.voices)     =. childTag "Voices"
      (parseInside' $ bareList $ isTag "Voice" $ parseInside' insideCodec)
    beats       <- (.beats)      =. childTag "Beats"
      (parseInside' $ bareList $ isTag "Beat" $ parseInside' insideCodec)
    notes       <- (.notes)      =. childTag "Notes"
      (parseInside' $ bareList $ isTag "Note" $ parseInside' insideCodec)
    rhythms     <- (.rhythms)    =. childTag "Rhythms"
      (parseInside' $ bareList $ isTag "Rhythm" $ parseInside' insideCodec)
    return GPIF{..}

data Score = Score
  -- probably some or all of these should be optional
  { title                     :: T.Text
  , subTitle                  :: T.Text
  , artist                    :: T.Text
  , album                     :: T.Text
  , words_                    :: T.Text
  , music                     :: T.Text
  , wordsAndMusic             :: T.Text
  , copyright                 :: T.Text
  , tabber                    :: T.Text
  , instructions              :: T.Text
  , notices                   :: T.Text
  , firstPageHeader           :: T.Text
  , firstPageFooter           :: T.Text
  , pageHeader                :: T.Text
  , pageFooter                :: T.Text
  , scoreSystemsDefaultLayout :: T.Text
  , scoreSystemsLayout        :: [Int]
  , scoreZoomPolicy           :: Maybe T.Text -- not in .gpx
  , scoreZoom                 :: Maybe T.Text -- not in .gpx
  , multiVoice                :: T.Text
  } deriving (Show)

instance IsInside Score where
  insideCodec = do
    title                     <- (.title                    ) =. childTag "Title"                     (parseInside' childText)
    subTitle                  <- (.subTitle                 ) =. childTag "SubTitle"                  (parseInside' childText)
    artist                    <- (.artist                   ) =. childTag "Artist"                    (parseInside' childText)
    album                     <- (.album                    ) =. childTag "Album"                     (parseInside' childText)
    words_                    <- (.words_                   ) =. childTag "Words"                     (parseInside' childText)
    music                     <- (.music                    ) =. childTag "Music"                     (parseInside' childText)
    wordsAndMusic             <- (.wordsAndMusic            ) =. childTag "WordsAndMusic"             (parseInside' childText)
    copyright                 <- (.copyright                ) =. childTag "Copyright"                 (parseInside' childText)
    tabber                    <- (.tabber                   ) =. childTag "Tabber"                    (parseInside' childText)
    instructions              <- (.instructions             ) =. childTag "Instructions"              (parseInside' childText)
    notices                   <- (.notices                  ) =. childTag "Notices"                   (parseInside' childText)
    firstPageHeader           <- (.firstPageHeader          ) =. childTag "FirstPageHeader"           (parseInside' childText)
    firstPageFooter           <- (.firstPageFooter          ) =. childTag "FirstPageFooter"           (parseInside' childText)
    pageHeader                <- (.pageHeader               ) =. childTag "PageHeader"                (parseInside' childText)
    pageFooter                <- (.pageFooter               ) =. childTag "PageFooter"                (parseInside' childText)
    scoreSystemsDefaultLayout <- (.scoreSystemsDefaultLayout) =. childTag "ScoreSystemsDefaultLayout" (parseInside' childText)
    scoreSystemsLayout        <- (.scoreSystemsLayout       ) =. childTag "ScoreSystemsLayout"        (parseInside' listOfInts)
    scoreZoomPolicy           <- (.scoreZoomPolicy          ) =. childTagOpt "ScoreZoomPolicy"        (parseInside' childText)
    scoreZoom                 <- (.scoreZoom                ) =. childTagOpt "ScoreZoom"                 (parseInside' childText)
    multiVoice                <- (.multiVoice               ) =. childTag "MultiVoice"                (parseInside' childText)
    return Score{..}

data MasterTrack = MasterTrack
  { tracks      :: [Int]
  , automations :: V.Vector Automation
  -- RSE
  } deriving (Show)

instance IsInside MasterTrack where
  insideCodec = do
    tracks      <- (.tracks)      =. childTag "Tracks" (parseInside' listOfInts)
    automations <- (.automations) =. childTag "Automations"
      (parseInside' $ bareList $ isTag "Automation" $ parseInside' insideCodec)
    ignoreChildTag "RSE"
    return MasterTrack{..}

data Automation = Automation
  { type_    :: T.Text
  , linear   :: Bool
  , bar      :: Int -- this appears to count from 0.
  , position :: Double -- this appears to count from 0. (beats?) not sure of num type
  , visible  :: Bool
  , value    :: T.Text
  } deriving (Show)

instance IsInside Automation where
  insideCodec = do
    type_    <- (.type_)    =. childTag "Type"     (parseInside' childText)
    linear   <- (.linear)   =. childTag "Linear"   (parseInside' $ boolWordText childText)
    bar      <- (.bar)      =. childTag "Bar"      (parseInside' $ intText childText)
    position <- (.position) =. childTag "Position" (parseInside' $ milliText childText)
    visible  <- (.visible)  =. childTag "Visible"  (parseInside' $ boolWordText childText)
    value    <- (.value)    =. childTag "Value"    (parseInside' childText)
    return Automation{..}

data Track = Track
  { id_         :: Int
  , name        :: T.Text
  , shortName   :: T.Text
  , color       :: [Int]
  -- SystemsDefautLayout, SystemsLayout, PalmMute, AutoAccentuation, PlayingStyle, Instrument (.gpx), UseOneChannelPerString, IconId, InstrumentSet
  , transpose   :: Maybe Transpose -- not in .gpx, has <PartSounding> instead
  -- RSE, ForcedSound, Sounds, MidiConnection, PlaybackState, AudioEngineState, Lyrics
  , staves      :: Maybe (V.Vector Staff) -- not in .gpx
  , automations :: Maybe (V.Vector Automation) -- not in .gpx
  , properties  :: Maybe Properties -- only in .gpx
  } deriving (Show)

instance IsInside Track where
  insideCodec = do
    id_         <- (.id_)         =. intText (reqAttr "id")
    name        <- (.name)        =. childTag "Name" (parseInside' childText)
    shortName   <- (.shortName)   =. childTag "ShortName" (parseInside' childText)
    color       <- (.color)       =. childTag "Color" (parseInside' listOfInts)
    ignoreChildTag "SystemsDefautLayout"
    ignoreChildTag "SystemsLayout"
    ignoreChildTag "PalmMute"
    ignoreChildTag "AutoAccentuation"
    ignoreChildTag "PlayingStyle"
    ignoreChildTag "Instrument"
    ignoreChildTag "UseOneChannelPerString"
    ignoreChildTag "IconId"
    ignoreChildTag "InstrumentSet"
    transpose   <- (.transpose)   =. childTagOpt "Transpose" (parseInside' insideCodec)
    ignoreChildTag "RSE"
    ignoreChildTag "ForcedSound"
    ignoreChildTag "Sounds"
    ignoreChildTag "MidiConnection"
    ignoreChildTag "PlaybackState"
    ignoreChildTag "AudioEngineState"
    ignoreChildTag "Lyrics"
    staves      <- (.staves)      =. childTagOpt "Staves"
      (parseInside' $ bareList $ isTag "Staff" $ parseInside' insideCodec)
    automations <- (.automations) =. childTagOpt "Automations"
      (parseInside' $ bareList $ isTag "Automation" $ parseInside' insideCodec)
    properties <- (.properties) =. childTagOpt "Properties" (parseInside' insideCodec)
    return Track{..}

data Transpose = Transpose
  { chromatic :: Int
  , octave    :: Int
  } deriving (Show)

instance IsInside Transpose where
  insideCodec = do
    chromatic <- (.chromatic) =. childTag "Chromatic" (parseInside' $ intText childText)
    octave    <- (.octave)    =. childTag "Octave"    (parseInside' $ intText childText)
    return Transpose{..}

data Staff = Staff
  { properties :: Properties
  } deriving (Show)

instance IsInside Staff where
  insideCodec = do
    properties <- (.properties) =. childTag "Properties" (parseInside' insideCodec)
    return Staff{..}

data Properties = Properties
  { properties :: V.Vector Property
  , name       :: Maybe T.Text
  } deriving (Show)

instance IsInside Properties where
  insideCodec = do
    -- first parse the Name, which removes it, so all that's left are Property children
    name       <- (.name)       =. childTagOpt "Name" (parseInside' childText)
    properties <- (.properties) =. bareList (isTag "Property" $ parseInside' insideCodec)
    return Properties{..}

data Property = Property
  { name  :: T.Text
  , value :: PropertyValue
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
  { pitches      :: [Int]
  , instrument   :: Maybe T.Text -- not in .gpx
  , label_       :: Maybe T.Text -- not in .gpx
  , labelVisible :: Maybe Bool -- not in .gpx
  } deriving (Show)

instance IsInside Tuning where
  insideCodec = do
    pitches      <- (.pitches     ) =. childTag    "Pitches"      (parseInside' listOfInts)
    instrument   <- (.instrument  ) =. childTagOpt "Instrument"   (parseInside' childText)
    label_       <- (.label_      ) =. childTagOpt "Label"        (parseInside' childText)
    labelVisible <- (.labelVisible) =. childTagOpt "LabelVisible" (parseInside' $ boolWordText childText)
    return Tuning{..}

data Pitch = Pitch
  { step       :: T.Text -- capital letter
  , accidental :: T.Text -- "#" or "b"
  , octave     :: Int
  } deriving (Show)

instance IsInside Pitch where
  insideCodec = do
    step       <- (.step)       =. childTag "Step"       (parseInside' childText)
    accidental <- (.accidental) =. childTag "Accidental" (parseInside' childText)
    octave     <- (.octave)     =. childTag "Octave"     (parseInside' $ intText childText)
    return Pitch{..}

instance IsInside Property where
  insideCodec = do
    name  <- (.name)  =. reqAttr "name"
    value <- (.value) =. insideCodec
    return Property{..}

data MasterBar = MasterBar
  { key       :: Key
  , time      :: T.Text
  -- Repeat, Section, Fermatas
  , doubleBar :: Bool -- true if empty DoubleBar tag is present
  , bars      :: [Int]
  -- XProperties
  } deriving (Show)

instance IsInside MasterBar where
  insideCodec = do
    key       <- (.key)       =. childTag "Key"  (parseInside' insideCodec)
    time      <- (.time)      =. childTag "Time" (parseInside' childText)
    ignoreChildTag "Repeat"
    ignoreChildTag "Section"
    ignoreChildTag "Fermatas"
    doubleBar <- (.doubleBar) =. dimap
      (\b -> guard b >> Just ())
      isJust
      (childTagOpt "DoubleBar" $ return ())
    bars      <- (.bars)      =. childTag "Bars" (parseInside' listOfInts)
    ignoreChildTag "XProperties"
    return MasterBar{..}

data Key = Key
  { accidentalCount :: Int
  , mode            :: T.Text
  , transposeAs     :: Maybe T.Text -- not in .gpx
  } deriving (Show)

instance IsInside Key where
  insideCodec = do
    accidentalCount <- (.accidentalCount) =. childTag "AccidentalCount" (parseInside' $ intText childText)
    mode            <- (.mode)            =. childTag "Mode"            (parseInside' childText)
    transposeAs     <- (.transposeAs)     =. childTagOpt "TransposeAs"  (parseInside' childText)
    return Key{..}

data Bar = Bar
  { id_    :: Int
  , clef   :: T.Text
  , voices :: [Int]
  -- XProperties (optional)
  } deriving (Show)

instance IsInside Bar where
  insideCodec = do
    id_    <- (.id_)    =. intText (reqAttr "id")
    clef   <- (.clef)   =. childTag "Clef"   (parseInside' childText)
    voices <- (.voices) =. childTag "Voices" (parseInside' listOfInts)
    ignoreChildTag "XProperties"
    return Bar{..}

data Voice = Voice
  { id_   :: Int
  , beats :: [Int]
  } deriving (Show)

instance IsInside Voice where
  insideCodec = do
    id_   <- (.id_)   =. intText (reqAttr "id")
    beats <- (.beats) =. childTag "Beats" (parseInside' listOfInts)
    return Voice{..}

data Beat = Beat
  { id_        :: Int
  , graceNotes :: Maybe T.Text
  -- Bank (.gpx)
  , dynamic_   :: T.Text
  , rhythm     :: RhythmRef
  -- TransposedPitchStemOrientation, ConcertPitchStemOrientation
  , arpeggio   :: Maybe T.Text
  , variation  :: Maybe Int
  , freeText   :: Maybe T.Text
  , notes      :: Maybe [Int]
  , properties :: Properties
  -- XProperties
  } deriving (Show)

instance IsInside Beat where
  insideCodec = do
    id_        <- (.id_)        =. intText (reqAttr "id")
    graceNotes <- (.graceNotes) =. childTagOpt "GraceNotes" (parseInside' childText)
    ignoreChildTag "Bank"
    dynamic_   <- (.dynamic_)   =. childTag "Dynamic" (parseInside' childText)
    rhythm     <- (.rhythm)     =. childTag "Rhythm" (parseInside' insideCodec)
    ignoreChildTag "TransposedPitchStemOrientation"
    ignoreChildTag "ConcertPitchStemOrientation"
    arpeggio   <- (.arpeggio)   =. childTagOpt "Arpeggio" (parseInside' childText)
    variation  <- (.variation)  =. childTagOpt "Variation" (parseInside' $ intText childText)
    freeText   <- (.freeText)   =. childTagOpt "FreeText" (parseInside' childText)
    notes      <- (.notes)      =. childTagOpt "Notes" (parseInside' listOfInts)
    properties <- (.properties) =. childTag "Properties" (parseInside' insideCodec)
    ignoreChildTag "XProperties"
    return Beat{..}

newtype RhythmRef = RhythmRef
  { ref :: Int
  } deriving (Show)

instance IsInside RhythmRef where
  insideCodec = do
    ref <- (.ref) =. intText (reqAttr "ref")
    return RhythmRef{..}

data Note = Note
  { id_                    :: Int
  , instrumentArticulation :: Maybe Int -- not in .gpx
  , properties             :: Properties
  , rightFingering         :: Maybe T.Text
  , accent                 :: Maybe Int
  , tie                    :: Maybe Tie
  , vibrato                :: Maybe T.Text
  , antiAccent             :: Maybe T.Text
  } deriving (Show)

instance IsInside Note where
  insideCodec = do
    id_                    <- (.id_                   ) =. intText (reqAttr "id")
    instrumentArticulation <- (.instrumentArticulation) =. childTagOpt "InstrumentArticulation" (parseInside' $ intText childText)
    properties             <- (.properties            ) =. childTag "Properties" (parseInside' insideCodec)
    rightFingering         <- (.rightFingering        ) =. childTagOpt "RightFingering" (parseInside' childText)
    accent                 <- (.accent                ) =. childTagOpt "Accent" (parseInside' $ intText childText)
    tie                    <- (.tie                   ) =. childTagOpt "Tie" (parseInside' insideCodec)
    vibrato                <- (.vibrato               ) =. childTagOpt "Vibrato" (parseInside' childText)
    antiAccent             <- (.antiAccent            ) =. childTagOpt "AntiAccent" (parseInside' childText)
    return Note{..}

data Tie = Tie
  { origin      :: Bool
  , destination :: Bool
  } deriving (Show)

instance IsInside Tie where
  insideCodec = do
    origin      <- (.origin     ) =. boolWordText (reqAttr "origin")
    destination <- (.destination) =. boolWordText (reqAttr "destination")
    return Tie{..}

data Rhythm = Rhythm
  { id_             :: Int
  , noteValue       :: T.Text
  , augmentationDot :: Maybe AugmentationDot
  , primaryTuplet   :: Maybe PrimaryTuplet
  } deriving (Show)

instance IsInside Rhythm where
  insideCodec = do
    id_             <- (.id_            ) =. intText (reqAttr "id")
    noteValue       <- (.noteValue      ) =. childTag "NoteValue" (parseInside' childText)
    augmentationDot <- (.augmentationDot) =. childTagOpt "AugmentationDot" (parseInside' insideCodec)
    primaryTuplet   <- (.primaryTuplet  ) =. childTagOpt "PrimaryTuplet" (parseInside' insideCodec)
    return Rhythm{..}

newtype AugmentationDot = AugmentationDot
  { count :: Int
  } deriving (Show)

instance IsInside AugmentationDot where
  insideCodec = do
    count <- (.count) =. intText (reqAttr "count")
    return AugmentationDot{..}

data PrimaryTuplet = PrimaryTuplet
  { num :: Int
  , den :: Int
  } deriving (Show)

instance IsInside PrimaryTuplet where
  insideCodec = do
    num <- (.num) =. intText (reqAttr "num")
    den <- (.den) =. intText (reqAttr "den")
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
  parseGPIF gpif

parseGPIF :: (SendMessage m) => B.ByteString -> StackTraceT m GPIF
parseGPIF gpif = do
  elt <- maybe (fatal "Couldn't parse XML") return $ parseXMLDoc $ TE.decodeUtf8 gpif
  mapStackTraceT (`runReaderT` elt) $ codecIn $ isTag "GPIF" $ parseInside' insideCodec

parseGPX :: (MonadIO m, SendMessage m) => FilePath -> StackTraceT m GPIF
parseGPX f = inside ("Loading: " <> f) $ do
  files <- stackIO (B.readFile f) >>= gpxFiles . BL.fromStrict
  case lookup "score.gpif" files of
    Just bs -> parseGPIF $ BL.toStrict bs
    Nothing -> fatal "No score.gpif found in .gpx contents"
