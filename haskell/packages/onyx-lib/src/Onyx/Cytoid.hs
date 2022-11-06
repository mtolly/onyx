{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Cytoid where

import           Control.Monad.Codec
import qualified Data.Aeson          as A
import           Data.Profunctor     (dimap)
import           Data.Scientific     (Scientific)
import qualified Data.Text           as T
import           Onyx.Codec.Common
import           Onyx.Codec.JSON
import qualified Sound.MIDI.Util     as U

-- | Contents of level.json
data CytoidLevel = CytoidLevel
  { level_format             :: T.Text
  , level_schema_version     :: Int
  , level_version            :: Int
  , level_id                 :: T.Text
  , level_title              :: T.Text
  , level_artist             :: T.Text
  , level_artist_source      :: T.Text
  , level_illustrator        :: T.Text
  , level_illustrator_source :: T.Text
  , level_charter            :: T.Text
  , level_music              :: FilePath
  , level_music_preview      :: FilePath
  , level_background         :: FilePath
  , level_charts             :: [ChartInfo]
  } deriving (Eq, Ord, Show)

instance StackJSON CytoidLevel where
  stackJSON = asObject "CytoidLevel" $ do
    level_format             <- level_format             =. req "format"             stackJSON
    level_schema_version     <- level_schema_version     =. req "schema_version"     stackJSON
    level_version            <- level_version            =. req "version"            stackJSON
    level_id                 <- level_id                 =. req "id"                 stackJSON
    level_title              <- level_title              =. req "title"              stackJSON
    level_artist             <- level_artist             =. req "artist"             stackJSON
    level_artist_source      <- level_artist_source      =. req "artist_source"      stackJSON
    level_illustrator        <- level_illustrator        =. req "illustrator"        stackJSON
    level_illustrator_source <- level_illustrator_source =. req "illustrator_source" stackJSON
    level_charter            <- level_charter            =. req "charter"            stackJSON
    let pathObj = asObject "path to audio or image" $ req "path" stackJSON
    level_music              <- level_music              =. req "music"              pathObj
    level_music_preview      <- level_music_preview      =. req "music_preview"      pathObj
    level_background         <- level_background         =. req "background"         pathObj
    level_charts             <- level_charts             =. req "charts"             stackJSON
    return CytoidLevel{..}

data ChartInfo = ChartInfo
  { ci_type       :: T.Text
  , ci_name       :: T.Text
  , ci_difficulty :: Int
  , ci_path       :: FilePath
  } deriving (Eq, Ord, Show)

instance StackJSON ChartInfo where
  stackJSON = asObject "ChartInfo" $ do
    ci_type       <- ci_type       =. req "type"       stackJSON
    ci_name       <- ci_name       =. req "name"       stackJSON
    ci_difficulty <- ci_difficulty =. req "difficulty" stackJSON
    ci_path       <- ci_path       =. req "path"       stackJSON
    return ChartInfo{..}

-- | Contents of single chart (difficulty) JSON file
data CytoidChart t = CytoidChart
  { chart_format_version    :: Int
  , chart_time_base         :: t
  , chart_start_offset_time :: U.Seconds
  , chart_page_list         :: [Page t]
  , chart_tempo_list        :: [Tempo t]
  , chart_event_order_list  :: [EventOrder t]
  , chart_note_list         :: [Note t]
  } deriving (Eq, Ord, Show)

instance (StackJSON t) => StackJSON (CytoidChart t) where
  stackJSON = asObject "CytoidChart" $ do
    chart_format_version    <- chart_format_version    =. req "format_version"    stackJSON
    chart_time_base         <- chart_time_base         =. req "time_base"         stackJSON
    let seconds = dimap
          -- TODO confirm that this is in seconds and not microseconds or something
          (\(U.Seconds s) -> realToFrac s :: Scientific)
          (U.Seconds . realToFrac)
          stackJSON
    chart_start_offset_time <- chart_start_offset_time =. req "start_offset_time" seconds
    chart_page_list         <- chart_page_list         =. req "page_list"         stackJSON
    chart_tempo_list        <- chart_tempo_list        =. req "tempo_list"        stackJSON
    chart_event_order_list  <- chart_event_order_list  =. req "event_order_list"  stackJSON
    chart_note_list         <- chart_note_list         =. req "note_list"         stackJSON
    return CytoidChart{..}

data ScanLineDirection = ScanDown | ScanUp
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON ScanLineDirection where
  stackJSON = enumCodec "a scan line direction (-1 for down, 1 for up)" $ \case
    ScanDown -> A.Number (-1)
    ScanUp   -> A.Number 1

data Page t = Page
  { page_start_tick          :: t
  , page_end_tick            :: t
  , page_scan_line_direction :: ScanLineDirection
  } deriving (Eq, Ord, Show)

instance (StackJSON t) => StackJSON (Page t) where
  stackJSON = asObject "Page" $ do
    page_start_tick          <- page_start_tick          =. req "start_tick"          stackJSON
    page_end_tick            <- page_end_tick            =. req "end_tick"            stackJSON
    page_scan_line_direction <- page_scan_line_direction =. req "scan_line_direction" stackJSON
    return Page{..}

data Tempo t = Tempo
  { tempo_tick  :: t
  , tempo_value :: U.BPS
  } deriving (Eq, Ord, Show)

instance (StackJSON t) => StackJSON (Tempo t) where
  stackJSON = asObject "Tempo" $ do
    tempo_tick  <- tempo_tick  =. req "tick"  stackJSON
    let tempo = dimap
          (\(U.BPS bps) -> realToFrac $ 1000000 / bps :: Scientific)
          (\uspqn -> U.BPS $ 1000000 / realToFrac uspqn)
          stackJSON
    tempo_value <- tempo_value =. req "value" tempo
    return Tempo{..}

data EventOrder t = EventOrder
  { eo_tick  :: t
  , eo_value :: Event
  } deriving (Eq, Ord, Show)

instance (StackJSON t) => StackJSON (EventOrder t) where
  stackJSON = asObject "EventOrder" $ do
    eo_tick  <- eo_tick  =. req "tick"  stackJSON
    eo_value <- eo_value =. req "value" stackJSON
    return EventOrder{..}

data Event = Event
  { event_type :: Int
  , event_args :: T.Text
  } deriving (Eq, Ord, Show)

instance StackJSON Event where
  stackJSON = asObject "Event" $ do
    event_type <- event_type =. req "tick" stackJSON
    event_args <- event_args =. req "args" stackJSON
    return Event{..}

data Note t = Note
  { note_page_index  :: Int
  , note_type        :: NoteType
  , note_id          :: Int
  , note_tick        :: t
  , note_x           :: Double
  , note_has_sibling :: Bool
  , note_hold_tick   :: t
  , note_next_id     :: Int
  , note_is_forward  :: Bool
  } deriving (Eq, Ord, Show)

instance (StackJSON t) => StackJSON (Note t) where
  stackJSON = asObject "Note" $ do
    note_page_index  <- note_page_index  =. req "page_index"  stackJSON
    note_type        <- note_type        =. req "type"        stackJSON
    note_id          <- note_id          =. req "id"          stackJSON
    note_tick        <- note_tick        =. req "tick"        stackJSON
    note_x           <- note_x           =. req "x"           stackJSON
    note_has_sibling <- note_has_sibling =. req "has_sibling" stackJSON
    note_hold_tick   <- note_hold_tick   =. req "hold_tick"   stackJSON
    note_next_id     <- note_next_id     =. req "next_id"     stackJSON
    note_is_forward  <- note_is_forward  =. req "is_forward"  stackJSON
    return Note{..}

data NoteType
  = TapNote
  | HoldNote
  | LongHoldNote
  | DragHeadNote
  | DragChildNote
  | FlickNote
  deriving (Eq, Ord, Show, Enum, Bounded)

instance StackJSON NoteType where
  stackJSON = enumCodec "a note type (0 to 5)" $ \case
    TapNote       -> A.Number 0
    HoldNote      -> A.Number 1
    LongHoldNote  -> A.Number 2
    DragHeadNote  -> A.Number 3
    DragChildNote -> A.Number 4
    FlickNote     -> A.Number 5
