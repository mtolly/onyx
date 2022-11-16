{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Onyx.Import.Freetar where

import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader       (runReaderT)
import qualified Data.ByteString                  as B
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.Audio
import           Onyx.Codec.XML
import           Onyx.Guitar                      (HOPOsAlgorithm (..), emit5',
                                                   strumHOPOTap)
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..))
import qualified Onyx.MIDI.Track.File             as RBFile
import qualified Onyx.MIDI.Track.FiveFret         as F
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import           Onyx.Util.Text.Decode            (decodeGeneral)
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))
import           Text.Read                        (readMaybe)
import           Text.XML.Light                   (parseXMLDoc)

data Properties = Properties
  { ftVersion            :: Maybe T.Text
  , ftTitle              :: Maybe T.Text
  , ftArtist             :: Maybe T.Text
  , ftAlbum              :: Maybe T.Text
  , ftYear               :: Maybe T.Text
  , ftBeatsPerSecond     :: Maybe Double
  , ftBeatOffset         :: Maybe Double
  , ftHammerOnTime       :: Maybe Double
  , ftPullOffTime        :: Maybe Double
  , ftDifficulty         :: Maybe T.Text
  , ftAllowableErrorTime :: Maybe Double
  , ftLength             :: Maybe Double
  , ftMusicFileName      :: Maybe T.Text
  , ftMusicDirectoryHint :: Maybe T.Text
  } deriving (Show)

instance IsInside Properties where
  insideCodec = do
    ftVersion            <- ftVersion            =. childTagOpt "Version"            (parseInside' childText)
    ftTitle              <- ftTitle              =. childTagOpt "Title"              (parseInside' childText)
    ftArtist             <- ftArtist             =. childTagOpt "Artist"             (parseInside' childText)
    ftAlbum              <- ftAlbum              =. childTagOpt "Album"              (parseInside' childText)
    ftYear               <- ftYear               =. childTagOpt "Year"               (parseInside' childText)
    ftBeatsPerSecond     <- ftBeatsPerSecond     =. childTagOpt "BeatsPerSecond"     (parseInside' $ milliText childText)
    ftBeatOffset         <- ftBeatOffset         =. childTagOpt "BeatOffset"         (parseInside' $ milliText childText)
    ftHammerOnTime       <- ftHammerOnTime       =. childTagOpt "HammerOnTime"       (parseInside' $ milliText childText)
    ftPullOffTime        <- ftPullOffTime        =. childTagOpt "PullOffTime"        (parseInside' $ milliText childText)
    ftDifficulty         <- ftDifficulty         =. childTagOpt "Difficulty"         (parseInside' childText)
    ftAllowableErrorTime <- ftAllowableErrorTime =. childTagOpt "AllowableErrorTime" (parseInside' $ milliText childText)
    ftLength             <- ftLength             =. childTagOpt "Length"             (parseInside' $ milliText childText)
    ftMusicFileName      <- ftMusicFileName      =. childTagOpt "MusicFileName"      (parseInside' childText)
    ftMusicDirectoryHint <- ftMusicDirectoryHint =. childTagOpt "MusicDirectoryHint" (parseInside' childText)
    return Properties{..}

data Note = Note
  { n_time     :: Double
  , n_duration :: Double
  , n_track    :: Int
  -- TODO HammerOnAllowed
  } deriving (Show)

instance IsInside Note where
  insideCodec = do
    n_time     <- n_time     =. milliText (reqAttr "time")
    n_duration <- n_duration =. milliText (reqAttr "duration")
    n_track    <- n_track    =. intText   (reqAttr "track")
    return Note{..}

data Song = Song
  { songProperties :: Properties
  , songData       :: V.Vector Note
  } deriving (Show)

instance IsInside Song where
  insideCodec = do
    songProperties <- songProperties =. childTag "Properties" (parseInside' insideCodec)
    songData       <- songData       =. childTag "Data"
      (parseInside' $ bareList $ isTag "Note" $ parseInside' insideCodec)
    return Song{..}

parseSong :: (SendMessage m) => T.Text -> StackTraceT m Song
parseSong xml = do
  elt <- maybe (fatal "Couldn't parse XML") return $ parseXMLDoc xml
  mapStackTraceT (`runReaderT` elt) $ codecIn $ isTag "Song" $ parseInside' insideCodec

songToMidi :: Song -> RBFile.Song (RBFile.OnyxFile U.Beats)
songToMidi song = let
  tempos = U.tempoMapFromBPS RTB.empty -- "BeatsPerSecond" is probably useless
  sigs = U.measureMapFromTimeSigs U.Truncate RTB.empty
  threshold :: U.Seconds
  threshold = case NE.nonEmpty $ catMaybes [ftHammerOnTime $ songProperties song, ftPullOffTime $ songProperties song] of
    Nothing -> 0.25
    Just ne -> realToFrac $ maximum ne
  gtr = emit5' $ U.unapplyTempoTrack tempos $ strumHOPOTap HOPOsRBGuitar threshold
    $ RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ sort
    $ map (\n -> let
      time = realToFrac $ n_time n
      fret = Just $ toEnum $ n_track n
      len = case n_duration n of
        0 -> Nothing
        d -> Just $ realToFrac d
      in (time, (fret, len))
      )
    $ V.toList
    $ songData song
  in RBFile.Song
    { s_tempos = tempos
    , s_signatures = sigs
    , s_tracks = mempty
      { RBFile.onyxParts = Map.singleton RBFile.FlexGuitar mempty
        { RBFile.onyxPartGuitar = mempty
          { F.fiveDifficulties = Map.singleton Expert gtr
          }
        }
      }
    }

importFreetar :: (SendMessage m, MonadIO m) => FilePath -> Import m
importFreetar sng level = do
  song <- stackIO (B.readFile sng) >>= parseSong . decodeGeneral
  let props = songProperties song
  return SongYaml
    { _metadata = def'
      { _title        = ftTitle props
      , _artist       = ftArtist props
      , _album        = ftAlbum props
      , _year         = ftYear props >>= readMaybe . T.unpack
      , _comments     = []
      , _fileAlbumArt = Nothing
      }
    , _jammit = mempty
    , _global = def'
      { _backgroundVideo = Nothing
      , _fileBackgroundImage = Nothing
      , _fileMidi = SoftFile "notes.mid" $ SoftChart $ case level of
        ImportFull  -> songToMidi song
        ImportQuick -> emptyChart
      , _fileSongAnim = Nothing
      }
    , _audio = HM.fromList $ toList $ flip fmap (ftMusicFileName props) $ \f -> let
      f' = takeDirectory sng </> T.unpack f
      audio = AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile ("audio" <.> takeExtension f') $ SoftReadable $ fileReadable f'
        , _rate = Nothing
        , _channels = 2 -- just assuming
        }
      in ("song", audio)
    , _plans = HM.fromList $ toList $ flip fmap (ftMusicFileName props) $ \_ -> let
      plan = Plan
        { _song = Just $ PlanAudio (Input $ Named "song") [] []
        , _countin = Countin []
        , _planParts = Parts HM.empty
        , _crowd = Nothing
        , _planComments = []
        , _tuningCents = 0
        , _fileTempo = Nothing
        }
      in ("freetar", plan)
    , _targets = HM.empty
    , _parts = Parts $ HM.singleton RBFile.FlexGuitar def
      { partGRYBO = Just def
      }
    }
