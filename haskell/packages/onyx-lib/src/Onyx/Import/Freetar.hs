{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
module Onyx.Import.Freetar (importFreetar) where

import           Control.Monad.Codec
import           Control.Monad.IO.Class           (MonadIO)
import           Control.Monad.Trans.Reader       (runReaderT)
import qualified Data.ByteString                  as B
import           Data.Char                        (toLower)
import           Data.Default.Class               (def)
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.Functor                     ((<&>))
import qualified Data.HashMap.Strict              as HM
import           Data.List                        (sort, sortOn)
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Onyx.Audio
import           Onyx.Codec.XML
import           Onyx.Guitar                      (HOPOsAlgorithm (..),
                                                   emitGuitar5, strumHOPOTap)
import           Onyx.Import.Base
import           Onyx.MIDI.Common                 (Difficulty (..))
import qualified Onyx.MIDI.Track.File             as F
import qualified Onyx.MIDI.Track.FiveFret         as Five
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle                 (fileReadable)
import           Onyx.Util.Text.Decode            (decodeGeneral)
import qualified Sound.MIDI.Util                  as U
import           System.Directory                 (doesFileExist, listDirectory)
import           System.FilePath                  (takeDirectory, takeExtension,
                                                   (<.>), (</>))
import           Text.Read                        (readMaybe)
import           Text.XML.Light                   (parseXMLDoc)

data Properties = Properties
  { version            :: Maybe T.Text
  , title              :: Maybe T.Text
  , artist             :: Maybe T.Text
  , album              :: Maybe T.Text
  , year               :: Maybe T.Text
  , beatsPerSecond     :: Maybe Double
  , beatOffset         :: Maybe Double
  , hammerOnTime       :: Maybe Double
  , pullOffTime        :: Maybe Double
  , difficulty         :: Maybe T.Text
  , allowableErrorTime :: Maybe Double
  , length_            :: Maybe Double
  , musicFileName      :: Maybe T.Text
  , musicDirectoryHint :: Maybe T.Text
  } deriving (Show)

instance IsInside Properties where
  insideCodec = do
    version            <- (.version)            =. childTagOpt "Version"            (parseInside' childText)
    title              <- (.title)              =. childTagOpt "Title"              (parseInside' childText)
    artist             <- (.artist)             =. childTagOpt "Artist"             (parseInside' childText)
    album              <- (.album)              =. childTagOpt "Album"              (parseInside' childText)
    year               <- (.year)               =. childTagOpt "Year"               (parseInside' childText)
    beatsPerSecond     <- (.beatsPerSecond)     =. childTagOpt "BeatsPerSecond"     (parseInside' $ milliText childText)
    beatOffset         <- (.beatOffset)         =. childTagOpt "BeatOffset"         (parseInside' $ milliText childText)
    hammerOnTime       <- (.hammerOnTime)       =. childTagOpt "HammerOnTime"       (parseInside' $ milliText childText)
    pullOffTime        <- (.pullOffTime)        =. childTagOpt "PullOffTime"        (parseInside' $ milliText childText)
    difficulty         <- (.difficulty)         =. childTagOpt "Difficulty"         (parseInside' childText)
    allowableErrorTime <- (.allowableErrorTime) =. childTagOpt "AllowableErrorTime" (parseInside' $ milliText childText)
    length_            <- (.length_)            =. childTagOpt "Length"             (parseInside' $ milliText childText)
    musicFileName      <- (.musicFileName)      =. childTagOpt "MusicFileName"      (parseInside' childText)
    musicDirectoryHint <- (.musicDirectoryHint) =. childTagOpt "MusicDirectoryHint" (parseInside' childText)
    return Properties{..}

data Note = Note
  { time     :: Double
  , duration :: Double
  , track    :: Int
  , special  :: Bool
  -- TODO HammerOnAllowed
  } deriving (Show)

instance IsInside Note where
  insideCodec = do
    time     <- (.time)     =. milliText (reqAttr "time")
    duration <- (.duration) =. milliText (reqAttr "duration")
    track    <- (.track)    =. intText   (reqAttr "track")
    special  <- (.special)  =. flag "special"
    return Note{..}

data Song = Song
  { properties :: Properties
  , data_      :: V.Vector Note
  } deriving (Show)

instance IsInside Song where
  insideCodec = do
    properties <- (.properties) =. childTag "Properties" (parseInside' insideCodec)
    data_      <- (.data_)      =. childTag "Data"
      (parseInside' $ bareList $ isTag "Note" $ parseInside' insideCodec)
    return Song{..}

parseSong :: (SendMessage m) => T.Text -> StackTraceT m Song
parseSong xml = do
  elt <- maybe (fatal "Couldn't parse XML") return $ parseXMLDoc xml
  mapStackTraceT (`runReaderT` elt) $ codecIn $ isTag "Song" $ parseInside' insideCodec

-- runs of special=1 notes turned into star power phrases; output is sorted (pos, len) in seconds
specialPhrases :: Song -> [(Double, Double)]
specialPhrases song = let
  go = \case
    []     -> []
    n : ns -> if n.special
      then let
        (special, notSpecial) = span (.special) ns
        lastEnd = foldr max (n.time + n.duration)
          [ s.time + s.duration | s <- special ]
        phraseEnd = case notSpecial of
          []    ->              lastEnd + 0.25
          x : _ -> min x.time $ lastEnd + 0.25
        len = phraseEnd - n.time
        in if len <= 0
          then go ns
          else (n.time, len) : go notSpecial
      else go ns
  in go $ sortOn (.time) $ V.toList song.data_

songToMidi :: Song -> F.Song (F.OnyxFile U.Beats)
songToMidi song = let
  tempos = U.tempoMapFromBPS RTB.empty -- "BeatsPerSecond" is probably useless
  sigs = U.measureMapFromTimeSigs U.Truncate RTB.empty
  threshold :: U.Seconds
  threshold = case NE.nonEmpty $ catMaybes [song.properties.hammerOnTime, song.properties.pullOffTime] of
    Nothing -> 0.25
    Just ne -> toSeconds $ maximum ne
  toSeconds :: Double -> U.Seconds
  toSeconds = realToFrac
  gtr = emitGuitar5 $ U.unapplyTempoTrack tempos $ strumHOPOTap HOPOsRBGuitar threshold
    $ RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ sort
    $ map (\n -> let
      time = toSeconds n.time
      fret = Just $ toEnum n.track
      len = case n.duration of
        0 -> Nothing
        d -> Just $ U.unapplyTempoMap tempos (time + toSeconds d) - U.unapplyTempoMap tempos time
      in (time, (fret, len))
      )
    $ V.toList song.data_
  in F.Song
    { tempos = tempos
    , timesigs = sigs
    , tracks = mempty
      { F.onyxParts = Map.singleton F.PartGuitar mempty
        { F.onyxPartGuitar = mempty
          { Five.difficulties = Map.singleton Expert gtr
          , Five.overdrive
            = U.unapplyTempoTrack tempos
            $ RTB.fromAbsoluteEventList
            $ ATB.fromPairList
            $ specialPhrases song >>= \(pos, len) ->
              [(toSeconds pos, True), (toSeconds $ pos + len, False)]
          }
        }
      }
    }

importFreetar :: (SendMessage m, MonadIO m) => FilePath -> Import m
importFreetar sng level = do
  song <- stackIO (B.readFile sng) >>= parseSong . decodeGeneral
  let props = song.properties
  -- music path is frequently wrong, so as a backup,
  -- check for a single audio file in the folder
  musicPath <- case level of
    ImportQuick -> return Nothing
    ImportFull -> case props.musicFileName of
      Nothing -> return Nothing
      Just f  -> do
        let sngFolder = takeDirectory sng
            reloc = sngFolder </> T.unpack f
        stackIO (doesFileExist reloc) >>= \case
          True -> return $ Just reloc
          False -> do
            entries <- stackIO $ listDirectory sngFolder
            let isAudio file = elem (map toLower $ takeExtension file) [".mp3", ".ogg", ".wav"]
            case filter isAudio entries of
              [match] -> do
                warn $ "Freetar song's audio path does not exist; using lone audio file in song folder: " <> match
                return $ Just $ sngFolder </> match
              []      -> fatal
                "Freetar song's audio path does not exist; no audio found in song folder"
              _       -> fatal
                "Freetar song's audio path does not exist; multiple audio files found in song folder"
  return SongYaml
    { metadata = def'
      { title        = props.title
      , artist       = props.artist
      , album        = props.album
      , year         = props.year >>= readMaybe . T.unpack
      , comments     = []
      , fileAlbumArt = Nothing
      }
    , jammit = mempty
    , global = def'
      { backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      , fileMidi = SoftFile "notes.mid" $ SoftChart case level of
        ImportFull  -> songToMidi song
        ImportQuick -> emptyChart
      , fileMidiOriginal = Nothing
      , fileSongAnim = Nothing
      }
    , audio = HM.fromList $ toList $ musicPath <&> \f ->
      ("song", AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile ("audio" <.> takeExtension f) $ SoftReadable $ fileReadable f
        , rate = Nothing
        , channels = 2 -- just assuming
        }
      )
    , plans = HM.fromList $ toList $ musicPath <&> \_ ->
      ("freetar", StandardPlan StandardPlanInfo
        { song = Just $ Input $ Named "song"
        , parts = Parts HM.empty
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
      )
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.PartGuitar emptyPart
      { grybo = Just def
      }
    }
