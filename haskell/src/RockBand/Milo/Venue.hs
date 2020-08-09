{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module RockBand.Milo.Venue where

import           Control.Monad                    (replicateM)
import           Control.Monad.Trans.StackTrace   (logStdout)
import           Data.Binary.Get
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Word
import qualified RockBand.Codec.File              as RBFile
import           RockBand.Milo.Dir
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

data Venue = Venue
  { venueVersion    :: Word32
  , venueSubversion :: Word32
  , venueDTAImport  :: B.ByteString
  , venueMystery    :: B.ByteString
  , venueTracks     :: [Track]
  } deriving (Eq, Show)

data Track = Track
  { trackVersion    :: Word32
  , trackSubversion :: Word32
  , trackDomain     :: B.ByteString
  , trackMystery    :: B.ByteString
  , trackName       :: B.ByteString
  , trackMystery2   :: Word32
  , trackName2      :: B.ByteString
  , trackMystery3   :: B.ByteString
  , trackEvents     :: ATB.T U.Seconds B.ByteString
  } deriving (Eq, Show)

data VenueEvent = VenueEvent
  { venueEvent :: B.ByteString
  , venueTime  :: U.Seconds
  } deriving (Eq, Show)

parseVenue :: Get Venue
parseVenue = do
  venueVersion <- getWord32be -- 0xD
  venueSubversion <- getWord32be -- 0x2
  venueDTAImport <- getStringBE -- "song_anim"
  venueMystery <- getByteString 17
    {-
      00
      00 00 00 00
      00 00 00 04
      46 6D F5 79 -- probably end timestamp
      00 00 00 01
    -}
  trackCount <- getWord32be
  venueTracks <- replicateM (fromIntegral trackCount) $ do
    trackVersion <- getWord32be -- usually 6, 2 in postproc track
    trackSubversion <- getWord32be -- usually 6, 2 in postproc track
    trackDomain <- getStringBE -- "BandDirector"
    trackMystery <- getByteString 11 -- 01 00 01 00 00 00 00 00 00 00 05
    trackName <- getStringBE -- like "bass_intensity"
    trackMystery2 <- getWord32be
    trackName2 <- getStringBE -- like "lightpreset_interp" but usually ""
    trackMystery3 <- getByteString 5
    eventCount <- getWord32be
    trackEvents <- fmap ATB.fromPairList $ replicateM (fromIntegral eventCount) $ do
      event <- getStringBE
      -- see "postproc" track where each event has 4 extra bytes of 0
      event' <- if B.null event then getStringBE else return event
      frames <- getFloatbe
      return (realToFrac $ frames / 30, event')
    return Track{..}
  return Venue{..}

venueToMIDI :: U.TempoMap -> U.MeasureMap -> Venue -> RBFile.Song (RBFile.RawFile U.Beats)
venueToMIDI tmap mmap venue = RBFile.Song tmap mmap $ RBFile.RawFile $ do
  trk <- venueTracks venue
  return
    $ U.setTrackName (B8.unpack $ trackName trk)
    $ U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ fmap (E.MetaEvent . Meta.TextEvent . B8.unpack)
    $ trackEvents trk

testConvertVenue :: FilePath -> FilePath -> FilePath -> IO ()
testConvertVenue fmid fven fout = do
  res <- logStdout $ RBFile.loadMIDI fmid
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  ven <- fmap (runGet parseVenue) $ BL.readFile fven
  let raw = venueToMIDI (RBFile.s_tempos mid) (RBFile.s_signatures mid) ven `asTypeOf` mid
  Save.toFile fout $ RBFile.showMIDIFile' raw
