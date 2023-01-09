{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
module Onyx.Harmonix.RockBand.Milo.Venue where

import           Control.Monad                    (forM_, replicateM)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Word
import           Onyx.Harmonix.RockBand.Milo.Dir
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.StackTrace                  (logStdout)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.File.Save             as Save
import qualified Sound.MIDI.Util                  as U

data Venue t = Venue
  { venueVersion    :: Word32
  , venueSubversion :: Word32
  , venueDTAImport  :: B.ByteString
  , venueMystery1   :: Word8
  , venueMystery2   :: Word32
  , venueMystery3   :: Word32
  , venueEnd        :: t
  , venueMystery4   :: Word32
  , venueTracks     :: [VenueTrack t]
  } deriving (Eq, Show, Functor)

data VenueTrack t = VenueTrack
  { trackVersion    :: Word32
  , trackSubversion :: Word32
  , trackDomain     :: B.ByteString
  , trackMystery1   :: B.ByteString
  , trackName       :: B.ByteString
  , trackMystery2   :: Word32
  , trackName2      :: B.ByteString
  , trackMystery3   :: B.ByteString
  , trackEvents     :: [VenueEvent t]
  } deriving (Eq, Show, Functor)

data VenueEvent t = VenueEvent
  { eventExtra :: Maybe Word32 -- 0, only in postproc track?
  , eventName  :: B.ByteString
  , eventTime  :: t
  } deriving (Eq, Show, Functor)

parseVenue :: Get (Venue Float)
parseVenue = do
  venueVersion <- getWord32be -- 0xD
  venueSubversion <- getWord32be -- 0x2
  venueDTAImport <- getStringBE -- "song_anim"
  -- not sure of boundaries here
  venueMystery1 <- getWord8 -- always 0
  venueMystery2 <- getWord32be -- always 0
  venueMystery3 <- getWord32be -- always 4
  venueEnd <- getFloatbe
  venueMystery4 <- getWord32be -- always 1
  trackCount <- getWord32be
  venueTracks <- replicateM (fromIntegral trackCount) $ do
    trackVersion <- getWord32be -- usually 6, 2 in postproc track
    trackSubversion <- getWord32be -- usually 6, 2 in postproc track
    trackDomain <- getStringBE -- "BandDirector"
    trackMystery1 <- getByteString 11 -- always 01 00 01 00 00 00 00 00 00 00 05
    trackName <- getStringBE -- like "bass_intensity"
    trackMystery2 <- getWord32be
    trackName2 <- getStringBE -- like "lightpreset_interp" but usually ""
    trackMystery3 <- getByteString 5
    eventCount <- getWord32be
    trackEvents <- replicateM (fromIntegral eventCount) $ do
      eventExtra <- if trackVersion == 2
        then Just <$> getWord32be
        else return Nothing
      eventName <- getStringBE
      eventTime <- getFloatbe
      return VenueEvent{..}
    return VenueTrack{..}
  return Venue{..}

putVenue :: Venue Float -> Put
putVenue Venue{..} = do
  putWord32be venueVersion
  putWord32be venueSubversion
  putStringBE venueDTAImport
  putWord8 venueMystery1
  putWord32be venueMystery2
  putWord32be venueMystery3
  putFloatbe venueEnd
  putWord32be venueMystery4
  putWord32be $ fromIntegral $ length venueTracks
  forM_ venueTracks $ \VenueTrack{..} -> do
    putWord32be trackVersion
    putWord32be trackSubversion
    putStringBE trackDomain
    putByteString trackMystery1
    putStringBE trackName
    putWord32be trackMystery2
    putStringBE trackName2
    putByteString trackMystery3
    putWord32be $ fromIntegral $ length trackEvents
    forM_ trackEvents $ \VenueEvent{..} -> do
      mapM_ putWord32be eventExtra
      putStringBE eventName
      putFloatbe eventTime

venueToMIDI :: U.TempoMap -> U.MeasureMap -> Venue Float -> F.Song (F.RawFile U.Beats)
venueToMIDI tmap mmap venue = F.Song tmap mmap $ F.RawFile $ do
  trk <- venueTracks venue
  return
    $ U.setTrackName (B8.unpack $ trackName trk)
    $ U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ map (\e -> (realToFrac $ eventTime e / 30, E.MetaEvent $ Meta.TextEvent $ B8.unpack $ eventName e))
    $ trackEvents trk

testConvertVenue :: FilePath -> FilePath -> FilePath -> IO ()
testConvertVenue fmid fven fout = do
  res <- logStdout $ F.loadMIDI fmid
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  ven <- fmap (runGet parseVenue) $ BL.readFile fven
  let raw = venueToMIDI (F.s_tempos mid) (F.s_signatures mid) ven `asTypeOf` mid
  Save.toFile fout $ F.showMIDIFile' raw

venueAdjustSpeed :: Rational -> Venue Float -> Venue Float
venueAdjustSpeed 1 = id
venueAdjustSpeed r = let
  r' = realToFrac r
  in fmap (/ r')

-- We'll see if we need to do anything more (new events in the added space or something)
venuePad :: U.Seconds -> Venue Float -> Venue Float
venuePad 0 = id
venuePad s = let
  ticks = realToFrac s * 30
  in fmap (+ ticks)
