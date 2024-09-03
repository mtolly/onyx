{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
module Onyx.Harmonix.RockBand.Milo.Venue where

import           Control.Monad                    (forM_, guard, replicateM)
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import qualified Data.EventList.Absolute.TimeBody as ATB
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import           Data.List                        (sort)
import qualified Data.Text.Encoding               as TE
import           Data.Word
import           Numeric                          (showFFloat)
import           Numeric.NonNegative.Class        ((-|))
import           Onyx.Harmonix.RockBand.Milo.Dir
import           Onyx.MIDI.Common                 (toCommand)
import           Onyx.MIDI.Track.Events
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.Venue
import           Onyx.MIDI.Track.VenueGen         (unbuildCamera,
                                                   unbuildLighting)
import           Onyx.StackTrace                  (SendMessage, StackTraceT,
                                                   inside, logStdout, warn)
import qualified Sound.MIDI.File.Event            as E
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.Util                  as U
import           Text.Read                        (readMaybe)

data Anim t = Anim
  { animVersion    :: Word32
  , animSubversion :: Word32
  , animDTAImport  :: B.ByteString
  , animMystery1   :: Word8
  , animMystery2   :: Word32
  , animMystery3   :: Word32
  , animEnd        :: t
  , animMystery4   :: Word32
  , animTracks     :: [AnimTrack t]
  } deriving (Eq, Show, Functor, Foldable)

data AnimTrack t = AnimTrack
  { trackVersion    :: Word32
  , trackSubversion :: Word32
  , trackDomain     :: B.ByteString
  , trackMystery1   :: B.ByteString
  , trackName       :: B.ByteString
  , trackMystery2   :: Word32
  , trackName2      :: B.ByteString
  , trackMystery3   :: B.ByteString
  , trackEvents     :: [AnimEvent t]
  } deriving (Eq, Show, Functor, Foldable)

data AnimEvent t = AnimEvent
  { eventExtra :: Maybe Word32 -- 0, only in postproc track?
  , eventName  :: B.ByteString
  , eventTime  :: t
  } deriving (Eq, Show, Functor, Foldable)

parseAnim :: Get (Anim Float)
parseAnim = do
  animVersion <- getWord32be -- 0xD
  animSubversion <- getWord32be -- 0x2
  animDTAImport <- getStringBE -- "song_anim"
  -- not sure of boundaries here
  animMystery1 <- getWord8 -- always 0
  animMystery2 <- getWord32be -- always 0
  animMystery3 <- getWord32be -- always 4
  animEnd <- getFloatbe
  animMystery4 <- getWord32be -- always 1
  trackCount <- getWord32be
  animTracks <- replicateM (fromIntegral trackCount) $ do
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
      return AnimEvent{..}
    return AnimTrack{..}
  return Anim{..}

putAnim :: Anim Float -> Put
putAnim Anim{..} = do
  putWord32be animVersion
  putWord32be animSubversion
  putStringBE animDTAImport
  putWord8 animMystery1
  putWord32be animMystery2
  putWord32be animMystery3
  putFloatbe animEnd
  putWord32be animMystery4
  putWord32be $ fromIntegral $ length animTracks
  forM_ animTracks $ \AnimTrack{..} -> do
    putWord32be trackVersion
    putWord32be trackSubversion
    putStringBE trackDomain
    putByteString trackMystery1
    putStringBE trackName
    putWord32be trackMystery2
    putStringBE trackName2
    putByteString trackMystery3
    putWord32be $ fromIntegral $ length trackEvents
    forM_ trackEvents $ \AnimEvent{..} -> do
      mapM_ putWord32be eventExtra
      putStringBE eventName
      putFloatbe eventTime

animToDebugMIDI :: U.TempoMap -> U.MeasureMap -> Anim Float -> F.Song (F.RawFile U.Beats)
animToDebugMIDI tmap mmap venue = F.Song tmap mmap $ F.RawFile $ do
  trk <- venue.animTracks
  return
    $ U.setTrackName (TE.decodeLatin1 trk.trackName)
    $ U.unapplyTempoTrack tmap
    $ RTB.fromAbsoluteEventList
    $ ATB.fromPairList
    $ map (\e -> (realToFrac $ e.eventTime / 30, E.MetaEvent $ Meta.TextEvent $ TE.decodeLatin1 e.eventName))
    $ trk.trackEvents

testConvertAnim :: FilePath -> FilePath -> FilePath -> IO ()
testConvertAnim fmid fven fout = do
  res <- logStdout $ F.loadMIDI fmid
  mid <- case res of
    Left err  -> error $ show err
    Right mid -> return mid
  ven <- fmap (runGet parseAnim) $ BL.readFile fven
  let raw = animToDebugMIDI mid.tempos mid.timesigs ven `asTypeOf` mid
  F.saveMIDIUtf8 fout raw

animAdjustSpeed :: Rational -> Anim Float -> Anim Float
animAdjustSpeed 1 = id
animAdjustSpeed r = let
  r' = realToFrac r
  in fmap (/ r')

-- We'll see if we need to do anything more (new events in the added space or something)
animPad :: U.Seconds -> Anim Float -> Anim Float
animPad 0 = id
animPad s = let
  ticks = realToFrac s * 30
  in fmap (+ ticks)

convertFromAnim :: (SendMessage m) => Anim Float -> StackTraceT m (F.OnyxFile U.Seconds)
convertFromAnim anim = inside "Converting venue info to MIDI" $ let
  animSeconds :: Anim U.Seconds
  -- need to clamp negative to 0, negative time seen in headlikeahole
  animSeconds = (\f -> realToFrac $ max 0 $ f / 30) <$> anim
  getEvents name = do
    trk <- animSeconds.animTracks
    guard $ trk.trackName == name
    trk.trackEvents
  toRTB :: (SendMessage m, Ord a) => [(U.Seconds, Either String a)] -> StackTraceT m (RTB.T U.Seconds a)
  toRTB xs = do
    let sorted = sort xs
    forM_ sorted $ \case
      (t, Left warning) -> inside (showFFloat (Just 3) (realToFrac t :: Double) "s") $ warn warning
      _                 -> return ()
    return $ RTB.fromAbsoluteEventList $ ATB.fromPairList [ (t, x) | (t, Right x) <- sorted ]
  -- make sure that things we will translate to midi notes are valid on/off pairs.
  -- usually fine, but seen rb4 songs with singalong tracks that have singalong_off at time 0
  validEdges :: [(U.Seconds, Either String Bool)] -> [(U.Seconds, Either String Bool)]
  validEdges = go False . sort {- could avoid double sort... -} where
    go False [] = []
    go True  [] = [(lastTime, Right False)] -- end last note at hopefully an ok time? not sure if this ever happens
    go b1 (pair@(_, Right b2) : rest) = if b1 == b2
      then go b2 rest -- drop event in case of double on, double off, or off at start of track
      else pair : go b2 rest
    go b1 (pair@(_, Left _) : rest) = pair : go b1 rest
  lastTime = foldr max 0 $ toList animSeconds
  singalong name = inside ("track: " <> B8.unpack name) $ toRTB $ validEdges $ do
    e <- getEvents name
    case e.eventName of
      "singalong_off" -> [(e.eventTime, Right False)]
      "singalong_on"  -> [(e.eventTime, Right True )]
      _               -> []
  onOff name = inside ("track: " <> B8.unpack name) $ toRTB $ validEdges $ do
    e <- getEvents name
    case e.eventName of
      "off" -> [(e.eventTime, Right False)]
      "on"  -> [(e.eventTime, Right True )]
      _     -> []
  getCommands name = inside ("track: " <> B8.unpack name) $ toRTB $ do
    e <- getEvents name
    guard $ e.eventName /= "TEST_CAM"
    case toCommand [TE.decodeLatin1 e.eventName] of
      Nothing -> [(e.eventTime, Left $ "Unrecognized command: " <> show e.eventName)]
      Just x  -> [(e.eventTime, Right x)]
  camera name = do
    cam <- getCommands name
    return $ unbuildCamera 1 $ mempty
      { venueCameraRB3 = cam
      }
  -- various redundant events seen in pulseofthemaggots postproc track;
  -- cleaning up so it converts to venuegen better
  cleanPostproc :: [AnimEvent U.Seconds] -> [AnimEvent U.Seconds]
  cleanPostproc = \case
    -- AnimEvent {eventExtra = Just 0, eventName = "horror_movie_special.pp", eventTime = 5967.908}
    -- AnimEvent {eventExtra = Just 0, eventName = "film_16mm.pp", eventTime = 5967.908}
    e1 : es@(e2 : _)
      -- blurredlines.rbsong has some very close but not exactly the same events:
      --   428.0001  film_contrast_blue
      --   428.00012 film_contrast_blue
      -- so putting in some wiggle room here
      | e2.eventTime -| e1.eventTime < 0.003
      -> cleanPostproc es
    -- AnimEvent {eventExtra = Just 0, eventName = "film_16mm.pp", eventTime = 37.613712}
    -- AnimEvent {eventExtra = Just 0, eventName = "film_16mm.pp", eventTime = 122.73703}
    -- AnimEvent {eventExtra = Just 0, eventName = "film_16mm.pp", eventTime = 123.55669}
    e1 : e2 : es@(e3 : _)
      | all (== e1.eventName) [e2.eventName, e3.eventName]
      -> cleanPostproc $ e1 : es
    -- AnimEvent {eventExtra = Just 0, eventName = "ProFilm_a.pp", eventTime = 6619.8555}
    -- AnimEvent {eventExtra = Just 0, eventName = "ProFilm_a.pp", eventTime = 6667.4985}
    -- (end of track)
    [e1, e2]
      | e1.eventName == e2.eventName
      -> [e1]
    [] -> []
    e : es -> e : cleanPostproc es

  in do
    sing2        <- singalong "part2_sing"
    sing3        <- singalong "part3_sing"
    sing4        <- singalong "part4_sing"
    spotKeys     <- onOff "spot_keyboard"
    spotVocal    <- onOff "spot_vocal"
    spotGuitar   <- onOff "spot_guitar"
    spotDrums    <- onOff "spot_drums"
    spotBass     <- onOff "spot_bass"
    lightingMode <- inside "track: world_event" $ toRTB $ do
      -- only a few songs have these, not sure if they do anything post-rb2?
      e <- getEvents "world_event"
      case e.eventName of
        "verse"  -> [(e.eventTime, Right ModeVerse )]
        "chorus" -> [(e.eventTime, Right ModeChorus)]
        _        -> []
    fog <- onOff "stagekit_fog"
    postProc <- inside "track: postproc" $ toRTB $ do
      e <- cleanPostproc $ getEvents "postproc"
      case e.eventName of
        "" -> [(e.eventTime, Right V3_ProFilm_a)] -- is this right? seen empty string in ziggystardust
        _  -> case toCommand [TE.decodeLatin1 e.eventName] of
          Nothing -> [(e.eventTime, Left $ "Unrecognized postproc event: " <> show e.eventName)]
          Just pp -> [(e.eventTime, Right pp)]
    lighting <- inside "track: lightpreset" $ toRTB $ do
      e <- cleanPostproc $ getEvents "lightpreset"
      case readMaybe $ "Lighting_" <> B8.unpack e.eventName of
        Nothing    -> [(e.eventTime, Left $ "Unrecognized lighting event: " <> show e.eventName)]
        Just light -> [(e.eventTime, Right light)]
    lightingCommands <- inside "track: lightpreset_keyframe" $ toRTB $ do
      e <- getEvents "lightpreset_keyframe"
      case e.eventName of
        "first" -> [(e.eventTime, Right (LightingFirst, RBN2))]
        "prev"  -> [(e.eventTime, Right (LightingPrev , RBN2))]
        "next"  -> [(e.eventTime, Right (LightingNext , RBN2))]
        _       -> []
    bonusFX <- inside "track: world_event" $ toRTB $ do
      e <- getEvents "world_event"
      guard $ e.eventName == "bonusfx"
      return (e.eventTime, Right ())
    camera5 <- camera "shot_5"
    cameraBG <- camera "shot_bg"
    cameraBK <- camera "shot_bk"
    cameraGK <- camera "shot_gk"
    crowd <- getCommands "crowd"

    return mempty
      { F.onyxVenue = mempty
        { venueSingGuitar   = sing2
        , venueSingDrums    = sing4
        , venueSingBass     = sing3
        , venueSpotKeys     = spotKeys
        , venueSpotVocal    = spotVocal
        , venueSpotGuitar   = spotGuitar
        , venueSpotDrums    = spotDrums
        , venueSpotBass     = spotBass
        , venueLightingMode = lightingMode
        , venueFog          = fog
        }
      , F.onyxLighting = unbuildLighting 1 mempty
        { venuePostProcessRB3 = postProc
        , venueLighting = lighting
        , venueLightingCommands = lightingCommands
        , venueBonusFX = bonusFX
        }
      , F.onyxCamera = camera5
      , F.onyxCameraBG = cameraBG
      , F.onyxCameraBK = cameraBK
      , F.onyxCameraGK = cameraGK
      , F.onyxEvents = mempty
        { eventsCrowd = crowd
        }
      {-
      -- not including as these seem to be in midis still, but off by 1 quarter note?
      -- but also see note in Onyx.Import.RockBand about being smarter when merging into midi
      , F.onyxParts = Map.fromList
        [ (F.FlexGuitar, mempty
          { F.onyxPartGuitar = mempty
            { Five.fiveMood = getCommands "guitar_intensity"
            }
          })
        , (F.FlexBass, mempty
          { F.onyxPartGuitar = mempty
            { Five.fiveMood = getCommands "bass_intensity"
            }
          })
        , (F.FlexKeys, mempty
          { F.onyxPartKeys = mempty
            { Five.fiveMood = getCommands "keyboard_intensity"
            }
          })
        , (F.FlexDrums, mempty
          { F.onyxPartDrums = mempty
            { Drums.drumMood = getCommands "drum_intensity"
            }
          })
        , (F.FlexVocal, mempty
          { F.onyxPartVocals = mempty
            { Vocal.vocalMood = getCommands "mic_intensity"
            }
          })
        ]
      -}
      }
