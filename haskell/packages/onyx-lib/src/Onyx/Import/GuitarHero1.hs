{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
module Onyx.Import.GuitarHero1 where

import           Control.Concurrent.Async             (forConcurrently)
import           Control.Monad                        (guard, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource         (MonadResource)
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Default.Class                   (def)
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            ((\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import qualified Data.Text                            as T
import           Onyx.Audio                           (Audio (..),
                                                       decibelDifferenceInPanRatios)
import           Onyx.Audio.VGS                       (splitOutVGSChannels,
                                                       vgsChannelCount)
import           Onyx.Harmonix.Ark
import           Onyx.Harmonix.Ark.GH2                (readSongListGH1)
import qualified Onyx.Harmonix.DTA                    as D
import           Onyx.Harmonix.DTA.Crypt              (decrypt, oldCrypt)
import           Onyx.Harmonix.DTA.Serialize.GH1
import           Onyx.Harmonix.DTA.Serialize.RockBand (AnimTempo (KTempoMedium))
import           Onyx.Harmonix.GH1.File
import           Onyx.Harmonix.GH2.PartGuitar
import           Onyx.Import.Base
import           Onyx.MIDI.Common                     (Difficulty (..),
                                                       Mood (..))
import qualified Onyx.MIDI.Track.Events               as RB
import qualified Onyx.MIDI.Track.File                 as F
import qualified Onyx.MIDI.Track.FiveFret             as RB
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle
import qualified Sound.MIDI.Util                      as U
import           System.FilePath                      ((<.>))

getSongList :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  dtb <- case filter (\fe -> fe.folder == Just "config/gen" && fe.name == "songs.dtb") hdr.files of
    entry : _ -> do
      r <- readFileEntry hdr arks entry
      stackIO $ useHandle r handleToByteString
    []        -> fatal "Couldn't find songs.dtb"
  fmap (addTrippolette hdr . addGraveyardShift hdr) $ readSongListGH1 $ D.decodeDTB $ decrypt oldCrypt dtb

-- Hacks on DTA info to be able to import Trippolette (if mid/vgs present and it's not already in dta).
addTrippolette :: Hdr -> [(T.Text, SongPackage)] -> [(T.Text, SongPackage)]
addTrippolette hdr songs = let
  hasTripFiles = all
    (\fn -> any (\fe -> fe.folder == Just "songs/advharmony" && fe.name == fn) hdr.files)
    ["advharmony.mid", "advharmony.vgs"]
  alreadyTrip = any ((== "advharmony") . fst) songs
  tripPackage = SongPackage
    { name = "Trippolette"
    , artist = "Andi Buch"
    , song = Song
      { songName = "songs/advharmony/advharmony"
      , tracks = 1
      , slip_tracks = [[2, 3]]
      , pans = [-1, 1, -1, 1]
      , vols = [0.8, 0.8, 0.8, 0.8]
      , cores = [-1, -1, 1, 1]
      , solo = ["riffs", "standard"]
      }
    , band = Just [Left KEYBOARD_METAL, Left BASS_METAL, Left DRUMMER_METAL]
    , bank = "sfx/song_default"
    , bpm = 206
    , animTempo = KTempoMedium
    , preview = (36199, 66199) -- sensible preview section
    , midiFile = "songs/advharmony/advharmony.mid"
    , quickplay = Quickplay
      { character = Left Char_alterna
      , guitar = Left Guitar_gibson_flying_v
      , guitar_skin = Nothing
      , venue = Left Venue_theatre
      }
    }
  in if hasTripFiles && not alreadyTrip
    then ("advharmony", tripPackage) : songs
    else songs

-- Hacks on DTA info to be able to import Graveyard Shift (if mid/vgs present and it's not already in dta).
addGraveyardShift :: Hdr -> [(T.Text, SongPackage)] -> [(T.Text, SongPackage)]
addGraveyardShift hdr songs = let
  hasGraveFiles = all
    (\fn -> any (\fe -> fe.folder == Just "songs/graveyardshift" && fe.name == fn) hdr.files)
    ["graveyardshift.mid", "graveyardshift.vgs"]
  alreadyGrave = any ((== "graveyardshift") . fst) songs
  tripPackage = SongPackage
    { name = "Graveyard Shift"
    , artist = "Gurney"
    , song = Song
      { songName = "songs/graveyardshift/graveyardshift"
      , tracks = 1
      , slip_tracks = [[2, 3]]
      , pans = [-1, 1, -1, 1]
      , vols = [0.8, 0.8, 0.8, 0.8]
      , cores = [-1, -1, 1, 1]
      , solo = ["riffs", "standard"]
      }
    , band = Just [Left BASS_METAL, Left DRUMMER_METAL, Left SINGER_MALE_METAL]
    , bank = "sfx/song_default"
    , bpm = 240
    , animTempo = KTempoMedium
    , preview = (43867, 73867) -- sensible preview section
    , midiFile = "songs/graveyardshift/graveyardshift.mid"
    , quickplay = Quickplay
      { character = Left Char_alterna
      , guitar = Left Guitar_gibson_flying_v
      , guitar_skin = Nothing
      , venue = Left Venue_theatre
      }
    }
  in if hasGraveFiles && not alreadyGrave
    then ("graveyardshift", tripPackage) : songs
    else songs

importGH1 :: (SendMessage m, MonadResource m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH1 path gen = map (\(_, pkg) -> importGH1Song pkg path gen) <$> getSongList gen

importGH1Song :: (SendMessage m, MonadResource m) => SongPackage -> FilePath -> Folder T.Text Readable -> Import m
importGH1Song pkg path gen level = do
  when (level == ImportFull) $ do
    lg $ "Importing GH1 song [" <> T.unpack (songName $ song pkg) <> "] from: " <> path
  (hdr, arks) <- stackIO $ loadGEN gen
  folder <- loadArkFolder hdr arks
  let encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> B8.unpack (B8.intercalate "/" $ toList p)
  midi <- split (midiFile pkg) >>= need . fmap encLatin1
  vgs <- split (songName (song pkg) <> ".vgs") >>= need . fmap encLatin1
  F.Song tmap mmap gh1 <- case level of
    ImportFull  -> F.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  let convmid :: F.Song (F.OnyxFile U.Beats)
      convmid = F.Song tmap mmap $ F.fixedToOnyx mempty
        { F.fixedPartGuitar = mempty
          { RB.fiveDifficulties = flip fmap diffs $ \diff -> mempty
            { RB.fiveGems = partGems diff
            }
          , RB.fiveOverdrive    = maybe RTB.empty partStarPower $ Map.lookup Expert diffs
          , RB.fivePlayer1      = maybe RTB.empty partPlayer1   $ Map.lookup Expert diffs
          , RB.fivePlayer2      = maybe RTB.empty partPlayer2   $ Map.lookup Expert diffs
          , RB.fiveFretPosition = flip fmap (animFretPosition $ gh1Anim gh1) $ \case
            (FretPosition p, b) -> (p        , b)
            (Fret60        , b) -> (RB.Fret59, b)
          , RB.fiveHandMap      = flip fmap (animHandMap $ gh1Anim gh1) $ \case
            HandMap_Default   -> RB.HandMap_Default
            HandMap_DropD2    -> RB.HandMap_DropD2
            HandMap_Solo      -> RB.HandMap_Solo
            HandMap_NoChords  -> RB.HandMap_NoChords
            HandMap_AllChords -> RB.HandMap_AllChords
          , RB.fiveMood         = flip RTB.mapMaybe (eventsList $ gh1Events gh1) $ \case
            Event_gtr_on  -> Just Mood_play
            Event_gtr_off -> Just Mood_idle
            _             -> Nothing
          }
        , F.fixedEvents = mempty
          { RB.eventsEnd = void $ RTB.filter (== Event_end) $ eventsList $ gh1Events gh1
          }
        }
      diffs = gemsDifficulties $ gh1T1Gems gh1
  numChannels <- stackIO $ vgsChannelCount vgs
  namedChans <- stackIO $ forConcurrently [0 .. numChannels - 1] $ \i -> do
    bs <- case level of
      ImportFull  -> splitOutVGSChannels [i] vgs
      ImportQuick -> return BL.empty
    return ("vgs-" <> show i, bs)
  return SongYaml
    { metadata = def'
      { title  = Just $ name pkg
      , artist = Just $ artist pkg
      , cover = False -- TODO maybe make this true if it's in the main setlist (I think that's how the game does it)
      , fileAlbumArt = Nothing
      , previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ preview pkg) / 1000
      , previewEnd = Just $ PreviewSeconds $ fromIntegral (snd $ preview pkg) / 1000
      }
    , global = def'
      { fileMidi            = SoftFile "notes.mid" $ SoftChart convmid
      , fileSongAnim        = Nothing
      , backgroundVideo     = Nothing
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList $ do
      (chanName, bs) <- namedChans
      return $ (T.pack chanName ,) $ AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile (chanName <.> "vgs") $ SoftReadable $ makeHandle chanName $ byteStringSimpleHandle bs
        , rate = Nothing
        , channels = 1
        }
    , jammit = HM.empty
    , plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ concat $ take 1 $ slip_tracks $ song pkg
      songChans = zipWith const [0..] (pans $ song pkg) \\ guitarChans
      -- in gh1, volumes are stored as gain ratios, unlike gh2 and later where they are decibels
      volumesDecibels = do
        volRatio <- vols $ song pkg
        return $ decibelDifferenceInPanRatios (1, 1) (volRatio, volRatio)
      mixChans cs = do
        cs' <- NE.nonEmpty cs
        -- return Nothing if all channels are silenced by vols
        guard $ any (\c -> (vols (song pkg) !! c) > 0) cs
        Just $ case cs' of
          c :| [] -> PlanAudio
            { expr = Input $ Named $ T.pack $ fst $ namedChans !! c
            , pans = map realToFrac [pans (song pkg) !! c]
            , vols = map realToFrac [volumesDecibels !! c]
            }
          _ -> PlanAudio
            { expr = Merge $ fmap (Input . Named . T.pack . fst . (namedChans !!)) cs'
            , pans = map realToFrac [ pans (song pkg) !! c | c <- cs ]
            , vols = map realToFrac [ volumesDecibels !! c | c <- cs ]
            }
      in StandardPlan StandardPlanInfo
        { song = mixChans songChans
        , parts = Parts $ HM.fromList $ catMaybes
          [ (F.FlexGuitar ,) . PartSingle <$> mixChans guitarChans
          ]
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.FlexGuitar emptyPart
      { grybo = Just def
      }
    }
