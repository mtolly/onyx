{-# LANGUAGE BlockArguments        #-}
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
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import           Data.Default.Class                   (def)
import qualified Data.EventList.Relative.TimeBody     as RTB
import           Data.Foldable                        (toList)
import           Data.Functor                         ((<&>))
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            ((\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import qualified Data.Text                            as T
import           Onyx.Audio                           (Audio (..),
                                                       decibelDifferenceInPanRatios)
import           Onyx.Audio.VGS                       (VGS (..), VGSHeader (..),
                                                       decryptReadVGS,
                                                       splitOutVGSChannels)
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
  D.decodeDTB (decrypt oldCrypt dtb) >>= fmap (addTrippolette hdr . addGraveyardShift hdr) . readSongListGH1

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
  gravePackage = SongPackage
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
    then ("graveyardshift", gravePackage) : songs
    else songs

importGH1 :: (SendMessage m, MonadResource m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH1 path gen = do
  (hdr, arks) <- stackIO $ loadGEN gen
  folder <- loadArkFolder hdr arks
  map (\(_, pkg) -> importGH1Song pkg path folder) <$> getSongList gen

importGH1Song :: (SendMessage m, MonadResource m) => SongPackage -> FilePath -> Folder B.ByteString Readable -> Import m
importGH1Song pkg path folder level = do
  when (level == ImportFull) do
    lg $ "Importing GH1 song [" <> T.unpack pkg.song.songName <> "] from: " <> path
  let encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> B8.unpack (B8.intercalate "/" $ toList p)
  midi <- split pkg.midiFile >>= need . fmap encLatin1
  vgsData <- split (pkg.song.songName <> ".vgs") >>= need . fmap encLatin1
  F.Song tmap mmap gh1 <- case level of
    ImportFull  -> F.loadMIDIReadable midi
    ImportQuick -> return (emptyChart :: F.Song (GH1File U.Beats))
  let convmid :: F.Song (F.OnyxFile U.Beats)
      convmid = F.Song tmap mmap $ F.fixedToOnyx mempty
        { F.fixedPartGuitar = mempty
          { RB.difficulties = diffs <&> \diff -> mempty
            { RB.gems = fmap (fmap Just) diff.partGems
            }
          , RB.overdrive    = maybe RTB.empty (.partStarPower) $ Map.lookup Expert diffs
          , RB.player1      = maybe RTB.empty (.partPlayer1)   $ Map.lookup Expert diffs
          , RB.player2      = maybe RTB.empty (.partPlayer2)   $ Map.lookup Expert diffs
          , RB.fretPosition = gh1.gh1Anim.animFretPosition <&> \case
            (FretPosition p, b) -> (p        , b)
            (Fret60        , b) -> (RB.Fret59, b)
          , RB.handMap      = gh1.gh1Anim.animHandMap <&> \case
            HandMap_Default   -> RB.HandMap_Default
            HandMap_DropD2    -> RB.HandMap_DropD2
            HandMap_Solo      -> RB.HandMap_Solo
            HandMap_NoChords  -> RB.HandMap_NoChords
            HandMap_AllChords -> RB.HandMap_AllChords
          , RB.mood         = flip RTB.mapMaybe gh1.gh1Events.eventsList \case
            Event_gtr_on  -> Just Mood_play
            Event_gtr_off -> Just Mood_idle
            _             -> Nothing
          }
        , F.fixedEvents = mempty
          { RB.end = void $ RTB.filter (== Event_end) gh1.gh1Events.eventsList
          }
        }
      diffs = gh1.gh1T1Gems.gemsDifficulties
  vgs <- stackIO $ decryptReadVGS vgsData
  namedChans <- stackIO $ forConcurrently [0 .. length vgs.header.channels - 1] \i -> do
    bs <- case level of
      ImportFull  -> splitOutVGSChannels [i] vgs
      ImportQuick -> return BL.empty
    return ("vgs-" <> show i, bs)
  return SongYaml
    { metadata = def'
      { title  = Just pkg.name
      , artist = Just pkg.artist
      , cover = False -- TODO maybe make this true if it's in the main setlist (I think that's how the game does it)
      , fileAlbumArt = Nothing
      , previewStart = Just $ PreviewSeconds $ fromIntegral (fst pkg.preview) / 1000
      , previewEnd = Just $ PreviewSeconds $ fromIntegral (snd pkg.preview) / 1000
      -- TODO import loadingPhrase
      }
    , global = def'
      { fileMidi            = SoftFile "notes.mid" $ SoftChart convmid
      , fileMidiOriginal    = Just $ SoftFile "original.mid" $ SoftReadable midi
      , fileSongAnim        = Nothing
      , backgroundVideo     = Nothing
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList do
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
    , plans = HM.singleton "vgs" let
      guitarChans = map fromIntegral $ concat $ take 1 pkg.song.slip_tracks
      songChans = zipWith const [0..] pkg.song.pans \\ guitarChans
      -- in gh1, volumes are stored as gain ratios, unlike gh2 and later where they are decibels
      volumesDecibels = do
        volRatio <- pkg.song.vols
        return $ decibelDifferenceInPanRatios (1, 1) (volRatio, volRatio)
      mixChans cs = do
        presentChannels <- NE.nonEmpty do
          c <- cs
          -- in some custom isos, vgs can have fewer channels than claimed by dta
          case drop c namedChans of
            (cname, _) : _ -> [(c, T.pack cname)]
            []             -> []
        -- return Nothing if all channels are silenced by vols
        guard $ any (\c -> (pkg.song.vols !! c) > 0) cs
        Just case presentChannels of
          (c, cname) :| [] -> PansVols
            (map realToFrac [pkg.song.pans !! c])
            (map realToFrac [volumesDecibels !! c])
            (Input $ Named cname)
          _ -> PansVols
            (map realToFrac [ pkg.song.pans !! c | (c, _) <- toList presentChannels ])
            (map realToFrac [ volumesDecibels !! c | (c, _) <- toList presentChannels ])
            (Merge $ fmap (Input . Named . snd) presentChannels)
      in StandardPlan StandardPlanInfo
        { song = mixChans songChans
        , parts = Parts $ HM.fromList $ catMaybes
          [ (F.PartGuitar ,) . PartSingle <$> mixChans guitarChans
          ]
        , crowd = Nothing
        , comments = []
        , tuningCents = 0
        , fileTempo = Nothing
        }
    , targets = HM.empty
    , parts = Parts $ HM.singleton F.PartGuitar emptyPart
      { grybo = Just def
      }
    }
