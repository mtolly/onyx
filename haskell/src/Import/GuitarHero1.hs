{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import.GuitarHero1 where

import           Amplitude.PS2.Ark                (FileEntry (..), entryFolder,
                                                   findSplitArk', readFileEntry)
import           Audio                            (Audio (..),
                                                   decibelDifferenceInPanRatios)
import           Config
import           Control.Concurrent.Async         (forConcurrently)
import           Control.Monad                    (guard, void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8            as B8
import qualified Data.ByteString.Lazy             as BL
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import           Data.DTA.Crypt                   (decrypt, oldCrypt)
import           Data.DTA.Serialize.GH1
import           Data.DTA.Serialize.RB3           (AnimTempo (KTempoMedium))
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((\\))
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.SimpleHandle
import qualified Data.Text                        as T
import           GuitarHeroI.File
import           GuitarHeroII.Ark                 (readFileEntries,
                                                   readSongListGH1)
import           GuitarHeroII.Audio               (splitOutVGSChannels,
                                                   vgsChannelCount)
import           GuitarHeroII.PartGuitar
import           Import.Base
import           OSFiles                          (fixFileCase)
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..), Mood (..))
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>), (</>))

getSongList :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  hdr <- fixFileCase $ gen </> "MAIN.HDR"
  entries <- stackIO $ readFileEntries hdr
  arks <- stackIO $ findSplitArk' hdr
  dtb <- case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == "songs.dtb") entries of
    entry : _ -> stackIO $ useHandle (readFileEntry entry arks) handleToByteString
    []        -> fatal "Couldn't find songs.dtb"
  fmap (addTrippolette entries) $ readSongListGH1 $ D.decodeDTB $ decrypt oldCrypt dtb

-- Hacks on DTA info to be able to import Trippolette (if mid/vgs present and it's not already in dta).
addTrippolette :: [FileEntry] -> [(T.Text, SongPackage)] -> [(T.Text, SongPackage)]
addTrippolette entries songs = let
  hasTripFiles = all
    (\fn -> any (\fe -> fe_folder fe == Just "songs/advharmony" && fe_name fe == fn) entries)
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

importGH1 :: (SendMessage m, MonadResource m) => FilePath -> StackTraceT m [Import m]
importGH1 gen = map (\(_, pkg) -> importGH1Song pkg gen) <$> getSongList gen

importGH1Song :: (SendMessage m, MonadResource m) => SongPackage -> FilePath -> Import m
importGH1Song pkg gen level = do
  when (level == ImportFull) $ do
    lg $ "Importing GH1 song [" <> T.unpack (songName $ song pkg) <> "] from folder: " <> gen
  hdr <- fixFileCase $ gen </> "MAIN.HDR"
  entries <- stackIO $ readFileEntries hdr
  arks <- stackIO $ findSplitArk' hdr
  let folder = fmap (\entry -> readFileEntry entry arks) $ entryFolder entries
      encLatin1 = B8.pack . T.unpack
      split s = case splitPath s of
        Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
        Just p  -> return p
      need p = case findFile p folder of
        Just r  -> return r
        Nothing -> fatal $ "Required file not found: " <> B8.unpack (B8.intercalate "/" $ toList p)
  midi <- split (midiFile pkg) >>= need . fmap encLatin1
  vgs <- split (songName (song pkg) <> ".vgs") >>= need . fmap encLatin1
  RBFile.Song tmap mmap gh1 <- case level of
    ImportFull  -> RBFile.loadMIDIReadable midi
    ImportQuick -> return emptyChart
  let convmid :: RBFile.Song (RBFile.OnyxFile U.Beats)
      convmid = RBFile.Song tmap mmap $ RBFile.fixedToOnyx mempty
        { RBFile.fixedPartGuitar = mempty
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
        , RBFile.fixedEvents = mempty
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
    { _metadata = def'
      { _title  = Just $ name pkg
      , _artist = Just $ artist pkg
      , _cover = False -- TODO maybe make this true if it's in the main setlist (I think that's how the game does it)
      , _fileAlbumArt = Nothing
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ preview pkg) / 1000
      , _previewEnd = Just $ PreviewSeconds $ fromIntegral (snd $ preview pkg) / 1000
      }
    , _global = def'
      { _fileMidi            = SoftFile "notes.mid" $ SoftChart convmid
      , _fileSongAnim        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _audio = HM.fromList $ do
      (chanName, bs) <- namedChans
      return $ (T.pack chanName ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile (chanName <.> "vgs") $ SoftReadable $ makeHandle chanName $ byteStringSimpleHandle bs
        , _rate = Nothing
        , _channels = 1
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
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
            { _planExpr = Input $ Named $ T.pack $ fst $ namedChans !! c
            , _planPans = map realToFrac [pans (song pkg) !! c]
            , _planVols = map realToFrac [volumesDecibels !! c]
            }
          _ -> PlanAudio
            { _planExpr = Merge $ fmap (Input . Named . T.pack . fst . (namedChans !!)) cs'
            , _planPans = map realToFrac [ pans (song pkg) !! c | c <- cs ]
            , _planVols = map realToFrac [ volumesDecibels !! c | c <- cs ]
            }
      in Plan
        { _song = mixChans songChans
        , _countin = Countin []
        , _planParts = Parts $ HM.fromList $ catMaybes
          [ (RBFile.FlexGuitar ,) . PartSingle <$> mixChans guitarChans
          ]
        , _crowd = Nothing
        , _planComments = []
        , _tuningCents = 0
        , _fileTempo = Nothing
        }
    , _targets = HM.empty
    , _parts = Parts $ HM.singleton RBFile.FlexGuitar $ def
      { partGRYBO = Just def
      }
    }
