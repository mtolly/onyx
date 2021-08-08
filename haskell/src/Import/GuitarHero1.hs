{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import.GuitarHero1 where

import           Amplitude.PS2.Ark                (FileEntry (..), entryFolder,
                                                   readFileEntry)
import           Audio                            (Audio (..))
import           Config
import           Control.Monad                    (void, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource     (MonadResource)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Char8            as B8
import qualified Data.Conduit.Audio               as CA
import           Data.Default.Class               (def)
import qualified Data.DTA                         as D
import           Data.DTA.Crypt                   (decrypt, oldCrypt)
import qualified Data.DTA.Serialize               as D
import           Data.DTA.Serialize.GH1
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List                        ((\\))
import           Data.List.NonEmpty               (NonEmpty (..))
import qualified Data.List.NonEmpty               as NE
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes)
import           Data.SimpleHandle                (findFile, handleToByteString,
                                                   splitPath, useHandle)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeLatin1)
import           GuitarHeroI.File
import           GuitarHeroII.Ark                 (readFileEntries)
import           GuitarHeroII.Audio               (readVGSReadable)
import           GuitarHeroII.PartGuitar
import           Import.Base
import qualified RockBand.Codec.Events            as RB
import qualified RockBand.Codec.File              as RBFile
import qualified RockBand.Codec.Five              as RB
import           RockBand.Common                  (Difficulty (..), Mood (..))
import qualified Sound.MIDI.Util                  as U
import           System.FilePath                  ((<.>), (</>))

getSongList :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [(T.Text, SongPackage)]
getSongList gen = do
  entries <- stackIO $ readFileEntries $ gen </> "MAIN.HDR"
  dtb <- case filter (\fe -> fe_folder fe == Just "config/gen" && fe_name fe == "songs.dtb") entries of
    entry : _ -> stackIO $ useHandle (readFileEntry entry $ gen </> "MAIN_0.ARK") handleToByteString
    []        -> fatal "Couldn't find songs.dtb"
  let editDTB d = d { D.topTree = editTree $ D.topTree d }
      editTree t = t { D.treeChunks = filter keepChunk $ D.treeChunks t }
      keepChunk = \case
        D.Parens _ -> True
        _          -> False
  fmap D.fromDictList
    $ D.unserialize (D.chunksDictList D.chunkSym D.stackChunks)
    $ editDTB $ decodeLatin1 <$> D.decodeDTB (decrypt oldCrypt dtb)

importGH1 :: (SendMessage m, MonadResource m) => FilePath -> StackTraceT m [Import m]
importGH1 gen = map (\(_, pkg) -> importGH1Song pkg gen) <$> getSongList gen

importGH1Song :: (SendMessage m, MonadResource m) => SongPackage -> FilePath -> Import m
importGH1Song pkg gen level = do
  when (level == ImportFull) $ do
    lg $ "Importing GH1 song [" <> T.unpack (songName $ song pkg) <> "] from folder: " <> gen
    lg $ "Converting audio may take a while!"
  entries <- stackIO $ readFileEntries $ gen </> "MAIN.HDR"
  let folder = fmap (\entry -> readFileEntry entry $ gen </> "MAIN_0.ARK") $ entryFolder entries
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
  srcs <- stackIO $ readVGSReadable vgs
  let namedSrcs = zip (map (\i -> "vgs-" <> show (i :: Int)) [0..]) srcs
  return SongYaml
    { _metadata = def'
      { _title  = Just $ name pkg
      , _artist = Just $ artist pkg
      , _cover = False -- TODO this doesn't appear to be in songs.dta, where is it?
      , _fileAlbumArt = Nothing
      }
    , _global = def'
      { _fileMidi            = SoftFile "notes.mid" $ SoftChart convmid
      , _fileSongAnim        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _audio = HM.fromList $ do
      (srcName, src) <- namedSrcs
      return $ (T.pack srcName ,) $ AudioFile AudioInfo
        { _md5 = Nothing
        , _frames = Nothing
        , _commands = []
        , _filePath = Just $ SoftFile (srcName <.> "wav") $ SoftAudio $ CA.mapSamples CA.fractionalSample src
        , _rate = Nothing
        , _channels = 1
        }
    , _jammit = HM.empty
    , _plans = HM.singleton "vgs" $ let
      guitarChans = map fromIntegral $ concat $ take 1 $ slip_tracks $ song pkg
      songChans = zipWith const [0..] (pans $ song pkg) \\ guitarChans
      mixChans cs = do
        cs' <- NE.nonEmpty cs
        Just $ case cs' of
          c :| [] -> PlanAudio
            { _planExpr = Input $ Named $ T.pack $ fst $ namedSrcs !! c
            , _planPans = map realToFrac [pans (song pkg) !! c]
            , _planVols = map realToFrac [vols (song pkg) !! c]
            }
          _ -> PlanAudio
            { _planExpr = Merge $ fmap (Input . Named . T.pack . fst . (namedSrcs !!)) cs'
            , _planPans = map realToFrac [ pans (song pkg) !! c | c <- cs ]
            , _planVols = map realToFrac [ vols (song pkg) !! c | c <- cs ]
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
