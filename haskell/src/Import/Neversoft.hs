{-# LANGUAGE OverloadedStrings #-}
module Import.Neversoft where

import           Audio                          (Audio (..))
import           Config
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, when)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import qualified Data.Conduit.Audio             as CA
import           Data.Default.Class             (def)
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe                     (catMaybes, listToMaybe)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Genre                          (displayWoRGenre)
import           Import.Base
import           Neversoft.Audio                (decryptFSB', readFSB)
import           Neversoft.Metadata
import           Neversoft.Note
import qualified RockBand.Codec.File            as RBFile

-- | Imports DLC STFS files for (eventually, Guitar Hero 5) and Guitar Hero: Warriors of Rock.
importGH5WoR :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH5WoR src folder = do
  let texts = [ r | (name, r) <- folderFiles folder, "_text.pak" `T.isInfixOf` T.toLower name ]
      findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
  qbSections <- fmap concat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    case readTextPakQB bs of
      Left  err      -> warn err >> return []
      Right contents -> return $ textPakSongStructs contents
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoStruct items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap catMaybes $ forM songInfo $ \info -> do
    let songPakName platform = "b" <> TE.decodeUtf8 (songName info) <> "_song.pak." <> platform
    case findFolded (songPakName "xen") <|> findFolded (songPakName "ps3") of
      Nothing      -> return Nothing -- song which is listed in the database, but not actually in this package
      Just pakFile -> do
        return $ Just $ \level -> do
          when (level == ImportFull) $ do
            lg $ "Importing GH song " <> show (songName info) <> " from: " <> src
            lg "Converting audio may take a while!"
            lg "Neversoft GH audio may also not play correctly in 3D preview at the moment."
          midiFixed <- case level of
            ImportFull -> do
              (bank, songPak) <- stackIO (useHandle pakFile handleToByteString) >>= loadSongPak
              return $ ghToMidi bank songPak
            ImportQuick -> return emptyChart
          let midiOnyx = midiFixed
                { RBFile.s_tracks = RBFile.fixedToOnyx $ RBFile.s_tracks midiFixed
                }
              -- TODO fix this on PS3 to just split .fsb.ps3 into individual mp3 files
              getAudio dummyCount getName = case level of
                ImportFull -> do
                  let xen = getName "xen"
                      ps3 = getName "ps3"
                  (bs, name) <- case findFolded xen of
                    Just r -> do
                      bs <- stackIO $ useHandle r handleToByteString
                      return (bs, xen)
                    Nothing -> case findFolded ps3 of
                      Just r -> do
                        bs <- stackIO $ useHandle r handleToByteString
                        return (bs, ps3)
                      Nothing -> fatal $ "Couldn't find audio file (xen/ps3): " <> show xen
                  dec <- case decryptFSB' (T.unpack name) $ BL.toStrict bs of
                    Nothing  -> fatal $ "Couldn't decrypt audio file: " <> show name
                    Just dec -> return dec
                  stackIO $ readFSB dec
                ImportQuick -> return $ CA.AudioSource (return ()) 48000 dummyCount 0
          src1 <- getAudio 8 $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_1.fsb." <> platform
          src2 <- getAudio 6 $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_2.fsb." <> platform
          src3 <- getAudio 4 $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_3.fsb." <> platform
          (kickChans, snareChans, kitChansMix) <- case CA.channels src1 of
            8 -> return ([0, 1], [2, 3], [4, 5] :| [[6, 7]]) -- last 4 are 2 tom channels, 2 cymbal channels
            n -> fatal $ "Unrecognized number of drum audio channels: " <> show n
          (gtrChans, bassChans, voxChans) <- case CA.channels src2 of
            6 -> return ([0, 1], [2, 3], [4, 5])
            n -> fatal $ "Unrecognized number of guitar/bass/vocal audio channels: " <> show n
          (songChans, crowdChans) <- case CA.channels src3 of
            4 -> return ([0, 1], [2, 3])
            n -> fatal $ "Unrecognized number of backing/crowd audio channels: " <> show n
          let chans name cs = PlanAudio (Channels (map Just cs) $ Input $ Named name) [] []
              chansMix name inputs = PlanAudio (Mix $ fmap (\cs -> Channels (map Just cs) $ Input $ Named name) inputs) [] []
              readTier 0 _ = Nothing
              readTier n f = Just $ f $ Rank $ fromIntegral n * 50
          return SongYaml
            { _metadata = def'
              { _title = Just $ snd $ songTitle info
              , _artist = Just $ snd $ songArtist info
              , _year = Just $ songYear info
              , _album = fmap snd $ songAlbumTitle info
              , _fileAlbumArt = Nothing
              , _genre = displayWoRGenre <$> songGenre info
              }
            , _global = def'
              { _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
              , _fileSongAnim = Nothing
              , _backgroundVideo = Nothing
              , _fileBackgroundImage = Nothing
              }
            , _audio = HM.fromList
              [ ("audio1", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio1.wav" $ SoftAudio src1
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src1
                })
              , ("audio2", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio2.wav" $ SoftAudio src2
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src2
                })
              , ("audio3", AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile "audio3.wav" $ SoftAudio src3
                , _commands = []
                , _rate = Nothing
                , _channels = CA.channels src3
                })
              ]
            , _jammit = HM.empty
            , _plans = HM.singleton "gh" Plan
              { _song = Just $ chans "audio3" songChans
              , _countin = Countin []
              , _planParts = Parts $ HM.fromList
                [ (RBFile.FlexDrums, PartDrumKit
                  (Just $ chans "audio1" kickChans)
                  (Just $ chans "audio1" snareChans)
                  (chansMix "audio1" kitChansMix)
                  )
                , (RBFile.FlexGuitar, PartSingle $ chans "audio2" gtrChans)
                , (RBFile.FlexBass  , PartSingle $ chans "audio2" bassChans)
                , (RBFile.FlexVocal , PartSingle $ chans "audio2" voxChans)
                ]
              , _crowd = Just $ chans "audio3" crowdChans
              , _planComments = []
              , _tuningCents = 0
              , _fileTempo = Nothing
              }
            , _targets = HM.empty
            , _parts = Parts $ HM.fromList
              [ (RBFile.FlexGuitar, def
                { partGRYBO = readTier (songTierGuitar info) $ \diff -> def { gryboDifficulty = diff }
                })
              , (RBFile.FlexBass, def
                { partGRYBO = readTier (songTierBass info) $ \diff -> def { gryboDifficulty = diff }
                })
              , (RBFile.FlexDrums, def
                { partDrums = readTier (songTierDrums info) $ \diff -> PartDrums
                  { drumsMode        = Drums5
                  , drumsDifficulty  = diff
                  -- TODO are there any WoR songs that have ghost note X+ with no double kicks?
                  -- if so, do they have `double_kick` on?
                  , drumsKicks       = if songDoubleKick info then KicksBoth else Kicks1x
                  , drumsFixFreeform = True
                  , drumsKit         = HardRockKit
                  , drumsLayout      = StandardLayout
                  , drumsFallback    = FallbackGreen
                  , drumsFileDTXKit  = Nothing
                  , drumsFullLayout  = FDStandard
                  }
                })
              , (RBFile.FlexVocal, def
                { partVocal = readTier (songTierVocals info) $ \diff -> PartVocal
                  { vocalDifficulty = diff
                  , vocalCount      = Vocal1
                  , vocalGender     = Nothing -- TODO is this stored somewhere?
                  , vocalKey        = Nothing
                  , vocalLipsyncRB3 = Nothing
                  }
                })
              ]
            }
