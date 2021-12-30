{-# LANGUAGE OverloadedStrings #-}
module Import.Neversoft where

import Sound.FSB (splitMultitrackFSB)
import           Audio                          (Audio (..))
import           Config
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, when)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import           Data.Default.Class             (def)
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe                     (catMaybes, listToMaybe)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Genre                          (displayWoRGenre)
import           Import.Base
import           Neversoft.Audio                (decryptFSB')
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
              getAudio getName = case level of
                ImportQuick -> return []
                ImportFull -> do
                  let xen = getName "xen"
                      ps3 = getName "ps3"
                  (bs, name, fmt) <- case findFolded xen of
                    Just r -> do
                      bs <- stackIO $ useHandle r handleToByteString
                      return (bs, xen, "xma")
                    Nothing -> case findFolded ps3 of
                      Just r -> do
                        bs <- stackIO $ useHandle r handleToByteString
                        return (bs, ps3, "mp3")
                      Nothing -> fatal $ "Couldn't find audio file (xen/ps3): " <> show xen
                  dec <- case decryptFSB' (T.unpack name) $ BL.toStrict bs of
                    Nothing  -> fatal $ "Couldn't decrypt audio file: " <> show name
                    Just dec -> return dec
                  streams <- stackIO $ splitMultitrackFSB dec
                  return $ zip [ name <> "." <> T.pack (show i) <> "." <> fmt | i <- [0..] :: [Int] ] streams
          streams1 <- getAudio $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_1.fsb." <> platform
          streams2 <- getAudio $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_2.fsb." <> platform
          streams3 <- getAudio $ \platform -> "a" <> TE.decodeUtf8 (songName info) <> "_3.fsb." <> platform
          let readTier 0 _ = Nothing
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
            , _audio = HM.fromList $ do
              (name, bs) <- streams1 <> streams2 <> streams3
              let str = T.unpack name
              return (name, AudioFile AudioInfo
                { _md5 = Nothing
                , _frames = Nothing
                , _filePath = Just $ SoftFile str $ SoftReadable
                  $ makeHandle str $ byteStringSimpleHandle bs
                , _commands = []
                , _rate = Nothing
                , _channels = 2
                })
            , _jammit = HM.empty
            , _plans = HM.singleton "gh" Plan
              { _song = case streams3 of
                (x, _) : _ -> Just $ PlanAudio (Input $ Named x) [] []
                []         -> Nothing
              , _countin = Countin []
              , _planParts = Parts $ HM.fromList $ let
                drums = case map fst streams1 of
                  d1 : d2 : d3 : d4 : _ -> [(RBFile.FlexDrums, PartDrumKit
                    (Just $ PlanAudio (Input $ Named d1) [] [])
                    (Just $ PlanAudio (Input $ Named d2) [] [])
                    (PlanAudio (Mix (Input (Named d3) :| [Input $ Named d4])) [] [])
                    )]
                  _ -> []
                gbv = case map fst streams2 of
                  g : b : v : _ ->
                    [ (RBFile.FlexGuitar, PartSingle $ PlanAudio (Input $ Named g) [] [])
                    , (RBFile.FlexBass  , PartSingle $ PlanAudio (Input $ Named b) [] [])
                    , (RBFile.FlexVocal , PartSingle $ PlanAudio (Input $ Named v) [] [])
                    ]
                  _ -> []
                in drums <> gbv
              , _crowd = case drop 1 streams3 of
                (x, _) : _ -> Just $ PlanAudio (Input $ Named x) [] []
                []         -> Nothing
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
