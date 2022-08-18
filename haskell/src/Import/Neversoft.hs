{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Import.Neversoft where

import           Audio                          (Audio (..))
import           Config
import           Control.Applicative            ((<|>))
import           Control.Monad                  (forM, when)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import           Data.Char                      (isDigit)
import           Data.Default.Class             (def)
import qualified Data.HashMap.Strict            as HM
import           Data.List.NonEmpty             (NonEmpty ((:|)))
import           Data.Maybe                     (catMaybes, listToMaybe)
import           Data.SimpleHandle
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as TE
import           Genre                          (displayWoRGenre)
import           Import.Base
import           Import.GuitarHero2             (ImportMode (..))
import           Neversoft.Audio                (decryptFSB', gh3Decrypt)
import           Neversoft.Checksum             (qbKeyCRC)
import           Neversoft.GH3                  (gh3ToMidi, parseMidQB)
import           Neversoft.Metadata
import           Neversoft.Note
import           Neversoft.Pak
import           Neversoft.QB                   (parseQB)
import qualified RockBand.Codec.File            as RBFile
import           Sound.FSB                      (getFSBStreamBytes, parseFSB,
                                                 splitFSBStreams,
                                                 splitFSBStreams')
import           STFS.Package                   (runGetM)
import           Text.Read                      (readMaybe)

importNeversoftGH :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importNeversoftGH src folder = let
  testGH3   name = "dl"        `T.isPrefixOf` name && ".pak.xen" `T.isSuffixOf` name
  testGHWT  name = "adl"       `T.isPrefixOf` name && ".pak.xen" `T.isSuffixOf` name
  testGH5   name = "bmanifest" `T.isPrefixOf` name && ".pak.xen" `T.isSuffixOf` name
  testGHWoR name = "cmanifest" `T.isPrefixOf` name && ".pak.xen" `T.isSuffixOf` name
  in if
    | any (testGH3   . T.toLower . fst) $ folderFiles folder -> importGH3 src folder
    | any (testGHWT  . T.toLower . fst) $ folderFiles folder -> fatal
      "Guitar Hero World Tour is not supported yet. (Soon?)"
    | any (testGH5   . T.toLower . fst) $ folderFiles folder -> importGH5WoR src folder
    | any (testGHWoR . T.toLower . fst) $ folderFiles folder -> importGH5WoR src folder
    | otherwise                                              -> fatal
      "Unrecognized Neversoft Guitar Hero package"

-- Imports DLC STFS files for Guitar Hero 5 and Guitar Hero: Warriors of Rock.
-- TODO Does not import GH5 songs from mixed GH5 + GHWoR packages like the Kiss pack
-- TODO Some other not working GH5 stuff like All Hallows Eve pack
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
                  streams <- stackIO $ parseFSB dec >>= splitFSBStreams
                  forM (zip ([0..] :: [Int]) streams) $ \(i, stream) -> do
                    (streamData, ext) <- stackIO $ getFSBStreamBytes stream
                    return (name <> "." <> T.pack (show i) <> "." <> ext, streamData)
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

importGH3 :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3 src folder = do
  let texts = [ r | (name, r) <- folderFiles folder, "_text.pak" `T.isInfixOf` T.toLower name ]
      findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
  qbSections <- fmap concat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    case readGH3TextPakQB bs of
      Left  err      -> warn err >> return []
      Right contents -> return $ textPakSongStructs contents
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoGH3 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap concat $ forM songInfo $ \info -> do
    let songPakName platform = TE.decodeUtf8 (gh3Name info) <> "_song.pak." <> platform
    case findFolded (songPakName "xen") <|> findFolded (songPakName "ps3") of
      Nothing      -> return [] -- song which is listed in the database, but not actually in this package
      Just pakFile -> let
        importModes = if gh3UseCoopNotetracks info
          then [ImportSolo, ImportCoop]
          else [ImportSolo]
        in return $ flip map importModes $ \mode level -> do
          when (level == ImportFull) $ do
            lg $ "Importing GH song " <> show (gh3Name info) <> " from: " <> src
          -- Dragonforce DLC has rhythm tracks in coop, but hidden bass tracks in non-coop. (use coop notetracks = true)
          -- The Pretender has rhythm track copied to non-coop rhythm track, and non-coop rhythm audio is silent. (should ignore)
          -- We Three Kings is only DLC with rhythm coop but use coop notetracks = false.
          let thisCoopRhythm = gh3CoopRhythm info && (mode == ImportCoop || not (gh3UseCoopNotetracks info))
              coopPart       = if thisCoopRhythm then RBFile.FlexExtra "rhythm" else RBFile.FlexBass
          midiFixed <- case level of
            ImportFull -> do
              nodes <- fmap splitPakNodes $ stackIO $ useHandle pakFile handleToByteString
              let isMidQB node = nodeFileType node == qbKeyCRC ".qb" && nodeFilenameCRC node == qbKeyCRC (gh3Name info)
              case filter (isMidQB . fst) nodes of
                [] -> fatal "Couldn't find chart .qb file"
                (_, bs) : _ -> do
                  midQB <- runGetM parseQB bs >>= parseMidQB (gh3Name info)
                  return $ gh3ToMidi
                    (mode == ImportCoop && gh3UseCoopNotetracks info)
                    thisCoopRhythm
                    mempty
                    midQB
            ImportQuick -> return emptyChart
          let midiOnyx = midiFixed
                { RBFile.s_tracks = RBFile.fixedToOnyx $ RBFile.s_tracks midiFixed
                }
          audio <- case level of
            ImportQuick -> return []
            ImportFull -> do
              let getName platform = TE.decodeUtf8 (gh3Name info) <> ".fsb." <> platform
                  xen = getName "xen"
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
              dec <- case gh3Decrypt bs of
                Nothing  -> fatal $ "Couldn't decrypt audio file: " <> show name
                Just dec -> return dec
              streams <- stackIO $ parseFSB dec >>= splitFSBStreams'
              forM (zip ([0..] :: [Int]) streams) $ \(i, stream) -> do
                (streamData, ext) <- stackIO $ getFSBStreamBytes stream
                return (name <> "." <> T.pack (show i) <> "." <> ext, streamData)
          -- Not sure these are dB but it would make sense.
          -- Also does guitar volume also apply to bass/rhythm?
          let guitarVol = replicate 2 $ realToFrac $ gh3GuitarPlaybackVolume info
              bandVol   = replicate 2 $ realToFrac $ gh3BandPlaybackVolume   info
          return SongYaml
            { _metadata = def'
              { _title = Just $ case mode of
                ImportSolo -> gh3Title info
                ImportCoop -> gh3Title info <> " (Co-op)"
              , _artist = Just $ gh3Artist info
              , _year = readMaybe $ T.unpack $ T.takeWhileEnd isDigit $ gh3Year info
              , _fileAlbumArt = Nothing
              }
            , _global = def'
              { _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
              , _fileSongAnim = Nothing
              , _backgroundVideo = Nothing
              , _fileBackgroundImage = Nothing
              }
            , _audio = HM.fromList $ do
              (name, bs) <- audio
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
            , _plans = case audio of
              -- note, even paint it black has a rhythm track.
              -- this should apply to all songs where gh3UseCoopNotetracks is false
              [guitar, _preview, rhythm, song] -> HM.singleton "gh" Plan
                { _song = Just $ PlanAudio (Input $ Named $ fst song) [] bandVol
                , _countin = Countin []
                , _planParts = Parts $ HM.fromList
                  [ (RBFile.FlexGuitar, PartSingle $ PlanAudio (Input $ Named $ fst guitar) [] guitarVol)
                  , (coopPart         , PartSingle $ PlanAudio (Input $ Named $ fst rhythm) [] guitarVol)
                  ]
                , _crowd = Nothing
                , _planComments = []
                , _tuningCents = 0
                , _fileTempo = Nothing
                }
              -- this is seen in SanicStudios customs? how does this work
              [song, guitar, _preview] -> HM.singleton "gh" Plan
                { _song = Just $ PlanAudio (Input $ Named $ fst song) [] bandVol
                , _countin = Countin []
                , _planParts = Parts $ HM.fromList
                  [ (RBFile.FlexGuitar, PartSingle $ PlanAudio (Input $ Named $ fst guitar) [] guitarVol)
                  ]
                , _crowd = Nothing
                , _planComments = []
                , _tuningCents = 0
                , _fileTempo = Nothing
                }
              -- this should apply to all songs where gh3UseCoopNotetracks is true
              [coopGuitar, coopRhythm, coopSong, guitar, _preview, rhythm, song] -> let
                (thisGuitar, thisCoop, thisSong) = if mode == ImportCoop && gh3UseCoopNotetracks info
                  then (coopGuitar, coopRhythm, coopSong)
                  else (guitar    , rhythm    , song    )
                in HM.singleton "gh" Plan
                  { _song = Just $ PlanAudio (Input $ Named $ fst thisSong) [] bandVol
                  , _countin = Countin []
                  , _planParts = Parts $ HM.fromList
                    [ (RBFile.FlexGuitar, PartSingle $ PlanAudio (Input $ Named $ fst thisGuitar) [] guitarVol)
                    , (coopPart         , PartSingle $ PlanAudio (Input $ Named $ fst thisCoop  ) [] guitarVol)
                    ]
                  , _crowd = Nothing
                  , _planComments = []
                  , _tuningCents = 0
                  , _fileTempo = Nothing
                  }
              _ -> HM.empty -- TODO warn or something (if ImportFull)
            , _targets = HM.empty
            , _parts = Parts $ HM.fromList
              [ (RBFile.FlexGuitar, def { partGRYBO = Just def })
              , (coopPart         , def { partGRYBO = Just def })
              ]
            }
