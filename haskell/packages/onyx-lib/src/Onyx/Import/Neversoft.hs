{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImplicitParams        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.Neversoft where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Crypto.Cipher.AES
import           Crypto.Cipher.Types
import           Crypto.Error
import           Data.Bifunctor                   (first)
import qualified Data.ByteString                  as B
import qualified Data.ByteString.Lazy             as BL
import           Data.Char                        (isDigit)
import           Data.Default.Class               (def)
import qualified Data.EventList.Relative.TimeBody as RTB
import           Data.Foldable                    (toList)
import qualified Data.HashMap.Strict              as HM
import           Data.List.NonEmpty               (NonEmpty ((:|)))
import qualified Data.List.NonEmpty               as NE
import           Data.List.Split                  (chunksOf)
import qualified Data.Map                         as Map
import           Data.Maybe                       (catMaybes, fromMaybe,
                                                   listToMaybe, mapMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           GHC.ByteOrder
import           Numeric                          (showHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.FSB                   (getFSBStreamBytes, parseFSB,
                                                   splitFSBStreams,
                                                   splitFSBStreams')
import           Onyx.Genre                       (displayWoRGenre, qbWoRGenre)
import           Onyx.Import.Base
import           Onyx.Import.GuitarHero2          (ImportMode (..))
import           Onyx.MIDI.Common                 (Difficulty (..))
import           Onyx.MIDI.Track.Drums.True       (tdDifficulties, tdGems,
                                                   tdKick2)
import qualified Onyx.MIDI.Track.File             as F
import           Onyx.MIDI.Track.FiveFret         (nullFive)
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.Crypt             (decryptFSB', gh3Decrypt)
import           Onyx.Neversoft.GH3.Metadata
import           Onyx.Neversoft.GH3.MidQB         (gh3ToMidi, parseMidQB)
import           Onyx.Neversoft.GH4.Metadata
import           Onyx.Neversoft.GH4.MidQB         (gh4ToMidi, parseGH4MidQB,
                                                   worldTourDiscMarkers)
import           Onyx.Neversoft.GH5.Metadata
import           Onyx.Neversoft.GH5.Note
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.PS2
import           Onyx.Neversoft.QB                (QBSection (..),
                                                   QBStructItem (..),
                                                   QSResult (..), parseQB,
                                                   parseSGHStruct)
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Binary                 (runGetM)
import           Onyx.Util.Handle
import           Text.Read                        (readMaybe)

importNeversoftGH :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importNeversoftGH src folder = let
  -- we check all games and combine their results,
  -- so that Death Magnetic imports both the GH3 and GHWT versions
  testGH3   name = "dl"        `T.isPrefixOf` name && pakSuffix name
  testGHWT  name = "adl"       `T.isPrefixOf` name && pakSuffix name
  testGH5   name = "bmanifest" `T.isPrefixOf` name && pakSuffix name
  testGHWoR name = "cmanifest" `T.isPrefixOf` name && pakSuffix name
  pakSuffix name = ".pak.xen" `T.isSuffixOf` name || ".pak.ps3" `T.isSuffixOf` name
  allNames = map (T.toLower . fst) $ folderFiles folder
  in fmap concat $ sequence
    [ if any testGH3                                   allNames then importGH3DLC src folder else return []
    , if any testGHWT                                  allNames then importGH4DLC src folder else return []
    , if any (\name -> testGH5 name || testGHWoR name) allNames then importGH5WoR src folder else return []
    ]

importWoRDisc :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importWoRDisc src folder = do
  let qsPath  = "data" :| ["compressed", "pak", "qs.pak.xen"]
      pakPath = "data" :| ["compressed", "pak", "qb.pak.xen"]
      pabPath = "data" :| ["compressed", "pak", "qb.pab.xen"]
  qs  <- maybe (fatal "Couldn't find qs.pak") (\r -> stackIO $ useHandle r handleToByteString) (findFileCI qsPath  folder)
  pak <- maybe (fatal "Couldn't find qb.pak") (\r -> stackIO $ useHandle r handleToByteString) (findFileCI pakPath folder)
  pab <- maybe (fatal "Couldn't find qb.pab") (\r -> stackIO $ useHandle r handleToByteString) (findFileCI pabPath folder)
  qbSections <- fmap textPakSongStructs $ readTextPakQB pak (Just pab) (Just qs)
  music <- maybe (fatal "Couldn't find music folder") return $ findFolderCI ["data", "music"] folder
  songs <- maybe (fatal "Couldn't find song pak folder") return $ findFolderCI ["data", "compressed", "SONGS"] folder
  importGH5WoRSongStructs True src (music <> songs) qbSections

-- Imports DLC STFS files for Guitar Hero 5 and Guitar Hero: Warriors of Rock.
-- TODO Does not import GH5 songs from mixed GH5 + GHWoR packages like the Kiss pack
-- TODO Some other not working GH5 stuff like All Hallows Eve pack
importGH5WoR :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH5WoR src folder = do
  let texts = [ r | (name, r) <- folderFiles folder, "_text.pak" `T.isInfixOf` T.toLower name ]
  qbSections <- fmap concat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    errorToWarning (readTextPakQB bs Nothing Nothing) >>= \case
      Nothing       -> return []
      Just contents -> return $ textPakSongStructs contents
  importGH5WoRSongStructs False src folder qbSections

importGH5WoRSongStructs
  :: (SendMessage m, MonadIO m)
  => Bool -> FilePath -> Folder T.Text Readable -> [TextPakSongStruct] -> StackTraceT m [Import m]
importGH5WoRSongStructs isDisc src folder qbSections = do
  let findFolded path = findFileCI (pure path) folder
  songInfo <- fmap concat $ forM qbSections $ \song -> do
    case parseSongInfoStruct $ songData song of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap catMaybes $ forM songInfo $ \info -> do
    let songPakName platform = (if isDisc then "" else "b") <> TE.decodeUtf8 (songName info) <> "_song.pak." <> platform
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
                { F.s_tracks = F.fixedToOnyx $ F.s_tracks midiFixed
                }
              audioPrefix = if isDisc then "" else "a"
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
          streams1 <- getAudio $ \platform -> audioPrefix <> TE.decodeUtf8 (songName info) <> "_1.fsb." <> platform
          streams2 <- getAudio $ \platform -> audioPrefix <> TE.decodeUtf8 (songName info) <> "_2.fsb." <> platform
          streams3 <- getAudio $ \platform -> audioPrefix <> TE.decodeUtf8 (songName info) <> "_3.fsb." <> platform
          let readTier 0 _ = Nothing
              readTier n f = Just $ f $ Rank $ fromIntegral n * 50
              adjustedInput
                = (case songOverallSongVolume info of 0 -> id; db -> PansVols [-1, 1] [db, db])
                . Input . Named
          return SongYaml
            { metadata = def'
              { title = Just $ snd $ songTitle info
              , artist = Just $ snd $ songArtist info
              , year = Just $ songYear info
              , album = fmap snd $ songAlbumTitle info
              , fileAlbumArt = Nothing
              , genre = displayWoRGenre <$> songGenre info
              }
            , global = def'
              { fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
              , fileSongAnim = Nothing
              , backgroundVideo = Nothing
              , fileBackgroundImage = Nothing
              }
            , audio = HM.fromList $ do
              (name, bs) <- streams1 <> streams2 <> streams3
              let str = T.unpack name
              return (name, AudioFile AudioInfo
                { md5 = Nothing
                , frames = Nothing
                , filePath = Just $ SoftFile str $ SoftReadable
                  $ makeHandle str $ byteStringSimpleHandle bs
                , commands = []
                , rate = Nothing
                , channels = 2
                })
            , jammit = HM.empty
            , plans = HM.singleton "gh" $ StandardPlan StandardPlanInfo
              { song = case streams3 of
                (x, _) : _ -> Just $ adjustedInput x
                []         -> Nothing
              , parts = Parts $ HM.fromList $ let
                drums = case map fst streams1 of
                  d1 : d2 : d3 : d4 : _ -> [(F.FlexDrums, PartDrumKit
                    { kick = Just $ adjustedInput d1
                    , snare = Just $ adjustedInput d2
                    , toms = Just $ adjustedInput d3
                    , kit = adjustedInput d4
                    })]
                  _ -> []
                gbv = case map fst streams2 of
                  g : b : v : _ ->
                    [ (F.FlexGuitar, PartSingle $ adjustedInput g)
                    , (F.FlexBass  , PartSingle $ adjustedInput b)
                    , (F.FlexVocal , PartSingle $ adjustedInput v)
                    ]
                  _ -> []
                in drums <> gbv
              , crowd = case drop 1 streams3 of
                (x, _) : _ -> Just $ adjustedInput x
                []         -> Nothing
              , comments = []
              , tuningCents = songVocalsPitchScoreShift info
              , fileTempo = Nothing
              }
            , targets = HM.empty
            , parts = Parts $ HM.fromList
              [ (F.FlexGuitar, (emptyPart :: Part SoftFile)
                { grybo = readTier (songTierGuitar info) $ \diff -> def { difficulty = diff }
                })
              , (F.FlexBass, (emptyPart :: Part SoftFile)
                { grybo = readTier (songTierBass info) $ \diff -> def { difficulty = diff }
                })
              , (F.FlexDrums, (emptyPart :: Part SoftFile)
                { drums = readTier (songTierDrums info) $ \diff -> let
                  -- TODO are there any WoR songs that have ghost note X+ with no double kicks?
                  -- if so, do they have `double_kick` on?
                  -- TODO check New Low and Sad To Know (Middle Class Rut), marked as 2x but aren't
                  kicks = if songDoubleKick info then KicksBoth else Kicks1x
                  in (emptyPartDrums Drums5 kicks :: PartDrums SoftFile)
                    { difficulty = diff
                    }
                })
              , (F.FlexVocal, (emptyPart :: Part SoftFile)
                { vocal = readTier (songTierVocals info) $ \diff -> PartVocal
                  { difficulty = diff
                  , count      = Vocal1
                  , gender     = Nothing -- TODO is this stored somewhere?
                  , key        = Nothing
                  , lipsyncRB3 = Nothing
                  }
                })
              ]
            }

------------------------------------------------------------------------

importGH4Disc :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH4Disc src folder = do
  let findFolded path = case findFileCI path folder of
        Nothing -> fatal $ "Missing file in disc: " <> show path
        Just r  -> return r
  qbpak <- findFolded ("DATA" :| ["COMPRESSED", "PAK", "qb.pak.xen"]) >>= \r -> stackIO $ useHandle r handleToByteString
  qbpab <- findFolded ("DATA" :| ["COMPRESSED", "PAK", "qb.pab.xen"]) >>= \r -> stackIO $ useHandle r handleToByteString
  qspak <- findFolded ("DATA" :| ["COMPRESSED", "PAK", "qs.pak.xen"]) >>= \r -> stackIO $ useHandle r handleToByteString
  pakResults <- errorToWarning $ let
    ?endian = BigEndian
    in readGH4TextPakQBDisc (qbpak, Just qbpab) (qspak, Nothing)
  let (textSections, textAllNodes) = case pakResults of
        Nothing       -> ([], [])
        Just contents -> (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM textSections $ \(_key, items) -> do
    case parseSongInfoGH4 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap catMaybes $ forM songInfo $ \info -> do
    let pathName = TE.decodeUtf8 $ gh4Name info
        songPath = "DATA" :| ["COMPRESSED", "SONGS", pathName <> "_song.pak.xen"]
    case findFileCI songPath folder of
      Nothing       -> return Nothing -- song in pak but not present, e.g. possibly WT songs in GHM
      Just rSongPak -> errorToWarning $ do
        let getAudio path = do
              r <- findFolded $ "DATA" :| ["MUSIC", path]
              return (r, path)
        audio1 <- getAudio $ pathName <> "_1.fsb.xen"
        audio2 <- getAudio $ pathName <> "_2.fsb.xen"
        audio3 <- getAudio $ pathName <> "_3.fsb.xen"
        return $ importGH4Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH4Audio360 audio1 audio2 audio3
          , ghiText     = textAllNodes
          }

importGH4DLC :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH4DLC src folder = do
  platform <- if any (\(name, _) -> ".xen" `T.isSuffixOf` T.toLower name) $ folderFiles folder
    then return (<> ".xen")
    else if any (\(name, _) -> ".ps3" `T.isSuffixOf` T.toLower name) $ folderFiles folder
      then return (<> ".ps3")
      else fail "Couldn't determine DLC platform (.xen or .ps3)"
  let findFolded path = case findFileCI path folder of
        Nothing -> fatal $ "Missing file in DLC: " <> show path
        Just r  -> return r
      texts = do
        (name, r) <- folderFiles folder
        let name' = T.toLower name
        guard $ "a" `T.isPrefixOf` name'
          && any (`T.isSuffixOf` name') [platform "_t.pak", platform "_text.pak"]
        -- most files use _t.pak.xen, but Death Magnetic uses _text.pak.xen.
        -- also we make sure to start with "a" to ignore the GH3 DM files
        return r
  (textSections, textAllNodes) <- fmap mconcat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    errorToWarning (readGH3TextPakQBDLC bs) >>= \case
      Nothing       -> return ([], [])
      Just contents -> return (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM textSections $ \(_key, items) -> do
    case parseSongInfoGH4 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap catMaybes $ forM songInfo $ \info -> do
    let pathName   = "a" <> TE.decodeUtf8 (gh4Name info)
        songPaths  =
          [ pathName <> platform "_s.pak"    -- most songs
          , pathName <> platform "_song.pak" -- again, Death Magnetic
          ]
    case mapMaybe (\p -> findFileCI (pure p) folder) songPaths of
      []           -> return Nothing -- song which is listed in the database, but not actually in this package
      rSongPak : _ -> do
        let getAudio path = do
              r <- findFolded $ pure path
              return (r, path)
        audio1 <- getAudio $ pathName <> platform "_1.fsb"
        audio2 <- getAudio $ pathName <> platform "_2.fsb"
        audio3 <- getAudio $ pathName <> platform "_3.fsb"
        return $ Just $ importGH4Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH4Audio360 audio1 audio2 audio3
          , ghiText     = textAllNodes
          }

importGH4DiscPS2 :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH4DiscPS2 src folder = do
  let findFolded path = case findFileCI path folder of
        Nothing -> fatal $ "Couldn't find file in PS2 filesystem: " <> show path
        Just r  -> return r
  hed <- findFolded (pure "DATAP.HED") >>= \r -> stackIO $ useHandle r handleToByteString
  wad <- findFolded $ pure "DATAP.WAD"
  wadFolder <- hookUpWAD wad . applyHed HedFormatGH4 <$> runGetM parseHed hed
  let findFoldedWad path = case findFileCI path wadFolder of
        Nothing -> fatal $ "Couldn't find file inside DATAP.WAD: " <> show path
        Just r  -> return r
  qbpak <- findFoldedWad ("pak" :| ["qb.pak.ps2"]) >>= \r -> stackIO $ useHandle r handleToByteString
  qbpab <- findFoldedWad ("pak" :| ["qb.pab.ps2"]) >>= \r -> stackIO $ useHandle r handleToByteString
  qspak <- findFoldedWad ("pak" :| ["qs.pak.ps2"]) >>= \r -> stackIO $ useHandle r handleToByteString
  qspab <- findFoldedWad ("pak" :| ["qs.pab.ps2"]) >>= \r -> stackIO $ useHandle r handleToByteString
  pakResults <- inside "Reading GH4 qb.pak" $ errorToWarning $ let
    ?endian = LittleEndian
    in readGH4TextPakQBDisc (qbpak, Just qbpab) (qspak, Just qspab)
  let (qbSections, allNodes) = case pakResults of
        Nothing       -> ([], [])
        Just contents -> (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoGH4 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap catMaybes $ forM songInfo $ \info -> do
    let songPath  = "songs" :| [TE.decodeUtf8 (gh4Name info) <> ".pak.ps2"]
        hashSolo = hashString $ qbKeyCRC $ gh4Name info
        hashString w = T.pack $ reverse $ take 8 $ reverse (showHex w "") <> repeat '0'
        audioPath = "MUSIC" :| [T.take 1 hashSolo, hashSolo <> ".IMF"]
    case findFileCI songPath wadFolder of
      Nothing       -> return Nothing -- song in pak but not present, e.g. GH3 songs in GH Aerosmith
      Just rSongPak -> case findFileCI audioPath folder of
        Nothing -> do
          warn $ "Couldn't find audio for song " <> show (gh4Name info) <> ": " <> show audioPath
          return Nothing
        Just rAudio -> return $ Just $ importGH4Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH4AudioPS2 rAudio
          , ghiText     = allNodes
          }

importGH4Song :: (SendMessage m, MonadIO m) => GH4Import -> Import m
importGH4Song ghi level = do
  let info = ghiSongInfo ghi
      rSongPak = ghiSongPak ghi
  when (level == ImportFull) $ do
    lg $ "Importing GH song " <> show (gh4Name info) <> " from: " <> ghiSource ghi
  midiFixed <- case level of
    ImportFull -> do
      let ?endian = case ghiAudio ghi of
            GH4Audio360{} -> BigEndian
            GH4AudioPS2{} -> LittleEndian
      songNodes <- stackIO (useHandle rSongPak handleToByteString) >>= \bs ->
        inside "Parsing song .pak" $ splitPakNodes (pakFormatGH3 ?endian) bs Nothing
      let matchQB node = nodeFileType node == qbKeyCRC ".qb"
            && nodeFilenameCRC node == qbKeyCRC (gh4Name info)
      case filter (matchQB . fst) songNodes of
        [] -> fatal "Couldn't find chart .qb file"
        (_, bs) : _ -> do
          midQB <- inside "Parsing .mid.qb" $ runGetM parseQB bs >>= parseGH4MidQB (gh4Name info)
          let songBank = qsBank songNodes -- has lyrics
              markerNodes
                =  ghiText ghi -- 360 (on a disc, marker qb/qs are in qb.pak.xen and qs.pak.xen)
                <> songNodes   -- PS2 (marker qb/qs are in songname.pak.ps2)
              textBank = qsBank markerNodes <> worldTourDiscMarkers -- has section names
          -- .mid.qb has a qb key, then a separate .qb links that to a qs,
          -- then a .qs has the actual string
          textSectionMap <- fmap (HM.fromList . concat) $ do
            forM [ txt | (node, txt) <- markerNodes, matchQB node ] $ \txt -> do
              qb <- runGetM parseQB txt
              return $ qb >>= \case
                QBSectionQbKeyStringQs n _ qs -> [(n, qs)]
                _                             -> []
          let combinedSectionMap = HM.fromList $ do
                (qb, qs) <- HM.toList textSectionMap
                str <- toList $ HM.lookup qs textBank
                return (qb, str)
          return $ gh4ToMidi info songBank combinedSectionMap midQB
    ImportQuick -> return emptyChart
  let midiOnyx = midiFixed
        { F.s_tracks = F.fixedToOnyx $ F.s_tracks midiFixed
        }

  let adjustedInput
        = (case gh4OverallSongVolume info of 0 -> id; db -> PansVols [-1, 1] [db, db])
        . Input . Named
      adjustedInputNS (name, (ns, _)) = case nsChannels ns of
        1 -> (case gh4OverallSongVolume info of 0 -> id; db -> PansVols [0] [db])
          $ Input $ Named name
        _ -> adjustedInput name
  (audio, plan) <- case ghiAudio ghi of
    GH4Audio360 fsb1 fsb2 fsb3 -> do
      let getAudio (r, path) = case level of
            ImportQuick -> return []
            ImportFull -> do
              bs <- stackIO $ useHandle r handleToByteString
              dec <- case decryptFSB' (T.unpack path) $ BL.toStrict bs of
                Nothing  -> fatal $ "Couldn't decrypt audio file: " <> show path
                Just dec -> return dec
              streams <- stackIO $ parseFSB dec >>= splitFSBStreams
              forM (zip ([0..] :: [Int]) streams) $ \(i, stream) -> do
                (streamData, ext) <- stackIO $ getFSBStreamBytes stream
                return (path <> "." <> T.pack (show i) <> "." <> ext, streamData)
      streams1 <- getAudio fsb1
      streams2 <- getAudio fsb2
      streams3 <- getAudio fsb3
      let audio = do
            (name, bs) <- streams1 <> streams2 <> streams3
            let str = T.unpack name
            return (name, AudioFile AudioInfo
              { md5 = Nothing
              , frames = Nothing
              , filePath = Just $ SoftFile str $ SoftReadable
                $ makeHandle str $ byteStringSimpleHandle bs
              , commands = []
              , rate = Nothing
              , channels = 2
              })
          plan = StandardPlan StandardPlanInfo
            { song = case streams3 of
              (x, _) : _ -> Just $ adjustedInput x
              []         -> Nothing
            , parts = Parts $ HM.fromList $ let
              drums = case map fst streams1 of
                d1 : d2 : d3 : d4 : _ -> [(F.FlexDrums, PartDrumKit
                  { kick  = Just $ adjustedInput d1
                  , snare = Just $ adjustedInput d2
                  , toms  = Just $ adjustedInput d3
                  , kit   = adjustedInput d4
                  })]
                _ -> []
              gbv = case map fst streams2 of
                g : b : v : _ ->
                  [ (F.FlexGuitar, PartSingle $ adjustedInput g)
                  , (F.FlexBass  , PartSingle $ adjustedInput b)
                  , (F.FlexVocal , PartSingle $ adjustedInput v)
                  ]
                _ -> []
              in drums <> gbv
            , crowd = case drop 1 streams3 of
              (x, _) : _ -> Just $ adjustedInput x
              []         -> Nothing
            , comments = []
            , tuningCents = gh4VocalsPitchScoreShift info
            , fileTempo = Nothing
            }
      return (audio, plan)
    GH4AudioPS2 imf -> do
      streams <- case level of
        ImportQuick -> return []
        ImportFull -> do
          imfBytes <- stackIO $ useHandle imf handleToByteString
          (\hdr -> splitNeoStreams hdr imfBytes) <$> runGetM getNeoHeader imfBytes
      let findStream name suffix = let
            key = qbKeyCRC $ gh4Name info <> suffix
            in case filter (\(ns, _) -> nsKey ns == key) streams of
              []        -> Nothing
              match : _ -> Just (name <> ".vgs", match)
          streamGuitar    = findStream "guitar"    "_guitar"
          streamBass      = findStream "bass"      "_rhythm"
          streamSong      = findStream "song"      "_song"
          streamCymbals   = findStream "cymbals"   "_drumcymbal"
          streamKick      = findStream "kick"      "_drumkick"
          streamSnareToms = findStream "snaretoms" "_drumsnare"
          audio = do
            (name, pair@(ns, _)) <- catMaybes [streamGuitar, streamBass, streamSong, streamCymbals, streamKick, streamSnareToms]
            let str = T.unpack name
            return (name, AudioFile AudioInfo
              { md5 = Nothing
              , frames = Nothing
              , filePath = Just $ SoftFile str $ SoftReadable
                $ makeHandle str $ byteStringSimpleHandle $ neoStreamToVGS pair
              , commands = []
              , rate = Nothing
              , channels = fromIntegral $ nsChannels ns
              })
          plan = StandardPlan StandardPlanInfo
            { song = adjustedInputNS <$> streamSong
            , parts = Parts $ HM.fromList $ catMaybes
              [ (\pair -> (F.FlexGuitar, PartSingle $ adjustedInputNS pair)) <$> streamGuitar
              , (\pair -> (F.FlexBass, PartSingle $ adjustedInputNS pair)) <$> streamBass
              , case (streamKick, streamCymbals, streamSnareToms) of
                (Just kick, Just cymbals, Just snareToms) ->
                  Just (F.FlexDrums, PartDrumKit
                    { kick = Just $ adjustedInputNS kick
                    , snare = Nothing
                    , toms = Nothing
                    , kit = Mix $ adjustedInputNS cymbals :| [adjustedInputNS snareToms]
                    })
                _ -> Nothing -- could mix whatever's present but they should all be present hopefully
              ]
            , crowd = Nothing
            , comments = []
            , tuningCents = gh4VocalsPitchScoreShift info
            , fileTempo = Nothing
            }
      return (audio, plan)

  return SongYaml
    { metadata = def'
      { title = Just $ gh4Title info
      , artist = Just $ gh4Artist info
      , year = readMaybe $ T.unpack $ fromMaybe (gh4Year info) $ T.stripPrefix ", " $ gh4Year info
      , fileAlbumArt = Nothing
      , genre = do
        k <- gh4Genre info
        Just $ case k of
          -- most are same as WoR, except this one (at least not in my list)
          0x33fb36dc -> "Goth"
          _          -> case listToMaybe $ filter (\wor -> qbWoRGenre wor == k ) [minBound .. maxBound] of
            Nothing -> T.pack $ show k
            Just g  -> displayWoRGenre g
      , cover = not $ gh4OriginalArtist info
      }
    , global = def'
      { fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
      , fileSongAnim = Nothing
      , backgroundVideo = Nothing
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList audio
    , jammit = HM.empty
    , plans = HM.singleton "gh" plan
    , targets = HM.empty
    , parts = Parts $ HM.fromList
      [ (F.FlexGuitar, (emptyPart :: Part SoftFile)
        { grybo = Just def
        })
      , (F.FlexBass, (emptyPart :: Part SoftFile)
        { grybo = Just def
        })
      , (F.FlexDrums, (emptyPart :: Part SoftFile)
        { drums = Just $ let
          kicks = if gh4DoubleKick info then KicksBoth else Kicks1x
          in emptyPartDrums Drums5 kicks
        })
      , (F.FlexVocal, (emptyPart :: Part SoftFile)
        { vocal = Just PartVocal
          { difficulty = Tier 1
          , count      = Vocal1
          , gender     = gh4Singer info
          , key        = Nothing
          , lipsyncRB3 = Nothing
          }
        })
      ]
    }

------------------------------------------------------------------------

importGH3DiscPS2 :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3DiscPS2 src folder = do
  let folder' = first T.toCaseFold folder
      findFolded path = findFile (fmap T.toCaseFold path) folder'
  hed <- maybe (fatal "Couldn't find DATAP.HED") (\r -> stackIO $ useHandle r handleToByteString) (findFolded $ pure "DATAP.HED")
  wad <- maybe (fatal "Couldn't find DATAP.WAD") return $ findFolded $ pure "DATAP.WAD"
  wadFolder <- hookUpWAD wad . applyHed HedFormatGH3 <$> runGetM parseHed hed
  let wadFolder' = first T.toCaseFold wadFolder
      findFoldedWad path = findFile (fmap T.toCaseFold path) wadFolder'
      pakPath = "pak" :| ["qb.pak.ps2"]
      pabPath = "pak" :| ["qb.pab.ps2"]
  pak <- maybe (fatal "Couldn't find qb.pak") (\r -> stackIO $ useHandle r handleToByteString) (findFoldedWad pakPath)
  pab <- maybe (fatal "Couldn't find qb.pab") (\r -> stackIO $ useHandle r handleToByteString) (findFoldedWad pabPath)
  (qbSections, allNodes) <- errorToWarning (let ?endian = LittleEndian in readGH3TextPakQBDisc pak pab) >>= \case
    Nothing       -> return ([], [])
    Just contents -> return (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoGH3 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap concat $ forM songInfo $ \info -> do
    let songPath  = "songs" :| [TE.decodeUtf8 (gh3Name info) <> ".pak.ps2"]
        hashSolo = hashString $ qbKeyCRC $ gh3Name info
        hashCoop = hashString $ qbKeyCRC $ gh3Name info <> "_coop"
        hashString w = T.pack $ reverse $ take 8 $ reverse (showHex w "") <> repeat '0'
        audioSoloPath = "MUSIC" :| [T.take 1 hashSolo, hashSolo <> ".IMF"]
        audioCoopPath = "MUSIC" :| [T.take 1 hashCoop, hashCoop <> ".IMF"]
    case findFoldedWad songPath of
      Nothing       -> return [] -- song in pak but not present, e.g. GH3 songs in GH Aerosmith
      Just rSongPak -> do
        let rAudioSolo = findFolded audioSoloPath
            rAudioCoop = findFolded audioCoopPath
        return $ importGH3Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH3AudioPS2 rAudioSolo rAudioCoop
          , ghiText     = allNodes
          }

importGH3Disc :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3Disc src folder = do
  let folder' = first T.toCaseFold folder
      findFolded path = findFile (fmap T.toCaseFold path) folder'
      -- TODO support ps3/pc
      pakPath = "DATA" :| ["COMPRESSED", "PAK", "qb.pak.xen"]
      pabPath = "DATA" :| ["COMPRESSED", "PAK", "qb.pab.xen"]
  pak <- maybe (fatal "Couldn't find qb.pak") (\r -> stackIO $ useHandle r handleToByteString) (findFolded pakPath)
  pab <- maybe (fatal "Couldn't find qb.pab") (\r -> stackIO $ useHandle r handleToByteString) (findFolded pabPath)
  (qbSections, allNodes) <- errorToWarning (let ?endian = BigEndian in readGH3TextPakQBDisc pak pab) >>= \case
    Nothing       -> return ([], [])
    Just contents -> return (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoGH3 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap concat $ forM songInfo $ \info -> do
    let pathName  = TE.decodeUtf8 $ gh3Name info
        songPath  = "DATA" :| ["COMPRESSED", "SONGS", pathName <> "_song.pak.xen"]
        audioPath = "DATA" :| ["MUSIC", pathName <> ".fsb.xen"]
        datPath   = "DATA" :| ["MUSIC", pathName <> ".dat.xen"]
    case findFolded songPath of
      Nothing       -> return [] -- song in pak but not present, e.g. GH3 songs in GH Aerosmith
      Just rSongPak -> do
        rAudio   <- maybe (fatal $ "Couldn't find: " <> show audioPath) return (findFolded audioPath)
        rDat     <- maybe (fatal $ "Couldn't find: " <> show datPath  ) return (findFolded datPath  )
        return $ importGH3Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH3Audio360 rAudio rDat
          , ghiText     = allNodes
          }

importGH3DLC :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3DLC src folder = do
  platform <- if any (\(name, _) -> ".xen" `T.isSuffixOf` T.toLower name) $ folderFiles folder
    then return (<> ".xen")
    else if any (\(name, _) -> ".ps3" `T.isSuffixOf` T.toLower name) $ folderFiles folder
      then return (<> ".ps3")
      else fail "Couldn't determine DLC platform (.xen or .ps3)"
  let texts = do
        (name, r) <- folderFiles folder
        let name' = T.toLower name
        -- we check that it starts with "dl" to ignore the "adl" GHWT files in Death Magnetic
        guard $ "dl" `T.isPrefixOf` name' && platform "_text.pak" `T.isSuffixOf` name'
        return r
      findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
  (qbSections, allNodes) <- fmap mconcat $ forM texts $ \r -> do
    bs <- stackIO $ useHandle r handleToByteString
    errorToWarning (readGH3TextPakQBDLC bs) >>= \case
      Nothing       -> return ([], [])
      Just contents -> return (gh3TextPakSongStructs contents, gh3OtherNodes contents)
  songInfo <- fmap concat $ forM qbSections $ \(_key, items) -> do
    case parseSongInfoGH3 items of
      Left  err  -> warn err >> return []
      Right info -> return [info]
  fmap concat $ forM songInfo $ \info -> do
    let pathName  = TE.decodeUtf8 $ gh3Name info
        songPath  = pathName <> platform "_song.pak"
        audioPath = pathName <> platform ".fsb"
        datPath   = pathName <> platform ".dat"
    case findFolded songPath of
      Nothing       -> return [] -- song which is listed in the database, but not actually in this package
      Just rSongPak -> do
        rAudio <- maybe (fatal $ "Couldn't find: " <> show audioPath) return (findFolded audioPath)
        rDat   <- maybe (fatal $ "Couldn't find: " <> show datPath  ) return (findFolded datPath  )
        return $ importGH3Song GHImport
          { ghiSource   = src
          , ghiSongInfo = info
          , ghiSongPak  = rSongPak
          , ghiAudio    = GH3Audio360 rAudio rDat
          , ghiText     = allNodes
          }

data GHImport info audio = GHImport
  { ghiSource   :: FilePath
  , ghiSongInfo :: info
  , ghiSongPak  :: Readable
  , ghiAudio    :: audio
  , ghiText     :: [(Node, BL.ByteString)]
  }
type GH3Import = GHImport SongInfoGH3 GH3Audio
type GH4Import = GHImport SongInfoGH4 GH4Audio

data GH3Audio
  = GH3Audio360 Readable         Readable         -- .fsb.*, .dat.*
  | GH3AudioPS2 (Maybe Readable) (Maybe Readable) -- solo .IMF, coop .IMF

data GH4Audio
  = GH4Audio360 (Readable, T.Text) (Readable, T.Text) (Readable, T.Text) -- _1.fsb _2.fsb _3.fsb
  | GH4AudioPS2 Readable                                                 -- .IMF

importGH3Song :: (SendMessage m, MonadIO m) => GH3Import -> [Import m]
importGH3Song gh3i = let
  info = ghiSongInfo gh3i
  importModes = if gh3UseCoopNotetracks info
    then [ImportSolo, ImportCoop]
    else [ImportSolo]
  in flip map importModes $ \mode level -> do
    when (level == ImportFull) $ do
      lg $ "Importing GH3 song " <> show (gh3Name info) <> " from: " <> ghiSource gh3i
    -- Dragonforce DLC has rhythm tracks in coop, but hidden bass tracks in non-coop. (use coop notetracks = true)
    -- The Pretender has rhythm track copied to non-coop rhythm track, and non-coop rhythm audio is silent. (should ignore)
    -- We Three Kings is only DLC with rhythm coop but use coop notetracks = false.
    let thisRhythmTrack = gh3RhythmTrack info && hasRealCoop
        hasRealCoop     = mode == ImportCoop || not (gh3UseCoopNotetracks info)
        coopPart        = if thisRhythmTrack then F.FlexExtra "rhythm" else F.FlexBass
    midiOnyx <- case level of
      ImportFull -> do
        let ?endian = case ghiAudio gh3i of
              GH3Audio360{} -> BigEndian
              GH3AudioPS2{} -> LittleEndian
        nodes <- stackIO (useHandle (ghiSongPak gh3i) handleToByteString) >>= \bs ->
          inside "Parsing song .pak" $ splitPakNodes (pakFormatGH3 ?endian) bs Nothing
        let isMidQB node
              = elem (nodeFileType node) [qbKeyCRC ".qb", qbKeyCRC ".mqb"] -- .mqb on PS2
              && nodeFilenameCRC node == qbKeyCRC (gh3Name info)
        case filter (isMidQB . fst) nodes of
          [] -> fatal "Couldn't find chart .qb file"
          (_, bs) : _ -> do
            midQB <- inside "Parsing .mid.qb" $ runGetM parseQB bs >>= parseMidQB (gh3Name info)
            -- look for .qb with nodeFilenameCRC of e.g. "dlc17" to find section names
            let markerKey = qbKeyCRC $ gh3Name info
            markerBank <- case [ txt | (node, txt) <- ghiText gh3i, nodeFilenameCRC node == markerKey ] of
              [] -> return HM.empty -- warn?
              txt : _ -> do
                qb <- runGetM parseQB txt
                return $ HM.fromList $ flip concatMap qb $ \case
                  QBSectionStringW n _ str -> [(n, str)] -- 360
                  QBSectionString  n _ str -> [(n, TE.decodeLatin1 str)] -- ps2, what encoding?
                  _                        -> []
            return $ gh3ToMidi
              info
              (mode == ImportCoop && gh3UseCoopNotetracks info)
              thisRhythmTrack
              markerBank
              midQB
      ImportQuick -> return emptyChart
    -- don't mark that we have a coop part if it has no notes (like many customs)
    let hasCoopGems = maybe False (not . nullFive . F.onyxPartGuitar)
          $ Map.lookup coopPart $ F.onyxParts $ F.s_tracks midiOnyx
        drums
          = maybe mempty F.onyxPartTrueDrums
          $ Map.lookup F.FlexDrums
          $ F.onyxParts
          $ F.s_tracks midiOnyx
    audio <- case level of
      ImportQuick -> return []
      ImportFull -> case ghiAudio gh3i of
        GH3Audio360 rfsb rdat -> do
          bs <- stackIO $ useHandle rfsb handleToByteString
          dec <- case gh3Decrypt bs of
            Nothing  -> fatal "Couldn't decrypt audio file"
            Just dec -> return dec
          dat <- stackIO (useHandle rdat handleToByteString) >>= runGetM getGH3Dat
          -- TODO maybe only convert xma1->xma2 the fsbs we'll actually use for this import
          streams <- stackIO $ parseFSB dec >>= splitFSBStreams'
          allAudio <- forM (zip ([0..] :: [Int]) streams) $ \(i, stream) -> do
            (streamData, ext) <- stackIO $ getFSBStreamBytes stream
            return ("audio." <> T.pack (show i) <> "." <> ext, streamData)
          return $ gh3DatAudio dat >>= \(key, index) -> case drop (fromIntegral index) allAudio of
            []       -> []
            pair : _ -> [(key, pure pair)]
        GH3AudioPS2 mrsolo mrcoop -> do
          -- add crc keys to pretend it's indexed like the 360 audio
          let applyNames names r = do
                bs <- stackIO $ useHandle r handleToByteString
                vgs <- fmap (chunksOf 2) $ mapM convertChannelToVGS $ splitChannels bs
                return $ flip map (zip names vgs) $ \(name, chans) -> let
                  crc = qbKeyCRC $ gh3Name info <> "_" <> name
                  makeChannel i chan = (TE.decodeLatin1 name <> "." <> T.pack (show (i :: Int)) <> ".vgs", chan)
                  in (crc, NE.fromList $ zipWith makeChannel [0..] chans)
          -- are these always the same order? or is there an equivalent of .dat files
          solo <- maybe (return []) (applyNames ["guitar"     , "song"     , "rhythm"     ]) mrsolo
          coop <- maybe (return []) (applyNames ["coop_guitar", "coop_song", "coop_rhythm"]) mrcoop
          return $ solo <> coop
    -- Not sure these are dB but it would make sense.
    -- Also does guitar volume also apply to bass/rhythm?
    let guitarVol = replicate 2 $ realToFrac $ gh3GuitarPlaybackVolume info
        bandVol   = replicate 2 $ realToFrac $ gh3BandPlaybackVolume   info
    return SongYaml
      { metadata = def'
        { title = Just $ case mode of
          ImportSolo -> gh3Title info
          ImportCoop -> gh3Title info <> " (Co-op)"
        , artist = Just $ gh3Artist info
        , year = gh3Year info >>= readMaybe . T.unpack . T.takeWhileEnd isDigit
        , fileAlbumArt = Nothing
        }
      , global = def'
        { fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
        , fileSongAnim = Nothing
        , backgroundVideo = Nothing
        , fileBackgroundImage = Nothing
        }
      , audio = HM.fromList $ do
        (_, group) <- audio
        (name, bs) <- NE.toList group
        let str = T.unpack name
        return (name, AudioFile AudioInfo
          { md5 = Nothing
          , frames = Nothing
          , filePath = Just $ SoftFile str $ SoftReadable
            $ makeHandle str $ byteStringSimpleHandle bs
          , commands = []
          , rate = Nothing
          , channels = case ghiAudio gh3i of
            GH3Audio360{} -> 2
            GH3AudioPS2{} -> 1 -- since we split up to mono vgs
          })
      , jammit = HM.empty
      , plans = let
        nameLead = gh3Name info <> "_" <> if mode == ImportCoop && gh3UseCoopNotetracks info
          then "coop_guitar"
          else "guitar"
        nameRhythm = gh3Name info <> "_" <> if mode == ImportCoop && gh3UseCoopNotetracks info
          then "coop_rhythm"
          else "rhythm"
        nameBand = gh3Name info <> "_" <> if mode == ImportCoop && gh3UseCoopNotetracks info
          then "coop_song"
          else "song"
        toExpr ((f, _) :| []) = Input $ Named f
        toExpr xs             = Merge $ fmap (Input . Named . fst) xs
        in HM.singleton "gh" $ StandardPlan StandardPlanInfo
          { song = flip fmap (lookup (qbKeyCRC nameBand) audio) $ \group ->
            PansVols [-1, 1] bandVol $ toExpr group
          , parts = Parts $ HM.fromList $ catMaybes
            [ flip fmap (lookup (qbKeyCRC nameLead  ) audio) $ \group ->
              (F.FlexGuitar, PartSingle $ PansVols [-1, 1] guitarVol $ toExpr group)
            , flip fmap (lookup (qbKeyCRC nameRhythm) audio) $ \group ->
              (coopPart    , PartSingle $ PansVols [-1, 1] guitarVol $ toExpr group)
            ]
          , crowd = Nothing
          , comments = []
          , tuningCents = 0
          , fileTempo = Nothing
          }
      , targets = HM.empty
      , parts = Parts $ HM.fromList $ catMaybes
        [ Just (F.FlexGuitar, emptyPart { grybo = Just def })
        , guard (hasRealCoop && hasCoopGems) >> Just (coopPart, emptyPart { grybo = Just def })
        , do
          let expert = fromMaybe mempty $ Map.lookup Expert $ tdDifficulties drums
          guard $ not $ RTB.null $ tdGems expert
          Just (F.FlexDrums, emptyPart
            { drums = Just $ emptyPartDrums DrumsTrue (if RTB.null $ tdKick2 expert then Kicks1x else KicksBoth)
            })
        ]
      }

importGH3SGHFolder :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3SGHFolder src folder = case findFileCI (pure "songs.info") folder of
  Nothing -> fatal "No songs.info found in .sgh"
  Just rSongs -> do
    songs <- stackIO (useHandle rSongs handleToByteString)
      >>= decryptSGHSongs . BL.toStrict
      >>= runGetM parseSGHStruct . BL.fromStrict
    let platform = (<> ".xen")
        findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
    songInfo <- fmap concat $ forM songs $ \case
      QBStructItemStruct8A0000 _ items -> case parseSongInfoGH3 $ map (first UnknownQS) items of
        Left  err  -> warn err >> return []
        Right info -> return [info]
      _ -> return []
    fmap concat $ forM songInfo $ \info -> do
      let pathName  = TE.decodeUtf8 $ gh3Name info
          songPath  = pathName <> platform "_song.pak"
          audioPath = pathName <> platform ".fsb"
          datPath   = pathName <> platform ".dat"
      case findFolded songPath of
        Nothing       -> return [] -- song which is listed in the database, but not actually in this package
        Just rSongPak -> do
          rAudio <- maybe (fatal $ "Couldn't find: " <> show audioPath) return (findFolded audioPath)
          rDat   <- maybe (fatal $ "Couldn't find: " <> show datPath  ) return (findFolded datPath  )
          return $ importGH3Song GHImport
            { ghiSource   = src
            , ghiSongInfo = info
            , ghiSongPak  = rSongPak
            , ghiAudio    = GH3Audio360 rAudio rDat
            , ghiText     = []
            }

{-

Silly AES keys GHTCP uses for songs.info + setlist.info

.NET docs for PasswordDeriveBytes say "This class uses an extension of the
PBKDF1 algorithm defined in the PKCS#5 v2.0 standard to derive bytes suitable
for use as key material from a password. The standard is documented in
IETF RRC 2898."

byte[] PwByteArray = { 73,118,97,110,32,77,101,100,118,101,100,101,118 }; // "Ivan Medvedev"
String password = isSetlist ? "SET4AES4KEY9MXKR" : "SNG4AES4KEY9MXKR";
PasswordDeriveBytes passwordDeriveBytes = new PasswordDeriveBytes(password, PwByteArray);
byte[] key = passwordDeriveBytes.GetBytes(32);
byte[] initVector = passwordDeriveBytes.GetBytes(16);

-}

sghSetlistKey, sghSetlistIV, sghSongsKey, sghSongsIV :: B.ByteString
sghSetlistKey = B.pack [61,250,11,73,254,154,8,191,18,188,243,32,246,40,148,145,62,219,250,196,15,63,217,91,29,73,8,22,197,186,176,81]
sghSetlistIV  = B.pack [18,188,243,32,246,40,148,145,247,74,25,30,94,26,230,111]
sghSongsKey   = B.pack [45,219,244,185,119,192,19,251,134,93,62,50,245,33,177,178,192,184,16,30,114,253,61,49,248,198,204,123,91,48,188,103]
sghSongsIV    = B.pack [134,93,62,50,245,33,177,178,239,48,31,166,143,179,180,49]

decryptSGHSetlist :: (MonadFail m) => B.ByteString -> m B.ByteString
decryptSGHSetlist bs = do
  Just iv <- return $ makeIV sghSetlistIV
  CryptoPassed cipher <- return $ cipherInit sghSetlistKey
  return $ cbcDecrypt (cipher :: AES256) iv bs

decryptSGHSongs :: (MonadFail m) => B.ByteString -> m B.ByteString
decryptSGHSongs bs = do
  Just iv <- return $ makeIV sghSongsIV
  CryptoPassed cipher <- return $ cipherInit sghSongsKey
  return $ cbcDecrypt (cipher :: AES256) iv bs
