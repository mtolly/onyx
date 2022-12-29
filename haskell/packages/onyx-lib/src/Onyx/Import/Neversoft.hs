{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Onyx.Import.Neversoft where

import           Control.Applicative              ((<|>))
import           Control.Monad                    (forM, guard, when)
import           Control.Monad.IO.Class           (MonadIO)
import           Data.Bifunctor                   (first)
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
import           Data.Maybe                       (catMaybes, listToMaybe)
import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE
import           GHC.ByteOrder
import           Numeric                          (showHex)
import           Onyx.Audio                       (Audio (..))
import           Onyx.Audio.FSB                   (getFSBStreamBytes, parseFSB,
                                                   splitFSBStreams,
                                                   splitFSBStreams')
import           Onyx.Genre                       (displayWoRGenre)
import           Onyx.Import.Base
import           Onyx.Import.GuitarHero2          (ImportMode (..))
import           Onyx.MIDI.Common                 (Difficulty (..))
import           Onyx.MIDI.Track.Drums.Full       (fdDifficulties, fdGems,
                                                   fdKick2)
import qualified Onyx.MIDI.Track.File             as RBFile
import           Onyx.MIDI.Track.FiveFret         (nullFive)
import           Onyx.Neversoft.CRC               (qbKeyCRC)
import           Onyx.Neversoft.Crypt             (decryptFSB', gh3Decrypt)
import           Onyx.Neversoft.GH3               (gh3ToMidi, parseMidQB)
import           Onyx.Neversoft.Metadata
import           Onyx.Neversoft.Note
import           Onyx.Neversoft.Pak
import           Onyx.Neversoft.PS2
import           Onyx.Neversoft.QB                (QBSection (..), parseQB)
import           Onyx.Project
import           Onyx.StackTrace
import           Onyx.Util.Handle
import           Onyx.Xbox.STFS                   (runGetM)
import           Text.Read                        (readMaybe)

importNeversoftGH :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importNeversoftGH src folder = let
  -- note: when we support ghwt, make sure we do something sensible for Death Magnetic.
  -- (right now it correctly imports the gh3 data)
  testGH3   name = "dl"        `T.isPrefixOf` name && pakSuffix name
  testGHWT  name = "adl"       `T.isPrefixOf` name && pakSuffix name
  testGH5   name = "bmanifest" `T.isPrefixOf` name && pakSuffix name
  testGHWoR name = "cmanifest" `T.isPrefixOf` name && pakSuffix name
  pakSuffix name = ".pak.xen" `T.isSuffixOf` name || ".pak.ps3" `T.isSuffixOf` name
  in if
    | any (testGH3   . T.toLower . fst) $ folderFiles folder -> importGH3DLC src folder
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
    errorToWarning (readTextPakQB bs) >>= \case
      Nothing       -> return []
      Just contents -> return $ textPakSongStructs contents
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

importGH3PS2 :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importGH3PS2 src folder = do
  let folder' = first T.toCaseFold folder
      findFolded path = findFile (fmap T.toCaseFold path) folder'
  hed <- maybe (fatal "Couldn't find DATAP.HED") (\r -> stackIO $ useHandle r handleToByteString) (findFolded $ pure "DATAP.HED")
  wad <- maybe (fatal "Couldn't find DATAP.WAD") return $ findFolded $ pure "DATAP.WAD"
  wadFolder <- hookUpWAD wad . applyHed <$> runGetM parseHed hed
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
    rSongPak   <- maybe (fatal $ "Couldn't find: " <> show songPath     ) return (findFoldedWad songPath     )
    let rAudioSolo = findFolded audioSoloPath
        rAudioCoop = findFolded audioCoopPath
    importGH3Song GH3Import
      { gh3iSource   = src
      , gh3iSongInfo = info
      , gh3iSongPak  = rSongPak
      , gh3iAudio    = GH3AudioPS2 rAudioSolo rAudioCoop
      , gh3iText     = allNodes
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
        importGH3Song GH3Import
          { gh3iSource   = src
          , gh3iSongInfo = info
          , gh3iSongPak  = rSongPak
          , gh3iAudio    = GH3Audio360 rAudio rDat
          , gh3iText     = allNodes
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
        dlName <- toList $ T.stripSuffix (platform "_text.pak") $ T.toLower name
        return (dlName, r)
      findFolded f = listToMaybe [ r | (name, r) <- folderFiles folder, T.toCaseFold name == T.toCaseFold f ]
  (qbSections, allNodes) <- fmap mconcat $ forM texts $ \(_dlName, r) -> do
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
        importGH3Song GH3Import
          { gh3iSource   = src
          , gh3iSongInfo = info
          , gh3iSongPak  = rSongPak
          , gh3iAudio    = GH3Audio360 rAudio rDat
          , gh3iText     = allNodes
          }

data GH3Import = GH3Import
  { gh3iSource   :: FilePath
  , gh3iSongInfo :: SongInfoGH3
  , gh3iSongPak  :: Readable
  , gh3iAudio    :: GH3Audio
  , gh3iText     :: [(Node, BL.ByteString)]
  }

data GH3Audio
  = GH3Audio360 Readable         Readable         -- .fsb.*, .dat.*
  | GH3AudioPS2 (Maybe Readable) (Maybe Readable) -- solo .IMF, coop .IMF

importGH3Song :: (SendMessage m, MonadIO m) => GH3Import -> StackTraceT m [Import m]
importGH3Song gh3i = let
  info = gh3iSongInfo gh3i
  importModes = if gh3UseCoopNotetracks info
    then [ImportSolo, ImportCoop]
    else [ImportSolo]
  in return $ flip map importModes $ \mode level -> do
    when (level == ImportFull) $ do
      lg $ "Importing GH3 song " <> show (gh3Name info) <> " from: " <> gh3iSource gh3i
    -- Dragonforce DLC has rhythm tracks in coop, but hidden bass tracks in non-coop. (use coop notetracks = true)
    -- The Pretender has rhythm track copied to non-coop rhythm track, and non-coop rhythm audio is silent. (should ignore)
    -- We Three Kings is only DLC with rhythm coop but use coop notetracks = false.
    let thisRhythmTrack = gh3RhythmTrack info && hasRealCoop
        hasRealCoop     = mode == ImportCoop || not (gh3UseCoopNotetracks info)
        coopPart        = if thisRhythmTrack then RBFile.FlexExtra "rhythm" else RBFile.FlexBass
    midiOnyx <- case level of
      ImportFull -> do
        let ?endian = case gh3iAudio gh3i of
              GH3Audio360{} -> BigEndian
              GH3AudioPS2{} -> LittleEndian
        nodes <- stackIO (useHandle (gh3iSongPak gh3i) handleToByteString) >>= \bs ->
          inside "Parsing song .pak" $ splitPakNodes ?endian bs Nothing
        let isMidQB node
              = elem (nodeFileType node) [qbKeyCRC ".qb", qbKeyCRC ".mqb"] -- .mqb on PS2
              && nodeFilenameCRC node == qbKeyCRC (gh3Name info)
        case filter (isMidQB . fst) nodes of
          [] -> fatal "Couldn't find chart .qb file"
          (_, bs) : _ -> do
            midQB <- inside "Parsing .mid.qb" $ runGetM parseQB bs >>= parseMidQB (gh3Name info)
            -- look for .qb with nodeFilenameCRC of e.g. "dlc17" to find section names
            let markerKey = qbKeyCRC $ gh3Name info
            markerBank <- case [ txt | (node, txt) <- gh3iText gh3i, nodeFilenameCRC node == markerKey ] of
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
    let hasCoopGems = maybe False (not . nullFive . RBFile.onyxPartGuitar)
          $ Map.lookup coopPart $ RBFile.onyxParts $ RBFile.s_tracks midiOnyx
        drums
          = maybe mempty RBFile.onyxPartFullDrums
          $ Map.lookup RBFile.FlexDrums
          $ RBFile.onyxParts
          $ RBFile.s_tracks midiOnyx
    audio <- case level of
      ImportQuick -> return []
      ImportFull -> case gh3iAudio gh3i of
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
                let vgs = chunksOf 2 $ map convertChannelToVGS $ splitChannels bs
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
      { _metadata = def'
        { _title = Just $ case mode of
          ImportSolo -> gh3Title info
          ImportCoop -> gh3Title info <> " (Co-op)"
        , _artist = Just $ gh3Artist info
        , _year = gh3Year info >>= readMaybe . T.unpack . T.takeWhileEnd isDigit
        , _fileAlbumArt = Nothing
        }
      , _global = def'
        { _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
        , _fileSongAnim = Nothing
        , _backgroundVideo = Nothing
        , _fileBackgroundImage = Nothing
        }
      , _audio = HM.fromList $ do
        (_, group) <- audio
        (name, bs) <- NE.toList group
        let str = T.unpack name
        return (name, AudioFile AudioInfo
          { _md5 = Nothing
          , _frames = Nothing
          , _filePath = Just $ SoftFile str $ SoftReadable
            $ makeHandle str $ byteStringSimpleHandle bs
          , _commands = []
          , _rate = Nothing
          , _channels = case gh3iAudio gh3i of
            GH3Audio360{} -> 2
            GH3AudioPS2{} -> 1 -- since we split up to mono vgs
          })
      , _jammit = HM.empty
      , _plans = let
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
        in HM.singleton "gh" Plan
          { _song = flip fmap (lookup (qbKeyCRC nameBand) audio) $ \group ->
            PlanAudio (toExpr group) [] bandVol
          , _countin = Countin []
          , _planParts = Parts $ HM.fromList $ catMaybes
            [ flip fmap (lookup (qbKeyCRC nameLead  ) audio) $ \group ->
              (RBFile.FlexGuitar, PartSingle $ PlanAudio (toExpr group) [] guitarVol)
            , flip fmap (lookup (qbKeyCRC nameRhythm) audio) $ \group ->
              (coopPart         , PartSingle $ PlanAudio (toExpr group) [] guitarVol)
            ]
          , _crowd = Nothing
          , _planComments = []
          , _tuningCents = 0
          , _fileTempo = Nothing
          }
      , _targets = HM.empty
      , _parts = Parts $ HM.fromList $ catMaybes
        [ Just (RBFile.FlexGuitar, def { partGRYBO = Just def })
        , guard (hasRealCoop && hasCoopGems) >> Just (coopPart, def { partGRYBO = Just def })
        , do
          guard $ maybe False (not . RTB.null . fdGems) $ Map.lookup Expert $ fdDifficulties drums
          Just (RBFile.FlexDrums, def
            { partDrums = Just PartDrums
              { drumsDifficulty  = Tier 1
              , drumsMode        = DrumsFull
              , drumsKicks       = if RTB.null $ fdKick2 drums then Kicks1x else KicksBoth
              , drumsFixFreeform = False
              , drumsKit         = HardRockKit
              , drumsLayout      = StandardLayout
              , drumsFallback    = FallbackGreen
              , drumsFileDTXKit  = Nothing
              , drumsFullLayout  = FDStandard
              }
            })
        ]
      }
