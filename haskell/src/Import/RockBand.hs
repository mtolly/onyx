{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Import.RockBand where

import           Config
import           Control.Arrow                  (second)
import           Control.Monad                  (forM, guard, void, when)
import           Control.Monad.IO.Class         (MonadIO)
import           Control.Monad.Trans.StackTrace
import qualified Data.ByteString.Lazy           as BL
import           Data.Default.Class             (def)
import qualified Data.DTA                       as D
import qualified Data.DTA.Serialize             as D
import qualified Data.DTA.Serialize.RB3         as D
import           Data.Foldable                  (toList)
import qualified Data.HashMap.Strict            as HM
import           Data.List.Extra                (elemIndex, nubOrd)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromMaybe, isNothing, mapMaybe)
import           Data.SimpleHandle              (Folder, Readable, crawlFolder,
                                                 fileReadable, findByteString,
                                                 findFile, handleToByteString,
                                                 splitPath, useHandle)
import qualified Data.Text                      as T
import           Data.Text.Encoding             (decodeLatin1, decodeUtf8With)
import           Data.Text.Encoding.Error       (lenientDecode)
import           Difficulty
import           Import.Base
import           Magma                          (rbaContents)
import           PrettyDTA                      (C3DTAComments (..),
                                                 DTASingle (..), readDTASingles)
import           Resources                      (rb3Updates)
import           RockBand.Codec.Drums           as RBDrums
import           RockBand.Codec.File            (FlexPartName (..))
import qualified RockBand.Codec.File            as RBFile
import           RockBand.Codec.ProGuitar       (GtrBase (..), GtrTuning (..))
import           RockBand.Common
import qualified Sound.MIDI.Util                as U
import           STFS.Package                   (getSTFSFolder)
import qualified System.Directory               as Dir
import           System.FilePath                (takeDirectory, takeFileName,
                                                 (<.>), (</>))
import           Text.Read                      (readMaybe)

data RBImport = RBImport
  { rbiSongPackage :: D.SongPackage
  , rbiComments    :: C3DTAComments
  , rbiMOGG        :: SoftContents
  , rbiAlbumArt    :: Maybe SoftFile
  , rbiMilo        :: Maybe SoftContents
  , rbiMIDI        :: Readable
  , rbiMIDIUpdate  :: Maybe Readable
  }

importSTFS :: (SendMessage m, MonadIO m) => Int -> FilePath -> FilePath -> StackTraceT m ()
importSTFS i src dest = do
  lg $ "Importing Rock Band CON/LIVE file from: " <> src
  stackIO (getSTFSFolder src)
    >>= newImportSTFSFolder
    >>= \imps -> (imps !! i) ImportFull
    >>= void . stackIO . saveImport dest

importSTFSDir :: (SendMessage m, MonadIO m) => Int -> FilePath -> FilePath -> StackTraceT m ()
importSTFSDir i src dest = stackIO (crawlFolder src)
  >>= newImportSTFSFolder
  >>= \imps -> (imps !! i) ImportFull
  >>= void . stackIO . saveImport dest

importRBA :: (SendMessage m, MonadIO m) => FilePath -> FilePath -> StackTraceT m ()
importRBA src dest = do
  lg $ "Importing RBA file from: " <> src
  newImportRBA src ImportFull >>= void . stackIO . saveImport dest

newImportSTFSFolder :: (SendMessage m, MonadIO m) => Folder T.Text Readable -> StackTraceT m [Import m]
newImportSTFSFolder folder = do
  packSongs <- stackIO (findByteString ("songs" :| ["songs.dta"]) folder) >>= \case
    Nothing -> fatal "songs/songs.dta not found"
    Just bs -> readDTASingles $ BL.toStrict bs
  updateDir <- stackIO rb3Updates
  forM packSongs $ \(DTASingle top pkg comments, _) -> do
    let base = T.unpack $ D.songName $ D.song pkg
        split s = case splitPath $ T.pack s of
          Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
          Just p  -> return p
        need p = case findFile p folder of
          Just r  -> return r
          Nothing -> fatal $ "Required file not found: " <> show p
    miloPath <- split $ takeDirectory base </> "gen" </> takeFileName base <.> "milo_xbox"
    moggPath <- split $ base <.> "mogg"
    midiPath <- split $ base <.> "mid"
    artPath <- split $ takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox")
    mogg <- SoftReadable <$> need moggPath
    midi <- need midiPath
    let missingArt = updateDir </> T.unpack top </> "gen" </> (T.unpack top ++ "_keep.png_xbox")
        updateMid = updateDir </> T.unpack top </> (T.unpack top ++ "_update.mid")
    art <- if fromMaybe False (D.albumArt pkg) || D.gameOrigin pkg == Just "beatles"
      then stackIO (Dir.doesFileExist missingArt) >>= \case
        True -> return $ Just $ fileReadable missingArt -- old rb1 song with album art on rb3 disc
        False -> do
          let res = findFile artPath folder
          when (isNothing res) $
            warn $ "Expected album art, but didn't find it: " <> show artPath
          return res
      else return Nothing
    update <- if maybe False ("disc_update" `elem`) $ D.extraAuthoring pkg
      then stackIO (Dir.doesFileExist updateMid) >>= \case
        True -> return $ Just $ fileReadable updateMid
        False -> do
          warn $ "Expected to find disc update MIDI but it's not installed: " <> updateMid
          return Nothing
      else return Nothing
    return $ importRB RBImport
      { rbiSongPackage = pkg
      , rbiComments = comments
      , rbiMOGG = mogg
      , rbiAlbumArt = SoftFile "cover.png_xbox" . SoftReadable <$> art
      , rbiMilo = SoftReadable <$> findFile miloPath folder
      , rbiMIDI = midi
      , rbiMIDIUpdate = update
      }

newImportRBA :: (SendMessage m, MonadIO m) => FilePath -> Import m
newImportRBA rba level = do
  let contents = rbaContents rba
      need i = case lookup i contents of
        Just r  -> return r
        Nothing -> fatal $ "Required RBA subfile " <> show i <> " not found"
  packSongs <- need 0 >>= \r -> stackIO (useHandle r handleToByteString) >>= readDTASingles . BL.toStrict
  (DTASingle _top pkg comments, isUTF8) <- case packSongs of
    [song] -> return song
    _      -> fatal $ "Expected 1 song in RBA, found " <> show (length packSongs)
  midi <- need 1
  mogg <- SoftReadable <$> need 2
  milo <- SoftReadable <$> need 3
  bmp <- SoftFile "cover.bmp" . SoftReadable <$> need 4
  extraBS <- need 6 >>= \r -> stackIO $ useHandle r handleToByteString
  extra <- fmap (if isUTF8 then decodeUtf8With lenientDecode else decodeLatin1)
    <$> D.readDTABytes (BL.toStrict extraBS)
  let author = case extra of
        D.DTA _ (D.Tree _ [D.Parens (D.Tree _
          ( D.String "backend"
          : D.Parens (D.Tree _ [D.Sym "author", D.String s])
          : _
          ))])
          -> Just s
        _ -> Nothing
      -- TODO: import more stuff from the extra dta
  importRB RBImport
    { rbiSongPackage = pkg
    , rbiComments = comments
      { c3dtaAuthoredBy = author
      }
    , rbiMOGG = mogg
    , rbiAlbumArt = Just bmp
    , rbiMilo = Just milo
    , rbiMIDI = midi
    , rbiMIDIUpdate = Nothing
    } level

dtaIsRB3 :: D.SongPackage -> Bool
dtaIsRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc", "ugc_plus"]) $ D.gameOrigin pkg
  -- rbn1 songs have (game_origin rb2) (ugc 1)

dtaIsHarmonixRB3 :: D.SongPackage -> Bool
dtaIsHarmonixRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc"]) $ D.gameOrigin pkg

importRB :: (SendMessage m, MonadIO m) => RBImport -> Import m
importRB rbi level = do

  let pkg = rbiSongPackage rbi
      files2x = Nothing
      localMilo = do
        -- if rbn2 and no vox, don't import milo
        guard $ dtaIsHarmonixRB3 pkg || maybe False (/= 0) (HM.lookup "vocals" $ D.rank pkg)
        let miloName = if dtaIsHarmonixRB3 pkg then "lipsync-venue.milo_xbox" else "lipsync.milo_xbox"
        SoftFile miloName <$> rbiMilo rbi
      (title, auto2x) = determine2xBass $ D.name pkg
      is2x = fromMaybe auto2x $ c3dta2xBass $ rbiComments rbi
      hasKicks = if is2x then Kicks2x else Kicks1x

  (midiFixed, midiOnyx) <- case level of
    ImportFull -> do
      RBFile.Song temps sigs (RBFile.RawFile trks1x) <- RBFile.loadMIDIReadable $ rbiMIDI rbi
      trksUpdate <- case rbiMIDIUpdate rbi of
        Nothing -> return []
        Just umid -> RBFile.rawTracks . RBFile.s_tracks <$> RBFile.loadMIDIReadable umid
      let updatedNames = map Just $ mapMaybe U.trackName trksUpdate
          trksUpdated
            = filter ((`notElem` updatedNames) . U.trackName) trks1x
            ++ trksUpdate
      midiFixed <- RBFile.interpretMIDIFile $ RBFile.Song temps sigs trksUpdated
      return (midiFixed, midiFixed { RBFile.s_tracks = RBFile.fixedToOnyx $ RBFile.s_tracks midiFixed })
    ImportQuick -> return (emptyChart, emptyChart)

  drumkit <- case D.drumBank pkg of
    Nothing -> return HardRockKit
    Just x -> case x of
      "sfx/kit01_bank.milo" -> return HardRockKit
      "sfx/kit02_bank.milo" -> return ArenaKit
      "sfx/kit03_bank.milo" -> return VintageKit
      "sfx/kit04_bank.milo" -> return TrashyKit
      "sfx/kit05_bank.milo" -> return ElectronicKit
      s -> do
        warn $ "Unrecognized drum bank " ++ show s
        return HardRockKit
  let diffMap :: HM.HashMap T.Text Config.Difficulty
      diffMap = let
        -- We assume that if every rank value is a tier boundary,
        -- it's a Magma-produced song where the author selected tiers.
        -- So we should import to tiers, not ranks.
        isTierBoundary (k, v) = case k of
          "drum"        -> (k,) <$> elemIndex v (0 : 1 : drumsDiffMap)
          "guitar"      -> (k,) <$> elemIndex v (0 : 1 : guitarDiffMap)
          "bass"        -> (k,) <$> elemIndex v (0 : 1 : bassDiffMap)
          "vocals"      -> (k,) <$> elemIndex v (0 : 1 : vocalDiffMap)
          "keys"        -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_keys"   -> (k,) <$> elemIndex v (0 : 1 : keysDiffMap)
          "real_guitar" -> (k,) <$> elemIndex v (0 : 1 : proGuitarDiffMap)
          "real_bass"   -> (k,) <$> elemIndex v (0 : 1 : proBassDiffMap)
          "band"        -> (k,) <$> elemIndex v (0 : 1 : bandDiffMap)
          _             -> Nothing
        in case mapM isTierBoundary $ HM.toList $ D.rank pkg of
          Nothing    -> Rank                <$> D.rank pkg
          Just tiers -> Tier . fromIntegral <$> HM.fromList tiers
      hasRankStr s = maybe False (/= 0) $ HM.lookup s $ D.rank pkg
  vocalMode <- if hasRankStr "vocals"
    then case D.vocalParts $ D.song pkg of
      Nothing -> return $ Just Vocal1
      Just 0  -> return Nothing
      Just 1  -> return $ Just Vocal1
      Just 2  -> return $ Just Vocal2
      Just 3  -> return $ Just Vocal3
      n       -> fatal $ "Invalid vocal count of " ++ show n
    else return Nothing
  let hopoThresh = fromIntegral $ fromMaybe 170 $ D.hopoThreshold $ D.song pkg

  let drumEvents = RBFile.fixedPartDrums $ RBFile.s_tracks midiFixed
  foundMix <- let
    drumMixes = do
      (_, dd) <- Map.toList $ drumDifficulties drumEvents
      (aud, _dsc) <- toList $ drumMix dd
      return aud
    in case drumMixes of
      [] -> return Nothing
      aud : auds -> if all (== aud) auds
        then return $ Just aud
        else do
          warn $ "Inconsistent drum mixes: " ++ show (nubOrd drumMixes)
          return Nothing
  let instChans :: [(T.Text, [Int])]
      instChans = map (second $ map fromIntegral) $ D.fromDictList $ D.tracks $ D.song pkg
      drumChans = fromMaybe [] $ lookup "drum" instChans
  drumSplit <- if not $ hasRankStr "drum" then return Nothing else case foundMix of
    Nothing -> case drumChans of
      -- No drum mix seen in The Kill (30STM), has 5 drum channels
      [kitL, kitR] -> return $ Just $ PartSingle [kitL, kitR]
      [kick, snare, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      [kick, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      [kick, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> do
        warn $ unwords
          [ "No drum mix (or inconsistent) and there are"
          , show $ length drumChans
          , "drum channels (expected 2-6)"
          ]
        return $ Just $ PartSingle drumChans
    Just RBDrums.D0 -> case drumChans of
      [kitL, kitR] -> return $ Just $ PartSingle [kitL, kitR]
      _ -> fatal $ "mix 0 needs 2 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D1 -> case drumChans of
      [kick, snare, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      _ -> fatal $ "mix 1 needs 4 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D2 -> case drumChans of
      [kick, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 2 needs 5 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D3 -> case drumChans of
      [kickL, kickR, snareL, snareR, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      _ -> fatal $ "mix 3 needs 6 drums channels, " ++ show (length drumChans) ++ " given"
    Just RBDrums.D4 -> case drumChans of
      [kick, kitL, kitR] -> return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> fatal $ "mix 4 needs 3 drums channels, " ++ show (length drumChans) ++ " given"

  let tone = fromMaybe Minor $ D.songTonality pkg
      -- Minor verified as default for PG chords if SK/VTN present and no song_tonality
      (skey, vkey) = case (D.songKey pkg, D.vocalTonicNote pkg) of
        (Just sk, Just vtn) -> (Just $ SongKey sk tone , Just vtn)
        (Just sk, Nothing ) -> (Just $ SongKey sk tone , Nothing )
        (Nothing, Just vtn) -> (Just $ SongKey vtn tone, Nothing )
        (Nothing, Nothing ) -> (Nothing                , Nothing )

  let bassBase = detectExtProBass $ RBFile.s_tracks midiFixed

  return SongYaml
    { _metadata = Metadata
      { _title        = Just title
      , _artist       = case (D.artist pkg, D.gameOrigin pkg) of
        (Nothing, Just "beatles") -> Just "The Beatles"
        _                         -> D.artist pkg
      , _album        = D.albumName pkg
      , _genre        = D.genre pkg
      , _subgenre     = D.subGenre pkg >>= T.stripPrefix "subgenre_"
      , _year         = case (D.yearReleased pkg, D.gameOrigin pkg, D.dateReleased pkg) of
        (Nothing, Just "beatles", Just date) -> readMaybe $ T.unpack $ T.take 4 date
        _ -> fromIntegral <$> D.yearReleased pkg
      , _fileAlbumArt = rbiAlbumArt rbi
      , _trackNumber  = fromIntegral <$> D.albumTrackNumber pkg
      , _comments     = []
      , _difficulty   = fromMaybe (Tier 1) $ HM.lookup "band" diffMap
      , _key          = skey
      , _author       = c3dtaAuthoredBy $ rbiComments rbi
      , _rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , _previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ D.preview pkg) / 1000
      , _previewEnd   = Just $ PreviewSeconds $ fromIntegral (snd $ D.preview pkg) / 1000
      , _languages    = fromMaybe [] $ c3dtaLanguages $ rbiComments rbi
      , _convert      = fromMaybe False $ c3dtaConvert $ rbiComments rbi
      , _rhythmKeys   = fromMaybe False $ c3dtaRhythmKeys $ rbiComments rbi
      , _rhythmBass   = fromMaybe False $ c3dtaRhythmBass $ rbiComments rbi
      , _catEMH       = fromMaybe False $ c3dtaCATemh $ rbiComments rbi
      , _expertOnly   = fromMaybe False $ c3dtaExpertOnly $ rbiComments rbi
      , _cover        = not $ D.master pkg
      }
    , _global = Global
      { _animTempo = D.animTempo pkg
      , _fileMidi = SoftFile "notes.mid" $ SoftChart midiOnyx
      , _fileSongAnim        = Nothing -- TODO actually extract this
      , _autogenTheme        = Nothing
      , _backgroundVideo     = Nothing
      , _fileBackgroundImage = Nothing
      }
    , _audio = HM.empty
    , _jammit = HM.empty
    , _plans = HM.singleton "mogg" MoggPlan
      { _fileMOGG = Just $ SoftFile "audio.mogg" $ rbiMOGG rbi
      , _moggMD5 = Nothing
      , _moggParts = Parts $ HM.fromList $ concat
        [ [ (FlexGuitar, PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
        , [ (FlexBass  , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
        , [ (FlexKeys  , PartSingle ns) | ns <- toList $ lookup "keys"   instChans ]
        , [ (FlexVocal , PartSingle ns) | ns <- toList $ lookup "vocals" instChans ]
        , [ (FlexDrums , ds           ) | Just ds <- [drumSplit] ]
        ]
      , _moggCrowd = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
      , _pans = map realToFrac $ D.pans $ D.song pkg
      , _vols = map realToFrac $ D.vols $ D.song pkg
      , _planComments = []
      , _tuningCents = maybe 0 round $ D.tuningOffsetCents pkg
      , _fileTempo = Nothing
      , _karaoke = fromMaybe False $ c3dtaKaraoke $ rbiComments rbi
      , _multitrack = fromMaybe True $ c3dtaMultitrack $ rbiComments rbi
      }
    , _targets = let
      getSongID = \case
        Left  i -> guard (i /= 0) >> Just (Left i)
        Right k -> Just $ Right k
      songID1x = D.songId pkg >>= getSongID
      songID2x = if hasKicks == Kicks2x
        then songID1x
        else files2x >>= D.songId . fst >>= getSongID
      version1x = songID1x >> Just (D.version pkg)
      version2x = songID2x >> fmap (D.version . fst) files2x
      targetShared = def'
        { rb3_Harmonix = dtaIsHarmonixRB3 pkg
        , rb3_FileMilo = localMilo
        }
      target1x = ("rb3", RB3 targetShared
        { rb3_2xBassPedal = False
        , rb3_SongID = songID1x
        , rb3_Version = version1x
        })
      target2x = ("rb3-2x", RB3 targetShared
        { rb3_2xBassPedal = True
        , rb3_SongID = songID2x
        , rb3_Version = version2x
        })
      in HM.fromList $ concat [[target1x | hasKicks /= Kicks2x], [target2x | hasKicks /= Kicks1x]]
    , _parts = Parts $ HM.fromList
      [ ( FlexDrums, def
        { partDrums = guard (hasRankStr "drum") >> Just PartDrums
          { drumsDifficulty = fromMaybe (Tier 1) $ HM.lookup "drum" diffMap
          , drumsMode = DrumsPro
          , drumsKicks = hasKicks
          , drumsFixFreeform = False
          , drumsKit = drumkit
          , drumsLayout = StandardLayout -- TODO import this
          , drumsFallback = FallbackGreen
          }
        })
      , ( FlexGuitar, def
        { partGRYBO = guard (hasRankStr "guitar") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "guitar" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = guard (hasRankStr "real_guitar") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_guitar" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = GtrTuning
            { gtrBase = Guitar6
            , gtrOffsets = map fromIntegral $ fromMaybe [] $ D.realGuitarTuning pkg
            , gtrGlobal = 0
            , gtrCapo = 0
            }
          , pgTuningRSBass  = Nothing
          , pgFixFreeform   = False
          , pgTones         = Nothing
          , pgPickedBass    = False
          }
        })
      , ( FlexBass, def
        { partGRYBO = guard (hasRankStr "bass") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "bass" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProGuitar = guard (hasRankStr "real_bass") >> Just PartProGuitar
          { pgDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_bass" diffMap
          , pgHopoThreshold = hopoThresh
          , pgTuning = GtrTuning
            { gtrBase = bassBase
            , gtrOffsets = map fromIntegral $ fromMaybe [] $ D.realBassTuning pkg
            , gtrGlobal = 0
            , gtrCapo = 0
            }
          , pgTuningRSBass  = Nothing
          , pgFixFreeform   = False
          , pgTones         = Nothing
          , pgPickedBass    = False
          }
        })
      , ( FlexKeys, def
        { partGRYBO = guard (hasRankStr "keys") >> Just PartGRYBO
          { gryboDifficulty = fromMaybe (Tier 1) $ HM.lookup "keys" diffMap
          , gryboHopoThreshold = hopoThresh
          , gryboFixFreeform = False
          , gryboSmoothFrets = False
          , gryboSustainGap = 60
          }
        , partProKeys = guard (hasRankStr "real_keys") >> Just PartProKeys
          { pkDifficulty = fromMaybe (Tier 1) $ HM.lookup "real_keys" diffMap
          , pkFixFreeform = False
          }
        })
      , ( FlexVocal, def
        { partVocal = flip fmap vocalMode $ \vc -> PartVocal
          { vocalDifficulty = fromMaybe (Tier 1) $ HM.lookup "vocals" diffMap
          , vocalCount = vc
          , vocalGender = D.vocalGender pkg
          , vocalKey = vkey
          -- TODO actually extract lipsync
          , vocalLipsyncRB3 = Nothing
          , vocalLipsyncRB2 = Nothing
          }
        })
      ]
    }
