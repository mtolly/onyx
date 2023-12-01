{- |
Common import functions for RB1, RB2, RB3, TBRB
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
module Onyx.Import.RockBand where

import           Codec.Picture.Types                  (dropTransparency,
                                                       pixelMap)
import           Control.Applicative                  ((<|>))
import           Control.Arrow                        (second)
import           Control.Concurrent.Async             (forConcurrently)
import           Control.Monad                        (forM, forM_, guard,
                                                       unless, when)
import           Control.Monad.IO.Class               (MonadIO)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Char8                as B8
import qualified Data.ByteString.Lazy                 as BL
import qualified Data.Conduit.Audio                   as CA
import           Data.Default.Class                   (def)
import           Data.Foldable                        (toList)
import qualified Data.HashMap.Strict                  as HM
import           Data.List.Extra                      (elemIndex, nubOrd, (\\))
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes, fromMaybe,
                                                       isJust, mapMaybe)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeLatin1,
                                                       decodeUtf8With)
import qualified Data.Text.Encoding                   as TE
import           Data.Text.Encoding.Error             (lenientDecode)
import           Onyx.Audio                           (Audio (..), Edge (..))
import           Onyx.Audio.VGS                       (splitOutVGSChannels,
                                                       vgsChannelCount)
import           Onyx.Codec.Binary                    (bin, codecIn, (=.))
import           Onyx.Codec.Common                    (req)
import           Onyx.Difficulty
import           Onyx.Harmonix.Ark.ArkTool            (ark_DecryptVgs)
import qualified Onyx.Harmonix.DTA                    as D
import           Onyx.Harmonix.DTA.C3                 (C3DTAComments (..),
                                                       DTASingle (..),
                                                       readDTASingles,
                                                       readDTBSingles)
import qualified Onyx.Harmonix.DTA.Serialize          as D
import           Onyx.Harmonix.DTA.Serialize.Magma    (Gender (..))
import qualified Onyx.Harmonix.DTA.Serialize.RockBand as D
import           Onyx.Harmonix.Magma                  (rbaContents)
import           Onyx.Harmonix.RockBand.Milo          (SongPref (..),
                                                       decompressMilo,
                                                       miloToFolder,
                                                       parseMiloFile)
import           Onyx.Harmonix.RockBand.RB4.RBMid
import           Onyx.Harmonix.RockBand.RB4.SongDTA
import           Onyx.Image.DXT.RB4
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import           Onyx.MIDI.Track.Drums                as Drums
import qualified Onyx.MIDI.Track.File                 as F
import           Onyx.MIDI.Track.ProGuitar            (GtrBase (..),
                                                       GtrTuning (..))
import           Onyx.PlayStation.PSS                 (extractVGSStream,
                                                       extractVideoStream,
                                                       scanPackets)
import           Onyx.Project
import           Onyx.Resources                       (rb3Updates)
import           Onyx.StackTrace
import           Onyx.Util.Handle                     (Folder (..), Readable,
                                                       byteStringSimpleHandle,
                                                       fileReadable,
                                                       findByteString,
                                                       findFileCI,
                                                       handleToByteString,
                                                       makeHandle, splitPath,
                                                       useHandle)
import           Onyx.Xbox.STFS                       (runGetM)
import qualified Sound.MIDI.File.Save                 as Save
import qualified Sound.MIDI.Util                      as U
import qualified System.Directory                     as Dir
import           System.FilePath                      (takeDirectory,
                                                       takeFileName, (-<.>),
                                                       (<.>), (</>))
import           System.IO.Temp                       (withSystemTempDirectory)
import           Text.Read                            (readMaybe)

data RBImport = RBImport
  { songPackage :: D.SongPackage
  , comments    :: C3DTAComments
  , mogg        :: Maybe SoftContents
  , pss         :: Maybe (IO (SoftFile, [BL.ByteString])) -- if ps2, load video and vgs channels
  , albumArt    :: Maybe (IO (Either String SoftFile))
  , milo        :: Maybe Readable
  , midi        :: Readable
  , midiUpdate  :: Maybe (IO (Either String Readable))
  , source      :: Maybe FilePath
  }

importSTFSFolder :: (SendMessage m, MonadIO m) => FilePath -> Folder T.Text Readable -> StackTraceT m [Import m]
importSTFSFolder src folder = do
  -- TODO some lookups like findByteString, we may want to make case-insensitive
  packSongs <- stackIO (findByteString ("songs" :| ["songs.dta"]) folder) >>= \case
    Just bs -> readDTASingles $ BL.toStrict bs
    Nothing -> stackIO (findByteString ("songs" :| ["gen", "songs.dtb"]) folder) >>= \case
      Just bs -> readDTBSingles $ BL.toStrict bs
      Nothing -> fatal "Couldn't find songs/songs.dta or songs/gen/songs.dtb"
  updateDir <- stackIO rb3Updates
  fmap catMaybes $ forM packSongs $ \(DTASingle top pkg comments, _) -> errorToWarning $ do
    let base = T.unpack $ D.songName $ D.song pkg
        split s = case splitPath $ T.pack s of
          Nothing -> fatal $ "Internal error, couldn't parse path: " <> show s
          Just p  -> return p
        need p = case findFileCI p folder of
          Just r  -> return r
          Nothing -> fatal $ "Required file not found: " <> T.unpack (T.intercalate "/" $ toList p)
    miloPath <- split $ takeDirectory base </> "gen" </> takeFileName base <.> "milo_xbox"
    moggPath <- split $ base <.> "mogg"
    pssPath  <- split $ base <.> "pss"
    midiPath <- split $ base <.> "mid"
    artPathXbox <- split $ takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_xbox")
    artPathPS3 <- split $ takeDirectory base </> "gen" </> (takeFileName base ++ "_keep.png_ps3")

    let mogg = SoftReadable <$> findFileCI moggPath folder
        pss = case findFileCI pssPath folder of
          Nothing -> Nothing
          Just r -> Just $ do
            (bsVideo, bsVGS) <- useHandle r $ \h -> do
              packets <- scanPackets h
              vid <- extractVideoStream 0xE0 packets h
              vgs <- extractVGSStream   0xBD packets h
              return (vid, vgs)
            let rVideo = SoftReadable $ makeHandle "video.m2v" $ byteStringSimpleHandle bsVideo
            chans <- withSystemTempDirectory "decrypt-vgs" $ \temp -> do
              let enc = temp </> "enc.vgs"
                  dec = temp </> "dec.vgs"
                  rVGS = fileReadable dec
              BL.writeFile enc bsVGS
              ark_DecryptVgs dec enc >>= \b -> unless b $ fail "Couldn't decrypt VGS file"
              numChannels <- vgsChannelCount rVGS
              forConcurrently [0 .. numChannels - 1] $ \i -> do
                splitOutVGSChannels [i] rVGS
            return (SoftFile "video.m2v" rVideo, chans)

    midi <- need midiPath
    let missingArt = updateDir </> T.unpack top </> "gen" </> (T.unpack top ++ "_keep.png_xbox")
        updateMid = updateDir </> T.unpack top </> (T.unpack top ++ "_update.mid")
    art <- if fromMaybe False (D.albumArt pkg) || D.gameOrigin pkg == Just "beatles"
      then return $ Just $ do
        Dir.doesFileExist missingArt >>= \case
          -- if True, old rb1 song with album art on rb3 disc
          True -> return $ Right $ SoftFile "cover.png_xbox" $ SoftReadable $ fileReadable missingArt
          False -> case findFileCI artPathXbox folder of
            Just res -> return $ Right $ SoftFile "cover.png_xbox" $ SoftReadable res
            Nothing -> case findFileCI artPathPS3 folder of
              Just res -> return $ Right $ SoftFile "cover.png_ps3" $ SoftReadable res
              Nothing -> return $ Left $ "Expected album art, but didn't find it: " <> show artPathXbox
      else return Nothing
    update <- if maybe False ("disc_update" `elem`) $ D.extraAuthoring pkg
      then return $ Just $ do
        Dir.doesFileExist updateMid >>= return . \case
          True  -> Right $ fileReadable updateMid
          False -> Left $ "Expected to find disc update MIDI but it's not installed: " <> updateMid
      else return Nothing
    return $ importRB RBImport
      { songPackage = pkg
      , comments = comments
      , mogg = mogg
      , pss = pss
      , albumArt = art
      , milo = findFileCI miloPath folder
      , midi = midi
      , midiUpdate = update
      , source = Just src
      }

importRBA :: (SendMessage m, MonadIO m) => FilePath -> Import m
importRBA rba level = do
  when (level == ImportFull) $ lg $ "Importing RBA from: " <> rba
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
  milo <- need 3
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
    { songPackage = pkg
    , comments = comments
      { c3dtaAuthoredBy = author
      }
    , mogg = Just mogg
    , pss = Nothing
    , albumArt = Just $ return $ Right bmp
    , milo = Just milo
    , midi = midi
    , midiUpdate = Nothing
    , source = Nothing
    } level

dtaIsRB3 :: D.SongPackage -> Bool
dtaIsRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc", "ugc_plus"]) $ D.gameOrigin pkg
  -- rbn1 songs have (game_origin rb2) (ugc 1)

dtaIsHarmonixRB3 :: D.SongPackage -> Bool
dtaIsHarmonixRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc"]) $ D.gameOrigin pkg

-- Time in seconds that the video/audio should start before the midi begins.
rockBandPS2PreSongTime :: (Fractional a) => D.SongPackage -> a
rockBandPS2PreSongTime pkg = if D.video pkg then 5 else 3

importRB :: (SendMessage m, MonadIO m) => RBImport -> Import m
importRB rbi level = do

  let pkg = rbi.songPackage
      files2x = Nothing
      (title, auto2x) = determine2xBass $ D.name pkg
      is2x = fromMaybe auto2x $ c3dta2xBass rbi.comments
      hasKicks = if is2x then Kicks2x else Kicks1x

  when (level == ImportFull) $ forM_ rbi.source $ \src -> do
    lg $ "Importing Rock Band song [" <> T.unpack (D.songName $ D.song pkg) <> "] from: " <> src

  (midiFixed, midiOnyx) <- case level of
    ImportFull -> do
      F.Song temps sigs (F.RawFile trks1x) <- F.loadMIDIReadable rbi.midi
      trksUpdate <- case rbi.midiUpdate of
        Nothing -> return []
        Just getUpdate -> stackIO getUpdate >>= \case
          Left  err  -> warn err >> return []
          Right umid -> F.rawTracks . F.s_tracks <$> F.loadMIDIReadable umid
      let updatedNames = map Just $ mapMaybe U.trackName trksUpdate
          trksUpdated
            = filter ((`notElem` updatedNames) . U.trackName) trks1x
            ++ trksUpdate
      midiFixed <- fmap checkEnableDynamics $ F.interpretMIDIFile $ F.Song temps sigs trksUpdated
      return (midiFixed, midiFixed { F.s_tracks = F.fixedToOnyx $ F.s_tracks midiFixed })
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
  let diffMap :: HM.HashMap T.Text Onyx.Project.Difficulty
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

  let drumEvents = F.fixedPartDrums $ F.s_tracks midiFixed
  (foundMix, foundMixStr) <- let
    drumMixes = do
      (_, dd) <- Map.toList $ drumDifficulties drumEvents
      (aud, _dsc) <- toList $ drumMix dd
      return aud
    in case drumMixes of
      [] -> return (Nothing, "MIDI has no mix")
      aud : auds -> if all (== aud) auds
        then return (Just aud, "MIDI has mix " <> take 1 (reverse $ show aud))
        else do
          warn $ "Inconsistent drum mixes: " ++ show (nubOrd drumMixes)
          return (Nothing, "MIDI specifies more than one mix")
  let instChans :: [(T.Text, [Int])]
      instChans = map (second $ map fromIntegral) $ D.fromDictList $ D.tracks $ D.song pkg
      drumChans = fromMaybe [] $ lookup "drum" instChans
  drumSplit <- if not (hasRankStr "drum") || level == ImportQuick then return Nothing else do
    -- Usually there should be 2-6 drum channels and a matching mix event. Seen exceptions:
    -- * The Kill (30STM) has 5 drum channels but no mix event
    -- * RB PS2 has wrong mix events leftover from 360/PS3, e.g. Can't Let Go has 4 drum channels but mix 3
    -- So, just use what the mix should be based on channel count. (But warn appropriately)
    case drumChans of
      [kitL, kitR] -> do
        when (foundMix /= Just Drums.D0) $ warn $ "Using drum mix 0 (2 drum channels found), " <> foundMixStr
        return $ Just $ PartSingle [kitL, kitR]
      [kick, snare, kitL, kitR] -> do
        when (foundMix /= Just Drums.D1) $ warn $ "Using drum mix 1 (4 drum channels found), " <> foundMixStr
        return $ Just $ PartDrumKit (Just [kick]) (Just [snare]) [kitL, kitR]
      [kick, snareL, snareR, kitL, kitR] -> do
        when (foundMix /= Just Drums.D2) $ warn $ "Using drum mix 2 (5 drum channels found), " <> foundMixStr
        return $ Just $ PartDrumKit (Just [kick]) (Just [snareL, snareR]) [kitL, kitR]
      [kickL, kickR, snareL, snareR, kitL, kitR] -> do
        when (foundMix /= Just Drums.D3) $ warn $ "Using drum mix 3 (6 drum channels found), " <> foundMixStr
        return $ Just $ PartDrumKit (Just [kickL, kickR]) (Just [snareL, snareR]) [kitL, kitR]
      [kick, kitL, kitR] -> do
        when (foundMix /= Just Drums.D4) $ warn $ "Using drum mix 4 (3 drum channels found), " <> foundMixStr
        return $ Just $ PartDrumKit (Just [kick]) Nothing [kitL, kitR]
      _ -> do
        warn $ "Unexpected number of drum channels (" <> show (length drumChans) <> "), importing as single-track stereo (mix 0)"
        return $ Just $ PartSingle drumChans

  let tone = fromMaybe Minor $ D.songTonality pkg
      -- Minor verified as default for PG chords if SK/VTN present and no song_tonality
      (skey, vkey) = case (D.songKey pkg, D.vocalTonicNote pkg) of
        (Just sk, Just vtn) -> (Just $ SongKey sk  tone, Just vtn)
        (Just sk, Nothing ) -> (Just $ SongKey sk  tone, Nothing )
        (Nothing, Just vtn) -> (Just $ SongKey vtn tone, Nothing )
        (Nothing, Nothing ) -> (Nothing                , Nothing )

      bassBase = detectExtProBass $ F.s_tracks midiFixed

  miloFolder <- case (level, rbi.milo) of
    (ImportFull, Just milo) -> errorToWarning $ do
      bs <- stackIO $ useHandle milo handleToByteString
      dec <- runGetM decompressMilo bs
      snd . miloToFolder <$> runGetM parseMiloFile dec
    _ -> return Nothing
  let flatten folder = folderFiles folder <> concatMap (flatten . snd) (folderSubfolders folder)
      flat = maybe [] flatten miloFolder
      lipsyncNames = ["song.lipsync", "part2.lipsync", "part3.lipsync", "part4.lipsync"]
      getLipsyncFile name = do
        bs <- lookup name flat
        return
          $ LipsyncFile
          $ SoftFile (B8.unpack name)
          $ SoftReadable
          $ makeHandle (B8.unpack name)
          $ byteStringSimpleHandle bs
      takeWhileJust (Just x : xs) = x : takeWhileJust xs
      takeWhileJust _             = []
  songPref <- case lookup "BandSongPref" flat of
    Nothing -> return Nothing
    Just bs -> do
      lg "Loading BandSongPref"
      errorToWarning $ runGetM (codecIn bin) bs
  let lookupPref fn defAssign = case songPref of
        Nothing -> return defAssign
        Just pref -> case fn pref of
          "guitar" -> return LipsyncGuitar
          "bass" -> return LipsyncBass
          "drum" -> return LipsyncDrums
          x -> do
            warn $ "Unrecognized lipsync part assignment: " <> show x
            return defAssign
  pref2 <- lookupPref prefPart2 LipsyncGuitar
  pref3 <- lookupPref prefPart3 LipsyncBass
  pref4 <- lookupPref prefPart4 LipsyncDrums
  -- TODO also import the animation style parameter
  let lipsync = case takeWhileJust $ map getLipsyncFile lipsyncNames of
        []   -> Nothing
        srcs -> Just $ LipsyncRB3 srcs pref2 pref3 pref4
  songAnim <- case lookup "song.anim" flat of
    Nothing -> return Nothing
    Just bs -> do
      lg "Loading song.anim"
      return $ Just $ SoftFile "song.anim" $ SoftReadable $ makeHandle "song.anim" $ byteStringSimpleHandle bs

  (video, vgs) <- case guard (level == ImportFull) >> rbi.pss of
    Nothing     -> return (Nothing, Nothing)
    Just getPSS -> do
      (video, vgs) <- stackIO getPSS
      return (Just video, Just vgs)
  let namedChans = do
        (i, chan) <- zip [0..] $ fromMaybe [] vgs
        return ("vgs-" <> show (i :: Int), chan)

  art <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> case rbi.albumArt of
      Nothing -> return Nothing
      Just getArt -> stackIO getArt >>= \case
        Left  err -> warn err >> return Nothing
        Right art -> return $ Just art

  return SongYaml
    { metadata = Metadata
      { title        = Just title
      , titleJP      = Nothing
      , artist       = case (D.artist pkg, D.gameOrigin pkg) of
        (Nothing, Just "beatles") -> Just "The Beatles"
        _                         -> D.artist pkg
      , artistJP     = Nothing
      , album        = D.albumName pkg
      , genre        = D.genre pkg
      , subgenre     = D.subGenre pkg >>= T.stripPrefix "subgenre_"
      , year         = case (D.yearReleased pkg, D.gameOrigin pkg, D.dateReleased pkg) of
        (Nothing, Just "beatles", Just date) -> readMaybe $ T.unpack $ T.take 4 date
        _ -> fromIntegral <$> D.yearReleased pkg
      , fileAlbumArt = art
      , trackNumber  = fromIntegral <$> D.albumTrackNumber pkg
      , comments     = []
      , difficulty   = fromMaybe (Tier 1) $ HM.lookup "band" diffMap
      , key          = skey
      , author       = D.author pkg <|> c3dtaAuthoredBy rbi.comments
      , rating       = toEnum $ fromIntegral $ D.rating pkg - 1
      , previewStart = Just $ PreviewSeconds $ fromIntegral (fst $ D.preview pkg) / 1000
      , previewEnd   = Just $ PreviewSeconds $ fromIntegral (snd $ D.preview pkg) / 1000
      , languages    = fromMaybe [] $ c3dtaLanguages rbi.comments
      , convert      = fromMaybe False $ c3dtaConvert rbi.comments
      , rhythmKeys   = fromMaybe False $ c3dtaRhythmKeys rbi.comments
      , rhythmBass   = fromMaybe False $ c3dtaRhythmBass rbi.comments
      , catEMH       = fromMaybe False $ c3dtaCATemh rbi.comments
      , expertOnly   = fromMaybe False $ c3dtaExpertOnly rbi.comments
      , cover        = not $ D.master pkg || D.gameOrigin pkg == Just "beatles"
      , loadingPhrase = D.loadingPhrase pkg
      }
    , global = def'
      { animTempo           = D.animTempo pkg
      , fileMidi            = SoftFile "notes.mid" $ SoftChart midiOnyx
      , fileSongAnim        = songAnim
      , backgroundVideo     = flip fmap video $ \videoFile -> VideoInfo
        { fileVideo      = videoFile
        , videoStartTime = Just $ rockBandPS2PreSongTime pkg
        , videoEndTime   = Nothing
        , videoLoop      = False
        }
      , fileBackgroundImage = Nothing
      }
    , audio = HM.fromList $ do
      (name, bs) <- namedChans
      return $ (T.pack name ,) $ AudioFile AudioInfo
        { md5 = Nothing
        , frames = Nothing
        , commands = []
        , filePath = Just $ SoftFile (name <.> "vgs") $ SoftReadable $ makeHandle name $ byteStringSimpleHandle bs
        , rate = Nothing
        , channels = 1
        }
    , jammit = HM.empty
    , plans = case rbi.mogg of
      Nothing -> case rbi.pss of
        Nothing -> HM.empty
        Just _pss -> HM.singleton "vgs" $ let
          songChans = [0 .. length namedChans - 1] \\ concat
            [ concatMap snd instChans
            , maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
            ]
          audioAdjust = Drop Start $ CA.Seconds $ rockBandPS2PreSongTime pkg
          mixChans cs = do
            cs' <- NE.nonEmpty cs
            Just $ case cs' of
              c :| [] -> PansVols
                (map realToFrac [D.pans (D.song pkg) !! c])
                (map realToFrac [D.vols (D.song pkg) !! c])
                (audioAdjust $ Input $ Named $ T.pack $ fst $ namedChans !! c)
              _ -> PansVols
                (map realToFrac [D.pans (D.song pkg) !! c | c <- cs])
                (map realToFrac [D.vols (D.song pkg) !! c | c <- cs])
                (audioAdjust $ Merge $ fmap (Input . Named . T.pack . fst . (namedChans !!)) cs')
          in StandardPlan StandardPlanInfo
            { song = mixChans songChans
            , parts = Parts $ HM.fromList $ catMaybes
              [ lookup "guitar" instChans >>= mixChans >>= \x -> return (F.FlexGuitar, PartSingle x)
              , lookup "bass"   instChans >>= mixChans >>= \x -> return (F.FlexBass  , PartSingle x)
              , lookup "keys"   instChans >>= mixChans >>= \x -> return (F.FlexKeys  , PartSingle x)
              , lookup "vocals" instChans >>= mixChans >>= \x -> return (F.FlexVocal , PartSingle x)
              , drumSplit >>= mapM mixChans            >>= \x -> return (F.FlexDrums , x)
              ]
            , crowd = D.crowdChannels (D.song pkg) >>= mixChans . map fromIntegral
            , comments = []
            , tuningCents = maybe 0 round $ D.tuningOffsetCents pkg
            , fileTempo = Nothing
            }
      Just mogg -> HM.singleton "mogg" $ MoggPlan MoggPlanInfo
        { fileMOGG = Just $ SoftFile "audio.mogg" mogg
        , moggMD5 = Nothing
        , parts = Parts $ HM.fromList $ concat
          [ [ (F.FlexGuitar, PartSingle ns) | ns <- toList $ lookup "guitar" instChans ]
          , [ (F.FlexBass  , PartSingle ns) | ns <- toList $ lookup "bass"   instChans ]
          , [ (F.FlexKeys  , PartSingle ns) | ns <- toList $ lookup "keys"   instChans ]
          , [ (F.FlexVocal , PartSingle ns) | ns <- toList $ lookup "vocals" instChans ]
          , [ (F.FlexDrums , ds           ) | Just ds <- [drumSplit] ]
          ]
        , crowd = maybe [] (map fromIntegral) $ D.crowdChannels $ D.song pkg
        , pans = map realToFrac $ D.pans $ D.song pkg
        , vols = map realToFrac $ D.vols $ D.song pkg
        , comments = []
        , tuningCents = maybe 0 round $ D.tuningOffsetCents pkg
        , fileTempo = Nothing
        , karaoke = fromMaybe False $ c3dtaKaraoke rbi.comments
        , multitrack = fromMaybe True $ c3dtaMultitrack rbi.comments
        , decryptSilent = False
        }
    , targets = let
      getSongID = \case
        Left  i -> if i /= 0
          then SongIDInt $ fromIntegral i
          else SongIDAutoSymbol
        Right k -> SongIDSymbol k
      songID1x = maybe SongIDAutoSymbol getSongID $ D.songId pkg
      songID2x = if hasKicks == Kicks2x
        then songID1x
        else maybe SongIDAutoSymbol getSongID $ files2x >>= D.songId . fst
      targetShared = def
        { harmonix = dtaIsHarmonixRB3 pkg
        }
      target1x = ("rb3", RB3 targetShared
        { is2xBassPedal = False
        , songID = songID1x
        })
      target2x = ("rb3-2x", RB3 targetShared
        { is2xBassPedal = True
        , songID = songID2x
        })
      in HM.fromList $ concat [[target1x | hasKicks /= Kicks2x], [target2x | hasKicks /= Kicks1x]]
    , parts = Parts $ HM.fromList
      [ ( F.FlexDrums, (emptyPart :: Part SoftFile)
        { drums = guard (hasRankStr "drum") >> Just (emptyPartDrums DrumsPro hasKicks)
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "drum" diffMap
          , kit = drumkit
          , layout = StandardLayout -- TODO import this
          }
        })
      , ( F.FlexGuitar, (emptyPart :: Part SoftFile)
        { grybo = guard (hasRankStr "guitar") >> Just PartGRYBO
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "guitar" diffMap
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = guard (hasRankStr "real_guitar") >> Just PartProGuitar
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "real_guitar" diffMap
          , hopoThreshold = hopoThresh
          , tuning = GtrTuning
            { gtrBase = Guitar6
            , gtrOffsets = map fromIntegral $ fromMaybe [] $ D.realGuitarTuning pkg
            , gtrGlobal = 0
            , gtrCapo = 0
            }
          , tuningRSBass  = Nothing
          , fixFreeform   = False
          , tones         = Nothing
          , pickedBass    = False
          }
        })
      , ( F.FlexBass, (emptyPart :: Part SoftFile)
        { grybo = guard (hasRankStr "bass") >> Just PartGRYBO
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "bass" diffMap
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proGuitar = guard (hasRankStr "real_bass") >> Just PartProGuitar
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "real_bass" diffMap
          , hopoThreshold = hopoThresh
          , tuning = GtrTuning
            { gtrBase = bassBase
            , gtrOffsets = map fromIntegral $ fromMaybe [] $ D.realBassTuning pkg
            , gtrGlobal = 0
            , gtrCapo = 0
            }
          , tuningRSBass  = Nothing
          , fixFreeform   = False
          , tones         = Nothing
          , pickedBass    = False
          }
        })
      , ( F.FlexKeys, (emptyPart :: Part SoftFile)
        { grybo = guard (hasRankStr "keys") >> Just PartGRYBO
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "keys" diffMap
          , hopoThreshold = hopoThresh
          , fixFreeform = False
          , smoothFrets = False
          , sustainGap = 60
          , detectMutedOpens = True
          }
        , proKeys = guard (hasRankStr "real_keys") >> Just PartProKeys
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "real_keys" diffMap
          , fixFreeform = False
          }
        })
      , ( F.FlexVocal, (emptyPart :: Part SoftFile)
        { vocal = flip fmap vocalMode $ \vc -> PartVocal
          { difficulty = fromMaybe (Tier 1) $ HM.lookup "vocals" diffMap
          , count = vc
          , gender = D.vocalGender pkg
          , key = vkey
          , lipsyncRB3 = lipsync
          }
        })
      ]
    }

importRB4 :: (SendMessage m, MonadIO m) => FilePath -> Import m
importRB4 fdta level = do
  dta <- stackIO (BL.fromStrict <$> B.readFile fdta) >>= runGetM (codecIn bin)
  let _ = dta :: SongDTA

  moggDTA <- stackIO (D.readFileDTA $ fdta -<.> "mogg.dta") >>= D.unserialize D.stackChunks
  let _ = moggDTA :: RB4MoggDta

  (midRead, hopoThreshold) <- case level of
    ImportQuick -> return (makeHandle "notes.mid" $ byteStringSimpleHandle BL.empty, 170)
    ImportFull  -> do
      rbmid <- stackIO (BL.fromStrict <$> B.readFile (fdta -<.> "rbmid_ps4")) >>= runGetM (codecIn bin)
      mid <- extractMidi rbmid
      return (makeHandle "notes.mid" $ byteStringSimpleHandle $ Save.toByteString mid, rbmid_HopoThreshold rbmid)

  -- dta.albumArt doesn't appear to be correct, it is False sometimes when song should have art
  art <- if level == ImportFull
    then errorToWarning $ do
      img <- stackIO (BL.fromStrict <$> B.readFile (fdta -<.> "png_ps4")) >>= runGetM readPNGPS4
      return $ SoftFile "cover.png" $ SoftImage $ pixelMap dropTransparency img
    else return Nothing

  let decodeBS = TE.decodeUtf8 -- is this correct? find example with non-ascii char
      pkg = D.SongPackage
        { D.name              = decodeBS dta.name
        , D.artist            = Just $ decodeBS dta.artist
        , D.master            = not dta.cover
        , D.song              = D.Song
          { D.songName         = decodeBS dta.shortname
          , D.tracksCount      = Nothing
          -- should be fine to keep 'fake' in tracks list, ignore later
          , D.tracks           = D.DictList
            -- need to merge the duplicate drum keys (see below)
            $ Map.toList
            $ Map.unionsWith (<>)
            $ map (uncurry Map.singleton)
            $ moggDTA.tracks
          , D.pans             = moggDTA.pans
          , D.vols             = moggDTA.vols
          , D.cores            = map (const (-1)) moggDTA.pans
          , D.crowdChannels    = Nothing -- TODO
          , D.vocalParts       = Just $ fromIntegral dta.vocalParts
          , D.drumSolo         = D.DrumSounds [] -- not used
          , D.drumFreestyle    = D.DrumSounds [] -- not used
          , D.muteVolume       = Nothing -- not used
          , D.muteVolumeVocals = Nothing -- not used
          , D.hopoThreshold    = Just $ fromIntegral hopoThreshold
          , D.midiFile         = Nothing
          }
        , D.songScrollSpeed   = 2300
        , D.bank              = Nothing
        , D.drumBank          = Nothing
        , D.animTempo         = case dta.animTempo of
          "medium" -> Left D.KTempoMedium
          -- TODO
          _        -> Left D.KTempoMedium
        , D.songLength        = Just $ round dta.songLength
        , D.preview           = (round dta.previewStart, round dta.previewEnd)
        , D.rank              = HM.fromList
          [ ("drum"       , round dta.drumRank    )
          , ("bass"       , round dta.bassRank    )
          , ("guitar"     , round dta.guitarRank  )
          , ("vocals"     , round dta.vocalsRank  )
          , ("keys"       , round dta.keysRank    )
          , ("real_keys"  , round dta.realKeysRank)
          , ("band"       , round dta.bandRank    )
          ]
        , D.genre             = Just $ decodeBS dta.genre
        , D.vocalGender       = case dta.vocalGender of
          1 -> Just Male
          2 -> Just Female
          _ -> Nothing
        , D.version           = fromIntegral dta.version
        , D.songFormat        = 10 -- we'll call it rb3 format for now
        , D.albumArt          = Just $ isJust art
        , D.yearReleased      = Just $ fromIntegral dta.albumYear
        , D.rating            = 4 -- TODO
        , D.subGenre          = Nothing
        , D.songId            = Just $ Left $ fromIntegral dta.songId
        , D.solo              = Nothing
        , D.tuningOffsetCents = Nothing -- TODO
        , D.guidePitchVolume  = Nothing
        , D.gameOrigin        = Just $ decodeBS dta.gameOrigin
        , D.encoding          = Just "utf8" -- dunno
        , D.albumName         = Just $ decodeBS dta.albumName -- can this be blank?
        , D.albumTrackNumber  = Just $ fromIntegral dta.albumTrackNumber
        , D.vocalTonicNote    = Nothing -- TODO
        , D.songTonality      = Nothing -- TODO
        , D.realGuitarTuning  = Nothing
        , D.realBassTuning    = Nothing
        , D.bandFailCue       = Nothing
        , D.fake              = Just dta.fake
        , D.ugc               = Nothing
        , D.shortVersion      = Nothing
        , D.yearRecorded      = guard (dta.originalYear /= dta.albumYear) >> Just (fromIntegral dta.originalYear)
        , D.packName          = Nothing
        , D.songKey           = Nothing
        , D.extraAuthoring    = Nothing
        , D.context           = Nothing
        , D.decade            = Nothing
        , D.downloaded        = Nothing
        , D.basePoints        = Nothing
        , D.alternatePath     = Nothing
        , D.videoVenues       = Nothing
        , D.dateReleased      = Nothing
        , D.dateRecorded      = Nothing
        , D.author            = Nothing
        , D.loadingPhrase     = Nothing
        , D.video             = False
        }

  importRB RBImport
    { songPackage = pkg
    , comments = def
    , mogg = Just $ SoftReadable $ fileReadable $ fdta -<.> "mogg"
    , pss = Nothing
    , albumArt = return . Right <$> art
    , milo = Nothing
    , midi = midRead
    , midiUpdate = Nothing
    , source = Nothing
    } level

{-

note: unlike pre-rb4, the tracks list has duplicate keys if there's more than 1 drum stream:

  (drum
     (0)
  )
  (drum
     (1 2)
  )
  (drum
     (3 4)
  )

-}

data RB4MoggDta = RB4MoggDta
  { tracks :: [(T.Text, [Integer])]
  , pans   :: [Float]
  , vols   :: [Float]
  } deriving (Eq, Show)

instance D.StackChunks RB4MoggDta where
  stackChunks = D.asWarnAssoc "RB4MoggDta" $ do
    tracks <- (.tracks) =. req "tracks" (D.chunksParens $ D.chunksList $ D.chunkParens $ D.chunksKeyRest D.chunkSym D.channelList)
    pans   <- (.pans)   =. req "pans"   (D.chunksParens D.stackChunks)
    vols   <- (.vols)   =. req "vols"   (D.chunksParens D.stackChunks)
    return RB4MoggDta{..}
