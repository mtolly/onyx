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
                                                       unless, void, when)
import           Control.Monad.IO.Class               (MonadIO)
import           Data.Bifunctor                       (bimap)
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
import           Onyx.Codec.Binary                    (bin, codecIn, codecOut,
                                                       runPut, (=.))
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
import           Onyx.Harmonix.MOGG.Crypt             (decryptBink)
import           Onyx.Harmonix.RockBand.Milo          (SongPref (..),
                                                       convertFromAnim,
                                                       decompressMilo,
                                                       emptyLipsync,
                                                       miloToFolder, parseAnim,
                                                       parseMiloFile, putAnim,
                                                       putLipsync,
                                                       standardVisemeCase)
import           Onyx.Harmonix.RockBand.RB4.Lipsync
import           Onyx.Harmonix.RockBand.RB4.RBMid
import           Onyx.Harmonix.RockBand.RB4.RBSong
import           Onyx.Harmonix.RockBand.RB4.SongDTA
import           Onyx.Image.DXT.RB4
import           Onyx.Import.Base
import           Onyx.MIDI.Common
import           Onyx.MIDI.Read                       (mapTrack)
import           Onyx.MIDI.Track.Drums                as Drums
import           Onyx.MIDI.Track.Events               (eventsEnd)
import qualified Onyx.MIDI.Track.File                 as F
import           Onyx.MIDI.Track.ProGuitar            (GtrBase (..),
                                                       GtrTuning (..))
import           Onyx.MIDI.Track.Venue
import           Onyx.Nintendo.WAD                    (getWAD, hackSplitU8s)
import           Onyx.PlayStation.PSS                 (extractVGSStream,
                                                       extractVideoStream,
                                                       scanPackets)
import           Onyx.Project
import           Onyx.Resources                       (rb3Updates)
import           Onyx.StackTrace
import           Onyx.Util.Binary                     (runGetM)
import           Onyx.Util.Handle                     (Folder (..), Readable,
                                                       allFiles,
                                                       byteStringSimpleHandle,
                                                       fileReadable,
                                                       findByteString,
                                                       findFileCI, findFolder,
                                                       fromFiles,
                                                       handleToByteString,
                                                       makeHandle, splitPath,
                                                       useHandle)
import           Sound.MIDI.File                      (T (..))
import qualified Sound.MIDI.File.Save                 as Save
import qualified Sound.MIDI.Util                      as U
import qualified System.Directory                     as Dir
import           System.FilePath                      ((-<.>), (<.>), (</>))
import           System.IO.Temp                       (withSystemTempDirectory)
import           Text.Read                            (readMaybe)

data RBImport = RBImport
  { songPackage   :: D.SongPackage
  , comments      :: C3DTAComments
  , mogg          :: Maybe SoftContents
  , bink          :: Maybe SoftContents
  , pss           :: Maybe (IO (SoftFile, [BL.ByteString])) -- if ps2, load video and vgs channels
  , albumArt      :: Maybe (IO (Either String SoftFile))
  , milo          :: [(B.ByteString, BL.ByteString)]
  , midi          :: Readable
  , midiUpdate    :: Maybe (IO (Either String Readable))
  , source        :: Maybe FilePath
  , addSingalongs :: [B.ByteString] -- RB4 .lipsync_ps4 slot names
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

    (baseX, baseY) <- case splitPath $ D.songName $ D.song pkg of
      Just ("songs" :| [x, y]) -> return (x, y) -- normal 360/ps3
      Just ("dlc" :| [_, _, "content", "songs", x, y]) -> return (x, y) -- wii dlc
      _ -> fatal $ "Unrecognized `name` field in songs.dta: " <> show (D.songName $ D.song pkg)

    let need p = case findFileCI p folder of
          Just r  -> return r
          Nothing -> fatal $ "Required file not found: " <> T.unpack (T.intercalate "/" $ toList p)
        moggPath = "songs" :| [baseX, baseY <> ".mogg"]
        binkPath = "songs" :| [baseX, baseY <> ".bik"]
        pssPath = "songs" :| [baseX, baseY <> ".pss"]
        -- TODO can midi_file in songs.dta override this?
        -- TODO we should support .mid.edat here with HMX0756 key for loose c3-ps3-format songs
        midiPath = "songs" :| [baseX, baseY <> ".mid"]

    let mogg = SoftReadable <$> findFileCI moggPath folder
        bink = flip fmap (findFileCI binkPath folder) $ \r -> SoftReadable
          $ makeHandle "decrypted bink" $ decryptBink r >>= byteStringSimpleHandle . BL.fromStrict
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
        Dir.doesFileExist missingArt >>= return . \case
          -- if True, old rb1 song with album art on rb3 disc
          True -> Right $ SoftFile "cover.png_xbox" $ SoftReadable $ fileReadable missingArt
          False -> let
            tryArt platform = do
              let path = "songs" :| [baseX, "gen", baseY <> "_keep.png_" <> platform]
              res <- findFileCI path folder
              Just $ SoftFile ("cover.png_" <> T.unpack platform) $ SoftReadable res
            in case mapMaybe tryArt ["xbox", "ps3", "wii"] of
              soft : _ -> Right soft
              []       -> Left "Expected album art, but didn't find it"
      else return Nothing
    update <- if maybe False ("disc_update" `elem`) $ D.extraAuthoring pkg
      then return $ Just $ do
        Dir.doesFileExist updateMid >>= return . \case
          True  -> Right $ fileReadable updateMid
          False -> Left $ "Expected to find disc update MIDI but it's not installed: " <> updateMid
      else return Nothing
    return $ \level -> do
      milo <- let
        tryMilo platform = let
          path = "songs" :| [baseX, "gen", baseY <> ".milo_" <> platform]
          in loadFlatMilo level <$> findFileCI path folder
        in case mapMaybe tryMilo ["xbox", "ps3", "wii"] of
          []         -> return []
          getter : _ -> getter
      importRB RBImport
        { songPackage = pkg
        , comments = comments
        , mogg = mogg
        , bink = bink
        , pss = pss
        , albumArt = art
        , milo = milo
        , midi = midi
        , midiUpdate = update
        , source = Just src
        , addSingalongs = []
        } level

importWAD :: (SendMessage m, MonadIO m) => FilePath -> StackTraceT m [Import m]
importWAD path = do
  wad <- stackIO (B.readFile path) >>= runGetM getWAD . BL.fromStrict
  (_, u8s) <- hackSplitU8s wad
  let folder
        = bimap TE.decodeLatin1 (makeHandle "" . byteStringSimpleHandle)
        $ fromFiles
        $ map fixSongsDTALocation
        $ allFiles
        $ fromMaybe mempty
        $ findFolder (pure "content")
        $ mconcat $ map snd u8s
      -- older songs appear to have the dta under a song folder for some reason?
      -- or maybe our u8 code is messing it up, dunno
      fixSongsDTALocation = \case
        ("songs" :| [_, "songs.dta"], x) -> ("songs" :| ["songs.dta"], x)
        pair                             -> pair
  importSTFSFolder path folder

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
  milo <- need 3 >>= loadFlatMilo level
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
    , bink = Nothing
    , pss = Nothing
    , albumArt = Just $ return $ Right bmp
    , milo = milo
    , midi = midi
    , midiUpdate = Nothing
    , source = Nothing
    , addSingalongs = []
    } level

dtaIsRB3 :: D.SongPackage -> Bool
dtaIsRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc", "ugc_plus"]) $ D.gameOrigin pkg
  -- rbn1 songs have (game_origin rb2) (ugc 1)

dtaIsHarmonixRB3 :: D.SongPackage -> Bool
dtaIsHarmonixRB3 pkg = maybe False (`elem` ["rb3", "rb3_dlc"]) $ D.gameOrigin pkg

-- Time in seconds that the video/audio should start before the midi begins.
rockBandPS2PreSongTime :: (Fractional a) => D.SongPackage -> a
rockBandPS2PreSongTime pkg = if D.video pkg then 5 else 3

loadFlatMilo :: (SendMessage m, MonadIO m) => ImportLevel -> Readable -> StackTraceT m [(B.ByteString, BL.ByteString)]
loadFlatMilo level milo = do
  miloFolder <- case level of
    ImportFull -> errorToWarning $ do
      bs <- stackIO $ useHandle milo handleToByteString
      dec <- runGetM decompressMilo bs
      snd . miloToFolder <$> runGetM parseMiloFile dec
    _ -> return Nothing
  let flatten folder = folderFiles folder <> concatMap (flatten . snd) (folderSubfolders folder)
  return $ maybe [] flatten miloFolder

-- adds singalong notes to the whole length of a song when importing RB4 .lipsync_ps4
midiAddSingalongs :: [B.ByteString] -> F.Song (F.OnyxFile U.Beats) -> F.Song (F.OnyxFile U.Beats)
midiAddSingalongs []    mid = mid
midiAddSingalongs slots mid = mid
  { F.tracks = mid.tracks
    { F.onyxVenue = let
      venue = mid.tracks.onyxVenue
      addSlot slot orig = if elem slot slots
        then case mid.tracks.onyxEvents.eventsEnd of
          -- in theory should be fine going the whole song length, but we'll shorten a bit to be safe
          Wait dt () _ -> Wait 0 True $ Wait (dt - 0.25) False RNil
          RNil         -> orig -- no [end], shouldn't happen
        else orig
      in venue
        { venueSingGuitar = addSlot "guitar" venue.venueSingGuitar
        , venueSingBass   = addSlot "bass"   venue.venueSingBass
        , venueSingDrums  = addSlot "drum"   venue.venueSingDrums
        }
    }
  }

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
          Right umid -> do
            raw <- F.loadMIDIReadable umid
            let _ = raw :: F.Song (F.RawFile U.Beats)
            return raw.tracks.rawTracks
      let updatedNames = map Just $ mapMaybe U.trackName trksUpdate
          trksUpdated
            = filter ((`notElem` updatedNames) . U.trackName) trks1x
            ++ trksUpdate
      midiFixed <- fmap checkEnableDynamics $ F.interpretMIDIFile $ F.Song temps sigs trksUpdated
      return (midiFixed, midiFixed { F.tracks = F.fixedToOnyx midiFixed.tracks })
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

  let drumEvents = midiFixed.tracks.fixedPartDrums
  (foundMix, foundMixStr) <- let
    drumMixes = do
      (_, dd) <- Map.toList drumEvents.drumDifficulties
      (aud, _dsc) <- toList dd.drumMix
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
        return $ Just PartDrumKit
          { kick = Just [kick]
          , snare = Just [snare]
          , toms = Nothing
          , kit = [kitL, kitR]
          }
      [kick, snareL, snareR, kitL, kitR] -> do
        when (foundMix /= Just Drums.D2) $ warn $ "Using drum mix 2 (5 drum channels found), " <> foundMixStr
        return $ Just PartDrumKit
          { kick = Just [kick]
          , snare = Just [snareL, snareR]
          , toms = Nothing
          , kit = [kitL, kitR]
          }
      [kickL, kickR, snareL, snareR, kitL, kitR] -> do
        when (foundMix /= Just Drums.D3) $ warn $ "Using drum mix 3 (6 drum channels found), " <> foundMixStr
        return $ Just PartDrumKit
          { kick = Just [kickL, kickR]
          , snare = Just [snareL, snareR]
          , toms = Nothing
          , kit = [kitL, kitR]
          }
      [kick, kitL, kitR] -> do
        when (foundMix /= Just Drums.D4) $ warn $ "Using drum mix 4 (3 drum channels found), " <> foundMixStr
        return $ Just PartDrumKit
          { kick = Just [kick]
          , snare = Nothing
          , toms = Nothing
          , kit = [kitL, kitR]
          }
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

      bassBase = detectExtProBass midiFixed.tracks

  let lipsyncNames = ["song.lipsync", "part2.lipsync", "part3.lipsync", "part4.lipsync"]
      getLipsyncFile name = do
        bs <- lookup name rbi.milo
        return
          $ LipsyncFile
          $ SoftFile (B8.unpack name)
          $ SoftReadable
          $ makeHandle (B8.unpack name)
          $ byteStringSimpleHandle bs
      takeWhileJust (Just x : xs) = x : takeWhileJust xs
      takeWhileJust _             = []
  songPref <- case lookup "BandSongPref" rbi.milo of
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
  songAnim <- case lookup "song.anim" rbi.milo of
    Nothing -> return Nothing
    Just bs -> do
      parsed <- inside "Loading song.anim" $ errorToWarning $ runGetM parseAnim bs
      return $ Just
        ( SoftFile "song.anim" $ SoftReadable $ makeHandle "song.anim" $ byteStringSimpleHandle bs
        , parsed
        )
  -- if we don't have all 3 guitar/bass/keys, only import the actually used camera track
  let adjustAnimMidi orig = case (hasRankStr "guitar", hasRankStr "bass", hasRankStr "keys") of
        (True, True, True) -> orig
        (_, _, False) -> orig
          { F.onyxCamera = orig.onyxCameraBG
          , F.onyxCameraBG = mempty
          , F.onyxCameraBK = mempty
          , F.onyxCameraGK = mempty
          }
        (False, _, True) -> orig
          { F.onyxCamera = orig.onyxCameraBK
          , F.onyxCameraBG = mempty
          , F.onyxCameraBK = mempty
          , F.onyxCameraGK = mempty
          }
        (True, False, True) -> orig
          { F.onyxCamera = orig.onyxCameraGK
          , F.onyxCameraBG = mempty
          , F.onyxCameraBK = mempty
          , F.onyxCameraGK = mempty
          }
      cutoffAnimAtEnd = case midiOnyx.tracks.onyxEvents.eventsEnd of
        RNil              -> id
        Wait endTime () _ -> chopTake endTime
  -- TODO we may need a smarter way to apply the song.anim data;
  -- various cases of data existing in both song.anim (or .rbsong) and the midi
  midiOnyxWithAnim <- midiAddSingalongs rbi.addSingalongs <$> case songAnim of
    Just (_, Just parsedAnim) -> do
      converted <- convertFromAnim parsedAnim
      return midiOnyx
        { F.tracks = midiOnyx.tracks
          { F.onyxVenue = mempty -- pre-RB3 songs still have old VENUE tracks we can replace entirely
          } <> cutoffAnimAtEnd (adjustAnimMidi $ mapTrack (U.unapplyTempoTrack midiOnyx.tempos) converted)
        }
    _ -> return midiOnyx

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
      , fileMidi            = SoftFile "notes.mid" $ SoftChart midiOnyxWithAnim
      -- disabled now that we're importing it to midi
      , fileSongAnim        = Nothing -- fst <$> songAnim
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
    , plans = let
      makeMoggBinkPlan ext name soft = HM.singleton ext $ MoggPlan MoggPlanInfo
        { fileMOGG = Just $ SoftFile name soft
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
      in case rbi.mogg of
        Just mogg -> makeMoggBinkPlan "mogg" "audio.mogg" mogg
        Nothing -> case rbi.bink of
          Just bink -> makeMoggBinkPlan "bink" "audio.bik" bink
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
            , gtrName = Nothing
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
            , gtrName = Nothing
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

  (midRead, hopoThreshold, beatTrackSeconds) <- case level of
    ImportQuick -> return
      ( makeHandle "notes.mid" $ byteStringSimpleHandle BL.empty
      , 170
      , mempty
      )
    ImportFull  -> do
      rbmid <- inside "Loading .rbmid_ps4" $ do
        stackIO (BL.fromStrict <$> B.readFile (fdta -<.> "rbmid_ps4")) >>= runGetM (codecIn bin)
      mid <- extractMidi rbmid
      -- only need to parse out the BEAT track so we can translate .rbsong times
      beatMidi <- F.readMIDIFile' $ fmap decodeLatin1 $ case mid of
        Cons typ dvn []       -> Cons typ dvn []
        Cons typ dvn (t : ts) -> Cons typ dvn $ t : filter (\trk -> U.trackName trk == Just "BEAT") ts
      let _ = beatMidi :: F.Song (F.OnyxFile U.Beats)
      return
        ( makeHandle "notes.mid" $ byteStringSimpleHandle $ Save.toByteString mid
        , rbmid_HopoThreshold rbmid
        , mapTrack (U.applyTempoTrack beatMidi.tempos) beatMidi.tracks.onyxBeat
        )

  -- dta.albumArt doesn't appear to be correct, it is False sometimes when song should have art
  art <- if level == ImportFull
    then errorToWarning $ do
      img <- stackIO (BL.fromStrict <$> B.readFile (fdta -<.> "png_ps4")) >>= runGetM readPNGPS4
      return $ SoftFile "cover.png" $ SoftImage $ pixelMap dropTransparency img
    else return Nothing

  let rbsongPath  = fdta -<.> "rbsong"
      lipsyncPath = fdta -<.> "lipsync_ps4"
  maybeRBSong <- case level of
    ImportQuick -> return Nothing
    ImportFull  -> stackIO (Dir.doesFileExist rbsongPath) >>= \case
      False -> return Nothing
      True  -> do
        bs <- stackIO $ B.readFile rbsongPath
        inside "Loading .rbsong" $ errorToWarning $ runGetM getEntityResource $ BL.fromStrict bs
  let songMeta       = maybe [] rbSongMetadata   maybeRBSong
      venueAuthoring = maybe [] rbVenueAuthoring maybeRBSong
  (miloLipsync, singalongs) <- case level of
    ImportQuick -> return ([], [])
    ImportFull -> stackIO (Dir.doesFileExist lipsyncPath) >>= \case

      True -> do
        lips <- fmap (fromMaybe []) $ inside "Loading .lipsync_ps4" $ errorToWarning $ do
          bs <- stackIO $ B.readFile lipsyncPath
          runGetM getLipsyncPS4 (BL.fromStrict bs) >>= fromLipsyncPS4

        -- lipsync_ps4 has explicit mic,guitar,bass,drum slots.
        -- to be simple we translate these to parts 1-4 in the default mapping.
        -- no singalong notes are present in the venue, so we need to add them.
        -- note, we need to fill in parts with empty lipsync if there are gaps in what is mapped.
        -- see askyfullofstars which has just mic + drum.
        -- and thesign which has mic bass drum.

        -- TODO 100baddays and change have weird overlapping parts? ["bass+drum","guitar","guitar+bass","mic"]
        -- and lockedoutofheaven has ["guitar+bass","mic","guitar+bass","mic"]

        let lipMapping = do
              (slotString, lip) <- lips
              slot <- B8.split '+' slotString
              return (slot, lip)
            filePart4 = ("part4.lipsync",) <$>  lookup "drum"   lipMapping
            filePart3 = ("part3.lipsync",) <$> (lookup "bass"   lipMapping <|> (guard (isJust filePart4) >> Just emptyLipsync))
            filePart2 = ("part2.lipsync",) <$> (lookup "guitar" lipMapping <|> (guard (isJust filePart3) >> Just emptyLipsync))
            fileSong  = ("song.lipsync" ,) <$> (lookup "mic"    lipMapping <|> (guard (isJust filePart2) >> Just emptyLipsync))
            -- probably don't need to fix viseme case on these but just making sure
            lipsyncs  = map (second $ runPut . putLipsync . standardVisemeCase)
              $ catMaybes [fileSong, filePart2, filePart3, filePart4]
        return (lipsyncs, map fst lipMapping)

      False -> case maybeRBSong of
        Nothing -> return ([], [])
        Just rbsong -> do
          lips <- inside "Loading lipsync from .rbsong" $ errorToWarning $ getRBSongLipsync rbsong
          let results = do
                (name, lip) <- fromMaybe [] lips
                -- note, these have all lowercase visemes and we need to fix them for RB3
                return (name <> ".lipsync", runPut $ putLipsync $ standardVisemeCase lip)
          -- old .rbsong style has singalong notes in the venue, so we don't need to add any
          return (results, [])

  let miloVenue = do
        guard $ level == ImportFull
        rbsong <- toList maybeRBSong
        return ("song.anim", runPut $ putAnim $ getRBSongVenue beatTrackSeconds rbsong)

      miloSongPref = do
        guard $ level == ImportFull
        let numToPart k fallback = case lookup k venueAuthoring of
              Just (IntValue 0) -> "bass"
              Just (IntValue 1) -> "drum"
              Just (IntValue 2) -> "guitar"
              _                 -> fallback
            songPref = SongPref
              { prefU1 = 3
              , prefU2 = 2
              , prefU3 = 0
              , prefU4 = 0
              , prefU5 = 0
              , prefPart2 = numToPart "part2_instrument" "guitar"
              , prefPart3 = numToPart "part3_instrument" "bass"
              , prefPart4 = numToPart "part4_instrument" "drum"
              , prefStyle = "rocker" -- does this exist anywhere? we don't import yet anyway
              }
        return ("BandSongPref", runPut $ void $ codecOut bin songPref)

  let decodeBS = TE.decodeUtf8 -- UTF-8 verified with Thunder and Lightning (Motörhead)
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
            -- this will still have 'fake' and 'crowd' but they will be ignored
          , D.pans             = moggDTA.pans
          , D.vols             = moggDTA.vols
          , D.cores            = map (const (-1)) moggDTA.pans
          , D.crowdChannels    = lookup "crowd" moggDTA.tracks
          , D.vocalParts       = Just $ fromIntegral dta.vocalParts
          , D.drumSolo         = D.DrumSounds [] -- not used in import
          , D.drumFreestyle    = D.DrumSounds [] -- not used in import
          , D.muteVolume       = Nothing -- not used in import
          , D.muteVolumeVocals = Nothing -- not used in import
          , D.hopoThreshold    = Just $ fromIntegral hopoThreshold
          , D.midiFile         = Nothing
          }
        , D.songScrollSpeed   = case lookup "vocal_track_scroll_duration_ms" songMeta of
          Just (LongValue n) -> fromIntegral n
          _                  -> 2300
        -- bank could come from "vocal_percussion_patch" in RBSongMetadata,
        -- but we just use midi events to distinguish cowbell/clap/tambourine anyway
        , D.bank              = Nothing
        , D.drumBank          = case lookup "drum_kit_patch" songMeta of
          Just (ResourcePathValue x _) -> case x of
            "fusion/patches/kit01.fusion" -> Just "sfx/kit01_bank.milo"
            "fusion/patches/kit02.fusion" -> Just "sfx/kit02_bank.milo"
            "fusion/patches/kit03.fusion" -> Just "sfx/kit03_bank.milo"
            "fusion/patches/kit04.fusion" -> Just "sfx/kit04_bank.milo"
            "fusion/patches/kit05.fusion" -> Just "sfx/kit05_bank.milo"
            _                             -> Nothing
          _                            -> Nothing
        , D.animTempo         = case dta.animTempo of
          -- TODO there's also "tempo" in RBSongMetadata? do these ever differ
          "fast"   -> Left D.KTempoFast
          "medium" -> Left D.KTempoMedium
          "slow"   -> Left D.KTempoSlow
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
        , D.rating            = 4 -- seemingly not there
        , D.subGenre          = Nothing
        , D.songId            = Just $ Left $ fromIntegral dta.songId
        , D.solo              = Nothing
        , D.tuningOffsetCents = case lookup "global_tuning_offset" songMeta of
          Just (FloatValue cents) -> Just cents
          _                       -> Nothing
        , D.guidePitchVolume  = Nothing
        , D.gameOrigin        = Just $ decodeBS dta.gameOrigin
        , D.encoding          = Just "utf8"
        , D.albumName         = Just $ decodeBS dta.albumName -- can this be blank?
        , D.albumTrackNumber  = Just $ fromIntegral dta.albumTrackNumber
        , D.vocalTonicNote    = case lookup "vocal_tonic_note" songMeta of
          Just (LongValue n) | 0 <= n && n <= 11 -> Just $ toEnum $ fromIntegral n
          -- note, RBN rereleases have vocal_tonic_note -1
          _                                      -> Nothing
        , D.songTonality      = Nothing -- seemingly not there
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
    , bink = Nothing
    , pss = Nothing
    , albumArt = return . Right <$> art
    , milo = miloLipsync <> miloVenue <> miloSongPref
    , midi = midRead
    , midiUpdate = Nothing
    , source = Nothing
    , addSingalongs = singalongs
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
